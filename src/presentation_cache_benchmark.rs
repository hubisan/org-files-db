//! Production-path benchmark support for registered presentation view caches.

use std::{
    ffi::OsString,
    fs,
    io::Read,
    path::{Path, PathBuf},
    process::{Child, Command, Stdio},
    thread,
    time::{Duration, Instant},
};

use serde::Serialize;

use crate::{
    config::Config,
    db::{open_existing_database_read_only, read_index_state},
    presentation_benchmark::{
        prepare_benchmark_databases, PresentationWorkload, Timing, CORPUS_CONTRACT_VERSION,
        WORKLOADS,
    },
    presentation_view::{
        register_presentation_view, remove_presentation_view, show_presentation_view,
        wait_for_presentation_view, PresentationViewDefinition, PresentationViewInclude,
        PresentationViewOutputMode, ViewControlClientError,
    },
};

pub const OUTPUT_SCHEMA_VERSION: &str = "2";
pub const DEFAULT_WARMUPS: usize = 1;
pub const DEFAULT_ITERATIONS: usize = 3;
pub const DEFAULT_ROW_COUNTS: &[usize] = &[50_000];

const WATCHER_READY_TIMEOUT: Duration = Duration::from_secs(15);
const WATCHER_READY_POLL_INTERVAL: Duration = Duration::from_millis(10);
const REPRESENTATIVE_WORKLOAD_IDS: &[&str] = &[
    "headings.wide",
    "headings.tags",
    "headings.effective-properties",
    "files.keywords",
];

#[derive(Debug, Clone)]
pub struct PresentationCacheBenchmarkOptions {
    pub row_counts: Vec<usize>,
    pub warmups: usize,
    pub iterations: usize,
}

impl Default for PresentationCacheBenchmarkOptions {
    fn default() -> Self {
        Self {
            row_counts: DEFAULT_ROW_COUNTS.to_vec(),
            warmups: DEFAULT_WARMUPS,
            iterations: DEFAULT_ITERATIONS,
        }
    }
}

#[derive(Debug, Serialize)]
pub struct PresentationCacheBenchmarkOutput {
    pub output_schema_version: &'static str,
    pub corpus_contract_version: &'static str,
    pub protocol: PresentationCacheBenchmarkProtocol,
    pub environment: PresentationCacheBenchmarkEnvironment,
    pub sizes: Vec<PresentationCacheSizeResult>,
}

#[derive(Debug, Serialize)]
pub struct PresentationCacheBenchmarkProtocol {
    pub warmups: usize,
    pub iterations: usize,
    pub row_counts: Vec<usize>,
    pub production_path: &'static str,
    pub rebuild_measurement: &'static str,
    pub cache_hit_measurement: &'static str,
    pub uncached_reference: &'static str,
    pub isolation: &'static str,
    pub machine_comparison: &'static str,
}

#[derive(Debug, Serialize)]
pub struct PresentationCacheBenchmarkEnvironment {
    pub command_arguments: Vec<String>,
    pub build_profile: &'static str,
    pub operating_system: &'static str,
    pub architecture: &'static str,
    pub available_cpus: Option<usize>,
    pub orgfdb_path: String,
}

#[derive(Debug, Serialize)]
pub struct PresentationCacheSizeResult {
    pub target_results: usize,
    pub database_id: String,
    pub generation: i64,
    pub workloads: Vec<PresentationCacheWorkloadResult>,
}

#[derive(Debug, Serialize)]
pub struct PresentationCacheWorkloadResult {
    pub id: &'static str,
    pub query: &'static str,
    pub presentation_spec_json: &'static str,
    pub payload_bytes: usize,
    pub initial_build_ready_elapsed_ns: u128,
    pub rebuild_ready_elapsed: Timing,
    pub cache_hit_cli_elapsed: Timing,
    pub uncached_cli_total_elapsed: Timing,
}

pub fn run(
    output: &Path,
    work_dir: &Path,
    orgfdb: &Path,
    options: PresentationCacheBenchmarkOptions,
) -> Result<(), String> {
    validate_options(&options)?;
    if output.exists() {
        return Err(format!(
            "benchmark output must not already exist: {}",
            output.display()
        ));
    }
    if work_dir.exists() {
        return Err(format!(
            "benchmark work directory must not already exist: {}",
            work_dir.display()
        ));
    }

    let orgfdb = absolute_existing_path(orgfdb, "orgfdb binary")?;
    fs::create_dir_all(work_dir).map_err(|error| error.to_string())?;
    let work_dir = fs::canonicalize(work_dir).map_err(|error| error.to_string())?;
    let runtime_dir = work_dir.join("runtime");
    let cache_home = work_dir.join("cache-home");
    fs::create_dir_all(&runtime_dir).map_err(|error| error.to_string())?;
    fs::create_dir_all(&cache_home).map_err(|error| error.to_string())?;
    let _runtime_override = EnvironmentOverride::set("XDG_RUNTIME_DIR", &runtime_dir);
    let _cache_override = EnvironmentOverride::set("XDG_CACHE_HOME", &cache_home);

    let presentation_work_dir = work_dir.join("presentation-corpus");
    prepare_benchmark_databases(&presentation_work_dir, &options.row_counts, 1)?;

    let workloads = representative_workloads()?;
    let mut sizes = Vec::with_capacity(options.row_counts.len());
    for target_results in &options.row_counts {
        sizes.push(run_size(
            &presentation_work_dir,
            &orgfdb,
            *target_results,
            &workloads,
            &options,
        )?);
    }

    let result = PresentationCacheBenchmarkOutput {
        output_schema_version: OUTPUT_SCHEMA_VERSION,
        corpus_contract_version: CORPUS_CONTRACT_VERSION,
        protocol: PresentationCacheBenchmarkProtocol {
            warmups: options.warmups,
            iterations: options.iterations,
            row_counts: options.row_counts.clone(),
            production_path: "real orgfdb watch process, production view control protocol, production rebuild workers, production cache store, and orgfdb view read",
            rebuild_measurement: "remove the current benchmark registration outside each timing sample, then time registration of a new revision until the production control protocol reports the cache ready; includes rebuild worker and atomic cache persistence; excludes payload reading",
            cache_hit_measurement: "fresh orgfdb view read process per sample; stdout is captured through a pipe and compared with the uncached presentation-json payload",
            uncached_reference: "fresh orgfdb query --format presentation-json process per sample on the same database generation",
            isolation: "benchmark-specific XDG_RUNTIME_DIR and XDG_CACHE_HOME below the work directory",
            machine_comparison: "absolute timings are for same-machine regression and diagnosis; do not compare absolute timings across machines",
        },
        environment: PresentationCacheBenchmarkEnvironment {
            command_arguments: std::env::args().collect(),
            build_profile: if cfg!(debug_assertions) {
                "debug"
            } else {
                "release"
            },
            operating_system: std::env::consts::OS,
            architecture: std::env::consts::ARCH,
            available_cpus: std::thread::available_parallelism()
                .ok()
                .map(usize::from),
            orgfdb_path: orgfdb.display().to_string(),
        },
        sizes,
    };

    if let Some(parent) = output
        .parent()
        .filter(|parent| !parent.as_os_str().is_empty())
    {
        fs::create_dir_all(parent).map_err(|error| error.to_string())?;
    }
    fs::write(
        output,
        serde_json::to_vec_pretty(&result).map_err(|error| error.to_string())?,
    )
    .map_err(|error| error.to_string())
}

fn run_size(
    presentation_work_dir: &Path,
    orgfdb: &Path,
    target_results: usize,
    workloads: &[PresentationWorkload],
    options: &PresentationCacheBenchmarkOptions,
) -> Result<PresentationCacheSizeResult, String> {
    let size_dir = presentation_work_dir.join(format!("rows-{target_results}"));
    let db_path = size_dir.join("org-files-db.sqlite");
    let config_path = size_dir.join("org-files-db.toml");
    let config = Config::load_from_file(&config_path).map_err(|error| error.to_string())?;
    let state = read_state(&db_path)?;
    let mut watcher = WatcherProcess::start(orgfdb, &config_path, &config)?;

    let measurement = (|| {
        let mut results = Vec::with_capacity(workloads.len());
        for workload in workloads {
            results.push(measure_workload(
                &config,
                &config_path,
                orgfdb,
                *workload,
                options,
            )?);
        }
        Ok(PresentationCacheSizeResult {
            target_results,
            database_id: state.database_id,
            generation: state.generation,
            workloads: results,
        })
    })();

    let shutdown = watcher.shutdown();
    match (measurement, shutdown) {
        (Ok(result), Ok(())) => Ok(result),
        (Err(error), Ok(())) => Err(error),
        (Ok(_), Err(error)) => Err(error),
        (Err(error), Err(shutdown_error)) => Err(format!(
            "{error}; watcher shutdown also failed: {shutdown_error}"
        )),
    }
}

fn measure_workload(
    config: &Config,
    config_path: &Path,
    orgfdb: &Path,
    workload: PresentationWorkload,
    options: &PresentationCacheBenchmarkOptions,
) -> Result<PresentationCacheWorkloadResult, String> {
    let expected_payload = run_uncached_cli(orgfdb, config_path, workload)?;
    let definition = benchmark_view_definition(config, workload)?;

    let initial_start = Instant::now();
    let registration = register_presentation_view(config, definition.clone())
        .map_err(|error| error.to_string())?;
    let ticket = wait_for_presentation_view(config, definition.name.clone())
        .map_err(|error| error.to_string())?;
    if ticket.view.revision != registration.revision {
        return Err(format!(
            "{} initial cache revision changed while it was building",
            workload.id
        ));
    }
    let initial_build_ready_elapsed_ns = initial_start.elapsed().as_nanos();

    let initial_cached_payload = run_cache_hit(orgfdb, config_path, &definition.name)?;
    ensure_payload_equal(workload.id, &expected_payload, &initial_cached_payload)?;

    for _ in 0..options.warmups {
        let _ = rebuild_view(config, &definition, workload.id)?;
    }
    let rebuild_ready_elapsed =
        measure_rebuilds(config, &definition, workload.id, options.iterations)?;

    for _ in 0..options.warmups {
        let payload = run_cache_hit(orgfdb, config_path, &definition.name)?;
        ensure_payload_equal(workload.id, &expected_payload, &payload)?;
    }
    let cache_hit_cli_elapsed = measure_operation(options.iterations, || {
        let payload = run_cache_hit(orgfdb, config_path, &definition.name)?;
        ensure_payload_equal(workload.id, &expected_payload, &payload)
    })?;

    for _ in 0..options.warmups {
        let payload = run_uncached_cli(orgfdb, config_path, workload)?;
        ensure_payload_equal(workload.id, &expected_payload, &payload)?;
    }
    let uncached_cli_total_elapsed = measure_operation(options.iterations, || {
        let payload = run_uncached_cli(orgfdb, config_path, workload)?;
        ensure_payload_equal(workload.id, &expected_payload, &payload)
    })?;

    Ok(PresentationCacheWorkloadResult {
        id: workload.id,
        query: workload.query,
        presentation_spec_json: workload.presentation_spec_json,
        payload_bytes: expected_payload.len(),
        initial_build_ready_elapsed_ns,
        rebuild_ready_elapsed,
        cache_hit_cli_elapsed,
        uncached_cli_total_elapsed,
    })
}

fn rebuild_view(
    config: &Config,
    definition: &PresentationViewDefinition,
    workload_id: &str,
) -> Result<Duration, String> {
    let removal = remove_presentation_view(config, definition.name.clone())
        .map_err(|error| error.to_string())?;
    if !removal.removed {
        return Err(format!(
            "{workload_id} benchmark view was not registered before rebuild"
        ));
    }

    let start = Instant::now();
    let registration = register_presentation_view(config, definition.clone())
        .map_err(|error| error.to_string())?;
    let ticket = wait_for_presentation_view(config, definition.name.clone())
        .map_err(|error| error.to_string())?;
    if ticket.view.revision != registration.revision {
        return Err(format!(
            "{workload_id} cache revision changed while it was rebuilding"
        ));
    }
    Ok(start.elapsed())
}

fn measure_rebuilds(
    config: &Config,
    definition: &PresentationViewDefinition,
    workload_id: &str,
    iterations: usize,
) -> Result<Timing, String> {
    let mut samples = Vec::with_capacity(iterations);
    for _ in 0..iterations {
        samples.push(rebuild_view(config, definition, workload_id)?);
    }
    Ok(timing(&mut samples))
}

fn benchmark_view_definition(
    config: &Config,
    workload: PresentationWorkload,
) -> Result<PresentationViewDefinition, String> {
    let presentation_spec =
        serde_json::from_str(workload.presentation_spec_json).map_err(|error| error.to_string())?;
    Ok(PresentationViewDefinition {
        name: format!("benchmark-{}", workload.id.replace('.', "-")),
        query: workload.query.to_string(),
        output: PresentationViewOutputMode::Flat,
        includes: Vec::<PresentationViewInclude>::new(),
        query_timezone: config.query.timezone.clone(),
        relative_date_dependent: false,
        presentation_spec,
    })
}

fn ensure_payload_equal(id: &str, expected: &[u8], actual: &[u8]) -> Result<(), String> {
    if actual != expected {
        return Err(format!(
            "{id} cached presentation payload differs from the uncached production payload"
        ));
    }
    Ok(())
}

fn run_uncached_cli(
    orgfdb: &Path,
    config_path: &Path,
    workload: PresentationWorkload,
) -> Result<Vec<u8>, String> {
    let output = Command::new(orgfdb)
        .args([
            "query",
            "--format",
            "presentation-json",
            "--presentation-spec-json",
            workload.presentation_spec_json,
            "--config",
        ])
        .arg(config_path)
        .arg(workload.query)
        .output()
        .map_err(|error| error.to_string())?;
    if !output.status.success() {
        return Err(format!(
            "{} uncached CLI sample failed with {}: {}",
            workload.id,
            output.status,
            String::from_utf8_lossy(&output.stderr)
        ));
    }
    Ok(output.stdout)
}

fn run_cache_hit(orgfdb: &Path, config_path: &Path, name: &str) -> Result<Vec<u8>, String> {
    let output = Command::new(orgfdb)
        .args(["view", "read", "--config"])
        .arg(config_path)
        .arg(name)
        .output()
        .map_err(|error| error.to_string())?;
    if !output.status.success() {
        return Err(format!(
            "cache-hit CLI sample failed with {}: {}",
            output.status,
            String::from_utf8_lossy(&output.stderr)
        ));
    }
    Ok(output.stdout)
}

fn read_state(db_path: &Path) -> Result<crate::db::index_state::IndexState, String> {
    let connection =
        open_existing_database_read_only(db_path).map_err(|error| error.to_string())?;
    read_index_state(&connection).map_err(|error| error.to_string())
}

fn measure_operation<F>(iterations: usize, mut operation: F) -> Result<Timing, String>
where
    F: FnMut() -> Result<(), String>,
{
    let mut samples = Vec::with_capacity(iterations);
    for _ in 0..iterations {
        let start = Instant::now();
        operation()?;
        samples.push(start.elapsed());
    }
    Ok(timing(&mut samples))
}

fn timing(samples: &mut [Duration]) -> Timing {
    samples.sort();
    let len = samples.len();
    let median = samples[len / 2];
    let p95_index = ((len * 95).div_ceil(100)).saturating_sub(1).min(len - 1);
    Timing {
        samples: len,
        min_ns: samples[0].as_nanos(),
        median_ns: median.as_nanos(),
        max_ns: samples[len - 1].as_nanos(),
        p95_ns: samples[p95_index].as_nanos(),
    }
}

fn representative_workloads() -> Result<Vec<PresentationWorkload>, String> {
    REPRESENTATIVE_WORKLOAD_IDS
        .iter()
        .map(|id| workload_by_id(id))
        .collect()
}

fn workload_by_id(id: &str) -> Result<PresentationWorkload, String> {
    WORKLOADS
        .iter()
        .copied()
        .find(|workload| workload.id == id)
        .ok_or_else(|| format!("unknown presentation benchmark workload {id:?}"))
}

fn absolute_existing_path(path: &Path, label: &str) -> Result<PathBuf, String> {
    if !path.exists() {
        return Err(format!("{label} does not exist: {}", path.display()));
    }
    fs::canonicalize(path).map_err(|error| error.to_string())
}

fn validate_options(options: &PresentationCacheBenchmarkOptions) -> Result<(), String> {
    if options.row_counts.is_empty() || options.row_counts.contains(&0) {
        return Err("benchmark row counts must be positive".into());
    }
    if options.iterations == 0 {
        return Err("benchmark iterations must be positive".into());
    }
    Ok(())
}

struct EnvironmentOverride {
    name: &'static str,
    previous: Option<OsString>,
}

impl EnvironmentOverride {
    fn set(name: &'static str, value: &Path) -> Self {
        let previous = std::env::var_os(name);
        std::env::set_var(name, value);
        Self { name, previous }
    }
}

impl Drop for EnvironmentOverride {
    fn drop(&mut self) {
        if let Some(previous) = &self.previous {
            std::env::set_var(self.name, previous);
        } else {
            std::env::remove_var(self.name);
        }
    }
}

struct WatcherProcess {
    child: Child,
    stopped: bool,
}

impl WatcherProcess {
    fn start(orgfdb: &Path, config_path: &Path, config: &Config) -> Result<Self, String> {
        let child = Command::new(orgfdb)
            .args(["watch", "--config"])
            .arg(config_path)
            .stdout(Stdio::null())
            .stderr(Stdio::piped())
            .spawn()
            .map_err(|error| error.to_string())?;
        let mut process = Self {
            child,
            stopped: false,
        };
        process.wait_until_ready(config)?;
        Ok(process)
    }

    fn wait_until_ready(&mut self, config: &Config) -> Result<(), String> {
        let start = Instant::now();
        loop {
            if let Some(status) = self.child.try_wait().map_err(|error| error.to_string())? {
                self.stopped = true;
                let stderr = read_child_stderr(&mut self.child);
                return Err(format!(
                    "orgfdb watch exited before view control became ready with {status}: {stderr}"
                ));
            }

            match show_presentation_view(config, "__benchmark_ready_probe__".to_string()) {
                Err(ViewControlClientError::Remote { code, .. }) if code == "view_not_found" => {
                    return Ok(())
                }
                Ok(_) => {
                    return Err(
                        "benchmark readiness probe unexpectedly found a registered view".into(),
                    )
                }
                Err(_) if start.elapsed() < WATCHER_READY_TIMEOUT => {
                    thread::sleep(WATCHER_READY_POLL_INTERVAL);
                }
                Err(error) => {
                    return Err(format!(
                        "orgfdb watch did not make view control ready within {} seconds: {error}",
                        WATCHER_READY_TIMEOUT.as_secs()
                    ))
                }
            }
        }
    }

    fn shutdown(&mut self) -> Result<(), String> {
        if self.stopped {
            return Ok(());
        }
        if self
            .child
            .try_wait()
            .map_err(|error| error.to_string())?
            .is_none()
        {
            let status = Command::new("kill")
                .arg("-TERM")
                .arg(self.child.id().to_string())
                .status()
                .map_err(|error| error.to_string())?;
            if !status.success() {
                return Err(format!(
                    "failed to send SIGTERM to orgfdb watch process {}",
                    self.child.id()
                ));
            }
        }
        let status = self.child.wait().map_err(|error| error.to_string())?;
        self.stopped = true;
        if !status.success() {
            let stderr = read_child_stderr(&mut self.child);
            return Err(format!("orgfdb watch exited with {status}: {stderr}"));
        }
        Ok(())
    }
}

impl Drop for WatcherProcess {
    fn drop(&mut self) {
        if !self.stopped {
            let _ = self.child.kill();
            let _ = self.child.wait();
            self.stopped = true;
        }
    }
}

fn read_child_stderr(child: &mut Child) -> String {
    let mut stderr = String::new();
    if let Some(mut stream) = child.stderr.take() {
        let _ = stream.read_to_string(&mut stderr);
    }
    stderr.trim().to_string()
}

#[cfg(test)]
mod tests {
    use super::{representative_workloads, validate_options, PresentationCacheBenchmarkOptions};

    #[test]
    fn representative_workloads_exist() {
        let workloads = representative_workloads().expect("representative workloads should exist");
        assert_eq!(workloads.len(), 4);
        assert_eq!(workloads[0].id, "headings.wide");
        assert_eq!(workloads[2].id, "headings.effective-properties");
    }

    #[test]
    fn benchmark_options_reject_empty_and_zero_values() {
        let options = PresentationCacheBenchmarkOptions {
            row_counts: Vec::new(),
            ..PresentationCacheBenchmarkOptions::default()
        };
        assert!(validate_options(&options).is_err());

        let options = PresentationCacheBenchmarkOptions {
            iterations: 0,
            ..PresentationCacheBenchmarkOptions::default()
        };
        assert!(validate_options(&options).is_err());
    }
}
