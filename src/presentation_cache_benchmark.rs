//! Measurement-only validation for eager caches of registered presentation views.

use std::{
    fs::{self, File},
    io::{self, Read, Write},
    path::{Path, PathBuf},
    process::{Child, Command, Stdio},
    thread,
    time::{Duration, Instant},
};

use serde::Serialize;

use crate::{
    db::{
        advance_index_generation, open_database_with_schema, open_existing_database_read_only,
        read_index_state, IndexGenerationChange, SchemaDefinition, CURRENT_SCHEMA_VERSION,
    },
    presentation_benchmark::{
        load_workload_response, prepare_benchmark_databases, PresentationWorkload, Timing,
        CORPUS_CONTRACT_VERSION, WORKLOADS,
    },
};

pub const OUTPUT_SCHEMA_VERSION: &str = "1";
pub const DEFAULT_WARMUPS: usize = 1;
pub const DEFAULT_ITERATIONS: usize = 3;
pub const DEFAULT_GROUP_ITERATIONS: usize = 2;
pub const DEFAULT_ROW_COUNTS: &[usize] = &[50_000];
pub const DEFAULT_VIEW_COUNTS: &[usize] = &[1, 3, 5, 10];
pub const DEFAULT_CANCEL_PUBLISH_DELAY_MS: u64 = 5_000;
pub const DEFAULT_POLL_INTERVAL_MS: u64 = 5;

const REPRESENTATIVE_WORKLOAD_IDS: &[&str] = &[
    "headings.wide",
    "headings.tags",
    "headings.effective-properties",
    "files.keywords",
];

#[derive(Debug, Clone)]
pub struct PresentationCacheBenchmarkOptions {
    pub row_counts: Vec<usize>,
    pub view_counts: Vec<usize>,
    pub warmups: usize,
    pub iterations: usize,
    pub group_iterations: usize,
}

impl Default for PresentationCacheBenchmarkOptions {
    fn default() -> Self {
        Self {
            row_counts: DEFAULT_ROW_COUNTS.to_vec(),
            view_counts: DEFAULT_VIEW_COUNTS.to_vec(),
            warmups: DEFAULT_WARMUPS,
            iterations: DEFAULT_ITERATIONS,
            group_iterations: DEFAULT_GROUP_ITERATIONS,
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
    pub group_iterations: usize,
    pub row_counts: Vec<usize>,
    pub view_counts: Vec<usize>,
    pub cache_value: &'static str,
    pub rebuild_worker: &'static str,
    pub persistence: &'static str,
    pub cache_hit: &'static str,
    pub wait_measurement: &'static str,
    pub cancellation: &'static str,
    pub worker_parallelism: &'static str,
    pub memory_measurement: &'static str,
    pub production_cache: &'static str,
}

#[derive(Debug, Serialize)]
pub struct PresentationCacheBenchmarkEnvironment {
    pub command_arguments: Vec<String>,
    pub build_profile: &'static str,
    pub operating_system: &'static str,
    pub architecture: &'static str,
    pub available_cpus: Option<usize>,
    pub orgfdb_path: String,
    pub benchmark_executable_path: String,
}

#[derive(Debug, Serialize)]
pub struct PresentationCacheSizeResult {
    pub target_results: usize,
    pub database_id: String,
    pub initial_generation: i64,
    pub rebuild_generation: i64,
    pub workloads: Vec<PresentationCacheWorkloadResult>,
    pub rebuild_groups: Vec<PresentationCacheRebuildGroupResult>,
    pub obsolete_rebuild: ObsoleteRebuildResult,
}

#[derive(Debug, Serialize)]
pub struct PresentationCacheWorkloadResult {
    pub id: &'static str,
    pub query: &'static str,
    pub presentation_spec_json: &'static str,
    pub payload_bytes: u64,
    pub initial_build_persist_elapsed_ns: u128,
    pub uncached_cli_total_elapsed: Timing,
    pub rebuild_persist_elapsed: Timing,
    pub cache_hit_cli_elapsed: Timing,
    pub immediate_request_wait_elapsed: Timing,
    pub rebuild_peak_worker_rss_bytes: Option<u64>,
    pub immediate_wait_peak_worker_rss_bytes: Option<u64>,
}

#[derive(Debug, Serialize)]
pub struct PresentationCacheRebuildGroupResult {
    pub view_count: usize,
    pub workload_ids: Vec<&'static str>,
    pub rayon_threads_per_parallel_worker: Option<usize>,
    pub sequential_elapsed: Timing,
    pub parallel_unbounded_elapsed: Timing,
    pub parallel_budgeted_elapsed: Timing,
    pub parallel_unbounded_peak_worker_rss_bytes: Option<u64>,
    pub parallel_budgeted_peak_worker_rss_bytes: Option<u64>,
}

#[derive(Debug, Serialize)]
pub struct ObsoleteRebuildResult {
    pub workload_id: &'static str,
    pub old_generation: i64,
    pub new_generation: i64,
    pub obsolete_payload_generation: i64,
    pub replacement_payload_generation: i64,
    pub obsolete_cache_published: bool,
    pub replacement_elapsed_ns: u128,
    pub termination_elapsed_ns: u128,
}

pub fn run(
    output: &Path,
    work_dir: &Path,
    orgfdb: &Path,
    benchmark_executable: &Path,
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
    let benchmark_executable =
        absolute_existing_path(benchmark_executable, "cache benchmark executable")?;
    fs::create_dir_all(work_dir).map_err(|error| error.to_string())?;
    let work_dir = fs::canonicalize(work_dir).map_err(|error| error.to_string())?;
    let presentation_work_dir = work_dir.join("presentation-corpus");
    prepare_benchmark_databases(&presentation_work_dir, &options.row_counts, 1)?;

    let workloads = representative_workloads()?;
    let mut sizes = Vec::with_capacity(options.row_counts.len());
    for target_results in &options.row_counts {
        sizes.push(run_size(
            &presentation_work_dir,
            &work_dir,
            &orgfdb,
            &benchmark_executable,
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
            group_iterations: options.group_iterations,
            row_counts: options.row_counts.clone(),
            view_counts: options.view_counts.clone(),
            cache_value: "complete presentation-json version 2 payload including trailing CLI newline",
            rebuild_worker: "fresh benchmark worker process uses the production query and presentation pipeline",
            persistence: "write a worker-specific temporary file, close it, then atomically rename it to the current cache path; no fsync",
            cache_hit: "fresh benchmark process reads the materialized payload and writes it to stdout; parent captures stdout through a pipe; excludes future registry IPC",
            wait_measurement: "request starts immediately after rebuild worker spawn, waits for rebuild completion, then performs one fresh-process cache hit",
            cancellation: "obsolete worker writes a complete temporary payload and pauses before publish; parent advances the benchmark database generation, kills the worker, then rebuilds the newest generation",
            worker_parallelism: "compare sequential workers, parallel workers with each default Rayon pool, and parallel workers with RAYON_NUM_THREADS divided across active views",
            memory_measurement: "poll Linux /proc/<pid>/status VmRSS and record the maximum aggregate active-worker RSS; unavailable on other operating systems",
            production_cache: "none; this benchmark does not add view registration, watcher cache state, or orgfdb view commands",
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
            benchmark_executable_path: benchmark_executable.display().to_string(),
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
    work_dir: &Path,
    orgfdb: &Path,
    benchmark_executable: &Path,
    target_results: usize,
    workloads: &[PresentationWorkload],
    options: &PresentationCacheBenchmarkOptions,
) -> Result<PresentationCacheSizeResult, String> {
    let size_dir = presentation_work_dir.join(format!("rows-{target_results}"));
    let db_path = size_dir.join("org-files-db.sqlite");
    let config_path = size_dir.join("org-files-db.toml");
    let cache_dir = work_dir.join(format!("cache-rows-{target_results}"));
    fs::create_dir_all(&cache_dir).map_err(|error| error.to_string())?;

    let initial_state = read_state(&db_path)?;
    let mut initial_builds = Vec::with_capacity(workloads.len());
    for workload in workloads {
        let cache_path = cache_dir.join(format!("{}.json", workload_file_name(workload.id)));
        let measurement = run_rebuild_sample(
            benchmark_executable,
            &db_path,
            *workload,
            target_results,
            &cache_path,
            None,
        )?;
        initial_builds.push((*workload, cache_path, measurement));
    }

    let rebuild_generation = advance_benchmark_generation(&db_path)?;
    if rebuild_generation <= initial_state.generation {
        return Err("benchmark generation did not advance".into());
    }

    let mut workload_results = Vec::with_capacity(workloads.len());
    for (workload, cache_path, initial_build) in initial_builds {
        let mut rebuild_peak = None;
        let rebuild_persist_elapsed = measure_process_operation(options, || {
            let sample = run_rebuild_sample(
                benchmark_executable,
                &db_path,
                workload,
                target_results,
                &cache_path,
                None,
            )?;
            rebuild_peak = max_optional(rebuild_peak, sample.peak_rss_bytes);
            Ok(())
        })?;

        let expected_payload = fs::read(&cache_path).map_err(|error| error.to_string())?;
        let payload_bytes = expected_payload.len() as u64;
        let cached_generation = payload_generation(&expected_payload)?;
        if cached_generation != rebuild_generation {
            return Err(format!(
                "{} cache generation {cached_generation} differs from current generation {rebuild_generation}",
                workload.id
            ));
        }

        let uncached_cli_total_elapsed = measure_process_operation(options, || {
            let output = run_uncached_cli(orgfdb, &config_path, workload)?;
            if output != expected_payload {
                return Err(format!(
                    "{} uncached CLI payload differs from the benchmark cache payload",
                    workload.id
                ));
            }
            Ok(())
        })?;

        let cache_hit_cli_elapsed = measure_process_operation(options, || {
            let output = run_cache_hit(benchmark_executable, &cache_path)?;
            if output != expected_payload {
                return Err(format!(
                    "{} cache-hit payload differs from the materialized payload",
                    workload.id
                ));
            }
            Ok(())
        })?;

        let mut wait_peak = None;
        let immediate_request_wait_elapsed = measure_process_operation(options, || {
            let sample = run_wait_sample(
                benchmark_executable,
                &db_path,
                workload,
                target_results,
                &cache_path,
                &expected_payload,
            )?;
            wait_peak = max_optional(wait_peak, sample.peak_rss_bytes);
            Ok(())
        })?;

        workload_results.push(PresentationCacheWorkloadResult {
            id: workload.id,
            query: workload.query,
            presentation_spec_json: workload.presentation_spec_json,
            payload_bytes,
            initial_build_persist_elapsed_ns: initial_build.elapsed.as_nanos(),
            uncached_cli_total_elapsed,
            rebuild_persist_elapsed,
            cache_hit_cli_elapsed,
            immediate_request_wait_elapsed,
            rebuild_peak_worker_rss_bytes: rebuild_peak,
            immediate_wait_peak_worker_rss_bytes: wait_peak,
        });
    }

    let rebuild_groups = measure_rebuild_groups(
        benchmark_executable,
        &db_path,
        &cache_dir,
        target_results,
        workloads,
        options,
    )?;
    let obsolete_rebuild = measure_obsolete_rebuild(
        benchmark_executable,
        &db_path,
        &cache_dir,
        target_results,
        workloads[2],
    )?;

    Ok(PresentationCacheSizeResult {
        target_results,
        database_id: initial_state.database_id,
        initial_generation: initial_state.generation,
        rebuild_generation,
        workloads: workload_results,
        rebuild_groups,
        obsolete_rebuild,
    })
}

fn measure_rebuild_groups(
    benchmark_executable: &Path,
    db_path: &Path,
    cache_dir: &Path,
    target_results: usize,
    workloads: &[PresentationWorkload],
    options: &PresentationCacheBenchmarkOptions,
) -> Result<Vec<PresentationCacheRebuildGroupResult>, String> {
    let available_cpus = std::thread::available_parallelism().ok().map(usize::from);
    let mut groups = Vec::with_capacity(options.view_counts.len());

    for view_count in &options.view_counts {
        let selected = (0..*view_count)
            .map(|index| workloads[index % workloads.len()])
            .collect::<Vec<_>>();
        let workload_ids = selected.iter().map(|workload| workload.id).collect();
        let budgeted_threads = available_cpus.map(|cpus| (cpus / *view_count).max(1));

        let sequential_elapsed = measure_group(options.group_iterations, || {
            run_sequential_group(
                benchmark_executable,
                db_path,
                cache_dir,
                target_results,
                &selected,
            )
        })?;

        let mut unbounded_peak = None;
        let parallel_unbounded_elapsed = measure_group(options.group_iterations, || {
            let sample = run_parallel_group(
                benchmark_executable,
                db_path,
                cache_dir,
                target_results,
                &selected,
                None,
                "unbounded",
            )?;
            unbounded_peak = max_optional(unbounded_peak, sample.peak_rss_bytes);
            Ok(())
        })?;

        let mut budgeted_peak = None;
        let parallel_budgeted_elapsed = measure_group(options.group_iterations, || {
            let sample = run_parallel_group(
                benchmark_executable,
                db_path,
                cache_dir,
                target_results,
                &selected,
                budgeted_threads,
                "budgeted",
            )?;
            budgeted_peak = max_optional(budgeted_peak, sample.peak_rss_bytes);
            Ok(())
        })?;

        groups.push(PresentationCacheRebuildGroupResult {
            view_count: *view_count,
            workload_ids,
            rayon_threads_per_parallel_worker: budgeted_threads,
            sequential_elapsed,
            parallel_unbounded_elapsed,
            parallel_budgeted_elapsed,
            parallel_unbounded_peak_worker_rss_bytes: unbounded_peak,
            parallel_budgeted_peak_worker_rss_bytes: budgeted_peak,
        });
    }

    Ok(groups)
}

fn run_sequential_group(
    benchmark_executable: &Path,
    db_path: &Path,
    cache_dir: &Path,
    target_results: usize,
    workloads: &[PresentationWorkload],
) -> Result<(), String> {
    for (index, workload) in workloads.iter().enumerate() {
        let cache_path = cache_dir.join(format!(
            "group-sequential-{index}-{}.json",
            workload_file_name(workload.id)
        ));
        run_rebuild_sample(
            benchmark_executable,
            db_path,
            *workload,
            target_results,
            &cache_path,
            None,
        )?;
    }
    Ok(())
}

fn run_parallel_group(
    benchmark_executable: &Path,
    db_path: &Path,
    cache_dir: &Path,
    target_results: usize,
    workloads: &[PresentationWorkload],
    rayon_threads: Option<usize>,
    label: &str,
) -> Result<ProcessMeasurement, String> {
    let start = Instant::now();
    let mut workers = Vec::with_capacity(workloads.len());
    for (index, workload) in workloads.iter().enumerate() {
        let cache_path = cache_dir.join(format!(
            "group-{label}-{index}-{}.json",
            workload_file_name(workload.id)
        ));
        workers.push(spawn_rebuild_worker(
            benchmark_executable,
            db_path,
            *workload,
            target_results,
            &cache_path,
            RebuildWorkerOptions {
                rayon_threads,
                ..RebuildWorkerOptions::default()
            },
        )?);
    }
    let peak_rss_bytes = wait_for_workers(&mut workers)?;
    Ok(ProcessMeasurement {
        elapsed: start.elapsed(),
        peak_rss_bytes,
    })
}

fn measure_obsolete_rebuild(
    benchmark_executable: &Path,
    db_path: &Path,
    cache_dir: &Path,
    target_results: usize,
    workload: PresentationWorkload,
) -> Result<ObsoleteRebuildResult, String> {
    let cache_path = cache_dir.join("obsolete-generation.json");
    let marker_path = cache_dir.join("obsolete-generation.ready");
    remove_if_exists(&cache_path)?;
    remove_if_exists(&marker_path)?;

    let old_generation = read_state(db_path)?.generation;
    let mut worker = spawn_rebuild_worker(
        benchmark_executable,
        db_path,
        workload,
        target_results,
        &cache_path,
        RebuildWorkerOptions {
            ready_marker: Some(&marker_path),
            publish_delay: Duration::from_millis(DEFAULT_CANCEL_PUBLISH_DELAY_MS),
            ..RebuildWorkerOptions::default()
        },
    )?;
    wait_for_marker(&mut worker, &marker_path, Duration::from_secs(60))?;

    let temp_path = cache_temp_path(&cache_path, worker.child.id());
    let obsolete_payload = fs::read(&temp_path).map_err(|error| error.to_string())?;
    let obsolete_payload_generation = payload_generation(&obsolete_payload)?;
    if obsolete_payload_generation != old_generation {
        return Err(format!(
            "obsolete worker payload generation {obsolete_payload_generation} differs from expected generation {old_generation}"
        ));
    }

    let new_generation = advance_benchmark_generation(db_path)?;
    let termination_start = Instant::now();
    worker.child.kill().map_err(|error| error.to_string())?;
    let status = worker.child.wait().map_err(|error| error.to_string())?;
    let termination_elapsed = termination_start.elapsed();
    if status.success() {
        return Err("obsolete worker unexpectedly exited successfully after kill".into());
    }
    remove_if_exists(&marker_path)?;
    remove_if_exists(&temp_path)?;
    let obsolete_cache_published = cache_path.exists();
    if obsolete_cache_published {
        return Err(
            "obsolete worker published a cache after the database generation changed".into(),
        );
    }

    let replacement_start = Instant::now();
    run_rebuild_sample(
        benchmark_executable,
        db_path,
        workload,
        target_results,
        &cache_path,
        None,
    )?;
    let replacement_elapsed = replacement_start.elapsed();
    let replacement_payload = fs::read(&cache_path).map_err(|error| error.to_string())?;
    let replacement_payload_generation = payload_generation(&replacement_payload)?;
    if replacement_payload_generation != new_generation {
        return Err(format!(
            "replacement payload generation {replacement_payload_generation} differs from current generation {new_generation}"
        ));
    }

    Ok(ObsoleteRebuildResult {
        workload_id: workload.id,
        old_generation,
        new_generation,
        obsolete_payload_generation,
        replacement_payload_generation,
        obsolete_cache_published,
        replacement_elapsed_ns: replacement_elapsed.as_nanos(),
        termination_elapsed_ns: termination_elapsed.as_nanos(),
    })
}

fn wait_for_marker(
    worker: &mut RunningWorker,
    marker_path: &Path,
    timeout: Duration,
) -> Result<(), String> {
    let start = Instant::now();
    loop {
        if marker_path.is_file() {
            return Ok(());
        }
        if let Some(status) = worker.child.try_wait().map_err(|error| error.to_string())? {
            let stderr = read_child_stderr(&mut worker.child);
            return Err(format!(
                "{} exited before the publish marker with {status}: {stderr}",
                worker.label
            ));
        }
        if start.elapsed() >= timeout {
            let _ = worker.child.kill();
            let _ = worker.child.wait();
            return Err(format!(
                "{} did not reach the publish marker within {} ms",
                worker.label,
                timeout.as_millis()
            ));
        }
        thread::sleep(Duration::from_millis(DEFAULT_POLL_INTERVAL_MS));
    }
}

fn advance_benchmark_generation(db_path: &Path) -> Result<i64, String> {
    let mut connection = open_database_with_schema(
        db_path,
        &SchemaDefinition::new(CURRENT_SCHEMA_VERSION, false),
    )
    .map_err(|error| error.to_string())?;
    let transaction = connection
        .transaction()
        .map_err(|error| error.to_string())?;
    advance_index_generation(&transaction, &IndexGenerationChange::full_invalidation())
        .map_err(|error| error.to_string())?;
    transaction.commit().map_err(|error| error.to_string())?;
    Ok(read_state(db_path)?.generation)
}

fn read_state(db_path: &Path) -> Result<crate::db::index_state::IndexState, String> {
    let connection =
        open_existing_database_read_only(db_path).map_err(|error| error.to_string())?;
    read_index_state(&connection).map_err(|error| error.to_string())
}

fn run_rebuild_sample(
    benchmark_executable: &Path,
    db_path: &Path,
    workload: PresentationWorkload,
    target_results: usize,
    cache_path: &Path,
    rayon_threads: Option<usize>,
) -> Result<ProcessMeasurement, String> {
    let start = Instant::now();
    let worker = spawn_rebuild_worker(
        benchmark_executable,
        db_path,
        workload,
        target_results,
        cache_path,
        RebuildWorkerOptions {
            rayon_threads,
            ..RebuildWorkerOptions::default()
        },
    )?;
    let mut workers = vec![worker];
    let peak_rss_bytes = wait_for_workers(&mut workers)?;
    Ok(ProcessMeasurement {
        elapsed: start.elapsed(),
        peak_rss_bytes,
    })
}

fn run_wait_sample(
    benchmark_executable: &Path,
    db_path: &Path,
    workload: PresentationWorkload,
    target_results: usize,
    cache_path: &Path,
    expected_payload: &[u8],
) -> Result<ProcessMeasurement, String> {
    remove_if_exists(cache_path)?;
    let start = Instant::now();
    let worker = spawn_rebuild_worker(
        benchmark_executable,
        db_path,
        workload,
        target_results,
        cache_path,
        RebuildWorkerOptions::default(),
    )?;
    let mut workers = vec![worker];
    let peak_rss_bytes = wait_for_workers(&mut workers)?;
    let output = run_cache_hit(benchmark_executable, cache_path)?;
    if output != expected_payload {
        return Err(format!(
            "{} waited cache-hit payload differs from the expected payload",
            workload.id
        ));
    }
    Ok(ProcessMeasurement {
        elapsed: start.elapsed(),
        peak_rss_bytes,
    })
}

#[derive(Default)]
struct RebuildWorkerOptions<'a> {
    rayon_threads: Option<usize>,
    ready_marker: Option<&'a Path>,
    publish_delay: Duration,
}

fn spawn_rebuild_worker(
    benchmark_executable: &Path,
    db_path: &Path,
    workload: PresentationWorkload,
    target_results: usize,
    cache_path: &Path,
    options: RebuildWorkerOptions<'_>,
) -> Result<RunningWorker, String> {
    let mut command = Command::new(benchmark_executable);
    command
        .arg("__worker-build")
        .arg("--db")
        .arg(db_path)
        .arg("--workload")
        .arg(workload.id)
        .arg("--expected-results")
        .arg(target_results.to_string())
        .arg("--cache-file")
        .arg(cache_path)
        .arg("--publish-delay-ms")
        .arg(options.publish_delay.as_millis().to_string())
        .stdout(Stdio::null())
        .stderr(Stdio::piped());
    if let Some(marker) = options.ready_marker {
        command.arg("--ready-marker").arg(marker);
    }
    if let Some(threads) = options.rayon_threads {
        command.env("RAYON_NUM_THREADS", threads.to_string());
    }
    let child = command.spawn().map_err(|error| error.to_string())?;
    Ok(RunningWorker {
        label: format!("rebuild worker {}", workload.id),
        child,
        done: false,
    })
}

fn wait_for_workers(workers: &mut [RunningWorker]) -> Result<Option<u64>, String> {
    let mut peak_rss_bytes = None;
    loop {
        let mut active = 0usize;
        let mut current_rss = 0u64;
        let mut has_rss = false;

        for worker in workers.iter_mut().filter(|worker| !worker.done) {
            match worker.child.try_wait().map_err(|error| error.to_string())? {
                Some(status) => {
                    worker.done = true;
                    if !status.success() {
                        let stderr = read_child_stderr(&mut worker.child);
                        return Err(format!("{} failed with {status}: {stderr}", worker.label));
                    }
                }
                None => {
                    active += 1;
                    if let Some(rss) = read_process_rss_bytes(worker.child.id()) {
                        current_rss = current_rss.saturating_add(rss);
                        has_rss = true;
                    }
                }
            }
        }

        if has_rss {
            peak_rss_bytes = Some(peak_rss_bytes.unwrap_or(0).max(current_rss));
        }
        if active == 0 {
            return Ok(peak_rss_bytes);
        }
        thread::sleep(Duration::from_millis(DEFAULT_POLL_INTERVAL_MS));
    }
}

fn read_child_stderr(child: &mut Child) -> String {
    let mut stderr = String::new();
    if let Some(mut stream) = child.stderr.take() {
        let _ = stream.read_to_string(&mut stderr);
    }
    stderr.trim().to_string()
}

fn read_process_rss_bytes(pid: u32) -> Option<u64> {
    if !cfg!(target_os = "linux") {
        return None;
    }
    let status = fs::read_to_string(format!("/proc/{pid}/status")).ok()?;
    let line = status.lines().find(|line| line.starts_with("VmRSS:"))?;
    let kib = line.split_whitespace().nth(1)?.parse::<u64>().ok()?;
    kib.checked_mul(1024)
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

fn run_cache_hit(benchmark_executable: &Path, cache_path: &Path) -> Result<Vec<u8>, String> {
    let output = Command::new(benchmark_executable)
        .arg("__worker-hit")
        .arg("--cache-file")
        .arg(cache_path)
        .output()
        .map_err(|error| error.to_string())?;
    if !output.status.success() {
        return Err(format!(
            "cache-hit worker failed with {}: {}",
            output.status,
            String::from_utf8_lossy(&output.stderr)
        ));
    }
    Ok(output.stdout)
}

pub fn run_build_worker(
    db_path: &Path,
    workload_id: &str,
    expected_results: usize,
    cache_path: &Path,
    ready_marker: Option<&Path>,
    publish_delay: Duration,
) -> Result<(), String> {
    let workload = workload_by_id(workload_id)?;
    if let Some(parent) = cache_path.parent() {
        fs::create_dir_all(parent).map_err(|error| error.to_string())?;
    }
    let response = load_workload_response(db_path, workload, expected_results)?;
    let mut payload = serde_json::to_vec(&response).map_err(|error| error.to_string())?;
    payload.push(b'\n');

    let temp_path = cache_temp_path(cache_path, std::process::id());
    remove_if_exists(&temp_path)?;
    let mut file = File::create(&temp_path).map_err(|error| error.to_string())?;
    file.write_all(&payload)
        .map_err(|error| error.to_string())?;
    file.flush().map_err(|error| error.to_string())?;
    drop(file);

    if let Some(marker) = ready_marker {
        fs::write(marker, response.generation.to_string()).map_err(|error| error.to_string())?;
    }
    if !publish_delay.is_zero() {
        thread::sleep(publish_delay);
    }
    fs::rename(&temp_path, cache_path).map_err(|error| error.to_string())?;
    Ok(())
}

pub fn run_hit_worker(cache_path: &Path) -> Result<(), String> {
    let mut file = File::open(cache_path).map_err(|error| error.to_string())?;
    let stdout = io::stdout();
    let mut handle = stdout.lock();
    io::copy(&mut file, &mut handle).map_err(|error| error.to_string())?;
    handle.flush().map_err(|error| error.to_string())
}

fn cache_temp_path(cache_path: &Path, pid: u32) -> PathBuf {
    let file_name = cache_path
        .file_name()
        .and_then(|value| value.to_str())
        .unwrap_or("view-cache");
    cache_path.with_file_name(format!(".{file_name}.tmp-{pid}"))
}

fn payload_generation(payload: &[u8]) -> Result<i64, String> {
    let value: serde_json::Value =
        serde_json::from_slice(payload).map_err(|error| error.to_string())?;
    value
        .get("generation")
        .and_then(serde_json::Value::as_i64)
        .ok_or_else(|| "presentation payload does not contain an integer generation".to_string())
}

fn measure_process_operation<F>(
    options: &PresentationCacheBenchmarkOptions,
    mut operation: F,
) -> Result<Timing, String>
where
    F: FnMut() -> Result<(), String>,
{
    for _ in 0..options.warmups {
        operation()?;
    }
    let mut samples = Vec::with_capacity(options.iterations);
    for _ in 0..options.iterations {
        let start = Instant::now();
        operation()?;
        samples.push(start.elapsed());
    }
    Ok(timing(&mut samples))
}

fn measure_group<F>(iterations: usize, mut operation: F) -> Result<Timing, String>
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

fn workload_file_name(id: &str) -> String {
    id.replace('.', "-")
}

fn absolute_existing_path(path: &Path, label: &str) -> Result<PathBuf, String> {
    if !path.exists() {
        return Err(format!("{label} does not exist: {}", path.display()));
    }
    fs::canonicalize(path).map_err(|error| error.to_string())
}

fn remove_if_exists(path: &Path) -> Result<(), String> {
    match fs::remove_file(path) {
        Ok(()) => Ok(()),
        Err(error) if error.kind() == io::ErrorKind::NotFound => Ok(()),
        Err(error) => Err(error.to_string()),
    }
}

fn max_optional(left: Option<u64>, right: Option<u64>) -> Option<u64> {
    match (left, right) {
        (Some(left), Some(right)) => Some(left.max(right)),
        (Some(value), None) | (None, Some(value)) => Some(value),
        (None, None) => None,
    }
}

fn validate_options(options: &PresentationCacheBenchmarkOptions) -> Result<(), String> {
    if options.row_counts.is_empty() || options.row_counts.contains(&0) {
        return Err("benchmark row counts must be positive".into());
    }
    if options.view_counts.is_empty() || options.view_counts.contains(&0) {
        return Err("benchmark view counts must be positive".into());
    }
    if options.iterations == 0 {
        return Err("benchmark iterations must be positive".into());
    }
    if options.group_iterations == 0 {
        return Err("benchmark group iterations must be positive".into());
    }
    Ok(())
}

struct ProcessMeasurement {
    elapsed: Duration,
    peak_rss_bytes: Option<u64>,
}

struct RunningWorker {
    label: String,
    child: Child,
    done: bool,
}

#[cfg(test)]
mod tests {
    use super::{
        cache_temp_path, max_optional, representative_workloads, validate_options,
        PresentationCacheBenchmarkOptions,
    };
    use std::path::Path;

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
            view_counts: vec![1, 0],
            ..PresentationCacheBenchmarkOptions::default()
        };
        assert!(validate_options(&options).is_err());

        let options = PresentationCacheBenchmarkOptions {
            iterations: 0,
            ..PresentationCacheBenchmarkOptions::default()
        };
        assert!(validate_options(&options).is_err());
    }

    #[test]
    fn cache_temp_path_is_worker_specific() {
        assert_eq!(
            cache_temp_path(Path::new("/tmp/view.json"), 42),
            Path::new("/tmp/.view.json.tmp-42")
        );
    }

    #[test]
    fn optional_max_preserves_available_measurements() {
        assert_eq!(max_optional(Some(5), Some(9)), Some(9));
        assert_eq!(max_optional(Some(5), None), Some(5));
        assert_eq!(max_optional(None, Some(9)), Some(9));
        assert_eq!(max_optional(None, None), None);
    }
}
