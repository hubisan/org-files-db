use std::{path::PathBuf, time::Duration};

use org_files_db::presentation_cache_benchmark::PresentationCacheBenchmarkOptions;

fn main() -> Result<(), String> {
    let arguments = std::env::args().skip(1).collect::<Vec<_>>();
    match arguments.first().map(String::as_str) {
        Some("__worker-build") => run_build_worker(&arguments[1..]),
        Some("__worker-hit") => run_hit_worker(&arguments[1..]),
        _ => run_parent(&arguments),
    }
}

fn run_parent(arguments: &[String]) -> Result<(), String> {
    let mut options = PresentationCacheBenchmarkOptions::default();
    let mut output = None;
    let mut work_dir = None;
    let mut orgfdb = None;
    let mut index = 0;
    while index < arguments.len() {
        let argument = &arguments[index];
        let value = arguments
            .get(index + 1)
            .ok_or_else(|| format!("{argument} requires a value"))?;
        match argument.as_str() {
            "--output" => output = Some(PathBuf::from(value)),
            "--work-dir" => work_dir = Some(PathBuf::from(value)),
            "--orgfdb" => orgfdb = Some(PathBuf::from(value)),
            "--rows" => options.row_counts = parse_usize_list(value, "--rows")?,
            "--view-counts" => options.view_counts = parse_usize_list(value, "--view-counts")?,
            "--warmups" => {
                options.warmups = value.parse().map_err(|_| "--warmups must be an integer")?
            }
            "--iterations" => {
                options.iterations = value
                    .parse()
                    .map_err(|_| "--iterations must be an integer")?
            }
            "--group-iterations" => {
                options.group_iterations = value
                    .parse()
                    .map_err(|_| "--group-iterations must be an integer")?
            }
            _ => return Err(format!("unknown option {argument}")),
        }
        index += 2;
    }

    let benchmark_executable = std::env::current_exe().map_err(|error| error.to_string())?;
    org_files_db::presentation_cache_benchmark::run(
        &output.ok_or("--output is required")?,
        &work_dir.ok_or("--work-dir is required")?,
        &orgfdb.ok_or("--orgfdb is required")?,
        &benchmark_executable,
        options,
    )
}

fn run_build_worker(arguments: &[String]) -> Result<(), String> {
    let mut db = None;
    let mut workload = None;
    let mut expected_results = None;
    let mut cache_file = None;
    let mut ready_marker = None;
    let mut publish_delay_ms = 0u64;
    let mut index = 0;
    while index < arguments.len() {
        let argument = &arguments[index];
        let value = arguments
            .get(index + 1)
            .ok_or_else(|| format!("{argument} requires a value"))?;
        match argument.as_str() {
            "--db" => db = Some(PathBuf::from(value)),
            "--workload" => workload = Some(value.clone()),
            "--expected-results" => {
                expected_results = Some(
                    value
                        .parse::<usize>()
                        .map_err(|_| "--expected-results must be an integer")?,
                )
            }
            "--cache-file" => cache_file = Some(PathBuf::from(value)),
            "--ready-marker" => ready_marker = Some(PathBuf::from(value)),
            "--publish-delay-ms" => {
                publish_delay_ms = value
                    .parse()
                    .map_err(|_| "--publish-delay-ms must be an integer")?
            }
            _ => return Err(format!("unknown worker option {argument}")),
        }
        index += 2;
    }

    org_files_db::presentation_cache_benchmark::run_build_worker(
        &db.ok_or("--db is required")?,
        &workload.ok_or("--workload is required")?,
        expected_results.ok_or("--expected-results is required")?,
        &cache_file.ok_or("--cache-file is required")?,
        ready_marker.as_deref(),
        Duration::from_millis(publish_delay_ms),
    )
}

fn run_hit_worker(arguments: &[String]) -> Result<(), String> {
    if arguments.len() != 2 || arguments[0] != "--cache-file" {
        return Err("__worker-hit requires --cache-file PATH".into());
    }
    org_files_db::presentation_cache_benchmark::run_hit_worker(&PathBuf::from(
        arguments[1].as_str(),
    ))
}

fn parse_usize_list(value: &str, option: &str) -> Result<Vec<usize>, String> {
    let values = value
        .split(',')
        .map(|part| {
            part.parse::<usize>()
                .map_err(|_| format!("{option} must be a comma-separated list of integers"))
        })
        .collect::<Result<Vec<_>, _>>()?;
    if values.is_empty() || values.contains(&0) {
        return Err(format!("{option} values must be positive"));
    }
    Ok(values)
}

#[cfg(test)]
mod tests {
    use super::parse_usize_list;

    #[test]
    fn parse_usize_list_accepts_positive_values() {
        assert_eq!(
            parse_usize_list("1,3,5,10", "--view-counts").expect("view counts should parse"),
            vec![1, 3, 5, 10]
        );
    }

    #[test]
    fn parse_usize_list_rejects_zero_and_invalid_values() {
        assert!(parse_usize_list("1,0", "--view-counts").is_err());
        assert!(parse_usize_list("1,nope", "--view-counts").is_err());
    }
}
