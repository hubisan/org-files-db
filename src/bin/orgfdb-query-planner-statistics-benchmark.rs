use std::path::PathBuf;

fn main() -> Result<(), String> {
    let mut options =
        org_files_db::query_sql_benchmark::PlannerStatisticsBenchmarkOptions::default();
    let mut output = None;
    let mut work_dir = None;
    let mut args = std::env::args().skip(1);
    while let Some(argument) = args.next() {
        let value = args
            .next()
            .ok_or_else(|| format!("{argument} requires a value"))?;
        match argument.as_str() {
            "--output" => output = Some(PathBuf::from(value)),
            "--work-dir" => work_dir = Some(PathBuf::from(value)),
            "--rows" => options.rows = parse_positive_usize(&value, "--rows")?,
            "--warmups" => {
                options.warmups = value
                    .parse()
                    .map_err(|_| "--warmups must be a non-negative integer")?
            }
            "--iterations" => options.iterations = parse_positive_usize(&value, "--iterations")?,
            "--candidate" => {
                options.candidate =
                    org_files_db::query_sql_benchmark::PlannerStatisticsCandidate::parse(&value)?
            }
            _ => return Err(format!("unknown option {argument}")),
        }
    }

    org_files_db::query_sql_benchmark::run_planner_statistics_experiment(
        &output.ok_or("--output is required")?,
        &work_dir.ok_or("--work-dir is required")?,
        options,
    )
}

fn parse_positive_usize(value: &str, option: &str) -> Result<usize, String> {
    let value = value
        .parse::<usize>()
        .map_err(|_| format!("{option} must be an integer"))?;
    if value == 0 {
        return Err(format!("{option} must be positive"));
    }
    Ok(value)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_positive_usize_accepts_positive_values() {
        assert_eq!(
            parse_positive_usize("50000", "--rows").expect("positive value should parse"),
            50_000
        );
    }

    #[test]
    fn parse_positive_usize_rejects_zero_and_invalid_values() {
        assert!(parse_positive_usize("0", "--rows").is_err());
        assert!(parse_positive_usize("nope", "--rows").is_err());
    }

    #[test]
    fn planner_statistics_candidate_parses_supported_values() {
        use org_files_db::query_sql_benchmark::PlannerStatisticsCandidate;

        assert_eq!(
            PlannerStatisticsCandidate::parse("analyze").expect("analyze candidate should parse"),
            PlannerStatisticsCandidate::FullAnalyze
        );
        assert_eq!(
            PlannerStatisticsCandidate::parse("optimize").expect("optimize candidate should parse"),
            PlannerStatisticsCandidate::Optimize
        );
        assert!(PlannerStatisticsCandidate::parse("unknown").is_err());
    }
}
