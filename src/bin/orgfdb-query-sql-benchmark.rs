use std::path::PathBuf;

fn main() -> Result<(), String> {
    let mut options = org_files_db::query_sql_benchmark::QuerySqlBenchmarkOptions::default();
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
            "--rows" => options.row_counts = parse_row_counts(&value)?,
            "--warmups" => {
                options.warmups = value.parse().map_err(|_| "--warmups must be an integer")?
            }
            "--iterations" => {
                options.iterations = value
                    .parse()
                    .map_err(|_| "--iterations must be an integer")?
            }
            "--path-variable-limits" => {
                options.path_variable_limits = parse_variable_limits(&value)?
            }
            _ => return Err(format!("unknown option {argument}")),
        }
    }

    org_files_db::query_sql_benchmark::run(
        &output.ok_or("--output is required")?,
        &work_dir.ok_or("--work-dir is required")?,
        options,
    )
}

fn parse_row_counts(value: &str) -> Result<Vec<usize>, String> {
    let rows = value
        .split(',')
        .map(|part| {
            part.parse::<usize>()
                .map_err(|_| "--rows must be a comma-separated list of integers".to_string())
        })
        .collect::<Result<Vec<_>, _>>()?;
    if rows.is_empty() || rows.contains(&0) {
        return Err("--rows values must be positive".into());
    }
    Ok(rows)
}

fn parse_variable_limits(value: &str) -> Result<Vec<usize>, String> {
    let limits = value
        .split(',')
        .map(|part| {
            part.parse::<usize>().map_err(|_| {
                "--path-variable-limits must be a comma-separated list of integers".to_string()
            })
        })
        .collect::<Result<Vec<_>, _>>()?;
    if limits.is_empty() || limits.contains(&0) {
        return Err("--path-variable-limits values must be positive".into());
    }
    Ok(limits)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_row_counts_accepts_comma_separated_values() {
        assert_eq!(
            parse_row_counts("100,1000,10000").expect("row counts should parse"),
            vec![100, 1_000, 10_000]
        );
    }

    #[test]
    fn parse_row_counts_rejects_zero_and_invalid_values() {
        assert!(parse_row_counts("100,0").is_err());
        assert!(parse_row_counts("100,nope").is_err());
    }

    #[test]
    fn parse_variable_limits_accepts_comma_separated_values() {
        assert_eq!(
            parse_variable_limits("250000,32766,999").expect("variable limits should parse"),
            vec![250_000, 32_766, 999]
        );
    }

    #[test]
    fn parse_variable_limits_rejects_zero_and_invalid_values() {
        assert!(parse_variable_limits("32766,0").is_err());
        assert!(parse_variable_limits("32766,nope").is_err());
    }
}
