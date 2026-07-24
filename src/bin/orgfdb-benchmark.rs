use std::path::PathBuf;

fn main() -> Result<(), String> {
    let mut options = org_files_db::benchmark::BenchmarkOptions::default();
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
            "--files" => options.files = value.parse().map_err(|_| "--files must be an integer")?,
            "--seed" => options.seed = value.parse().map_err(|_| "--seed must be an integer")?,
            "--warmups" => {
                options.warmups = value.parse().map_err(|_| "--warmups must be an integer")?
            }
            "--iterations" => {
                options.iterations = value
                    .parse()
                    .map_err(|_| "--iterations must be an integer")?
            }
            _ => return Err(format!("unknown option {argument}")),
        }
    }
    org_files_db::benchmark::run(
        &output.ok_or("--output is required")?,
        &work_dir.ok_or("--work-dir is required")?,
        options,
    )
}
