mod config;
mod parser;
mod types;

use clap::Parser;
use std::fs::File;
use std::io::Write;

/// A simple CLI to parse org-mode files and output them as JSON.
#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
struct Cli {
    /// The org-mode file to parse
    #[arg()]
    file: String,

    /// A comma-separated list of TODO keywords
    #[arg(long)]
    todo: Option<String>,

    /// A file containing a list of TODO keywords
    #[arg(long)]
    todo_file: Option<String>,

    /// The output file for the JSON dump
    #[arg(short, long, default_value = "dump.json")]
    output: String,
}

fn main() -> std::io::Result<()> {
    let cli = Cli::parse();

    // 1. Parsen
    let absolute_path = std::fs::canonicalize(&cli.file)
        .map_err(|e| std::io::Error::new(std::io::ErrorKind::NotFound, format!("File not found: {}: {}", &cli.file, e)))?;
    let headings = parser::parse_org_from_file(
        absolute_path.to_str().unwrap(),
        cli.todo.as_deref(),
        cli.todo_file.as_deref(),
    )?;

    // 2. JSON erzeugen
    let json = serde_json::to_string_pretty(&headings)
        .map_err(|e| std::io::Error::new(std::io::ErrorKind::Other, e))?;

    // 3. Dump schreiben
    let mut outfile = File::create(&cli.output)?;
    outfile.write_all(json.as_bytes())?;

    println!("✓ Parsed ORG file: {}", &cli.file);
    println!("✓ JSON dump written to {}", &cli.output);

    Ok(())
}

