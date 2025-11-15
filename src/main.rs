use clap::Parser;
use std::fs::File;
use std::io::Write;

use org_files_db::parser;

/// A simple CLI to parse org-mode files and output them as JSON.
#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
struct Cli {
    /// The org-mode file to parse
    #[arg()]
    file: String,

    /// A comma-separated list of TODO keywords
    #[arg(long)]
    todo: Option<String>, // forwarded directly to parser

    /// A file containing a list of TODO keywords
    #[arg(long)]
    todo_file: Option<String>, // forwarded directly to parser

    /// The output file for the JSON dump
    #[arg(short, long, default_value = "dump.json")]
    output: String,
}

fn main() -> std::io::Result<()> {
    let cli = Cli::parse();

    // make path absolute and readable
    let absolute_path = std::fs::canonicalize(&cli.file).map_err(|e| {
        std::io::Error::new(
            std::io::ErrorKind::NotFound,
            format!("File not found: {}: {}", &cli.file, e),
        )
    })?;

    //
    // 1) DELEGATE EVERYTHNG TO YOUR REAL PARSER API
    //
    let headings = parser::parse_org_from_file(
        absolute_path.to_str().unwrap(),
        cli.todo.as_deref(),      // Option<&str>
        cli.todo_file.as_deref(), // Option<&str>
    )?;

    //
    // 2) Write JSON
    //
    let json = serde_json::to_string_pretty(&headings).map_err(|e| std::io::Error::other(e))?;

    let mut outfile = File::create(&cli.output)?;
    outfile.write_all(json.as_bytes())?;

    println!("✓ Parsed ORG file: {}", &cli.file);
    println!("✓ JSON dump written to {}", &cli.output);

    Ok(())
}
