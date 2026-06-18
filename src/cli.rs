use std::{
    error::Error,
    fmt,
    io::{self, Write},
    path::PathBuf,
    process::ExitCode,
};

use clap::{Parser, Subcommand};
use rusqlite::Connection;

use crate::{
    config::Config,
    db::{open_database, DbError, DbReader, HeadingListRow},
    indexer::{Indexer, IndexerError, RebuildReport},
    parser::OrgizeAdapter,
};

#[derive(Debug, Parser)]
#[command(name = "orgfdb", version, about = "Minimal Org files database CLI")]
struct Cli {
    #[command(subcommand)]
    command: Command,
}

#[derive(Debug, Subcommand)]
enum Command {
    Rebuild {
        #[arg(long)]
        config: PathBuf,
    },
    Headings {
        #[arg(long)]
        json: bool,
    },
}

pub fn run() -> ExitCode {
    match run_with_args(std::env::args_os()) {
        Ok(()) => ExitCode::SUCCESS,
        Err(error) => {
            eprintln!("{error}");
            ExitCode::from(error.exit_code())
        }
    }
}

pub fn run_with_args<I, T>(args: I) -> Result<(), CliError>
where
    I: IntoIterator<Item = T>,
    T: Into<std::ffi::OsString> + Clone,
{
    let cli = Cli::try_parse_from(args).map_err(CliError::Parse)?;
    match cli.command {
        Command::Rebuild { config } => {
            let report = rebuild(&config)?;
            print_diagnostics(&report);
            Ok(())
        }
        Command::Headings { json } => {
            let rows = headings_json_rows(json)?;
            let stdout = io::stdout();
            let mut handle = stdout.lock();
            serde_json::to_writer_pretty(&mut handle, &rows).map_err(CliError::Json)?;
            handle.write_all(b"\n").map_err(CliError::Io)?;
            Ok(())
        }
    }
}

pub fn rebuild(config_path: impl AsRef<std::path::Path>) -> Result<RebuildReport, CliError> {
    Indexer::new(OrgizeAdapter::new())
        .rebuild_from_config_path(config_path)
        .map_err(CliError::Indexer)
}

fn headings_json_rows(json: bool) -> Result<Vec<HeadingListRow>, CliError> {
    if !json {
        return Err(CliError::MissingJsonFlag);
    }

    let connection = open_headings_database()?;
    headings_rows_for_json(&connection)
}

fn headings_rows_for_json(connection: &Connection) -> Result<Vec<HeadingListRow>, CliError> {
    // Keep CLI JSON focused on user-authored headings; the synthetic level 0 file row
    // stays available in the DB for rebuild and outline bookkeeping.
    let mut rows = DbReader::list_headings(connection).map_err(CliError::DbRead)?;
    rows.retain(|row| row.level > 0);
    Ok(rows)
}

fn open_headings_database() -> Result<Connection, CliError> {
    let db_path = Config::default().db_path;
    open_database(&db_path).map_err(CliError::Database)
}

fn print_diagnostics(report: &RebuildReport) {
    for diagnostic in &report.diagnostics {
        let label = match diagnostic.severity {
            crate::parser::DiagnosticSeverity::Warning => "warning",
            crate::parser::DiagnosticSeverity::Error => "error",
        };
        match (&diagnostic.file_path, diagnostic.line_number) {
            (Some(path), Some(line)) => {
                eprintln!("{label}: {}:{line}: {}", path.display(), diagnostic.message);
            }
            (Some(path), None) => {
                eprintln!("{label}: {}: {}", path.display(), diagnostic.message);
            }
            (None, _) => {
                eprintln!("{label}: {}", diagnostic.message);
            }
        }
    }
}

#[derive(Debug)]
pub enum CliError {
    Parse(clap::Error),
    MissingJsonFlag,
    Database(DbError),
    DbRead(crate::db::DbReadError),
    Indexer(IndexerError),
    Json(serde_json::Error),
    Io(io::Error),
}

impl CliError {
    fn exit_code(&self) -> u8 {
        match self {
            Self::Parse(_) | Self::MissingJsonFlag => 2,
            Self::Database(_)
            | Self::DbRead(_)
            | Self::Indexer(_)
            | Self::Json(_)
            | Self::Io(_) => 1,
        }
    }
}

impl fmt::Display for CliError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Parse(error) => write!(f, "{error}"),
            Self::MissingJsonFlag => {
                write!(f, "headings currently only supports --json")
            }
            Self::Database(source) => write!(f, "{source}"),
            Self::DbRead(source) => write!(f, "{source}"),
            Self::Indexer(source) => write!(f, "{source}"),
            Self::Json(source) => write!(f, "failed to render JSON output: {source}"),
            Self::Io(source) => write!(f, "failed to write CLI output: {source}"),
        }
    }
}

impl Error for CliError {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        match self {
            Self::Parse(error) => Some(error),
            Self::MissingJsonFlag => None,
            Self::Database(source) => Some(source),
            Self::DbRead(source) => Some(source),
            Self::Indexer(source) => Some(source),
            Self::Json(source) => Some(source),
            Self::Io(source) => Some(source),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::{rebuild, Cli, CliError};
    use crate::db::{
        open_in_memory_database_with_schema, DbWriter, FileRecordInput, HeadingRecord,
        SchemaDefinition,
    };
    use clap::Parser;
    use std::path::{Path, PathBuf};

    #[test]
    fn parses_rebuild_and_headings_arguments() {
        let cli = Cli::try_parse_from(["orgfdb", "rebuild", "--config", "config.toml"])
            .expect("rebuild args should parse");

        match cli.command {
            super::Command::Rebuild { config } => {
                assert_eq!(config, PathBuf::from("config.toml"));
            }
            other => panic!("unexpected command: {other:?}"),
        }

        let cli = Cli::try_parse_from(["orgfdb", "headings", "--json"])
            .expect("headings args should parse");

        match cli.command {
            super::Command::Headings { json } => assert!(json),
            other => panic!("unexpected command: {other:?}"),
        }
    }

    #[test]
    fn headings_json_excludes_level_zero_rows_by_default() {
        let schema = SchemaDefinition::new(1, false);
        let mut connection =
            open_in_memory_database_with_schema(&schema).expect("database should open");
        let file = FileRecordInput {
            path: PathBuf::from("/tmp/project.org"),
            mtime_ns: 10,
            size: 100,
            content_hash: None,
            indexed_at: None,
        };

        DbWriter::rebuild_file(&mut connection, &file, |tx, file_id| {
            let level0_id = DbWriter::insert_level0_heading(
                tx,
                &HeadingRecord {
                    id: None,
                    file_id,
                    parent_id: None,
                    level: 0,
                    line_number: None,
                    byte_start: -1,
                    byte_end: 100,
                    title: "/tmp/project.org".to_string(),
                    title_raw: "/tmp/project.org".to_string(),
                    todo_keyword: None,
                    todo_type: None,
                    priority: None,
                    scheduled_raw: None,
                    scheduled_ts: None,
                    deadline_raw: None,
                    deadline_ts: None,
                    closed_raw: None,
                    closed_ts: None,
                    archivedp: false,
                    footnote_section_p: false,
                    all_tags_json: "[]".to_string(),
                },
            )?;
            DbWriter::insert_headings(
                tx,
                &[HeadingRecord {
                    id: None,
                    file_id,
                    parent_id: Some(level0_id),
                    level: 1,
                    line_number: Some(2),
                    byte_start: 10,
                    byte_end: 25,
                    title: "Inbox".to_string(),
                    title_raw: "Inbox".to_string(),
                    todo_keyword: Some("TODO".to_string()),
                    todo_type: Some("open".to_string()),
                    priority: Some('A'),
                    scheduled_raw: None,
                    scheduled_ts: None,
                    deadline_raw: None,
                    deadline_ts: None,
                    closed_raw: None,
                    closed_ts: None,
                    archivedp: false,
                    footnote_section_p: false,
                    all_tags_json: "[\"rust\"]".to_string(),
                }],
            )?;
            Ok(())
        })
        .expect("rebuild should succeed");

        let rows = super::headings_rows_for_json(&connection).expect("rows should load");
        assert_eq!(rows.len(), 1);
        assert_eq!(rows[0].level, 1);

        let json = serde_json::to_value(&rows).expect("rows should serialize");
        let array = json.as_array().expect("rows should serialize as an array");
        assert_eq!(array.len(), 1);
        assert_eq!(array[0]["level"], 1);
    }

    #[test]
    fn rebuild_helper_propagates_indexer_errors() {
        let error = rebuild(Path::new("missing-config.toml")).expect_err("rebuild should fail");

        assert!(matches!(error, CliError::Indexer(_)));
    }
}
