use std::{
    error::Error,
    fmt,
    io::{self, Write},
    path::{Path, PathBuf},
    process::ExitCode,
};

use clap::{Parser, Subcommand};
use rusqlite::Connection;

use crate::{
    config::{Config, ConfigError},
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
        #[arg(long)]
        include_root: bool,
        #[arg(long)]
        config: Option<PathBuf>,
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
        Command::Headings {
            json,
            include_root,
            config,
        } => {
            let rows = headings_json_rows(json, include_root, config.as_deref())?;
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

fn headings_json_rows(
    json: bool,
    include_root: bool,
    config_path: Option<&Path>,
) -> Result<Vec<HeadingListRow>, CliError> {
    if !json {
        return Err(CliError::MissingJsonFlag);
    }

    let connection = open_headings_database(config_path)?;
    headings_rows_for_json(&connection, include_root)
}

fn headings_rows_for_json(
    connection: &Connection,
    include_root: bool,
) -> Result<Vec<HeadingListRow>, CliError> {
    // Keep CLI JSON focused on user-authored headings; the synthetic level 0 file row
    // stays available in the DB for rebuild and outline bookkeeping.
    let mut rows = DbReader::list_headings(connection).map_err(CliError::DbRead)?;
    if !include_root {
        rows.retain(|row| row.level > 0);
    }
    Ok(rows)
}

fn open_headings_database(config_path: Option<&std::path::Path>) -> Result<Connection, CliError> {
    let db_path = if let Some(config_path) = config_path {
        Config::load_from_file(config_path)
            .map_err(CliError::Config)?
            .db_path
    } else {
        Config::default().db_path
    };
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
    Config(ConfigError),
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
            Self::Config(_)
            | Self::Database(_)
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
            Self::Config(source) => write!(f, "{source}"),
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
            Self::Config(source) => Some(source),
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
        open_database, open_in_memory_database_with_schema, DbWriter, FileRecordInput,
        HeadingRecord, SchemaDefinition,
    };
    use clap::Parser;
    use std::{
        fs,
        path::{Path, PathBuf},
        time::{SystemTime, UNIX_EPOCH},
    };

    struct TestDir {
        path: PathBuf,
    }

    impl TestDir {
        fn new(name: &str) -> Self {
            let unique = SystemTime::now()
                .duration_since(UNIX_EPOCH)
                .expect("system time should be after unix epoch")
                .as_nanos();
            let path = std::env::temp_dir().join(format!(
                "org-files-db-cli-tests-{}-{}-{}",
                name,
                std::process::id(),
                unique
            ));
            fs::create_dir_all(&path).expect("test dir should be created");
            Self { path }
        }

        fn path(&self) -> &Path {
            &self.path
        }
    }

    impl Drop for TestDir {
        fn drop(&mut self) {
            let _ = fs::remove_dir_all(&self.path);
        }
    }

    fn write_file(path: &Path, content: &str) {
        if let Some(parent) = path.parent() {
            fs::create_dir_all(parent).expect("parent dir should be created");
        }
        fs::write(path, content).expect("file should be written");
    }

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
            super::Command::Headings {
                json, include_root, ..
            } => {
                assert!(json);
                assert!(!include_root);
            }
            other => panic!("unexpected command: {other:?}"),
        }

        let cli = Cli::try_parse_from(["orgfdb", "headings", "--json", "--include-root"])
            .expect("headings include-root args should parse");

        match cli.command {
            super::Command::Headings {
                json, include_root, ..
            } => {
                assert!(json);
                assert!(include_root);
            }
            other => panic!("unexpected command: {other:?}"),
        }

        let cli = Cli::try_parse_from(["orgfdb", "headings", "--json", "--config", "config.toml"])
            .expect("headings config args should parse");

        match cli.command {
            super::Command::Headings {
                json,
                include_root,
                config,
            } => {
                assert!(json);
                assert!(!include_root);
                assert_eq!(config, Some(PathBuf::from("config.toml")));
            }
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

        let rows = super::headings_rows_for_json(&connection, false).expect("rows should load");
        assert_eq!(rows.len(), 1);
        assert_eq!(rows[0].level, 1);

        let json = serde_json::to_value(&rows).expect("rows should serialize");
        let array = json.as_array().expect("rows should serialize as an array");
        assert_eq!(array.len(), 1);
        assert_eq!(array[0]["level"], 1);
    }

    #[test]
    fn headings_json_can_include_level_zero_rows() {
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

        let rows = super::headings_rows_for_json(&connection, true).expect("rows should load");
        assert_eq!(rows.len(), 2);
        assert_eq!(rows[0].level, 0);
        assert_eq!(rows[1].level, 1);
    }

    #[test]
    fn headings_uses_configured_db_path_when_config_is_provided() {
        let test_dir = TestDir::new("headings-config");
        let config_dir = test_dir.path().join("nested/config");
        let db_path = config_dir.join("../db.sqlite");
        let config_path = config_dir.join("config.toml");
        let file_path = config_dir.join("notes.org");

        write_file(
            &config_path,
            r#"
db_path = "../db.sqlite"
"#,
        );
        write_file(&file_path, "* Heading\n");

        let mut configured_db = open_database(&db_path).expect("configured database should open");
        DbWriter::rebuild_file(
            &mut configured_db,
            &FileRecordInput {
                path: file_path.clone(),
                mtime_ns: 10,
                size: 100,
                content_hash: None,
                indexed_at: None,
            },
            |tx, file_id| {
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
                        title: file_path.display().to_string(),
                        title_raw: file_path.display().to_string(),
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
                        byte_end: 20,
                        title: "Heading".to_string(),
                        title_raw: "Heading".to_string(),
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
                    }],
                )?;
                Ok(())
            },
        )
        .expect("configured db should be populated");

        let rows = super::headings_rows_for_json(&configured_db, false)
            .expect("rows should load from configured db");

        assert_eq!(rows.len(), 1);
        assert_eq!(rows[0].title, "Heading");
    }

    #[test]
    fn rebuild_helper_propagates_indexer_errors() {
        let error = rebuild(Path::new("missing-config.toml")).expect_err("rebuild should fail");

        assert!(matches!(error, CliError::Indexer(_)));
    }
}
