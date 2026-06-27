use std::{
    error::Error,
    fmt,
    io::{self, Write},
    path::{Path, PathBuf},
    process::ExitCode,
};

use clap::{Parser, Subcommand};
use rusqlite::Connection;
use serde::Serialize;

use crate::{
    config::{Config, ConfigError},
    db::{open_existing_database_read_only, DbError, DbReader, HeadingListRow},
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
        #[arg(long)]
        allow_empty: bool,
    },
    Headings {
        #[arg(long)]
        json: bool,
        #[arg(long)]
        no_root: bool,
        #[arg(
            long,
            help = "Deprecated compatibility flag; root rows are included by default"
        )]
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

fn run_with_args<I, T>(args: I) -> Result<(), CliError>
where
    I: IntoIterator<Item = T>,
    T: Into<std::ffi::OsString> + Clone,
{
    let cli = Cli::try_parse_from(args).map_err(CliError::Parse)?;
    match cli.command {
        Command::Rebuild {
            config,
            allow_empty,
        } => {
            let report = rebuild_with_options(&config, allow_empty)?;
            print_diagnostics(&report);
            Ok(())
        }
        Command::Headings {
            json,
            no_root,
            include_root,
            config,
        } => {
            let _deprecated_include_root = include_root;
            let rows = headings_json_rows(json, no_root, config.as_deref())?;
            let stdout = io::stdout();
            let mut handle = stdout.lock();
            serde_json::to_writer_pretty(&mut handle, &rows).map_err(CliError::Json)?;
            handle.write_all(b"\n").map_err(CliError::Io)?;
            Ok(())
        }
    }
}

#[cfg(test)]
fn rebuild(config_path: impl AsRef<std::path::Path>) -> Result<RebuildReport, CliError> {
    rebuild_with_options(config_path, false)
}

fn rebuild_with_options(
    config_path: impl AsRef<std::path::Path>,
    allow_empty: bool,
) -> Result<RebuildReport, CliError> {
    Indexer::new(OrgizeAdapter::new())
        .rebuild_from_config_path_with_options(config_path, allow_empty)
        .map_err(CliError::Indexer)
}

fn headings_json_rows(
    json: bool,
    exclude_root: bool,
    config_path: Option<&Path>,
) -> Result<Vec<HeadingJsonRow>, CliError> {
    if !json {
        return Err(CliError::MissingJsonFlag);
    }

    let connection = open_headings_database(config_path)?;
    headings_rows_for_json(&connection, exclude_root)
}

fn headings_rows_for_json(
    connection: &Connection,
    exclude_root: bool,
) -> Result<Vec<HeadingJsonRow>, CliError> {
    let mut rows = DbReader::list_headings(connection).map_err(CliError::DbRead)?;
    if exclude_root {
        rows.retain(|row| row.level > 0);
    }
    rows.into_iter().map(HeadingJsonRow::try_from).collect()
}

fn open_headings_database(config_path: Option<&std::path::Path>) -> Result<Connection, CliError> {
    let db_path = if let Some(config_path) = config_path {
        Config::load_from_file(config_path)
            .map_err(CliError::Config)?
            .db_path
    } else {
        Config::default().db_path
    };
    open_existing_database_read_only(&db_path).map_err(CliError::Database)
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
enum CliError {
    Parse(clap::Error),
    MissingJsonFlag,
    Config(ConfigError),
    Database(DbError),
    DbRead(crate::db::DbReadError),
    Indexer(IndexerError),
    InvalidHeadingTags {
        heading_id: i64,
        source: serde_json::Error,
    },
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
            | Self::InvalidHeadingTags { .. }
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
            Self::InvalidHeadingTags { heading_id, source } => {
                write!(
                    f,
                    "failed to decode heading tags for heading {}: {}",
                    heading_id, source
                )
            }
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
            Self::InvalidHeadingTags { source, .. } => Some(source),
            Self::Json(source) => Some(source),
            Self::Io(source) => Some(source),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
struct HeadingJsonRow {
    id: i64,
    file_id: i64,
    file_path: String,
    parent_id: Option<i64>,
    level: i64,
    line_number: Option<i64>,
    byte_start: i64,
    byte_end: i64,
    title: String,
    title_raw: String,
    todo_keyword: Option<String>,
    todo_type: Option<String>,
    priority: Option<char>,
    scheduled_raw: Option<String>,
    scheduled_ts: Option<i64>,
    deadline_raw: Option<String>,
    deadline_ts: Option<i64>,
    closed_raw: Option<String>,
    closed_ts: Option<i64>,
    archivedp: bool,
    footnote_section_p: bool,
    all_tags: Vec<String>,
}

impl TryFrom<HeadingListRow> for HeadingJsonRow {
    type Error = CliError;

    fn try_from(row: HeadingListRow) -> Result<Self, Self::Error> {
        let all_tags = serde_json::from_str(&row.all_tags_json).map_err(|source| {
            CliError::InvalidHeadingTags {
                heading_id: row.id,
                source,
            }
        })?;

        Ok(Self {
            id: row.id,
            file_id: row.file_id,
            file_path: row.file_path,
            parent_id: row.parent_id,
            level: row.level,
            line_number: row.line_number,
            byte_start: row.byte_start,
            byte_end: row.byte_end,
            title: row.title,
            title_raw: row.title_raw,
            todo_keyword: row.todo_keyword,
            todo_type: row.todo_type,
            priority: row.priority,
            scheduled_raw: row.scheduled_raw,
            scheduled_ts: row.scheduled_ts,
            deadline_raw: row.deadline_raw,
            deadline_ts: row.deadline_ts,
            closed_raw: row.closed_raw,
            closed_ts: row.closed_ts,
            archivedp: row.archivedp,
            footnote_section_p: row.footnote_section_p,
            all_tags,
        })
    }
}

#[cfg(test)]
mod tests {
    use super::{rebuild, Cli, CliError};
    use crate::db::{
        open_database, open_in_memory_database_with_schema, DbError, DbWriter, FileRecordInput,
        HeadingRecord, SchemaDefinition, CURRENT_SCHEMA_VERSION,
    };
    use clap::Parser;
    use rusqlite::Connection;
    use serde_json::Value;
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
            super::Command::Rebuild {
                config,
                allow_empty,
            } => {
                assert_eq!(config, PathBuf::from("config.toml"));
                assert!(!allow_empty);
            }
            other => panic!("unexpected command: {other:?}"),
        }

        let cli = Cli::try_parse_from([
            "orgfdb",
            "rebuild",
            "--config",
            "config.toml",
            "--allow-empty",
        ])
        .expect("rebuild allow-empty args should parse");

        match cli.command {
            super::Command::Rebuild {
                config,
                allow_empty,
            } => {
                assert_eq!(config, PathBuf::from("config.toml"));
                assert!(allow_empty);
            }
            other => panic!("unexpected command: {other:?}"),
        }

        let cli = Cli::try_parse_from(["orgfdb", "headings", "--json"])
            .expect("headings args should parse");

        match cli.command {
            super::Command::Headings {
                json,
                no_root,
                include_root,
                ..
            } => {
                assert!(json);
                assert!(!no_root);
                assert!(!include_root);
            }
            other => panic!("unexpected command: {other:?}"),
        }

        let cli = Cli::try_parse_from(["orgfdb", "headings", "--json", "--include-root"])
            .expect("headings include-root args should parse");

        match cli.command {
            super::Command::Headings {
                json,
                no_root,
                include_root,
                ..
            } => {
                assert!(json);
                assert!(!no_root);
                assert!(include_root);
            }
            other => panic!("unexpected command: {other:?}"),
        }

        let cli = Cli::try_parse_from(["orgfdb", "headings", "--json", "--no-root"])
            .expect("headings no-root args should parse");

        match cli.command {
            super::Command::Headings {
                json,
                no_root,
                include_root,
                ..
            } => {
                assert!(json);
                assert!(no_root);
                assert!(!include_root);
            }
            other => panic!("unexpected command: {other:?}"),
        }

        let cli = Cli::try_parse_from(["orgfdb", "headings", "--json", "--config", "config.toml"])
            .expect("headings config args should parse");

        match cli.command {
            super::Command::Headings {
                json,
                no_root,
                include_root,
                config,
            } => {
                assert!(json);
                assert!(!no_root);
                assert!(!include_root);
                assert_eq!(config, Some(PathBuf::from("config.toml")));
            }
            other => panic!("unexpected command: {other:?}"),
        }
    }

    #[test]
    fn headings_json_includes_level_zero_rows_by_default() {
        let schema = SchemaDefinition::new(CURRENT_SCHEMA_VERSION, false);
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
        assert_eq!(rows.len(), 2);
        assert_eq!(rows[0].level, 0);
        assert_eq!(rows[0].all_tags, Vec::<String>::new());
        assert_eq!(rows[1].level, 1);
        assert_eq!(rows[1].all_tags, vec!["rust".to_string()]);

        let json = serde_json::to_value(&rows).expect("rows should serialize");
        let array = json.as_array().expect("rows should serialize as an array");
        assert_eq!(array.len(), 2);
        assert_eq!(array[0]["level"], 0);
        assert_eq!(array[0]["all_tags"], Value::Array(vec![]));
        assert_eq!(sorted_object_keys(&array[0]), expected_heading_json_keys());
        assert_eq!(array[1]["level"], 1);
        assert_eq!(
            array[1]["all_tags"],
            Value::Array(vec![Value::String("rust".to_string())])
        );
        assert!(array[1].get("all_tags_json").is_none());
        assert_eq!(sorted_object_keys(&array[1]), expected_heading_json_keys());
    }

    #[test]
    fn headings_json_excludes_level_zero_rows_with_no_root() {
        let schema = SchemaDefinition::new(CURRENT_SCHEMA_VERSION, false);
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
        assert_eq!(rows.len(), 1);
        assert_eq!(rows[0].level, 1);
        assert_eq!(rows[0].all_tags, vec!["rust".to_string()]);
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

        drop(configured_db);

        let rows = super::headings_json_rows(true, false, Some(&config_path))
            .expect("rows should load from configured db");

        assert_eq!(rows.len(), 2);
        assert_eq!(rows[0].level, 0);
        assert_eq!(rows[1].title, "Heading");
    }

    #[test]
    fn rebuild_and_headings_json_cover_minimal_end_to_end_slice() {
        let test_dir = TestDir::new("minimal-end-to-end");
        let config_path = test_dir.path().join("config.toml");
        let db_path = test_dir.path().join("db.sqlite");
        let org_path = test_dir.path().join("notes.org");

        write_file(
            &org_path,
            "#+TITLE: Minimal Slice\n#+TODO: PLAN(p) | DONE(d)\n* PLAN Inbox\nSCHEDULED: <2024-11-20 Wed 09:15>\n",
        );
        write_file(
            &config_path,
            r#"
db_path = "./db.sqlite"
files = ["./notes.org"]

[search]
fts5_enabled = false
index_body_text = false
"#,
        );

        let report = rebuild(&config_path).expect("rebuild should succeed");
        assert_eq!(report.indexed_files.len(), 1);
        assert_eq!(report.indexed_files[0].path, org_path);

        let json_rows = super::headings_json_rows(true, false, Some(&config_path))
            .expect("json rows should load");
        assert_eq!(json_rows.len(), 2);
        assert_eq!(json_rows[0].level, 0);
        assert_eq!(json_rows[1].level, 1);
        assert_eq!(json_rows[1].title, "Inbox");
        assert_eq!(json_rows[1].todo_keyword.as_deref(), Some("PLAN"));
        assert_eq!(json_rows[1].todo_type.as_deref(), Some("open"));
        assert_eq!(json_rows[1].file_path, org_path.display().to_string());
        assert_eq!(
            json_rows[1].scheduled_raw.as_deref(),
            Some("<2024-11-20 Wed 09:15>")
        );
        assert_eq!(json_rows[1].scheduled_ts, Some(1_732_094_100));
        assert!(json_rows[1].deadline_raw.is_none());
        assert!(json_rows[1].closed_raw.is_none());
        assert!(json_rows[1].all_tags.is_empty());

        let excluded_rows = super::headings_json_rows(true, true, Some(&config_path))
            .expect("excluded rows should load");
        assert_eq!(excluded_rows.len(), 1);
        assert_eq!(excluded_rows[0].level, 1);
        assert_eq!(excluded_rows[0].title, "Inbox");

        let include_root_rows = super::headings_json_rows(true, false, Some(&config_path))
            .expect("included rows should load");
        assert_eq!(include_root_rows.len(), 2);
        assert_eq!(include_root_rows[0].level, 0);
        assert_eq!(include_root_rows[0].title, "Minimal Slice");
        assert_eq!(include_root_rows[0].title_raw, "Minimal Slice");
        assert!(include_root_rows[0].scheduled_raw.is_none());
        assert!(include_root_rows[0].all_tags.is_empty());
        assert_eq!(include_root_rows[1].level, 1);
        assert_eq!(include_root_rows[1].title, "Inbox");

        let connection = open_database(&db_path).expect("database should open");
        let heading_count: i64 = connection
            .query_row("SELECT COUNT(*) FROM headings", [], |row| row.get(0))
            .expect("heading count should load");
        assert_eq!(heading_count, 2);
    }

    #[test]
    fn headings_json_read_only_open_does_not_create_missing_database() {
        let test_dir = TestDir::new("headings-missing-db");
        let config_path = test_dir.path().join("config.toml");
        let db_path = test_dir.path().join("missing.sqlite");

        write_file(
            &config_path,
            r#"
db_path = "./missing.sqlite"
"#,
        );

        let error = super::headings_json_rows(true, false, Some(&config_path))
            .expect_err("missing database should fail");
        assert!(
            matches!(error, CliError::Database(DbError::Open { .. })),
            "expected read-only open error, got {error}"
        );
        assert!(
            !db_path.exists(),
            "read-only headings should not create a database"
        );
    }

    #[test]
    fn headings_json_read_only_open_leaves_current_database_unchanged() {
        let test_dir = TestDir::new("headings-read-only-current");
        let config_path = test_dir.path().join("config.toml");
        let db_path = test_dir.path().join("db.sqlite");
        let org_path = test_dir.path().join("notes.org");

        write_file(
            &config_path,
            r#"
db_path = "./db.sqlite"
"#,
        );

        let mut connection = open_database(&db_path).expect("database should open");
        DbWriter::rebuild_file(
            &mut connection,
            &FileRecordInput {
                path: org_path.clone(),
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
                        title: org_path.display().to_string(),
                        title_raw: org_path.display().to_string(),
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
                        line_number: Some(1),
                        byte_start: 0,
                        byte_end: 9,
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
                        all_tags_json: "[\"tagged\"]".to_string(),
                    }],
                )?;
                Ok(())
            },
        )
        .expect("configured db should be populated");

        let version_before: u32 = connection
            .pragma_query_value(None, "user_version", |row| row.get(0))
            .expect("user_version should load");
        let heading_fts_before: i64 = connection
            .query_row(
                "SELECT COUNT(*) FROM sqlite_master WHERE type = 'table' AND name = 'heading_fts'",
                [],
                |row| row.get(0),
            )
            .expect("heading_fts existence should load");
        drop(connection);

        let rows = super::headings_json_rows(true, false, Some(&config_path))
            .expect("rows should load from existing database");
        assert_eq!(rows.len(), 2);
        assert_eq!(rows[0].level, 0);
        assert_eq!(rows[1].all_tags, vec!["tagged".to_string()]);

        let reopened = Connection::open(&db_path).expect("database should reopen");
        let version_after: u32 = reopened
            .pragma_query_value(None, "user_version", |row| row.get(0))
            .expect("user_version should load after read-only query");
        let heading_fts_after: i64 = reopened
            .query_row(
                "SELECT COUNT(*) FROM sqlite_master WHERE type = 'table' AND name = 'heading_fts'",
                [],
                |row| row.get(0),
            )
            .expect("heading_fts existence should load after read-only query");

        assert_eq!(version_before, CURRENT_SCHEMA_VERSION);
        assert_eq!(version_after, version_before);
        assert_eq!(heading_fts_after, heading_fts_before);
    }

    #[test]
    fn headings_json_read_only_open_rejects_future_schema_versions() {
        let test_dir = TestDir::new("headings-future-db");
        let config_path = test_dir.path().join("config.toml");
        let db_path = test_dir.path().join("future.sqlite");

        write_file(
            &config_path,
            r#"
db_path = "./future.sqlite"
"#,
        );

        let connection = Connection::open(&db_path).expect("future database should open");
        connection
            .pragma_update(None, "user_version", i64::from(CURRENT_SCHEMA_VERSION + 1))
            .expect("future user_version should seed");
        drop(connection);

        let error = super::headings_json_rows(true, false, Some(&config_path))
            .expect_err("future schema version should fail closed");
        match error {
            CliError::Database(DbError::UnsupportedFutureSchemaVersion {
                on_disk_version,
                supported_version,
                ..
            }) => {
                assert_eq!(on_disk_version, CURRENT_SCHEMA_VERSION + 1);
                assert_eq!(supported_version, CURRENT_SCHEMA_VERSION);
            }
            other => panic!("expected UnsupportedFutureSchemaVersion, got {other}"),
        }

        let reopened = Connection::open(&db_path).expect("future database should reopen");
        let version_after: u32 = reopened
            .pragma_query_value(None, "user_version", |row| row.get(0))
            .expect("future schema version should remain unchanged");
        assert_eq!(version_after, CURRENT_SCHEMA_VERSION + 1);
    }

    #[test]
    fn rebuild_helper_propagates_indexer_errors() {
        let error = rebuild(Path::new("missing-config.toml")).expect_err("rebuild should fail");

        assert!(matches!(error, CliError::Indexer(_)));
    }

    fn sorted_object_keys(value: &Value) -> Vec<String> {
        let mut keys = value
            .as_object()
            .expect("JSON value should be an object")
            .keys()
            .cloned()
            .collect::<Vec<_>>();
        keys.sort();
        keys
    }

    fn expected_heading_json_keys() -> Vec<String> {
        vec![
            "all_tags",
            "archivedp",
            "byte_end",
            "byte_start",
            "closed_raw",
            "closed_ts",
            "deadline_raw",
            "deadline_ts",
            "file_id",
            "file_path",
            "footnote_section_p",
            "id",
            "level",
            "line_number",
            "parent_id",
            "priority",
            "scheduled_raw",
            "scheduled_ts",
            "title",
            "title_raw",
            "todo_keyword",
            "todo_type",
        ]
        .into_iter()
        .map(str::to_string)
        .collect()
    }
}
