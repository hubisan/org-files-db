use std::{
    error::Error,
    fmt,
    io::{self, Write},
    path::{Path, PathBuf},
    process::ExitCode,
};

use clap::{Parser, Subcommand, ValueEnum};
use rusqlite::Connection;
use serde::Serialize;

use crate::{
    config::{Config, ConfigError},
    db::{open_existing_database_read_only, DbError, DbReader, HeadingListRow, LinkListRow},
    indexer::{Indexer, IndexerError, RebuildReport},
    parser::OrgizeAdapter,
    query::{
        execute_and_shape_query, parse_query, validate_query, QueryExecutionOptions, QueryInclude,
        QueryOutputMode, QueryParseError, QueryResponse, QueryShapeError, QueryValidationError,
        QueryValidationOptions,
    },
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
    Links {
        #[arg(long)]
        json: bool,
        #[arg(long)]
        config: Option<PathBuf>,
    },
    Query {
        #[arg(long)]
        json: bool,
        #[arg(long, value_enum, default_value_t = CliQueryOutput::Flat)]
        output: CliQueryOutput,
        #[arg(long, value_enum, value_delimiter = ',')]
        include: Vec<CliQueryInclude>,
        #[arg(long)]
        config: Option<PathBuf>,
        #[arg(help = "Query Model v0 expression, for example '(todo \"NEXT\")'")]
        query: String,
    },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, ValueEnum)]
enum CliQueryOutput {
    Flat,
    Outline,
}

impl From<CliQueryOutput> for QueryOutputMode {
    fn from(value: CliQueryOutput) -> Self {
        match value {
            CliQueryOutput::Flat => Self::Flat,
            CliQueryOutput::Outline => Self::Outline,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, ValueEnum)]
enum CliQueryInclude {
    Path,
    Properties,
    Keywords,
    Links,
    Backlinks,
    Source,
    Target,
}

impl From<CliQueryInclude> for QueryInclude {
    fn from(value: CliQueryInclude) -> Self {
        match value {
            CliQueryInclude::Path => Self::Path,
            CliQueryInclude::Properties => Self::Properties,
            CliQueryInclude::Keywords => Self::Keywords,
            CliQueryInclude::Links => Self::Links,
            CliQueryInclude::Backlinks => Self::Backlinks,
            CliQueryInclude::Source => Self::Source,
            CliQueryInclude::Target => Self::Target,
        }
    }
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
        Command::Links { json, config } => {
            let rows = links_json_rows(json, config.as_deref())?;
            write_json_output(&rows)?;
            Ok(())
        }
        Command::Query {
            json,
            output,
            include,
            config,
            query,
        } => {
            let response = query_json_response(json, &query, output, &include, config.as_deref())?;
            write_json_output(&response)?;
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
        return Err(CliError::MissingJsonFlag("headings"));
    }

    let connection = open_cli_database(config_path)?;
    headings_rows_for_json(&connection, exclude_root)
}

fn links_json_rows(json: bool, config_path: Option<&Path>) -> Result<Vec<LinkJsonRow>, CliError> {
    if !json {
        return Err(CliError::MissingJsonFlag("links"));
    }

    let connection = open_cli_database(config_path)?;
    links_rows_for_json(&connection)
}

fn query_json_response(
    json: bool,
    query: &str,
    output: CliQueryOutput,
    includes: &[CliQueryInclude],
    config_path: Option<&Path>,
) -> Result<QueryResponse, CliError> {
    if !json {
        return Err(CliError::MissingJsonFlag("query"));
    }

    let connection = open_cli_database(config_path)?;
    let parsed = parse_query(query).map_err(CliError::QueryParse)?;
    let validated = validate_query(parsed, &QueryValidationOptions::default())
        .map_err(CliError::QueryValidate)?;
    let options = QueryExecutionOptions {
        output_mode: output.into(),
        includes: includes.iter().copied().map(QueryInclude::from).collect(),
    };
    execute_and_shape_query(&connection, &validated, &options).map_err(CliError::QueryShape)
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

fn links_rows_for_json(connection: &Connection) -> Result<Vec<LinkJsonRow>, CliError> {
    DbReader::list_links(connection)
        .map_err(CliError::DbRead)?
        .into_iter()
        .map(LinkJsonRow::try_from)
        .collect()
}

fn open_cli_database(config_path: Option<&std::path::Path>) -> Result<Connection, CliError> {
    let db_path = if let Some(config_path) = config_path {
        Config::load_from_file(config_path)
            .map_err(CliError::Config)?
            .db_path
    } else {
        Config::default().db_path
    };
    open_existing_database_read_only(&db_path).map_err(CliError::Database)
}

fn write_json_output<T: Serialize>(value: &T) -> Result<(), CliError> {
    let stdout = io::stdout();
    let mut handle = stdout.lock();
    serde_json::to_writer_pretty(&mut handle, value).map_err(CliError::Json)?;
    handle.write_all(b"\n").map_err(CliError::Io)?;
    Ok(())
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
    MissingJsonFlag(&'static str),
    Config(ConfigError),
    Database(DbError),
    DbRead(crate::db::DbReadError),
    Indexer(IndexerError),
    QueryParse(QueryParseError),
    QueryValidate(QueryValidationError),
    QueryShape(QueryShapeError),
    InvalidHeadingTags {
        heading_id: i64,
        source: serde_json::Error,
    },
    InvalidHeadingPath {
        heading_id: i64,
        source: serde_json::Error,
    },
    Json(serde_json::Error),
    Io(io::Error),
}

impl CliError {
    fn exit_code(&self) -> u8 {
        match self {
            Self::Parse(_) | Self::MissingJsonFlag(_) => 2,
            Self::Config(_)
            | Self::Database(_)
            | Self::DbRead(_)
            | Self::Indexer(_)
            | Self::QueryParse(_)
            | Self::QueryValidate(_)
            | Self::QueryShape(_)
            | Self::InvalidHeadingTags { .. }
            | Self::InvalidHeadingPath { .. }
            | Self::Json(_)
            | Self::Io(_) => 1,
        }
    }
}

impl fmt::Display for CliError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Parse(error) => write!(f, "{error}"),
            Self::MissingJsonFlag(command) => {
                write!(f, "{command} currently only supports --json")
            }
            Self::Config(source) => write!(f, "{source}"),
            Self::Database(source) => write!(f, "{source}"),
            Self::DbRead(source) => write!(f, "{source}"),
            Self::Indexer(source) => write!(f, "{source}"),
            Self::QueryParse(source) => write!(f, "{source}"),
            Self::QueryValidate(source) => write!(f, "{source}"),
            Self::QueryShape(source) => write!(f, "{source}"),
            Self::InvalidHeadingTags { heading_id, source } => {
                write!(
                    f,
                    "failed to decode heading tags for heading {}: {}",
                    heading_id, source
                )
            }
            Self::InvalidHeadingPath { heading_id, source } => {
                write!(
                    f,
                    "failed to decode heading path for heading {}: {}",
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
            Self::MissingJsonFlag(_) => None,
            Self::Config(source) => Some(source),
            Self::Database(source) => Some(source),
            Self::DbRead(source) => Some(source),
            Self::Indexer(source) => Some(source),
            Self::QueryParse(source) => Some(source),
            Self::QueryValidate(source) => Some(source),
            Self::QueryShape(source) => Some(source),
            Self::InvalidHeadingTags { source, .. } => Some(source),
            Self::InvalidHeadingPath { source, .. } => Some(source),
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

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
struct LinkJsonRow {
    file_id: i64,
    file_path: String,
    heading_id: i64,
    heading_path: Vec<String>,
    heading_level: i64,
    source_context: String,
    format: String,
    link_type: String,
    raw: String,
    raw_target: String,
    raw_description: Option<String>,
    path: String,
    search_option: Option<String>,
    path_absolute: Option<String>,
    target_file_id: Option<i64>,
    target_heading_id: Option<i64>,
    target_custom_id: Option<String>,
    target_id: Option<String>,
    resolution_status: Option<String>,
    resolution_diagnostic: Option<String>,
    byte_start: i64,
    byte_end: i64,
    line: i64,
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

impl TryFrom<LinkListRow> for LinkJsonRow {
    type Error = CliError;

    fn try_from(row: LinkListRow) -> Result<Self, Self::Error> {
        let breadcrumbs: Vec<String> = serde_json::from_str(&row.heading_breadcrumbs_json)
            .map_err(|source| CliError::InvalidHeadingPath {
                heading_id: row.heading_id,
                source,
            })?;
        let heading_path = strip_root_breadcrumb(breadcrumbs, row.heading_level);

        Ok(Self {
            file_id: row.file_id,
            file_path: row.file_path,
            heading_id: row.heading_id,
            heading_path,
            heading_level: row.heading_level,
            source_context: row.source_context,
            format: row.format,
            link_type: row.link_type,
            raw: row.raw,
            raw_target: row.raw_target,
            raw_description: row.raw_description,
            path: row.path,
            search_option: row.search_option,
            path_absolute: row.path_absolute,
            target_file_id: row.target_file_id,
            target_heading_id: row.target_heading_id,
            target_custom_id: row.target_custom_id,
            target_id: row.target_id,
            resolution_status: row.resolution_status,
            resolution_diagnostic: row.resolution_diagnostic,
            byte_start: row.byte_start,
            byte_end: row.byte_end,
            line: row.line,
        })
    }
}

fn strip_root_breadcrumb(mut breadcrumbs: Vec<String>, heading_level: i64) -> Vec<String> {
    if heading_level == 0 {
        Vec::new()
    } else {
        if !breadcrumbs.is_empty() {
            breadcrumbs.remove(0);
        }
        breadcrumbs
    }
}

#[cfg(test)]
mod tests {
    use super::{rebuild, Cli, CliError};
    use crate::db::{
        open_database, open_in_memory_database_with_schema, DbError, DbWriter, FileRecordInput,
        HeadingRecord, LinkRecord, OutlinePathRecord, SchemaDefinition, CURRENT_SCHEMA_VERSION,
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

        let cli =
            Cli::try_parse_from(["orgfdb", "links", "--json"]).expect("links args should parse");

        match cli.command {
            super::Command::Links { json, config } => {
                assert!(json);
                assert_eq!(config, None);
            }
            other => panic!("unexpected command: {other:?}"),
        }

        let cli = Cli::try_parse_from(["orgfdb", "links", "--json", "--config", "config.toml"])
            .expect("links config args should parse");

        match cli.command {
            super::Command::Links { json, config } => {
                assert!(json);
                assert_eq!(config, Some(PathBuf::from("config.toml")));
            }
            other => panic!("unexpected command: {other:?}"),
        }

        let cli = Cli::try_parse_from([
            "orgfdb",
            "query",
            "--json",
            "--output",
            "outline",
            "--include",
            "path,links,path",
            "(todo \"NEXT\")",
        ])
        .expect("query args should parse");

        match cli.command {
            super::Command::Query {
                json,
                output,
                include,
                config,
                query,
            } => {
                assert!(json);
                assert_eq!(output, super::CliQueryOutput::Outline);
                assert_eq!(
                    include,
                    vec![
                        super::CliQueryInclude::Path,
                        super::CliQueryInclude::Links,
                        super::CliQueryInclude::Path,
                    ]
                );
                assert_eq!(config, None);
                assert_eq!(query, "(todo \"NEXT\")");
            }
            other => panic!("unexpected command: {other:?}"),
        }
    }

    #[test]
    fn query_cli_rejects_unknown_include_value() {
        let error = Cli::try_parse_from([
            "orgfdb",
            "query",
            "--json",
            "--include",
            "path,unknown",
            "(todo)",
        ])
        .expect_err("unknown include should fail");
        assert_eq!(error.kind(), clap::error::ErrorKind::InvalidValue);
        let rendered = error.to_string();
        assert!(rendered.contains("unknown"));
        assert!(rendered.contains("path"));
    }

    #[test]
    fn query_cli_rejects_invalid_output_value() {
        let error =
            Cli::try_parse_from(["orgfdb", "query", "--json", "--output", "tree", "(todo)"])
                .expect_err("invalid output should fail");
        assert_eq!(error.kind(), clap::error::ErrorKind::InvalidValue);
        let rendered = error.to_string();
        assert!(rendered.contains("tree"));
        assert!(rendered.contains("outline"));
    }

    #[test]
    fn query_json_supports_heading_link_and_file_targets() {
        let test_dir = TestDir::new("query-targets");
        let config_path = write_query_fixture(&test_dir);

        let heading = super::query_json_response(
            true,
            "(todo \"NEXT\")",
            super::CliQueryOutput::Flat,
            &[],
            Some(&config_path),
        )
        .expect("heading query should succeed");
        assert_eq!(heading.target, crate::query::QueryTarget::Headings);
        assert_eq!(heading.output, crate::query::QueryOutputMode::Flat);
        assert_eq!(heading.results.len(), 1);
        match &heading.results[0] {
            crate::query::QueryResultNode::Heading(node) => {
                assert_eq!(node.title, "Query engine");
            }
            other => panic!("expected heading result, got {other:?}"),
        }

        let links = super::query_json_response(
            true,
            "(links (status \"broken\"))",
            super::CliQueryOutput::Flat,
            &[],
            Some(&config_path),
        )
        .expect("link query should succeed");
        assert_eq!(links.target, crate::query::QueryTarget::Links);
        assert_eq!(links.results.len(), 1);
        match &links.results[0] {
            crate::query::QueryResultNode::Link(node) => {
                assert_eq!(node.resolution_status.as_deref(), Some("broken"));
            }
            other => panic!("expected link result, got {other:?}"),
        }

        let files = super::query_json_response(
            true,
            "(files (file-title \"Projects\"))",
            super::CliQueryOutput::Flat,
            &[],
            Some(&config_path),
        )
        .expect("file query should succeed");
        assert_eq!(files.target, crate::query::QueryTarget::Files);
        assert_eq!(files.results.len(), 1);
        match &files.results[0] {
            crate::query::QueryResultNode::File(node) => {
                assert_eq!(node.title, "Projects");
            }
            other => panic!("expected file result, got {other:?}"),
        }
    }

    #[test]
    fn query_json_supports_outline_and_multiple_includes() {
        let test_dir = TestDir::new("query-outline");
        let config_path = write_query_fixture(&test_dir);

        let response = super::query_json_response(
            true,
            "(headings (title \"sqlite\"))",
            super::CliQueryOutput::Outline,
            &[
                super::CliQueryInclude::Path,
                super::CliQueryInclude::Links,
                super::CliQueryInclude::Path,
            ],
            Some(&config_path),
        )
        .expect("outline query should succeed");

        assert_eq!(response.output, crate::query::QueryOutputMode::Outline);
        assert_eq!(
            response.includes,
            vec![
                crate::query::QueryInclude::Path,
                crate::query::QueryInclude::Links
            ]
        );
        assert_eq!(response.results.len(), 1);
        match &response.results[0] {
            crate::query::QueryResultNode::File(file) => {
                assert!(!file.matched);
                assert_eq!(
                    file.path,
                    test_dir.path().join("notes.org").display().to_string()
                );
                let child = match &file.children.as_ref().expect("children")[0] {
                    crate::query::QueryResultNode::Heading(node) => node,
                    other => panic!("expected heading child, got {other:?}"),
                };
                assert!(child.matched);
                assert_eq!(child.title, "SQLite notes");
            }
            other => panic!("expected outline file result, got {other:?}"),
        }
    }

    #[test]
    fn query_json_examples_from_todo_work() {
        let test_dir = TestDir::new("query-examples");
        let config_path = write_query_fixture(&test_dir);

        let examples = [
            "(todo \"NEXT\")",
            "(headings (tags \"project\" :match :all))",
            "(links (status \"broken\"))",
            "(files (file-title \"Projects\"))",
        ];

        for query in examples {
            let response = super::query_json_response(
                true,
                query,
                super::CliQueryOutput::Flat,
                &[],
                Some(&config_path),
            )
            .expect("example query should succeed");
            assert!(!response.results.is_empty(), "expected matches for {query}");
        }

        let include_response = super::query_json_response(
            true,
            "(headings (todo \"NEXT\"))",
            super::CliQueryOutput::Flat,
            &[super::CliQueryInclude::Path, super::CliQueryInclude::Links],
            Some(&config_path),
        )
        .expect("include example should succeed");
        match &include_response.results[0] {
            crate::query::QueryResultNode::Heading(node) => {
                assert!(node.node_path.is_some());
                assert!(node.links.is_some());
            }
            other => panic!("expected heading result, got {other:?}"),
        }
    }

    #[test]
    fn query_json_reports_invalid_syntax_semantics_and_backend_requirements() {
        let test_dir = TestDir::new("query-errors");
        let config_path = write_query_fixture(&test_dir);

        let syntax_error = super::query_json_response(
            true,
            "(todo \"NEXT\"",
            super::CliQueryOutput::Flat,
            &[],
            Some(&config_path),
        )
        .expect_err("invalid syntax should fail");
        assert!(matches!(syntax_error, CliError::QueryParse(_)));
        assert!(syntax_error.to_string().contains("unterminated"));

        let semantic_error = super::query_json_response(
            true,
            "(links (todo \"NEXT\"))",
            super::CliQueryOutput::Flat,
            &[],
            Some(&config_path),
        )
        .expect_err("invalid semantics should fail");
        assert!(matches!(semantic_error, CliError::QueryValidate(_)));
        assert!(semantic_error
            .to_string()
            .contains("predicate todo is not valid for target links"));

        let backend_error = super::query_json_response(
            true,
            "(links (link-target \"notes.*\" :regexp t))",
            super::CliQueryOutput::Flat,
            &[],
            Some(&config_path),
        )
        .expect_err("unsupported backend requirement should fail");
        assert!(matches!(backend_error, CliError::QueryShape(_)));
        assert!(backend_error.to_string().contains("regexp"));
    }

    #[test]
    fn query_json_is_read_only_and_does_not_reparse_files() {
        let test_dir = TestDir::new("query-read-only");
        let config_path = write_query_fixture(&test_dir);
        let db_path = test_dir.path().join("db.sqlite");
        let org_path = test_dir.path().join("projects.org");

        let initial = super::query_json_response(
            true,
            "(todo \"NEXT\")",
            super::CliQueryOutput::Flat,
            &[],
            Some(&config_path),
        )
        .expect("initial query should succeed");

        write_file(&org_path, "#+TITLE: Changed\n* DONE Different\n");

        let stored = super::query_json_response(
            true,
            "(todo \"NEXT\")",
            super::CliQueryOutput::Flat,
            &[],
            Some(&config_path),
        )
        .expect("stored query should succeed");
        assert_eq!(initial, stored);

        let reopened = Connection::open(&db_path).expect("database should reopen");
        let version_after: u32 = reopened
            .pragma_query_value(None, "user_version", |row| row.get(0))
            .expect("user_version should load after read-only query");
        assert_eq!(version_after, CURRENT_SCHEMA_VERSION);
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
                    title_raw: "TODO [#A] Inbox".to_string(),
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
        assert_eq!(array[1]["title"], "Inbox");
        assert_eq!(array[1]["title_raw"], "TODO [#A] Inbox");
        assert_eq!(array[1]["todo_keyword"], "TODO");
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
    fn links_json_contract_includes_root_links_and_stable_ordering() {
        let schema = SchemaDefinition::new(CURRENT_SCHEMA_VERSION, false);
        let mut connection =
            open_in_memory_database_with_schema(&schema).expect("database should open");

        seed_links_fixture(
            &mut connection,
            "/tmp/a.org",
            "Alpha",
            "Inbox",
            &[
                SeedLink {
                    id: 2,
                    heading_kind: HeadingKind::Root,
                    byte_start: 0,
                    byte_end: 16,
                    line: 1,
                    source_context: "normal",
                    format: "bracket",
                    raw: "[[id:root-link]]",
                    raw_target: "id:root-link",
                    raw_description: None,
                    link_type: "id",
                    path: "root-link",
                    search_option: None,
                    path_absolute: None,
                    target_file_id: None,
                    target_heading_id: None,
                    target_custom_id: None,
                    target_id: None,
                    resolution_status: None,
                    resolution_diagnostic: None,
                },
                SeedLink {
                    id: 3,
                    heading_kind: HeadingKind::Child,
                    byte_start: 35,
                    byte_end: 54,
                    line: 3,
                    source_context: "normal",
                    format: "plain",
                    raw: "https://example.org",
                    raw_target: "https://example.org",
                    raw_description: None,
                    link_type: "https",
                    path: "//example.org",
                    search_option: None,
                    path_absolute: None,
                    target_file_id: None,
                    target_heading_id: None,
                    target_custom_id: None,
                    target_id: None,
                    resolution_status: None,
                    resolution_diagnostic: None,
                },
            ],
        );
        seed_links_fixture(
            &mut connection,
            "/tmp/b.org",
            "Beta",
            "Todo",
            &[SeedLink {
                id: 1,
                heading_kind: HeadingKind::Child,
                byte_start: 5,
                byte_end: 27,
                line: 2,
                source_context: "drawer",
                format: "bracket",
                raw: "[[file:notes.org::42]]",
                raw_target: "file:notes.org::42",
                raw_description: None,
                link_type: "file",
                path: "notes.org",
                search_option: Some("42"),
                path_absolute: None,
                target_file_id: None,
                target_heading_id: None,
                target_custom_id: None,
                target_id: None,
                resolution_status: None,
                resolution_diagnostic: None,
            }],
        );

        let rows = super::links_rows_for_json(&connection).expect("rows should load");
        assert_eq!(rows.len(), 3);
        assert_eq!(rows[0].file_path, "/tmp/a.org");
        assert_eq!(rows[0].heading_level, 0);
        assert!(rows[0].heading_path.is_empty());
        assert_eq!(rows[1].file_path, "/tmp/a.org");
        assert_eq!(rows[1].heading_level, 1);
        assert_eq!(rows[1].heading_path, vec!["Inbox".to_string()]);
        assert_eq!(rows[2].file_path, "/tmp/b.org");
        assert_eq!(rows[2].search_option.as_deref(), Some("42"));

        let json = serde_json::to_value(&rows).expect("rows should serialize");
        let array = json.as_array().expect("rows should serialize as an array");
        assert_eq!(sorted_object_keys(&array[0]), expected_link_json_keys());
        assert!(array[0].get("type").is_none());
        assert_eq!(array[0]["link_type"], "id");
        assert_eq!(array[0]["source_context"], "normal");
        assert_eq!(array[0]["heading_path"], Value::Array(vec![]));
        assert_eq!(array[0]["heading_level"], 0);
        assert_eq!(array[1]["heading_path"], serde_json::json!(["Inbox"]));
        assert_eq!(array[2]["file_path"], "/tmp/b.org");
        assert_eq!(array[2]["byte_start"], 5);
        assert_eq!(array[2]["resolution_status"], Value::Null);
        assert_eq!(array[2]["resolution_diagnostic"], Value::Null);
    }

    #[test]
    fn links_json_includes_resolution_fields_for_all_resolution_states() {
        let schema = SchemaDefinition::new(CURRENT_SCHEMA_VERSION, false);
        let mut connection =
            open_in_memory_database_with_schema(&schema).expect("database should open");

        seed_links_fixture(
            &mut connection,
            "/tmp/resolution.org",
            "Resolution",
            "Links",
            &[
                SeedLink {
                    id: 1,
                    heading_kind: HeadingKind::Root,
                    byte_start: 0,
                    byte_end: 17,
                    line: 1,
                    source_context: "normal",
                    format: "bracket",
                    raw: "[[id:resolved]]",
                    raw_target: "id:resolved",
                    raw_description: None,
                    link_type: "id",
                    path: "resolved",
                    search_option: None,
                    path_absolute: None,
                    target_file_id: None,
                    target_heading_id: None,
                    target_custom_id: None,
                    target_id: Some("resolved"),
                    resolution_status: Some("resolved"),
                    resolution_diagnostic: None,
                },
                SeedLink {
                    id: 2,
                    heading_kind: HeadingKind::Child,
                    byte_start: 20,
                    byte_end: 39,
                    line: 2,
                    source_context: "normal",
                    format: "bracket",
                    raw: "[[file:missing.org]]",
                    raw_target: "file:missing.org",
                    raw_description: None,
                    link_type: "file",
                    path: "missing.org",
                    search_option: None,
                    path_absolute: Some("/tmp/missing.org"),
                    target_file_id: None,
                    target_heading_id: None,
                    target_custom_id: None,
                    target_id: None,
                    resolution_status: Some("broken"),
                    resolution_diagnostic: Some("file not found"),
                },
                SeedLink {
                    id: 3,
                    heading_kind: HeadingKind::Child,
                    byte_start: 40,
                    byte_end: 55,
                    line: 3,
                    source_context: "normal",
                    format: "plain",
                    raw: "id:duplicate",
                    raw_target: "id:duplicate",
                    raw_description: None,
                    link_type: "id",
                    path: "duplicate",
                    search_option: None,
                    path_absolute: None,
                    target_file_id: None,
                    target_heading_id: None,
                    target_custom_id: None,
                    target_id: Some("duplicate"),
                    resolution_status: Some("ambiguous"),
                    resolution_diagnostic: Some("duplicate id"),
                },
                SeedLink {
                    id: 4,
                    heading_kind: HeadingKind::Child,
                    byte_start: 60,
                    byte_end: 81,
                    line: 4,
                    source_context: "normal",
                    format: "bracket",
                    raw: "[[id:outside-universe]]",
                    raw_target: "id:outside-universe",
                    raw_description: None,
                    link_type: "id",
                    path: "outside-universe",
                    search_option: None,
                    path_absolute: None,
                    target_file_id: None,
                    target_heading_id: None,
                    target_custom_id: None,
                    target_id: Some("outside-universe"),
                    resolution_status: Some("unresolved"),
                    resolution_diagnostic: Some("id not found"),
                },
                SeedLink {
                    id: 5,
                    heading_kind: HeadingKind::Child,
                    byte_start: 90,
                    byte_end: 106,
                    line: 5,
                    source_context: "normal",
                    format: "angle",
                    raw: "<shell:ls>",
                    raw_target: "shell:ls",
                    raw_description: None,
                    link_type: "shell",
                    path: "ls",
                    search_option: None,
                    path_absolute: None,
                    target_file_id: None,
                    target_heading_id: None,
                    target_custom_id: None,
                    target_id: None,
                    resolution_status: Some("unsupported"),
                    resolution_diagnostic: Some("unsupported link type"),
                },
            ],
        );

        let resolved_file_id: i64 = connection
            .query_row(
                "SELECT id FROM files WHERE path = '/tmp/resolution.org'",
                [],
                |row| row.get(0),
            )
            .expect("fixture file should exist");
        let resolved_heading_id: i64 = connection
            .query_row(
                "SELECT id
                 FROM headings
                 WHERE file_id = ?1 AND level = 1 AND title = 'Links'",
                [resolved_file_id],
                |row| row.get(0),
            )
            .expect("fixture child heading should exist");
        connection
            .execute(
                "UPDATE links
                 SET target_file_id = ?2,
                     target_heading_id = ?3
                 WHERE id = ?1",
                rusqlite::params![1, resolved_file_id, resolved_heading_id],
            )
            .expect("resolved link target ids should update");

        let rows = super::links_rows_for_json(&connection).expect("rows should load");
        let json = serde_json::to_value(&rows).expect("rows should serialize");
        let array = json.as_array().expect("rows should serialize as an array");

        assert_eq!(array.len(), 5);
        assert_eq!(sorted_object_keys(&array[0]), expected_link_json_keys());
        assert_eq!(array[0]["resolution_status"], "resolved");
        assert_eq!(array[0]["target_file_id"], resolved_file_id);
        assert_eq!(array[0]["target_heading_id"], resolved_heading_id);
        assert_eq!(array[0]["target_id"], "resolved");
        assert_eq!(array[1]["resolution_status"], "broken");
        assert_eq!(array[1]["path_absolute"], "/tmp/missing.org");
        assert_eq!(array[1]["resolution_diagnostic"], "file not found");
        assert_eq!(array[2]["resolution_status"], "ambiguous");
        assert_eq!(array[2]["target_id"], "duplicate");
        assert_eq!(array[3]["resolution_status"], "unresolved");
        assert_eq!(array[3]["target_id"], "outside-universe");
        assert_eq!(array[4]["resolution_status"], "unsupported");
        assert_eq!(array[4]["resolution_diagnostic"], "unsupported link type");
    }

    #[test]
    fn links_uses_configured_db_path_when_config_is_provided() {
        let test_dir = TestDir::new("links-config");
        let config_dir = test_dir.path().join("nested/config");
        let db_path = config_dir.join("../db.sqlite");
        let config_path = config_dir.join("config.toml");

        write_file(
            &config_path,
            r#"
db_path = "../db.sqlite"
"#,
        );

        let mut configured_db = open_database(&db_path).expect("configured database should open");
        seed_links_fixture(
            &mut configured_db,
            "/tmp/configured.org",
            "Configured",
            "Heading",
            &[SeedLink {
                id: 1,
                heading_kind: HeadingKind::Root,
                byte_start: 0,
                byte_end: 16,
                line: 1,
                source_context: "normal",
                format: "bracket",
                raw: "[[id:config]]",
                raw_target: "id:config",
                raw_description: None,
                link_type: "id",
                path: "config",
                search_option: None,
                path_absolute: None,
                target_file_id: None,
                target_heading_id: None,
                target_custom_id: None,
                target_id: None,
                resolution_status: None,
                resolution_diagnostic: None,
            }],
        );
        drop(configured_db);

        let rows =
            super::links_json_rows(true, Some(&config_path)).expect("rows should load from db");

        assert_eq!(rows.len(), 1);
        assert_eq!(rows[0].file_path, "/tmp/configured.org");
        assert_eq!(rows[0].heading_level, 0);
    }

    #[test]
    fn rebuild_and_links_json_read_stored_source_facts_without_rebuild() {
        let test_dir = TestDir::new("links-read-only");
        let config_path = test_dir.path().join("config.toml");
        let org_path = test_dir.path().join("notes.org");

        write_file(
            &org_path,
            "Root [[id:root]]\n* Inbox\nSee https://example.org and [[file:ref.org::42][Ref]]\n",
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

        let initial_rows =
            super::links_json_rows(true, Some(&config_path)).expect("rows should load");
        assert_eq!(initial_rows.len(), 3);
        assert_eq!(initial_rows[0].heading_level, 0);
        assert!(initial_rows[0].heading_path.is_empty());
        assert_eq!(initial_rows[0].raw, "[[id:root]]");
        assert_eq!(initial_rows[1].heading_path, vec!["Inbox".to_string()]);
        assert_eq!(initial_rows[1].raw, "https://example.org");
        assert_eq!(initial_rows[2].raw_description.as_deref(), Some("Ref"));
        assert_eq!(initial_rows[2].search_option.as_deref(), Some("42"));

        write_file(
            &org_path,
            "Changed file without the stored links anymore.\n* Different\nNo original links.\n",
        );

        let stored_rows =
            super::links_json_rows(true, Some(&config_path)).expect("stored rows should load");
        assert_eq!(stored_rows, initial_rows);
    }

    #[test]
    fn links_json_read_only_open_does_not_create_missing_database() {
        let test_dir = TestDir::new("links-missing-db");
        let config_path = test_dir.path().join("config.toml");
        let db_path = test_dir.path().join("missing.sqlite");

        write_file(
            &config_path,
            r#"
db_path = "./missing.sqlite"
"#,
        );

        let error = super::links_json_rows(true, Some(&config_path))
            .expect_err("missing database should fail");
        assert!(
            matches!(error, CliError::Database(DbError::Open { .. })),
            "expected read-only open error, got {error}"
        );
        assert!(
            !db_path.exists(),
            "read-only links should not create a database"
        );
    }

    #[test]
    fn links_json_read_only_open_rejects_future_schema_versions() {
        let test_dir = TestDir::new("links-future-db");
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

        let error = super::links_json_rows(true, Some(&config_path))
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
    }

    #[test]
    fn rebuild_helper_propagates_indexer_errors() {
        let error = rebuild(Path::new("missing-config.toml")).expect_err("rebuild should fail");

        assert!(matches!(error, CliError::Indexer(_)));
    }

    fn write_query_fixture(test_dir: &TestDir) -> PathBuf {
        let config_path = test_dir.path().join("config.toml");
        let projects_path = test_dir.path().join("projects.org");
        let notes_path = test_dir.path().join("notes.org");

        write_file(
            &projects_path,
            r#"#+TITLE: Projects
#+AUTHOR: Alice
#+CATEGORY: work
[[file:notes.org][Preamble]]
* NEXT Query engine :project:
:PROPERTIES:
:AREA: infra
:END:
[[file:notes.org::*SQLite notes][Notes heading]]
* Broken refs
[[file:notes.org::*Missing heading][Missing]]
"#,
        );
        write_file(
            &notes_path,
            r#"#+TITLE: Notes
* SQLite notes
"#,
        );
        write_file(
            &config_path,
            r#"
db_path = "./db.sqlite"
files = ["./projects.org", "./notes.org"]

[todo]
default_open_keywords = ["TODO(t)", "NEXT(n)"]
default_closed_keywords = ["DONE(d)"]

[search]
fts5_enabled = false
index_body_text = false
"#,
        );
        let report = rebuild(&config_path).expect("fixture rebuild should succeed");
        assert_eq!(report.indexed_files.len(), 2);
        config_path
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

    fn expected_link_json_keys() -> Vec<String> {
        vec![
            "byte_end",
            "byte_start",
            "file_id",
            "file_path",
            "format",
            "heading_id",
            "heading_level",
            "heading_path",
            "line",
            "link_type",
            "path",
            "path_absolute",
            "raw",
            "raw_description",
            "raw_target",
            "resolution_diagnostic",
            "resolution_status",
            "search_option",
            "source_context",
            "target_custom_id",
            "target_file_id",
            "target_heading_id",
            "target_id",
        ]
        .into_iter()
        .map(str::to_string)
        .collect()
    }

    #[derive(Clone, Copy)]
    enum HeadingKind {
        Root,
        Child,
    }

    struct SeedLink<'a> {
        id: i64,
        heading_kind: HeadingKind,
        byte_start: i64,
        byte_end: i64,
        line: i64,
        source_context: &'a str,
        format: &'a str,
        raw: &'a str,
        raw_target: &'a str,
        raw_description: Option<&'a str>,
        link_type: &'a str,
        path: &'a str,
        search_option: Option<&'a str>,
        path_absolute: Option<&'a str>,
        target_file_id: Option<i64>,
        target_heading_id: Option<i64>,
        target_custom_id: Option<&'a str>,
        target_id: Option<&'a str>,
        resolution_status: Option<&'a str>,
        resolution_diagnostic: Option<&'a str>,
    }

    fn seed_links_fixture(
        connection: &mut Connection,
        file_path: &str,
        root_title: &str,
        child_title: &str,
        links: &[SeedLink<'_>],
    ) {
        let file = FileRecordInput {
            path: PathBuf::from(file_path),
            mtime_ns: 10,
            size: 100,
            content_hash: None,
            indexed_at: None,
        };

        DbWriter::rebuild_file(connection, &file, |tx, file_id| {
            let root_id = DbWriter::insert_level0_heading(
                tx,
                &HeadingRecord {
                    id: None,
                    file_id,
                    parent_id: None,
                    level: 0,
                    line_number: None,
                    byte_start: -1,
                    byte_end: 100,
                    title: root_title.to_string(),
                    title_raw: root_title.to_string(),
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
            DbWriter::insert_outline_path(
                tx,
                &[OutlinePathRecord {
                    heading_id: root_id,
                    file_id,
                    parent_id: None,
                    depth: 0,
                    materialized_path: "0000".to_string(),
                    breadcrumbs_json: format!("[\"{root_title}\"]"),
                }],
            )?;
            DbWriter::insert_headings(
                tx,
                &[HeadingRecord {
                    id: None,
                    file_id,
                    parent_id: Some(root_id),
                    level: 1,
                    line_number: Some(2),
                    byte_start: 10,
                    byte_end: 25,
                    title: child_title.to_string(),
                    title_raw: child_title.to_string(),
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
            let child_id = tx.last_insert_rowid();
            DbWriter::insert_outline_path(
                tx,
                &[OutlinePathRecord {
                    heading_id: child_id,
                    file_id,
                    parent_id: Some(root_id),
                    depth: 1,
                    materialized_path: "0000.0001".to_string(),
                    breadcrumbs_json: format!("[\"{root_title}\",\"{child_title}\"]"),
                }],
            )?;

            let rows = links
                .iter()
                .map(|link| LinkRecord {
                    id: Some(link.id),
                    file_id,
                    heading_id: match link.heading_kind {
                        HeadingKind::Root => root_id,
                        HeadingKind::Child => child_id,
                    },
                    byte_start: link.byte_start,
                    byte_end: link.byte_end,
                    line: link.line,
                    source_context: link.source_context.to_string(),
                    format: link.format.to_string(),
                    raw: link.raw.to_string(),
                    raw_target: link.raw_target.to_string(),
                    raw_description: link.raw_description.map(str::to_string),
                    link_type: link.link_type.to_string(),
                    path: link.path.to_string(),
                    search_option: link.search_option.map(str::to_string),
                })
                .collect::<Vec<_>>();
            DbWriter::insert_links(tx, &rows)?;

            for link in links {
                tx.execute(
                    "UPDATE links
                     SET path_absolute = ?2,
                         target_file_id = ?3,
                         target_heading_id = ?4,
                         target_custom_id = ?5,
                         target_id = ?6,
                         resolution_status = ?7,
                         resolution_diagnostic = ?8
                     WHERE id = ?1",
                    rusqlite::params![
                        link.id,
                        link.path_absolute,
                        link.target_file_id,
                        link.target_heading_id,
                        link.target_custom_id,
                        link.target_id,
                        link.resolution_status,
                        link.resolution_diagnostic
                    ],
                )
                .map_err(|source| crate::db::DbWriteError::Write {
                    operation: "seed_links_fixture.update_links",
                    source,
                })?;
            }

            Ok(())
        })
        .expect("fixture rebuild should succeed");
    }
}
