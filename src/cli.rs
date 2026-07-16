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
    db::{
        open_existing_database_read_only, sqlite_supports_fts5, DbError, DbReader, HeadingListRow,
        LinkListRow, SearchHeadingRow, DB_METADATA_FTS_AVAILABLE_KEY,
        DB_METADATA_FTS_BODY_INDEXED_KEY, DB_METADATA_FTS_SCHEMA_VERSION_KEY,
        FTS_SCHEMA_CONTRACT_VERSION,
    },
    indexer::{Indexer, IndexerError, RebuildReport},
    parser::OrgizeAdapter,
    query::{
        execute_and_shape_query, parse_query, sqlite_query_validation_options, validate_query,
        QueryExecutionError, QueryExecutionOptions, QueryInclude, QueryOutputMode, QueryParseError,
        QueryResponse, QueryShapeError, QueryValidationError,
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
    Search {
        #[arg(long)]
        json: bool,
        #[arg(long, conflicts_with = "body")]
        title: bool,
        #[arg(long, conflicts_with = "title")]
        body: bool,
        #[arg(long)]
        config: Option<PathBuf>,
        #[arg(help = "Raw SQLite FTS5 MATCH expression")]
        expression: String,
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
        Command::Search {
            json,
            title,
            body,
            config,
            expression,
        } => {
            let rows = search_json_rows(
                json,
                cli_search_scope(title, body),
                &expression,
                config.as_deref(),
            )?;
            write_json_output(&rows)?;
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

    let config = load_cli_config(config_path)?;
    let connection =
        open_existing_database_read_only(&config.db_path).map_err(CliError::Database)?;
    let parsed = parse_query(query).map_err(CliError::QueryParse)?;
    let validation_options =
        sqlite_query_validation_options(&connection).map_err(CliError::QueryExecute)?;
    let validated = validate_query(parsed, &validation_options).map_err(CliError::QueryValidate)?;
    let options = QueryExecutionOptions {
        output_mode: output.into(),
        includes: includes.iter().copied().map(QueryInclude::from).collect(),
        query_timezone: config.query.timezone.clone(),
        now_utc: None,
    };
    execute_and_shape_query(&connection, &validated, &options).map_err(CliError::QueryShape)
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum CliSearchScope {
    All,
    Title,
    Body,
}

fn cli_search_scope(title: bool, body: bool) -> CliSearchScope {
    if title {
        CliSearchScope::Title
    } else if body {
        CliSearchScope::Body
    } else {
        CliSearchScope::All
    }
}

fn search_json_rows(
    json: bool,
    scope: CliSearchScope,
    expression: &str,
    config_path: Option<&Path>,
) -> Result<Vec<SearchJsonRow>, CliError> {
    if !json {
        return Err(CliError::MissingJsonFlag("search"));
    }

    if expression.trim().is_empty() {
        return Err(CliError::InvalidSearchUsage(
            "search requires a non-empty FTS expression".to_string(),
        ));
    }

    let compiled_expression = compile_search_expression(scope, expression)?;

    let config = load_cli_config(config_path)?;
    if !config.search.fts5_enabled {
        return Err(CliError::Search(SearchError::DisabledByConfig));
    }

    let connection =
        open_existing_database_read_only(&config.db_path).map_err(CliError::Database)?;
    match sqlite_supports_fts5(&connection) {
        Ok(true) => {}
        Ok(false) => return Err(CliError::Search(SearchError::FtsUnavailable)),
        Err(source) => {
            return Err(CliError::Search(SearchError::Inspect {
                operation: "probe SQLite FTS5 support",
                source,
            }))
        }
    }

    let trust = inspect_search_backend_state(&connection)?;
    if scope == CliSearchScope::Body && !trust.body_indexed {
        return Err(CliError::Search(SearchError::BodyScopeUnavailable));
    }

    DbReader::search_headings(&connection, &compiled_expression)
        .map_err(map_search_db_read_error)?
        .into_iter()
        .map(SearchJsonRow::from_db_row)
        .collect()
}

fn map_search_db_read_error(error: crate::db::DbReadError) -> CliError {
    match error {
        crate::db::DbReadError::Query { operation, source }
            if is_expected_fts5_expression_error(&source) =>
        {
            let _operation = operation;
            CliError::Search(SearchError::InvalidExpression { expression: None })
        }
        crate::db::DbReadError::Query { operation, source } => {
            CliError::Search(SearchError::Execute { operation, source })
        }
    }
}

fn compile_search_expression(scope: CliSearchScope, expression: &str) -> Result<String, CliError> {
    match scope {
        CliSearchScope::All => Ok(expression.to_string()),
        CliSearchScope::Title => {
            reject_explicit_column_filter(expression, "title")?;
            Ok(format!("title: ({expression})"))
        }
        CliSearchScope::Body => {
            reject_explicit_column_filter(expression, "body")?;
            Ok(format!("body: ({expression})"))
        }
    }
}

fn reject_explicit_column_filter(expression: &str, scope: &'static str) -> Result<(), CliError> {
    if contains_unquoted_colon(expression) {
        return Err(CliError::Search(SearchError::ScopedColumnFilter { scope }));
    }
    Ok(())
}

fn contains_unquoted_colon(expression: &str) -> bool {
    let mut in_quotes = false;
    let mut chars = expression.chars().peekable();
    while let Some(ch) = chars.next() {
        match ch {
            '"' => {
                if in_quotes && chars.peek() == Some(&'"') {
                    chars.next();
                } else {
                    in_quotes = !in_quotes;
                }
            }
            ':' if !in_quotes => return true,
            _ => {}
        }
    }
    false
}

fn inspect_search_backend_state(connection: &Connection) -> Result<SearchBackendState, CliError> {
    let metadata_table_exists = table_exists(connection, "db_metadata").map_err(|source| {
        CliError::Search(SearchError::Inspect {
            operation: "inspect db_metadata existence",
            source,
        })
    })?;
    if !metadata_table_exists {
        return Err(CliError::Search(SearchError::MissingTrustMetadata));
    }

    let fts_available =
        load_metadata_value(connection, DB_METADATA_FTS_AVAILABLE_KEY).map_err(|source| {
            CliError::Search(SearchError::Inspect {
                operation: "load fts_available",
                source,
            })
        })?;
    let fts_body_indexed = load_metadata_value(connection, DB_METADATA_FTS_BODY_INDEXED_KEY)
        .map_err(|source| {
            CliError::Search(SearchError::Inspect {
                operation: "load fts_body_indexed",
                source,
            })
        })?;
    let fts_schema_version = load_metadata_value(connection, DB_METADATA_FTS_SCHEMA_VERSION_KEY)
        .map_err(|source| {
            CliError::Search(SearchError::Inspect {
                operation: "load fts_schema_version",
                source,
            })
        })?;

    let Some(fts_available) = fts_available else {
        return Err(CliError::Search(SearchError::MissingTrustMetadata));
    };
    let Some(fts_body_indexed) = fts_body_indexed else {
        return Err(CliError::Search(SearchError::MissingTrustMetadata));
    };
    let Some(fts_schema_version) = fts_schema_version else {
        return Err(CliError::Search(SearchError::MissingTrustMetadata));
    };

    if fts_available != "1" && fts_available != "0" {
        return Err(CliError::Search(SearchError::InvalidTrustMetadata));
    }
    if fts_body_indexed != "1" && fts_body_indexed != "0" {
        return Err(CliError::Search(SearchError::InvalidTrustMetadata));
    }
    if fts_schema_version != FTS_SCHEMA_CONTRACT_VERSION && fts_schema_version != "0" {
        return Err(CliError::Search(SearchError::InvalidTrustMetadata));
    }
    if fts_available != "1" || fts_schema_version != FTS_SCHEMA_CONTRACT_VERSION {
        return Err(CliError::Search(SearchError::MissingTrustedIndex));
    }

    let heading_fts_exists = table_exists(connection, "heading_fts").map_err(|source| {
        CliError::Search(SearchError::Inspect {
            operation: "inspect heading_fts existence",
            source,
        })
    })?;
    if !heading_fts_exists {
        return Err(CliError::Search(SearchError::MissingTrustedIndex));
    }

    let table_sql = connection
        .query_row(
            "SELECT sql FROM sqlite_master WHERE type = 'table' AND name = 'heading_fts'",
            [],
            |row| row.get::<_, String>(0),
        )
        .map_err(|source| {
            CliError::Search(SearchError::Inspect {
                operation: "load heading_fts definition",
                source,
            })
        })?;
    let normalized = normalize_sql_definition(&table_sql);
    if !normalized.contains("createvirtualtable")
        || !normalized.contains("usingfts5")
        || !normalized.contains("content=''")
        || !normalized.contains("tokenize='unicode61'")
    {
        return Err(CliError::Search(SearchError::IncompatibleIndexSchema));
    }

    let columns = load_table_columns(connection, "heading_fts").map_err(|source| {
        CliError::Search(SearchError::Inspect {
            operation: "inspect heading_fts columns",
            source,
        })
    })?;
    if columns != ["title".to_string(), "body".to_string()] {
        return Err(CliError::Search(SearchError::IncompatibleIndexSchema));
    }

    Ok(SearchBackendState {
        body_indexed: fts_body_indexed == "1",
    })
}

fn load_metadata_value(
    connection: &Connection,
    key: &str,
) -> Result<Option<String>, rusqlite::Error> {
    connection
        .query_row(
            "SELECT value FROM db_metadata WHERE key = ?1",
            [key],
            |row| row.get::<_, String>(0),
        )
        .map(Some)
        .or_else(|error| match error {
            rusqlite::Error::QueryReturnedNoRows => Ok(None),
            other => Err(other),
        })
}

fn table_exists(connection: &Connection, table: &str) -> Result<bool, rusqlite::Error> {
    connection.query_row(
        "SELECT EXISTS(
             SELECT 1
             FROM sqlite_master
             WHERE type = 'table' AND name = ?1
         )",
        [table],
        |row| row.get(0),
    )
}

fn load_table_columns(
    connection: &Connection,
    table: &str,
) -> Result<Vec<String>, rusqlite::Error> {
    let pragma = format!("PRAGMA table_info({table})");
    let mut statement = connection.prepare(&pragma)?;
    let rows = statement.query_map([], |row| row.get::<_, String>(1))?;
    rows.collect::<Result<Vec<_>, _>>()
}

fn normalize_sql_definition(sql: &str) -> String {
    sql.chars()
        .filter(|ch| !ch.is_whitespace())
        .flat_map(char::to_lowercase)
        .collect()
}

fn is_expected_fts5_expression_error(error: &rusqlite::Error) -> bool {
    let Some(message) = sqlite_error_message(error) else {
        return false;
    };
    message.contains("fts5:")
        || message.contains("unterminated string")
        || message.contains("no such column:")
}

fn sqlite_error_message(error: &rusqlite::Error) -> Option<&str> {
    match error {
        rusqlite::Error::SqliteFailure(_, Some(message)) => Some(message.as_str()),
        _ => None,
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct SearchBackendState {
    body_indexed: bool,
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
    let config = load_cli_config(config_path)?;
    open_existing_database_read_only(&config.db_path).map_err(CliError::Database)
}

fn load_cli_config(config_path: Option<&std::path::Path>) -> Result<Config, CliError> {
    match config_path {
        Some(config_path) => Config::load_from_file(config_path).map_err(CliError::Config),
        None => Ok(Config::default()),
    }
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
    InvalidSearchUsage(String),
    Config(ConfigError),
    Database(DbError),
    DbRead(crate::db::DbReadError),
    Indexer(IndexerError),
    QueryParse(QueryParseError),
    QueryValidate(QueryValidationError),
    QueryExecute(QueryExecutionError),
    QueryShape(QueryShapeError),
    Search(SearchError),
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
            Self::Parse(_) | Self::MissingJsonFlag(_) | Self::InvalidSearchUsage(_) => 2,
            Self::Config(_)
            | Self::Database(_)
            | Self::DbRead(_)
            | Self::Indexer(_)
            | Self::QueryParse(_)
            | Self::QueryValidate(_)
            | Self::QueryExecute(_)
            | Self::QueryShape(_)
            | Self::Search(_)
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
            Self::InvalidSearchUsage(message) => write!(f, "{message}"),
            Self::Config(source) => write!(f, "{source}"),
            Self::Database(source) => write!(f, "{source}"),
            Self::DbRead(source) => write!(f, "{source}"),
            Self::Indexer(source) => write!(f, "{source}"),
            Self::QueryParse(source) => write!(f, "{source}"),
            Self::QueryValidate(source) => write!(f, "{source}"),
            Self::QueryExecute(source) => write!(f, "{source}"),
            Self::QueryShape(source) => write!(f, "{source}"),
            Self::Search(source) => write!(f, "{source}"),
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
            Self::MissingJsonFlag(_) | Self::InvalidSearchUsage(_) => None,
            Self::Config(source) => Some(source),
            Self::Database(source) => Some(source),
            Self::DbRead(source) => Some(source),
            Self::Indexer(source) => Some(source),
            Self::QueryParse(source) => Some(source),
            Self::QueryValidate(source) => Some(source),
            Self::QueryExecute(source) => Some(source),
            Self::QueryShape(source) => Some(source),
            Self::Search(source) => Some(source),
            Self::InvalidHeadingTags { source, .. } => Some(source),
            Self::InvalidHeadingPath { source, .. } => Some(source),
            Self::Json(source) => Some(source),
            Self::Io(source) => Some(source),
        }
    }
}

#[derive(Debug)]
enum SearchError {
    DisabledByConfig,
    FtsUnavailable,
    MissingTrustMetadata,
    InvalidTrustMetadata,
    MissingTrustedIndex,
    IncompatibleIndexSchema,
    BodyScopeUnavailable,
    ScopedColumnFilter {
        scope: &'static str,
    },
    InvalidExpression {
        expression: Option<String>,
    },
    Inspect {
        operation: &'static str,
        source: rusqlite::Error,
    },
    Execute {
        operation: &'static str,
        source: rusqlite::Error,
    },
}

impl fmt::Display for SearchError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::DisabledByConfig => {
                write!(f, "search requires [search].fts5_enabled = true in the active config")
            }
            Self::FtsUnavailable => write!(
                f,
                "search is unavailable because the opened SQLite connection does not support FTS5"
            ),
            Self::MissingTrustMetadata => write!(
                f,
                "search is unavailable because this database has no trusted FTS metadata; run orgfdb rebuild to create the search index"
            ),
            Self::InvalidTrustMetadata => write!(
                f,
                "search is unavailable because this database has invalid FTS metadata; run orgfdb rebuild to refresh the search index"
            ),
            Self::MissingTrustedIndex => write!(
                f,
                "search is unavailable because this database does not contain a trusted FTS index; run orgfdb rebuild to create the search index"
            ),
            Self::IncompatibleIndexSchema => write!(
                f,
                "search is unavailable because the stored FTS index is incompatible with this build; run orgfdb rebuild to refresh the search index"
            ),
            Self::BodyScopeUnavailable => write!(
                f,
                "body search is unavailable because the stored FTS index was built without body text; rebuild with body indexing enabled"
            ),
            Self::ScopedColumnFilter { scope } => write!(
                f,
                "invalid SQLite FTS5 search expression: explicit column filters are not allowed with --{scope}"
            ),
            Self::InvalidExpression { expression } => {
                if let Some(expression) = expression {
                    write!(f, "invalid SQLite FTS5 search expression: {expression}")
                } else {
                    write!(f, "invalid SQLite FTS5 search expression")
                }
            }
            Self::Inspect { operation, source } => {
                write!(f, "failed to inspect SQLite search state during {operation}: {source}")
            }
            Self::Execute { operation, source } => {
                write!(f, "failed to execute SQLite search query during {operation}: {source}")
            }
        }
    }
}

impl Error for SearchError {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        match self {
            Self::DisabledByConfig
            | Self::FtsUnavailable
            | Self::MissingTrustMetadata
            | Self::InvalidTrustMetadata
            | Self::MissingTrustedIndex
            | Self::IncompatibleIndexSchema
            | Self::BodyScopeUnavailable
            | Self::ScopedColumnFilter { .. }
            | Self::InvalidExpression { .. } => None,
            Self::Inspect { source, .. } | Self::Execute { source, .. } => Some(source),
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
    title_raw: Option<String>,
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

#[derive(Debug, Clone, PartialEq, Serialize)]
struct SearchJsonRow {
    heading_id: i64,
    path: String,
    title: String,
    line_number: Option<i64>,
    byte_start: i64,
    byte_end: i64,
    rank: f64,
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

impl SearchJsonRow {
    fn from_db_row(row: SearchHeadingRow) -> Result<Self, CliError> {
        Ok(Self {
            heading_id: row.heading_id,
            path: row.path,
            title: row.title,
            line_number: row.line_number,
            byte_start: row.byte_start,
            byte_end: row.byte_end,
            rank: row.rank,
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
    use super::{rebuild, search_json_rows, Cli, CliError, CliSearchScope, SearchError};
    use crate::db::{
        open_database, open_database_with_schema, open_in_memory_database_with_schema,
        sqlite_supports_fts5, DbError, DbWriter, FileRecordInput, HeadingRecord, LinkRecord,
        OutlinePathRecord, SchemaDefinition, CURRENT_SCHEMA_VERSION, DB_METADATA_FTS_AVAILABLE_KEY,
        DB_METADATA_FTS_BODY_INDEXED_KEY, DB_METADATA_FTS_SCHEMA_VERSION_KEY,
        FTS_SCHEMA_CONTRACT_VERSION,
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

    fn write_search_config(
        path: &Path,
        db_path: &str,
        file_names: &[&str],
        fts5_enabled: bool,
        index_body_text: bool,
    ) {
        let files = file_names
            .iter()
            .map(|file| format!("{file:?}"))
            .collect::<Vec<_>>()
            .join(", ");
        write_file(
            path,
            &format!(
                "db_path = {db_path:?}\nfiles = [{files}]\n\n[search]\nfts5_enabled = {fts5_enabled}\nindex_body_text = {index_body_text}\n",
            ),
        );
    }

    fn build_search_fixture(
        name: &str,
        files: &[(&str, &str)],
        index_body_text: bool,
    ) -> (TestDir, PathBuf, PathBuf) {
        let probe = Connection::open_in_memory().expect("probe should open");
        assert!(
            sqlite_supports_fts5(&probe).expect("fts5 probe should run"),
            "search fixture requires SQLite FTS5 support"
        );

        let test_dir = TestDir::new(name);
        let config_path = test_dir.path().join("config.toml");
        let db_path = test_dir.path().join("db.sqlite");

        for (file_name, content) in files {
            write_file(&test_dir.path().join(file_name), content);
        }
        let file_names = files.iter().map(|(name, _)| *name).collect::<Vec<_>>();
        write_search_config(
            &config_path,
            "./db.sqlite",
            &file_names,
            true,
            index_body_text,
        );

        rebuild(&config_path).expect("search fixture rebuild should succeed");
        (test_dir, config_path, db_path)
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

        let cli = Cli::try_parse_from(["orgfdb", "search", "--json", "sqlite"])
            .expect("search args should parse");
        match cli.command {
            super::Command::Search {
                json,
                title,
                body,
                config,
                expression,
            } => {
                assert!(json);
                assert!(!title);
                assert!(!body);
                assert_eq!(config, None);
                assert_eq!(expression, "sqlite");
            }
            other => panic!("unexpected command: {other:?}"),
        }

        let cli = Cli::try_parse_from([
            "orgfdb",
            "search",
            "--json",
            "--title",
            "--config",
            "config.toml",
            "\"sqlite phrase\"",
        ])
        .expect("search scoped args should parse");
        match cli.command {
            super::Command::Search {
                json,
                title,
                body,
                config,
                expression,
            } => {
                assert!(json);
                assert!(title);
                assert!(!body);
                assert_eq!(config, Some(PathBuf::from("config.toml")));
                assert_eq!(expression, "\"sqlite phrase\"");
            }
            other => panic!("unexpected command: {other:?}"),
        }
    }

    #[test]
    fn search_cli_rejects_conflicting_scope_flags() {
        let error =
            Cli::try_parse_from(["orgfdb", "search", "--json", "--title", "--body", "sqlite"])
                .expect_err("conflicting scope flags should fail");
        assert_eq!(error.kind(), clap::error::ErrorKind::ArgumentConflict);
    }

    #[test]
    fn search_cli_rejects_unexpected_extra_expression_arguments() {
        let error = Cli::try_parse_from(["orgfdb", "search", "--json", "sqlite", "extra"])
            .expect_err("multiple expression args should fail");
        assert_eq!(error.kind(), clap::error::ErrorKind::UnknownArgument);
    }

    #[test]
    fn search_requires_json_flag() {
        let error = search_json_rows(false, CliSearchScope::All, "sqlite", None)
            .expect_err("search should require json");
        assert!(matches!(error, CliError::MissingJsonFlag("search")));
    }

    #[test]
    fn search_rejects_empty_expression() {
        let error = search_json_rows(true, CliSearchScope::All, "   ", None)
            .expect_err("empty expression should fail");
        match error {
            CliError::InvalidSearchUsage(message) => {
                assert!(message.contains("non-empty FTS expression"));
            }
            other => panic!("unexpected error: {other}"),
        }
    }

    #[test]
    fn search_returns_json_rows_from_canonical_tables() {
        let (_test_dir, config_path, db_path) = build_search_fixture(
            "search-canonical",
            &[(
                "notes.org",
                "* Searchable Heading\nBody phrase for sqlite search.\n",
            )],
            true,
        );

        let connection = open_database(&db_path).expect("database should open");
        connection
            .execute(
                "UPDATE headings SET title = 'Canonical Override' WHERE level > 0",
                [],
            )
            .expect("canonical title should update");
        drop(connection);

        let rows = search_json_rows(true, CliSearchScope::All, "Searchable", Some(&config_path))
            .expect("search should succeed");
        assert_eq!(rows.len(), 1);
        assert_eq!(rows[0].title, "Canonical Override");
        assert_eq!(
            rows[0].path,
            db_path
                .parent()
                .unwrap()
                .join("notes.org")
                .display()
                .to_string()
        );
        assert_eq!(rows[0].line_number, Some(1));
        assert_eq!(rows[0].byte_start, 0);
        assert!(rows[0].byte_end >= rows[0].byte_start);
    }

    #[test]
    fn search_supports_default_title_and_body_matching() {
        let (_test_dir, config_path, _) = build_search_fixture(
            "search-default-scope",
            &[(
                "notes.org",
                "* Searchable Heading\nBody phrase for sqlite search.\n",
            )],
            true,
        );

        let title_rows =
            search_json_rows(true, CliSearchScope::All, "Searchable", Some(&config_path))
                .expect("title search should succeed");
        let body_rows = search_json_rows(true, CliSearchScope::All, "phrase", Some(&config_path))
            .expect("body search should succeed");

        assert_eq!(title_rows.len(), 1);
        assert_eq!(body_rows.len(), 1);
    }

    #[test]
    fn search_enforces_title_and_body_scopes() {
        let (_test_dir, config_path, _) = build_search_fixture(
            "search-scopes",
            &[(
                "notes.org",
                "* Searchable Heading\nBody phrase for sqlite search.\n",
            )],
            true,
        );

        let title_rows = search_json_rows(
            true,
            CliSearchScope::Title,
            "Searchable",
            Some(&config_path),
        )
        .expect("title scope should succeed");
        let no_body_rows =
            search_json_rows(true, CliSearchScope::Title, "phrase", Some(&config_path))
                .expect("title scope should return empty on body term");
        let body_rows = search_json_rows(true, CliSearchScope::Body, "phrase", Some(&config_path))
            .expect("body scope should succeed");

        assert_eq!(title_rows.len(), 1);
        assert!(no_body_rows.is_empty());
        assert_eq!(body_rows.len(), 1);
    }

    #[test]
    fn search_rejects_explicit_column_filters_when_scope_is_fixed() {
        let error = search_json_rows(true, CliSearchScope::Title, "body:sqlite", None)
            .expect_err("scoped explicit column filter should fail");
        match error {
            CliError::Search(SearchError::ScopedColumnFilter { scope }) => {
                assert_eq!(scope, "title");
            }
            other => panic!("unexpected error: {other}"),
        }
    }

    #[test]
    fn search_all_scope_allows_explicit_column_filters() {
        let (_test_dir, config_path, _) = build_search_fixture(
            "search-allows-column-filters",
            &[(
                "notes.org",
                "* Searchable Heading\nBody phrase for sqlite search.\n",
            )],
            true,
        );

        let title_rows = search_json_rows(
            true,
            CliSearchScope::All,
            "title:(Searchable)",
            Some(&config_path),
        )
        .expect("title filter should succeed");
        let body_rows = search_json_rows(
            true,
            CliSearchScope::All,
            "body:(phrase)",
            Some(&config_path),
        )
        .expect("body filter should succeed");

        assert_eq!(title_rows.len(), 1);
        assert_eq!(body_rows.len(), 1);
    }

    #[test]
    fn search_rejects_body_scope_when_trusted_index_is_title_only() {
        let (_test_dir, config_path, _) = build_search_fixture(
            "search-title-only",
            &[(
                "notes.org",
                "* Searchable Heading\nBody phrase for sqlite search.\n",
            )],
            false,
        );

        let error = search_json_rows(true, CliSearchScope::Body, "phrase", Some(&config_path))
            .expect_err("body scope should fail for title-only index");
        assert!(matches!(
            error,
            CliError::Search(SearchError::BodyScopeUnavailable)
        ));
    }

    #[test]
    fn rebuild_persists_search_trust_metadata_for_search_command() {
        let (_test_dir, _config_path, db_path) = build_search_fixture(
            "search-trust-metadata",
            &[(
                "notes.org",
                "* Searchable Heading\nBody phrase for sqlite search.\n",
            )],
            true,
        );
        let connection = open_database(&db_path).expect("database should open");
        let rows: Vec<(String, String)> = {
            let mut statement = connection
                .prepare(
                    "SELECT key, value FROM db_metadata
                     WHERE key IN (?1, ?2, ?3)
                     ORDER BY key",
                )
                .expect("metadata query should prepare");
            statement
                .query_map(
                    [
                        DB_METADATA_FTS_AVAILABLE_KEY,
                        DB_METADATA_FTS_BODY_INDEXED_KEY,
                        DB_METADATA_FTS_SCHEMA_VERSION_KEY,
                    ],
                    |row| Ok((row.get(0)?, row.get(1)?)),
                )
                .expect("metadata query should run")
                .collect::<Result<Vec<_>, _>>()
                .expect("metadata rows should collect")
        };

        assert_eq!(
            rows,
            vec![
                (DB_METADATA_FTS_AVAILABLE_KEY.to_string(), "1".to_string()),
                (
                    DB_METADATA_FTS_BODY_INDEXED_KEY.to_string(),
                    "1".to_string(),
                ),
                (
                    DB_METADATA_FTS_SCHEMA_VERSION_KEY.to_string(),
                    FTS_SCHEMA_CONTRACT_VERSION.to_string(),
                ),
            ]
        );
    }

    #[test]
    fn search_rejects_missing_trust_metadata_and_requires_rebuild() {
        let test_dir = TestDir::new("search-missing-metadata");
        let db_path = test_dir.path().join("db.sqlite");
        let config_path = test_dir.path().join("config.toml");
        write_search_config(&config_path, "./db.sqlite", &[], true, true);

        let connection = open_database_with_schema(
            &db_path,
            &SchemaDefinition::new(CURRENT_SCHEMA_VERSION, true),
        )
        .expect("database should open");
        drop(connection);

        let error = search_json_rows(true, CliSearchScope::All, "sqlite", Some(&config_path))
            .expect_err("missing metadata should fail");
        match error {
            CliError::Search(SearchError::MissingTrustMetadata) => {}
            other => panic!("unexpected error: {other}"),
        }
        assert!(error.to_string().contains("run orgfdb rebuild"));
    }

    #[test]
    fn search_accepts_trusted_empty_fts_rebuilds() {
        let probe = Connection::open_in_memory().expect("probe should open");
        if !sqlite_supports_fts5(&probe).expect("fts5 probe should run") {
            return;
        }

        let test_dir = TestDir::new("search-empty-trusted-rebuild");
        let config_path = test_dir.path().join("config.toml");
        let db_path = test_dir.path().join("db.sqlite");
        write_file(
            &config_path,
            r#"
db_path = "./db.sqlite"

[search]
fts5_enabled = true
index_body_text = true
"#,
        );

        let report = rebuild(&config_path).expect("empty rebuild should succeed");
        assert!(report.indexed_files.is_empty());
        assert!(report.diagnostics.is_empty());

        let rows = search_json_rows(true, CliSearchScope::All, "sqlite", Some(&config_path))
            .expect("search should trust the empty rebuild");
        assert!(rows.is_empty());

        let connection = open_database(&db_path).expect("database should open");
        let metadata_rows: Vec<(String, String)> = {
            let mut statement = connection
                .prepare(
                    "SELECT key, value FROM db_metadata
                     WHERE key IN (?1, ?2, ?3)
                     ORDER BY key",
                )
                .expect("metadata query should prepare");
            statement
                .query_map(
                    [
                        DB_METADATA_FTS_AVAILABLE_KEY,
                        DB_METADATA_FTS_BODY_INDEXED_KEY,
                        DB_METADATA_FTS_SCHEMA_VERSION_KEY,
                    ],
                    |row| Ok((row.get(0)?, row.get(1)?)),
                )
                .expect("metadata query should run")
                .collect::<Result<Vec<_>, _>>()
                .expect("metadata rows should collect")
        };

        assert_eq!(
            metadata_rows,
            vec![
                (DB_METADATA_FTS_AVAILABLE_KEY.to_string(), "1".to_string()),
                (
                    DB_METADATA_FTS_BODY_INDEXED_KEY.to_string(),
                    "1".to_string(),
                ),
                (
                    DB_METADATA_FTS_SCHEMA_VERSION_KEY.to_string(),
                    FTS_SCHEMA_CONTRACT_VERSION.to_string(),
                ),
            ]
        );
    }

    #[test]
    fn search_rejects_disabled_config_even_when_index_is_trusted() {
        let (test_dir, trusted_config_path, _db_path) = build_search_fixture(
            "search-disabled-config",
            &[("notes.org", "* Searchable Heading\nBody phrase.\n")],
            true,
        );
        let disabled_config_path = test_dir.path().join("disabled.toml");
        write_search_config(
            &disabled_config_path,
            "./db.sqlite",
            &["notes.org"],
            false,
            true,
        );

        let error = search_json_rows(
            true,
            CliSearchScope::All,
            "Searchable",
            Some(&disabled_config_path),
        )
        .expect_err("disabled config should fail");
        assert!(matches!(
            error,
            CliError::Search(SearchError::DisabledByConfig)
        ));

        let trusted_rows = search_json_rows(
            true,
            CliSearchScope::All,
            "Searchable",
            Some(&trusted_config_path),
        )
        .expect("trusted config should still work");
        assert_eq!(trusted_rows.len(), 1);
    }

    #[test]
    fn search_rejects_missing_heading_fts_even_with_trusted_metadata() {
        let (_test_dir, config_path, db_path) = build_search_fixture(
            "search-missing-heading-fts",
            &[("notes.org", "* Searchable Heading\nBody phrase.\n")],
            true,
        );

        let connection = open_database(&db_path).expect("database should open");
        connection
            .execute_batch("DROP TABLE heading_fts;")
            .expect("fts table should drop");
        drop(connection);

        let error = search_json_rows(true, CliSearchScope::All, "Searchable", Some(&config_path))
            .expect_err("missing table should fail");
        assert!(matches!(
            error,
            CliError::Search(SearchError::MissingTrustedIndex)
        ));
    }

    #[test]
    fn search_rejects_incompatible_heading_fts_schema() {
        let (_test_dir, config_path, db_path) = build_search_fixture(
            "search-incompatible-heading-fts",
            &[("notes.org", "* Searchable Heading\nBody phrase.\n")],
            true,
        );

        let connection = open_database(&db_path).expect("database should open");
        connection
            .execute_batch(
                "DROP TABLE heading_fts;
                 CREATE TABLE heading_fts (title TEXT, body TEXT);",
            )
            .expect("incompatible fts table should install");
        drop(connection);

        let error = search_json_rows(true, CliSearchScope::All, "Searchable", Some(&config_path))
            .expect_err("incompatible schema should fail");
        assert!(matches!(
            error,
            CliError::Search(SearchError::IncompatibleIndexSchema)
        ));
    }

    #[test]
    fn search_rejects_invalid_fts_expression_without_raw_sqlite_leak() {
        let (_test_dir, config_path, _) = build_search_fixture(
            "search-invalid-expression",
            &[("notes.org", "* Searchable Heading\nBody phrase.\n")],
            true,
        );

        let error = search_json_rows(true, CliSearchScope::All, "AND", Some(&config_path))
            .expect_err("invalid expression should fail");
        match error {
            CliError::Search(SearchError::InvalidExpression { .. }) => {}
            other => panic!("unexpected error: {other}"),
        }
        assert_eq!(error.to_string(), "invalid SQLite FTS5 search expression");
    }

    #[test]
    fn search_is_read_only_and_does_not_scan_org_files() {
        let (test_dir, config_path, db_path) = build_search_fixture(
            "search-read-only",
            &[("notes.org", "* Searchable Heading\nBody phrase.\n")],
            true,
        );

        let writable = open_database(&db_path).expect("database should open");
        let version_before: u32 = writable
            .pragma_query_value(None, "user_version", |row| row.get(0))
            .expect("user_version should load");
        let metadata_before: Vec<(String, String)> = {
            let mut stmt = writable
                .prepare("SELECT key, value FROM db_metadata ORDER BY key")
                .expect("metadata query should prepare");
            stmt.query_map([], |row| Ok((row.get(0)?, row.get(1)?)))
                .expect("metadata query should run")
                .collect::<Result<Vec<_>, _>>()
                .expect("metadata rows should collect")
        };
        let heading_fts_rows_before: i64 = writable
            .query_row("SELECT COUNT(*) FROM heading_fts", [], |row| row.get(0))
            .expect("fts row count should load");
        drop(writable);

        fs::remove_file(test_dir.path().join("notes.org")).expect("source org file should delete");

        let rows = search_json_rows(true, CliSearchScope::All, "Searchable", Some(&config_path))
            .expect("search should succeed without source file");
        assert_eq!(rows.len(), 1);

        let reopened = Connection::open(&db_path).expect("database should reopen");
        let version_after: u32 = reopened
            .pragma_query_value(None, "user_version", |row| row.get(0))
            .expect("user_version should reload");
        let metadata_after: Vec<(String, String)> = {
            let mut stmt = reopened
                .prepare("SELECT key, value FROM db_metadata ORDER BY key")
                .expect("metadata query should prepare");
            stmt.query_map([], |row| Ok((row.get(0)?, row.get(1)?)))
                .expect("metadata query should run")
                .collect::<Result<Vec<_>, _>>()
                .expect("metadata rows should collect")
        };
        let heading_fts_rows_after: i64 = reopened
            .query_row("SELECT COUNT(*) FROM heading_fts", [], |row| row.get(0))
            .expect("fts row count should reload");

        assert_eq!(version_after, version_before);
        assert_eq!(metadata_after, metadata_before);
        assert_eq!(heading_fts_rows_after, heading_fts_rows_before);
    }

    #[test]
    fn search_orders_equal_rank_results_deterministically_by_heading_id() {
        let (_test_dir, config_path, _) = build_search_fixture(
            "search-deterministic-order",
            &[
                ("a.org", "* Shared\nsqlite\n"),
                ("b.org", "* Shared\nsqlite\n"),
            ],
            true,
        );

        let rows = search_json_rows(true, CliSearchScope::All, "Shared", Some(&config_path))
            .expect("search should succeed");
        assert_eq!(rows.len(), 2);
        assert!(rows[0].rank <= rows[1].rank);
        if (rows[0].rank - rows[1].rank).abs() < f64::EPSILON {
            assert!(rows[0].heading_id < rows[1].heading_id);
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
    fn query_json_rejects_has_text_when_body_text_capability_is_disabled() {
        let test_dir = TestDir::new("query-has-text-disabled");
        let config_path = write_query_fixture(&test_dir);

        let error = super::query_json_response(
            true,
            "(headings (has-text \"sqlite\"))",
            super::CliQueryOutput::Flat,
            &[],
            Some(&config_path),
        )
        .expect_err("query should fail");

        assert!(matches!(error, CliError::QueryValidate(_)));
        assert_eq!(
            error.to_string(),
            "has-text requires body text to be available in the database (target headings, predicate has-text)"
        );
    }

    #[test]
    fn query_json_distinguishes_file_source_titles_from_fallback_titles() {
        let test_dir = TestDir::new("query-fallback-file-title");
        let config_path = test_dir.path().join("config.toml");
        let untitled_path = test_dir.path().join("no-title-set.org");

        write_file(&untitled_path, "* Heading\n");
        write_file(
            &config_path,
            r#"
db_path = "./db.sqlite"
files = ["./no-title-set.org"]

[search]
fts5_enabled = false
index_body_text = false
"#,
        );
        let report = rebuild(&config_path).expect("fixture rebuild should succeed");
        assert_eq!(report.indexed_files.len(), 1);

        let response = super::query_json_response(
            true,
            "(files (file-title \"no-title-set\" :exact t))",
            super::CliQueryOutput::Flat,
            &[super::CliQueryInclude::Path],
            Some(&config_path),
        )
        .expect("fallback file-title query should succeed");

        let json = serde_json::to_value(&response).expect("response should serialize");
        let file = &json["results"][0];
        assert_eq!(file["kind"], "file");
        assert_eq!(file["title"], "no-title-set");
        assert!(file["title_raw"].is_null());

        let path_entries = file["node_path"]
            .as_array()
            .expect("path include should serialize as array");
        assert_eq!(path_entries.len(), 1);
        assert!(path_entries[0]["title_raw"].is_null());
    }

    #[test]
    fn query_json_heading_title_root_matches_return_file_kind() {
        let test_dir = TestDir::new("query-title-root-match");
        let config_path = test_dir.path().join("config.toml");
        let org_path = test_dir.path().join("projects.org");
        let db_path = test_dir.path().join("org-files.sqlite");

        write_file(&org_path, "#+TITLE: Projects\n* Projects overview\n");
        write_file(
            &config_path,
            &format!(
                "db_path = {:?}\nfiles = [{:?}]\n\n[search]\nfts5_enabled = false\nindex_body_text = false\n",
                db_path, org_path
            ),
        );

        rebuild(&config_path).expect("rebuild should succeed");

        let response = super::query_json_response(
            true,
            "(headings (title \"Projects\" :exact t))",
            super::CliQueryOutput::Flat,
            &[],
            Some(&config_path),
        )
        .expect("root title query should succeed");

        let value: serde_json::Value =
            serde_json::to_value(&response).expect("json output should serialize");
        assert_eq!(value["target"], "headings");
        assert_eq!(value["results"].as_array().expect("results array").len(), 1);
        let file = &value["results"][0];
        assert_eq!(file["kind"], "file");
        assert_eq!(file["title"], "Projects");
    }

    #[test]
    fn query_json_heading_title_root_matches_fallback_file_titles() {
        let test_dir = TestDir::new("query-title-root-fallback");
        let config_path = test_dir.path().join("config.toml");
        let org_path = test_dir.path().join("no-title-set.org");

        write_file(&org_path, "* Heading\n");
        write_file(
            &config_path,
            r#"
db_path = "./db.sqlite"
files = ["./no-title-set.org"]

[search]
fts5_enabled = false
index_body_text = false
"#,
        );

        rebuild(&config_path).expect("rebuild should succeed");

        let response = super::query_json_response(
            true,
            "(headings (title \"no-title-set\" :exact t))",
            super::CliQueryOutput::Flat,
            &[],
            Some(&config_path),
        )
        .expect("fallback root title query should succeed");

        let value = serde_json::to_value(&response).expect("response should serialize");
        let file = &value["results"][0];
        assert_eq!(file["kind"], "file");
        assert_eq!(file["title"], "no-title-set");
        assert!(file["title_raw"].is_null());
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
                    title_raw: None,
                    todo_keyword: None,
                    todo_type: None,
                    priority: None,
                    scheduled_raw: None,
                    scheduled_ts: None,
                    scheduled_has_time: None,
                    deadline_raw: None,
                    deadline_ts: None,
                    deadline_has_time: None,
                    closed_raw: None,
                    closed_ts: None,
                    closed_has_time: None,
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
                    title_raw: Some("TODO [#A] Inbox".to_string()),
                    todo_keyword: Some("TODO".to_string()),
                    todo_type: Some("open".to_string()),
                    priority: Some('A'),
                    scheduled_raw: None,
                    scheduled_ts: None,
                    scheduled_has_time: None,
                    deadline_raw: None,
                    deadline_ts: None,
                    deadline_has_time: None,
                    closed_raw: None,
                    closed_ts: None,
                    closed_has_time: None,
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
                    title_raw: None,
                    todo_keyword: None,
                    todo_type: None,
                    priority: None,
                    scheduled_raw: None,
                    scheduled_ts: None,
                    scheduled_has_time: None,
                    deadline_raw: None,
                    deadline_ts: None,
                    deadline_has_time: None,
                    closed_raw: None,
                    closed_ts: None,
                    closed_has_time: None,
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
                    title_raw: Some("Inbox".to_string()),
                    todo_keyword: Some("TODO".to_string()),
                    todo_type: Some("open".to_string()),
                    priority: Some('A'),
                    scheduled_raw: None,
                    scheduled_ts: None,
                    scheduled_has_time: None,
                    deadline_raw: None,
                    deadline_ts: None,
                    deadline_has_time: None,
                    closed_raw: None,
                    closed_ts: None,
                    closed_has_time: None,
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
                        title_raw: None,
                        todo_keyword: None,
                        todo_type: None,
                        priority: None,
                        scheduled_raw: None,
                        scheduled_ts: None,
                        scheduled_has_time: None,
                        deadline_raw: None,
                        deadline_ts: None,
                        deadline_has_time: None,
                        closed_raw: None,
                        closed_ts: None,
                        closed_has_time: None,
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
                        title_raw: Some("Heading".to_string()),
                        todo_keyword: None,
                        todo_type: None,
                        priority: None,
                        scheduled_raw: None,
                        scheduled_ts: None,
                        scheduled_has_time: None,
                        deadline_raw: None,
                        deadline_ts: None,
                        deadline_has_time: None,
                        closed_raw: None,
                        closed_ts: None,
                        closed_has_time: None,
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
        assert_eq!(
            include_root_rows[0].title_raw.as_deref(),
            Some("Minimal Slice")
        );
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
                        title_raw: None,
                        todo_keyword: None,
                        todo_type: None,
                        priority: None,
                        scheduled_raw: None,
                        scheduled_ts: None,
                        scheduled_has_time: None,
                        deadline_raw: None,
                        deadline_ts: None,
                        deadline_has_time: None,
                        closed_raw: None,
                        closed_ts: None,
                        closed_has_time: None,
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
                        title_raw: Some("Heading".to_string()),
                        todo_keyword: None,
                        todo_type: None,
                        priority: None,
                        scheduled_raw: None,
                        scheduled_ts: None,
                        scheduled_has_time: None,
                        deadline_raw: None,
                        deadline_ts: None,
                        deadline_has_time: None,
                        closed_raw: None,
                        closed_ts: None,
                        closed_has_time: None,
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
                    title_raw: Some(root_title.to_string()),
                    todo_keyword: None,
                    todo_type: None,
                    priority: None,
                    scheduled_raw: None,
                    scheduled_ts: None,
                    scheduled_has_time: None,
                    deadline_raw: None,
                    deadline_ts: None,
                    deadline_has_time: None,
                    closed_raw: None,
                    closed_ts: None,
                    closed_has_time: None,
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
                    title_raw: Some(child_title.to_string()),
                    todo_keyword: None,
                    todo_type: None,
                    priority: None,
                    scheduled_raw: None,
                    scheduled_ts: None,
                    scheduled_has_time: None,
                    deadline_raw: None,
                    deadline_ts: None,
                    deadline_has_time: None,
                    closed_raw: None,
                    closed_ts: None,
                    closed_has_time: None,
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

    #[test]
    fn docs_cli_query_reference_mentions_public_predicates_includes_and_options() {
        const CLI_DOCS: &str = include_str!("../docs/cli.org");

        for predicate in [
            "todo",
            "done",
            "title",
            "has-text",
            "level",
            "priority",
            "tags",
            "tags-all",
            "property",
            "keyword",
            "file-name",
            "file-path",
            "file-dir",
            "file-title",
            "file-modified",
            "outline-contains",
            "outline-sequence",
            "ts",
            "ts-active",
            "ts-inactive",
            "deadline",
            "scheduled",
            "closed",
            "planning",
            "parent",
            "children",
            "ancestors",
            "descendants",
            "has-link",
            "links-to",
            "linked-from",
            "link-type",
            "link-target",
            "link-description",
            "has-description",
            "status",
            "source",
            "target",
        ] {
            assert!(
                CLI_DOCS.contains(&format!("~{predicate}~")),
                "docs/cli.org should mention predicate {predicate}"
            );
        }

        for include in [
            "path",
            "properties",
            "keywords",
            "links",
            "backlinks",
            "source",
            "target",
        ] {
            assert!(
                CLI_DOCS.contains(&format!("~{include}~")),
                "docs/cli.org should mention include {include}"
            );
        }

        for option in [
            ":exact",
            ":regexp",
            ":match",
            ":inherit",
            ":without-root",
            ":from",
            ":to",
            ":on",
            ":with-time",
        ] {
            assert!(
                CLI_DOCS.contains(&format!("~{option}~")),
                "docs/cli.org should mention query option {option}"
            );
        }
    }
}
