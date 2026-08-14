use std::{
    error::Error,
    fmt, fs,
    io::{self, Read, Write},
    path::{Path, PathBuf},
    process::ExitCode,
};

use clap::{Args, Parser, Subcommand, ValueEnum};
use rusqlite::Connection;
use serde::Serialize;

use crate::{
    config::{Config, ConfigError},
    db::{
        open_existing_database_read_only, read_index_changes, read_index_state,
        read_schema_version, sqlite_supports_fts5, DbError, DbReader, HeadingListRow, IndexChanges,
        IndexStateReadError, LinkListRow, CURRENT_SCHEMA_VERSION, DB_METADATA_FTS_AVAILABLE_KEY,
        DB_METADATA_FTS_BODY_INDEXED_KEY, DB_METADATA_FTS_SCHEMA_VERSION_KEY,
        FTS_SCHEMA_CONTRACT_VERSION,
    },
    indexer::{Indexer, IndexerError, RebuildOptions, RebuildReport},
    parser::OrgizeAdapter,
    query::{
        execute_and_shape_query, parse_query, shape_matched_heading_nodes,
        sqlite_query_validation_options, validate_query, HeadingResultNode, QueryExecutionError,
        QueryExecutionOptions, QueryInclude, QueryOutputMode, QueryParseError, QueryResponse,
        QueryShapeError, QueryValidationError,
    },
    watcher_cli::{run_watch_command, WatcherCommandError},
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
        #[arg(
            long,
            help = "Accept changed configured directory-root identities for this manual rebuild"
        )]
        accept_source_root_changes: bool,
    },
    #[command(
        about = "Watch configured Org inputs and apply incremental updates",
        long_about = "Watch configured Org inputs and apply incremental reconciliations. Supported on Unix-like systems only. Routine activity is silent; lifecycle messages and errors are written to stderr."
    )]
    Watch {
        #[arg(long)]
        config: PathBuf,
    },
    Headings {
        #[command(flatten)]
        format: CliOutputArgs,
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
        #[command(flatten)]
        format: CliOutputArgs,
        #[arg(long)]
        config: Option<PathBuf>,
    },
    #[command(
        about = "Read the committed database identity, index generation, and canonical path"
    )]
    Status {
        #[command(flatten)]
        format: CliOutputArgs,
        #[arg(long)]
        config: Option<PathBuf>,
    },
    #[command(about = "Read committed affected-file changes after a generation")]
    Changes {
        #[command(flatten)]
        format: CliOutputArgs,
        #[arg(long)]
        database_id: String,
        #[arg(long)]
        since_generation: i64,
        #[arg(long)]
        config: Option<PathBuf>,
    },
    Query {
        #[command(flatten)]
        format: CliQueryFormatArgs,
        #[arg(long, value_enum, default_value_t = CliQueryOutput::Flat)]
        output: CliQueryOutput,
        #[arg(long, value_enum, value_delimiter = ',')]
        include: Vec<CliQueryInclude>,
        #[arg(
            long,
            value_name = "PATH_OR_DASH",
            help = "Read a JSON array of canonical file paths from PATH, or from stdin with '-'"
        )]
        restrict_files_json: Option<String>,
        #[arg(long)]
        config: Option<PathBuf>,
        #[arg(help = "Structural query expression, for example '(todo \"NEXT\")'")]
        query: String,
    },
    Search {
        #[command(flatten)]
        format: CliOutputArgs,
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

#[derive(Debug, Clone, Copy, Args)]
struct CliOutputArgs {
    #[arg(long, value_enum, default_value_t = CliOutputFormat::Json, conflicts_with = "json")]
    format: CliOutputFormat,
    #[arg(
        long,
        conflicts_with = "format",
        help = "Compatibility form for --format json"
    )]
    json: bool,
}

impl CliOutputArgs {
    fn selected(self) -> CliOutputFormat {
        if self.json {
            CliOutputFormat::Json
        } else {
            self.format
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, ValueEnum)]
enum CliOutputFormat {
    Json,
}

#[derive(Debug, Clone, Copy, Args)]
struct CliQueryFormatArgs {
    #[arg(
        long,
        value_enum,
        default_value_t = CliQueryOutputFormat::Json,
        conflicts_with = "json"
    )]
    format: CliQueryOutputFormat,
    #[arg(
        long,
        conflicts_with = "format",
        help = "Compatibility form for --format json"
    )]
    json: bool,
}

impl CliQueryFormatArgs {
    fn selected(self) -> CliQueryOutputFormat {
        if self.json {
            CliQueryOutputFormat::Json
        } else {
            self.format
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, ValueEnum)]
enum CliQueryOutputFormat {
    Json,
    PresentationJson,
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
    EffectiveProperties,
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
            CliQueryInclude::EffectiveProperties => Self::EffectiveProperties,
            CliQueryInclude::Keywords => Self::Keywords,
            CliQueryInclude::Links => Self::Links,
            CliQueryInclude::Backlinks => Self::Backlinks,
            CliQueryInclude::Source => Self::Source,
            CliQueryInclude::Target => Self::Target,
        }
    }
}

pub fn run() -> ExitCode {
    let stdout = io::stdout();
    let mut handle = stdout.lock();
    match run_with_args_and_writer(std::env::args_os(), &mut handle) {
        Ok(()) => ExitCode::SUCCESS,
        Err(error) => {
            eprintln!("{error}");
            ExitCode::from(error.exit_code())
        }
    }
}

fn run_with_args_and_writer<I, T, W>(args: I, writer: &mut W) -> Result<(), CliError>
where
    I: IntoIterator<Item = T>,
    T: Into<std::ffi::OsString> + Clone,
    W: Write,
{
    let cli = Cli::try_parse_from(args).map_err(CliError::Parse)?;
    match cli.command {
        Command::Rebuild {
            config,
            allow_empty,
            accept_source_root_changes,
        } => {
            let report = rebuild_with_options(&config, allow_empty, accept_source_root_changes)?;
            print_diagnostics(&report);
            Ok(())
        }
        Command::Watch { config } => {
            let config = Config::load_from_file(config).map_err(CliError::Config)?;
            let stderr = io::stderr();
            let mut handle = stderr.lock();
            run_watch_command(&config, &mut handle).map_err(CliError::Watcher)
        }
        Command::Headings {
            format,
            no_root,
            include_root,
            config,
        } => {
            let _deprecated_include_root = include_root;
            let output_format = format.selected();
            let rows = headings_json_rows(no_root, config.as_deref())?;
            write_output(output_format, writer, &rows)
        }
        Command::Links { format, config } => {
            let output_format = format.selected();
            let rows = links_json_rows(config.as_deref())?;
            write_output(output_format, writer, &rows)
        }
        Command::Status { format, config } => {
            let output_format = format.selected();
            let config = load_cli_config(config.as_deref())?;
            let connection =
                open_existing_database_read_only(&config.db_path).map_err(CliError::Database)?;
            let database_path = fs::canonicalize(&config.db_path).map_err(|source| {
                CliError::CanonicalizeDatabasePath {
                    path: config.db_path.clone(),
                    source,
                }
            })?;
            let schema_version = current_index_state_schema_version(&connection)?;
            let state = read_index_state(&connection).map_err(CliError::IndexState)?;
            let response = StatusJsonResponse {
                schema_version,
                database_path: database_path.display().to_string(),
                database_id: state.database_id,
                generation: state.generation,
                last_changed_at: state.last_changed_at,
            };
            write_output(output_format, writer, &response)
        }
        Command::Changes {
            format,
            database_id,
            since_generation,
            config,
        } => {
            let output_format = format.selected();
            let connection = open_cli_database(config.as_deref())?;
            let schema_version = current_index_state_schema_version(&connection)?;
            let changes = read_index_changes(&connection, &database_id, since_generation)
                .map_err(CliError::IndexState)?;
            let response = ChangesJsonResponse {
                schema_version,
                changes,
            };
            write_output(output_format, writer, &response)
        }
        Command::Query {
            format,
            output,
            include,
            restrict_files_json,
            config,
            query,
        } => {
            let output_format = format.selected();
            let response = if let Some(source) = restrict_files_json.as_deref() {
                let paths = read_restricted_file_paths(source)?;
                query_json_response_with_restriction(
                    &query,
                    output,
                    &include,
                    config.as_deref(),
                    Some(paths),
                )?
            } else {
                query_json_response(&query, output, &include, config.as_deref())?
            };
            write_query_output(output_format, writer, &response)
        }
        Command::Search {
            format,
            title,
            body,
            config,
            expression,
        } => {
            let output_format = format.selected();
            let rows = search_json_rows(
                cli_search_scope(title, body),
                &expression,
                config.as_deref(),
            )?;
            write_output(output_format, writer, &rows)
        }
    }
}

#[cfg(test)]
fn rebuild(config_path: impl AsRef<std::path::Path>) -> Result<RebuildReport, CliError> {
    rebuild_with_options(config_path, false, false)
}

fn rebuild_with_options(
    config_path: impl AsRef<std::path::Path>,
    allow_empty: bool,
    accept_source_root_changes: bool,
) -> Result<RebuildReport, CliError> {
    Indexer::new(OrgizeAdapter::new())
        .rebuild_from_config_path_with_rebuild_options(
            config_path,
            RebuildOptions {
                allow_empty,
                accept_source_root_changes,
            },
        )
        .map_err(CliError::Indexer)
}

fn headings_json_rows(
    exclude_root: bool,
    config_path: Option<&Path>,
) -> Result<Vec<HeadingJsonRow>, CliError> {
    let connection = open_cli_database(config_path)?;
    headings_rows_for_json(&connection, exclude_root)
}

fn links_json_rows(config_path: Option<&Path>) -> Result<Vec<LinkJsonRow>, CliError> {
    let connection = open_cli_database(config_path)?;
    links_rows_for_json(&connection)
}

fn query_json_response(
    query: &str,
    output: CliQueryOutput,
    includes: &[CliQueryInclude],
    config_path: Option<&Path>,
) -> Result<QueryResponse, CliError> {
    query_json_response_with_restriction(query, output, includes, config_path, None)
}

fn query_json_response_with_restriction(
    query: &str,
    output: CliQueryOutput,
    includes: &[CliQueryInclude],
    config_path: Option<&Path>,
    restricted_file_paths: Option<Vec<String>>,
) -> Result<QueryResponse, CliError> {
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
        restricted_file_paths,
    };
    execute_and_shape_query(&connection, &validated, &options).map_err(CliError::QueryShape)
}

fn read_restricted_file_paths(source: &str) -> Result<Vec<String>, CliError> {
    let content = if source == "-" {
        let mut content = String::new();
        io::stdin()
            .read_to_string(&mut content)
            .map_err(|source| CliError::ReadRestriction {
                location: "stdin".to_string(),
                source,
            })?;
        content
    } else {
        fs::read_to_string(source).map_err(|error| CliError::ReadRestriction {
            location: source.to_string(),
            source: error,
        })?
    };
    let paths = serde_json::from_str::<Vec<String>>(&content).map_err(CliError::RestrictionJson)?;
    if paths.iter().any(|path| path.is_empty()) {
        return Err(CliError::InvalidRestriction(
            "restricted file paths must not be empty".to_string(),
        ));
    }
    Ok(paths
        .into_iter()
        .collect::<std::collections::BTreeSet<_>>()
        .into_iter()
        .collect())
}

#[derive(Debug, Serialize)]
struct StatusJsonResponse {
    schema_version: u32,
    database_path: String,
    database_id: String,
    generation: i64,
    last_changed_at: String,
}

#[derive(Debug, Serialize)]
struct ChangesJsonResponse {
    schema_version: u32,
    #[serde(flatten)]
    changes: IndexChanges,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum CliSearchScope {
    All,
    Title,
    Body,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct ProductionSearchResultKey {
    pub(crate) file_path: String,
    pub(crate) byte_start: Option<i64>,
    pub(crate) rank_bits: u64,
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
    scope: CliSearchScope,
    expression: &str,
    config_path: Option<&Path>,
) -> Result<Vec<SearchJsonRow>, CliError> {
    let config = load_cli_config(config_path)?;
    search_json_rows_for_config(scope, expression, &config)
}

fn search_json_rows_for_config(
    scope: CliSearchScope,
    expression: &str,
    config: &Config,
) -> Result<Vec<SearchJsonRow>, CliError> {
    // Validate purely syntactic usage before opening the configured database.
    // This preserves the CLI error contract for invalid input even when the
    // configured database does not exist yet.
    if expression.trim().is_empty() {
        return Err(CliError::InvalidSearchUsage(
            "search requires a non-empty FTS expression".to_string(),
        ));
    }
    let _ = compile_search_expression(scope, expression)?;
    let connection =
        open_existing_database_read_only(&config.db_path).map_err(CliError::Database)?;
    production_search_rows_with_connection(&connection, scope, expression, config)
}

pub(crate) fn production_search_result_count_with_connection(
    connection: &Connection,
    scope: CliSearchScope,
    expression: &str,
    config: &Config,
) -> Result<usize, String> {
    production_search_rows_with_connection(connection, scope, expression, config)
        .map(|rows| rows.len())
        .map_err(|error| error.to_string())
}

pub(crate) fn production_search_stable_results_with_connection(
    connection: &Connection,
    scope: CliSearchScope,
    expression: &str,
    config: &Config,
) -> Result<Vec<ProductionSearchResultKey>, String> {
    let mut rows = production_search_rows_with_connection(connection, scope, expression, config)
        .map_err(|error| error.to_string())?;
    rows.sort_by(|left, right| {
        left.rank
            .total_cmp(&right.rank)
            .then_with(|| {
                left.heading
                    .location
                    .file_path
                    .cmp(&right.heading.location.file_path)
            })
            .then_with(|| {
                left.heading
                    .location
                    .byte_start
                    .cmp(&right.heading.location.byte_start)
            })
    });
    Ok(rows
        .into_iter()
        .map(|row| ProductionSearchResultKey {
            file_path: row.heading.location.file_path,
            byte_start: row.heading.location.byte_start,
            rank_bits: row.rank.to_bits(),
        })
        .collect())
}

fn production_search_rows_with_connection(
    connection: &Connection,
    scope: CliSearchScope,
    expression: &str,
    config: &Config,
) -> Result<Vec<SearchJsonRow>, CliError> {
    if expression.trim().is_empty() {
        return Err(CliError::InvalidSearchUsage(
            "search requires a non-empty FTS expression".to_string(),
        ));
    }
    let compiled_expression = compile_search_expression(scope, expression)?;
    if !config.search.fts5_enabled {
        return Err(CliError::Search(SearchError::DisabledByConfig));
    }
    match sqlite_supports_fts5(connection) {
        Ok(true) => {}
        Ok(false) => return Err(CliError::Search(SearchError::FtsUnavailable)),
        Err(source) => {
            return Err(CliError::Search(SearchError::Inspect {
                operation: "probe SQLite FTS5 support",
                source,
            }))
        }
    }
    let trust = inspect_search_backend_state(connection)?;
    if scope == CliSearchScope::Body && !trust.body_indexed {
        return Err(CliError::Search(SearchError::BodyScopeUnavailable));
    }
    let search_rows = DbReader::search_headings(connection, &compiled_expression)
        .map_err(map_search_db_read_error)?;
    let heading_ids = search_rows
        .iter()
        .map(|row| row.heading_id)
        .collect::<Vec<_>>();
    let headings =
        shape_matched_heading_nodes(connection, &heading_ids).map_err(CliError::QueryShape)?;
    Ok(search_rows
        .into_iter()
        .zip(headings)
        .map(|(row, heading)| SearchJsonRow::from_parts(heading, row.rank))
        .collect())
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
    let Ok(fts_schema_version) = fts_schema_version.parse::<u64>() else {
        return Err(CliError::Search(SearchError::InvalidTrustMetadata));
    };
    let current_fts_schema_version = FTS_SCHEMA_CONTRACT_VERSION
        .parse::<u64>()
        .expect("FTS schema contract version must be a non-negative integer");
    if fts_available != "1" || fts_schema_version != current_fts_schema_version {
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

fn current_index_state_schema_version(connection: &Connection) -> Result<u32, CliError> {
    let version = read_schema_version(connection).map_err(CliError::SchemaInspect)?;
    if version != CURRENT_SCHEMA_VERSION {
        return Err(CliError::UnsupportedIndexStateSchema {
            on_disk_version: version,
            required_version: CURRENT_SCHEMA_VERSION,
        });
    }
    Ok(version)
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

fn write_output<T: Serialize>(
    format: CliOutputFormat,
    writer: &mut impl Write,
    value: &T,
) -> Result<(), CliError> {
    match format {
        CliOutputFormat::Json => write_json_output(writer, value),
    }
}

fn write_query_output(
    format: CliQueryOutputFormat,
    writer: &mut impl Write,
    value: &QueryResponse,
) -> Result<(), CliError> {
    match format {
        CliQueryOutputFormat::Json => write_json_output(writer, value),
        CliQueryOutputFormat::PresentationJson => Err(CliError::PresentationOutputUnavailable),
    }
}

fn write_json_output<T: Serialize>(writer: &mut impl Write, value: &T) -> Result<(), CliError> {
    serde_json::to_writer_pretty(&mut *writer, value).map_err(CliError::Json)?;
    writer.write_all(b"\n").map_err(CliError::Io)
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
    InvalidSearchUsage(String),
    Config(ConfigError),
    Database(DbError),
    DbRead(crate::db::DbReadError),
    Indexer(IndexerError),
    Watcher(WatcherCommandError),
    QueryParse(QueryParseError),
    QueryValidate(QueryValidationError),
    QueryExecute(QueryExecutionError),
    QueryShape(QueryShapeError),
    Search(SearchError),
    IndexState(IndexStateReadError),
    SchemaInspect(rusqlite::Error),
    UnsupportedIndexStateSchema {
        on_disk_version: u32,
        required_version: u32,
    },
    CanonicalizeDatabasePath {
        path: PathBuf,
        source: io::Error,
    },
    ReadRestriction {
        location: String,
        source: io::Error,
    },
    RestrictionJson(serde_json::Error),
    InvalidRestriction(String),
    PresentationOutputUnavailable,
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
            Self::Parse(_) | Self::InvalidSearchUsage(_) => 2,
            Self::Config(_)
            | Self::Database(_)
            | Self::DbRead(_)
            | Self::Indexer(_)
            | Self::Watcher(_)
            | Self::QueryParse(_)
            | Self::QueryValidate(_)
            | Self::QueryExecute(_)
            | Self::QueryShape(_)
            | Self::Search(_)
            | Self::IndexState(_)
            | Self::SchemaInspect(_)
            | Self::UnsupportedIndexStateSchema { .. }
            | Self::CanonicalizeDatabasePath { .. }
            | Self::ReadRestriction { .. }
            | Self::RestrictionJson(_)
            | Self::InvalidRestriction(_)
            | Self::PresentationOutputUnavailable
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
            Self::InvalidSearchUsage(message) => write!(f, "{message}"),
            Self::Config(source) => write!(f, "{source}"),
            Self::Database(source) => write!(f, "{source}"),
            Self::DbRead(source) => write!(f, "{source}"),
            Self::Indexer(source) => write!(f, "{source}"),
            Self::Watcher(source) => write!(f, "{source}"),
            Self::QueryParse(source) => write!(f, "{source}"),
            Self::QueryValidate(source) => write!(f, "{source}"),
            Self::QueryExecute(source) => write!(f, "{source}"),
            Self::QueryShape(source) => write!(f, "{source}"),
            Self::Search(source) => write!(f, "{source}"),
            Self::IndexState(source) => write!(f, "{source}"),
            Self::SchemaInspect(source) => write!(f, "failed to read database schema version: {source}"),
            Self::UnsupportedIndexStateSchema {
                on_disk_version,
                required_version,
            } => write!(
                f,
                "database schema version {on_disk_version} does not support index state; run an indexing command to migrate it to version {required_version}"
            ),
            Self::CanonicalizeDatabasePath { path, source } => write!(
                f,
                "failed to canonicalize configured database path {}: {source}",
                path.display()
            ),
            Self::ReadRestriction { location, source } => {
                write!(f, "failed to read restricted file paths from {location}: {source}")
            }
            Self::RestrictionJson(source) => {
                write!(f, "failed to parse restricted file paths as JSON: {source}")
            }
            Self::InvalidRestriction(message) => write!(f, "invalid file restriction: {message}"),
            Self::PresentationOutputUnavailable => write!(
                f,
                "presentation-json output is not available until presentation specification support is implemented"
            ),
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
            Self::InvalidSearchUsage(_) => None,
            Self::Config(source) => Some(source),
            Self::Database(source) => Some(source),
            Self::DbRead(source) => Some(source),
            Self::Indexer(source) => Some(source),
            Self::Watcher(source) => Some(source),
            Self::QueryParse(source) => Some(source),
            Self::QueryValidate(source) => Some(source),
            Self::QueryExecute(source) => Some(source),
            Self::QueryShape(source) => Some(source),
            Self::Search(source) => Some(source),
            Self::IndexState(source) => Some(source),
            Self::SchemaInspect(source) => Some(source),
            Self::UnsupportedIndexStateSchema { .. } => None,
            Self::CanonicalizeDatabasePath { source, .. } => Some(source),
            Self::ReadRestriction { source, .. } => Some(source),
            Self::RestrictionJson(source) => Some(source),
            Self::InvalidRestriction(_) | Self::PresentationOutputUnavailable => None,
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
    priority: Option<String>,
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
    #[serde(flatten)]
    heading: HeadingResultNode,
    rank: f64,
}

impl TryFrom<HeadingListRow> for HeadingJsonRow {
    type Error = CliError;

    fn try_from(row: HeadingListRow) -> Result<Self, Self::Error> {
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
            all_tags: row.all_tags,
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
    fn from_parts(heading: HeadingResultNode, rank: f64) -> Self {
        Self { heading, rank }
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
    use super::{
        rebuild, run_with_args_and_writer, search_json_rows, Cli, CliError, CliSearchScope,
        SearchError,
    };
    use crate::db::{
        open_database, open_database_with_schema, open_in_memory_database_with_schema,
        sqlite_supports_fts5, DbError, DbWriter, EffectiveTagRecord, FileRecordInput,
        HeadingRecord, LinkRecord, OutlinePathRecord, SchemaDefinition, TagRecord,
        CURRENT_SCHEMA_VERSION, DB_METADATA_FTS_AVAILABLE_KEY, DB_METADATA_FTS_BODY_INDEXED_KEY,
        DB_METADATA_FTS_SCHEMA_VERSION_KEY, FTS_SCHEMA_CONTRACT_VERSION,
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

    fn seed_single_heading_tag(
        connection: &Connection,
        file_id: i64,
        heading_id: i64,
        tag: &str,
    ) -> Result<(), crate::db::DbWriteError> {
        DbWriter::insert_tags(
            connection,
            &[TagRecord {
                heading_id,
                tag: tag.to_string(),
            }],
        )?;
        DbWriter::insert_effective_tags(
            connection,
            &[EffectiveTagRecord {
                heading_id,
                file_id,
                tag: tag.to_string(),
                position: 0,
            }],
        )
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

    fn run_cli_output(args: Vec<String>) -> Result<Vec<u8>, CliError> {
        let mut output = Vec::new();
        run_with_args_and_writer(args, &mut output)?;
        Ok(output)
    }

    fn assert_equivalent_json_output(
        implicit: Vec<String>,
        explicit_json: Vec<String>,
        explicit_format: Vec<String>,
    ) {
        let implicit_output =
            run_cli_output(implicit).expect("implicit JSON command should succeed");
        let json_output =
            run_cli_output(explicit_json).expect("explicit --json command should succeed");
        let format_output =
            run_cli_output(explicit_format).expect("explicit --format command should succeed");

        assert_eq!(implicit_output, json_output);
        assert_eq!(implicit_output, format_output);
    }

    fn cli_error_summary(args: Vec<String>) -> (u8, String) {
        let error = run_cli_output(args).expect_err("command should fail");
        (error.exit_code(), error.to_string())
    }

    fn assert_equivalent_cli_error(
        implicit: Vec<String>,
        explicit_json: Vec<String>,
        explicit_format: Vec<String>,
    ) {
        let implicit_error = cli_error_summary(implicit);
        assert_eq!(implicit_error, cli_error_summary(explicit_json));
        assert_eq!(implicit_error, cli_error_summary(explicit_format));
    }

    #[test]
    fn parses_rebuild_and_headings_arguments() {
        let cli = Cli::try_parse_from(["orgfdb", "rebuild", "--config", "config.toml"])
            .expect("rebuild args should parse");

        match cli.command {
            super::Command::Rebuild {
                config,
                allow_empty,
                accept_source_root_changes,
            } => {
                assert_eq!(config, PathBuf::from("config.toml"));
                assert!(!allow_empty);
                assert!(!accept_source_root_changes);
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
                accept_source_root_changes,
            } => {
                assert_eq!(config, PathBuf::from("config.toml"));
                assert!(allow_empty);
                assert!(!accept_source_root_changes);
            }
            other => panic!("unexpected command: {other:?}"),
        }

        let cli = Cli::try_parse_from([
            "orgfdb",
            "rebuild",
            "--config",
            "config.toml",
            "--accept-source-root-changes",
        ])
        .expect("rebuild root-acceptance args should parse");

        match cli.command {
            super::Command::Rebuild {
                config,
                allow_empty,
                accept_source_root_changes,
            } => {
                assert_eq!(config, PathBuf::from("config.toml"));
                assert!(!allow_empty);
                assert!(accept_source_root_changes);
            }
            other => panic!("unexpected command: {other:?}"),
        }

        let cli = Cli::try_parse_from(["orgfdb", "headings", "--json"])
            .expect("headings args should parse");

        match cli.command {
            super::Command::Headings {
                format,
                no_root,
                include_root,
                ..
            } => {
                assert!(format.json);
                assert_eq!(format.selected(), super::CliOutputFormat::Json);
                assert!(!no_root);
                assert!(!include_root);
            }
            other => panic!("unexpected command: {other:?}"),
        }

        let cli = Cli::try_parse_from(["orgfdb", "headings", "--json", "--include-root"])
            .expect("headings include-root args should parse");

        match cli.command {
            super::Command::Headings {
                format,
                no_root,
                include_root,
                ..
            } => {
                assert!(format.json);
                assert!(!no_root);
                assert!(include_root);
            }
            other => panic!("unexpected command: {other:?}"),
        }

        let cli = Cli::try_parse_from(["orgfdb", "headings", "--json", "--no-root"])
            .expect("headings no-root args should parse");

        match cli.command {
            super::Command::Headings {
                format,
                no_root,
                include_root,
                ..
            } => {
                assert!(format.json);
                assert!(no_root);
                assert!(!include_root);
            }
            other => panic!("unexpected command: {other:?}"),
        }

        let cli = Cli::try_parse_from(["orgfdb", "headings", "--json", "--config", "config.toml"])
            .expect("headings config args should parse");

        match cli.command {
            super::Command::Headings {
                format,
                no_root,
                include_root,
                config,
                ..
            } => {
                assert!(format.json);
                assert!(!no_root);
                assert!(!include_root);
                assert_eq!(config, Some(PathBuf::from("config.toml")));
            }
            other => panic!("unexpected command: {other:?}"),
        }

        let cli =
            Cli::try_parse_from(["orgfdb", "links", "--json"]).expect("links args should parse");

        match cli.command {
            super::Command::Links { format, config } => {
                assert!(format.json);
                assert_eq!(config, None);
            }
            other => panic!("unexpected command: {other:?}"),
        }

        let cli = Cli::try_parse_from(["orgfdb", "links", "--json", "--config", "config.toml"])
            .expect("links config args should parse");

        match cli.command {
            super::Command::Links { format, config } => {
                assert!(format.json);
                assert_eq!(config, Some(PathBuf::from("config.toml")));
            }
            other => panic!("unexpected command: {other:?}"),
        }

        let cli = Cli::try_parse_from([
            "orgfdb",
            "status",
            "--format",
            "json",
            "--config",
            "config.toml",
        ])
        .expect("status args should parse");
        match cli.command {
            super::Command::Status { format, config } => {
                assert_eq!(format.selected(), super::CliOutputFormat::Json);
                assert_eq!(config, Some(PathBuf::from("config.toml")));
            }
            other => panic!("unexpected command: {other:?}"),
        }

        let cli = Cli::try_parse_from([
            "orgfdb",
            "changes",
            "--database-id",
            "database-id",
            "--since-generation",
            "42",
            "--config",
            "config.toml",
        ])
        .expect("changes args should parse");
        match cli.command {
            super::Command::Changes {
                database_id,
                since_generation,
                config,
                ..
            } => {
                assert_eq!(database_id, "database-id");
                assert_eq!(since_generation, 42);
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
            "--restrict-files-json",
            "paths.json",
            "(todo \"NEXT\")",
        ])
        .expect("query args should parse");

        match cli.command {
            super::Command::Query {
                format,
                output,
                include,
                restrict_files_json,
                config,
                query,
            } => {
                assert!(format.json);
                assert_eq!(format.selected(), super::CliQueryOutputFormat::Json);
                assert_eq!(output, super::CliQueryOutput::Outline);
                assert_eq!(
                    include,
                    vec![
                        super::CliQueryInclude::Path,
                        super::CliQueryInclude::Links,
                        super::CliQueryInclude::Path,
                    ]
                );
                assert_eq!(restrict_files_json.as_deref(), Some("paths.json"));
                assert_eq!(config, None);
                assert_eq!(query, "(todo \"NEXT\")");
            }
            other => panic!("unexpected command: {other:?}"),
        }

        let cli = Cli::try_parse_from(["orgfdb", "search", "--json", "sqlite"])
            .expect("search args should parse");
        match cli.command {
            super::Command::Search {
                format,
                title,
                body,
                config,
                expression,
            } => {
                assert!(format.json);
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
                format,
                title,
                body,
                config,
                expression,
            } => {
                assert!(format.json);
                assert!(title);
                assert!(!body);
                assert_eq!(config, Some(PathBuf::from("config.toml")));
                assert_eq!(expression, "\"sqlite phrase\"");
            }
            other => panic!("unexpected command: {other:?}"),
        }
    }

    #[test]
    fn manual_rebuild_flag_accepts_an_intentional_root_replacement() {
        let test_dir = TestDir::new("rebuild-accept-root-change");
        let config_path = test_dir.path().join("config.toml");
        let root = test_dir.path().join("notes");
        write_file(&root.join("old.org"), "* Old\n");
        write_file(
            &config_path,
            r#"db_path = "./db.sqlite"

[[dirs]]
path = "./notes"
recursive = true

[search]
fts5_enabled = false
"#,
        );
        let config_arg = config_path.to_string_lossy().into_owned();
        run_cli_output(vec![
            "orgfdb".to_string(),
            "rebuild".to_string(),
            "--config".to_string(),
            config_arg.clone(),
        ])
        .expect("initial rebuild should succeed");
        let _previous = test_dir.path().join("notes-previous");
        fs::rename(&root, &_previous).expect("original root should move aside");
        write_file(&root.join("new.org"), "* New\n");

        let error = run_cli_output(vec![
            "orgfdb".to_string(),
            "rebuild".to_string(),
            "--config".to_string(),
            config_arg.clone(),
        ])
        .expect_err("automatic rebuild must reject the replacement root");
        assert!(error.to_string().contains("--accept-source-root-changes"));

        run_cli_output(vec![
            "orgfdb".to_string(),
            "rebuild".to_string(),
            "--config".to_string(),
            config_arg,
            "--accept-source-root-changes".to_string(),
        ])
        .expect("manual acceptance should rebuild the replacement root");
        let connection =
            Connection::open(test_dir.path().join("db.sqlite")).expect("database should open");
        let titles = connection
            .prepare("SELECT title FROM headings WHERE level = 1 ORDER BY title")
            .expect("title query should prepare")
            .query_map([], |row| row.get::<_, String>(0))
            .expect("title query should execute")
            .collect::<Result<Vec<_>, _>>()
            .expect("titles should collect");
        assert_eq!(titles, vec!["New"]);
    }

    #[test]
    fn parses_watcher_arguments_and_documents_unix_scope() {
        let cli = Cli::try_parse_from(["orgfdb", "watch", "--config", "config.toml"])
            .expect("watch args should parse");

        match cli.command {
            super::Command::Watch { config } => {
                assert_eq!(config, PathBuf::from("config.toml"));
            }
            other => panic!("unexpected command: {other:?}"),
        }

        let error = Cli::try_parse_from(["orgfdb", "watch", "--help"])
            .expect_err("help should stop argument parsing");
        assert_eq!(error.kind(), clap::error::ErrorKind::DisplayHelp);
        let help = error.to_string();
        assert!(help.contains("Unix-like systems only"));
        assert!(help.contains("incremental reconciliations"));
        assert!(help.contains("--config <CONFIG>"));
    }

    #[test]
    fn watcher_registration_failure_returns_exit_code_one_with_path_context() {
        let test_dir = TestDir::new("watch-registration-failure");
        let config_path = test_dir.path().join("config.toml");
        write_file(
            &config_path,
            r#"db_path = "./db.sqlite"
files = ["missing/note.org"]
"#,
        );

        let error = run_cli_output(vec![
            "orgfdb".into(),
            "watch".into(),
            "--config".into(),
            config_path.display().to_string(),
        ])
        .expect_err("missing explicit-file parent should fail watch registration");

        assert_eq!(error.exit_code(), 1);
        let message = error.to_string();
        assert!(message.contains("watcher startup failed"));
        assert!(message.contains("missing"));
    }

    #[test]
    fn missing_configured_directory_returns_actionable_watcher_error() {
        let test_dir = TestDir::new("watch-missing-configured-directory");
        let config_path = test_dir.path().join("config.toml");
        write_file(
            &config_path,
            r#"db_path = "./db.sqlite"

[[dirs]]
path = "missing"
recursive = true
"#,
        );

        let error = run_cli_output(vec![
            "orgfdb".into(),
            "watch".into(),
            "--config".into(),
            config_path.display().to_string(),
        ])
        .expect_err("missing configured directory should prevent watcher startup");

        assert_eq!(error.exit_code(), 1);
        let message = error.to_string();
        assert!(message.contains("watcher startup failed"));
        assert!(message.contains("configured source directory does not exist"));
        assert!(message.contains(&test_dir.path().join("missing").display().to_string()));
        assert!(message.contains("restore the directory or update the configured path"));
    }

    #[test]
    fn watcher_startup_reconciliation_failure_returns_exit_code_one() {
        let test_dir = TestDir::new("watch-startup-failure");
        let config_path = test_dir.path().join("config.toml");
        let invalid_org = test_dir.path().join("invalid.org");
        fs::write(&invalid_org, [0xff, 0xfe, 0xfd]).expect("invalid UTF-8 file should be written");
        write_file(
            &config_path,
            r#"db_path = "./db.sqlite"
files = ["invalid.org"]

[search]
fts5_enabled = false
"#,
        );

        let error = run_cli_output(vec![
            "orgfdb".into(),
            "watch".into(),
            "--config".into(),
            config_path.display().to_string(),
        ])
        .expect_err("failed startup reconciliation should stop the watch command");

        assert_eq!(error.exit_code(), 1);
        assert!(error
            .to_string()
            .contains("watcher startup reconciliation failed"));
    }

    #[test]
    fn output_format_defaults_to_json_and_rejects_duplicate_selection() {
        let cli = Cli::try_parse_from(["orgfdb", "headings"]).expect("default format should parse");
        match cli.command {
            super::Command::Headings { format, .. } => {
                assert!(!format.json);
                assert_eq!(format.selected(), super::CliOutputFormat::Json);
            }
            other => panic!("unexpected command: {other:?}"),
        }

        let cli = Cli::try_parse_from(["orgfdb", "query", "(headings)"])
            .expect("query default format should parse");
        match cli.command {
            super::Command::Query { format, .. } => {
                assert!(!format.json);
                assert_eq!(format.selected(), super::CliQueryOutputFormat::Json);
            }
            other => panic!("unexpected command: {other:?}"),
        }

        for args in [
            vec!["orgfdb", "headings", "--json", "--format", "json"],
            vec!["orgfdb", "links", "--json", "--format", "json"],
            vec!["orgfdb", "query", "--json", "--format", "json", "(todo)"],
            vec![
                "orgfdb",
                "query",
                "--json",
                "--format",
                "presentation-json",
                "(todo)",
            ],
            vec!["orgfdb", "search", "--json", "--format", "json", "sqlite"],
            vec!["orgfdb", "headings", "--format", "json", "--format", "json"],
        ] {
            let error =
                Cli::try_parse_from(args).expect_err("duplicate format selection should fail");
            assert_eq!(error.kind(), clap::error::ErrorKind::ArgumentConflict);
        }

        let error = Cli::try_parse_from(["orgfdb", "headings", "--format", "invalid"])
            .expect_err("unknown output format should fail");
        assert_eq!(error.kind(), clap::error::ErrorKind::InvalidValue);
        assert_eq!(error.exit_code(), 2);
    }

    #[test]
    fn query_output_format_is_query_specific() {
        let cli = Cli::try_parse_from([
            "orgfdb",
            "query",
            "--format",
            "presentation-json",
            "(headings)",
        ])
        .expect("query presentation format should parse");
        match cli.command {
            super::Command::Query { format, .. } => {
                assert_eq!(
                    format.selected(),
                    super::CliQueryOutputFormat::PresentationJson
                );
            }
            other => panic!("unexpected command: {other:?}"),
        }

        for args in [
            vec!["orgfdb", "headings", "--format", "presentation-json"],
            vec!["orgfdb", "links", "--format", "presentation-json"],
            vec!["orgfdb", "status", "--format", "presentation-json"],
            vec![
                "orgfdb",
                "changes",
                "--format",
                "presentation-json",
                "--database-id",
                "database-id",
                "--since-generation",
                "0",
            ],
            vec![
                "orgfdb",
                "search",
                "--format",
                "presentation-json",
                "sqlite",
            ],
        ] {
            let error = Cli::try_parse_from(args)
                .expect_err("presentation-json should be rejected outside query");
            assert_eq!(error.kind(), clap::error::ErrorKind::InvalidValue);
        }
    }

    #[test]
    fn output_format_help_is_scoped_by_command() {
        for command in ["headings", "links", "status", "changes", "search"] {
            let error = Cli::try_parse_from(["orgfdb", command, "--help"])
                .expect_err("help should stop argument parsing");
            assert_eq!(error.kind(), clap::error::ErrorKind::DisplayHelp);
            let help = error.to_string();
            assert!(help.contains("--format <FORMAT>"));
            assert!(help.contains("[default: json]"));
            assert!(help.contains("--json"));
            assert!(!help.contains("presentation-json"));
        }

        let error = Cli::try_parse_from(["orgfdb", "query", "--help"])
            .expect_err("help should stop argument parsing");
        assert_eq!(error.kind(), clap::error::ErrorKind::DisplayHelp);
        let help = error.to_string();
        assert!(help.contains("--format <FORMAT>"));
        assert!(help.contains("[default: json]"));
        assert!(help.contains("presentation-json"));
        assert!(help.contains("--json"));
    }

    #[test]
    fn query_presentation_format_uses_shared_query_validation() {
        let test_dir = TestDir::new("presentation-format-validation");
        let config_path = write_query_fixture(&test_dir);
        let config = config_path.display().to_string();
        let json_error = cli_error_summary(vec![
            "orgfdb".into(),
            "query".into(),
            "--format".into(),
            "json".into(),
            "--config".into(),
            config.clone(),
            "(todo".into(),
        ]);
        let presentation_error = cli_error_summary(vec![
            "orgfdb".into(),
            "query".into(),
            "--format".into(),
            "presentation-json".into(),
            "--config".into(),
            config,
            "(todo".into(),
        ]);
        assert_eq!(json_error, presentation_error);
    }

    #[test]
    fn query_presentation_format_reports_staged_output_error_after_execution() {
        let test_dir = TestDir::new("presentation-format-placeholder");
        let config_path = write_query_fixture(&test_dir);
        let error = run_cli_output(vec![
            "orgfdb".into(),
            "query".into(),
            "--format".into(),
            "presentation-json".into(),
            "--config".into(),
            config_path.display().to_string(),
            "(headings)".into(),
        ])
        .expect_err("presentation output should remain unavailable before its wire model exists");

        assert!(matches!(&error, CliError::PresentationOutputUnavailable));
        assert_eq!(
            error.to_string(),
            "presentation-json output is not available until presentation specification support is implemented"
        );
    }

    #[test]
    fn implicit_and_explicit_json_forms_produce_identical_stdout() {
        let (_test_dir, config_path, _) = build_search_fixture(
            "output-format-equivalence",
            &[(
                "notes.org",
                "* TODO Searchable Heading\nBody phrase for sqlite search.\n",
            )],
            true,
        );
        let config = config_path.display().to_string();

        assert_equivalent_json_output(
            vec![
                "orgfdb".into(),
                "headings".into(),
                "--config".into(),
                config.clone(),
            ],
            vec![
                "orgfdb".into(),
                "headings".into(),
                "--json".into(),
                "--config".into(),
                config.clone(),
            ],
            vec![
                "orgfdb".into(),
                "headings".into(),
                "--format".into(),
                "json".into(),
                "--config".into(),
                config.clone(),
            ],
        );
        assert_equivalent_json_output(
            vec![
                "orgfdb".into(),
                "links".into(),
                "--config".into(),
                config.clone(),
            ],
            vec![
                "orgfdb".into(),
                "links".into(),
                "--json".into(),
                "--config".into(),
                config.clone(),
            ],
            vec![
                "orgfdb".into(),
                "links".into(),
                "--format".into(),
                "json".into(),
                "--config".into(),
                config.clone(),
            ],
        );

        for extra_args in [
            vec!["(headings (title \"Searchable Heading\"))"],
            vec!["--output", "outline", "(headings (todo \"TODO\"))"],
            vec![
                "--include",
                "path,path",
                "--include",
                "links",
                "(headings (todo \"TODO\"))",
            ],
        ] {
            let mut implicit = vec![
                "orgfdb".to_string(),
                "query".to_string(),
                "--config".to_string(),
                config.clone(),
            ];
            implicit.extend(extra_args.iter().map(|arg| (*arg).to_string()));
            let mut explicit_json = vec![
                "orgfdb".to_string(),
                "query".to_string(),
                "--json".to_string(),
                "--config".to_string(),
                config.clone(),
            ];
            explicit_json.extend(extra_args.iter().map(|arg| (*arg).to_string()));
            let mut explicit_format = vec![
                "orgfdb".to_string(),
                "query".to_string(),
                "--format".to_string(),
                "json".to_string(),
                "--config".to_string(),
                config.clone(),
            ];
            explicit_format.extend(extra_args.iter().map(|arg| (*arg).to_string()));
            assert_equivalent_json_output(implicit, explicit_json, explicit_format);
        }

        for extra_args in [
            vec!["sqlite"],
            vec!["--title", "Searchable"],
            vec!["--body", "phrase"],
        ] {
            let mut implicit = vec![
                "orgfdb".to_string(),
                "search".to_string(),
                "--config".to_string(),
                config.clone(),
            ];
            implicit.extend(extra_args.iter().map(|arg| (*arg).to_string()));
            let mut explicit_json = vec![
                "orgfdb".to_string(),
                "search".to_string(),
                "--json".to_string(),
                "--config".to_string(),
                config.clone(),
            ];
            explicit_json.extend(extra_args.iter().map(|arg| (*arg).to_string()));
            let mut explicit_format = vec![
                "orgfdb".to_string(),
                "search".to_string(),
                "--format".to_string(),
                "json".to_string(),
                "--config".to_string(),
                config.clone(),
            ];
            explicit_format.extend(extra_args.iter().map(|arg| (*arg).to_string()));
            assert_equivalent_json_output(implicit, explicit_json, explicit_format);
        }
    }

    #[test]
    fn implicit_and_explicit_json_forms_preserve_runtime_errors() {
        let missing_config = TestDir::new("output-format-errors")
            .path()
            .join("missing.toml");
        let config = missing_config.display().to_string();

        assert_equivalent_cli_error(
            vec!["orgfdb".into(), "query".into(), "(todo".into()],
            vec![
                "orgfdb".into(),
                "query".into(),
                "--json".into(),
                "(todo".into(),
            ],
            vec![
                "orgfdb".into(),
                "query".into(),
                "--format".into(),
                "json".into(),
                "(todo".into(),
            ],
        );
        assert_equivalent_cli_error(
            vec!["orgfdb".into(), "search".into(), "AND".into()],
            vec![
                "orgfdb".into(),
                "search".into(),
                "--json".into(),
                "AND".into(),
            ],
            vec![
                "orgfdb".into(),
                "search".into(),
                "--format".into(),
                "json".into(),
                "AND".into(),
            ],
        );
        assert_equivalent_cli_error(
            vec![
                "orgfdb".into(),
                "headings".into(),
                "--config".into(),
                config.clone(),
            ],
            vec![
                "orgfdb".into(),
                "headings".into(),
                "--json".into(),
                "--config".into(),
                config.clone(),
            ],
            vec![
                "orgfdb".into(),
                "headings".into(),
                "--format".into(),
                "json".into(),
                "--config".into(),
                config,
            ],
        );
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
    fn search_rejects_empty_expression() {
        let error = search_json_rows(CliSearchScope::All, "   ", None)
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

        let rows = search_json_rows(CliSearchScope::All, "Searchable", Some(&config_path))
            .expect("search should succeed");
        assert_eq!(rows.len(), 1);
        assert_eq!(rows[0].heading.kind.as_str(), "heading");
        assert_eq!(rows[0].heading.title, "Canonical Override");
        assert_eq!(
            rows[0].heading.location.file_path,
            db_path
                .parent()
                .unwrap()
                .join("notes.org")
                .display()
                .to_string()
        );
        assert_eq!(rows[0].heading.location.line, Some(1));
        assert_eq!(rows[0].heading.location.byte_start, Some(0));
        assert!(rows[0].heading.location.byte_end.unwrap() >= 0);
    }

    #[test]
    fn search_serializes_the_canonical_flat_heading_shape_with_rank() {
        let (_test_dir, config_path, _) = build_search_fixture(
            "search-canonical-shape",
            &[(
                "notes.org",
                "* TODO [#A] Searchable Heading :project:rust:\nSCHEDULED: <2026-07-20 Mon> DEADLINE: <2026-07-21 Tue> CLOSED: [2026-07-22 Wed]\n",
            )],
            true,
        );

        let rows = search_json_rows(CliSearchScope::All, "Searchable", Some(&config_path))
            .expect("search should succeed");
        let query = super::query_json_response(
            "(headings (title \"Searchable Heading\"))",
            super::CliQueryOutput::Flat,
            &[],
            Some(&config_path),
        )
        .expect("heading query should succeed");

        assert_eq!(rows.len(), 1);
        assert_eq!(query.results.len(), 1);

        let mut search_json = serde_json::to_value(&rows[0]).expect("search row should serialize");
        let rank = search_json
            .as_object_mut()
            .expect("search row should be an object")
            .remove("rank")
            .expect("search row should include rank");
        let query_json =
            serde_json::to_value(&query.results[0]).expect("query row should serialize");

        assert!(rank.is_number());
        assert_eq!(search_json, query_json);
        assert_eq!(search_json["kind"], "heading");
        assert_eq!(search_json["matched"], true);
        assert_eq!(search_json["todo_keyword"], "TODO");
        assert!(search_json["todo_type"].is_string());
        assert_eq!(search_json["priority"], "A");
        assert_eq!(
            search_json["all_tags"],
            serde_json::json!(["project", "rust"])
        );
        assert!(search_json["title_raw"].is_string());
        assert!(search_json["scheduled_raw"].is_string());
        assert!(search_json["deadline_raw"].is_string());
        assert!(search_json["closed_raw"].is_string());
        assert!(search_json["location"].is_object());
        assert!(search_json.get("heading_id").is_none());
        assert!(search_json.get("path").is_none());
        assert!(search_json.get("line_number").is_none());
    }

    #[test]
    fn search_indexes_source_title_text_without_changing_query_title_semantics() {
        let (_test_dir, config_path, _) = build_search_fixture(
            "search-source-title-text",
            &[
                (
                    "headline.org",
                    "* TODO [#A] Searchable Heading [2/5] :project:\nBody phrase without marker.\n",
                ),
                ("titled-root.org", "#+TITLE: Explicit Root Title\n"),
                ("fallbacktitle.org", ""),
            ],
            true,
        );

        let todo_rows = search_json_rows(CliSearchScope::Title, "TODO", Some(&config_path))
            .expect("TODO should match source title text");
        let title_rows = search_json_rows(CliSearchScope::Title, "Searchable", Some(&config_path))
            .expect("normalized title text should remain searchable");
        let statistics_rows = search_json_rows(CliSearchScope::Title, "2", Some(&config_path))
            .expect("statistics cookie token should match source title text");
        let tag_rows = search_json_rows(CliSearchScope::Title, "project", Some(&config_path))
            .expect("trailing tag search should succeed");
        let default_rows = search_json_rows(CliSearchScope::All, "TODO", Some(&config_path))
            .expect("default search should include title text");
        let body_rows = search_json_rows(CliSearchScope::Body, "TODO", Some(&config_path))
            .expect("body search should succeed");

        assert_eq!(todo_rows.len(), 1);
        assert_eq!(title_rows.len(), 1);
        assert_eq!(statistics_rows.len(), 1);
        assert!(tag_rows.is_empty());
        assert_eq!(default_rows.len(), 1);
        assert!(body_rows.is_empty());

        let heading = &todo_rows[0].heading;
        assert_eq!(heading.title, "Searchable Heading");
        assert_eq!(
            heading.title_raw.as_deref(),
            Some("TODO [#A] Searchable Heading [2/5]")
        );
        assert_eq!(heading.todo_keyword.as_deref(), Some("TODO"));
        assert_eq!(heading.priority.as_deref(), Some("A"));
        assert_eq!(heading.all_tags, vec!["project"]);

        let explicit_root_rows =
            search_json_rows(CliSearchScope::Title, "Explicit", Some(&config_path))
                .expect("source-titled root should remain searchable");
        assert_eq!(explicit_root_rows.len(), 1);
        assert_eq!(explicit_root_rows[0].heading.kind.as_str(), "root");
        assert_eq!(explicit_root_rows[0].heading.title, "Explicit Root Title");

        let fallback_root_rows =
            search_json_rows(CliSearchScope::Title, "fallbacktitle", Some(&config_path))
                .expect("fallback root title should remain searchable");
        assert_eq!(fallback_root_rows.len(), 1);
        assert_eq!(fallback_root_rows[0].heading.kind.as_str(), "root");
        assert_eq!(fallback_root_rows[0].heading.title, "fallbacktitle");
        assert_eq!(fallback_root_rows[0].heading.title_raw, None);

        let raw_title_query = super::query_json_response(
            "(headings (title \"TODO\"))",
            super::CliQueryOutput::Flat,
            &[],
            Some(&config_path),
        )
        .expect("normalized title query should succeed");
        let normalized_title_query = super::query_json_response(
            "(headings (title \"Searchable Heading\"))",
            super::CliQueryOutput::Flat,
            &[],
            Some(&config_path),
        )
        .expect("normalized title query should succeed");
        assert!(raw_title_query.results.is_empty());
        assert_eq!(normalized_title_query.results.len(), 1);
    }

    #[test]
    fn search_indexes_file_roots_with_kind_and_preamble_bodies() {
        let (test_dir, config_path, db_path) = build_search_fixture(
            "search-file-roots",
            &[
                (
                    "roots.org",
                    "#+TITLE: Root Title Sapphire\n\npreamblequartz\n\n* Child Heading Amber\nchildbodycopper\n",
                ),
                (
                    "preamble-only.org",
                    "#+TITLE: Preamble Only\n\nsolopreamblezinc\n",
                ),
            ],
            true,
        );

        let root_path = test_dir.path().join("roots.org");
        let connection = open_database(&db_path).expect("database should open");
        let root_id: i64 = connection
            .query_row(
                "SELECT headings.id
                 FROM headings
                 INNER JOIN files ON files.id = headings.file_id
                 WHERE files.path = ?1 AND headings.level = 0",
                [root_path.display().to_string()],
                |row| row.get(0),
            )
            .expect("root heading should load");
        let real_heading_count: i64 = connection
            .query_row(
                "SELECT COUNT(*)
                 FROM headings
                 INNER JOIN files ON files.id = headings.file_id
                 WHERE files.path = ?1 AND headings.level > 0",
                [test_dir
                    .path()
                    .join("preamble-only.org")
                    .display()
                    .to_string()],
                |row| row.get(0),
            )
            .expect("preamble-only heading count should load");
        assert_eq!(real_heading_count, 0);
        drop(connection);

        let preamble_rows =
            search_json_rows(CliSearchScope::All, "preamblequartz", Some(&config_path))
                .expect("default preamble search should succeed");
        assert_eq!(preamble_rows.len(), 1);
        assert_eq!(preamble_rows[0].heading.id, root_id);
        assert_eq!(preamble_rows[0].heading.kind.as_str(), "root");
        assert_eq!(preamble_rows[0].heading.level, 0);
        assert_eq!(preamble_rows[0].heading.parent_id, None);
        assert_eq!(
            serde_json::to_value(&preamble_rows).expect("search rows should serialize")[0]["kind"],
            "root"
        );

        let body_rows =
            search_json_rows(CliSearchScope::Body, "preamblequartz", Some(&config_path))
                .expect("body preamble search should succeed");
        assert_eq!(body_rows.len(), 1);
        assert_eq!(body_rows[0].heading.kind.as_str(), "root");

        let title_preamble_rows =
            search_json_rows(CliSearchScope::Title, "preamblequartz", Some(&config_path))
                .expect("title preamble search should succeed");
        assert!(title_preamble_rows.is_empty());

        let root_title_rows =
            search_json_rows(CliSearchScope::Title, "Sapphire", Some(&config_path))
                .expect("root title search should succeed");
        assert_eq!(root_title_rows.len(), 1);
        assert_eq!(root_title_rows[0].heading.kind.as_str(), "root");

        let heading_rows = search_json_rows(CliSearchScope::All, "Amber", Some(&config_path))
            .expect("real heading search should succeed");
        assert_eq!(heading_rows.len(), 1);
        assert_eq!(heading_rows[0].heading.kind.as_str(), "heading");

        let preamble_only_rows =
            search_json_rows(CliSearchScope::Body, "solopreamblezinc", Some(&config_path))
                .expect("preamble-only search should succeed");
        assert_eq!(preamble_only_rows.len(), 1);
        assert_eq!(preamble_only_rows[0].heading.kind.as_str(), "root");
    }

    #[test]
    fn search_title_only_index_includes_root_titles_without_root_preambles() {
        let (_test_dir, config_path, _) = build_search_fixture(
            "search-title-only-roots",
            &[(
                "title-only.org",
                "#+TITLE: Root Title Emerald\n\npreambleruby\n",
            )],
            false,
        );

        let title_rows = search_json_rows(CliSearchScope::Title, "Emerald", Some(&config_path))
            .expect("title-only root title search should succeed");
        assert_eq!(title_rows.len(), 1);
        assert_eq!(title_rows[0].heading.kind.as_str(), "root");

        let default_preamble_rows =
            search_json_rows(CliSearchScope::All, "preambleruby", Some(&config_path))
                .expect("title-only preamble search should succeed");
        assert!(default_preamble_rows.is_empty());
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

        let title_rows = search_json_rows(CliSearchScope::All, "Searchable", Some(&config_path))
            .expect("title search should succeed");
        let body_rows = search_json_rows(CliSearchScope::All, "phrase", Some(&config_path))
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

        let title_rows = search_json_rows(CliSearchScope::Title, "Searchable", Some(&config_path))
            .expect("title scope should succeed");
        let no_body_rows = search_json_rows(CliSearchScope::Title, "phrase", Some(&config_path))
            .expect("title scope should return empty on body term");
        let body_rows = search_json_rows(CliSearchScope::Body, "phrase", Some(&config_path))
            .expect("body scope should succeed");

        assert_eq!(title_rows.len(), 1);
        assert!(no_body_rows.is_empty());
        assert_eq!(body_rows.len(), 1);
    }

    #[test]
    fn search_rejects_explicit_column_filters_when_scope_is_fixed() {
        let error = search_json_rows(CliSearchScope::Title, "body:sqlite", None)
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
            CliSearchScope::All,
            "title:(Searchable)",
            Some(&config_path),
        )
        .expect("title filter should succeed");
        let body_rows = search_json_rows(CliSearchScope::All, "body:(phrase)", Some(&config_path))
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

        let error = search_json_rows(CliSearchScope::Body, "phrase", Some(&config_path))
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

        let error = search_json_rows(CliSearchScope::All, "sqlite", Some(&config_path))
            .expect_err("missing metadata should fail");
        match error {
            CliError::Search(SearchError::MissingTrustMetadata) => {}
            other => panic!("unexpected error: {other}"),
        }
        assert!(error.to_string().contains("run orgfdb rebuild"));
    }

    #[test]
    fn search_rejects_non_current_numeric_fts_contract_versions_as_stale() {
        let (_test_dir, config_path, db_path) = build_search_fixture(
            "search-stale-fts-contract",
            &[("notes.org", "* Searchable Heading\nBody phrase.\n")],
            true,
        );
        let connection = open_database(&db_path).expect("database should open");
        for version in ["0", "1", "2", "4"] {
            connection
                .execute(
                    "UPDATE db_metadata SET value = ?1 WHERE key = ?2",
                    (version, DB_METADATA_FTS_SCHEMA_VERSION_KEY),
                )
                .expect("non-current FTS contract version should store");

            let error = search_json_rows(CliSearchScope::All, "Searchable", Some(&config_path))
                .expect_err("non-current FTS contract version should be stale");
            assert!(matches!(
                error,
                CliError::Search(SearchError::MissingTrustedIndex)
            ));
            assert!(error.to_string().contains("run orgfdb rebuild"));
        }
    }

    #[test]
    fn search_rejects_non_numeric_fts_contract_metadata() {
        let (_test_dir, config_path, db_path) = build_search_fixture(
            "search-invalid-fts-contract",
            &[("notes.org", "* Searchable Heading\nBody phrase.\n")],
            true,
        );
        let connection = open_database(&db_path).expect("database should open");
        connection
            .execute(
                "UPDATE db_metadata SET value = 'invalid' WHERE key = ?1",
                [DB_METADATA_FTS_SCHEMA_VERSION_KEY],
            )
            .expect("invalid FTS contract metadata should store");

        let error = search_json_rows(CliSearchScope::All, "Searchable", Some(&config_path))
            .expect_err("non-numeric FTS contract metadata should be invalid");
        assert!(matches!(
            error,
            CliError::Search(SearchError::InvalidTrustMetadata)
        ));
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

        let rows = search_json_rows(CliSearchScope::All, "sqlite", Some(&config_path))
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

        let error = search_json_rows(CliSearchScope::All, "Searchable", Some(&config_path))
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

        let error = search_json_rows(CliSearchScope::All, "Searchable", Some(&config_path))
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

        let error = search_json_rows(CliSearchScope::All, "AND", Some(&config_path))
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

        let rows = search_json_rows(CliSearchScope::All, "Searchable", Some(&config_path))
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

        let rows = search_json_rows(CliSearchScope::All, "Shared", Some(&config_path))
            .expect("search should succeed");
        assert_eq!(rows.len(), 2);
        assert!(rows[0].rank <= rows[1].rank);
        if (rows[0].rank - rows[1].rank).abs() < f64::EPSILON {
            assert!(rows[0].heading.id < rows[1].heading.id);
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
    fn query_json_heading_title_root_matches_return_root_kind() {
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
        assert_eq!(file["kind"], "root");
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
            "(headings (title \"no-title-set\" :exact t))",
            super::CliQueryOutput::Flat,
            &[],
            Some(&config_path),
        )
        .expect("fallback root title query should succeed");

        let value = serde_json::to_value(&response).expect("response should serialize");
        let file = &value["results"][0];
        assert_eq!(file["kind"], "root");
        assert_eq!(file["title"], "no-title-set");
        assert!(file["title_raw"].is_null());
    }

    #[test]
    fn query_json_supports_outline_and_multiple_includes() {
        let test_dir = TestDir::new("query-outline");
        let config_path = write_query_fixture(&test_dir);

        let response = super::query_json_response(
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
                query,
                super::CliQueryOutput::Flat,
                &[],
                Some(&config_path),
            )
            .expect("example query should succeed");
            assert!(!response.results.is_empty(), "expected matches for {query}");
        }

        let include_response = super::query_json_response(
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
            "(todo \"NEXT\"",
            super::CliQueryOutput::Flat,
            &[],
            Some(&config_path),
        )
        .expect_err("invalid syntax should fail");
        assert!(matches!(syntax_error, CliError::QueryParse(_)));
        assert!(syntax_error.to_string().contains("unterminated"));

        let semantic_error = super::query_json_response(
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

        let backend_response = super::query_json_response(
            "(links (link-target \"notes.*\" :regexp t))",
            super::CliQueryOutput::Flat,
            &[],
            Some(&config_path),
        )
        .expect("regexp metadata query should succeed");
        assert!(!backend_response.results.is_empty());
    }

    #[test]
    fn query_json_is_read_only_and_does_not_reparse_files() {
        let test_dir = TestDir::new("query-read-only");
        let config_path = write_query_fixture(&test_dir);
        let db_path = test_dir.path().join("db.sqlite");
        let org_path = test_dir.path().join("projects.org");

        let initial = super::query_json_response(
            "(todo \"NEXT\")",
            super::CliQueryOutput::Flat,
            &[],
            Some(&config_path),
        )
        .expect("initial query should succeed");

        write_file(&org_path, "#+TITLE: Changed\n* DONE Different\n");

        let stored = super::query_json_response(
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
    fn query_json_read_only_open_rejects_outdated_schema_without_mutation() {
        let test_dir = TestDir::new("query-outdated-db");
        let config_path = write_query_fixture(&test_dir);
        let db_path = test_dir.path().join("db.sqlite");

        let connection = Connection::open(&db_path).expect("query database should open");
        connection
            .pragma_update(None, "user_version", i64::from(CURRENT_SCHEMA_VERSION - 1))
            .expect("outdated user_version should seed");
        drop(connection);

        let error = super::query_json_response(
            "(todo \"NEXT\")",
            super::CliQueryOutput::Flat,
            &[],
            Some(&config_path),
        )
        .expect_err("outdated read-only query database should fail before execution");

        let message = error.to_string();
        match error {
            CliError::Database(DbError::OutdatedSchemaVersion {
                on_disk_version,
                required_version,
                ..
            }) => {
                assert_eq!(on_disk_version, CURRENT_SCHEMA_VERSION - 1);
                assert_eq!(required_version, CURRENT_SCHEMA_VERSION);
            }
            other => panic!("expected OutdatedSchemaVersion, got {other}"),
        }

        assert!(message.contains(&format!(
            "run an indexing command to migrate it to version {}",
            CURRENT_SCHEMA_VERSION
        )));

        let reopened = Connection::open(&db_path).expect("outdated database should reopen");
        let version_after: u32 = reopened
            .pragma_query_value(None, "user_version", |row| row.get(0))
            .expect("outdated schema version should remain unchanged");
        assert_eq!(version_after, CURRENT_SCHEMA_VERSION - 1);
    }

    #[test]
    fn headings_json_includes_level_zero_rows_by_default() {
        let schema = SchemaDefinition::new(CURRENT_SCHEMA_VERSION, false);
        let mut connection =
            open_in_memory_database_with_schema(&schema).expect("database should open");
        let file = FileRecordInput {
            path: PathBuf::from("/tmp/project.org"),
            identity: None,
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
                    priority: Some("A".to_string()),
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
                }],
            )?;
            let heading_id = tx.last_insert_rowid();
            seed_single_heading_tag(tx, file_id, heading_id, "rust")?;
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
            identity: None,
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
                    priority: Some("A".to_string()),
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
                }],
            )?;
            let heading_id = tx.last_insert_rowid();
            seed_single_heading_tag(tx, file_id, heading_id, "rust")?;
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
                identity: None,
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
                    }],
                )?;
                Ok(())
            },
        )
        .expect("configured db should be populated");

        drop(configured_db);

        let rows = super::headings_json_rows(false, Some(&config_path))
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

        let json_rows =
            super::headings_json_rows(false, Some(&config_path)).expect("json rows should load");
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

        let excluded_rows =
            super::headings_json_rows(true, Some(&config_path)).expect("excluded rows should load");
        assert_eq!(excluded_rows.len(), 1);
        assert_eq!(excluded_rows[0].level, 1);
        assert_eq!(excluded_rows[0].title, "Inbox");

        let include_root_rows = super::headings_json_rows(false, Some(&config_path))
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

        let error = super::headings_json_rows(false, Some(&config_path))
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
                identity: None,
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
                    }],
                )?;
                let heading_id = tx.last_insert_rowid();
                seed_single_heading_tag(tx, file_id, heading_id, "tagged")?;
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

        let rows = super::headings_json_rows(false, Some(&config_path))
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

        let error = super::headings_json_rows(false, Some(&config_path))
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

        let rows = super::links_json_rows(Some(&config_path)).expect("rows should load from db");

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

        let initial_rows = super::links_json_rows(Some(&config_path)).expect("rows should load");
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
            super::links_json_rows(Some(&config_path)).expect("stored rows should load");
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

        let error =
            super::links_json_rows(Some(&config_path)).expect_err("missing database should fail");
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

        let error = super::links_json_rows(Some(&config_path))
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
    fn status_and_changes_commands_return_the_committed_generation_contract() {
        let test_dir = TestDir::new("index-state-cli");
        let config_path = write_query_fixture(&test_dir);
        let config = config_path.display().to_string();

        let status_output = run_cli_output(vec![
            "orgfdb".to_string(),
            "status".to_string(),
            "--config".to_string(),
            config.clone(),
        ])
        .expect("status command should succeed");
        let status: Value =
            serde_json::from_slice(&status_output).expect("status output should be valid JSON");
        assert_eq!(status["schema_version"], CURRENT_SCHEMA_VERSION);
        assert_eq!(status["generation"], 1);
        let database_id = status["database_id"]
            .as_str()
            .expect("database ID should be a string")
            .to_string();
        assert!(!database_id.is_empty());
        assert!(status["last_changed_at"].as_str().is_some());
        let database_path = fs::canonicalize(test_dir.path().join("db.sqlite"))
            .expect("fixture database path should canonicalize");
        assert_eq!(status["database_path"], database_path.display().to_string());

        let unchanged_output = run_cli_output(vec![
            "orgfdb".to_string(),
            "changes".to_string(),
            "--database-id".to_string(),
            database_id.clone(),
            "--since-generation".to_string(),
            "1".to_string(),
            "--config".to_string(),
            config.clone(),
        ])
        .expect("unchanged command should succeed");
        let unchanged: Value =
            serde_json::from_slice(&unchanged_output).expect("changes output should be valid JSON");
        assert_eq!(unchanged["cache_action"], "unchanged");
        assert_eq!(unchanged["complete"], true);
        assert_eq!(unchanged["upsert_files"], serde_json::json!([]));
        assert_eq!(unchanged["deleted_files"], serde_json::json!([]));

        let initial_output = run_cli_output(vec![
            "orgfdb".to_string(),
            "changes".to_string(),
            "--database-id".to_string(),
            database_id,
            "--since-generation".to_string(),
            "0".to_string(),
            "--config".to_string(),
            config,
        ])
        .expect("initial generation changes should succeed");
        let initial: Value =
            serde_json::from_slice(&initial_output).expect("changes output should be valid JSON");
        assert_eq!(initial["cache_action"], "rebuild");
        assert_eq!(initial["reason"], "full-invalidation");
        assert_eq!(initial["complete"], true);
    }

    #[test]
    fn status_requires_the_index_state_schema_without_migrating_read_only() {
        let test_dir = TestDir::new("status-old-schema");
        let config_path = test_dir.path().join("config.toml");
        let db_path = test_dir.path().join("old.sqlite");
        write_file(
            &config_path,
            r#"
db_path = "./old.sqlite"
"#,
        );
        let connection = Connection::open(&db_path).expect("legacy database should open");
        connection
            .pragma_update(None, "user_version", 11_u32)
            .expect("legacy schema version should seed");
        drop(connection);

        let error = run_cli_output(vec![
            "orgfdb".to_string(),
            "status".to_string(),
            "--config".to_string(),
            config_path.display().to_string(),
        ])
        .expect_err("status should not migrate an old database");
        assert!(error
            .to_string()
            .contains("run an indexing command to migrate it to version 12"));

        let version = Connection::open(&db_path)
            .expect("legacy database should reopen")
            .pragma_query_value(None, "user_version", |row| row.get::<_, u32>(0))
            .expect("legacy schema version should remain readable");
        assert_eq!(version, 11);
    }

    #[test]
    fn changes_rejects_a_generation_newer_than_the_database() {
        let test_dir = TestDir::new("changes-future-generation");
        let config_path = write_query_fixture(&test_dir);
        let status_output = run_cli_output(vec![
            "orgfdb".to_string(),
            "status".to_string(),
            "--config".to_string(),
            config_path.display().to_string(),
        ])
        .expect("status command should succeed");
        let status: Value =
            serde_json::from_slice(&status_output).expect("status output should be valid JSON");
        let database_id = status["database_id"]
            .as_str()
            .expect("database ID should be a string");

        let error = run_cli_output(vec![
            "orgfdb".to_string(),
            "changes".to_string(),
            "--database-id".to_string(),
            database_id.to_string(),
            "--since-generation".to_string(),
            "2".to_string(),
            "--config".to_string(),
            config_path.display().to_string(),
        ])
        .expect_err("future generation should fail");
        assert!(error
            .to_string()
            .contains("newer than current generation 1"));
    }

    #[test]
    fn query_file_restriction_reads_bulk_json_and_preserves_row_ownership() {
        let test_dir = TestDir::new("query-file-restriction");
        let config_path = write_query_fixture(&test_dir);
        let paths_path = test_dir.path().join("paths.json");
        let notes_path = test_dir.path().join("notes.org").display().to_string();
        write_file(
            &paths_path,
            &serde_json::to_string(&vec![notes_path.clone(), notes_path.clone()])
                .expect("path list should serialize"),
        );

        let output = run_cli_output(vec![
            "orgfdb".to_string(),
            "query".to_string(),
            "--config".to_string(),
            config_path.display().to_string(),
            "--restrict-files-json".to_string(),
            paths_path.display().to_string(),
            "(headings)".to_string(),
        ])
        .expect("restricted query should succeed");
        let response: Value =
            serde_json::from_slice(&output).expect("restricted query output should be valid JSON");
        let results = response["results"]
            .as_array()
            .expect("results should be an array");
        assert!(!results.is_empty());
        for result in results {
            assert_eq!(result["location"]["file_path"], notes_path);
        }

        write_file(&paths_path, "[]");
        let empty_output = run_cli_output(vec![
            "orgfdb".to_string(),
            "query".to_string(),
            "--config".to_string(),
            config_path.display().to_string(),
            "--restrict-files-json".to_string(),
            paths_path.display().to_string(),
            "(headings)".to_string(),
        ])
        .expect("empty restriction should succeed");
        let empty_response: Value = serde_json::from_slice(&empty_output)
            .expect("empty restricted query output should be valid JSON");
        assert!(empty_response["results"]
            .as_array()
            .expect("results should be an array")
            .is_empty());

        write_file(&paths_path, r#"[""]"#);
        let error = run_cli_output(vec![
            "orgfdb".to_string(),
            "query".to_string(),
            "--config".to_string(),
            config_path.display().to_string(),
            "--restrict-files-json".to_string(),
            paths_path.display().to_string(),
            "(headings)".to_string(),
        ])
        .expect_err("empty path entry should fail");
        assert!(error
            .to_string()
            .contains("restricted file paths must not be empty"));
    }

    #[test]
    fn changes_reports_database_identity_replacement_without_failing() {
        let test_dir = TestDir::new("changes-database-id");
        let config_path = write_query_fixture(&test_dir);
        let output = run_cli_output(vec![
            "orgfdb".to_string(),
            "changes".to_string(),
            "--database-id".to_string(),
            "different-database".to_string(),
            "--since-generation".to_string(),
            "0".to_string(),
            "--config".to_string(),
            config_path.display().to_string(),
        ])
        .expect("database identity mismatch should return a rebuild response");
        let response: Value =
            serde_json::from_slice(&output).expect("changes output should be valid JSON");
        assert_eq!(response["cache_action"], "rebuild");
        assert_eq!(response["reason"], "database-id-changed");
        assert_eq!(response["complete"], false);
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
            identity: None,
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
    fn docs_query_reference_mentions_public_predicates_includes_and_options() {
        const CLI_DOCS: &str = include_str!("../docs/cli.org");
        const QUERY_DOCS: &str = include_str!("../docs/reference/query-language.org");

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
                QUERY_DOCS.contains(&format!("~{predicate}~")),
                "docs/reference/query-language.org should mention predicate {predicate}"
            );
        }

        for include in [
            "path",
            "properties",
            "effective_properties",
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
            ":exact", ":regexp", ":match", ":inherit", ":from", ":to", ":on",
        ] {
            assert!(
                QUERY_DOCS.contains(&format!("~{option}~")),
                "docs/reference/query-language.org should mention query option {option}"
            );
        }
    }
}
