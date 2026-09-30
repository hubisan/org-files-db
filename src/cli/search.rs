use std::{error::Error, fmt, path::Path};

use rusqlite::Connection;
use serde::Serialize;

use crate::{
    config::Config,
    db::{
        open_existing_database_read_only, sqlite_supports_fts5, DbReader,
        DB_METADATA_FTS_AVAILABLE_KEY, DB_METADATA_FTS_BODY_INDEXED_KEY,
        DB_METADATA_FTS_SCHEMA_VERSION_KEY, FTS_SCHEMA_CONTRACT_VERSION,
    },
    query::{shape_matched_heading_nodes, HeadingResultNode},
};

use super::{error::CliError, load_cli_config};

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

pub(super) fn cli_search_scope(title: bool, body: bool) -> CliSearchScope {
    if title {
        CliSearchScope::Title
    } else if body {
        CliSearchScope::Body
    } else {
        CliSearchScope::All
    }
}

pub(super) fn search_json_rows(
    scope: CliSearchScope,
    expression: &str,
    config_path: Option<&Path>,
) -> Result<Vec<SearchJsonRow>, CliError> {
    let config = load_cli_config(config_path)?;
    search_json_rows_for_config(scope, expression, &config)
}

pub(super) fn search_json_rows_for_config(
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

pub(super) fn production_search_rows_with_connection(
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

pub(super) fn map_search_db_read_error(error: crate::db::DbReadError) -> CliError {
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

pub(super) fn compile_search_expression(
    scope: CliSearchScope,
    expression: &str,
) -> Result<String, CliError> {
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

pub(super) fn reject_explicit_column_filter(
    expression: &str,
    scope: &'static str,
) -> Result<(), CliError> {
    if contains_unquoted_colon(expression) {
        return Err(CliError::Search(SearchError::ScopedColumnFilter { scope }));
    }
    if !parentheses_balanced_outside_quotes(expression) {
        return Err(CliError::Search(SearchError::ScopedUnbalancedParentheses {
            scope,
        }));
    }
    Ok(())
}

/// True when unquoted parentheses never close more than they open and end balanced,
/// so user text cannot break out of the scope wrapper.
pub(super) fn parentheses_balanced_outside_quotes(expression: &str) -> bool {
    let mut in_quotes = false;
    let mut depth: usize = 0;
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
            '(' if !in_quotes => depth += 1,
            ')' if !in_quotes => match depth.checked_sub(1) {
                Some(next) => depth = next,
                None => return false,
            },
            _ => {}
        }
    }
    depth == 0
}

pub(super) fn contains_unquoted_colon(expression: &str) -> bool {
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

pub(super) fn inspect_search_backend_state(
    connection: &Connection,
) -> Result<SearchBackendState, CliError> {
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

pub(super) fn load_metadata_value(
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

pub(super) fn table_exists(connection: &Connection, table: &str) -> Result<bool, rusqlite::Error> {
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

pub(super) fn load_table_columns(
    connection: &Connection,
    table: &str,
) -> Result<Vec<String>, rusqlite::Error> {
    let pragma = format!("PRAGMA table_info({table})");
    let mut statement = connection.prepare(&pragma)?;
    let rows = statement.query_map([], |row| row.get::<_, String>(1))?;
    rows.collect::<Result<Vec<_>, _>>()
}

pub(super) fn normalize_sql_definition(sql: &str) -> String {
    sql.chars()
        .filter(|ch| !ch.is_whitespace())
        .flat_map(char::to_lowercase)
        .collect()
}

pub(super) fn is_expected_fts5_expression_error(error: &rusqlite::Error) -> bool {
    let Some(message) = sqlite_error_message(error) else {
        return false;
    };
    message.contains("fts5:")
        || message.contains("unterminated string")
        || message.contains("no such column:")
}

pub(super) fn sqlite_error_message(error: &rusqlite::Error) -> Option<&str> {
    match error {
        rusqlite::Error::SqliteFailure(_, Some(message)) => Some(message.as_str()),
        _ => None,
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) struct SearchBackendState {
    pub(super) body_indexed: bool,
}

#[derive(Debug)]
pub(super) enum SearchError {
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
    ScopedUnbalancedParentheses {
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
            Self::ScopedUnbalancedParentheses { scope } => write!(
                f,
                "invalid SQLite FTS5 search expression: unbalanced parentheses are not allowed with --{scope}"
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
            | Self::ScopedUnbalancedParentheses { .. }
            | Self::InvalidExpression { .. } => None,
            Self::Inspect { source, .. } | Self::Execute { source, .. } => Some(source),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub(super) struct SearchJsonRow {
    #[serde(flatten)]
    pub(super) heading: HeadingResultNode,
    pub(super) rank: f64,
}

impl SearchJsonRow {
    pub(super) fn from_parts(heading: HeadingResultNode, rank: f64) -> Self {
        Self { heading, rank }
    }
}
