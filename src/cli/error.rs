use std::{error::Error, fmt, io, path::PathBuf};

use crate::{
    config::ConfigError,
    db::{DbError, IndexStateReadError},
    indexer::IndexerError,
    presentation::{PresentationBuildError, PresentationSpecError},
    presentation_view::ViewControlClientError,
    presentation_view_cache::{PresentationViewCachePathError, PresentationViewCacheReadError},
    query::{QueryExecutionError, QueryParseError, QueryShapeError, QueryValidationError},
    watcher_cli::WatcherCommandError,
};

use super::search::SearchError;

/// How the top-level error is written to standard error.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub(super) enum ErrorFormat {
    #[default]
    Text,
    Json,
}

impl ErrorFormat {
    /// Reads `--error-format` from raw arguments. Clap fails before its struct
    /// exists on usage errors, so the flag is scanned early. Unknown values
    /// fall back to text; clap reports them afterwards.
    pub(super) fn from_args<T: AsRef<std::ffi::OsStr>>(args: &[T]) -> Self {
        let mut format = Self::Text;
        let mut iter = args
            .iter()
            .skip(1)
            .map(|arg| arg.as_ref().to_string_lossy());
        while let Some(arg) = iter.next() {
            let value = if arg == "--" {
                break;
            } else if arg == "--error-format" {
                iter.next()
            } else {
                arg.strip_prefix("--error-format=")
                    .map(|v| std::borrow::Cow::Owned(v.to_string()))
            };
            match value.as_deref() {
                Some("json") => format = Self::Json,
                Some("text") => format = Self::Text,
                _ => {}
            }
        }
        format
    }
}

#[derive(Debug)]
pub(super) enum CliError {
    Parse(clap::Error),
    InvalidSearchUsage(String),
    Config(ConfigError),
    Database(DbError),
    DbRead(crate::db::DbReadError),
    Indexer(IndexerError),
    Watcher(WatcherCommandError),
    ViewControl(ViewControlClientError),
    PresentationViewCachePath(PresentationViewCachePathError),
    PresentationViewCacheRead(PresentationViewCacheReadError),
    PresentationViewRebuild(String),
    InvalidPresentationViewUsage(String),
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
    InvalidPresentationUsage(String),
    PresentationSpec(PresentationSpecError),
    PresentationBuild(PresentationBuildError),
    PresentationSnapshot {
        operation: &'static str,
        source: rusqlite::Error,
    },
    InvalidHeadingPath {
        heading_id: i64,
        source: serde_json::Error,
    },
    StaleIndex {
        expected_database_id: Option<String>,
        expected_generation: Option<i64>,
        actual_database_id: String,
        actual_generation: i64,
    },
    Json(serde_json::Error),
    Io(io::Error),
}

impl CliError {
    pub(super) fn exit_code(&self) -> u8 {
        match self {
            Self::Parse(_)
            | Self::InvalidSearchUsage(_)
            | Self::InvalidPresentationUsage(_)
            | Self::InvalidPresentationViewUsage(_)
            | Self::PresentationSpec(_) => 2,
            Self::Config(_)
            | Self::Database(_)
            | Self::DbRead(_)
            | Self::Indexer(_)
            | Self::Watcher(_)
            | Self::ViewControl(_)
            | Self::PresentationViewCachePath(_)
            | Self::PresentationViewCacheRead(_)
            | Self::PresentationViewRebuild(_)
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
            | Self::PresentationBuild(_)
            | Self::PresentationSnapshot { .. }
            | Self::InvalidHeadingPath { .. }
            | Self::StaleIndex { .. }
            | Self::Json(_)
            | Self::Io(_) => 1,
        }
    }
}

impl CliError {
    /// Stable machine-readable error kind (see `docs/cli.org`).
    pub(super) fn kind(&self) -> &'static str {
        match self {
            Self::Parse(_)
            | Self::InvalidSearchUsage(_)
            | Self::InvalidPresentationUsage(_)
            | Self::InvalidPresentationViewUsage(_)
            | Self::PresentationSpec(_)
            | Self::ReadRestriction { .. }
            | Self::RestrictionJson(_)
            | Self::InvalidRestriction(_) => "usage",
            Self::Config(_) => "config",
            Self::Database(_)
            | Self::DbRead(_)
            | Self::Indexer(_)
            | Self::IndexState(_)
            | Self::SchemaInspect(_)
            | Self::UnsupportedIndexStateSchema { .. }
            | Self::PresentationSnapshot { .. } => "database",
            Self::StaleIndex { .. } => "stale-index",
            Self::Search(SearchError::DisabledByConfig) => "search-disabled",
            Self::Search(
                SearchError::FtsUnavailable
                | SearchError::MissingTrustMetadata
                | SearchError::InvalidTrustMetadata
                | SearchError::MissingTrustedIndex
                | SearchError::IncompatibleIndexSchema
                | SearchError::BodyScopeUnavailable,
            ) => "index-not-trusted",
            Self::Search(
                SearchError::ScopedColumnFilter { .. }
                | SearchError::ScopedUnbalancedParentheses { .. }
                | SearchError::InvalidExpression { .. },
            )
            | Self::QueryParse(_)
            | Self::QueryValidate(_) => "query-invalid",
            Self::Search(SearchError::Inspect { .. } | SearchError::Execute { .. }) => "database",
            Self::Io(_)
            | Self::CanonicalizeDatabasePath { .. }
            | Self::Watcher(_)
            | Self::ViewControl(_)
            | Self::PresentationViewCachePath(_)
            | Self::PresentationViewCacheRead(_) => "io",
            Self::PresentationViewRebuild(_)
            | Self::QueryExecute(_)
            | Self::QueryShape(_)
            | Self::PresentationBuild(_)
            | Self::InvalidHeadingPath { .. }
            | Self::Json(_) => "internal",
        }
    }

    fn path(&self) -> Option<&std::path::Path> {
        match self {
            Self::Config(
                ConfigError::ReadFile { path, .. }
                | ConfigError::ParseToml { path, .. }
                | ConfigError::UnsupportedConfig { path, .. }
                | ConfigError::MissingFile { path }
                | ConfigError::MissingDirectory { path }
                | ConfigError::MissingHomeDirectory { path }
                | ConfigError::InvalidTimezone { path, .. }
                | ConfigError::InvalidTodoKeyword { path, .. }
                | ConfigError::InvalidExclusionPattern { path, .. },
            )
            | Self::Database(DbError::Open { path, .. })
            | Self::CanonicalizeDatabasePath { path, .. } => Some(path),
            _ => None,
        }
    }

    /// Renders the error as written to standard error, without a trailing newline.
    pub(super) fn render(&self, format: ErrorFormat) -> String {
        match format {
            ErrorFormat::Text => self.to_string(),
            ErrorFormat::Json => {
                let mut error = serde_json::Map::new();
                error.insert("kind".into(), self.kind().into());
                error.insert("message".into(), self.to_string().trim_end().into());
                if let Some(path) = self.path() {
                    error.insert("path".into(), path.display().to_string().into());
                }
                serde_json::json!({ "error": error }).to_string()
            }
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
            Self::ViewControl(source) => write!(f, "{source}"),
            Self::PresentationViewCachePath(source) => write!(f, "{source}"),
            Self::PresentationViewCacheRead(source) => write!(f, "{source}"),
            Self::PresentationViewRebuild(message) => write!(f, "{message}"),
            Self::InvalidPresentationViewUsage(message) => write!(f, "{message}"),
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
            Self::InvalidPresentationUsage(message) => write!(f, "{message}"),
            Self::PresentationSpec(source) => write!(f, "{source}"),
            Self::PresentationBuild(source) => write!(f, "{source}"),
            Self::PresentationSnapshot { operation, source } => write!(
                f,
                "failed to {operation} presentation database snapshot: {source}"
            ),
            Self::InvalidHeadingPath { heading_id, source } => {
                write!(
                    f,
                    "failed to decode heading path for heading {}: {}",
                    heading_id, source
                )
            }
            Self::StaleIndex {
                expected_database_id,
                expected_generation,
                actual_database_id,
                actual_generation,
            } => {
                write!(f, "stale index: ")?;
                let mut mismatches = Vec::new();
                if let Some(expected) = expected_database_id {
                    if expected != actual_database_id {
                        mismatches.push(format!(
                            "expected database id {expected}, found {actual_database_id}"
                        ));
                    }
                }
                if let Some(expected) = expected_generation {
                    if *expected != *actual_generation {
                        mismatches.push(format!(
                            "expected generation {expected}, found {actual_generation}"
                        ));
                    }
                }
                write!(f, "{}", mismatches.join("; "))
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
            Self::ViewControl(source) => Some(source),
            Self::PresentationViewCachePath(source) => Some(source),
            Self::PresentationViewCacheRead(source) => Some(source),
            Self::PresentationViewRebuild(_) => None,
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
            Self::PresentationSpec(source) => Some(source),
            Self::PresentationBuild(source) => Some(source),
            Self::PresentationSnapshot { source, .. } => Some(source),
            Self::InvalidRestriction(_)
            | Self::InvalidPresentationUsage(_)
            | Self::InvalidPresentationViewUsage(_)
            | Self::StaleIndex { .. } => None,
            Self::InvalidHeadingPath { source, .. } => Some(source),
            Self::Json(source) => Some(source),
            Self::Io(source) => Some(source),
        }
    }
}
