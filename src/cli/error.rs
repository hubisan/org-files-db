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
            | Self::InvalidPresentationViewUsage(_) => None,
            Self::InvalidHeadingPath { source, .. } => Some(source),
            Self::Json(source) => Some(source),
            Self::Io(source) => Some(source),
        }
    }
}
