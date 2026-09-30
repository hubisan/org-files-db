use std::{
    collections::{BTreeMap, BTreeSet, HashMap},
    fmt,
    path::Path,
    time::{Duration, Instant},
};

use chrono::{DateTime, Utc};
use rusqlite::{params_from_iter, Connection};
use serde::Serialize;

use super::benchmark_trace;
use super::sql_support::id_chunk_capacity;
use super::sqlite::{
    cleanup_temporary_matched_relation, execute_sqlite_query_with_relation_and_strategies,
    file_relation_columns, heading_relation_columns, HeadingQueryMatch,
    MatchedRelationReuseStrategy, MatchedSqlRelation, MetadataPredicateSqlStrategy,
    PRODUCTION_MATCHED_RELATION_REUSE_STRATEGY, PRODUCTION_METADATA_PREDICATE_SQL_STRATEGY,
};
use super::{
    FileQueryRow, HeadingQueryRow, LinkQueryRow, QueryExecutionError, QueryRows, QueryTarget,
    ValidatedQuery,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum QueryOutputMode {
    Flat,
    Outline,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum QueryInclude {
    Path,
    Properties,
    EffectiveProperties,
    Keywords,
    Links,
    Backlinks,
    Source,
    Target,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct QueryExecutionOptions {
    pub output_mode: QueryOutputMode,
    pub includes: Vec<QueryInclude>,
    pub query_timezone: Option<String>,
    pub now_utc: Option<DateTime<Utc>>,
    pub restricted_file_paths: Option<Vec<String>>,
}

impl Default for QueryExecutionOptions {
    fn default() -> Self {
        Self {
            output_mode: QueryOutputMode::Flat,
            includes: Vec::new(),
            query_timezone: None,
            now_utc: None,
            restricted_file_paths: None,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct QueryResponse {
    pub target: QueryTarget,
    pub output: QueryOutputMode,
    pub includes: Vec<QueryInclude>,
    pub results: Vec<QueryResultNode>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
#[serde(untagged)]
pub enum QueryResultNode {
    File(FileResultNode),
    Heading(HeadingResultNode),
    Link(Box<LinkResultNode>),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum QueryResultKind {
    File,
    Root,
    Heading,
    Link,
}

impl QueryResultKind {
    pub fn from_heading_level(level: i64) -> Self {
        if level == 0 {
            Self::Root
        } else {
            Self::Heading
        }
    }

    pub const fn as_str(self) -> &'static str {
        match self {
            Self::File => "file",
            Self::Root => "root",
            Self::Heading => "heading",
            Self::Link => "link",
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ResultDomain {
    Files,
    Headings,
    Links,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum HeadingPathStrategy {
    RecursiveQueryDerived,
    RustDrivenBulkAncestors,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum DirectFlatShapingStrategy {
    CloneBaseline,
    MoveOwned,
}

const PRODUCTION_DIRECT_FLAT_SHAPING_STRATEGY: DirectFlatShapingStrategy =
    DirectFlatShapingStrategy::MoveOwned;

fn public_result_kind(domain: ResultDomain, heading_level: i64) -> QueryResultKind {
    match domain {
        ResultDomain::Files => QueryResultKind::File,
        ResultDomain::Headings => QueryResultKind::from_heading_level(heading_level),
        ResultDomain::Links => QueryResultKind::Link,
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct FileResultNode {
    pub kind: QueryResultKind,
    pub matched: bool,
    pub id: i64,
    pub level: i64,
    pub path: String,
    pub name: String,
    pub dir: String,
    pub title: String,
    pub title_raw: Option<String>,
    pub root_heading_id: i64,
    pub mtime_ns: i64,
    pub size: i64,
    pub content_hash: Option<String>,
    pub indexed_at: Option<i64>,
    pub location: Location,
    pub tags: Vec<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub node_path: Option<Vec<PathEntry>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub properties: Option<Vec<PropertyFact>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub effective_properties: Option<Vec<EffectivePropertyFact>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub keywords: Option<Vec<KeywordFact>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub links: Option<Vec<IncludedLink>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub backlinks: Option<Vec<IncludedLink>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub children: Option<Vec<QueryResultNode>>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct HeadingResultNode {
    pub kind: QueryResultKind,
    pub matched: bool,
    pub id: i64,
    pub file_id: i64,
    pub parent_id: Option<i64>,
    pub level: i64,
    pub title: String,
    pub title_raw: Option<String>,
    pub todo_keyword: Option<String>,
    pub todo_type: Option<String>,
    pub priority: Option<String>,
    pub scheduled_raw: Option<String>,
    pub scheduled_ts: Option<i64>,
    pub deadline_raw: Option<String>,
    pub deadline_ts: Option<i64>,
    pub closed_raw: Option<String>,
    pub closed_ts: Option<i64>,
    pub archivedp: bool,
    pub footnote_section_p: bool,
    pub all_tags: Vec<String>,
    pub location: Location,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub node_path: Option<Vec<PathEntry>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub properties: Option<Vec<PropertyFact>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub effective_properties: Option<Vec<EffectivePropertyFact>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub keywords: Option<Vec<KeywordFact>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub links: Option<Vec<IncludedLink>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub backlinks: Option<Vec<IncludedLink>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub children: Option<Vec<QueryResultNode>>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct LinkResultNode {
    pub kind: QueryResultKind,
    pub matched: bool,
    pub id: i64,
    pub file_id: i64,
    pub heading_id: i64,
    pub heading_level: i64,
    pub source_context: String,
    pub format: String,
    pub link_type: String,
    pub raw: String,
    pub raw_target: String,
    pub raw_description: Option<String>,
    pub link_path: String,
    pub search_option: Option<String>,
    pub path_absolute: Option<String>,
    pub target_file_id: Option<i64>,
    pub target_heading_id: Option<i64>,
    pub target_custom_id: Option<String>,
    pub target_id: Option<String>,
    pub resolution_status: Option<String>,
    pub resolution_diagnostic: Option<String>,
    pub location: Location,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub node_path: Option<Vec<PathEntry>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub source: Option<LinkSource>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub target: Option<LinkTarget>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct Location {
    pub file_path: String,
    pub line: Option<i64>,
    pub byte_start: Option<i64>,
    pub byte_end: Option<i64>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
#[serde(tag = "kind", rename_all = "snake_case")]
pub enum PathEntry {
    File(FilePathEntry),
    Heading(HeadingPathEntry),
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct FilePathEntry {
    pub id: i64,
    pub path: String,
    pub title: String,
    pub title_raw: Option<String>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct HeadingPathEntry {
    pub id: i64,
    pub title: String,
    pub title_raw: String,
    pub level: i64,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct PropertyFact {
    pub key: String,
    pub value: Option<String>,
    pub source: String,
    pub append: bool,
    pub line_number: Option<i64>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct EffectivePropertyFact {
    pub key: String,
    pub value: Option<String>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct KeywordFact {
    pub keyword: String,
    pub value: Option<String>,
    pub line_number: Option<i64>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct LinkSource {
    pub file: FileRef,
    pub heading: Option<HeadingRef>,
    pub source_path: Vec<PathEntry>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct LinkTarget {
    pub resolved_kind: Option<QueryTarget>,
    pub file: Option<FileRef>,
    pub heading: Option<HeadingRef>,
    pub raw_target: String,
    pub resolution_status: Option<String>,
    pub resolution_diagnostic: Option<String>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct FileRef {
    pub id: i64,
    pub path: String,
    pub title: String,
    pub title_raw: Option<String>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct HeadingRef {
    pub id: i64,
    pub title: String,
    pub title_raw: String,
    pub level: i64,
    pub outline_path: Vec<String>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct IncludedLink {
    pub id: i64,
    pub source_context: String,
    pub format: String,
    pub link_type: String,
    pub raw: String,
    pub raw_target: String,
    pub raw_description: Option<String>,
    pub link_path: String,
    pub search_option: Option<String>,
    pub path_absolute: Option<String>,
    pub target_file_id: Option<i64>,
    pub target_heading_id: Option<i64>,
    pub target_custom_id: Option<String>,
    pub target_id: Option<String>,
    pub resolution_status: Option<String>,
    pub resolution_diagnostic: Option<String>,
    pub location: Location,
    pub source_path: Vec<PathEntry>,
    pub source: LinkSource,
    pub target: LinkTarget,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum QueryShapeErrorKind {
    Database,
    InvalidStoredJson,
    MissingStoredData,
}

#[derive(Debug)]
pub struct QueryShapeError {
    pub kind: QueryShapeErrorKind,
    pub message: String,
    source: Option<Box<dyn std::error::Error + Send + Sync>>,
}

impl QueryShapeError {
    fn database(operation: &'static str, source: rusqlite::Error) -> Self {
        Self {
            kind: QueryShapeErrorKind::Database,
            message: format!(
                "failed to load query result enrichment data during {operation}: {source}"
            ),
            source: Some(Box::new(source)),
        }
    }

    fn invalid_json(field: &'static str, id: i64, source: serde_json::Error) -> Self {
        Self {
            kind: QueryShapeErrorKind::InvalidStoredJson,
            message: format!("failed to decode stored JSON field {field} for row {id}: {source}"),
            source: Some(Box::new(source)),
        }
    }

    fn missing(message: impl Into<String>) -> Self {
        Self {
            kind: QueryShapeErrorKind::MissingStoredData,
            message: message.into(),
            source: None,
        }
    }
}

impl fmt::Display for QueryShapeError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(&self.message)
    }
}

impl std::error::Error for QueryShapeError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        self.source
            .as_ref()
            .map(|source| source.as_ref() as &(dyn std::error::Error + 'static))
    }
}

impl From<QueryExecutionError> for QueryShapeError {
    fn from(error: QueryExecutionError) -> Self {
        Self {
            kind: QueryShapeErrorKind::Database,
            message: error.to_string(),
            source: Some(Box::new(error)),
        }
    }
}

mod enrich;
mod execute;
mod flat;
mod loaders;
mod outline;
mod paths;
#[cfg(test)]
mod tests;

use self::enrich::*;
pub use self::execute::{
    execute_and_shape_query, shape_matched_heading_nodes, shape_query_results,
};
pub(crate) use self::execute::{
    execute_and_shape_query_with_direct_flat_shaping_strategy,
    execute_and_shape_query_with_metadata_strategy, execute_and_shape_query_with_path_strategy,
    execute_and_shape_query_with_relation_reuse_strategy,
};
use self::flat::*;
use self::loaders::*;
use self::outline::*;
use self::paths::*;
pub(crate) use self::paths::{
    load_heading_paths_from_relation, load_heading_paths_recursive_from_relation,
};
