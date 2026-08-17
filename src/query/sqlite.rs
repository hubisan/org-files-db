use std::{
    env, fmt,
    time::{Duration, Instant},
};

use regex::Regex;
use rusqlite::{
    params_from_iter,
    types::{ToSqlOutput, Value},
    Connection, ToSql,
};
use serde::Serialize;

use super::benchmark_trace;
use super::priority::normalize_priority;
use super::result::QueryExecutionOptions;
use super::sql_support::id_chunk_capacity;
use super::{
    ensure_relative_dates_resolved, resolve_relative_dates, resolve_temporal_bounds,
    QueryDateResolutionOptions, QueryTarget, QueryValidationOptions, QueryValue, ValidatedArg,
    ValidatedExpr, ValidatedOption, ValidatedPredicate, ValidatedQuery,
};
use crate::db::DB_METADATA_BODY_TEXT_AVAILABLE_KEY;
use crate::property::normalize_property_key;

const QUERY_FILE_RESTRICTION_TABLE: &str = "orgfdb_query_file_restriction";

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CompiledSqlQuery {
    pub target: QueryTarget,
    pub sql: String,
    pub params: Vec<QueryParam>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum MetadataPredicateSqlStrategy {
    HeadingDrivenExists,
    PredicateDrivenIn,
}

pub(crate) const PRODUCTION_METADATA_PREDICATE_SQL_STRATEGY: MetadataPredicateSqlStrategy =
    MetadataPredicateSqlStrategy::PredicateDrivenIn;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum QueryParam {
    Integer(i64),
    Text(String),
}

impl ToSql for QueryParam {
    fn to_sql(&self) -> rusqlite::Result<ToSqlOutput<'_>> {
        match self {
            Self::Integer(value) => Ok(ToSqlOutput::Owned(Value::Integer(*value))),
            Self::Text(value) => Ok(ToSqlOutput::Owned(Value::Text(value.clone()))),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub enum QueryRows {
    Headings(Vec<HeadingQueryMatch>),
    Links(Vec<LinkQueryRow>),
    Files(Vec<FileQueryRow>),
}

const HEADING_RELATION_COLUMNS: &str = "id, file_id, file_path, parent_id, level, line_number, byte_start, byte_end, title, title_raw, todo_keyword, todo_type, priority, scheduled_raw, scheduled_ts, deadline_raw, deadline_ts, closed_raw, closed_ts, archivedp, footnote_section_p";
const FILE_RELATION_COLUMNS: &str = "id, path, mtime_ns, size, content_hash, indexed_at, root_heading_id, root_title, root_title_raw, root_line_number";

#[derive(Debug, Clone)]
pub(crate) enum MatchedSqlRelation {
    Headings {
        headings: CompiledSqlQuery,
        roots: Option<CompiledSqlQuery>,
    },
    Links,
    Files(CompiledSqlQuery),
}

impl MatchedSqlRelation {
    pub(crate) fn heading_relation(&self) -> Option<&CompiledSqlQuery> {
        match self {
            Self::Headings { headings, .. } => Some(headings),
            Self::Links | Self::Files(_) => None,
        }
    }

    pub(crate) fn root_relation(&self) -> Option<&CompiledSqlQuery> {
        match self {
            Self::Headings { roots, .. } => roots.as_ref(),
            Self::Files(compiled) => Some(compiled),
            Self::Links => None,
        }
    }
}

#[derive(Debug, Clone)]
pub(crate) struct ExecutedSqliteQuery {
    pub(crate) rows: QueryRows,
    pub(crate) relation: MatchedSqlRelation,
}

pub(crate) fn heading_relation_columns() -> &'static str {
    HEADING_RELATION_COLUMNS
}

pub(crate) fn file_relation_columns() -> &'static str {
    FILE_RELATION_COLUMNS
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
#[serde(tag = "kind", rename_all = "snake_case")]
#[allow(clippy::large_enum_variant)] // Source-preserving heading fields intentionally remain inline.
pub enum HeadingQueryMatch {
    File(FileQueryRow),
    Heading(HeadingQueryRow),
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct HeadingQueryRow {
    pub id: i64,
    pub file_id: i64,
    pub file_path: String,
    pub parent_id: Option<i64>,
    pub level: i64,
    pub line_number: Option<i64>,
    pub byte_start: i64,
    pub byte_end: i64,
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
    pub all_tags_json: String,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct LinkQueryRow {
    pub id: i64,
    pub file_id: i64,
    pub file_path: String,
    pub heading_id: i64,
    pub heading_level: i64,
    pub source_context: String,
    pub format: String,
    pub link_type: String,
    pub raw: String,
    pub raw_target: String,
    pub raw_description: Option<String>,
    pub path: String,
    pub search_option: Option<String>,
    pub path_absolute: Option<String>,
    pub target_file_id: Option<i64>,
    pub target_heading_id: Option<i64>,
    pub target_custom_id: Option<String>,
    pub target_id: Option<String>,
    pub resolution_status: Option<String>,
    pub resolution_diagnostic: Option<String>,
    pub byte_start: i64,
    pub byte_end: i64,
    pub line: i64,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct FileQueryRow {
    pub id: i64,
    pub path: String,
    pub mtime_ns: i64,
    pub size: i64,
    pub content_hash: Option<String>,
    pub indexed_at: Option<i64>,
    pub root_heading_id: i64,
    pub root_title: String,
    pub root_title_raw: Option<String>,
    pub root_line_number: Option<i64>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum HeadingMatchKind {
    RealHeading,
    RootFile,
    AllHeadings,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum QueryExecutionErrorKind {
    UnsupportedPredicate,
    UnsupportedBackendFeature,
    DateResolution,
    Database,
}

#[derive(Debug)]
pub struct QueryExecutionError {
    pub kind: QueryExecutionErrorKind,
    pub target: QueryTarget,
    pub predicate: String,
    pub message: String,
    source: Option<rusqlite::Error>,
}

impl QueryExecutionError {
    fn unsupported_predicate(
        target: QueryTarget,
        predicate: impl Into<String>,
        message: impl Into<String>,
    ) -> Self {
        Self {
            kind: QueryExecutionErrorKind::UnsupportedPredicate,
            target,
            predicate: predicate.into(),
            message: message.into(),
            source: None,
        }
    }

    fn unsupported_backend_feature(
        target: QueryTarget,
        predicate: impl Into<String>,
        message: impl Into<String>,
    ) -> Self {
        Self {
            kind: QueryExecutionErrorKind::UnsupportedBackendFeature,
            target,
            predicate: predicate.into(),
            message: message.into(),
            source: None,
        }
    }

    fn date_resolution(
        target: QueryTarget,
        predicate: impl Into<String>,
        message: impl Into<String>,
    ) -> Self {
        Self {
            kind: QueryExecutionErrorKind::DateResolution,
            target,
            predicate: predicate.into(),
            message: message.into(),
            source: None,
        }
    }

    fn database(target: QueryTarget, operation: &'static str, source: rusqlite::Error) -> Self {
        Self {
            kind: QueryExecutionErrorKind::Database,
            target,
            predicate: operation.to_string(),
            message: format!("failed to execute SQLite query {operation}: {source}"),
            source: Some(source),
        }
    }
}

impl fmt::Display for QueryExecutionError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{} (target {}, predicate {})",
            self.message,
            target_name(self.target),
            self.predicate
        )
    }
}

impl std::error::Error for QueryExecutionError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        self.source
            .as_ref()
            .map(|source| source as &(dyn std::error::Error + 'static))
    }
}

#[derive(Debug, Clone)]
struct SqlFragment {
    sql: String,
    params: Vec<QueryParam>,
}

#[derive(Debug, Clone)]
struct QueryScope {
    target: QueryTarget,
    heading_match_kind: HeadingMatchKind,
    heading_alias: String,
    file_alias: String,
    root_alias: String,
    link_alias: String,
    link_heading_alias: String,
    outline_alias: String,
}

impl QueryScope {
    fn new(target: QueryTarget, id: usize) -> Self {
        Self {
            target,
            heading_match_kind: HeadingMatchKind::RealHeading,
            heading_alias: format!("h{id}"),
            file_alias: format!("f{id}"),
            root_alias: format!("r{id}"),
            link_alias: format!("l{id}"),
            link_heading_alias: format!("lh{id}"),
            outline_alias: format!("op{id}"),
        }
    }

    fn heading_root(id: usize) -> Self {
        Self {
            target: QueryTarget::Headings,
            heading_match_kind: HeadingMatchKind::RootFile,
            heading_alias: format!("h{id}"),
            file_alias: format!("f{id}"),
            root_alias: format!("r{id}"),
            link_alias: format!("l{id}"),
            link_heading_alias: format!("lh{id}"),
            outline_alias: format!("op{id}"),
        }
    }

    fn all_headings(id: usize) -> Self {
        Self {
            target: QueryTarget::Headings,
            heading_match_kind: HeadingMatchKind::AllHeadings,
            heading_alias: format!("h{id}"),
            file_alias: format!("f{id}"),
            root_alias: format!("r{id}"),
            link_alias: format!("l{id}"),
            link_heading_alias: format!("lh{id}"),
            outline_alias: format!("op{id}"),
        }
    }

    fn heading_col(&self, column: &str) -> String {
        format!("{}.{}", self.heading_alias, column)
    }

    fn file_col(&self, column: &str) -> String {
        format!("{}.{}", self.file_alias, column)
    }

    fn root_col(&self, column: &str) -> String {
        if self.target == QueryTarget::Headings
            && self.heading_match_kind == HeadingMatchKind::RootFile
        {
            self.heading_col(column)
        } else {
            format!("{}.{}", self.root_alias, column)
        }
    }

    fn link_col(&self, column: &str) -> String {
        format!("{}.{}", self.link_alias, column)
    }

    fn link_heading_col(&self, column: &str) -> String {
        format!("{}.{}", self.link_heading_alias, column)
    }
}

#[derive(Debug)]
struct AliasAllocator {
    next_scope_id: usize,
    metadata_predicate_strategy: MetadataPredicateSqlStrategy,
}

impl AliasAllocator {
    fn with_metadata_predicate_strategy(strategy: MetadataPredicateSqlStrategy) -> Self {
        Self {
            next_scope_id: 0,
            metadata_predicate_strategy: strategy,
        }
    }

    fn next_scope(&mut self, target: QueryTarget) -> QueryScope {
        let scope = QueryScope::new(target, self.next_scope_id);
        self.next_scope_id += 1;
        scope
    }

    fn next_heading_root_scope(&mut self) -> QueryScope {
        let scope = QueryScope::heading_root(self.next_scope_id);
        self.next_scope_id += 1;
        scope
    }

    fn next_all_headings_scope(&mut self) -> QueryScope {
        let scope = QueryScope::all_headings(self.next_scope_id);
        self.next_scope_id += 1;
        scope
    }
}

pub fn compile_sqlite_query(
    query: &ValidatedQuery,
) -> Result<CompiledSqlQuery, QueryExecutionError> {
    compile_sqlite_query_with_metadata_strategy(
        query,
        false,
        PRODUCTION_METADATA_PREDICATE_SQL_STRATEGY,
    )
}

pub(crate) fn compile_sqlite_query_with_file_restriction(
    query: &ValidatedQuery,
    restrict_files: bool,
) -> Result<CompiledSqlQuery, QueryExecutionError> {
    compile_sqlite_query_with_metadata_strategy(
        query,
        restrict_files,
        PRODUCTION_METADATA_PREDICATE_SQL_STRATEGY,
    )
}

pub(crate) fn compile_sqlite_query_with_metadata_strategy(
    query: &ValidatedQuery,
    restrict_files: bool,
    metadata_predicate_strategy: MetadataPredicateSqlStrategy,
) -> Result<CompiledSqlQuery, QueryExecutionError> {
    ensure_relative_dates_resolved(query).map_err(|error| {
        QueryExecutionError::date_resolution(
            query.target,
            "relative-date-resolution",
            error.to_string(),
        )
    })?;
    let mut aliases = AliasAllocator::with_metadata_predicate_strategy(metadata_predicate_strategy);
    let scope = aliases.next_scope(query.target);
    let where_clause = add_file_restriction(
        compile_query_match_filter(query, &scope, &mut aliases)?,
        &scope,
        restrict_files,
    );
    let bound_parameter_count = where_clause
        .as_ref()
        .map_or(0, |fragment| fragment.params.len());

    let sql = match query.target {
        QueryTarget::Headings => format!(
            "/* orgfdb:match-headings params={bound_parameter_count} */
             SELECT
                {heading_id},
                {heading_file_id},
                {file_path},
                {heading_parent_id},
                {heading_level},
                {heading_line_number},
                {heading_byte_start},
                {heading_byte_end},
                {heading_title},
                {heading_title_raw},
                {heading_todo_keyword},
                {heading_todo_type},
                {heading_priority},
                {heading_scheduled_raw},
                {heading_scheduled_ts},
                {heading_deadline_raw},
                {heading_deadline_ts},
                {heading_closed_raw},
                {heading_closed_ts},
                {heading_archivedp},
                {heading_footnote_section_p}
             {}
             {}",
            heading_from_clause(
                &scope,
                true,
                fragment_references_root(&scope, where_clause.as_ref()),
            ),
            render_where_clause(where_clause.as_ref()),
            heading_id = scope.heading_col("id"),
            heading_file_id = scope.heading_col("file_id"),
            file_path = scope.file_col("path"),
            heading_parent_id = scope.heading_col("parent_id"),
            heading_level = scope.heading_col("level"),
            heading_line_number = scope.heading_col("line_number"),
            heading_byte_start = scope.heading_col("byte_start"),
            heading_byte_end = scope.heading_col("byte_end"),
            heading_title = scope.heading_col("title"),
            heading_title_raw = scope.heading_col("title_raw"),
            heading_todo_keyword = scope.heading_col("todo_keyword"),
            heading_todo_type = scope.heading_col("todo_type"),
            heading_priority = scope.heading_col("priority"),
            heading_scheduled_raw = scope.heading_col("scheduled_raw"),
            heading_scheduled_ts = scope.heading_col("scheduled_ts"),
            heading_deadline_raw = scope.heading_col("deadline_raw"),
            heading_deadline_ts = scope.heading_col("deadline_ts"),
            heading_closed_raw = scope.heading_col("closed_raw"),
            heading_closed_ts = scope.heading_col("closed_ts"),
            heading_archivedp = scope.heading_col("archivedp"),
            heading_footnote_section_p = scope.heading_col("footnote_section_p"),
        ),
        QueryTarget::Links => format!(
            "/* orgfdb:match-links params={bound_parameter_count} */
             SELECT
                {link_id},
                {link_file_id},
                {file_path},
                {link_heading_id},
                {link_heading_level},
                {link_source_context},
                {link_format},
                {link_type},
                {link_raw},
                {link_raw_target},
                {link_raw_description},
                {link_path},
                {link_search_option},
                {link_path_absolute},
                {link_target_file_id},
                {link_target_heading_id},
                {link_target_custom_id},
                {link_target_id},
                {link_resolution_status},
                {link_resolution_diagnostic},
                {link_byte_start},
                {link_byte_end},
                {link_line}
             {}
             {}",
            link_from_clause(&scope, true),
            render_where_clause(where_clause.as_ref()),
            link_id = scope.link_col("id"),
            link_file_id = scope.link_col("file_id"),
            file_path = scope.file_col("path"),
            link_heading_id = scope.link_col("heading_id"),
            link_heading_level = scope.link_heading_col("level"),
            link_source_context = scope.link_col("source_context"),
            link_format = scope.link_col("format"),
            link_type = scope.link_col("link_type"),
            link_raw = scope.link_col("raw"),
            link_raw_target = scope.link_col("raw_target"),
            link_raw_description = scope.link_col("raw_description"),
            link_path = scope.link_col("path"),
            link_search_option = scope.link_col("search_option"),
            link_path_absolute = scope.link_col("path_absolute"),
            link_target_file_id = scope.link_col("target_file_id"),
            link_target_heading_id = scope.link_col("target_heading_id"),
            link_target_custom_id = scope.link_col("target_custom_id"),
            link_target_id = scope.link_col("target_id"),
            link_resolution_status = scope.link_col("resolution_status"),
            link_resolution_diagnostic = scope.link_col("resolution_diagnostic"),
            link_byte_start = scope.link_col("byte_start"),
            link_byte_end = scope.link_col("byte_end"),
            link_line = scope.link_col("line"),
        ),
        QueryTarget::Files => format!(
            "/* orgfdb:match-files params={bound_parameter_count} */
             SELECT
                {file_id},
                {file_path},
                {file_mtime_ns},
                {file_size},
                {file_content_hash},
                {file_indexed_at},
                {root_id},
                {root_title},
                {root_title_raw},
                {root_line_number}
             {}
             {}",
            file_from_clause(&scope, true),
            render_where_clause(where_clause.as_ref()),
            file_id = scope.file_col("id"),
            file_path = scope.file_col("path"),
            file_mtime_ns = scope.file_col("mtime_ns"),
            file_size = scope.file_col("size"),
            file_content_hash = scope.file_col("content_hash"),
            file_indexed_at = scope.file_col("indexed_at"),
            root_id = scope.root_col("id"),
            root_title = scope.root_col("title"),
            root_title_raw = scope.root_col("title_raw"),
            root_line_number = scope.root_col("line_number"),
        ),
    };

    Ok(CompiledSqlQuery {
        target: query.target,
        sql,
        params: where_clause.map_or_else(Vec::new, |fragment| fragment.params),
    })
}

pub fn execute_sqlite_query(
    connection: &Connection,
    query: &ValidatedQuery,
) -> Result<QueryRows, QueryExecutionError> {
    execute_sqlite_query_with_options(connection, query, &QueryExecutionOptions::default())
}

pub fn execute_sqlite_query_with_options(
    connection: &Connection,
    query: &ValidatedQuery,
    options: &QueryExecutionOptions,
) -> Result<QueryRows, QueryExecutionError> {
    let owns_snapshot = connection.is_autocommit();
    if owns_snapshot {
        connection
            .execute_batch("BEGIN DEFERRED TRANSACTION")
            .map_err(|source| {
                QueryExecutionError::database(query.target, "query_snapshot.begin", source)
            })?;
    }

    let result = execute_sqlite_query_with_relation(connection, query, options)
        .map(|executed| executed.rows);
    if !owns_snapshot {
        return result;
    }

    match result {
        Ok(rows) => {
            connection.execute_batch("COMMIT").map_err(|source| {
                QueryExecutionError::database(query.target, "query_snapshot.commit", source)
            })?;
            Ok(rows)
        }
        Err(error) => {
            let _ = connection.execute_batch("ROLLBACK");
            Err(error)
        }
    }
}

pub(crate) fn execute_sqlite_query_with_relation(
    connection: &Connection,
    query: &ValidatedQuery,
    options: &QueryExecutionOptions,
) -> Result<ExecutedSqliteQuery, QueryExecutionError> {
    execute_sqlite_query_with_relation_and_metadata_strategy(
        connection,
        query,
        options,
        PRODUCTION_METADATA_PREDICATE_SQL_STRATEGY,
    )
}

pub(crate) fn execute_sqlite_query_with_relation_and_metadata_strategy(
    connection: &Connection,
    query: &ValidatedQuery,
    options: &QueryExecutionOptions,
    metadata_predicate_strategy: MetadataPredicateSqlStrategy,
) -> Result<ExecutedSqliteQuery, QueryExecutionError> {
    let resolved_relative_dates = resolve_relative_dates(
        query,
        &QueryDateResolutionOptions {
            timezone: options.query_timezone.clone(),
            now_utc: options.now_utc,
        },
    )
    .map_err(|error| {
        QueryExecutionError::date_resolution(
            query.target,
            "relative-date-resolution",
            error.to_string(),
        )
    })?;
    let resolved = resolve_temporal_bounds(
        &resolved_relative_dates,
        &QueryDateResolutionOptions {
            timezone: options.query_timezone.clone(),
            now_utc: options.now_utc,
        },
    )
    .map_err(|error| {
        QueryExecutionError::date_resolution(
            query.target,
            "temporal-bound-resolution",
            error.to_string(),
        )
    })?;

    ensure_body_text_backend_capabilities(connection, &resolved)?;
    let restrict_files = options.restricted_file_paths.is_some();
    if let Some(paths) = options.restricted_file_paths.as_deref() {
        prepare_file_restriction(connection, resolved.target, paths)?;
    }

    match resolved.target {
        QueryTarget::Headings => {
            let (compiled, compile_duration) = benchmark_trace::timed(|| {
                compile_sqlite_query_with_metadata_strategy(
                    &resolved,
                    restrict_files,
                    metadata_predicate_strategy,
                )
            });
            let compiled = compiled?;
            benchmark_trace::record(
                benchmark_trace::QUERY_COMPILATION,
                "match-headings",
                compile_duration,
                0,
                0,
                0,
            );
            let mut rows = execute_heading_rows_query(connection, &compiled)?
                .into_iter()
                .map(HeadingQueryMatch::Heading)
                .collect::<Vec<_>>();
            let root_compiled =
                if heading_root_truth(resolved.predicate.as_ref()) != StaticTruth::False {
                    let (root_compiled, compile_duration) = benchmark_trace::timed(|| {
                        compile_heading_root_file_query(
                            &resolved,
                            restrict_files,
                            metadata_predicate_strategy,
                        )
                    });
                    let root_compiled = root_compiled?;
                    benchmark_trace::record(
                        benchmark_trace::QUERY_COMPILATION,
                        "match-heading-roots",
                        compile_duration,
                        0,
                        0,
                        0,
                    );
                    rows.extend(
                        execute_file_rows_query(connection, &root_compiled)?
                            .into_iter()
                            .map(HeadingQueryMatch::File),
                    );
                    Some(root_compiled)
                } else {
                    None
                };
            let ((), sort_duration) =
                benchmark_trace::timed(|| rows.sort_by(compare_heading_query_matches));
            benchmark_trace::record(
                benchmark_trace::RUST_LOCAL_SORTING,
                "matched-heading-order",
                sort_duration,
                rows.len(),
                0,
                0,
            );
            Ok(ExecutedSqliteQuery {
                rows: QueryRows::Headings(rows),
                relation: MatchedSqlRelation::Headings {
                    headings: compiled,
                    roots: root_compiled,
                },
            })
        }
        QueryTarget::Links => {
            let (compiled, compile_duration) = benchmark_trace::timed(|| {
                compile_sqlite_query_with_file_restriction(&resolved, restrict_files)
            });
            let compiled = compiled?;
            benchmark_trace::record(
                benchmark_trace::QUERY_COMPILATION,
                "match-links",
                compile_duration,
                0,
                0,
                0,
            );
            let rows = execute_links_query(connection, &compiled)?;
            Ok(ExecutedSqliteQuery {
                rows,
                relation: MatchedSqlRelation::Links,
            })
        }
        QueryTarget::Files => {
            let (compiled, compile_duration) = benchmark_trace::timed(|| {
                compile_sqlite_query_with_file_restriction(&resolved, restrict_files)
            });
            let compiled = compiled?;
            benchmark_trace::record(
                benchmark_trace::QUERY_COMPILATION,
                "match-files",
                compile_duration,
                0,
                0,
                0,
            );
            let rows = execute_files_query(connection, &compiled)?;
            Ok(ExecutedSqliteQuery {
                rows,
                relation: MatchedSqlRelation::Files(compiled),
            })
        }
    }
}

pub fn sqlite_query_validation_options(
    connection: &Connection,
) -> Result<QueryValidationOptions, QueryExecutionError> {
    Ok(QueryValidationOptions {
        body_text_available: sqlite_body_text_available(connection)?,
        regexp_matching_supported: true,
    })
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum StaticTruth {
    True,
    False,
    Unknown,
}

fn heading_root_truth(expr: Option<&ValidatedExpr>) -> StaticTruth {
    let Some(expr) = expr else {
        return StaticTruth::True;
    };
    match expr {
        ValidatedExpr::And(children) => {
            let mut saw_unknown = false;
            for child in children {
                match heading_root_truth(Some(child)) {
                    StaticTruth::False => return StaticTruth::False,
                    StaticTruth::Unknown => saw_unknown = true,
                    StaticTruth::True => {}
                }
            }
            if saw_unknown {
                StaticTruth::Unknown
            } else {
                StaticTruth::True
            }
        }
        ValidatedExpr::Or(children) => {
            let mut saw_unknown = false;
            for child in children {
                match heading_root_truth(Some(child)) {
                    StaticTruth::True => return StaticTruth::True,
                    StaticTruth::Unknown => saw_unknown = true,
                    StaticTruth::False => {}
                }
            }
            if saw_unknown {
                StaticTruth::Unknown
            } else {
                StaticTruth::False
            }
        }
        ValidatedExpr::Not(child) => match heading_root_truth(Some(child)) {
            StaticTruth::True => StaticTruth::False,
            StaticTruth::False => StaticTruth::True,
            StaticTruth::Unknown => StaticTruth::Unknown,
        },
        ValidatedExpr::Predicate(predicate) => heading_root_predicate_truth(predicate),
    }
}

fn heading_root_predicate_truth(predicate: &ValidatedPredicate) -> StaticTruth {
    match predicate.name.as_str() {
        "level" => level_predicate_truth_at_zero(predicate),
        "parent" | "ancestors" | "has-text" => StaticTruth::False,
        _ => StaticTruth::Unknown,
    }
}

fn level_predicate_truth_at_zero(predicate: &ValidatedPredicate) -> StaticTruth {
    let matches = match predicate.args.as_slice() {
        [ValidatedArg::Scalar(QueryValue::Integer(value))] => *value == 0,
        [ValidatedArg::Scalar(QueryValue::Integer(minimum)), ValidatedArg::Scalar(QueryValue::Integer(maximum))] => {
            *minimum <= 0 && 0 <= *maximum
        }
        [ValidatedArg::Scalar(QueryValue::Symbol(comparator)), ValidatedArg::Scalar(QueryValue::Integer(value))] => {
            match comparator.as_str() {
                "<" => 0 < *value,
                "<=" => 0 <= *value,
                ">" => 0 > *value,
                ">=" => 0 >= *value,
                _ => unreachable!("validator should guarantee a valid level comparator"),
            }
        }
        _ => unreachable!("validator should guarantee valid level arguments"),
    };
    if matches {
        StaticTruth::True
    } else {
        StaticTruth::False
    }
}

struct SqliteTraceTimer {
    sql_duration: Duration,
    decode_duration: Duration,
    rows: usize,
}

impl SqliteTraceTimer {
    fn new() -> Self {
        Self {
            sql_duration: Duration::ZERO,
            decode_duration: Duration::ZERO,
            rows: 0,
        }
    }

    fn sql<T>(&mut self, operation: impl FnOnce() -> T) -> T {
        let started = Instant::now();
        let result = operation();
        self.sql_duration += started.elapsed();
        result
    }

    fn decode<T>(&mut self, operation: impl FnOnce() -> T) -> T {
        self.rows += 1;
        let started = Instant::now();
        let result = operation();
        self.decode_duration += started.elapsed();
        result
    }

    fn record(
        self,
        phase: &'static str,
        operation: &'static str,
        statement_count: usize,
        bound_parameters: usize,
    ) {
        benchmark_trace::record(
            phase,
            operation,
            self.sql_duration,
            self.rows,
            statement_count,
            bound_parameters,
        );
        benchmark_trace::record(
            benchmark_trace::SQLITE_ROW_DECODING,
            operation,
            self.decode_duration,
            self.rows,
            0,
            0,
        );
    }
}

fn execute_heading_rows_query(
    connection: &Connection,
    compiled: &CompiledSqlQuery,
) -> Result<Vec<HeadingQueryRow>, QueryExecutionError> {
    if !benchmark_trace::active() {
        return execute_heading_rows_query_untraced(connection, compiled);
    }
    execute_heading_rows_query_traced(connection, compiled)
}

fn execute_heading_rows_query_traced(
    connection: &Connection,
    compiled: &CompiledSqlQuery,
) -> Result<Vec<HeadingQueryRow>, QueryExecutionError> {
    let mut timer = SqliteTraceTimer::new();
    let mut statement = timer
        .sql(|| connection.prepare(&compiled.sql))
        .map_err(|source| QueryExecutionError::database(compiled.target, "prepare", source))?;
    let mut sqlite_rows = timer
        .sql(|| statement.query(params_from_iter(compiled.params.iter())))
        .map_err(|source| QueryExecutionError::database(compiled.target, "query", source))?;
    let mut rows = Vec::new();
    loop {
        let row = timer
            .sql(|| sqlite_rows.next())
            .map_err(|source| QueryExecutionError::database(compiled.target, "collect", source))?;
        let Some(row) = row else {
            break;
        };
        let decoded = timer.decode(|| -> rusqlite::Result<HeadingQueryRow> {
            let priority: Option<String> = row.get(12)?;
            Ok(HeadingQueryRow {
                id: row.get(0)?,
                file_id: row.get(1)?,
                file_path: row.get(2)?,
                parent_id: row.get(3)?,
                level: row.get(4)?,
                line_number: row.get(5)?,
                byte_start: row.get(6)?,
                byte_end: row.get(7)?,
                title: row.get(8)?,
                title_raw: row.get(9)?,
                todo_keyword: row.get(10)?,
                todo_type: row.get(11)?,
                priority,
                scheduled_raw: row.get(13)?,
                scheduled_ts: row.get(14)?,
                deadline_raw: row.get(15)?,
                deadline_ts: row.get(16)?,
                closed_raw: row.get(17)?,
                closed_ts: row.get(18)?,
                archivedp: row.get::<_, i64>(19)? != 0,
                footnote_section_p: row.get::<_, i64>(20)? != 0,
                all_tags_json: "[]".to_string(),
            })
        });
        rows.push(
            decoded.map_err(|source| {
                QueryExecutionError::database(compiled.target, "collect", source)
            })?,
        );
    }
    timer.record(
        benchmark_trace::MATCHED_SQL_EXECUTION,
        "match-headings",
        1,
        compiled.params.len(),
    );
    load_effective_tags_for_heading_rows(connection, compiled, &mut rows)?;
    Ok(rows)
}

fn execute_heading_rows_query_untraced(
    connection: &Connection,
    compiled: &CompiledSqlQuery,
) -> Result<Vec<HeadingQueryRow>, QueryExecutionError> {
    let mut statement = connection
        .prepare(&compiled.sql)
        .map_err(|source| QueryExecutionError::database(compiled.target, "prepare", source))?;
    let rows = statement
        .query_map(params_from_iter(compiled.params.iter()), |row| {
            let priority: Option<String> = row.get(12)?;
            Ok(HeadingQueryRow {
                id: row.get(0)?,
                file_id: row.get(1)?,
                file_path: row.get(2)?,
                parent_id: row.get(3)?,
                level: row.get(4)?,
                line_number: row.get(5)?,
                byte_start: row.get(6)?,
                byte_end: row.get(7)?,
                title: row.get(8)?,
                title_raw: row.get(9)?,
                todo_keyword: row.get(10)?,
                todo_type: row.get(11)?,
                priority,
                scheduled_raw: row.get(13)?,
                scheduled_ts: row.get(14)?,
                deadline_raw: row.get(15)?,
                deadline_ts: row.get(16)?,
                closed_raw: row.get(17)?,
                closed_ts: row.get(18)?,
                archivedp: row.get::<_, i64>(19)? != 0,
                footnote_section_p: row.get::<_, i64>(20)? != 0,
                all_tags_json: "[]".to_string(),
            })
        })
        .map_err(|source| QueryExecutionError::database(compiled.target, "query", source))?;
    let mut rows = rows
        .collect::<Result<Vec<_>, _>>()
        .map_err(|source| QueryExecutionError::database(compiled.target, "collect", source))?;
    load_effective_tags_for_heading_rows(connection, compiled, &mut rows)?;
    Ok(rows)
}

fn load_effective_tags_for_heading_rows(
    connection: &Connection,
    compiled: &CompiledSqlQuery,
    rows: &mut [HeadingQueryRow],
) -> Result<(), QueryExecutionError> {
    if !benchmark_trace::active() {
        return load_effective_tags_for_heading_rows_untraced(connection, compiled, rows);
    }
    load_effective_tags_for_heading_rows_traced(connection, compiled, rows)
}

fn load_effective_tags_for_heading_rows_traced(
    connection: &Connection,
    compiled: &CompiledSqlQuery,
    rows: &mut [HeadingQueryRow],
) -> Result<(), QueryExecutionError> {
    if rows.is_empty() {
        return Ok(());
    }

    let sql = format!(
        "/* orgfdb:query-heading-tags params={} */
         WITH matched({}) AS ({})
         SELECT effective_tags.heading_id, effective_tags.position, effective_tags.tag
         FROM matched
         INNER JOIN effective_tags
           ON effective_tags.heading_id = matched.id",
        compiled.params.len(),
        HEADING_RELATION_COLUMNS,
        compiled.sql
    );
    let mut timer = SqliteTraceTimer::new();
    let mut statement = timer.sql(|| connection.prepare(&sql)).map_err(|source| {
        QueryExecutionError::database(compiled.target, "load_effective_tags.prepare", source)
    })?;
    let mut tag_rows = timer
        .sql(|| statement.query(params_from_iter(compiled.params.iter())))
        .map_err(|source| {
            QueryExecutionError::database(compiled.target, "load_effective_tags.query", source)
        })?;
    let mut by_heading = std::collections::HashMap::<i64, Vec<(i64, String)>>::new();
    let mut grouping_duration = Duration::ZERO;
    loop {
        let row = timer.sql(|| tag_rows.next()).map_err(|source| {
            QueryExecutionError::database(compiled.target, "load_effective_tags.collect", source)
        })?;
        let Some(row) = row else {
            break;
        };
        let decoded = timer.decode(|| -> rusqlite::Result<(i64, i64, String)> {
            Ok((row.get(0)?, row.get(1)?, row.get(2)?))
        });
        let (heading_id, position, tag) = decoded.map_err(|source| {
            QueryExecutionError::database(compiled.target, "load_effective_tags.collect", source)
        })?;
        let started = Instant::now();
        by_heading
            .entry(heading_id)
            .or_default()
            .push((position, tag));
        grouping_duration += started.elapsed();
    }
    let loaded_rows = timer.rows;
    timer.record(
        benchmark_trace::ENRICHMENT_SQL_EXECUTION,
        "query-heading-tags",
        1,
        compiled.params.len(),
    );
    benchmark_trace::record(
        benchmark_trace::RUST_GROUPING,
        "query-heading-tags",
        grouping_duration,
        loaded_rows,
        0,
        0,
    );

    let started = Instant::now();
    let mut tag_count = 0usize;
    for row in rows {
        let mut positioned_tags = by_heading.remove(&row.id).unwrap_or_default();
        tag_count += positioned_tags.len();
        positioned_tags.sort_by_key(|(position, _)| *position);
        let tags = positioned_tags
            .into_iter()
            .map(|(_, tag)| tag)
            .collect::<Vec<_>>();
        row.all_tags_json = serde_json::to_string(&tags)
            .expect("serializing effective tag strings to JSON cannot fail");
    }
    benchmark_trace::record(
        benchmark_trace::RUST_LOCAL_SORTING,
        "query-heading-tags",
        started.elapsed(),
        tag_count,
        0,
        0,
    );
    Ok(())
}

fn load_effective_tags_for_heading_rows_untraced(
    connection: &Connection,
    compiled: &CompiledSqlQuery,
    rows: &mut [HeadingQueryRow],
) -> Result<(), QueryExecutionError> {
    if rows.is_empty() {
        return Ok(());
    }

    let sql = format!(
        "/* orgfdb:query-heading-tags params={} */
         WITH matched({}) AS ({})
         SELECT effective_tags.heading_id, effective_tags.position, effective_tags.tag
         FROM matched
         INNER JOIN effective_tags
           ON effective_tags.heading_id = matched.id",
        compiled.params.len(),
        HEADING_RELATION_COLUMNS,
        compiled.sql
    );
    let mut statement = connection.prepare(&sql).map_err(|source| {
        QueryExecutionError::database(compiled.target, "load_effective_tags.prepare", source)
    })?;
    let tag_rows = statement
        .query_map(params_from_iter(compiled.params.iter()), |row| {
            Ok((
                row.get::<_, i64>(0)?,
                row.get::<_, i64>(1)?,
                row.get::<_, String>(2)?,
            ))
        })
        .map_err(|source| {
            QueryExecutionError::database(compiled.target, "load_effective_tags.query", source)
        })?;
    let mut by_heading = std::collections::HashMap::<i64, Vec<(i64, String)>>::new();
    for tag_row in tag_rows {
        let (heading_id, position, tag) = tag_row.map_err(|source| {
            QueryExecutionError::database(compiled.target, "load_effective_tags.collect", source)
        })?;
        by_heading
            .entry(heading_id)
            .or_default()
            .push((position, tag));
    }

    for row in rows {
        let mut positioned_tags = by_heading.remove(&row.id).unwrap_or_default();
        positioned_tags.sort_by_key(|(position, _)| *position);
        let tags = positioned_tags
            .into_iter()
            .map(|(_, tag)| tag)
            .collect::<Vec<_>>();
        row.all_tags_json = serde_json::to_string(&tags)
            .expect("serializing effective tag strings to JSON cannot fail");
    }
    Ok(())
}

fn execute_links_query(
    connection: &Connection,
    compiled: &CompiledSqlQuery,
) -> Result<QueryRows, QueryExecutionError> {
    if !benchmark_trace::active() {
        return execute_links_query_untraced(connection, compiled);
    }
    execute_links_query_traced(connection, compiled)
}

fn execute_links_query_traced(
    connection: &Connection,
    compiled: &CompiledSqlQuery,
) -> Result<QueryRows, QueryExecutionError> {
    let mut timer = SqliteTraceTimer::new();
    let mut statement = timer
        .sql(|| connection.prepare(&compiled.sql))
        .map_err(|source| QueryExecutionError::database(compiled.target, "prepare", source))?;
    let mut sqlite_rows = timer
        .sql(|| statement.query(params_from_iter(compiled.params.iter())))
        .map_err(|source| QueryExecutionError::database(compiled.target, "query", source))?;
    let mut rows = Vec::new();
    loop {
        let row = timer
            .sql(|| sqlite_rows.next())
            .map_err(|source| QueryExecutionError::database(compiled.target, "collect", source))?;
        let Some(row) = row else {
            break;
        };
        let decoded = timer.decode(|| -> rusqlite::Result<LinkQueryRow> {
            Ok(LinkQueryRow {
                id: row.get(0)?,
                file_id: row.get(1)?,
                file_path: row.get(2)?,
                heading_id: row.get(3)?,
                heading_level: row.get(4)?,
                source_context: row.get(5)?,
                format: row.get(6)?,
                link_type: row.get(7)?,
                raw: row.get(8)?,
                raw_target: row.get(9)?,
                raw_description: row.get(10)?,
                path: row.get(11)?,
                search_option: row.get(12)?,
                path_absolute: row.get(13)?,
                target_file_id: row.get(14)?,
                target_heading_id: row.get(15)?,
                target_custom_id: row.get(16)?,
                target_id: row.get(17)?,
                resolution_status: row.get(18)?,
                resolution_diagnostic: row.get(19)?,
                byte_start: row.get(20)?,
                byte_end: row.get(21)?,
                line: row.get(22)?,
            })
        });
        rows.push(
            decoded.map_err(|source| {
                QueryExecutionError::database(compiled.target, "collect", source)
            })?,
        );
    }
    timer.record(
        benchmark_trace::MATCHED_SQL_EXECUTION,
        "match-links",
        1,
        compiled.params.len(),
    );
    let ((), sort_duration) = benchmark_trace::timed(|| {
        rows.sort_by(|left, right| {
            left.file_path
                .cmp(&right.file_path)
                .then_with(|| left.byte_start.cmp(&right.byte_start))
                .then_with(|| left.id.cmp(&right.id))
        });
    });
    benchmark_trace::record(
        benchmark_trace::RUST_LOCAL_SORTING,
        "matched-links-order",
        sort_duration,
        rows.len(),
        0,
        0,
    );
    Ok(QueryRows::Links(rows))
}

fn execute_links_query_untraced(
    connection: &Connection,
    compiled: &CompiledSqlQuery,
) -> Result<QueryRows, QueryExecutionError> {
    let mut statement = connection
        .prepare(&compiled.sql)
        .map_err(|source| QueryExecutionError::database(compiled.target, "prepare", source))?;
    let rows = statement
        .query_map(params_from_iter(compiled.params.iter()), |row| {
            Ok(LinkQueryRow {
                id: row.get(0)?,
                file_id: row.get(1)?,
                file_path: row.get(2)?,
                heading_id: row.get(3)?,
                heading_level: row.get(4)?,
                source_context: row.get(5)?,
                format: row.get(6)?,
                link_type: row.get(7)?,
                raw: row.get(8)?,
                raw_target: row.get(9)?,
                raw_description: row.get(10)?,
                path: row.get(11)?,
                search_option: row.get(12)?,
                path_absolute: row.get(13)?,
                target_file_id: row.get(14)?,
                target_heading_id: row.get(15)?,
                target_custom_id: row.get(16)?,
                target_id: row.get(17)?,
                resolution_status: row.get(18)?,
                resolution_diagnostic: row.get(19)?,
                byte_start: row.get(20)?,
                byte_end: row.get(21)?,
                line: row.get(22)?,
            })
        })
        .map_err(|source| QueryExecutionError::database(compiled.target, "query", source))?;
    let mut rows = rows
        .collect::<Result<Vec<_>, _>>()
        .map_err(|source| QueryExecutionError::database(compiled.target, "collect", source))?;
    rows.sort_by(|left, right| {
        left.file_path
            .cmp(&right.file_path)
            .then_with(|| left.byte_start.cmp(&right.byte_start))
            .then_with(|| left.id.cmp(&right.id))
    });
    Ok(QueryRows::Links(rows))
}

fn execute_files_query(
    connection: &Connection,
    compiled: &CompiledSqlQuery,
) -> Result<QueryRows, QueryExecutionError> {
    execute_file_rows_query(connection, compiled).map(QueryRows::Files)
}

fn execute_file_rows_query(
    connection: &Connection,
    compiled: &CompiledSqlQuery,
) -> Result<Vec<FileQueryRow>, QueryExecutionError> {
    if !benchmark_trace::active() {
        return execute_file_rows_query_untraced(connection, compiled);
    }
    execute_file_rows_query_traced(connection, compiled)
}

fn execute_file_rows_query_traced(
    connection: &Connection,
    compiled: &CompiledSqlQuery,
) -> Result<Vec<FileQueryRow>, QueryExecutionError> {
    let operation = if compiled.sql.contains("orgfdb:match-heading-roots") {
        "match-heading-roots"
    } else {
        "match-files"
    };
    let mut timer = SqliteTraceTimer::new();
    let mut statement = timer
        .sql(|| connection.prepare(&compiled.sql))
        .map_err(|source| QueryExecutionError::database(compiled.target, "prepare", source))?;
    let mut sqlite_rows = timer
        .sql(|| statement.query(params_from_iter(compiled.params.iter())))
        .map_err(|source| QueryExecutionError::database(compiled.target, "query", source))?;
    let mut rows = Vec::new();
    loop {
        let row = timer
            .sql(|| sqlite_rows.next())
            .map_err(|source| QueryExecutionError::database(compiled.target, "collect", source))?;
        let Some(row) = row else {
            break;
        };
        let decoded = timer.decode(|| -> rusqlite::Result<FileQueryRow> {
            Ok(FileQueryRow {
                id: row.get(0)?,
                path: row.get(1)?,
                mtime_ns: row.get(2)?,
                size: row.get(3)?,
                content_hash: row.get(4)?,
                indexed_at: row.get(5)?,
                root_heading_id: row.get(6)?,
                root_title: row.get(7)?,
                root_title_raw: row.get(8)?,
                root_line_number: row.get(9)?,
            })
        });
        rows.push(
            decoded.map_err(|source| {
                QueryExecutionError::database(compiled.target, "collect", source)
            })?,
        );
    }
    timer.record(
        benchmark_trace::MATCHED_SQL_EXECUTION,
        operation,
        1,
        compiled.params.len(),
    );
    let ((), sort_duration) = benchmark_trace::timed(|| {
        rows.sort_by(|left, right| {
            left.path
                .cmp(&right.path)
                .then_with(|| left.id.cmp(&right.id))
        });
    });
    benchmark_trace::record(
        benchmark_trace::RUST_LOCAL_SORTING,
        "matched-files-order",
        sort_duration,
        rows.len(),
        0,
        0,
    );
    Ok(rows)
}

fn execute_file_rows_query_untraced(
    connection: &Connection,
    compiled: &CompiledSqlQuery,
) -> Result<Vec<FileQueryRow>, QueryExecutionError> {
    let mut statement = connection
        .prepare(&compiled.sql)
        .map_err(|source| QueryExecutionError::database(compiled.target, "prepare", source))?;
    let rows = statement
        .query_map(params_from_iter(compiled.params.iter()), |row| {
            Ok(FileQueryRow {
                id: row.get(0)?,
                path: row.get(1)?,
                mtime_ns: row.get(2)?,
                size: row.get(3)?,
                content_hash: row.get(4)?,
                indexed_at: row.get(5)?,
                root_heading_id: row.get(6)?,
                root_title: row.get(7)?,
                root_title_raw: row.get(8)?,
                root_line_number: row.get(9)?,
            })
        })
        .map_err(|source| QueryExecutionError::database(compiled.target, "query", source))?;
    let mut rows = rows
        .collect::<Result<Vec<_>, _>>()
        .map_err(|source| QueryExecutionError::database(compiled.target, "collect", source))?;
    rows.sort_by(|left, right| {
        left.path
            .cmp(&right.path)
            .then_with(|| left.id.cmp(&right.id))
    });
    Ok(rows)
}

fn ensure_body_text_backend_capabilities(
    connection: &Connection,
    query: &ValidatedQuery,
) -> Result<(), QueryExecutionError> {
    if !query_requires_body_text(query) {
        return Ok(());
    }

    if sqlite_body_text_available(connection)? {
        return Ok(());
    }

    Err(QueryExecutionError::unsupported_backend_feature(
        query.target,
        "has-text",
        "has-text requires body text to be available in the database",
    ))
}

fn query_requires_body_text(query: &ValidatedQuery) -> bool {
    query
        .predicate
        .as_ref()
        .is_some_and(expr_requires_body_text)
}

fn expr_requires_body_text(expr: &ValidatedExpr) -> bool {
    match expr {
        ValidatedExpr::And(children) | ValidatedExpr::Or(children) => {
            children.iter().any(expr_requires_body_text)
        }
        ValidatedExpr::Not(child) => expr_requires_body_text(child),
        ValidatedExpr::Predicate(predicate) => {
            predicate.name == "has-text"
                || predicate
                    .args
                    .iter()
                    .any(|arg| matches!(arg, ValidatedArg::NestedQuery(query) if query_requires_body_text(query)))
        }
    }
}

fn sqlite_body_text_available(connection: &Connection) -> Result<bool, QueryExecutionError> {
    if !table_exists(connection, "db_metadata").map_err(|source| {
        QueryExecutionError::database(QueryTarget::Headings, "inspect schema", source)
    })? {
        return Ok(false);
    }
    if !table_exists(connection, "heading_bodies").map_err(|source| {
        QueryExecutionError::database(QueryTarget::Headings, "inspect schema", source)
    })? {
        return Ok(false);
    }

    let value = connection
        .query_row(
            "SELECT value FROM db_metadata WHERE key = ?1",
            [DB_METADATA_BODY_TEXT_AVAILABLE_KEY],
            |row| row.get::<_, String>(0),
        )
        .map(Some)
        .or_else(|error| match error {
            rusqlite::Error::QueryReturnedNoRows => Ok(None),
            other => Err(other),
        })
        .map_err(|source| {
            QueryExecutionError::database(QueryTarget::Headings, "load db_metadata", source)
        })?;

    Ok(matches!(value.as_deref(), Some("1")))
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

fn compile_query_match_filter(
    query: &ValidatedQuery,
    scope: &QueryScope,
    aliases: &mut AliasAllocator,
) -> Result<Option<SqlFragment>, QueryExecutionError> {
    compile_scope_match_filter(scope, aliases, query.predicate.as_ref())
}

fn compile_scope_match_filter(
    scope: &QueryScope,
    aliases: &mut AliasAllocator,
    predicate: Option<&ValidatedExpr>,
) -> Result<Option<SqlFragment>, QueryExecutionError> {
    let mut fragments = Vec::new();
    if let Some(base_filter) = scope_base_filter(scope) {
        fragments.push(base_filter);
    }
    if let Some(predicate) = predicate {
        fragments.push(compile_expr(scope, aliases, predicate)?);
    }
    Ok(combine_fragments_with("AND", fragments))
}

fn scope_base_filter(scope: &QueryScope) -> Option<SqlFragment> {
    match scope.target {
        QueryTarget::Headings => match scope.heading_match_kind {
            HeadingMatchKind::RealHeading => Some(sql_literal(&format!(
                "({} > 0)",
                scope.heading_col("level")
            ))),
            HeadingMatchKind::RootFile => Some(sql_literal(&format!(
                "({} = 0)",
                scope.heading_col("level")
            ))),
            HeadingMatchKind::AllHeadings => None,
        },
        QueryTarget::Links | QueryTarget::Files => None,
    }
}

fn combine_fragments_with(op: &str, fragments: Vec<SqlFragment>) -> Option<SqlFragment> {
    let mut fragments = fragments.into_iter();
    let first = fragments.next()?;
    let mut sql_parts = vec![first.sql];
    let mut params = first.params;
    for fragment in fragments {
        sql_parts.push(fragment.sql);
        params.extend(fragment.params);
    }
    Some(SqlFragment {
        sql: format!("({})", sql_parts.join(&format!(" {op} "))),
        params,
    })
}

fn compile_expr(
    scope: &QueryScope,
    aliases: &mut AliasAllocator,
    expr: &ValidatedExpr,
) -> Result<SqlFragment, QueryExecutionError> {
    match expr {
        ValidatedExpr::And(children) => compile_logical(scope, aliases, children, "AND", "1 = 1"),
        ValidatedExpr::Or(children) => compile_logical(scope, aliases, children, "OR", "0 = 1"),
        ValidatedExpr::Not(child) => {
            let fragment = compile_expr(scope, aliases, child)?;
            Ok(SqlFragment {
                sql: format!("(NOT {})", fragment.sql),
                params: fragment.params,
            })
        }
        ValidatedExpr::Predicate(predicate) => compile_predicate(scope, aliases, predicate),
    }
}

fn compile_logical(
    scope: &QueryScope,
    aliases: &mut AliasAllocator,
    children: &[ValidatedExpr],
    op: &str,
    empty_sql: &str,
) -> Result<SqlFragment, QueryExecutionError> {
    if children.is_empty() {
        return Ok(SqlFragment {
            sql: format!("({empty_sql})"),
            params: Vec::new(),
        });
    }

    let mut sql_parts = Vec::with_capacity(children.len());
    let mut params = Vec::new();
    for child in children {
        let fragment = compile_expr(scope, aliases, child)?;
        sql_parts.push(fragment.sql);
        params.extend(fragment.params);
    }
    Ok(SqlFragment {
        sql: format!("({})", sql_parts.join(&format!(" {op} "))),
        params,
    })
}

fn compile_predicate(
    scope: &QueryScope,
    aliases: &mut AliasAllocator,
    predicate: &ValidatedPredicate,
) -> Result<SqlFragment, QueryExecutionError> {
    match scope.target {
        QueryTarget::Headings => compile_heading_predicate(scope, aliases, predicate),
        QueryTarget::Links => compile_link_predicate(scope, aliases, predicate),
        QueryTarget::Files => compile_file_predicate(scope, aliases, predicate),
    }
}

fn compile_heading_predicate(
    scope: &QueryScope,
    aliases: &mut AliasAllocator,
    predicate: &ValidatedPredicate,
) -> Result<SqlFragment, QueryExecutionError> {
    match predicate.name.as_str() {
        "todo" => compile_todo_predicate(scope, predicate),
        "done" => Ok(sql_literal(&format!(
            "({} = 'closed')",
            scope.heading_col("todo_type")
        ))),
        "priority" => compile_priority_predicate(
            QueryTarget::Headings,
            &scope.heading_col("priority"),
            predicate,
        ),
        "title" => compile_heading_title_predicate(scope, predicate),
        "level" => compile_level_predicate(scope, predicate),
        "file-name" => compile_text_predicate(
            QueryTarget::Headings,
            &sqlite_file_name_expr(&scope.file_col("path")),
            predicate,
            false,
        ),
        "file-path" => compile_home_path_predicate(
            QueryTarget::Headings,
            "file-path",
            &scope.file_col("path"),
            predicate,
        ),
        "file-dir" => compile_home_path_predicate(
            QueryTarget::Headings,
            "file-dir",
            &sqlite_file_dir_expr(&scope.file_col("path")),
            predicate,
        ),
        "file-title" => compile_text_predicate(
            QueryTarget::Headings,
            &scope.root_col("title"),
            predicate,
            false,
        ),
        "file-modified" => compile_date_predicate(
            QueryTarget::Headings,
            "file-modified",
            &scope.file_col("mtime_ns"),
            &predicate.options,
            true,
        ),
        "tags" => {
            compile_heading_tags_predicate(scope, predicate, aliases.metadata_predicate_strategy)
        }
        "property" => compile_heading_property_predicate(
            scope,
            predicate,
            aliases.metadata_predicate_strategy,
        ),
        "keyword" => {
            compile_heading_keyword_predicate(scope, predicate, aliases.metadata_predicate_strategy)
        }
        "scheduled" => compile_date_predicate(
            QueryTarget::Headings,
            "scheduled",
            &scope.heading_col("scheduled_ts"),
            &predicate.options,
            true,
        ),
        "deadline" => compile_date_predicate(
            QueryTarget::Headings,
            "deadline",
            &scope.heading_col("deadline_ts"),
            &predicate.options,
            true,
        ),
        "closed" => compile_date_predicate(
            QueryTarget::Headings,
            "closed",
            &scope.heading_col("closed_ts"),
            &predicate.options,
            true,
        ),
        "planning" => compile_planning_predicate(scope, predicate),
        "ts" => compile_timestamp_exists_predicate(scope, predicate, None),
        "ts-active" => compile_timestamp_exists_predicate(scope, predicate, Some("active")),
        "ts-inactive" => compile_timestamp_exists_predicate(scope, predicate, Some("inactive")),
        "parent" => compile_heading_root_false_predicate(
            scope,
            aliases,
            predicate,
            HeadingHierarchyRelation::Parent,
        ),
        "ancestors" => compile_heading_root_false_predicate(
            scope,
            aliases,
            predicate,
            HeadingHierarchyRelation::Ancestor,
        ),
        "children" => compile_heading_hierarchy_predicate(
            scope,
            aliases,
            predicate,
            HeadingHierarchyRelation::Child,
        ),
        "descendants" => compile_heading_hierarchy_predicate(
            scope,
            aliases,
            predicate,
            HeadingHierarchyRelation::Descendant,
        ),
        "has-link" => compile_has_link_predicate(scope, aliases, predicate),
        "links-to" => compile_links_to_predicate(scope, aliases, predicate),
        "linked-from" => compile_linked_from_predicate(scope, aliases, predicate),
        "has-text" => compile_has_text_predicate(scope, predicate),
        "outline-contains" => compile_outline_contains_predicate(scope, predicate),
        "outline-sequence" => compile_outline_sequence_predicate(scope, predicate),
        "link-type" | "link-target" | "link-description" | "has-description" | "status"
        | "source" | "target" => Err(QueryExecutionError::unsupported_predicate(
            QueryTarget::Headings,
            predicate.name.as_str(),
            format!(
                "predicate {} is not valid for target headings",
                predicate.name
            ),
        )),
        other => Err(QueryExecutionError::unsupported_predicate(
            QueryTarget::Headings,
            other,
            format!("predicate {other} is not supported by the SQLite metadata backend"),
        )),
    }
}

fn compile_heading_title_predicate(
    scope: &QueryScope,
    predicate: &ValidatedPredicate,
) -> Result<SqlFragment, QueryExecutionError> {
    compile_text_predicate(
        QueryTarget::Headings,
        &scope.heading_col("title"),
        predicate,
        false,
    )
}

fn compile_has_text_predicate(
    scope: &QueryScope,
    predicate: &ValidatedPredicate,
) -> Result<SqlFragment, QueryExecutionError> {
    if scope.heading_match_kind == HeadingMatchKind::RootFile {
        return Ok(sql_literal("(0 = 1)"));
    }

    let regexp = option_bool(&predicate.options, "regexp")?;

    let values = predicate
        .args
        .iter()
        .map(arg_as_string)
        .collect::<Result<Vec<_>, _>>()
        .map_err(|message| {
            QueryExecutionError::unsupported_backend_feature(
                QueryTarget::Headings,
                "has-text",
                message,
            )
        })?;

    let mut parts = Vec::with_capacity(values.len());
    let mut params = Vec::with_capacity(values.len());

    for value in values {
        parts.push(format!(
            "EXISTS (
                SELECT 1
                FROM heading_bodies
                WHERE heading_bodies.heading_id = {}
                  AND {}
            )",
            scope.heading_col("id"),
            if regexp {
                validate_regexp_pattern(QueryTarget::Headings, "has-text", &value)?;
                "orgfdb_regexp(?, heading_bodies.body_text) = 1".to_string()
            } else {
                "INSTR(LOWER(heading_bodies.body_text), LOWER(?)) > 0".to_string()
            }
        ));
        params.push(QueryParam::Text(value));
    }

    Ok(SqlFragment {
        sql: format!("({})", parts.join(" AND ")),
        params,
    })
}

fn compile_outline_contains_predicate(
    scope: &QueryScope,
    predicate: &ValidatedPredicate,
) -> Result<SqlFragment, QueryExecutionError> {
    let regexp = option_bool(&predicate.options, "regexp")?;

    let values = predicate
        .args
        .iter()
        .map(arg_as_string)
        .collect::<Result<Vec<_>, _>>()
        .map_err(|message| {
            QueryExecutionError::unsupported_backend_feature(
                QueryTarget::Headings,
                "outline-contains",
                message,
            )
        })?;

    let mut parts = Vec::with_capacity(values.len());
    let mut params = Vec::with_capacity(values.len());
    for value in values {
        let match_sql = if regexp {
            validate_regexp_pattern(QueryTarget::Headings, "outline-contains", &value)?;
            "orgfdb_regexp(?, CAST(breadcrumb.value AS TEXT)) = 1".to_string()
        } else {
            "INSTR(LOWER(CAST(breadcrumb.value AS TEXT)), LOWER(?)) > 0".to_string()
        };
        parts.push(format!(
            "EXISTS (
                SELECT 1
                FROM outline_path AS outline_match
                INNER JOIN json_each(outline_match.breadcrumbs_json) AS breadcrumb
                WHERE outline_match.heading_id = {}
                  AND {}
            )",
            scope.heading_col("id"),
            match_sql
        ));
        params.push(QueryParam::Text(value));
    }

    Ok(SqlFragment {
        sql: format!("({})", parts.join(" AND ")),
        params,
    })
}

fn compile_outline_sequence_predicate(
    scope: &QueryScope,
    predicate: &ValidatedPredicate,
) -> Result<SqlFragment, QueryExecutionError> {
    let regexp = option_bool(&predicate.options, "regexp")?;
    let exact = option_bool(&predicate.options, "exact")?;
    let values = predicate
        .args
        .iter()
        .map(arg_as_string)
        .collect::<Result<Vec<_>, _>>()
        .map_err(|message| {
            QueryExecutionError::unsupported_backend_feature(
                QueryTarget::Headings,
                "outline-sequence",
                message,
            )
        })?;

    let mut joins = Vec::new();
    let mut predicates = vec![format!(
        "outline_match.heading_id = {}",
        scope.heading_col("id")
    )];
    let mut params = Vec::with_capacity(values.len());

    for (index, value) in values.into_iter().enumerate() {
        let alias = if index == 0 {
            "start_breadcrumb".to_string()
        } else {
            let alias = format!("breadcrumb_{index}");
            joins.push(format!(
                "INNER JOIN json_each(outline_match.breadcrumbs_json) AS {alias}
                    ON {alias}.key = start_breadcrumb.key + {index}"
            ));
            alias
        };
        let expression = format!("CAST({alias}.value AS TEXT)");
        if regexp {
            validate_regexp_pattern(QueryTarget::Headings, "outline-sequence", &value)?;
            predicates.push(format!("orgfdb_regexp(?, {expression}) = 1"));
        } else if exact {
            predicates.push(format!("LOWER({expression}) = LOWER(?)"));
        } else {
            predicates.push(format!("INSTR(LOWER({expression}), LOWER(?)) > 0"));
        }
        params.push(QueryParam::Text(value));
    }

    Ok(SqlFragment {
        sql: format!(
            "(EXISTS (
                SELECT 1
                FROM outline_path AS outline_match
                INNER JOIN json_each(outline_match.breadcrumbs_json) AS start_breadcrumb
                {}
                WHERE {}
            ))",
            if joins.is_empty() {
                String::new()
            } else {
                format!("\n                {}", joins.join("\n                "))
            },
            predicates.join("\n                  AND ")
        ),
        params,
    })
}

fn compile_link_predicate(
    scope: &QueryScope,
    aliases: &mut AliasAllocator,
    predicate: &ValidatedPredicate,
) -> Result<SqlFragment, QueryExecutionError> {
    match predicate.name.as_str() {
        "link-type" => compile_in_list(
            QueryTarget::Links,
            &scope.link_col("link_type"),
            &predicate.args,
        ),
        "link-target" => compile_text_predicate(
            QueryTarget::Links,
            &scope.link_col("raw_target"),
            predicate,
            true,
        ),
        "link-description" => compile_text_predicate(
            QueryTarget::Links,
            &scope.link_col("raw_description"),
            predicate,
            true,
        ),
        "has-description" => Ok(sql_literal(&format!(
            "({} IS NOT NULL AND {} <> '')",
            scope.link_col("raw_description"),
            scope.link_col("raw_description")
        ))),
        "status" => compile_in_list(
            QueryTarget::Links,
            &scope.link_col("resolution_status"),
            &predicate.args,
        ),
        "source" => {
            compile_link_endpoint_predicate(scope, aliases, predicate, LinkEndpoint::Source)
        }
        "target" => {
            compile_link_endpoint_predicate(scope, aliases, predicate, LinkEndpoint::Target)
        }
        "has-text" | "outline-contains" | "outline-sequence" | "file-name" | "file-dir"
        | "parent" | "ancestors" | "children" | "descendants" | "has-link" | "links-to"
        | "linked-from" => Err(QueryExecutionError::unsupported_predicate(
            QueryTarget::Links,
            predicate.name.as_str(),
            format!(
                "predicate {} is not supported by the SQLite metadata backend",
                predicate.name
            ),
        )),
        "todo" | "done" | "priority" | "title" | "level" | "tags" | "property" | "keyword"
        | "file-path" | "file-title" | "file-modified" | "scheduled" | "deadline" | "closed"
        | "planning" | "ts" | "ts-active" | "ts-inactive" => {
            Err(QueryExecutionError::unsupported_predicate(
                QueryTarget::Links,
                predicate.name.as_str(),
                format!("predicate {} is not valid for target links", predicate.name),
            ))
        }
        other => Err(QueryExecutionError::unsupported_predicate(
            QueryTarget::Links,
            other,
            format!("predicate {other} is not supported by the SQLite metadata backend"),
        )),
    }
}

fn compile_file_predicate(
    scope: &QueryScope,
    aliases: &mut AliasAllocator,
    predicate: &ValidatedPredicate,
) -> Result<SqlFragment, QueryExecutionError> {
    match predicate.name.as_str() {
        "file-name" => compile_text_predicate(
            QueryTarget::Files,
            &sqlite_file_name_expr(&scope.file_col("path")),
            predicate,
            false,
        ),
        "file-path" => compile_home_path_predicate(
            QueryTarget::Files,
            "file-path",
            &scope.file_col("path"),
            predicate,
        ),
        "file-dir" => compile_home_path_predicate(
            QueryTarget::Files,
            "file-dir",
            &sqlite_file_dir_expr(&scope.file_col("path")),
            predicate,
        ),
        "file-title" => compile_text_predicate(
            QueryTarget::Files,
            &scope.root_col("title"),
            predicate,
            false,
        ),
        "file-modified" => compile_date_predicate(
            QueryTarget::Files,
            "file-modified",
            &scope.file_col("mtime_ns"),
            &predicate.options,
            true,
        ),
        "tags" => compile_file_tags_predicate(scope, predicate),
        "property" => compile_file_property_predicate(scope, predicate),
        "keyword" => compile_keyword_predicate(
            QueryTarget::Files,
            predicate,
            &scope.root_col("id"),
            MetadataPredicateSqlStrategy::HeadingDrivenExists,
        ),
        "has-link" => compile_has_link_predicate(scope, aliases, predicate),
        "links-to" => compile_links_to_predicate(scope, aliases, predicate),
        "linked-from" => compile_linked_from_predicate(scope, aliases, predicate),
        "has-text" | "outline-contains" | "outline-sequence" | "parent" | "ancestors"
        | "children" | "descendants" => Err(QueryExecutionError::unsupported_predicate(
            QueryTarget::Files,
            predicate.name.as_str(),
            format!(
                "predicate {} is not supported by the SQLite metadata backend",
                predicate.name
            ),
        )),
        "todo" | "done" | "priority" | "title" | "level" | "scheduled" | "deadline" | "closed"
        | "planning" | "ts" | "ts-active" | "ts-inactive" | "link-type" | "link-target"
        | "link-description" | "has-description" | "status" | "source" | "target" => {
            Err(QueryExecutionError::unsupported_predicate(
                QueryTarget::Files,
                predicate.name.as_str(),
                format!("predicate {} is not valid for target files", predicate.name),
            ))
        }
        other => Err(QueryExecutionError::unsupported_predicate(
            QueryTarget::Files,
            other,
            format!("predicate {other} is not supported by the SQLite metadata backend"),
        )),
    }
}

fn compile_todo_predicate(
    scope: &QueryScope,
    predicate: &ValidatedPredicate,
) -> Result<SqlFragment, QueryExecutionError> {
    if predicate.args.is_empty() {
        return Ok(sql_literal(&format!(
            "({} = 'open')",
            scope.heading_col("todo_type")
        )));
    }
    compile_in_list(
        QueryTarget::Headings,
        &scope.heading_col("todo_keyword"),
        &predicate.args,
    )
}

fn compile_level_predicate(
    scope: &QueryScope,
    predicate: &ValidatedPredicate,
) -> Result<SqlFragment, QueryExecutionError> {
    match predicate.args.as_slice() {
        [ValidatedArg::Scalar(QueryValue::Integer(value))] => Ok(SqlFragment {
            sql: format!("({} = ?)", scope.heading_col("level")),
            params: vec![QueryParam::Integer(*value)],
        }),
        [ValidatedArg::Scalar(QueryValue::Integer(minimum)), ValidatedArg::Scalar(QueryValue::Integer(maximum))] => {
            Ok(SqlFragment {
                sql: format!("({} BETWEEN ? AND ?)", scope.heading_col("level")),
                params: vec![QueryParam::Integer(*minimum), QueryParam::Integer(*maximum)],
            })
        }
        [ValidatedArg::Scalar(QueryValue::Symbol(comparator)), ValidatedArg::Scalar(QueryValue::Integer(value))]
            if matches!(comparator.as_str(), "<" | "<=" | ">" | ">=") =>
        {
            Ok(SqlFragment {
                sql: format!("({} {} ?)", scope.heading_col("level"), comparator),
                params: vec![QueryParam::Integer(*value)],
            })
        }
        _ => unreachable!("validator should guarantee valid level args"),
    }
}

fn compile_priority_predicate(
    target: QueryTarget,
    column: &str,
    predicate: &ValidatedPredicate,
) -> Result<SqlFragment, QueryExecutionError> {
    let rank = priority_rank_sql(column);
    match predicate.args.as_slice() {
        [ValidatedArg::Scalar(QueryValue::Symbol(comparator)), ValidatedArg::Scalar(QueryValue::String(value))]
            if matches!(comparator.as_str(), "<" | "<=" | ">" | ">=") =>
        {
            let value_rank = normalize_priority(value)
                .expect("validator should guarantee valid priority values");
            let comparison = match comparator.as_str() {
                ">" => "<",
                ">=" => "<=",
                "<" => ">",
                "<=" => ">=",
                _ => unreachable!("comparator was checked above"),
            };
            Ok(SqlFragment {
                sql: format!("({rank} {comparison} ?)"),
                params: vec![QueryParam::Integer(value_rank)],
            })
        }
        args => {
            let ranks = args
                .iter()
                .map(|arg| {
                    let value = arg_as_string(arg).map_err(|message| {
                        QueryExecutionError::unsupported_backend_feature(
                            target, "priority", message,
                        )
                    })?;
                    normalize_priority(&value).map_err(|message| {
                        QueryExecutionError::unsupported_backend_feature(
                            target, "priority", message,
                        )
                    })
                })
                .collect::<Result<Vec<_>, _>>()?;
            let placeholders = vec!["?"; ranks.len()].join(", ");
            Ok(SqlFragment {
                sql: format!("({rank} IN ({placeholders}))"),
                params: ranks.into_iter().map(QueryParam::Integer).collect(),
            })
        }
    }
}

fn priority_rank_sql(column: &str) -> String {
    format!(
        "(CASE \
         WHEN length({column}) = 1 AND {column} GLOB '[A-Z]' \
             THEN unicode({column}) - unicode('A') + 1 \
         WHEN {column} NOT GLOB '*[^0-9]*' AND {column} <> '' \
              AND CAST({column} AS INTEGER) BETWEEN 0 AND 64 \
             THEN CAST({column} AS INTEGER) \
         ELSE NULL END)"
    )
}

fn compile_text_predicate(
    target: QueryTarget,
    column: &str,
    predicate: &ValidatedPredicate,
    allow_nulls: bool,
) -> Result<SqlFragment, QueryExecutionError> {
    let regexp = option_bool(&predicate.options, "regexp")?;
    let exact = option_bool(&predicate.options, "exact")?;
    let values = predicate
        .args
        .iter()
        .map(arg_as_string)
        .collect::<Result<Vec<_>, _>>()
        .map_err(|message| {
            QueryExecutionError::unsupported_backend_feature(
                target,
                predicate.name.as_str(),
                message,
            )
        })?;

    let sql_column = if allow_nulls {
        format!("COALESCE({column}, '')")
    } else {
        column.to_string()
    };

    let mut parts = Vec::with_capacity(values.len());
    let mut params = Vec::with_capacity(values.len());
    for value in values {
        if regexp {
            validate_regexp_pattern(target, predicate.name.as_str(), &value)?;
            parts.push(format!("orgfdb_regexp(?, {sql_column}) = 1"));
        } else if exact {
            parts.push(format!("LOWER({sql_column}) = LOWER(?)"));
        } else {
            parts.push(format!("INSTR(LOWER({sql_column}), LOWER(?)) > 0"));
        }
        params.push(QueryParam::Text(value));
    }

    Ok(SqlFragment {
        sql: format!("({})", parts.join(" AND ")),
        params,
    })
}

fn compile_home_path_predicate(
    target: QueryTarget,
    predicate_name: &str,
    column: &str,
    predicate: &ValidatedPredicate,
) -> Result<SqlFragment, QueryExecutionError> {
    let mut predicate = predicate.clone();
    for argument in &mut predicate.args {
        let ValidatedArg::Scalar(QueryValue::String(value)) = argument else {
            unreachable!("validator should guarantee string path predicate arguments");
        };
        *value = expand_leading_home_path(value).map_err(|message| {
            QueryExecutionError::unsupported_backend_feature(target, predicate_name, message)
        })?;
    }
    compile_text_predicate(target, column, &predicate, false)
}

fn expand_leading_home_path(value: &str) -> Result<String, String> {
    expand_leading_home_path_with_home(value, env::var("HOME").ok().as_deref())
}

fn expand_leading_home_path_with_home(value: &str, home: Option<&str>) -> Result<String, String> {
    if value != "~" && !value.starts_with("~/") {
        return Ok(value.to_string());
    }
    let home = home.ok_or_else(|| {
        "a leading ~ requires the HOME environment variable to be available".to_string()
    })?;
    Ok(format!("{home}{}", &value[1..]))
}

fn sqlite_file_name_expr(path_sql: &str) -> String {
    format!(
        "(
            WITH RECURSIVE split(rest, segment) AS (
                SELECT {path_sql}, NULL
                UNION ALL
                SELECT
                    CASE
                        WHEN INSTR(rest, '/') = 0 THEN ''
                        ELSE SUBSTR(rest, INSTR(rest, '/') + 1)
                    END,
                    CASE
                        WHEN INSTR(rest, '/') = 0 THEN rest
                        ELSE SUBSTR(rest, 1, INSTR(rest, '/') - 1)
                    END
                FROM split
                WHERE rest <> ''
            )
            SELECT COALESCE(segment, '')
            FROM split
            WHERE rest = ''
            LIMIT 1
        )"
    )
}

fn sqlite_file_dir_expr(path_sql: &str) -> String {
    format!(
        "(
            WITH RECURSIVE slash_scan(rest, offset, last_slash) AS (
                SELECT {path_sql}, 0, 0
                UNION ALL
                SELECT
                    SUBSTR(rest, INSTR(rest, '/') + 1),
                    offset + INSTR(rest, '/'),
                    offset + INSTR(rest, '/')
                FROM slash_scan
                WHERE INSTR(rest, '/') > 0
            )
            SELECT CASE
                WHEN MAX(last_slash) = 0 THEN ''
                WHEN MAX(last_slash) = 1 THEN '/'
                ELSE SUBSTR({path_sql}, 1, MAX(last_slash) - 1)
            END
            FROM slash_scan
        )"
    )
}

fn compile_heading_tags_predicate(
    scope: &QueryScope,
    predicate: &ValidatedPredicate,
    metadata_predicate_strategy: MetadataPredicateSqlStrategy,
) -> Result<SqlFragment, QueryExecutionError> {
    if !option_bool_with_default(&predicate.options, "inherit", true)? {
        return compile_tags_predicate(
            QueryTarget::Headings,
            predicate,
            &scope.heading_col("id"),
            metadata_predicate_strategy,
        );
    }

    compile_heading_effective_tags_exists(scope, predicate)
}

fn compile_heading_effective_tags_exists(
    scope: &QueryScope,
    predicate: &ValidatedPredicate,
) -> Result<SqlFragment, QueryExecutionError> {
    let regexp = option_bool(&predicate.options, "regexp")?;
    let match_all = matches!(
        keyword_option(&predicate.options, "match")?.as_deref(),
        Some("all")
    );
    let tags = predicate
        .args
        .iter()
        .map(arg_as_string)
        .collect::<Result<Vec<_>, _>>()
        .map_err(|message| {
            QueryExecutionError::unsupported_backend_feature(QueryTarget::Headings, "tags", message)
        })?;
    if regexp {
        for tag in &tags {
            validate_regexp_pattern(QueryTarget::Headings, "tags", tag)?;
        }
    }

    let heading_id_sql = scope.heading_col("id");
    if match_all {
        let mut parts = Vec::with_capacity(tags.len());
        let mut params = Vec::with_capacity(tags.len());
        for tag in tags {
            let match_sql = if regexp {
                "orgfdb_regexp(?, effective_tags.tag) = 1"
            } else {
                "effective_tags.tag = ?"
            };
            parts.push(format!(
                "{heading_id_sql} IN (SELECT effective_tags.heading_id FROM effective_tags WHERE {match_sql})"
            ));
            params.push(QueryParam::Text(tag));
        }
        return Ok(SqlFragment {
            sql: format!("({})", parts.join(" AND ")),
            params,
        });
    }

    let match_sql = if regexp {
        let parts = vec!["orgfdb_regexp(?, effective_tags.tag) = 1"; tags.len()];
        format!("({})", parts.join(" OR "))
    } else {
        let placeholders = vec!["?"; tags.len()].join(", ");
        format!("effective_tags.tag IN ({placeholders})")
    };
    Ok(SqlFragment {
        sql: format!(
            "({heading_id_sql} IN (SELECT effective_tags.heading_id FROM effective_tags WHERE {match_sql}))"
        ),
        params: tags.into_iter().map(QueryParam::Text).collect(),
    })
}

fn compile_file_tags_predicate(
    scope: &QueryScope,
    predicate: &ValidatedPredicate,
) -> Result<SqlFragment, QueryExecutionError> {
    compile_tags_predicate(
        QueryTarget::Files,
        predicate,
        &scope.root_col("id"),
        MetadataPredicateSqlStrategy::HeadingDrivenExists,
    )
}

fn compile_tags_predicate(
    target: QueryTarget,
    predicate: &ValidatedPredicate,
    heading_id_sql: &str,
    metadata_predicate_strategy: MetadataPredicateSqlStrategy,
) -> Result<SqlFragment, QueryExecutionError> {
    let regexp = option_bool(&predicate.options, "regexp")?;
    let match_all = matches!(
        keyword_option(&predicate.options, "match")?.as_deref(),
        Some("all")
    );
    let tags = predicate
        .args
        .iter()
        .map(arg_as_string)
        .collect::<Result<Vec<_>, _>>()
        .map_err(|message| {
            QueryExecutionError::unsupported_backend_feature(target, "tags", message)
        })?;

    let predicate_driven =
        metadata_predicate_strategy == MetadataPredicateSqlStrategy::PredicateDrivenIn && !regexp;
    let mut parts = Vec::new();
    let mut params = Vec::new();
    if match_all {
        for tag in tags {
            let match_sql = if regexp {
                validate_regexp_pattern(target, "tags", &tag)?;
                "orgfdb_regexp(?, tags.tag) = 1".to_string()
            } else {
                "tags.tag = ?".to_string()
            };
            if predicate_driven {
                parts.push(format!(
                    "{heading_id_sql} IN (SELECT tags.heading_id FROM tags WHERE {match_sql})"
                ));
            } else {
                parts.push(format!(
                    "EXISTS (SELECT 1 FROM tags WHERE tags.heading_id = {heading_id_sql} AND {match_sql})"
                ));
            }
            params.push(QueryParam::Text(tag));
        }
        return Ok(SqlFragment {
            sql: format!("({})", parts.join(" AND ")),
            params,
        });
    }

    if regexp {
        let mut predicate_parts = Vec::with_capacity(tags.len());
        for tag in tags {
            validate_regexp_pattern(target, "tags", &tag)?;
            predicate_parts.push("orgfdb_regexp(?, tags.tag) = 1".to_string());
            params.push(QueryParam::Text(tag));
        }
        Ok(SqlFragment {
            sql: format!(
                "(EXISTS (SELECT 1 FROM tags WHERE tags.heading_id = {heading_id_sql} AND ({})))",
                predicate_parts.join(" OR ")
            ),
            params,
        })
    } else {
        let placeholders = vec!["?"; tags.len()].join(", ");
        params.extend(tags.into_iter().map(QueryParam::Text));
        let sql = if predicate_driven {
            format!(
                "({heading_id_sql} IN (SELECT tags.heading_id FROM tags WHERE tags.tag IN ({placeholders})))"
            )
        } else {
            format!(
                "(EXISTS (SELECT 1 FROM tags WHERE tags.heading_id = {heading_id_sql} AND tags.tag IN ({placeholders})))"
            )
        };
        Ok(SqlFragment { sql, params })
    }
}

fn compile_heading_property_predicate(
    scope: &QueryScope,
    predicate: &ValidatedPredicate,
    metadata_predicate_strategy: MetadataPredicateSqlStrategy,
) -> Result<SqlFragment, QueryExecutionError> {
    compile_resolved_property_predicate(
        QueryTarget::Headings,
        predicate,
        &scope.heading_col("id"),
        option_bool_with_default(&predicate.options, "inherit", true)?,
        metadata_predicate_strategy,
    )
}

fn compile_file_property_predicate(
    scope: &QueryScope,
    predicate: &ValidatedPredicate,
) -> Result<SqlFragment, QueryExecutionError> {
    compile_resolved_property_predicate(
        QueryTarget::Files,
        predicate,
        &scope.root_col("id"),
        false,
        MetadataPredicateSqlStrategy::HeadingDrivenExists,
    )
}

fn compile_resolved_property_predicate(
    target: QueryTarget,
    predicate: &ValidatedPredicate,
    heading_id_sql: &str,
    inherit: bool,
    metadata_predicate_strategy: MetadataPredicateSqlStrategy,
) -> Result<SqlFragment, QueryExecutionError> {
    let regexp = option_bool(&predicate.options, "regexp")?;
    let key = arg_as_string(&predicate.args[0]).map_err(|message| {
        QueryExecutionError::unsupported_backend_feature(target, "property", message)
    })?;
    let key = normalize_property_key(&key).0;
    let value_column = if inherit {
        "effective_value"
    } else {
        "local_value"
    };
    let predicate_driven =
        metadata_predicate_strategy == MetadataPredicateSqlStrategy::PredicateDrivenIn && !regexp;
    let mut sql = if predicate_driven {
        format!("({heading_id_sql} IN (SELECT heading_id FROM effective_properties WHERE key = ?")
    } else {
        format!(
            "(EXISTS (SELECT 1 FROM effective_properties WHERE heading_id = {heading_id_sql} AND key = ?"
        )
    };
    let mut params = vec![QueryParam::Text(key)];
    if !inherit {
        sql.push_str(" AND local_value IS NOT NULL");
    }
    if let Some(value) = predicate.args.get(1) {
        let value = arg_as_string(value).map_err(|message| {
            QueryExecutionError::unsupported_backend_feature(target, "property", message)
        })?;
        if regexp {
            validate_regexp_pattern(target, "property", &value)?;
            sql.push_str(&format!(" AND orgfdb_regexp(?, {value_column}) = 1"));
        } else {
            sql.push_str(&format!(" AND {value_column} = ?"));
        }
        params.push(QueryParam::Text(value));
    }
    sql.push_str("))");
    Ok(SqlFragment { sql, params })
}

fn compile_keyword_predicate(
    target: QueryTarget,
    predicate: &ValidatedPredicate,
    heading_id_sql: &str,
    metadata_predicate_strategy: MetadataPredicateSqlStrategy,
) -> Result<SqlFragment, QueryExecutionError> {
    let regexp = option_bool(&predicate.options, "regexp")?;
    let key = arg_as_string(&predicate.args[0]).map_err(|message| {
        QueryExecutionError::unsupported_backend_feature(target, "keyword", message)
    })?;
    let predicate_driven =
        metadata_predicate_strategy == MetadataPredicateSqlStrategy::PredicateDrivenIn && !regexp;
    let mut sql = if predicate_driven {
        format!(
            "({heading_id_sql} IN (SELECT keywords.heading_id FROM keywords WHERE keywords.keyword = ? COLLATE NOCASE"
        )
    } else {
        format!(
            "(EXISTS (SELECT 1 FROM keywords WHERE keywords.heading_id = {heading_id_sql} AND keywords.keyword = ? COLLATE NOCASE"
        )
    };
    let mut params = vec![QueryParam::Text(key)];
    if let Some(value) = predicate.args.get(1) {
        let value = arg_as_string(value).map_err(|message| {
            QueryExecutionError::unsupported_backend_feature(target, "keyword", message)
        })?;
        if regexp {
            validate_regexp_pattern(target, "keyword", &value)?;
            sql.push_str(" AND orgfdb_regexp(?, COALESCE(keywords.value, '')) = 1");
        } else {
            sql.push_str(" AND keywords.value = ?");
        }
        params.push(QueryParam::Text(value));
    }
    sql.push_str("))");
    Ok(SqlFragment { sql, params })
}

fn compile_heading_keyword_predicate(
    scope: &QueryScope,
    predicate: &ValidatedPredicate,
    metadata_predicate_strategy: MetadataPredicateSqlStrategy,
) -> Result<SqlFragment, QueryExecutionError> {
    let heading_id = if option_bool_with_default(&predicate.options, "inherit", true)? {
        scope.root_col("id")
    } else {
        scope.heading_col("id")
    };
    compile_keyword_predicate(
        QueryTarget::Headings,
        predicate,
        &heading_id,
        metadata_predicate_strategy,
    )
}

fn validate_regexp_pattern(
    target: QueryTarget,
    predicate: &str,
    pattern: &str,
) -> Result<(), QueryExecutionError> {
    Regex::new(pattern).map(|_| ()).map_err(|error| {
        QueryExecutionError::unsupported_backend_feature(
            target,
            predicate,
            format!(
                "invalid regular expression for {} ({:?}): {}",
                predicate, pattern, error
            ),
        )
    })
}

fn compile_date_predicate(
    target: QueryTarget,
    predicate_name: &str,
    column: &str,
    options: &[ValidatedOption],
    require_not_null_when_unbounded: bool,
) -> Result<SqlFragment, QueryExecutionError> {
    let on = option_value(options, "on");
    let from = option_value(options, "from");
    let to = option_value(options, "to");

    let mut parts = Vec::new();
    let mut params = Vec::new();

    if let Some(value) = on {
        let start = start_bound(target, predicate_name, value)?;
        let end = exclusive_end_bound(target, predicate_name, value)?;
        parts.push(format!("{column} IS NOT NULL"));
        parts.push(format!("{column} >= {}", start.sql));
        parts.push(format!("{column} < {}", end.sql));
        params.extend(start.params);
        params.extend(end.params);
    } else {
        if let Some(value) = from {
            let bound = start_bound(target, predicate_name, value)?;
            parts.push(format!("{column} IS NOT NULL"));
            parts.push(format!("{column} >= {}", bound.sql));
            params.extend(bound.params);
        }
        if let Some(value) = to {
            let bound = exclusive_end_bound(target, predicate_name, value)?;
            if from.is_none() {
                parts.push(format!("{column} IS NOT NULL"));
            }
            parts.push(format!("{column} < {}", bound.sql));
            params.extend(bound.params);
        }
    }

    if parts.is_empty() && require_not_null_when_unbounded {
        parts.push(format!("{column} IS NOT NULL"));
    }

    Ok(SqlFragment {
        sql: format!("({})", parts.join(" AND ")),
        params,
    })
}

fn compile_planning_predicate(
    scope: &QueryScope,
    predicate: &ValidatedPredicate,
) -> Result<SqlFragment, QueryExecutionError> {
    let scheduled = compile_date_predicate(
        QueryTarget::Headings,
        "scheduled",
        &scope.heading_col("scheduled_ts"),
        &predicate.options,
        true,
    )?;
    let deadline = compile_date_predicate(
        QueryTarget::Headings,
        "deadline",
        &scope.heading_col("deadline_ts"),
        &predicate.options,
        true,
    )?;
    let closed = compile_date_predicate(
        QueryTarget::Headings,
        "closed",
        &scope.heading_col("closed_ts"),
        &predicate.options,
        true,
    )?;

    let mut params = scheduled.params;
    params.extend(deadline.params);
    params.extend(closed.params);
    Ok(SqlFragment {
        sql: format!("({} OR {} OR {})", scheduled.sql, deadline.sql, closed.sql),
        params,
    })
}

fn compile_timestamp_exists_predicate(
    scope: &QueryScope,
    predicate: &ValidatedPredicate,
    timestamp_type: Option<&str>,
) -> Result<SqlFragment, QueryExecutionError> {
    let date_fragment = compile_date_predicate(
        QueryTarget::Headings,
        predicate.name.as_str(),
        "timestamps.start_ts",
        &predicate.options,
        true,
    )?;

    let mut sql = String::from(&format!(
        "(EXISTS (SELECT 1 FROM timestamps WHERE timestamps.heading_id = {} AND ",
        scope.heading_col("id")
    ));
    sql.push_str(&date_fragment.sql);
    if timestamp_type.is_some() {
        sql.push_str(" AND timestamps.type = ?");
    }
    sql.push_str("))");
    let mut params = date_fragment.params;
    if let Some(timestamp_type) = timestamp_type {
        params.push(QueryParam::Text(timestamp_type.to_string()));
    }
    Ok(SqlFragment { sql, params })
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum LinkEndpoint {
    Source,
    Target,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum HeadingHierarchyRelation {
    Parent,
    Ancestor,
    Child,
    Descendant,
}

fn compile_link_endpoint_predicate(
    scope: &QueryScope,
    aliases: &mut AliasAllocator,
    predicate: &ValidatedPredicate,
    endpoint: LinkEndpoint,
) -> Result<SqlFragment, QueryExecutionError> {
    match &predicate.args[0] {
        ValidatedArg::Scalar(QueryValue::Keyword(value)) if value == "any" => match endpoint {
            LinkEndpoint::Source => Ok(sql_literal("(1 = 1)")),
            LinkEndpoint::Target => Ok(combine_fragments_with(
                "AND",
                vec![
                    resolved_link_target_fragment(scope),
                    sql_literal(&format!(
                        "({} IS NOT NULL OR {} IS NOT NULL)",
                        scope.link_col("target_file_id"),
                        scope.link_col("target_heading_id")
                    )),
                ],
            )
            .expect("target :any should compile")),
        },
        ValidatedArg::NestedQuery(query) => {
            let target_column = match (endpoint, query.target) {
                (LinkEndpoint::Source, QueryTarget::Headings) => scope.link_col("heading_id"),
                (LinkEndpoint::Source, QueryTarget::Files) => scope.link_col("file_id"),
                (LinkEndpoint::Target, QueryTarget::Headings) => {
                    scope.link_col("target_heading_id")
                }
                (LinkEndpoint::Target, QueryTarget::Files) => scope.link_col("target_file_id"),
                _ => unreachable!("validator should constrain source/target nested queries"),
            };
            let target_fragment = compile_nested_target_exists(query, aliases, &target_column)?;
            match endpoint {
                LinkEndpoint::Source => Ok(target_fragment),
                LinkEndpoint::Target => Ok(combine_fragments_with(
                    "AND",
                    vec![resolved_link_target_fragment(scope), target_fragment],
                )
                .expect("structured target query should compile")),
            }
        }
        _ => unreachable!("validator should constrain source/target args"),
    }
}

fn resolved_link_target_fragment(scope: &QueryScope) -> SqlFragment {
    sql_literal(&format!(
        "({} = 'resolved')",
        scope.link_col("resolution_status")
    ))
}

fn compile_nested_target_exists(
    query: &ValidatedQuery,
    aliases: &mut AliasAllocator,
    outer_id_sql: &str,
) -> Result<SqlFragment, QueryExecutionError> {
    let nested_scope = match query.target {
        QueryTarget::Headings => aliases.next_all_headings_scope(),
        QueryTarget::Files => aliases.next_scope(QueryTarget::Files),
        QueryTarget::Links => unreachable!("validator should reject nested links here"),
    };
    let filter = compile_query_match_filter(query, &nested_scope, aliases)?;

    let (id_column, from_clause) = match query.target {
        QueryTarget::Headings => (
            nested_scope.heading_col("id"),
            heading_from_clause(
                &nested_scope,
                fragment_references_file(&nested_scope, filter.as_ref()),
                fragment_references_root(&nested_scope, filter.as_ref()),
            ),
        ),
        QueryTarget::Files => (
            nested_scope.file_col("id"),
            file_from_clause(
                &nested_scope,
                fragment_references_root(&nested_scope, filter.as_ref()),
            ),
        ),
        QueryTarget::Links => unreachable!("validator should reject nested links here"),
    };

    let mut fragments = vec![sql_literal(&format!("({id_column} = {outer_id_sql})"))];
    if let Some(filter) = filter {
        fragments.push(filter);
    }
    let combined = combine_fragments_with("AND", fragments).expect("relation filter should exist");
    Ok(SqlFragment {
        sql: format!("(EXISTS (SELECT 1 {from_clause} WHERE {}))", combined.sql),
        params: combined.params,
    })
}

fn compile_heading_hierarchy_predicate(
    scope: &QueryScope,
    aliases: &mut AliasAllocator,
    predicate: &ValidatedPredicate,
    relation: HeadingHierarchyRelation,
) -> Result<SqlFragment, QueryExecutionError> {
    let nested_query = predicate.args.first().map(|arg| match arg {
        ValidatedArg::NestedQuery(query) => query.as_ref(),
        _ => unreachable!("validator should constrain hierarchy args"),
    });
    let nested_scope = aliases.next_all_headings_scope();
    let nested_filter = compile_scope_match_filter(
        &nested_scope,
        aliases,
        nested_query.and_then(|query| query.predicate.as_ref()),
    )?;

    let (extra_joins, relation_fragment) = match relation {
        HeadingHierarchyRelation::Parent => (
            String::new(),
            sql_literal(&format!(
                "({} = {})",
                nested_scope.heading_col("id"),
                scope.heading_col("parent_id")
            )),
        ),
        HeadingHierarchyRelation::Child => (
            String::new(),
            sql_literal(&format!(
                "({} = {})",
                nested_scope.heading_col("parent_id"),
                scope.heading_col("id")
            )),
        ),
        HeadingHierarchyRelation::Ancestor => {
            let current_outline_alias = format!("current_{}", nested_scope.outline_alias);
            (
                format!(
                    " INNER JOIN outline_path AS {} ON {}.heading_id = {}
                      INNER JOIN outline_path AS {} ON {}.heading_id = {}",
                    nested_scope.outline_alias,
                    nested_scope.outline_alias,
                    nested_scope.heading_col("id"),
                    current_outline_alias,
                    current_outline_alias,
                    scope.heading_col("id"),
                ),
                sql_literal(&format!(
                    "({}.file_id = {}.file_id AND {}.depth < {}.depth AND {}.materialized_path LIKE {}.materialized_path || '.%')",
                    nested_scope.outline_alias,
                    current_outline_alias,
                    nested_scope.outline_alias,
                    current_outline_alias,
                    current_outline_alias,
                    nested_scope.outline_alias
                )),
            )
        }
        HeadingHierarchyRelation::Descendant => {
            let current_outline_alias = format!("current_{}", nested_scope.outline_alias);
            (
                format!(
                    " INNER JOIN outline_path AS {} ON {}.heading_id = {}
                      INNER JOIN outline_path AS {} ON {}.heading_id = {}",
                    nested_scope.outline_alias,
                    nested_scope.outline_alias,
                    nested_scope.heading_col("id"),
                    current_outline_alias,
                    current_outline_alias,
                    scope.heading_col("id"),
                ),
                sql_literal(&format!(
                    "({}.file_id = {}.file_id AND {}.depth > {}.depth AND {}.materialized_path LIKE {}.materialized_path || '.%')",
                    nested_scope.outline_alias,
                    current_outline_alias,
                    nested_scope.outline_alias,
                    current_outline_alias,
                    nested_scope.outline_alias,
                    current_outline_alias
                )),
            )
        }
    };

    let mut fragments = vec![relation_fragment];
    if let Some(nested_filter) = nested_filter {
        fragments.push(nested_filter);
    }
    let combined = combine_fragments_with("AND", fragments).expect("hierarchy filter should exist");
    Ok(SqlFragment {
        sql: format!(
            "(EXISTS (SELECT 1 {}{} WHERE {}))",
            heading_from_clause(
                &nested_scope,
                fragment_references_file(&nested_scope, Some(&combined)),
                fragment_references_root(&nested_scope, Some(&combined)),
            ),
            extra_joins,
            combined.sql
        ),
        params: combined.params,
    })
}

fn compile_has_link_predicate(
    scope: &QueryScope,
    aliases: &mut AliasAllocator,
    predicate: &ValidatedPredicate,
) -> Result<SqlFragment, QueryExecutionError> {
    let nested_query = predicate.args.first().map(|arg| match arg {
        ValidatedArg::NestedQuery(query) => query.as_ref(),
        _ => unreachable!("validator should constrain has-link args"),
    });
    let link_scope = aliases.next_scope(QueryTarget::Links);
    let link_filter = compile_scope_match_filter(
        &link_scope,
        aliases,
        nested_query.and_then(|query| query.predicate.as_ref()),
    )?;
    let source_fragment = match scope.target {
        QueryTarget::Headings => sql_literal(&format!(
            "({} = {})",
            link_scope.link_col("heading_id"),
            scope.heading_col("id")
        )),
        QueryTarget::Files => sql_literal(&format!(
            "({} = {})",
            link_scope.link_col("file_id"),
            scope.file_col("id")
        )),
        QueryTarget::Links => unreachable!("validator should constrain has-link target"),
    };

    let mut fragments = vec![source_fragment];
    if let Some(link_filter) = link_filter {
        fragments.push(link_filter);
    }
    let combined = combine_fragments_with("AND", fragments).expect("has-link filter should exist");
    Ok(SqlFragment {
        sql: format!(
            "(EXISTS (SELECT 1 {} WHERE {}))",
            link_from_clause(&link_scope, false),
            combined.sql
        ),
        params: combined.params,
    })
}

fn compile_links_to_predicate(
    scope: &QueryScope,
    aliases: &mut AliasAllocator,
    predicate: &ValidatedPredicate,
) -> Result<SqlFragment, QueryExecutionError> {
    let ValidatedArg::NestedQuery(target_query) = &predicate.args[0] else {
        unreachable!("validator should constrain links-to args");
    };
    let link_scope = aliases.next_scope(QueryTarget::Links);
    let target_column = match target_query.target {
        QueryTarget::Headings => link_scope.link_col("target_heading_id"),
        QueryTarget::Files => link_scope.link_col("target_file_id"),
        QueryTarget::Links => unreachable!("validator should constrain links-to targets"),
    };
    let source_fragment = match scope.target {
        QueryTarget::Headings => sql_literal(&format!(
            "({} = {})",
            link_scope.link_col("heading_id"),
            scope.heading_col("id")
        )),
        QueryTarget::Files => sql_literal(&format!(
            "({} = {})",
            link_scope.link_col("file_id"),
            scope.file_col("id")
        )),
        QueryTarget::Links => unreachable!("validator should constrain links-to target"),
    };
    let target_fragment = compile_nested_target_exists(target_query, aliases, &target_column)?;
    let combined = combine_fragments_with(
        "AND",
        vec![
            source_fragment,
            resolved_link_target_fragment(&link_scope),
            target_fragment,
        ],
    )
    .expect("links-to");
    Ok(SqlFragment {
        sql: format!(
            "(EXISTS (SELECT 1 {} WHERE {}))",
            link_from_clause(&link_scope, false),
            combined.sql
        ),
        params: combined.params,
    })
}

fn compile_linked_from_predicate(
    scope: &QueryScope,
    aliases: &mut AliasAllocator,
    predicate: &ValidatedPredicate,
) -> Result<SqlFragment, QueryExecutionError> {
    let link_scope = aliases.next_scope(QueryTarget::Links);
    let target_fragment = match scope.target {
        QueryTarget::Headings => sql_literal(&format!(
            "({} = {})",
            link_scope.link_col("target_heading_id"),
            scope.heading_col("id")
        )),
        QueryTarget::Files => sql_literal(&format!(
            "({} = {})",
            link_scope.link_col("target_file_id"),
            scope.file_col("id")
        )),
        QueryTarget::Links => unreachable!("validator should constrain linked-from target"),
    };
    let source_fragment = match &predicate.args[0] {
        ValidatedArg::Scalar(QueryValue::Keyword(value)) if value == "any" => None,
        ValidatedArg::NestedQuery(query) => {
            let outer_id = match query.target {
                QueryTarget::Headings => link_scope.link_col("heading_id"),
                QueryTarget::Files => link_scope.link_col("file_id"),
                QueryTarget::Links => unreachable!("validator should constrain linked-from source"),
            };
            Some(compile_nested_target_exists(query, aliases, &outer_id)?)
        }
        _ => unreachable!("validator should constrain linked-from args"),
    };

    let mut fragments = vec![target_fragment];
    fragments.push(resolved_link_target_fragment(&link_scope));
    if let Some(source_fragment) = source_fragment {
        fragments.push(source_fragment);
    }
    let combined =
        combine_fragments_with("AND", fragments).expect("linked-from filter should exist");
    Ok(SqlFragment {
        sql: format!(
            "(EXISTS (SELECT 1 {} WHERE {}))",
            link_from_clause(&link_scope, false),
            combined.sql
        ),
        params: combined.params,
    })
}

fn compile_in_list(
    target: QueryTarget,
    column: &str,
    args: &[ValidatedArg],
) -> Result<SqlFragment, QueryExecutionError> {
    let values = args
        .iter()
        .map(arg_as_string)
        .collect::<Result<Vec<_>, _>>()
        .map_err(|message| {
            QueryExecutionError::unsupported_backend_feature(target, column, message)
        })?;
    let placeholders = vec!["?"; values.len()].join(", ");
    Ok(SqlFragment {
        sql: format!("({column} IN ({placeholders}))"),
        params: values.into_iter().map(QueryParam::Text).collect(),
    })
}

fn start_bound(
    target: QueryTarget,
    predicate_name: &str,
    value: &QueryValue,
) -> Result<SqlFragment, QueryExecutionError> {
    let QueryValue::TemporalBounds(bounds) = value else {
        return Err(QueryExecutionError::date_resolution(
            target,
            predicate_name,
            format!(
                "cannot compile unresolved temporal bound for {predicate_name}; resolve temporal bounds before SQL compilation"
            ),
        ));
    };
    Ok(SqlFragment {
        sql: "?".to_string(),
        params: vec![QueryParam::Integer(bounds.start)],
    })
}

fn exclusive_end_bound(
    target: QueryTarget,
    predicate_name: &str,
    value: &QueryValue,
) -> Result<SqlFragment, QueryExecutionError> {
    let QueryValue::TemporalBounds(bounds) = value else {
        return Err(QueryExecutionError::date_resolution(
            target,
            predicate_name,
            format!(
                "cannot compile unresolved temporal bound for {predicate_name}; resolve temporal bounds before SQL compilation"
            ),
        ));
    };
    Ok(SqlFragment {
        sql: "?".to_string(),
        params: vec![QueryParam::Integer(bounds.exclusive_end)],
    })
}

fn option_value<'a>(options: &'a [ValidatedOption], name: &str) -> Option<&'a QueryValue> {
    options
        .iter()
        .find(|option| option.name == name)
        .map(|option| &option.value)
}

fn option_bool(options: &[ValidatedOption], name: &str) -> Result<bool, QueryExecutionError> {
    Ok(match option_value(options, name) {
        Some(QueryValue::Bool(value)) => *value,
        Some(_) => unreachable!("validator should constrain boolean options"),
        None => false,
    })
}

fn option_bool_with_default(
    options: &[ValidatedOption],
    name: &str,
    default: bool,
) -> Result<bool, QueryExecutionError> {
    Ok(match option_value(options, name) {
        Some(QueryValue::Bool(value)) => *value,
        Some(_) => unreachable!("validator should constrain boolean options"),
        None => default,
    })
}

fn keyword_option(
    options: &[ValidatedOption],
    name: &str,
) -> Result<Option<String>, QueryExecutionError> {
    match option_value(options, name) {
        Some(QueryValue::Keyword(value)) => Ok(Some(value.clone())),
        Some(_) => unreachable!("validator should constrain keyword options"),
        None => Ok(None),
    }
}

fn arg_as_string(arg: &ValidatedArg) -> Result<String, &'static str> {
    match arg {
        ValidatedArg::Scalar(QueryValue::String(value)) => Ok(value.clone()),
        _ => Err("expected string scalar argument"),
    }
}

fn sql_literal(sql: &str) -> SqlFragment {
    SqlFragment {
        sql: sql.to_string(),
        params: Vec::new(),
    }
}

fn add_file_restriction(
    fragment: Option<SqlFragment>,
    scope: &QueryScope,
    restrict_files: bool,
) -> Option<SqlFragment> {
    if !restrict_files {
        return fragment;
    }
    let restriction = format!(
        "EXISTS (SELECT 1 FROM temp.{QUERY_FILE_RESTRICTION_TABLE} AS restricted_file WHERE restricted_file.path = {})",
        scope.file_col("path")
    );
    Some(match fragment {
        Some(fragment) => SqlFragment {
            sql: format!("({}) AND ({restriction})", fragment.sql),
            params: fragment.params,
        },
        None => SqlFragment {
            sql: restriction,
            params: Vec::new(),
        },
    })
}

fn prepare_file_restriction(
    connection: &Connection,
    target: QueryTarget,
    paths: &[String],
) -> Result<(), QueryExecutionError> {
    connection
        .execute_batch(&format!(
            "/* orgfdb:restrict-files-reset params=0 */
             CREATE TEMP TABLE IF NOT EXISTS {QUERY_FILE_RESTRICTION_TABLE} (
                 path TEXT PRIMARY KEY
             );
             /* orgfdb:restrict-files-reset params=0 */
             DELETE FROM {QUERY_FILE_RESTRICTION_TABLE};"
        ))
        .map_err(|source| {
            QueryExecutionError::database(target, "prepare_file_restriction.reset", source)
        })?;
    let chunk_size = id_chunk_capacity(connection, 0);
    for chunk in paths.chunks(chunk_size) {
        let values = vec!["(?)"; chunk.len()].join(", ");
        let sql = format!(
            "/* orgfdb:restrict-files-insert params={} */
             INSERT OR IGNORE INTO temp.{QUERY_FILE_RESTRICTION_TABLE} (path) VALUES {values}",
            chunk.len()
        );
        connection
            .execute(&sql, params_from_iter(chunk.iter()))
            .map_err(|source| {
                QueryExecutionError::database(target, "prepare_file_restriction.insert", source)
            })?;
    }
    Ok(())
}

fn render_where_clause(fragment: Option<&SqlFragment>) -> String {
    match fragment {
        Some(fragment) => format!("WHERE {}", fragment.sql),
        None => String::new(),
    }
}

fn heading_from_clause(scope: &QueryScope, include_file: bool, include_root: bool) -> String {
    let mut sql = format!("FROM headings AS {}", scope.heading_alias);
    if include_file {
        sql.push_str(&format!(
            "\n         INNER JOIN files AS {} ON {}.id = {}.file_id",
            scope.file_alias, scope.file_alias, scope.heading_alias
        ));
    }
    if include_root {
        sql.push_str(&format!(
            "\n         INNER JOIN headings AS {} ON {}.file_id = {}.file_id AND {}.level = 0",
            scope.root_alias, scope.root_alias, scope.heading_alias, scope.root_alias
        ));
    }
    sql
}

fn fragment_references_file(scope: &QueryScope, fragment: Option<&SqlFragment>) -> bool {
    fragment.is_some_and(|fragment| fragment.sql.contains(&format!("{}.", scope.file_alias)))
}

fn fragment_references_root(scope: &QueryScope, fragment: Option<&SqlFragment>) -> bool {
    if scope.target == QueryTarget::Headings
        && scope.heading_match_kind == HeadingMatchKind::RootFile
    {
        return false;
    }
    fragment.is_some_and(|fragment| fragment.sql.contains(&format!("{}.", scope.root_alias)))
}

fn link_from_clause(scope: &QueryScope, include_output_context: bool) -> String {
    if !include_output_context {
        return format!("FROM links AS {}", scope.link_alias);
    }

    format!(
        "FROM links AS {}
         INNER JOIN files AS {} ON {}.id = {}.file_id
         INNER JOIN headings AS {} ON {}.id = {}.heading_id",
        scope.link_alias,
        scope.file_alias,
        scope.file_alias,
        scope.link_alias,
        scope.link_heading_alias,
        scope.link_heading_alias,
        scope.link_alias
    )
}

fn file_from_clause(scope: &QueryScope, include_root: bool) -> String {
    let mut sql = format!("FROM files AS {}", scope.file_alias);
    if include_root {
        sql.push_str(&format!(
            "\n         INNER JOIN headings AS {} ON {}.file_id = {}.id AND {}.level = 0",
            scope.root_alias, scope.root_alias, scope.file_alias, scope.root_alias
        ));
    }
    sql
}

fn compile_heading_root_file_query(
    query: &ValidatedQuery,
    restrict_files: bool,
    metadata_predicate_strategy: MetadataPredicateSqlStrategy,
) -> Result<CompiledSqlQuery, QueryExecutionError> {
    let mut aliases = AliasAllocator::with_metadata_predicate_strategy(metadata_predicate_strategy);
    let scope = aliases.next_heading_root_scope();
    let where_clause = add_file_restriction(
        compile_query_match_filter(query, &scope, &mut aliases)?,
        &scope,
        restrict_files,
    );
    let bound_parameter_count = where_clause
        .as_ref()
        .map_or(0, |fragment| fragment.params.len());

    Ok(CompiledSqlQuery {
        target: QueryTarget::Files,
        sql: format!(
            "/* orgfdb:match-heading-roots params={bound_parameter_count} */
             SELECT
                {file_id},
                {file_path},
                {file_mtime_ns},
                {file_size},
                {file_content_hash},
                {file_indexed_at},
                {root_id},
                {root_title},
                {root_title_raw},
                {root_line_number}
             {}
             {}",
            heading_from_clause(
                &scope,
                true,
                fragment_references_root(&scope, where_clause.as_ref()),
            ),
            render_where_clause(where_clause.as_ref()),
            file_id = scope.file_col("id"),
            file_path = scope.file_col("path"),
            file_mtime_ns = scope.file_col("mtime_ns"),
            file_size = scope.file_col("size"),
            file_content_hash = scope.file_col("content_hash"),
            file_indexed_at = scope.file_col("indexed_at"),
            root_id = scope.root_col("id"),
            root_title = scope.root_col("title"),
            root_title_raw = scope.root_col("title_raw"),
            root_line_number = scope.root_col("line_number"),
        ),
        params: where_clause.map_or_else(Vec::new, |fragment| fragment.params),
    })
}

fn compile_heading_root_false_predicate(
    scope: &QueryScope,
    aliases: &mut AliasAllocator,
    predicate: &ValidatedPredicate,
    relation: HeadingHierarchyRelation,
) -> Result<SqlFragment, QueryExecutionError> {
    if scope.heading_match_kind == HeadingMatchKind::RootFile {
        return Ok(sql_literal("(0 = 1)"));
    }

    compile_heading_hierarchy_predicate(scope, aliases, predicate, relation)
}

fn compare_heading_query_matches(
    left: &HeadingQueryMatch,
    right: &HeadingQueryMatch,
) -> std::cmp::Ordering {
    use std::cmp::Ordering;

    let left_key = match left {
        HeadingQueryMatch::File(row) => (&row.path, -1_i64, row.id),
        HeadingQueryMatch::Heading(row) => (&row.file_path, row.byte_start, row.id),
    };
    let right_key = match right {
        HeadingQueryMatch::File(row) => (&row.path, -1_i64, row.id),
        HeadingQueryMatch::Heading(row) => (&row.file_path, row.byte_start, row.id),
    };

    match left_key.0.cmp(right_key.0) {
        Ordering::Equal => match left_key.1.cmp(&right_key.1) {
            Ordering::Equal => left_key.2.cmp(&right_key.2),
            other => other,
        },
        other => other,
    }
}

fn target_name(target: QueryTarget) -> &'static str {
    match target {
        QueryTarget::Headings => "headings",
        QueryTarget::Links => "links",
        QueryTarget::Files => "files",
    }
}

#[cfg(test)]
mod tests {
    use super::{
        compile_sqlite_query, compile_sqlite_query_with_metadata_strategy, execute_sqlite_query,
        execute_sqlite_query_with_options,
        execute_sqlite_query_with_relation_and_metadata_strategy,
        expand_leading_home_path_with_home, params_from_iter, sqlite_query_validation_options,
        FileQueryRow, HeadingQueryMatch, HeadingQueryRow, LinkQueryRow,
        MetadataPredicateSqlStrategy, QueryExecutionErrorKind, QueryParam, QueryRows,
    };
    use crate::db::{
        open_database, open_in_memory_database_with_schema, DbWriter, EffectivePropertyRecord,
        EffectiveTagRecord, FileRecordInput, HeadingBodyRecord, HeadingRecord, KeywordRecord,
        LinkRecord, OutlinePathRecord, PropertyRecord, SchemaDefinition, TagRecord,
        TimestampRecord,
    };
    use crate::property::{derive_effective_properties, PropertyRow};
    use crate::query::{
        parse_query, resolve_relative_dates, resolve_temporal_bounds, validate_query,
        QueryDateResolutionOptions, QueryExecutionOptions, QueryTarget, QueryValidationOptions,
    };
    use crate::tag::derive_effective_tags;
    use chrono::NaiveDate;
    use rusqlite::{limits::Limit, Connection};
    use std::{
        fs,
        path::{Path, PathBuf},
        time::{SystemTime, UNIX_EPOCH},
    };

    struct PlanningFixture<'a> {
        kind: &'a str,
        timestamp: Option<i64>,
        has_time: Option<bool>,
        raw_value: &'a str,
    }

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
                "org-files-db-query-sqlite-{}-{}-{}",
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

    fn validation_options() -> QueryValidationOptions {
        QueryValidationOptions {
            body_text_available: true,
            regexp_matching_supported: true,
        }
    }

    fn validated(query: &str) -> crate::query::ValidatedQuery {
        let parsed = parse_query(query).expect("query should parse");
        validate_query(parsed, &validation_options()).expect("query should validate")
    }

    fn temporal_resolved(query: &str) -> crate::query::ValidatedQuery {
        resolve_temporal_bounds(
            &validated(query),
            &QueryDateResolutionOptions {
                timezone: Some("UTC".to_string()),
                now_utc: None,
            },
        )
        .expect("temporal bounds should resolve")
    }

    #[test]
    fn validation_options_read_persisted_body_text_capability() {
        let connection = open_in_memory_database_with_schema(&SchemaDefinition::new(
            crate::db::CURRENT_SCHEMA_VERSION,
            false,
        ))
        .expect("database should open");

        let unavailable =
            sqlite_query_validation_options(&connection).expect("validation options should load");
        assert!(!unavailable.body_text_available);
        assert!(unavailable.regexp_matching_supported);

        DbWriter::set_metadata_flag(
            &connection,
            crate::db::DB_METADATA_BODY_TEXT_AVAILABLE_KEY,
            true,
        )
        .expect("metadata should persist");

        let available =
            sqlite_query_validation_options(&connection).expect("validation options should reload");
        assert!(available.body_text_available);
        assert!(available.regexp_matching_supported);
    }

    #[test]
    fn validation_options_treat_missing_metadata_table_as_body_text_unavailable() {
        let connection = Connection::open_in_memory().expect("database should open");

        let options =
            sqlite_query_validation_options(&connection).expect("validation options should load");
        assert!(!options.body_text_available);
        assert!(options.regexp_matching_supported);
    }

    #[test]
    fn validation_options_treat_missing_heading_bodies_table_as_body_text_unavailable() {
        let connection = reduced_body_text_capability_connection(true, true);

        let options =
            sqlite_query_validation_options(&connection).expect("validation options should load");
        assert!(!options.body_text_available);
        assert!(options.regexp_matching_supported);
    }

    #[test]
    fn validation_options_treat_missing_body_text_metadata_row_as_unavailable() {
        let connection = reduced_body_text_capability_connection(false, true);

        let options =
            sqlite_query_validation_options(&connection).expect("validation options should load");
        assert!(!options.body_text_available);
        assert!(options.regexp_matching_supported);
    }

    #[test]
    fn validation_options_treat_disabled_body_text_metadata_value_as_unavailable() {
        let connection = reduced_body_text_capability_connection(true, false);

        let options =
            sqlite_query_validation_options(&connection).expect("validation options should load");
        assert!(!options.body_text_available);
        assert!(options.regexp_matching_supported);
    }

    #[test]
    fn execution_rejects_has_text_when_body_text_capability_is_unavailable() {
        let connection = open_in_memory_database_with_schema(&SchemaDefinition::new(
            crate::db::CURRENT_SCHEMA_VERSION,
            false,
        ))
        .expect("database should open");
        let parsed = parse_query(r#"(headings (has-text "sqlite"))"#).expect("query should parse");
        let query = validate_query(
            parsed,
            &QueryValidationOptions {
                body_text_available: true,
                regexp_matching_supported: true,
            },
        )
        .expect("query should validate with permissive options");

        let error = execute_sqlite_query(&connection, &query).expect_err("query should fail");
        assert_eq!(
            error.kind,
            QueryExecutionErrorKind::UnsupportedBackendFeature
        );
        assert_eq!(
            error.message,
            "has-text requires body text to be available in the database"
        );
    }

    #[test]
    fn execution_rejects_has_text_when_metadata_claims_capability_but_heading_bodies_is_missing() {
        let connection = reduced_body_text_capability_connection(true, true);
        let parsed = parse_query(r#"(headings (has-text "sqlite"))"#).expect("query should parse");
        let query = validate_query(
            parsed,
            &QueryValidationOptions {
                body_text_available: true,
                regexp_matching_supported: true,
            },
        )
        .expect("query should validate with permissive options");

        let error = execute_sqlite_query(&connection, &query).expect_err("query should fail");
        assert_eq!(
            error.kind,
            QueryExecutionErrorKind::UnsupportedBackendFeature
        );
        assert_eq!(
            error.message,
            "has-text requires body text to be available in the database"
        );
        assert!(!error.to_string().contains("no such table: heading_bodies"));
    }

    #[test]
    fn compile_uses_placeholders_instead_of_inlining_user_payload() {
        let user_value = "x' OR 1=1 --";
        for query in [
            validated(&format!(r#"(headings (title "{user_value}"))"#)),
            validated(&format!(r#"(headings (has-text "{user_value}"))"#)),
            validated(&format!(r#"(headings (title "{user_value}" :regexp t))"#)),
            validated(&format!(
                r#"(headings (has-text "{user_value}" :regexp t))"#
            )),
            validated(&format!(r#"(headings (outline-contains "{user_value}"))"#)),
            validated(&format!(
                r#"(headings (outline-contains "{user_value}" :regexp t))"#
            )),
            validated(&format!(
                r#"(headings (outline-sequence "{user_value}" "nested"))"#
            )),
            validated(&format!(
                r#"(headings (outline-sequence "{user_value}" "nested" :regexp t))"#
            )),
            validated(&format!(r#"(headings (tags "{user_value}" :inherit nil))"#)),
            validated(&format!(
                r#"(headings (tags "{user_value}" :inherit nil :regexp t))"#
            )),
            validated(&format!(r#"(headings (property "OWNER" "{user_value}"))"#)),
            validated(&format!(
                r#"(headings (property "OWNER" "{user_value}" :regexp t))"#
            )),
            validated(&format!(r#"(files (keyword "AUTHOR" "{user_value}"))"#)),
            validated(&format!(
                r#"(files (keyword "AUTHOR" "{user_value}" :regexp t))"#
            )),
            validated(&format!(r#"(files (file-path "{user_value}" :regexp t))"#)),
            validated(&format!(
                r#"(links (link-target "{user_value}" :regexp t))"#
            )),
            validated(&format!(
                r#"(headings (links-to (headings (title "{user_value}"))))"#
            )),
        ] {
            let compiled = compile_sqlite_query(&query).expect("query should compile");
            assert!(compiled.sql.contains('?'));
            assert!(!compiled.sql.contains(user_value));
            assert!(!compiled.params.is_empty());
            assert!(compiled.params.iter().any(|param| matches!(
                param,
                super::QueryParam::Text(value) if value == user_value
            )));
        }
    }

    #[test]
    fn heading_root_branch_is_skipped_when_predicates_cannot_match_level_zero() {
        let level_one = validated(r#"(headings (level 1))"#);
        assert_eq!(
            super::heading_root_truth(level_one.predicate.as_ref()),
            super::StaticTruth::False
        );

        let negated_level_one = validated(r#"(headings (not (level 1)))"#);
        assert_eq!(
            super::heading_root_truth(negated_level_one.predicate.as_ref()),
            super::StaticTruth::True
        );

        let unknown_title = validated(r#"(headings (title "Project" :exact t))"#);
        assert_eq!(
            super::heading_root_truth(unknown_title.predicate.as_ref()),
            super::StaticTruth::Unknown
        );

        let impossible_and =
            validated(r#"(headings (and (title "Project" :exact t) (level 2 4)))"#);
        assert_eq!(
            super::heading_root_truth(impossible_and.predicate.as_ref()),
            super::StaticTruth::False
        );
    }

    #[test]
    fn compile_omits_unused_heading_root_joins() {
        let simple = compile_sqlite_query(&validated(r#"(headings (level 1))"#))
            .expect("simple heading query should compile");
        assert!(simple.sql.contains("INNER JOIN files AS f0"));
        assert!(!simple.sql.contains("INNER JOIN headings AS r0"));

        let with_file_title =
            compile_sqlite_query(&validated(r#"(headings (file-title "Project" :exact t))"#))
                .expect("file-title query should compile");
        assert!(with_file_title.sql.contains("INNER JOIN headings AS r0"));
    }

    #[test]
    fn compile_top_level_queries_leave_public_order_to_rust() {
        for expression in [
            r#"(headings (level 1))"#,
            r#"(links (link-type "file"))"#,
            "(files)",
        ] {
            let compiled = compile_sqlite_query(&validated(expression))
                .expect("top-level query should compile");
            assert!(!compiled.sql.contains("ORDER BY"), "{}", compiled.sql);
        }
    }

    #[test]
    fn compile_top_level_links_omit_unused_outline_path_join() {
        let compiled = compile_sqlite_query(&validated(r#"(links (link-type "file"))"#))
            .expect("link query should compile");

        assert!(compiled.sql.contains("INNER JOIN files AS f0"));
        assert!(compiled.sql.contains("INNER JOIN headings AS lh0"));
        assert!(!compiled.sql.contains("INNER JOIN outline_path AS op0"));
    }

    #[test]
    fn compile_nested_link_filters_use_only_the_links_table_when_possible() {
        let compiled = compile_sqlite_query(&validated(
            r#"(headings (has-link (links (link-type "file"))))"#,
        ))
        .expect("nested link query should compile");

        assert!(compiled.sql.contains("EXISTS (SELECT 1 FROM links AS l1"));
        assert!(!compiled.sql.contains("INNER JOIN files AS f1"));
        assert!(!compiled.sql.contains("INNER JOIN headings AS lh1"));
        assert!(!compiled.sql.contains("INNER JOIN outline_path AS op1"));
    }

    #[test]
    fn compile_nested_heading_and_file_targets_add_only_required_joins() {
        let heading_title = compile_sqlite_query(&validated(
            r#"(headings (links-to (headings (title "Target" :exact t))))"#,
        ))
        .expect("nested heading title query should compile");
        assert!(heading_title.sql.contains("FROM headings AS h2"));
        assert!(!heading_title.sql.contains("INNER JOIN files AS f2"));
        assert!(!heading_title.sql.contains("INNER JOIN headings AS r2"));

        let heading_file_title = compile_sqlite_query(&validated(
            r#"(headings (links-to (headings (file-title "Project" :exact t))))"#,
        ))
        .expect("nested heading file-title query should compile");
        assert!(heading_file_title.sql.contains("INNER JOIN headings AS r2"));

        let file_path = compile_sqlite_query(&validated(
            r#"(headings (links-to (files (file-path "/tmp/target.org" :exact t))))"#,
        ))
        .expect("nested file path query should compile");
        assert!(file_path.sql.contains("FROM files AS f2"));
        assert!(!file_path.sql.contains("INNER JOIN headings AS r2"));

        let file_title = compile_sqlite_query(&validated(
            r#"(headings (links-to (files (file-title "Project" :exact t))))"#,
        ))
        .expect("nested file title query should compile");
        assert!(file_title.sql.contains("INNER JOIN headings AS r2"));
    }

    #[test]
    fn expands_only_leading_file_path_home_syntax() {
        assert_eq!(
            expand_leading_home_path_with_home("~", Some("/home/tester")),
            Ok("/home/tester".to_string())
        );
        assert_eq!(
            expand_leading_home_path_with_home("~/notes.org", Some("/home/tester")),
            Ok("/home/tester/notes.org".to_string())
        );
        assert_eq!(
            expand_leading_home_path_with_home("projects/~/notes.org", Some("/home/tester")),
            Ok("projects/~/notes.org".to_string())
        );
        assert_eq!(
            expand_leading_home_path_with_home("file~backup.org", Some("/home/tester")),
            Ok("file~backup.org".to_string())
        );
        assert_eq!(
            expand_leading_home_path_with_home("~alice/notes.org", Some("/home/tester")),
            Ok("~alice/notes.org".to_string())
        );
        assert!(expand_leading_home_path_with_home("~/notes.org", None)
            .expect_err("missing HOME should fail")
            .contains("HOME"));
        assert_eq!(
            expand_leading_home_path_with_home("notes.org", None),
            Ok("notes.org".to_string())
        );
    }

    #[test]
    fn compiles_expanded_file_paths_for_all_file_path_contexts() {
        let home = std::env::var("HOME").expect("test environment should provide HOME");
        let expected = format!("{home}/projects/ancestors.org");

        for query in [
            r#"(headings (file-path "~/projects/ancestors.org" :exact t))"#,
            r#"(files (file-path "~/projects/ancestors.org" :exact t))"#,
            r#"(headings (links-to (files (file-path "~/projects/ancestors.org" :exact t))))"#,
            r#"(links (target (files (file-path "~/projects/ancestors.org" :exact t))))"#,
        ] {
            let compiled = compile_sqlite_query(&validated(query)).expect("query should compile");
            assert_eq!(compiled.params, vec![QueryParam::Text(expected.clone())]);
        }
    }

    #[test]
    fn compiles_expanded_file_dirs_for_all_file_dir_contexts() {
        let home = std::env::var("HOME").expect("test environment should provide HOME");
        let expected = format!("{home}/projects");

        for query in [
            r#"(headings (file-dir "~/projects" :exact t))"#,
            r#"(files (file-dir "~/projects" :exact t))"#,
            r#"(headings (links-to (files (file-dir "~/projects" :exact t))))"#,
            r#"(links (target (files (file-dir "~/projects" :exact t))))"#,
        ] {
            let compiled = compile_sqlite_query(&validated(query)).expect("query should compile");
            assert_eq!(compiled.params, vec![QueryParam::Text(expected.clone())]);
        }

        for (query, expected) in [
            (
                r#"(files (file-dir "/var/projects" :exact t))"#,
                "/var/projects",
            ),
            (r#"(files (file-dir "projects" :exact t))"#, "projects"),
        ] {
            let compiled = compile_sqlite_query(&validated(query)).expect("query should compile");
            assert_eq!(
                compiled.params,
                vec![QueryParam::Text(expected.to_string())]
            );
        }
    }

    #[test]
    fn execution_expands_file_path_home_without_changing_returned_paths() {
        let connection = open_in_memory_database_with_schema(&SchemaDefinition::new(
            crate::db::CURRENT_SCHEMA_VERSION,
            false,
        ))
        .expect("database should open");
        let home = std::env::var("HOME").expect("test environment should provide HOME");
        let path = format!("{home}/projects/ancestors.org");
        connection
            .execute(
                "INSERT INTO files (id, path, mtime_ns, size) VALUES (1, ?1, 0, 0)",
                rusqlite::params![path],
            )
            .expect("file should insert");
        connection
            .execute_batch(
                "INSERT INTO headings
                 (id, file_id, parent_id, level, byte_start, byte_end, title)
                 VALUES
                 (1, 1, NULL, 0, -1, 0, 'Index'),
                 (2, 1, 1, 1, 0, 0, 'Child');",
            )
            .expect("headings should insert");

        let file_rows = execute_sqlite_query(
            &connection,
            &validated(r#"(files (file-path "~/projects/ancestors.org" :exact t))"#),
        )
        .expect("exact file query should execute");
        assert_eq!(file_paths(file_rows), vec![path.clone()]);

        let heading_rows = execute_sqlite_query(
            &connection,
            &validated(r#"(headings (file-path "~/projects" "ancestors"))"#),
        )
        .expect("substring heading query should execute");
        assert_eq!(heading_ids(heading_rows), vec![2]);

        let regexp_rows = execute_sqlite_query(
            &connection,
            &validated(r#"(files (file-path "~/projects/.*\\.org" :regexp t))"#),
        )
        .expect("regexp file query should execute");
        assert_eq!(file_paths(regexp_rows), vec![path]);
    }

    #[test]
    fn execution_expands_file_dir_home_without_changing_returned_paths() {
        let connection = open_in_memory_database_with_schema(&SchemaDefinition::new(
            crate::db::CURRENT_SCHEMA_VERSION,
            false,
        ))
        .expect("database should open");
        let home = std::env::var("HOME").expect("test environment should provide HOME");
        let path = format!("{home}/projects/ancestors.org");
        connection
            .execute(
                "INSERT INTO files (id, path, mtime_ns, size) VALUES (1, ?1, 0, 0)",
                rusqlite::params![path],
            )
            .expect("file should insert");
        connection
            .execute_batch(
                "INSERT INTO headings
                 (id, file_id, parent_id, level, byte_start, byte_end, title)
                 VALUES
                 (1, 1, NULL, 0, -1, 0, 'Index'),
                 (2, 1, 1, 1, 0, 0, 'Child');",
            )
            .expect("headings should insert");

        let file_rows = execute_sqlite_query(
            &connection,
            &validated(r#"(files (file-dir "~/projects" :exact t))"#),
        )
        .expect("exact file directory query should execute");
        assert_eq!(file_paths(file_rows), vec![path.clone()]);

        let heading_rows =
            execute_sqlite_query(&connection, &validated(r#"(headings (file-dir "~/proj"))"#))
                .expect("substring heading directory query should execute");
        assert_eq!(heading_ids(heading_rows), vec![2]);

        let regexp_rows = execute_sqlite_query(
            &connection,
            &validated(r#"(files (file-dir "~/proj.*" :regexp t))"#),
        )
        .expect("regexp file directory query should execute");
        assert_eq!(file_paths(regexp_rows), vec![path]);
    }

    #[test]
    fn execution_file_modified_uses_configured_timezone_for_calendar_boundaries() {
        let connection = open_in_memory_database_with_schema(&SchemaDefinition::new(
            crate::db::CURRENT_SCHEMA_VERSION,
            false,
        ))
        .expect("database should open");
        connection
            .execute(
                "INSERT INTO files (id, path, mtime_ns, size) VALUES (1, '/tmp/late.org', ?1, 0)",
                rusqlite::params![1_784_327_696_000_000_000_i64],
            )
            .expect("file should insert");
        connection
            .execute(
                "INSERT INTO headings (id, file_id, parent_id, level, byte_start, byte_end, title) VALUES (1, 1, NULL, 0, -1, 0, 'Late')",
                [],
            )
            .expect("root heading should insert");
        let options = QueryExecutionOptions {
            query_timezone: Some("Europe/Zurich".to_string()),
            ..QueryExecutionOptions::default()
        };

        for (query, expected) in [
            (r#"(files (file-modified :on "2026-07-17"))"#, Vec::new()),
            (
                r#"(files (file-modified :on "2026-07-18"))"#,
                vec!["/tmp/late.org"],
            ),
            (
                r#"(files (file-modified :from "2026-07-18"))"#,
                vec!["/tmp/late.org"],
            ),
            (r#"(files (file-modified :to "2026-07-17"))"#, Vec::new()),
        ] {
            let rows = execute_sqlite_query_with_options(&connection, &validated(query), &options)
                .expect("file modification query should execute");
            assert_eq!(
                file_paths(rows),
                expected.into_iter().map(str::to_string).collect::<Vec<_>>()
            );
        }
    }

    #[test]
    fn compile_supports_regex_predicates_and_rejects_invalid_patterns() {
        for query in [
            validated(r#"(headings (tags "proj-.*" :regexp t))"#),
            validated(r#"(headings (property "OWNER" "A.*" :regexp t))"#),
            validated(r#"(files (keyword "AUTHOR" "A.*" :regexp t))"#),
            validated(r#"(links (link-target "notes.*" :regexp t))"#),
            validated(r#"(headings (title "Query.*" :regexp t))"#),
            validated(r#"(files (file-path ".*/query-alpha\\.org" :regexp t))"#),
            validated(r#"(headings (outline-contains "Query.*" :regexp t))"#),
            validated(r#"(headings (outline-sequence "Query.*" "Nested.*" :regexp t))"#),
        ] {
            compile_sqlite_query(&query).expect("regexp query should compile");
        }

        for (query, predicate) in [
            (r#"(headings (has-text "(" :regexp t))"#, "has-text"),
            (r#"(headings (title "(" :regexp t))"#, "title"),
            (r#"(headings (tags "(" :regexp t))"#, "tags"),
            (r#"(headings (property "OWNER" "(" :regexp t))"#, "property"),
            (r#"(files (keyword "AUTHOR" "(" :regexp t))"#, "keyword"),
            (
                r#"(headings (outline-contains "(" :regexp t))"#,
                "outline-contains",
            ),
            (
                r#"(headings (outline-sequence "(" "Nested.*" :regexp t))"#,
                "outline-sequence",
            ),
            (r#"(links (link-target "(" :regexp t))"#, "link-target"),
        ] {
            let error = compile_sqlite_query(&validated(query)).expect_err("regexp should fail");
            assert_eq!(
                error.kind,
                QueryExecutionErrorKind::UnsupportedBackendFeature
            );
            assert!(error
                .message
                .contains(&format!("invalid regular expression for {predicate}")));
        }
    }

    #[test]
    fn compile_rejects_unresolved_relative_date_values() {
        for query in [
            validated(r#"(headings (scheduled :on today))"#),
            validated(r#"(files (file-modified :from -7))"#),
        ] {
            let error =
                compile_sqlite_query(&query).expect_err("unresolved relative date should fail");
            assert_eq!(error.kind, QueryExecutionErrorKind::DateResolution);
            assert!(error
                .to_string()
                .contains("resolve relative dates before SQL compilation"));
        }
    }

    #[test]
    fn execution_file_restriction_filters_structural_targets_and_combines_with_query() {
        let connection = seeded_connection();
        let alpha = "/tmp/query-alpha.org".to_string();
        let beta = "/tmp/query-beta.org".to_string();

        let heading_rows = execute_sqlite_query_with_options(
            &connection,
            &validated("(headings)"),
            &QueryExecutionOptions {
                restricted_file_paths: Some(vec![alpha.clone()]),
                ..QueryExecutionOptions::default()
            },
        )
        .expect("restricted heading query should execute");
        let QueryRows::Headings(heading_rows) = heading_rows else {
            panic!("expected heading rows");
        };
        assert!(!heading_rows.is_empty());
        assert!(heading_rows.iter().all(|row| match row {
            HeadingQueryMatch::File(row) => row.path == alpha,
            HeadingQueryMatch::Heading(row) => row.file_path == alpha,
        }));

        let link_rows = execute_sqlite_query_with_options(
            &connection,
            &validated("(links)"),
            &QueryExecutionOptions {
                restricted_file_paths: Some(vec![alpha.clone()]),
                ..QueryExecutionOptions::default()
            },
        )
        .expect("restricted link query should execute");
        let QueryRows::Links(link_rows) = link_rows else {
            panic!("expected link rows");
        };
        assert!(!link_rows.is_empty());
        assert!(link_rows.iter().all(|row| row.file_path == alpha));

        let file_rows = execute_sqlite_query_with_options(
            &connection,
            &validated("(files)"),
            &QueryExecutionOptions {
                restricted_file_paths: Some(vec![beta.clone()]),
                ..QueryExecutionOptions::default()
            },
        )
        .expect("restricted file query should execute");
        assert_eq!(file_paths(file_rows), vec![beta]);

        let mismatch = execute_sqlite_query_with_options(
            &connection,
            &validated(r#"(files (file-title "Beta Index" :exact t))"#),
            &QueryExecutionOptions {
                restricted_file_paths: Some(vec![alpha]),
                ..QueryExecutionOptions::default()
            },
        )
        .expect("restriction should combine with the user query");
        assert!(file_paths(mismatch).is_empty());

        for query in ["(headings)", "(links)", "(files)"] {
            let rows = execute_sqlite_query_with_options(
                &connection,
                &validated(query),
                &QueryExecutionOptions {
                    restricted_file_paths: Some(Vec::new()),
                    ..QueryExecutionOptions::default()
                },
            )
            .expect("an empty restriction should execute");
            let empty = match rows {
                QueryRows::Headings(rows) => rows.is_empty(),
                QueryRows::Links(rows) => rows.is_empty(),
                QueryRows::Files(rows) => rows.is_empty(),
            };
            assert!(empty, "empty restriction returned rows for {query}");
        }
    }

    #[test]
    fn execution_file_restriction_preserves_unicode_and_space_paths() {
        let schema = SchemaDefinition::new(3, false);
        let mut connection =
            open_in_memory_database_with_schema(&schema).expect("database should open");
        let selected = Path::new("/tmp/über space.org");
        seed_database(&mut connection, selected, Path::new("/tmp/other.org"));

        let rows = execute_sqlite_query_with_options(
            &connection,
            &validated("(files)"),
            &QueryExecutionOptions {
                restricted_file_paths: Some(vec![selected.display().to_string()]),
                ..QueryExecutionOptions::default()
            },
        )
        .expect("Unicode restriction should execute");
        assert_eq!(file_paths(rows), vec![selected.display().to_string()]);
    }

    #[test]
    fn execution_file_restriction_respects_small_runtime_variable_limit() {
        let connection = seeded_connection();
        let restricted = vec![
            "/tmp/query-alpha.org".to_string(),
            "/tmp/query-beta.org".to_string(),
            "/tmp/query-gamma.org".to_string(),
        ];
        let previous = connection.set_limit(Limit::SQLITE_LIMIT_VARIABLE_NUMBER, 2);

        let rows = execute_sqlite_query_with_options(
            &connection,
            &validated("(files)"),
            &QueryExecutionOptions {
                restricted_file_paths: Some(restricted.clone()),
                ..QueryExecutionOptions::default()
            },
        )
        .expect("restricted query should batch inserts below the variable limit");
        connection.set_limit(Limit::SQLITE_LIMIT_VARIABLE_NUMBER, previous);

        assert_eq!(file_paths(rows), restricted);
    }

    #[test]
    fn execution_matches_metadata_queries_and_boolean_composition() {
        let connection = seeded_connection();

        let headings_query = validated(
            r#"(headings
                (and
                  (todo "NEXT")
                  (priority "A")
                  (title "Engine")
                  (file-path "alpha.org")
                  (not (file-title "Beta"))))"#,
        );
        let heading_rows = execute_sqlite_query(&connection, &headings_query)
            .expect("heading query should execute");
        assert_eq!(
            heading_rows,
            QueryRows::Headings(vec![HeadingQueryMatch::Heading(HeadingQueryRow {
                id: 11,
                file_id: 2,
                file_path: "/tmp/query-alpha.org".to_string(),
                parent_id: Some(10),
                level: 1,
                line_number: Some(3),
                byte_start: 10,
                byte_end: 40,
                title: "Query Engine".to_string(),
                title_raw: Some("Query Engine".to_string()),
                todo_keyword: Some("NEXT".to_string()),
                todo_type: Some("open".to_string()),
                priority: Some("A".to_string()),
                scheduled_raw: Some("<2026-01-03 Fri>".to_string()),
                scheduled_ts: Some(1_767_398_400),
                deadline_raw: None,
                deadline_ts: None,
                closed_raw: None,
                closed_ts: None,
                archivedp: false,
                footnote_section_p: false,
                all_tags_json: "[\"filetag\",\"project\"]".to_string(),
            })])
        );

        let links_query = validated(
            r#"(links
                (and
                  (link-type "file")
                  (link-target "beta.org")
                  (has-description)
                  (status "resolved")
                  (source (headings (title "Engine")))
                  (target (files (file-title "Beta Index" :exact t)))))"#,
        );
        let link_rows =
            execute_sqlite_query(&connection, &links_query).expect("link query should execute");
        assert_eq!(
            link_rows,
            QueryRows::Links(vec![LinkQueryRow {
                id: 100,
                file_id: 2,
                file_path: "/tmp/query-alpha.org".to_string(),
                heading_id: 11,
                heading_level: 1,
                source_context: "normal".to_string(),
                format: "bracket".to_string(),
                link_type: "file".to_string(),
                raw: "[[file:beta.org][Beta notes]]".to_string(),
                raw_target: "file:beta.org".to_string(),
                raw_description: Some("Beta notes".to_string()),
                path: "beta.org".to_string(),
                search_option: None,
                path_absolute: Some("/tmp/query-beta.org".to_string()),
                target_file_id: Some(1),
                target_heading_id: Some(20),
                target_custom_id: None,
                target_id: None,
                resolution_status: Some("resolved".to_string()),
                resolution_diagnostic: None,
                byte_start: 50,
                byte_end: 80,
                line: 4,
            }])
        );

        let files_query = validated(
            r#"(files
                (or
                  (file-title "Beta Index" :exact t)
                  (and
                    (keyword "AUTHOR" "Alice")
                    (property "CATEGORY" "work")
                    (tags "filetag"))))"#,
        );
        let file_rows =
            execute_sqlite_query(&connection, &files_query).expect("file query should execute");
        assert_eq!(
            file_rows,
            QueryRows::Files(vec![
                FileQueryRow {
                    id: 2,
                    path: "/tmp/query-alpha.org".to_string(),
                    mtime_ns: 1_767_398_400_000_000_000,
                    size: 100,
                    content_hash: None,
                    indexed_at: Some(1_767_398_410),
                    root_heading_id: 10,
                    root_title: "Alpha Index".to_string(),
                    root_title_raw: Some("Alpha Index".to_string()),
                    root_line_number: None,
                },
                FileQueryRow {
                    id: 1,
                    path: "/tmp/query-beta.org".to_string(),
                    mtime_ns: 1_767_484_800_000_000_000,
                    size: 120,
                    content_hash: None,
                    indexed_at: Some(1_767_484_810),
                    root_heading_id: 20,
                    root_title: "Beta Index".to_string(),
                    root_title_raw: Some("Beta Index".to_string()),
                    root_line_number: None,
                },
            ])
        );
    }

    #[test]
    fn execution_compares_priorities_by_semantic_rank_and_preserves_source_values() {
        let connection = seeded_connection();
        for (id, priority) in [(101, "1"), (102, "2"), (103, "C"), (104, "3"), (105, "10")] {
            connection
                .execute(
                    "INSERT INTO headings
                     (id, file_id, parent_id, level, byte_start, byte_end, title, priority)
                     VALUES (?1, 2, 10, 1, ?2, ?3, ?4, ?5)",
                    rusqlite::params![
                        id,
                        id * 10,
                        id * 10 + 5,
                        format!("Priority {priority}"),
                        priority
                    ],
                )
                .expect("priority heading should insert");
        }

        let matching_priorities = |query: &str| {
            let QueryRows::Headings(rows) = execute_sqlite_query(&connection, &validated(query))
                .expect("priority query should execute")
            else {
                panic!("headings query should return heading rows");
            };
            rows.into_iter()
                .filter_map(|row| match row {
                    HeadingQueryMatch::Heading(row) => row.priority,
                    HeadingQueryMatch::File(_) => None,
                })
                .collect::<Vec<_>>()
        };

        assert_eq!(
            matching_priorities(r#"(headings (priority "A"))"#),
            ["A", "1"]
        );
        assert_eq!(
            matching_priorities(r#"(headings (priority "1"))"#),
            ["A", "1"]
        );
        assert_eq!(
            matching_priorities(r#"(headings (priority "B"))"#),
            ["B", "2"]
        );
        assert_eq!(
            matching_priorities(r#"(headings (priority > "B"))"#),
            ["A", "1"]
        );
        assert_eq!(
            matching_priorities(r#"(headings (priority >= "B"))"#),
            ["A", "B", "1", "2"]
        );
        assert_eq!(
            matching_priorities(r#"(headings (priority < "B"))"#),
            ["C", "3", "10"]
        );
        assert_eq!(
            matching_priorities(r#"(headings (priority <= "B"))"#),
            ["B", "2", "C", "3", "10"]
        );
        assert_eq!(
            matching_priorities(r#"(headings (priority > "2"))"#),
            ["A", "1"]
        );
        assert_eq!(
            matching_priorities(r#"(headings (priority >= "2"))"#),
            ["A", "B", "1", "2"]
        );
        assert_eq!(
            matching_priorities(r#"(headings (priority < "2"))"#),
            ["C", "3", "10"]
        );
        assert_eq!(
            matching_priorities(r#"(headings (priority <= "2"))"#),
            ["B", "2", "C", "3", "10"]
        );
    }

    #[test]
    fn execution_matches_hierarchy_predicates() {
        let connection = seeded_connection();

        let parent_rows = execute_sqlite_query(&connection, &validated(r#"(headings (parent))"#))
            .expect("parent query should execute");
        assert_eq!(heading_ids(parent_rows), vec![11, 12, 13, 14, 15, 21, 31]);

        let parent_nested_rows = execute_sqlite_query(
            &connection,
            &validated(r#"(headings (parent (headings (title "Query Engine" :exact t))))"#),
        )
        .expect("nested parent query should execute");
        assert_eq!(heading_ids(parent_nested_rows), vec![12, 14, 15]);

        let ancestor_rows = execute_sqlite_query(
            &connection,
            &validated(r#"(headings (ancestors (headings (title "Query Engine" :exact t))))"#),
        )
        .expect("ancestor query should execute");
        assert_eq!(heading_ids(ancestor_rows), vec![12, 14, 15]);

        let children_rows =
            execute_sqlite_query(&connection, &validated(r#"(headings (children))"#))
                .expect("children query should execute");
        assert_eq!(heading_ids(children_rows), vec![11]);

        let child_nested_rows = execute_sqlite_query(
            &connection,
            &validated(r#"(headings (children (headings (title "Nested Task" :exact t))))"#),
        )
        .expect("child nested query should execute");
        assert_eq!(heading_ids(child_nested_rows), vec![11]);

        let descendant_rows = execute_sqlite_query(
            &connection,
            &validated(r#"(headings (descendants (headings (title "Nested Task" :exact t))))"#),
        )
        .expect("descendant query should execute");
        assert_eq!(heading_ids(descendant_rows), vec![11]);

        let root_parent_rows = execute_sqlite_query(
            &connection,
            &validated(r#"(headings (parent (headings (level 0))))"#),
        )
        .expect("root parent query should execute");
        assert_eq!(heading_ids(root_parent_rows), vec![11, 13, 21, 31]);

        let root_parent_boolean_rows = execute_sqlite_query(
            &connection,
            &validated(
                r#"(headings
                    (parent
                      (headings
                        (and
                          (level 0)
                          (title "Alpha Index" :exact t)))))"#,
            ),
        )
        .expect("boolean root parent query should execute");
        assert_eq!(heading_ids(root_parent_boolean_rows), vec![11, 13]);

        let root_ancestor_rows = execute_sqlite_query(
            &connection,
            &validated(r#"(headings (ancestors (headings (level 0))))"#),
        )
        .expect("root ancestor query should execute");
        assert_eq!(
            heading_ids(root_ancestor_rows),
            vec![11, 12, 13, 14, 15, 21, 31]
        );

        let root_tag_ancestor_rows = execute_sqlite_query(
            &connection,
            &validated(r#"(headings (ancestors (headings (tags "filetag" :inherit nil))))"#),
        )
        .expect("root tag ancestor query should execute");
        assert_eq!(
            heading_ids(root_tag_ancestor_rows),
            vec![11, 12, 13, 14, 15]
        );

        let root_children_rows = execute_sqlite_query(
            &connection,
            &validated(r#"(headings (children (headings (title "Query Engine" :exact t))))"#),
        )
        .expect("root children query should execute");
        assert_eq!(
            heading_file_paths(root_children_rows),
            vec!["/tmp/query-alpha.org"]
        );

        let root_descendant_rows = execute_sqlite_query(
            &connection,
            &validated(r#"(headings (descendants (headings (title "Nested Task" :exact t))))"#),
        )
        .expect("root descendant query should execute");
        assert_eq!(
            heading_file_paths(root_descendant_rows),
            vec!["/tmp/query-alpha.org"]
        );

        let root_parent_outer_rows = execute_sqlite_query(
            &connection,
            &validated(
                r#"(headings
                    (and
                      (level 0)
                      (parent (headings (title "Query Engine" :exact t)))))"#,
            ),
        )
        .expect("root parent outer query should execute");
        assert!(heading_file_paths(root_parent_outer_rows).is_empty());

        let root_ancestor_outer_rows = execute_sqlite_query(
            &connection,
            &validated(
                r#"(headings
                    (and
                      (level 0)
                      (ancestors (headings (title "Query Engine" :exact t)))))"#,
            ),
        )
        .expect("root ancestor outer query should execute");
        assert!(heading_file_paths(root_ancestor_outer_rows).is_empty());

        let self_descendant_rows = execute_sqlite_query(
            &connection,
            &validated(
                r#"(headings
                    (and
                      (title "Nested Task" :exact t)
                      (descendants (headings (title "Nested Task" :exact t)))))"#,
            ),
        )
        .expect("self descendant query should execute");
        assert!(heading_ids(self_descendant_rows).is_empty());
    }

    #[test]
    fn nested_hierarchy_heading_scopes_do_not_filter_out_synthetic_roots() {
        let compiled =
            compile_sqlite_query(&validated(r#"(headings (parent (headings (level 0))))"#))
                .expect("nested hierarchy query should compile");

        assert!(compiled.sql.contains("h0.level > 0"));
        assert!(!compiled.sql.contains("h1.level > 0"));
        assert!(compiled.sql.contains("h1.level = ?"));
    }

    #[test]
    fn nested_relation_heading_scopes_do_not_filter_out_synthetic_roots() {
        let compiled = compile_sqlite_query(&validated(r#"(links (source (headings (level 0))))"#))
            .expect("nested relation query should compile");

        assert!(!compiled.sql.contains("h1.level > 0"));
        assert!(compiled.sql.contains("h1.level = ?"));
    }

    #[test]
    fn compile_supports_documented_outline_and_hierarchy_heading_predicates() {
        for query in [
            r#"(headings (outline-contains "Query"))"#,
            r#"(headings (outline-sequence "Query" "Nested"))"#,
            r#"(headings (parent))"#,
            r#"(headings (children))"#,
            r#"(headings (ancestors))"#,
            r#"(headings (descendants))"#,
        ] {
            compile_sqlite_query(&validated(query)).expect("query should compile");
        }
    }

    #[test]
    fn execution_matches_outline_predicates() {
        let connection = seeded_connection();

        let contains_rows = execute_sqlite_query(
            &connection,
            &validated(r#"(headings (outline-contains "Query" "Nested"))"#),
        )
        .expect("outline contains query should execute");
        assert_eq!(heading_ids(contains_rows), vec![12]);

        let contains_order_insensitive_rows = execute_sqlite_query(
            &connection,
            &validated(r#"(headings (outline-contains "Nested" "Query"))"#),
        )
        .expect("outline contains query should execute");
        assert_eq!(heading_ids(contains_order_insensitive_rows), vec![12]);

        let sequence_top_level_rows = execute_sqlite_query(
            &connection,
            &validated(
                r#"(headings
                    (and
                      (outline-sequence "Query Engine" :exact t)
                      (level 1)))"#,
            ),
        )
        .expect("outline sequence top-level query should execute");
        assert_eq!(heading_ids(sequence_top_level_rows), vec![11]);

        let contains_root_name_rows = execute_sqlite_query(
            &connection,
            &validated(r#"(headings (outline-contains "Alpha Index"))"#),
        )
        .expect("outline contains should execute");
        assert_eq!(
            heading_ids(contains_root_name_rows),
            vec![11, 12, 13, 14, 15]
        );

        let contains_root_and_child_rows = execute_sqlite_query(
            &connection,
            &validated(r#"(headings (outline-contains "Alpha Index" "Nested Task" :regexp nil))"#),
        )
        .expect("outline contains should match root and child components");
        assert_eq!(heading_ids(contains_root_and_child_rows), vec![12]);

        let root_contains_rows = execute_sqlite_query(
            &connection,
            &validated(r#"(headings (and (level 0) (outline-contains "Alpha Index")))"#),
        )
        .expect("root outline contains should execute");
        assert_eq!(
            heading_file_paths(root_contains_rows),
            vec!["/tmp/query-alpha.org"]
        );

        let sequence_exact_rows = execute_sqlite_query(
            &connection,
            &validated(r#"(headings (outline-sequence "Query Engine" "Nested Task" :exact t))"#),
        )
        .expect("outline sequence exact query should execute");
        assert_eq!(heading_ids(sequence_exact_rows), vec![12]);

        let sequence_substring_rows = execute_sqlite_query(
            &connection,
            &validated(r#"(headings (outline-sequence "Query" "Nested"))"#),
        )
        .expect("outline sequence query should execute");
        assert_eq!(heading_ids(sequence_substring_rows), vec![12]);

        let sequence_root_and_parent_rows = execute_sqlite_query(
            &connection,
            &validated(r#"(headings (outline-sequence "Alpha Index" "Query Engine" :exact t))"#),
        )
        .expect("root-leading outline sequence should execute");
        assert_eq!(
            heading_ids(sequence_root_and_parent_rows),
            vec![11, 12, 14, 15]
        );

        let sequence_root_parent_child_rows = execute_sqlite_query(
            &connection,
            &validated(
                r#"(headings (outline-sequence "Alpha Index" "Query Engine" "Nested Task" :exact t))"#,
            ),
        )
        .expect("root-leading child outline sequence should execute");
        assert_eq!(heading_ids(sequence_root_parent_child_rows), vec![12]);

        let sequence_root_only_rows = execute_sqlite_query(
            &connection,
            &validated(r#"(headings (outline-sequence "Alpha Index" :exact t))"#),
        )
        .expect("one-component root sequence should execute");
        assert_eq!(
            heading_ids(sequence_root_only_rows),
            vec![11, 12, 13, 14, 15]
        );

        let root_sequence_rows = execute_sqlite_query(
            &connection,
            &validated(r#"(headings (and (level 0) (outline-sequence "Alpha Index" :exact t)))"#),
        )
        .expect("root outline sequence should execute");
        assert_eq!(
            heading_file_paths(root_sequence_rows),
            vec!["/tmp/query-alpha.org"]
        );

        let sequence_non_contiguous_rows = execute_sqlite_query(
            &connection,
            &validated(
                r#"(headings (outline-sequence "Query Engine" "Statistic Cookies" :exact t))"#,
            ),
        )
        .expect("outline sequence query should execute");
        assert_eq!(heading_ids(sequence_non_contiguous_rows), vec![14]);

        let sequence_missing_rows = execute_sqlite_query(
            &connection,
            &validated(r#"(headings (outline-sequence "Query Engine" "Loose Note" :exact t))"#),
        )
        .expect("outline sequence should execute");
        assert_eq!(heading_ids(sequence_missing_rows), Vec::<i64>::new());

        let regexp_contains_rows = execute_sqlite_query(
            &connection,
            &validated(r#"(headings (outline-contains "Query.*" "Nested.*" :regexp t))"#),
        )
        .expect("outline regexp contains query should execute");
        assert_eq!(heading_ids(regexp_contains_rows), vec![12]);

        let regexp_sequence_rows = execute_sqlite_query(
            &connection,
            &validated(r#"(headings (outline-sequence "Query.*" "Nested.*" :regexp t))"#),
        )
        .expect("outline regexp sequence query should execute");
        assert_eq!(heading_ids(regexp_sequence_rows), vec![12]);

        let regexp_root_rows = execute_sqlite_query(
            &connection,
            &validated(r#"(headings (outline-contains "Alpha.*" :regexp t))"#),
        )
        .expect("outline regexp root query should execute");
        assert_eq!(heading_ids(regexp_root_rows), vec![11, 12, 13, 14, 15]);
    }

    #[test]
    fn compiled_tag_predicates_use_normalized_direct_and_effective_tables() {
        let inherited = compile_sqlite_query(&validated(r#"(headings (tags "project"))"#))
            .expect("inherited tag query should compile");
        assert!(inherited.sql.contains("FROM effective_tags"));
        assert!(!inherited.sql.contains("WITH RECURSIVE lineage"));

        let local = compile_sqlite_query(&validated(r#"(headings (tags "project" :inherit nil))"#))
            .expect("local tag query should compile");
        assert!(local.sql.contains("FROM tags"));
        assert!(!local.sql.contains("FROM effective_tags"));
        assert!(!local.sql.contains("WITH RECURSIVE lineage"));

        let connection = seeded_connection();
        let explain_sql = format!("EXPLAIN QUERY PLAN {}", inherited.sql);
        let mut statement = connection
            .prepare(&explain_sql)
            .expect("inherited tag plan should prepare");
        let plan = statement
            .query_map(params_from_iter(inherited.params.iter()), |row| {
                row.get::<_, String>(3)
            })
            .expect("inherited tag plan should query")
            .collect::<Result<Vec<_>, _>>()
            .expect("inherited tag plan should decode");
        assert!(
            plan.iter()
                .any(|detail| detail.contains("idx_effective_tags_tag_heading")),
            "expected effective tag index in plan: {plan:?}"
        );
    }

    #[test]
    fn compiled_property_predicates_use_materialized_effective_properties() {
        for query in [
            r#"(headings (property "OWNER" "Alice"))"#,
            r#"(headings (property "OWNER" "Alice" :inherit t))"#,
            r#"(headings (property "OWNER" "Alice" :inherit nil))"#,
            r#"(headings (property "OWNER" "A.*" :regexp t))"#,
        ] {
            let compiled = compile_sqlite_query(&validated(query)).expect("query should compile");
            assert!(compiled.sql.contains("FROM effective_properties"));
            assert!(!compiled.sql.contains("lineage_up"));
            assert!(!compiled.sql.contains("local_summary"));
        }
    }

    #[test]
    fn production_predicate_driven_metadata_strategy_preserves_boolean_query_rows() {
        let connection = seeded_connection();
        for query in [
            r#"(headings (and (level 1) (tags "urgent" :inherit nil)))"#,
            r#"(headings (and (level 1) (property "OWNER" "Bob" :inherit nil)))"#,
            r#"(headings (and (level 1) (property "CATEGORY" "work" :inherit t)))"#,
            r#"(headings (and (level 1) (keyword "AUTHOR" "Alice" :inherit nil)))"#,
            r#"(headings (and (level 1) (not (property "OWNER" "Bob" :inherit nil))))"#,
            r#"(headings (and (level 1) (or (tags "urgent" :inherit nil) (property "OWNER" "Bob" :inherit nil))))"#,
        ] {
            let validated = validated(query);
            let legacy = execute_sqlite_query_with_relation_and_metadata_strategy(
                &connection,
                &validated,
                &QueryExecutionOptions::default(),
                MetadataPredicateSqlStrategy::HeadingDrivenExists,
            )
            .expect("heading-driven metadata query should execute")
            .rows;
            let production = execute_sqlite_query(&connection, &validated)
                .expect("production metadata query should execute");
            assert_eq!(legacy, production, "query changed result: {query}");
        }
    }

    #[test]
    fn production_metadata_strategy_compiles_indexable_equality_subqueries() {
        let tag = compile_sqlite_query(&validated(
            r#"(headings (and (level 1) (tags "urgent" :inherit nil)))"#,
        ))
        .expect("production tag query should compile");
        assert!(tag
            .sql
            .contains("IN (SELECT tags.heading_id FROM tags WHERE tags.tag IN"));

        let property = compile_sqlite_query(&validated(
            r#"(headings (and (level 1) (property "OWNER" "Bob" :inherit nil)))"#,
        ))
        .expect("production property query should compile");
        assert!(property.sql.contains(
            "IN (SELECT heading_id FROM effective_properties WHERE key = ? AND local_value IS NOT NULL AND local_value = ?"
        ));

        let keyword = compile_sqlite_query(&validated(
            r#"(headings (and (level 1) (keyword "AUTHOR" "Alice" :inherit nil)))"#,
        ))
        .expect("production keyword query should compile");
        assert!(keyword.sql.contains(
            "IN (SELECT keywords.heading_id FROM keywords WHERE keywords.keyword = ? COLLATE NOCASE AND keywords.value = ?"
        ));

        let regexp = compile_sqlite_query(&validated(
            r#"(headings (property "OWNER" "B.*" :regexp t :inherit nil))"#,
        ))
        .expect("regexp property query should compile");
        assert!(regexp
            .sql
            .contains("EXISTS (SELECT 1 FROM effective_properties"));
    }

    #[test]
    fn predicate_driven_metadata_strategy_compiles_indexable_subqueries() {
        let tag = compile_sqlite_query_with_metadata_strategy(
            &validated(r#"(headings (and (level 1) (tags "urgent" :inherit nil)))"#),
            false,
            MetadataPredicateSqlStrategy::PredicateDrivenIn,
        )
        .expect("predicate-driven tag query should compile");
        assert!(tag
            .sql
            .contains("IN (SELECT tags.heading_id FROM tags WHERE tags.tag IN"));

        let property = compile_sqlite_query_with_metadata_strategy(
            &validated(r#"(headings (and (level 1) (property "OWNER" "Bob" :inherit nil)))"#),
            false,
            MetadataPredicateSqlStrategy::PredicateDrivenIn,
        )
        .expect("predicate-driven property query should compile");
        assert!(property.sql.contains(
            "IN (SELECT heading_id FROM effective_properties WHERE key = ? AND local_value IS NOT NULL AND local_value = ?"
        ));

        let keyword = compile_sqlite_query_with_metadata_strategy(
            &validated(r#"(headings (and (level 1) (keyword "AUTHOR" "Alice" :inherit nil)))"#),
            false,
            MetadataPredicateSqlStrategy::PredicateDrivenIn,
        )
        .expect("predicate-driven keyword query should compile");
        assert!(keyword.sql.contains(
            "IN (SELECT keywords.heading_id FROM keywords WHERE keywords.keyword = ? COLLATE NOCASE AND keywords.value = ?"
        ));
    }

    #[test]
    fn execution_matches_outline_predicates_in_boolean_and_hierarchy_queries() {
        let connection = seeded_connection();

        let boolean_rows = execute_sqlite_query(
            &connection,
            &validated(
                r#"(headings
                    (and
                      (outline-sequence "Query Engine" "Statistic Cookies" :exact t)
                      (property "ADD-VALUE" "is valid" :inherit t)))"#,
            ),
        )
        .expect("boolean outline query should execute");
        assert_eq!(heading_ids(boolean_rows), vec![14]);

        let descendant_rows = execute_sqlite_query(
            &connection,
            &validated(
                r#"(headings
                    (descendants
                      (headings
                        (outline-sequence "Query Engine" "Nested Task" :exact t))))"#,
            ),
        )
        .expect("hierarchy outline query should execute");
        assert_eq!(heading_ids(descendant_rows), vec![11]);
    }

    #[test]
    fn execution_matches_heading_and_file_link_relation_queries() {
        let connection = seeded_connection();

        let has_link_rows =
            execute_sqlite_query(&connection, &validated(r#"(headings (has-link))"#))
                .expect("has-link query should execute");
        assert_eq!(heading_ids(has_link_rows), vec![11, 12, 13, 21]);

        let has_file_link_rows = execute_sqlite_query(
            &connection,
            &validated(r#"(headings (has-link (links (link-type "file"))))"#),
        )
        .expect("has-link file query should execute");
        assert_eq!(heading_ids(has_file_link_rows), vec![11, 12, 13, 21]);

        let links_to_file_rows = execute_sqlite_query(
            &connection,
            &validated(r#"(headings (links-to (files (file-title "Beta Index" :exact t))))"#),
        )
        .expect("links-to file query should execute");
        assert_eq!(heading_ids(links_to_file_rows), vec![11, 12]);

        let links_to_heading_rows = execute_sqlite_query(
            &connection,
            &validated(r#"(headings (links-to (headings (title "Beta Target" :exact t))))"#),
        )
        .expect("links-to heading query should execute");
        assert_eq!(heading_ids(links_to_heading_rows), vec![12]);

        let links_to_root_rows = execute_sqlite_query(
            &connection,
            &validated(r#"(headings (and (level 0) (links-to (headings (level 0)))))"#),
        )
        .expect("links-to root heading query should execute");
        let QueryRows::Headings(links_to_root_rows) = links_to_root_rows else {
            panic!("expected heading matches");
        };
        assert!(matches!(
            links_to_root_rows.as_slice(),
            [HeadingQueryMatch::File(alpha), HeadingQueryMatch::File(beta)]
                if alpha.path == "/tmp/query-alpha.org" && beta.path == "/tmp/query-beta.org"
        ));

        let file_links_to_root_rows = execute_sqlite_query(
            &connection,
            &validated(r#"(files (links-to (headings (level 0))))"#),
        )
        .expect("file links-to root heading query should execute");
        assert_eq!(
            file_paths(file_links_to_root_rows),
            vec![
                "/tmp/query-alpha.org".to_string(),
                "/tmp/query-beta.org".to_string()
            ]
        );

        let linked_from_any_rows =
            execute_sqlite_query(&connection, &validated(r#"(headings (linked-from :any))"#))
                .expect("linked-from any query should execute");
        assert_eq!(heading_ids(linked_from_any_rows), vec![11, 21]);

        let linked_from_heading_rows = execute_sqlite_query(
            &connection,
            &validated(r#"(headings (linked-from (headings (title "Nested Task" :exact t))))"#),
        )
        .expect("linked-from heading query should execute");
        assert_eq!(heading_ids(linked_from_heading_rows), vec![21]);

        let linked_from_root_rows = execute_sqlite_query(
            &connection,
            &validated(r#"(headings (and (level 0) (linked-from (headings (level 0)))))"#),
        )
        .expect("linked-from root heading query should execute");
        let QueryRows::Headings(linked_from_root_rows) = linked_from_root_rows else {
            panic!("expected heading matches");
        };
        assert!(matches!(
            linked_from_root_rows.as_slice(),
            [HeadingQueryMatch::File(alpha), HeadingQueryMatch::File(beta)]
                if alpha.path == "/tmp/query-alpha.org" && beta.path == "/tmp/query-beta.org"
        ));

        let file_linked_from_root_rows = execute_sqlite_query(
            &connection,
            &validated(r#"(files (linked-from (headings (level 0))))"#),
        )
        .expect("file linked-from root heading query should execute");
        assert_eq!(
            file_paths(file_linked_from_root_rows),
            vec![
                "/tmp/query-alpha.org".to_string(),
                "/tmp/query-beta.org".to_string()
            ]
        );

        let linked_from_file_rows = execute_sqlite_query(
            &connection,
            &validated(r#"(headings (linked-from (files (file-title "Beta Index" :exact t))))"#),
        )
        .expect("linked-from file query should execute");
        assert_eq!(heading_ids(linked_from_file_rows), vec![11]);

        let file_has_link_rows =
            execute_sqlite_query(&connection, &validated(r#"(files (has-link))"#))
                .expect("file has-link query should execute");
        assert_eq!(
            file_paths(file_has_link_rows),
            vec![
                "/tmp/query-alpha.org".to_string(),
                "/tmp/query-beta.org".to_string()
            ]
        );

        let file_links_to_heading_rows = execute_sqlite_query(
            &connection,
            &validated(r#"(files (links-to (headings (title "Beta Target" :exact t))))"#),
        )
        .expect("file links-to heading query should execute");
        assert_eq!(
            file_paths(file_links_to_heading_rows),
            vec!["/tmp/query-alpha.org".to_string()]
        );

        let file_linked_from_any_rows =
            execute_sqlite_query(&connection, &validated(r#"(files (linked-from :any))"#))
                .expect("file linked-from any query should execute");
        assert_eq!(
            file_paths(file_linked_from_any_rows),
            vec![
                "/tmp/query-alpha.org".to_string(),
                "/tmp/query-beta.org".to_string()
            ]
        );

        let file_linked_from_heading_rows = execute_sqlite_query(
            &connection,
            &validated(r#"(files (linked-from (headings (title "Beta Target" :exact t))))"#),
        )
        .expect("file linked-from heading query should execute");
        assert_eq!(
            file_paths(file_linked_from_heading_rows),
            vec!["/tmp/query-alpha.org".to_string()]
        );
    }

    #[test]
    fn execution_matches_link_source_target_and_status_queries() {
        let connection = seeded_connection();

        let source_heading_rows = execute_sqlite_query(
            &connection,
            &validated(r#"(links (source (headings (title "Nested Task" :exact t))))"#),
        )
        .expect("source heading query should execute");
        assert_eq!(link_ids(source_heading_rows), vec![101]);

        let source_root_rows = execute_sqlite_query(
            &connection,
            &validated(r#"(links (source (headings (level 0))))"#),
        )
        .expect("source root heading query should execute");
        assert_eq!(link_ids(source_root_rows), vec![102, 104]);

        let source_root_file_name_rows = execute_sqlite_query(
            &connection,
            &validated(
                r#"(links (source (headings (and (level 0) (file-name "query-alpha.org" :exact t)))))"#,
            ),
        )
        .expect("source root file-name query should execute");
        assert_eq!(link_ids(source_root_file_name_rows), vec![102]);

        let source_file_rows = execute_sqlite_query(
            &connection,
            &validated(r#"(links (source (files (file-title "Beta Index" :exact t))))"#),
        )
        .expect("source file query should execute");
        assert_eq!(link_ids(source_file_rows), vec![104, 103, 106]);

        let target_heading_rows = execute_sqlite_query(
            &connection,
            &validated(r#"(links (target (headings (title "Beta Target" :exact t))))"#),
        )
        .expect("target heading query should execute");
        assert_eq!(link_ids(target_heading_rows), vec![101]);

        let target_root_rows = execute_sqlite_query(
            &connection,
            &validated(r#"(links (target (headings (level 0))))"#),
        )
        .expect("target root heading query should execute");
        assert_eq!(link_ids(target_root_rows), vec![102, 100, 104]);

        let target_file_rows = execute_sqlite_query(
            &connection,
            &validated(r#"(links (target (files (file-title "Beta Index" :exact t))))"#),
        )
        .expect("target file query should execute");
        assert_eq!(link_ids(target_file_rows), vec![102, 100, 101]);

        let target_any_rows =
            execute_sqlite_query(&connection, &validated(r#"(links (target :any))"#))
                .expect("target any query should execute");
        assert_eq!(link_ids(target_any_rows), vec![102, 100, 101, 104, 103]);

        let broken_rows = execute_sqlite_query(
            &connection,
            &validated(
                r#"(links (and (status "broken") (link-target "file:missing.org" :exact t)))"#,
            ),
        )
        .expect("broken status query should execute");
        assert_eq!(link_ids(broken_rows), vec![105]);

        let unresolved_rows =
            execute_sqlite_query(&connection, &validated(r#"(links (status "unresolved"))"#))
                .expect("unresolved status query should execute");
        assert_eq!(link_ids(unresolved_rows), vec![106]);

        let ambiguous_rows =
            execute_sqlite_query(&connection, &validated(r#"(links (status "ambiguous"))"#))
                .expect("ambiguous status query should execute");
        assert_eq!(link_ids(ambiguous_rows), vec![107]);

        let ambiguous_target_rows = execute_sqlite_query(
            &connection,
            &validated(r#"(links (and (status "ambiguous") (target :any)))"#),
        )
        .expect("ambiguous target-any query should execute");
        assert_eq!(link_ids(ambiguous_target_rows), Vec::<i64>::new());

        let ambiguous_target_file_rows = execute_sqlite_query(
            &connection,
            &validated(
                r#"(links
                    (and
                      (status "ambiguous")
                      (target (files (file-title "Gamma Index" :exact t)))))"#,
            ),
        )
        .expect("ambiguous target-file query should execute");
        assert_eq!(link_ids(ambiguous_target_file_rows), Vec::<i64>::new());

        let ambiguous_target_heading_rows = execute_sqlite_query(
            &connection,
            &validated(
                r#"(links
                    (and
                      (status "ambiguous")
                      (target (headings (title "Gamma Candidate" :exact t)))))"#,
            ),
        )
        .expect("ambiguous target-heading query should execute");
        assert_eq!(link_ids(ambiguous_target_heading_rows), Vec::<i64>::new());

        let ambiguous_links_to_file_rows = execute_sqlite_query(
            &connection,
            &validated(r#"(headings (links-to (files (file-title "Gamma Index" :exact t))))"#),
        )
        .expect("ambiguous links-to file query should execute");
        assert_eq!(heading_ids(ambiguous_links_to_file_rows), Vec::<i64>::new());

        let ambiguous_links_to_heading_rows = execute_sqlite_query(
            &connection,
            &validated(r#"(headings (links-to (headings (title "Gamma Candidate" :exact t))))"#),
        )
        .expect("ambiguous links-to heading query should execute");
        assert_eq!(
            heading_ids(ambiguous_links_to_heading_rows),
            Vec::<i64>::new()
        );

        let ambiguous_file_links_to_heading_rows = execute_sqlite_query(
            &connection,
            &validated(r#"(files (links-to (headings (title "Gamma Candidate" :exact t))))"#),
        )
        .expect("ambiguous file links-to heading query should execute");
        assert_eq!(
            file_paths(ambiguous_file_links_to_heading_rows),
            Vec::<String>::new()
        );

        let ambiguous_file_links_to_file_rows = execute_sqlite_query(
            &connection,
            &validated(r#"(files (links-to (files (file-title "Gamma Index" :exact t))))"#),
        )
        .expect("ambiguous file links-to file query should execute");
        assert_eq!(
            file_paths(ambiguous_file_links_to_file_rows),
            Vec::<String>::new()
        );

        let gamma_heading_backlink_rows = execute_sqlite_query(
            &connection,
            &validated(r#"(headings (linked-from (files (file-title "Gamma Index" :exact t))))"#),
        )
        .expect("gamma heading backlink query should execute");
        assert_eq!(heading_ids(gamma_heading_backlink_rows), Vec::<i64>::new());

        let gamma_file_backlink_rows = execute_sqlite_query(
            &connection,
            &validated(r#"(files (linked-from (headings (title "Gamma Candidate" :exact t))))"#),
        )
        .expect("gamma file backlink query should execute");
        assert_eq!(file_paths(gamma_file_backlink_rows), Vec::<String>::new());
    }

    #[test]
    fn execution_matches_effective_heading_tag_queries() {
        let connection = seeded_connection();

        let local_rows = execute_sqlite_query(
            &connection,
            &validated(
                r#"(headings (and (title "Nested Task" :exact t) (tags "urgent" :inherit nil)))"#,
            ),
        )
        .expect("local tag query should execute");
        assert_eq!(heading_ids(local_rows), vec![12]);

        let inherited_rows = execute_sqlite_query(
            &connection,
            &validated(r#"(headings (and (title "Nested Task" :exact t) (tags "project")))"#),
        )
        .expect("inherited tag query should execute");
        assert_eq!(heading_ids(inherited_rows), vec![12]);

        let explicit_inherited_rows = execute_sqlite_query(
            &connection,
            &validated(
                r#"(headings (and (title "Nested Task" :exact t) (tags "project" :inherit t)))"#,
            ),
        )
        .expect("explicit inherited tag query should execute");
        assert_eq!(heading_ids(explicit_inherited_rows), vec![12]);

        let alias_rows = execute_sqlite_query(
            &connection,
            &validated(
                r#"(headings (and (title "Nested Task" :exact t) (tags-all "project" "urgent")))"#,
            ),
        )
        .expect("tags-all alias query should execute");
        assert_eq!(heading_ids(alias_rows), vec![12]);

        let root_rows = execute_sqlite_query(
            &connection,
            &validated(r#"(headings (and (title "Nested Task" :exact t) (tags "filetag")))"#),
        )
        .expect("root tag query should execute");
        assert_eq!(heading_ids(root_rows), vec![12]);

        let match_all_rows = execute_sqlite_query(
            &connection,
            &validated(
                r#"(headings (and (title "Nested Task" :exact t) (tags "project" "urgent" :match :all)))"#,
            ),
        )
        .expect("match-all tag query should execute");
        assert_eq!(heading_ids(match_all_rows), vec![12]);

        let correlated_rows =
            execute_sqlite_query(&connection, &validated(r#"(headings (tags "project"))"#))
                .expect("correlated tag query should execute");
        assert_eq!(heading_ids(correlated_rows), vec![11, 12, 14, 15]);
        assert_eq!(
            heading_file_paths(
                execute_sqlite_query(&connection, &validated(r#"(headings (tags "filetag"))"#))
                    .expect("root filetag heading query should execute")
            ),
            vec!["/tmp/query-alpha.org".to_string()]
        );

        let file_tag_rows =
            execute_sqlite_query(&connection, &validated(r#"(files (tags "filetag"))"#))
                .expect("file tag query should execute");
        assert_eq!(
            file_paths(file_tag_rows),
            vec!["/tmp/query-alpha.org".to_string()]
        );

        let file_heading_tag_rows =
            execute_sqlite_query(&connection, &validated(r#"(files (tags "project"))"#))
                .expect("file direct tag query should execute");
        assert_eq!(file_paths(file_heading_tag_rows), Vec::<String>::new());

        let regexp_local_rows = execute_sqlite_query(
            &connection,
            &validated(
                r#"(headings (and (title "Nested Task" :exact t) (tags "urg.*" :inherit nil :regexp t)))"#,
            ),
        )
        .expect("regexp local tag query should execute");
        assert_eq!(heading_ids(regexp_local_rows), vec![12]);

        let regexp_inherited_rows = execute_sqlite_query(
            &connection,
            &validated(
                r#"(headings (and (title "Nested Task" :exact t) (tags "proj.*" :regexp t)))"#,
            ),
        )
        .expect("regexp inherited tag query should execute");
        assert_eq!(heading_ids(regexp_inherited_rows), vec![12]);

        let regexp_root_rows = execute_sqlite_query(
            &connection,
            &validated(
                r#"(headings (and (title "Nested Task" :exact t) (tags "file.*" :regexp t)))"#,
            ),
        )
        .expect("regexp root tag query should execute");
        assert_eq!(heading_ids(regexp_root_rows), vec![12]);

        let regexp_match_all_rows = execute_sqlite_query(
            &connection,
            &validated(
                r#"(headings (and (title "Nested Task" :exact t) (tags "proj.*" "urg.*" :regexp t :match :all)))"#,
            ),
        )
        .expect("regexp match-all tag query should execute");
        assert_eq!(heading_ids(regexp_match_all_rows), vec![12]);
    }

    #[test]
    fn execution_matches_effective_heading_property_queries() {
        let connection = seeded_connection();
        let before_rows = property_rows(&connection, 11, "LANG");

        let local_rows = execute_sqlite_query(
            &connection,
            &validated(
                r#"(headings (and (title "Nested Task" :exact t) (property "OWNER" "Bob" :inherit nil)))"#,
            ),
        )
        .expect("local property query should execute");
        assert_eq!(heading_ids(local_rows), vec![12]);

        let inherited_rows = execute_sqlite_query(
            &connection,
            &validated(
                r#"(headings (and (title "Nested Task" :exact t) (property "AREA" "infra")))"#,
            ),
        )
        .expect("inherited property query should execute");
        assert_eq!(heading_ids(inherited_rows), vec![12]);

        let root_rows = execute_sqlite_query(
            &connection,
            &validated(
                r#"(headings (and (title "Nested Task" :exact t) (property "CATEGORY" "work")))"#,
            ),
        )
        .expect("root property query should execute");
        assert_eq!(heading_ids(root_rows), vec![12]);

        let file_rows = execute_sqlite_query(
            &connection,
            &validated(r#"(files (property "CATEGORY" "work"))"#),
        )
        .expect("file property query should execute");
        assert_eq!(
            file_paths(file_rows),
            vec!["/tmp/query-alpha.org".to_string()]
        );

        let appended_rows = execute_sqlite_query(
            &connection,
            &validated(
                r#"(headings (and (title "Query Engine" :exact t) (property "LANG" "rust emacs" :inherit nil)))"#,
            ),
        )
        .expect("append property query should execute");
        assert_eq!(heading_ids(appended_rows), vec![11]);

        let non_resolved_component_rows = execute_sqlite_query(
            &connection,
            &validated(
                r#"(headings (and (title "Query Engine" :exact t) (property "LANG" "emacs" :inherit nil)))"#,
            ),
        )
        .expect("append property query should execute");
        assert_eq!(heading_ids(non_resolved_component_rows), Vec::<i64>::new());

        let append_before_rows = execute_sqlite_query(
            &connection,
            &validated(
                r#"(headings (and (title "Query Engine" :exact t) (property "APPEND_BEFORE" "definition appending before" :inherit nil)))"#,
            ),
        )
        .expect("append-before property query should execute");
        assert_eq!(heading_ids(append_before_rows), vec![11]);

        let append_before_inherited_rows = execute_sqlite_query(
            &connection,
            &validated(
                r#"(headings (and (title "Query Engine" :exact t) (property "APPEND_BEFORE" "definition appending before")))"#,
            ),
        )
        .expect("append-before inherited property query should execute");
        assert_eq!(heading_ids(append_before_inherited_rows), vec![11]);

        let append_before_base_only_rows = execute_sqlite_query(
            &connection,
            &validated(
                r#"(headings (and (title "Query Engine" :exact t) (property "APPEND_BEFORE" "definition" :inherit nil)))"#,
            ),
        )
        .expect("append-before base-only query should execute");
        assert_eq!(heading_ids(append_before_base_only_rows), Vec::<i64>::new());

        let append_between_duplicate_rows = execute_sqlite_query(
            &connection,
            &validated(
                r#"(headings (and (title "Loose Note" :exact t) (property "APPEND_REPLACED" "second appended" :inherit nil)))"#,
            ),
        )
        .expect("append-between-duplicates property query should execute");
        assert_eq!(heading_ids(append_between_duplicate_rows), vec![13]);

        let stale_base_rows = execute_sqlite_query(
            &connection,
            &validated(
                r#"(headings (and (title "Loose Note" :exact t) (property "APPEND_REPLACED" "first appended" :inherit nil)))"#,
            ),
        )
        .expect("stale base query should execute");
        assert_eq!(heading_ids(stale_base_rows), Vec::<i64>::new());

        let overwrite_rows = execute_sqlite_query(
            &connection,
            &validated(
                r#"(headings (and (title "Loose Note" :exact t) (property "DEFINED_TWICE" "second is effective" :inherit nil)))"#,
            ),
        )
        .expect("overwrite property query should execute");
        assert_eq!(heading_ids(overwrite_rows), vec![13]);

        let overwritten_value_rows = execute_sqlite_query(
            &connection,
            &validated(
                r#"(headings (and (title "Loose Note" :exact t) (property "DEFINED_TWICE" "works" :inherit nil)))"#,
            ),
        )
        .expect("overwritten value query should execute");
        assert_eq!(heading_ids(overwritten_value_rows), Vec::<i64>::new());

        let add_value_rows = execute_sqlite_query(
            &connection,
            &validated(
                r#"(headings (and (title "Statistic Cookies" :exact t) (property "ADD-VALUE" "is valid" :inherit nil)))"#,
            ),
        )
        .expect("add-value property query should execute");
        assert_eq!(heading_ids(add_value_rows), vec![14]);

        let add_value_fragment_rows = execute_sqlite_query(
            &connection,
            &validated(
                r#"(headings (and (title "Statistic Cookies" :exact t) (property "ADD-VALUE" "valid" :inherit nil)))"#,
            ),
        )
        .expect("add-value fragment property query should execute");
        assert_eq!(heading_ids(add_value_fragment_rows), Vec::<i64>::new());

        let multiple_append_positions_rows = execute_sqlite_query(
            &connection,
            &validated(
                r#"(headings (and (title "Statistic Cookies" :exact t) (property "MULTI_APPEND" "second before middle after" :inherit nil)))"#,
            ),
        )
        .expect("multiple-append-positions property query should execute");
        assert_eq!(heading_ids(multiple_append_positions_rows), vec![14]);

        let partial_multiple_append_rows = execute_sqlite_query(
            &connection,
            &validated(
                r#"(headings (and (title "Statistic Cookies" :exact t) (property "MULTI_APPEND" "second after" :inherit nil)))"#,
            ),
        )
        .expect("partial multiple-append query should execute");
        assert_eq!(heading_ids(partial_multiple_append_rows), Vec::<i64>::new());

        let root_append_rows = execute_sqlite_query(
            &connection,
            &validated(r#"(headings (property "KEYWORD_APPEND" "foo=1 bar=2" :inherit nil))"#),
        )
        .expect("root append property query should execute");
        assert_eq!(
            heading_file_paths(root_append_rows),
            vec!["/tmp/query-alpha.org".to_string()]
        );

        let root_overwrite_rows = execute_sqlite_query(
            &connection,
            &validated(
                r#"(headings (property "KEYWORD_OVERWRITTEN_BY_SECOND" "valid" :inherit nil))"#,
            ),
        )
        .expect("root overwrite property query should execute");
        assert_eq!(
            heading_file_paths(root_overwrite_rows),
            vec!["/tmp/query-alpha.org".to_string()]
        );

        let append_inherited_rows = execute_sqlite_query(
            &connection,
            &validated(
                r#"(headings (and (title "Nested Task" :exact t) (property "APPEND_INHERITED" "parent child")))"#,
            ),
        )
        .expect("append inherited property query should execute");
        assert_eq!(heading_ids(append_inherited_rows), vec![12]);

        let local_base_with_appends_direct_rows = execute_sqlite_query(
            &connection,
            &validated(
                r#"(headings (and (title "Nested Task" :exact t) (property "LOCAL_BASE_APPEND" "child before after" :inherit nil)))"#,
            ),
        )
        .expect("local-base-with-appends direct query should execute");
        assert_eq!(heading_ids(local_base_with_appends_direct_rows), vec![12]);

        let local_base_with_appends_inherited_rows = execute_sqlite_query(
            &connection,
            &validated(
                r#"(headings (and (title "Nested Task" :exact t) (property "LOCAL_BASE_APPEND" "child before after")))"#,
            ),
        )
        .expect("local-base-with-appends inherited query should execute");
        assert_eq!(
            heading_ids(local_base_with_appends_inherited_rows),
            vec![12]
        );

        let override_parent_rows = execute_sqlite_query(
            &connection,
            &validated(r#"(headings (property "OVERRIDE_CHAIN" "parent"))"#),
        )
        .expect("override parent property query should execute");
        assert_eq!(heading_ids(override_parent_rows), vec![11, 12, 14]);

        let override_child_rows = execute_sqlite_query(
            &connection,
            &validated(r#"(headings (property "OVERRIDE_CHAIN" "child"))"#),
        )
        .expect("override child property query should execute");
        assert_eq!(heading_ids(override_child_rows), vec![15]);

        let correlated_rows = execute_sqlite_query(
            &connection,
            &validated(r#"(headings (property "AREA" "infra"))"#),
        )
        .expect("correlated property query should execute");
        assert_eq!(heading_ids(correlated_rows), vec![11, 12, 14, 15]);
        assert_eq!(
            heading_file_paths(
                execute_sqlite_query(
                    &connection,
                    &validated(r#"(headings (property "CATEGORY" "work"))"#),
                )
                .expect("root property heading query should execute")
            ),
            vec!["/tmp/query-alpha.org".to_string()]
        );

        let after_rows = property_rows(&connection, 11, "LANG");
        assert_eq!(before_rows, after_rows);
        assert_eq!(
            after_rows,
            vec![
                (Some("rust".to_string()), false),
                (Some("emacs".to_string()), true),
            ]
        );

        let regexp_local_rows = execute_sqlite_query(
            &connection,
            &validated(
                r#"(headings (and (title "Query Engine" :exact t) (property "LANG" "rust em.*" :inherit nil :regexp t)))"#,
            ),
        )
        .expect("regexp local property query should execute");
        assert_eq!(heading_ids(regexp_local_rows), vec![11]);

        let regexp_inherited_rows = execute_sqlite_query(
            &connection,
            &validated(
                r#"(headings (and (title "Nested Task" :exact t) (property "APPEND_INHERITED" "parent child" :regexp t)))"#,
            ),
        )
        .expect("regexp inherited property query should execute");
        assert_eq!(heading_ids(regexp_inherited_rows), vec![12]);

        let regexp_overwrite_rows = execute_sqlite_query(
            &connection,
            &validated(
                r#"(headings (and (title "Loose Note" :exact t) (property "DEFINED_TWICE" "second.*effective" :inherit nil :regexp t)))"#,
            ),
        )
        .expect("regexp overwrite property query should execute");
        assert_eq!(heading_ids(regexp_overwrite_rows), vec![13]);

        let regexp_stale_rows = execute_sqlite_query(
            &connection,
            &validated(
                r#"(headings (and (title "Loose Note" :exact t) (property "DEFINED_TWICE" "works" :inherit nil :regexp t)))"#,
            ),
        )
        .expect("regexp stale property query should execute");
        assert_eq!(heading_ids(regexp_stale_rows), Vec::<i64>::new());
    }

    #[test]
    fn execution_matches_keyword_queries_through_root_context() {
        let connection = seeded_connection();

        let heading_rows = execute_sqlite_query(
            &connection,
            &validated(
                r#"(headings (and (title "Loose Note" :exact t) (keyword "AUTHOR" "Alice")))"#,
            ),
        )
        .expect("heading keyword query should execute");
        assert_eq!(heading_ids(heading_rows), vec![13]);

        let file_rows = execute_sqlite_query(
            &connection,
            &validated(r#"(files (keyword "AUTHOR" "Alice"))"#),
        )
        .expect("file keyword query should execute");
        assert_eq!(
            file_paths(file_rows),
            vec!["/tmp/query-alpha.org".to_string()]
        );

        let regexp_heading_rows = execute_sqlite_query(
            &connection,
            &validated(
                r#"(headings (and (title "Loose Note" :exact t) (keyword "AUTHOR" "A.*" :regexp t)))"#,
            ),
        )
        .expect("regexp heading keyword query should execute");
        assert_eq!(heading_ids(regexp_heading_rows), vec![13]);

        let regexp_file_rows = execute_sqlite_query(
            &connection,
            &validated(r#"(files (keyword "AUTHOR" "A.*" :regexp t))"#),
        )
        .expect("regexp file keyword query should execute");
        assert_eq!(
            file_paths(regexp_file_rows),
            vec!["/tmp/query-alpha.org".to_string()]
        );
    }

    #[test]
    fn execution_keyword_inherit_controls_root_and_local_heading_matching() {
        let connection = seeded_connection();

        let inherited = execute_sqlite_query(
            &connection,
            &validated(r#"(headings (keyword "AUTHOR" "Alice"))"#),
        )
        .expect("inherited keyword query should execute");
        let explicit_inherited = execute_sqlite_query(
            &connection,
            &validated(r#"(headings (keyword "AUTHOR" "Alice" :inherit t))"#),
        )
        .expect("explicit inherited keyword query should execute");
        assert_eq!(inherited, explicit_inherited);
        assert_eq!(heading_ids(inherited), vec![11, 12, 13, 14, 15]);

        for (query, expected_file_ids) in [
            (r#"(headings (keyword "AUTHOR" :inherit nil))"#, vec![2, 1]),
            (
                r#"(headings (keyword "AUTHOR" "Alice" :inherit nil))"#,
                vec![2],
            ),
            (
                r#"(headings (keyword "AUTHOR" "A.*" :inherit nil :regexp t))"#,
                vec![2],
            ),
        ] {
            let rows = execute_sqlite_query(&connection, &validated(query))
                .expect("local keyword query should execute");
            let QueryRows::Headings(rows) = rows else {
                panic!("heading query should return heading rows");
            };
            let file_ids = rows
                .into_iter()
                .map(|row| match row {
                    HeadingQueryMatch::File(file) => file.id,
                    HeadingQueryMatch::Heading(_) => {
                        panic!("local-only query must not match headings")
                    }
                })
                .collect::<Vec<_>>();
            assert_eq!(file_ids, expected_file_ids);
        }

        for query in [
            r#"(headings (keyword "AUTHOR" "missing"))"#,
            r#"(headings (keyword "AUTHOR" "missing" :inherit nil))"#,
        ] {
            let rows = execute_sqlite_query(&connection, &validated(query))
                .expect("non-matching keyword query should execute");
            assert!(matches!(rows, QueryRows::Headings(rows) if rows.is_empty()));
        }
    }

    #[test]
    fn execution_title_queries_match_normalized_heading_titles() {
        let connection = seeded_connection();

        let normalized_rows = execute_sqlite_query(
            &connection,
            &validated(r#"(headings (title "Statistic Cookies" :exact t))"#),
        )
        .expect("normalized title query should execute");
        match normalized_rows {
            QueryRows::Headings(rows) => {
                assert_eq!(rows.len(), 1);
                let HeadingQueryMatch::Heading(row) = &rows[0] else {
                    panic!("expected heading row");
                };
                assert_eq!(row.id, 14);
                assert_eq!(row.title, "Statistic Cookies");
                assert_eq!(
                    row.title_raw.as_deref(),
                    Some("REVIEW [#B] Statistic Cookies [0/1]")
                );
            }
            other => panic!("unexpected rows for normalized title query: {other:?}"),
        }

        let raw_rows = execute_sqlite_query(
            &connection,
            &validated(r#"(headings (title "REVIEW [#B] Statistic Cookies [0/1]" :exact t))"#),
        )
        .expect("raw title query should execute");
        match raw_rows {
            QueryRows::Headings(rows) => assert!(rows.is_empty()),
            other => panic!("unexpected rows for raw title query: {other:?}"),
        }

        let regexp_rows = execute_sqlite_query(
            &connection,
            &validated(r#"(headings (title "Query.*" :regexp t))"#),
        )
        .expect("regexp title query should execute");
        assert_eq!(heading_ids(regexp_rows), vec![11]);
    }

    #[test]
    fn execution_title_queries_can_match_root_files_without_propagating_to_headings() {
        let connection = seeded_connection();

        let default_rows = execute_sqlite_query(
            &connection,
            &validated(r#"(headings (title "Alpha Index" :exact t))"#),
        )
        .expect("default root title query should execute");
        match default_rows {
            QueryRows::Headings(rows) => {
                assert_eq!(rows.len(), 1);
                assert!(matches!(rows[0], HeadingQueryMatch::File(_)));
            }
            other => panic!("unexpected default title rows: {other:?}"),
        }

        let file_title_rows = execute_sqlite_query(
            &connection,
            &validated(r#"(headings (file-title "Alpha Index" :exact t))"#),
        )
        .expect("file-title query should execute");
        assert_eq!(heading_ids(file_title_rows), vec![11, 12, 13, 14, 15]);
    }

    #[test]
    fn execution_heading_queries_return_matching_root_rows_across_predicates() {
        let connection = seeded_connection();

        assert_eq!(
            heading_file_paths(
                execute_sqlite_query(
                    &connection,
                    &validated(r#"(headings (keyword "AUTHOR" "Alice"))"#),
                )
                .expect("keyword heading query should execute")
            ),
            vec!["/tmp/query-alpha.org".to_string()]
        );

        assert_eq!(
            heading_file_paths(
                execute_sqlite_query(&connection, &validated(r#"(headings (level 0))"#))
                    .expect("level heading query should execute")
            ),
            vec![
                "/tmp/query-alpha.org".to_string(),
                "/tmp/query-beta.org".to_string(),
                "/tmp/query-gamma.org".to_string(),
            ]
        );
    }

    #[test]
    fn execution_children_and_descendants_can_match_root_rows() {
        let connection = seeded_connection();

        assert_eq!(
            heading_file_paths(
                execute_sqlite_query(&connection, &validated(r#"(headings (children))"#))
                    .expect("children heading query should execute")
            ),
            vec![
                "/tmp/query-alpha.org".to_string(),
                "/tmp/query-beta.org".to_string(),
                "/tmp/query-gamma.org".to_string(),
            ]
        );

        assert_eq!(
            heading_file_paths(
                execute_sqlite_query(&connection, &validated(r#"(headings (descendants))"#))
                    .expect("descendants heading query should execute")
            ),
            vec![
                "/tmp/query-alpha.org".to_string(),
                "/tmp/query-beta.org".to_string(),
                "/tmp/query-gamma.org".to_string(),
            ]
        );

        let parent_rows = execute_sqlite_query(&connection, &validated(r#"(headings (parent))"#))
            .expect("parent heading query should execute");
        assert!(heading_file_paths(parent_rows).is_empty());
    }

    #[test]
    fn execution_file_predicates_return_matching_roots_in_heading_queries() {
        let connection = seeded_connection();

        let file_path_rows = execute_sqlite_query(
            &connection,
            &validated(r#"(headings (file-path "/tmp/query-alpha.org" :exact t))"#),
        )
        .expect("file-path heading query should execute");
        assert_eq!(
            heading_file_paths(file_path_rows),
            vec!["/tmp/query-alpha.org".to_string()]
        );

        let file_name_rows = execute_sqlite_query(
            &connection,
            &validated(r#"(headings (file-name "query-alpha.org" :exact t))"#),
        )
        .expect("file-name heading query should execute");
        assert_eq!(
            heading_file_paths(file_name_rows),
            vec!["/tmp/query-alpha.org".to_string()]
        );

        let file_dir_rows = execute_sqlite_query(
            &connection,
            &validated(r#"(headings (file-dir "/tmp" :exact t))"#),
        )
        .expect("file-dir heading query should execute");
        assert_eq!(
            heading_file_paths(file_dir_rows),
            vec![
                "/tmp/query-alpha.org".to_string(),
                "/tmp/query-beta.org".to_string(),
                "/tmp/query-gamma.org".to_string(),
            ]
        );

        let file_title_rows = execute_sqlite_query(
            &connection,
            &validated(r#"(headings (file-title "Alpha Index" :exact t))"#),
        )
        .expect("file-title heading query should execute");
        assert_eq!(
            heading_file_paths(file_title_rows),
            vec!["/tmp/query-alpha.org".to_string()]
        );

        let file_modified_rows = execute_sqlite_query(
            &connection,
            &validated(r#"(headings (file-modified :to "2026-01-03"))"#),
        )
        .expect("file-modified heading query should execute");
        assert_eq!(
            heading_file_paths(file_modified_rows),
            vec!["/tmp/query-alpha.org".to_string()]
        );

        let regexp_file_path_rows = execute_sqlite_query(
            &connection,
            &validated(r#"(headings (file-path ".*/query-alpha\\.org" :regexp t))"#),
        )
        .expect("regexp file-path heading query should execute");
        assert_eq!(
            heading_file_paths(regexp_file_path_rows),
            vec!["/tmp/query-alpha.org".to_string()]
        );

        let regexp_file_name_rows = execute_sqlite_query(
            &connection,
            &validated(r#"(headings (file-name "query-(alpha|beta)\\.org" :regexp t))"#),
        )
        .expect("regexp file-name heading query should execute");
        assert_eq!(
            heading_file_paths(regexp_file_name_rows),
            vec![
                "/tmp/query-alpha.org".to_string(),
                "/tmp/query-beta.org".to_string(),
            ]
        );

        let regexp_file_title_rows = execute_sqlite_query(
            &connection,
            &validated(r#"(headings (file-title "Alpha.*" :regexp t))"#),
        )
        .expect("regexp file-title heading query should execute");
        assert_eq!(
            heading_file_paths(regexp_file_title_rows),
            vec!["/tmp/query-alpha.org".to_string()]
        );
    }

    #[test]
    fn execution_file_predicate_roots_respect_boolean_composition() {
        let connection = seeded_connection();

        let and_rows = execute_sqlite_query(
            &connection,
            &validated(
                r#"(headings
                    (and
                      (file-path "/tmp/query-alpha.org" :exact t)
                      (todo "NEXT")))"#,
            ),
        )
        .expect("and heading query should execute");
        assert_eq!(heading_ids(and_rows), vec![11]);

        let or_rows = execute_sqlite_query(
            &connection,
            &validated(
                r#"(headings
                    (or
                      (file-path "/tmp/query-alpha.org" :exact t)
                      (title "Beta Index" :exact t)))"#,
            ),
        )
        .expect("or heading query should execute");
        assert_eq!(
            heading_file_paths(or_rows),
            vec![
                "/tmp/query-alpha.org".to_string(),
                "/tmp/query-beta.org".to_string(),
            ]
        );
    }

    #[test]
    fn execution_file_queries_support_file_name_and_file_dir() {
        let connection = seeded_connection();

        let file_name_rows = execute_sqlite_query(
            &connection,
            &validated(r#"(files (file-name "query-alpha.org" :exact t))"#),
        )
        .expect("file-name files query should execute");
        assert_eq!(
            file_paths(file_name_rows),
            vec!["/tmp/query-alpha.org".to_string()]
        );

        let file_dir_rows = execute_sqlite_query(
            &connection,
            &validated(r#"(files (file-dir "/tmp" :exact t))"#),
        )
        .expect("file-dir files query should execute");
        assert_eq!(
            file_paths(file_dir_rows),
            vec![
                "/tmp/query-alpha.org".to_string(),
                "/tmp/query-beta.org".to_string(),
                "/tmp/query-gamma.org".to_string(),
            ]
        );

        let regexp_file_rows = execute_sqlite_query(
            &connection,
            &validated(r#"(files (file-path ".*/query-(alpha|beta)\\.org" :regexp t))"#),
        )
        .expect("regexp file query should execute");
        assert_eq!(
            file_paths(regexp_file_rows),
            vec![
                "/tmp/query-alpha.org".to_string(),
                "/tmp/query-beta.org".to_string(),
            ]
        );
    }

    #[test]
    fn bare_headings_query_returns_file_roots_and_real_headings() {
        let connection = seeded_connection();

        let rows = execute_sqlite_query(&connection, &validated(r#"(headings)"#))
            .expect("bare headings query should execute");
        let QueryRows::Headings(rows) = rows else {
            panic!("expected heading rows");
        };

        let expected_file_count: usize = connection
            .query_row("SELECT COUNT(*) FROM files", [], |row| row.get(0))
            .expect("file count should load");
        let expected_heading_count: usize = connection
            .query_row("SELECT COUNT(*) FROM headings WHERE level > 0", [], |row| {
                row.get(0)
            })
            .expect("real heading count should load");

        let file_rows = rows
            .iter()
            .filter_map(|row| match row {
                HeadingQueryMatch::File(row) => Some(row),
                HeadingQueryMatch::Heading(_) => None,
            })
            .collect::<Vec<_>>();
        let heading_rows = rows
            .iter()
            .filter_map(|row| match row {
                HeadingQueryMatch::Heading(row) => Some(row),
                HeadingQueryMatch::File(_) => None,
            })
            .collect::<Vec<_>>();

        assert_eq!(file_rows.len(), expected_file_count);
        assert_eq!(heading_rows.len(), expected_heading_count);
        assert!(heading_rows.iter().all(|row| row.level > 0));

        let mut unique_file_ids = file_rows.iter().map(|row| row.id).collect::<Vec<_>>();
        unique_file_ids.sort_unstable();
        unique_file_ids.dedup();
        assert_eq!(unique_file_ids.len(), expected_file_count);

        assert!(
            matches!(rows.first(), Some(HeadingQueryMatch::File(row)) if row.path == "/tmp/query-alpha.org")
        );
        assert!(
            matches!(rows.get(1), Some(HeadingQueryMatch::Heading(row)) if row.file_path == "/tmp/query-alpha.org" && row.id == 11)
        );

        let bare_heading_ids = heading_rows.iter().map(|row| row.id).collect::<Vec<_>>();
        let filtered_heading_ids = heading_ids(
            execute_sqlite_query(&connection, &validated(r#"(headings (title "Property"))"#))
                .expect("filtered headings query should execute"),
        );
        assert!(filtered_heading_ids
            .iter()
            .all(|id| bare_heading_ids.contains(id)));
    }

    #[test]
    fn compile_has_text_uses_correlated_exists_with_bound_params() {
        let compiled = compile_sqlite_query(&validated(
            r#"(headings (has-text "sqlite" "fts" "x' OR 1=1 --"))"#,
        ))
        .expect("query should compile");

        assert_eq!(compiled.target, QueryTarget::Headings);
        assert_eq!(compiled.params.len(), 3);
        assert_eq!(
            compiled.params,
            vec![
                super::QueryParam::Text("sqlite".to_string()),
                super::QueryParam::Text("fts".to_string()),
                super::QueryParam::Text("x' OR 1=1 --".to_string()),
            ]
        );
        assert!(compiled.sql.contains("FROM heading_bodies"));
        assert!(compiled.sql.contains("heading_bodies.heading_id = h0.id"));
        assert!(compiled.sql.matches("EXISTS (").count() >= 3);
        assert!(!compiled.sql.contains("x' OR 1=1 --"));
        assert!(!compiled.sql.contains("1=1 --"));
    }

    #[test]
    fn execution_matches_has_text_against_persisted_heading_bodies() {
        let connection = seeded_connection();

        let single_term_rows =
            execute_sqlite_query(&connection, &validated(r#"(headings (has-text "sqlite"))"#))
                .expect("single-term has-text query should execute");
        assert_eq!(heading_ids(single_term_rows), vec![11, 12, 21]);

        let and_rows = execute_sqlite_query(
            &connection,
            &validated(r#"(headings (has-text "sqlite" "fts"))"#),
        )
        .expect("multi-term has-text query should execute");
        assert_eq!(heading_ids(and_rows), vec![11, 21]);

        let case_rows = execute_sqlite_query(
            &connection,
            &validated(r#"(headings (has-text "SQLITE" "FTS"))"#),
        )
        .expect("case-insensitive has-text query should execute");
        assert_eq!(heading_ids(case_rows), vec![11, 21]);

        let no_match_rows = execute_sqlite_query(
            &connection,
            &validated(r#"(headings (has-text "missing phrase"))"#),
        )
        .expect("no-match has-text query should execute");
        assert_eq!(heading_ids(no_match_rows), Vec::<i64>::new());

        let regexp_rows = execute_sqlite_query(
            &connection,
            &validated(r#"(headings (has-text "(?i)sqlite.*fts" :regexp t))"#),
        )
        .expect("regexp has-text query should execute");
        assert_eq!(heading_ids(regexp_rows), vec![11, 21]);

        let regexp_no_match_rows = execute_sqlite_query(
            &connection,
            &validated(r#"(headings (has-text "(?i)fts.*sqlite" :regexp t))"#),
        )
        .expect("regexp no-match has-text query should execute");
        assert_eq!(heading_ids(regexp_no_match_rows), Vec::<i64>::new());
    }

    #[test]
    fn execution_has_text_excludes_empty_or_missing_body_rows_without_mutation() {
        let connection = seeded_connection();
        let body_count_before: i64 = connection
            .query_row("SELECT COUNT(*) FROM heading_bodies", [], |row| row.get(0))
            .expect("body count should load");

        let empty_body_rows = execute_sqlite_query(
            &connection,
            &validated(
                r#"(headings (and (title "Statistic Cookies" :exact t) (has-text "sqlite")))"#,
            ),
        )
        .expect("empty-body query should execute");
        assert_eq!(heading_ids(empty_body_rows), Vec::<i64>::new());

        let missing_body_rows = execute_sqlite_query(
            &connection,
            &validated(
                r#"(headings (and (title "Gamma Candidate" :exact t) (has-text "sqlite")))"#,
            ),
        )
        .expect("missing-body query should execute");
        assert_eq!(heading_ids(missing_body_rows), Vec::<i64>::new());

        let body_count_after: i64 = connection
            .query_row("SELECT COUNT(*) FROM heading_bodies", [], |row| row.get(0))
            .expect("body count should reload");
        assert_eq!(body_count_before, body_count_after);
    }

    #[test]
    fn injection_like_strings_remain_bound_and_do_not_broaden_results() {
        let connection = seeded_connection();

        for query in [
            validated(r#"(headings (title "x' OR 1=1 --"))"#),
            validated(r#"(headings (has-text "x' OR 1=1 --"))"#),
            validated(r#"(headings (title "x' OR 1=1 --" :regexp t))"#),
            validated(r#"(headings (has-text "x' OR 1=1 --" :regexp t))"#),
            validated(r#"(headings (tags "x' OR 1=1 --" :inherit nil))"#),
            validated(r#"(headings (tags "x' OR 1=1 --" :inherit nil :regexp t))"#),
            validated(r#"(headings (property "OWNER" "x' OR 1=1 --"))"#),
            validated(r#"(headings (property "OWNER" "x' OR 1=1 --" :regexp t))"#),
            validated(r#"(files (keyword "AUTHOR" "x' OR 1=1 --"))"#),
            validated(r#"(files (keyword "AUTHOR" "x' OR 1=1 --" :regexp t))"#),
            validated(r#"(links (link-target "x' OR 1=1 --" :regexp t))"#),
            validated(r#"(headings (links-to (headings (title "x' OR 1=1 --"))))"#),
        ] {
            let compiled = compile_sqlite_query(&query).expect("query should compile");
            assert!(!compiled.sql.contains("1=1"));
            assert!(!compiled.params.is_empty());

            match execute_sqlite_query(&connection, &query).expect("query should execute") {
                QueryRows::Headings(rows) => assert!(rows.is_empty()),
                QueryRows::Files(rows) => assert!(rows.is_empty()),
                QueryRows::Links(rows) => assert!(rows.is_empty()),
            }
        }
    }

    #[test]
    fn execution_matches_link_and_file_regex_metadata_predicates() {
        let connection = seeded_connection();

        let link_target_rows = execute_sqlite_query(
            &connection,
            &validated(r#"(links (link-target "file:beta\\.org.*" :regexp t))"#),
        )
        .expect("regexp link-target query should execute");
        assert_eq!(link_ids(link_target_rows), vec![102, 100, 101]);

        let link_description_rows = execute_sqlite_query(
            &connection,
            &validated(r#"(links (link-description "Beta.*" :regexp t))"#),
        )
        .expect("regexp link-description query should execute");
        assert_eq!(link_ids(link_description_rows), vec![100, 101]);
    }

    #[test]
    fn execution_matches_persisted_scheduled_and_ts_active_predicates() {
        let connection = seeded_connection();

        let scheduled_rows = execute_sqlite_query(
            &connection,
            &validated(r#"(headings (scheduled :on "2026-01-03"))"#),
        )
        .expect("scheduled query should execute");
        match scheduled_rows {
            QueryRows::Headings(rows) => {
                assert_eq!(rows.len(), 1);
                let HeadingQueryMatch::Heading(row) = &rows[0] else {
                    panic!("expected heading row");
                };
                assert_eq!(row.id, 11);
                assert_eq!(row.scheduled_ts, Some(1_767_398_400));
            }
            other => panic!("unexpected scheduled rows: {other:?}"),
        }

        let ts_active_rows = execute_sqlite_query(
            &connection,
            &validated(r#"(headings (ts-active :on "2026-01-03"))"#),
        )
        .expect("ts-active query should execute");
        match ts_active_rows {
            QueryRows::Headings(rows) => {
                assert_eq!(rows.len(), 1);
                let HeadingQueryMatch::Heading(row) = &rows[0] else {
                    panic!("expected heading row");
                };
                assert_eq!(row.id, 11);
                assert_eq!(row.title, "Query Engine");
            }
            other => panic!("unexpected ts-active rows: {other:?}"),
        }
    }

    #[test]
    fn execution_temporal_predicates_match_date_only_and_timed_rows() {
        let connection = temporal_test_connection();

        assert_eq!(
            heading_ids(
                execute_sqlite_query(
                    &connection,
                    &validated(r#"(headings (scheduled :on "2026-01-03"))"#),
                )
                .expect("scheduled date query should execute")
            ),
            vec![201, 202, 203, 204]
        );
        assert_eq!(
            heading_ids(
                execute_sqlite_query(&connection, &validated(r#"(headings (scheduled))"#),)
                    .expect("scheduled query should execute")
            ),
            vec![201, 202, 203, 204]
        );

        assert_eq!(
            heading_ids(
                execute_sqlite_query(&connection, &validated(r#"(headings (deadline))"#),)
                    .expect("deadline query should execute")
            ),
            vec![205, 206, 207, 208]
        );

        assert_eq!(
            heading_ids(
                execute_sqlite_query(&connection, &validated(r#"(headings (closed))"#),)
                    .expect("closed query should execute")
            ),
            vec![209, 210, 211, 212]
        );

        assert_eq!(
            heading_ids(
                execute_sqlite_query(&connection, &validated(r#"(headings (planning))"#),)
                    .expect("planning query should execute")
            ),
            vec![201, 202, 203, 204, 205, 206, 207, 208, 209, 210, 211, 212]
        );
    }

    #[test]
    fn execution_generic_timestamp_predicates_match_date_only_and_timed_rows() {
        let connection = temporal_test_connection();

        assert_eq!(
            heading_ids(
                execute_sqlite_query(&connection, &validated(r#"(headings (ts))"#))
                    .expect("ts query should execute")
            ),
            vec![213, 214, 215, 216, 217, 218, 219, 220]
        );
        assert_eq!(
            heading_ids(
                execute_sqlite_query(&connection, &validated(r#"(headings (ts-active))"#),)
                    .expect("ts-active query should execute")
            ),
            vec![213, 214, 215, 216]
        );
        assert_eq!(
            heading_ids(
                execute_sqlite_query(&connection, &validated(r#"(headings (ts-inactive))"#),)
                    .expect("ts-inactive query should execute")
            ),
            vec![217, 218, 219, 220]
        );
    }

    #[test]
    fn execution_date_only_to_includes_full_day_and_excludes_following_day() {
        let connection = seeded_connection();

        let file_rows = execute_sqlite_query(
            &connection,
            &validated(r#"(files (file-modified :to "2026-01-03"))"#),
        )
        .expect("file query should execute");

        assert_eq!(
            file_paths(file_rows),
            vec!["/tmp/query-alpha.org".to_string()]
        );
    }

    #[test]
    fn execution_date_only_from_uses_inclusive_start_of_day_boundary() {
        let connection = date_bound_test_connection();

        let rows = execute_sqlite_query(
            &connection,
            &validated(r#"(headings (scheduled :from "2026-01-03"))"#),
        )
        .expect("date-only :from query should execute");

        assert_eq!(
            heading_ids(rows),
            vec![100, 101, 102, 103, 106, 107, 104, 105]
        );
    }

    #[test]
    fn compile_distinguishes_date_only_and_datetime_bounds() {
        let date_only = compile_sqlite_query(&temporal_resolved(
            r#"(headings (scheduled :from "2026-01-03" :to "2026-01-03"))"#,
        ))
        .expect("date-only query should compile");
        let datetime = compile_sqlite_query(&temporal_resolved(
            r#"(headings (scheduled :from "2026-01-03 09:15" :to "2026-01-03 09:15"))"#,
        ))
        .expect("datetime query should compile");

        assert_eq!(
            date_only.params,
            vec![
                super::QueryParam::Integer(1_767_398_400),
                super::QueryParam::Integer(1_767_484_800),
            ]
        );
        assert!(!date_only.sql.contains("unixepoch"));
        assert!(!date_only.sql.contains("localtime"));

        assert_eq!(
            datetime.params,
            vec![
                super::QueryParam::Integer(1_767_431_700),
                super::QueryParam::Integer(1_767_431_760),
            ]
        );
        assert!(!datetime.sql.contains("unixepoch"));
        assert!(!datetime.sql.contains("localtime"));
    }

    #[test]
    fn execution_datetime_bounds_preserve_hour_and_minute_without_timezone_conversion() {
        let connection = date_bound_test_connection();

        let on_rows = execute_sqlite_query(
            &connection,
            &validated(r#"(headings (scheduled :on "2026-01-03 09:15"))"#),
        )
        .expect("datetime :on query should execute");
        assert_eq!(heading_ids(on_rows), vec![101]);

        let from_rows = execute_sqlite_query(
            &connection,
            &validated(r#"(headings (scheduled :from "2026-01-03 09:15"))"#),
        )
        .expect("datetime :from query should execute");
        assert_eq!(
            heading_ids(from_rows),
            vec![101, 102, 103, 106, 107, 104, 105]
        );

        let to_rows = execute_sqlite_query(
            &connection,
            &validated(r#"(headings (scheduled :to "2026-01-03 09:15"))"#),
        )
        .expect("datetime :to query should execute");
        assert_eq!(heading_ids(to_rows), vec![100, 101]);
    }

    #[test]
    fn execution_datetime_bounds_cover_complete_minutes_and_seconds() {
        let connection = date_bound_test_connection();
        connection
            .execute(
                "UPDATE headings SET scheduled_ts = ?1 WHERE id = 101",
                [naive_date_time_seconds(2026, 1, 3, 9, 15) + 45],
            )
            .expect("scheduled timestamp should update");

        let minute_rows = execute_sqlite_query(
            &connection,
            &validated(r#"(headings (scheduled :on "2026-01-03 09:15"))"#),
        )
        .expect("minute query should execute");
        assert_eq!(heading_ids(minute_rows), vec![101]);

        let second_rows = execute_sqlite_query(
            &connection,
            &validated(r#"(headings (scheduled :on "2026-01-03 09:15:45"))"#),
        )
        .expect("second query should execute");
        assert_eq!(heading_ids(second_rows), vec![101]);

        let next_second_rows = execute_sqlite_query(
            &connection,
            &validated(r#"(headings (scheduled :on "2026-01-03 09:15:46"))"#),
        )
        .expect("next-second query should execute");
        assert!(heading_ids(next_second_rows).is_empty());

        for value in ["2026-01-03 09:15", "2026-01-03 09:15:45"] {
            let from_rows = execute_sqlite_query(
                &connection,
                &validated(&format!(r#"(headings (scheduled :from "{value}"))"#)),
            )
            .expect("from query should execute");
            assert!(heading_ids(from_rows).contains(&101));

            let to_rows = execute_sqlite_query(
                &connection,
                &validated(&format!(r#"(headings (scheduled :to "{value}"))"#)),
            )
            .expect("to query should execute");
            assert_eq!(heading_ids(to_rows), vec![100, 101]);

            let equal_range_rows = execute_sqlite_query(
                &connection,
                &validated(&format!(
                    r#"(headings (scheduled :from "{value}" :to "{value}"))"#
                )),
            )
            .expect("equal range query should execute");
            assert_eq!(heading_ids(equal_range_rows), vec![101]);
        }
    }

    #[test]
    fn execution_file_modified_datetime_bounds_include_nanoseconds_before_the_next_interval() {
        let connection = date_bound_test_connection();
        let minute_start = naive_date_time_seconds(2026, 1, 3, 9, 15) * 1_000_000_000;
        connection
            .execute(
                "UPDATE files SET mtime_ns = ?1",
                [minute_start + 59_999_999_999],
            )
            .expect("file mtime should update");

        let execution_options = QueryExecutionOptions {
            query_timezone: Some("UTC".to_string()),
            ..QueryExecutionOptions::default()
        };

        let minute_rows = execute_sqlite_query_with_options(
            &connection,
            &validated(r#"(files (file-modified :on "2026-01-03 09:15"))"#),
            &execution_options,
        )
        .expect("minute query should execute");
        assert_eq!(
            file_paths(minute_rows),
            vec!["/tmp/date-bounds.org".to_string()]
        );

        let second_rows = execute_sqlite_query_with_options(
            &connection,
            &validated(r#"(files (file-modified :on "2026-01-03 09:15:59"))"#),
            &execution_options,
        )
        .expect("second query should execute");
        assert_eq!(
            file_paths(second_rows),
            vec!["/tmp/date-bounds.org".to_string()]
        );

        let next_second_rows = execute_sqlite_query_with_options(
            &connection,
            &validated(r#"(files (file-modified :on "2026-01-03 09:16:00"))"#),
            &execution_options,
        )
        .expect("next-second query should execute");
        assert!(file_paths(next_second_rows).is_empty());
    }

    #[test]
    fn execution_distinguishes_date_only_and_explicit_midnight_datetime_bounds() {
        let connection = date_bound_test_connection();

        let day_rows = execute_sqlite_query(
            &connection,
            &validated(r#"(headings (scheduled :on "2026-01-03"))"#),
        )
        .expect("date-only query should execute");
        assert_eq!(heading_ids(day_rows), vec![100, 101, 102]);

        let midnight_rows = execute_sqlite_query(
            &connection,
            &validated(r#"(headings (scheduled :on "2026-01-03 00:00"))"#),
        )
        .expect("midnight datetime query should execute");
        assert_eq!(heading_ids(midnight_rows), vec![100]);

        let midnight_to_rows = execute_sqlite_query(
            &connection,
            &validated(r#"(headings (scheduled :to "2026-01-03 00:00"))"#),
        )
        .expect("midnight datetime :to query should execute");
        assert_eq!(heading_ids(midnight_to_rows), vec![100]);
    }

    #[test]
    fn execution_date_only_next_day_boundaries_cover_month_end_year_end_and_dst_dates() {
        let connection = date_bound_test_connection();

        let month_end_rows = execute_sqlite_query(
            &connection,
            &validated(r#"(headings (scheduled :to "2026-01-31"))"#),
        )
        .expect("month-end query should execute");
        assert_eq!(heading_ids(month_end_rows), vec![100, 101, 102, 106]);

        let february_first_rows = execute_sqlite_query(
            &connection,
            &validated(r#"(headings (scheduled :to "2026-02-01"))"#),
        )
        .expect("february-first query should execute");
        assert_eq!(
            heading_ids(february_first_rows),
            vec![100, 101, 102, 106, 107]
        );

        let year_end_rows = execute_sqlite_query(
            &connection,
            &validated(r#"(headings (scheduled :to "2026-12-31"))"#),
        )
        .expect("year-end query should execute");
        assert_eq!(
            heading_ids(year_end_rows),
            vec![100, 101, 102, 103, 106, 107, 104]
        );

        let dst_day_rows = execute_sqlite_query(
            &connection,
            &validated(r#"(headings (scheduled :on "2026-03-29"))"#),
        )
        .expect("dst date-only query should execute");
        assert_eq!(heading_ids(dst_day_rows), vec![103]);

        let dst_datetime_rows = execute_sqlite_query(
            &connection,
            &validated(r#"(headings (scheduled :on "2026-03-29 02:30"))"#),
        )
        .expect("dst datetime query should execute");
        assert_eq!(heading_ids(dst_datetime_rows), vec![103]);
    }

    #[test]
    fn execution_resolves_relative_dates_before_sql_with_bound_parameters() {
        let query = validated(r#"(headings (scheduled :from today :to 1))"#);
        let resolved = resolve_relative_dates(
            &query,
            &QueryDateResolutionOptions {
                timezone: Some("UTC".to_string()),
                now_utc: Some(
                    "2026-01-03T12:00:00Z"
                        .parse()
                        .expect("timestamp should parse"),
                ),
            },
        )
        .expect("query should resolve");
        let resolved = resolve_temporal_bounds(
            &resolved,
            &QueryDateResolutionOptions {
                timezone: Some("UTC".to_string()),
                now_utc: None,
            },
        )
        .expect("temporal bounds should resolve");
        let compiled = compile_sqlite_query(&resolved).expect("query should compile");

        assert!(!compiled.sql.contains("localtime"));
        assert_eq!(
            compiled.params,
            vec![
                super::QueryParam::Integer(1_767_398_400),
                super::QueryParam::Integer(1_767_571_200),
            ]
        );

        let connection = seeded_connection();
        let rows = execute_sqlite_query_with_options(
            &connection,
            &query,
            &QueryExecutionOptions {
                now_utc: Some(
                    "2026-01-03T12:00:00Z"
                        .parse()
                        .expect("timestamp should parse"),
                ),
                query_timezone: Some("UTC".to_string()),
                ..QueryExecutionOptions::default()
            },
        )
        .expect("query should execute");

        match rows {
            QueryRows::Headings(rows) => {
                assert_eq!(rows.len(), 1);
                let HeadingQueryMatch::Heading(row) = &rows[0] else {
                    panic!("expected heading row");
                };
                assert_eq!(row.id, 11);
                assert_eq!(row.scheduled_ts, Some(1_767_398_400));
            }
            other => panic!("unexpected scheduled rows: {other:?}"),
        }
    }

    #[test]
    fn execution_uses_effective_timezone_for_relative_dates() {
        let connection = seeded_connection();
        let query = validated(r#"(headings (scheduled :on today))"#);

        let utc_rows = execute_sqlite_query_with_options(
            &connection,
            &query,
            &QueryExecutionOptions {
                now_utc: Some(
                    "2026-01-03T23:30:00Z"
                        .parse()
                        .expect("timestamp should parse"),
                ),
                query_timezone: Some("UTC".to_string()),
                ..QueryExecutionOptions::default()
            },
        )
        .expect("utc query should execute");
        let zurich_rows = execute_sqlite_query_with_options(
            &connection,
            &query,
            &QueryExecutionOptions {
                now_utc: Some(
                    "2026-01-03T23:30:00Z"
                        .parse()
                        .expect("timestamp should parse"),
                ),
                query_timezone: Some("Europe/Zurich".to_string()),
                ..QueryExecutionOptions::default()
            },
        )
        .expect("zurich query should execute");

        assert_eq!(heading_ids(utc_rows), vec![11]);
        assert_eq!(heading_ids(zurich_rows), Vec::<i64>::new());
    }

    #[test]
    fn execution_returns_date_resolution_error_for_invalid_timezone() {
        let connection = seeded_connection();
        let query = validated(r#"(headings (scheduled :on today))"#);

        let error = execute_sqlite_query_with_options(
            &connection,
            &query,
            &QueryExecutionOptions {
                now_utc: Some(
                    "2026-01-03T23:30:00Z"
                        .parse()
                        .expect("timestamp should parse"),
                ),
                query_timezone: Some("Mars/Olympus".to_string()),
                ..QueryExecutionOptions::default()
            },
        )
        .expect_err("query should fail");

        assert_eq!(error.kind, QueryExecutionErrorKind::DateResolution);
        assert!(error.to_string().contains("invalid query timezone"));
    }

    #[test]
    fn execution_returns_date_resolution_error_for_out_of_range_offset() {
        let connection = seeded_connection();
        let query = validated(r#"(headings (scheduled :to 9223372036854775807))"#);

        let error = execute_sqlite_query_with_options(
            &connection,
            &query,
            &QueryExecutionOptions {
                now_utc: Some(
                    "2026-01-03T23:30:00Z"
                        .parse()
                        .expect("timestamp should parse"),
                ),
                query_timezone: Some("UTC".to_string()),
                ..QueryExecutionOptions::default()
            },
        )
        .expect_err("query should fail");

        assert_eq!(error.kind, QueryExecutionErrorKind::DateResolution);
        assert!(error.to_string().contains("out of range"));
    }

    #[test]
    fn execution_reads_persisted_db_only_and_does_not_mutate_database() {
        let test_dir = TestDir::new("persisted");
        let db_path = test_dir.path().join("query.sqlite");
        let org_path = test_dir.path().join("notes.org");
        fs::write(&org_path, "* changed after indexing\n").expect("org file should write");

        {
            let mut connection = open_database(&db_path).expect("database should open");
            seed_database(
                &mut connection,
                &org_path,
                &test_dir.path().join("beta.org"),
            );
        }

        fs::write(&org_path, "* completely different content\n").expect("org file should rewrite");

        let connection = open_database(&db_path).expect("database should reopen");
        let before_counts = table_counts(&connection);

        let rows = execute_sqlite_query(
            &connection,
            &validated(r#"(headings (links-to (files (file-title "Beta Index" :exact t))))"#),
        )
        .expect("query should execute from stored DB rows");

        match rows {
            QueryRows::Headings(rows) => {
                assert_eq!(rows.len(), 3);
                assert!(matches!(rows[0], HeadingQueryMatch::File(_)));
                let heading_titles = rows
                    .iter()
                    .filter_map(|row| match row {
                        HeadingQueryMatch::Heading(row) => Some(row.title.as_str()),
                        HeadingQueryMatch::File(_) => None,
                    })
                    .collect::<Vec<_>>();
                assert_eq!(heading_titles, vec!["Query Engine", "Nested Task"]);
            }
            other => panic!("unexpected query rows: {other:?}"),
        }

        let after_counts = table_counts(&connection);
        assert_eq!(before_counts, after_counts);
    }

    fn heading_ids(rows: QueryRows) -> Vec<i64> {
        match rows {
            QueryRows::Headings(rows) => rows
                .into_iter()
                .filter_map(|row| match row {
                    HeadingQueryMatch::Heading(row) => Some(row.id),
                    HeadingQueryMatch::File(_) => None,
                })
                .collect(),
            other => panic!("expected heading rows, got {other:?}"),
        }
    }

    fn link_ids(rows: QueryRows) -> Vec<i64> {
        match rows {
            QueryRows::Links(rows) => rows.into_iter().map(|row| row.id).collect(),
            other => panic!("expected link rows, got {other:?}"),
        }
    }

    fn file_paths(rows: QueryRows) -> Vec<String> {
        match rows {
            QueryRows::Files(rows) => rows.into_iter().map(|row| row.path).collect(),
            other => panic!("expected file rows, got {other:?}"),
        }
    }

    fn heading_file_paths(rows: QueryRows) -> Vec<String> {
        match rows {
            QueryRows::Headings(rows) => rows
                .into_iter()
                .filter_map(|row| match row {
                    HeadingQueryMatch::File(row) => Some(row.path),
                    HeadingQueryMatch::Heading(_) => None,
                })
                .collect(),
            other => panic!("expected heading rows, got {other:?}"),
        }
    }

    fn property_rows(
        connection: &Connection,
        heading_id: i64,
        key: &str,
    ) -> Vec<(Option<String>, bool)> {
        let mut statement = connection
            .prepare(
                "SELECT value, append
                 FROM properties
                 WHERE heading_id = ?1 AND key = ?2 COLLATE NOCASE
                 ORDER BY id",
            )
            .expect("property statement should prepare");
        let rows = statement
            .query_map(rusqlite::params![heading_id, key], |row| {
                Ok((row.get::<_, Option<String>>(0)?, row.get::<_, i64>(1)? != 0))
            })
            .expect("property rows should query");
        rows.collect::<Result<Vec<_>, _>>()
            .expect("property rows should collect")
    }

    fn seed_effective_tags(connection: &Connection, file_id: i64) {
        let parents = {
            let mut statement = connection
                .prepare("SELECT id, parent_id FROM headings WHERE file_id = ?1 ORDER BY id")
                .expect("seed heading query should prepare");
            statement
                .query_map([file_id], |row| {
                    Ok((row.get::<_, i64>(0)?, row.get::<_, Option<i64>>(1)?))
                })
                .expect("seed heading query should run")
                .collect::<Result<std::collections::HashMap<_, _>, _>>()
                .expect("seed heading rows should decode")
        };
        let direct_tags = {
            let mut statement = connection
                .prepare(
                    "SELECT tags.heading_id, tags.tag
                     FROM tags
                     INNER JOIN headings ON headings.id = tags.heading_id
                     WHERE headings.file_id = ?1
                     ORDER BY tags.heading_id, tags.tag",
                )
                .expect("seed tag query should prepare");
            let rows = statement
                .query_map([file_id], |row| {
                    Ok((row.get::<_, i64>(0)?, row.get::<_, String>(1)?))
                })
                .expect("seed tag query should run")
                .collect::<Result<Vec<_>, _>>()
                .expect("seed tag rows should decode");
            let mut by_heading = std::collections::HashMap::<i64, Vec<String>>::new();
            for (heading_id, tag) in rows {
                by_heading.entry(heading_id).or_default().push(tag);
            }
            by_heading
        };
        let rows = derive_effective_tags(&parents, &direct_tags)
            .into_iter()
            .map(|row| EffectiveTagRecord {
                heading_id: row.heading_id,
                file_id,
                tag: row.tag,
                position: row.position,
            })
            .collect::<Vec<_>>();
        DbWriter::insert_effective_tags(connection, &rows)
            .expect("effective tags should seed through the production writer");
    }

    fn seed_effective_properties(connection: &Connection, file_id: i64) {
        let parents = {
            let mut statement = connection
                .prepare("SELECT id, parent_id FROM headings WHERE file_id = ?1 ORDER BY id")
                .expect("seed heading query should prepare");
            statement
                .query_map([file_id], |row| {
                    Ok((row.get::<_, i64>(0)?, row.get::<_, Option<i64>>(1)?))
                })
                .expect("seed heading query should run")
                .collect::<Result<std::collections::HashMap<_, _>, _>>()
                .expect("seed heading rows should decode")
        };
        let rows_by_heading = {
            let mut statement = connection
                .prepare(
                    "SELECT properties.id, properties.heading_id, properties.key, properties.value,
                            properties.append, properties.line_number
                     FROM properties
                     INNER JOIN headings ON headings.id = properties.heading_id
                     WHERE headings.file_id = ?1
                     ORDER BY properties.line_number, properties.id",
                )
                .expect("seed property query should prepare");
            let rows = statement
                .query_map([file_id], |row| {
                    Ok(PropertyRow {
                        id: row.get(0)?,
                        heading_id: row.get(1)?,
                        key: row.get(2)?,
                        value: row.get(3)?,
                        append: row.get::<_, i64>(4)? != 0,
                        line_number: row.get(5)?,
                    })
                })
                .expect("seed property query should run")
                .collect::<Result<Vec<_>, _>>()
                .expect("seed property rows should decode");
            let mut by_heading = std::collections::HashMap::<i64, Vec<PropertyRow>>::new();
            for row in rows {
                by_heading.entry(row.heading_id).or_default().push(row);
            }
            by_heading
        };
        let rows = derive_effective_properties(&parents, &rows_by_heading)
            .into_iter()
            .map(|row| EffectivePropertyRecord {
                heading_id: row.heading_id,
                file_id,
                key: row.key,
                local_value: row.local_value,
                effective_value: row.effective_value,
            })
            .collect::<Vec<_>>();
        DbWriter::insert_effective_properties(connection, &rows)
            .expect("effective properties should seed through the production writer");
    }

    fn seeded_connection() -> Connection {
        let schema = SchemaDefinition::new(3, false);
        let mut connection =
            open_in_memory_database_with_schema(&schema).expect("database should open");
        seed_database(
            &mut connection,
            Path::new("/tmp/query-alpha.org"),
            Path::new("/tmp/query-beta.org"),
        );
        connection
    }

    fn date_bound_test_connection() -> Connection {
        let schema = SchemaDefinition::new(3, false);
        let mut connection =
            open_in_memory_database_with_schema(&schema).expect("database should open");

        let file = FileRecordInput {
            path: Path::new("/tmp/date-bounds.org").to_path_buf(),
            identity: None,
            mtime_ns: naive_date_time_seconds(2026, 1, 3, 0, 0) * 1_000_000_000,
            size: 100,
            content_hash: None,
            indexed_at: Some(naive_date_time_seconds(2026, 1, 3, 0, 1)),
        };

        DbWriter::rebuild_file(&mut connection, &file, |tx, file_id| {
            let root_id = DbWriter::insert_level0_heading(
                tx,
                &HeadingRecord {
                    id: Some(90),
                    file_id,
                    parent_id: None,
                    level: 0,
                    line_number: None,
                    byte_start: -1,
                    byte_end: 100,
                    title: "Date Bounds".to_string(),
                    title_raw: Some("Date Bounds".to_string()),
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
                &[
                    scheduled_heading(
                        file_id,
                        root_id,
                        100,
                        1,
                        "Start Of Day",
                        naive_date_time_seconds(2026, 1, 3, 0, 0),
                        "<2026-01-03 00:00>",
                    ),
                    scheduled_heading(
                        file_id,
                        root_id,
                        101,
                        2,
                        "Morning Task",
                        naive_date_time_seconds(2026, 1, 3, 9, 15),
                        "<2026-01-03 09:15>",
                    ),
                    scheduled_heading(
                        file_id,
                        root_id,
                        102,
                        3,
                        "Late Task",
                        naive_date_time_seconds(2026, 1, 3, 23, 59),
                        "<2026-01-03 23:59>",
                    ),
                    scheduled_heading(
                        file_id,
                        root_id,
                        103,
                        4,
                        "Dst Task",
                        naive_date_time_seconds(2026, 3, 29, 2, 30),
                        "<2026-03-29 02:30>",
                    ),
                    scheduled_heading(
                        file_id,
                        root_id,
                        106,
                        5,
                        "Month End Task",
                        naive_date_time_seconds(2026, 1, 31, 23, 59),
                        "<2026-01-31 23:59>",
                    ),
                    scheduled_heading(
                        file_id,
                        root_id,
                        107,
                        6,
                        "February Start Task",
                        naive_date_time_seconds(2026, 2, 1, 0, 0),
                        "<2026-02-01 00:00>",
                    ),
                    scheduled_heading(
                        file_id,
                        root_id,
                        104,
                        7,
                        "Year End Task",
                        naive_date_time_seconds(2026, 12, 31, 23, 59),
                        "<2026-12-31 23:59>",
                    ),
                    scheduled_heading(
                        file_id,
                        root_id,
                        105,
                        8,
                        "Next Year Task",
                        naive_date_time_seconds(2027, 1, 1, 0, 0),
                        "<2027-01-01 00:00>",
                    ),
                ],
            )?;

            DbWriter::insert_outline_path(
                tx,
                &[
                    outline_row(90, file_id, None, 0, "0000", "[\"Date Bounds\"]"),
                    outline_row(
                        100,
                        file_id,
                        Some(90),
                        1,
                        "0000.0001",
                        "[\"Date Bounds\",\"Start Of Day\"]",
                    ),
                    outline_row(
                        101,
                        file_id,
                        Some(90),
                        1,
                        "0000.0002",
                        "[\"Date Bounds\",\"Morning Task\"]",
                    ),
                    outline_row(
                        102,
                        file_id,
                        Some(90),
                        1,
                        "0000.0003",
                        "[\"Date Bounds\",\"Late Task\"]",
                    ),
                    outline_row(
                        103,
                        file_id,
                        Some(90),
                        1,
                        "0000.0004",
                        "[\"Date Bounds\",\"Dst Task\"]",
                    ),
                    outline_row(
                        106,
                        file_id,
                        Some(90),
                        1,
                        "0000.0005",
                        "[\"Date Bounds\",\"Month End Task\"]",
                    ),
                    outline_row(
                        107,
                        file_id,
                        Some(90),
                        1,
                        "0000.0006",
                        "[\"Date Bounds\",\"February Start Task\"]",
                    ),
                    outline_row(
                        104,
                        file_id,
                        Some(90),
                        1,
                        "0000.0007",
                        "[\"Date Bounds\",\"Year End Task\"]",
                    ),
                    outline_row(
                        105,
                        file_id,
                        Some(90),
                        1,
                        "0000.0008",
                        "[\"Date Bounds\",\"Next Year Task\"]",
                    ),
                ],
            )?;

            DbWriter::insert_timestamps(
                tx,
                &[
                    scheduled_timestamp(100, 2026, 1, 3, 0, 0, "<2026-01-03 Sat 00:00>"),
                    scheduled_timestamp(101, 2026, 1, 3, 9, 15, "<2026-01-03 Sat 09:15>"),
                    scheduled_timestamp(102, 2026, 1, 3, 23, 59, "<2026-01-03 Sat 23:59>"),
                    scheduled_timestamp(103, 2026, 3, 29, 2, 30, "<2026-03-29 Sun 02:30>"),
                    scheduled_timestamp(106, 2026, 1, 31, 23, 59, "<2026-01-31 Sat 23:59>"),
                    scheduled_timestamp(107, 2026, 2, 1, 0, 0, "<2026-02-01 Sun 00:00>"),
                    scheduled_timestamp(104, 2026, 12, 31, 23, 59, "<2026-12-31 Thu 23:59>"),
                    scheduled_timestamp(105, 2027, 1, 1, 0, 0, "<2027-01-01 Fri 00:00>"),
                ],
            )?;

            Ok(())
        })
        .expect("date bound fixture should seed");

        connection
    }

    fn temporal_test_connection() -> Connection {
        let mut connection = open_in_memory_database_with_schema(&SchemaDefinition::default())
            .expect("database should open");
        let file = FileRecordInput {
            path: Path::new("/tmp/temporal-predicates.org").to_path_buf(),
            identity: None,
            mtime_ns: naive_date_time_seconds(2026, 1, 3, 0, 0) * 1_000_000_000,
            size: 100,
            content_hash: None,
            indexed_at: Some(naive_date_time_seconds(2026, 1, 3, 0, 1)),
        };

        DbWriter::rebuild_file(&mut connection, &file, |tx, file_id| {
            let root_id = DbWriter::insert_level0_heading(
                tx,
                &base_heading(file_id, None, 200, 0, "With Time"),
            )?;

            let headings = vec![
                planning_heading(
                    file_id,
                    root_id,
                    201,
                    1,
                    "Scheduled Date Only",
                    PlanningFixture {
                        kind: "scheduled",
                        timestamp: Some(naive_date_time_seconds(2026, 1, 3, 0, 0)),
                        has_time: Some(false),
                        raw_value: "<2026-01-03 Sat>",
                    },
                ),
                planning_heading(
                    file_id,
                    root_id,
                    202,
                    2,
                    "Scheduled Timed",
                    PlanningFixture {
                        kind: "scheduled",
                        timestamp: Some(naive_date_time_seconds(2026, 1, 3, 9, 15)),
                        has_time: Some(true),
                        raw_value: "<2026-01-03 Sat 09:15>",
                    },
                ),
                planning_heading(
                    file_id,
                    root_id,
                    203,
                    3,
                    "Scheduled Midnight",
                    PlanningFixture {
                        kind: "scheduled",
                        timestamp: Some(naive_date_time_seconds(2026, 1, 3, 0, 0)),
                        has_time: Some(true),
                        raw_value: "<2026-01-03 Sat 00:00>",
                    },
                ),
                planning_heading(
                    file_id,
                    root_id,
                    204,
                    4,
                    "Scheduled Unknown",
                    PlanningFixture {
                        kind: "scheduled",
                        timestamp: Some(naive_date_time_seconds(2026, 1, 3, 12, 0)),
                        has_time: None,
                        raw_value: "<2026-01-03 Sat 12:00>",
                    },
                ),
                planning_heading(
                    file_id,
                    root_id,
                    205,
                    5,
                    "Deadline Date Only",
                    PlanningFixture {
                        kind: "deadline",
                        timestamp: Some(naive_date_time_seconds(2026, 1, 3, 0, 0)),
                        has_time: Some(false),
                        raw_value: "<2026-01-03 Sat>",
                    },
                ),
                planning_heading(
                    file_id,
                    root_id,
                    206,
                    6,
                    "Deadline Timed",
                    PlanningFixture {
                        kind: "deadline",
                        timestamp: Some(naive_date_time_seconds(2026, 1, 3, 10, 45)),
                        has_time: Some(true),
                        raw_value: "<2026-01-03 Sat 10:45>",
                    },
                ),
                planning_heading(
                    file_id,
                    root_id,
                    207,
                    7,
                    "Deadline Midnight",
                    PlanningFixture {
                        kind: "deadline",
                        timestamp: Some(naive_date_time_seconds(2026, 1, 3, 0, 0)),
                        has_time: Some(true),
                        raw_value: "<2026-01-03 Sat 00:00>",
                    },
                ),
                planning_heading(
                    file_id,
                    root_id,
                    208,
                    8,
                    "Deadline Unknown",
                    PlanningFixture {
                        kind: "deadline",
                        timestamp: Some(naive_date_time_seconds(2026, 1, 3, 18, 0)),
                        has_time: None,
                        raw_value: "<2026-01-03 Sat 18:00>",
                    },
                ),
                planning_heading(
                    file_id,
                    root_id,
                    209,
                    9,
                    "Closed Date Only",
                    PlanningFixture {
                        kind: "closed",
                        timestamp: Some(naive_date_time_seconds(2026, 1, 3, 0, 0)),
                        has_time: Some(false),
                        raw_value: "[2026-01-03 Sat]",
                    },
                ),
                planning_heading(
                    file_id,
                    root_id,
                    210,
                    10,
                    "Closed Timed",
                    PlanningFixture {
                        kind: "closed",
                        timestamp: Some(naive_date_time_seconds(2026, 1, 3, 11, 30)),
                        has_time: Some(true),
                        raw_value: "[2026-01-03 Sat 11:30]",
                    },
                ),
                planning_heading(
                    file_id,
                    root_id,
                    211,
                    11,
                    "Closed Midnight",
                    PlanningFixture {
                        kind: "closed",
                        timestamp: Some(naive_date_time_seconds(2026, 1, 3, 0, 0)),
                        has_time: Some(true),
                        raw_value: "[2026-01-03 Sat 00:00]",
                    },
                ),
                planning_heading(
                    file_id,
                    root_id,
                    212,
                    12,
                    "Closed Unknown",
                    PlanningFixture {
                        kind: "closed",
                        timestamp: Some(naive_date_time_seconds(2026, 1, 3, 16, 0)),
                        has_time: None,
                        raw_value: "[2026-01-03 Sat 16:00]",
                    },
                ),
                base_heading(file_id, Some(root_id), 213, 13, "Active Date Only"),
                base_heading(file_id, Some(root_id), 214, 14, "Active Timed"),
                base_heading(file_id, Some(root_id), 215, 15, "Active Midnight"),
                base_heading(file_id, Some(root_id), 216, 16, "Active Unknown"),
                base_heading(file_id, Some(root_id), 217, 17, "Inactive Date Only"),
                base_heading(file_id, Some(root_id), 218, 18, "Inactive Timed"),
                base_heading(file_id, Some(root_id), 219, 19, "Inactive Midnight"),
                base_heading(file_id, Some(root_id), 220, 20, "Inactive Unknown"),
            ];
            DbWriter::insert_headings(tx, &headings)?;

            let outline_rows = (1_i64..=20_i64)
                .map(|line_number| {
                    let heading_id = 200 + line_number;
                    outline_row(
                        heading_id,
                        file_id,
                        Some(root_id),
                        1,
                        &format!("0000.{line_number:04}"),
                        &format!("[\"With Time\",\"{}\"]", heading_title(heading_id)),
                    )
                })
                .collect::<Vec<_>>();
            let mut outline_rows_with_root = vec![outline_row(
                200,
                file_id,
                None,
                0,
                "0000",
                "[\"With Time\"]",
            )];
            outline_rows_with_root.extend(outline_rows);
            DbWriter::insert_outline_path(tx, &outline_rows_with_root)?;

            DbWriter::insert_timestamps(
                tx,
                &[
                    generic_timestamp(
                        213,
                        Some(false),
                        naive_date_time_seconds(2026, 1, 3, 0, 0),
                        "active",
                        "<2026-01-03 Sat>",
                    ),
                    generic_timestamp(
                        214,
                        Some(true),
                        naive_date_time_seconds(2026, 1, 3, 9, 15),
                        "active",
                        "<2026-01-03 Sat 09:15>",
                    ),
                    generic_timestamp(
                        215,
                        Some(true),
                        naive_date_time_seconds(2026, 1, 3, 0, 0),
                        "active",
                        "<2026-01-03 Sat 00:00>",
                    ),
                    generic_timestamp(
                        216,
                        None,
                        naive_date_time_seconds(2026, 1, 3, 12, 0),
                        "active",
                        "<2026-01-03 Sat 12:00>",
                    ),
                    generic_timestamp(
                        217,
                        Some(false),
                        naive_date_time_seconds(2026, 1, 3, 0, 0),
                        "inactive",
                        "[2026-01-03 Sat]",
                    ),
                    generic_timestamp(
                        218,
                        Some(true),
                        naive_date_time_seconds(2026, 1, 3, 13, 45),
                        "inactive",
                        "[2026-01-03 Sat 13:45]",
                    ),
                    generic_timestamp(
                        219,
                        Some(true),
                        naive_date_time_seconds(2026, 1, 3, 0, 0),
                        "inactive",
                        "[2026-01-03 Sat 00:00]",
                    ),
                    generic_timestamp(
                        220,
                        None,
                        naive_date_time_seconds(2026, 1, 3, 17, 0),
                        "inactive",
                        "[2026-01-03 Sat 17:00]",
                    ),
                ],
            )?;

            Ok(())
        })
        .expect("temporal predicate fixture should seed");

        connection
    }

    fn reduced_body_text_capability_connection(
        include_metadata_row: bool,
        metadata_value: bool,
    ) -> Connection {
        let connection = Connection::open_in_memory().expect("reduced schema should open");
        connection
            .execute_batch(
                r#"
CREATE TABLE db_metadata (
    key     TEXT PRIMARY KEY,
    value   TEXT NOT NULL
);
"#,
            )
            .expect("reduced body-text schema should initialize");

        if include_metadata_row {
            connection
                .execute(
                    "INSERT INTO db_metadata (key, value) VALUES (?1, ?2)",
                    rusqlite::params![
                        crate::db::DB_METADATA_BODY_TEXT_AVAILABLE_KEY,
                        if metadata_value { "1" } else { "0" }
                    ],
                )
                .expect("body-text metadata should insert");
        }

        connection
    }

    fn base_heading(
        file_id: i64,
        parent_id: Option<i64>,
        id: i64,
        line_number: i64,
        title: &str,
    ) -> HeadingRecord {
        HeadingRecord {
            id: Some(id),
            file_id,
            parent_id,
            level: if parent_id.is_some() { 1 } else { 0 },
            line_number: if line_number > 0 {
                Some(line_number)
            } else {
                None
            },
            byte_start: if line_number > 0 {
                line_number * 10
            } else {
                -1
            },
            byte_end: if line_number > 0 {
                line_number * 10 + 5
            } else {
                100
            },
            title: title.to_string(),
            title_raw: Some(title.to_string()),
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
        }
    }

    fn planning_heading(
        file_id: i64,
        root_id: i64,
        id: i64,
        line_number: i64,
        title: &str,
        planning: PlanningFixture<'_>,
    ) -> HeadingRecord {
        let mut heading = base_heading(file_id, Some(root_id), id, line_number, title);
        match planning.kind {
            "scheduled" => {
                heading.scheduled_raw = Some(planning.raw_value.to_string());
                heading.scheduled_ts = planning.timestamp;
                heading.scheduled_has_time = planning.has_time;
            }
            "deadline" => {
                heading.deadline_raw = Some(planning.raw_value.to_string());
                heading.deadline_ts = planning.timestamp;
                heading.deadline_has_time = planning.has_time;
            }
            "closed" => {
                heading.closed_raw = Some(planning.raw_value.to_string());
                heading.closed_ts = planning.timestamp;
                heading.closed_has_time = planning.has_time;
            }
            _ => unreachable!("unexpected planning heading kind"),
        }
        heading
    }

    fn generic_timestamp(
        heading_id: i64,
        has_time: Option<bool>,
        start_ts: i64,
        timestamp_type: &str,
        raw_value: &str,
    ) -> TimestampRecord {
        TimestampRecord {
            heading_id,
            role: None,
            has_time,
            start_ts: Some(start_ts),
            end_ts: None,
            timestamp_type: Some(timestamp_type.to_string()),
            range_type: Some("none".to_string()),
            raw_value: raw_value.to_string(),
            byte_start: heading_id,
            byte_end: heading_id + 1,
            line_number: Some(heading_id - 200),
        }
    }

    fn heading_title(heading_id: i64) -> &'static str {
        match heading_id {
            201 => "Scheduled Date Only",
            202 => "Scheduled Timed",
            203 => "Scheduled Midnight",
            204 => "Scheduled Unknown",
            205 => "Deadline Date Only",
            206 => "Deadline Timed",
            207 => "Deadline Midnight",
            208 => "Deadline Unknown",
            209 => "Closed Date Only",
            210 => "Closed Timed",
            211 => "Closed Midnight",
            212 => "Closed Unknown",
            213 => "Active Date Only",
            214 => "Active Timed",
            215 => "Active Midnight",
            216 => "Active Unknown",
            217 => "Inactive Date Only",
            218 => "Inactive Timed",
            219 => "Inactive Midnight",
            220 => "Inactive Unknown",
            _ => unreachable!("unexpected temporal predicate fixture heading"),
        }
    }

    fn naive_date_time_seconds(year: i32, month: u32, day: u32, hour: u32, minute: u32) -> i64 {
        NaiveDate::from_ymd_opt(year, month, day)
            .expect("date should be valid")
            .and_hms_opt(hour, minute, 0)
            .expect("time should be valid")
            .and_utc()
            .timestamp()
    }

    fn scheduled_heading(
        file_id: i64,
        root_id: i64,
        id: i64,
        line_number: i64,
        title: &str,
        scheduled_ts: i64,
        scheduled_raw: &str,
    ) -> HeadingRecord {
        HeadingRecord {
            id: Some(id),
            file_id,
            parent_id: Some(root_id),
            level: 1,
            line_number: Some(line_number),
            byte_start: line_number * 10,
            byte_end: line_number * 10 + 5,
            title: title.to_string(),
            title_raw: Some(title.to_string()),
            todo_keyword: None,
            todo_type: None,
            priority: None,
            scheduled_raw: Some(scheduled_raw.to_string()),
            scheduled_ts: Some(scheduled_ts),
            scheduled_has_time: None,
            deadline_raw: None,
            deadline_ts: None,
            deadline_has_time: None,
            closed_raw: None,
            closed_ts: None,
            closed_has_time: None,
            archivedp: false,
            footnote_section_p: false,
        }
    }

    fn scheduled_timestamp(
        heading_id: i64,
        year: i32,
        month: u32,
        day: u32,
        hour: u32,
        minute: u32,
        raw_value: &str,
    ) -> TimestampRecord {
        TimestampRecord {
            heading_id,
            role: Some("scheduled".to_string()),
            has_time: None,
            start_ts: Some(naive_date_time_seconds(year, month, day, hour, minute)),
            end_ts: None,
            timestamp_type: Some("active".to_string()),
            range_type: Some("none".to_string()),
            raw_value: raw_value.to_string(),
            byte_start: heading_id,
            byte_end: heading_id + 1,
            line_number: Some(heading_id - 99),
        }
    }

    fn outline_row(
        heading_id: i64,
        file_id: i64,
        parent_id: Option<i64>,
        depth: i64,
        materialized_path: &str,
        breadcrumbs_json: &str,
    ) -> OutlinePathRecord {
        OutlinePathRecord {
            heading_id,
            file_id,
            parent_id,
            depth,
            materialized_path: materialized_path.to_string(),
            breadcrumbs_json: breadcrumbs_json.to_string(),
        }
    }

    fn seed_database(connection: &mut Connection, alpha_path: &Path, beta_path: &Path) {
        let alpha = FileRecordInput {
            path: alpha_path.to_path_buf(),
            identity: None,
            mtime_ns: 1_767_398_400_000_000_000,
            size: 100,
            content_hash: None,
            indexed_at: Some(1_767_398_410),
        };
        let beta = FileRecordInput {
            path: beta_path.to_path_buf(),
            identity: None,
            mtime_ns: 1_767_484_800_000_000_000,
            size: 120,
            content_hash: None,
            indexed_at: Some(1_767_484_810),
        };
        let gamma = FileRecordInput {
            path: Path::new("/tmp/query-gamma.org").to_path_buf(),
            identity: None,
            mtime_ns: 1_767_571_200_000_000_000,
            size: 80,
            content_hash: None,
            indexed_at: Some(1_767_571_210),
        };

        let (beta_file_id, ()) = DbWriter::rebuild_file(connection, &beta, |tx, file_id| {
            let root_id = DbWriter::insert_level0_heading(
                tx,
                &HeadingRecord {
                    id: Some(20),
                    file_id,
                    parent_id: None,
                    level: 0,
                    line_number: None,
                    byte_start: -1,
                    byte_end: 120,
                    title: "Beta Index".to_string(),
                    title_raw: Some("Beta Index".to_string()),
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
                    breadcrumbs_json: "[\"Beta Index\"]".to_string(),
                }],
            )?;
            DbWriter::insert_tags(
                tx,
                &[TagRecord {
                    heading_id: root_id,
                    tag: "archive".to_string(),
                }],
            )?;
            DbWriter::insert_keywords(
                tx,
                &[KeywordRecord {
                    heading_id: root_id,
                    keyword: "AUTHOR".to_string(),
                    value: Some("Bob".to_string()),
                    line_number: Some(1),
                }],
            )?;
            Ok(())
        })
        .expect("beta file should seed");

        let (alpha_file_id, ()) = DbWriter::rebuild_file(connection, &alpha, |tx, file_id| {
            let root_id = DbWriter::insert_level0_heading(
                tx,
                &HeadingRecord {
                    id: Some(10),
                    file_id,
                    parent_id: None,
                    level: 0,
                    line_number: None,
                    byte_start: -1,
                    byte_end: 100,
                    title: "Alpha Index".to_string(),
                    title_raw: Some("Alpha Index".to_string()),
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
                &[
                    HeadingRecord {
                        id: Some(11),
                        file_id,
                        parent_id: Some(root_id),
                        level: 1,
                        line_number: Some(3),
                        byte_start: 10,
                        byte_end: 40,
                        title: "Query Engine".to_string(),
                        title_raw: Some("Query Engine".to_string()),
                        todo_keyword: Some("NEXT".to_string()),
                        todo_type: Some("open".to_string()),
                        priority: Some("A".to_string()),
                        scheduled_raw: Some("<2026-01-03 Fri>".to_string()),
                        scheduled_ts: Some(1_767_398_400),
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
                    HeadingRecord {
                        id: Some(12),
                        file_id,
                        parent_id: Some(11),
                        level: 2,
                        line_number: Some(6),
                        byte_start: 41,
                        byte_end: 70,
                        title: "Nested Task".to_string(),
                        title_raw: Some("Nested Task".to_string()),
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
                    HeadingRecord {
                        id: Some(13),
                        file_id,
                        parent_id: Some(root_id),
                        level: 1,
                        line_number: Some(8),
                        byte_start: 71,
                        byte_end: 95,
                        title: "Loose Note".to_string(),
                        title_raw: Some("Loose Note".to_string()),
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
                    HeadingRecord {
                        id: Some(14),
                        file_id,
                        parent_id: Some(11),
                        level: 2,
                        line_number: Some(10),
                        byte_start: 96,
                        byte_end: 130,
                        title: "Statistic Cookies".to_string(),
                        title_raw: Some("REVIEW [#B] Statistic Cookies [0/1]".to_string()),
                        todo_keyword: Some("REVIEW".to_string()),
                        todo_type: Some("open".to_string()),
                        priority: Some("B".to_string()),
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
                    HeadingRecord {
                        id: Some(15),
                        file_id,
                        parent_id: Some(11),
                        level: 2,
                        line_number: Some(12),
                        byte_start: 131,
                        byte_end: 160,
                        title: "Overriding Child".to_string(),
                        title_raw: Some("Overriding Child".to_string()),
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
                ],
            )?;
            DbWriter::insert_outline_path(
                tx,
                &[
                    OutlinePathRecord {
                        heading_id: 10,
                        file_id,
                        parent_id: None,
                        depth: 0,
                        materialized_path: "0000".to_string(),
                        breadcrumbs_json: "[\"Alpha Index\"]".to_string(),
                    },
                    OutlinePathRecord {
                        heading_id: 11,
                        file_id,
                        parent_id: Some(10),
                        depth: 1,
                        materialized_path: "0000.0001".to_string(),
                        breadcrumbs_json: "[\"Alpha Index\",\"Query Engine\"]".to_string(),
                    },
                    OutlinePathRecord {
                        heading_id: 12,
                        file_id,
                        parent_id: Some(11),
                        depth: 2,
                        materialized_path: "0000.0001.0001".to_string(),
                        breadcrumbs_json: "[\"Alpha Index\",\"Query Engine\",\"Nested Task\"]"
                            .to_string(),
                    },
                    OutlinePathRecord {
                        heading_id: 13,
                        file_id,
                        parent_id: Some(10),
                        depth: 1,
                        materialized_path: "0000.0002".to_string(),
                        breadcrumbs_json: "[\"Alpha Index\",\"Loose Note\"]".to_string(),
                    },
                    OutlinePathRecord {
                        heading_id: 14,
                        file_id,
                        parent_id: Some(11),
                        depth: 2,
                        materialized_path: "0000.0001.0002".to_string(),
                        breadcrumbs_json:
                            "[\"Alpha Index\",\"Query Engine\",\"Statistic Cookies\"]".to_string(),
                    },
                    OutlinePathRecord {
                        heading_id: 15,
                        file_id,
                        parent_id: Some(11),
                        depth: 2,
                        materialized_path: "0000.0001.0003".to_string(),
                        breadcrumbs_json: "[\"Alpha Index\",\"Query Engine\",\"Overriding Child\"]"
                            .to_string(),
                    },
                ],
            )?;
            DbWriter::insert_tags(
                tx,
                &[
                    TagRecord {
                        heading_id: 10,
                        tag: "filetag".to_string(),
                    },
                    TagRecord {
                        heading_id: 11,
                        tag: "project".to_string(),
                    },
                    TagRecord {
                        heading_id: 12,
                        tag: "urgent".to_string(),
                    },
                    TagRecord {
                        heading_id: 13,
                        tag: "misc".to_string(),
                    },
                    TagRecord {
                        heading_id: 14,
                        tag: "filetag".to_string(),
                    },
                ],
            )?;
            seed_effective_tags(tx, file_id);
            DbWriter::insert_keywords(
                tx,
                &[KeywordRecord {
                    heading_id: 10,
                    keyword: "AUTHOR".to_string(),
                    value: Some("Alice".to_string()),
                    line_number: Some(1),
                }],
            )?;
            DbWriter::insert_properties(
                tx,
                &[
                    PropertyRecord {
                        heading_id: 10,
                        key: "CATEGORY".to_string(),
                        value: Some("work".to_string()),
                        source: "category_keyword".to_string(),
                        append: false,
                        line_number: Some(2),
                    },
                    PropertyRecord {
                        heading_id: 10,
                        key: "OWNER".to_string(),
                        value: Some("Alice".to_string()),
                        source: "property_keyword".to_string(),
                        append: false,
                        line_number: Some(2),
                    },
                    PropertyRecord {
                        heading_id: 10,
                        key: "KEYWORD_APPEND".to_string(),
                        value: Some("foo=1".to_string()),
                        source: "property_keyword".to_string(),
                        append: false,
                        line_number: Some(2),
                    },
                    PropertyRecord {
                        heading_id: 10,
                        key: "KEYWORD_APPEND".to_string(),
                        value: Some("bar=2".to_string()),
                        source: "property_keyword".to_string(),
                        append: true,
                        line_number: Some(3),
                    },
                    PropertyRecord {
                        heading_id: 10,
                        key: "KEYWORD_OVERWRITTEN_BY_SECOND".to_string(),
                        value: Some("invalid".to_string()),
                        source: "property_keyword".to_string(),
                        append: false,
                        line_number: Some(4),
                    },
                    PropertyRecord {
                        heading_id: 10,
                        key: "KEYWORD_OVERWRITTEN_BY_SECOND".to_string(),
                        value: Some("valid".to_string()),
                        source: "property_keyword".to_string(),
                        append: false,
                        line_number: Some(5),
                    },
                    PropertyRecord {
                        heading_id: 10,
                        key: "ROOT_ONLY".to_string(),
                        value: Some("root".to_string()),
                        source: "property_drawer".to_string(),
                        append: false,
                        line_number: Some(6),
                    },
                    PropertyRecord {
                        heading_id: 10,
                        key: "OVERRIDE_CHAIN".to_string(),
                        value: Some("root".to_string()),
                        source: "property_drawer".to_string(),
                        append: false,
                        line_number: Some(7),
                    },
                    PropertyRecord {
                        heading_id: 11,
                        key: "AREA".to_string(),
                        value: Some("infra".to_string()),
                        source: "property_drawer".to_string(),
                        append: false,
                        line_number: Some(4),
                    },
                    PropertyRecord {
                        heading_id: 11,
                        key: "LANG".to_string(),
                        value: Some("rust".to_string()),
                        source: "property_drawer".to_string(),
                        append: false,
                        line_number: Some(5),
                    },
                    PropertyRecord {
                        heading_id: 11,
                        key: "LANG".to_string(),
                        value: Some("emacs".to_string()),
                        source: "property_drawer".to_string(),
                        append: true,
                        line_number: Some(6),
                    },
                    PropertyRecord {
                        heading_id: 11,
                        key: "OVERRIDE_CHAIN".to_string(),
                        value: Some("parent".to_string()),
                        source: "property_drawer".to_string(),
                        append: false,
                        line_number: Some(7),
                    },
                    PropertyRecord {
                        heading_id: 11,
                        key: "APPEND_INHERITED".to_string(),
                        value: Some("parent".to_string()),
                        source: "property_drawer".to_string(),
                        append: false,
                        line_number: Some(8),
                    },
                    PropertyRecord {
                        heading_id: 11,
                        key: "APPEND_BEFORE".to_string(),
                        value: Some("appending before".to_string()),
                        source: "property_drawer".to_string(),
                        append: true,
                        line_number: Some(9),
                    },
                    PropertyRecord {
                        heading_id: 11,
                        key: "APPEND_BEFORE".to_string(),
                        value: Some("definition".to_string()),
                        source: "property_drawer".to_string(),
                        append: false,
                        line_number: Some(10),
                    },
                    PropertyRecord {
                        heading_id: 11,
                        key: "LOCAL_BASE_APPEND".to_string(),
                        value: Some("parent".to_string()),
                        source: "property_drawer".to_string(),
                        append: false,
                        line_number: Some(11),
                    },
                    PropertyRecord {
                        heading_id: 12,
                        key: "OWNER".to_string(),
                        value: Some("Bob".to_string()),
                        source: "property_drawer".to_string(),
                        append: false,
                        line_number: Some(9),
                    },
                    PropertyRecord {
                        heading_id: 12,
                        key: "APPEND_INHERITED".to_string(),
                        value: Some("child".to_string()),
                        source: "property_drawer".to_string(),
                        append: true,
                        line_number: Some(10),
                    },
                    PropertyRecord {
                        heading_id: 12,
                        key: "LOCAL_BASE_APPEND".to_string(),
                        value: Some("before".to_string()),
                        source: "property_drawer".to_string(),
                        append: true,
                        line_number: Some(11),
                    },
                    PropertyRecord {
                        heading_id: 12,
                        key: "LOCAL_BASE_APPEND".to_string(),
                        value: Some("child".to_string()),
                        source: "property_drawer".to_string(),
                        append: false,
                        line_number: Some(12),
                    },
                    PropertyRecord {
                        heading_id: 12,
                        key: "LOCAL_BASE_APPEND".to_string(),
                        value: Some("after".to_string()),
                        source: "property_drawer".to_string(),
                        append: true,
                        line_number: Some(13),
                    },
                    PropertyRecord {
                        heading_id: 13,
                        key: "DEFINED_TWICE".to_string(),
                        value: Some("works".to_string()),
                        source: "property_drawer".to_string(),
                        append: false,
                        line_number: Some(11),
                    },
                    PropertyRecord {
                        heading_id: 13,
                        key: "DEFINED_TWICE".to_string(),
                        value: Some("second is effective".to_string()),
                        source: "property_drawer".to_string(),
                        append: false,
                        line_number: Some(12),
                    },
                    PropertyRecord {
                        heading_id: 13,
                        key: "APPEND_REPLACED".to_string(),
                        value: Some("first".to_string()),
                        source: "property_drawer".to_string(),
                        append: false,
                        line_number: Some(13),
                    },
                    PropertyRecord {
                        heading_id: 13,
                        key: "APPEND_REPLACED".to_string(),
                        value: Some("appended".to_string()),
                        source: "property_drawer".to_string(),
                        append: true,
                        line_number: Some(14),
                    },
                    PropertyRecord {
                        heading_id: 13,
                        key: "APPEND_REPLACED".to_string(),
                        value: Some("second".to_string()),
                        source: "property_drawer".to_string(),
                        append: false,
                        line_number: Some(15),
                    },
                    PropertyRecord {
                        heading_id: 14,
                        key: "ADD-VALUE".to_string(),
                        value: Some("is".to_string()),
                        source: "property_drawer".to_string(),
                        append: false,
                        line_number: Some(13),
                    },
                    PropertyRecord {
                        heading_id: 14,
                        key: "ADD-VALUE".to_string(),
                        value: Some("valid".to_string()),
                        source: "property_drawer".to_string(),
                        append: true,
                        line_number: Some(14),
                    },
                    PropertyRecord {
                        heading_id: 14,
                        key: "MULTI_APPEND".to_string(),
                        value: Some("before".to_string()),
                        source: "property_drawer".to_string(),
                        append: true,
                        line_number: Some(15),
                    },
                    PropertyRecord {
                        heading_id: 14,
                        key: "MULTI_APPEND".to_string(),
                        value: Some("first".to_string()),
                        source: "property_drawer".to_string(),
                        append: false,
                        line_number: Some(16),
                    },
                    PropertyRecord {
                        heading_id: 14,
                        key: "MULTI_APPEND".to_string(),
                        value: Some("middle".to_string()),
                        source: "property_drawer".to_string(),
                        append: true,
                        line_number: Some(17),
                    },
                    PropertyRecord {
                        heading_id: 14,
                        key: "MULTI_APPEND".to_string(),
                        value: Some("second".to_string()),
                        source: "property_drawer".to_string(),
                        append: false,
                        line_number: Some(18),
                    },
                    PropertyRecord {
                        heading_id: 14,
                        key: "MULTI_APPEND".to_string(),
                        value: Some("after".to_string()),
                        source: "property_drawer".to_string(),
                        append: true,
                        line_number: Some(19),
                    },
                    PropertyRecord {
                        heading_id: 15,
                        key: "OVERRIDE_CHAIN".to_string(),
                        value: Some("child".to_string()),
                        source: "property_drawer".to_string(),
                        append: false,
                        line_number: Some(15),
                    },
                ],
            )?;
            DbWriter::insert_timestamps(
                tx,
                &[TimestampRecord {
                    heading_id: 11,
                    role: Some("scheduled".to_string()),
                    has_time: None,
                    start_ts: Some(1_767_398_400),
                    end_ts: None,
                    timestamp_type: Some("active".to_string()),
                    range_type: Some("none".to_string()),
                    raw_value: "<2026-01-03 Fri>".to_string(),
                    byte_start: 12,
                    byte_end: 28,
                    line_number: Some(3),
                }],
            )?;
            DbWriter::insert_links(
                tx,
                &[LinkRecord {
                    id: Some(100),
                    file_id,
                    heading_id: 11,
                    byte_start: 50,
                    byte_end: 80,
                    line: 4,
                    source_context: "normal".to_string(),
                    format: "bracket".to_string(),
                    raw: "[[file:beta.org][Beta notes]]".to_string(),
                    raw_target: "file:beta.org".to_string(),
                    raw_description: Some("Beta notes".to_string()),
                    link_type: "file".to_string(),
                    path: "beta.org".to_string(),
                    search_option: None,
                }],
            )?;
            tx.execute(
                "UPDATE links
                 SET path_absolute = ?1,
                     target_file_id = ?2,
                     target_heading_id = ?3,
                     resolution_status = 'resolved'
                 WHERE id = 100",
                rusqlite::params![beta_path.to_string_lossy().to_string(), beta_file_id, 20],
            )
            .expect("link target should update");
            Ok(())
        })
        .expect("alpha file should seed");
        seed_effective_properties(connection, alpha_file_id);

        let (gamma_file_id, ()) = DbWriter::rebuild_file(connection, &gamma, |tx, file_id| {
            let root_id = DbWriter::insert_level0_heading(
                tx,
                &HeadingRecord {
                    id: Some(30),
                    file_id,
                    parent_id: None,
                    level: 0,
                    line_number: None,
                    byte_start: -1,
                    byte_end: 80,
                    title: "Gamma Index".to_string(),
                    title_raw: Some("Gamma Index".to_string()),
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
                    id: Some(31),
                    file_id,
                    parent_id: Some(root_id),
                    level: 1,
                    line_number: Some(2),
                    byte_start: 10,
                    byte_end: 28,
                    title: "Gamma Candidate".to_string(),
                    title_raw: Some("Gamma Candidate".to_string()),
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
            DbWriter::insert_outline_path(
                tx,
                &[
                    OutlinePathRecord {
                        heading_id: root_id,
                        file_id,
                        parent_id: None,
                        depth: 0,
                        materialized_path: "0000".to_string(),
                        breadcrumbs_json: "[\"Gamma Index\"]".to_string(),
                    },
                    OutlinePathRecord {
                        heading_id: 31,
                        file_id,
                        parent_id: Some(root_id),
                        depth: 1,
                        materialized_path: "0000.0001".to_string(),
                        breadcrumbs_json: "[\"Gamma Index\",\"Gamma Candidate\"]".to_string(),
                    },
                ],
            )?;
            Ok(())
        })
        .expect("gamma file should seed");

        connection
            .execute(
                "INSERT INTO headings
                 (id, file_id, parent_id, level, line_number, byte_start, byte_end, title, title_raw,
                  todo_keyword, todo_type, priority, scheduled_raw, scheduled_ts, deadline_raw,
                  deadline_ts, closed_raw, closed_ts, archivedp, footnote_section_p)
                 VALUES
                 (21, ?1, 20, 1, 3, 10, 40, 'Beta Target', 'Beta Target',
                  NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, 0, 0)",
                rusqlite::params![beta_file_id],
            )
            .expect("beta child heading should insert");
        DbWriter::insert_outline_path(
            connection,
            &[OutlinePathRecord {
                heading_id: 21,
                file_id: beta_file_id,
                parent_id: Some(20),
                depth: 1,
                materialized_path: "0000.0001".to_string(),
                breadcrumbs_json: "[\"Beta Index\",\"Beta Target\"]".to_string(),
            }],
        )
        .expect("beta child outline should insert");
        DbWriter::insert_tags(
            connection,
            &[TagRecord {
                heading_id: 21,
                tag: "target".to_string(),
            }],
        )
        .expect("beta child tag should insert");
        seed_effective_tags(connection, beta_file_id);

        DbWriter::set_metadata_flag(
            connection,
            crate::db::DB_METADATA_BODY_TEXT_AVAILABLE_KEY,
            true,
        )
        .expect("body-text capability should persist");
        DbWriter::insert_heading_bodies(
            connection,
            &[
                HeadingBodyRecord {
                    heading_id: 11,
                    body_text: "SQLite index notes with FTS fallback guidance.".to_string(),
                    body_byte_start: Some(16),
                    body_byte_end: Some(62),
                },
                HeadingBodyRecord {
                    heading_id: 12,
                    body_text: "Nested sqlite implementation checklist.".to_string(),
                    body_byte_start: Some(63),
                    body_byte_end: Some(101),
                },
                HeadingBodyRecord {
                    heading_id: 13,
                    body_text: "General notes without the keyword.".to_string(),
                    body_byte_start: Some(102),
                    body_byte_end: Some(136),
                },
                HeadingBodyRecord {
                    heading_id: 14,
                    body_text: String::new(),
                    body_byte_start: Some(137),
                    body_byte_end: Some(137),
                },
                HeadingBodyRecord {
                    heading_id: 21,
                    body_text: "BETA target body mentions sqlite and fts together.".to_string(),
                    body_byte_start: Some(12),
                    body_byte_end: Some(60),
                },
            ],
        )
        .expect("heading body rows should insert");

        DbWriter::insert_links(
            connection,
            &[
                LinkRecord {
                    id: Some(101),
                    file_id: alpha_file_id,
                    heading_id: 12,
                    byte_start: 60,
                    byte_end: 95,
                    line: 6,
                    source_context: "normal".to_string(),
                    format: "bracket".to_string(),
                    raw: "[[file:beta.org::*Beta Target][Beta heading]]".to_string(),
                    raw_target: "file:beta.org::*Beta Target".to_string(),
                    raw_description: Some("Beta heading".to_string()),
                    link_type: "file".to_string(),
                    path: "beta.org".to_string(),
                    search_option: Some("*Beta Target".to_string()),
                },
                LinkRecord {
                    id: Some(102),
                    file_id: alpha_file_id,
                    heading_id: 10,
                    byte_start: 0,
                    byte_end: 24,
                    line: 1,
                    source_context: "normal".to_string(),
                    format: "bracket".to_string(),
                    raw: "[[file:beta.org][Preamble]]".to_string(),
                    raw_target: "file:beta.org".to_string(),
                    raw_description: Some("Preamble".to_string()),
                    link_type: "file".to_string(),
                    path: "beta.org".to_string(),
                    search_option: None,
                },
                LinkRecord {
                    id: Some(103),
                    file_id: beta_file_id,
                    heading_id: 21,
                    byte_start: 50,
                    byte_end: 84,
                    line: 4,
                    source_context: "normal".to_string(),
                    format: "bracket".to_string(),
                    raw: "[[file:alpha.org::*Query Engine][Backlink]]".to_string(),
                    raw_target: "file:alpha.org::*Query Engine".to_string(),
                    raw_description: Some("Backlink".to_string()),
                    link_type: "file".to_string(),
                    path: "alpha.org".to_string(),
                    search_option: Some("*Query Engine".to_string()),
                },
                LinkRecord {
                    id: Some(104),
                    file_id: beta_file_id,
                    heading_id: 20,
                    byte_start: 0,
                    byte_end: 22,
                    line: 1,
                    source_context: "normal".to_string(),
                    format: "bracket".to_string(),
                    raw: "[[file:alpha.org][Root]]".to_string(),
                    raw_target: "file:alpha.org".to_string(),
                    raw_description: Some("Root".to_string()),
                    link_type: "file".to_string(),
                    path: "alpha.org".to_string(),
                    search_option: None,
                },
                LinkRecord {
                    id: Some(105),
                    file_id: alpha_file_id,
                    heading_id: 13,
                    byte_start: 80,
                    byte_end: 104,
                    line: 8,
                    source_context: "normal".to_string(),
                    format: "bracket".to_string(),
                    raw: "[[file:missing.org]]".to_string(),
                    raw_target: "file:missing.org".to_string(),
                    raw_description: None,
                    link_type: "file".to_string(),
                    path: "missing.org".to_string(),
                    search_option: None,
                },
                LinkRecord {
                    id: Some(106),
                    file_id: beta_file_id,
                    heading_id: 21,
                    byte_start: 85,
                    byte_end: 100,
                    line: 5,
                    source_context: "normal".to_string(),
                    format: "bracket".to_string(),
                    raw: "[[id:missing-id]]".to_string(),
                    raw_target: "id:missing-id".to_string(),
                    raw_description: None,
                    link_type: "id".to_string(),
                    path: "missing-id".to_string(),
                    search_option: None,
                },
                LinkRecord {
                    id: Some(107),
                    file_id: alpha_file_id,
                    heading_id: 11,
                    byte_start: 81,
                    byte_end: 92,
                    line: 5,
                    source_context: "normal".to_string(),
                    format: "bracket".to_string(),
                    raw: "[[id:dup-id]]".to_string(),
                    raw_target: "id:dup-id".to_string(),
                    raw_description: None,
                    link_type: "id".to_string(),
                    path: "dup-id".to_string(),
                    search_option: None,
                },
            ],
        )
        .expect("relation links should seed");

        connection
            .execute(
                "UPDATE links
                 SET path_absolute = ?1,
                     target_file_id = ?2,
                     target_heading_id = ?3,
                     resolution_status = 'resolved'
                 WHERE id = 101",
                rusqlite::params![beta_path.to_string_lossy().to_string(), beta_file_id, 21],
            )
            .expect("heading target should update");
        connection
            .execute(
                "UPDATE links
                 SET path_absolute = ?1,
                     target_file_id = ?2,
                     target_heading_id = ?3,
                     resolution_status = 'resolved'
                 WHERE id = 102",
                rusqlite::params![beta_path.to_string_lossy().to_string(), beta_file_id, 20],
            )
            .expect("preamble file target should update");
        connection
            .execute(
                "UPDATE links
                 SET path_absolute = ?1,
                     target_file_id = ?2,
                     target_heading_id = ?3,
                     resolution_status = 'resolved'
                 WHERE id = 103",
                rusqlite::params![alpha_path.to_string_lossy().to_string(), alpha_file_id, 11],
            )
            .expect("backlink heading target should update");
        connection
            .execute(
                "UPDATE links
                 SET path_absolute = ?1,
                     target_file_id = ?2,
                     target_heading_id = ?3,
                     resolution_status = 'resolved'
                 WHERE id = 104",
                rusqlite::params![alpha_path.to_string_lossy().to_string(), alpha_file_id, 10],
            )
            .expect("backlink root target should update");
        connection
            .execute(
                "UPDATE links
                 SET resolution_status = 'broken',
                     resolution_diagnostic = 'missing target'
                 WHERE id = 105",
                [],
            )
            .expect("broken link should update");
        connection
            .execute(
                "UPDATE links
                 SET target_id = 'missing-id',
                     resolution_status = 'unresolved',
                     resolution_diagnostic = 'missing org id'
                 WHERE id = 106",
                [],
            )
            .expect("unresolved link should update");
        connection
            .execute(
                "UPDATE links
                 SET target_file_id = ?1,
                     target_heading_id = ?2,
                     target_id = 'dup-id',
                     resolution_status = 'ambiguous',
                     resolution_diagnostic = 'duplicate org id'
                 WHERE id = 107",
                rusqlite::params![gamma_file_id, 31],
            )
            .expect("ambiguous link should update");
    }

    fn table_counts(connection: &Connection) -> Vec<(&'static str, i64)> {
        [
            "files",
            "headings",
            "links",
            "tags",
            "properties",
            "keywords",
            "timestamps",
        ]
        .into_iter()
        .map(|table| {
            let sql = format!("SELECT COUNT(*) FROM {table}");
            let count = connection
                .query_row(&sql, [], |row| row.get(0))
                .expect("table count should load");
            (table, count)
        })
        .collect()
    }
}
