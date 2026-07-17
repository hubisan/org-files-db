use std::collections::HashSet;
use std::fmt;

use rusqlite::{
    params_from_iter,
    types::{ToSqlOutput, Value},
    Connection, ToSql,
};
use serde::Serialize;

use super::result::QueryExecutionOptions;
use super::{
    ensure_relative_dates_resolved, resolve_relative_dates, QueryDateResolutionOptions,
    QueryTarget, QueryValidationOptions, QueryValue, ValidatedArg, ValidatedExpr, ValidatedOption,
    ValidatedPredicate, ValidatedQuery,
};
use crate::db::DB_METADATA_BODY_TEXT_AVAILABLE_KEY;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CompiledSqlQuery {
    pub target: QueryTarget,
    pub sql: String,
    pub params: Vec<QueryParam>,
}

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

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
#[serde(tag = "kind", rename_all = "snake_case")]
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
    pub priority: Option<char>,
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
    pub heading_breadcrumbs_json: String,
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
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum HeadingMatchKind {
    RealHeading,
    RootFile,
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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum TemporalUnit {
    Seconds,
    Nanoseconds,
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

    fn heading_col(&self, column: &str) -> String {
        format!("{}.{}", self.heading_alias, column)
    }

    fn file_col(&self, column: &str) -> String {
        format!("{}.{}", self.file_alias, column)
    }

    fn root_col(&self, column: &str) -> String {
        format!("{}.{}", self.root_alias, column)
    }

    fn link_col(&self, column: &str) -> String {
        format!("{}.{}", self.link_alias, column)
    }

    fn link_heading_col(&self, column: &str) -> String {
        format!("{}.{}", self.link_heading_alias, column)
    }

    fn outline_col(&self, column: &str) -> String {
        format!("{}.{}", self.outline_alias, column)
    }
}

#[derive(Debug, Default)]
struct AliasAllocator {
    next_scope_id: usize,
}

impl AliasAllocator {
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
}

#[derive(Debug, Clone)]
struct WithTimeRequirement {
    target: QueryTarget,
    predicate: String,
    table: &'static str,
    columns: &'static [&'static str],
}

pub fn compile_sqlite_query(
    query: &ValidatedQuery,
) -> Result<CompiledSqlQuery, QueryExecutionError> {
    ensure_relative_dates_resolved(query).map_err(|error| {
        QueryExecutionError::date_resolution(
            query.target,
            "relative-date-resolution",
            error.to_string(),
        )
    })?;
    let mut aliases = AliasAllocator::default();
    let scope = aliases.next_scope(query.target);
    let where_clause = compile_query_match_filter(query, &scope, &mut aliases)?;

    let sql = match query.target {
        QueryTarget::Headings => format!(
            "SELECT
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
                {heading_footnote_section_p},
                {heading_all_tags_json}
             {}
             {}
             ORDER BY {file_path}, {heading_byte_start}, {heading_id}",
            heading_from_clause(&scope),
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
            heading_all_tags_json = scope.heading_col("all_tags_json"),
        ),
        QueryTarget::Links => format!(
            "SELECT
                {link_id},
                {link_file_id},
                {file_path},
                {link_heading_id},
                {link_heading_level},
                {outline_breadcrumbs},
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
             {}
             ORDER BY {file_path}, {link_byte_start}, {link_id}",
            link_from_clause(&scope),
            render_where_clause(where_clause.as_ref()),
            link_id = scope.link_col("id"),
            link_file_id = scope.link_col("file_id"),
            file_path = scope.file_col("path"),
            link_heading_id = scope.link_col("heading_id"),
            link_heading_level = scope.link_heading_col("level"),
            outline_breadcrumbs = scope.outline_col("breadcrumbs_json"),
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
            "SELECT
                {file_id},
                {file_path},
                {file_mtime_ns},
                {file_size},
                {file_content_hash},
                {file_indexed_at},
                {root_id},
                {root_title},
                {root_title_raw}
             {}
             {}
             ORDER BY {file_path}, {file_id}",
            file_from_clause(&scope),
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
    let resolved = resolve_relative_dates(
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

    ensure_body_text_backend_capabilities(connection, &resolved)?;
    ensure_with_time_backend_capabilities(connection, &resolved)?;

    match resolved.target {
        QueryTarget::Headings => execute_headings_query(connection, &resolved),
        QueryTarget::Links => {
            let compiled = compile_sqlite_query(&resolved)?;
            execute_links_query(connection, &compiled)
        }
        QueryTarget::Files => {
            let compiled = compile_sqlite_query(&resolved)?;
            execute_files_query(connection, &compiled)
        }
    }
}

pub fn sqlite_query_validation_options(
    connection: &Connection,
) -> Result<QueryValidationOptions, QueryExecutionError> {
    Ok(QueryValidationOptions {
        body_text_available: sqlite_body_text_available(connection)?,
        regexp_body_matching_supported: false,
    })
}

fn execute_headings_query(
    connection: &Connection,
    query: &ValidatedQuery,
) -> Result<QueryRows, QueryExecutionError> {
    let compiled = compile_sqlite_query(query)?;
    let mut rows = execute_heading_rows_query(connection, &compiled)?
        .into_iter()
        .map(HeadingQueryMatch::Heading)
        .collect::<Vec<_>>();

    let root_compiled = compile_heading_root_file_query(query)?;
    rows.extend(
        execute_file_rows_query(connection, &root_compiled)?
            .into_iter()
            .map(HeadingQueryMatch::File),
    );

    rows.sort_by(compare_heading_query_matches);
    Ok(QueryRows::Headings(rows))
}

fn execute_heading_rows_query(
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
                priority: priority.and_then(|value| value.chars().next()),
                scheduled_raw: row.get(13)?,
                scheduled_ts: row.get(14)?,
                deadline_raw: row.get(15)?,
                deadline_ts: row.get(16)?,
                closed_raw: row.get(17)?,
                closed_ts: row.get(18)?,
                archivedp: row.get::<_, i64>(19)? != 0,
                footnote_section_p: row.get::<_, i64>(20)? != 0,
                all_tags_json: row.get(21)?,
            })
        })
        .map_err(|source| QueryExecutionError::database(compiled.target, "query", source))?;
    rows.collect::<Result<Vec<_>, _>>()
        .map_err(|source| QueryExecutionError::database(compiled.target, "collect", source))
}

fn execute_links_query(
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
                heading_breadcrumbs_json: row.get(5)?,
                source_context: row.get(6)?,
                format: row.get(7)?,
                link_type: row.get(8)?,
                raw: row.get(9)?,
                raw_target: row.get(10)?,
                raw_description: row.get(11)?,
                path: row.get(12)?,
                search_option: row.get(13)?,
                path_absolute: row.get(14)?,
                target_file_id: row.get(15)?,
                target_heading_id: row.get(16)?,
                target_custom_id: row.get(17)?,
                target_id: row.get(18)?,
                resolution_status: row.get(19)?,
                resolution_diagnostic: row.get(20)?,
                byte_start: row.get(21)?,
                byte_end: row.get(22)?,
                line: row.get(23)?,
            })
        })
        .map_err(|source| QueryExecutionError::database(compiled.target, "query", source))?;
    rows.collect::<Result<Vec<_>, _>>()
        .map(QueryRows::Links)
        .map_err(|source| QueryExecutionError::database(compiled.target, "collect", source))
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
            })
        })
        .map_err(|source| QueryExecutionError::database(compiled.target, "query", source))?;
    rows.collect::<Result<Vec<_>, _>>()
        .map_err(|source| QueryExecutionError::database(compiled.target, "collect", source))
}

fn ensure_with_time_backend_capabilities(
    connection: &Connection,
    query: &ValidatedQuery,
) -> Result<(), QueryExecutionError> {
    let requirements = collect_with_time_requirements(query);
    if requirements.is_empty() {
        return Ok(());
    }

    let mut headings_columns = None;
    let mut timestamps_columns = None;

    for requirement in requirements {
        let available_columns = match requirement.table {
            "headings" => {
                if headings_columns.is_none() {
                    headings_columns = Some(load_table_columns(connection, "headings").map_err(
                        |source| {
                            QueryExecutionError::database(
                                requirement.target,
                                "inspect schema",
                                source,
                            )
                        },
                    )?);
                }
                headings_columns
                    .as_ref()
                    .expect("headings columns should be loaded")
            }
            "timestamps" => {
                if timestamps_columns.is_none() {
                    timestamps_columns = Some(
                        load_table_columns(connection, "timestamps").map_err(|source| {
                            QueryExecutionError::database(
                                requirement.target,
                                "inspect schema",
                                source,
                            )
                        })?,
                    );
                }
                timestamps_columns
                    .as_ref()
                    .expect("timestamps columns should be loaded")
            }
            _ => unreachable!("unexpected with-time requirement table"),
        };

        let missing = requirement
            .columns
            .iter()
            .copied()
            .filter(|column| !available_columns.contains(*column))
            .collect::<Vec<_>>();
        if !missing.is_empty() {
            let missing_columns = missing
                .iter()
                .map(|column| format!("{}.{}", requirement.table, column))
                .collect::<Vec<_>>()
                .join(", ");
            return Err(QueryExecutionError::unsupported_backend_feature(
                requirement.target,
                requirement.predicate,
                format!(
                    ":with-time requires backend column(s) {missing_columns}, but the SQLite metadata backend schema does not provide them"
                ),
            ));
        }
    }

    Ok(())
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

fn collect_with_time_requirements(query: &ValidatedQuery) -> Vec<WithTimeRequirement> {
    let mut requirements = Vec::new();
    if let Some(predicate) = &query.predicate {
        collect_with_time_requirements_from_expr(predicate, query.target, &mut requirements);
    }
    requirements
}

fn collect_with_time_requirements_from_expr(
    expr: &ValidatedExpr,
    target: QueryTarget,
    requirements: &mut Vec<WithTimeRequirement>,
) {
    match expr {
        ValidatedExpr::And(children) | ValidatedExpr::Or(children) => {
            for child in children {
                collect_with_time_requirements_from_expr(child, target, requirements);
            }
        }
        ValidatedExpr::Not(child) => {
            collect_with_time_requirements_from_expr(child, target, requirements);
        }
        ValidatedExpr::Predicate(predicate) => {
            collect_with_time_requirements_from_predicate(predicate, target, requirements);
        }
    }
}

fn collect_with_time_requirements_from_predicate(
    predicate: &ValidatedPredicate,
    target: QueryTarget,
    requirements: &mut Vec<WithTimeRequirement>,
) {
    if option_present(&predicate.options, "with-time") {
        let requirement = match predicate.name.as_str() {
            "scheduled" => Some(WithTimeRequirement {
                target,
                predicate: predicate.name.clone(),
                table: "headings",
                columns: &["scheduled_has_time"],
            }),
            "deadline" => Some(WithTimeRequirement {
                target,
                predicate: predicate.name.clone(),
                table: "headings",
                columns: &["deadline_has_time"],
            }),
            "closed" => Some(WithTimeRequirement {
                target,
                predicate: predicate.name.clone(),
                table: "headings",
                columns: &["closed_has_time"],
            }),
            "planning" => Some(WithTimeRequirement {
                target,
                predicate: predicate.name.clone(),
                table: "headings",
                columns: &["scheduled_has_time", "deadline_has_time", "closed_has_time"],
            }),
            "ts" | "ts-active" | "ts-inactive" => Some(WithTimeRequirement {
                target,
                predicate: predicate.name.clone(),
                table: "timestamps",
                columns: &["has_time"],
            }),
            _ => None,
        };
        if let Some(requirement) = requirement {
            requirements.push(requirement);
        }
    }

    for arg in &predicate.args {
        if let ValidatedArg::NestedQuery(query) = arg {
            if let Some(predicate) = &query.predicate {
                collect_with_time_requirements_from_expr(predicate, query.target, requirements);
            }
        }
    }
}

fn load_table_columns(
    connection: &Connection,
    table: &str,
) -> Result<HashSet<String>, rusqlite::Error> {
    let pragma = format!("PRAGMA table_info({table})");
    let mut statement = connection.prepare(&pragma)?;
    let rows = statement.query_map([], |row| row.get::<_, String>(1))?;
    rows.collect::<Result<HashSet<_>, _>>()
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
        QueryTarget::Headings => Some(sql_literal(&match scope.heading_match_kind {
            HeadingMatchKind::RealHeading => format!("({} > 0)", scope.heading_col("level")),
            HeadingMatchKind::RootFile => format!("({} = 0)", scope.heading_col("level")),
        })),
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
        "file-path" => compile_text_predicate(
            QueryTarget::Headings,
            &scope.file_col("path"),
            predicate,
            false,
        ),
        "file-dir" => compile_text_predicate(
            QueryTarget::Headings,
            &sqlite_file_dir_expr(&scope.file_col("path")),
            predicate,
            false,
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
            None,
            &predicate.options,
            TemporalUnit::Nanoseconds,
            true,
        ),
        "tags" => compile_heading_tags_predicate(scope, predicate),
        "property" => compile_heading_property_predicate(scope, predicate),
        "keyword" => {
            compile_keyword_predicate(QueryTarget::Headings, predicate, &scope.root_col("id"))
        }
        "scheduled" => compile_date_predicate(
            QueryTarget::Headings,
            "scheduled",
            &scope.heading_col("scheduled_ts"),
            Some(&scope.heading_col("scheduled_has_time")),
            &predicate.options,
            TemporalUnit::Seconds,
            true,
        ),
        "deadline" => compile_date_predicate(
            QueryTarget::Headings,
            "deadline",
            &scope.heading_col("deadline_ts"),
            Some(&scope.heading_col("deadline_has_time")),
            &predicate.options,
            TemporalUnit::Seconds,
            true,
        ),
        "closed" => compile_date_predicate(
            QueryTarget::Headings,
            "closed",
            &scope.heading_col("closed_ts"),
            Some(&scope.heading_col("closed_has_time")),
            &predicate.options,
            TemporalUnit::Seconds,
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
        "outline-contains" | "outline-sequence" => Err(QueryExecutionError::unsupported_predicate(
            QueryTarget::Headings,
            predicate.name.as_str(),
            format!(
                "predicate {} is not supported by the SQLite metadata backend",
                predicate.name
            ),
        )),
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

    if option_bool(&predicate.options, "regexp")? {
        return Err(QueryExecutionError::unsupported_backend_feature(
            QueryTarget::Headings,
            "has-text",
            "predicate has-text with :regexp t is not supported by the SQLite metadata backend",
        ));
    }

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
                  AND INSTR(LOWER(heading_bodies.body_text), LOWER(?)) > 0
            )",
            scope.heading_col("id")
        ));
        params.push(QueryParam::Text(value));
    }

    Ok(SqlFragment {
        sql: format!("({})", parts.join(" AND ")),
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
        "file-path" => compile_text_predicate(
            QueryTarget::Files,
            &scope.file_col("path"),
            predicate,
            false,
        ),
        "file-dir" => compile_text_predicate(
            QueryTarget::Files,
            &sqlite_file_dir_expr(&scope.file_col("path")),
            predicate,
            false,
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
            None,
            &predicate.options,
            TemporalUnit::Nanoseconds,
            true,
        ),
        "tags" => compile_file_tags_predicate(scope, predicate),
        "property" => compile_file_property_predicate(scope, predicate),
        "keyword" => {
            compile_keyword_predicate(QueryTarget::Files, predicate, &scope.root_col("id"))
        }
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
    match predicate.args.as_slice() {
        [ValidatedArg::Scalar(QueryValue::Symbol(comparator)), ValidatedArg::Scalar(QueryValue::String(value))]
            if matches!(comparator.as_str(), "<" | "<=" | ">" | ">=") =>
        {
            Ok(SqlFragment {
                sql: format!("({column} IS NOT NULL AND {column} {} ?)", comparator),
                params: vec![QueryParam::Text(value.clone())],
            })
        }
        args => {
            let strings = args
                .iter()
                .map(arg_as_string)
                .collect::<Result<Vec<_>, _>>()
                .map_err(|message| {
                    QueryExecutionError::unsupported_backend_feature(target, "priority", message)
                })?;
            let placeholders = vec!["?"; strings.len()].join(", ");
            Ok(SqlFragment {
                sql: format!("({column} IN ({placeholders}))"),
                params: strings.into_iter().map(QueryParam::Text).collect(),
            })
        }
    }
}

fn compile_text_predicate(
    target: QueryTarget,
    column: &str,
    predicate: &ValidatedPredicate,
    allow_nulls: bool,
) -> Result<SqlFragment, QueryExecutionError> {
    if option_bool(&predicate.options, "regexp")? {
        return Err(QueryExecutionError::unsupported_backend_feature(
            target,
            predicate.name.as_str(),
            format!(
                "predicate {} with :regexp t is not supported by the SQLite metadata backend",
                predicate.name
            ),
        ));
    }

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
        if exact {
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
) -> Result<SqlFragment, QueryExecutionError> {
    if option_bool(&predicate.options, "regexp")? {
        return Err(QueryExecutionError::unsupported_backend_feature(
            QueryTarget::Headings,
            "tags",
            "heading tags with :regexp t are not supported by the SQLite metadata backend",
        ));
    }
    if !option_bool_with_default(&predicate.options, "inherit", true)? {
        return compile_tags_exists(QueryTarget::Headings, predicate, &scope.heading_col("id"));
    }

    compile_heading_effective_tags_exists(scope, predicate, true)
}

fn compile_heading_effective_tags_exists(
    scope: &QueryScope,
    predicate: &ValidatedPredicate,
    include_root: bool,
) -> Result<SqlFragment, QueryExecutionError> {
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

    if match_all {
        let mut parts = Vec::with_capacity(tags.len());
        let mut params = Vec::with_capacity(tags.len());
        for tag in tags {
            parts.push(heading_lineage_exists_sql(
                &scope.heading_col("id"),
                &scope.heading_col("parent_id"),
                &scope.heading_col("level"),
                "tags",
                "matched_tags",
                "matched_tags.tag = ?",
                include_root,
            ));
            params.push(QueryParam::Text(tag));
        }
        return Ok(SqlFragment {
            sql: format!("({})", parts.join(" AND ")),
            params,
        });
    }

    let placeholders = vec!["?"; tags.len()].join(", ");
    Ok(SqlFragment {
        sql: heading_lineage_exists_sql(
            &scope.heading_col("id"),
            &scope.heading_col("parent_id"),
            &scope.heading_col("level"),
            "tags",
            "matched_tags",
            &format!("matched_tags.tag IN ({placeholders})"),
            include_root,
        ),
        params: tags.into_iter().map(QueryParam::Text).collect(),
    })
}

fn compile_file_tags_predicate(
    scope: &QueryScope,
    predicate: &ValidatedPredicate,
) -> Result<SqlFragment, QueryExecutionError> {
    if option_bool(&predicate.options, "regexp")? {
        return Err(QueryExecutionError::unsupported_backend_feature(
            QueryTarget::Files,
            "tags",
            "file tags with :regexp t are not supported by the SQLite metadata backend",
        ));
    }
    compile_tags_exists(QueryTarget::Files, predicate, &scope.root_col("id"))
}

fn compile_tags_exists(
    target: QueryTarget,
    predicate: &ValidatedPredicate,
    heading_id_sql: &str,
) -> Result<SqlFragment, QueryExecutionError> {
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

    let mut parts = Vec::new();
    let mut params = Vec::new();
    if match_all {
        for tag in tags {
            parts.push(format!(
                "EXISTS (SELECT 1 FROM tags WHERE tags.heading_id = {heading_id_sql} AND tags.tag = ?)"
            ));
            params.push(QueryParam::Text(tag));
        }
        return Ok(SqlFragment {
            sql: format!("({})", parts.join(" AND ")),
            params,
        });
    }

    let placeholders = vec!["?"; tags.len()].join(", ");
    params.extend(tags.into_iter().map(QueryParam::Text));
    Ok(SqlFragment {
        sql: format!(
            "(EXISTS (SELECT 1 FROM tags WHERE tags.heading_id = {heading_id_sql} AND tags.tag IN ({placeholders})))"
        ),
        params,
    })
}

fn heading_lineage_exists_sql(
    outer_heading_id_sql: &str,
    outer_parent_id_sql: &str,
    outer_level_sql: &str,
    fact_table: &str,
    fact_alias: &str,
    fact_match_sql: &str,
    include_root: bool,
) -> String {
    let lineage_filter = if include_root {
        String::new()
    } else {
        "lineage.level > 0 AND ".to_string()
    };

    format!(
        "(EXISTS (
            WITH RECURSIVE lineage(id, parent_id, level) AS (
                SELECT {outer_heading_id_sql}, {outer_parent_id_sql}, {outer_level_sql}
                UNION ALL
                SELECT ancestor.id, ancestor.parent_id, ancestor.level
                FROM headings AS ancestor
                INNER JOIN lineage ON lineage.parent_id = ancestor.id
            )
            SELECT 1
            FROM lineage
            INNER JOIN {fact_table} AS {fact_alias} ON {fact_alias}.heading_id = lineage.id
            WHERE {lineage_filter}{fact_match_sql}
        ))"
    )
}

fn compile_heading_property_predicate(
    scope: &QueryScope,
    predicate: &ValidatedPredicate,
) -> Result<SqlFragment, QueryExecutionError> {
    if !option_bool_with_default(&predicate.options, "inherit", true)? {
        return compile_property_exists(QueryTarget::Headings, predicate, &scope.heading_col("id"));
    }

    if option_bool(&predicate.options, "regexp")? {
        return Err(QueryExecutionError::unsupported_backend_feature(
            QueryTarget::Headings,
            "property",
            "property with :regexp t is not supported by the SQLite metadata backend",
        ));
    }

    let key = arg_as_string(&predicate.args[0]).map_err(|message| {
        QueryExecutionError::unsupported_backend_feature(QueryTarget::Headings, "property", message)
    })?;
    let mut fact_match_sql = "matched_properties.key = ? COLLATE NOCASE".to_string();
    let mut params = vec![QueryParam::Text(key)];
    if let Some(value) = predicate.args.get(1) {
        fact_match_sql.push_str(" AND matched_properties.value = ?");
        params.push(QueryParam::Text(arg_as_string(value).map_err(
            |message| {
                QueryExecutionError::unsupported_backend_feature(
                    QueryTarget::Headings,
                    "property",
                    message,
                )
            },
        )?));
    }

    Ok(SqlFragment {
        sql: heading_lineage_exists_sql(
            &scope.heading_col("id"),
            &scope.heading_col("parent_id"),
            &scope.heading_col("level"),
            "properties",
            "matched_properties",
            &fact_match_sql,
            true,
        ),
        params,
    })
}

fn compile_file_property_predicate(
    scope: &QueryScope,
    predicate: &ValidatedPredicate,
) -> Result<SqlFragment, QueryExecutionError> {
    compile_property_exists(QueryTarget::Files, predicate, &scope.root_col("id"))
}

fn compile_property_exists(
    target: QueryTarget,
    predicate: &ValidatedPredicate,
    heading_id_sql: &str,
) -> Result<SqlFragment, QueryExecutionError> {
    if option_bool(&predicate.options, "regexp")? {
        return Err(QueryExecutionError::unsupported_backend_feature(
            target,
            "property",
            "property with :regexp t is not supported by the SQLite metadata backend",
        ));
    }

    let key = arg_as_string(&predicate.args[0]).map_err(|message| {
        QueryExecutionError::unsupported_backend_feature(target, "property", message)
    })?;
    let mut sql = format!(
        "(EXISTS (SELECT 1 FROM properties WHERE properties.heading_id = {heading_id_sql} AND properties.key = ? COLLATE NOCASE"
    );
    let mut params = vec![QueryParam::Text(key)];
    if let Some(value) = predicate.args.get(1) {
        sql.push_str(" AND properties.value = ?");
        params.push(QueryParam::Text(arg_as_string(value).map_err(
            |message| QueryExecutionError::unsupported_backend_feature(target, "property", message),
        )?));
    }
    sql.push_str("))");
    Ok(SqlFragment { sql, params })
}

fn compile_keyword_predicate(
    target: QueryTarget,
    predicate: &ValidatedPredicate,
    heading_id_sql: &str,
) -> Result<SqlFragment, QueryExecutionError> {
    if option_bool(&predicate.options, "regexp")? {
        return Err(QueryExecutionError::unsupported_backend_feature(
            target,
            "keyword",
            "keyword with :regexp t is not supported by the SQLite metadata backend",
        ));
    }

    let key = arg_as_string(&predicate.args[0]).map_err(|message| {
        QueryExecutionError::unsupported_backend_feature(target, "keyword", message)
    })?;
    let mut sql = format!(
        "(EXISTS (SELECT 1 FROM keywords WHERE keywords.heading_id = {heading_id_sql} AND keywords.keyword = ? COLLATE NOCASE"
    );
    let mut params = vec![QueryParam::Text(key)];
    if let Some(value) = predicate.args.get(1) {
        sql.push_str(" AND keywords.value = ?");
        params.push(QueryParam::Text(arg_as_string(value).map_err(
            |message| QueryExecutionError::unsupported_backend_feature(target, "keyword", message),
        )?));
    }
    sql.push_str("))");
    Ok(SqlFragment { sql, params })
}

fn compile_date_predicate(
    _target: QueryTarget,
    _predicate_name: &str,
    column: &str,
    with_time_column: Option<&str>,
    options: &[ValidatedOption],
    unit: TemporalUnit,
    require_not_null_when_unbounded: bool,
) -> Result<SqlFragment, QueryExecutionError> {
    let on = option_value(options, "on");
    let from = option_value(options, "from");
    let to = option_value(options, "to");
    let with_time = option_value(options, "with-time");

    let mut parts = Vec::new();
    let mut params = Vec::new();

    if let Some(value) = on {
        let start = start_bound(value, unit);
        let end = exclusive_end_bound(value, unit);
        parts.push(format!("{column} IS NOT NULL"));
        parts.push(format!("{column} >= {}", start.sql));
        parts.push(format!("{column} < {}", end.sql));
        params.extend(start.params);
        params.extend(end.params);
    } else {
        if let Some(value) = from {
            let bound = start_bound(value, unit);
            parts.push(format!("{column} IS NOT NULL"));
            parts.push(format!("{column} >= {}", bound.sql));
            params.extend(bound.params);
        }
        if let Some(value) = to {
            let bound = exclusive_end_bound(value, unit);
            if from.is_none() {
                parts.push(format!("{column} IS NOT NULL"));
            }
            parts.push(format!("{column} < {}", bound.sql));
            params.extend(bound.params);
        }
    }

    if let Some(with_time) = with_time {
        let with_time_column = with_time_column
            .expect("validator should constrain :with-time to timestamp predicates");
        let explicit_time_value = match with_time {
            QueryValue::Bool(true) => "1",
            QueryValue::Bool(false) => "0",
            _ => unreachable!("validator should constrain boolean options"),
        };
        parts.push(format!("{with_time_column} = {explicit_time_value}"));
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
        Some(&scope.heading_col("scheduled_has_time")),
        &predicate.options,
        TemporalUnit::Seconds,
        true,
    )?;
    let deadline = compile_date_predicate(
        QueryTarget::Headings,
        "deadline",
        &scope.heading_col("deadline_ts"),
        Some(&scope.heading_col("deadline_has_time")),
        &predicate.options,
        TemporalUnit::Seconds,
        true,
    )?;
    let closed = compile_date_predicate(
        QueryTarget::Headings,
        "closed",
        &scope.heading_col("closed_ts"),
        Some(&scope.heading_col("closed_has_time")),
        &predicate.options,
        TemporalUnit::Seconds,
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
        Some("timestamps.has_time"),
        &predicate.options,
        TemporalUnit::Seconds,
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
    let nested_scope = aliases.next_scope(query.target);
    let filter = compile_query_match_filter(query, &nested_scope, aliases)?;

    let (id_column, from_clause) = match query.target {
        QueryTarget::Headings => (
            nested_scope.heading_col("id"),
            heading_from_clause(&nested_scope),
        ),
        QueryTarget::Files => (nested_scope.file_col("id"), file_from_clause(&nested_scope)),
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
    let nested_scope = aliases.next_scope(QueryTarget::Headings);
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
            heading_from_clause(&nested_scope),
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
            link_from_clause(&link_scope),
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
            link_from_clause(&link_scope),
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
            link_from_clause(&link_scope),
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

fn start_bound(value: &QueryValue, unit: TemporalUnit) -> SqlFragment {
    let (seconds_sql, params) = start_seconds_sql(value);
    scale_temporal_sql(seconds_sql, params, unit)
}

fn exclusive_end_bound(value: &QueryValue, unit: TemporalUnit) -> SqlFragment {
    let (seconds_sql, params) = exclusive_end_seconds_sql(value);
    scale_temporal_sql(seconds_sql, params, unit)
}

fn scale_temporal_sql(sql: String, params: Vec<QueryParam>, unit: TemporalUnit) -> SqlFragment {
    match unit {
        TemporalUnit::Seconds => SqlFragment { sql, params },
        TemporalUnit::Nanoseconds => SqlFragment {
            sql: format!("(({sql}) * 1000000000)"),
            params,
        },
    }
}

fn start_seconds_sql(value: &QueryValue) -> (String, Vec<QueryParam>) {
    match value {
        QueryValue::Integer(days) => (
            "CAST(unixepoch('now', 'localtime', 'start of day', printf('%+d days', ?)) AS INTEGER)"
                .to_string(),
            vec![QueryParam::Integer(*days)],
        ),
        QueryValue::Symbol(symbol) if symbol == "today" => (
            "CAST(unixepoch('now', 'localtime', 'start of day') AS INTEGER)".to_string(),
            Vec::new(),
        ),
        QueryValue::String(value) if looks_like_datetime(value) => (
            "CAST(unixepoch(?) AS INTEGER)".to_string(),
            vec![QueryParam::Text(value.clone())],
        ),
        QueryValue::String(value) => (
            "CAST(unixepoch(? || ' 00:00:00') AS INTEGER)".to_string(),
            vec![QueryParam::Text(value.clone())],
        ),
        _ => unreachable!("validator should constrain date option values"),
    }
}

fn exclusive_end_seconds_sql(value: &QueryValue) -> (String, Vec<QueryParam>) {
    match value {
        QueryValue::Integer(days) => (
            "CAST(unixepoch('now', 'localtime', 'start of day', printf('%+d days', ?), '+1 day') AS INTEGER)"
                .to_string(),
            vec![QueryParam::Integer(*days)],
        ),
        QueryValue::Symbol(symbol) if symbol == "today" => (
            "CAST(unixepoch('now', 'localtime', 'start of day', '+1 day') AS INTEGER)"
                .to_string(),
            Vec::new(),
        ),
        QueryValue::String(value) if looks_like_datetime(value) => (
            "(CAST(unixepoch(?) AS INTEGER) + 1)".to_string(),
            vec![QueryParam::Text(value.clone())],
        ),
        QueryValue::String(value) => (
            "CAST(unixepoch(? || ' 00:00:00', '+1 day') AS INTEGER)".to_string(),
            vec![QueryParam::Text(value.clone())],
        ),
        _ => unreachable!("validator should constrain date option values"),
    }
}

fn option_present(options: &[ValidatedOption], name: &str) -> bool {
    options.iter().any(|option| option.name == name)
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

fn looks_like_datetime(value: &str) -> bool {
    value.contains(':') || value.contains('T')
}

fn sql_literal(sql: &str) -> SqlFragment {
    SqlFragment {
        sql: sql.to_string(),
        params: Vec::new(),
    }
}

fn render_where_clause(fragment: Option<&SqlFragment>) -> String {
    match fragment {
        Some(fragment) => format!("WHERE {}", fragment.sql),
        None => String::new(),
    }
}

fn heading_from_clause(scope: &QueryScope) -> String {
    format!(
        "FROM headings AS {}
         INNER JOIN files AS {} ON {}.id = {}.file_id
         INNER JOIN headings AS {} ON {}.file_id = {}.file_id AND {}.level = 0",
        scope.heading_alias,
        scope.file_alias,
        scope.file_alias,
        scope.heading_alias,
        scope.root_alias,
        scope.root_alias,
        scope.heading_alias,
        scope.root_alias
    )
}

fn link_from_clause(scope: &QueryScope) -> String {
    format!(
        "FROM links AS {}
         INNER JOIN files AS {} ON {}.id = {}.file_id
         INNER JOIN headings AS {} ON {}.id = {}.heading_id
         INNER JOIN outline_path AS {} ON {}.heading_id = {}.heading_id",
        scope.link_alias,
        scope.file_alias,
        scope.file_alias,
        scope.link_alias,
        scope.link_heading_alias,
        scope.link_heading_alias,
        scope.link_alias,
        scope.outline_alias,
        scope.outline_alias,
        scope.link_alias
    )
}

fn file_from_clause(scope: &QueryScope) -> String {
    format!(
        "FROM files AS {}
         INNER JOIN headings AS {} ON {}.file_id = {}.id AND {}.level = 0",
        scope.file_alias, scope.root_alias, scope.root_alias, scope.file_alias, scope.root_alias
    )
}

fn compile_heading_root_file_query(
    query: &ValidatedQuery,
) -> Result<CompiledSqlQuery, QueryExecutionError> {
    let mut aliases = AliasAllocator::default();
    let scope = aliases.next_heading_root_scope();
    let where_clause = compile_query_match_filter(query, &scope, &mut aliases)?;

    Ok(CompiledSqlQuery {
        target: QueryTarget::Files,
        sql: format!(
            "SELECT DISTINCT
                {file_id},
                {file_path},
                {file_mtime_ns},
                {file_size},
                {file_content_hash},
                {file_indexed_at},
                {root_id},
                {root_title},
                {root_title_raw}
             {}
             {}
             ORDER BY {file_path}, {file_id}",
            heading_from_clause(&scope),
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
        compile_sqlite_query, execute_sqlite_query, execute_sqlite_query_with_options,
        sqlite_query_validation_options, FileQueryRow, HeadingQueryMatch, HeadingQueryRow,
        LinkQueryRow, QueryExecutionErrorKind, QueryRows,
    };
    use crate::db::{
        open_database, open_in_memory_database_with_schema, DbWriter, FileRecordInput,
        HeadingBodyRecord, HeadingRecord, KeywordRecord, LinkRecord, OutlinePathRecord,
        PropertyRecord, SchemaDefinition, TagRecord, TimestampRecord,
    };
    use crate::query::{
        parse_query, resolve_relative_dates, validate_query, QueryDateResolutionOptions,
        QueryExecutionOptions, QueryTarget, QueryValidationOptions,
    };
    use chrono::NaiveDate;
    use rusqlite::Connection;
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
            regexp_body_matching_supported: false,
        }
    }

    fn validation_options_with_regexp() -> QueryValidationOptions {
        QueryValidationOptions {
            body_text_available: true,
            regexp_body_matching_supported: true,
        }
    }

    fn validated(query: &str) -> crate::query::ValidatedQuery {
        let parsed = parse_query(query).expect("query should parse");
        validate_query(parsed, &validation_options()).expect("query should validate")
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
        assert!(!unavailable.regexp_body_matching_supported);

        DbWriter::set_metadata_flag(
            &connection,
            crate::db::DB_METADATA_BODY_TEXT_AVAILABLE_KEY,
            true,
        )
        .expect("metadata should persist");

        let available =
            sqlite_query_validation_options(&connection).expect("validation options should reload");
        assert!(available.body_text_available);
        assert!(!available.regexp_body_matching_supported);
    }

    #[test]
    fn validation_options_treat_missing_metadata_table_as_body_text_unavailable() {
        let connection = Connection::open_in_memory().expect("database should open");

        let options =
            sqlite_query_validation_options(&connection).expect("validation options should load");
        assert!(!options.body_text_available);
        assert!(!options.regexp_body_matching_supported);
    }

    #[test]
    fn validation_options_treat_missing_heading_bodies_table_as_body_text_unavailable() {
        let connection = reduced_body_text_capability_connection(true, true);

        let options =
            sqlite_query_validation_options(&connection).expect("validation options should load");
        assert!(!options.body_text_available);
        assert!(!options.regexp_body_matching_supported);
    }

    #[test]
    fn validation_options_treat_missing_body_text_metadata_row_as_unavailable() {
        let connection = reduced_body_text_capability_connection(false, true);

        let options =
            sqlite_query_validation_options(&connection).expect("validation options should load");
        assert!(!options.body_text_available);
        assert!(!options.regexp_body_matching_supported);
    }

    #[test]
    fn validation_options_treat_disabled_body_text_metadata_value_as_unavailable() {
        let connection = reduced_body_text_capability_connection(true, false);

        let options =
            sqlite_query_validation_options(&connection).expect("validation options should load");
        assert!(!options.body_text_available);
        assert!(!options.regexp_body_matching_supported);
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
                regexp_body_matching_supported: false,
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
                regexp_body_matching_supported: false,
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
            validated(&format!(r#"(headings (tags "{user_value}" :inherit nil))"#)),
            validated(&format!(r#"(headings (property "OWNER" "{user_value}"))"#)),
            validated(&format!(r#"(files (keyword "AUTHOR" "{user_value}"))"#)),
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
    fn compile_rejects_regex_and_deferred_relation_predicates() {
        for query in [
            validated(r#"(headings (tags "proj-.*" :regexp t))"#),
            validated(r#"(headings (property "OWNER" "A.*" :regexp t))"#),
            validated(r#"(files (keyword "AUTHOR" "A.*" :regexp t))"#),
            validated(r#"(links (link-target "notes.*" :regexp t))"#),
        ] {
            let error = compile_sqlite_query(&query).expect_err("regexp should fail");
            assert_eq!(
                error.kind,
                QueryExecutionErrorKind::UnsupportedBackendFeature
            );
        }

        let parsed = parse_query(r#"(headings (has-text "sqlite.*fts" :regexp t))"#)
            .expect("query should parse");
        let validated_query = validate_query(parsed, &validation_options_with_regexp())
            .expect("query should validate for compile coverage");
        let has_text_error =
            compile_sqlite_query(&validated_query).expect_err("regexp should fail");
        assert_eq!(
            has_text_error.kind,
            QueryExecutionErrorKind::UnsupportedBackendFeature
        );
        assert_eq!(
            has_text_error.message,
            "predicate has-text with :regexp t is not supported by the SQLite metadata backend"
        );

        let relation_query = validated(r#"(headings (outline-sequence "todo" "done"))"#);
        let relation_error =
            compile_sqlite_query(&relation_query).expect_err("relation should fail");
        assert_eq!(
            relation_error.kind,
            QueryExecutionErrorKind::UnsupportedPredicate
        );
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
                priority: Some('A'),
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
                heading_breadcrumbs_json: "[\"Alpha Index\",\"Query Engine\"]".to_string(),
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
                target_heading_id: None,
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
                },
            ])
        );
    }

    #[test]
    fn execution_matches_hierarchy_predicates() {
        let connection = seeded_connection();

        let parent_rows = execute_sqlite_query(&connection, &validated(r#"(headings (parent))"#))
            .expect("parent query should execute");
        assert_eq!(heading_ids(parent_rows), vec![12]);

        let parent_nested_rows = execute_sqlite_query(
            &connection,
            &validated(r#"(headings (parent (headings (title "Query Engine" :exact t))))"#),
        )
        .expect("nested parent query should execute");
        assert_eq!(heading_ids(parent_nested_rows), vec![12]);

        let ancestor_rows = execute_sqlite_query(
            &connection,
            &validated(r#"(headings (ancestors (headings (title "Query Engine" :exact t))))"#),
        )
        .expect("ancestor query should execute");
        assert_eq!(heading_ids(ancestor_rows), vec![12]);

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
        assert_eq!(heading_ids(correlated_rows), vec![11, 12]);
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

        let append_rust_rows = execute_sqlite_query(
            &connection,
            &validated(
                r#"(headings (and (title "Query Engine" :exact t) (property "LANG" "rust" :inherit nil)))"#,
            ),
        )
        .expect("append property query should execute");
        assert_eq!(heading_ids(append_rust_rows), vec![11]);

        let append_emacs_rows = execute_sqlite_query(
            &connection,
            &validated(
                r#"(headings (and (title "Query Engine" :exact t) (property "LANG" "emacs" :inherit nil)))"#,
            ),
        )
        .expect("append property query should execute");
        assert_eq!(heading_ids(append_emacs_rows), vec![11]);

        let correlated_rows = execute_sqlite_query(
            &connection,
            &validated(r#"(headings (property "AREA" "infra"))"#),
        )
        .expect("correlated property query should execute");
        assert_eq!(heading_ids(correlated_rows), vec![11, 12]);
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
                (Some("rust".to_string()), true),
                (Some("emacs".to_string()), true),
            ]
        );
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
        assert_eq!(heading_ids(file_title_rows), vec![11, 12, 13, 14]);
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
            validated(r#"(headings (tags "x' OR 1=1 --" :inherit nil))"#),
            validated(r#"(headings (property "OWNER" "x' OR 1=1 --"))"#),
            validated(r#"(files (keyword "AUTHOR" "x' OR 1=1 --"))"#),
            validated(r#"(headings (links-to (headings (title "x' OR 1=1 --"))))"#),
        ] {
            let compiled = compile_sqlite_query(&query).expect("query should compile");
            assert!(!compiled.sql.contains("1=1"));
            assert!(!compiled.params.is_empty());

            match execute_sqlite_query(&connection, &query).expect("query should execute") {
                QueryRows::Headings(rows) => assert!(rows.is_empty()),
                QueryRows::Files(rows) => assert!(rows.is_empty()),
                other => panic!("unexpected rows for injection query: {other:?}"),
            }
        }
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
    fn compile_with_time_uses_persisted_columns_and_bound_date_params() {
        let scheduled = compile_sqlite_query(&validated(
            r#"(headings (scheduled :from "2026-01-03" :to "2026-01-03" :with-time t))"#,
        ))
        .expect("scheduled query should compile");
        assert!(scheduled.sql.contains("h0.scheduled_has_time = 1"));
        assert_eq!(
            scheduled.params,
            vec![
                super::QueryParam::Text("2026-01-03".to_string()),
                super::QueryParam::Text("2026-01-03".to_string()),
            ]
        );

        let ts_active = compile_sqlite_query(&validated(
            r#"(headings (ts-active :on "2026-01-03" :with-time nil))"#,
        ))
        .expect("ts-active query should compile");
        assert!(ts_active.sql.contains("timestamps.has_time = 0"));
        assert_eq!(
            ts_active.params,
            vec![
                super::QueryParam::Text("2026-01-03".to_string()),
                super::QueryParam::Text("2026-01-03".to_string()),
                super::QueryParam::Text("active".to_string()),
            ]
        );
    }

    #[test]
    fn execution_supports_with_time_for_planning_predicates() {
        let connection = with_time_test_connection();

        assert_eq!(
            heading_ids(
                execute_sqlite_query(
                    &connection,
                    &validated(r#"(headings (scheduled :with-time nil))"#),
                )
                .expect("scheduled nil query should execute")
            ),
            vec![201]
        );
        assert_eq!(
            heading_ids(
                execute_sqlite_query(
                    &connection,
                    &validated(r#"(headings (scheduled :with-time t))"#),
                )
                .expect("scheduled timed query should execute")
            ),
            vec![202, 203]
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
                execute_sqlite_query(
                    &connection,
                    &validated(r#"(headings (deadline :with-time nil))"#),
                )
                .expect("deadline nil query should execute")
            ),
            vec![205]
        );
        assert_eq!(
            heading_ids(
                execute_sqlite_query(
                    &connection,
                    &validated(r#"(headings (deadline :with-time t))"#),
                )
                .expect("deadline timed query should execute")
            ),
            vec![206, 207]
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
                execute_sqlite_query(
                    &connection,
                    &validated(r#"(headings (closed :with-time nil))"#),
                )
                .expect("closed nil query should execute")
            ),
            vec![209]
        );
        assert_eq!(
            heading_ids(
                execute_sqlite_query(
                    &connection,
                    &validated(r#"(headings (closed :with-time t))"#),
                )
                .expect("closed timed query should execute")
            ),
            vec![210, 211]
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
                execute_sqlite_query(
                    &connection,
                    &validated(r#"(headings (planning :with-time nil))"#),
                )
                .expect("planning nil query should execute")
            ),
            vec![201, 205, 209]
        );
        assert_eq!(
            heading_ids(
                execute_sqlite_query(
                    &connection,
                    &validated(r#"(headings (planning :with-time t))"#),
                )
                .expect("planning timed query should execute")
            ),
            vec![202, 203, 206, 207, 210, 211]
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
    fn execution_supports_with_time_for_generic_timestamp_predicates() {
        let connection = with_time_test_connection();

        assert_eq!(
            heading_ids(
                execute_sqlite_query(&connection, &validated(r#"(headings (ts))"#))
                    .expect("ts query should execute")
            ),
            vec![213, 214, 215, 216, 217, 218, 219, 220]
        );
        assert_eq!(
            heading_ids(
                execute_sqlite_query(&connection, &validated(r#"(headings (ts :with-time nil))"#),)
                    .expect("ts nil query should execute")
            ),
            vec![213, 217]
        );
        assert_eq!(
            heading_ids(
                execute_sqlite_query(&connection, &validated(r#"(headings (ts :with-time t))"#),)
                    .expect("ts timed query should execute")
            ),
            vec![214, 215, 218, 219]
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
                execute_sqlite_query(
                    &connection,
                    &validated(r#"(headings (ts-active :with-time nil))"#),
                )
                .expect("ts-active nil query should execute")
            ),
            vec![213]
        );
        assert_eq!(
            heading_ids(
                execute_sqlite_query(
                    &connection,
                    &validated(r#"(headings (ts-active :with-time t))"#),
                )
                .expect("ts-active timed query should execute")
            ),
            vec![214, 215]
        );

        assert_eq!(
            heading_ids(
                execute_sqlite_query(&connection, &validated(r#"(headings (ts-inactive))"#),)
                    .expect("ts-inactive query should execute")
            ),
            vec![217, 218, 219, 220]
        );
        assert_eq!(
            heading_ids(
                execute_sqlite_query(
                    &connection,
                    &validated(r#"(headings (ts-inactive :with-time nil))"#),
                )
                .expect("ts-inactive nil query should execute")
            ),
            vec![217]
        );
        assert_eq!(
            heading_ids(
                execute_sqlite_query(
                    &connection,
                    &validated(r#"(headings (ts-inactive :with-time t))"#),
                )
                .expect("ts-inactive timed query should execute")
            ),
            vec![218, 219]
        );
    }

    #[test]
    fn execution_composes_with_time_with_date_bounds() {
        let connection = with_time_test_connection();

        assert_eq!(
            heading_ids(
                execute_sqlite_query(
                    &connection,
                    &validated(r#"(headings (scheduled :with-time t :on "2026-01-03"))"#),
                )
                .expect("scheduled composition query should execute")
            ),
            vec![202, 203]
        );
        assert_eq!(
            heading_ids(
                execute_sqlite_query(
                    &connection,
                    &validated(r#"(headings (ts-active :with-time t :to "2026-01-03 09:15"))"#),
                )
                .expect("ts-active composition query should execute")
            ),
            vec![214, 215]
        );
    }

    #[test]
    fn execution_returns_clear_error_when_planning_with_time_columns_are_missing() {
        let connection = reduced_planning_with_time_schema_connection();
        let error = execute_sqlite_query(
            &connection,
            &validated(r#"(headings (planning :with-time t))"#),
        )
        .expect_err("planning with-time query should fail on reduced schema");

        assert_eq!(
            error.kind,
            QueryExecutionErrorKind::UnsupportedBackendFeature
        );
        assert!(error.to_string().contains("headings.scheduled_has_time"));
        assert!(!error.to_string().contains("no such column"));
    }

    #[test]
    fn execution_returns_clear_error_when_generic_with_time_column_is_missing() {
        let connection = reduced_generic_with_time_schema_connection();
        let error =
            execute_sqlite_query(&connection, &validated(r#"(headings (ts :with-time t))"#))
                .expect_err("generic with-time query should fail on reduced schema");

        assert_eq!(
            error.kind,
            QueryExecutionErrorKind::UnsupportedBackendFeature
        );
        assert!(error.to_string().contains("timestamps.has_time"));
        assert!(!error.to_string().contains("no such column"));
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
        let date_only = compile_sqlite_query(&validated(
            r#"(headings (scheduled :from "2026-01-03" :to "2026-01-03"))"#,
        ))
        .expect("date-only query should compile");
        let datetime = compile_sqlite_query(&validated(
            r#"(headings (scheduled :from "2026-01-03 09:15" :to "2026-01-03 09:15"))"#,
        ))
        .expect("datetime query should compile");

        assert_eq!(
            date_only.params,
            vec![
                super::QueryParam::Text("2026-01-03".to_string()),
                super::QueryParam::Text("2026-01-03".to_string()),
            ]
        );
        assert!(date_only.sql.contains("? || ' 00:00:00'"));
        assert!(date_only.sql.contains("? || ' 00:00:00', '+1 day'"));

        assert_eq!(
            datetime.params,
            vec![
                super::QueryParam::Text("2026-01-03 09:15".to_string()),
                super::QueryParam::Text("2026-01-03 09:15".to_string()),
            ]
        );
        assert!(!datetime.sql.contains("? || ' 00:00:00'"));
        assert!(datetime.sql.contains("CAST(unixepoch(?) AS INTEGER)"));
        assert!(datetime.sql.contains("(CAST(unixepoch(?) AS INTEGER) + 1)"));
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
        let compiled = compile_sqlite_query(&resolved).expect("query should compile");

        assert!(!compiled.sql.contains("localtime"));
        assert_eq!(
            compiled.params,
            vec![
                super::QueryParam::Text("2026-01-03".to_string()),
                super::QueryParam::Text("2026-01-04".to_string()),
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
                    all_tags_json: "[]".to_string(),
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

    fn with_time_test_connection() -> Connection {
        let mut connection = open_in_memory_database_with_schema(&SchemaDefinition::default())
            .expect("database should open");
        let file = FileRecordInput {
            path: Path::new("/tmp/with-time.org").to_path_buf(),
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
        .expect("with-time fixture should seed");

        connection
    }

    fn reduced_planning_with_time_schema_connection() -> Connection {
        let connection = Connection::open_in_memory().expect("reduced schema should open");
        connection
            .execute_batch(
                r#"
CREATE TABLE files (
    id              INTEGER PRIMARY KEY,
    path            TEXT NOT NULL,
    mtime_ns        INTEGER NOT NULL,
    size            INTEGER NOT NULL,
    content_hash    TEXT,
    indexed_at      INTEGER
);

CREATE TABLE headings (
    id                  INTEGER PRIMARY KEY,
    file_id             INTEGER NOT NULL,
    parent_id           INTEGER,
    level               INTEGER NOT NULL,
    line_number         INTEGER,
    byte_start          INTEGER NOT NULL,
    byte_end            INTEGER NOT NULL,
    title               TEXT NOT NULL,
    title_raw           TEXT,
    todo_keyword        TEXT,
    todo_type           TEXT,
    priority            TEXT,
    scheduled_raw       TEXT,
    scheduled_ts        INTEGER,
    deadline_raw        TEXT,
    deadline_ts         INTEGER,
    closed_raw          TEXT,
    closed_ts           INTEGER,
    archivedp           INTEGER NOT NULL DEFAULT 0,
    footnote_section_p  INTEGER NOT NULL DEFAULT 0,
    all_tags_json       TEXT NOT NULL DEFAULT '[]'
);

CREATE TABLE timestamps (
    id              INTEGER PRIMARY KEY,
    heading_id      INTEGER NOT NULL,
    role            TEXT,
    has_time        INTEGER,
    start_ts        INTEGER,
    end_ts          INTEGER,
    type            TEXT,
    range_type      TEXT,
    raw_value       TEXT NOT NULL,
    byte_start      INTEGER NOT NULL,
    byte_end        INTEGER NOT NULL,
    line_number     INTEGER
);
"#,
            )
            .expect("reduced planning schema should initialize");
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

    fn reduced_generic_with_time_schema_connection() -> Connection {
        let connection = Connection::open_in_memory().expect("reduced schema should open");
        connection
            .execute_batch(
                r#"
CREATE TABLE files (
    id              INTEGER PRIMARY KEY,
    path            TEXT NOT NULL,
    mtime_ns        INTEGER NOT NULL,
    size            INTEGER NOT NULL,
    content_hash    TEXT,
    indexed_at      INTEGER
);

CREATE TABLE headings (
    id                  INTEGER PRIMARY KEY,
    file_id             INTEGER NOT NULL,
    parent_id           INTEGER,
    level               INTEGER NOT NULL,
    line_number         INTEGER,
    byte_start          INTEGER NOT NULL,
    byte_end            INTEGER NOT NULL,
    title               TEXT NOT NULL,
    title_raw           TEXT,
    todo_keyword        TEXT,
    todo_type           TEXT,
    priority            TEXT,
    scheduled_raw       TEXT,
    scheduled_ts        INTEGER,
    scheduled_has_time  INTEGER,
    deadline_raw        TEXT,
    deadline_ts         INTEGER,
    deadline_has_time   INTEGER,
    closed_raw          TEXT,
    closed_ts           INTEGER,
    closed_has_time     INTEGER,
    archivedp           INTEGER NOT NULL DEFAULT 0,
    footnote_section_p  INTEGER NOT NULL DEFAULT 0,
    all_tags_json       TEXT NOT NULL DEFAULT '[]'
);

CREATE TABLE timestamps (
    id              INTEGER PRIMARY KEY,
    heading_id      INTEGER NOT NULL,
    role            TEXT,
    start_ts        INTEGER,
    end_ts          INTEGER,
    type            TEXT,
    range_type      TEXT,
    raw_value       TEXT NOT NULL,
    byte_start      INTEGER NOT NULL,
    byte_end        INTEGER NOT NULL,
    line_number     INTEGER
);
"#,
            )
            .expect("reduced generic schema should initialize");
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
            all_tags_json: "[]".to_string(),
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
            _ => unreachable!("unexpected with-time fixture heading"),
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
            all_tags_json: "[]".to_string(),
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
            mtime_ns: 1_767_398_400_000_000_000,
            size: 100,
            content_hash: None,
            indexed_at: Some(1_767_398_410),
        };
        let beta = FileRecordInput {
            path: beta_path.to_path_buf(),
            mtime_ns: 1_767_484_800_000_000_000,
            size: 120,
            content_hash: None,
            indexed_at: Some(1_767_484_810),
        };
        let gamma = FileRecordInput {
            path: Path::new("/tmp/query-gamma.org").to_path_buf(),
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
                    all_tags_json: "[\"archive\"]".to_string(),
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
                    all_tags_json: "[\"filetag\"]".to_string(),
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
                        priority: Some('A'),
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
                        all_tags_json: "[\"filetag\",\"project\"]".to_string(),
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
                        all_tags_json: "[\"filetag\",\"project\",\"urgent\"]".to_string(),
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
                        all_tags_json: "[\"filetag\",\"misc\"]".to_string(),
                    },
                    HeadingRecord {
                        id: Some(14),
                        file_id,
                        parent_id: Some(root_id),
                        level: 1,
                        line_number: Some(10),
                        byte_start: 96,
                        byte_end: 130,
                        title: "Statistic Cookies".to_string(),
                        title_raw: Some("REVIEW [#B] Statistic Cookies [0/1]".to_string()),
                        todo_keyword: Some("REVIEW".to_string()),
                        todo_type: Some("open".to_string()),
                        priority: Some('B'),
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
                        all_tags_json: "[\"filetag\"]".to_string(),
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
                        parent_id: Some(10),
                        depth: 1,
                        materialized_path: "0000.0003".to_string(),
                        breadcrumbs_json: "[\"Alpha Index\",\"Statistic Cookies\"]".to_string(),
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
                        append: true,
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
                        heading_id: 12,
                        key: "OWNER".to_string(),
                        value: Some("Bob".to_string()),
                        source: "property_drawer".to_string(),
                        append: false,
                        line_number: Some(7),
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
                     resolution_status = 'resolved'
                 WHERE id = 100",
                rusqlite::params![beta_path.to_string_lossy().to_string(), beta_file_id],
            )
            .expect("link target should update");
            Ok(())
        })
        .expect("alpha file should seed");

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
                    all_tags_json: "[]".to_string(),
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
                    all_tags_json: "[]".to_string(),
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
                  deadline_ts, closed_raw, closed_ts, archivedp, footnote_section_p, all_tags_json)
                 VALUES
                 (21, ?1, 20, 1, 3, 10, 40, 'Beta Target', 'Beta Target',
                  NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, 0, 0, '[\"archive\",\"target\"]')",
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
                     resolution_status = 'resolved'
                 WHERE id = 102",
                rusqlite::params![beta_path.to_string_lossy().to_string(), beta_file_id],
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
                     resolution_status = 'resolved'
                 WHERE id = 104",
                rusqlite::params![alpha_path.to_string_lossy().to_string(), alpha_file_id],
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
