use std::fmt;

use rusqlite::{
    params_from_iter,
    types::{ToSqlOutput, Value},
    Connection, ToSql,
};
use serde::Serialize;

use super::{
    QueryTarget, QueryValue, ValidatedArg, ValidatedExpr, ValidatedOption, ValidatedPredicate,
    ValidatedQuery,
};

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
    Headings(Vec<HeadingQueryRow>),
    Links(Vec<LinkQueryRow>),
    Files(Vec<FileQueryRow>),
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
    pub title_raw: String,
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
    pub root_title_raw: String,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum QueryExecutionErrorKind {
    UnsupportedPredicate,
    UnsupportedBackendFeature,
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
}

pub fn compile_sqlite_query(
    query: &ValidatedQuery,
) -> Result<CompiledSqlQuery, QueryExecutionError> {
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
    let compiled = compile_sqlite_query(query)?;
    match compiled.target {
        QueryTarget::Headings => execute_headings_query(connection, &compiled),
        QueryTarget::Links => execute_links_query(connection, &compiled),
        QueryTarget::Files => execute_files_query(connection, &compiled),
    }
}

fn execute_headings_query(
    connection: &Connection,
    compiled: &CompiledSqlQuery,
) -> Result<QueryRows, QueryExecutionError> {
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
        .map(QueryRows::Headings)
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
        .map(QueryRows::Files)
        .map_err(|source| QueryExecutionError::database(compiled.target, "collect", source))
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
        QueryTarget::Headings => Some(sql_literal(&format!(
            "({} > 0)",
            scope.heading_col("level")
        ))),
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
        "title" => compile_text_predicate(
            QueryTarget::Headings,
            &scope.heading_col("title"),
            predicate,
            false,
        ),
        "level" => compile_level_predicate(scope, predicate),
        "file-path" => compile_text_predicate(
            QueryTarget::Headings,
            &scope.file_col("path"),
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
            &predicate.options,
            TemporalUnit::Seconds,
            true,
        ),
        "deadline" => compile_date_predicate(
            QueryTarget::Headings,
            "deadline",
            &scope.heading_col("deadline_ts"),
            &predicate.options,
            TemporalUnit::Seconds,
            true,
        ),
        "closed" => compile_date_predicate(
            QueryTarget::Headings,
            "closed",
            &scope.heading_col("closed_ts"),
            &predicate.options,
            TemporalUnit::Seconds,
            true,
        ),
        "planning" => compile_planning_predicate(scope, predicate),
        "ts" => compile_timestamp_exists_predicate(scope, predicate, None),
        "ts-active" => compile_timestamp_exists_predicate(scope, predicate, Some("active")),
        "ts-inactive" => compile_timestamp_exists_predicate(scope, predicate, Some("inactive")),
        "parent" => compile_heading_hierarchy_predicate(
            scope,
            aliases,
            predicate,
            HeadingHierarchyRelation::Parent,
        ),
        "ancestors" => compile_heading_hierarchy_predicate(
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
        "has-text" | "outline-contains" | "outline-sequence" | "file-name" | "file-dir" => {
            Err(QueryExecutionError::unsupported_predicate(
                QueryTarget::Headings,
                predicate.name.as_str(),
                format!(
                    "predicate {} is not supported by the SQLite metadata backend",
                    predicate.name
                ),
            ))
        }
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
        "file-path" => compile_text_predicate(
            QueryTarget::Files,
            &scope.file_col("path"),
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
        "has-text" | "outline-contains" | "outline-sequence" | "file-name" | "file-dir"
        | "parent" | "ancestors" | "children" | "descendants" => {
            Err(QueryExecutionError::unsupported_predicate(
                QueryTarget::Files,
                predicate.name.as_str(),
                format!(
                    "predicate {} is not supported by the SQLite metadata backend",
                    predicate.name
                ),
            ))
        }
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

    let with_root = option_bool_with_default(&predicate.options, "with-root", true)?;
    compile_heading_effective_tags_exists(scope, predicate, with_root)
}

fn compile_heading_effective_tags_exists(
    scope: &QueryScope,
    predicate: &ValidatedPredicate,
    with_root: bool,
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
                with_root,
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
            with_root,
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
    with_root: bool,
) -> String {
    let lineage_filter = if with_root {
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
    let with_root = option_bool_with_default(&predicate.options, "with-root", true)?;
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
            with_root,
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
    target: QueryTarget,
    predicate_name: &str,
    column: &str,
    options: &[ValidatedOption],
    unit: TemporalUnit,
    require_not_null_when_unbounded: bool,
) -> Result<SqlFragment, QueryExecutionError> {
    if option_present(options, "with-time") {
        return Err(QueryExecutionError::unsupported_backend_feature(
            target,
            predicate_name,
            format!(
                "{predicate_name} with :with-time is not supported by the SQLite metadata backend"
            ),
        ));
    }

    let on = option_value(options, "on");
    let from = option_value(options, "from");
    let to = option_value(options, "to");

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
    if option_present(&predicate.options, "with-time") {
        return Err(QueryExecutionError::unsupported_backend_feature(
            QueryTarget::Headings,
            "planning",
            "planning with :with-time is not supported by the SQLite metadata backend",
        ));
    }

    if option_value(&predicate.options, "on").is_none()
        && option_value(&predicate.options, "from").is_none()
        && option_value(&predicate.options, "to").is_none()
    {
        return Ok(sql_literal(&format!(
            "({} IS NOT NULL OR {} IS NOT NULL OR {} IS NOT NULL)",
            scope.heading_col("scheduled_ts"),
            scope.heading_col("deadline_ts"),
            scope.heading_col("closed_ts")
        )));
    }

    let scheduled = compile_date_predicate(
        QueryTarget::Headings,
        "scheduled",
        &scope.heading_col("scheduled_ts"),
        &predicate.options,
        TemporalUnit::Seconds,
        true,
    )?;
    let deadline = compile_date_predicate(
        QueryTarget::Headings,
        "deadline",
        &scope.heading_col("deadline_ts"),
        &predicate.options,
        TemporalUnit::Seconds,
        true,
    )?;
    let closed = compile_date_predicate(
        QueryTarget::Headings,
        "closed",
        &scope.heading_col("closed_ts"),
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
    if option_present(&predicate.options, "with-time") {
        return Err(QueryExecutionError::unsupported_backend_feature(
            QueryTarget::Headings,
            predicate.name.as_str(),
            format!(
                "{} with :with-time is not supported by the SQLite metadata backend",
                predicate.name
            ),
        ));
    }

    let date_fragment = compile_date_predicate(
        QueryTarget::Headings,
        predicate.name.as_str(),
        "timestamps.start_ts",
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
        compile_sqlite_query, execute_sqlite_query, FileQueryRow, HeadingQueryRow, LinkQueryRow,
        QueryExecutionErrorKind, QueryRows,
    };
    use crate::db::{
        open_database, open_in_memory_database_with_schema, DbWriter, FileRecordInput,
        HeadingRecord, KeywordRecord, LinkRecord, OutlinePathRecord, PropertyRecord,
        SchemaDefinition, TagRecord, TimestampRecord,
    };
    use crate::query::{parse_query, validate_query, QueryValidationOptions};
    use rusqlite::Connection;
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

    fn validated(query: &str) -> crate::query::ValidatedQuery {
        let parsed = parse_query(query).expect("query should parse");
        validate_query(parsed, &validation_options()).expect("query should validate")
    }

    #[test]
    fn compile_uses_placeholders_instead_of_inlining_user_payload() {
        let user_value = "x' OR 1=1 --";
        for query in [
            validated(&format!(r#"(headings (title "{user_value}"))"#)),
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

        let relation_query = validated(r#"(headings (outline-sequence "todo" "done"))"#);
        let relation_error =
            compile_sqlite_query(&relation_query).expect_err("relation should fail");
        assert_eq!(
            relation_error.kind,
            QueryExecutionErrorKind::UnsupportedPredicate
        );
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
            QueryRows::Headings(vec![HeadingQueryRow {
                id: 11,
                file_id: 2,
                file_path: "/tmp/query-alpha.org".to_string(),
                parent_id: Some(10),
                level: 1,
                line_number: Some(3),
                byte_start: 10,
                byte_end: 40,
                title: "Query Engine".to_string(),
                title_raw: "Query Engine".to_string(),
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
            }])
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
                    root_title_raw: "Alpha Index".to_string(),
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
                    root_title_raw: "Beta Index".to_string(),
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

        let without_root_rows = execute_sqlite_query(
            &connection,
            &validated(
                r#"(headings (and (title "Nested Task" :exact t) (tags "filetag" :with-root nil)))"#,
            ),
        )
        .expect("without-root tag query should execute");
        assert_eq!(heading_ids(without_root_rows), Vec::<i64>::new());

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

        let without_root_rows = execute_sqlite_query(
            &connection,
            &validated(
                r#"(headings (and (title "Nested Task" :exact t) (property "CATEGORY" "work" :with-root nil)))"#,
            ),
        )
        .expect("without-root property query should execute");
        assert_eq!(heading_ids(without_root_rows), Vec::<i64>::new());

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
                assert_eq!(rows[0].id, 14);
                assert_eq!(rows[0].title, "Statistic Cookies");
                assert_eq!(rows[0].title_raw, "[#B] Statistic Cookies [0/1]");
            }
            other => panic!("unexpected rows for normalized title query: {other:?}"),
        }

        let raw_rows = execute_sqlite_query(
            &connection,
            &validated(r#"(headings (title "[#B] Statistic Cookies [0/1]" :exact t))"#),
        )
        .expect("raw title query should execute");
        match raw_rows {
            QueryRows::Headings(rows) => assert!(rows.is_empty()),
            other => panic!("unexpected rows for raw title query: {other:?}"),
        }
    }

    #[test]
    fn injection_like_strings_remain_bound_and_do_not_broaden_results() {
        let connection = seeded_connection();

        for query in [
            validated(r#"(headings (title "x' OR 1=1 --"))"#),
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
                assert_eq!(rows[0].id, 11);
                assert_eq!(rows[0].scheduled_ts, Some(1_767_398_400));
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
                assert_eq!(rows[0].id, 11);
                assert_eq!(rows[0].title, "Query Engine");
            }
            other => panic!("unexpected ts-active rows: {other:?}"),
        }
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
                assert_eq!(rows.len(), 2);
                assert_eq!(rows[0].title, "Query Engine");
                assert_eq!(rows[1].title, "Nested Task");
            }
            other => panic!("unexpected query rows: {other:?}"),
        }

        let after_counts = table_counts(&connection);
        assert_eq!(before_counts, after_counts);
    }

    fn heading_ids(rows: QueryRows) -> Vec<i64> {
        match rows {
            QueryRows::Headings(rows) => rows.into_iter().map(|row| row.id).collect(),
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
                    title_raw: "Beta Index".to_string(),
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
                    title_raw: "Alpha Index".to_string(),
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
                        title_raw: "Query Engine".to_string(),
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
                        title_raw: "Nested Task".to_string(),
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
                        title_raw: "Loose Note".to_string(),
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
                        title_raw: "[#B] Statistic Cookies [0/1]".to_string(),
                        todo_keyword: Some("REVIEW".to_string()),
                        todo_type: Some("open".to_string()),
                        priority: Some('B'),
                        scheduled_raw: None,
                        scheduled_ts: None,
                        deadline_raw: None,
                        deadline_ts: None,
                        closed_raw: None,
                        closed_ts: None,
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
                    title_raw: "Gamma Index".to_string(),
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
                    id: Some(31),
                    file_id,
                    parent_id: Some(root_id),
                    level: 1,
                    line_number: Some(2),
                    byte_start: 10,
                    byte_end: 28,
                    title: "Gamma Candidate".to_string(),
                    title_raw: "Gamma Candidate".to_string(),
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
