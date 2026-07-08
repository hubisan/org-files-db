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

pub fn compile_sqlite_query(
    query: &ValidatedQuery,
) -> Result<CompiledSqlQuery, QueryExecutionError> {
    let where_clause = query
        .predicate
        .as_ref()
        .map(|expr| compile_expr(query.target, expr))
        .transpose()?;

    let sql = match query.target {
        QueryTarget::Headings => format!(
            "SELECT
                headings.id,
                headings.file_id,
                files.path,
                headings.parent_id,
                headings.level,
                headings.line_number,
                headings.byte_start,
                headings.byte_end,
                headings.title,
                headings.title_raw,
                headings.todo_keyword,
                headings.todo_type,
                headings.priority,
                headings.scheduled_raw,
                headings.scheduled_ts,
                headings.deadline_raw,
                headings.deadline_ts,
                headings.closed_raw,
                headings.closed_ts,
                headings.archivedp,
                headings.footnote_section_p,
                headings.all_tags_json
             {}
             {}
             ORDER BY files.path, headings.byte_start, headings.id",
            heading_from_clause(),
            render_where_clause(where_clause.as_ref())
        ),
        QueryTarget::Links => format!(
            "SELECT
                links.id,
                links.file_id,
                files.path,
                links.heading_id,
                headings.level,
                outline_path.breadcrumbs_json,
                links.source_context,
                links.format,
                links.link_type,
                links.raw,
                links.raw_target,
                links.raw_description,
                links.path,
                links.search_option,
                links.path_absolute,
                links.target_file_id,
                links.target_heading_id,
                links.target_custom_id,
                links.target_id,
                links.resolution_status,
                links.resolution_diagnostic,
                links.byte_start,
                links.byte_end,
                links.line
             {}
             {}
             ORDER BY files.path, links.byte_start, links.id",
            link_from_clause(),
            render_where_clause(where_clause.as_ref())
        ),
        QueryTarget::Files => format!(
            "SELECT
                files.id,
                files.path,
                files.mtime_ns,
                files.size,
                files.content_hash,
                files.indexed_at,
                root.id,
                root.title,
                root.title_raw
             {}
             {}
             ORDER BY files.path, files.id",
            file_from_clause(),
            render_where_clause(where_clause.as_ref())
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

fn compile_expr(
    target: QueryTarget,
    expr: &ValidatedExpr,
) -> Result<SqlFragment, QueryExecutionError> {
    match expr {
        ValidatedExpr::And(children) => compile_logical(target, children, "AND", "1 = 1"),
        ValidatedExpr::Or(children) => compile_logical(target, children, "OR", "0 = 1"),
        ValidatedExpr::Not(child) => {
            let fragment = compile_expr(target, child)?;
            Ok(SqlFragment {
                sql: format!("(NOT {})", fragment.sql),
                params: fragment.params,
            })
        }
        ValidatedExpr::Predicate(predicate) => compile_predicate(target, predicate),
    }
}

fn compile_logical(
    target: QueryTarget,
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
        let fragment = compile_expr(target, child)?;
        sql_parts.push(fragment.sql);
        params.extend(fragment.params);
    }
    Ok(SqlFragment {
        sql: format!("({})", sql_parts.join(&format!(" {op} "))),
        params,
    })
}

fn compile_predicate(
    target: QueryTarget,
    predicate: &ValidatedPredicate,
) -> Result<SqlFragment, QueryExecutionError> {
    match target {
        QueryTarget::Headings => compile_heading_predicate(predicate),
        QueryTarget::Links => compile_link_predicate(predicate),
        QueryTarget::Files => compile_file_predicate(predicate),
    }
}

fn compile_heading_predicate(
    predicate: &ValidatedPredicate,
) -> Result<SqlFragment, QueryExecutionError> {
    match predicate.name.as_str() {
        "todo" => compile_todo_predicate(predicate),
        "done" => Ok(sql_literal("(headings.todo_type = 'closed')")),
        "priority" => {
            compile_priority_predicate(QueryTarget::Headings, "headings.priority", predicate)
        }
        "title" => {
            compile_text_predicate(QueryTarget::Headings, "headings.title", predicate, false)
        }
        "level" => compile_level_predicate(predicate),
        "file-path" => {
            compile_text_predicate(QueryTarget::Headings, "files.path", predicate, false)
        }
        "file-title" => {
            compile_text_predicate(QueryTarget::Headings, "root.title", predicate, false)
        }
        "file-modified" => compile_date_predicate(
            QueryTarget::Headings,
            "file-modified",
            "files.mtime_ns",
            &predicate.options,
            TemporalUnit::Nanoseconds,
            true,
        ),
        "tags" => compile_heading_tags_predicate(predicate),
        "property" => compile_heading_property_predicate(predicate),
        "keyword" => compile_keyword_predicate(QueryTarget::Headings, predicate, "root.id"),
        "scheduled" => compile_date_predicate(
            QueryTarget::Headings,
            "scheduled",
            "headings.scheduled_ts",
            &predicate.options,
            TemporalUnit::Seconds,
            true,
        ),
        "deadline" => compile_date_predicate(
            QueryTarget::Headings,
            "deadline",
            "headings.deadline_ts",
            &predicate.options,
            TemporalUnit::Seconds,
            true,
        ),
        "closed" => compile_date_predicate(
            QueryTarget::Headings,
            "closed",
            "headings.closed_ts",
            &predicate.options,
            TemporalUnit::Seconds,
            true,
        ),
        "planning" => compile_planning_predicate(predicate),
        "ts" => compile_timestamp_exists_predicate(predicate, None),
        "ts-active" => compile_timestamp_exists_predicate(predicate, Some("active")),
        "ts-inactive" => compile_timestamp_exists_predicate(predicate, Some("inactive")),
        "has-text" | "outline-contains" | "outline-sequence" | "file-name" | "file-dir"
        | "parent" | "ancestors" | "children" | "descendants" | "has-link" | "links-to"
        | "linked-from" => Err(QueryExecutionError::unsupported_predicate(
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

fn compile_link_predicate(
    predicate: &ValidatedPredicate,
) -> Result<SqlFragment, QueryExecutionError> {
    match predicate.name.as_str() {
        "link-type" => compile_in_list("links.link_type", &predicate.args),
        "link-target" => {
            compile_text_predicate(QueryTarget::Links, "links.raw_target", predicate, true)
        }
        "link-description" => {
            compile_text_predicate(QueryTarget::Links, "links.raw_description", predicate, true)
        }
        "has-description" => Ok(sql_literal(
            "(links.raw_description IS NOT NULL AND links.raw_description <> '')",
        )),
        "status" => compile_in_list("links.resolution_status", &predicate.args),
        "source" => compile_link_endpoint_predicate(predicate, LinkEndpoint::Source),
        "target" => compile_link_endpoint_predicate(predicate, LinkEndpoint::Target),
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
    predicate: &ValidatedPredicate,
) -> Result<SqlFragment, QueryExecutionError> {
    match predicate.name.as_str() {
        "file-path" => compile_text_predicate(QueryTarget::Files, "files.path", predicate, false),
        "file-title" => compile_text_predicate(QueryTarget::Files, "root.title", predicate, false),
        "file-modified" => compile_date_predicate(
            QueryTarget::Files,
            "file-modified",
            "files.mtime_ns",
            &predicate.options,
            TemporalUnit::Nanoseconds,
            true,
        ),
        "tags" => compile_file_tags_predicate(predicate),
        "property" => compile_file_property_predicate(predicate),
        "keyword" => compile_keyword_predicate(QueryTarget::Files, predicate, "root.id"),
        "has-text" | "outline-contains" | "outline-sequence" | "file-name" | "file-dir"
        | "parent" | "ancestors" | "children" | "descendants" | "has-link" | "links-to"
        | "linked-from" => Err(QueryExecutionError::unsupported_predicate(
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
    predicate: &ValidatedPredicate,
) -> Result<SqlFragment, QueryExecutionError> {
    if predicate.args.is_empty() {
        return Ok(sql_literal("(headings.todo_type = 'open')"));
    }
    compile_in_list("headings.todo_keyword", &predicate.args)
}

fn compile_level_predicate(
    predicate: &ValidatedPredicate,
) -> Result<SqlFragment, QueryExecutionError> {
    match predicate.args.as_slice() {
        [ValidatedArg::Scalar(QueryValue::Integer(value))] => Ok(SqlFragment {
            sql: "(headings.level = ?)".to_string(),
            params: vec![QueryParam::Integer(*value)],
        }),
        [ValidatedArg::Scalar(QueryValue::Integer(minimum)), ValidatedArg::Scalar(QueryValue::Integer(maximum))] => {
            Ok(SqlFragment {
                sql: "(headings.level BETWEEN ? AND ?)".to_string(),
                params: vec![QueryParam::Integer(*minimum), QueryParam::Integer(*maximum)],
            })
        }
        [ValidatedArg::Scalar(QueryValue::Symbol(comparator)), ValidatedArg::Scalar(QueryValue::Integer(value))]
            if matches!(comparator.as_str(), "<" | "<=" | ">" | ">=") =>
        {
            Ok(SqlFragment {
                sql: format!("(headings.level {} ?)", comparator),
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
    predicate: &ValidatedPredicate,
) -> Result<SqlFragment, QueryExecutionError> {
    if option_bool_with_default(&predicate.options, "inherit", true)? {
        return Err(QueryExecutionError::unsupported_backend_feature(
            QueryTarget::Headings,
            "tags",
            "heading tags require effective/inherited semantics unless :inherit nil is used",
        ));
    }
    if option_bool_with_default(&predicate.options, "with-root", false)? {
        return Err(QueryExecutionError::unsupported_backend_feature(
            QueryTarget::Headings,
            "tags",
            "heading tags with :with-root t are not supported by the SQLite metadata backend",
        ));
    }
    if option_bool(&predicate.options, "regexp")? {
        return Err(QueryExecutionError::unsupported_backend_feature(
            QueryTarget::Headings,
            "tags",
            "heading tags with :regexp t are not supported by the SQLite metadata backend",
        ));
    }
    compile_tags_exists(QueryTarget::Headings, predicate, "headings.id")
}

fn compile_file_tags_predicate(
    predicate: &ValidatedPredicate,
) -> Result<SqlFragment, QueryExecutionError> {
    if option_bool(&predicate.options, "regexp")? {
        return Err(QueryExecutionError::unsupported_backend_feature(
            QueryTarget::Files,
            "tags",
            "file tags with :regexp t are not supported by the SQLite metadata backend",
        ));
    }
    compile_tags_exists(QueryTarget::Files, predicate, "root.id")
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

fn compile_heading_property_predicate(
    predicate: &ValidatedPredicate,
) -> Result<SqlFragment, QueryExecutionError> {
    if option_bool_with_default(&predicate.options, "inherit", true)? {
        return Err(QueryExecutionError::unsupported_backend_feature(
            QueryTarget::Headings,
            "property",
            "heading property queries require effective/inherited semantics unless :inherit nil is used",
        ));
    }
    if option_bool_with_default(&predicate.options, "with-root", false)? {
        return Err(QueryExecutionError::unsupported_backend_feature(
            QueryTarget::Headings,
            "property",
            "heading property queries with :with-root t are not supported by the SQLite metadata backend",
        ));
    }
    compile_property_exists(QueryTarget::Headings, predicate, "headings.id")
}

fn compile_file_property_predicate(
    predicate: &ValidatedPredicate,
) -> Result<SqlFragment, QueryExecutionError> {
    compile_property_exists(QueryTarget::Files, predicate, "root.id")
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
        return Ok(sql_literal(
            "(headings.scheduled_ts IS NOT NULL OR headings.deadline_ts IS NOT NULL OR headings.closed_ts IS NOT NULL)",
        ));
    }

    let scheduled = compile_date_predicate(
        QueryTarget::Headings,
        "scheduled",
        "headings.scheduled_ts",
        &predicate.options,
        TemporalUnit::Seconds,
        true,
    )?;
    let deadline = compile_date_predicate(
        QueryTarget::Headings,
        "deadline",
        "headings.deadline_ts",
        &predicate.options,
        TemporalUnit::Seconds,
        true,
    )?;
    let closed = compile_date_predicate(
        QueryTarget::Headings,
        "closed",
        "headings.closed_ts",
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

    let mut sql = String::from(
        "(EXISTS (SELECT 1 FROM timestamps WHERE timestamps.heading_id = headings.id AND ",
    );
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

fn compile_link_endpoint_predicate(
    predicate: &ValidatedPredicate,
    endpoint: LinkEndpoint,
) -> Result<SqlFragment, QueryExecutionError> {
    match &predicate.args[0] {
        ValidatedArg::Scalar(QueryValue::Keyword(value)) if value == "any" => match endpoint {
            LinkEndpoint::Source => Ok(sql_literal("(1 = 1)")),
            LinkEndpoint::Target => Ok(sql_literal(
                "(links.target_file_id IS NOT NULL OR links.target_heading_id IS NOT NULL)",
            )),
        },
        ValidatedArg::NestedQuery(query) => {
            let (target_column, expected_target) = match (endpoint, query.target) {
                (LinkEndpoint::Source, QueryTarget::Headings) => {
                    ("links.heading_id", QueryTarget::Headings)
                }
                (LinkEndpoint::Source, QueryTarget::Files) => ("links.file_id", QueryTarget::Files),
                (LinkEndpoint::Target, QueryTarget::Headings) => {
                    ("links.target_heading_id", QueryTarget::Headings)
                }
                (LinkEndpoint::Target, QueryTarget::Files) => {
                    ("links.target_file_id", QueryTarget::Files)
                }
                _ => unreachable!("validator should constrain source/target nested queries"),
            };
            compile_nested_exists(query, expected_target, target_column)
        }
        _ => unreachable!("validator should constrain source/target args"),
    }
}

fn compile_nested_exists(
    query: &ValidatedQuery,
    expected_target: QueryTarget,
    outer_id_sql: &str,
) -> Result<SqlFragment, QueryExecutionError> {
    debug_assert_eq!(query.target, expected_target);
    let predicate_fragment = query
        .predicate
        .as_ref()
        .map(|expr| compile_expr(query.target, expr))
        .transpose()?;

    let (id_column, from_clause) = match query.target {
        QueryTarget::Headings => ("headings.id", heading_from_clause()),
        QueryTarget::Files => ("files.id", file_from_clause()),
        QueryTarget::Links => unreachable!("validator should reject nested links here"),
    };

    let mut sql = format!("(EXISTS (SELECT 1 {from_clause} WHERE {id_column} = {outer_id_sql}");
    let mut params = Vec::new();
    if let Some(fragment) = predicate_fragment {
        sql.push_str(" AND ");
        sql.push_str(&fragment.sql);
        params.extend(fragment.params);
    }
    sql.push_str("))");
    Ok(SqlFragment { sql, params })
}

fn compile_in_list(
    column: &str,
    args: &[ValidatedArg],
) -> Result<SqlFragment, QueryExecutionError> {
    let values = args
        .iter()
        .map(arg_as_string)
        .collect::<Result<Vec<_>, _>>()
        .map_err(|message| {
            QueryExecutionError::unsupported_backend_feature(QueryTarget::Links, column, message)
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

fn heading_from_clause() -> &'static str {
    "FROM headings
     INNER JOIN files ON files.id = headings.file_id
     INNER JOIN headings AS root ON root.file_id = headings.file_id AND root.level = 0"
}

fn link_from_clause() -> &'static str {
    "FROM links
     INNER JOIN files ON files.id = links.file_id
     INNER JOIN headings ON headings.id = links.heading_id
     INNER JOIN outline_path ON outline_path.heading_id = links.heading_id"
}

fn file_from_clause() -> &'static str {
    "FROM files
     INNER JOIN headings AS root ON root.file_id = files.id AND root.level = 0"
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
        let query = validated(&format!(r#"(headings (title "{user_value}"))"#));

        let compiled = compile_sqlite_query(&query).expect("query should compile");

        assert!(compiled.sql.contains('?'));
        assert!(!compiled.sql.contains(user_value));
        assert_eq!(compiled.params.len(), 1);
    }

    #[test]
    fn compile_rejects_regex_and_deferred_relation_predicates() {
        let regex_query = validated(r#"(links (link-target "notes.*" :regexp t))"#);
        let regex_error = compile_sqlite_query(&regex_query).expect_err("regexp should fail");
        assert_eq!(
            regex_error.kind,
            QueryExecutionErrorKind::UnsupportedBackendFeature
        );

        let relation_query =
            validated(r#"(headings (links-to (files (file-path "notes.org" :exact t))))"#);
        let relation_error =
            compile_sqlite_query(&relation_query).expect_err("relation should fail");
        assert_eq!(
            relation_error.kind,
            QueryExecutionErrorKind::UnsupportedPredicate
        );
    }

    #[test]
    fn execution_matches_metadata_queries_and_boolean_composition() {
        let mut connection = seeded_connection();

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
                all_tags_json: "[\"project\"]".to_string(),
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
                    (tags "project"))))"#,
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

        let _ = &mut connection;
    }

    #[test]
    fn injection_like_strings_remain_bound_and_do_not_broaden_results() {
        let connection = seeded_connection();
        let query = validated(r#"(headings (title "x' OR 1=1 --"))"#);

        let compiled = compile_sqlite_query(&query).expect("query should compile");
        assert!(!compiled.sql.contains("1=1"));
        assert_eq!(compiled.params.len(), 1);

        let rows = execute_sqlite_query(&connection, &query).expect("query should execute");
        assert_eq!(rows, QueryRows::Headings(Vec::new()));
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
            &validated(r#"(headings (title "Query Engine"))"#),
        )
        .expect("query should execute from stored DB rows");

        match rows {
            QueryRows::Headings(rows) => {
                assert_eq!(rows.len(), 1);
                assert_eq!(rows[0].title, "Query Engine");
            }
            other => panic!("unexpected query rows: {other:?}"),
        }

        let after_counts = table_counts(&connection);
        assert_eq!(before_counts, after_counts);
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
            Ok(())
        })
        .expect("beta file should seed");

        DbWriter::rebuild_file(connection, &alpha, |tx, file_id| {
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
                    all_tags_json: "[\"project\"]".to_string(),
                },
            )?;
            DbWriter::insert_headings(
                tx,
                &[HeadingRecord {
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
                    all_tags_json: "[\"project\"]".to_string(),
                }],
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
                ],
            )?;
            DbWriter::insert_tags(
                tx,
                &[
                    TagRecord {
                        heading_id: 10,
                        tag: "project".to_string(),
                    },
                    TagRecord {
                        heading_id: 11,
                        tag: "project".to_string(),
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
                &[PropertyRecord {
                    heading_id: 10,
                    key: "CATEGORY".to_string(),
                    value: Some("work".to_string()),
                    source: "category_keyword".to_string(),
                    append: false,
                    line_number: Some(2),
                }],
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
