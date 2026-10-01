use super::*;

pub(in crate::query::sqlite) const QUERY_FILE_RESTRICTION_TABLE: &str =
    "orgfdb_query_file_restriction";

#[derive(Debug, Clone)]
pub(in crate::query::sqlite) struct SqlFragment {
    pub(in crate::query::sqlite) sql: String,
    pub(in crate::query::sqlite) params: Vec<QueryParam>,
}

#[derive(Debug, Clone)]
pub(in crate::query::sqlite) struct QueryScope {
    pub(in crate::query::sqlite) target: QueryTarget,
    pub(in crate::query::sqlite) heading_match_kind: HeadingMatchKind,
    pub(in crate::query::sqlite) heading_alias: String,
    pub(in crate::query::sqlite) file_alias: String,
    pub(in crate::query::sqlite) root_alias: String,
    pub(in crate::query::sqlite) link_alias: String,
    pub(in crate::query::sqlite) link_heading_alias: String,
    pub(in crate::query::sqlite) outline_alias: String,
}

impl QueryScope {
    pub(in crate::query::sqlite) fn new(target: QueryTarget, id: usize) -> Self {
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

    pub(in crate::query::sqlite) fn heading_root(id: usize) -> Self {
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

    pub(in crate::query::sqlite) fn all_headings(id: usize) -> Self {
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

    pub(in crate::query::sqlite) fn heading_col(&self, column: &str) -> String {
        format!("{}.{}", self.heading_alias, column)
    }

    pub(in crate::query::sqlite) fn file_col(&self, column: &str) -> String {
        format!("{}.{}", self.file_alias, column)
    }

    pub(in crate::query::sqlite) fn root_col(&self, column: &str) -> String {
        if self.target == QueryTarget::Headings
            && self.heading_match_kind == HeadingMatchKind::RootFile
        {
            self.heading_col(column)
        } else {
            format!("{}.{}", self.root_alias, column)
        }
    }

    pub(in crate::query::sqlite) fn link_col(&self, column: &str) -> String {
        format!("{}.{}", self.link_alias, column)
    }

    pub(in crate::query::sqlite) fn link_heading_col(&self, column: &str) -> String {
        format!("{}.{}", self.link_heading_alias, column)
    }
}

#[derive(Debug)]
pub(in crate::query::sqlite) struct AliasAllocator {
    pub(in crate::query::sqlite) next_scope_id: usize,
}

impl AliasAllocator {
    pub(in crate::query::sqlite) fn new() -> Self {
        Self { next_scope_id: 0 }
    }

    pub(in crate::query::sqlite) fn next_scope(&mut self, target: QueryTarget) -> QueryScope {
        let scope = QueryScope::new(target, self.next_scope_id);
        self.next_scope_id += 1;
        scope
    }

    pub(in crate::query::sqlite) fn next_heading_root_scope(&mut self) -> QueryScope {
        let scope = QueryScope::heading_root(self.next_scope_id);
        self.next_scope_id += 1;
        scope
    }

    pub(in crate::query::sqlite) fn next_all_headings_scope(&mut self) -> QueryScope {
        let scope = QueryScope::all_headings(self.next_scope_id);
        self.next_scope_id += 1;
        scope
    }
}

pub(in crate::query::sqlite) fn combine_fragments_with(
    op: &str,
    fragments: Vec<SqlFragment>,
) -> Option<SqlFragment> {
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

pub(in crate::query::sqlite) fn compile_in_list(
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

pub(in crate::query::sqlite) fn start_bound(
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

pub(in crate::query::sqlite) fn exclusive_end_bound(
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

pub(in crate::query::sqlite) fn option_value<'a>(
    options: &'a [ValidatedOption],
    name: &str,
) -> Option<&'a QueryValue> {
    options
        .iter()
        .find(|option| option.name == name)
        .map(|option| &option.value)
}

pub(in crate::query::sqlite) fn option_bool(
    options: &[ValidatedOption],
    name: &str,
) -> Result<bool, QueryExecutionError> {
    Ok(match option_value(options, name) {
        Some(QueryValue::Bool(value)) => *value,
        Some(_) => unreachable!("validator should constrain boolean options"),
        None => false,
    })
}

pub(in crate::query::sqlite) fn option_bool_with_default(
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

pub(in crate::query::sqlite) fn keyword_option(
    options: &[ValidatedOption],
    name: &str,
) -> Result<Option<String>, QueryExecutionError> {
    match option_value(options, name) {
        Some(QueryValue::Keyword(value)) => Ok(Some(value.clone())),
        Some(_) => unreachable!("validator should constrain keyword options"),
        None => Ok(None),
    }
}

pub(in crate::query::sqlite) fn arg_as_string(arg: &ValidatedArg) -> Result<String, &'static str> {
    match arg {
        ValidatedArg::Scalar(QueryValue::String(value)) => Ok(value.clone()),
        _ => Err("expected string scalar argument"),
    }
}

pub(in crate::query::sqlite) fn sql_literal(sql: &str) -> SqlFragment {
    SqlFragment {
        sql: sql.to_string(),
        params: Vec::new(),
    }
}

pub(in crate::query::sqlite) fn add_file_restriction(
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

pub(in crate::query::sqlite) fn prepare_file_restriction(
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

pub(in crate::query::sqlite) fn render_where_clause(fragment: Option<&SqlFragment>) -> String {
    match fragment {
        Some(fragment) => format!("WHERE {}", fragment.sql),
        None => String::new(),
    }
}

pub(in crate::query::sqlite) fn heading_from_clause(
    scope: &QueryScope,
    include_file: bool,
    include_root: bool,
) -> String {
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

pub(in crate::query::sqlite) fn fragment_references_file(
    scope: &QueryScope,
    fragment: Option<&SqlFragment>,
) -> bool {
    fragment.is_some_and(|fragment| fragment.sql.contains(&format!("{}.", scope.file_alias)))
}

pub(in crate::query::sqlite) fn fragment_references_root(
    scope: &QueryScope,
    fragment: Option<&SqlFragment>,
) -> bool {
    if scope.target == QueryTarget::Headings
        && scope.heading_match_kind == HeadingMatchKind::RootFile
    {
        return false;
    }
    fragment.is_some_and(|fragment| fragment.sql.contains(&format!("{}.", scope.root_alias)))
}

pub(in crate::query::sqlite) fn link_from_clause(
    scope: &QueryScope,
    include_output_context: bool,
) -> String {
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

pub(in crate::query::sqlite) fn file_from_clause(scope: &QueryScope, include_root: bool) -> String {
    let mut sql = format!("FROM files AS {}", scope.file_alias);
    if include_root {
        sql.push_str(&format!(
            "\n         INNER JOIN headings AS {} ON {}.file_id = {}.id AND {}.level = 0",
            scope.root_alias, scope.root_alias, scope.file_alias, scope.root_alias
        ));
    }
    sql
}

pub(in crate::query::sqlite) fn target_name(target: QueryTarget) -> &'static str {
    match target {
        QueryTarget::Headings => "headings",
        QueryTarget::Links => "links",
        QueryTarget::Files => "files",
    }
}
