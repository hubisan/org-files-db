use super::*;

pub fn compile_sqlite_query(
    query: &ValidatedQuery,
) -> Result<CompiledSqlQuery, QueryExecutionError> {
    compile_sqlite_query_with_file_restriction(query, false)
}

pub(crate) fn compile_sqlite_query_with_file_restriction(
    query: &ValidatedQuery,
    restrict_files: bool,
) -> Result<CompiledSqlQuery, QueryExecutionError> {
    ensure_relative_dates_resolved(query).map_err(|error| {
        QueryExecutionError::date_resolution(
            query.target,
            "relative-date-resolution",
            error.to_string(),
        )
    })?;
    let mut aliases = AliasAllocator::new();
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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(in crate::query::sqlite) enum StaticTruth {
    True,
    False,
    Unknown,
}

pub(in crate::query::sqlite) fn heading_root_truth(expr: Option<&ValidatedExpr>) -> StaticTruth {
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

pub(in crate::query::sqlite) fn heading_root_predicate_truth(
    predicate: &ValidatedPredicate,
) -> StaticTruth {
    match predicate.name.as_str() {
        "level" => level_predicate_truth_at_zero(predicate),
        "parent" | "ancestors" | "has-text" => StaticTruth::False,
        _ => StaticTruth::Unknown,
    }
}

pub(in crate::query::sqlite) fn level_predicate_truth_at_zero(
    predicate: &ValidatedPredicate,
) -> StaticTruth {
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

pub(in crate::query::sqlite) fn compile_query_match_filter(
    query: &ValidatedQuery,
    scope: &QueryScope,
    aliases: &mut AliasAllocator,
) -> Result<Option<SqlFragment>, QueryExecutionError> {
    compile_scope_match_filter(scope, aliases, query.predicate.as_ref())
}

pub(in crate::query::sqlite) fn compile_scope_match_filter(
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

pub(in crate::query::sqlite) fn scope_base_filter(scope: &QueryScope) -> Option<SqlFragment> {
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

pub(in crate::query::sqlite) fn compile_expr(
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

pub(in crate::query::sqlite) fn compile_logical(
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

pub(in crate::query::sqlite) fn compile_predicate(
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

pub(in crate::query::sqlite) fn compile_heading_predicate(
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
        "tags" => compile_heading_tags_predicate(scope, predicate),
        "property" => compile_heading_property_predicate(scope, predicate),
        "keyword" => compile_heading_keyword_predicate(scope, predicate),
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

pub(in crate::query::sqlite) fn compile_heading_title_predicate(
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

pub(in crate::query::sqlite) fn compile_file_predicate(
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

pub(in crate::query::sqlite) fn compile_todo_predicate(
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

pub(in crate::query::sqlite) fn compile_level_predicate(
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

pub(in crate::query::sqlite) fn compile_priority_predicate(
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

pub(in crate::query::sqlite) fn priority_rank_sql(column: &str) -> String {
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

pub(in crate::query::sqlite) fn compile_heading_root_file_query(
    query: &ValidatedQuery,
    restrict_files: bool,
) -> Result<CompiledSqlQuery, QueryExecutionError> {
    let mut aliases = AliasAllocator::new();
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

pub(in crate::query::sqlite) fn compile_heading_root_false_predicate(
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

pub(in crate::query::sqlite) fn compare_heading_query_matches(
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
