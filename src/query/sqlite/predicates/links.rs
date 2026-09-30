use super::*;

pub(in crate::query::sqlite) fn compile_outline_contains_predicate(
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
            "INSTR(orgfdb_lower(CAST(breadcrumb.value AS TEXT)), orgfdb_lower(?)) > 0".to_string()
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

pub(in crate::query::sqlite) fn compile_outline_sequence_predicate(
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
            predicates.push(format!("orgfdb_lower({expression}) = orgfdb_lower(?)"));
        } else {
            predicates.push(format!(
                "INSTR(orgfdb_lower({expression}), orgfdb_lower(?)) > 0"
            ));
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

pub(in crate::query::sqlite) fn compile_link_predicate(
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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(in crate::query::sqlite) enum LinkEndpoint {
    Source,
    Target,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(in crate::query::sqlite) enum HeadingHierarchyRelation {
    Parent,
    Ancestor,
    Child,
    Descendant,
}

pub(in crate::query::sqlite) fn compile_link_endpoint_predicate(
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

pub(in crate::query::sqlite) fn resolved_link_target_fragment(scope: &QueryScope) -> SqlFragment {
    sql_literal(&format!(
        "({} = 'resolved')",
        scope.link_col("resolution_status")
    ))
}

pub(in crate::query::sqlite) fn compile_nested_target_exists(
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

pub(in crate::query::sqlite) fn compile_heading_hierarchy_predicate(
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

pub(in crate::query::sqlite) fn compile_has_link_predicate(
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

pub(in crate::query::sqlite) fn compile_links_to_predicate(
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

pub(in crate::query::sqlite) fn compile_linked_from_predicate(
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
