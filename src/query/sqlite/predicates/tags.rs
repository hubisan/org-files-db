use super::*;

pub(in crate::query::sqlite) fn compile_heading_tags_predicate(
    scope: &QueryScope,
    predicate: &ValidatedPredicate,
) -> Result<SqlFragment, QueryExecutionError> {
    if !option_bool_with_default(&predicate.options, "inherit", true)? {
        return compile_tags_predicate(QueryTarget::Headings, predicate, &scope.heading_col("id"));
    }

    compile_heading_effective_tags_exists(scope, predicate)
}

pub(in crate::query::sqlite) fn compile_heading_effective_tags_exists(
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

pub(in crate::query::sqlite) fn compile_file_tags_predicate(
    scope: &QueryScope,
    predicate: &ValidatedPredicate,
) -> Result<SqlFragment, QueryExecutionError> {
    compile_tags_predicate(QueryTarget::Files, predicate, &scope.root_col("id"))
}

pub(in crate::query::sqlite) fn compile_tags_predicate(
    target: QueryTarget,
    predicate: &ValidatedPredicate,
    heading_id_sql: &str,
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

    // Heading targets match through `IN (SELECT ...)`; file targets and regexp
    // predicates keep the heading-driven `EXISTS` form.
    let predicate_driven = target == QueryTarget::Headings && !regexp;
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

pub(in crate::query::sqlite) fn compile_heading_property_predicate(
    scope: &QueryScope,
    predicate: &ValidatedPredicate,
) -> Result<SqlFragment, QueryExecutionError> {
    compile_resolved_property_predicate(
        QueryTarget::Headings,
        predicate,
        &scope.heading_col("id"),
        option_bool_with_default(&predicate.options, "inherit", true)?,
    )
}

pub(in crate::query::sqlite) fn compile_file_property_predicate(
    scope: &QueryScope,
    predicate: &ValidatedPredicate,
) -> Result<SqlFragment, QueryExecutionError> {
    compile_resolved_property_predicate(QueryTarget::Files, predicate, &scope.root_col("id"), false)
}

pub(in crate::query::sqlite) fn compile_resolved_property_predicate(
    target: QueryTarget,
    predicate: &ValidatedPredicate,
    heading_id_sql: &str,
    inherit: bool,
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
    // Heading targets match through `IN (SELECT ...)`; file targets and regexp
    // predicates keep the heading-driven `EXISTS` form.
    let predicate_driven = target == QueryTarget::Headings && !regexp;
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

pub(in crate::query::sqlite) fn compile_keyword_predicate(
    target: QueryTarget,
    predicate: &ValidatedPredicate,
    heading_id_sql: &str,
) -> Result<SqlFragment, QueryExecutionError> {
    let regexp = option_bool(&predicate.options, "regexp")?;
    let key = arg_as_string(&predicate.args[0]).map_err(|message| {
        QueryExecutionError::unsupported_backend_feature(target, "keyword", message)
    })?;
    // Heading targets match through `IN (SELECT ...)`; file targets and regexp
    // predicates keep the heading-driven `EXISTS` form.
    let predicate_driven = target == QueryTarget::Headings && !regexp;
    let mut sql = if predicate_driven {
        format!(
            "({heading_id_sql} IN (SELECT keywords.heading_id FROM keywords WHERE orgfdb_lower(keywords.keyword) = orgfdb_lower(?)"
        )
    } else {
        format!(
            "(EXISTS (SELECT 1 FROM keywords WHERE keywords.heading_id = {heading_id_sql} AND orgfdb_lower(keywords.keyword) = orgfdb_lower(?)"
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

pub(in crate::query::sqlite) fn compile_heading_keyword_predicate(
    scope: &QueryScope,
    predicate: &ValidatedPredicate,
) -> Result<SqlFragment, QueryExecutionError> {
    let heading_id = if option_bool_with_default(&predicate.options, "inherit", true)? {
        scope.root_col("id")
    } else {
        scope.heading_col("id")
    };
    compile_keyword_predicate(QueryTarget::Headings, predicate, &heading_id)
}
