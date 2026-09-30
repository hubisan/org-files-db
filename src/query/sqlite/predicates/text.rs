use super::*;

pub(in crate::query::sqlite) fn compile_has_text_predicate(
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
                "INSTR(orgfdb_lower(heading_bodies.body_text), orgfdb_lower(?)) > 0".to_string()
            }
        ));
        params.push(QueryParam::Text(value));
    }

    Ok(SqlFragment {
        sql: format!("({})", parts.join(" AND ")),
        params,
    })
}

pub(in crate::query::sqlite) fn compile_text_predicate(
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
            parts.push(format!("orgfdb_lower({sql_column}) = orgfdb_lower(?)"));
        } else {
            parts.push(format!(
                "INSTR(orgfdb_lower({sql_column}), orgfdb_lower(?)) > 0"
            ));
        }
        params.push(QueryParam::Text(value));
    }

    Ok(SqlFragment {
        sql: format!("({})", parts.join(" AND ")),
        params,
    })
}

pub(in crate::query::sqlite) fn compile_home_path_predicate(
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

pub(in crate::query::sqlite) fn expand_leading_home_path(value: &str) -> Result<String, String> {
    expand_leading_home_path_with_home(value, env::var("HOME").ok().as_deref())
}

pub(in crate::query::sqlite) fn expand_leading_home_path_with_home(
    value: &str,
    home: Option<&str>,
) -> Result<String, String> {
    if value != "~" && !value.starts_with("~/") {
        return Ok(value.to_string());
    }
    let home = home.ok_or_else(|| {
        "a leading ~ requires the HOME environment variable to be available".to_string()
    })?;
    Ok(format!("{home}{}", &value[1..]))
}

pub(in crate::query::sqlite) fn sqlite_file_name_expr(path_sql: &str) -> String {
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

pub(in crate::query::sqlite) fn sqlite_file_dir_expr(path_sql: &str) -> String {
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

pub(in crate::query::sqlite) fn validate_regexp_pattern(
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
