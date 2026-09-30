use super::*;

pub(in crate::query::sqlite) fn compile_date_predicate(
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

pub(in crate::query::sqlite) fn compile_planning_predicate(
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

pub(in crate::query::sqlite) fn compile_timestamp_exists_predicate(
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
