use super::*;

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
    execute_sqlite_query_with_relation_and_strategies(
        connection,
        query,
        options,
        PRODUCTION_METADATA_PREDICATE_SQL_STRATEGY,
        MatchedRelationReuseStrategy::QueryDerived,
    )
}

#[cfg(test)]
pub(crate) fn execute_sqlite_query_with_relation_and_metadata_strategy(
    connection: &Connection,
    query: &ValidatedQuery,
    options: &QueryExecutionOptions,
    metadata_predicate_strategy: MetadataPredicateSqlStrategy,
) -> Result<ExecutedSqliteQuery, QueryExecutionError> {
    execute_sqlite_query_with_relation_and_strategies(
        connection,
        query,
        options,
        metadata_predicate_strategy,
        MatchedRelationReuseStrategy::QueryDerived,
    )
}

pub(crate) fn execute_sqlite_query_with_relation_and_strategies(
    connection: &Connection,
    query: &ValidatedQuery,
    options: &QueryExecutionOptions,
    metadata_predicate_strategy: MetadataPredicateSqlStrategy,
    relation_reuse_strategy: MatchedRelationReuseStrategy,
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
        QueryTarget::Headings => execute_heading_query_with_relation_strategy(
            connection,
            &resolved,
            restrict_files,
            metadata_predicate_strategy,
            relation_reuse_strategy,
        ),
        QueryTarget::Links => {
            let compiled = compile_sqlite_query_with_file_restriction(&resolved, restrict_files)?;
            let rows = execute_links_query(connection, &compiled)?;
            Ok(ExecutedSqliteQuery {
                rows,
                relation: MatchedSqlRelation::Links,
                temporary_relation: None,
            })
        }
        QueryTarget::Files => {
            let compiled = compile_sqlite_query_with_file_restriction(&resolved, restrict_files)?;
            let rows = execute_files_query(connection, &compiled)?;
            Ok(ExecutedSqliteQuery {
                rows,
                relation: MatchedSqlRelation::Files(compiled),
                temporary_relation: None,
            })
        }
    }
}

pub(in crate::query::sqlite) fn execute_heading_query_with_relation_strategy(
    connection: &Connection,
    resolved: &ValidatedQuery,
    restrict_files: bool,
    metadata_predicate_strategy: MetadataPredicateSqlStrategy,
    relation_reuse_strategy: MatchedRelationReuseStrategy,
) -> Result<ExecutedSqliteQuery, QueryExecutionError> {
    let compiled = compile_sqlite_query_with_metadata_strategy(
        resolved,
        restrict_files,
        metadata_predicate_strategy,
    )?;

    let materialize = relation_reuse_strategy == MatchedRelationReuseStrategy::SelectiveTemp
        && heading_matched_relation_cost(resolved) == MatchedRelationCost::Expensive;
    let (heading_relation, temporary_relation) = if materialize {
        (
            materialize_heading_relation(connection, &compiled)?,
            Some(TemporaryMatchedRelation::Headings),
        )
    } else {
        (compiled, None)
    };

    let result = (|| {
        let mut rows = execute_heading_rows_query(connection, &heading_relation)?
            .into_iter()
            .map(HeadingQueryMatch::Heading)
            .collect::<Vec<_>>();
        let root_compiled = if heading_root_truth(resolved.predicate.as_ref()) != StaticTruth::False
        {
            let root_compiled = compile_heading_root_file_query(
                resolved,
                restrict_files,
                metadata_predicate_strategy,
            )?;
            rows.extend(
                execute_file_rows_query(connection, &root_compiled)?
                    .into_iter()
                    .map(HeadingQueryMatch::File),
            );
            Some(root_compiled)
        } else {
            None
        };
        rows.sort_by(compare_heading_query_matches);
        Ok(ExecutedSqliteQuery {
            rows: QueryRows::Headings(rows),
            relation: MatchedSqlRelation::Headings {
                headings: heading_relation,
                roots: root_compiled,
            },
            temporary_relation,
        })
    })();

    if result.is_err() {
        if let Some(temporary_relation) = temporary_relation {
            let _ = cleanup_temporary_matched_relation(connection, temporary_relation);
        }
    }
    result
}

pub fn sqlite_query_validation_options(
    connection: &Connection,
) -> Result<QueryValidationOptions, QueryExecutionError> {
    Ok(QueryValidationOptions {
        body_text_available: sqlite_body_text_available(connection)?,
        regexp_matching_supported: true,
    })
}

pub(in crate::query::sqlite) fn execute_heading_rows_query(
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

pub(in crate::query::sqlite) fn load_effective_tags_for_heading_rows(
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

pub(in crate::query::sqlite) fn execute_links_query(
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

pub(in crate::query::sqlite) fn execute_files_query(
    connection: &Connection,
    compiled: &CompiledSqlQuery,
) -> Result<QueryRows, QueryExecutionError> {
    execute_file_rows_query(connection, compiled).map(QueryRows::Files)
}

pub(in crate::query::sqlite) fn execute_file_rows_query(
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

pub(in crate::query::sqlite) fn ensure_body_text_backend_capabilities(
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

pub(in crate::query::sqlite) fn query_requires_body_text(query: &ValidatedQuery) -> bool {
    query
        .predicate
        .as_ref()
        .is_some_and(expr_requires_body_text)
}

pub(in crate::query::sqlite) fn expr_requires_body_text(expr: &ValidatedExpr) -> bool {
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

pub(in crate::query::sqlite) fn sqlite_body_text_available(
    connection: &Connection,
) -> Result<bool, QueryExecutionError> {
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

pub(in crate::query::sqlite) fn table_exists(
    connection: &Connection,
    table: &str,
) -> Result<bool, rusqlite::Error> {
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
