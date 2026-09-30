use super::*;

pub fn execute_and_shape_query(
    connection: &Connection,
    query: &ValidatedQuery,
    options: &QueryExecutionOptions,
) -> Result<QueryResponse, QueryShapeError> {
    execute_and_shape_query_with_strategies(
        connection,
        query,
        options,
        HeadingPathStrategy::RustDrivenBulkAncestors,
        PRODUCTION_METADATA_PREDICATE_SQL_STRATEGY,
        PRODUCTION_MATCHED_RELATION_REUSE_STRATEGY,
        PRODUCTION_DIRECT_FLAT_SHAPING_STRATEGY,
    )
}

pub(crate) fn execute_and_shape_query_with_path_strategy(
    connection: &Connection,
    query: &ValidatedQuery,
    options: &QueryExecutionOptions,
    path_strategy: HeadingPathStrategy,
) -> Result<QueryResponse, QueryShapeError> {
    execute_and_shape_query_with_strategies(
        connection,
        query,
        options,
        path_strategy,
        PRODUCTION_METADATA_PREDICATE_SQL_STRATEGY,
        PRODUCTION_MATCHED_RELATION_REUSE_STRATEGY,
        PRODUCTION_DIRECT_FLAT_SHAPING_STRATEGY,
    )
}

pub(crate) fn execute_and_shape_query_with_metadata_strategy(
    connection: &Connection,
    query: &ValidatedQuery,
    options: &QueryExecutionOptions,
    metadata_predicate_strategy: MetadataPredicateSqlStrategy,
) -> Result<QueryResponse, QueryShapeError> {
    execute_and_shape_query_with_strategies(
        connection,
        query,
        options,
        HeadingPathStrategy::RustDrivenBulkAncestors,
        metadata_predicate_strategy,
        PRODUCTION_MATCHED_RELATION_REUSE_STRATEGY,
        PRODUCTION_DIRECT_FLAT_SHAPING_STRATEGY,
    )
}

pub(crate) fn execute_and_shape_query_with_relation_reuse_strategy(
    connection: &Connection,
    query: &ValidatedQuery,
    options: &QueryExecutionOptions,
    relation_reuse_strategy: MatchedRelationReuseStrategy,
) -> Result<QueryResponse, QueryShapeError> {
    execute_and_shape_query_with_strategies(
        connection,
        query,
        options,
        HeadingPathStrategy::RustDrivenBulkAncestors,
        PRODUCTION_METADATA_PREDICATE_SQL_STRATEGY,
        relation_reuse_strategy,
        PRODUCTION_DIRECT_FLAT_SHAPING_STRATEGY,
    )
}

pub(crate) fn execute_and_shape_query_with_direct_flat_shaping_strategy(
    connection: &Connection,
    query: &ValidatedQuery,
    options: &QueryExecutionOptions,
    shaping_strategy: DirectFlatShapingStrategy,
) -> Result<QueryResponse, QueryShapeError> {
    execute_and_shape_query_with_strategies(
        connection,
        query,
        options,
        HeadingPathStrategy::RustDrivenBulkAncestors,
        PRODUCTION_METADATA_PREDICATE_SQL_STRATEGY,
        PRODUCTION_MATCHED_RELATION_REUSE_STRATEGY,
        shaping_strategy,
    )
}

pub(in crate::query::result) fn execute_and_shape_query_with_strategies(
    connection: &Connection,
    query: &ValidatedQuery,
    options: &QueryExecutionOptions,
    path_strategy: HeadingPathStrategy,
    metadata_predicate_strategy: MetadataPredicateSqlStrategy,
    relation_reuse_strategy: MatchedRelationReuseStrategy,
    shaping_strategy: DirectFlatShapingStrategy,
) -> Result<QueryResponse, QueryShapeError> {
    let owns_snapshot = connection.is_autocommit();
    if owns_snapshot {
        connection
            .execute_batch("BEGIN DEFERRED TRANSACTION")
            .map_err(|source| QueryShapeError::database("query_snapshot.begin", source))?;
    }

    let result = execute_and_shape_query_in_snapshot(
        connection,
        query,
        options,
        path_strategy,
        metadata_predicate_strategy,
        relation_reuse_strategy,
        shaping_strategy,
    );
    if !owns_snapshot {
        return result;
    }

    match result {
        Ok(response) => {
            connection
                .execute_batch("COMMIT")
                .map_err(|source| QueryShapeError::database("query_snapshot.commit", source))?;
            Ok(response)
        }
        Err(error) => {
            let _ = connection.execute_batch("ROLLBACK");
            Err(error)
        }
    }
}

pub(in crate::query::result) fn execute_and_shape_query_in_snapshot(
    connection: &Connection,
    query: &ValidatedQuery,
    options: &QueryExecutionOptions,
    path_strategy: HeadingPathStrategy,
    metadata_predicate_strategy: MetadataPredicateSqlStrategy,
    relation_reuse_strategy: MatchedRelationReuseStrategy,
    shaping_strategy: DirectFlatShapingStrategy,
) -> Result<QueryResponse, QueryShapeError> {
    let executed = execute_sqlite_query_with_relation_and_strategies(
        connection,
        query,
        options,
        metadata_predicate_strategy,
        relation_reuse_strategy,
    )?;
    let crate::query::sqlite::ExecutedSqliteQuery {
        rows,
        relation,
        temporary_relation,
    } = executed;
    let shaped = shape_query_results_internal(
        connection,
        rows,
        options,
        Some(&relation),
        path_strategy,
        shaping_strategy,
    );
    let cleanup = match temporary_relation {
        Some(temporary_relation) => {
            cleanup_temporary_matched_relation(connection, temporary_relation)
        }
        None => Ok(()),
    };

    match (shaped, cleanup) {
        (Err(error), _) => Err(error),
        (Ok(_), Err(error)) => Err(error.into()),
        (Ok(response), Ok(())) => Ok(response),
    }
}

pub fn shape_query_results(
    connection: &Connection,
    rows: QueryRows,
    options: &QueryExecutionOptions,
) -> Result<QueryResponse, QueryShapeError> {
    shape_query_results_internal(
        connection,
        rows,
        options,
        None,
        HeadingPathStrategy::RustDrivenBulkAncestors,
        PRODUCTION_DIRECT_FLAT_SHAPING_STRATEGY,
    )
}

pub(in crate::query::result) fn shape_query_results_internal(
    connection: &Connection,
    rows: QueryRows,
    options: &QueryExecutionOptions,
    relation: Option<&MatchedSqlRelation>,
    path_strategy: HeadingPathStrategy,
    shaping_strategy: DirectFlatShapingStrategy,
) -> Result<QueryResponse, QueryShapeError> {
    let includes = normalized_includes(&options.includes);
    if options.output_mode == QueryOutputMode::Flat
        && supports_direct_flat_shaping(&rows, &includes, relation.is_some())
    {
        return shape_direct_flat_results(
            connection,
            rows,
            includes,
            relation,
            path_strategy,
            shaping_strategy,
        );
    }

    let context = EnrichmentContext::load(connection, &rows, &includes)?;
    let results = match (&rows, options.output_mode) {
        (QueryRows::Headings(rows), QueryOutputMode::Flat) => rows
            .iter()
            .map(|row| match row {
                HeadingQueryMatch::File(row) => context
                    .shape_file_node(row.id, ResultDomain::Headings, true, &includes, false)
                    .map(QueryResultNode::File),
                HeadingQueryMatch::Heading(row) => context
                    .shape_heading_node(row.id, true, &includes, false)
                    .map(QueryResultNode::Heading),
            })
            .collect::<Result<Vec<_>, QueryShapeError>>()?,
        (QueryRows::Headings(rows), QueryOutputMode::Outline) => {
            context.shape_heading_outline(rows, &includes)?
        }
        (QueryRows::Links(rows), QueryOutputMode::Flat) => rows
            .iter()
            .map(|row| {
                context
                    .shape_link_node(row, &includes)
                    .map(|node| QueryResultNode::Link(Box::new(node)))
            })
            .collect::<Result<Vec<_>, QueryShapeError>>()?,
        (QueryRows::Links(rows), QueryOutputMode::Outline) => {
            context.shape_link_outline(rows, &includes)?
        }
        (QueryRows::Files(rows), QueryOutputMode::Flat) => rows
            .iter()
            .map(|row| {
                context
                    .shape_file_node(row.id, ResultDomain::Files, true, &includes, false)
                    .map(QueryResultNode::File)
            })
            .collect::<Result<Vec<_>, QueryShapeError>>()?,
        (QueryRows::Files(rows), QueryOutputMode::Outline) => rows
            .iter()
            .map(|row| {
                context
                    .shape_file_node(row.id, ResultDomain::Files, true, &includes, true)
                    .map(QueryResultNode::File)
            })
            .collect::<Result<Vec<_>, QueryShapeError>>()?,
    };

    Ok(QueryResponse {
        target: match rows {
            QueryRows::Headings(_) => QueryTarget::Headings,
            QueryRows::Links(_) => QueryTarget::Links,
            QueryRows::Files(_) => QueryTarget::Files,
        },
        output: options.output_mode,
        includes,
        results,
    })
}

pub(in crate::query::result) fn supports_direct_flat_shaping(
    rows: &QueryRows,
    includes: &[QueryInclude],
    has_relation: bool,
) -> bool {
    let supports_path = has_relation && !matches!(rows, QueryRows::Links(_));
    includes.iter().all(|include| {
        matches!(
            include,
            QueryInclude::Properties | QueryInclude::EffectiveProperties | QueryInclude::Keywords
        ) || (supports_path && *include == QueryInclude::Path)
    })
}

pub(in crate::query::result) fn shape_direct_flat_results(
    connection: &Connection,
    rows: QueryRows,
    includes: Vec<QueryInclude>,
    relation: Option<&MatchedSqlRelation>,
    path_strategy: HeadingPathStrategy,
    shaping_strategy: DirectFlatShapingStrategy,
) -> Result<QueryResponse, QueryShapeError> {
    let mut metadata =
        FlatMetadataContext::load(connection, &rows, &includes, relation, path_strategy)?;
    let target = match &rows {
        QueryRows::Headings(_) => QueryTarget::Headings,
        QueryRows::Links(_) => QueryTarget::Links,
        QueryRows::Files(_) => QueryTarget::Files,
    };
    let shaping_started = benchmark_trace::active().then(Instant::now);
    let results = match shaping_strategy {
        DirectFlatShapingStrategy::CloneBaseline => match &rows {
            QueryRows::Headings(rows) => rows
                .iter()
                .map(|row| match row {
                    HeadingQueryMatch::File(row) => metadata
                        .shape_file_row_cloned(row, ResultDomain::Headings, &includes)
                        .map(QueryResultNode::File),
                    HeadingQueryMatch::Heading(row) => metadata
                        .shape_heading_row_cloned(row, &includes)
                        .map(QueryResultNode::Heading),
                })
                .collect::<Result<Vec<_>, QueryShapeError>>()?,
            QueryRows::Links(rows) => rows
                .iter()
                .map(|row| QueryResultNode::Link(Box::new(metadata.shape_link_row(row))))
                .collect(),
            QueryRows::Files(rows) => rows
                .iter()
                .map(|row| {
                    metadata
                        .shape_file_row_cloned(row, ResultDomain::Files, &includes)
                        .map(QueryResultNode::File)
                })
                .collect::<Result<Vec<_>, QueryShapeError>>()?,
        },
        DirectFlatShapingStrategy::MoveOwned => match rows {
            QueryRows::Headings(rows) => rows
                .into_iter()
                .map(|row| match row {
                    HeadingQueryMatch::File(row) => metadata
                        .shape_file_row(row, ResultDomain::Headings, &includes)
                        .map(QueryResultNode::File),
                    HeadingQueryMatch::Heading(row) => metadata
                        .shape_heading_row(row, &includes)
                        .map(QueryResultNode::Heading),
                })
                .collect::<Result<Vec<_>, QueryShapeError>>()?,
            QueryRows::Links(rows) => rows
                .iter()
                .map(|row| QueryResultNode::Link(Box::new(metadata.shape_link_row(row))))
                .collect(),
            QueryRows::Files(rows) => rows
                .into_iter()
                .map(|row| {
                    metadata
                        .shape_file_row(row, ResultDomain::Files, &includes)
                        .map(QueryResultNode::File)
                })
                .collect::<Result<Vec<_>, QueryShapeError>>()?,
        },
    };
    let shaping_duration = shaping_started.map(|started| started.elapsed());
    metadata.record_shaping_detail();
    if let Some(duration) = shaping_duration {
        benchmark_trace::record(
            benchmark_trace::FINAL_RESULT_SHAPING,
            "direct-flat-results",
            duration,
            results.len(),
            0,
            0,
        );
    }

    Ok(QueryResponse {
        target,
        output: QueryOutputMode::Flat,
        includes,
        results,
    })
}

pub fn shape_matched_heading_nodes(
    connection: &Connection,
    heading_ids: &[i64],
) -> Result<Vec<HeadingResultNode>, QueryShapeError> {
    if heading_ids.is_empty() {
        return Ok(Vec::new());
    }

    let file_ids = load_file_ids_for_headings(connection, heading_ids)?;
    let context = EnrichmentContext {
        files: load_files(connection, &file_ids)?,
        headings: load_headings_for_files(connection, &file_ids)?,
        properties: HashMap::new(),
        effective_properties: HashMap::new(),
        keywords: HashMap::new(),
        links_by_file: HashMap::new(),
        links_by_heading: HashMap::new(),
        backlinks_by_file: HashMap::new(),
        backlinks_by_heading: HashMap::new(),
    };

    heading_ids
        .iter()
        .map(|heading_id| context.shape_heading_node(*heading_id, true, &[], false))
        .collect()
}

pub(in crate::query::result) fn normalized_includes(
    includes: &[QueryInclude],
) -> Vec<QueryInclude> {
    includes
        .iter()
        .copied()
        .collect::<BTreeSet<_>>()
        .into_iter()
        .collect()
}
