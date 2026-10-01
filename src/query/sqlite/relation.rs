use super::*;

pub(in crate::query::sqlite) const HEADING_RELATION_COLUMNS: &str = "id, file_id, file_path, parent_id, level, line_number, byte_start, byte_end, title, title_raw, todo_keyword, todo_type, priority, scheduled_raw, scheduled_ts, deadline_raw, deadline_ts, closed_raw, closed_ts, archivedp, footnote_section_p";

pub(in crate::query::sqlite) const FILE_RELATION_COLUMNS: &str = "id, path, mtime_ns, size, content_hash, indexed_at, root_heading_id, root_title, root_title_raw, root_line_number";

pub(in crate::query::sqlite) const QUERY_MATCHED_HEADINGS_TABLE: &str =
    "orgfdb_query_matched_headings";

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum MatchedRelationReuseStrategy {
    QueryDerived,
    SelectiveTemp,
}

pub(crate) const PRODUCTION_MATCHED_RELATION_REUSE_STRATEGY: MatchedRelationReuseStrategy =
    MatchedRelationReuseStrategy::SelectiveTemp;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum MatchedRelationCost {
    Cheap,
    Expensive,
}

#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub(in crate::query::sqlite) struct MatchedRelationCostProfile {
    pub(in crate::query::sqlite) predicate_driven_metadata_relations: usize,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum TemporaryMatchedRelation {
    Headings,
}

#[derive(Debug, Clone)]
pub(crate) enum MatchedSqlRelation {
    Headings {
        headings: CompiledSqlQuery,
        roots: Option<CompiledSqlQuery>,
    },
    Links,
    Files(CompiledSqlQuery),
}

impl MatchedSqlRelation {
    pub(crate) fn heading_relation(&self) -> Option<&CompiledSqlQuery> {
        match self {
            Self::Headings { headings, .. } => Some(headings),
            Self::Links | Self::Files(_) => None,
        }
    }

    pub(crate) fn root_relation(&self) -> Option<&CompiledSqlQuery> {
        match self {
            Self::Headings { roots, .. } => roots.as_ref(),
            Self::Files(compiled) => Some(compiled),
            Self::Links => None,
        }
    }
}

#[derive(Debug, Clone)]
pub(crate) struct ExecutedSqliteQuery {
    pub(crate) rows: QueryRows,
    pub(crate) relation: MatchedSqlRelation,
    pub(crate) temporary_relation: Option<TemporaryMatchedRelation>,
}

pub(crate) fn heading_relation_columns() -> &'static str {
    HEADING_RELATION_COLUMNS
}

pub(crate) fn file_relation_columns() -> &'static str {
    FILE_RELATION_COLUMNS
}

pub(crate) fn heading_matched_relation_cost(query: &ValidatedQuery) -> MatchedRelationCost {
    let profile = query.predicate.as_ref().map_or_else(
        MatchedRelationCostProfile::default,
        matched_relation_cost_profile,
    );

    if profile.predicate_driven_metadata_relations >= 2 {
        MatchedRelationCost::Expensive
    } else {
        MatchedRelationCost::Cheap
    }
}

pub(in crate::query::sqlite) fn matched_relation_cost_profile(
    expr: &ValidatedExpr,
) -> MatchedRelationCostProfile {
    match expr {
        ValidatedExpr::And(children) | ValidatedExpr::Or(children) => {
            let mut profile = MatchedRelationCostProfile::default();
            for child in children {
                profile.add(matched_relation_cost_profile(child));
            }
            profile
        }
        ValidatedExpr::Not(child) => matched_relation_cost_profile(child),
        ValidatedExpr::Predicate(predicate) => matched_predicate_cost_profile(predicate),
    }
}

pub(in crate::query::sqlite) fn matched_predicate_cost_profile(
    predicate: &ValidatedPredicate,
) -> MatchedRelationCostProfile {
    let mut profile = MatchedRelationCostProfile::default();
    if matches!(predicate.name.as_str(), "tags" | "property" | "keyword") {
        profile.predicate_driven_metadata_relations += 1;
    }

    for arg in &predicate.args {
        if let ValidatedArg::NestedQuery(query) = arg {
            if let Some(expr) = query.predicate.as_ref() {
                profile.add(matched_relation_cost_profile(expr));
            }
        }
    }

    profile
}

impl MatchedRelationCostProfile {
    pub(in crate::query::sqlite) fn add(&mut self, other: Self) {
        self.predicate_driven_metadata_relations += other.predicate_driven_metadata_relations;
    }
}

pub(in crate::query::sqlite) fn materialize_heading_relation(
    connection: &Connection,
    compiled: &CompiledSqlQuery,
) -> Result<CompiledSqlQuery, QueryExecutionError> {
    reset_temporary_heading_relation(connection)?;

    let create_sql = format!(
        "/* orgfdb:temp-matched-headings-create params=0 */
         CREATE TEMP TABLE temp.{QUERY_MATCHED_HEADINGS_TABLE} (
             id INTEGER PRIMARY KEY,
             file_id INTEGER NOT NULL,
             file_path TEXT NOT NULL,
             parent_id INTEGER,
             level INTEGER NOT NULL,
             line_number INTEGER,
             byte_start INTEGER NOT NULL,
             byte_end INTEGER NOT NULL,
             title TEXT NOT NULL,
             title_raw TEXT,
             todo_keyword TEXT,
             todo_type TEXT,
             priority TEXT,
             scheduled_raw TEXT,
             scheduled_ts INTEGER,
             deadline_raw TEXT,
             deadline_ts INTEGER,
             closed_raw TEXT,
             closed_ts INTEGER,
             archivedp INTEGER NOT NULL,
             footnote_section_p INTEGER NOT NULL
         ) WITHOUT ROWID"
    );
    connection.execute_batch(&create_sql).map_err(|source| {
        QueryExecutionError::database(
            QueryTarget::Headings,
            "materialize_heading_relation.create",
            source,
        )
    })?;

    let populate_sql = format!(
        "/* orgfdb:temp-matched-headings-populate params={} */
         INSERT INTO temp.{QUERY_MATCHED_HEADINGS_TABLE} ({HEADING_RELATION_COLUMNS}) {}",
        compiled.params.len(),
        compiled.sql,
    );
    if let Err(source) = connection.execute(&populate_sql, params_from_iter(compiled.params.iter()))
    {
        let _ = cleanup_temporary_matched_relation(connection, TemporaryMatchedRelation::Headings);
        return Err(QueryExecutionError::database(
            QueryTarget::Headings,
            "materialize_heading_relation.populate",
            source,
        ));
    }

    Ok(CompiledSqlQuery {
        target: QueryTarget::Headings,
        sql: format!(
            "/* orgfdb:match-headings-temp params=0 */
             SELECT {HEADING_RELATION_COLUMNS}
             FROM temp.{QUERY_MATCHED_HEADINGS_TABLE}"
        ),
        params: Vec::new(),
    })
}

pub(in crate::query::sqlite) fn reset_temporary_heading_relation(
    connection: &Connection,
) -> Result<(), QueryExecutionError> {
    let sql = format!(
        "/* orgfdb:temp-matched-headings-reset params=0 */
         DROP TABLE IF EXISTS temp.{QUERY_MATCHED_HEADINGS_TABLE}"
    );
    connection.execute_batch(&sql).map_err(|source| {
        QueryExecutionError::database(
            QueryTarget::Headings,
            "materialize_heading_relation.reset",
            source,
        )
    })
}

pub(crate) fn cleanup_temporary_matched_relation(
    connection: &Connection,
    relation: TemporaryMatchedRelation,
) -> Result<(), QueryExecutionError> {
    match relation {
        TemporaryMatchedRelation::Headings => {
            let sql = format!(
                "/* orgfdb:temp-matched-headings-drop params=0 */
                 DROP TABLE IF EXISTS temp.{QUERY_MATCHED_HEADINGS_TABLE}"
            );
            connection.execute_batch(&sql).map_err(|source| {
                QueryExecutionError::database(
                    QueryTarget::Headings,
                    "materialize_heading_relation.cleanup",
                    source,
                )
            })
        }
    }
}
