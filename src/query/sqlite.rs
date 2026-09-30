use std::{
    env, fmt,
    time::{Duration, Instant},
};

use regex::Regex;
use rusqlite::{
    params_from_iter,
    types::{ToSqlOutput, Value},
    Connection, ToSql,
};
use serde::Serialize;

use super::benchmark_trace;
use super::priority::normalize_priority;
use super::result::QueryExecutionOptions;
use super::sql_support::id_chunk_capacity;
use super::{
    ensure_relative_dates_resolved, resolve_relative_dates, resolve_temporal_bounds,
    QueryDateResolutionOptions, QueryTarget, QueryValidationOptions, QueryValue, ValidatedArg,
    ValidatedExpr, ValidatedOption, ValidatedPredicate, ValidatedQuery,
};
use crate::db::DB_METADATA_BODY_TEXT_AVAILABLE_KEY;
use crate::property::normalize_property_key;

mod compile;
mod execute;
mod predicates;
mod relation;
mod rows;
mod sql;
#[cfg(test)]
mod tests;

pub use self::compile::compile_sqlite_query;
use self::compile::*;
pub(crate) use self::compile::{
    compile_sqlite_query_with_file_restriction, compile_sqlite_query_with_metadata_strategy,
    MetadataPredicateSqlStrategy, PRODUCTION_METADATA_PREDICATE_SQL_STRATEGY,
};
#[cfg(test)]
pub(crate) use self::execute::execute_sqlite_query_with_relation_and_metadata_strategy;
pub use self::execute::{
    execute_sqlite_query, execute_sqlite_query_with_options, sqlite_query_validation_options,
};
pub(crate) use self::execute::{
    execute_sqlite_query_with_relation, execute_sqlite_query_with_relation_and_strategies,
};
use self::predicates::*;
use self::relation::*;
pub(crate) use self::relation::{
    cleanup_temporary_matched_relation, file_relation_columns, heading_matched_relation_cost,
    heading_relation_columns, ExecutedSqliteQuery, MatchedRelationCost,
    MatchedRelationReuseStrategy, MatchedSqlRelation, TemporaryMatchedRelation,
    PRODUCTION_MATCHED_RELATION_REUSE_STRATEGY,
};
use self::rows::*;
pub use self::rows::{
    CompiledSqlQuery, FileQueryRow, HeadingQueryMatch, HeadingQueryRow, LinkQueryRow,
    QueryExecutionError, QueryExecutionErrorKind, QueryParam, QueryRows,
};
use self::sql::*;
