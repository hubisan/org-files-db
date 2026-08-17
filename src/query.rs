pub mod ast;
pub(crate) mod benchmark_trace;
pub mod error;
pub mod parser;
mod priority;
pub mod resolve;
pub mod result;
pub(crate) mod sql_support;
pub mod sqlite;
pub mod validate;

pub use ast::{
    Expr, PredicateArg, PredicateCall, QueryAst, QueryOption, QueryTarget, QueryValue,
    TemporalBounds,
};
pub use error::{QueryParseError, QueryParseErrorKind};
pub use parser::parse_query;
pub use resolve::{
    ensure_relative_dates_resolved, resolve_relative_dates, resolve_temporal_bounds,
    QueryDateResolutionError, QueryDateResolutionErrorKind, QueryDateResolutionOptions,
};
pub use result::{
    execute_and_shape_query, shape_matched_heading_nodes, shape_query_results,
    EffectivePropertyFact, FileResultNode, HeadingResultNode, IncludedLink, KeywordFact,
    LinkResultNode, LinkSource, LinkTarget, Location, PathEntry, PropertyFact,
    QueryExecutionOptions, QueryInclude, QueryOutputMode, QueryResponse, QueryResultKind,
    QueryResultNode, QueryShapeError, QueryShapeErrorKind,
};
pub use sqlite::{
    compile_sqlite_query, execute_sqlite_query, execute_sqlite_query_with_options,
    sqlite_query_validation_options, CompiledSqlQuery, FileQueryRow, HeadingQueryMatch,
    HeadingQueryRow, LinkQueryRow, QueryExecutionError, QueryExecutionErrorKind, QueryParam,
    QueryRows,
};
pub use validate::{
    validate_query, QueryValidationError, QueryValidationErrorKind, QueryValidationOptions,
    ValidatedArg, ValidatedExpr, ValidatedOption, ValidatedPredicate, ValidatedQuery,
};
