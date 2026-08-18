//! Measurement-only benchmark support for query SQL execution and enrichment.

use std::{
    cell::RefCell,
    collections::BTreeMap,
    fs,
    path::Path,
    time::{Duration, Instant},
};

use rusqlite::{limits::Limit, params, params_from_iter, Connection};
use serde::Serialize;

use crate::{
    db::open_existing_database_read_only,
    presentation_benchmark::{prepare_benchmark_databases, Timing},
    query::{
        compile_sqlite_query, execute_and_shape_query, parse_query, resolve_relative_dates,
        resolve_temporal_bounds, sqlite_query_validation_options, validate_query, CompiledSqlQuery,
        QueryDateResolutionOptions, QueryExecutionOptions, QueryInclude, QueryOutputMode,
        QueryParam, QueryRows,
    },
};

use crate::query::benchmark_trace::{self, BenchmarkTraceRecord};
use crate::query::result::{
    execute_and_shape_query_with_direct_flat_shaping_strategy,
    execute_and_shape_query_with_metadata_strategy, execute_and_shape_query_with_path_strategy,
    execute_and_shape_query_with_relation_reuse_strategy, load_heading_paths_from_relation,
    load_heading_paths_recursive_from_relation, DirectFlatShapingStrategy, HeadingPathStrategy,
};
use crate::query::sql_support::{id_chunk_capacity, variable_number_limit};
use crate::query::sqlite::{
    compile_sqlite_query_with_file_restriction, compile_sqlite_query_with_metadata_strategy,
    execute_sqlite_query_with_relation, heading_matched_relation_cost, heading_relation_columns,
    MatchedRelationCost, MatchedRelationReuseStrategy, MetadataPredicateSqlStrategy,
};

pub const OUTPUT_SCHEMA_VERSION: &str = "12";
pub const DEFAULT_WARMUPS: usize = 1;
pub const DEFAULT_ITERATIONS: usize = 3;
pub const DEFAULT_ROW_COUNTS: &[usize] = &[100, 1_000, 10_000, 50_000];

#[derive(Debug, Clone)]
pub struct QuerySqlBenchmarkOptions {
    pub row_counts: Vec<usize>,
    pub warmups: usize,
    pub iterations: usize,
    pub path_variable_limits: Vec<usize>,
}

impl Default for QuerySqlBenchmarkOptions {
    fn default() -> Self {
        Self {
            row_counts: DEFAULT_ROW_COUNTS.to_vec(),
            warmups: DEFAULT_WARMUPS,
            iterations: DEFAULT_ITERATIONS,
            path_variable_limits: Vec::new(),
        }
    }
}

#[derive(Debug, Serialize)]
pub struct QuerySqlBenchmarkOutput {
    pub output_schema_version: &'static str,
    pub protocol: QuerySqlBenchmarkProtocol,
    pub environment: QuerySqlBenchmarkEnvironment,
    pub sizes: Vec<QuerySqlSizeResult>,
    pub query_plans: Vec<QueryPlanResult>,
    pub compiler_audit: Vec<QueryCompilerAuditResult>,
}

#[derive(Debug, Serialize)]
pub struct QuerySqlBenchmarkProtocol {
    pub warmups: usize,
    pub iterations: usize,
    pub row_counts: Vec<usize>,
    pub path_variable_limits: Vec<usize>,
    pub database_source: &'static str,
    pub timing_policy: &'static str,
    pub profile_policy: &'static str,
    pub phase_policy: &'static str,
    pub strategy_policy: &'static str,
}

#[derive(Debug, Serialize)]
pub struct QuerySqlBenchmarkEnvironment {
    pub command_arguments: Vec<String>,
    pub build_profile: &'static str,
    pub operating_system: &'static str,
    pub architecture: &'static str,
}

#[derive(Debug, Serialize)]
pub struct QuerySqlSizeResult {
    pub target_results: usize,
    pub sqlite_variable_limit: usize,
    pub id_chunk_capacity: usize,
    pub workloads: Vec<QuerySqlWorkloadResult>,
    pub final_shaping_strategies: Vec<FinalShapingStrategyResult>,
    pub lookup_strategies: Vec<LookupStrategyResult>,
    pub relation_reuse_strategies: Vec<RelationReuseStrategyResult>,
    pub production_relation_reuse_strategies: Vec<ProductionRelationReuseStrategyResult>,
    pub path_strategies: Vec<PathStrategyResult>,
    pub production_path_strategies: Vec<ProductionPathStrategyResult>,
    pub metadata_predicate_strategies: Vec<MetadataPredicateStrategyResult>,
    pub production_metadata_predicate_strategies: Vec<ProductionMetadataPredicateStrategyResult>,
    pub metadata_index_costs: Vec<MetadataIndexCostResult>,
}

#[derive(Debug, Serialize)]
pub struct QuerySqlWorkloadResult {
    pub id: &'static str,
    pub query: &'static str,
    pub includes: Vec<QueryInclude>,
    pub result_count: usize,
    pub total_query_time: Timing,
    pub sql_profile: SqlProfileSummary,
    pub direct_phase_profile: DirectPhaseProfile,
}

#[derive(Debug, Serialize)]
pub struct FinalShapingStrategyResult {
    pub workload: &'static str,
    pub result_count: usize,
    pub baseline_strategy: &'static str,
    pub candidate_strategy: &'static str,
    pub baseline_total_query_time: Timing,
    pub candidate_total_query_time: Timing,
    pub baseline_direct_phase_profile: DirectPhaseProfile,
    pub candidate_direct_phase_profile: DirectPhaseProfile,
}

#[derive(Debug, Serialize)]
pub struct DirectPhaseProfile {
    pub rows_transferred_from_sqlite: usize,
    pub statement_count: usize,
    pub total_bound_parameters: usize,
    pub max_bound_parameters: usize,
    pub phases: Vec<DirectPhaseSummary>,
    pub operations: Vec<DirectPhaseOperation>,
}

#[derive(Debug, Serialize)]
pub struct DirectPhaseSummary {
    pub phase: String,
    pub duration_ns: u128,
    pub rows: usize,
    pub statement_count: usize,
    pub total_bound_parameters: usize,
}

#[derive(Debug, Serialize)]
pub struct DirectPhaseOperation {
    pub phase: &'static str,
    pub operation: &'static str,
    pub duration_ns: u128,
    pub rows: usize,
    pub statement_count: usize,
    pub bound_parameters: usize,
}

#[derive(Debug, Serialize)]
pub struct SqlProfileSummary {
    pub statement_count: usize,
    pub total_bound_parameters: usize,
    pub max_bound_parameters: usize,
    pub sqlite_profile_ns: u128,
    pub stages: Vec<SqlProfileStage>,
}

#[derive(Debug, Serialize)]
pub struct SqlProfileStage {
    pub stage: String,
    pub statement_count: usize,
    pub total_bound_parameters: usize,
    pub max_bound_parameters: usize,
    pub sqlite_profile_ns: u128,
}

#[derive(Debug, Serialize)]
pub struct LookupStrategyResult {
    pub strategy: &'static str,
    pub selected_heading_ids: usize,
    pub returned_rows: usize,
    pub statement_count_per_sample: usize,
    pub max_bound_parameters: usize,
    pub timing: Timing,
}

#[derive(Debug, Serialize)]
pub struct RelationReuseStrategyResult {
    pub workload: &'static str,
    pub strategy: &'static str,
    pub matched_heading_ids: usize,
    pub returned_rows: usize,
    pub rows_transferred_to_rust_per_sample: usize,
    pub statement_count_per_sample: usize,
    pub timing: Timing,
    pub sql_profile: SqlProfileSummary,
    pub query_plan: Vec<String>,
}

#[derive(Debug, Serialize)]
pub struct ProductionRelationReuseStrategyResult {
    pub workload: &'static str,
    pub query: &'static str,
    pub includes: Vec<QueryInclude>,
    pub relation_cost: &'static str,
    pub strategy: &'static str,
    pub temp_selected: bool,
    pub result_count: usize,
    pub total_query_time: Timing,
    pub sql_profile: SqlProfileSummary,
    pub direct_phase_profile: DirectPhaseProfile,
}

#[derive(Debug, Serialize)]
pub struct PathStrategyResult {
    pub strategy: &'static str,
    pub requested_sqlite_variable_limit: usize,
    pub sqlite_variable_limit: usize,
    pub id_chunk_capacity: usize,
    pub matched_heading_ids: usize,
    pub returned_paths: usize,
    pub timing: Timing,
    pub direct_phase_profile: DirectPhaseProfile,
}

#[derive(Debug, Serialize)]
pub struct ProductionPathStrategyResult {
    pub strategy: &'static str,
    pub result_count: usize,
    pub total_query_time: Timing,
    pub sql_profile: SqlProfileSummary,
    pub direct_phase_profile: DirectPhaseProfile,
}

#[derive(Debug, Serialize)]
pub struct ProductionMetadataPredicateStrategyResult {
    pub predicate: &'static str,
    pub selectivity_percent: usize,
    pub actual_selectivity_percent: f64,
    pub strategy: &'static str,
    pub experimental_index: Option<&'static str>,
    pub eligible_headings: usize,
    pub result_count: usize,
    pub total_query_time: Timing,
    pub sql_profile: SqlProfileSummary,
    pub direct_phase_profile: DirectPhaseProfile,
    pub query_plan: Vec<String>,
}

#[derive(Debug, Serialize)]
pub struct MetadataPredicateStrategyResult {
    pub predicate: &'static str,
    pub selectivity_percent: usize,
    pub actual_selectivity_percent: f64,
    pub strategy: &'static str,
    pub experimental_index: Option<&'static str>,
    pub eligible_headings: usize,
    pub matched_headings: usize,
    pub returned_rows: usize,
    pub rows_scanned: Option<usize>,
    pub statement_count_per_sample: usize,
    pub rows_transferred_to_rust_per_sample: usize,
    pub total_query_time: Timing,
    pub query_plan: Vec<String>,
}

#[derive(Debug, Serialize)]
pub struct MetadataIndexCostResult {
    pub index_set: &'static str,
    pub indexes: Vec<&'static str>,
    pub database_size_before_bytes: u64,
    pub database_size_after_bytes: u64,
    pub database_size_delta_bytes: u64,
    pub database_size_delta_percent: f64,
    pub build_time: Timing,
    pub representative_write: Option<MetadataIndexWriteCostResult>,
}

#[derive(Debug, Serialize)]
pub struct MetadataIndexWriteCostResult {
    pub table: &'static str,
    pub rows: usize,
    pub operation: &'static str,
    pub without_indexes: Timing,
    pub with_indexes: Timing,
}

#[derive(Debug, Serialize)]
pub struct QueryCompilerAuditResult {
    pub predicate: &'static str,
    pub current_lookup: &'static str,
    pub skip_related_lookup: bool,
    pub finding: &'static str,
}

#[derive(Debug, Serialize)]
pub struct QueryPlanResult {
    pub id: &'static str,
    pub query: &'static str,
    pub details: Vec<String>,
}

#[derive(Debug, Clone, Copy)]
struct Workload {
    id: &'static str,
    query: &'static str,
    includes: &'static [QueryInclude],
}

const NO_INCLUDES: &[QueryInclude] = &[];
const PROPERTIES_INCLUDE: &[QueryInclude] = &[QueryInclude::Properties];
const EFFECTIVE_PROPERTIES_INCLUDE: &[QueryInclude] = &[QueryInclude::EffectiveProperties];
const KEYWORDS_INCLUDE: &[QueryInclude] = &[QueryInclude::Keywords];
const PATH_INCLUDE: &[QueryInclude] = &[QueryInclude::Path];

const WORKLOADS: &[Workload] = &[
    Workload {
        id: "headings.normal",
        query: "(headings (level 1))",
        includes: NO_INCLUDES,
    },
    Workload {
        id: "headings.tags",
        query: "(headings (level 1))",
        includes: NO_INCLUDES,
    },
    Workload {
        id: "headings.properties",
        query: "(headings (level 1))",
        includes: PROPERTIES_INCLUDE,
    },
    Workload {
        id: "headings.effective-properties",
        query: "(headings (level 1))",
        includes: EFFECTIVE_PROPERTIES_INCLUDE,
    },
    Workload {
        id: "headings.path",
        query: "(headings (level 1))",
        includes: PATH_INCLUDE,
    },
    Workload {
        id: "files.normal",
        query: "(files)",
        includes: NO_INCLUDES,
    },
    Workload {
        id: "files.keywords",
        query: "(files)",
        includes: KEYWORDS_INCLUDE,
    },
];

#[derive(Debug, Clone)]
struct ProfileRecord {
    stage: String,
    bound_parameters: usize,
    duration_ns: u128,
}

thread_local! {
    static PROFILE_RECORDS: RefCell<Vec<ProfileRecord>> = const { RefCell::new(Vec::new()) };
}

pub fn run(
    output: &Path,
    work_dir: &Path,
    options: QuerySqlBenchmarkOptions,
) -> Result<(), String> {
    validate_options(&options)?;
    if output.exists() {
        return Err(format!(
            "SQL benchmark output must not already exist: {}",
            output.display()
        ));
    }
    let work_dir = prepare_benchmark_databases(work_dir, &options.row_counts, 1)?;

    let mut sizes = Vec::with_capacity(options.row_counts.len());
    for target_results in &options.row_counts {
        sizes.push(run_size(&work_dir, *target_results, &options)?);
    }
    let plan_size = *options
        .row_counts
        .iter()
        .max()
        .expect("validated benchmark row counts are non-empty");
    let plan_db = work_dir
        .join(format!("rows-{plan_size}"))
        .join("org-files-db.sqlite");
    let query_plans = collect_query_plans(&plan_db)?;

    let result = QuerySqlBenchmarkOutput {
        output_schema_version: OUTPUT_SCHEMA_VERSION,
        protocol: QuerySqlBenchmarkProtocol {
            warmups: options.warmups,
            iterations: options.iterations,
            row_counts: options.row_counts.clone(),
            path_variable_limits: options.path_variable_limits.clone(),
            database_source: "existing or generated orgfdb-presentation-benchmark corpus databases",
            timing_policy: "profile callbacks are disabled during latency samples",
            profile_policy: "one separate profiled production query records statement stages and bound parameters",
            phase_policy: "one separate instrumented production query records direct Rust and SQLite boundary timings; latency samples run without phase instrumentation",
            strategy_policy: "lookup microbenchmarks compare ID transport, compiler-derived relation reuse, selective production TEMP reuse, paired final shaping baseline versus owned moves, isolated path loading, complete production path shaping, metadata predicate SQL shapes, complete production metadata shaping, persistent metadata index costs, variable-limit sensitivity, and representative query plans",
        },
        environment: QuerySqlBenchmarkEnvironment {
            command_arguments: std::env::args().collect(),
            build_profile: if cfg!(debug_assertions) {
                "debug"
            } else {
                "release"
            },
            operating_system: std::env::consts::OS,
            architecture: std::env::consts::ARCH,
        },
        sizes,
        query_plans,
        compiler_audit: query_compiler_audit(),
    };

    if let Some(parent) = output
        .parent()
        .filter(|parent| !parent.as_os_str().is_empty())
    {
        fs::create_dir_all(parent).map_err(|error| error.to_string())?;
    }
    fs::write(
        output,
        serde_json::to_vec_pretty(&result).map_err(|error| error.to_string())?,
    )
    .map_err(|error| error.to_string())
}

fn validate_options(options: &QuerySqlBenchmarkOptions) -> Result<(), String> {
    if options.row_counts.is_empty() || options.row_counts.contains(&0) {
        return Err("--rows must contain positive row counts".into());
    }
    if options.iterations == 0 {
        return Err("--iterations must be positive".into());
    }
    for limit in &options.path_variable_limits {
        if *limit == 0 {
            return Err("--path-variable-limits values must be positive".into());
        }
        i32::try_from(*limit)
            .map_err(|_| "--path-variable-limits values must fit a signed 32-bit integer")?;
    }
    Ok(())
}

fn run_size(
    work_dir: &Path,
    target_results: usize,
    options: &QuerySqlBenchmarkOptions,
) -> Result<QuerySqlSizeResult, String> {
    let db_path = work_dir
        .join(format!("rows-{target_results}"))
        .join("org-files-db.sqlite");
    if !db_path.is_file() {
        return Err(format!(
            "benchmark database does not exist for {target_results} results: {}",
            db_path.display()
        ));
    }

    let probe = open_existing_database_read_only(&db_path).map_err(|error| error.to_string())?;
    let sqlite_variable_limit = variable_number_limit(&probe);
    let chunk_capacity = id_chunk_capacity(&probe, 0);
    drop(probe);

    let mut workloads = Vec::with_capacity(WORKLOADS.len());
    for workload in WORKLOADS {
        workloads.push(measure_workload(
            &db_path,
            *workload,
            target_results,
            options,
        )?);
    }

    let final_shaping_strategies =
        measure_final_shaping_strategies(&db_path, target_results, options)?;

    let lookup_strategies = measure_lookup_strategies(&db_path, target_results, options)?;
    let relation_reuse_strategies = measure_relation_reuse_strategies(&db_path, options)?;
    let production_relation_reuse_strategies =
        measure_production_relation_reuse_strategies(&db_path, options)?;
    let path_strategies = measure_path_strategies(&db_path, target_results, options)?;
    let production_path_strategies =
        measure_production_path_strategies(&db_path, target_results, options)?;
    let metadata_predicate_strategies =
        measure_metadata_predicate_strategies(&db_path, target_results, options)?;
    let production_metadata_predicate_strategies =
        measure_production_metadata_predicate_strategies(&db_path, target_results, options)?;
    let metadata_index_costs = if target_results >= 50_000 {
        measure_metadata_index_costs(&db_path, target_results, options)?
    } else {
        Vec::new()
    };

    Ok(QuerySqlSizeResult {
        target_results,
        sqlite_variable_limit,
        id_chunk_capacity: chunk_capacity,
        workloads,
        final_shaping_strategies,
        lookup_strategies,
        relation_reuse_strategies,
        production_relation_reuse_strategies,
        path_strategies,
        production_path_strategies,
        metadata_predicate_strategies,
        production_metadata_predicate_strategies,
        metadata_index_costs,
    })
}

fn measure_workload(
    db_path: &Path,
    workload: Workload,
    expected_results: usize,
    options: &QuerySqlBenchmarkOptions,
) -> Result<QuerySqlWorkloadResult, String> {
    let mut connection =
        open_existing_database_read_only(db_path).map_err(|error| error.to_string())?;
    connection
        .execute_batch("BEGIN DEFERRED TRANSACTION")
        .map_err(|error| error.to_string())?;
    let parsed = parse_query(workload.query).map_err(|error| error.to_string())?;
    let validation_options =
        sqlite_query_validation_options(&connection).map_err(|error| error.to_string())?;
    let validated =
        validate_query(parsed, &validation_options).map_err(|error| error.to_string())?;
    let query_options = QueryExecutionOptions {
        output_mode: QueryOutputMode::Flat,
        includes: workload.includes.to_vec(),
        ..Default::default()
    };

    let first = execute_and_shape_query(&connection, &validated, &query_options)
        .map_err(|error| error.to_string())?;
    if first.results.len() != expected_results {
        return Err(format!(
            "{} returned {} results, expected {}",
            workload.id,
            first.results.len(),
            expected_results
        ));
    }

    let total_query_time = measure(options, || {
        let response = execute_and_shape_query(&connection, &validated, &query_options)
            .map_err(|error| error.to_string())?;
        if response.results.len() != expected_results {
            return Err(format!(
                "{} result count changed during SQL benchmark",
                workload.id
            ));
        }
        std::hint::black_box(response);
        Ok(())
    })?;

    PROFILE_RECORDS.with(|records| records.borrow_mut().clear());
    connection.profile(Some(profile_callback));
    let profiled = execute_and_shape_query(&connection, &validated, &query_options)
        .map_err(|error| error.to_string());
    connection.profile(None);
    let profiled = profiled?;
    if profiled.results.len() != expected_results {
        return Err(format!(
            "{} result count changed during SQL profile",
            workload.id
        ));
    }
    let sql_profile = take_profile_summary();

    benchmark_trace::begin();
    let phase_response = execute_and_shape_query(&connection, &validated, &query_options)
        .map_err(|error| error.to_string());
    let phase_records = benchmark_trace::finish();
    let phase_response = phase_response?;
    if phase_response.results.len() != expected_results {
        return Err(format!(
            "{} result count changed during direct phase profile",
            workload.id
        ));
    }
    let direct_phase_profile = summarize_direct_phase_profile(phase_records);

    connection
        .execute_batch("COMMIT")
        .map_err(|error| error.to_string())?;

    Ok(QuerySqlWorkloadResult {
        id: workload.id,
        query: workload.query,
        includes: workload.includes.to_vec(),
        result_count: expected_results,
        total_query_time,
        sql_profile,
        direct_phase_profile,
    })
}

fn measure_final_shaping_strategies(
    db_path: &Path,
    expected_results: usize,
    options: &QuerySqlBenchmarkOptions,
) -> Result<Vec<FinalShapingStrategyResult>, String> {
    let mut results = Vec::with_capacity(WORKLOADS.len());

    for workload in WORKLOADS {
        let connection =
            open_existing_database_read_only(db_path).map_err(|error| error.to_string())?;
        connection
            .execute_batch("BEGIN DEFERRED TRANSACTION")
            .map_err(|error| error.to_string())?;
        let parsed = parse_query(workload.query).map_err(|error| error.to_string())?;
        let validation_options =
            sqlite_query_validation_options(&connection).map_err(|error| error.to_string())?;
        let validated =
            validate_query(parsed, &validation_options).map_err(|error| error.to_string())?;
        let query_options = QueryExecutionOptions {
            output_mode: QueryOutputMode::Flat,
            includes: workload.includes.to_vec(),
            ..Default::default()
        };

        let baseline_reference = execute_and_shape_query_with_direct_flat_shaping_strategy(
            &connection,
            &validated,
            &query_options,
            DirectFlatShapingStrategy::CloneBaseline,
        )
        .map_err(|error| error.to_string())?;
        let candidate_reference = execute_and_shape_query_with_direct_flat_shaping_strategy(
            &connection,
            &validated,
            &query_options,
            DirectFlatShapingStrategy::MoveOwned,
        )
        .map_err(|error| error.to_string())?;
        if baseline_reference != candidate_reference {
            return Err(format!(
                "{} final shaping strategies returned different public query results",
                workload.id
            ));
        }
        if baseline_reference.results.len() != expected_results {
            return Err(format!(
                "{} final shaping strategy comparison returned {} results, expected {}",
                workload.id,
                baseline_reference.results.len(),
                expected_results
            ));
        }

        let (baseline_total_query_time, candidate_total_query_time) = measure_paired(
            options,
            || {
                let response = execute_and_shape_query_with_direct_flat_shaping_strategy(
                    &connection,
                    &validated,
                    &query_options,
                    DirectFlatShapingStrategy::CloneBaseline,
                )
                .map_err(|error| error.to_string())?;
                if response.results.len() != expected_results {
                    return Err(format!(
                        "{} clone baseline result count changed during paired benchmark",
                        workload.id
                    ));
                }
                std::hint::black_box(response);
                Ok(())
            },
            || {
                let response = execute_and_shape_query_with_direct_flat_shaping_strategy(
                    &connection,
                    &validated,
                    &query_options,
                    DirectFlatShapingStrategy::MoveOwned,
                )
                .map_err(|error| error.to_string())?;
                if response.results.len() != expected_results {
                    return Err(format!(
                        "{} owned candidate result count changed during paired benchmark",
                        workload.id
                    ));
                }
                std::hint::black_box(response);
                Ok(())
            },
        )?;

        benchmark_trace::begin();
        let baseline_phase = execute_and_shape_query_with_direct_flat_shaping_strategy(
            &connection,
            &validated,
            &query_options,
            DirectFlatShapingStrategy::CloneBaseline,
        )
        .map_err(|error| error.to_string());
        let baseline_phase_records = benchmark_trace::finish();
        let baseline_phase = baseline_phase?;
        if baseline_phase.results.len() != expected_results {
            return Err(format!(
                "{} clone baseline result count changed during direct phase profile",
                workload.id
            ));
        }

        benchmark_trace::begin();
        let candidate_phase = execute_and_shape_query_with_direct_flat_shaping_strategy(
            &connection,
            &validated,
            &query_options,
            DirectFlatShapingStrategy::MoveOwned,
        )
        .map_err(|error| error.to_string());
        let candidate_phase_records = benchmark_trace::finish();
        let candidate_phase = candidate_phase?;
        if candidate_phase.results.len() != expected_results {
            return Err(format!(
                "{} owned candidate result count changed during direct phase profile",
                workload.id
            ));
        }

        connection
            .execute_batch("COMMIT")
            .map_err(|error| error.to_string())?;

        results.push(FinalShapingStrategyResult {
            workload: workload.id,
            result_count: expected_results,
            baseline_strategy: "clone-baseline",
            candidate_strategy: "move-owned",
            baseline_total_query_time,
            candidate_total_query_time,
            baseline_direct_phase_profile: summarize_direct_phase_profile(baseline_phase_records),
            candidate_direct_phase_profile: summarize_direct_phase_profile(candidate_phase_records),
        });
    }

    Ok(results)
}

fn profile_callback(sql: &str, duration: Duration) {
    let (stage, bound_parameters) =
        parse_profile_marker(sql).unwrap_or_else(|| ("unclassified".to_string(), 0));
    PROFILE_RECORDS.with(|records| {
        records.borrow_mut().push(ProfileRecord {
            stage,
            bound_parameters,
            duration_ns: duration.as_nanos(),
        });
    });
}

fn parse_profile_marker(sql: &str) -> Option<(String, usize)> {
    let start = sql.find("/* orgfdb:")? + "/* orgfdb:".len();
    let end = sql[start..].find(" */")? + start;
    let marker = &sql[start..end];
    let mut parts = marker.split_whitespace();
    let stage = parts.next()?.to_string();
    let bound_parameters = parts
        .find_map(|part| part.strip_prefix("params="))?
        .parse()
        .ok()?;
    Some((stage, bound_parameters))
}

fn take_profile_summary() -> SqlProfileSummary {
    let records = PROFILE_RECORDS.with(|records| std::mem::take(&mut *records.borrow_mut()));
    let mut stages = BTreeMap::<String, SqlProfileStage>::new();
    let mut total_bound_parameters = 0usize;
    let mut max_bound_parameters = 0usize;
    let mut sqlite_profile_ns = 0u128;

    for record in &records {
        total_bound_parameters += record.bound_parameters;
        max_bound_parameters = max_bound_parameters.max(record.bound_parameters);
        sqlite_profile_ns += record.duration_ns;
        let stage = stages
            .entry(record.stage.clone())
            .or_insert_with(|| SqlProfileStage {
                stage: record.stage.clone(),
                statement_count: 0,
                total_bound_parameters: 0,
                max_bound_parameters: 0,
                sqlite_profile_ns: 0,
            });
        stage.statement_count += 1;
        stage.total_bound_parameters += record.bound_parameters;
        stage.max_bound_parameters = stage.max_bound_parameters.max(record.bound_parameters);
        stage.sqlite_profile_ns += record.duration_ns;
    }

    SqlProfileSummary {
        statement_count: records.len(),
        total_bound_parameters,
        max_bound_parameters,
        sqlite_profile_ns,
        stages: stages.into_values().collect(),
    }
}

fn summarize_direct_phase_profile(records: Vec<BenchmarkTraceRecord>) -> DirectPhaseProfile {
    let rows_transferred_from_sqlite = records
        .iter()
        .filter(|record| {
            record.phase == benchmark_trace::MATCHED_SQL_EXECUTION
                || record.phase == benchmark_trace::ENRICHMENT_SQL_EXECUTION
        })
        .map(|record| record.rows)
        .sum();
    let statement_count = records.iter().map(|record| record.statement_count).sum();
    let total_bound_parameters = records.iter().map(|record| record.bound_parameters).sum();
    let max_bound_parameters = records
        .iter()
        .map(|record| record.bound_parameters)
        .max()
        .unwrap_or(0);

    let mut phases = BTreeMap::<String, DirectPhaseSummary>::new();
    for record in &records {
        let phase = phases
            .entry(record.phase.to_string())
            .or_insert_with(|| DirectPhaseSummary {
                phase: record.phase.to_string(),
                duration_ns: 0,
                rows: 0,
                statement_count: 0,
                total_bound_parameters: 0,
            });
        phase.duration_ns += record.duration_ns;
        phase.rows += record.rows;
        phase.statement_count += record.statement_count;
        phase.total_bound_parameters += record.bound_parameters;
    }

    let operations = records
        .into_iter()
        .map(|record| DirectPhaseOperation {
            phase: record.phase,
            operation: record.operation,
            duration_ns: record.duration_ns,
            rows: record.rows,
            statement_count: record.statement_count,
            bound_parameters: record.bound_parameters,
        })
        .collect();

    DirectPhaseProfile {
        rows_transferred_from_sqlite,
        statement_count,
        total_bound_parameters,
        max_bound_parameters,
        phases: phases.into_values().collect(),
        operations,
    }
}

fn measure_path_strategies(
    db_path: &Path,
    expected_results: usize,
    options: &QuerySqlBenchmarkOptions,
) -> Result<Vec<PathStrategyResult>, String> {
    if options.path_variable_limits.is_empty() {
        return measure_path_strategies_at_limit(db_path, expected_results, options, None);
    }

    let mut results = Vec::with_capacity(options.path_variable_limits.len() * 2);
    for requested_limit in &options.path_variable_limits {
        results.extend(measure_path_strategies_at_limit(
            db_path,
            expected_results,
            options,
            Some(*requested_limit),
        )?);
    }
    Ok(results)
}

fn measure_path_strategies_at_limit(
    db_path: &Path,
    expected_results: usize,
    options: &QuerySqlBenchmarkOptions,
    requested_limit: Option<usize>,
) -> Result<Vec<PathStrategyResult>, String> {
    let connection =
        open_existing_database_read_only(db_path).map_err(|error| error.to_string())?;
    let default_limit = variable_number_limit(&connection);
    let requested_sqlite_variable_limit = requested_limit.unwrap_or(default_limit);
    if let Some(limit) = requested_limit {
        let limit = i32::try_from(limit)
            .map_err(|_| "path variable limit must fit a signed 32-bit integer")?;
        connection.set_limit(Limit::SQLITE_LIMIT_VARIABLE_NUMBER, limit);
    }
    let sqlite_variable_limit = variable_number_limit(&connection);
    let chunk_capacity = id_chunk_capacity(&connection, 0);

    connection
        .execute_batch("BEGIN DEFERRED TRANSACTION")
        .map_err(|error| error.to_string())?;

    let parsed = parse_query("(headings (level 1))").map_err(|error| error.to_string())?;
    let validation_options =
        sqlite_query_validation_options(&connection).map_err(|error| error.to_string())?;
    let validated =
        validate_query(parsed, &validation_options).map_err(|error| error.to_string())?;
    let query_options = QueryExecutionOptions {
        output_mode: QueryOutputMode::Flat,
        includes: vec![QueryInclude::Path],
        ..Default::default()
    };
    let executed = execute_sqlite_query_with_relation(&connection, &validated, &query_options)
        .map_err(|error| error.to_string())?;
    let rows = match &executed.rows {
        QueryRows::Headings(rows) => rows,
        QueryRows::Links(_) | QueryRows::Files(_) => {
            return Err("path strategy benchmark requires heading rows".into());
        }
    };
    let matched_heading_ids = rows
        .iter()
        .filter(|row| matches!(row, crate::query::HeadingQueryMatch::Heading(_)))
        .count();
    if matched_heading_ids != expected_results {
        return Err(format!(
            "path strategy benchmark matched {matched_heading_ids} headings, expected {expected_results}"
        ));
    }

    let recursive_reference =
        load_heading_paths_recursive_from_relation(&connection, &executed.relation, rows)
            .map_err(|error| error.to_string())?;
    let rust_reference = load_heading_paths_from_relation(&connection, &executed.relation, rows)
        .map_err(|error| error.to_string())?;
    if recursive_reference != rust_reference {
        return Err(format!(
            "Rust-driven path strategy changed heading path output at SQLite variable limit {sqlite_variable_limit}"
        ));
    }
    if recursive_reference.len() != expected_results {
        return Err(format!(
            "path strategy benchmark returned {} paths, expected {expected_results}",
            recursive_reference.len()
        ));
    }

    let recursive_timing = measure(options, || {
        let paths =
            load_heading_paths_recursive_from_relation(&connection, &executed.relation, rows)
                .map_err(|error| error.to_string())?;
        if paths.len() != expected_results {
            return Err("recursive path result count changed during benchmark".into());
        }
        std::hint::black_box(paths);
        Ok(())
    })?;
    benchmark_trace::begin();
    let recursive_profiled =
        load_heading_paths_recursive_from_relation(&connection, &executed.relation, rows)
            .map_err(|error| error.to_string());
    let recursive_records = benchmark_trace::finish();
    let recursive_profiled = recursive_profiled?;
    if recursive_profiled.len() != expected_results {
        return Err("recursive path result count changed during phase profile".into());
    }
    let recursive_profile = summarize_direct_phase_profile(recursive_records);

    let rust_timing = measure(options, || {
        let paths = load_heading_paths_from_relation(&connection, &executed.relation, rows)
            .map_err(|error| error.to_string())?;
        if paths.len() != expected_results {
            return Err("Rust-driven path result count changed during benchmark".into());
        }
        std::hint::black_box(paths);
        Ok(())
    })?;
    benchmark_trace::begin();
    let rust_profiled = load_heading_paths_from_relation(&connection, &executed.relation, rows)
        .map_err(|error| error.to_string());
    let rust_records = benchmark_trace::finish();
    let rust_profiled = rust_profiled?;
    if rust_profiled.len() != expected_results {
        return Err("Rust-driven path result count changed during phase profile".into());
    }
    let rust_profile = summarize_direct_phase_profile(rust_records);

    connection
        .execute_batch("COMMIT")
        .map_err(|error| error.to_string())?;

    Ok(vec![
        PathStrategyResult {
            strategy: "recursive-query-derived",
            requested_sqlite_variable_limit,
            sqlite_variable_limit,
            id_chunk_capacity: chunk_capacity,
            matched_heading_ids,
            returned_paths: expected_results,
            timing: recursive_timing,
            direct_phase_profile: recursive_profile,
        },
        PathStrategyResult {
            strategy: "rust-driven-bulk-ancestors",
            requested_sqlite_variable_limit,
            sqlite_variable_limit,
            id_chunk_capacity: chunk_capacity,
            matched_heading_ids,
            returned_paths: expected_results,
            timing: rust_timing,
            direct_phase_profile: rust_profile,
        },
    ])
}

fn measure_production_path_strategies(
    db_path: &Path,
    expected_results: usize,
    options: &QuerySqlBenchmarkOptions,
) -> Result<Vec<ProductionPathStrategyResult>, String> {
    let mut connection =
        open_existing_database_read_only(db_path).map_err(|error| error.to_string())?;
    connection
        .execute_batch("BEGIN DEFERRED TRANSACTION")
        .map_err(|error| error.to_string())?;

    let parsed = parse_query("(headings (level 1))").map_err(|error| error.to_string())?;
    let validation_options =
        sqlite_query_validation_options(&connection).map_err(|error| error.to_string())?;
    let validated =
        validate_query(parsed, &validation_options).map_err(|error| error.to_string())?;
    let query_options = QueryExecutionOptions {
        output_mode: QueryOutputMode::Flat,
        includes: vec![QueryInclude::Path],
        ..Default::default()
    };

    let recursive_reference = execute_and_shape_query_with_path_strategy(
        &connection,
        &validated,
        &query_options,
        HeadingPathStrategy::RecursiveQueryDerived,
    )
    .map_err(|error| error.to_string())?;
    let rust_reference = execute_and_shape_query_with_path_strategy(
        &connection,
        &validated,
        &query_options,
        HeadingPathStrategy::RustDrivenBulkAncestors,
    )
    .map_err(|error| error.to_string())?;
    if recursive_reference != rust_reference {
        return Err("complete production path strategies changed query output".into());
    }
    if recursive_reference.results.len() != expected_results {
        return Err(format!(
            "complete production path benchmark returned {} results, expected {expected_results}",
            recursive_reference.results.len()
        ));
    }

    let recursive = measure_production_path_strategy(
        &mut connection,
        &validated,
        &query_options,
        HeadingPathStrategy::RecursiveQueryDerived,
        "recursive-query-derived",
        expected_results,
        options,
    )?;
    let rust = measure_production_path_strategy(
        &mut connection,
        &validated,
        &query_options,
        HeadingPathStrategy::RustDrivenBulkAncestors,
        "rust-driven-bulk-ancestors",
        expected_results,
        options,
    )?;

    connection
        .execute_batch("COMMIT")
        .map_err(|error| error.to_string())?;

    Ok(vec![recursive, rust])
}

fn measure_production_path_strategy(
    connection: &mut Connection,
    validated: &crate::query::ValidatedQuery,
    query_options: &QueryExecutionOptions,
    strategy: HeadingPathStrategy,
    strategy_name: &'static str,
    expected_results: usize,
    options: &QuerySqlBenchmarkOptions,
) -> Result<ProductionPathStrategyResult, String> {
    let total_query_time = measure(options, || {
        let response = execute_and_shape_query_with_path_strategy(
            connection,
            validated,
            query_options,
            strategy,
        )
        .map_err(|error| error.to_string())?;
        if response.results.len() != expected_results {
            return Err(format!(
                "{strategy_name} complete production path result count changed during benchmark"
            ));
        }
        std::hint::black_box(response);
        Ok(())
    })?;

    PROFILE_RECORDS.with(|records| records.borrow_mut().clear());
    connection.profile(Some(profile_callback));
    let profiled =
        execute_and_shape_query_with_path_strategy(connection, validated, query_options, strategy)
            .map_err(|error| error.to_string());
    connection.profile(None);
    let profiled = profiled?;
    if profiled.results.len() != expected_results {
        return Err(format!(
            "{strategy_name} complete production path result count changed during SQL profile"
        ));
    }
    let sql_profile = take_profile_summary();

    benchmark_trace::begin();
    let phase_response =
        execute_and_shape_query_with_path_strategy(connection, validated, query_options, strategy)
            .map_err(|error| error.to_string());
    let phase_records = benchmark_trace::finish();
    let phase_response = phase_response?;
    if phase_response.results.len() != expected_results {
        return Err(format!(
            "{strategy_name} complete production path result count changed during phase profile"
        ));
    }
    let direct_phase_profile = summarize_direct_phase_profile(phase_records);

    Ok(ProductionPathStrategyResult {
        strategy: strategy_name,
        result_count: expected_results,
        total_query_time,
        sql_profile,
        direct_phase_profile,
    })
}

fn measure_lookup_strategies(
    db_path: &Path,
    target_results: usize,
    options: &QuerySqlBenchmarkOptions,
) -> Result<Vec<LookupStrategyResult>, String> {
    let mut connection =
        open_existing_database_read_only(db_path).map_err(|error| error.to_string())?;
    let heading_ids = select_heading_ids(&connection, target_results)?;
    if heading_ids.len() != target_results {
        return Err(format!(
            "strategy benchmark selected {} heading IDs, expected {target_results}",
            heading_ids.len()
        ));
    }
    let expected_rows = count_effective_property_rows(&connection, &heading_ids)?;

    let in_strategy = measure_in_strategy(&connection, &heading_ids, expected_rows, options)?;
    let cte_strategy = measure_cte_strategy(&connection, &heading_ids, expected_rows, options)?;
    let json_each_strategy =
        measure_json_each_strategy(&connection, &heading_ids, expected_rows, options)?;
    let temp_table_strategy =
        measure_temp_table_strategy(&mut connection, &heading_ids, expected_rows, options)?;
    let query_derived_join_strategy =
        measure_query_derived_join_strategy(&connection, target_results, expected_rows, options)?;

    Ok(vec![
        in_strategy,
        cte_strategy,
        json_each_strategy,
        temp_table_strategy,
        query_derived_join_strategy,
    ])
}

fn select_heading_ids(connection: &Connection, limit: usize) -> Result<Vec<i64>, String> {
    let mut statement = connection
        .prepare(
            "SELECT id
             FROM headings
             WHERE level = 1
             ORDER BY id
             LIMIT ?1",
        )
        .map_err(|error| error.to_string())?;
    let rows = statement
        .query_map(
            [i64::try_from(limit).map_err(|error| error.to_string())?],
            |row| row.get(0),
        )
        .map_err(|error| error.to_string())?;
    rows.collect::<Result<Vec<_>, _>>()
        .map_err(|error| error.to_string())
}

fn count_effective_property_rows(
    connection: &Connection,
    heading_ids: &[i64],
) -> Result<usize, String> {
    run_chunked_lookup(connection, heading_ids, LookupSql::In)
}

#[derive(Debug, Clone, Copy)]
enum LookupSql {
    In,
    Cte,
}

fn run_chunked_lookup(
    connection: &Connection,
    heading_ids: &[i64],
    strategy: LookupSql,
) -> Result<usize, String> {
    let chunk_size = id_chunk_capacity(connection, 0);
    let mut row_count = 0usize;
    for chunk in heading_ids.chunks(chunk_size) {
        let placeholders = vec!["?"; chunk.len()].join(", ");
        let sql = match strategy {
            LookupSql::In => format!(
                "SELECT heading_id, key, effective_value
                 FROM effective_properties
                 WHERE heading_id IN ({placeholders})"
            ),
            LookupSql::Cte => {
                let values = vec!["(?)"; chunk.len()].join(", ");
                format!(
                    "WITH selected(heading_id) AS (VALUES {values})
                     SELECT effective_properties.heading_id,
                            effective_properties.key,
                            effective_properties.effective_value
                     FROM selected
                     INNER JOIN effective_properties
                       ON effective_properties.heading_id = selected.heading_id"
                )
            }
        };
        let mut statement = connection
            .prepare(&sql)
            .map_err(|error| error.to_string())?;
        let rows = statement
            .query_map(params_from_iter(chunk.iter()), |_| Ok(()))
            .map_err(|error| error.to_string())?;
        for row in rows {
            row.map_err(|error| error.to_string())?;
            row_count += 1;
        }
    }
    Ok(row_count)
}

fn measure_in_strategy(
    connection: &Connection,
    heading_ids: &[i64],
    expected_rows: usize,
    options: &QuerySqlBenchmarkOptions,
) -> Result<LookupStrategyResult, String> {
    let timing = measure(options, || {
        let rows = run_chunked_lookup(connection, heading_ids, LookupSql::In)?;
        if rows != expected_rows {
            return Err("IN lookup row count changed".into());
        }
        Ok(())
    })?;
    let chunk_size = id_chunk_capacity(connection, 0);
    Ok(LookupStrategyResult {
        strategy: "runtime-limit-in",
        selected_heading_ids: heading_ids.len(),
        returned_rows: expected_rows,
        statement_count_per_sample: heading_ids.len().div_ceil(chunk_size),
        max_bound_parameters: heading_ids.len().min(chunk_size),
        timing,
    })
}

fn measure_cte_strategy(
    connection: &Connection,
    heading_ids: &[i64],
    expected_rows: usize,
    options: &QuerySqlBenchmarkOptions,
) -> Result<LookupStrategyResult, String> {
    let timing = measure(options, || {
        let rows = run_chunked_lookup(connection, heading_ids, LookupSql::Cte)?;
        if rows != expected_rows {
            return Err("CTE lookup row count changed".into());
        }
        Ok(())
    })?;
    let chunk_size = id_chunk_capacity(connection, 0);
    Ok(LookupStrategyResult {
        strategy: "cte-values",
        selected_heading_ids: heading_ids.len(),
        returned_rows: expected_rows,
        statement_count_per_sample: heading_ids.len().div_ceil(chunk_size),
        max_bound_parameters: heading_ids.len().min(chunk_size),
        timing,
    })
}

fn measure_json_each_strategy(
    connection: &Connection,
    heading_ids: &[i64],
    expected_rows: usize,
    options: &QuerySqlBenchmarkOptions,
) -> Result<LookupStrategyResult, String> {
    let timing = measure(options, || {
        let ids_json = serde_json::to_string(heading_ids).map_err(|error| error.to_string())?;
        let mut statement = connection
            .prepare(
                "SELECT effective_properties.heading_id,
                        effective_properties.key,
                        effective_properties.effective_value
                 FROM json_each(?1) AS selected
                 INNER JOIN effective_properties
                   ON effective_properties.heading_id = CAST(selected.value AS INTEGER)",
            )
            .map_err(|error| error.to_string())?;
        let rows = statement
            .query_map([ids_json], |_| Ok(()))
            .map_err(|error| error.to_string())?;
        let mut row_count = 0usize;
        for row in rows {
            row.map_err(|error| error.to_string())?;
            row_count += 1;
        }
        if row_count != expected_rows {
            return Err("json_each lookup row count changed".into());
        }
        Ok(())
    })?;

    Ok(LookupStrategyResult {
        strategy: "json-each-relation",
        selected_heading_ids: heading_ids.len(),
        returned_rows: expected_rows,
        statement_count_per_sample: 1,
        max_bound_parameters: 1,
        timing,
    })
}

fn measure_temp_table_strategy(
    connection: &mut Connection,
    heading_ids: &[i64],
    expected_rows: usize,
    options: &QuerySqlBenchmarkOptions,
) -> Result<LookupStrategyResult, String> {
    connection
        .execute_batch(
            "CREATE TEMP TABLE IF NOT EXISTS orgfdb_sql_benchmark_ids (
                 id INTEGER PRIMARY KEY
             ) WITHOUT ROWID;",
        )
        .map_err(|error| error.to_string())?;

    let timing = measure(options, || {
        let rows = run_temp_table_lookup(connection, heading_ids)?;
        if rows != expected_rows {
            return Err("temporary ID relation row count changed".into());
        }
        Ok(())
    })?;
    let chunk_size = id_chunk_capacity(connection, 0);
    let insert_statements = heading_ids.len().div_ceil(chunk_size);
    Ok(LookupStrategyResult {
        strategy: "temporary-id-relation",
        selected_heading_ids: heading_ids.len(),
        returned_rows: expected_rows,
        statement_count_per_sample: 2 + insert_statements,
        max_bound_parameters: heading_ids.len().min(chunk_size),
        timing,
    })
}

fn measure_query_derived_join_strategy(
    connection: &Connection,
    expected_heading_ids: usize,
    expected_rows: usize,
    options: &QuerySqlBenchmarkOptions,
) -> Result<LookupStrategyResult, String> {
    let timing = measure(options, || {
        let mut statement = connection
            .prepare(
                "SELECT effective_properties.heading_id,
                        effective_properties.key,
                        effective_properties.effective_value
                 FROM headings
                 INNER JOIN effective_properties
                   ON effective_properties.heading_id = headings.id
                 WHERE headings.level = 1",
            )
            .map_err(|error| error.to_string())?;
        let rows = statement
            .query_map([], |_| Ok(()))
            .map_err(|error| error.to_string())?;
        let mut row_count = 0usize;
        for row in rows {
            row.map_err(|error| error.to_string())?;
            row_count += 1;
        }
        if row_count != expected_rows {
            return Err("query-derived join row count changed".into());
        }
        Ok(())
    })?;

    Ok(LookupStrategyResult {
        strategy: "query-derived-join",
        selected_heading_ids: expected_heading_ids,
        returned_rows: expected_rows,
        statement_count_per_sample: 1,
        max_bound_parameters: 0,
        timing,
    })
}

fn run_temp_table_lookup(connection: &Connection, heading_ids: &[i64]) -> Result<usize, String> {
    connection
        .execute("DELETE FROM temp.orgfdb_sql_benchmark_ids", [])
        .map_err(|error| error.to_string())?;
    let chunk_size = id_chunk_capacity(connection, 0);
    for chunk in heading_ids.chunks(chunk_size) {
        let values = vec!["(?)"; chunk.len()].join(", ");
        connection
            .execute(
                &format!("INSERT INTO temp.orgfdb_sql_benchmark_ids (id) VALUES {values}"),
                params_from_iter(chunk.iter()),
            )
            .map_err(|error| error.to_string())?;
    }

    let mut statement = connection
        .prepare(
            "SELECT effective_properties.heading_id,
                    effective_properties.key,
                    effective_properties.effective_value
             FROM temp.orgfdb_sql_benchmark_ids AS selected
             INNER JOIN effective_properties
               ON effective_properties.heading_id = selected.id",
        )
        .map_err(|error| error.to_string())?;
    let rows = statement
        .query_map([], |_| Ok(()))
        .map_err(|error| error.to_string())?;
    let mut row_count = 0usize;
    for row in rows {
        row.map_err(|error| error.to_string())?;
        row_count += 1;
    }
    Ok(row_count)
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum RelationReuseWorkload {
    Single,
    Multi,
}

impl RelationReuseWorkload {
    fn id(self, complex: bool) -> &'static str {
        match (self, complex) {
            (Self::Single, false) => "simple.single-effective-properties",
            (Self::Multi, false) => "simple.multi-enrichment",
            (Self::Single, true) => "complex.single-effective-properties",
            (Self::Multi, true) => "complex.multi-enrichment",
        }
    }

    fn loader_count(self) -> usize {
        match self {
            Self::Single => 1,
            Self::Multi => 4,
        }
    }
}

fn measure_relation_reuse_strategies(
    db_path: &Path,
    options: &QuerySqlBenchmarkOptions,
) -> Result<Vec<RelationReuseStrategyResult>, String> {
    let mut connection =
        open_existing_database_read_only(db_path).map_err(|error| error.to_string())?;
    connection
        .execute_batch("BEGIN DEFERRED TRANSACTION")
        .map_err(|error| error.to_string())?;

    let mut results = Vec::new();
    for complex in [false, true] {
        let compiled = compile_relation_reuse_source(&connection, complex)?;
        let matched_heading_ids = count_relation_headings(&connection, &compiled)?;
        let query_plan = explain_query_plan(&connection, &compiled.sql, &compiled.params)?;
        for workload in [RelationReuseWorkload::Single, RelationReuseWorkload::Multi] {
            let expected_rows = run_repeated_derived_relation(&connection, &compiled, workload)?;

            let derived_timing = measure(options, || {
                let rows = run_repeated_derived_relation(&connection, &compiled, workload)?;
                if rows != expected_rows {
                    return Err("query-derived relation reuse output changed".into());
                }
                Ok(())
            })?;
            let derived_profile =
                profile_relation_reuse_strategy(&mut connection, &compiled, workload, false)?;
            results.push(RelationReuseStrategyResult {
                workload: workload.id(complex),
                strategy: "repeated-query-derived",
                matched_heading_ids,
                returned_rows: expected_rows.len(),
                rows_transferred_to_rust_per_sample: expected_rows.len(),
                statement_count_per_sample: workload.loader_count(),
                timing: derived_timing,
                sql_profile: derived_profile,
                query_plan: query_plan.clone(),
            });

            let temp_reference = run_shared_temp_relation(&connection, &compiled, workload)?;
            if temp_reference != expected_rows {
                return Err("shared TEMP relation reuse output changed".into());
            }
            let temp_timing = measure(options, || {
                let rows = run_shared_temp_relation(&connection, &compiled, workload)?;
                if rows != expected_rows {
                    return Err("shared TEMP relation reuse output changed".into());
                }
                Ok(())
            })?;
            let temp_profile =
                profile_relation_reuse_strategy(&mut connection, &compiled, workload, true)?;
            results.push(RelationReuseStrategyResult {
                workload: workload.id(complex),
                strategy: "shared-temp-query-relation",
                matched_heading_ids,
                returned_rows: expected_rows.len(),
                rows_transferred_to_rust_per_sample: expected_rows.len(),
                statement_count_per_sample: 4 + workload.loader_count(),
                timing: temp_timing,
                sql_profile: temp_profile,
                query_plan: query_plan.clone(),
            });
        }
    }
    connection
        .execute_batch("COMMIT")
        .map_err(|error| error.to_string())?;
    Ok(results)
}

fn compile_relation_reuse_source(
    connection: &Connection,
    complex: bool,
) -> Result<CompiledSqlQuery, String> {
    let query = if complex {
        r#"(headings (and (level 1) (tags "project") (property "GROUP" "group0")))"#
    } else {
        "(headings (level 1))"
    };
    let parsed = parse_query(query).map_err(|error| error.to_string())?;
    let validation_options =
        sqlite_query_validation_options(connection).map_err(|error| error.to_string())?;
    let validated =
        validate_query(parsed, &validation_options).map_err(|error| error.to_string())?;
    compile_sqlite_query(&validated).map_err(|error| error.to_string())
}

fn count_relation_headings(
    connection: &Connection,
    compiled: &CompiledSqlQuery,
) -> Result<usize, String> {
    let sql = format!("SELECT COUNT(*) FROM ({}) AS matched", compiled.sql);
    connection
        .query_row(&sql, params_from_iter(compiled.params.iter()), |row| {
            row.get::<_, i64>(0)
        })
        .map_err(|error| error.to_string())
        .and_then(|count| usize::try_from(count).map_err(|error| error.to_string()))
}

fn run_repeated_derived_relation(
    connection: &Connection,
    compiled: &CompiledSqlQuery,
    workload: RelationReuseWorkload,
) -> Result<Vec<(u8, i64)>, String> {
    let mut rows = run_relation_loader(connection, compiled, 0, "effective_properties")?;
    if workload == RelationReuseWorkload::Multi {
        rows.extend(run_relation_loader(
            connection,
            compiled,
            1,
            "effective_tags",
        )?);
        rows.extend(run_relation_loader(connection, compiled, 2, "properties")?);
        rows.extend(run_relation_loader(
            connection,
            compiled,
            3,
            "outline_path",
        )?);
    }
    rows.sort_unstable();
    Ok(rows)
}

fn run_relation_loader(
    connection: &Connection,
    compiled: &CompiledSqlQuery,
    loader_id: u8,
    table: &str,
) -> Result<Vec<(u8, i64)>, String> {
    let sql = format!(
        "/* orgfdb:relation-reuse-derived params={} */
         WITH matched({}) AS ({})
         SELECT {table}.heading_id
         FROM matched
         INNER JOIN {table} ON {table}.heading_id = matched.id",
        compiled.params.len(),
        heading_relation_columns(),
        compiled.sql,
    );
    let mut statement = connection
        .prepare(&sql)
        .map_err(|error| error.to_string())?;
    let rows = statement
        .query_map(params_from_iter(compiled.params.iter()), |row| {
            row.get::<_, i64>(0)
        })
        .map_err(|error| error.to_string())?;
    rows.map(|row| {
        row.map(|heading_id| (loader_id, heading_id))
            .map_err(|error| error.to_string())
    })
    .collect()
}

fn run_shared_temp_relation(
    connection: &Connection,
    compiled: &CompiledSqlQuery,
    workload: RelationReuseWorkload,
) -> Result<Vec<(u8, i64)>, String> {
    drop_relation_reuse_temp_table(connection, "relation-reuse-temp-reset")?;
    connection
        .execute_batch(
            "/* orgfdb:relation-reuse-temp-create params=0 */
             CREATE TEMP TABLE temp.orgfdb_sql_benchmark_matched (
                 id INTEGER PRIMARY KEY
             ) WITHOUT ROWID",
        )
        .map_err(|error| error.to_string())?;
    let populate_sql = format!(
        "/* orgfdb:relation-reuse-temp-populate params={} */
         WITH matched({}) AS ({})
         INSERT INTO temp.orgfdb_sql_benchmark_matched (id)
         SELECT matched.id FROM matched",
        compiled.params.len(),
        heading_relation_columns(),
        compiled.sql,
    );
    if let Err(error) = connection.execute(&populate_sql, params_from_iter(compiled.params.iter()))
    {
        let _ = drop_relation_reuse_temp_table(connection, "relation-reuse-temp-drop");
        return Err(error.to_string());
    }

    let result = (|| {
        let mut rows = run_temp_relation_loader(connection, 0, "effective_properties")?;
        if workload == RelationReuseWorkload::Multi {
            rows.extend(run_temp_relation_loader(connection, 1, "effective_tags")?);
            rows.extend(run_temp_relation_loader(connection, 2, "properties")?);
            rows.extend(run_temp_relation_loader(connection, 3, "outline_path")?);
        }
        rows.sort_unstable();
        Ok(rows)
    })();
    let cleanup = drop_relation_reuse_temp_table(connection, "relation-reuse-temp-drop");
    match (result, cleanup) {
        (Err(error), _) => Err(error),
        (Ok(_), Err(error)) => Err(error),
        (Ok(rows), Ok(())) => Ok(rows),
    }
}

fn run_temp_relation_loader(
    connection: &Connection,
    loader_id: u8,
    table: &str,
) -> Result<Vec<(u8, i64)>, String> {
    let sql = format!(
        "/* orgfdb:relation-reuse-temp-load params=0 */
         SELECT {table}.heading_id
         FROM temp.orgfdb_sql_benchmark_matched AS matched
         INNER JOIN {table} ON {table}.heading_id = matched.id"
    );
    let mut statement = connection
        .prepare(&sql)
        .map_err(|error| error.to_string())?;
    let rows = statement
        .query_map([], |row| row.get::<_, i64>(0))
        .map_err(|error| error.to_string())?;
    rows.map(|row| {
        row.map(|heading_id| (loader_id, heading_id))
            .map_err(|error| error.to_string())
    })
    .collect()
}

fn drop_relation_reuse_temp_table(
    connection: &Connection,
    marker: &'static str,
) -> Result<(), String> {
    connection
        .execute_batch(&format!(
            "/* orgfdb:{marker} params=0 */ DROP TABLE IF EXISTS temp.orgfdb_sql_benchmark_matched"
        ))
        .map_err(|error| error.to_string())
}

fn profile_relation_reuse_strategy(
    connection: &mut Connection,
    compiled: &CompiledSqlQuery,
    workload: RelationReuseWorkload,
    use_temp: bool,
) -> Result<SqlProfileSummary, String> {
    PROFILE_RECORDS.with(|records| records.borrow_mut().clear());
    connection.profile(Some(profile_callback));
    let result = if use_temp {
        run_shared_temp_relation(connection, compiled, workload).map(|_| ())
    } else {
        run_repeated_derived_relation(connection, compiled, workload).map(|_| ())
    };
    connection.profile(None);
    result?;
    Ok(take_profile_summary())
}

#[derive(Debug, Clone, Copy)]
struct ProductionRelationReuseWorkload {
    id: &'static str,
    query: &'static str,
    includes: &'static [QueryInclude],
}

const RELATION_REUSE_MULTI_INCLUDES: &[QueryInclude] = &[
    QueryInclude::Properties,
    QueryInclude::EffectiveProperties,
    QueryInclude::Keywords,
];

const PRODUCTION_RELATION_REUSE_WORKLOADS: &[ProductionRelationReuseWorkload] = &[
    ProductionRelationReuseWorkload {
        id: "cheap.single-effective-properties",
        query: "(headings (level 1))",
        includes: EFFECTIVE_PROPERTIES_INCLUDE,
    },
    ProductionRelationReuseWorkload {
        id: "cheap.multi-enrichment",
        query: "(headings (level 1))",
        includes: RELATION_REUSE_MULTI_INCLUDES,
    },
    ProductionRelationReuseWorkload {
        id: "expensive.single-effective-properties",
        query: r#"(headings (and (level 1) (tags "project") (property "GROUP" "group0")))"#,
        includes: EFFECTIVE_PROPERTIES_INCLUDE,
    },
    ProductionRelationReuseWorkload {
        id: "expensive.multi-enrichment",
        query: r#"(headings (and (level 1) (tags "project") (property "GROUP" "group0")))"#,
        includes: RELATION_REUSE_MULTI_INCLUDES,
    },
];

fn measure_production_relation_reuse_strategies(
    db_path: &Path,
    options: &QuerySqlBenchmarkOptions,
) -> Result<Vec<ProductionRelationReuseStrategyResult>, String> {
    let mut connection =
        open_existing_database_read_only(db_path).map_err(|error| error.to_string())?;
    connection
        .execute_batch("BEGIN DEFERRED TRANSACTION")
        .map_err(|error| error.to_string())?;
    let validation_options =
        sqlite_query_validation_options(&connection).map_err(|error| error.to_string())?;
    let mut results = Vec::new();

    for workload in PRODUCTION_RELATION_REUSE_WORKLOADS {
        let parsed = parse_query(workload.query).map_err(|error| error.to_string())?;
        let validated =
            validate_query(parsed, &validation_options).map_err(|error| error.to_string())?;
        let query_options = QueryExecutionOptions {
            output_mode: QueryOutputMode::Flat,
            includes: workload.includes.to_vec(),
            ..Default::default()
        };
        let relation_cost = heading_matched_relation_cost(&validated);
        let derived = execute_and_shape_query_with_relation_reuse_strategy(
            &connection,
            &validated,
            &query_options,
            MatchedRelationReuseStrategy::QueryDerived,
        )
        .map_err(|error| error.to_string())?;
        let selective = execute_and_shape_query_with_relation_reuse_strategy(
            &connection,
            &validated,
            &query_options,
            MatchedRelationReuseStrategy::SelectiveTemp,
        )
        .map_err(|error| error.to_string())?;
        if derived != selective {
            return Err(format!(
                "production relation reuse changed public output for {}",
                workload.id
            ));
        }

        for strategy in [
            MatchedRelationReuseStrategy::QueryDerived,
            MatchedRelationReuseStrategy::SelectiveTemp,
        ] {
            let timing = measure(options, || {
                let response = execute_and_shape_query_with_relation_reuse_strategy(
                    &connection,
                    &validated,
                    &query_options,
                    strategy,
                )
                .map_err(|error| error.to_string())?;
                if response != derived {
                    return Err(format!(
                        "production relation reuse output changed during timing for {}",
                        workload.id
                    ));
                }
                std::hint::black_box(response);
                Ok(())
            })?;

            PROFILE_RECORDS.with(|records| records.borrow_mut().clear());
            connection.profile(Some(profile_callback));
            let profiled = execute_and_shape_query_with_relation_reuse_strategy(
                &connection,
                &validated,
                &query_options,
                strategy,
            )
            .map_err(|error| error.to_string());
            connection.profile(None);
            let profiled = profiled?;
            if profiled != derived {
                return Err(format!(
                    "production relation reuse output changed during SQL profile for {}",
                    workload.id
                ));
            }
            let sql_profile = take_profile_summary();

            benchmark_trace::begin();
            let phase_response = execute_and_shape_query_with_relation_reuse_strategy(
                &connection,
                &validated,
                &query_options,
                strategy,
            )
            .map_err(|error| error.to_string());
            let phase_records = benchmark_trace::finish();
            let phase_response = phase_response?;
            if phase_response != derived {
                return Err(format!(
                    "production relation reuse output changed during direct phase profile for {}",
                    workload.id
                ));
            }

            results.push(ProductionRelationReuseStrategyResult {
                workload: workload.id,
                query: workload.query,
                includes: workload.includes.to_vec(),
                relation_cost: match relation_cost {
                    MatchedRelationCost::Cheap => "cheap",
                    MatchedRelationCost::Expensive => "expensive",
                },
                strategy: match strategy {
                    MatchedRelationReuseStrategy::QueryDerived => "query-derived-disabled",
                    MatchedRelationReuseStrategy::SelectiveTemp => "selective-production",
                },
                temp_selected: strategy == MatchedRelationReuseStrategy::SelectiveTemp
                    && relation_cost == MatchedRelationCost::Expensive,
                result_count: derived.results.len(),
                total_query_time: timing,
                sql_profile,
                direct_phase_profile: summarize_direct_phase_profile(phase_records),
            });
        }
    }

    connection
        .execute_batch("COMMIT")
        .map_err(|error| error.to_string())?;
    Ok(results)
}

const METADATA_PREDICATE_SELECTIVITIES: &[usize] = &[1, 10, 50, 100];

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum MetadataPredicateKind {
    DirectTag,
    DirectProperty,
    EffectiveProperty,
    Keyword,
}

impl MetadataPredicateKind {
    const ALL: [Self; 4] = [
        Self::DirectTag,
        Self::DirectProperty,
        Self::EffectiveProperty,
        Self::Keyword,
    ];

    fn id(self) -> &'static str {
        match self {
            Self::DirectTag => "direct-tag",
            Self::DirectProperty => "direct-property",
            Self::EffectiveProperty => "effective-property",
            Self::Keyword => "keyword",
        }
    }

    fn marker(self, selectivity_percent: usize) -> String {
        match self {
            Self::DirectTag => format!("orgfdb-bench-tag-{selectivity_percent:03}"),
            Self::DirectProperty => format!("ORGFDB_BENCH_DIRECT_{selectivity_percent:03}"),
            Self::EffectiveProperty => format!("ORGFDB_BENCH_EFFECTIVE_{selectivity_percent:03}"),
            Self::Keyword => format!("ORGFDB_BENCH_KEYWORD_{selectivity_percent:03}"),
        }
    }

    fn experimental_index(self) -> &'static str {
        match self {
            Self::DirectTag => "orgfdb_bench_tags_tag_heading",
            Self::DirectProperty => "orgfdb_bench_effective_properties_key_local_heading",
            Self::EffectiveProperty => "orgfdb_bench_effective_properties_key_effective_heading",
            Self::Keyword => "orgfdb_bench_keywords_keyword_value_heading",
        }
    }
}

fn measure_metadata_predicate_strategies(
    db_path: &Path,
    expected_results: usize,
    options: &QuerySqlBenchmarkOptions,
) -> Result<Vec<MetadataPredicateStrategyResult>, String> {
    let current_path = db_path.with_file_name("org-files-db-metadata-predicate-current.sqlite");
    let indexed_path = db_path.with_file_name("org-files-db-metadata-predicate-indexed.sqlite");
    remove_sqlite_benchmark_clone(&current_path)?;
    remove_sqlite_benchmark_clone(&indexed_path)?;
    fs::copy(db_path, &current_path).map_err(|error| error.to_string())?;

    let mut seed_connection = Connection::open(&current_path).map_err(|error| error.to_string())?;
    let eligible_headings = count_level_one_headings(&seed_connection)?;
    if eligible_headings != expected_results {
        return Err(format!(
            "metadata predicate benchmark found {eligible_headings} level-1 headings, expected {expected_results}"
        ));
    }
    seed_metadata_predicate_rows(&mut seed_connection, eligible_headings)?;
    drop_production_metadata_predicate_indexes(&seed_connection)?;
    drop(seed_connection);

    fs::copy(&current_path, &indexed_path).map_err(|error| error.to_string())?;
    let current_connection =
        open_existing_database_read_only(&current_path).map_err(|error| error.to_string())?;
    let indexed_connection = Connection::open(&indexed_path).map_err(|error| error.to_string())?;
    create_metadata_predicate_experimental_indexes(&indexed_connection)?;

    let mut results = Vec::with_capacity(
        MetadataPredicateKind::ALL.len() * METADATA_PREDICATE_SELECTIVITIES.len() * 3,
    );
    for kind in MetadataPredicateKind::ALL {
        for &selectivity_percent in METADATA_PREDICATE_SELECTIVITIES {
            let expected_matches =
                selectivity_heading_count(eligible_headings, selectivity_percent);
            let params = metadata_predicate_params(kind, selectivity_percent);
            let current_sql = metadata_heading_driven_sql(kind);
            let relation_sql = metadata_relation_driven_sql(kind);

            let mut current_reference =
                run_metadata_predicate_statement(&current_connection, current_sql, &params)?;
            let mut relation_reference =
                run_metadata_predicate_statement(&current_connection, relation_sql, &params)?;
            let mut indexed_reference =
                run_metadata_predicate_statement(&indexed_connection, relation_sql, &params)?;
            current_reference.sort_unstable();
            relation_reference.sort_unstable();
            indexed_reference.sort_unstable();
            if current_reference.len() != expected_matches {
                return Err(format!(
                    "{} at {selectivity_percent}% returned {} headings, expected {expected_matches}",
                    kind.id(),
                    current_reference.len()
                ));
            }
            if current_reference != relation_reference || current_reference != indexed_reference {
                return Err(format!(
                    "metadata predicate SQL shapes disagree for {} at {selectivity_percent}%",
                    kind.id()
                ));
            }

            results.push(measure_metadata_predicate_strategy(
                &current_connection,
                kind,
                selectivity_percent,
                eligible_headings,
                expected_matches,
                "heading-driven-exists",
                None,
                current_sql,
                &params,
                options,
            )?);
            results.push(measure_metadata_predicate_strategy(
                &current_connection,
                kind,
                selectivity_percent,
                eligible_headings,
                expected_matches,
                "relation-driven-current-indexes",
                None,
                relation_sql,
                &params,
                options,
            )?);
            results.push(measure_metadata_predicate_strategy(
                &indexed_connection,
                kind,
                selectivity_percent,
                eligible_headings,
                expected_matches,
                "relation-driven-experimental-index",
                Some(kind.experimental_index()),
                relation_sql,
                &params,
                options,
            )?);
        }
    }
    Ok(results)
}

fn measure_production_metadata_predicate_strategies(
    db_path: &Path,
    eligible_headings: usize,
    options: &QuerySqlBenchmarkOptions,
) -> Result<Vec<ProductionMetadataPredicateStrategyResult>, String> {
    let current_path = db_path.with_file_name("org-files-db-metadata-predicate-current.sqlite");
    let indexed_path = db_path.with_file_name("org-files-db-metadata-predicate-indexed.sqlite");
    if !current_path.is_file() || !indexed_path.is_file() {
        return Err(
            "metadata predicate production benchmark requires the prepared predicate database copies"
                .into(),
        );
    }

    let mut current_connection =
        open_existing_database_read_only(&current_path).map_err(|error| error.to_string())?;
    let mut indexed_connection =
        open_existing_database_read_only(&indexed_path).map_err(|error| error.to_string())?;
    current_connection
        .execute_batch("BEGIN DEFERRED TRANSACTION")
        .map_err(|error| error.to_string())?;
    indexed_connection
        .execute_batch("BEGIN DEFERRED TRANSACTION")
        .map_err(|error| error.to_string())?;

    let query_options = QueryExecutionOptions {
        output_mode: QueryOutputMode::Flat,
        includes: Vec::new(),
        ..Default::default()
    };
    let mut results = Vec::new();

    for kind in MetadataPredicateKind::ALL {
        for &selectivity_percent in METADATA_PREDICATE_SELECTIVITIES {
            let expected_matches =
                selectivity_heading_count(eligible_headings, selectivity_percent);
            let query_text = metadata_production_query(kind, selectivity_percent);
            let parsed = parse_query(&query_text).map_err(|error| error.to_string())?;
            let validation_options = sqlite_query_validation_options(&current_connection)
                .map_err(|error| error.to_string())?;
            let validated =
                validate_query(parsed, &validation_options).map_err(|error| error.to_string())?;

            let current_reference = execute_and_shape_query_with_metadata_strategy(
                &current_connection,
                &validated,
                &query_options,
                MetadataPredicateSqlStrategy::HeadingDrivenExists,
            )
            .map_err(|error| error.to_string())?;
            let candidate_reference = execute_and_shape_query_with_metadata_strategy(
                &current_connection,
                &validated,
                &query_options,
                MetadataPredicateSqlStrategy::PredicateDrivenIn,
            )
            .map_err(|error| error.to_string())?;
            if current_reference != candidate_reference {
                return Err(format!(
                    "complete production metadata strategies changed query output for {} at {selectivity_percent}%",
                    kind.id()
                ));
            }
            if current_reference.results.len() != expected_matches {
                return Err(format!(
                    "complete production metadata benchmark returned {} results for {} at {selectivity_percent}%, expected {expected_matches}",
                    current_reference.results.len(),
                    kind.id()
                ));
            }

            results.push(measure_production_metadata_predicate_strategy(
                &mut current_connection,
                &validated,
                &query_options,
                kind,
                selectivity_percent,
                eligible_headings,
                expected_matches,
                MetadataPredicateSqlStrategy::HeadingDrivenExists,
                "heading-driven-exists",
                None,
                options,
            )?);
            results.push(measure_production_metadata_predicate_strategy(
                &mut current_connection,
                &validated,
                &query_options,
                kind,
                selectivity_percent,
                eligible_headings,
                expected_matches,
                MetadataPredicateSqlStrategy::PredicateDrivenIn,
                "predicate-driven-in-current-indexes",
                None,
                options,
            )?);

            if kind != MetadataPredicateKind::DirectTag {
                let indexed_reference = execute_and_shape_query_with_metadata_strategy(
                    &indexed_connection,
                    &validated,
                    &query_options,
                    MetadataPredicateSqlStrategy::PredicateDrivenIn,
                )
                .map_err(|error| error.to_string())?;
                if current_reference != indexed_reference {
                    return Err(format!(
                        "indexed complete production metadata strategy changed query output for {} at {selectivity_percent}%",
                        kind.id()
                    ));
                }
                results.push(measure_production_metadata_predicate_strategy(
                    &mut indexed_connection,
                    &validated,
                    &query_options,
                    kind,
                    selectivity_percent,
                    eligible_headings,
                    expected_matches,
                    MetadataPredicateSqlStrategy::PredicateDrivenIn,
                    "predicate-driven-in-experimental-index",
                    Some(kind.experimental_index()),
                    options,
                )?);
            }
        }
    }

    current_connection
        .execute_batch("COMMIT")
        .map_err(|error| error.to_string())?;
    indexed_connection
        .execute_batch("COMMIT")
        .map_err(|error| error.to_string())?;
    Ok(results)
}

fn metadata_production_query(kind: MetadataPredicateKind, selectivity_percent: usize) -> String {
    let marker = kind.marker(selectivity_percent);
    match kind {
        MetadataPredicateKind::DirectTag => {
            format!("(headings (and (level 1) (tags \"{marker}\" :inherit nil)))")
        }
        MetadataPredicateKind::DirectProperty => {
            format!("(headings (and (level 1) (property \"{marker}\" \"match\" :inherit nil)))")
        }
        MetadataPredicateKind::EffectiveProperty => {
            format!("(headings (and (level 1) (property \"{marker}\" \"match\" :inherit t)))")
        }
        MetadataPredicateKind::Keyword => {
            format!("(headings (and (level 1) (keyword \"{marker}\" \"match\" :inherit nil)))")
        }
    }
}

#[allow(clippy::too_many_arguments)]
fn measure_production_metadata_predicate_strategy(
    connection: &mut Connection,
    validated: &crate::query::ValidatedQuery,
    query_options: &QueryExecutionOptions,
    kind: MetadataPredicateKind,
    selectivity_percent: usize,
    eligible_headings: usize,
    expected_matches: usize,
    metadata_predicate_strategy: MetadataPredicateSqlStrategy,
    strategy_name: &'static str,
    experimental_index: Option<&'static str>,
    options: &QuerySqlBenchmarkOptions,
) -> Result<ProductionMetadataPredicateStrategyResult, String> {
    let total_query_time = measure(options, || {
        let response = execute_and_shape_query_with_metadata_strategy(
            connection,
            validated,
            query_options,
            metadata_predicate_strategy,
        )
        .map_err(|error| error.to_string())?;
        if response.results.len() != expected_matches {
            return Err(format!(
                "{} {strategy_name} complete production result count changed during benchmark",
                kind.id()
            ));
        }
        std::hint::black_box(response);
        Ok(())
    })?;

    PROFILE_RECORDS.with(|records| records.borrow_mut().clear());
    connection.profile(Some(profile_callback));
    let profiled = execute_and_shape_query_with_metadata_strategy(
        connection,
        validated,
        query_options,
        metadata_predicate_strategy,
    )
    .map_err(|error| error.to_string());
    connection.profile(None);
    let profiled = profiled?;
    if profiled.results.len() != expected_matches {
        return Err(format!(
            "{} {strategy_name} complete production result count changed during SQL profile",
            kind.id()
        ));
    }
    let sql_profile = take_profile_summary();

    benchmark_trace::begin();
    let phase_response = execute_and_shape_query_with_metadata_strategy(
        connection,
        validated,
        query_options,
        metadata_predicate_strategy,
    )
    .map_err(|error| error.to_string());
    let phase_records = benchmark_trace::finish();
    let phase_response = phase_response?;
    if phase_response.results.len() != expected_matches {
        return Err(format!(
            "{} {strategy_name} complete production result count changed during phase profile",
            kind.id()
        ));
    }
    let direct_phase_profile = summarize_direct_phase_profile(phase_records);

    let compiled =
        compile_sqlite_query_with_metadata_strategy(validated, false, metadata_predicate_strategy)
            .map_err(|error| error.to_string())?;
    let query_plan = explain_query_plan(connection, &compiled.sql, &compiled.params)?;

    Ok(ProductionMetadataPredicateStrategyResult {
        predicate: kind.id(),
        selectivity_percent,
        actual_selectivity_percent: if eligible_headings == 0 {
            0.0
        } else {
            expected_matches as f64 * 100.0 / eligible_headings as f64
        },
        strategy: strategy_name,
        experimental_index,
        eligible_headings,
        result_count: expected_matches,
        total_query_time,
        sql_profile,
        direct_phase_profile,
        query_plan,
    })
}

fn remove_sqlite_benchmark_clone(path: &Path) -> Result<(), String> {
    let path_text = path.to_string_lossy();
    for candidate in [
        path.to_path_buf(),
        format!("{path_text}-wal").into(),
        format!("{path_text}-shm").into(),
    ] {
        if candidate.exists() {
            fs::remove_file(&candidate).map_err(|error| error.to_string())?;
        }
    }
    Ok(())
}

fn count_level_one_headings(connection: &Connection) -> Result<usize, String> {
    let count = connection
        .query_row("SELECT COUNT(*) FROM headings WHERE level = 1", [], |row| {
            row.get::<_, i64>(0)
        })
        .map_err(|error| error.to_string())?;
    usize::try_from(count).map_err(|error| error.to_string())
}

fn selectivity_heading_count(eligible_headings: usize, selectivity_percent: usize) -> usize {
    if eligible_headings == 0 {
        return 0;
    }
    (eligible_headings * selectivity_percent / 100).max(1)
}

fn seed_metadata_predicate_rows(
    connection: &mut Connection,
    eligible_headings: usize,
) -> Result<(), String> {
    let transaction = connection
        .transaction()
        .map_err(|error| error.to_string())?;
    for &selectivity_percent in METADATA_PREDICATE_SELECTIVITIES {
        let selected = selectivity_heading_count(eligible_headings, selectivity_percent);
        let selected = i64::try_from(selected).map_err(|error| error.to_string())?;

        let tag = MetadataPredicateKind::DirectTag.marker(selectivity_percent);
        transaction
            .execute(
                "INSERT INTO tags (heading_id, tag)\n                 SELECT id, ?1 FROM headings\n                 WHERE level = 1\n                 ORDER BY id\n                 LIMIT ?2",
                params![tag, selected],
            )
            .map_err(|error| error.to_string())?;

        let direct_key = MetadataPredicateKind::DirectProperty.marker(selectivity_percent);
        transaction
            .execute(
                "INSERT INTO effective_properties\n                     (heading_id, file_id, key, local_value, effective_value)\n                 SELECT id, file_id, ?1, 'match', 'match' FROM headings\n                 WHERE level = 1\n                 ORDER BY id\n                 LIMIT ?2",
                params![direct_key, selected],
            )
            .map_err(|error| error.to_string())?;

        let effective_key = MetadataPredicateKind::EffectiveProperty.marker(selectivity_percent);
        transaction
            .execute(
                "INSERT INTO effective_properties\n                     (heading_id, file_id, key, local_value, effective_value)\n                 SELECT id, file_id, ?1, NULL, 'match' FROM headings\n                 WHERE level = 1\n                 ORDER BY id\n                 LIMIT ?2",
                params![effective_key, selected],
            )
            .map_err(|error| error.to_string())?;

        let keyword = MetadataPredicateKind::Keyword.marker(selectivity_percent);
        transaction
            .execute(
                "INSERT INTO keywords (heading_id, keyword, value, line_number)\n                 SELECT id, ?1, 'match', NULL FROM headings\n                 WHERE level = 1\n                 ORDER BY id\n                 LIMIT ?2",
                params![keyword, selected],
            )
            .map_err(|error| error.to_string())?;
    }
    transaction.commit().map_err(|error| error.to_string())
}

const BENCH_TAG_PREDICATE_INDEX_SQL: &str =
    "CREATE INDEX orgfdb_bench_tags_tag_heading ON tags(tag, heading_id);";
const BENCH_PROPERTY_PREDICATE_INDEXES_SQL: &str =
    "CREATE INDEX orgfdb_bench_effective_properties_key_local_heading
         ON effective_properties(key, local_value, heading_id)
         WHERE local_value IS NOT NULL;
     CREATE INDEX orgfdb_bench_effective_properties_key_effective_heading
         ON effective_properties(key, effective_value, heading_id);";
const BENCH_KEYWORD_PREDICATE_INDEX_SQL: &str =
    "CREATE INDEX orgfdb_bench_keywords_keyword_value_heading
         ON keywords(keyword COLLATE NOCASE, value, heading_id);";
const BENCH_COMBINED_PREDICATE_INDEXES_SQL: &str =
    "CREATE INDEX orgfdb_bench_effective_properties_key_local_heading
         ON effective_properties(key, local_value, heading_id)
         WHERE local_value IS NOT NULL;
     CREATE INDEX orgfdb_bench_effective_properties_key_effective_heading
         ON effective_properties(key, effective_value, heading_id);
     CREATE INDEX orgfdb_bench_keywords_keyword_value_heading
         ON keywords(keyword COLLATE NOCASE, value, heading_id);";

fn drop_production_metadata_predicate_indexes(connection: &Connection) -> Result<(), String> {
    connection
        .execute_batch(
            "DROP INDEX IF EXISTS idx_effective_properties_key_local_heading;
             DROP INDEX IF EXISTS idx_effective_properties_key_effective_heading;
             DROP INDEX IF EXISTS idx_keywords_keyword_value_heading;",
        )
        .map_err(|error| error.to_string())
}

fn create_metadata_predicate_experimental_indexes(connection: &Connection) -> Result<(), String> {
    connection
        .execute_batch(BENCH_TAG_PREDICATE_INDEX_SQL)
        .map_err(|error| error.to_string())?;
    connection
        .execute_batch(BENCH_PROPERTY_PREDICATE_INDEXES_SQL)
        .map_err(|error| error.to_string())?;
    connection
        .execute_batch(BENCH_KEYWORD_PREDICATE_INDEX_SQL)
        .map_err(|error| error.to_string())
}

#[derive(Debug, Clone, Copy)]
enum MetadataIndexWriteKind {
    EffectiveProperties,
    Keywords,
}

fn measure_metadata_index_costs(
    db_path: &Path,
    eligible_headings: usize,
    options: &QuerySqlBenchmarkOptions,
) -> Result<Vec<MetadataIndexCostResult>, String> {
    let base_path = db_path.with_file_name("org-files-db-metadata-predicate-current.sqlite");
    if !base_path.is_file() {
        return Err(
            "metadata index cost benchmark requires the prepared predicate database copy".into(),
        );
    }

    let base_connection = Connection::open(&base_path).map_err(|error| error.to_string())?;
    base_connection
        .execute_batch("PRAGMA wal_checkpoint(TRUNCATE);")
        .map_err(|error| error.to_string())?;
    let base_size = database_logical_size_bytes(&base_connection)?;
    drop(base_connection);

    let write_rows = eligible_headings.min(1_000);
    let property = measure_metadata_index_cost_set(
        &base_path,
        base_size,
        "effective-properties-predicate-indexes",
        vec![
            "orgfdb_bench_effective_properties_key_local_heading",
            "orgfdb_bench_effective_properties_key_effective_heading",
        ],
        BENCH_PROPERTY_PREDICATE_INDEXES_SQL,
        Some(MetadataIndexWriteKind::EffectiveProperties),
        write_rows,
        options,
    )?;
    let keyword = measure_metadata_index_cost_set(
        &base_path,
        base_size,
        "keyword-predicate-index",
        vec!["orgfdb_bench_keywords_keyword_value_heading"],
        BENCH_KEYWORD_PREDICATE_INDEX_SQL,
        Some(MetadataIndexWriteKind::Keywords),
        write_rows,
        options,
    )?;
    let combined = measure_metadata_index_cost_set(
        &base_path,
        base_size,
        "combined-persistent-candidates",
        vec![
            "orgfdb_bench_effective_properties_key_local_heading",
            "orgfdb_bench_effective_properties_key_effective_heading",
            "orgfdb_bench_keywords_keyword_value_heading",
        ],
        BENCH_COMBINED_PREDICATE_INDEXES_SQL,
        None,
        write_rows,
        options,
    )?;

    Ok(vec![property, keyword, combined])
}

#[allow(clippy::too_many_arguments)]
fn measure_metadata_index_cost_set(
    base_path: &Path,
    base_size: u64,
    index_set: &'static str,
    indexes: Vec<&'static str>,
    index_sql: &'static str,
    write_kind: Option<MetadataIndexWriteKind>,
    write_rows: usize,
    options: &QuerySqlBenchmarkOptions,
) -> Result<MetadataIndexCostResult, String> {
    let build_time = measure_metadata_index_build_time(base_path, index_set, index_sql, options)?;
    let indexed_path = base_path.with_file_name(format!(
        "org-files-db-metadata-index-cost-{index_set}-indexed.sqlite"
    ));
    prepare_sqlite_benchmark_clone(base_path, &indexed_path)?;
    let indexed_connection = Connection::open(&indexed_path).map_err(|error| error.to_string())?;
    indexed_connection
        .execute_batch(index_sql)
        .map_err(|error| error.to_string())?;
    indexed_connection
        .execute_batch("PRAGMA wal_checkpoint(TRUNCATE);")
        .map_err(|error| error.to_string())?;
    let indexed_size = database_logical_size_bytes(&indexed_connection)?;
    drop(indexed_connection);

    let representative_write = if let Some(kind) = write_kind {
        let without_indexes = measure_metadata_index_write_time(
            base_path,
            index_set,
            "without-indexes",
            kind,
            write_rows,
            options,
        )?;
        let with_indexes = measure_metadata_index_write_time(
            &indexed_path,
            index_set,
            "with-indexes",
            kind,
            write_rows,
            options,
        )?;
        Some(MetadataIndexWriteCostResult {
            table: match kind {
                MetadataIndexWriteKind::EffectiveProperties => "effective_properties",
                MetadataIndexWriteKind::Keywords => "keywords",
            },
            rows: write_rows,
            operation: "insert-and-commit benchmark marker rows",
            without_indexes,
            with_indexes,
        })
    } else {
        None
    };

    remove_sqlite_benchmark_clone(&indexed_path)?;
    let database_size_delta_bytes = indexed_size.saturating_sub(base_size);
    let database_size_delta_percent = if base_size == 0 {
        0.0
    } else {
        database_size_delta_bytes as f64 * 100.0 / base_size as f64
    };

    Ok(MetadataIndexCostResult {
        index_set,
        indexes,
        database_size_before_bytes: base_size,
        database_size_after_bytes: indexed_size,
        database_size_delta_bytes,
        database_size_delta_percent,
        build_time,
        representative_write,
    })
}

fn measure_metadata_index_build_time(
    base_path: &Path,
    index_set: &str,
    index_sql: &str,
    options: &QuerySqlBenchmarkOptions,
) -> Result<Timing, String> {
    for sample in 0..options.warmups {
        run_metadata_index_build_sample(base_path, index_set, "warmup", sample, index_sql)?;
    }

    let mut samples = Vec::with_capacity(options.iterations);
    for sample in 0..options.iterations {
        samples.push(run_metadata_index_build_sample(
            base_path, index_set, "sample", sample, index_sql,
        )?);
    }
    samples.sort();
    Ok(timing(&samples))
}

fn run_metadata_index_build_sample(
    base_path: &Path,
    index_set: &str,
    phase: &str,
    sample: usize,
    index_sql: &str,
) -> Result<Duration, String> {
    let sample_path = base_path.with_file_name(format!(
        "org-files-db-metadata-index-cost-{index_set}-{phase}-{sample}.sqlite"
    ));
    prepare_sqlite_benchmark_clone(base_path, &sample_path)?;
    let connection = Connection::open(&sample_path).map_err(|error| error.to_string())?;
    let start = Instant::now();
    connection
        .execute_batch(index_sql)
        .map_err(|error| error.to_string())?;
    let elapsed = start.elapsed();
    drop(connection);
    remove_sqlite_benchmark_clone(&sample_path)?;
    Ok(elapsed)
}

fn measure_metadata_index_write_time(
    template_path: &Path,
    index_set: &str,
    variant: &str,
    kind: MetadataIndexWriteKind,
    rows: usize,
    options: &QuerySqlBenchmarkOptions,
) -> Result<Timing, String> {
    for sample in 0..options.warmups {
        run_metadata_index_write_sample(
            template_path,
            index_set,
            variant,
            "warmup",
            sample,
            kind,
            rows,
        )?;
    }

    let mut samples = Vec::with_capacity(options.iterations);
    for sample in 0..options.iterations {
        samples.push(run_metadata_index_write_sample(
            template_path,
            index_set,
            variant,
            "sample",
            sample,
            kind,
            rows,
        )?);
    }
    samples.sort();
    Ok(timing(&samples))
}

fn run_metadata_index_write_sample(
    template_path: &Path,
    index_set: &str,
    variant: &str,
    phase: &str,
    sample: usize,
    kind: MetadataIndexWriteKind,
    rows: usize,
) -> Result<Duration, String> {
    let sample_path = template_path.with_file_name(format!(
        "org-files-db-metadata-index-write-{index_set}-{variant}-{phase}-{sample}.sqlite"
    ));
    prepare_sqlite_benchmark_clone(template_path, &sample_path)?;
    let mut connection = Connection::open(&sample_path).map_err(|error| error.to_string())?;
    let rows = i64::try_from(rows).map_err(|error| error.to_string())?;
    let start = Instant::now();
    {
        let transaction = connection
            .transaction()
            .map_err(|error| error.to_string())?;
        match kind {
            MetadataIndexWriteKind::EffectiveProperties => {
                transaction
                    .execute(
                        "INSERT INTO effective_properties
                             (heading_id, file_id, key, local_value, effective_value)
                         SELECT id, file_id, 'ORGFDB_BENCH_INDEX_COST_WRITE', 'match', 'match'
                         FROM headings
                         WHERE level = 1
                         ORDER BY id
                         LIMIT ?1",
                        params![rows],
                    )
                    .map_err(|error| error.to_string())?;
            }
            MetadataIndexWriteKind::Keywords => {
                transaction
                    .execute(
                        "INSERT INTO keywords (heading_id, keyword, value, line_number)
                         SELECT id, 'ORGFDB_BENCH_INDEX_COST_WRITE', 'match', -1
                         FROM headings
                         WHERE level = 1
                         ORDER BY id
                         LIMIT ?1",
                        params![rows],
                    )
                    .map_err(|error| error.to_string())?;
            }
        }
        transaction.commit().map_err(|error| error.to_string())?;
    }
    let elapsed = start.elapsed();
    drop(connection);
    remove_sqlite_benchmark_clone(&sample_path)?;
    Ok(elapsed)
}

fn prepare_sqlite_benchmark_clone(source: &Path, destination: &Path) -> Result<(), String> {
    remove_sqlite_benchmark_clone(destination)?;
    fs::copy(source, destination).map_err(|error| error.to_string())?;
    Ok(())
}

fn database_logical_size_bytes(connection: &Connection) -> Result<u64, String> {
    let page_count = connection
        .query_row("PRAGMA page_count", [], |row| row.get::<_, u64>(0))
        .map_err(|error| error.to_string())?;
    let page_size = connection
        .query_row("PRAGMA page_size", [], |row| row.get::<_, u64>(0))
        .map_err(|error| error.to_string())?;
    Ok(page_count.saturating_mul(page_size))
}

fn metadata_predicate_params(
    kind: MetadataPredicateKind,
    selectivity_percent: usize,
) -> Vec<QueryParam> {
    let marker = kind.marker(selectivity_percent);
    match kind {
        MetadataPredicateKind::DirectTag => vec![QueryParam::Text(marker)],
        MetadataPredicateKind::DirectProperty
        | MetadataPredicateKind::EffectiveProperty
        | MetadataPredicateKind::Keyword => {
            vec![
                QueryParam::Text(marker),
                QueryParam::Text("match".to_string()),
            ]
        }
    }
}

fn metadata_heading_driven_sql(kind: MetadataPredicateKind) -> &'static str {
    match kind {
        MetadataPredicateKind::DirectTag => {
            "SELECT h.id\n             FROM headings AS h\n             WHERE h.level = 1\n               AND EXISTS (\n                   SELECT 1 FROM tags\n                   WHERE tags.heading_id = h.id\n                     AND tags.tag = ?1\n               )"
        }
        MetadataPredicateKind::DirectProperty => {
            "SELECT h.id\n             FROM headings AS h\n             WHERE h.level = 1\n               AND EXISTS (\n                   SELECT 1 FROM effective_properties\n                   WHERE effective_properties.heading_id = h.id\n                     AND effective_properties.key = ?1\n                     AND effective_properties.local_value IS NOT NULL\n                     AND effective_properties.local_value = ?2\n               )"
        }
        MetadataPredicateKind::EffectiveProperty => {
            "SELECT h.id\n             FROM headings AS h\n             WHERE h.level = 1\n               AND EXISTS (\n                   SELECT 1 FROM effective_properties\n                   WHERE effective_properties.heading_id = h.id\n                     AND effective_properties.key = ?1\n                     AND effective_properties.effective_value = ?2\n               )"
        }
        MetadataPredicateKind::Keyword => {
            "SELECT h.id\n             FROM headings AS h\n             WHERE h.level = 1\n               AND EXISTS (\n                   SELECT 1 FROM keywords\n                   WHERE keywords.heading_id = h.id\n                     AND keywords.keyword = ?1 COLLATE NOCASE\n                     AND keywords.value = ?2\n               )"
        }
    }
}

fn metadata_relation_driven_sql(kind: MetadataPredicateKind) -> &'static str {
    match kind {
        MetadataPredicateKind::DirectTag => {
            "SELECT h.id\n             FROM tags AS metadata\n             CROSS JOIN headings AS h\n             WHERE metadata.tag = ?1\n               AND h.id = metadata.heading_id\n               AND h.level = 1"
        }
        MetadataPredicateKind::DirectProperty => {
            "SELECT h.id\n             FROM effective_properties AS metadata\n             CROSS JOIN headings AS h\n             WHERE metadata.key = ?1\n               AND metadata.local_value IS NOT NULL\n               AND metadata.local_value = ?2\n               AND h.id = metadata.heading_id\n               AND h.level = 1"
        }
        MetadataPredicateKind::EffectiveProperty => {
            "SELECT h.id\n             FROM effective_properties AS metadata\n             CROSS JOIN headings AS h\n             WHERE metadata.key = ?1\n               AND metadata.effective_value = ?2\n               AND h.id = metadata.heading_id\n               AND h.level = 1"
        }
        MetadataPredicateKind::Keyword => {
            "SELECT h.id\n             FROM keywords AS metadata\n             CROSS JOIN headings AS h\n             WHERE metadata.keyword = ?1 COLLATE NOCASE\n               AND metadata.value = ?2\n               AND h.id = metadata.heading_id\n               AND h.level = 1"
        }
    }
}

fn run_metadata_predicate_statement(
    connection: &Connection,
    sql: &str,
    params: &[QueryParam],
) -> Result<Vec<i64>, String> {
    let mut statement = connection.prepare(sql).map_err(|error| error.to_string())?;
    let rows = statement
        .query_map(params_from_iter(params.iter()), |row| row.get::<_, i64>(0))
        .map_err(|error| error.to_string())?;
    rows.collect::<Result<Vec<_>, _>>()
        .map_err(|error| error.to_string())
}

#[allow(clippy::too_many_arguments)]
fn measure_metadata_predicate_strategy(
    connection: &Connection,
    kind: MetadataPredicateKind,
    selectivity_percent: usize,
    eligible_headings: usize,
    expected_matches: usize,
    strategy: &'static str,
    experimental_index: Option<&'static str>,
    sql: &str,
    params: &[QueryParam],
    options: &QuerySqlBenchmarkOptions,
) -> Result<MetadataPredicateStrategyResult, String> {
    let total_query_time = measure(options, || {
        let rows = run_metadata_predicate_statement(connection, sql, params)?;
        if rows.len() != expected_matches {
            return Err(format!(
                "{} {strategy} result count changed during metadata predicate benchmark",
                kind.id()
            ));
        }
        std::hint::black_box(rows);
        Ok(())
    })?;
    Ok(MetadataPredicateStrategyResult {
        predicate: kind.id(),
        selectivity_percent,
        actual_selectivity_percent: if eligible_headings == 0 {
            0.0
        } else {
            expected_matches as f64 * 100.0 / eligible_headings as f64
        },
        strategy,
        experimental_index,
        eligible_headings,
        matched_headings: expected_matches,
        returned_rows: expected_matches,
        rows_scanned: None,
        statement_count_per_sample: 1,
        rows_transferred_to_rust_per_sample: expected_matches,
        total_query_time,
        query_plan: explain_query_plan(connection, sql, params)?,
    })
}

fn query_compiler_audit() -> Vec<QueryCompilerAuditResult> {
    vec![
        QueryCompilerAuditResult {
            predicate: "direct-tags",
            current_lookup: "tags by heading_id",
            skip_related_lookup: false,
            finding: "Direct tags can exist on root and regular headings, so heading level alone cannot eliminate the lookup.",
        },
        QueryCompilerAuditResult {
            predicate: "direct-properties",
            current_lookup: "effective_properties.local_value by heading_id and key",
            skip_related_lookup: false,
            finding: "The local-value projection can exist on root and regular headings and preserves resolved local property semantics.",
        },
        QueryCompilerAuditResult {
            predicate: "effective-properties",
            current_lookup: "effective_properties.effective_value by heading_id and key",
            skip_related_lookup: false,
            finding: "An effective property can exist on any heading where the key is visible, so no heading-level state makes the lookup impossible.",
        },
        QueryCompilerAuditResult {
            predicate: "keywords",
            current_lookup: "keywords by selected heading_id or root heading_id",
            skip_related_lookup: false,
            finding: "Direct and inherited keyword modes use different heading identities, and the stored row model does not make either lookup impossible from heading level alone.",
        },
        QueryCompilerAuditResult {
            predicate: "heading-root-static-truth",
            current_lookup: "root file relation for heading queries",
            skip_related_lookup: true,
            finding: "The compiler already skips the root relation when level predicates exclude level 0 or parent, ancestors, or has-text makes a root match impossible.",
        },
    ]
}

fn collect_query_plans(db_path: &Path) -> Result<Vec<QueryPlanResult>, String> {
    let connection =
        open_existing_database_read_only(db_path).map_err(|error| error.to_string())?;
    connection
        .execute_batch(
            "CREATE TEMP TABLE IF NOT EXISTS orgfdb_query_file_restriction (
                 path TEXT PRIMARY KEY
             );",
        )
        .map_err(|error| error.to_string())?;

    let cases = [
        ("heading-level", "(headings (level 1))"),
        (
            "heading-title",
            "(headings (title \"Project 00000\" :exact t))",
        ),
        (
            "file-path",
            "(files (file-path \"/tmp/example.org\" :exact t))",
        ),
        ("direct-tag", "(headings (tags \"project\" :inherit nil))"),
        ("effective-tag", "(headings (tags \"project\"))"),
        (
            "direct-property",
            "(headings (property \"GROUP\" \"group0\" :inherit nil))",
        ),
        (
            "effective-property",
            "(headings (property \"GROUP\" \"group0\"))",
        ),
        (
            "keyword",
            "(headings (keyword \"AUTHOR\" \"Alice\" :inherit nil))",
        ),
        ("hierarchy-parent", "(headings (parent))"),
        ("link-source", "(links (source (headings (level 1))))"),
        ("link-target", "(links (target (headings (level 1))))"),
        (
            "scheduled-time",
            "(headings (scheduled :from \"2026-01-01\" :to \"2026-12-31\"))",
        ),
    ];

    let mut plans = cases
        .into_iter()
        .map(|(id, query)| compiled_query_plan(&connection, id, query, false))
        .collect::<Result<Vec<_>, _>>()?;
    plans.push(compiled_query_plan(
        &connection,
        "file-restriction",
        "(headings (level 1))",
        true,
    )?);
    Ok(plans)
}

fn compiled_query_plan(
    connection: &Connection,
    id: &'static str,
    query: &'static str,
    restrict_files: bool,
) -> Result<QueryPlanResult, String> {
    let parsed = parse_query(query).map_err(|error| error.to_string())?;
    let validation_options =
        sqlite_query_validation_options(connection).map_err(|error| error.to_string())?;
    let validated =
        validate_query(parsed, &validation_options).map_err(|error| error.to_string())?;
    let date_options = QueryDateResolutionOptions {
        timezone: Some("UTC".to_string()),
        ..Default::default()
    };
    let resolved_relative =
        resolve_relative_dates(&validated, &date_options).map_err(|error| error.to_string())?;
    let resolved = resolve_temporal_bounds(&resolved_relative, &date_options)
        .map_err(|error| error.to_string())?;
    let compiled = if restrict_files {
        compile_sqlite_query_with_file_restriction(&resolved, true)
    } else {
        compile_sqlite_query(&resolved)
    }
    .map_err(|error| error.to_string())?;

    Ok(QueryPlanResult {
        id,
        query,
        details: explain_query_plan(connection, &compiled.sql, &compiled.params)?,
    })
}

fn explain_query_plan(
    connection: &Connection,
    sql: &str,
    params: &[QueryParam],
) -> Result<Vec<String>, String> {
    let mut statement = connection
        .prepare(&format!("EXPLAIN QUERY PLAN {sql}"))
        .map_err(|error| error.to_string())?;
    let rows = statement
        .query_map(params_from_iter(params.iter()), |row| {
            row.get::<_, String>(3)
        })
        .map_err(|error| error.to_string())?;
    rows.collect::<Result<Vec<_>, _>>()
        .map_err(|error| error.to_string())
}

fn measure<F>(options: &QuerySqlBenchmarkOptions, mut operation: F) -> Result<Timing, String>
where
    F: FnMut() -> Result<(), String>,
{
    for _ in 0..options.warmups {
        operation()?;
    }
    let mut samples = Vec::with_capacity(options.iterations);
    for _ in 0..options.iterations {
        let start = Instant::now();
        operation()?;
        samples.push(start.elapsed());
    }
    samples.sort();
    Ok(timing(&samples))
}

fn measure_paired<F, G>(
    options: &QuerySqlBenchmarkOptions,
    mut baseline: F,
    mut candidate: G,
) -> Result<(Timing, Timing), String>
where
    F: FnMut() -> Result<(), String>,
    G: FnMut() -> Result<(), String>,
{
    for _ in 0..options.warmups {
        baseline()?;
        candidate()?;
        candidate()?;
        baseline()?;
    }

    let mut baseline_samples = Vec::with_capacity(options.iterations * 2);
    let mut candidate_samples = Vec::with_capacity(options.iterations * 2);
    for _ in 0..options.iterations {
        let started = Instant::now();
        baseline()?;
        baseline_samples.push(started.elapsed());

        let started = Instant::now();
        candidate()?;
        candidate_samples.push(started.elapsed());

        let started = Instant::now();
        candidate()?;
        candidate_samples.push(started.elapsed());

        let started = Instant::now();
        baseline()?;
        baseline_samples.push(started.elapsed());
    }

    baseline_samples.sort();
    candidate_samples.sort();
    Ok((timing(&baseline_samples), timing(&candidate_samples)))
}

fn timing(samples: &[Duration]) -> Timing {
    let ns = |index: usize| samples[index].as_nanos();
    Timing {
        samples: samples.len(),
        min_ns: ns(0),
        median_ns: ns(samples.len() / 2),
        max_ns: ns(samples.len() - 1),
        p95_ns: ns((samples.len() - 1) * 95 / 100),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn profile_marker_reports_stage_and_bound_parameters() {
        assert_eq!(
            parse_profile_marker(
                "/* orgfdb:enrich-keywords params=17 */ SELECT heading_id FROM keywords"
            ),
            Some(("enrich-keywords".to_string(), 17))
        );
    }

    #[test]
    fn paired_measurement_balances_sample_order() {
        let options = QuerySqlBenchmarkOptions {
            row_counts: vec![1],
            warmups: 1,
            iterations: 3,
            path_variable_limits: Vec::new(),
        };
        let mut baseline_calls = 0usize;
        let mut candidate_calls = 0usize;

        let (baseline, candidate) = measure_paired(
            &options,
            || {
                baseline_calls += 1;
                Ok(())
            },
            || {
                candidate_calls += 1;
                Ok(())
            },
        )
        .expect("paired measurement should run");

        assert_eq!(baseline.samples, 6);
        assert_eq!(candidate.samples, 6);
        assert_eq!(baseline_calls, 8);
        assert_eq!(candidate_calls, 8);
    }

    #[test]
    fn direct_phase_summary_counts_sql_transfer_without_row_decode_duplication() {
        let summary = summarize_direct_phase_profile(vec![
            BenchmarkTraceRecord {
                phase: benchmark_trace::MATCHED_SQL_EXECUTION,
                operation: "match-headings",
                duration_ns: 10,
                rows: 3,
                statement_count: 1,
                bound_parameters: 1,
            },
            BenchmarkTraceRecord {
                phase: benchmark_trace::SQLITE_ROW_DECODING,
                operation: "match-headings",
                duration_ns: 5,
                rows: 3,
                statement_count: 0,
                bound_parameters: 0,
            },
            BenchmarkTraceRecord {
                phase: benchmark_trace::TEMP_RELATION_MATERIALIZATION,
                operation: "temp-matched-headings-populate",
                duration_ns: 8,
                rows: 3,
                statement_count: 1,
                bound_parameters: 2,
            },
            BenchmarkTraceRecord {
                phase: benchmark_trace::ENRICHMENT_SQL_EXECUTION,
                operation: "enrich-properties",
                duration_ns: 20,
                rows: 6,
                statement_count: 1,
                bound_parameters: 2,
            },
        ]);

        assert_eq!(summary.rows_transferred_from_sqlite, 9);
        assert_eq!(summary.statement_count, 3);
        assert_eq!(summary.total_bound_parameters, 5);
        assert_eq!(summary.max_bound_parameters, 2);
        assert_eq!(summary.operations.len(), 4);
        assert_eq!(
            summary
                .phases
                .iter()
                .find(|phase| phase.phase == benchmark_trace::SQLITE_ROW_DECODING)
                .map(|phase| phase.rows),
            Some(3)
        );
    }

    #[test]
    fn metadata_predicate_sql_shapes_return_identical_ids() {
        let mut connection = Connection::open_in_memory().expect("in-memory SQLite should open");
        connection
            .execute_batch(
                "CREATE TABLE headings (id INTEGER PRIMARY KEY, file_id INTEGER NOT NULL, level INTEGER NOT NULL);\n                 CREATE TABLE tags (heading_id INTEGER NOT NULL, tag TEXT NOT NULL, PRIMARY KEY (heading_id, tag));\n                 CREATE INDEX idx_tags_tag ON tags(tag);\n                 CREATE TABLE effective_properties (\n                     heading_id INTEGER NOT NULL,\n                     file_id INTEGER NOT NULL,\n                     key TEXT NOT NULL,\n                     local_value TEXT,\n                     effective_value TEXT NOT NULL,\n                     PRIMARY KEY (heading_id, key)\n                 );\n                 CREATE TABLE keywords (\n                     id INTEGER PRIMARY KEY,\n                     heading_id INTEGER NOT NULL,\n                     keyword TEXT NOT NULL,\n                     value TEXT,\n                     line_number INTEGER\n                 );\n                 CREATE INDEX idx_keywords_keyword ON keywords(keyword);",
            )
            .expect("benchmark schema should create");
        for id in 1_i64..=100 {
            connection
                .execute(
                    "INSERT INTO headings (id, file_id, level) VALUES (?1, ?2, 1)",
                    params![id, id],
                )
                .expect("heading should insert");
        }
        seed_metadata_predicate_rows(&mut connection, 100)
            .expect("metadata predicate rows should seed");

        for kind in MetadataPredicateKind::ALL {
            for &selectivity_percent in METADATA_PREDICATE_SELECTIVITIES {
                let params = metadata_predicate_params(kind, selectivity_percent);
                let mut heading_driven = run_metadata_predicate_statement(
                    &connection,
                    metadata_heading_driven_sql(kind),
                    &params,
                )
                .expect("heading-driven SQL should run");
                let mut relation_driven = run_metadata_predicate_statement(
                    &connection,
                    metadata_relation_driven_sql(kind),
                    &params,
                )
                .expect("relation-driven SQL should run");
                heading_driven.sort_unstable();
                relation_driven.sort_unstable();
                assert_eq!(heading_driven, relation_driven);
                assert_eq!(
                    heading_driven.len(),
                    selectivity_heading_count(100, selectivity_percent)
                );
            }
        }

        create_metadata_predicate_experimental_indexes(&connection)
            .expect("experimental indexes should create");
        for kind in MetadataPredicateKind::ALL {
            let params = metadata_predicate_params(kind, 10);
            assert_eq!(
                run_metadata_predicate_statement(
                    &connection,
                    metadata_relation_driven_sql(kind),
                    &params,
                )
                .expect("indexed relation-driven SQL should run")
                .len(),
                10
            );
        }
    }

    #[test]
    fn compiler_audit_skips_only_guaranteed_impossible_root_work() {
        let audit = query_compiler_audit();
        assert!(audit
            .iter()
            .filter(|entry| entry.predicate != "heading-root-static-truth")
            .all(|entry| !entry.skip_related_lookup));
        assert!(audit
            .iter()
            .find(|entry| entry.predicate == "heading-root-static-truth")
            .is_some_and(|entry| entry.skip_related_lookup));
    }

    #[test]
    fn benchmark_options_reject_zero_values() {
        let options = QuerySqlBenchmarkOptions {
            row_counts: vec![100, 0],
            ..Default::default()
        };
        assert!(validate_options(&options).is_err());

        let options = QuerySqlBenchmarkOptions {
            row_counts: vec![100],
            iterations: 0,
            ..Default::default()
        };
        assert!(validate_options(&options).is_err());

        let options = QuerySqlBenchmarkOptions {
            row_counts: vec![100],
            path_variable_limits: vec![0],
            ..Default::default()
        };
        assert!(validate_options(&options).is_err());

        let options = QuerySqlBenchmarkOptions {
            row_counts: vec![100],
            path_variable_limits: vec![usize::MAX],
            ..Default::default()
        };
        assert!(validate_options(&options).is_err());
    }
}
