//! Measurement-only benchmark support for query SQL execution and enrichment.

use std::{
    cell::RefCell,
    collections::BTreeMap,
    fs,
    path::Path,
    time::{Duration, Instant},
};

use rusqlite::{params_from_iter, Connection};
use serde::Serialize;

use crate::{
    db::open_existing_database_read_only,
    presentation_benchmark::{prepare_benchmark_databases, Timing},
    query::{
        execute_and_shape_query, parse_query, sqlite_query_validation_options, validate_query,
        QueryExecutionOptions, QueryInclude, QueryOutputMode,
    },
};

use crate::query::sql_support::{id_chunk_capacity, variable_number_limit};

pub const OUTPUT_SCHEMA_VERSION: &str = "1";
pub const DEFAULT_WARMUPS: usize = 1;
pub const DEFAULT_ITERATIONS: usize = 3;
pub const DEFAULT_ROW_COUNTS: &[usize] = &[100, 1_000, 10_000, 50_000];

#[derive(Debug, Clone)]
pub struct QuerySqlBenchmarkOptions {
    pub row_counts: Vec<usize>,
    pub warmups: usize,
    pub iterations: usize,
}

impl Default for QuerySqlBenchmarkOptions {
    fn default() -> Self {
        Self {
            row_counts: DEFAULT_ROW_COUNTS.to_vec(),
            warmups: DEFAULT_WARMUPS,
            iterations: DEFAULT_ITERATIONS,
        }
    }
}

#[derive(Debug, Serialize)]
pub struct QuerySqlBenchmarkOutput {
    pub output_schema_version: &'static str,
    pub protocol: QuerySqlBenchmarkProtocol,
    pub environment: QuerySqlBenchmarkEnvironment,
    pub sizes: Vec<QuerySqlSizeResult>,
}

#[derive(Debug, Serialize)]
pub struct QuerySqlBenchmarkProtocol {
    pub warmups: usize,
    pub iterations: usize,
    pub row_counts: Vec<usize>,
    pub database_source: &'static str,
    pub timing_policy: &'static str,
    pub profile_policy: &'static str,
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
    pub lookup_strategies: Vec<LookupStrategyResult>,
}

#[derive(Debug, Serialize)]
pub struct QuerySqlWorkloadResult {
    pub id: &'static str,
    pub query: &'static str,
    pub includes: Vec<QueryInclude>,
    pub result_count: usize,
    pub total_query_time: Timing,
    pub sql_profile: SqlProfileSummary,
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

    let result = QuerySqlBenchmarkOutput {
        output_schema_version: OUTPUT_SCHEMA_VERSION,
        protocol: QuerySqlBenchmarkProtocol {
            warmups: options.warmups,
            iterations: options.iterations,
            row_counts: options.row_counts.clone(),
            database_source: "existing or generated orgfdb-presentation-benchmark corpus databases",
            timing_policy: "profile callbacks are disabled during latency samples",
            profile_policy: "one separate profiled production query records statement stages and bound parameters",
            strategy_policy: "lookup microbenchmarks compare runtime-limit IN, CTE VALUES, json_each, temporary ID relations, and a query-derived join",
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

    let lookup_strategies = measure_lookup_strategies(&db_path, target_results, options)?;

    Ok(QuerySqlSizeResult {
        target_results,
        sqlite_variable_limit,
        id_chunk_capacity: chunk_capacity,
        workloads,
        lookup_strategies,
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
    })
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
    }
}
