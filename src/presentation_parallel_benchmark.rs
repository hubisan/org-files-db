//! Measurement-only benchmark support for presentation pipeline parallelism.

use std::{
    fs,
    path::{Path, PathBuf},
    time::{Duration, Instant},
};

use serde::Serialize;

use crate::{
    presentation::{PresentationResponse, PresentationSpec},
    presentation_benchmark::{
        load_workload_response, PresentationBenchmarkOptions, PresentationWorkload, Timing,
        CORPUS_CONTRACT_VERSION, WORKLOADS,
    },
    query::QueryResultNode,
};

pub const OUTPUT_SCHEMA_VERSION: &str = "2";

const PARALLEL_SORT_ROW_THRESHOLD: usize = 100_000;
const PARALLEL_LAYOUT_CELL_THRESHOLD: usize = 200_000;
const PARALLEL_WIDE_COLUMN_THRESHOLD: usize = 8;
const PARALLEL_WIDE_CELL_THRESHOLD: usize = 100_000;

#[derive(Debug, Serialize)]
pub struct PresentationParallelBenchmarkOutput {
    pub output_schema_version: &'static str,
    pub corpus_contract_version: &'static str,
    pub protocol: PresentationParallelBenchmarkProtocol,
    pub environment: PresentationParallelBenchmarkEnvironment,
    pub sizes: Vec<PresentationParallelSizeResult>,
}

#[derive(Debug, Serialize)]
pub struct PresentationParallelBenchmarkProtocol {
    pub warmups: usize,
    pub iterations: usize,
    pub row_counts: Vec<usize>,
    pub baseline: &'static str,
    pub input_policy: &'static str,
    pub equality_policy: &'static str,
    pub ordering_policy: &'static str,
    pub sqlite_policy: &'static str,
    pub candidate_policy: &'static str,
    pub cli_policy: &'static str,
}

#[derive(Debug, Serialize)]
pub struct PresentationParallelBenchmarkEnvironment {
    pub command_arguments: Vec<String>,
    pub build_profile: &'static str,
    pub operating_system: &'static str,
    pub architecture: &'static str,
    pub available_cpus: Option<usize>,
    pub rayon_threads: usize,
}

#[derive(Debug, Serialize)]
pub struct PresentationParallelSizeResult {
    pub target_results: usize,
    pub workloads: Vec<PresentationParallelWorkloadResult>,
}

#[derive(Debug, Serialize)]
pub struct PresentationParallelWorkloadResult {
    pub id: &'static str,
    pub query: &'static str,
    pub presentation_spec_json: &'static str,
    pub result_count: usize,
    pub presentation_row_count: usize,
    pub cell_count: usize,
    pub strategy: PresentationParallelStrategyDecision,
    pub equality: PresentationParallelEquality,
    pub stages: PresentationParallelStageResults,
    pub totals: PresentationParallelTotalResults,
    pub serialization_unchanged: Timing,
}

#[derive(Debug, Clone, Copy, Serialize)]
pub struct PresentationParallelStrategyDecision {
    pub parallel_sort: bool,
    pub parallel_value_extraction: bool,
    pub parallel_formatting: bool,
    pub sort_row_threshold: usize,
    pub layout_cell_threshold: usize,
    pub wide_column_threshold: usize,
    pub wide_cell_threshold: usize,
}

#[derive(Debug, Serialize)]
pub struct PresentationParallelEquality {
    pub parallel_sort_rows_equal: bool,
    pub parallel_layout_rows_equal: bool,
    pub parallel_sort_response_equal: bool,
    pub parallel_value_extraction_response_equal: bool,
    pub parallel_formatting_response_equal: bool,
    pub parallel_layout_response_equal: bool,
    pub parallel_all_response_equal: bool,
    pub selective_strategy_response_equal: bool,
}

#[derive(Debug, Serialize)]
pub struct PresentationParallelStageResults {
    pub row_expansion_sequential: Timing,
    pub sort_key_creation: PresentationParallelComparison,
    pub sorting: PresentationParallelComparison,
    pub value_extraction_and_width_reduction: PresentationParallelComparison,
    pub width_resolution_sequential: Timing,
    pub truncation_padding_and_row_formatting: PresentationParallelComparison,
}

#[derive(Debug, Serialize)]
pub struct PresentationParallelTotalResults {
    pub sequential: Timing,
    pub parallel_sort: PresentationParallelCandidateTiming,
    pub parallel_value_extraction: PresentationParallelCandidateTiming,
    pub parallel_formatting: PresentationParallelCandidateTiming,
    pub parallel_layout: PresentationParallelCandidateTiming,
    pub parallel_all: PresentationParallelCandidateTiming,
    pub selective_strategy: PresentationParallelCandidateTiming,
}

#[derive(Debug, Serialize)]
pub struct PresentationParallelComparison {
    pub sequential: Timing,
    pub parallel: Timing,
    pub parallel_change_percent: f64,
}

#[derive(Debug, Serialize)]
pub struct PresentationParallelCandidateTiming {
    pub timing: Timing,
    pub change_from_sequential_percent: f64,
}

pub fn run(
    output: &Path,
    work_dir: &Path,
    options: PresentationBenchmarkOptions,
) -> Result<(), String> {
    validate_options(&options)?;
    if output.exists() {
        return Err(format!(
            "benchmark output must not already exist: {}",
            output.display()
        ));
    }

    let work_dir = absolute_existing_path(work_dir, "benchmark work directory")?;
    let mut sizes = Vec::with_capacity(options.row_counts.len());
    for target_results in &options.row_counts {
        sizes.push(measure_size(&work_dir, *target_results, &options)?);
    }

    let result = PresentationParallelBenchmarkOutput {
        output_schema_version: OUTPUT_SCHEMA_VERSION,
        corpus_contract_version: CORPUS_CONTRACT_VERSION,
        protocol: PresentationParallelBenchmarkProtocol {
            warmups: options.warmups,
            iterations: options.iterations,
            row_counts: options.row_counts,
            baseline: "final sequential presentation-json version 2 presentation pipeline",
            input_policy: "sequential and parallel candidates use identical in-memory query results loaded from the same benchmark database",
            equality_policy: "all candidate rows and complete PresentationResponse values must equal the sequential baseline before timings are accepted",
            ordering_policy: "parallel sorting uses the production comparator including original_index as the final deterministic tie-breaker",
            sqlite_policy: "query loading stays sequential on one read-only database snapshot; SQLite execution is outside candidate timings",
            candidate_policy: "v2 measures isolated sort, value-extraction, formatting, layout, and all-parallel candidates plus one data-derived selective threshold strategy; production remains sequential",
            cli_policy: "complete CLI timing is deferred until the selective strategy proves useful enough for production integration",
        },
        environment: PresentationParallelBenchmarkEnvironment {
            command_arguments: std::env::args().collect(),
            build_profile: if cfg!(debug_assertions) {
                "debug"
            } else {
                "release"
            },
            operating_system: std::env::consts::OS,
            architecture: std::env::consts::ARCH,
            available_cpus: std::thread::available_parallelism()
                .ok()
                .map(usize::from),
            rayon_threads: rayon::current_num_threads(),
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

fn measure_size(
    work_dir: &Path,
    target_results: usize,
    options: &PresentationBenchmarkOptions,
) -> Result<PresentationParallelSizeResult, String> {
    let db_path = work_dir
        .join(format!("rows-{target_results}"))
        .join("org-files-db.sqlite");
    if !db_path.is_file() {
        return Err(format!(
            "benchmark database does not exist for {target_results} results: {}",
            db_path.display()
        ));
    }

    let mut workloads = Vec::with_capacity(WORKLOADS.len());
    for workload in WORKLOADS {
        workloads.push(measure_workload(
            &db_path,
            *workload,
            target_results,
            options,
        )?);
    }

    Ok(PresentationParallelSizeResult {
        target_results,
        workloads,
    })
}

fn measure_workload(
    db_path: &Path,
    workload: PresentationWorkload,
    expected_results: usize,
    options: &PresentationBenchmarkOptions,
) -> Result<PresentationParallelWorkloadResult, String> {
    let sequential_response = load_workload_response(db_path, workload, expected_results)?;
    let spec = PresentationSpec::parse_json(workload.presentation_spec_json)
        .map_err(|error| error.to_string())?;
    let database_id = sequential_response.database_id.clone();
    let generation = sequential_response.generation;
    let results = sequential_response.results.clone();

    let expanded_rows = spec
        .expand_rows(&results)
        .map_err(|error| error.to_string())?;
    let presentation_row_count = expanded_rows.len();
    let cell_count = presentation_row_count.saturating_mul(spec.columns.len());
    let strategy = select_strategy(results.len(), presentation_row_count, spec.columns.len());

    let sequential_sort_plan = spec
        .prepare_sort_rows(&results, expanded_rows.clone())
        .map_err(|error| error.to_string())?;
    let parallel_sort_plan = spec
        .prepare_sort_rows_parallel(&results, expanded_rows.clone())
        .map_err(|error| error.to_string())?;
    let sequential_sorted_rows = spec.finish_sort_rows(sequential_sort_plan.clone());
    let parallel_sorted_rows = spec.finish_sort_rows_parallel(parallel_sort_plan.clone());
    let parallel_sort_rows_equal = parallel_sorted_rows == sequential_sorted_rows;
    if !parallel_sort_rows_equal {
        return Err(format!(
            "{} parallel sort rows differ from sequential rows",
            workload.id
        ));
    }

    let sequential_layout_plan = spec
        .prepare_layout_rows(&results, sequential_sorted_rows.clone())
        .map_err(|error| error.to_string())?;
    let parallel_layout_plan = spec
        .prepare_layout_rows_parallel(&results, sequential_sorted_rows.clone())
        .map_err(|error| error.to_string())?;
    if sequential_layout_plan.natural_widths() != parallel_layout_plan.natural_widths() {
        return Err(format!(
            "{} parallel natural widths differ from sequential widths",
            workload.id
        ));
    }
    let widths = spec
        .resolve_layout_widths(sequential_layout_plan.natural_widths())
        .map_err(|error| error.to_string())?;
    let sequential_layout_rows = spec.finish_layout_rows(sequential_layout_plan.clone(), &widths);
    let parallel_layout_rows =
        spec.finish_layout_rows_parallel(parallel_layout_plan.clone(), &widths);
    let parallel_layout_rows_equal = parallel_layout_rows == sequential_layout_rows;
    if !parallel_layout_rows_equal {
        return Err(format!(
            "{} parallel layout rows differ from sequential rows",
            workload.id
        ));
    }

    let parallel_sort_response = build_response_candidate(
        &spec,
        database_id.clone(),
        generation,
        results.clone(),
        true,
        false,
        false,
    )?;
    let parallel_value_extraction_response = build_response_candidate(
        &spec,
        database_id.clone(),
        generation,
        results.clone(),
        false,
        true,
        false,
    )?;
    let parallel_formatting_response = build_response_candidate(
        &spec,
        database_id.clone(),
        generation,
        results.clone(),
        false,
        false,
        true,
    )?;
    let parallel_layout_response = build_response_candidate(
        &spec,
        database_id.clone(),
        generation,
        results.clone(),
        false,
        true,
        true,
    )?;
    let parallel_all_response = build_response_candidate(
        &spec,
        database_id.clone(),
        generation,
        results.clone(),
        true,
        true,
        true,
    )?;
    let selective_strategy_response = build_response_candidate(
        &spec,
        database_id.clone(),
        generation,
        results.clone(),
        strategy.parallel_sort,
        strategy.parallel_value_extraction,
        strategy.parallel_formatting,
    )?;

    let parallel_sort_response_equal = parallel_sort_response == sequential_response;
    let parallel_value_extraction_response_equal =
        parallel_value_extraction_response == sequential_response;
    let parallel_formatting_response_equal = parallel_formatting_response == sequential_response;
    let parallel_layout_response_equal = parallel_layout_response == sequential_response;
    let parallel_all_response_equal = parallel_all_response == sequential_response;
    let selective_strategy_response_equal = selective_strategy_response == sequential_response;
    if !parallel_sort_response_equal
        || !parallel_value_extraction_response_equal
        || !parallel_formatting_response_equal
        || !parallel_layout_response_equal
        || !parallel_all_response_equal
        || !selective_strategy_response_equal
    {
        return Err(format!(
            "{} parallel complete response differs from sequential response",
            workload.id
        ));
    }

    let row_expansion_sequential = measure(options, || {
        let rows = spec
            .expand_rows(&results)
            .map_err(|error| error.to_string())?;
        std::hint::black_box(rows);
        Ok(())
    })?;

    let sort_key_creation = compare_stage(
        measure_with_setup(
            options,
            || expanded_rows.clone(),
            |rows| {
                let plan = spec
                    .prepare_sort_rows(&results, rows)
                    .map_err(|error| error.to_string())?;
                std::hint::black_box(plan);
                Ok(())
            },
        )?,
        measure_with_setup(
            options,
            || expanded_rows.clone(),
            |rows| {
                let plan = spec
                    .prepare_sort_rows_parallel(&results, rows)
                    .map_err(|error| error.to_string())?;
                std::hint::black_box(plan);
                Ok(())
            },
        )?,
    );

    let sorting = compare_stage(
        measure_with_setup(
            options,
            || sequential_sort_plan.clone(),
            |plan| {
                let rows = spec.finish_sort_rows(plan);
                std::hint::black_box(rows);
                Ok(())
            },
        )?,
        measure_with_setup(
            options,
            || sequential_sort_plan.clone(),
            |plan| {
                let rows = spec.finish_sort_rows_parallel(plan);
                std::hint::black_box(rows);
                Ok(())
            },
        )?,
    );

    let value_extraction_and_width_reduction = compare_stage(
        measure_with_setup(
            options,
            || sequential_sorted_rows.clone(),
            |rows| {
                let plan = spec
                    .prepare_layout_rows(&results, rows)
                    .map_err(|error| error.to_string())?;
                std::hint::black_box(plan);
                Ok(())
            },
        )?,
        measure_with_setup(
            options,
            || sequential_sorted_rows.clone(),
            |rows| {
                let plan = spec
                    .prepare_layout_rows_parallel(&results, rows)
                    .map_err(|error| error.to_string())?;
                std::hint::black_box(plan);
                Ok(())
            },
        )?,
    );

    let natural_widths = sequential_layout_plan.natural_widths().to_vec();
    let width_resolution_sequential = measure(options, || {
        let widths = spec
            .resolve_layout_widths(&natural_widths)
            .map_err(|error| error.to_string())?;
        std::hint::black_box(widths);
        Ok(())
    })?;

    let truncation_padding_and_row_formatting = compare_stage(
        measure_with_setup(
            options,
            || sequential_layout_plan.clone(),
            |plan| {
                let rows = spec.finish_layout_rows(plan, &widths);
                std::hint::black_box(rows);
                Ok(())
            },
        )?,
        measure_with_setup(
            options,
            || sequential_layout_plan.clone(),
            |plan| {
                let rows = spec.finish_layout_rows_parallel(plan, &widths);
                std::hint::black_box(rows);
                Ok(())
            },
        )?,
    );

    let sequential_total = measure_with_setup(
        options,
        || results.clone(),
        |results| {
            let response = spec
                .build_response(database_id.clone(), generation, results)
                .map_err(|error| error.to_string())?;
            std::hint::black_box(response);
            Ok(())
        },
    )?;
    let parallel_sort_total = measure_with_setup(
        options,
        || results.clone(),
        |results| {
            let response = build_response_candidate(
                &spec,
                database_id.clone(),
                generation,
                results,
                true,
                false,
                false,
            )?;
            std::hint::black_box(response);
            Ok(())
        },
    )?;
    let parallel_value_extraction_total = measure_with_setup(
        options,
        || results.clone(),
        |results| {
            let response = build_response_candidate(
                &spec,
                database_id.clone(),
                generation,
                results,
                false,
                true,
                false,
            )?;
            std::hint::black_box(response);
            Ok(())
        },
    )?;
    let parallel_formatting_total = measure_with_setup(
        options,
        || results.clone(),
        |results| {
            let response = build_response_candidate(
                &spec,
                database_id.clone(),
                generation,
                results,
                false,
                false,
                true,
            )?;
            std::hint::black_box(response);
            Ok(())
        },
    )?;
    let parallel_layout_total = measure_with_setup(
        options,
        || results.clone(),
        |results| {
            let response = build_response_candidate(
                &spec,
                database_id.clone(),
                generation,
                results,
                false,
                true,
                true,
            )?;
            std::hint::black_box(response);
            Ok(())
        },
    )?;
    let parallel_all_total = measure_with_setup(
        options,
        || results.clone(),
        |results| {
            let response = build_response_candidate(
                &spec,
                database_id.clone(),
                generation,
                results,
                true,
                true,
                true,
            )?;
            std::hint::black_box(response);
            Ok(())
        },
    )?;
    let selective_strategy_total = measure_with_setup(
        options,
        || results.clone(),
        |results| {
            let response = build_response_candidate(
                &spec,
                database_id.clone(),
                generation,
                results,
                strategy.parallel_sort,
                strategy.parallel_value_extraction,
                strategy.parallel_formatting,
            )?;
            std::hint::black_box(response);
            Ok(())
        },
    )?;

    let serialization_unchanged = measure(options, || {
        let bytes = serde_json::to_vec(&sequential_response).map_err(|error| error.to_string())?;
        std::hint::black_box(bytes);
        Ok(())
    })?;

    Ok(PresentationParallelWorkloadResult {
        id: workload.id,
        query: workload.query,
        presentation_spec_json: workload.presentation_spec_json,
        result_count: results.len(),
        presentation_row_count,
        cell_count,
        strategy,
        equality: PresentationParallelEquality {
            parallel_sort_rows_equal,
            parallel_layout_rows_equal,
            parallel_sort_response_equal,
            parallel_value_extraction_response_equal,
            parallel_formatting_response_equal,
            parallel_layout_response_equal,
            parallel_all_response_equal,
            selective_strategy_response_equal,
        },
        stages: PresentationParallelStageResults {
            row_expansion_sequential,
            sort_key_creation,
            sorting,
            value_extraction_and_width_reduction,
            width_resolution_sequential,
            truncation_padding_and_row_formatting,
        },
        totals: PresentationParallelTotalResults {
            parallel_sort: candidate_timing(&sequential_total, parallel_sort_total),
            parallel_value_extraction: candidate_timing(
                &sequential_total,
                parallel_value_extraction_total,
            ),
            parallel_formatting: candidate_timing(&sequential_total, parallel_formatting_total),
            parallel_layout: candidate_timing(&sequential_total, parallel_layout_total),
            parallel_all: candidate_timing(&sequential_total, parallel_all_total),
            selective_strategy: candidate_timing(&sequential_total, selective_strategy_total),
            sequential: sequential_total,
        },
        serialization_unchanged,
    })
}

fn build_response_candidate(
    spec: &PresentationSpec,
    database_id: String,
    generation: i64,
    results: Vec<QueryResultNode>,
    parallel_sort: bool,
    parallel_value_extraction: bool,
    parallel_formatting: bool,
) -> Result<PresentationResponse, String> {
    let rows = spec
        .expand_rows(&results)
        .map_err(|error| error.to_string())?;
    let sort_plan = if parallel_sort {
        spec.prepare_sort_rows_parallel(&results, rows)
    } else {
        spec.prepare_sort_rows(&results, rows)
    }
    .map_err(|error| error.to_string())?;
    let rows = if parallel_sort {
        spec.finish_sort_rows_parallel(sort_plan)
    } else {
        spec.finish_sort_rows(sort_plan)
    };
    let layout_plan = if parallel_value_extraction {
        spec.prepare_layout_rows_parallel(&results, rows)
    } else {
        spec.prepare_layout_rows(&results, rows)
    }
    .map_err(|error| error.to_string())?;
    let widths = spec
        .resolve_layout_widths(layout_plan.natural_widths())
        .map_err(|error| error.to_string())?;
    let rows = if parallel_formatting {
        spec.finish_layout_rows_parallel(layout_plan, &widths)
    } else {
        spec.finish_layout_rows(layout_plan, &widths)
    };

    Ok(PresentationResponse::new(
        database_id,
        generation,
        results,
        rows,
    ))
}

fn select_strategy(
    result_count: usize,
    row_count: usize,
    column_count: usize,
) -> PresentationParallelStrategyDecision {
    let cell_count = row_count.saturating_mul(column_count);
    let wide_layout = column_count >= PARALLEL_WIDE_COLUMN_THRESHOLD
        && cell_count >= PARALLEL_WIDE_CELL_THRESHOLD;
    let large_layout = cell_count >= PARALLEL_LAYOUT_CELL_THRESHOLD;
    let one_row_per_result = row_count == result_count;

    PresentationParallelStrategyDecision {
        parallel_sort: row_count >= PARALLEL_SORT_ROW_THRESHOLD,
        parallel_value_extraction: large_layout || wide_layout,
        parallel_formatting: one_row_per_result && (large_layout || wide_layout),
        sort_row_threshold: PARALLEL_SORT_ROW_THRESHOLD,
        layout_cell_threshold: PARALLEL_LAYOUT_CELL_THRESHOLD,
        wide_column_threshold: PARALLEL_WIDE_COLUMN_THRESHOLD,
        wide_cell_threshold: PARALLEL_WIDE_CELL_THRESHOLD,
    }
}

fn compare_stage(sequential: Timing, parallel: Timing) -> PresentationParallelComparison {
    PresentationParallelComparison {
        parallel_change_percent: percent_change(sequential.median_ns, parallel.median_ns),
        sequential,
        parallel,
    }
}

fn candidate_timing(sequential: &Timing, timing: Timing) -> PresentationParallelCandidateTiming {
    PresentationParallelCandidateTiming {
        change_from_sequential_percent: percent_change(sequential.median_ns, timing.median_ns),
        timing,
    }
}

fn percent_change(baseline_ns: u128, candidate_ns: u128) -> f64 {
    if baseline_ns == 0 {
        return 0.0;
    }
    ((candidate_ns as f64 / baseline_ns as f64) - 1.0) * 100.0
}

fn validate_options(options: &PresentationBenchmarkOptions) -> Result<(), String> {
    if options.row_counts.is_empty() {
        return Err("--rows must contain at least one positive row count".into());
    }
    if options.row_counts.contains(&0) {
        return Err("--rows values must be positive".into());
    }
    if options.iterations == 0 {
        return Err("--iterations must be positive".into());
    }
    Ok(())
}

fn absolute_existing_path(path: &Path, label: &str) -> Result<PathBuf, String> {
    let path = if path.is_absolute() {
        path.to_path_buf()
    } else {
        std::env::current_dir()
            .map_err(|error| error.to_string())?
            .join(path)
    };
    fs::canonicalize(&path).map_err(|error| format!("{label} {}: {error}", path.display()))
}

fn measure<F>(options: &PresentationBenchmarkOptions, mut operation: F) -> Result<Timing, String>
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

fn measure_with_setup<S, Setup, Operation>(
    options: &PresentationBenchmarkOptions,
    mut setup: Setup,
    mut operation: Operation,
) -> Result<Timing, String>
where
    Setup: FnMut() -> S,
    Operation: FnMut(S) -> Result<(), String>,
{
    for _ in 0..options.warmups {
        operation(setup())?;
    }
    let mut samples = Vec::with_capacity(options.iterations);
    for _ in 0..options.iterations {
        let input = setup();
        let start = Instant::now();
        operation(input)?;
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
    use super::select_strategy;

    #[test]
    fn selective_strategy_keeps_small_and_narrow_workloads_sequential() {
        let small = select_strategy(1_000, 1_000, 11);
        assert!(!small.parallel_sort);
        assert!(!small.parallel_value_extraction);
        assert!(!small.parallel_formatting);

        let narrow = select_strategy(50_000, 50_000, 1);
        assert!(!narrow.parallel_sort);
        assert!(!narrow.parallel_value_extraction);
        assert!(!narrow.parallel_formatting);
    }

    #[test]
    fn selective_strategy_parallelizes_measured_large_layout_work() {
        let wide = select_strategy(10_000, 10_000, 11);
        assert!(!wide.parallel_sort);
        assert!(wide.parallel_value_extraction);
        assert!(wide.parallel_formatting);

        let normal = select_strategy(50_000, 50_000, 4);
        assert!(!normal.parallel_sort);
        assert!(normal.parallel_value_extraction);
        assert!(normal.parallel_formatting);
    }

    #[test]
    fn selective_strategy_keeps_expanded_formatting_sequential() {
        let expanded = select_strategy(50_000, 150_000, 3);
        assert!(expanded.parallel_sort);
        assert!(expanded.parallel_value_extraction);
        assert!(!expanded.parallel_formatting);
    }
}
