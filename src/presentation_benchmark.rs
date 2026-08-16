//! Measurement-only benchmark support for Rust-prepared presentation output.

use std::{
    fs,
    path::{Path, PathBuf},
    process::{Command, Stdio},
    time::{Duration, Instant},
};

use serde::Serialize;
use sha2::{Digest, Sha256};

use crate::{
    config::{Config, ConfiguredDir, SearchConfig},
    db::{
        open_database_with_schema, open_existing_database_read_only, read_index_state,
        SchemaDefinition, CURRENT_SCHEMA_VERSION,
    },
    indexer::Indexer,
    parser::OrgizeAdapter,
    presentation::{
        PresentationCell, PresentationLayoutPlan, PresentationResponse, PresentationRow,
        PresentationRowContext, PresentationSortPlan, PresentationSpec,
    },
    query::{
        execute_and_shape_query, parse_query, sqlite_query_validation_options, validate_query,
        QueryExecutionOptions, QueryOutputMode, QueryResultNode,
    },
};

pub const OUTPUT_SCHEMA_VERSION: &str = "1";
pub const PAYLOAD_ANALYSIS_SCHEMA_VERSION: &str = "1";
pub const CORPUS_CONTRACT_VERSION: &str = "1";
pub const DEFAULT_WARMUPS: usize = 3;
pub const DEFAULT_ITERATIONS: usize = 10;
pub const DEFAULT_ROW_COUNTS: &[usize] = &[100, 1_000, 10_000, 50_000];

#[derive(Debug, Clone)]
pub struct PresentationBenchmarkOptions {
    pub row_counts: Vec<usize>,
    pub warmups: usize,
    pub iterations: usize,
    pub seed: u64,
}

impl Default for PresentationBenchmarkOptions {
    fn default() -> Self {
        Self {
            row_counts: DEFAULT_ROW_COUNTS.to_vec(),
            warmups: DEFAULT_WARMUPS,
            iterations: DEFAULT_ITERATIONS,
            seed: 1,
        }
    }
}

#[derive(Debug, Serialize)]
pub struct PresentationBenchmarkOutput {
    pub output_schema_version: &'static str,
    pub corpus_contract_version: &'static str,
    pub protocol: PresentationBenchmarkProtocol,
    pub environment: PresentationBenchmarkEnvironment,
    pub process_startup: Timing,
    pub sizes: Vec<PresentationSizeResult>,
}

#[derive(Debug, Serialize)]
pub struct PresentationBenchmarkProtocol {
    pub warmups: usize,
    pub iterations: usize,
    pub row_counts: Vec<usize>,
    pub query_connection: &'static str,
    pub cli_measurement: &'static str,
    pub process_startup_measurement: &'static str,
    pub cache_policy: &'static str,
    pub parallel_policy: &'static str,
}

#[derive(Debug, Serialize)]
pub struct PresentationBenchmarkEnvironment {
    pub command_arguments: Vec<String>,
    pub build_profile: &'static str,
    pub operating_system: &'static str,
    pub architecture: &'static str,
    pub available_cpus: Option<usize>,
    pub orgfdb_path: String,
}

#[derive(Debug, Serialize)]
pub struct PresentationSizeResult {
    pub target_results: usize,
    pub corpus: PresentationCorpusResult,
    pub workloads: Vec<PresentationWorkloadResult>,
}

#[derive(Debug, Serialize)]
pub struct PresentationCorpusResult {
    pub files: usize,
    pub fingerprint: String,
    pub generation_ns: u128,
    pub rebuild_ns: u128,
    pub database_bytes: u64,
}

#[derive(Debug, Serialize)]
pub struct PresentationWorkloadResult {
    pub id: &'static str,
    pub query: &'static str,
    pub presentation_spec_json: &'static str,
    pub result_count: usize,
    pub presentation_row_count: usize,
    pub payload_bytes: usize,
    pub stages: PresentationStageTimings,
    pub cli_total_elapsed: Timing,
}

#[derive(Debug, Serialize)]
pub struct PresentationStageTimings {
    pub database_query_execution: Timing,
    pub row_expansion: Timing,
    pub sort_key_creation: Timing,
    pub sorting: Timing,
    pub value_extraction: Timing,
    pub width_calculation: Timing,
    pub truncation_padding_and_row_formatting: Timing,
    pub presentation_total: Timing,
    pub json_serialization: Timing,
    pub internal_total_with_serialization: Timing,
}

#[derive(Debug, Serialize)]
pub struct Timing {
    pub samples: usize,
    pub min_ns: u128,
    pub median_ns: u128,
    pub max_ns: u128,
    pub p95_ns: u128,
}

#[derive(Debug, Clone)]
pub struct PresentationPayloadAnalysisOptions {
    pub row_counts: Vec<usize>,
}

impl Default for PresentationPayloadAnalysisOptions {
    fn default() -> Self {
        Self {
            row_counts: DEFAULT_ROW_COUNTS.to_vec(),
        }
    }
}

#[derive(Debug, Serialize)]
pub struct PresentationPayloadAnalysisOutput {
    pub output_schema_version: &'static str,
    pub corpus_contract_version: &'static str,
    pub protocol: PresentationPayloadAnalysisProtocol,
    pub environment: PresentationPayloadAnalysisEnvironment,
    pub sizes: Vec<PresentationPayloadAnalysisSizeResult>,
}

#[derive(Debug, Serialize)]
pub struct PresentationPayloadAnalysisProtocol {
    pub row_counts: Vec<usize>,
    pub source: &'static str,
    pub public_wire_format: &'static str,
    pub compression: &'static str,
    pub streaming: &'static str,
    pub cache_policy: &'static str,
    pub parallel_policy: &'static str,
}

#[derive(Debug, Serialize)]
pub struct PresentationPayloadAnalysisEnvironment {
    pub command_arguments: Vec<String>,
    pub build_profile: &'static str,
    pub operating_system: &'static str,
    pub architecture: &'static str,
}

#[derive(Debug, Serialize)]
pub struct PresentationPayloadAnalysisSizeResult {
    pub target_results: usize,
    pub workloads: Vec<PresentationPayloadAnalysisWorkloadResult>,
}

#[derive(Debug, Serialize)]
pub struct PresentationPayloadAnalysisWorkloadResult {
    pub id: &'static str,
    pub query: &'static str,
    pub presentation_spec_json: &'static str,
    pub result_count: usize,
    pub presentation_row_count: usize,
    pub cell_count: usize,
    pub compact_json_bytes: usize,
    pub cli_payload_bytes: usize,
    pub sections: PresentationPayloadSections,
    pub field_names: PresentationPayloadFieldNames,
    pub structural_overhead: PresentationPayloadStructuralOverhead,
    pub averages: PresentationPayloadAverages,
    pub repetition: PresentationPayloadRepetition,
}

#[derive(Debug, Serialize)]
pub struct PresentationPayloadSections {
    pub response_metadata_bytes: usize,
    pub results_bytes: usize,
    pub rows_bytes: usize,
    pub row_metadata_bytes: usize,
    pub row_context_bytes: usize,
    pub cells_bytes: usize,
    pub cell_structure_bytes: usize,
    pub search_text_bytes: usize,
    pub display_text_bytes: usize,
    pub semantic_role_bytes: usize,
}

#[derive(Debug, Serialize)]
pub struct PresentationPayloadFieldNames {
    pub total_bytes: usize,
    pub response_metadata_bytes: usize,
    pub results_bytes: usize,
    pub row_metadata_bytes: usize,
    pub row_context_bytes: usize,
    pub cells_bytes: usize,
}

#[derive(Debug, Serialize)]
pub struct PresentationPayloadStructuralOverhead {
    pub field_name_bytes: usize,
    pub json_syntax_bytes: usize,
    pub combined_bytes: usize,
}

#[derive(Debug, Serialize)]
pub struct PresentationPayloadAverages {
    pub results_component_bytes_per_result: f64,
    pub rows_component_bytes_per_row: f64,
    pub cells_component_bytes_per_cell: f64,
    pub total_payload_bytes_per_result: f64,
}

#[derive(Debug, Serialize)]
pub struct PresentationPayloadRepetition {
    pub search_display_equal_cells: usize,
    pub search_display_duplicate_bytes: usize,
    pub display_is_padded_search_cells: usize,
    pub padded_search_repeated_content_bytes: usize,
    pub row_context_values: usize,
    pub row_context_values_repeated_in_cells: usize,
    pub row_context_cell_duplicate_bytes: usize,
    pub cells_matching_result_scalars: usize,
    pub result_cell_duplicate_bytes: usize,
}

#[derive(Debug, Clone, Copy)]
struct PresentationWorkload {
    id: &'static str,
    query: &'static str,
    presentation_spec_json: &'static str,
}

const NORMAL_SPEC: &str = r#"{"columns":[{"name":"title","width":{"mode":"max","value":48},"truncate":{"position":"middle","marker":"…"}},{"name":"todo-keyword","width":{"mode":"fixed","value":5}},{"name":"tags","width":{"mode":"max","value":32}},{"name":"file-name","width":{"mode":"max","value":28}}],"sort":[{"column":"file-name","direction":"asc"},{"column":"line-number","direction":"desc"}]}"#;

const TAGS_SPEC: &str = r#"{"columns":[{"name":"title","width":{"mode":"max","value":44}},{"name":"tag","width":{"mode":"max","value":20}},{"name":"file-name","width":{"mode":"max","value":28}}],"sort":[{"column":"tag","direction":"asc"},{"column":"title","direction":"asc"}],"row_source":{"kind":"tags"}}"#;

const EFFECTIVE_PROPERTIES_SPEC: &str = r#"{"columns":[{"name":"title","width":{"mode":"max","value":44}},{"name":"property-name","width":{"mode":"fixed","value":10}},{"name":"property-value","width":{"mode":"max","value":24}}],"sort":[{"column":"property-name","direction":"asc"},{"column":"property-value","direction":"asc"},{"column":"file-name","direction":"asc"}],"row_source":{"kind":"effective-properties"}}"#;

const KEYWORDS_SPEC: &str = r#"{"columns":[{"name":"file-name","width":{"mode":"max","value":28}},{"name":"keyword-name","width":{"mode":"fixed","value":10}},{"name":"keyword-value","width":{"mode":"max","value":32}}],"sort":[{"column":"keyword-name","direction":"asc"},{"column":"file-name","direction":"asc"}],"row_source":{"kind":"keywords"}}"#;

const WORKLOADS: &[PresentationWorkload] = &[
    PresentationWorkload {
        id: "headings.normal",
        query: "(headings (level 1))",
        presentation_spec_json: NORMAL_SPEC,
    },
    PresentationWorkload {
        id: "headings.tags",
        query: "(headings (level 1))",
        presentation_spec_json: TAGS_SPEC,
    },
    PresentationWorkload {
        id: "headings.effective-properties",
        query: "(headings (level 1))",
        presentation_spec_json: EFFECTIVE_PROPERTIES_SPEC,
    },
    PresentationWorkload {
        id: "files.keywords",
        query: "(files)",
        presentation_spec_json: KEYWORDS_SPEC,
    },
];

pub fn run(
    output: &Path,
    work_dir: &Path,
    orgfdb: &Path,
    options: PresentationBenchmarkOptions,
) -> Result<(), String> {
    validate_options(&options)?;
    if output.exists() {
        return Err(format!(
            "benchmark output must not already exist: {}",
            output.display()
        ));
    }
    if work_dir.exists() {
        return Err(format!(
            "benchmark work directory must not already exist: {}",
            work_dir.display()
        ));
    }

    let orgfdb = absolute_existing_path(orgfdb, "orgfdb binary")?;
    fs::create_dir_all(work_dir).map_err(|error| error.to_string())?;
    let work_dir = fs::canonicalize(work_dir).map_err(|error| error.to_string())?;

    let process_startup = measure_process_startup(&orgfdb, &options)?;
    let mut sizes = Vec::with_capacity(options.row_counts.len());
    for target_results in &options.row_counts {
        sizes.push(run_size(&work_dir, &orgfdb, *target_results, &options)?);
    }

    let result = PresentationBenchmarkOutput {
        output_schema_version: OUTPUT_SCHEMA_VERSION,
        corpus_contract_version: CORPUS_CONTRACT_VERSION,
        protocol: PresentationBenchmarkProtocol {
            warmups: options.warmups,
            iterations: options.iterations,
            row_counts: options.row_counts.clone(),
            query_connection: "one warmed read-only connection and one deferred read transaction per workload",
            cli_measurement: "fresh orgfdb process per sample; stdout is captured through a pipe",
            process_startup_measurement: "fresh orgfdb --help process; includes help generation; stdout and stderr are discarded",
            cache_policy: "no presentation cache",
            parallel_policy: "no presentation parallelism",
        },
        environment: PresentationBenchmarkEnvironment {
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
            orgfdb_path: orgfdb.display().to_string(),
        },
        process_startup,
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

pub fn analyze_payloads(
    output: &Path,
    work_dir: &Path,
    options: PresentationPayloadAnalysisOptions,
) -> Result<(), String> {
    validate_payload_analysis_options(&options)?;
    if output.exists() {
        return Err(format!(
            "payload analysis output must not already exist: {}",
            output.display()
        ));
    }

    let work_dir = absolute_existing_path(work_dir, "benchmark work directory")?;
    let mut sizes = Vec::with_capacity(options.row_counts.len());
    for target_results in &options.row_counts {
        sizes.push(analyze_payload_size(&work_dir, *target_results)?);
    }

    let result = PresentationPayloadAnalysisOutput {
        output_schema_version: PAYLOAD_ANALYSIS_SCHEMA_VERSION,
        corpus_contract_version: CORPUS_CONTRACT_VERSION,
        protocol: PresentationPayloadAnalysisProtocol {
            row_counts: options.row_counts,
            source: "existing orgfdb-presentation-benchmark work directory",
            public_wire_format: "unchanged presentation-json version 1",
            compression: "none",
            streaming: "none",
            cache_policy: "no presentation cache",
            parallel_policy: "no presentation parallelism",
        },
        environment: PresentationPayloadAnalysisEnvironment {
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

fn validate_payload_analysis_options(
    options: &PresentationPayloadAnalysisOptions,
) -> Result<(), String> {
    if options.row_counts.is_empty() {
        return Err("--rows must contain at least one positive row count".into());
    }
    if options.row_counts.contains(&0) {
        return Err("--rows values must be positive".into());
    }
    Ok(())
}

fn analyze_payload_size(
    work_dir: &Path,
    target_results: usize,
) -> Result<PresentationPayloadAnalysisSizeResult, String> {
    let size_dir = work_dir.join(format!("rows-{target_results}"));
    let db_path = size_dir.join("org-files-db.sqlite");
    if !db_path.is_file() {
        return Err(format!(
            "benchmark database does not exist for {target_results} results: {}",
            db_path.display()
        ));
    }

    let mut workloads = Vec::with_capacity(WORKLOADS.len());
    for workload in WORKLOADS {
        let response = load_workload_response(&db_path, *workload, target_results)?;
        workloads.push(analyze_payload_response(*workload, &response)?);
    }

    Ok(PresentationPayloadAnalysisSizeResult {
        target_results,
        workloads,
    })
}

fn load_workload_response(
    db_path: &Path,
    workload: PresentationWorkload,
    expected_results: usize,
) -> Result<PresentationResponse, String> {
    let spec = PresentationSpec::parse_json(workload.presentation_spec_json)
        .map_err(|error| error.to_string())?;
    let connection =
        open_existing_database_read_only(db_path).map_err(|error| error.to_string())?;
    connection
        .execute_batch("BEGIN DEFERRED TRANSACTION")
        .map_err(|error| error.to_string())?;
    let state = read_index_state(&connection).map_err(|error| error.to_string())?;
    let parsed = parse_query(workload.query).map_err(|error| error.to_string())?;
    let validation_options =
        sqlite_query_validation_options(&connection).map_err(|error| error.to_string())?;
    let validated =
        validate_query(parsed, &validation_options).map_err(|error| error.to_string())?;
    let includes = spec
        .combined_includes_for_query_target(validated.target, &[])
        .map_err(|error| error.to_string())?;
    let query_options = QueryExecutionOptions {
        output_mode: QueryOutputMode::Flat,
        includes,
        ..Default::default()
    };
    let query_response = execute_and_shape_query(&connection, &validated, &query_options)
        .map_err(|error| error.to_string())?;
    if query_response.results.len() != expected_results {
        return Err(format!(
            "{} returned {} results, expected {}",
            workload.id,
            query_response.results.len(),
            expected_results
        ));
    }
    let response = spec
        .build_response(state.database_id, state.generation, query_response.results)
        .map_err(|error| error.to_string())?;
    connection
        .execute_batch("COMMIT")
        .map_err(|error| error.to_string())?;
    Ok(response)
}

fn analyze_payload_response(
    workload: PresentationWorkload,
    response: &PresentationResponse,
) -> Result<PresentationPayloadAnalysisWorkloadResult, String> {
    let compact_json = serde_json::to_vec(response).map_err(|error| error.to_string())?;
    let results_json = serde_json::to_vec(&response.results).map_err(|error| error.to_string())?;
    let rows_json = serde_json::to_vec(&response.rows).map_err(|error| error.to_string())?;

    let mut row_context_bytes = 0usize;
    let mut cells_bytes = 0usize;
    let mut search_text_bytes = 0usize;
    let mut display_text_bytes = 0usize;
    let mut semantic_role_bytes = 0usize;
    let mut row_context_field_name_bytes = 0usize;
    let mut cell_field_name_bytes = 0usize;
    let mut cell_count = 0usize;
    let mut search_display_equal_cells = 0usize;
    let mut search_display_duplicate_bytes = 0usize;
    let mut display_is_padded_search_cells = 0usize;
    let mut padded_search_repeated_content_bytes = 0usize;
    let mut row_context_values = 0usize;
    let mut row_context_values_repeated_in_cells = 0usize;
    let mut row_context_cell_duplicate_bytes = 0usize;

    for row in &response.rows {
        let context_json =
            serde_json::to_vec(&row.row_context).map_err(|error| error.to_string())?;
        row_context_bytes += context_json.len();
        row_context_field_name_bytes += count_json_field_name_bytes(&context_json);

        let row_cells_json = serde_json::to_vec(&row.cells).map_err(|error| error.to_string())?;
        cells_bytes += row_cells_json.len();
        cell_field_name_bytes += count_json_field_name_bytes(&row_cells_json);

        let context_values = row_context_scalar_values(row.row_context.as_ref());
        row_context_values += context_values.len();
        for context_value in context_values {
            if row
                .cells
                .iter()
                .any(|cell| cell.search_text == context_value)
            {
                row_context_values_repeated_in_cells += 1;
                row_context_cell_duplicate_bytes += json_string_bytes(context_value)?;
            }
        }

        for cell in &row.cells {
            cell_count += 1;
            search_text_bytes += json_string_bytes(&cell.search_text)?;
            display_text_bytes += json_string_bytes(&cell.display_text)?;
            semantic_role_bytes += serde_json::to_vec(&cell.role)
                .map_err(|error| error.to_string())?
                .len();
            if cell.search_text == cell.display_text {
                search_display_equal_cells += 1;
                search_display_duplicate_bytes += json_string_bytes(&cell.search_text)?;
            } else if display_is_padded_search(&cell.search_text, &cell.display_text) {
                display_is_padded_search_cells += 1;
                padded_search_repeated_content_bytes +=
                    json_string_bytes(&cell.search_text)?.saturating_sub(2);
            }
        }
    }

    let response_metadata_bytes = compact_json
        .len()
        .checked_sub(results_json.len() + rows_json.len())
        .ok_or("payload section sizes exceed the compact response size")?;
    let row_metadata_bytes = rows_json
        .len()
        .checked_sub(row_context_bytes + cells_bytes)
        .ok_or("row payload sections exceed the rows payload size")?;
    let cell_structure_bytes = cells_bytes
        .checked_sub(search_text_bytes + display_text_bytes + semantic_role_bytes)
        .ok_or("cell payload values exceed the cells payload size")?;

    let total_field_name_bytes = count_json_field_name_bytes(&compact_json);
    let results_field_name_bytes = count_json_field_name_bytes(&results_json);
    let all_rows_field_name_bytes = count_json_field_name_bytes(&rows_json);
    let row_metadata_field_name_bytes = all_rows_field_name_bytes
        .checked_sub(row_context_field_name_bytes + cell_field_name_bytes)
        .ok_or("row field-name sections exceed total row field-name bytes")?;
    let response_metadata_field_name_bytes = total_field_name_bytes
        .checked_sub(results_field_name_bytes + all_rows_field_name_bytes)
        .ok_or("nested field-name sections exceed total field-name bytes")?;
    let json_syntax_bytes = count_json_syntax_bytes(&compact_json);

    let (cells_matching_result_scalars, result_cell_duplicate_bytes) =
        result_cell_repetition(&response.results, &response.rows)?;

    Ok(PresentationPayloadAnalysisWorkloadResult {
        id: workload.id,
        query: workload.query,
        presentation_spec_json: workload.presentation_spec_json,
        result_count: response.results.len(),
        presentation_row_count: response.rows.len(),
        cell_count,
        compact_json_bytes: compact_json.len(),
        cli_payload_bytes: compact_json.len() + 1,
        sections: PresentationPayloadSections {
            response_metadata_bytes,
            results_bytes: results_json.len(),
            rows_bytes: rows_json.len(),
            row_metadata_bytes,
            row_context_bytes,
            cells_bytes,
            cell_structure_bytes,
            search_text_bytes,
            display_text_bytes,
            semantic_role_bytes,
        },
        field_names: PresentationPayloadFieldNames {
            total_bytes: total_field_name_bytes,
            response_metadata_bytes: response_metadata_field_name_bytes,
            results_bytes: results_field_name_bytes,
            row_metadata_bytes: row_metadata_field_name_bytes,
            row_context_bytes: row_context_field_name_bytes,
            cells_bytes: cell_field_name_bytes,
        },
        structural_overhead: PresentationPayloadStructuralOverhead {
            field_name_bytes: total_field_name_bytes,
            json_syntax_bytes,
            combined_bytes: total_field_name_bytes + json_syntax_bytes,
        },
        averages: PresentationPayloadAverages {
            results_component_bytes_per_result: average_bytes(
                results_json.len(),
                response.results.len(),
            ),
            rows_component_bytes_per_row: average_bytes(rows_json.len(), response.rows.len()),
            cells_component_bytes_per_cell: average_bytes(cells_bytes, cell_count),
            total_payload_bytes_per_result: average_bytes(
                compact_json.len(),
                response.results.len(),
            ),
        },
        repetition: PresentationPayloadRepetition {
            search_display_equal_cells,
            search_display_duplicate_bytes,
            display_is_padded_search_cells,
            padded_search_repeated_content_bytes,
            row_context_values,
            row_context_values_repeated_in_cells,
            row_context_cell_duplicate_bytes,
            cells_matching_result_scalars,
            result_cell_duplicate_bytes,
        },
    })
}

fn average_bytes(bytes: usize, count: usize) -> f64 {
    if count == 0 {
        0.0
    } else {
        bytes as f64 / count as f64
    }
}

fn json_string_bytes(value: &str) -> Result<usize, String> {
    serde_json::to_vec(value)
        .map(|bytes| bytes.len())
        .map_err(|error| error.to_string())
}

fn display_is_padded_search(search_text: &str, display_text: &str) -> bool {
    if display_text.len() <= search_text.len() || !display_text.starts_with(search_text) {
        return false;
    }
    display_text[search_text.len()..]
        .chars()
        .all(|character| character == ' ')
}

fn row_context_scalar_values(context: Option<&PresentationRowContext>) -> Vec<&str> {
    match context {
        None => Vec::new(),
        Some(PresentationRowContext::Tag { value }) => vec![value.as_str()],
        Some(PresentationRowContext::EffectiveProperty { name, value })
        | Some(PresentationRowContext::Keyword { name, value }) => {
            vec![name.as_str(), value.as_str()]
        }
    }
}

fn result_cell_repetition(
    results: &[QueryResultNode],
    rows: &[PresentationRow],
) -> Result<(usize, usize), String> {
    let mut cells_by_result: Vec<Vec<&PresentationCell>> = vec![Vec::new(); results.len()];
    for row in rows {
        let cells = cells_by_result.get_mut(row.result_index).ok_or_else(|| {
            format!(
                "presentation row result_index {} is outside {} results",
                row.result_index,
                results.len()
            )
        })?;
        cells.extend(row.cells.iter());
    }

    let mut matched_cells = 0usize;
    let mut duplicate_bytes = 0usize;
    for (result, cells) in results.iter().zip(cells_by_result) {
        if cells.is_empty() {
            continue;
        }
        let result_value = serde_json::to_value(result).map_err(|error| error.to_string())?;
        let mut matches = vec![false; cells.len()];
        mark_result_scalar_matches(&result_value, &cells, &mut matches);
        for (cell, matched) in cells.into_iter().zip(matches) {
            if matched {
                matched_cells += 1;
                duplicate_bytes += json_string_bytes(&cell.search_text)?;
            }
        }
    }
    Ok((matched_cells, duplicate_bytes))
}

fn mark_result_scalar_matches(
    value: &serde_json::Value,
    cells: &[&PresentationCell],
    matches: &mut [bool],
) {
    match value {
        serde_json::Value::Null => {}
        serde_json::Value::Bool(value) => {
            mark_scalar_text_match(if *value { "true" } else { "false" }, cells, matches)
        }
        serde_json::Value::Number(value) => {
            let text = value.to_string();
            mark_scalar_text_match(&text, cells, matches);
        }
        serde_json::Value::String(value) => mark_scalar_text_match(value, cells, matches),
        serde_json::Value::Array(values) => {
            for value in values {
                mark_result_scalar_matches(value, cells, matches);
            }
        }
        serde_json::Value::Object(values) => {
            for value in values.values() {
                mark_result_scalar_matches(value, cells, matches);
            }
        }
    }
}

fn mark_scalar_text_match(value: &str, cells: &[&PresentationCell], matches: &mut [bool]) {
    for (index, cell) in cells.iter().enumerate() {
        if !matches[index] && cell.search_text == value {
            matches[index] = true;
        }
    }
}

fn count_json_field_name_bytes(input: &[u8]) -> usize {
    let mut total = 0usize;
    let mut index = 0usize;
    while index < input.len() {
        if input[index] != b'"' {
            index += 1;
            continue;
        }

        let start = index;
        index += 1;
        while index < input.len() {
            match input[index] {
                b'\\' => index = (index + 2).min(input.len()),
                b'"' => break,
                _ => index += 1,
            }
        }
        if index >= input.len() {
            break;
        }
        let end = index;
        index += 1;
        let mut next = index;
        while next < input.len() && input[next].is_ascii_whitespace() {
            next += 1;
        }
        if next < input.len() && input[next] == b':' {
            total += end - start + 1;
        }
    }
    total
}

fn count_json_syntax_bytes(input: &[u8]) -> usize {
    let mut total = 0usize;
    let mut index = 0usize;
    while index < input.len() {
        match input[index] {
            b'"' => {
                index += 1;
                while index < input.len() {
                    match input[index] {
                        b'\\' => index = (index + 2).min(input.len()),
                        b'"' => {
                            index += 1;
                            break;
                        }
                        _ => index += 1,
                    }
                }
            }
            b'{' | b'}' | b'[' | b']' | b',' | b':' => {
                total += 1;
                index += 1;
            }
            _ => index += 1,
        }
    }
    total
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

fn run_size(
    work_dir: &Path,
    orgfdb: &Path,
    target_results: usize,
    options: &PresentationBenchmarkOptions,
) -> Result<PresentationSizeResult, String> {
    let size_dir = work_dir.join(format!("rows-{target_results}"));
    let source_dir = size_dir.join("corpus");
    let db_path = size_dir.join("org-files-db.sqlite");
    let config_path = size_dir.join("org-files-db.toml");
    fs::create_dir_all(&size_dir).map_err(|error| error.to_string())?;

    let generation_start = Instant::now();
    let fingerprint = generate_corpus(&source_dir, target_results, options.seed)?;
    let generation_ns = generation_start.elapsed().as_nanos();

    let config = benchmark_config(&source_dir, &db_path);
    let mut connection = open_database_with_schema(
        &db_path,
        &SchemaDefinition::new(CURRENT_SCHEMA_VERSION, false),
    )
    .map_err(|error| error.to_string())?;
    let rebuild_start = Instant::now();
    Indexer::new(OrgizeAdapter::new())
        .rebuild(&mut connection, &config)
        .map_err(|error| error.to_string())?;
    let rebuild_ns = rebuild_start.elapsed().as_nanos();
    drop(connection);
    write_cli_config(&config_path, &source_dir, &db_path)?;

    let database_bytes = fs::metadata(&db_path)
        .map_err(|error| error.to_string())?
        .len();
    let mut workloads = Vec::with_capacity(WORKLOADS.len());
    for workload in WORKLOADS {
        workloads.push(measure_workload(
            &db_path,
            &config_path,
            orgfdb,
            *workload,
            target_results,
            options,
        )?);
    }

    Ok(PresentationSizeResult {
        target_results,
        corpus: PresentationCorpusResult {
            files: target_results,
            fingerprint,
            generation_ns,
            rebuild_ns,
            database_bytes,
        },
        workloads,
    })
}

fn benchmark_config(source_dir: &Path, db_path: &Path) -> Config {
    Config {
        db_path: db_path.to_path_buf(),
        files: Vec::new(),
        dirs: vec![ConfiguredDir {
            path: source_dir.to_path_buf(),
            recursive: true,
            exclude: Vec::new(),
        }],
        search: SearchConfig {
            fts5_enabled: false,
            index_body_text: false,
        },
        ..Config::default()
    }
}

fn write_cli_config(config_path: &Path, source_dir: &Path, db_path: &Path) -> Result<(), String> {
    let db_path = toml::Value::String(db_path.display().to_string()).to_string();
    let source_dir = toml::Value::String(source_dir.display().to_string()).to_string();
    let content = format!(
        "db_path = {db_path}\n\n[[dirs]]\npath = {source_dir}\nrecursive = true\n\n[search]\nfts5_enabled = false\nindex_body_text = false\n"
    );
    fs::write(config_path, content).map_err(|error| error.to_string())
}

fn generate_corpus(directory: &Path, rows: usize, seed: u64) -> Result<String, String> {
    fs::create_dir_all(directory).map_err(|error| error.to_string())?;
    let mut digest = Sha256::new();
    for index in 0..rows {
        let adjusted = index as u64 + seed;
        let state = if adjusted.is_multiple_of(3) {
            "DONE"
        } else {
            "TODO"
        };
        let priority = match adjusted % 3 {
            0 => "A",
            1 => "B",
            _ => "C",
        };
        let content = format!(
            "#+TITLE: Benchmark {index:05}\n#+AUTHOR: Author {}\n#+CATEGORY: Category {}\n* {state} [#{priority}] Project {index:05} :project:group{}:common:\nSCHEDULED: <2026-01-{:02} Thu>\n:PROPERTIES:\n:GROUP: group{}\n:OWNER: owner{}\n:END:\nBenchmark presentation body {index}\n",
            adjusted % 17,
            adjusted % 11,
            adjusted % 7,
            adjusted % 28 + 1,
            adjusted % 13,
            adjusted % 19,
        );
        let relative = format!("bucket-{:04}/note-{index:05}.org", index / 500);
        digest.update(relative.as_bytes());
        digest.update([0]);
        digest.update(content.as_bytes());
        digest.update([0]);
        let path = directory.join(relative);
        fs::create_dir_all(path.parent().expect("benchmark file has a parent"))
            .map_err(|error| error.to_string())?;
        fs::write(path, content).map_err(|error| error.to_string())?;
    }
    Ok(format!("sha256:{:x}", digest.finalize()))
}

fn measure_workload(
    db_path: &Path,
    config_path: &Path,
    orgfdb: &Path,
    workload: PresentationWorkload,
    expected_results: usize,
    options: &PresentationBenchmarkOptions,
) -> Result<PresentationWorkloadResult, String> {
    let spec = PresentationSpec::parse_json(workload.presentation_spec_json)
        .map_err(|error| error.to_string())?;
    let connection =
        open_existing_database_read_only(db_path).map_err(|error| error.to_string())?;
    connection
        .execute_batch("BEGIN DEFERRED TRANSACTION")
        .map_err(|error| error.to_string())?;
    let state = read_index_state(&connection).map_err(|error| error.to_string())?;
    let parsed = parse_query(workload.query).map_err(|error| error.to_string())?;
    let validation_options =
        sqlite_query_validation_options(&connection).map_err(|error| error.to_string())?;
    let validated =
        validate_query(parsed, &validation_options).map_err(|error| error.to_string())?;
    let includes = spec
        .combined_includes_for_query_target(validated.target, &[])
        .map_err(|error| error.to_string())?;
    let query_options = QueryExecutionOptions {
        output_mode: QueryOutputMode::Flat,
        includes,
        ..Default::default()
    };

    let response = execute_and_shape_query(&connection, &validated, &query_options)
        .map_err(|error| error.to_string())?;
    if response.results.len() != expected_results {
        return Err(format!(
            "{} returned {} results, expected {}",
            workload.id,
            response.results.len(),
            expected_results
        ));
    }
    let results = response.results;

    let database_query_execution = measure(options, || {
        let response = execute_and_shape_query(&connection, &validated, &query_options)
            .map_err(|error| error.to_string())?;
        if response.results.len() != expected_results {
            return Err(format!(
                "{} result count changed during query measurement",
                workload.id
            ));
        }
        std::hint::black_box(response);
        Ok(())
    })?;

    let expanded_rows = spec
        .expand_rows(&results)
        .map_err(|error| error.to_string())?;
    let presentation_row_count = expanded_rows.len();
    let row_expansion = measure(options, || {
        let rows = spec
            .expand_rows(&results)
            .map_err(|error| error.to_string())?;
        if rows.len() != presentation_row_count {
            return Err(format!(
                "{} row count changed during expansion measurement",
                workload.id
            ));
        }
        std::hint::black_box(rows);
        Ok(())
    })?;

    let sort_plan = spec
        .prepare_sort_rows(&results, expanded_rows.clone())
        .map_err(|error| error.to_string())?;
    let sort_key_creation = measure_with_setup(
        options,
        || expanded_rows.clone(),
        |rows| {
            let plan = spec
                .prepare_sort_rows(&results, rows)
                .map_err(|error| error.to_string())?;
            std::hint::black_box(plan);
            Ok(())
        },
    )?;

    let sorted_rows = spec.finish_sort_rows(sort_plan.clone());
    let sorting = measure_with_setup(
        options,
        || sort_plan.clone(),
        |plan: PresentationSortPlan| {
            let rows = spec.finish_sort_rows(plan);
            std::hint::black_box(rows);
            Ok(())
        },
    )?;

    let layout_plan = spec
        .prepare_layout_rows(&results, sorted_rows.clone())
        .map_err(|error| error.to_string())?;
    let value_extraction = measure_with_setup(
        options,
        || sorted_rows.clone(),
        |rows| {
            let plan = spec
                .prepare_layout_rows(&results, rows)
                .map_err(|error| error.to_string())?;
            std::hint::black_box(plan);
            Ok(())
        },
    )?;

    let widths = spec
        .resolve_layout_widths(layout_plan.natural_widths())
        .map_err(|error| error.to_string())?;
    let natural_widths = layout_plan.natural_widths().to_vec();
    let width_calculation = measure(options, || {
        let widths = spec
            .resolve_layout_widths(&natural_widths)
            .map_err(|error| error.to_string())?;
        std::hint::black_box(widths);
        Ok(())
    })?;

    let final_rows = spec.finish_layout_rows(layout_plan.clone(), &widths);
    let truncation_padding_and_row_formatting = measure_with_setup(
        options,
        || layout_plan.clone(),
        |plan: PresentationLayoutPlan| {
            let rows = spec.finish_layout_rows(plan, &widths);
            std::hint::black_box(rows);
            Ok(())
        },
    )?;

    let response_template = PresentationResponse::new(
        state.database_id.clone(),
        state.generation,
        results.clone(),
        final_rows,
    );
    let compact_json = serde_json::to_vec(&response_template).map_err(|error| error.to_string())?;
    let mut expected_cli_payload = compact_json.clone();
    expected_cli_payload.push(b'\n');

    let presentation_total = measure_with_setup(
        options,
        || results.clone(),
        |results| {
            let response = spec
                .build_response(state.database_id.clone(), state.generation, results)
                .map_err(|error| error.to_string())?;
            std::hint::black_box(response);
            Ok(())
        },
    )?;

    let json_serialization = measure(options, || {
        let bytes = serde_json::to_vec(&response_template).map_err(|error| error.to_string())?;
        if bytes.len() != compact_json.len() {
            return Err(format!(
                "{} JSON payload size changed during serialization measurement",
                workload.id
            ));
        }
        std::hint::black_box(bytes);
        Ok(())
    })?;

    let internal_total_with_serialization = measure(options, || {
        let query_response = execute_and_shape_query(&connection, &validated, &query_options)
            .map_err(|error| error.to_string())?;
        let response = spec
            .build_response(
                state.database_id.clone(),
                state.generation,
                query_response.results,
            )
            .map_err(|error| error.to_string())?;
        let bytes = serde_json::to_vec(&response).map_err(|error| error.to_string())?;
        std::hint::black_box(bytes);
        Ok(())
    })?;

    let (cli_total_elapsed, payload_bytes) = measure_cli_total(
        orgfdb,
        config_path,
        workload,
        &expected_cli_payload,
        options,
    )?;

    connection
        .execute_batch("COMMIT")
        .map_err(|error| error.to_string())?;

    Ok(PresentationWorkloadResult {
        id: workload.id,
        query: workload.query,
        presentation_spec_json: workload.presentation_spec_json,
        result_count: expected_results,
        presentation_row_count,
        payload_bytes,
        stages: PresentationStageTimings {
            database_query_execution,
            row_expansion,
            sort_key_creation,
            sorting,
            value_extraction,
            width_calculation,
            truncation_padding_and_row_formatting,
            presentation_total,
            json_serialization,
            internal_total_with_serialization,
        },
        cli_total_elapsed,
    })
}

fn measure_process_startup(
    orgfdb: &Path,
    options: &PresentationBenchmarkOptions,
) -> Result<Timing, String> {
    measure(options, || {
        let status = Command::new(orgfdb)
            .arg("--help")
            .stdout(Stdio::null())
            .stderr(Stdio::null())
            .status()
            .map_err(|error| error.to_string())?;
        if !status.success() && status.code() != Some(2) {
            return Err(format!("orgfdb --help exited with {status}"));
        }
        Ok(())
    })
}

fn measure_cli_total(
    orgfdb: &Path,
    config_path: &Path,
    workload: PresentationWorkload,
    expected_payload: &[u8],
    options: &PresentationBenchmarkOptions,
) -> Result<(Timing, usize), String> {
    for _ in 0..options.warmups {
        let output = run_cli_query(orgfdb, config_path, workload)?;
        if output != expected_payload {
            return Err(format!(
                "{} CLI payload differs from the internal production pipeline",
                workload.id
            ));
        }
    }

    let mut samples = Vec::with_capacity(options.iterations);
    let mut payload_bytes = None;
    for _ in 0..options.iterations {
        let start = Instant::now();
        let output = run_cli_query(orgfdb, config_path, workload)?;
        let elapsed = start.elapsed();
        if output != expected_payload {
            return Err(format!(
                "{} CLI payload differs from the internal production pipeline",
                workload.id
            ));
        }
        if let Some(previous) = payload_bytes {
            if previous != output.len() {
                return Err(format!(
                    "{} CLI payload size changed during measurement",
                    workload.id
                ));
            }
        } else {
            payload_bytes = Some(output.len());
        }
        std::hint::black_box(output);
        samples.push(elapsed);
    }
    samples.sort();
    Ok((
        timing(&samples),
        payload_bytes.unwrap_or(expected_payload.len()),
    ))
}

fn run_cli_query(
    orgfdb: &Path,
    config_path: &Path,
    workload: PresentationWorkload,
) -> Result<Vec<u8>, String> {
    let output = Command::new(orgfdb)
        .args([
            "query",
            "--format",
            "presentation-json",
            "--presentation-spec-json",
            workload.presentation_spec_json,
            "--config",
        ])
        .arg(config_path)
        .arg(workload.query)
        .output()
        .map_err(|error| error.to_string())?;
    if !output.status.success() {
        return Err(format!(
            "{} CLI sample failed with {}: {}",
            workload.id,
            output.status,
            String::from_utf8_lossy(&output.stderr)
        ));
    }
    Ok(output.stdout)
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
    use super::*;

    #[test]
    fn default_row_counts_cover_requested_sizes() {
        assert_eq!(
            PresentationBenchmarkOptions::default().row_counts,
            vec![100, 1_000, 10_000, 50_000]
        );
    }

    #[test]
    fn workload_specs_parse_and_match_their_query_targets() {
        let connection = crate::db::open_in_memory_database().expect("database should open");
        for workload in WORKLOADS {
            let spec = PresentationSpec::parse_json(workload.presentation_spec_json)
                .expect("benchmark presentation spec should parse");
            let parsed = parse_query(workload.query).expect("benchmark query should parse");
            let validation = sqlite_query_validation_options(&connection)
                .expect("validation options should load");
            let validated =
                validate_query(parsed, &validation).expect("benchmark query should validate");
            spec.validate_for_query_target(validated.target)
                .expect("benchmark spec should match query target");
        }
    }

    #[test]
    fn benchmark_options_reject_empty_or_zero_rows() {
        let mut options = PresentationBenchmarkOptions::default();
        options.row_counts.clear();
        assert!(validate_options(&options).is_err());
        options.row_counts.push(0);
        assert!(validate_options(&options).is_err());
    }

    #[test]
    fn payload_analysis_options_reject_empty_or_zero_rows() {
        let mut options = PresentationPayloadAnalysisOptions::default();
        options.row_counts.clear();
        assert!(validate_payload_analysis_options(&options).is_err());
        options.row_counts.push(0);
        assert!(validate_payload_analysis_options(&options).is_err());
    }

    #[test]
    fn json_field_name_counter_ignores_string_content() {
        let input = br#"{"outer":{"inner":"value:still-value"},"list":[{"name":"x"}]}"#;
        assert_eq!(count_json_field_name_bytes(input), 26);
    }

    #[test]
    fn padded_display_detection_requires_only_right_side_spaces() {
        assert!(display_is_padded_search("tag", "tag  "));
        assert!(!display_is_padded_search("tag", "tag"));
        assert!(!display_is_padded_search("tag", " tag"));
        assert!(!display_is_padded_search("tag", "tag x"));
    }

    #[test]
    fn payload_analysis_sections_are_exact_and_track_repeated_values() {
        let result = QueryResultNode::File(crate::query::FileResultNode {
            kind: crate::query::QueryResultKind::File,
            matched: true,
            id: 1,
            level: 0,
            path: "/tmp/note.org".into(),
            name: "note.org".into(),
            dir: "/tmp".into(),
            title: "Note".into(),
            title_raw: None,
            root_heading_id: 1,
            mtime_ns: 0,
            size: 0,
            content_hash: None,
            indexed_at: None,
            location: crate::query::Location {
                file_path: "/tmp/note.org".into(),
                line: Some(1),
                byte_start: Some(0),
                byte_end: Some(10),
            },
            tags: vec!["tag".into()],
            node_path: None,
            properties: None,
            effective_properties: None,
            keywords: None,
            links: None,
            backlinks: None,
            children: None,
        });
        let response = PresentationResponse::new(
            "db",
            1,
            vec![result],
            vec![PresentationRow {
                result_index: 0,
                row_context: Some(PresentationRowContext::Tag {
                    value: "tag".into(),
                }),
                cells: vec![PresentationCell {
                    search_text: "tag".into(),
                    display_text: "tag".into(),
                    role: Some(crate::presentation::PresentationRole::Tag),
                }],
            }],
        );
        let analysis = analyze_payload_response(WORKLOADS[1], &response)
            .expect("payload analysis should succeed");

        assert_eq!(
            analysis.compact_json_bytes,
            analysis.sections.response_metadata_bytes
                + analysis.sections.results_bytes
                + analysis.sections.rows_bytes
        );
        assert_eq!(
            analysis.sections.rows_bytes,
            analysis.sections.row_metadata_bytes
                + analysis.sections.row_context_bytes
                + analysis.sections.cells_bytes
        );
        assert_eq!(
            analysis.sections.cells_bytes,
            analysis.sections.cell_structure_bytes
                + analysis.sections.search_text_bytes
                + analysis.sections.display_text_bytes
                + analysis.sections.semantic_role_bytes
        );
        assert_eq!(
            analysis.field_names.total_bytes,
            analysis.field_names.response_metadata_bytes
                + analysis.field_names.results_bytes
                + analysis.field_names.row_metadata_bytes
                + analysis.field_names.row_context_bytes
                + analysis.field_names.cells_bytes
        );
        assert_eq!(analysis.repetition.search_display_equal_cells, 1);
        assert_eq!(analysis.repetition.row_context_values_repeated_in_cells, 1);
        assert_eq!(analysis.repetition.cells_matching_result_scalars, 1);
    }
}
