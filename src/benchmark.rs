//! Reproducible, measurement-only SQLite baseline support.

use std::{
    fs,
    fs::File,
    path::{Path, PathBuf},
    time::{Duration, Instant, SystemTime},
};

use rusqlite::{Connection, OptionalExtension};
use serde::Serialize;
use sha2::{Digest, Sha256};

use crate::{
    cli::{
        production_search_result_count_with_connection,
        production_search_stable_results_with_connection, CliSearchScope,
    },
    config::{Config, SearchConfig},
    db::{
        open_database_with_schema, open_existing_database_read_only, SchemaDefinition,
        CURRENT_SCHEMA_VERSION, DB_METADATA_FTS_AVAILABLE_KEY, DB_METADATA_FTS_BODY_INDEXED_KEY,
        DB_METADATA_FTS_SCHEMA_VERSION_KEY, FTS_SCHEMA_CONTRACT_VERSION,
    },
    indexer::Indexer,
    parser::OrgizeAdapter,
    query::{
        compile_sqlite_query, execute_and_shape_query, parse_query,
        sqlite_query_validation_options, validate_query, QueryExecutionOptions, QueryOutputMode,
    },
};

pub const CORPUS_CONTRACT_VERSION: &str = "1";
pub const OUTPUT_SCHEMA_VERSION: &str = "2";
pub const DEFAULT_FILES: usize = 1_000;
pub const DEFAULT_WARMUPS: usize = 5;
pub const DEFAULT_ITERATIONS: usize = 30;
const TITLE_SEARCH_EXPRESSION: &str = "Project";
const BODY_SEARCH_EXPRESSION: &str = "token";

#[derive(Debug, Clone, Copy)]
pub struct BenchmarkOptions {
    pub files: usize,
    pub seed: u64,
    pub warmups: usize,
    pub iterations: usize,
}

impl Default for BenchmarkOptions {
    fn default() -> Self {
        Self {
            files: DEFAULT_FILES,
            seed: 1,
            warmups: DEFAULT_WARMUPS,
            iterations: DEFAULT_ITERATIONS,
        }
    }
}

#[derive(Debug, Serialize)]
pub struct BenchmarkOutput {
    pub output_schema_version: &'static str,
    pub corpus_contract_version: &'static str,
    pub manifest: CorpusManifest,
    pub protocol: Protocol,
    pub environment: Environment,
    pub variants: Vec<VariantResult>,
}

#[derive(Debug, Serialize)]
pub struct Environment {
    pub command_arguments: Vec<String>,
    pub build_profile: &'static str,
    pub operating_system: &'static str,
    pub architecture: &'static str,
    pub available_cpus: Option<usize>,
    pub available_memory_bytes: Option<u64>,
    pub sqlite_version: String,
    pub sqlite_compile_options: Vec<String>,
    pub pragmas: ConnectionPragmas,
    pub statistics_tables: Vec<String>,
    pub storage_notes: &'static str,
}

#[derive(Debug, Serialize)]
pub struct ConnectionPragmas {
    pub cache_size: i64,
    pub mmap_size: i64,
    pub journal_mode: String,
    pub transaction_state: &'static str,
}

#[derive(Debug, Serialize)]
pub struct CorpusManifest {
    pub files: usize,
    pub seed: u64,
    pub expected_headings: usize,
    pub expected_links: usize,
    pub fingerprint: String,
    pub generation_duration_ns: u128,
}

#[derive(Debug, Serialize)]
pub struct Protocol {
    pub warmups: usize,
    pub iterations: usize,
    pub ordering: &'static str,
    pub connection: &'static str,
}

#[derive(Debug, Serialize)]
pub struct VariantResult {
    pub id: &'static str,
    pub profile_id: &'static str,
    pub profile_kind: &'static str,
    pub explicit_indexes: Vec<String>,
    pub index_inventory: Vec<IndexInventoryEntry>,
    pub dbstat: DbstatAvailability,
    pub fts5_available: bool,
    pub fts_workloads: Vec<SkippedWorkload>,
    pub search_workloads: Vec<SearchWorkloadResult>,
    pub workloads: Vec<WorkloadResult>,
    pub database: DatabaseSize,
    pub incremental: IncrementalSequence,
}

#[derive(Debug, Serialize)]
pub struct IncrementalSequence {
    pub id: &'static str,
    pub before: FileSizes,
    pub after: FileSizes,
    pub total_planning_ns: u128,
    pub total_application_ns: u128,
    pub operations: Vec<IncrementalOperation>,
    pub semantic_equivalent: Option<bool>,
    pub semantic_status: &'static str,
    pub equivalence_error: Option<String>,
}
#[derive(Debug, Serialize)]
pub struct IncrementalOperation {
    pub id: &'static str,
    pub classifications: ChangeCounts,
    pub planning_ns: u128,
    pub application_ns: u128,
    pub before: FileSizes,
    pub after: FileSizes,
}
#[derive(Debug, Serialize)]
pub struct ChangeCounts {
    pub unchanged: usize,
    pub metadata_only: usize,
    pub created: usize,
    pub modified: usize,
    pub deleted: usize,
    pub failed: usize,
}
#[derive(Debug, Serialize)]
pub struct FileSizes {
    pub database_bytes: u64,
    pub wal_bytes: u64,
}

#[derive(Debug, Serialize)]
pub struct IndexInventoryEntry {
    pub table: String,
    pub name: Option<String>,
    pub classification: &'static str,
    pub unique: bool,
    pub partial: bool,
    pub columns: Vec<IndexColumn>,
    pub sql: Option<String>,
    pub dbstat: Option<IndexDbstat>,
}

#[derive(Debug, Serialize)]
pub struct IndexDbstat {
    pub pages: i64,
    pub total_bytes: i64,
    pub payload_bytes: i64,
}

#[derive(Debug, Serialize)]
pub struct IndexColumn {
    pub sequence: i64,
    pub column: Option<String>,
    pub expression: bool,
}

#[derive(Debug, Serialize)]
pub struct DbstatAvailability {
    pub available: bool,
    pub reason: Option<String>,
}

#[derive(Debug, Clone, Copy)]
struct IndexProfile {
    id: &'static str,
    kind: &'static str,
    sql: &'static str,
}

const INDEX_PROFILES: &[IndexProfile] = &[
    IndexProfile { id: "baseline-v8", kind: "baseline", sql: "" },
    IndexProfile { id: "candidate-add-links-resolution-status", kind: "individual-candidate", sql: "CREATE INDEX idx_benchmark_links_resolution_status ON links(resolution_status);" },
    IndexProfile { id: "candidate-add-files-path-lower", kind: "individual-candidate", sql: "CREATE INDEX idx_benchmark_files_path_lower ON files(LOWER(path));" },
    IndexProfile { id: "candidate-add-headings-title-lower", kind: "individual-candidate", sql: "CREATE INDEX idx_benchmark_headings_title_lower ON headings(LOWER(title));" },
    IndexProfile { id: "candidate-remove-tags-heading", kind: "individual-candidate", sql: "DROP INDEX IF EXISTS idx_tags_heading;" },
    IndexProfile { id: "candidate-remove-keywords-heading", kind: "individual-candidate", sql: "DROP INDEX IF EXISTS idx_keywords_heading;" },
    IndexProfile { id: "candidate-remove-repeaters-timestamp", kind: "individual-candidate", sql: "DROP INDEX IF EXISTS idx_timestamp_repeaters_timestamp_id;" },
    IndexProfile { id: "combined-candidates", kind: "combined-candidate", sql: "CREATE INDEX idx_benchmark_links_resolution_status ON links(resolution_status); CREATE INDEX idx_benchmark_files_path_lower ON files(LOWER(path)); CREATE INDEX idx_benchmark_headings_title_lower ON headings(LOWER(title)); DROP INDEX IF EXISTS idx_tags_heading; DROP INDEX IF EXISTS idx_keywords_heading; DROP INDEX IF EXISTS idx_timestamp_repeaters_timestamp_id;" },
];

#[derive(Debug, Serialize)]
pub struct SearchWorkloadResult {
    pub id: &'static str,
    pub production_path: &'static str,
    pub expression: &'static str,
    pub result_count: usize,
    pub cold_connection_end_to_end_ns: u128,
    pub warmed: Timing,
}

#[derive(Debug, Serialize)]
pub struct SkippedWorkload {
    pub id: &'static str,
    pub reason: String,
}

#[derive(Debug, Serialize)]
pub struct WorkloadResult {
    pub id: &'static str,
    pub production_path: &'static str,
    pub expression: String,
    pub sql: String,
    pub parameters: Vec<Parameter>,
    pub result_count: usize,
    pub output_mode: QueryOutputMode,
    pub cold_connection_end_to_end_ns: u128,
    pub parse_validate_compile: Timing,
    pub validated_query_end_to_end: Timing,
    pub explain_query_plan: Vec<String>,
}

#[derive(Debug, Serialize)]
#[serde(tag = "kind", content = "value", rename_all = "snake_case")]
pub enum Parameter {
    Integer(i64),
    Text(String),
}

#[derive(Debug, Serialize)]
pub struct Timing {
    pub samples: usize,
    pub min_ns: u128,
    pub median_ns: u128,
    pub max_ns: u128,
    pub p95_ns: u128,
}

#[derive(Debug, Serialize)]
pub struct DatabaseSize {
    pub main_before_checkpoint_bytes: u64,
    pub main_after_checkpoint_bytes: u64,
    pub wal_before_checkpoint_bytes: u64,
    pub wal_after_checkpoint_bytes: u64,
    pub page_count: i64,
    pub page_size: i64,
    pub freelist_count: i64,
    pub journal_mode: String,
    pub checkpoint: &'static str,
    pub rebuild_duration_ns: u128,
}

pub fn run(output: &Path, work_dir: &Path, options: BenchmarkOptions) -> Result<(), String> {
    if options.files < 3 || options.iterations == 0 {
        return Err("--files must be at least 3 and --iterations must be positive".into());
    }
    if work_dir.exists() {
        return Err(format!(
            "benchmark work directory must not already exist: {}",
            work_dir.display()
        ));
    }
    if output.exists() {
        return Err(format!(
            "benchmark output must not already exist: {}",
            output.display()
        ));
    }
    fs::create_dir_all(work_dir).map_err(|error| error.to_string())?;
    let source_dir = work_dir.join("corpus");
    let corpus_start = Instant::now();
    let mut manifest = generate_corpus(&source_dir, options.files, options.seed)?;
    manifest.generation_duration_ns = corpus_start.elapsed().as_nanos();
    let mut variants = Vec::new();
    for (id, search) in [
        (
            "no-fts",
            SearchConfig {
                fts5_enabled: false,
                index_body_text: false,
            },
        ),
        (
            "fts-title",
            SearchConfig {
                fts5_enabled: true,
                index_body_text: false,
            },
        ),
        (
            "fts-body",
            SearchConfig {
                fts5_enabled: true,
                index_body_text: true,
            },
        ),
    ] {
        for profile in INDEX_PROFILES {
            variants.push(run_variant(
                work_dir,
                &source_dir,
                id,
                *profile,
                search.clone(),
                options,
            )?);
        }
    }
    ensure_comparable_profiles(&variants)?;
    let result = BenchmarkOutput {
        output_schema_version: OUTPUT_SCHEMA_VERSION,
        corpus_contract_version: CORPUS_CONTRACT_VERSION,
        manifest,
        protocol: Protocol {
            warmups: options.warmups,
            iterations: options.iterations,
            ordering: "stable workload-id order; isolated groups",
            connection: "cold connection followed by one warmed reused connection",
        },
        environment: environment()?,
        variants,
    };
    fs::write(
        output,
        serde_json::to_vec_pretty(&result).map_err(|error| error.to_string())?,
    )
    .map_err(|error| error.to_string())
}

fn ensure_comparable_profiles(variants: &[VariantResult]) -> Result<(), String> {
    for baseline in variants
        .iter()
        .filter(|variant| variant.profile_id == "baseline-v8")
    {
        for candidate in variants.iter().filter(|variant| variant.id == baseline.id) {
            if candidate.workloads.len() != baseline.workloads.len()
                || candidate.search_workloads.len() != baseline.search_workloads.len()
            {
                return Err(format!(
                    "incomparable profile {} for {}",
                    candidate.profile_id, candidate.id
                ));
            }
            for (expected, actual) in baseline.workloads.iter().zip(&candidate.workloads) {
                if expected.id != actual.id || expected.result_count != actual.result_count {
                    return Err(format!(
                        "result mismatch for {} in {}",
                        actual.id, candidate.profile_id
                    ));
                }
            }
            for (expected, actual) in baseline
                .search_workloads
                .iter()
                .zip(&candidate.search_workloads)
            {
                if expected.id != actual.id || expected.result_count != actual.result_count {
                    return Err(format!(
                        "search result mismatch for {} in {}",
                        actual.id, candidate.profile_id
                    ));
                }
            }
        }
    }
    Ok(())
}

pub fn generate_corpus(
    directory: &Path,
    files: usize,
    seed: u64,
) -> Result<CorpusManifest, String> {
    if files == 0 {
        return Err("corpus file count must be positive".into());
    }
    if directory.exists() {
        let mut entries = fs::read_dir(directory).map_err(|error| error.to_string())?;
        if entries.next().is_some() {
            return Err(format!(
                "corpus directory must be empty: {}",
                directory.display()
            ));
        }
    } else {
        fs::create_dir_all(directory).map_err(|error| error.to_string())?;
    }
    let mut digest = Sha256::new();
    for index in 0..files {
        let todo = if (index as u64 + seed).is_multiple_of(3) {
            "TODO"
        } else {
            "DONE"
        };
        let deep = if index % 10 == 0 {
            "*** Deep A\n**** Deep B\n***** Deep C\n"
        } else {
            ""
        };
        let broken = if index % 11 == 0 {
            "[[file:missing-target.org]]\n"
        } else {
            ""
        };
        let sibling_count = if index % 8 == 0 { 12 } else { 1 };
        let wide = (0..sibling_count)
            .map(|sibling| format!("** Wide sibling {index}-{sibling}\n"))
            .collect::<String>();
        let content = format!("#+TITLE: Benchmark {index}\n* {todo} Project {index} :project:tag{}:common:\nSCHEDULED: <2025-01-{:02} Wed +1w> DEADLINE: <2025-02-{:02} Thu> CLOSED: [2025-03-{:02} Fri]\n:PROPERTIES:\n:GROUP: group{}\n:OWNER: owner{}\n:END:\nBody benchmark token {} {}\n** Child {index}\n<2025-04-01 Tue> [2025-04-02 Wed]\n[[file:note-{index:05}.org]]\n{}{}{}", index % 7, index % 28 + 1, index % 28 + 1, index % 28 + 1, index % 10, index % 5, index % 17, "x".repeat(index % 32), broken, wide, deep);
        let group = if index.is_multiple_of(2) {
            "projects/alpha"
        } else {
            "archive/beta"
        };
        let relative = format!("{group}/note-{index:05}.org");
        digest.update(b"orgfdb-benchmark-path-v1\0");
        digest.update((relative.len() as u64).to_be_bytes());
        digest.update(relative.as_bytes());
        digest.update((content.len() as u64).to_be_bytes());
        digest.update(content.as_bytes());
        let path = directory.join(&relative);
        fs::create_dir_all(path.parent().expect("relative file has a parent"))
            .map_err(|error| error.to_string())?;
        fs::write(path, content).map_err(|error| error.to_string())?;
    }
    Ok(CorpusManifest {
        files,
        seed,
        expected_headings: files * 4 + files.div_ceil(8) * 11 + files.div_ceil(10) * 3,
        expected_links: files + files.div_ceil(11),
        fingerprint: format!("sha256:{:x}", digest.finalize()),
        generation_duration_ns: 0,
    })
}

fn run_variant(
    work_dir: &Path,
    source_dir: &Path,
    id: &'static str,
    profile: IndexProfile,
    search: SearchConfig,
    options: BenchmarkOptions,
) -> Result<VariantResult, String> {
    let db_path = work_dir.join(format!("{id}-{}.sqlite", profile.id));
    let variant_source = work_dir.join(format!("corpus-{id}-{}", profile.id));
    copy_directory(source_dir, &variant_source)?;
    let config = Config {
        db_path: db_path.clone(),
        files: Vec::new(),
        dirs: vec![crate::config::ConfiguredDir {
            path: variant_source.clone(),
            recursive: true,
            exclude: Vec::new(),
        }],
        discovery: Default::default(),
        links: Default::default(),
        todo: Default::default(),
        search: search.clone(),
        query: Default::default(),
    };
    let capability_connection = Connection::open(&db_path).map_err(|error| error.to_string())?;
    let fts5_available = crate::db::sqlite_supports_fts5(&capability_connection)
        .map_err(|error| error.to_string())?;
    drop(capability_connection);
    if search.fts5_enabled && !fts5_available {
        let connection = Connection::open(&db_path).map_err(|error| error.to_string())?;
        return Ok(VariantResult {
            id,
            profile_id: profile.id,
            profile_kind: profile.kind,
            explicit_indexes: explicit_indexes(&connection)?,
            index_inventory: index_inventory(&connection)?,
            dbstat: dbstat_availability(&connection),
            fts5_available,
            fts_workloads: vec![
                SkippedWorkload {
                    id: "search.title.project",
                    reason: "SQLite FTS5 unavailable".into(),
                },
                SkippedWorkload {
                    id: "search.body.token",
                    reason: "SQLite FTS5 unavailable".into(),
                },
            ],
            workloads: Vec::new(),
            search_workloads: Vec::new(),
            database: database_size(&connection, &db_path, 0)?,
            incremental: IncrementalSequence {
                id: "incremental-v1",
                before: FileSizes {
                    database_bytes: 0,
                    wal_bytes: 0,
                },
                after: FileSizes {
                    database_bytes: 0,
                    wal_bytes: 0,
                },
                total_planning_ns: 0,
                total_application_ns: 0,
                operations: Vec::new(),
                semantic_equivalent: None,
                semantic_status: "skipped",
                equivalence_error: Some("SQLite FTS5 unavailable; profile skipped".into()),
            },
        });
    }
    let mut connection = open_database_with_schema(
        &db_path,
        &SchemaDefinition::new(CURRENT_SCHEMA_VERSION, search.fts5_enabled),
    )
    .map_err(|error| error.to_string())?;
    connection
        .execute_batch(profile.sql)
        .map_err(|error| error.to_string())?;
    let rebuild_start = Instant::now();
    Indexer::new(OrgizeAdapter::new())
        .rebuild(&mut connection, &config)
        .map_err(|error| error.to_string())?;
    let rebuild_duration_ns = rebuild_start.elapsed().as_nanos();
    let database = database_size(&connection, &db_path, rebuild_duration_ns)?;
    let explicit_indexes = explicit_indexes(&connection)?;
    let index_inventory = index_inventory(&connection)?;
    let dbstat = dbstat_availability(&connection);
    drop(connection);
    let workloads = query_workloads(&variant_source);
    let mut results = Vec::new();
    for workload in workloads {
        let workload_id = workload.id;
        results.push(
            measure_query(&db_path, &workload, options)
                .map_err(|error| format!("{workload_id}: {error}"))?,
        );
    }
    let (fts_workloads, search_workloads) = if search.fts5_enabled && search.index_body_text {
        (
            vec![],
            vec![
                measure_search(
                    &db_path,
                    &config,
                    "search.title.project",
                    CliSearchScope::Title,
                    TITLE_SEARCH_EXPRESSION,
                    options,
                )
                .map_err(|error| format!("search.title.project: {error}"))?,
                measure_search(
                    &db_path,
                    &config,
                    "search.body.token",
                    CliSearchScope::Body,
                    BODY_SEARCH_EXPRESSION,
                    options,
                )
                .map_err(|error| format!("search.body.token: {error}"))?,
            ],
        )
    } else if search.fts5_enabled {
        (
            vec![SkippedWorkload {
                id: "search.body.token",
                reason: "body indexing disabled for this variant".into(),
            }],
            vec![measure_search(
                &db_path,
                &config,
                "search.title.project",
                CliSearchScope::Title,
                "Project",
                options,
            )
            .map_err(|error| format!("search.title.project: {error}"))?],
        )
    } else {
        (
            vec![
                SkippedWorkload {
                    id: "search.title.project",
                    reason: "FTS disabled for this variant".into(),
                },
                SkippedWorkload {
                    id: "search.body.token",
                    reason: "FTS disabled for this variant".into(),
                },
            ],
            Vec::new(),
        )
    };
    let mut incremental_connection = open_database_with_schema(
        &db_path,
        &SchemaDefinition::new(CURRENT_SCHEMA_VERSION, search.fts5_enabled),
    )
    .map_err(|error| error.to_string())?;
    let incremental = measure_incremental_sequence(
        &mut incremental_connection,
        &config,
        &variant_source,
        &db_path,
        profile,
    )?;
    Ok(VariantResult {
        id,
        profile_id: profile.id,
        profile_kind: profile.kind,
        explicit_indexes,
        index_inventory,
        dbstat,
        fts5_available,
        fts_workloads,
        workloads: results,
        search_workloads,
        database,
        incremental,
    })
}

fn measure_search(
    db_path: &Path,
    config: &Config,
    id: &'static str,
    scope: CliSearchScope,
    expression: &'static str,
    options: BenchmarkOptions,
) -> Result<SearchWorkloadResult, String> {
    let cold = Instant::now();
    let cold_connection =
        open_existing_database_read_only(db_path).map_err(|error| error.to_string())?;
    let result_count = production_search_result_count_with_connection(
        &cold_connection,
        scope,
        expression,
        config,
    )?;
    let cold_connection_end_to_end_ns = cold.elapsed().as_nanos();
    drop(cold_connection);
    let warmed_connection =
        open_existing_database_read_only(db_path).map_err(|error| error.to_string())?;
    for _ in 0..options.warmups {
        let _ = production_search_result_count_with_connection(
            &warmed_connection,
            scope,
            expression,
            config,
        )?;
    }
    let mut samples = Vec::with_capacity(options.iterations);
    for _ in 0..options.iterations {
        let start = Instant::now();
        let rows = production_search_result_count_with_connection(
            &warmed_connection,
            scope,
            expression,
            config,
        )?;
        std::hint::black_box(rows);
        samples.push(start.elapsed());
    }
    samples.sort();
    Ok(SearchWorkloadResult {
        id,
        production_path: "production-search",
        expression,
        result_count,
        cold_connection_end_to_end_ns,
        warmed: timing(&samples),
    })
}

fn measure_incremental_sequence(
    connection: &mut Connection,
    config: &Config,
    source_dir: &Path,
    db_path: &Path,
    profile: IndexProfile,
) -> Result<IncrementalSequence, String> {
    let sequence_before = file_sizes(db_path)?;
    let mut operations = Vec::new();
    let metadata = source_dir.join("projects/alpha/note-00000.org");
    rewrite_metadata_only(&metadata)?;
    operations.push(apply_incremental(
        connection,
        config,
        db_path,
        "metadata-only",
    )?);

    let modified = source_dir.join("archive/beta/note-00001.org");
    fs::write(
        &modified,
        format!(
            "{}\nIncremental replacement\n",
            fs::read_to_string(&modified).map_err(|error| error.to_string())?
        ),
    )
    .map_err(|error| error.to_string())?;
    operations.push(apply_incremental(
        connection,
        config,
        db_path,
        "modified-replacement",
    )?);

    let created = source_dir.join("projects/alpha/incremental-created.org");
    fs::write(&created, "#+TITLE: Incremental Created\n* TODO Created\n")
        .map_err(|error| error.to_string())?;
    operations.push(apply_incremental(
        connection,
        config,
        db_path,
        "created-file",
    )?);

    let deleted = source_dir.join("projects/alpha/note-00002.org");
    fs::remove_file(&deleted).map_err(|error| error.to_string())?;
    operations.push(apply_incremental(
        connection,
        config,
        db_path,
        "deleted-file",
    )?);

    let mixed = source_dir.join("archive/beta/incremental-mixed.org");
    fs::write(&mixed, "* TODO Mixed\n").map_err(|error| error.to_string())?;
    let mixed_modified = source_dir.join("projects/alpha/note-00000.org");
    fs::write(
        &mixed_modified,
        format!(
            "{}\nMixed replacement\n",
            fs::read_to_string(&mixed_modified).map_err(|error| error.to_string())?
        ),
    )
    .map_err(|error| error.to_string())?;
    fs::remove_file(&created).map_err(|error| error.to_string())?;
    operations.push(apply_incremental(
        connection,
        config,
        db_path,
        "mixed-change-set",
    )?);
    let total_planning_ns = operations
        .iter()
        .map(|operation| operation.planning_ns)
        .sum();
    let total_application_ns = operations
        .iter()
        .map(|operation| operation.application_ns)
        .sum();
    let reference_path = db_path.with_extension("reference.sqlite");
    let mut reference_config = config.clone();
    reference_config.db_path = reference_path.clone();
    let mut reference = open_database_with_schema(
        &reference_path,
        &SchemaDefinition::new(CURRENT_SCHEMA_VERSION, config.search.fts5_enabled),
    )
    .map_err(|error| error.to_string())?;
    reference
        .execute_batch(profile.sql)
        .map_err(|error| error.to_string())?;
    Indexer::new(OrgizeAdapter::new())
        .rebuild(&mut reference, &reference_config)
        .map_err(|error| error.to_string())?;
    let incremental_snapshot = semantic_snapshot(connection)?;
    let reference_snapshot = semantic_snapshot(&reference)?;
    let equivalent = incremental_snapshot == reference_snapshot;
    if !equivalent {
        let tables = incremental_snapshot
            .iter()
            .zip(&reference_snapshot)
            .filter_map(|(left, right)| (left != right).then_some(left.0.as_str()))
            .collect::<Vec<_>>()
            .join(", ");
        return Err(format!(
            "incremental database differs from clean production rebuild: {tables}"
        ));
    }
    verify_fts_equivalence(connection, &reference, config)?;
    Ok(IncrementalSequence {
        id: "incremental-v1",
        before: sequence_before,
        after: file_sizes(db_path)?,
        total_planning_ns,
        total_application_ns,
        semantic_equivalent: Some(true),
        semantic_status: "completed",
        equivalence_error: None,
        operations,
    })
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct FtsMetadataState {
    available: bool,
    body_indexed: bool,
    schema_version: String,
}

fn required_metadata_value(connection: &Connection, key: &str) -> Result<String, String> {
    connection
        .query_row(
            "SELECT value FROM db_metadata WHERE key = ?1",
            [key],
            |row| row.get(0),
        )
        .optional()
        .map_err(|error| error.to_string())?
        .ok_or_else(|| format!("missing required metadata key {key}"))
}

fn required_metadata_flag(connection: &Connection, key: &str) -> Result<bool, String> {
    match required_metadata_value(connection, key)?.as_str() {
        "0" => Ok(false),
        "1" => Ok(true),
        value => Err(format!("invalid metadata flag {key}={value}")),
    }
}

fn fts_metadata_state(connection: &Connection) -> Result<FtsMetadataState, String> {
    Ok(FtsMetadataState {
        available: required_metadata_flag(connection, DB_METADATA_FTS_AVAILABLE_KEY)?,
        body_indexed: required_metadata_flag(connection, DB_METADATA_FTS_BODY_INDEXED_KEY)?,
        schema_version: required_metadata_value(connection, DB_METADATA_FTS_SCHEMA_VERSION_KEY)?,
    })
}

fn verify_fts_row_coverage(connection: &Connection) -> Result<(), String> {
    let load_ids = |sql: &str| -> Result<Vec<i64>, String> {
        let mut statement = connection.prepare(sql).map_err(|error| error.to_string())?;
        let ids = statement
            .query_map([], |row| row.get(0))
            .map_err(|error| error.to_string())?
            .collect::<Result<Vec<i64>, _>>()
            .map_err(|error| error.to_string())?;
        Ok(ids)
    };
    let heading_ids = load_ids("SELECT id FROM headings ORDER BY id")?;
    let fts_row_ids = load_ids("SELECT rowid FROM heading_fts ORDER BY rowid")?;
    if fts_row_ids != heading_ids {
        return Err("trusted FTS row IDs do not exactly match canonical heading IDs".into());
    }
    Ok(())
}

fn verify_production_search_equivalence(
    left: &Connection,
    right: &Connection,
    config: &Config,
    scope: CliSearchScope,
    expression: &str,
) -> Result<(), String> {
    let left_results =
        production_search_stable_results_with_connection(left, scope, expression, config)?;
    let right_results =
        production_search_stable_results_with_connection(right, scope, expression, config)?;
    if left_results != right_results {
        return Err(format!(
            "production-search results differ for scope {scope:?} and expression {expression:?}"
        ));
    }
    Ok(())
}

fn verify_fts_equivalence(
    left: &Connection,
    right: &Connection,
    config: &Config,
) -> Result<(), String> {
    let left_state = fts_metadata_state(left)?;
    let right_state = fts_metadata_state(right)?;
    if left_state != right_state {
        return Err(format!(
            "FTS trust metadata differs: left={left_state:?} right={right_state:?}"
        ));
    }

    let expected_available = config.search.fts5_enabled;
    let expected_body_indexed = expected_available && config.search.index_body_text;
    let expected_schema_version = if expected_available {
        FTS_SCHEMA_CONTRACT_VERSION
    } else {
        "0"
    };
    let expected_state = FtsMetadataState {
        available: expected_available,
        body_indexed: expected_body_indexed,
        schema_version: expected_schema_version.to_string(),
    };
    if left_state != expected_state {
        return Err(format!(
            "FTS trust metadata does not match configured policy: actual={left_state:?} expected={expected_state:?}"
        ));
    }
    if !left_state.available {
        return Ok(());
    }

    verify_fts_row_coverage(left)?;
    verify_fts_row_coverage(right)?;
    verify_production_search_equivalence(
        left,
        right,
        config,
        CliSearchScope::Title,
        TITLE_SEARCH_EXPRESSION,
    )?;
    if expected_body_indexed {
        verify_production_search_equivalence(
            left,
            right,
            config,
            CliSearchScope::Body,
            BODY_SEARCH_EXPRESSION,
        )?;
    }
    Ok(())
}

type SemanticSnapshot = Vec<(String, Vec<String>)>;

fn semantic_snapshot(connection: &Connection) -> Result<SemanticSnapshot, String> {
    let specs = [
        (
            "files",
            r#"SELECT hex(identity) || '|' || quote(path) || '|' || mtime_ns || '|' || size || '|' || quote(content_hash)
               FROM files
               ORDER BY 1"#,
        ),
        (
            "headings",
            r#"SELECT hex(file.identity) || '|' || heading.byte_start || '|' ||
                      COALESCE(hex(parent_file.identity) || ':' || parent.byte_start, '') || '|' ||
                      heading.level || '|' || quote(heading.title) || '|' || quote(heading.title_raw) || '|' ||
                      quote(heading.todo_keyword) || '|' || quote(heading.todo_type) || '|' ||
                      quote(heading.priority) || '|' || quote(heading.all_tags_json)
               FROM headings AS heading
               INNER JOIN files AS file ON file.id = heading.file_id
               LEFT JOIN headings AS parent ON parent.id = heading.parent_id
               LEFT JOIN files AS parent_file ON parent_file.id = parent.file_id
               ORDER BY 1"#,
        ),
        (
            "todo_keywords",
            r#"SELECT hex(file.identity) || '|' || quote(todo.keyword) || '|' || quote(todo.state_type) || '|' ||
                      todo.sequence_no || '|' || quote(todo.source_kind)
               FROM todo_keywords AS todo
               INNER JOIN files AS file ON file.id = todo.file_id
               ORDER BY 1"#,
        ),
        (
            "keywords",
            r#"SELECT hex(file.identity) || '|' || heading.byte_start || '|' || quote(keyword.keyword) || '|' ||
                      quote(keyword.value) || '|' || quote(keyword.line_number)
               FROM keywords AS keyword
               INNER JOIN headings AS heading ON heading.id = keyword.heading_id
               INNER JOIN files AS file ON file.id = heading.file_id
               ORDER BY 1"#,
        ),
        (
            "properties",
            r#"SELECT hex(file.identity) || '|' || heading.byte_start || '|' || quote(property.key) || '|' ||
                      quote(property.value) || '|' || quote(property.source) || '|' || property.append || '|' ||
                      quote(property.line_number)
               FROM properties AS property
               INNER JOIN headings AS heading ON heading.id = property.heading_id
               INNER JOIN files AS file ON file.id = heading.file_id
               ORDER BY 1"#,
        ),
        (
            "tags",
            r#"SELECT hex(file.identity) || '|' || heading.byte_start || '|' || quote(tag.tag)
               FROM tags AS tag
               INNER JOIN headings AS heading ON heading.id = tag.heading_id
               INNER JOIN files AS file ON file.id = heading.file_id
               ORDER BY 1"#,
        ),
        (
            "timestamps",
            r#"SELECT hex(file.identity) || '|' || heading.byte_start || '|' || quote(timestamp.role) || '|' ||
                      quote(timestamp.start_ts) || '|' || quote(timestamp.end_ts) || '|' ||
                      quote(timestamp.raw_value) || '|' || timestamp.byte_start
               FROM timestamps AS timestamp
               INNER JOIN headings AS heading ON heading.id = timestamp.heading_id
               INNER JOIN files AS file ON file.id = heading.file_id
               ORDER BY 1"#,
        ),
        (
            "timestamp_repeaters",
            r#"SELECT hex(file.identity) || '|' || heading.byte_start || '|' || timestamp.byte_start || '|' ||
                      quote(repeater.repeater_type) || '|' || quote(repeater.repeater_value) || '|' ||
                      quote(repeater.repeater_unit)
               FROM timestamp_repeaters AS repeater
               INNER JOIN timestamps AS timestamp ON timestamp.id = repeater.timestamp_id
               INNER JOIN headings AS heading ON heading.id = timestamp.heading_id
               INNER JOIN files AS file ON file.id = heading.file_id
               ORDER BY 1"#,
        ),
        (
            "heading_bodies",
            r#"SELECT hex(file.identity) || '|' || heading.byte_start || '|' || quote(body.body_text) || '|' ||
                      quote(body.body_byte_start) || '|' || quote(body.body_byte_end)
               FROM heading_bodies AS body
               INNER JOIN headings AS heading ON heading.id = body.heading_id
               INNER JOIN files AS file ON file.id = heading.file_id
               ORDER BY 1"#,
        ),
        (
            "outline",
            r#"SELECT hex(heading_file.identity) || '|' || heading.byte_start || '|' ||
                      hex(outline_file.identity) || '|' ||
                      COALESCE(hex(parent_file.identity) || ':' || parent.byte_start, '') || '|' ||
                      quote(outline.materialized_path) || '|' || quote(outline.breadcrumbs_json)
               FROM outline_path AS outline
               INNER JOIN headings AS heading ON heading.id = outline.heading_id
               INNER JOIN files AS heading_file ON heading_file.id = heading.file_id
               INNER JOIN files AS outline_file ON outline_file.id = outline.file_id
               LEFT JOIN headings AS parent ON parent.id = outline.parent_id
               LEFT JOIN files AS parent_file ON parent_file.id = parent.file_id
               ORDER BY 1"#,
        ),
        (
            "links",
            r#"SELECT hex(source_heading_file.identity) || '|' || source_heading.byte_start || '|' ||
                      hex(link_file.identity) || '|' || link.byte_start || '|' || quote(link.raw_target) || '|' ||
                      quote(link.path) || '|' || quote(link.search_option) || '|' || quote(link.resolution_status) || '|' ||
                      quote(link.resolution_diagnostic) || '|' || COALESCE(hex(target_file.identity), '') || '|' ||
                      COALESCE(hex(target_heading_file.identity) || ':' || target_heading.byte_start, '') || '|' ||
                      quote(link.target_custom_id) || '|' || quote(link.target_id)
               FROM links AS link
               INNER JOIN headings AS source_heading ON source_heading.id = link.heading_id
               INNER JOIN files AS source_heading_file ON source_heading_file.id = source_heading.file_id
               INNER JOIN files AS link_file ON link_file.id = link.file_id
               LEFT JOIN files AS target_file ON target_file.id = link.target_file_id
               LEFT JOIN headings AS target_heading ON target_heading.id = link.target_heading_id
               LEFT JOIN files AS target_heading_file ON target_heading_file.id = target_heading.file_id
               ORDER BY 1"#,
        ),
        (
            "metadata",
            "SELECT quote(key) || '|' || quote(value) FROM db_metadata ORDER BY 1",
        ),
    ];
    specs
        .into_iter()
        .map(|(name, sql)| {
            let mut statement = connection.prepare(sql).map_err(|error| error.to_string())?;
            let rows = statement
                .query_map([], |row| row.get(0))
                .map_err(|error| error.to_string())?
                .collect::<Result<Vec<String>, _>>()
                .map_err(|error| error.to_string())?;
            Ok((name.to_string(), rows))
        })
        .collect()
}

fn rewrite_metadata_only(path: &Path) -> Result<(), String> {
    let bytes = fs::read(path).map_err(|error| error.to_string())?;
    let before = fs::metadata(path).map_err(|error| error.to_string())?;
    fs::write(path, &bytes).map_err(|error| error.to_string())?;
    File::open(path)
        .map_err(|error| error.to_string())?
        .set_times(fs::FileTimes::new().set_modified(SystemTime::now() + Duration::from_secs(1)))
        .map_err(|error| {
            format!(
                "set metadata-only modification time for {}: {error}",
                path.display()
            )
        })?;
    let after = fs::metadata(path).map_err(|error| error.to_string())?;
    if fs::read(path).map_err(|error| error.to_string())? != bytes {
        return Err("metadata-only rewrite changed bytes".into());
    }
    if before.modified().ok() == after.modified().ok() && before.len() == after.len() {
        return Err(format!(
            "metadata-only rewrite did not change a fast snapshot signal for {}",
            path.display()
        ));
    }
    Ok(())
}

fn apply_incremental(
    connection: &mut Connection,
    config: &Config,
    db_path: &Path,
    id: &'static str,
) -> Result<IncrementalOperation, String> {
    let before = file_sizes(db_path)?;
    let indexer = Indexer::new(OrgizeAdapter::new());
    let planning = Instant::now();
    let planned = indexer
        .plan_changes(connection, config)
        .map_err(|error| format!("{error:?}"))?;
    let classifications = validate_plan(&planned, id)?;
    let actionable = indexer
        .actionable_plan(planned)
        .map_err(|error| format!("{error:?}"))?;
    let planning_ns = planning.elapsed().as_nanos();
    let application = Instant::now();
    let result = indexer
        .apply_change_plan(connection, config, actionable)
        .map_err(|error| error.to_string())?;
    if !matches!(result, crate::indexer::ChangeApplicationResult::Applied(_)) {
        return Err(format!("incremental operation {id} was rejected"));
    }
    Ok(IncrementalOperation {
        id,
        classifications,
        planning_ns,
        application_ns: application.elapsed().as_nanos(),
        before,
        after: file_sizes(db_path)?,
    })
}

fn file_sizes(path: &Path) -> Result<FileSizes, String> {
    let mut wal = path.as_os_str().to_os_string();
    wal.push("-wal");
    let database_bytes = fs::metadata(path)
        .map_err(|error| format!("inspect database {}: {error}", path.display()))?
        .len();
    let wal_path = PathBuf::from(wal);
    let wal_bytes = match fs::metadata(&wal_path) {
        Ok(metadata) => metadata.len(),
        Err(error) if error.kind() == std::io::ErrorKind::NotFound => 0,
        Err(error) => return Err(format!("inspect WAL {}: {error}", wal_path.display())),
    };
    Ok(FileSizes {
        database_bytes,
        wal_bytes,
    })
}

fn validate_plan(
    result: &crate::indexer::ChangePlanningResult,
    id: &str,
) -> Result<ChangeCounts, String> {
    let crate::indexer::ChangePlanningResult::Ready(plan) = result else {
        return Err(format!("{id}: full rebuild required"));
    };
    let counts = ChangeCounts {
        unchanged: plan.unchanged.len(),
        metadata_only: plan.metadata_only.len(),
        created: plan.created.len(),
        modified: plan.modified.len(),
        deleted: plan.deleted.len(),
        failed: plan.failed.len(),
    };
    let valid = match id {
        "metadata-only" => (1, 0, 0, 0, 0),
        "modified-replacement" => (0, 0, 1, 0, 0),
        "created-file" => (0, 1, 0, 0, 0),
        "deleted-file" => (0, 0, 0, 1, 0),
        "mixed-change-set" => (0, 1, 1, 1, 0),
        _ => return Err(format!("unknown operation {id}")),
    };
    if (
        counts.metadata_only,
        counts.created,
        counts.modified,
        counts.deleted,
        counts.failed,
    ) != (valid.0, valid.1, valid.2, valid.3, valid.4)
    {
        return Err(format!("{id}: unexpected classifications metadata={} created={} modified={} deleted={} failed={}", counts.metadata_only, counts.created, counts.modified, counts.deleted, counts.failed));
    }
    Ok(counts)
}

fn copy_directory(source: &Path, destination: &Path) -> Result<(), String> {
    fs::create_dir_all(destination).map_err(|error| error.to_string())?;
    for entry in fs::read_dir(source).map_err(|error| error.to_string())? {
        let entry = entry.map_err(|error| error.to_string())?;
        let destination_path = destination.join(entry.file_name());
        if entry.path().is_dir() {
            copy_directory(&entry.path(), &destination_path)?;
        } else {
            fs::copy(entry.path(), destination_path).map_err(|error| error.to_string())?;
        }
    }
    Ok(())
}

#[derive(Debug)]
struct QueryWorkload {
    id: &'static str,
    expression: String,
    output_mode: QueryOutputMode,
}

fn query_workloads(source_dir: &Path) -> Vec<QueryWorkload> {
    let path = source_dir.join("projects/alpha/note-00000.org");
    let path = path.to_str().expect("benchmark corpus paths are UTF-8");
    let directory = source_dir.join("projects/alpha");
    let directory = directory
        .to_str()
        .expect("benchmark corpus paths are UTF-8");
    let mut specs = vec![
        (
            "query.ancestors",
            "(headings (ancestors))".to_owned(),
            QueryOutputMode::Flat,
        ),
        (
            "query.children",
            "(headings (children))".to_owned(),
            QueryOutputMode::Flat,
        ),
        (
            "query.deep.descendants",
            "(headings (descendants (headings (title \"Deep A\" :exact t))))".to_owned(),
            QueryOutputMode::Flat,
        ),
        (
            "query.descendants",
            "(headings (descendants))".to_owned(),
            QueryOutputMode::Flat,
        ),
        (
            "query.file.dir",
            format!("(headings (file-dir \"{directory}\" :exact t))"),
            QueryOutputMode::Flat,
        ),
        (
            "query.file.name",
            "(headings (file-name \"note-00000.org\" :exact t))".to_owned(),
            QueryOutputMode::Flat,
        ),
        (
            "query.file.path",
            format!("(headings (file-path \"{path}\" :exact t))"),
            QueryOutputMode::Flat,
        ),
        (
            "query.file.title",
            "(headings (file-title \"Benchmark 0\" :exact t))".to_owned(),
            QueryOutputMode::Flat,
        ),
        (
            "query.has-link",
            "(headings (has-link))".to_owned(),
            QueryOutputMode::Flat,
        ),
        (
            "query.links.broken",
            "(links (status \"broken\"))".to_owned(),
            QueryOutputMode::Flat,
        ),
        (
            "query.links.resolved",
            "(links (status \"resolved\"))".to_owned(),
            QueryOutputMode::Flat,
        ),
        (
            "query.outline.common-tags",
            "(headings (tags \"common\"))".to_owned(),
            QueryOutputMode::Outline,
        ),
        (
            "query.parent",
            "(headings (parent))".to_owned(),
            QueryOutputMode::Flat,
        ),
        (
            "query.planning",
            "(headings (planning))".to_owned(),
            QueryOutputMode::Flat,
        ),
        (
            "query.property.direct",
            "(headings (property \"GROUP\" \"group1\" :inherit nil))".to_owned(),
            QueryOutputMode::Flat,
        ),
        (
            "query.property.effective",
            "(headings (property \"GROUP\" \"group1\"))".to_owned(),
            QueryOutputMode::Flat,
        ),
        (
            "query.tags.common",
            "(headings (tags \"common\"))".to_owned(),
            QueryOutputMode::Flat,
        ),
        (
            "query.tags.selective",
            "(headings (tags \"tag1\"))".to_owned(),
            QueryOutputMode::Flat,
        ),
        (
            "query.timestamps.closed",
            "(headings (closed))".to_owned(),
            QueryOutputMode::Flat,
        ),
        (
            "query.timestamps.deadline",
            "(headings (deadline))".to_owned(),
            QueryOutputMode::Flat,
        ),
        (
            "query.timestamps.scheduled",
            "(headings (scheduled))".to_owned(),
            QueryOutputMode::Flat,
        ),
        (
            "query.timestamps.active",
            "(headings (ts-active))".to_owned(),
            QueryOutputMode::Flat,
        ),
        (
            "query.timestamps.inactive",
            "(headings (ts-inactive))".to_owned(),
            QueryOutputMode::Flat,
        ),
        (
            "query.timestamps.generic",
            "(headings (ts))".to_owned(),
            QueryOutputMode::Flat,
        ),
        (
            "query.todo.nonselective",
            "(headings (todo \"TODO\" \"DONE\"))".to_owned(),
            QueryOutputMode::Flat,
        ),
        (
            "query.todo.selective",
            "(headings (todo \"TODO\"))".to_owned(),
            QueryOutputMode::Flat,
        ),
        (
            "query.wide.children",
            "(headings (children (headings (title \"Project 0\" :exact t))))".to_owned(),
            QueryOutputMode::Flat,
        ),
    ];
    specs.sort_by_key(|(id, _, _)| *id);
    specs
        .into_iter()
        .map(|(id, expression, output_mode)| QueryWorkload {
            id,
            expression,
            output_mode,
        })
        .collect()
}

fn measure_query(
    db_path: &Path,
    workload: &QueryWorkload,
    options: BenchmarkOptions,
) -> Result<WorkloadResult, String> {
    let cold_start = Instant::now();
    let cold_connection =
        open_existing_database_read_only(db_path).map_err(|error| error.to_string())?;
    let cold_parsed = parse_query(&workload.expression).map_err(|error| error.to_string())?;
    let cold_validation =
        sqlite_query_validation_options(&cold_connection).map_err(|error| error.to_string())?;
    let cold_validated =
        validate_query(cold_parsed, &cold_validation).map_err(|error| error.to_string())?;
    let cold_rows = execute_and_shape_query(
        &cold_connection,
        &cold_validated,
        &QueryExecutionOptions {
            output_mode: workload.output_mode,
            ..Default::default()
        },
    )
    .map_err(|error| error.to_string())?;
    std::hint::black_box(cold_rows);
    let cold_connection_end_to_end_ns = cold_start.elapsed().as_nanos();
    drop(cold_connection);
    let connection =
        open_existing_database_read_only(db_path).map_err(|error| error.to_string())?;
    let parsed = parse_query(&workload.expression).map_err(|error| error.to_string())?;
    let validation =
        sqlite_query_validation_options(&connection).map_err(|error| error.to_string())?;
    let validated = validate_query(parsed, &validation).map_err(|error| error.to_string())?;
    let compiled = compile_sqlite_query(&validated).map_err(|error| error.to_string())?;
    let parse_validate_compile = measure_timing(options, || {
        let parsed = parse_query(&workload.expression).map_err(|error| error.to_string())?;
        let validation =
            sqlite_query_validation_options(&connection).map_err(|error| error.to_string())?;
        let validated = validate_query(parsed, &validation).map_err(|error| error.to_string())?;
        let _ = compile_sqlite_query(&validated).map_err(|error| error.to_string())?;
        Ok(())
    })?;
    for _ in 0..options.warmups {
        let response = execute_and_shape_query(
            &connection,
            &validated,
            &QueryExecutionOptions {
                output_mode: workload.output_mode,
                ..Default::default()
            },
        )
        .map_err(|error| error.to_string())?;
        std::hint::black_box(response);
    }
    let mut samples = Vec::with_capacity(options.iterations);
    let mut result_count = 0;
    for _ in 0..options.iterations {
        let start = Instant::now();
        let response = execute_and_shape_query(
            &connection,
            &validated,
            &QueryExecutionOptions {
                output_mode: workload.output_mode,
                ..Default::default()
            },
        )
        .map_err(|error| error.to_string())?;
        result_count = response.results.len();
        std::hint::black_box(response);
        samples.push(start.elapsed());
    }
    samples.sort();
    let parameters = compiled
        .params
        .iter()
        .map(|value| match value {
            crate::query::QueryParam::Integer(value) => Parameter::Integer(*value),
            crate::query::QueryParam::Text(value) => Parameter::Text(value.clone()),
        })
        .collect();
    let explain_query_plan = explain(&connection, &compiled.sql, &compiled.params)?;
    Ok(WorkloadResult {
        id: workload.id,
        production_path: "query-model-v0",
        expression: workload.expression.clone(),
        sql: compiled.sql,
        parameters,
        result_count,
        output_mode: workload.output_mode,
        cold_connection_end_to_end_ns,
        parse_validate_compile,
        validated_query_end_to_end: timing(&samples),
        explain_query_plan,
    })
}

fn explain(
    connection: &Connection,
    sql: &str,
    params: &[crate::query::QueryParam],
) -> Result<Vec<String>, String> {
    let mut statement = connection
        .prepare(&format!("EXPLAIN QUERY PLAN {sql}"))
        .map_err(|error| error.to_string())?;
    let rows = statement
        .query_map(rusqlite::params_from_iter(params), |row| {
            row.get::<_, String>(3)
        })
        .map_err(|error| error.to_string())?;
    rows.collect::<Result<Vec<_>, _>>()
        .map_err(|error| error.to_string())
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
fn measure_timing<F>(options: BenchmarkOptions, mut operation: F) -> Result<Timing, String>
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

fn environment() -> Result<Environment, String> {
    let connection = Connection::open_in_memory().map_err(|error| error.to_string())?;
    let sqlite_version = connection
        .query_row("SELECT sqlite_version()", [], |row| row.get(0))
        .map_err(|error| error.to_string())?;
    let mut statement = connection
        .prepare("PRAGMA compile_options")
        .map_err(|error| error.to_string())?;
    let sqlite_compile_options = statement
        .query_map([], |row| row.get(0))
        .map_err(|error| error.to_string())?
        .collect::<Result<Vec<String>, _>>()
        .map_err(|error| error.to_string())?;
    let cache_size = connection
        .query_row("PRAGMA cache_size", [], |row| row.get(0))
        .map_err(|error| error.to_string())?;
    let mmap_size = connection
        .query_row("PRAGMA mmap_size", [], |row| row.get(0))
        .optional()
        .map_err(|error| error.to_string())?
        .unwrap_or(0);
    let journal_mode = connection
        .query_row("PRAGMA journal_mode", [], |row| row.get(0))
        .map_err(|error| error.to_string())?;
    let statistics_tables = ["sqlite_stat1", "sqlite_stat4"]
        .into_iter()
        .filter(|table| {
            connection
                .query_row(
                    "SELECT EXISTS(SELECT 1 FROM sqlite_master WHERE type = 'table' AND name = ?1)",
                    [table],
                    |row| row.get::<_, bool>(0),
                )
                .unwrap_or(false)
        })
        .map(str::to_string)
        .collect();
    Ok(Environment {
        command_arguments: std::env::args().collect(),
        build_profile: if cfg!(debug_assertions) {
            "debug"
        } else {
            "release"
        },
        operating_system: std::env::consts::OS,
        architecture: std::env::consts::ARCH,
        available_cpus: std::thread::available_parallelism().ok().map(usize::from),
        available_memory_bytes: None,
        sqlite_version,
        sqlite_compile_options,
        pragmas: ConnectionPragmas {
            cache_size,
            mmap_size,
            journal_mode,
            transaction_state: "read-only autocommit for workload groups",
        },
        statistics_tables,
        storage_notes: "unknown",
    })
}

fn explicit_indexes(connection: &Connection) -> Result<Vec<String>, String> {
    let mut statement = connection
        .prepare(
            "SELECT name FROM sqlite_master WHERE type = 'index' AND sql IS NOT NULL ORDER BY name",
        )
        .map_err(|error| error.to_string())?;
    let indexes = statement
        .query_map([], |row| row.get(0))
        .map_err(|error| error.to_string())?
        .collect::<Result<Vec<String>, _>>()
        .map_err(|error| error.to_string())?;
    Ok(indexes)
}

fn index_inventory(connection: &Connection) -> Result<Vec<IndexInventoryEntry>, String> {
    let mut tables = connection
        .prepare("SELECT name FROM sqlite_master WHERE type = 'table' AND name NOT LIKE 'sqlite_%' ORDER BY name")
        .map_err(|error| error.to_string())?;
    let tables = tables
        .query_map([], |row| row.get::<_, String>(0))
        .map_err(|error| error.to_string())?
        .collect::<Result<Vec<_>, _>>()
        .map_err(|error| error.to_string())?;
    let mut result = Vec::new();
    for table in tables {
        let quoted = format!("'{}'", table.replace('\'', "''"));
        let mut statement = connection
            .prepare(&format!("PRAGMA index_list({quoted})"))
            .map_err(|error| error.to_string())?;
        let indexes = statement
            .query_map([], |row| {
                Ok((
                    row.get::<_, String>(1)?,
                    row.get::<_, i64>(2)? != 0,
                    row.get::<_, String>(3)?,
                    row.get::<_, i64>(4)? != 0,
                ))
            })
            .map_err(|error| error.to_string())?
            .collect::<Result<Vec<_>, _>>()
            .map_err(|error| error.to_string())?;
        for (name, unique, origin, partial) in indexes {
            let mut columns_statement = connection
                .prepare(&format!(
                    "PRAGMA index_xinfo('{}')",
                    name.replace('\'', "''")
                ))
                .map_err(|error| error.to_string())?;
            let columns = columns_statement
                .query_map([], |row| {
                    let key: i64 = row.get(5)?;
                    Ok((
                        row.get::<_, i64>(0)?,
                        row.get::<_, Option<String>>(2)?,
                        key != 0,
                    ))
                })
                .map_err(|error| error.to_string())?
                .collect::<Result<Vec<_>, _>>()
                .map_err(|error| error.to_string())?
                .into_iter()
                .filter_map(|(sequence, column, key)| {
                    key.then_some(IndexColumn {
                        sequence,
                        expression: column.is_none(),
                        column,
                    })
                })
                .collect();
            let sql = connection
                .query_row(
                    "SELECT sql FROM sqlite_master WHERE type = 'index' AND name = ?1",
                    [&name],
                    |row| row.get::<_, Option<String>>(0),
                )
                .optional()
                .map_err(|error| error.to_string())?
                .flatten();
            let classification = match origin.as_str() {
                "c" => "explicit_declared",
                "u" => "unique_constraint_implied",
                "pk" => "primary_key_implied",
                _ => "sqlite_internal",
            };
            let dbstat = connection
                .query_row(
                    "SELECT COUNT(*), COALESCE(SUM(pgsize), 0), COALESCE(SUM(payload), 0) FROM dbstat WHERE name = ?1",
                    [&name],
                    |row| {
                        Ok(IndexDbstat {
                            pages: row.get(0)?,
                            total_bytes: row.get(1)?,
                            payload_bytes: row.get(2)?,
                        })
                    },
                )
                .ok();
            result.push(IndexInventoryEntry {
                table: table.clone(),
                name: Some(name),
                classification,
                unique,
                partial,
                columns,
                sql,
                dbstat,
            });
        }
    }
    Ok(result)
}

fn dbstat_availability(connection: &Connection) -> DbstatAvailability {
    match connection.query_row("SELECT COUNT(*) FROM dbstat", [], |row| {
        row.get::<_, i64>(0)
    }) {
        Ok(_) => DbstatAvailability {
            available: true,
            reason: None,
        },
        Err(error) => DbstatAvailability {
            available: false,
            reason: Some(error.to_string()),
        },
    }
}

fn database_size(
    connection: &Connection,
    path: &Path,
    rebuild_duration_ns: u128,
) -> Result<DatabaseSize, String> {
    let mut wal_name = path.as_os_str().to_os_string();
    wal_name.push("-wal");
    let wal = PathBuf::from(wal_name);
    let main_before_checkpoint_bytes = fs::metadata(path).map_err(|e| e.to_string())?.len();
    let wal_before_checkpoint_bytes = fs::metadata(&wal).map(|m| m.len()).unwrap_or(0);
    connection
        .execute_batch("PRAGMA wal_checkpoint(TRUNCATE);")
        .map_err(|error| error.to_string())?;
    let wal_after_checkpoint_bytes = fs::metadata(&wal).map(|m| m.len()).unwrap_or(0);
    let page_count = connection
        .query_row("PRAGMA page_count", [], |row| row.get(0))
        .map_err(|e| e.to_string())?;
    let page_size = connection
        .query_row("PRAGMA page_size", [], |row| row.get(0))
        .map_err(|e| e.to_string())?;
    let freelist_count = connection
        .query_row("PRAGMA freelist_count", [], |row| row.get(0))
        .map_err(|e| e.to_string())?;
    let journal_mode = connection
        .query_row("PRAGMA journal_mode", [], |row| row.get(0))
        .map_err(|e| e.to_string())?;
    Ok(DatabaseSize {
        main_before_checkpoint_bytes,
        main_after_checkpoint_bytes: fs::metadata(path).map_err(|e| e.to_string())?.len(),
        wal_before_checkpoint_bytes,
        wal_after_checkpoint_bytes,
        page_count,
        page_size,
        freelist_count,
        journal_mode,
        checkpoint: "wal_checkpoint(TRUNCATE)",
        rebuild_duration_ns,
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    fn temporary_root(label: &str) -> PathBuf {
        let nonce = std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .expect("system clock")
            .as_nanos();
        std::env::temp_dir().join(format!(
            "orgfdb-benchmark-{label}-{}-{nonce}",
            std::process::id()
        ))
    }

    fn build_equivalent_databases(
        label: &str,
        search: SearchConfig,
    ) -> (PathBuf, Config, Connection, Connection) {
        let root = temporary_root(label);
        let source_dir = root.join("corpus");
        generate_corpus(&source_dir, 3, 1).expect("corpus should generate");
        let left_path = root.join("left.sqlite");
        let config = Config {
            db_path: left_path.clone(),
            files: Vec::new(),
            dirs: vec![crate::config::ConfiguredDir {
                path: source_dir,
                recursive: true,
                exclude: Vec::new(),
            }],
            discovery: Default::default(),
            links: Default::default(),
            todo: Default::default(),
            search: search.clone(),
            query: Default::default(),
        };
        let mut left = open_database_with_schema(
            &left_path,
            &SchemaDefinition::new(CURRENT_SCHEMA_VERSION, search.fts5_enabled),
        )
        .expect("left database should open");
        Indexer::new(OrgizeAdapter::new())
            .rebuild(&mut left, &config)
            .expect("left database should rebuild");

        let right_path = root.join("right.sqlite");
        let mut right_config = config.clone();
        right_config.db_path = right_path.clone();
        let mut right = open_database_with_schema(
            &right_path,
            &SchemaDefinition::new(CURRENT_SCHEMA_VERSION, search.fts5_enabled),
        )
        .expect("right database should open");
        Indexer::new(OrgizeAdapter::new())
            .rebuild(&mut right, &right_config)
            .expect("right database should rebuild");

        assert_eq!(
            semantic_snapshot(&left).expect("left semantic snapshot"),
            semantic_snapshot(&right).expect("right semantic snapshot")
        );
        verify_fts_equivalence(&left, &right, &config)
            .expect("equivalent databases should have equivalent FTS state");
        (root, config, left, right)
    }

    fn assert_relationship_corruption_detected<F>(label: &str, corrupt: F)
    where
        F: FnOnce(&Connection) -> Result<(), String>,
    {
        let (root, _config, left, right) = build_equivalent_databases(
            label,
            SearchConfig {
                fts5_enabled: false,
                index_body_text: false,
            },
        );
        corrupt(&right).expect("corruption should be applied deterministically");
        assert_ne!(
            semantic_snapshot(&left).expect("left semantic snapshot"),
            semantic_snapshot(&right).expect("corrupted semantic snapshot"),
            "{label} corruption must be detected"
        );
        drop(left);
        drop(right);
        let _ = fs::remove_dir_all(root);
    }

    fn sqlite_has_fts5() -> bool {
        let connection = Connection::open_in_memory().expect("probe database should open");
        crate::db::sqlite_supports_fts5(&connection).unwrap_or(false)
    }

    #[test]
    fn deterministic_manifest_has_expected_cardinality() {
        let root = temporary_root("manifest");
        let first = generate_corpus(&root.join("first"), 3, 7).expect("corpus");
        let second = generate_corpus(&root.join("second"), 3, 7).expect("corpus");
        assert_eq!(first.files, 3);
        assert_eq!(first.expected_headings, 26);
        assert_eq!(first.expected_links, 4);
        assert_eq!(first.fingerprint, second.fingerprint);
        let _ = fs::remove_dir_all(root);
    }

    #[test]
    fn corpus_directory_rejects_stale_files_and_zero_files() {
        let root = temporary_root("stale");
        let directory = root.join("corpus");
        generate_corpus(&directory, 4, 1).expect("initial corpus");
        assert!(generate_corpus(&directory, 2, 1).is_err());
        assert!(generate_corpus(&root.join("zero"), 0, 1).is_err());
        let _ = fs::remove_dir_all(root);
    }

    #[test]
    fn semantic_snapshot_rejects_relationship_corruption() {
        assert_relationship_corruption_detected("parent", |connection| {
            let (heading_id, current_parent_id, alternate_parent_id): (i64, i64, i64) = connection
                .query_row(
                    "SELECT child.id, child.parent_id, alternative.id
                     FROM headings AS child
                     INNER JOIN headings AS alternative
                       ON alternative.id != child.parent_id
                      AND alternative.id != child.id
                     WHERE child.parent_id IS NOT NULL
                     ORDER BY child.id, alternative.id
                     LIMIT 1",
                    [],
                    |row| Ok((row.get(0)?, row.get(1)?, row.get(2)?)),
                )
                .map_err(|error| error.to_string())?;
            if current_parent_id == alternate_parent_id {
                return Err("parent corruption target did not change".into());
            }
            let changed = connection
                .execute(
                    "UPDATE headings SET parent_id = ?1 WHERE id = ?2 AND parent_id = ?3",
                    rusqlite::params![alternate_parent_id, heading_id, current_parent_id],
                )
                .map_err(|error| error.to_string())?;
            if changed != 1 {
                return Err(format!("parent corruption changed {changed} rows"));
            }
            let stored: i64 = connection
                .query_row(
                    "SELECT parent_id FROM headings WHERE id = ?1",
                    [heading_id],
                    |row| row.get(0),
                )
                .map_err(|error| error.to_string())?;
            if stored != alternate_parent_id {
                return Err("parent corruption did not persist the alternate parent".into());
            }
            Ok(())
        });

        assert_relationship_corruption_detected("tag-owner", |connection| {
            let (current_heading_id, tag, alternate_heading_id): (i64, String, i64) = connection
                .query_row(
                    "SELECT tag.heading_id, tag.tag, alternative.id
                     FROM tags AS tag
                     INNER JOIN headings AS alternative ON alternative.id != tag.heading_id
                     WHERE NOT EXISTS (
                         SELECT 1
                         FROM tags AS existing
                         WHERE existing.heading_id = alternative.id
                           AND existing.tag = tag.tag
                     )
                     ORDER BY tag.heading_id, tag.tag, alternative.id
                     LIMIT 1",
                    [],
                    |row| Ok((row.get(0)?, row.get(1)?, row.get(2)?)),
                )
                .map_err(|error| error.to_string())?;
            if current_heading_id == alternate_heading_id {
                return Err("tag owner corruption target did not change".into());
            }
            let changed = connection
                .execute(
                    "UPDATE tags SET heading_id = ?1 WHERE heading_id = ?2 AND tag = ?3",
                    rusqlite::params![alternate_heading_id, current_heading_id, &tag],
                )
                .map_err(|error| error.to_string())?;
            if changed != 1 {
                return Err(format!("tag owner corruption changed {changed} rows"));
            }
            let stored: i64 = connection
                .query_row(
                    "SELECT heading_id FROM tags WHERE heading_id = ?1 AND tag = ?2",
                    rusqlite::params![alternate_heading_id, &tag],
                    |row| row.get(0),
                )
                .map_err(|error| error.to_string())?;
            if stored != alternate_heading_id {
                return Err("tag owner corruption did not persist the alternate owner".into());
            }
            Ok(())
        });

        assert_relationship_corruption_detected("link-source", |connection| {
            let (link_id, current_heading_id, alternate_heading_id): (i64, i64, i64) = connection
                .query_row(
                    "SELECT link.id, link.heading_id, alternative.id
                     FROM links AS link
                     INNER JOIN headings AS alternative
                       ON alternative.file_id = link.file_id
                      AND alternative.id != link.heading_id
                     ORDER BY link.id, alternative.id
                     LIMIT 1",
                    [],
                    |row| Ok((row.get(0)?, row.get(1)?, row.get(2)?)),
                )
                .map_err(|error| error.to_string())?;
            if current_heading_id == alternate_heading_id {
                return Err("link source corruption target did not change".into());
            }
            let changed = connection
                .execute(
                    "UPDATE links SET heading_id = ?1 WHERE id = ?2 AND heading_id = ?3",
                    rusqlite::params![alternate_heading_id, link_id, current_heading_id],
                )
                .map_err(|error| error.to_string())?;
            if changed != 1 {
                return Err(format!("link source corruption changed {changed} rows"));
            }
            let stored: i64 = connection
                .query_row(
                    "SELECT heading_id FROM links WHERE id = ?1",
                    [link_id],
                    |row| row.get(0),
                )
                .map_err(|error| error.to_string())?;
            if stored != alternate_heading_id {
                return Err("link source corruption did not persist the alternate source".into());
            }
            Ok(())
        });

        assert_relationship_corruption_detected("outline-parent", |connection| {
            let (heading_id, current_parent_id, alternate_parent_id): (i64, i64, i64) = connection
                .query_row(
                    "SELECT outline.heading_id, outline.parent_id, alternative.id
                     FROM outline_path AS outline
                     INNER JOIN headings AS alternative
                       ON alternative.id != outline.parent_id
                      AND alternative.id != outline.heading_id
                     WHERE outline.parent_id IS NOT NULL
                     ORDER BY outline.heading_id, alternative.id
                     LIMIT 1",
                    [],
                    |row| Ok((row.get(0)?, row.get(1)?, row.get(2)?)),
                )
                .map_err(|error| error.to_string())?;
            if current_parent_id == alternate_parent_id {
                return Err("outline parent corruption target did not change".into());
            }
            let changed = connection
                .execute(
                    "UPDATE outline_path SET parent_id = ?1 WHERE heading_id = ?2 AND parent_id = ?3",
                    rusqlite::params![alternate_parent_id, heading_id, current_parent_id],
                )
                .map_err(|error| error.to_string())?;
            if changed != 1 {
                return Err(format!("outline parent corruption changed {changed} rows"));
            }
            let stored: i64 = connection
                .query_row(
                    "SELECT parent_id FROM outline_path WHERE heading_id = ?1",
                    [heading_id],
                    |row| row.get(0),
                )
                .map_err(|error| error.to_string())?;
            if stored != alternate_parent_id {
                return Err("outline corruption did not persist the alternate parent".into());
            }
            Ok(())
        });
    }

    #[test]
    fn fts_equivalence_rejects_extra_rows_and_body_policy_mismatch() {
        if !sqlite_has_fts5() {
            return;
        }
        let body_search = SearchConfig {
            fts5_enabled: true,
            index_body_text: true,
        };

        let (root, config, left, right) =
            build_equivalent_databases("fts-extra-row", body_search.clone());
        let bogus_row_id: i64 = right
            .query_row(
                "SELECT COALESCE(MAX(id), 0) + 1000 FROM headings",
                [],
                |row| row.get(0),
            )
            .expect("bogus FTS row ID should load");
        let inserted = right
            .execute(
                "INSERT INTO heading_fts (rowid, title, body) VALUES (?1, ?2, ?3)",
                rusqlite::params![bogus_row_id, "Corrupt Project", "token"],
            )
            .expect("bogus FTS row should insert");
        assert_eq!(inserted, 1);
        assert!(verify_fts_equivalence(&left, &right, &config).is_err());
        drop(left);
        drop(right);
        let _ = fs::remove_dir_all(root);

        let (root, config, left, right) =
            build_equivalent_databases("fts-body-policy", body_search);
        crate::db::DbWriter::rebuild_heading_fts(&right, false)
            .expect("title-only FTS rebuild should succeed");
        let error = verify_fts_equivalence(&left, &right, &config)
            .expect_err("body-content mismatch must be detected");
        assert!(error.contains("production-search results differ"));
        drop(left);
        drop(right);
        let _ = fs::remove_dir_all(root);
    }

    #[test]
    fn benchmark_output_covers_workloads_protocol_and_database_evidence() {
        let root = temporary_root("output");
        let output = root.join("result.json");
        let work_dir = root.join("run");
        run(
            &output,
            &work_dir,
            BenchmarkOptions {
                files: 3,
                seed: 1,
                warmups: 0,
                iterations: 1,
            },
        )
        .expect("benchmark run");
        let value: serde_json::Value =
            serde_json::from_slice(&fs::read(&output).expect("output")).expect("valid JSON");
        assert_eq!(value["output_schema_version"], OUTPUT_SCHEMA_VERSION);
        assert_eq!(value["manifest"]["files"], 3);
        assert!(value["manifest"]["generation_duration_ns"].is_number());
        assert_eq!(value["protocol"]["warmups"], 0);
        assert!(value["environment"]["sqlite_version"].is_string());
        assert!(value["environment"]["pragmas"]["cache_size"].is_number());
        let variants = value["variants"].as_array().expect("variants");
        assert_eq!(variants.len(), 3 * INDEX_PROFILES.len());
        for variant in variants {
            let workloads = variant["workloads"].as_array().expect("workloads");
            let ids = workloads
                .iter()
                .map(|workload| workload["id"].as_str().expect("workload ID"))
                .collect::<Vec<_>>();
            assert!(ids.windows(2).all(|pair| pair[0] < pair[1]));
            assert!(ids.contains(&"query.outline.common-tags"));
            assert!(workloads.iter().all(|workload| {
                workload["cold_connection_end_to_end_ns"].is_number()
                    && workload["parse_validate_compile"]["samples"].is_number()
                    && workload["validated_query_end_to_end"]["samples"].is_number()
            }));
            assert!(variant["database"]["main_before_checkpoint_bytes"].is_number());
            assert!(variant["database"]["main_after_checkpoint_bytes"].is_number());
            assert!(variant["database"]["wal_before_checkpoint_bytes"].is_number());
            assert!(variant["profile_id"].is_string());
            assert!(variant["profile_kind"].is_string());
            assert!(variant["explicit_indexes"].is_array());
            assert!(variant["dbstat"]["available"].is_boolean());
        }
        let no_fts = &variants[0];
        let connection =
            open_existing_database_read_only(work_dir.join("no-fts-baseline-v8.sqlite"))
                .expect("read-only database");
        let headings: usize = connection
            .query_row("SELECT COUNT(*) FROM headings", [], |row| row.get(0))
            .expect("heading count");
        let links: usize = connection
            .query_row("SELECT COUNT(*) FROM links", [], |row| row.get(0))
            .expect("link count");
        assert_eq!(headings, 24);
        assert_eq!(links, 3);
        assert!(no_fts["incremental"]["semantic_equivalent"].as_bool() == Some(true));
        assert_eq!(
            no_fts["incremental"]["operations"].as_array().map(Vec::len),
            Some(5)
        );
        let operations = no_fts["incremental"]["operations"]
            .as_array()
            .expect("operations");
        let expected = [
            ("metadata-only", 1, 0, 0, 0),
            ("modified-replacement", 0, 0, 1, 0),
            ("created-file", 0, 1, 0, 0),
            ("deleted-file", 0, 0, 0, 1),
            ("mixed-change-set", 0, 1, 1, 1),
        ];
        for (operation, (id, metadata_only, created, modified, deleted)) in
            operations.iter().zip(expected)
        {
            let classifications = &operation["classifications"];
            assert_eq!(operation["id"], id);
            assert_eq!(classifications["metadata_only"], metadata_only);
            assert_eq!(classifications["created"], created);
            assert_eq!(classifications["modified"], modified);
            assert_eq!(classifications["deleted"], deleted);
            assert_eq!(classifications["failed"], 0);
        }
        assert!(no_fts["fts_workloads"].as_array().expect("skips").len() == 2);
        let _ = fs::remove_dir_all(root);
    }
}
