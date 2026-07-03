use std::{
    collections::BTreeMap,
    error::Error,
    fmt, fs, io,
    path::{Path, PathBuf},
    time::{SystemTime, UNIX_EPOCH},
};

use rusqlite::Connection;

use crate::{
    config::{Config, ConfigError},
    db::{
        open_database_with_schema, DbError, DbWriteError, DbWriter, FileRecordInput,
        HeadingBodyRecord, HeadingFtsRecord, HeadingRecord, KeywordRecord, LinkRecord,
        OutlinePathRecord, PropertyRecord, SchemaDefinition, TagRecord, TimestampRecord,
        TimestampRepeaterRecord, TodoKeywordRecord, CURRENT_SCHEMA_VERSION,
    },
    link_resolver::IndexedUniverse,
    link_resolver::LinkResolver,
    parser::{
        DiagnosticSeverity, OrgParserCore, ParseDiagnostic, ParseOptions, ParsedHeading,
        ParsedLink, ParsedOrgDocument, ParsedTimestamp, ParsedTimestampModifierKind,
        ParsedTimestampModifierType, ParsedTimestampRole, ParsedTimestampUnit, TodoType,
    },
    todo_keywords::{
        resolve_todo_keywords_with_default_source, ResolvedTodoKeywordEntry, ResolvedTodoKeywords,
        TodoKeywordSourceKind,
    },
};

#[derive(Debug)]
pub struct Indexer<P> {
    parser: P,
}

impl<P> Indexer<P>
where
    P: OrgParserCore,
{
    pub fn new(parser: P) -> Self {
        Self { parser }
    }

    pub fn rebuild_from_config_path(
        &self,
        config_path: impl AsRef<Path>,
    ) -> Result<RebuildReport, IndexerError> {
        self.rebuild_from_config_path_with_options(config_path, false)
    }

    pub(crate) fn rebuild_from_config_path_with_options(
        &self,
        config_path: impl AsRef<Path>,
        allow_empty: bool,
    ) -> Result<RebuildReport, IndexerError> {
        let config = Config::load_from_file(config_path).map_err(IndexerError::Config)?;
        let schema = SchemaDefinition::new(CURRENT_SCHEMA_VERSION, config.search.fts5_enabled);
        let mut connection =
            open_database_with_schema(&config.db_path, &schema).map_err(IndexerError::Database)?;
        self.rebuild_with_options(&mut connection, &config, allow_empty)
    }

    pub fn rebuild(
        &self,
        connection: &mut Connection,
        config: &Config,
    ) -> Result<RebuildReport, IndexerError> {
        self.rebuild_with_options(connection, config, false)
    }

    pub(crate) fn rebuild_with_options(
        &self,
        connection: &mut Connection,
        config: &Config,
        allow_empty: bool,
    ) -> Result<RebuildReport, IndexerError> {
        let discovery = discover_org_files(config)?;
        if discovery.files.is_empty() {
            let existing_indexed_files = existing_indexed_file_count(connection)?;
            if existing_indexed_files == 0 {
                return Ok(RebuildReport::default());
            }
            if !allow_empty {
                return Err(IndexerError::RefusedEmptyRebuild {
                    existing_indexed_files,
                });
            }
        }

        let parse_options = config.parse_options();
        let mut pending = Vec::with_capacity(discovery.files.len());
        for discovered in discovery.files {
            let path = discovered.path;
            let metadata = fs::metadata(&path).map_err(|source| IndexerError::ReadFile {
                path: path.clone(),
                source,
            })?;
            let content = fs::read_to_string(&path).map_err(|source| IndexerError::ReadFile {
                path: path.clone(),
                source,
            })?;
            let resolved_todo_keywords = resolve_todo_keywords_with_default_source(
                &content,
                &parse_options.todo_keywords,
                TodoKeywordSourceKind::ConfigDefault,
            );
            let document = self
                .parser
                .parse_document_core(
                    &path,
                    &content,
                    &ParseOptions {
                        todo_keywords: resolved_todo_keywords.effective.clone(),
                        link_scanner: parse_options.link_scanner.clone(),
                    },
                )
                .map_err(|diagnostic| IndexerError::Parse {
                    path: path.clone(),
                    diagnostic,
                })?;
            let normalized = normalize_document(document, &path, &content);
            let file_record = build_file_record(&path, &metadata)?;
            pending.push(PendingRebuildFile {
                path,
                document: normalized,
                todo_keywords: resolved_todo_keywords,
                diagnostics: Vec::new(),
                file_record,
            });
        }

        let tx = connection
            .transaction()
            .map_err(|source| IndexerError::Write(DbWriteError::Transaction { source }))?;
        DbWriter::delete_all_indexed_data(&tx).map_err(IndexerError::Write)?;

        let mut report = RebuildReport::default();
        for pending_file in pending {
            let file_id = DbWriter::upsert_file(&tx, &pending_file.file_record)
                .map_err(IndexerError::Write)?;
            let heading_count = index_document(
                &tx,
                file_id,
                &pending_file.document,
                &pending_file.todo_keywords,
                config.search.fts5_enabled,
                config.search.index_body_text,
            )
            .map_err(IndexerError::Write)?;
            let indexed_file = IndexedFile {
                path: pending_file.path.clone(),
                file_id,
                heading_count,
            };

            report
                .diagnostics
                .extend(pending_file.diagnostics.iter().cloned());
            report.diagnostics.extend(
                pending_file
                    .document
                    .diagnostics
                    .iter()
                    .cloned()
                    .map(IndexDiagnostic::from),
            );
            report.indexed_files.push(indexed_file);
        }

        LinkResolver::resolve_all(&tx, &discovery.indexed_universe).map_err(IndexerError::Write)?;

        tx.commit()
            .map_err(|source| IndexerError::Write(DbWriteError::Transaction { source }))?;

        Ok(report)
    }
}

struct PendingRebuildFile {
    path: PathBuf,
    document: ParsedOrgDocument,
    todo_keywords: ResolvedTodoKeywords,
    diagnostics: Vec<IndexDiagnostic>,
    file_record: FileRecordInput,
}

struct DiscoveryResult {
    files: Vec<DiscoveredOrgFile>,
    indexed_universe: IndexedUniverse,
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct DiscoveredOrgFile {
    path: PathBuf,
    scan_root: PathBuf,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
enum ScanRootKind {
    ExplicitFile,
    ConfiguredDir,
}

#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct RebuildReport {
    pub indexed_files: Vec<IndexedFile>,
    pub diagnostics: Vec<IndexDiagnostic>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct IndexedFile {
    pub path: PathBuf,
    pub file_id: i64,
    pub heading_count: usize,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct IndexDiagnostic {
    pub severity: DiagnosticSeverity,
    pub message: String,
    pub file_path: Option<PathBuf>,
    pub line_number: Option<u32>,
    pub byte_range: Option<(usize, usize)>,
}

impl From<ParseDiagnostic> for IndexDiagnostic {
    fn from(value: ParseDiagnostic) -> Self {
        Self {
            severity: value.severity,
            message: value.message,
            file_path: value.file_path,
            line_number: value.line_number,
            byte_range: value.byte_range,
        }
    }
}

#[derive(Debug)]
pub enum IndexerError {
    Config(ConfigError),
    Database(DbError),
    Discover {
        path: PathBuf,
        source: std::io::Error,
    },
    InvalidDocument(&'static str),
    InvalidFileMetadata {
        path: PathBuf,
        field: &'static str,
    },
    Parse {
        path: PathBuf,
        diagnostic: ParseDiagnostic,
    },
    ReadFile {
        path: PathBuf,
        source: std::io::Error,
    },
    Serialize {
        field: &'static str,
        source: serde_json::Error,
    },
    RefusedEmptyRebuild {
        existing_indexed_files: usize,
    },
    Write(DbWriteError),
}

impl fmt::Display for IndexerError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Config(source) => write!(f, "{source}"),
            Self::Database(source) => write!(f, "{source}"),
            Self::Discover { path, source } => {
                write!(
                    f,
                    "failed to discover Org files under {}: {}",
                    path.display(),
                    source
                )
            }
            Self::InvalidDocument(message) => write!(f, "invalid parsed document: {message}"),
            Self::InvalidFileMetadata { path, field } => write!(
                f,
                "failed to convert file metadata field {field} for {}",
                path.display()
            ),
            Self::Parse { path, diagnostic } => {
                write!(
                    f,
                    "failed to parse {}: {}",
                    path.display(),
                    diagnostic.message
                )
            }
            Self::ReadFile { path, source } => {
                write!(f, "failed to read Org file {}: {}", path.display(), source)
            }
            Self::Serialize { field, source } => {
                write!(f, "failed to serialize {field} for DB write: {source}")
            }
            Self::RefusedEmptyRebuild {
                existing_indexed_files,
            } => write!(
                f,
                "rebuild found zero input Org files and was refused to avoid deleting {existing_indexed_files} indexed file(s); rerun with --allow-empty if this is intentional"
            ),
            Self::Write(source) => write!(f, "{source}"),
        }
    }
}

impl Error for IndexerError {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        match self {
            Self::Config(source) => Some(source),
            Self::Database(source) => Some(source),
            Self::Discover { source, .. } => Some(source),
            Self::InvalidDocument(_) => None,
            Self::InvalidFileMetadata { .. } => None,
            Self::Parse { .. } => None,
            Self::ReadFile { source, .. } => Some(source),
            Self::Serialize { source, .. } => Some(source),
            Self::RefusedEmptyRebuild { .. } => None,
            Self::Write(source) => Some(source),
        }
    }
}

fn discover_org_files(config: &Config) -> Result<DiscoveryResult, IndexerError> {
    let mut paths = BTreeMap::new();
    let mut indexed_universe = IndexedUniverse::default();

    for file in &config.files {
        let canonical_file = canonicalize_existing_file(file)?;
        let scan_root = canonical_file
            .parent()
            .unwrap_or(canonical_file.as_path())
            .to_path_buf();
        indexed_universe.add_exact_path(canonical_file.clone());
        insert_discovered_path(
            &mut paths,
            canonical_file,
            scan_root,
            ScanRootKind::ExplicitFile,
        );
    }

    for dir in &config.dirs {
        let canonical_dir = canonicalize_existing_dir(&dir.path)?;
        if dir.recursive {
            indexed_universe.add_recursive_root(canonical_dir.clone());
        }
        collect_org_files(
            &canonical_dir,
            &canonical_dir,
            dir.recursive,
            &mut paths,
            &mut indexed_universe,
        )?;
    }

    Ok(DiscoveryResult {
        files: paths
            .into_iter()
            .map(|(path, (scan_root, _))| DiscoveredOrgFile { path, scan_root })
            .collect(),
        indexed_universe,
    })
}

fn existing_indexed_file_count(connection: &Connection) -> Result<usize, IndexerError> {
    let count = connection
        .query_row("SELECT COUNT(*) FROM files", [], |row| row.get::<_, i64>(0))
        .map_err(|source| {
            IndexerError::Database(DbError::Inspect {
                target: "existing indexed files".to_string(),
                source,
            })
        })?;

    Ok(count as usize)
}

fn collect_org_files(
    scan_root: &Path,
    dir: &Path,
    recursive: bool,
    output: &mut BTreeMap<PathBuf, (PathBuf, ScanRootKind)>,
    indexed_universe: &mut IndexedUniverse,
) -> Result<(), IndexerError> {
    let mut entries = fs::read_dir(dir)
        .map_err(|source| IndexerError::Discover {
            path: dir.to_path_buf(),
            source,
        })?
        .collect::<Result<Vec<_>, _>>()
        .map_err(|source| IndexerError::Discover {
            path: dir.to_path_buf(),
            source,
        })?;
    entries.sort_by_key(|entry| entry.path());

    for entry in entries {
        let path = entry.path();
        let file_type = entry.file_type().map_err(|source| IndexerError::Discover {
            path: path.clone(),
            source,
        })?;

        if file_type.is_file() {
            if path
                .extension()
                .and_then(|value| value.to_str())
                .is_some_and(|value| value.eq_ignore_ascii_case("org"))
            {
                let canonical_path = canonicalize_existing_file(&path)?;
                if !recursive {
                    indexed_universe.add_exact_path(canonical_path.clone());
                }
                insert_discovered_path(
                    output,
                    canonical_path,
                    scan_root.to_path_buf(),
                    ScanRootKind::ConfiguredDir,
                );
            }
        } else if recursive && file_type.is_dir() {
            let canonical_dir = canonicalize_existing_dir(&path)?;
            collect_org_files(
                scan_root,
                &canonical_dir,
                recursive,
                output,
                indexed_universe,
            )?;
        }
    }

    Ok(())
}

fn insert_discovered_path(
    output: &mut BTreeMap<PathBuf, (PathBuf, ScanRootKind)>,
    path: PathBuf,
    scan_root: PathBuf,
    scan_root_kind: ScanRootKind,
) {
    match output.get_mut(&path) {
        Some((existing_root, existing_kind)) => {
            let prefer_new_root = match (scan_root_kind, *existing_kind) {
                (ScanRootKind::ConfiguredDir, ScanRootKind::ExplicitFile) => true,
                (ScanRootKind::ExplicitFile, ScanRootKind::ConfiguredDir) => false,
                _ => path_depth(&scan_root) > path_depth(existing_root),
            };

            if prefer_new_root {
                *existing_root = scan_root;
                *existing_kind = scan_root_kind;
            }
        }
        None => {
            output.insert(path, (scan_root, scan_root_kind));
        }
    }
}

fn path_depth(path: &Path) -> usize {
    path.components().count()
}

fn canonicalize_existing_file(path: &Path) -> Result<PathBuf, IndexerError> {
    let canonical = fs::canonicalize(path).map_err(|source| IndexerError::Discover {
        path: path.to_path_buf(),
        source,
    })?;
    let metadata = fs::metadata(&canonical).map_err(|source| IndexerError::Discover {
        path: path.to_path_buf(),
        source,
    })?;
    if !metadata.is_file() {
        return Err(IndexerError::Discover {
            path: path.to_path_buf(),
            source: io::Error::new(io::ErrorKind::InvalidInput, "configured path is not a file"),
        });
    }
    Ok(canonical)
}

fn canonicalize_existing_dir(path: &Path) -> Result<PathBuf, IndexerError> {
    let canonical = fs::canonicalize(path).map_err(|source| IndexerError::Discover {
        path: path.to_path_buf(),
        source,
    })?;
    let metadata = fs::metadata(&canonical).map_err(|source| IndexerError::Discover {
        path: path.to_path_buf(),
        source,
    })?;
    if !metadata.is_dir() {
        return Err(IndexerError::Discover {
            path: path.to_path_buf(),
            source: io::Error::new(
                io::ErrorKind::InvalidInput,
                "configured path is not a directory",
            ),
        });
    }
    Ok(canonical)
}

fn build_file_record(
    path: &Path,
    metadata: &fs::Metadata,
) -> Result<FileRecordInput, IndexerError> {
    let modified = metadata
        .modified()
        .map_err(|source| IndexerError::ReadFile {
            path: path.to_path_buf(),
            source,
        })?;
    let modified_ns = modified
        .duration_since(UNIX_EPOCH)
        .map_err(|_| IndexerError::InvalidFileMetadata {
            path: path.to_path_buf(),
            field: "mtime_ns",
        })?
        .as_nanos();
    let size = metadata.len();
    let indexed_at = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .map_err(|_| IndexerError::InvalidFileMetadata {
            path: path.to_path_buf(),
            field: "indexed_at",
        })?
        .as_secs();

    Ok(FileRecordInput {
        path: path.to_path_buf(),
        mtime_ns: i64::try_from(modified_ns).map_err(|_| IndexerError::InvalidFileMetadata {
            path: path.to_path_buf(),
            field: "mtime_ns",
        })?,
        size: i64::try_from(size).map_err(|_| IndexerError::InvalidFileMetadata {
            path: path.to_path_buf(),
            field: "size",
        })?,
        content_hash: None,
        indexed_at: Some(i64::try_from(indexed_at).map_err(|_| {
            IndexerError::InvalidFileMetadata {
                path: path.to_path_buf(),
                field: "indexed_at",
            }
        })?),
    })
}

fn normalize_document(
    document: ParsedOrgDocument,
    path: &Path,
    content: &str,
) -> ParsedOrgDocument {
    let mut normalized = document;
    let level_zero_title = synthetic_level_zero_title(path, normalized.metadata.title.as_deref());
    let needs_level_zero = normalized
        .headings
        .first()
        .map(|heading| heading.level != 0 || heading.parent_index.is_some())
        .unwrap_or(true);

    if needs_level_zero {
        normalized.headings.insert(
            0,
            synthetic_level_zero_heading(path, content, &level_zero_title),
        );
    } else {
        let level_zero = &mut normalized.headings[0];
        level_zero.file_path = path.to_path_buf();
        level_zero.level = 0;
        level_zero.parent_index = None;
        level_zero.byte_start = 0;
        level_zero.byte_end = content.len();
        level_zero.line_number = Some(1);
        level_zero.title = level_zero_title;
        level_zero.title_raw = level_zero.title.clone();
        level_zero.is_root = true;
    }

    normalize_heading_parent_indexes(&mut normalized.headings);

    for heading in &mut normalized.headings {
        heading.file_path = path.to_path_buf();
    }

    normalized.file_path = path.to_path_buf();
    normalized
}

fn normalize_heading_parent_indexes(headings: &mut [ParsedHeading]) {
    if headings.is_empty() {
        return;
    }

    headings[0].level = 0;
    headings[0].parent_index = None;
    headings[0].is_root = true;

    let mut stack = vec![0usize];
    for index in 1..headings.len() {
        let current_level = headings[index].level;
        headings[index].is_root = false;

        while let Some(&parent_index) = stack.last() {
            if headings[parent_index].level < current_level {
                break;
            }
            stack.pop();
        }

        let parent_index = stack.last().copied().unwrap_or(0);
        headings[index].parent_index = Some(parent_index);
        stack.push(index);
    }
}

fn synthetic_level_zero_heading(path: &Path, content: &str, title: &str) -> ParsedHeading {
    let mut heading = ParsedHeading::new(path, 0, title.to_string(), 0, content.len());
    heading.title_raw = title.to_string();
    heading.line_number = Some(1);
    heading.is_root = true;
    heading
}

fn synthetic_level_zero_title(path: &Path, document_title: Option<&str>) -> String {
    if let Some(title) = document_title
        .map(str::trim)
        .filter(|title| !title.is_empty())
    {
        return title.to_string();
    }

    path.file_stem()
        .or_else(|| path.file_name())
        .map(|name| name.to_string_lossy().into_owned())
        .filter(|name| !name.is_empty())
        .unwrap_or_else(|| path.display().to_string())
}

fn index_document(
    connection: &Connection,
    file_id: i64,
    document: &ParsedOrgDocument,
    todo_keywords: &ResolvedTodoKeywords,
    fts5_enabled: bool,
    index_body_text: bool,
) -> Result<usize, DbWriteError> {
    if document.headings.is_empty() || document.headings[0].level != 0 {
        return Err(DbWriteError::InvalidInput(
            "normalized documents must start with a level 0 heading",
        ));
    }

    let effective_tags = effective_tags_for_document(document);
    let level0_heading = &document.headings[0];
    let level0_id = DbWriter::insert_level0_heading(
        connection,
        &heading_record(file_id, None, level0_heading, &effective_tags[0])
            .map_err(db_write_invalid_input)?,
    )?;

    let mut heading_ids = vec![level0_id];
    let mut outline_rows = vec![outline_record(
        level0_id,
        file_id,
        None,
        0,
        outline_root_materialized_path(),
        vec![level0_heading.title.clone()],
    )
    .map_err(db_write_invalid_input)?];
    let mut fts_rows = Vec::new();

    if fts5_enabled {
        fts_rows.push(HeadingFtsRecord {
            heading_id: level0_id,
            title: level0_heading.title.clone(),
            body: body_for_fts(level0_heading, index_body_text),
        });
    }

    let mut child_ordinals = vec![0usize; document.headings.len()];

    for (heading_index, heading) in document.headings.iter().enumerate().skip(1) {
        let parent_index = heading.parent_index.unwrap_or(0);
        let parent_id = heading_ids.get(parent_index).copied().ok_or_else(|| {
            DbWriteError::InvalidInput("heading parent_index must reference an earlier heading")
        })?;
        let heading_id = DbWriter::insert_headings(
            connection,
            &[heading_record(
                file_id,
                Some(parent_id),
                heading,
                &effective_tags[heading_index],
            )
            .map_err(db_write_invalid_input)?],
        )?[0];

        if heading_ids.len() != heading_index {
            return Err(DbWriteError::InvalidInput(
                "heading insertion order must match parsed heading order",
            ));
        }

        heading_ids.push(heading_id);
        let parent_outline = &outline_rows[parent_index];
        let sibling_ordinal = child_ordinals[parent_index] + 1;
        child_ordinals[parent_index] = sibling_ordinal;
        outline_rows.push(
            outline_record(
                heading_id,
                file_id,
                Some(parent_id),
                parent_outline.depth + 1,
                outline_child_materialized_path(&parent_outline.materialized_path, sibling_ordinal),
                extend_breadcrumbs(&parent_outline.breadcrumbs_json, &heading.title)
                    .map_err(db_write_invalid_input)?,
            )
            .map_err(db_write_invalid_input)?,
        );

        if fts5_enabled {
            fts_rows.push(HeadingFtsRecord {
                heading_id,
                title: heading.title.clone(),
                body: body_for_fts(heading, index_body_text),
            });
        }
    }

    let keyword_rows = document
        .metadata
        .keywords
        .iter()
        .map(|keyword| KeywordRecord {
            heading_id: level0_id,
            keyword: keyword.key.clone(),
            value: keyword.value.clone(),
            line_number: keyword.line_number.map(i64::from),
        })
        .collect::<Vec<_>>();
    let todo_rows = todo_keyword_rows(file_id, &todo_keywords.entries);
    let tag_rows = document
        .headings
        .iter()
        .enumerate()
        .flat_map(|(index, heading)| {
            let heading_id = heading_ids[index];
            heading
                .tags
                .iter()
                .cloned()
                .map(move |tag| TagRecord { heading_id, tag })
        })
        .collect::<Vec<_>>();
    let property_rows = document
        .headings
        .iter()
        .enumerate()
        .flat_map(|(index, heading)| {
            let heading_id = heading_ids[index];
            heading
                .properties
                .iter()
                .map(move |property| PropertyRecord {
                    heading_id,
                    key: property.key.clone(),
                    value: property.value.clone(),
                    source: property.source.as_db_str().to_string(),
                    append: property.append,
                    line_number: property.line_number.map(i64::from),
                })
        })
        .collect::<Vec<_>>();
    let body_rows = if index_body_text {
        document
            .headings
            .iter()
            .enumerate()
            .map(|(index, heading)| heading_body_record(heading_ids[index], heading))
            .filter_map(Result::transpose)
            .collect::<Result<Vec<_>, _>>()
            .map_err(db_write_invalid_input)?
    } else {
        Vec::new()
    };
    let timestamp_rows = document
        .headings
        .iter()
        .enumerate()
        .flat_map(|(index, heading)| {
            let heading_id = heading_ids[index];
            heading
                .timestamps
                .iter()
                .map(move |timestamp| timestamp_record(heading_id, timestamp))
        })
        .collect::<Result<Vec<_>, _>>()
        .map_err(db_write_invalid_input)?;
    let link_rows = document
        .links
        .iter()
        .map(|link| link_record(file_id, &heading_ids, &document.headings, link))
        .collect::<Result<Vec<_>, _>>()
        .map_err(db_write_invalid_input)?;

    DbWriter::insert_todo_keywords(connection, &todo_rows)?;
    DbWriter::insert_keywords(connection, &keyword_rows)?;
    DbWriter::insert_tags(connection, &tag_rows)?;
    DbWriter::insert_properties(connection, &property_rows)?;
    DbWriter::insert_outline_path(connection, &outline_rows)?;
    DbWriter::insert_heading_bodies(connection, &body_rows)?;
    DbWriter::insert_links(connection, &link_rows)?;
    let timestamp_ids = DbWriter::insert_timestamps(connection, &timestamp_rows)?;
    let timestamp_repeater_rows = document
        .headings
        .iter()
        .flat_map(|heading| heading.timestamps.iter())
        .zip(timestamp_ids.iter().copied())
        .filter_map(|(timestamp, timestamp_id)| {
            timestamp_repeater_record(timestamp_id, &timestamp.modifiers).transpose()
        })
        .collect::<Result<Vec<_>, _>>()
        .map_err(db_write_invalid_input)?;
    DbWriter::insert_timestamp_repeaters(connection, &timestamp_repeater_rows)?;

    if fts5_enabled {
        DbWriter::insert_heading_fts(connection, &fts_rows)?;
    }

    Ok(document.headings.len())
}

fn timestamp_record(
    heading_id: i64,
    timestamp: &ParsedTimestamp,
) -> Result<TimestampRecord, &'static str> {
    Ok(TimestampRecord {
        heading_id,
        role: timestamp.role.map(timestamp_role_name),
        start_ts: timestamp.start_ts,
        end_ts: timestamp.end_ts,
        timestamp_type: Some(timestamp_type_name(timestamp).to_string()),
        range_type: Some(timestamp_range_type_name(timestamp).to_string()),
        raw_value: timestamp.raw_value.clone(),
        byte_start: i64::try_from(timestamp.byte_start)
            .map_err(|_| "timestamp byte_start out of range")?,
        byte_end: i64::try_from(timestamp.byte_end)
            .map_err(|_| "timestamp byte_end out of range")?,
        line_number: timestamp.line_number.map(i64::from),
    })
}

fn link_record(
    file_id: i64,
    heading_ids: &[i64],
    headings: &[ParsedHeading],
    link: &ParsedLink,
) -> Result<LinkRecord, &'static str> {
    let heading_index = owning_heading_index(headings, link.byte_start)
        .ok_or("link byte range must attach to a heading including root")?;

    Ok(LinkRecord {
        id: None,
        file_id,
        heading_id: heading_ids
            .get(heading_index)
            .copied()
            .ok_or("link heading index must reference an inserted heading")?,
        byte_start: i64::try_from(link.byte_start).map_err(|_| "link byte_start out of range")?,
        byte_end: i64::try_from(link.byte_end).map_err(|_| "link byte_end out of range")?,
        line: i64::from(link.line),
        source_context: link.source_context.as_db_str().to_string(),
        format: link.format.clone(),
        raw: link.raw.clone(),
        raw_target: link.raw_target.clone(),
        raw_description: link.raw_description.clone(),
        link_type: link.link_type.clone(),
        path: link.path.clone(),
        search_option: link.search_option.clone(),
    })
}

fn owning_heading_index(headings: &[ParsedHeading], byte_start: usize) -> Option<usize> {
    headings
        .iter()
        .enumerate()
        .rev()
        .find(|(_, heading)| heading.byte_start <= byte_start && byte_start < heading.byte_end)
        .map(|(index, _)| index)
}

fn timestamp_repeater_record(
    timestamp_id: i64,
    modifiers: &[crate::parser::ParsedTimestampModifier],
) -> Result<Option<TimestampRepeaterRecord>, &'static str> {
    let mut row = TimestampRepeaterRecord {
        timestamp_id,
        repeater_type: None,
        repeater_value: None,
        repeater_unit: None,
        repeater_deadline_value: None,
        repeater_deadline_unit: None,
        warning_type: None,
        warning_value: None,
        warning_unit: None,
    };

    for modifier in modifiers {
        match modifier.kind {
            ParsedTimestampModifierKind::Repeater => {
                if row.repeater_type.is_some() {
                    return Err("timestamp modifiers must not contain multiple repeater entries");
                }
                row.repeater_type = Some(repeater_modifier_type_name(modifier.modifier_type)?);
                row.repeater_value = Some(modifier.value);
                row.repeater_unit = Some(timestamp_unit_name(modifier.unit).to_string());
                row.repeater_deadline_value = modifier.repeater_deadline_value;
                row.repeater_deadline_unit = modifier
                    .repeater_deadline_unit
                    .map(|unit| timestamp_unit_name(unit).to_string());
            }
            ParsedTimestampModifierKind::Warning => {
                if row.warning_type.is_some() {
                    return Err("timestamp modifiers must not contain multiple warning entries");
                }
                row.warning_type = Some(warning_modifier_type_name(modifier.modifier_type)?);
                row.warning_value = Some(modifier.value);
                row.warning_unit = Some(timestamp_unit_name(modifier.unit).to_string());
            }
        }
    }

    if row.repeater_type.is_none() && row.warning_type.is_none() {
        return Ok(None);
    }

    Ok(Some(row))
}

fn heading_record(
    file_id: i64,
    parent_id: Option<i64>,
    heading: &ParsedHeading,
    effective_tags: &[String],
) -> Result<HeadingRecord, &'static str> {
    let todo_type = heading.todo_type.as_ref().map(|value| match value {
        TodoType::Open => "open".to_string(),
        TodoType::Closed => "closed".to_string(),
    });

    Ok(HeadingRecord {
        id: None,
        file_id,
        parent_id,
        level: i64::from(heading.level),
        line_number: heading.line_number.map(i64::from),
        byte_start: if heading.level == 0 {
            -1
        } else {
            i64::try_from(heading.byte_start).map_err(|_| "byte_start out of range")?
        },
        byte_end: i64::try_from(heading.byte_end).map_err(|_| "byte_end out of range")?,
        title: heading.title.clone(),
        title_raw: heading.title_raw.clone(),
        todo_keyword: heading.todo_keyword.clone(),
        todo_type,
        priority: heading.priority,
        scheduled_raw: heading.planning.scheduled_raw().map(str::to_string),
        scheduled_ts: heading.planning.scheduled_ts(),
        deadline_raw: heading.planning.deadline_raw().map(str::to_string),
        deadline_ts: heading.planning.deadline_ts(),
        closed_raw: heading.planning.closed_raw().map(str::to_string),
        closed_ts: heading.planning.closed_ts(),
        archivedp: heading.is_archived,
        footnote_section_p: false,
        all_tags_json: serde_json::to_string(effective_tags)
            .map_err(|_| "tag serialization failed")?,
    })
}

fn timestamp_role_name(role: ParsedTimestampRole) -> String {
    match role {
        ParsedTimestampRole::Scheduled => "scheduled".to_string(),
        ParsedTimestampRole::Deadline => "deadline".to_string(),
        ParsedTimestampRole::Closed => "closed".to_string(),
        ParsedTimestampRole::Body => "body".to_string(),
    }
}

fn timestamp_type_name(timestamp: &ParsedTimestamp) -> &'static str {
    match timestamp.timestamp_type {
        crate::parser::ParsedTimestampType::Active => "active",
        crate::parser::ParsedTimestampType::Inactive => "inactive",
        crate::parser::ParsedTimestampType::Diary => "diary",
    }
}

fn timestamp_range_type_name(timestamp: &ParsedTimestamp) -> &'static str {
    match timestamp.range_type {
        crate::parser::ParsedTimestampRangeType::None => "none",
        crate::parser::ParsedTimestampRangeType::DateRange => "date_range",
        crate::parser::ParsedTimestampRangeType::TimeRange => "time_range",
        crate::parser::ParsedTimestampRangeType::DateTimeRange => "datetime_range",
        crate::parser::ParsedTimestampRangeType::Unknown => "unknown",
    }
}

fn repeater_modifier_type_name(
    modifier_type: ParsedTimestampModifierType,
) -> Result<String, &'static str> {
    match modifier_type {
        ParsedTimestampModifierType::Cumulate => Ok("cumulate".to_string()),
        ParsedTimestampModifierType::CatchUp => Ok("catch_up".to_string()),
        ParsedTimestampModifierType::Restart => Ok("restart".to_string()),
        ParsedTimestampModifierType::All | ParsedTimestampModifierType::First => {
            Err("warning modifier type cannot be stored as a repeater")
        }
    }
}

fn warning_modifier_type_name(
    modifier_type: ParsedTimestampModifierType,
) -> Result<String, &'static str> {
    match modifier_type {
        ParsedTimestampModifierType::All => Ok("all".to_string()),
        ParsedTimestampModifierType::First => Ok("first".to_string()),
        ParsedTimestampModifierType::Cumulate
        | ParsedTimestampModifierType::CatchUp
        | ParsedTimestampModifierType::Restart => {
            Err("repeater modifier type cannot be stored as a warning")
        }
    }
}

fn timestamp_unit_name(unit: ParsedTimestampUnit) -> &'static str {
    match unit {
        ParsedTimestampUnit::Hour => "hour",
        ParsedTimestampUnit::Day => "day",
        ParsedTimestampUnit::Week => "week",
        ParsedTimestampUnit::Month => "month",
        ParsedTimestampUnit::Year => "year",
    }
}

fn effective_tags_for_document(document: &ParsedOrgDocument) -> Vec<Vec<String>> {
    let mut effective_tags: Vec<Vec<String>> = Vec::with_capacity(document.headings.len());

    for heading in &document.headings {
        let inherited = heading
            .parent_index
            .and_then(|index| effective_tags.get(index))
            .cloned()
            .unwrap_or_default();
        effective_tags.push(merge_effective_tags(&inherited, &heading.tags));
    }

    effective_tags
}

fn merge_effective_tags(inherited: &[String], local: &[String]) -> Vec<String> {
    let mut merged = Vec::with_capacity(inherited.len() + local.len());

    for tag in inherited.iter().chain(local.iter()) {
        if !merged.iter().any(|existing| existing == tag) {
            merged.push(tag.clone());
        }
    }

    merged
}

fn todo_keyword_rows(
    file_id: i64,
    todo_keywords: &[ResolvedTodoKeywordEntry],
) -> Vec<TodoKeywordRecord> {
    todo_keywords
        .iter()
        .map(|keyword| TodoKeywordRecord {
            file_id,
            keyword: keyword.keyword.clone(),
            state_type: keyword.state_type.clone(),
            shortcut: keyword.shortcut,
            sequence_no: keyword.sequence_no,
            source_kind: keyword.source_kind.as_db_str().to_string(),
            source_keyword: keyword.source_keyword.clone(),
            source_line_number: keyword.source_line_number.map(i64::from),
        })
        .collect()
}

fn outline_record(
    heading_id: i64,
    file_id: i64,
    parent_id: Option<i64>,
    depth: i64,
    materialized_path: String,
    breadcrumbs: Vec<String>,
) -> Result<OutlinePathRecord, &'static str> {
    Ok(OutlinePathRecord {
        heading_id,
        file_id,
        parent_id,
        depth,
        materialized_path,
        breadcrumbs_json: serde_json::to_string(&breadcrumbs)
            .map_err(|_| "outline breadcrumb serialization failed")?,
    })
}

fn extend_breadcrumbs(breadcrumbs_json: &str, title: &str) -> Result<Vec<String>, &'static str> {
    let mut breadcrumbs: Vec<String> = serde_json::from_str(breadcrumbs_json)
        .map_err(|_| "outline breadcrumb deserialization failed")?;
    breadcrumbs.push(title.to_string());
    Ok(breadcrumbs)
}

fn zero_pad_path_segment(value: usize) -> String {
    format!("{value:04}")
}

fn outline_root_materialized_path() -> String {
    zero_pad_path_segment(0)
}

fn outline_child_materialized_path(parent_path: &str, sibling_ordinal: usize) -> String {
    format!("{parent_path}.{}", zero_pad_path_segment(sibling_ordinal))
}

fn heading_body_record(
    heading_id: i64,
    heading: &ParsedHeading,
) -> Result<Option<HeadingBodyRecord>, &'static str> {
    let Some(body_text) = heading.body_text.clone() else {
        return Ok(None);
    };

    Ok(Some(HeadingBodyRecord {
        heading_id,
        body_text,
        body_byte_start: heading
            .body_byte_start
            .map(i64::try_from)
            .transpose()
            .map_err(|_| "heading body_byte_start out of range")?,
        body_byte_end: heading
            .body_byte_end
            .map(i64::try_from)
            .transpose()
            .map_err(|_| "heading body_byte_end out of range")?,
    }))
}

fn body_for_fts(heading: &ParsedHeading, index_body_text: bool) -> String {
    if !index_body_text {
        return String::new();
    }

    heading.body_text.clone().unwrap_or_default()
}

fn db_write_invalid_input(message: &'static str) -> DbWriteError {
    DbWriteError::InvalidInput(message)
}

#[cfg(test)]
mod tests {
    use super::{IndexedFile, Indexer, IndexerError};
    use crate::{
        config::{Config, SearchConfig},
        db::{
            open_in_memory_database_with_schema, sqlite_supports_fts5, DbReader, DbWriter,
            FileRecordInput, HeadingRecord, SchemaDefinition, CURRENT_SCHEMA_VERSION,
        },
        link_resolver::{
            CUSTOM_ID_MISSING_DIAGNOSTIC, DUPLICATE_ID_DIAGNOSTIC, FILE_MISSING_DIAGNOSTIC,
            FILE_OUTSIDE_UNIVERSE_DIAGNOSTIC, HEADING_TITLE_MISSING_DIAGNOSTIC,
            ID_MISSING_DIAGNOSTIC, SAME_FILE_STAR_HEADING_MISSING_DIAGNOSTIC,
            UNSUPPORTED_DIAGNOSTIC,
        },
        parser::{OrgParserCore, OrgizeAdapter, ParseDiagnostic, ParseOptions, ParsedOrgDocument},
    };
    use rusqlite::Connection;
    use std::{
        fs,
        path::{Path, PathBuf},
        time::{SystemTime, UNIX_EPOCH},
    };

    #[derive(Debug, PartialEq, Eq)]
    struct StoredLinkRow {
        heading_title: String,
        format: String,
        raw: String,
        raw_target: String,
        raw_description: Option<String>,
        link_type: String,
        path: String,
        search_option: Option<String>,
        source_context: String,
    }

    type TimestampRow = (String, String, String, String, Option<i64>, Option<i64>);
    type RepeaterRow = (
        String,
        Option<String>,
        Option<i64>,
        Option<String>,
        Option<i64>,
        Option<String>,
        Option<String>,
        Option<i64>,
        Option<String>,
    );
    type PropertyRow = (i64, String, Option<String>, String, i64, Option<i64>);
    type KeywordRow = (String, Option<String>, Option<i64>);
    type TodoProvenanceRow = (
        String,
        String,
        Option<String>,
        i64,
        String,
        Option<String>,
        Option<i64>,
    );
    type LinkResolutionRow = (String, Option<String>, Option<String>, Option<String>);
    type TargetRemovalLinkRow = (String, String, Option<i64>, Option<String>, Option<String>);
    type FileHeadingSearchResolutionRow = (
        String,
        Option<String>,
        Option<String>,
        Option<String>,
        Option<String>,
        Option<String>,
    );
    type SameFileStarHeadingResolutionRow = (
        String,
        Option<String>,
        Option<String>,
        Option<String>,
        Option<String>,
        Option<String>,
        Option<String>,
    );
    type SameFileCustomIdResolutionRow = (
        String,
        Option<String>,
        Option<String>,
        Option<String>,
        Option<String>,
        Option<String>,
        Option<String>,
    );
    type FileContextCustomIdResolutionRow = (
        String,
        Option<String>,
        Option<String>,
        Option<String>,
        Option<String>,
        Option<String>,
        Option<String>,
    );
    type OrgIdResolutionRow = (
        String,
        Option<String>,
        Option<String>,
        Option<String>,
        Option<String>,
        Option<String>,
        Option<String>,
    );

    struct TestDir {
        path: PathBuf,
    }

    impl TestDir {
        fn new(name: &str) -> Self {
            let unique = SystemTime::now()
                .duration_since(UNIX_EPOCH)
                .expect("system time should be after unix epoch")
                .as_nanos();
            let path = std::env::temp_dir().join(format!(
                "org-files-db-indexer-tests-{}-{}-{}",
                name,
                std::process::id(),
                unique
            ));
            fs::create_dir_all(&path).expect("test dir should be created");
            Self { path }
        }

        fn path(&self) -> &Path {
            &self.path
        }
    }

    impl Drop for TestDir {
        fn drop(&mut self) {
            let _ = fs::remove_dir_all(&self.path);
        }
    }

    fn write_file(path: &Path, content: &str) {
        if let Some(parent) = path.parent() {
            fs::create_dir_all(parent).expect("parent dir should exist");
        }
        fs::write(path, content).expect("file should be written");
    }

    fn write_config(path: &Path, body: &str) {
        write_file(path, body);
    }

    fn seed_indexed_file(connection: &Connection) {
        let file_id = DbWriter::upsert_file(
            connection,
            &FileRecordInput {
                path: PathBuf::from("/tmp/existing.org"),
                mtime_ns: 1,
                size: 1,
                content_hash: None,
                indexed_at: None,
            },
        )
        .expect("file should insert");

        DbWriter::insert_level0_heading(
            connection,
            &HeadingRecord {
                id: None,
                file_id,
                parent_id: None,
                level: 0,
                line_number: Some(1),
                byte_start: -1,
                byte_end: 1,
                title: "Existing".to_string(),
                title_raw: "Existing".to_string(),
                todo_keyword: None,
                todo_type: None,
                priority: None,
                scheduled_raw: None,
                scheduled_ts: None,
                deadline_raw: None,
                deadline_ts: None,
                closed_raw: None,
                closed_ts: None,
                archivedp: false,
                footnote_section_p: false,
                all_tags_json: "[]".to_string(),
            },
        )
        .expect("heading should insert");
    }

    #[test]
    fn rebuild_processes_org_files_from_loaded_config() {
        let test_dir = TestDir::new("rebuild-from-config");
        let notes_dir = test_dir.path().join("notes");
        let db_path = test_dir.path().join("db.sqlite");
        let config_path = test_dir.path().join("config.toml");
        let org_path = notes_dir.join("project.org");

        write_file(
            &org_path,
            "#+TITLE: Project Notes\n#+TODO: PLAN(p) | DONE(d)\n* PLAN Inbox :rust:\n:PROPERTIES:\n:CUSTOM_ID: inbox\n:END:\nSCHEDULED: <2026-06-18 Thu>\n",
        );
        write_config(
            &config_path,
            r#"
db_path = "db.sqlite"
[[dirs]]
path = "notes/../notes"
recursive = true

[todo]
default_open_keywords = ["TODO(t)"]
default_closed_keywords = ["DONE(d)"]

[search]
fts5_enabled = false
index_body_text = false
"#,
        );

        let report = Indexer::new(OrgizeAdapter::new())
            .rebuild_from_config_path(&config_path)
            .expect("rebuild should succeed");

        assert_eq!(
            report.indexed_files,
            vec![IndexedFile {
                path: org_path.clone(),
                file_id: 1,
                heading_count: 2,
            }]
        );
        assert!(report.diagnostics.is_empty());

        let connection = Connection::open(&db_path).expect("db should open");
        let headings = DbReader::list_headings(&connection).expect("headings should load");
        assert_eq!(headings.len(), 2);
        assert_eq!(headings[0].level, 0);
        assert_eq!(headings[0].title, "Project Notes");
        assert_eq!(headings[1].title, "Inbox");
        assert_eq!(headings[1].todo_keyword.as_deref(), Some("PLAN"));
        assert_eq!(headings[1].todo_type.as_deref(), Some("open"));
        assert_eq!(headings[1].all_tags_json, "[\"rust\"]");

        let todo_rows: Vec<TodoProvenanceRow> = query_rows(
            &connection,
            "SELECT keyword, state_type, shortcut, sequence_no, source_kind, source_keyword, source_line_number
             FROM todo_keywords
             ORDER BY sequence_no",
            |row| {
                Ok((
                    row.get(0)?,
                    row.get(1)?,
                    row.get(2)?,
                    row.get(3)?,
                    row.get(4)?,
                    row.get(5)?,
                    row.get(6)?,
                ))
            },
        );
        assert_eq!(
            todo_rows,
            vec![
                (
                    "PLAN".to_string(),
                    "open".to_string(),
                    Some("p".to_string()),
                    0,
                    "org_keyword".to_string(),
                    Some("TODO".to_string()),
                    Some(2),
                ),
                (
                    "DONE".to_string(),
                    "closed".to_string(),
                    Some("d".to_string()),
                    1,
                    "org_keyword".to_string(),
                    Some("TODO".to_string()),
                    Some(2),
                ),
            ]
        );

        let keywords: Vec<(String, Option<String>)> = query_rows(
            &connection,
            "SELECT keyword, value FROM keywords ORDER BY keyword",
            |row| Ok((row.get(0)?, row.get(1)?)),
        );
        assert_eq!(
            keywords,
            vec![
                ("TITLE".to_string(), Some("Project Notes".to_string())),
                ("TODO".to_string(), Some("PLAN(p) | DONE(d)".to_string())),
            ]
        );

        let properties: Vec<(String, Option<String>, String)> = query_rows(
            &connection,
            "SELECT key, value, source FROM properties",
            |row| Ok((row.get(0)?, row.get(1)?, row.get(2)?)),
        );
        assert_eq!(
            properties,
            vec![(
                "CUSTOM_ID".to_string(),
                Some("inbox".to_string()),
                "property_drawer".to_string(),
            )]
        );
    }

    #[test]
    fn rebuild_can_mix_recursive_and_non_recursive_directory_roots() {
        let test_dir = TestDir::new("mixed-dir-recursion");
        let recursive_dir = test_dir.path().join("notes");
        let non_recursive_dir = test_dir.path().join("inbox");
        let config_path = test_dir.path().join("config.toml");

        let recursive_root = recursive_dir.join("root.org");
        let recursive_child = recursive_dir.join("nested/child.org");
        let non_recursive_root = non_recursive_dir.join("top.org");
        let non_recursive_child = non_recursive_dir.join("nested/skipped.org");

        write_file(&recursive_root, "* Recursive root\n");
        write_file(&recursive_child, "* Recursive child\n");
        write_file(&non_recursive_root, "* Inbox root\n");
        write_file(&non_recursive_child, "* Inbox child\n");

        write_config(
            &config_path,
            r#"
db_path = "db.sqlite"

[[dirs]]
path = "notes"
recursive = true

[[dirs]]
path = "inbox"
recursive = false

[search]
fts5_enabled = false
index_body_text = false
"#,
        );

        let report = Indexer::new(OrgizeAdapter::new())
            .rebuild_from_config_path(&config_path)
            .expect("rebuild should succeed");

        assert_eq!(
            report
                .indexed_files
                .iter()
                .map(|file| file.path.clone())
                .collect::<Vec<_>>(),
            vec![non_recursive_root, recursive_child, recursive_root]
        );
    }

    #[test]
    fn rebuild_defaults_directory_entries_to_non_recursive() {
        let test_dir = TestDir::new("default-dir-recursion");
        let notes_dir = test_dir.path().join("notes");
        let db_path = test_dir.path().join("db.sqlite");
        let config_path = test_dir.path().join("config.toml");
        let root_file = notes_dir.join("root.org");
        let nested_file = notes_dir.join("nested/skipped.org");

        write_file(&root_file, "* Root\n");
        write_file(&nested_file, "* Nested\n");

        write_config(
            &config_path,
            r#"
db_path = "db.sqlite"

[[dirs]]
path = "notes"

[search]
fts5_enabled = false
index_body_text = false
"#,
        );

        let report = Indexer::new(OrgizeAdapter::new())
            .rebuild_from_config_path(&config_path)
            .expect("rebuild should succeed");

        assert_eq!(
            report
                .indexed_files
                .iter()
                .map(|file| file.path.clone())
                .collect::<Vec<_>>(),
            vec![root_file]
        );

        let connection = Connection::open(&db_path).expect("db should open");
        let file_count: i64 = connection
            .query_row("SELECT COUNT(*) FROM files", [], |row| row.get(0))
            .expect("file count should load");
        assert_eq!(file_count, 1);
    }

    #[test]
    fn rebuild_persists_level_zero_and_duplicate_direct_properties() {
        let test_dir = TestDir::new("rebuild-properties");
        let notes_dir = test_dir.path().join("notes");
        let db_path = test_dir.path().join("db.sqlite");
        let config_path = test_dir.path().join("config.toml");
        let org_path = notes_dir.join("properties.org");

        write_file(
            &org_path,
            ":PROPERTIES:\n:CATEGORY: Level 0 Category Property\n:var+: root\n:END:\n#+TITLE: Project Notes\n#+PROPERTY: Effort_ALL 0:10 0:30 1:00\n* TODO Inbox :rust:\n:PROPERTIES:\n:CUSTOM_ID: inbox\n:Owner: Alice\n:owner: Bob\n:var+: baz=3\n:END:\n#+PROPERTY: var+ bar=2\n#+CATEGORY: project\n",
        );
        write_config(
            &config_path,
            r#"
db_path = "db.sqlite"
[[dirs]]
path = "notes"
recursive = true

[todo]
default_open_keywords = ["TODO(t)"]
default_closed_keywords = ["DONE(d)"]

[search]
fts5_enabled = false
index_body_text = false
"#,
        );

        let report = Indexer::new(OrgizeAdapter::new())
            .rebuild_from_config_path(&config_path)
            .expect("rebuild should succeed");

        assert_eq!(report.indexed_files.len(), 1);
        assert!(report.diagnostics.is_empty());

        let connection = Connection::open(&db_path).expect("db should open");
        let properties: Vec<PropertyRow> = query_rows(
            &connection,
            "SELECT headings.level, properties.key, properties.value, properties.source, properties.append, properties.line_number
             FROM properties
             INNER JOIN headings ON headings.id = properties.heading_id
             ORDER BY headings.level, properties.line_number, properties.id",
            |row| {
                Ok((
                    row.get(0)?,
                    row.get(1)?,
                    row.get(2)?,
                    row.get(3)?,
                    row.get(4)?,
                    row.get(5)?,
                ))
            },
        );
        assert_eq!(
            properties,
            vec![
                (
                    0,
                    "CATEGORY".to_string(),
                    Some("Level 0 Category Property".to_string()),
                    "property_drawer".to_string(),
                    0,
                    Some(2),
                ),
                (
                    0,
                    "VAR".to_string(),
                    Some("root".to_string()),
                    "property_drawer".to_string(),
                    1,
                    Some(3),
                ),
                (
                    0,
                    "EFFORT_ALL".to_string(),
                    Some("0:10 0:30 1:00".to_string()),
                    "property_keyword".to_string(),
                    0,
                    Some(6),
                ),
                (
                    0,
                    "VAR".to_string(),
                    Some("bar=2".to_string()),
                    "property_keyword".to_string(),
                    1,
                    Some(14),
                ),
                (
                    0,
                    "CATEGORY".to_string(),
                    Some("project".to_string()),
                    "category_keyword".to_string(),
                    0,
                    Some(15),
                ),
                (
                    1,
                    "CUSTOM_ID".to_string(),
                    Some("inbox".to_string()),
                    "property_drawer".to_string(),
                    0,
                    Some(9),
                ),
                (
                    1,
                    "OWNER".to_string(),
                    Some("Alice".to_string()),
                    "property_drawer".to_string(),
                    0,
                    Some(10),
                ),
                (
                    1,
                    "OWNER".to_string(),
                    Some("Bob".to_string()),
                    "property_drawer".to_string(),
                    0,
                    Some(11),
                ),
                (
                    1,
                    "VAR".to_string(),
                    Some("baz=3".to_string()),
                    "property_drawer".to_string(),
                    1,
                    Some(12),
                ),
            ]
        );

        let raw_keywords: Vec<KeywordRow> = query_rows(
            &connection,
            "SELECT keyword, value, line_number
             FROM keywords
             WHERE keyword IN ('PROPERTY', 'CATEGORY')
             ORDER BY line_number, rowid",
            |row| Ok((row.get(0)?, row.get(1)?, row.get(2)?)),
        );
        assert_eq!(
            raw_keywords,
            vec![
                (
                    "PROPERTY".to_string(),
                    Some("Effort_ALL 0:10 0:30 1:00".to_string()),
                    Some(6),
                ),
                (
                    "PROPERTY".to_string(),
                    Some("var+ bar=2".to_string()),
                    Some(14),
                ),
                (
                    "CATEGORY".to_string(),
                    Some("project".to_string()),
                    Some(15),
                ),
            ]
        );
    }

    #[test]
    fn rebuild_indexes_late_file_level_keywords_on_level_zero_heading() {
        let test_dir = TestDir::new("rebuild-late-file-level-keywords");
        let notes_dir = test_dir.path().join("notes");
        let db_path = test_dir.path().join("db.sqlite");
        let config_path = test_dir.path().join("config.toml");
        let org_path = notes_dir.join("late-file-keywords.org");
        let content =
            include_str!("../tests/data/parser/properties/late-file-keywords/fixture.org");

        write_file(&org_path, content);
        write_config(
            &config_path,
            r#"
db_path = "db.sqlite"
[[dirs]]
path = "notes"
recursive = true

[todo]
default_open_keywords = ["TODO(t)"]
default_closed_keywords = ["DONE(d)"]

[search]
fts5_enabled = false
index_body_text = false
"#,
        );

        let report = Indexer::new(OrgizeAdapter::new())
            .rebuild_from_config_path(&config_path)
            .expect("rebuild should succeed");

        assert_eq!(report.indexed_files.len(), 1);
        assert!(report.diagnostics.is_empty());

        let connection = Connection::open(&db_path).expect("db should open");
        let headings = DbReader::list_headings(&connection).expect("headings should load");
        assert_eq!(headings.len(), 4);
        assert_eq!(
            headings
                .iter()
                .map(|heading| heading.title.clone())
                .collect::<Vec<_>>(),
            vec![
                "Keyword and Property Normalization Fixture Later Title".to_string(),
                "First heading".to_string(),
                "Child heading".to_string(),
                "Second heading".to_string(),
            ]
        );
        assert_eq!(headings[1].parent_id, Some(headings[0].id));
        assert_eq!(headings[2].parent_id, Some(headings[1].id));
        assert_eq!(headings[3].parent_id, Some(headings[0].id));

        let properties: Vec<PropertyRow> = query_rows(
            &connection,
            "SELECT headings.level, properties.key, properties.value, properties.source, properties.append, properties.line_number
             FROM properties
             INNER JOIN headings ON headings.id = properties.heading_id
             ORDER BY properties.line_number, properties.id",
            |row| {
                Ok((
                    row.get(0)?,
                    row.get(1)?,
                    row.get(2)?,
                    row.get(3)?,
                    row.get(4)?,
                    row.get(5)?,
                ))
            },
        );
        assert_eq!(
            properties,
            vec![
                (
                    0,
                    "BEFORE_PROP".to_string(),
                    Some("before-value".to_string()),
                    "property_keyword".to_string(),
                    0,
                    Some(3),
                ),
                (
                    0,
                    "CATEGORY".to_string(),
                    Some("before-category".to_string()),
                    "category_keyword".to_string(),
                    0,
                    Some(4),
                ),
                (
                    0,
                    "AFTER_PROP".to_string(),
                    Some("after-value".to_string()),
                    "property_keyword".to_string(),
                    0,
                    Some(11),
                ),
                (
                    0,
                    "REPEATED_PROP".to_string(),
                    Some("first".to_string()),
                    "property_keyword".to_string(),
                    0,
                    Some(12),
                ),
                (
                    0,
                    "REPEATED_PROP".to_string(),
                    Some("second".to_string()),
                    "property_keyword".to_string(),
                    0,
                    Some(13),
                ),
                (
                    0,
                    "APPENDED_PROP".to_string(),
                    Some("base".to_string()),
                    "property_keyword".to_string(),
                    0,
                    Some(14),
                ),
                (
                    0,
                    "APPENDED_PROP".to_string(),
                    Some("extra".to_string()),
                    "property_keyword".to_string(),
                    1,
                    Some(15),
                ),
                (
                    0,
                    "CATEGORY".to_string(),
                    Some("after-category".to_string()),
                    "category_keyword".to_string(),
                    0,
                    Some(16),
                ),
                (
                    0,
                    "SECOND_AFTER_HEADING".to_string(),
                    Some("works".to_string()),
                    "property_keyword".to_string(),
                    0,
                    Some(27),
                ),
                (
                    0,
                    "CATEGORY".to_string(),
                    Some("second-category".to_string()),
                    "category_keyword".to_string(),
                    0,
                    Some(28),
                ),
            ]
        );

        let raw_keywords: Vec<KeywordRow> = query_rows(
            &connection,
            "SELECT keyword, value, line_number
             FROM keywords
             ORDER BY line_number, rowid",
            |row| Ok((row.get(0)?, row.get(1)?, row.get(2)?)),
        );
        assert_eq!(
            raw_keywords,
            vec![
                (
                    "TITLE".to_string(),
                    Some("Keyword and Property Normalization Fixture".to_string()),
                    Some(1),
                ),
                ("STARTUP".to_string(), Some("showall".to_string()), Some(2)),
                (
                    "PROPERTY".to_string(),
                    Some("before_prop before-value".to_string()),
                    Some(3),
                ),
                (
                    "CATEGORY".to_string(),
                    Some("before-category".to_string()),
                    Some(4),
                ),
                (
                    "AUTHOR".to_string(),
                    Some("Later Author".to_string()),
                    Some(9),
                ),
                (
                    "OPTIONS".to_string(),
                    Some("toc:nil num:t".to_string()),
                    Some(10),
                ),
                (
                    "PROPERTY".to_string(),
                    Some("after_prop after-value".to_string()),
                    Some(11),
                ),
                (
                    "PROPERTY".to_string(),
                    Some("repeated_prop first".to_string()),
                    Some(12),
                ),
                (
                    "PROPERTY".to_string(),
                    Some("repeated_prop second".to_string()),
                    Some(13),
                ),
                (
                    "PROPERTY".to_string(),
                    Some("appended_prop base".to_string()),
                    Some(14),
                ),
                (
                    "PROPERTY".to_string(),
                    Some("appended_prop+ extra".to_string()),
                    Some(15),
                ),
                (
                    "CATEGORY".to_string(),
                    Some("after-category".to_string()),
                    Some(16),
                ),
                (
                    "TITLE".to_string(),
                    Some("Later Title".to_string()),
                    Some(21),
                ),
                (
                    "EXPORT_FILE_NAME".to_string(),
                    Some("later-export-name".to_string()),
                    Some(22),
                ),
                (
                    "PROPERTY".to_string(),
                    Some("second_after_heading works".to_string()),
                    Some(27),
                ),
                (
                    "CATEGORY".to_string(),
                    Some("second-category".to_string()),
                    Some(28),
                ),
            ]
        );
    }

    #[test]
    fn rebuild_persists_one_synthetic_root_row_per_file_with_db_sentinels() {
        let test_dir = TestDir::new("synthetic-root-sentinels");
        let notes_dir = test_dir.path().join("notes");
        let db_path = test_dir.path().join("db.sqlite");
        let config_path = test_dir.path().join("config.toml");
        let alpha_path = notes_dir.join("alpha.org");
        let beta_path = notes_dir.join("nested/beta.org");

        write_file(
            &alpha_path,
            "#+TITLE: Alpha Root\n* Alpha Top\n** Alpha Child\n",
        );
        write_file(&beta_path, "* Beta Top\n");
        write_config(
            &config_path,
            r#"
db_path = "db.sqlite"
[[dirs]]
path = "notes"
recursive = true

[search]
fts5_enabled = false
index_body_text = false
"#,
        );

        let report = Indexer::new(OrgizeAdapter::new())
            .rebuild_from_config_path(&config_path)
            .expect("rebuild should succeed");

        assert_eq!(report.indexed_files.len(), 2);
        assert!(report.diagnostics.is_empty());

        let connection = Connection::open(&db_path).expect("db should open");
        let headings: Vec<(String, i64, Option<i64>, i64, i64, String)> = query_rows(
            &connection,
            "SELECT files.path, headings.id, headings.parent_id, headings.level, headings.byte_start, headings.title
             FROM headings
             INNER JOIN files ON files.id = headings.file_id
             ORDER BY files.path, headings.level, headings.byte_start, headings.id",
            |row| {
                Ok((
                    row.get(0)?,
                    row.get(1)?,
                    row.get(2)?,
                    row.get(3)?,
                    row.get(4)?,
                    row.get(5)?,
                ))
            },
        );

        let alpha_rows = headings
            .iter()
            .filter(|(path, ..)| path == &alpha_path.display().to_string())
            .collect::<Vec<_>>();
        let beta_rows = headings
            .iter()
            .filter(|(path, ..)| path == &beta_path.display().to_string())
            .collect::<Vec<_>>();

        assert_eq!(alpha_rows.len(), 3);
        assert_eq!(beta_rows.len(), 2);

        let alpha_root = alpha_rows
            .iter()
            .find(|(_, _, _, level, _, _)| *level == 0)
            .expect("alpha root should exist");
        let alpha_top = alpha_rows
            .iter()
            .find(|(_, _, _, level, _, title)| *level == 1 && title == "Alpha Top")
            .expect("alpha top heading should exist");
        let alpha_child = alpha_rows
            .iter()
            .find(|(_, _, _, level, _, title)| *level == 2 && title == "Alpha Child")
            .expect("alpha child heading should exist");
        let beta_root = beta_rows
            .iter()
            .find(|(_, _, _, level, _, _)| *level == 0)
            .expect("beta root should exist");
        let beta_top = beta_rows
            .iter()
            .find(|(_, _, _, level, _, title)| *level == 1 && title == "Beta Top")
            .expect("beta top heading should exist");

        assert_eq!(alpha_root.2, None);
        assert_eq!(alpha_root.4, -1);
        assert_eq!(alpha_root.5, "Alpha Root");
        assert_eq!(alpha_top.2, Some(alpha_root.1));
        assert_eq!(alpha_top.4, 20);
        assert_eq!(alpha_child.2, Some(alpha_top.1));
        assert_eq!(alpha_child.4, 32);

        assert_eq!(beta_root.2, None);
        assert_eq!(beta_root.4, -1);
        assert_eq!(beta_root.5, "beta");
        assert_eq!(beta_top.2, Some(beta_root.1));
        assert_eq!(beta_top.4, 0);

        let root_outline_rows: Vec<(String, i64, Option<i64>, String, String)> = query_rows(
            &connection,
            "SELECT files.path, outline_path.depth, outline_path.parent_id, outline_path.materialized_path, outline_path.breadcrumbs_json
             FROM outline_path
             INNER JOIN files ON files.id = outline_path.file_id
             WHERE outline_path.depth = 0
             ORDER BY files.path",
            |row| {
                Ok((
                    row.get(0)?,
                    row.get(1)?,
                    row.get(2)?,
                    row.get(3)?,
                    row.get(4)?,
                ))
            },
        );

        assert_eq!(
            root_outline_rows,
            vec![
                (
                    alpha_path.display().to_string(),
                    0,
                    None,
                    "0000".to_string(),
                    "[\"Alpha Root\"]".to_string(),
                ),
                (
                    beta_path.display().to_string(),
                    0,
                    None,
                    "0000".to_string(),
                    "[\"beta\"]".to_string(),
                ),
            ]
        );
    }

    #[test]
    fn rebuild_stores_generic_raw_keywords_on_the_synthetic_level_zero_heading() {
        let test_dir = TestDir::new("rebuild-generic-raw-keywords");
        let notes_dir = test_dir.path().join("notes");
        let db_path = test_dir.path().join("db.sqlite");
        let config_path = test_dir.path().join("config.toml");
        let org_path = notes_dir.join("raw-generic-keywords.org");
        let content =
            include_str!("../tests/data/parser/file-scope/raw-generic-keywords/fixture.org");

        write_file(&org_path, content);
        write_config(
            &config_path,
            r#"
db_path = "db.sqlite"
[[dirs]]
path = "notes"
recursive = true

[search]
fts5_enabled = false
index_body_text = false
"#,
        );

        let report = Indexer::new(OrgizeAdapter::new())
            .rebuild_from_config_path(&config_path)
            .expect("rebuild should succeed");

        assert_eq!(report.indexed_files.len(), 1);
        assert!(report.diagnostics.is_empty());

        let connection = Connection::open(&db_path).expect("db should open");
        let headings = DbReader::list_headings(&connection).expect("headings should load");
        assert_eq!(headings.len(), 3);
        assert_eq!(
            headings
                .iter()
                .map(|heading| heading.title.clone())
                .collect::<Vec<_>>(),
            vec![
                "First title Later title".to_string(),
                "First heading".to_string(),
                "Child heading".to_string(),
            ]
        );
        assert_eq!(headings[1].parent_id, Some(headings[0].id));
        assert_eq!(headings[2].parent_id, Some(headings[1].id));

        let raw_keywords: Vec<(i64, String, Option<String>, Option<i64>)> = query_rows(
            &connection,
            "SELECT headings.level, keywords.keyword, keywords.value, keywords.line_number
             FROM keywords
             INNER JOIN headings ON headings.id = keywords.heading_id
             ORDER BY keywords.line_number, keywords.id",
            |row| Ok((row.get(0)?, row.get(1)?, row.get(2)?, row.get(3)?)),
        );
        assert_eq!(
            raw_keywords,
            vec![
                (
                    0,
                    "TITLE".to_string(),
                    Some("First title".to_string()),
                    Some(1),
                ),
                (
                    0,
                    "STARTUP".to_string(),
                    Some("showall".to_string()),
                    Some(2),
                ),
                (
                    0,
                    "AUTHOR".to_string(),
                    Some("Jane Doe".to_string()),
                    Some(7),
                ),
                (
                    0,
                    "OPTIONS".to_string(),
                    Some("toc:nil num:t".to_string()),
                    Some(8),
                ),
                (
                    0,
                    "TITLE".to_string(),
                    Some("Later title".to_string()),
                    Some(13),
                ),
                (
                    0,
                    "EXPORT_FILE_NAME".to_string(),
                    Some("export-name".to_string()),
                    Some(14),
                ),
            ]
        );

        let generic_properties: Vec<(String, Option<String>, String)> = query_rows(
            &connection,
            "SELECT key, value, source
             FROM properties
             WHERE key IN ('TITLE', 'AUTHOR', 'STARTUP', 'OPTIONS', 'EXPORT_FILE_NAME')",
            |row| Ok((row.get(0)?, row.get(1)?, row.get(2)?)),
        );
        assert!(generic_properties.is_empty());
    }

    #[test]
    fn rebuild_handles_overlapping_org_todo_keyword_lines_without_duplicate_rows() {
        let test_dir = TestDir::new("overlapping-org-todo");
        let notes_dir = test_dir.path().join("notes");
        let db_path = test_dir.path().join("db.sqlite");
        let config_path = test_dir.path().join("config.toml");
        let org_path = notes_dir.join("overlapping.org");
        let content = include_str!(
            "../tests/data/parser/todo-keywords/overlapping-file-local-lines/fixture.org"
        );

        write_file(&org_path, content);
        write_config(
            &config_path,
            r#"
db_path = "db.sqlite"
[[dirs]]
path = "notes"
recursive = true

[todo]
default_open_keywords = ["TODO(t)"]
default_closed_keywords = ["DONE(d)"]

[search]
fts5_enabled = false
index_body_text = false
"#,
        );

        let report = Indexer::new(OrgizeAdapter::new())
            .rebuild_from_config_path(&config_path)
            .expect("rebuild should succeed");

        assert_eq!(report.indexed_files.len(), 1);
        assert!(report.diagnostics.is_empty());

        let connection = Connection::open(&db_path).expect("db should open");
        let headings = DbReader::list_headings(&connection).expect("headings should load");
        assert_eq!(headings.len(), 4);
        assert_eq!(headings[1].title, "First heading");
        assert_eq!(headings[1].title_raw, "First heading");
        assert_eq!(headings[1].todo_keyword.as_deref(), Some("TODO"));
        assert_eq!(headings[1].todo_type.as_deref(), Some("open"));
        assert_eq!(headings[2].title, "Second heading");
        assert_eq!(headings[2].title_raw, "Second heading");
        assert_eq!(headings[2].todo_keyword.as_deref(), Some("NEXT"));
        assert_eq!(headings[2].todo_type.as_deref(), Some("open"));
        assert_eq!(headings[3].title, "Finished heading");
        assert_eq!(headings[3].title_raw, "Finished heading");
        assert_eq!(headings[3].todo_keyword.as_deref(), Some("DONE"));
        assert_eq!(headings[3].todo_type.as_deref(), Some("closed"));

        let todo_rows: Vec<TodoProvenanceRow> = query_rows(
            &connection,
            "SELECT keyword, state_type, shortcut, sequence_no, source_kind, source_keyword, source_line_number
             FROM todo_keywords
             ORDER BY sequence_no",
            |row| Ok((
                row.get(0)?,
                row.get(1)?,
                row.get(2)?,
                row.get(3)?,
                row.get(4)?,
                row.get(5)?,
                row.get(6)?,
            )),
        );
        assert_eq!(
            todo_rows,
            vec![
                (
                    "TODO".to_string(),
                    "open".to_string(),
                    None,
                    0,
                    "org_keyword".to_string(),
                    Some("TODO".to_string()),
                    Some(2),
                ),
                (
                    "NEXT".to_string(),
                    "open".to_string(),
                    None,
                    1,
                    "org_keyword".to_string(),
                    Some("TODO".to_string()),
                    Some(2),
                ),
                (
                    "WAIT".to_string(),
                    "open".to_string(),
                    None,
                    2,
                    "org_keyword".to_string(),
                    Some("SEQ_TODO".to_string()),
                    Some(3),
                ),
                (
                    "DONE".to_string(),
                    "closed".to_string(),
                    None,
                    3,
                    "org_keyword".to_string(),
                    Some("TODO".to_string()),
                    Some(2),
                ),
                (
                    "CANCELED".to_string(),
                    "closed".to_string(),
                    None,
                    4,
                    "org_keyword".to_string(),
                    Some("SEQ_TODO".to_string()),
                    Some(3),
                ),
            ]
        );
    }

    #[test]
    fn rebuild_uses_combined_document_title_for_root_heading() {
        let test_dir = TestDir::new("combined-document-title");
        let notes_dir = test_dir.path().join("notes");
        let db_path = test_dir.path().join("db.sqlite");
        let config_path = test_dir.path().join("config.toml");
        let org_path = notes_dir.join("multiple-title.org");

        write_file(
            &org_path,
            "#+TITLE: Title can span\n#+TITLE: multiple lines,\n#+AUTHOR: Hubisan\n\n* Unfortunately Everywhere\n\n#+TITLE: even here\n#+TITLE:\n\n* Plain Heading\n",
        );
        write_config(
            &config_path,
            r#"
db_path = "db.sqlite"
[[dirs]]
path = "notes"
recursive = true

[search]
fts5_enabled = false
index_body_text = false
"#,
        );

        Indexer::new(OrgizeAdapter::new())
            .rebuild_from_config_path(&config_path)
            .expect("rebuild should succeed");

        let connection = Connection::open(&db_path).expect("db should open");
        let headings = DbReader::list_headings(&connection).expect("headings should load");

        assert_eq!(headings.len(), 3);
        assert_eq!(
            headings[0].title,
            "Title can span multiple lines, even here"
        );
        assert_eq!(
            headings[0].title_raw,
            "Title can span multiple lines, even here"
        );
        assert_eq!(headings[1].title, "Unfortunately Everywhere");
        assert_eq!(headings[1].title_raw, "Unfortunately Everywhere");
        assert_eq!(headings[2].title, "Plain Heading");
        assert_eq!(headings[2].title_raw, "Plain Heading");
    }

    #[test]
    fn rebuild_persists_heading_shortcuts_and_rich_timestamp_rows() {
        let test_dir = TestDir::new("timestamp-rebuild");
        let notes_dir = test_dir.path().join("notes");
        let db_path = test_dir.path().join("db.sqlite");
        let config_path = test_dir.path().join("config.toml");
        let org_path = notes_dir.join("planning-timestamp.org");

        write_file(
            &org_path,
            include_str!("../tests/data/parser/timestamps/planning-timestamp/fixture.org"),
        );
        write_config(
            &config_path,
            r#"
db_path = "db.sqlite"
[[dirs]]
path = "notes"
recursive = true

[search]
fts5_enabled = false
index_body_text = false
"#,
        );

        Indexer::new(OrgizeAdapter::new())
            .rebuild_from_config_path(&config_path)
            .expect("rebuild should succeed");

        let connection = Connection::open(&db_path).expect("db should open");
        let timestamp_columns: Vec<String> =
            query_rows(&connection, "PRAGMA table_info(timestamps)", |row| {
                row.get(1)
            });
        assert!(
            !timestamp_columns
                .iter()
                .any(|column| column == "has_repeater"),
            "timestamps table should not have has_repeater"
        );

        let headings = DbReader::list_headings(&connection).expect("headings should load");
        let scheduled = headings
            .iter()
            .find(|heading| heading.title == "Simple scheduled")
            .expect("scheduled heading should exist");
        assert_eq!(scheduled.scheduled_raw.as_deref(), Some("<2024-11-20 Wed>"));
        assert_eq!(scheduled.scheduled_ts, Some(1_732_060_800));

        let duplicate = headings
            .iter()
            .find(|heading| heading.title == "Multiple same keyword")
            .expect("duplicate heading should exist");
        assert_eq!(duplicate.scheduled_raw.as_deref(), Some("<2024-11-21 Thu>"));
        assert_eq!(duplicate.scheduled_ts, Some(1_732_147_200));

        let diary = headings
            .iter()
            .find(|heading| heading.title == "Diary expression")
            .expect("diary heading should exist");
        assert!(diary.scheduled_ts.is_none());

        let timestamp_rows: Vec<TimestampRow> = query_rows(
            &connection,
            "SELECT h.title, t.role, t.type, t.range_type, t.start_ts, t.end_ts
                 FROM timestamps t
                 JOIN headings h ON h.id = t.heading_id
                 WHERE h.level > 0
                 ORDER BY h.title, t.byte_start",
            |row| {
                Ok((
                    row.get(0)?,
                    row.get(1)?,
                    row.get(2)?,
                    row.get(3)?,
                    row.get(4)?,
                    row.get(5)?,
                ))
            },
        );
        assert!(timestamp_rows.iter().any(|row| {
            row.0 == "Time range same day"
                && row.1 == "scheduled"
                && row.2 == "active"
                && row.3 == "time_range"
                && row.4 == Some(1_732_095_000)
                && row.5 == Some(1_732_100_400)
        }));
        assert!(timestamp_rows.iter().any(|row| {
            row.0 == "Date range"
                && row.1 == "deadline"
                && row.3 == "date_range"
                && row.4 == Some(1_733_011_200)
                && row.5 == Some(1_733_184_000)
        }));
        assert!(timestamp_rows.iter().any(|row| {
            row.0 == "Task" && row.1 == "body" && row.2 == "active" && row.4 == Some(1_782_172_800)
        }));

        let repeater_rows: Vec<RepeaterRow> = query_rows(
            &connection,
            "SELECT h.title, tr.repeater_type, tr.repeater_value, tr.repeater_unit,
                    tr.repeater_deadline_value, tr.repeater_deadline_unit,
                    tr.warning_type, tr.warning_value, tr.warning_unit
             FROM timestamp_repeaters tr
             JOIN timestamps t ON t.id = tr.timestamp_id
             JOIN headings h ON h.id = t.heading_id
             ORDER BY h.title, tr.id",
            |row| {
                Ok((
                    row.get(0)?,
                    row.get(1)?,
                    row.get(2)?,
                    row.get(3)?,
                    row.get(4)?,
                    row.get(5)?,
                    row.get(6)?,
                    row.get(7)?,
                    row.get(8)?,
                ))
            },
        );
        assert_eq!(
            repeater_rows,
            vec![
                (
                    "Repeater".to_string(),
                    Some("cumulate".to_string()),
                    Some(1),
                    Some("week".to_string()),
                    None,
                    None,
                    None,
                    None,
                    None,
                ),
                (
                    "Repeater with deadline and warning".to_string(),
                    Some("catch_up".to_string()),
                    Some(1),
                    Some("month".to_string()),
                    Some(2),
                    Some("day".to_string()),
                    Some("all".to_string()),
                    Some(5),
                    Some("day".to_string()),
                ),
                (
                    "Warning only all".to_string(),
                    None,
                    None,
                    None,
                    None,
                    None,
                    Some("all".to_string()),
                    Some(5),
                    Some("day".to_string()),
                ),
                (
                    "Warning only first".to_string(),
                    None,
                    None,
                    None,
                    None,
                    None,
                    Some("first".to_string()),
                    Some(2),
                    Some("week".to_string()),
                ),
            ]
        );
    }

    #[test]
    fn rebuild_deduplicates_equivalent_config_file_paths() {
        let test_dir = TestDir::new("equivalent-config-files");
        let notes_dir = test_dir.path().join("files");
        let db_path = test_dir.path().join("db.sqlite");
        let config_path = test_dir.path().join("config.toml");
        let org_path = notes_dir.join("a.org");

        write_file(&org_path, "#+TITLE: Example\n* Heading\n");
        write_config(
            &config_path,
            r#"
db_path = "db.sqlite"
files = ["././files/a.org", "files/../files/a.org"]

[search]
fts5_enabled = false
index_body_text = false
"#,
        );

        let report = Indexer::new(OrgizeAdapter::new())
            .rebuild_from_config_path(&config_path)
            .expect("rebuild should succeed");

        assert_eq!(report.indexed_files.len(), 1);
        assert_eq!(report.indexed_files[0].path, org_path);

        let connection = Connection::open(&db_path).expect("db should open");
        let files_count: i64 = connection
            .query_row("SELECT COUNT(*) FROM files", [], |row| row.get(0))
            .expect("files count should load");
        let headings = DbReader::list_headings(&connection).expect("headings should load");

        assert_eq!(files_count, 1);
        assert_eq!(headings.len(), 2);
        assert_eq!(headings[0].title, "Example");
        assert_eq!(headings[1].title, "Heading");
    }

    #[test]
    fn rebuild_removes_stale_relative_file_rows_for_configured_scope() {
        let test_dir = TestDir::new("stale-relative-rows");
        let files_dir = test_dir.path().join("files");
        let db_path = test_dir.path().join("db.sqlite");
        let config_path = test_dir.path().join("config.toml");
        let org_path = files_dir.join("a.org");
        let absolute_path = org_path.to_string_lossy().to_string();

        write_file(&org_path, "#+TITLE: Current\n* Fresh\n");
        write_config(
            &config_path,
            r#"
db_path = "db.sqlite"
files = ["././files/a.org", "./files/../files/a.org"]

[search]
fts5_enabled = false
index_body_text = false
"#,
        );

        let mut connection = crate::db::open_database_with_schema(
            &db_path,
            &crate::db::SchemaDefinition::new(1, false),
        )
        .expect("db should open");
        connection
            .execute(
                "INSERT INTO files (id, path, mtime_ns, size) VALUES (1, '././files/a.org', 1, 1)",
                [],
            )
            .expect("first stale file row should insert");
        connection
            .execute(
                "INSERT INTO files (id, path, mtime_ns, size) VALUES (2, 'files/a.org', 1, 1)",
                [],
            )
            .expect("second stale file row should insert");
        connection
            .execute(
                "INSERT INTO headings (id, file_id, parent_id, level, line_number, byte_start, byte_end, title, title_raw, archivedp, footnote_section_p, all_tags_json)
                 VALUES (1, 1, NULL, 0, 1, -1, 1, 'Old Root', 'Old Root', 0, 0, '[]')",
                [],
            )
            .expect("first stale root should insert");
        connection
            .execute(
                "INSERT INTO headings (id, file_id, parent_id, level, line_number, byte_start, byte_end, title, title_raw, archivedp, footnote_section_p, all_tags_json)
                 VALUES (2, 1, 1, 1, 1, 0, 1, 'Old Child', 'Old Child', 0, 0, '[]')",
                [],
            )
            .expect("first stale child should insert");
        connection
            .execute(
                "INSERT INTO headings (id, file_id, parent_id, level, line_number, byte_start, byte_end, title, title_raw, archivedp, footnote_section_p, all_tags_json)
                 VALUES (3, 2, NULL, 0, 1, -1, 1, 'Older Root', 'Older Root', 0, 0, '[]')",
                [],
            )
            .expect("second stale root should insert");

        let report = Indexer::new(OrgizeAdapter::new())
            .rebuild(
                &mut connection,
                &Config::load_from_file(&config_path).expect("config should load"),
            )
            .expect("rebuild should succeed");

        assert_eq!(report.indexed_files.len(), 1);
        assert_eq!(report.indexed_files[0].path, org_path);

        let file_rows: Vec<(i64, String)> = query_rows(
            &connection,
            "SELECT id, path FROM files ORDER BY id",
            |row| Ok((row.get(0)?, row.get(1)?)),
        );
        let heading_rows: Vec<(i64, i64, String)> = query_rows(
            &connection,
            "SELECT id, file_id, title FROM headings ORDER BY id",
            |row| Ok((row.get(0)?, row.get(1)?, row.get(2)?)),
        );

        assert_eq!(file_rows.len(), 1);
        assert_eq!(file_rows[0].1, absolute_path);
        assert_eq!(heading_rows.len(), 2);
        assert!(heading_rows
            .iter()
            .all(|(_, file_id, _)| *file_id == file_rows[0].0));
        assert_eq!(
            heading_rows
                .iter()
                .map(|(_, _, title)| title.clone())
                .collect::<Vec<_>>(),
            vec!["Current".to_string(), "Fresh".to_string()]
        );
    }

    #[test]
    fn rebuild_reports_missing_configured_files_at_rebuild_time() {
        let test_dir = TestDir::new("missing-configured-file");
        let config_path = test_dir.path().join("config.toml");
        let missing_file = test_dir.path().join("missing.org");

        write_config(
            &config_path,
            r#"
db_path = "db.sqlite"
files = ["missing.org"]

[search]
fts5_enabled = false
index_body_text = false
"#,
        );

        let config = Config::load_from_file(&config_path).expect("config should load");
        let mut connection = open_in_memory_database_with_schema(&SchemaDefinition::new(
            CURRENT_SCHEMA_VERSION,
            false,
        ))
        .expect("database should open");

        let error = Indexer::new(OrgizeAdapter::new())
            .rebuild(&mut connection, &config)
            .expect_err("rebuild should fail for a missing configured file");

        match error {
            IndexerError::Discover { path, .. } => {
                assert_eq!(path, missing_file);
            }
            other => panic!("unexpected error: {other}"),
        }
    }

    #[test]
    fn rebuild_refuses_zero_input_when_existing_indexed_data_would_be_deleted() {
        let mut connection = open_in_memory_database_with_schema(&SchemaDefinition::new(
            CURRENT_SCHEMA_VERSION,
            false,
        ))
        .expect("database should open");
        seed_indexed_file(&connection);

        let config = Config {
            db_path: PathBuf::from("db.sqlite"),
            files: Vec::new(),
            dirs: Vec::new(),
            links: Default::default(),
            todo: Default::default(),
            search: SearchConfig {
                fts5_enabled: false,
                index_body_text: false,
            },
        };

        let error = Indexer::new(OrgizeAdapter::new())
            .rebuild_with_options(&mut connection, &config, false)
            .expect_err("rebuild should refuse to wipe existing data");

        match &error {
            IndexerError::RefusedEmptyRebuild {
                existing_indexed_files,
            } => {
                assert_eq!(*existing_indexed_files, 1);
            }
            other => panic!("unexpected error: {other}"),
        }

        let message = error.to_string();
        assert!(message.contains("zero input Org files"));
        assert!(message.contains("avoid deleting 1 indexed file"));
        assert!(message.contains("--allow-empty"));

        let files_count: i64 = connection
            .query_row("SELECT COUNT(*) FROM files", [], |row| row.get(0))
            .expect("file count should load");
        let headings_count: i64 = connection
            .query_row("SELECT COUNT(*) FROM headings", [], |row| row.get(0))
            .expect("heading count should load");

        assert_eq!(files_count, 1);
        assert_eq!(headings_count, 1);
    }

    #[test]
    fn rebuild_allows_zero_input_when_existing_database_is_empty() {
        let mut connection = open_in_memory_database_with_schema(&SchemaDefinition::new(
            CURRENT_SCHEMA_VERSION,
            false,
        ))
        .expect("database should open");

        let config = Config {
            db_path: PathBuf::from("db.sqlite"),
            files: Vec::new(),
            dirs: Vec::new(),
            links: Default::default(),
            todo: Default::default(),
            search: SearchConfig {
                fts5_enabled: false,
                index_body_text: false,
            },
        };

        let report = Indexer::new(OrgizeAdapter::new())
            .rebuild_with_options(&mut connection, &config, false)
            .expect("rebuild should succeed for an empty database");

        assert!(report.indexed_files.is_empty());
        assert!(report.diagnostics.is_empty());

        let files_count: i64 = connection
            .query_row("SELECT COUNT(*) FROM files", [], |row| row.get(0))
            .expect("file count should load");
        let headings_count: i64 = connection
            .query_row("SELECT COUNT(*) FROM headings", [], |row| row.get(0))
            .expect("heading count should load");

        assert_eq!(files_count, 0);
        assert_eq!(headings_count, 0);
    }

    #[test]
    fn rebuild_from_config_path_allows_zero_input_on_fresh_database() {
        let test_dir = TestDir::new("zero-input-fresh-db");
        let config_path = test_dir.path().join("config.toml");
        let db_path = test_dir.path().join("db.sqlite");

        write_config(
            &config_path,
            r#"
db_path = "./db.sqlite"

[search]
fts5_enabled = false
index_body_text = false
"#,
        );

        let report = Indexer::new(OrgizeAdapter::new())
            .rebuild_from_config_path_with_options(&config_path, false)
            .expect("fresh database rebuild should succeed");

        assert!(report.indexed_files.is_empty());
        assert!(report.diagnostics.is_empty());

        let connection = crate::db::open_database(&db_path).expect("database should open");
        let files_count: i64 = connection
            .query_row("SELECT COUNT(*) FROM files", [], |row| row.get(0))
            .expect("file count should load");
        assert_eq!(files_count, 0);
    }

    #[test]
    fn rebuild_with_allow_empty_clears_existing_indexed_data() {
        let mut connection = open_in_memory_database_with_schema(&SchemaDefinition::new(
            CURRENT_SCHEMA_VERSION,
            false,
        ))
        .expect("database should open");
        seed_indexed_file(&connection);

        let config = Config {
            db_path: PathBuf::from("db.sqlite"),
            files: Vec::new(),
            dirs: Vec::new(),
            links: Default::default(),
            todo: Default::default(),
            search: SearchConfig {
                fts5_enabled: false,
                index_body_text: false,
            },
        };

        let report = Indexer::new(OrgizeAdapter::new())
            .rebuild_with_options(&mut connection, &config, true)
            .expect("rebuild should allow empty input with override");

        assert!(report.indexed_files.is_empty());
        assert!(report.diagnostics.is_empty());

        let files_count: i64 = connection
            .query_row("SELECT COUNT(*) FROM files", [], |row| row.get(0))
            .expect("file count should load");
        let headings_count: i64 = connection
            .query_row("SELECT COUNT(*) FROM headings", [], |row| row.get(0))
            .expect("heading count should load");

        assert_eq!(files_count, 0);
        assert_eq!(headings_count, 0);
    }

    #[test]
    fn rebuild_reports_missing_configured_directories_at_rebuild_time() {
        let test_dir = TestDir::new("missing-configured-dir");
        let config_path = test_dir.path().join("config.toml");
        let missing_dir = test_dir.path().join("missing-dir");

        write_config(
            &config_path,
            r#"
db_path = "db.sqlite"
[[dirs]]
path = "missing-dir"

[search]
fts5_enabled = false
index_body_text = false
"#,
        );

        let config = Config::load_from_file(&config_path).expect("config should load");
        let mut connection = open_in_memory_database_with_schema(&SchemaDefinition::new(
            CURRENT_SCHEMA_VERSION,
            false,
        ))
        .expect("database should open");

        let error = Indexer::new(OrgizeAdapter::new())
            .rebuild(&mut connection, &config)
            .expect_err("rebuild should fail for a missing configured directory");

        match error {
            IndexerError::Discover { path, .. } => {
                assert_eq!(path, missing_dir);
            }
            other => panic!("unexpected error: {other}"),
        }
    }

    #[test]
    fn rebuild_respects_org_todo_keywords_as_overrides() {
        let test_dir = TestDir::new("org-todo-overrides");
        let notes_dir = test_dir.path().join("notes");
        let db_path = test_dir.path().join("db.sqlite");
        let config_path = test_dir.path().join("config.toml");
        let org_path = notes_dir.join("todo.org");

        write_file(
            &org_path,
            "#+TITLE: TODO Overrides\n#+TODO: PLAN(p) | DONE(d)\n* PLAN me\n* DONE me\n* REVIEW Mist\n",
        );
        write_config(
            &config_path,
            r#"
db_path = "db.sqlite"
[[dirs]]
path = "notes"
recursive = true

[todo]
default_open_keywords = ["REVIEW(r)", "TODO(t)"]
default_closed_keywords = ["DONE(d)"]

[search]
fts5_enabled = false
index_body_text = false
"#,
        );

        Indexer::new(OrgizeAdapter::new())
            .rebuild_from_config_path(&config_path)
            .expect("rebuild should succeed");

        let connection = Connection::open(&db_path).expect("db should open");
        let headings = DbReader::list_headings(&connection).expect("headings should load");

        assert_eq!(headings.len(), 4);
        assert_eq!(headings[0].title, "TODO Overrides");
        assert_eq!(headings[1].title, "me");
        assert_eq!(headings[1].title_raw, "me");
        assert_eq!(headings[1].todo_keyword.as_deref(), Some("PLAN"));
        assert_eq!(headings[1].todo_type.as_deref(), Some("open"));
        assert_eq!(headings[2].title, "me");
        assert_eq!(headings[2].title_raw, "me");
        assert_eq!(headings[2].todo_keyword.as_deref(), Some("DONE"));
        assert_eq!(headings[2].todo_type.as_deref(), Some("closed"));
        assert_eq!(headings[3].title, "REVIEW Mist");
        assert_eq!(headings[3].title_raw, "REVIEW Mist");
        assert_eq!(headings[3].todo_keyword, None);
        assert_eq!(headings[3].todo_type, None);
    }

    #[test]
    fn rebuild_records_config_default_todo_keyword_provenance() {
        let test_dir = TestDir::new("config-default-todo");
        let notes_dir = test_dir.path().join("notes");
        let db_path = test_dir.path().join("db.sqlite");
        let config_path = test_dir.path().join("config.toml");
        let org_path = notes_dir.join("default.org");

        write_file(
            &org_path,
            "#+TITLE: Defaults\n* TODO Inbox\n* DONE Closed\n",
        );
        write_config(
            &config_path,
            r#"
db_path = "db.sqlite"
[[dirs]]
path = "notes"
recursive = true

[todo]
default_open_keywords = ["TODO(t)"]
default_closed_keywords = ["DONE(d)"]

[search]
fts5_enabled = false
index_body_text = false
"#,
        );

        Indexer::new(OrgizeAdapter::new())
            .rebuild_from_config_path(&config_path)
            .expect("rebuild should succeed");

        let connection = Connection::open(&db_path).expect("db should open");
        let todo_rows: Vec<TodoProvenanceRow> = query_rows(
            &connection,
            "SELECT keyword, state_type, shortcut, sequence_no, source_kind, source_keyword, source_line_number
             FROM todo_keywords
             ORDER BY sequence_no",
            |row| Ok((
                row.get(0)?,
                row.get(1)?,
                row.get(2)?,
                row.get(3)?,
                row.get(4)?,
                row.get(5)?,
                row.get(6)?,
            )),
        );

        assert_eq!(
            todo_rows,
            vec![
                (
                    "TODO".to_string(),
                    "open".to_string(),
                    Some("t".to_string()),
                    0,
                    "config_default".to_string(),
                    None,
                    None,
                ),
                (
                    "DONE".to_string(),
                    "closed".to_string(),
                    Some("d".to_string()),
                    1,
                    "config_default".to_string(),
                    None,
                    None,
                ),
            ]
        );
    }

    #[test]
    fn rebuild_handles_manual_org_todo_fixture() {
        let test_dir = TestDir::new("manual-org-todo");
        let org_path = test_dir.path().join("test.org");
        let db_path = test_dir.path().join("db.sqlite");
        let config_path = test_dir.path().join("config.toml");

        write_file(
            &org_path,
            "#+TITLE:\n#+STARTUP: showall\n#+TODO: TODO(t) NEXT(n) PLAN(p) | DONE(d) CANCEL(c)\n\n* REVIEW *Mist*\n\n* PLAN me\n\n* TODO me                                                              :test:\n\n** again                                                                :me:\n\n* DONE me\n",
        );
        write_config(
            &config_path,
            r#"
db_path = "db.sqlite"
files = ["test.org"]

[search]
fts5_enabled = false
index_body_text = false
"#,
        );

        Indexer::new(OrgizeAdapter::new())
            .rebuild_from_config_path(&config_path)
            .expect("rebuild should succeed");

        let connection = Connection::open(&db_path).expect("db should open");
        let headings = DbReader::list_headings(&connection).expect("headings should load");

        assert_eq!(headings.len(), 6);
        assert_eq!(headings[0].title, "test");
        assert_eq!(headings[1].title, "REVIEW Mist");
        assert_eq!(headings[1].title_raw, "REVIEW *Mist*");
        assert_eq!(headings[1].todo_keyword, None);
        assert_eq!(headings[1].todo_type, None);
        assert_eq!(headings[2].title, "me");
        assert_eq!(headings[2].title_raw, "me");
        assert_eq!(headings[2].todo_keyword.as_deref(), Some("PLAN"));
        assert_eq!(headings[2].todo_type.as_deref(), Some("open"));
        assert_eq!(headings[3].title, "me");
        assert_eq!(headings[3].title_raw, "me");
        assert_eq!(headings[3].todo_keyword.as_deref(), Some("TODO"));
        assert_eq!(headings[3].todo_type.as_deref(), Some("open"));
        assert_eq!(headings[4].title, "again");
        assert_eq!(headings[5].title, "me");
        assert_eq!(headings[5].title_raw, "me");
        assert_eq!(headings[5].todo_keyword.as_deref(), Some("DONE"));
        assert_eq!(headings[5].todo_type.as_deref(), Some("closed"));

        let todo_rows: Vec<TodoProvenanceRow> = query_rows(
            &connection,
            "SELECT keyword, state_type, shortcut, sequence_no, source_kind, source_keyword, source_line_number
             FROM todo_keywords
             ORDER BY sequence_no",
            |row| {
                Ok((
                    row.get(0)?,
                    row.get(1)?,
                    row.get(2)?,
                    row.get(3)?,
                    row.get(4)?,
                    row.get(5)?,
                    row.get(6)?,
                ))
            },
        );
        assert_eq!(
            todo_rows,
            vec![
                (
                    "TODO".to_string(),
                    "open".to_string(),
                    Some("t".to_string()),
                    0,
                    "org_keyword".to_string(),
                    Some("TODO".to_string()),
                    Some(3),
                ),
                (
                    "NEXT".to_string(),
                    "open".to_string(),
                    Some("n".to_string()),
                    1,
                    "org_keyword".to_string(),
                    Some("TODO".to_string()),
                    Some(3),
                ),
                (
                    "PLAN".to_string(),
                    "open".to_string(),
                    Some("p".to_string()),
                    2,
                    "org_keyword".to_string(),
                    Some("TODO".to_string()),
                    Some(3),
                ),
                (
                    "DONE".to_string(),
                    "closed".to_string(),
                    Some("d".to_string()),
                    3,
                    "org_keyword".to_string(),
                    Some("TODO".to_string()),
                    Some(3),
                ),
                (
                    "CANCEL".to_string(),
                    "closed".to_string(),
                    Some("c".to_string()),
                    4,
                    "org_keyword".to_string(),
                    Some("TODO".to_string()),
                    Some(3),
                ),
            ]
        );
    }

    #[test]
    fn rebuild_supports_simplified_org_todo_keyword_lines() {
        let test_dir = TestDir::new("simplified-org-todo");
        let org_path = test_dir.path().join("test.org");
        let db_path = test_dir.path().join("db.sqlite");
        let config_path = test_dir.path().join("config.toml");

        write_file(
            &org_path,
            include_str!(
                "../tests/data/parser/todo-keywords/simplified-file-local-lines/fixture.org"
            ),
        );
        write_config(
            &config_path,
            r#"
db_path = "db.sqlite"
files = ["test.org"]

[todo]
default_open_keywords = ["TODO(t)"]
default_closed_keywords = ["DONE(d)"]

[search]
fts5_enabled = false
index_body_text = false
"#,
        );

        Indexer::new(OrgizeAdapter::new())
            .rebuild_from_config_path(&config_path)
            .expect("rebuild should succeed");

        let connection = Connection::open(&db_path).expect("db should open");
        let headings = DbReader::list_headings(&connection).expect("headings should load");

        assert_eq!(headings.len(), 14);
        assert_eq!(
            headings[1].title,
            "TODO invalid keyword, even though it is a default it is overwritten"
        );
        assert_eq!(headings[1].todo_keyword, None);
        assert_eq!(headings[2].todo_keyword, None);

        let expected_rows = vec![
            ("one", "open", Some("t"), 0),
            ("two", "open", Some("n"), 1),
            ("FIVE", "open", None, 2),
            ("SIX", "open", None, 3),
            ("seven", "open", None, 4),
            ("nine", "open", None, 5),
            ("three", "closed", Some("d"), 6),
            ("four", "closed", Some("w"), 7),
            ("eight", "closed", None, 8),
            ("ten", "closed", None, 9),
            ("eleven", "closed", Some("c"), 10),
        ];

        let todo_rows: Vec<(String, String, Option<String>, i64)> = query_rows(
            &connection,
            "SELECT keyword, state_type, shortcut, sequence_no FROM todo_keywords ORDER BY sequence_no",
            |row| Ok((row.get(0)?, row.get(1)?, row.get(2)?, row.get(3)?)),
        );
        assert_eq!(
            todo_rows,
            expected_rows
                .into_iter()
                .map(|(keyword, state_type, shortcut, sequence_no)| {
                    (
                        keyword.to_string(),
                        state_type.to_string(),
                        shortcut.map(str::to_string),
                        sequence_no,
                    )
                })
                .collect::<Vec<_>>()
        );
    }

    #[test]
    fn rebuild_stores_phase3_links_as_source_facts() {
        let test_dir = TestDir::new("bracket-links");
        let notes_dir = test_dir.path().join("notes");
        let db_path = test_dir.path().join("db.sqlite");
        let config_path = test_dir.path().join("config.toml");
        let org_path = notes_dir.join("links.org");

        write_file(
            &org_path,
            "\
#+TITLE: Bracket Links
[[FILE:notes.org::42]]
[[unknown:foo]]
[[target][description]]
[[./local.org::10]]
[[../parent.org]]
[[~/home.org]]
[[/tmp/system.org]]
[[#custom-id]]
[[*Heading]]
[[dedicated target]]
[[notes.org]]
<https://example.com/some path with spaces>
<file:~/code/main.c::255>
<file:~/xx.org::*My Target>
<file:~/xx.org::#my-custom-id>
<file:~/xx.org::/regexp/>
<file+sys:~/sys/path::7>
<file+emacs:~/emacs/path::*Target>
<unknown:foo>
<jira:ABC-123>
file:~/plain.c::255
attachment:projects.org::10
* Heading
<shell:ls *.org>
[[shell:ls]]
https://example.org
<https://example.org>
",
        );
        write_config(
            &config_path,
            r#"
db_path = "db.sqlite"
files = ["notes/links.org"]

[search]
fts5_enabled = false
index_body_text = false
"#,
        );

        let report = Indexer::new(OrgizeAdapter::new())
            .rebuild_from_config_path(&config_path)
            .expect("rebuild should succeed");

        assert_eq!(report.indexed_files.len(), 1);
        assert_eq!(report.indexed_files[0].path, org_path);

        let connection = Connection::open(&db_path).expect("db should open");
        let rows: Vec<StoredLinkRow> = query_rows(
            &connection,
            "SELECT h.title, l.format, l.raw, l.raw_target, l.raw_description, l.link_type,
                    l.path, l.search_option, l.source_context
             FROM links l
             JOIN headings h ON h.id = l.heading_id
             ORDER BY l.byte_start",
            |row| {
                Ok(StoredLinkRow {
                    heading_title: row.get(0)?,
                    format: row.get(1)?,
                    raw: row.get(2)?,
                    raw_target: row.get(3)?,
                    raw_description: row.get(4)?,
                    link_type: row.get(5)?,
                    path: row.get(6)?,
                    search_option: row.get(7)?,
                    source_context: row.get(8)?,
                })
            },
        );

        assert_eq!(
            rows,
            vec![
                StoredLinkRow {
                    heading_title: "Bracket Links".to_string(),
                    format: "bracket".to_string(),
                    raw: "[[FILE:notes.org::42]]".to_string(),
                    raw_target: "FILE:notes.org::42".to_string(),
                    raw_description: None,
                    link_type: "file".to_string(),
                    path: "notes.org".to_string(),
                    search_option: Some("42".to_string()),
                    source_context: "normal".to_string(),
                },
                StoredLinkRow {
                    heading_title: "Bracket Links".to_string(),
                    format: "bracket".to_string(),
                    raw: "[[unknown:foo]]".to_string(),
                    raw_target: "unknown:foo".to_string(),
                    raw_description: None,
                    link_type: "unknown".to_string(),
                    path: "foo".to_string(),
                    search_option: None,
                    source_context: "normal".to_string(),
                },
                StoredLinkRow {
                    heading_title: "Bracket Links".to_string(),
                    format: "bracket".to_string(),
                    raw: "[[target][description]]".to_string(),
                    raw_target: "target".to_string(),
                    raw_description: Some("description".to_string()),
                    link_type: "fuzzy".to_string(),
                    path: "target".to_string(),
                    search_option: None,
                    source_context: "normal".to_string(),
                },
                StoredLinkRow {
                    heading_title: "Bracket Links".to_string(),
                    format: "bracket".to_string(),
                    raw: "[[./local.org::10]]".to_string(),
                    raw_target: "./local.org::10".to_string(),
                    raw_description: None,
                    link_type: "file".to_string(),
                    path: "./local.org".to_string(),
                    search_option: Some("10".to_string()),
                    source_context: "normal".to_string(),
                },
                StoredLinkRow {
                    heading_title: "Bracket Links".to_string(),
                    format: "bracket".to_string(),
                    raw: "[[../parent.org]]".to_string(),
                    raw_target: "../parent.org".to_string(),
                    raw_description: None,
                    link_type: "file".to_string(),
                    path: "../parent.org".to_string(),
                    search_option: None,
                    source_context: "normal".to_string(),
                },
                StoredLinkRow {
                    heading_title: "Bracket Links".to_string(),
                    format: "bracket".to_string(),
                    raw: "[[~/home.org]]".to_string(),
                    raw_target: "~/home.org".to_string(),
                    raw_description: None,
                    link_type: "file".to_string(),
                    path: "~/home.org".to_string(),
                    search_option: None,
                    source_context: "normal".to_string(),
                },
                StoredLinkRow {
                    heading_title: "Bracket Links".to_string(),
                    format: "bracket".to_string(),
                    raw: "[[/tmp/system.org]]".to_string(),
                    raw_target: "/tmp/system.org".to_string(),
                    raw_description: None,
                    link_type: "file".to_string(),
                    path: "/tmp/system.org".to_string(),
                    search_option: None,
                    source_context: "normal".to_string(),
                },
                StoredLinkRow {
                    heading_title: "Bracket Links".to_string(),
                    format: "bracket".to_string(),
                    raw: "[[#custom-id]]".to_string(),
                    raw_target: "#custom-id".to_string(),
                    raw_description: None,
                    link_type: "custom-id".to_string(),
                    path: "custom-id".to_string(),
                    search_option: None,
                    source_context: "normal".to_string(),
                },
                StoredLinkRow {
                    heading_title: "Bracket Links".to_string(),
                    format: "bracket".to_string(),
                    raw: "[[*Heading]]".to_string(),
                    raw_target: "*Heading".to_string(),
                    raw_description: None,
                    link_type: "fuzzy".to_string(),
                    path: "*Heading".to_string(),
                    search_option: None,
                    source_context: "normal".to_string(),
                },
                StoredLinkRow {
                    heading_title: "Bracket Links".to_string(),
                    format: "bracket".to_string(),
                    raw: "[[dedicated target]]".to_string(),
                    raw_target: "dedicated target".to_string(),
                    raw_description: None,
                    link_type: "fuzzy".to_string(),
                    path: "dedicated target".to_string(),
                    search_option: None,
                    source_context: "normal".to_string(),
                },
                StoredLinkRow {
                    heading_title: "Bracket Links".to_string(),
                    format: "bracket".to_string(),
                    raw: "[[notes.org]]".to_string(),
                    raw_target: "notes.org".to_string(),
                    raw_description: None,
                    link_type: "fuzzy".to_string(),
                    path: "notes.org".to_string(),
                    search_option: None,
                    source_context: "normal".to_string(),
                },
                StoredLinkRow {
                    heading_title: "Bracket Links".to_string(),
                    format: "angle".to_string(),
                    raw: "<https://example.com/some path with spaces>".to_string(),
                    raw_target: "https://example.com/some path with spaces".to_string(),
                    raw_description: None,
                    link_type: "https".to_string(),
                    path: "//example.com/some path with spaces".to_string(),
                    search_option: None,
                    source_context: "normal".to_string(),
                },
                StoredLinkRow {
                    heading_title: "Bracket Links".to_string(),
                    format: "angle".to_string(),
                    raw: "<file:~/code/main.c::255>".to_string(),
                    raw_target: "file:~/code/main.c::255".to_string(),
                    raw_description: None,
                    link_type: "file".to_string(),
                    path: "~/code/main.c".to_string(),
                    search_option: Some("255".to_string()),
                    source_context: "normal".to_string(),
                },
                StoredLinkRow {
                    heading_title: "Bracket Links".to_string(),
                    format: "angle".to_string(),
                    raw: "<file:~/xx.org::*My Target>".to_string(),
                    raw_target: "file:~/xx.org::*My Target".to_string(),
                    raw_description: None,
                    link_type: "file".to_string(),
                    path: "~/xx.org".to_string(),
                    search_option: Some("*My Target".to_string()),
                    source_context: "normal".to_string(),
                },
                StoredLinkRow {
                    heading_title: "Bracket Links".to_string(),
                    format: "angle".to_string(),
                    raw: "<file:~/xx.org::#my-custom-id>".to_string(),
                    raw_target: "file:~/xx.org::#my-custom-id".to_string(),
                    raw_description: None,
                    link_type: "file".to_string(),
                    path: "~/xx.org".to_string(),
                    search_option: Some("#my-custom-id".to_string()),
                    source_context: "normal".to_string(),
                },
                StoredLinkRow {
                    heading_title: "Bracket Links".to_string(),
                    format: "angle".to_string(),
                    raw: "<file:~/xx.org::/regexp/>".to_string(),
                    raw_target: "file:~/xx.org::/regexp/".to_string(),
                    raw_description: None,
                    link_type: "file".to_string(),
                    path: "~/xx.org".to_string(),
                    search_option: Some("/regexp/".to_string()),
                    source_context: "normal".to_string(),
                },
                StoredLinkRow {
                    heading_title: "Bracket Links".to_string(),
                    format: "angle".to_string(),
                    raw: "<file+sys:~/sys/path::7>".to_string(),
                    raw_target: "file+sys:~/sys/path::7".to_string(),
                    raw_description: None,
                    link_type: "file+sys".to_string(),
                    path: "~/sys/path".to_string(),
                    search_option: Some("7".to_string()),
                    source_context: "normal".to_string(),
                },
                StoredLinkRow {
                    heading_title: "Bracket Links".to_string(),
                    format: "angle".to_string(),
                    raw: "<file+emacs:~/emacs/path::*Target>".to_string(),
                    raw_target: "file+emacs:~/emacs/path::*Target".to_string(),
                    raw_description: None,
                    link_type: "file+emacs".to_string(),
                    path: "~/emacs/path".to_string(),
                    search_option: Some("*Target".to_string()),
                    source_context: "normal".to_string(),
                },
                StoredLinkRow {
                    heading_title: "Bracket Links".to_string(),
                    format: "angle".to_string(),
                    raw: "<unknown:foo>".to_string(),
                    raw_target: "unknown:foo".to_string(),
                    raw_description: None,
                    link_type: "unknown".to_string(),
                    path: "foo".to_string(),
                    search_option: None,
                    source_context: "normal".to_string(),
                },
                StoredLinkRow {
                    heading_title: "Bracket Links".to_string(),
                    format: "angle".to_string(),
                    raw: "<jira:ABC-123>".to_string(),
                    raw_target: "jira:ABC-123".to_string(),
                    raw_description: None,
                    link_type: "jira".to_string(),
                    path: "ABC-123".to_string(),
                    search_option: None,
                    source_context: "normal".to_string(),
                },
                StoredLinkRow {
                    heading_title: "Bracket Links".to_string(),
                    format: "plain".to_string(),
                    raw: "file:~/plain.c::255".to_string(),
                    raw_target: "file:~/plain.c::255".to_string(),
                    raw_description: None,
                    link_type: "file".to_string(),
                    path: "~/plain.c".to_string(),
                    search_option: Some("255".to_string()),
                    source_context: "normal".to_string(),
                },
                StoredLinkRow {
                    heading_title: "Bracket Links".to_string(),
                    format: "plain".to_string(),
                    raw: "attachment:projects.org::10".to_string(),
                    raw_target: "attachment:projects.org::10".to_string(),
                    raw_description: None,
                    link_type: "attachment".to_string(),
                    path: "projects.org::10".to_string(),
                    search_option: None,
                    source_context: "normal".to_string(),
                },
                StoredLinkRow {
                    heading_title: "Heading".to_string(),
                    format: "angle".to_string(),
                    raw: "<shell:ls *.org>".to_string(),
                    raw_target: "shell:ls *.org".to_string(),
                    raw_description: None,
                    link_type: "shell".to_string(),
                    path: "ls *.org".to_string(),
                    search_option: None,
                    source_context: "normal".to_string(),
                },
                StoredLinkRow {
                    heading_title: "Heading".to_string(),
                    format: "bracket".to_string(),
                    raw: "[[shell:ls]]".to_string(),
                    raw_target: "shell:ls".to_string(),
                    raw_description: None,
                    link_type: "shell".to_string(),
                    path: "ls".to_string(),
                    search_option: None,
                    source_context: "normal".to_string(),
                },
                StoredLinkRow {
                    heading_title: "Heading".to_string(),
                    format: "plain".to_string(),
                    raw: "https://example.org".to_string(),
                    raw_target: "https://example.org".to_string(),
                    raw_description: None,
                    link_type: "https".to_string(),
                    path: "//example.org".to_string(),
                    search_option: None,
                    source_context: "normal".to_string(),
                },
                StoredLinkRow {
                    heading_title: "Heading".to_string(),
                    format: "angle".to_string(),
                    raw: "<https://example.org>".to_string(),
                    raw_target: "https://example.org".to_string(),
                    raw_description: None,
                    link_type: "https".to_string(),
                    path: "//example.org".to_string(),
                    search_option: None,
                    source_context: "normal".to_string(),
                },
            ]
        );

        let resolution_rows: Vec<(String, Option<String>, Option<String>)> = query_rows(
            &connection,
            "SELECT raw, resolution_status, resolution_diagnostic
             FROM links
             ORDER BY byte_start",
            |row| Ok((row.get(0)?, row.get(1)?, row.get(2)?)),
        );
        assert_eq!(
            resolution_rows,
            vec![
                (
                    "[[FILE:notes.org::42]]".to_string(),
                    Some("unresolved".to_string()),
                    Some(FILE_OUTSIDE_UNIVERSE_DIAGNOSTIC.to_string()),
                ),
                (
                    "[[unknown:foo]]".to_string(),
                    Some("unsupported".to_string()),
                    Some(UNSUPPORTED_DIAGNOSTIC.to_string()),
                ),
                (
                    "[[target][description]]".to_string(),
                    Some("unsupported".to_string()),
                    Some(UNSUPPORTED_DIAGNOSTIC.to_string()),
                ),
                (
                    "[[./local.org::10]]".to_string(),
                    Some("unresolved".to_string()),
                    Some(FILE_OUTSIDE_UNIVERSE_DIAGNOSTIC.to_string()),
                ),
                (
                    "[[../parent.org]]".to_string(),
                    Some("unresolved".to_string()),
                    Some(FILE_OUTSIDE_UNIVERSE_DIAGNOSTIC.to_string()),
                ),
                (
                    "[[~/home.org]]".to_string(),
                    Some("unresolved".to_string()),
                    Some(FILE_OUTSIDE_UNIVERSE_DIAGNOSTIC.to_string()),
                ),
                (
                    "[[/tmp/system.org]]".to_string(),
                    Some("unresolved".to_string()),
                    Some(FILE_OUTSIDE_UNIVERSE_DIAGNOSTIC.to_string()),
                ),
                (
                    "[[#custom-id]]".to_string(),
                    Some("broken".to_string()),
                    Some(CUSTOM_ID_MISSING_DIAGNOSTIC.to_string()),
                ),
                (
                    "[[*Heading]]".to_string(),
                    Some("resolved".to_string()),
                    None,
                ),
                (
                    "[[dedicated target]]".to_string(),
                    Some("unsupported".to_string()),
                    Some(UNSUPPORTED_DIAGNOSTIC.to_string()),
                ),
                (
                    "[[notes.org]]".to_string(),
                    Some("unsupported".to_string()),
                    Some(UNSUPPORTED_DIAGNOSTIC.to_string()),
                ),
                (
                    "<https://example.com/some path with spaces>".to_string(),
                    Some("unsupported".to_string()),
                    Some(UNSUPPORTED_DIAGNOSTIC.to_string()),
                ),
                (
                    "<file:~/code/main.c::255>".to_string(),
                    Some("unresolved".to_string()),
                    Some(FILE_OUTSIDE_UNIVERSE_DIAGNOSTIC.to_string()),
                ),
                (
                    "<file:~/xx.org::*My Target>".to_string(),
                    Some("unresolved".to_string()),
                    Some(FILE_OUTSIDE_UNIVERSE_DIAGNOSTIC.to_string()),
                ),
                (
                    "<file:~/xx.org::#my-custom-id>".to_string(),
                    Some("unresolved".to_string()),
                    Some(FILE_OUTSIDE_UNIVERSE_DIAGNOSTIC.to_string()),
                ),
                (
                    "<file:~/xx.org::/regexp/>".to_string(),
                    Some("unresolved".to_string()),
                    Some(FILE_OUTSIDE_UNIVERSE_DIAGNOSTIC.to_string()),
                ),
                (
                    "<file+sys:~/sys/path::7>".to_string(),
                    Some("unsupported".to_string()),
                    Some(UNSUPPORTED_DIAGNOSTIC.to_string()),
                ),
                (
                    "<file+emacs:~/emacs/path::*Target>".to_string(),
                    Some("unsupported".to_string()),
                    Some(UNSUPPORTED_DIAGNOSTIC.to_string()),
                ),
                (
                    "<unknown:foo>".to_string(),
                    Some("unsupported".to_string()),
                    Some(UNSUPPORTED_DIAGNOSTIC.to_string()),
                ),
                (
                    "<jira:ABC-123>".to_string(),
                    Some("unsupported".to_string()),
                    Some(UNSUPPORTED_DIAGNOSTIC.to_string()),
                ),
                (
                    "file:~/plain.c::255".to_string(),
                    Some("unresolved".to_string()),
                    Some(FILE_OUTSIDE_UNIVERSE_DIAGNOSTIC.to_string()),
                ),
                (
                    "attachment:projects.org::10".to_string(),
                    Some("unsupported".to_string()),
                    Some(UNSUPPORTED_DIAGNOSTIC.to_string()),
                ),
                (
                    "<shell:ls *.org>".to_string(),
                    Some("unsupported".to_string()),
                    Some(UNSUPPORTED_DIAGNOSTIC.to_string()),
                ),
                (
                    "[[shell:ls]]".to_string(),
                    Some("unsupported".to_string()),
                    Some(UNSUPPORTED_DIAGNOSTIC.to_string()),
                ),
                (
                    "https://example.org".to_string(),
                    Some("unsupported".to_string()),
                    Some(UNSUPPORTED_DIAGNOSTIC.to_string()),
                ),
                (
                    "<https://example.org>".to_string(),
                    Some("unsupported".to_string()),
                    Some(UNSUPPORTED_DIAGNOSTIC.to_string()),
                ),
            ]
        );
    }

    #[test]
    fn rebuild_runs_link_resolver_after_all_files_are_indexed() {
        let test_dir = TestDir::new("link-resolution-order");
        let notes_dir = test_dir.path().join("notes");
        let db_path = test_dir.path().join("db.sqlite");
        let config_path = test_dir.path().join("config.toml");
        let source_path = notes_dir.join("a-source.org");
        let target_path = notes_dir.join("z-target.org");

        write_file(&source_path, "#+TITLE: Source\n[[file:z-target.org]]\n");
        write_file(&target_path, "#+TITLE: Target\n* Later file\n");
        write_config(
            &config_path,
            r#"
db_path = "db.sqlite"
dirs = [{ path = "notes", recursive = false }]

[search]
fts5_enabled = false
index_body_text = false
"#,
        );

        let report = Indexer::new(OrgizeAdapter::new())
            .rebuild_from_config_path(&config_path)
            .expect("rebuild should succeed");

        assert_eq!(
            report
                .indexed_files
                .iter()
                .map(|file| file.path.clone())
                .collect::<Vec<_>>(),
            vec![source_path.clone(), target_path.clone()]
        );

        let connection = Connection::open(&db_path).expect("db should open");
        let file_rows: Vec<String> =
            query_rows(&connection, "SELECT path FROM files ORDER BY path", |row| {
                row.get(0)
            });
        let link_rows: Vec<(String, String, Option<String>, Option<String>)> = query_rows(
            &connection,
            "SELECT files.path, links.raw, links.resolution_status, links.resolution_diagnostic
             FROM links
             INNER JOIN files ON files.id = links.file_id
             ORDER BY files.path, links.byte_start, links.id",
            |row| Ok((row.get(0)?, row.get(1)?, row.get(2)?, row.get(3)?)),
        );

        assert_eq!(
            file_rows,
            vec![
                source_path.to_string_lossy().to_string(),
                target_path.to_string_lossy().to_string(),
            ]
        );
        assert_eq!(
            link_rows,
            vec![(
                source_path.to_string_lossy().to_string(),
                "[[file:z-target.org]]".to_string(),
                Some("resolved".to_string()),
                None,
            )]
        );
    }

    #[test]
    fn rebuild_resolves_file_links_to_known_indexed_files_and_marks_missing_and_external_targets() {
        let test_dir = TestDir::new("file-link-resolution");
        let notes_dir = test_dir.path().join("notes");
        let db_path = test_dir.path().join("db.sqlite");
        let config_path = test_dir.path().join("config.toml");
        let source_path = notes_dir.join("source.org");
        let local_path = notes_dir.join("local.org");
        let parent_path = test_dir.path().join("parent.org");
        let absolute_path = test_dir.path().join("absolute.org");
        let external_path = test_dir.path().join("outside").join("external.org");

        write_file(
            &source_path,
            &format!(
                "#+TITLE: Source\n[[./local.org]]\n[[../parent.org]]\n[[{}]]\n[[./missing.org]]\n[[{}]]\n[[unknown:foo]]\n",
                absolute_path.to_string_lossy(),
                external_path.to_string_lossy(),
            ),
        );
        write_file(&local_path, "* Local\n");
        write_file(&parent_path, "* Parent\n");
        write_file(&absolute_path, "* Absolute\n");
        write_config(
            &config_path,
            &format!(
                "db_path = \"db.sqlite\"\nfiles = [\"{}\", \"{}\"]\n\n[[dirs]]\npath = \"notes\"\nrecursive = true\n\n[search]\nfts5_enabled = false\nindex_body_text = false\n",
                parent_path.file_name().expect("parent file name").to_string_lossy(),
                absolute_path.file_name().expect("absolute file name").to_string_lossy(),
            ),
        );

        let report = Indexer::new(OrgizeAdapter::new())
            .rebuild_from_config_path(&config_path)
            .expect("rebuild should succeed");
        assert_eq!(report.indexed_files.len(), 4);

        let connection = Connection::open(&db_path).expect("db should open");
        let rows: Vec<LinkResolutionRow> = query_rows(
            &connection,
            "SELECT links.raw, links.resolution_status, files.path, links.resolution_diagnostic
             FROM links
             LEFT JOIN files ON files.id = links.target_file_id
             ORDER BY byte_start",
            |row| Ok((row.get(0)?, row.get(1)?, row.get(2)?, row.get(3)?)),
        );
        let path_rows: Vec<(String, Option<String>)> = query_rows(
            &connection,
            "SELECT raw, path_absolute
             FROM links
             ORDER BY byte_start",
            |row| Ok((row.get(0)?, row.get(1)?)),
        );

        assert_eq!(
            rows,
            vec![
                (
                    "[[./local.org]]".to_string(),
                    Some("resolved".to_string()),
                    Some(local_path.to_string_lossy().to_string()),
                    None,
                ),
                (
                    "[[../parent.org]]".to_string(),
                    Some("resolved".to_string()),
                    Some(parent_path.to_string_lossy().to_string()),
                    None,
                ),
                (
                    format!("[[{}]]", absolute_path.to_string_lossy()),
                    Some("resolved".to_string()),
                    Some(absolute_path.to_string_lossy().to_string()),
                    None,
                ),
                (
                    "[[./missing.org]]".to_string(),
                    Some("broken".to_string()),
                    None,
                    Some(FILE_MISSING_DIAGNOSTIC.to_string()),
                ),
                (
                    format!("[[{}]]", external_path.to_string_lossy()),
                    Some("unresolved".to_string()),
                    None,
                    Some(FILE_OUTSIDE_UNIVERSE_DIAGNOSTIC.to_string()),
                ),
                (
                    "[[unknown:foo]]".to_string(),
                    Some("unsupported".to_string()),
                    None,
                    Some(UNSUPPORTED_DIAGNOSTIC.to_string()),
                ),
            ]
        );
        assert_eq!(
            path_rows,
            vec![
                (
                    "[[./local.org]]".to_string(),
                    Some(local_path.to_string_lossy().to_string()),
                ),
                (
                    "[[../parent.org]]".to_string(),
                    Some(parent_path.to_string_lossy().to_string()),
                ),
                (
                    format!("[[{}]]", absolute_path.to_string_lossy()),
                    Some(absolute_path.to_string_lossy().to_string()),
                ),
                (
                    "[[./missing.org]]".to_string(),
                    Some(notes_dir.join("missing.org").to_string_lossy().to_string()),
                ),
                (
                    format!("[[{}]]", external_path.to_string_lossy()),
                    Some(external_path.to_string_lossy().to_string()),
                ),
                ("[[unknown:foo]]".to_string(), None),
            ]
        );
    }

    #[test]
    fn rebuild_resolves_file_heading_title_search_options() {
        let test_dir = TestDir::new("file-heading-title-resolution");
        let notes_dir = test_dir.path().join("notes");
        let db_path = test_dir.path().join("db.sqlite");
        let config_path = test_dir.path().join("config.toml");
        let source_path = notes_dir.join("source.org");
        let target_path = notes_dir.join("target.org");

        write_file(
            &source_path,
            "\
#+TITLE: Source
[[file:target.org::*Heading]]
[[file:target.org::*   Main Index   ]]
[[file:target.org::*ärger]]
[[file:target.org::*\\[2026-07-01 Wed\\] Implement deterministic link resolution pass after rebuild]]
[[file:target.org::*Missing]]
[[file:target.org::*Duplicate]]
[[file:target.org::#custom-id]]
",
        );
        write_file(
            &target_path,
            "\
#+TITLE: Target
* TODO [#A] Heading :tag:
* Main index
* Ärger
* [2026-07-01 Wed] Implement deterministic link resolution pass after rebuild
* Duplicate
* Duplicate
",
        );
        write_config(
            &config_path,
            r#"
db_path = "db.sqlite"

[[dirs]]
path = "notes"
recursive = true

[search]
fts5_enabled = false
index_body_text = false
"#,
        );

        Indexer::new(OrgizeAdapter::new())
            .rebuild_from_config_path(&config_path)
            .expect("rebuild should succeed");

        let connection = Connection::open(&db_path).expect("db should open");
        let rows: Vec<FileHeadingSearchResolutionRow> = query_rows(
            &connection,
            "SELECT
                 links.raw,
                 target_files.path,
                 target_headings.title,
                 links.search_option,
                 links.resolution_status,
                 links.resolution_diagnostic
             FROM links
             LEFT JOIN files AS target_files ON target_files.id = links.target_file_id
             LEFT JOIN headings AS target_headings ON target_headings.id = links.target_heading_id
             ORDER BY links.byte_start",
            |row| {
                Ok((
                    row.get(0)?,
                    row.get(1)?,
                    row.get(2)?,
                    row.get(3)?,
                    row.get(4)?,
                    row.get(5)?,
                ))
            },
        );

        assert_eq!(
            rows,
            vec![
                (
                    "[[file:target.org::*Heading]]".to_string(),
                    Some(target_path.to_string_lossy().to_string()),
                    Some("Heading".to_string()),
                    Some("*Heading".to_string()),
                    Some("resolved".to_string()),
                    None,
                ),
                (
                    "[[file:target.org::*   Main Index   ]]".to_string(),
                    Some(target_path.to_string_lossy().to_string()),
                    Some("Main index".to_string()),
                    Some("*   Main Index   ".to_string()),
                    Some("resolved".to_string()),
                    None,
                ),
                (
                    "[[file:target.org::*ärger]]".to_string(),
                    Some(target_path.to_string_lossy().to_string()),
                    Some("Ärger".to_string()),
                    Some("*ärger".to_string()),
                    Some("resolved".to_string()),
                    None,
                ),
                (
                    "[[file:target.org::*\\[2026-07-01 Wed\\] Implement deterministic link resolution pass after rebuild]]".to_string(),
                    Some(target_path.to_string_lossy().to_string()),
                    Some(
                        "[2026-07-01 Wed] Implement deterministic link resolution pass after rebuild"
                            .to_string(),
                    ),
                    Some(
                        "*\\[2026-07-01 Wed\\] Implement deterministic link resolution pass after rebuild"
                            .to_string(),
                    ),
                    Some("resolved".to_string()),
                    None,
                ),
                (
                    "[[file:target.org::*Missing]]".to_string(),
                    Some(target_path.to_string_lossy().to_string()),
                    None,
                    Some("*Missing".to_string()),
                    Some("broken".to_string()),
                    Some(HEADING_TITLE_MISSING_DIAGNOSTIC.to_string()),
                ),
                (
                    "[[file:target.org::*Duplicate]]".to_string(),
                    Some(target_path.to_string_lossy().to_string()),
                    Some("Duplicate".to_string()),
                    Some("*Duplicate".to_string()),
                    Some("resolved".to_string()),
                    None,
                ),
                (
                    "[[file:target.org::#custom-id]]".to_string(),
                    Some(target_path.to_string_lossy().to_string()),
                    None,
                    Some("#custom-id".to_string()),
                    Some("broken".to_string()),
                    Some(CUSTOM_ID_MISSING_DIAGNOSTIC.to_string()),
                ),
            ]
        );
    }

    #[test]
    fn rebuild_resolves_file_context_custom_id_links() {
        let test_dir = TestDir::new("file-context-custom-id-resolution");
        let notes_dir = test_dir.path().join("notes");
        let db_path = test_dir.path().join("db.sqlite");
        let config_path = test_dir.path().join("config.toml");
        let source_path = notes_dir.join("source.org");
        let target_path = notes_dir.join("target.org");

        write_file(
            &source_path,
            "\
#+TITLE: Source
[[file:target.org::#custom-id]]
[[file:target.org::# Custom-ID ][Description]]
[[./target.org::#dup]]
<file:target.org::#angle-id>
file:target.org::#plain-id
[[file:target.org::#missing]]
[[file:missing.org::#custom-id]]
",
        );
        write_file(
            &target_path,
            "\
#+TITLE: Target
* Target heading
:PROPERTIES:
:CUSTOM_ID: custom-id
:END:
* First duplicate
:PROPERTIES:
:CUSTOM_ID: dup
:END:
* Second duplicate
:PROPERTIES:
:CUSTOM_ID: DUP
:END:
* Angle heading
:PROPERTIES:
:CUSTOM_ID: angle-id
:END:
* Plain heading
:PROPERTIES:
:CUSTOM_ID: plain-id
:END:
",
        );
        write_config(
            &config_path,
            r#"
db_path = "db.sqlite"

[[dirs]]
path = "notes"
recursive = true

[search]
fts5_enabled = false
index_body_text = false
"#,
        );

        Indexer::new(OrgizeAdapter::new())
            .rebuild_from_config_path(&config_path)
            .expect("rebuild should succeed");

        let connection = Connection::open(&db_path).expect("db should open");
        let rows: Vec<FileContextCustomIdResolutionRow> = query_rows(
            &connection,
            "SELECT
                 links.raw,
                 target_files.path,
                 target_headings.title,
                 links.raw_description,
                 links.target_custom_id,
                 links.resolution_status,
                 links.resolution_diagnostic
             FROM links
             LEFT JOIN files AS target_files ON target_files.id = links.target_file_id
             LEFT JOIN headings AS target_headings ON target_headings.id = links.target_heading_id
             ORDER BY links.byte_start",
            |row| {
                Ok((
                    row.get(0)?,
                    row.get(1)?,
                    row.get(2)?,
                    row.get(3)?,
                    row.get(4)?,
                    row.get(5)?,
                    row.get(6)?,
                ))
            },
        );

        assert_eq!(
            rows,
            vec![
                (
                    "[[file:target.org::#custom-id]]".to_string(),
                    Some(target_path.to_string_lossy().to_string()),
                    Some("Target heading".to_string()),
                    None,
                    Some("custom-id".to_string()),
                    Some("resolved".to_string()),
                    None,
                ),
                (
                    "[[file:target.org::# Custom-ID ][Description]]".to_string(),
                    Some(target_path.to_string_lossy().to_string()),
                    Some("Target heading".to_string()),
                    Some("Description".to_string()),
                    Some("Custom-ID".to_string()),
                    Some("resolved".to_string()),
                    None,
                ),
                (
                    "[[./target.org::#dup]]".to_string(),
                    Some(target_path.to_string_lossy().to_string()),
                    Some("First duplicate".to_string()),
                    None,
                    Some("dup".to_string()),
                    Some("resolved".to_string()),
                    None,
                ),
                (
                    "<file:target.org::#angle-id>".to_string(),
                    Some(target_path.to_string_lossy().to_string()),
                    Some("Angle heading".to_string()),
                    None,
                    Some("angle-id".to_string()),
                    Some("resolved".to_string()),
                    None,
                ),
                (
                    "file:target.org::#plain-id".to_string(),
                    Some(target_path.to_string_lossy().to_string()),
                    Some("Plain heading".to_string()),
                    None,
                    Some("plain-id".to_string()),
                    Some("resolved".to_string()),
                    None,
                ),
                (
                    "[[file:target.org::#missing]]".to_string(),
                    Some(target_path.to_string_lossy().to_string()),
                    None,
                    None,
                    Some("missing".to_string()),
                    Some("broken".to_string()),
                    Some(CUSTOM_ID_MISSING_DIAGNOSTIC.to_string()),
                ),
                (
                    "[[file:missing.org::#custom-id]]".to_string(),
                    None,
                    None,
                    None,
                    None,
                    Some("broken".to_string()),
                    Some(FILE_MISSING_DIAGNOSTIC.to_string()),
                ),
            ]
        );
    }

    #[test]
    fn rebuild_resolves_org_id_links() {
        let test_dir = TestDir::new("org-id-resolution");
        let notes_dir = test_dir.path().join("notes");
        let db_path = test_dir.path().join("db.sqlite");
        let config_path = test_dir.path().join("config.toml");
        let source_path = notes_dir.join("source.org");
        let target_a_path = notes_dir.join("target-a.org");
        let target_b_path = notes_dir.join("target-b.org");
        let target_c_path = notes_dir.join("target-c.org");

        write_file(
            &source_path,
            "\
#+TITLE: Source
id:foo
[[id: FOO ][Description]]
<id:angle-id>
id:dup
[[id:missing]]
",
        );
        write_file(
            &target_a_path,
            "\
#+TITLE: Target A
* Exact target
:PROPERTIES:
:ID: foo
:END:
* Angle target
:PROPERTIES:
:ID: angle-id
:END:
",
        );
        write_file(
            &target_b_path,
            "\
#+TITLE: Target B
* First duplicate
:PROPERTIES:
:ID: dup
:END:
",
        );
        write_file(
            &target_c_path,
            "\
#+TITLE: Target C
* Second duplicate
:PROPERTIES:
:ID: DUP
:END:
",
        );
        write_config(
            &config_path,
            r#"
db_path = "db.sqlite"

[[dirs]]
path = "notes"
recursive = true

[search]
fts5_enabled = false
index_body_text = false
"#,
        );

        Indexer::new(OrgizeAdapter::new())
            .rebuild_from_config_path(&config_path)
            .expect("rebuild should succeed");

        let connection = Connection::open(&db_path).expect("db should open");
        let rows: Vec<OrgIdResolutionRow> = query_rows(
            &connection,
            "SELECT
                 links.raw,
                 target_files.path,
                 target_headings.title,
                 links.raw_description,
                 links.target_id,
                 links.resolution_status,
                 links.resolution_diagnostic
             FROM links
             LEFT JOIN files AS target_files ON target_files.id = links.target_file_id
             LEFT JOIN headings AS target_headings ON target_headings.id = links.target_heading_id
             ORDER BY links.byte_start",
            |row| {
                Ok((
                    row.get(0)?,
                    row.get(1)?,
                    row.get(2)?,
                    row.get(3)?,
                    row.get(4)?,
                    row.get(5)?,
                    row.get(6)?,
                ))
            },
        );

        assert_eq!(
            rows,
            vec![
                (
                    "id:foo".to_string(),
                    Some(target_a_path.to_string_lossy().to_string()),
                    Some("Exact target".to_string()),
                    None,
                    Some("foo".to_string()),
                    Some("resolved".to_string()),
                    None,
                ),
                (
                    "[[id: FOO ][Description]]".to_string(),
                    Some(target_a_path.to_string_lossy().to_string()),
                    Some("Exact target".to_string()),
                    Some("Description".to_string()),
                    Some("FOO".to_string()),
                    Some("resolved".to_string()),
                    None,
                ),
                (
                    "<id:angle-id>".to_string(),
                    Some(target_a_path.to_string_lossy().to_string()),
                    Some("Angle target".to_string()),
                    None,
                    Some("angle-id".to_string()),
                    Some("resolved".to_string()),
                    None,
                ),
                (
                    "id:dup".to_string(),
                    None,
                    None,
                    None,
                    Some("dup".to_string()),
                    Some("ambiguous".to_string()),
                    Some(DUPLICATE_ID_DIAGNOSTIC.to_string()),
                ),
                (
                    "[[id:missing]]".to_string(),
                    None,
                    None,
                    None,
                    Some("missing".to_string()),
                    Some("unresolved".to_string()),
                    Some(ID_MISSING_DIAGNOSTIC.to_string()),
                ),
            ]
        );
    }

    #[test]
    fn rebuild_resolves_same_file_fuzzy_star_heading_links() {
        let test_dir = TestDir::new("same-file-fuzzy-star-heading-resolution");
        let notes_dir = test_dir.path().join("notes");
        let db_path = test_dir.path().join("db.sqlite");
        let config_path = test_dir.path().join("config.toml");
        let source_path = notes_dir.join("source.org");

        write_file(
            &source_path,
            "\
#+TITLE: Source
[[*Heading]]
[[*Heading][Description]]
[[*   Peer heading   ]]
[[*ärger]]
[[*\\[2026-07-01 Wed\\] Implement deterministic link resolution pass after rebuild]]
[[*Missing]]
[[*Duplicate]]
[[Heading]]
* TODO [#A] Heading :tag:
* Peer Heading
* Ärger
* [2026-07-01 Wed] Implement deterministic link resolution pass after rebuild
* Duplicate
* Duplicate
",
        );
        write_config(
            &config_path,
            r#"
db_path = "db.sqlite"

[[dirs]]
path = "notes"
recursive = true

[search]
fts5_enabled = false
index_body_text = false
"#,
        );

        Indexer::new(OrgizeAdapter::new())
            .rebuild_from_config_path(&config_path)
            .expect("rebuild should succeed");

        let connection = Connection::open(&db_path).expect("db should open");
        let rows: Vec<SameFileStarHeadingResolutionRow> = query_rows(
            &connection,
            "SELECT
                 links.raw,
                 target_files.path,
                 target_headings.title,
                 links.raw_description,
                 links.resolution_status,
                 links.resolution_diagnostic,
                 links.search_option
             FROM links
             LEFT JOIN files AS target_files ON target_files.id = links.target_file_id
             LEFT JOIN headings AS target_headings ON target_headings.id = links.target_heading_id
             ORDER BY links.byte_start",
            |row| {
                Ok((
                    row.get(0)?,
                    row.get(1)?,
                    row.get(2)?,
                    row.get(3)?,
                    row.get(4)?,
                    row.get(5)?,
                    row.get(6)?,
                ))
            },
        );

        assert_eq!(
            rows,
            vec![
                (
                    "[[*Heading]]".to_string(),
                    Some(source_path.to_string_lossy().to_string()),
                    Some("Heading".to_string()),
                    None,
                    Some("resolved".to_string()),
                    None,
                    None,
                ),
                (
                    "[[*Heading][Description]]".to_string(),
                    Some(source_path.to_string_lossy().to_string()),
                    Some("Heading".to_string()),
                    Some("Description".to_string()),
                    Some("resolved".to_string()),
                    None,
                    None,
                ),
                (
                    "[[*   Peer heading   ]]".to_string(),
                    Some(source_path.to_string_lossy().to_string()),
                    Some("Peer Heading".to_string()),
                    None,
                    Some("resolved".to_string()),
                    None,
                    None,
                ),
                (
                    "[[*ärger]]".to_string(),
                    Some(source_path.to_string_lossy().to_string()),
                    Some("Ärger".to_string()),
                    None,
                    Some("resolved".to_string()),
                    None,
                    None,
                ),
                (
                    "[[*\\[2026-07-01 Wed\\] Implement deterministic link resolution pass after rebuild]]"
                        .to_string(),
                    Some(source_path.to_string_lossy().to_string()),
                    Some(
                        "[2026-07-01 Wed] Implement deterministic link resolution pass after rebuild"
                            .to_string(),
                    ),
                    None,
                    Some("resolved".to_string()),
                    None,
                    None,
                ),
                (
                    "[[*Missing]]".to_string(),
                    Some(source_path.to_string_lossy().to_string()),
                    None,
                    None,
                    Some("broken".to_string()),
                    Some(SAME_FILE_STAR_HEADING_MISSING_DIAGNOSTIC.to_string()),
                    None,
                ),
                (
                    "[[*Duplicate]]".to_string(),
                    Some(source_path.to_string_lossy().to_string()),
                    Some("Duplicate".to_string()),
                    None,
                    Some("resolved".to_string()),
                    None,
                    None,
                ),
                (
                    "[[Heading]]".to_string(),
                    None,
                    None,
                    None,
                    Some("unsupported".to_string()),
                    Some(UNSUPPORTED_DIAGNOSTIC.to_string()),
                    None,
                ),
            ]
        );
    }

    #[test]
    fn rebuild_resolves_same_file_custom_id_links() {
        let test_dir = TestDir::new("same-file-fuzzy-custom-id-resolution");
        let notes_dir = test_dir.path().join("notes");
        let db_path = test_dir.path().join("db.sqlite");
        let config_path = test_dir.path().join("config.toml");
        let source_path = notes_dir.join("source.org");

        write_file(
            &source_path,
            "\
#+TITLE: Source
[[#custom-id]]
[[# Custom-ID ][Description]]
[[#dup]]
[[#missing]]
[[Heading]]
* Target heading
:PROPERTIES:
:CUSTOM_ID: custom-id
:END:
* First duplicate
:PROPERTIES:
:CUSTOM_ID: dup
:END:
* Second duplicate
:PROPERTIES:
:CUSTOM_ID: DUP
:END:
",
        );
        write_config(
            &config_path,
            r#"
db_path = "db.sqlite"

[[dirs]]
path = "notes"
recursive = true

[search]
fts5_enabled = false
index_body_text = false
"#,
        );

        Indexer::new(OrgizeAdapter::new())
            .rebuild_from_config_path(&config_path)
            .expect("rebuild should succeed");

        let connection = Connection::open(&db_path).expect("db should open");
        let rows: Vec<SameFileCustomIdResolutionRow> = query_rows(
            &connection,
            "SELECT
                 links.raw,
                 target_files.path,
                 target_headings.title,
                 links.raw_description,
                 links.target_custom_id,
                 links.resolution_status,
                 links.resolution_diagnostic
             FROM links
             LEFT JOIN files AS target_files ON target_files.id = links.target_file_id
             LEFT JOIN headings AS target_headings ON target_headings.id = links.target_heading_id
             ORDER BY links.byte_start",
            |row| {
                Ok((
                    row.get(0)?,
                    row.get(1)?,
                    row.get(2)?,
                    row.get(3)?,
                    row.get(4)?,
                    row.get(5)?,
                    row.get(6)?,
                ))
            },
        );

        assert_eq!(
            rows,
            vec![
                (
                    "[[#custom-id]]".to_string(),
                    Some(source_path.to_string_lossy().to_string()),
                    Some("Target heading".to_string()),
                    None,
                    Some("custom-id".to_string()),
                    Some("resolved".to_string()),
                    None,
                ),
                (
                    "[[# Custom-ID ][Description]]".to_string(),
                    Some(source_path.to_string_lossy().to_string()),
                    Some("Target heading".to_string()),
                    Some("Description".to_string()),
                    Some("Custom-ID".to_string()),
                    Some("resolved".to_string()),
                    None,
                ),
                (
                    "[[#dup]]".to_string(),
                    Some(source_path.to_string_lossy().to_string()),
                    Some("First duplicate".to_string()),
                    None,
                    Some("dup".to_string()),
                    Some("resolved".to_string()),
                    None,
                ),
                (
                    "[[#missing]]".to_string(),
                    Some(source_path.to_string_lossy().to_string()),
                    None,
                    None,
                    Some("missing".to_string()),
                    Some("broken".to_string()),
                    Some(CUSTOM_ID_MISSING_DIAGNOSTIC.to_string()),
                ),
                (
                    "[[Heading]]".to_string(),
                    None,
                    None,
                    None,
                    None,
                    Some("unsupported".to_string()),
                    Some(UNSUPPORTED_DIAGNOSTIC.to_string()),
                ),
            ]
        );
    }

    #[test]
    fn rebuild_keeps_source_link_rows_when_target_file_disappears_from_indexed_set() {
        let test_dir = TestDir::new("file-link-target-removed");
        let notes_dir = test_dir.path().join("notes");
        let db_path = test_dir.path().join("db.sqlite");
        let config_path = test_dir.path().join("config.toml");
        let source_path = notes_dir.join("source.org");
        let target_path = notes_dir.join("target.org");

        write_file(&source_path, "#+TITLE: Source\n[[./target.org]]\n");
        write_file(&target_path, "* Target\n");
        write_config(
            &config_path,
            r#"
db_path = "db.sqlite"

[[dirs]]
path = "notes"
recursive = true

[search]
fts5_enabled = false
index_body_text = false
"#,
        );

        Indexer::new(OrgizeAdapter::new())
            .rebuild_from_config_path(&config_path)
            .expect("first rebuild should succeed");
        std::fs::remove_file(&target_path).expect("target file should delete");

        Indexer::new(OrgizeAdapter::new())
            .rebuild_from_config_path(&config_path)
            .expect("second rebuild should succeed");

        let connection = Connection::open(&db_path).expect("db should open");
        let link_rows: Vec<TargetRemovalLinkRow> = query_rows(
            &connection,
            "SELECT raw, raw_target, target_file_id, resolution_status, resolution_diagnostic
                 FROM links
                 ORDER BY byte_start",
            |row| {
                Ok((
                    row.get(0)?,
                    row.get(1)?,
                    row.get(2)?,
                    row.get(3)?,
                    row.get(4)?,
                ))
            },
        );

        assert_eq!(
            link_rows,
            vec![(
                "[[./target.org]]".to_string(),
                "./target.org".to_string(),
                None,
                Some("broken".to_string()),
                Some(FILE_MISSING_DIAGNOSTIC.to_string()),
            )]
        );
    }

    #[test]
    fn rebuild_stores_plain_links_with_reviewed_boundary_and_end_semantics() {
        let test_dir = TestDir::new("plain-link-boundaries");
        let notes_dir = test_dir.path().join("notes");
        let db_path = test_dir.path().join("db.sqlite");
        let config_path = test_dir.path().join("config.toml");
        let org_path = notes_dir.join("links.org");

        write_file(
            &org_path,
            "\
#+TITLE: Plain Boundaries
!https://www.example.com
\"https://www.example.com
_https://www.example.com
'https://www.example.com
$https://www.example.com
%https://www.example.com
xhttps://www.example.com
Prefix:https://www.example.com
https://example.org/path with text after whitespace
https://example.org/path<balanced-suffix>
https://example.org/path(foo)
https://example.org/path[foo]
https://example.org/path.
https://example.org/path/
https://example.org/path-
https://example.org/path>not-part-of-plain-link
https://example.org/path<not-part-of-plain-link
",
        );
        write_config(
            &config_path,
            r#"
db_path = "db.sqlite"
files = ["notes/links.org"]

[search]
fts5_enabled = false
index_body_text = false
"#,
        );

        Indexer::new(OrgizeAdapter::new())
            .rebuild_from_config_path(&config_path)
            .expect("rebuild should succeed");

        let connection = Connection::open(&db_path).expect("db should open");
        let rows: Vec<String> = query_rows(
            &connection,
            "SELECT raw FROM links ORDER BY byte_start",
            |row| row.get(0),
        );

        assert_eq!(
            rows,
            vec![
                "https://www.example.com".to_string(),
                "https://www.example.com".to_string(),
                "https://www.example.com".to_string(),
                "https://www.example.com".to_string(),
                "https://example.org/path".to_string(),
                "https://example.org/path<balanced-suffix>".to_string(),
                "https://example.org/path(foo)".to_string(),
                "https://example.org/path[foo]".to_string(),
                "https://example.org/path".to_string(),
                "https://example.org/path/".to_string(),
                "https://example.org/path-".to_string(),
                "https://example.org/path".to_string(),
                "https://example.org/path".to_string(),
            ]
        );
    }

    #[test]
    fn rebuild_ignores_links_in_ignored_regions_and_persists_source_contexts() {
        let test_dir = TestDir::new("links-source-context");
        let notes_dir = test_dir.path().join("notes");
        let db_path = test_dir.path().join("db.sqlite");
        let config_path = test_dir.path().join("config.toml");
        let org_path = notes_dir.join("links.org");

        write_file(
            &org_path,
            "\
#+TITLE: Source Contexts
#+PROPERTY: ignored https://example.org/in-property-keyword
Before heading https://example.org/in-root-paragraph

* Heading with https://example.org/in-heading
Inline =https://example.org/in-code= and ~https://example.org/in-verbatim~
src_sh{https://example.org/in-inline-src}
@@html:https://example.org/in-inline-export@@

#+BEGIN_SRC text
https://example.org/in-source-block
#+END_SRC

#+BEGIN_EXAMPLE
https://example.org/in-example-block
#+END_EXAMPLE

: https://example.org/in-colon-example-line

#+BEGIN_COMMENT
https://example.org/in-comment-block
#+END_COMMENT

# https://example.org/in-comment-line

#+BEGIN_EXPORT HTML
https://example.org/in-export-block
#+END_EXPORT

Paragraph https://example.org/in-paragraph

#+BEGIN_VERSE
https://example.org/in-verse
#+END_VERSE

#+BEGIN_QUOTE
https://example.org/in-quote
#+END_QUOTE

#+BEGIN_CENTER
https://example.org/in-center
#+END_CENTER

#+BEGIN_JUSTIFY
https://example.org/in-justify
#+END_JUSTIFY

:PROPERTIES:
:LINK: https://example.org/in-property-drawer
:END:

:A_DRAWER:
https://example.org/in-drawer
:END:
",
        );
        write_config(
            &config_path,
            r#"
db_path = "db.sqlite"
files = ["notes/links.org"]

[search]
fts5_enabled = false
index_body_text = false
"#,
        );

        Indexer::new(OrgizeAdapter::new())
            .rebuild_from_config_path(&config_path)
            .expect("rebuild should succeed");

        let connection = Connection::open(&db_path).expect("db should open");
        let rows: Vec<(String, String, String)> = query_rows(
            &connection,
            "SELECT h.title, l.raw, l.source_context
             FROM links l
             JOIN headings h ON h.id = l.heading_id
             ORDER BY l.byte_start",
            |row| Ok((row.get(0)?, row.get(1)?, row.get(2)?)),
        );

        assert_eq!(
            rows,
            vec![
                (
                    "Source Contexts".to_string(),
                    "https://example.org/in-root-paragraph".to_string(),
                    "normal".to_string(),
                ),
                (
                    "Heading with https://example.org/in-heading".to_string(),
                    "https://example.org/in-heading".to_string(),
                    "heading".to_string(),
                ),
                (
                    "Heading with https://example.org/in-heading".to_string(),
                    "https://example.org/in-paragraph".to_string(),
                    "normal".to_string(),
                ),
                (
                    "Heading with https://example.org/in-heading".to_string(),
                    "https://example.org/in-verse".to_string(),
                    "verse_block".to_string(),
                ),
                (
                    "Heading with https://example.org/in-heading".to_string(),
                    "https://example.org/in-quote".to_string(),
                    "quote_block".to_string(),
                ),
                (
                    "Heading with https://example.org/in-heading".to_string(),
                    "https://example.org/in-center".to_string(),
                    "center_block".to_string(),
                ),
                (
                    "Heading with https://example.org/in-heading".to_string(),
                    "https://example.org/in-justify".to_string(),
                    "justify_block".to_string(),
                ),
                (
                    "Heading with https://example.org/in-heading".to_string(),
                    "https://example.org/in-property-drawer".to_string(),
                    "property_drawer".to_string(),
                ),
                (
                    "Heading with https://example.org/in-heading".to_string(),
                    "https://example.org/in-drawer".to_string(),
                    "drawer".to_string(),
                ),
            ]
        );
    }

    #[test]
    fn child_heading_inherits_parent_tags_in_all_tags_json() {
        let test_dir = TestDir::new("inherited-tags");
        let org_path = test_dir.path().join("tags.org");
        let db_path = test_dir.path().join("db.sqlite");
        let config = Config {
            db_path: db_path.clone(),
            files: vec![org_path.clone()],
            dirs: Vec::new(),
            links: Default::default(),
            todo: Default::default(),
            search: crate::config::SearchConfig {
                fts5_enabled: false,
                index_body_text: false,
            },
        };
        let mut connection = crate::db::open_database_with_schema(
            &db_path,
            &crate::db::SchemaDefinition::new(1, false),
        )
        .expect("db should open");

        write_file(&org_path, "* TODO me :test:\n** again :me:\n");
        Indexer::new(OrgizeAdapter::new())
            .rebuild(&mut connection, &config)
            .expect("rebuild should succeed");

        let headings = DbReader::list_headings(&connection).expect("headings should load");
        assert_eq!(headings[1].all_tags_json, "[\"test\"]");
        assert_eq!(headings[2].all_tags_json, "[\"test\",\"me\"]");

        let tag_rows: Vec<String> = query_rows(
            &connection,
            "SELECT tag FROM tags ORDER BY heading_id, tag",
            |row| row.get(0),
        );
        assert_eq!(tag_rows, vec!["test".to_string(), "me".to_string()]);
    }

    #[test]
    fn rebuild_stores_direct_heading_tags_and_filetags_without_materializing_inherited_rows() {
        let test_dir = TestDir::new("filetags-direct-tags");
        let org_path = test_dir.path().join("tags.org");
        let db_path = test_dir.path().join("db.sqlite");
        let config = Config {
            db_path: db_path.clone(),
            files: vec![org_path.clone()],
            dirs: Vec::new(),
            links: Default::default(),
            todo: Default::default(),
            search: crate::config::SearchConfig {
                fts5_enabled: false,
                index_body_text: false,
            },
        };
        let mut connection = crate::db::open_database_with_schema(
            &db_path,
            &crate::db::SchemaDefinition::new(1, false),
        )
        .expect("db should open");

        write_file(
            &org_path,
            "#+TITLE: Tags Fixture\n#+FILETAGS: :file:project:\n\n* Parent :parent:\nParent body.\n\n** Child :child:\nChild body.\n\n* Sibling\nSibling body.\n",
        );
        Indexer::new(OrgizeAdapter::new())
            .rebuild(&mut connection, &config)
            .expect("rebuild should succeed");

        let headings = DbReader::list_headings(&connection).expect("headings should load");
        assert_eq!(headings[0].all_tags_json, "[\"file\",\"project\"]");
        assert_eq!(
            headings[1].all_tags_json,
            "[\"file\",\"project\",\"parent\"]"
        );
        assert_eq!(
            headings[2].all_tags_json,
            "[\"file\",\"project\",\"parent\",\"child\"]"
        );
        assert_eq!(headings[3].all_tags_json, "[\"file\",\"project\"]");

        let tag_rows: Vec<(i64, String)> = query_rows(
            &connection,
            "SELECT headings.level, tags.tag
             FROM tags
             INNER JOIN headings ON headings.id = tags.heading_id
             ORDER BY headings.level, tags.tag",
            |row| Ok((row.get(0)?, row.get(1)?)),
        );
        assert_eq!(
            tag_rows,
            vec![
                (0, "file".to_string()),
                (0, "project".to_string()),
                (1, "parent".to_string()),
                (2, "child".to_string()),
            ]
        );

        let raw_keywords: Vec<(i64, String, Option<String>, Option<i64>)> = query_rows(
            &connection,
            "SELECT headings.level, keywords.keyword, keywords.value, keywords.line_number
             FROM keywords
             INNER JOIN headings ON headings.id = keywords.heading_id
             WHERE keywords.keyword = 'FILETAGS'
             ORDER BY keywords.line_number, keywords.id",
            |row| Ok((row.get(0)?, row.get(1)?, row.get(2)?, row.get(3)?)),
        );
        assert_eq!(
            raw_keywords,
            vec![(
                0,
                "FILETAGS".to_string(),
                Some(":file:project:".to_string()),
                Some(2),
            )]
        );
    }

    #[test]
    fn duplicate_inherited_tags_are_not_repeated_in_all_tags_json() {
        let test_dir = TestDir::new("duplicate-inherited-tags");
        let org_path = test_dir.path().join("tags.org");
        let db_path = test_dir.path().join("db.sqlite");
        let config = Config {
            db_path: db_path.clone(),
            files: vec![org_path.clone()],
            dirs: Vec::new(),
            links: Default::default(),
            todo: Default::default(),
            search: crate::config::SearchConfig {
                fts5_enabled: false,
                index_body_text: false,
            },
        };
        let mut connection = crate::db::open_database_with_schema(
            &db_path,
            &crate::db::SchemaDefinition::new(1, false),
        )
        .expect("db should open");

        write_file(
            &org_path,
            "* Parent :outer:shared:\n** Child :shared:inner:\n*** Grandchild :outer:leaf:\n",
        );
        Indexer::new(OrgizeAdapter::new())
            .rebuild(&mut connection, &config)
            .expect("rebuild should succeed");

        let headings = DbReader::list_headings(&connection).expect("headings should load");
        assert_eq!(headings[1].all_tags_json, "[\"outer\",\"shared\"]");
        assert_eq!(
            headings[2].all_tags_json,
            "[\"outer\",\"shared\",\"inner\"]"
        );
        assert_eq!(
            headings[3].all_tags_json,
            "[\"outer\",\"shared\",\"inner\",\"leaf\"]"
        );

        let tag_rows: Vec<String> = query_rows(
            &connection,
            "SELECT tag FROM tags ORDER BY heading_id, tag",
            |row| row.get(0),
        );
        assert_eq!(
            tag_rows,
            vec![
                "outer".to_string(),
                "shared".to_string(),
                "inner".to_string(),
                "shared".to_string(),
                "leaf".to_string(),
                "outer".to_string(),
            ]
        );
    }

    #[test]
    fn rebuilding_same_file_twice_is_idempotent_and_replaces_old_rows() {
        let test_dir = TestDir::new("idempotent");
        let org_path = test_dir.path().join("notes.org");
        let db_path = test_dir.path().join("db.sqlite");
        let config = Config {
            db_path: db_path.clone(),
            files: vec![org_path.clone()],
            dirs: Vec::new(),
            links: Default::default(),
            todo: Default::default(),
            search: crate::config::SearchConfig {
                fts5_enabled: false,
                index_body_text: false,
            },
        };
        let mut connection = crate::db::open_database_with_schema(
            &db_path,
            &crate::db::SchemaDefinition::new(1, false),
        )
        .expect("db should open");

        write_file(&org_path, "* TODO First :old:\n");
        let first_report = Indexer::new(OrgizeAdapter::new())
            .rebuild(&mut connection, &config)
            .expect("first rebuild should succeed");
        assert_eq!(first_report.indexed_files.len(), 1);

        write_file(&org_path, "* DONE Second :new:\n");
        let second_report = Indexer::new(OrgizeAdapter::new())
            .rebuild(&mut connection, &config)
            .expect("second rebuild should succeed");
        assert_eq!(second_report.indexed_files.len(), 1);

        let heading_count: i64 = connection
            .query_row("SELECT COUNT(*) FROM headings", [], |row| row.get(0))
            .expect("heading count should load");
        let tag_count: i64 = connection
            .query_row("SELECT COUNT(*) FROM tags", [], |row| row.get(0))
            .expect("tag count should load");
        let titles: Vec<String> = query_rows(
            &connection,
            "SELECT title FROM headings WHERE level > 0 ORDER BY title",
            |row| row.get(0),
        );
        let tags: Vec<String> =
            query_rows(&connection, "SELECT tag FROM tags ORDER BY tag", |row| {
                row.get(0)
            });

        assert_eq!(heading_count, 2);
        assert_eq!(tag_count, 1);
        assert_eq!(titles, vec!["Second".to_string()]);
        assert_eq!(tags, vec!["new".to_string()]);
    }

    #[test]
    fn rebuilding_same_links_twice_keeps_resolution_fields_deterministic() {
        let test_dir = TestDir::new("link-resolution-repeat");
        let org_path = test_dir.path().join("notes.org");
        let db_path = test_dir.path().join("db.sqlite");
        let config = Config {
            db_path: db_path.clone(),
            files: vec![org_path.clone()],
            dirs: Vec::new(),
            links: Default::default(),
            todo: Default::default(),
            search: crate::config::SearchConfig {
                fts5_enabled: false,
                index_body_text: false,
            },
        };
        let mut connection = crate::db::open_database_with_schema(
            &db_path,
            &crate::db::SchemaDefinition::new(CURRENT_SCHEMA_VERSION, false),
        )
        .expect("db should open");

        write_file(&org_path, "#+TITLE: Repeat\n[[unknown:foo]]\n");

        let first_report = Indexer::new(OrgizeAdapter::new())
            .rebuild(&mut connection, &config)
            .expect("first rebuild should succeed");
        let first_rows: Vec<(String, String, Option<String>, Option<String>)> = query_rows(
            &connection,
            "SELECT raw, raw_target, resolution_status, resolution_diagnostic
             FROM links
             ORDER BY file_id, byte_start, id",
            |row| Ok((row.get(0)?, row.get(1)?, row.get(2)?, row.get(3)?)),
        );

        let second_report = Indexer::new(OrgizeAdapter::new())
            .rebuild(&mut connection, &config)
            .expect("second rebuild should succeed");
        let second_rows: Vec<(String, String, Option<String>, Option<String>)> = query_rows(
            &connection,
            "SELECT raw, raw_target, resolution_status, resolution_diagnostic
             FROM links
             ORDER BY file_id, byte_start, id",
            |row| Ok((row.get(0)?, row.get(1)?, row.get(2)?, row.get(3)?)),
        );

        assert_eq!(first_report.indexed_files.len(), 1);
        assert_eq!(second_report.indexed_files.len(), 1);
        assert_eq!(
            first_rows,
            vec![(
                "[[unknown:foo]]".to_string(),
                "unknown:foo".to_string(),
                Some("unsupported".to_string()),
                Some(UNSUPPORTED_DIAGNOSTIC.to_string()),
            )]
        );
        assert_eq!(second_rows, first_rows);
    }

    #[test]
    fn rebuild_computes_parent_ids_for_nested_headings() {
        let test_dir = TestDir::new("parent-ids");
        let org_path = test_dir.path().join("tree.org");
        let db_path = test_dir.path().join("db.sqlite");
        let config = Config {
            db_path: db_path.clone(),
            files: vec![org_path.clone()],
            dirs: Vec::new(),
            links: Default::default(),
            todo: Default::default(),
            search: crate::config::SearchConfig {
                fts5_enabled: false,
                index_body_text: false,
            },
        };
        let mut connection = crate::db::open_database_with_schema(
            &db_path,
            &crate::db::SchemaDefinition::new(1, false),
        )
        .expect("db should open");

        write_file(
            &org_path,
            "#+TITLE: Tree\n* Parent\n** Child\n*** Grandchild\n* Sibling\n** Cousin\n",
        );

        let indexer = Indexer::new(OrgizeAdapter::new());
        indexer
            .rebuild(&mut connection, &config)
            .expect("first rebuild should succeed");
        indexer
            .rebuild(&mut connection, &config)
            .expect("second rebuild should succeed");

        let headings: Vec<(i64, Option<i64>, i64, String)> = query_rows(
            &connection,
            "SELECT id, parent_id, level, title FROM headings ORDER BY byte_start, id",
            |row| Ok((row.get(0)?, row.get(1)?, row.get(2)?, row.get(3)?)),
        );

        let level0_count: i64 = connection
            .query_row("SELECT COUNT(*) FROM headings WHERE level = 0", [], |row| {
                row.get(0)
            })
            .expect("level 0 count should load");

        let level0 = headings
            .iter()
            .find(|(_, _, level, _)| *level == 0)
            .expect("level 0 heading should exist");
        let parent = headings
            .iter()
            .find(|(_, _, _, title)| title == "Parent")
            .expect("parent heading should exist");
        let child = headings
            .iter()
            .find(|(_, _, _, title)| title == "Child")
            .expect("child heading should exist");
        let grandchild = headings
            .iter()
            .find(|(_, _, _, title)| title == "Grandchild")
            .expect("grandchild heading should exist");
        let sibling = headings
            .iter()
            .find(|(_, _, _, title)| title == "Sibling")
            .expect("sibling heading should exist");
        let cousin = headings
            .iter()
            .find(|(_, _, _, title)| title == "Cousin")
            .expect("cousin heading should exist");

        assert_eq!(headings.len(), 6);
        assert_eq!(level0_count, 1);
        assert_eq!(level0.1, None);
        assert_eq!(parent.1, Some(level0.0));
        assert_eq!(child.1, Some(parent.0));
        assert_eq!(grandchild.1, Some(child.0));
        assert_eq!(sibling.1, Some(level0.0));
        assert_eq!(cousin.1, Some(sibling.0));
    }

    #[test]
    fn outline_rows_remain_consistent_after_rebuild() {
        let test_dir = TestDir::new("outline");
        let org_path = test_dir.path().join("outline.org");
        let db_path = test_dir.path().join("db.sqlite");
        let config = Config {
            db_path: db_path.clone(),
            files: vec![org_path.clone()],
            dirs: Vec::new(),
            links: Default::default(),
            todo: Default::default(),
            search: crate::config::SearchConfig {
                fts5_enabled: false,
                index_body_text: false,
            },
        };
        let mut connection = crate::db::open_database_with_schema(
            &db_path,
            &crate::db::SchemaDefinition::new(1, false),
        )
        .expect("db should open");

        write_file(
            &org_path,
            "#+TITLE: Outline\n* Parent A\n** Child A1\n*** Grandchild A1a\n** Child A2\n* Parent B\n** Child B1\n",
        );
        let indexer = Indexer::new(OrgizeAdapter::new());
        indexer
            .rebuild(&mut connection, &config)
            .expect("first rebuild should succeed");
        let first_outline: Vec<(i64, String, String)> = query_rows(
            &connection,
            "SELECT depth, materialized_path, breadcrumbs_json FROM outline_path ORDER BY materialized_path",
            |row| Ok((row.get(0)?, row.get(1)?, row.get(2)?)),
        );

        indexer
            .rebuild(&mut connection, &config)
            .expect("second rebuild should succeed");
        let second_outline: Vec<(i64, String, String)> = query_rows(
            &connection,
            "SELECT depth, materialized_path, breadcrumbs_json FROM outline_path ORDER BY materialized_path",
            |row| Ok((row.get(0)?, row.get(1)?, row.get(2)?)),
        );
        let heading_count: i64 = connection
            .query_row("SELECT COUNT(*) FROM headings", [], |row| row.get(0))
            .expect("heading count should load");
        let outline_count: i64 = connection
            .query_row("SELECT COUNT(*) FROM outline_path", [], |row| row.get(0))
            .expect("outline count should load");

        assert_eq!(
            first_outline,
            vec![
                (0, "0000".to_string(), "[\"Outline\"]".to_string()),
                (
                    1,
                    "0000.0001".to_string(),
                    "[\"Outline\",\"Parent A\"]".to_string()
                ),
                (
                    2,
                    "0000.0001.0001".to_string(),
                    "[\"Outline\",\"Parent A\",\"Child A1\"]".to_string()
                ),
                (
                    3,
                    "0000.0001.0001.0001".to_string(),
                    "[\"Outline\",\"Parent A\",\"Child A1\",\"Grandchild A1a\"]".to_string()
                ),
                (
                    2,
                    "0000.0001.0002".to_string(),
                    "[\"Outline\",\"Parent A\",\"Child A2\"]".to_string()
                ),
                (
                    1,
                    "0000.0002".to_string(),
                    "[\"Outline\",\"Parent B\"]".to_string()
                ),
                (
                    2,
                    "0000.0002.0001".to_string(),
                    "[\"Outline\",\"Parent B\",\"Child B1\"]".to_string()
                ),
            ]
        );
        assert_eq!(second_outline, first_outline);
        assert_eq!(heading_count, 7);
        assert_eq!(outline_count, 7);
        assert_materialized_paths_are_four_digits(&first_outline);
    }

    #[test]
    fn rebuild_works_when_fts_is_disabled() {
        let test_dir = TestDir::new("fts-disabled");
        let org_path = test_dir.path().join("notes.org");
        let config_path = test_dir.path().join("config.toml");
        let db_path = test_dir.path().join("db.sqlite");

        write_file(&org_path, "* Heading\n");
        write_config(
            &config_path,
            r#"
db_path = "db.sqlite"
files = ["notes.org"]

[search]
fts5_enabled = false
index_body_text = false
"#,
        );

        Indexer::new(OrgizeAdapter::new())
            .rebuild_from_config_path(&config_path)
            .expect("rebuild should succeed");

        let connection = Connection::open(&db_path).expect("db should open");
        let fts_table_count: i64 = connection
            .query_row(
                "SELECT COUNT(*) FROM sqlite_master WHERE type = 'table' AND name = 'heading_fts'",
                [],
                |row| row.get(0),
            )
            .expect("fts table count should load");
        assert_eq!(fts_table_count, 0);
    }

    #[test]
    fn rebuild_populates_fts_rows_when_supported_and_enabled() {
        let probe = Connection::open_in_memory().expect("probe should open");
        if !sqlite_supports_fts5(&probe) {
            return;
        }

        let test_dir = TestDir::new("fts-enabled");
        let org_path = test_dir.path().join("notes.org");
        let config_path = test_dir.path().join("config.toml");
        let db_path = test_dir.path().join("db.sqlite");

        write_file(
            &org_path,
            "* TODO Searchable Heading\nBody phrase for full text search.\n",
        );
        write_config(
            &config_path,
            r#"
db_path = "db.sqlite"
files = ["notes.org"]

[search]
fts5_enabled = true
index_body_text = true
"#,
        );

        Indexer::new(OrgizeAdapter::new())
            .rebuild_from_config_path(&config_path)
            .expect("rebuild should succeed");

        let connection = Connection::open(&db_path).expect("db should open");
        let row_count: i64 = connection
            .query_row("SELECT COUNT(*) FROM heading_fts", [], |row| row.get(0))
            .expect("fts rows should load");
        let match_count: i64 = connection
            .query_row(
                "SELECT COUNT(*) FROM heading_fts WHERE heading_fts MATCH 'Searchable'",
                [],
                |row| row.get(0),
            )
            .expect("fts match should load");
        let body_match_count: i64 = connection
            .query_row(
                "SELECT COUNT(*) FROM heading_fts WHERE heading_fts MATCH 'phrase'",
                [],
                |row| row.get(0),
            )
            .expect("fts body match should load");

        assert_eq!(row_count, 2);
        assert_eq!(match_count, 1);
        assert_eq!(body_match_count, 1);
    }

    #[test]
    fn rebuild_stores_heading_bodies_only_when_body_indexing_is_enabled() {
        let test_dir = TestDir::new("heading-bodies-enabled");
        let org_path = test_dir.path().join("notes.org");
        let db_path = test_dir.path().join("db.sqlite");
        let config = Config {
            db_path: db_path.clone(),
            files: vec![org_path.clone()],
            dirs: Vec::new(),
            links: Default::default(),
            todo: Default::default(),
            search: crate::config::SearchConfig {
                fts5_enabled: false,
                index_body_text: true,
            },
        };
        let mut connection = crate::db::open_database_with_schema(
            &db_path,
            &crate::db::SchemaDefinition::new(1, false),
        )
        .expect("db should open");

        write_file(
            &org_path,
            "#+TITLE: Body Text Fixture\nFile-level introduction before the first heading.\n\n* Parent\nParent paragraph one.\n\nParent paragraph two.\n\n** Child\nChild paragraph.\nThis text belongs to Child, not Parent.\n\n*** Grandchild\nGrandchild paragraph.\n\n* Empty Body Parent\n** Child Under Empty Parent\nChild body only.\n\n* Parent With Metadata\nSCHEDULED: <2026-06-23 Tue>\n:PROPERTIES:\n:Owner: Alice\n:END:\n\nBody after planning and property drawer.\n",
        );
        Indexer::new(OrgizeAdapter::new())
            .rebuild(&mut connection, &config)
            .expect("rebuild should succeed");

        let level_zero_body: String = connection
            .query_row(
                "SELECT heading_bodies.body_text
                 FROM heading_bodies
                 INNER JOIN headings ON headings.id = heading_bodies.heading_id
                 WHERE headings.level = 0",
                [],
                |row| row.get(0),
            )
            .expect("level 0 body should load");
        assert!(level_zero_body.contains("File-level introduction before the first heading."));

        let parent_body: (String, i64, i64) = connection
            .query_row(
                "SELECT heading_bodies.body_text, heading_bodies.body_byte_start, heading_bodies.body_byte_end
                 FROM heading_bodies
                 INNER JOIN headings ON headings.id = heading_bodies.heading_id
                 WHERE headings.title = 'Parent'",
                [],
                |row| Ok((row.get(0)?, row.get(1)?, row.get(2)?)),
            )
            .expect("parent body should load");
        assert_eq!(
            parent_body.0,
            "Parent paragraph one.\n\nParent paragraph two."
        );
        assert!(parent_body.1 < parent_body.2);

        let child_body: String = connection
            .query_row(
                "SELECT heading_bodies.body_text
                 FROM heading_bodies
                 INNER JOIN headings ON headings.id = heading_bodies.heading_id
                 WHERE headings.title = 'Child'",
                [],
                |row| row.get(0),
            )
            .expect("child body should load");
        assert_eq!(
            child_body,
            "Child paragraph.\nThis text belongs to Child, not Parent."
        );

        let grandchild_body: String = connection
            .query_row(
                "SELECT heading_bodies.body_text
                 FROM heading_bodies
                 INNER JOIN headings ON headings.id = heading_bodies.heading_id
                 WHERE headings.title = 'Grandchild'",
                [],
                |row| row.get(0),
            )
            .expect("grandchild body should load");
        assert_eq!(grandchild_body, "Grandchild paragraph.");

        let empty_parent_rows: i64 = connection
            .query_row(
                "SELECT COUNT(*)
                 FROM heading_bodies
                 INNER JOIN headings ON headings.id = heading_bodies.heading_id
                 WHERE headings.title = 'Empty Body Parent'",
                [],
                |row| row.get(0),
            )
            .expect("empty body parent count should load");
        assert_eq!(empty_parent_rows, 0);

        let child_under_empty_parent_body: String = connection
            .query_row(
                "SELECT heading_bodies.body_text
                 FROM heading_bodies
                 INNER JOIN headings ON headings.id = heading_bodies.heading_id
                 WHERE headings.title = 'Child Under Empty Parent'",
                [],
                |row| row.get(0),
            )
            .expect("child under empty parent body should load");
        assert_eq!(child_under_empty_parent_body, "Child body only.");

        let metadata_body: String = connection
            .query_row(
                "SELECT heading_bodies.body_text
                 FROM heading_bodies
                 INNER JOIN headings ON headings.id = heading_bodies.heading_id
                 WHERE headings.title = 'Parent With Metadata'",
                [],
                |row| row.get(0),
            )
            .expect("metadata body should load");
        assert_eq!(metadata_body, "Body after planning and property drawer.");
    }

    #[test]
    fn rebuild_skips_heading_bodies_when_body_indexing_is_disabled() {
        let test_dir = TestDir::new("heading-bodies-disabled");
        let org_path = test_dir.path().join("notes.org");
        let db_path = test_dir.path().join("db.sqlite");
        let config = Config {
            db_path: db_path.clone(),
            files: vec![org_path.clone()],
            dirs: Vec::new(),
            links: Default::default(),
            todo: Default::default(),
            search: crate::config::SearchConfig {
                fts5_enabled: false,
                index_body_text: false,
            },
        };
        let mut connection = crate::db::open_database_with_schema(
            &db_path,
            &crate::db::SchemaDefinition::new(1, false),
        )
        .expect("db should open");

        write_file(
            &org_path,
            "#+TITLE: Disabled Bodies\n* Parent\nBody that should not be stored.\n",
        );
        Indexer::new(OrgizeAdapter::new())
            .rebuild(&mut connection, &config)
            .expect("rebuild should succeed");

        let body_count: i64 = connection
            .query_row("SELECT COUNT(*) FROM heading_bodies", [], |row| row.get(0))
            .expect("heading body count should load");
        assert_eq!(body_count, 0);
    }

    #[test]
    fn rebuild_excludes_structured_metadata_from_stored_heading_bodies() {
        let test_dir = TestDir::new("heading-bodies-structured-metadata");
        let org_path = test_dir.path().join("notes.org");
        let db_path = test_dir.path().join("db.sqlite");
        let config = Config {
            db_path: db_path.clone(),
            files: vec![org_path.clone()],
            dirs: Vec::new(),
            links: Default::default(),
            todo: Default::default(),
            search: crate::config::SearchConfig {
                fts5_enabled: false,
                index_body_text: true,
            },
        };
        let mut connection = crate::db::open_database_with_schema(
            &db_path,
            &crate::db::SchemaDefinition::new(1, false),
        )
        .expect("db should open");

        write_file(
            &org_path,
            ":PROPERTIES:\n:CATEGORY: Level 0 Category Property\n:END:\n#+TITLE: Body Metadata Fixture\nIntro before heading.\n\n* Task\nSCHEDULED: <2026-06-23 Tue>\n:PROPERTIES:\n:Owner: Bob\n:END:\nReal body text.\n\n#+AUTHOR: Jane Doe\n\nBody after keyword.\n\n** Child\nChild body.\n\n* Invalid Planning\nSCHEDULED: <%%(diary-float t 42)>\nBody after invalid planning.\n",
        );
        Indexer::new(OrgizeAdapter::new())
            .rebuild(&mut connection, &config)
            .expect("rebuild should succeed");

        let level_zero_body: String = connection
            .query_row(
                "SELECT heading_bodies.body_text
                 FROM heading_bodies
                 INNER JOIN headings ON headings.id = heading_bodies.heading_id
                 WHERE headings.level = 0",
                [],
                |row| row.get(0),
            )
            .expect("level 0 body should load");
        assert_eq!(level_zero_body, "Intro before heading.");

        let task_body: (String, Option<i64>, Option<i64>) = connection
            .query_row(
                "SELECT heading_bodies.body_text, heading_bodies.body_byte_start, heading_bodies.body_byte_end
                 FROM heading_bodies
                 INNER JOIN headings ON headings.id = heading_bodies.heading_id
                 WHERE headings.title = 'Task'",
                [],
                |row| Ok((row.get(0)?, row.get(1)?, row.get(2)?)),
            )
            .expect("task body should load");
        assert_eq!(task_body.0, "Real body text.\n\nBody after keyword.");
        assert_eq!(task_body.1, None);
        assert_eq!(task_body.2, None);

        let invalid_planning_body: String = connection
            .query_row(
                "SELECT heading_bodies.body_text
                 FROM heading_bodies
                 INNER JOIN headings ON headings.id = heading_bodies.heading_id
                 WHERE headings.title = 'Invalid Planning'",
                [],
                |row| row.get(0),
            )
            .expect("invalid planning body should load");
        assert_eq!(
            invalid_planning_body,
            "SCHEDULED: <%%(diary-float t 42)>\nBody after invalid planning."
        );
    }

    #[test]
    fn faulty_file_stops_cleanly_with_clear_error() {
        struct FailingParser;

        impl OrgParserCore for FailingParser {
            fn parse_document_core(
                &self,
                path: &Path,
                _content: &str,
                _options: &ParseOptions,
            ) -> Result<ParsedOrgDocument, ParseDiagnostic> {
                if path
                    .file_name()
                    .and_then(|value| value.to_str())
                    .is_some_and(|value| value.contains("bad"))
                {
                    Err(ParseDiagnostic::error("intentional test parse failure")
                        .with_file_path(path))
                } else {
                    Ok(ParsedOrgDocument::new(path))
                }
            }
        }

        let test_dir = TestDir::new("fault");
        let good_path = test_dir.path().join("a-good.org");
        let bad_path = test_dir.path().join("b-bad.org");
        let db_path = test_dir.path().join("db.sqlite");
        let config = Config {
            db_path: db_path.clone(),
            files: vec![good_path.clone(), bad_path.clone()],
            dirs: Vec::new(),
            links: Default::default(),
            todo: Default::default(),
            search: crate::config::SearchConfig {
                fts5_enabled: false,
                index_body_text: false,
            },
        };
        let mut connection = crate::db::open_database_with_schema(
            &db_path,
            &crate::db::SchemaDefinition::new(1, false),
        )
        .expect("db should open");

        write_file(&good_path, "* Good\n");
        write_file(&bad_path, "* Bad\n");

        let error = Indexer::new(FailingParser)
            .rebuild(&mut connection, &config)
            .expect_err("rebuild should stop on parser failure");

        match error {
            IndexerError::Parse { path, diagnostic } => {
                assert_eq!(path, bad_path);
                assert_eq!(diagnostic.message, "intentional test parse failure");
            }
            other => panic!("unexpected error: {other}"),
        }

        let files_count: i64 = connection
            .query_row("SELECT COUNT(*) FROM files", [], |row| row.get(0))
            .expect("file count should load");
        let headings_count: i64 = connection
            .query_row("SELECT COUNT(*) FROM headings", [], |row| row.get(0))
            .expect("heading count should load");

        assert_eq!(files_count, 0);
        assert_eq!(headings_count, 0);
    }

    fn query_rows<T, F>(connection: &Connection, sql: &str, mut map: F) -> Vec<T>
    where
        F: FnMut(&rusqlite::Row<'_>) -> rusqlite::Result<T>,
    {
        let mut statement = connection.prepare(sql).expect("statement should prepare");
        let rows = statement
            .query_map([], |row| map(row))
            .expect("query should run");
        rows.collect::<Result<Vec<_>, _>>()
            .expect("rows should collect")
    }

    fn assert_materialized_paths_are_four_digits(rows: &[(i64, String, String)]) {
        for (_, path, _) in rows {
            for segment in path.split('.') {
                assert_eq!(
                    segment.len(),
                    4,
                    "outline path segment should be exactly 4 digits: {path}"
                );
            }
        }
    }
}
