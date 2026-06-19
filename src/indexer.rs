use std::{
    collections::BTreeSet,
    error::Error,
    fmt, fs,
    path::{Path, PathBuf},
    time::{SystemTime, UNIX_EPOCH},
};

use rusqlite::Connection;

use crate::{
    config::{Config, ConfigError},
    db::{
        open_database_with_schema, DbError, DbWriteError, DbWriter, FileRecordInput,
        HeadingFtsRecord, HeadingRecord, KeywordRecord, OutlinePathRecord, PropertyRecord,
        SchemaDefinition, TagRecord, TodoKeywordRecord,
    },
    parser::{
        file_local_todo_keyword_config, DiagnosticSeverity, OrgParser, ParseDiagnostic,
        ParsedHeading, ParsedOrgDocument, TodoKeywordConfig, TodoType,
    },
};

#[derive(Debug)]
pub struct Indexer<P> {
    parser: P,
}

impl<P> Indexer<P>
where
    P: OrgParser,
{
    pub fn new(parser: P) -> Self {
        Self { parser }
    }

    pub fn rebuild_from_config_path(
        &self,
        config_path: impl AsRef<Path>,
    ) -> Result<RebuildReport, IndexerError> {
        let config = Config::load_from_file(config_path).map_err(IndexerError::Config)?;
        let schema = SchemaDefinition::new(1, config.search.fts5_enabled);
        let mut connection =
            open_database_with_schema(&config.db_path, &schema).map_err(IndexerError::Database)?;
        self.rebuild(&mut connection, &config)
    }

    pub fn rebuild(
        &self,
        connection: &mut Connection,
        config: &Config,
    ) -> Result<RebuildReport, IndexerError> {
        let paths = discover_org_files(config)?;
        let mut pending = Vec::with_capacity(paths.len());
        for path in paths {
            let metadata = fs::metadata(&path).map_err(|source| IndexerError::ReadFile {
                path: path.clone(),
                source,
            })?;
            let content = fs::read_to_string(&path).map_err(|source| IndexerError::ReadFile {
                path: path.clone(),
                source,
            })?;
            let document = self
                .parser
                .parse_document(&path, &content, &config.parse_options())
                .map_err(|diagnostic| IndexerError::Parse {
                    path: path.clone(),
                    diagnostic,
                })?;
            let normalized = normalize_document(document, &path, &content);
            let todo_keywords =
                active_todo_keywords(&normalized, &config.parse_options().todo_keywords);
            let file_record = build_file_record(&path, &metadata)?;
            pending.push(PendingRebuildFile {
                path,
                document: normalized,
                todo_keywords,
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

        tx.commit()
            .map_err(|source| IndexerError::Write(DbWriteError::Transaction { source }))?;

        Ok(report)
    }
}

struct PendingRebuildFile {
    path: PathBuf,
    document: ParsedOrgDocument,
    todo_keywords: TodoKeywordConfig,
    file_record: FileRecordInput,
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
            Self::Write(source) => Some(source),
        }
    }
}

fn discover_org_files(config: &Config) -> Result<Vec<PathBuf>, IndexerError> {
    let mut paths = BTreeSet::new();

    for file in &config.files {
        paths.insert(file.clone());
    }

    for dir in &config.dirs {
        collect_org_files(dir, config.recursive, &mut paths)?;
    }

    Ok(paths.into_iter().collect())
}

fn collect_org_files(
    dir: &Path,
    recursive: bool,
    output: &mut BTreeSet<PathBuf>,
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
                output.insert(path);
            }
        } else if recursive && file_type.is_dir() {
            collect_org_files(&path, recursive, output)?;
        }
    }

    Ok(())
}

fn active_todo_keywords(
    document: &ParsedOrgDocument,
    default_keywords: &TodoKeywordConfig,
) -> TodoKeywordConfig {
    file_local_todo_keyword_config(&document.metadata.keywords)
        .unwrap_or_else(|| default_keywords.clone())
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
        for heading in &mut normalized.headings {
            heading.parent_index = heading.parent_index.map(|index| index + 1);
        }
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

    for heading in normalized.headings.iter_mut().skip(1) {
        heading.file_path = path.to_path_buf();
        heading.is_root = false;
        if heading.parent_index.is_none() {
            heading.parent_index = Some(0);
        }
    }

    normalized.file_path = path.to_path_buf();
    normalized
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
    todo_keywords: &TodoKeywordConfig,
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
        "0000".to_string(),
        vec![level0_heading.title.clone()],
    )
    .map_err(db_write_invalid_input)?];
    let mut fts_rows = Vec::new();

    if fts5_enabled {
        fts_rows.push(HeadingFtsRecord {
            heading_id: level0_id,
            title: level0_heading.title.clone(),
            body: body_for_fts(index_body_text),
        });
    }

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
        outline_rows.push(
            outline_record(
                heading_id,
                file_id,
                Some(parent_id),
                parent_outline.depth + 1,
                format!(
                    "{}.{}",
                    parent_outline.materialized_path,
                    zero_pad_path_segment(heading_index)
                ),
                extend_breadcrumbs(&parent_outline.breadcrumbs_json, &heading.title)
                    .map_err(db_write_invalid_input)?,
            )
            .map_err(db_write_invalid_input)?,
        );

        if fts5_enabled {
            fts_rows.push(HeadingFtsRecord {
                heading_id,
                title: heading.title.clone(),
                body: body_for_fts(index_body_text),
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
            line_number: None,
        })
        .collect::<Vec<_>>();
    let todo_rows = todo_keyword_rows(file_id, todo_keywords);
    let tag_rows = document
        .headings
        .iter()
        .enumerate()
        .flat_map(|(index, heading)| {
            let heading_id = heading_ids[index];
            heading.tags.iter().cloned().map(move |tag| TagRecord {
                heading_id,
                tag,
                inherited: false,
            })
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
                    value: Some(property.value.clone()),
                    source: "property_drawer".to_string(),
                    inherited: property.inherited,
                    line_number: heading.line_number.map(i64::from),
                })
        })
        .collect::<Vec<_>>();

    DbWriter::insert_todo_keywords(connection, &todo_rows)?;
    DbWriter::insert_keywords(connection, &keyword_rows)?;
    DbWriter::insert_tags(connection, &tag_rows)?;
    DbWriter::insert_properties(connection, &property_rows)?;
    DbWriter::insert_outline_path(connection, &outline_rows)?;

    if fts5_enabled {
        DbWriter::insert_heading_fts(connection, &fts_rows)?;
    }

    Ok(document.headings.len())
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
        scheduled_raw: heading.planning.scheduled.clone(),
        scheduled_ts: None,
        deadline_raw: heading.planning.deadline.clone(),
        deadline_ts: None,
        closed_raw: heading.planning.closed.clone(),
        closed_ts: None,
        archivedp: heading.is_archived,
        footnote_section_p: false,
        all_tags_json: serde_json::to_string(effective_tags)
            .map_err(|_| "tag serialization failed")?,
    })
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

fn todo_keyword_rows(file_id: i64, todo_keywords: &TodoKeywordConfig) -> Vec<TodoKeywordRecord> {
    todo_keywords
        .open
        .iter()
        .enumerate()
        .map(|(sequence_no, keyword)| TodoKeywordRecord {
            file_id,
            keyword: keyword.name.clone(),
            state_type: "open".to_string(),
            shortcut: keyword.fast_key,
            sequence_no: sequence_no as i64,
        })
        .chain(
            todo_keywords
                .closed
                .iter()
                .enumerate()
                .map(|(sequence_no, keyword)| TodoKeywordRecord {
                    file_id,
                    keyword: keyword.name.clone(),
                    state_type: "closed".to_string(),
                    shortcut: keyword.fast_key,
                    sequence_no: (todo_keywords.open.len() + sequence_no) as i64,
                }),
        )
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

fn body_for_fts(_index_body_text: bool) -> String {
    String::new()
}

fn db_write_invalid_input(message: &'static str) -> DbWriteError {
    DbWriteError::InvalidInput(message)
}

#[cfg(test)]
mod tests {
    use super::{IndexedFile, Indexer, IndexerError};
    use crate::{
        config::Config,
        db::{sqlite_supports_fts5, DbReader},
        parser::{OrgParser, OrgizeAdapter, ParseDiagnostic, ParseOptions, ParsedOrgDocument},
    };
    use rusqlite::Connection;
    use std::{
        fs,
        path::{Path, PathBuf},
        time::{SystemTime, UNIX_EPOCH},
    };

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
dirs = ["notes"]
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
        assert_eq!(report.diagnostics.len(), 1);
        assert_eq!(
            report.diagnostics[0].severity,
            crate::parser::DiagnosticSeverity::Warning
        );
        assert_eq!(
            report.diagnostics[0].message,
            "Orgize adapter property extraction is currently local-only and does not handle inheritance"
        );
        assert_eq!(report.diagnostics[0].file_path, Some(org_path.clone()));
        assert_eq!(report.diagnostics[0].line_number, Some(3));
        assert!(report.diagnostics[0].byte_range.is_some());

        let connection = Connection::open(&db_path).expect("db should open");
        let headings = DbReader::list_headings(&connection).expect("headings should load");
        assert_eq!(headings.len(), 2);
        assert_eq!(headings[0].level, 0);
        assert_eq!(headings[0].title, "Project Notes");
        assert_eq!(headings[1].title, "Inbox");
        assert_eq!(headings[1].todo_keyword.as_deref(), Some("PLAN"));
        assert_eq!(headings[1].todo_type.as_deref(), Some("open"));
        assert_eq!(headings[1].all_tags_json, "[\"rust\"]");

        let todo_rows: Vec<(String, String, Option<String>, i64)> = query_rows(
            &connection,
            "SELECT keyword, state_type, shortcut, sequence_no FROM todo_keywords ORDER BY sequence_no",
            |row| Ok((row.get(0)?, row.get(1)?, row.get(2)?, row.get(3)?)),
        );
        assert_eq!(
            todo_rows,
            vec![
                (
                    "PLAN".to_string(),
                    "open".to_string(),
                    Some("p".to_string()),
                    0
                ),
                (
                    "DONE".to_string(),
                    "closed".to_string(),
                    Some("d".to_string()),
                    1
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
dirs = ["notes"]
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
files = ["././files/a.org", "files/a.org"]

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
files = ["././files/a.org"]

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
    fn rebuild_respects_file_local_todo_keywords_as_overrides() {
        let test_dir = TestDir::new("file-local-todo");
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
dirs = ["notes"]
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
    fn rebuild_handles_manual_file_local_todo_fixture() {
        let test_dir = TestDir::new("manual-file-local-todo");
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

        let todo_rows: Vec<(String, String, Option<String>, i64)> = query_rows(
            &connection,
            "SELECT keyword, state_type, shortcut, sequence_no FROM todo_keywords ORDER BY sequence_no",
            |row| Ok((row.get(0)?, row.get(1)?, row.get(2)?, row.get(3)?)),
        );
        assert_eq!(
            todo_rows,
            vec![
                (
                    "TODO".to_string(),
                    "open".to_string(),
                    Some("t".to_string()),
                    0
                ),
                (
                    "NEXT".to_string(),
                    "open".to_string(),
                    Some("n".to_string()),
                    1
                ),
                (
                    "PLAN".to_string(),
                    "open".to_string(),
                    Some("p".to_string()),
                    2
                ),
                (
                    "DONE".to_string(),
                    "closed".to_string(),
                    Some("d".to_string()),
                    3
                ),
                (
                    "CANCEL".to_string(),
                    "closed".to_string(),
                    Some("c".to_string()),
                    4
                ),
            ]
        );
    }

    #[test]
    fn rebuild_supports_simplified_file_local_todo_keyword_lines() {
        let test_dir = TestDir::new("simplified-file-local-todo");
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
    fn child_heading_inherits_parent_tags_in_all_tags_json() {
        let test_dir = TestDir::new("inherited-tags");
        let org_path = test_dir.path().join("tags.org");
        let db_path = test_dir.path().join("db.sqlite");
        let config = Config {
            db_path: db_path.clone(),
            files: vec![org_path.clone()],
            dirs: Vec::new(),
            recursive: false,
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

        let tag_rows: Vec<(String, i64)> = query_rows(
            &connection,
            "SELECT tag, inherited FROM tags ORDER BY heading_id, tag",
            |row| Ok((row.get(0)?, row.get(1)?)),
        );
        assert_eq!(
            tag_rows,
            vec![("test".to_string(), 0), ("me".to_string(), 0)]
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
            recursive: false,
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

        let tag_rows: Vec<(String, i64)> = query_rows(
            &connection,
            "SELECT tag, inherited FROM tags ORDER BY heading_id, tag",
            |row| Ok((row.get(0)?, row.get(1)?)),
        );
        assert_eq!(
            tag_rows,
            vec![
                ("outer".to_string(), 0),
                ("shared".to_string(), 0),
                ("inner".to_string(), 0),
                ("shared".to_string(), 0),
                ("leaf".to_string(), 0),
                ("outer".to_string(), 0),
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
            recursive: false,
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
    fn outline_rows_remain_consistent_after_rebuild() {
        let test_dir = TestDir::new("outline");
        let org_path = test_dir.path().join("outline.org");
        let db_path = test_dir.path().join("db.sqlite");
        let config = Config {
            db_path: db_path.clone(),
            files: vec![org_path.clone()],
            dirs: Vec::new(),
            recursive: false,
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

        write_file(&org_path, "* Parent\n** Child\n");
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

        assert_eq!(
            first_outline,
            vec![
                (0, "0000".to_string(), "[\"outline\"]".to_string()),
                (
                    1,
                    "0000.0001".to_string(),
                    "[\"outline\",\"Parent\"]".to_string()
                ),
                (
                    2,
                    "0000.0001.0002".to_string(),
                    "[\"outline\",\"Parent\",\"Child\"]".to_string()
                ),
            ]
        );
        assert_eq!(second_outline, first_outline);
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

        write_file(&org_path, "* TODO Searchable Heading\n");
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

        assert_eq!(row_count, 2);
        assert_eq!(match_count, 1);
    }

    #[test]
    fn faulty_file_stops_cleanly_with_clear_error() {
        struct FailingParser;

        impl OrgParser for FailingParser {
            fn parse_document(
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
            recursive: false,
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
}
