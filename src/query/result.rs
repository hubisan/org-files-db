use std::{
    collections::{BTreeMap, BTreeSet, HashMap},
    fmt,
    path::Path,
};

use chrono::{DateTime, Utc};
use rusqlite::{params_from_iter, Connection};
use serde::Serialize;

use super::sqlite::HeadingQueryMatch;
use super::{
    execute_sqlite_query_with_options, LinkQueryRow, QueryExecutionError, QueryRows, QueryTarget,
    ValidatedQuery,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum QueryOutputMode {
    Flat,
    Outline,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum QueryInclude {
    Path,
    Properties,
    EffectiveProperties,
    Keywords,
    Links,
    Backlinks,
    Source,
    Target,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct QueryExecutionOptions {
    pub output_mode: QueryOutputMode,
    pub includes: Vec<QueryInclude>,
    pub query_timezone: Option<String>,
    pub now_utc: Option<DateTime<Utc>>,
}

impl Default for QueryExecutionOptions {
    fn default() -> Self {
        Self {
            output_mode: QueryOutputMode::Flat,
            includes: Vec::new(),
            query_timezone: None,
            now_utc: None,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct QueryResponse {
    pub target: QueryTarget,
    pub output: QueryOutputMode,
    pub includes: Vec<QueryInclude>,
    pub results: Vec<QueryResultNode>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
#[serde(untagged)]
pub enum QueryResultNode {
    File(FileResultNode),
    Heading(HeadingResultNode),
    Link(Box<LinkResultNode>),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum QueryResultKind {
    File,
    Root,
    Heading,
    Link,
}

impl QueryResultKind {
    pub fn from_heading_level(level: i64) -> Self {
        if level == 0 {
            Self::Root
        } else {
            Self::Heading
        }
    }

    pub const fn as_str(self) -> &'static str {
        match self {
            Self::File => "file",
            Self::Root => "root",
            Self::Heading => "heading",
            Self::Link => "link",
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ResultDomain {
    Files,
    Headings,
    Links,
}

fn public_result_kind(domain: ResultDomain, heading_level: i64) -> QueryResultKind {
    match domain {
        ResultDomain::Files => QueryResultKind::File,
        ResultDomain::Headings => QueryResultKind::from_heading_level(heading_level),
        ResultDomain::Links => QueryResultKind::Link,
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct FileResultNode {
    pub kind: QueryResultKind,
    pub matched: bool,
    pub id: i64,
    pub level: i64,
    pub path: String,
    pub name: String,
    pub dir: String,
    pub title: String,
    pub title_raw: Option<String>,
    pub root_heading_id: i64,
    pub mtime_ns: i64,
    pub size: i64,
    pub content_hash: Option<String>,
    pub indexed_at: Option<i64>,
    pub location: Location,
    pub tags: Vec<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub node_path: Option<Vec<PathEntry>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub properties: Option<Vec<PropertyFact>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub effective_properties: Option<Vec<EffectivePropertyFact>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub keywords: Option<Vec<KeywordFact>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub links: Option<Vec<IncludedLink>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub backlinks: Option<Vec<IncludedLink>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub children: Option<Vec<QueryResultNode>>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct HeadingResultNode {
    pub kind: QueryResultKind,
    pub matched: bool,
    pub id: i64,
    pub file_id: i64,
    pub parent_id: Option<i64>,
    pub level: i64,
    pub title: String,
    pub title_raw: Option<String>,
    pub todo_keyword: Option<String>,
    pub todo_type: Option<String>,
    pub priority: Option<String>,
    pub scheduled_raw: Option<String>,
    pub scheduled_ts: Option<i64>,
    pub deadline_raw: Option<String>,
    pub deadline_ts: Option<i64>,
    pub closed_raw: Option<String>,
    pub closed_ts: Option<i64>,
    pub archivedp: bool,
    pub footnote_section_p: bool,
    pub all_tags: Vec<String>,
    pub location: Location,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub node_path: Option<Vec<PathEntry>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub properties: Option<Vec<PropertyFact>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub effective_properties: Option<Vec<EffectivePropertyFact>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub keywords: Option<Vec<KeywordFact>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub links: Option<Vec<IncludedLink>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub backlinks: Option<Vec<IncludedLink>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub children: Option<Vec<QueryResultNode>>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct LinkResultNode {
    pub kind: QueryResultKind,
    pub matched: bool,
    pub id: i64,
    pub file_id: i64,
    pub heading_id: i64,
    pub heading_level: i64,
    pub source_context: String,
    pub format: String,
    pub link_type: String,
    pub raw: String,
    pub raw_target: String,
    pub raw_description: Option<String>,
    pub link_path: String,
    pub search_option: Option<String>,
    pub path_absolute: Option<String>,
    pub target_file_id: Option<i64>,
    pub target_heading_id: Option<i64>,
    pub target_custom_id: Option<String>,
    pub target_id: Option<String>,
    pub resolution_status: Option<String>,
    pub resolution_diagnostic: Option<String>,
    pub location: Location,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub node_path: Option<Vec<PathEntry>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub source: Option<LinkSource>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub target: Option<LinkTarget>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct Location {
    pub file_path: String,
    pub line: Option<i64>,
    pub byte_start: Option<i64>,
    pub byte_end: Option<i64>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
#[serde(tag = "kind", rename_all = "snake_case")]
pub enum PathEntry {
    File(FilePathEntry),
    Heading(HeadingPathEntry),
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct FilePathEntry {
    pub id: i64,
    pub path: String,
    pub title: String,
    pub title_raw: Option<String>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct HeadingPathEntry {
    pub id: i64,
    pub title: String,
    pub title_raw: String,
    pub level: i64,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct PropertyFact {
    pub key: String,
    pub value: Option<String>,
    pub source: String,
    pub append: bool,
    pub line_number: Option<i64>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct EffectivePropertyFact {
    pub key: String,
    pub value: Option<String>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct KeywordFact {
    pub keyword: String,
    pub value: Option<String>,
    pub line_number: Option<i64>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct LinkSource {
    pub file: FileRef,
    pub heading: Option<HeadingRef>,
    pub source_path: Vec<PathEntry>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct LinkTarget {
    pub resolved_kind: Option<QueryTarget>,
    pub file: Option<FileRef>,
    pub heading: Option<HeadingRef>,
    pub raw_target: String,
    pub resolution_status: Option<String>,
    pub resolution_diagnostic: Option<String>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct FileRef {
    pub id: i64,
    pub path: String,
    pub title: String,
    pub title_raw: Option<String>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct HeadingRef {
    pub id: i64,
    pub title: String,
    pub title_raw: String,
    pub level: i64,
    pub outline_path: Vec<String>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct IncludedLink {
    pub id: i64,
    pub source_context: String,
    pub format: String,
    pub link_type: String,
    pub raw: String,
    pub raw_target: String,
    pub raw_description: Option<String>,
    pub link_path: String,
    pub search_option: Option<String>,
    pub path_absolute: Option<String>,
    pub target_file_id: Option<i64>,
    pub target_heading_id: Option<i64>,
    pub target_custom_id: Option<String>,
    pub target_id: Option<String>,
    pub resolution_status: Option<String>,
    pub resolution_diagnostic: Option<String>,
    pub location: Location,
    pub source_path: Vec<PathEntry>,
    pub source: LinkSource,
    pub target: LinkTarget,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum QueryShapeErrorKind {
    Database,
    InvalidStoredJson,
    MissingStoredData,
}

#[derive(Debug)]
pub struct QueryShapeError {
    pub kind: QueryShapeErrorKind,
    pub message: String,
    source: Option<Box<dyn std::error::Error + Send + Sync>>,
}

impl QueryShapeError {
    fn database(operation: &'static str, source: rusqlite::Error) -> Self {
        Self {
            kind: QueryShapeErrorKind::Database,
            message: format!(
                "failed to load query result enrichment data during {operation}: {source}"
            ),
            source: Some(Box::new(source)),
        }
    }

    fn invalid_json(field: &'static str, id: i64, source: serde_json::Error) -> Self {
        Self {
            kind: QueryShapeErrorKind::InvalidStoredJson,
            message: format!("failed to decode stored JSON field {field} for row {id}: {source}"),
            source: Some(Box::new(source)),
        }
    }

    fn missing(message: impl Into<String>) -> Self {
        Self {
            kind: QueryShapeErrorKind::MissingStoredData,
            message: message.into(),
            source: None,
        }
    }
}

impl fmt::Display for QueryShapeError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(&self.message)
    }
}

impl std::error::Error for QueryShapeError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        self.source
            .as_ref()
            .map(|source| source.as_ref() as &(dyn std::error::Error + 'static))
    }
}

impl From<QueryExecutionError> for QueryShapeError {
    fn from(error: QueryExecutionError) -> Self {
        Self {
            kind: QueryShapeErrorKind::Database,
            message: error.to_string(),
            source: Some(Box::new(error)),
        }
    }
}

pub fn execute_and_shape_query(
    connection: &Connection,
    query: &ValidatedQuery,
    options: &QueryExecutionOptions,
) -> Result<QueryResponse, QueryShapeError> {
    let rows = execute_sqlite_query_with_options(connection, query, options)?;
    shape_query_results(connection, rows, options)
}

pub fn shape_query_results(
    connection: &Connection,
    rows: QueryRows,
    options: &QueryExecutionOptions,
) -> Result<QueryResponse, QueryShapeError> {
    let includes = normalized_includes(&options.includes);
    let context = EnrichmentContext::load(connection, &rows, &includes)?;
    let results = match (&rows, options.output_mode) {
        (QueryRows::Headings(rows), QueryOutputMode::Flat) => rows
            .iter()
            .map(|row| match row {
                HeadingQueryMatch::File(row) => context
                    .shape_file_node(row.id, ResultDomain::Headings, true, &includes, false)
                    .map(QueryResultNode::File),
                HeadingQueryMatch::Heading(row) => context
                    .shape_heading_node(row.id, true, &includes, false)
                    .map(QueryResultNode::Heading),
            })
            .collect::<Result<Vec<_>, QueryShapeError>>()?,
        (QueryRows::Headings(rows), QueryOutputMode::Outline) => {
            context.shape_heading_outline(rows, &includes)?
        }
        (QueryRows::Links(rows), QueryOutputMode::Flat) => rows
            .iter()
            .map(|row| {
                context
                    .shape_link_node(row, &includes)
                    .map(|node| QueryResultNode::Link(Box::new(node)))
            })
            .collect::<Result<Vec<_>, QueryShapeError>>()?,
        (QueryRows::Links(rows), QueryOutputMode::Outline) => {
            context.shape_link_outline(rows, &includes)?
        }
        (QueryRows::Files(rows), QueryOutputMode::Flat) => rows
            .iter()
            .map(|row| {
                context
                    .shape_file_node(row.id, ResultDomain::Files, true, &includes, false)
                    .map(QueryResultNode::File)
            })
            .collect::<Result<Vec<_>, QueryShapeError>>()?,
        (QueryRows::Files(rows), QueryOutputMode::Outline) => rows
            .iter()
            .map(|row| {
                context
                    .shape_file_node(row.id, ResultDomain::Files, true, &includes, true)
                    .map(QueryResultNode::File)
            })
            .collect::<Result<Vec<_>, QueryShapeError>>()?,
    };

    Ok(QueryResponse {
        target: match rows {
            QueryRows::Headings(_) => QueryTarget::Headings,
            QueryRows::Links(_) => QueryTarget::Links,
            QueryRows::Files(_) => QueryTarget::Files,
        },
        output: options.output_mode,
        includes,
        results,
    })
}

pub fn shape_matched_heading_nodes(
    connection: &Connection,
    heading_ids: &[i64],
) -> Result<Vec<HeadingResultNode>, QueryShapeError> {
    if heading_ids.is_empty() {
        return Ok(Vec::new());
    }

    let file_ids = load_file_ids_for_headings(connection, heading_ids)?;
    let context = EnrichmentContext {
        files: load_files(connection, &file_ids)?,
        headings: load_headings_for_files(connection, &file_ids)?,
        properties: HashMap::new(),
        effective_properties: HashMap::new(),
        keywords: HashMap::new(),
        links_by_file: HashMap::new(),
        links_by_heading: HashMap::new(),
        backlinks_by_file: HashMap::new(),
        backlinks_by_heading: HashMap::new(),
    };

    heading_ids
        .iter()
        .map(|heading_id| context.shape_heading_node(*heading_id, true, &[], false))
        .collect()
}

fn normalized_includes(includes: &[QueryInclude]) -> Vec<QueryInclude> {
    includes
        .iter()
        .copied()
        .collect::<BTreeSet<_>>()
        .into_iter()
        .collect()
}

#[derive(Debug, Clone)]
struct StoredFile {
    id: i64,
    path: String,
    name: String,
    dir: String,
    mtime_ns: i64,
    size: i64,
    content_hash: Option<String>,
    indexed_at: Option<i64>,
    root_heading_id: i64,
    root_title: String,
    root_title_raw: Option<String>,
    root_line_number: Option<i64>,
}

#[derive(Debug, Clone)]
struct StoredHeading {
    id: i64,
    file_id: i64,
    parent_id: Option<i64>,
    level: i64,
    line_number: Option<i64>,
    byte_start: i64,
    byte_end: i64,
    title: String,
    title_raw: Option<String>,
    todo_keyword: Option<String>,
    todo_type: Option<String>,
    priority: Option<String>,
    scheduled_raw: Option<String>,
    scheduled_ts: Option<i64>,
    deadline_raw: Option<String>,
    deadline_ts: Option<i64>,
    closed_raw: Option<String>,
    closed_ts: Option<i64>,
    archivedp: bool,
    footnote_section_p: bool,
    all_tags: Vec<String>,
    breadcrumbs: Vec<String>,
}

#[derive(Debug, Clone)]
struct StoredLink {
    id: i64,
    file_id: i64,
    file_path: String,
    heading_id: i64,
    source_context: String,
    format: String,
    link_type: String,
    raw: String,
    raw_target: String,
    raw_description: Option<String>,
    link_path: String,
    search_option: Option<String>,
    path_absolute: Option<String>,
    target_file_id: Option<i64>,
    target_heading_id: Option<i64>,
    target_custom_id: Option<String>,
    target_id: Option<String>,
    resolution_status: Option<String>,
    resolution_diagnostic: Option<String>,
    byte_start: i64,
    byte_end: i64,
    line: i64,
}

#[derive(Debug, Clone)]
struct StoredProperty {
    heading_id: i64,
    fact: PropertyFact,
}

#[derive(Debug, Clone)]
struct StoredKeyword {
    heading_id: i64,
    fact: KeywordFact,
}

struct EnrichmentContext {
    files: HashMap<i64, StoredFile>,
    headings: HashMap<i64, StoredHeading>,
    properties: HashMap<i64, Vec<PropertyFact>>,
    effective_properties: HashMap<i64, Vec<EffectivePropertyFact>>,
    keywords: HashMap<i64, Vec<KeywordFact>>,
    links_by_file: HashMap<i64, Vec<StoredLink>>,
    links_by_heading: HashMap<i64, Vec<StoredLink>>,
    backlinks_by_file: HashMap<i64, Vec<StoredLink>>,
    backlinks_by_heading: HashMap<i64, Vec<StoredLink>>,
}

impl EnrichmentContext {
    fn load(
        connection: &Connection,
        rows: &QueryRows,
        includes: &[QueryInclude],
    ) -> Result<Self, QueryShapeError> {
        let include_set = includes.iter().copied().collect::<BTreeSet<_>>();
        let (matched_file_ids, matched_heading_ids, matched_link_rows) = collect_matched_ids(rows);

        let mut relevant_file_ids = matched_file_ids.clone();
        let mut relevant_heading_ids = matched_heading_ids.clone();

        for link in &matched_link_rows {
            relevant_file_ids.insert(link.file_id);
            relevant_heading_ids.insert(link.heading_id);
            if let Some(file_id) = link.target_file_id {
                relevant_file_ids.insert(file_id);
            }
            if let Some(heading_id) = link.target_heading_id {
                relevant_heading_ids.insert(heading_id);
            }
        }

        let links_by_file = if include_set.contains(&QueryInclude::Links) {
            load_links_by_file(connection, &matched_file_ids)?
        } else {
            HashMap::new()
        };
        let backlinks_by_file = if include_set.contains(&QueryInclude::Backlinks) {
            load_backlinks_by_file(connection, &matched_file_ids)?
        } else {
            HashMap::new()
        };
        let links_by_heading = if include_set.contains(&QueryInclude::Links) {
            load_links_by_heading(connection, &matched_heading_ids)?
        } else {
            HashMap::new()
        };
        let backlinks_by_heading = if include_set.contains(&QueryInclude::Backlinks) {
            load_backlinks_by_heading(connection, &matched_heading_ids)?
        } else {
            HashMap::new()
        };

        for links in links_by_file.values() {
            for link in links {
                relevant_file_ids.insert(link.file_id);
                relevant_heading_ids.insert(link.heading_id);
                if let Some(file_id) = link.target_file_id {
                    relevant_file_ids.insert(file_id);
                }
                if let Some(heading_id) = link.target_heading_id {
                    relevant_heading_ids.insert(heading_id);
                }
            }
        }
        for links in backlinks_by_file.values() {
            for link in links {
                relevant_file_ids.insert(link.file_id);
                relevant_heading_ids.insert(link.heading_id);
                if let Some(file_id) = link.target_file_id {
                    relevant_file_ids.insert(file_id);
                }
                if let Some(heading_id) = link.target_heading_id {
                    relevant_heading_ids.insert(heading_id);
                }
            }
        }
        for links in links_by_heading.values() {
            for link in links {
                relevant_file_ids.insert(link.file_id);
                relevant_heading_ids.insert(link.heading_id);
                if let Some(file_id) = link.target_file_id {
                    relevant_file_ids.insert(file_id);
                }
                if let Some(heading_id) = link.target_heading_id {
                    relevant_heading_ids.insert(heading_id);
                }
            }
        }
        for links in backlinks_by_heading.values() {
            for link in links {
                relevant_file_ids.insert(link.file_id);
                relevant_heading_ids.insert(link.heading_id);
                if let Some(file_id) = link.target_file_id {
                    relevant_file_ids.insert(file_id);
                }
                if let Some(heading_id) = link.target_heading_id {
                    relevant_heading_ids.insert(heading_id);
                }
            }
        }

        let files = load_files(connection, &relevant_file_ids)?;
        let headings = load_headings_for_files(connection, &relevant_file_ids)?;

        let mut properties = HashMap::new();
        let loaded_properties = if include_set.contains(&QueryInclude::Properties) {
            Some(load_properties(
                connection,
                &matched_heading_ids,
                &matched_file_ids,
                &files,
                include_set.contains(&QueryInclude::EffectiveProperties),
            )?)
        } else {
            None
        };
        if let Some(loaded_properties) = loaded_properties.as_ref() {
            for property in loaded_properties {
                properties
                    .entry(property.heading_id)
                    .or_insert_with(Vec::new)
                    .push(property.fact.clone());
            }
        }

        let effective_properties = if include_set.contains(&QueryInclude::EffectiveProperties) {
            load_effective_properties(connection, &matched_heading_ids, &matched_file_ids, &files)?
        } else {
            HashMap::new()
        };

        let mut keywords = HashMap::new();
        if include_set.contains(&QueryInclude::Keywords) {
            for keyword in
                load_keywords(connection, &matched_heading_ids, &matched_file_ids, &files)?
            {
                keywords
                    .entry(keyword.heading_id)
                    .or_insert_with(Vec::new)
                    .push(keyword.fact);
            }
        }

        Ok(Self {
            files,
            headings,
            properties,
            effective_properties,
            keywords,
            links_by_file,
            links_by_heading,
            backlinks_by_file,
            backlinks_by_heading,
        })
    }

    fn shape_file_node(
        &self,
        file_id: i64,
        domain: ResultDomain,
        matched: bool,
        includes: &[QueryInclude],
        with_children: bool,
    ) -> Result<FileResultNode, QueryShapeError> {
        let file = self.file(file_id)?;
        let root_heading = self.heading(file.root_heading_id)?;
        Ok(FileResultNode {
            kind: public_result_kind(domain, root_heading.level),
            matched,
            id: file.id,
            level: 0,
            path: file.path.clone(),
            name: file.name.clone(),
            dir: file.dir.clone(),
            title: file.root_title.clone(),
            title_raw: file.root_title_raw.clone(),
            root_heading_id: file.root_heading_id,
            mtime_ns: file.mtime_ns,
            size: file.size,
            content_hash: file.content_hash.clone(),
            indexed_at: file.indexed_at,
            location: Location {
                file_path: file.path.clone(),
                line: file.root_line_number,
                byte_start: None,
                byte_end: None,
            },
            tags: root_heading.all_tags.clone(),
            node_path: includes.contains(&QueryInclude::Path).then(|| {
                vec![self
                    .file_path_entry(file.id)
                    .expect("file path entry should build")]
            }),
            properties: includes.contains(&QueryInclude::Properties).then(|| {
                self.properties
                    .get(&file.root_heading_id)
                    .cloned()
                    .unwrap_or_default()
            }),
            effective_properties: includes.contains(&QueryInclude::EffectiveProperties).then(
                || {
                    self.effective_properties
                        .get(&file.root_heading_id)
                        .cloned()
                        .unwrap_or_default()
                },
            ),
            keywords: includes.contains(&QueryInclude::Keywords).then(|| {
                self.keywords
                    .get(&file.root_heading_id)
                    .cloned()
                    .unwrap_or_default()
            }),
            links: includes
                .contains(&QueryInclude::Links)
                .then(|| self.build_included_links(self.links_by_file.get(&file.id))),
            backlinks: includes
                .contains(&QueryInclude::Backlinks)
                .then(|| self.build_included_links(self.backlinks_by_file.get(&file.id))),
            children: with_children.then(Vec::new),
        })
    }

    fn shape_heading_node(
        &self,
        heading_id: i64,
        matched: bool,
        includes: &[QueryInclude],
        with_children: bool,
    ) -> Result<HeadingResultNode, QueryShapeError> {
        let heading = self.heading(heading_id)?;
        let file = self.file(heading.file_id)?;
        Ok(HeadingResultNode {
            kind: public_result_kind(ResultDomain::Headings, heading.level),
            matched,
            id: heading.id,
            file_id: heading.file_id,
            parent_id: heading.parent_id,
            level: heading.level,
            title: heading.title.clone(),
            title_raw: heading.title_raw.clone(),
            todo_keyword: heading.todo_keyword.clone(),
            todo_type: heading.todo_type.clone(),
            priority: heading.priority.clone(),
            scheduled_raw: heading.scheduled_raw.clone(),
            scheduled_ts: heading.scheduled_ts,
            deadline_raw: heading.deadline_raw.clone(),
            deadline_ts: heading.deadline_ts,
            closed_raw: heading.closed_raw.clone(),
            closed_ts: heading.closed_ts,
            archivedp: heading.archivedp,
            footnote_section_p: heading.footnote_section_p,
            all_tags: heading.all_tags.clone(),
            location: Location {
                file_path: file.path.clone(),
                line: heading.line_number,
                byte_start: Some(heading.byte_start),
                byte_end: Some(heading.byte_end),
            },
            node_path: includes
                .contains(&QueryInclude::Path)
                .then(|| self.path_entries_for_heading(heading.id)),
            properties: includes.contains(&QueryInclude::Properties).then(|| {
                self.properties
                    .get(&heading.id)
                    .cloned()
                    .unwrap_or_default()
            }),
            effective_properties: includes.contains(&QueryInclude::EffectiveProperties).then(
                || {
                    self.effective_properties
                        .get(&heading.id)
                        .cloned()
                        .unwrap_or_default()
                },
            ),
            keywords: includes
                .contains(&QueryInclude::Keywords)
                .then(|| self.keywords.get(&heading.id).cloned().unwrap_or_default()),
            links: includes
                .contains(&QueryInclude::Links)
                .then(|| self.build_included_links(self.links_by_heading.get(&heading.id))),
            backlinks: includes
                .contains(&QueryInclude::Backlinks)
                .then(|| self.build_included_links(self.backlinks_by_heading.get(&heading.id))),
            children: with_children.then(Vec::new),
        })
    }

    fn shape_link_node(
        &self,
        row: &LinkQueryRow,
        includes: &[QueryInclude],
    ) -> Result<LinkResultNode, QueryShapeError> {
        let file = self.file(row.file_id)?;
        Ok(LinkResultNode {
            kind: public_result_kind(ResultDomain::Links, row.heading_level),
            matched: true,
            id: row.id,
            file_id: row.file_id,
            heading_id: row.heading_id,
            heading_level: row.heading_level,
            source_context: row.source_context.clone(),
            format: row.format.clone(),
            link_type: row.link_type.clone(),
            raw: row.raw.clone(),
            raw_target: row.raw_target.clone(),
            raw_description: row.raw_description.clone(),
            link_path: row.path.clone(),
            search_option: row.search_option.clone(),
            path_absolute: row.path_absolute.clone(),
            target_file_id: row.target_file_id,
            target_heading_id: row.target_heading_id,
            target_custom_id: row.target_custom_id.clone(),
            target_id: row.target_id.clone(),
            resolution_status: row.resolution_status.clone(),
            resolution_diagnostic: row.resolution_diagnostic.clone(),
            location: Location {
                file_path: file.path.clone(),
                line: Some(row.line),
                byte_start: Some(row.byte_start),
                byte_end: Some(row.byte_end),
            },
            node_path: includes
                .contains(&QueryInclude::Path)
                .then(|| self.path_entries_for_link_source(row.heading_id)),
            source: includes
                .contains(&QueryInclude::Source)
                .then(|| self.link_source(row.file_id, row.heading_id))
                .transpose()?,
            target: includes
                .contains(&QueryInclude::Target)
                .then(|| self.link_target_from_row(row))
                .transpose()?,
        })
    }

    fn shape_heading_outline(
        &self,
        rows: &[HeadingQueryMatch],
        includes: &[QueryInclude],
    ) -> Result<Vec<QueryResultNode>, QueryShapeError> {
        let mut roots = BTreeMap::<String, FileResultNode>::new();
        for row in rows {
            let file = match row {
                HeadingQueryMatch::File(row) => self.file(row.id)?,
                HeadingQueryMatch::Heading(row) => self.file(row.file_id)?,
            };
            roots.entry(file.path.clone()).or_insert_with(|| {
                self.shape_file_node(file.id, ResultDomain::Headings, false, &[], true)
                    .expect("file root should shape")
            });
        }

        for row in rows {
            let file_node = roots
                .get_mut(match row {
                    HeadingQueryMatch::File(row) => &row.path,
                    HeadingQueryMatch::Heading(row) => &row.file_path,
                })
                .expect("outline file root should exist");
            match row {
                HeadingQueryMatch::File(file_row) => {
                    file_node.matched = true;
                    if includes.contains(&QueryInclude::Path) {
                        file_node.node_path = Some(vec![self.file_path_entry(file_row.id)?]);
                    }
                }
                HeadingQueryMatch::Heading(row) => {
                    let heading_path = self.heading_chain_without_root(row.id)?;
                    insert_heading_outline(file_node, &heading_path, row.id, self, includes)?;
                }
            }
        }

        Ok(roots.into_values().map(QueryResultNode::File).collect())
    }

    fn shape_link_outline(
        &self,
        rows: &[LinkQueryRow],
        includes: &[QueryInclude],
    ) -> Result<Vec<QueryResultNode>, QueryShapeError> {
        let mut roots = BTreeMap::<String, FileResultNode>::new();
        for row in rows {
            let file = self.file(row.file_id)?;
            roots.entry(file.path.clone()).or_insert_with(|| {
                self.shape_file_node(file.id, ResultDomain::Headings, false, &[], true)
                    .expect("file root should shape")
            });
        }

        for row in rows {
            let file = self.file(row.file_id)?;
            let file_node = roots
                .get_mut(&file.path)
                .expect("outline file root should exist");
            if row.heading_level == 0 {
                file_node
                    .children
                    .as_mut()
                    .expect("outline file node should have children")
                    .push(QueryResultNode::Link(Box::new(
                        self.shape_link_node(row, includes)?,
                    )));
            } else {
                let heading_path = self.heading_chain_without_root(row.heading_id)?;
                let parent = ensure_heading_outline_path(file_node, &heading_path, self, &[])?;
                parent
                    .children
                    .as_mut()
                    .expect("outline heading should have children")
                    .push(QueryResultNode::Link(Box::new(
                        self.shape_link_node(row, includes)?,
                    )));
            }
        }

        let mut results = roots.into_values().collect::<Vec<_>>();
        for file in &mut results {
            sort_outline_children(file.children.as_mut());
        }
        Ok(results.into_iter().map(QueryResultNode::File).collect())
    }

    fn build_included_links(&self, links: Option<&Vec<StoredLink>>) -> Vec<IncludedLink> {
        links
            .cloned()
            .unwrap_or_default()
            .into_iter()
            .map(|link| {
                self.included_link(&link)
                    .expect("included link should shape")
            })
            .collect()
    }

    fn included_link(&self, link: &StoredLink) -> Result<IncludedLink, QueryShapeError> {
        let source_path = self.path_entries_for_link_source(link.heading_id);
        Ok(IncludedLink {
            id: link.id,
            source_context: link.source_context.clone(),
            format: link.format.clone(),
            link_type: link.link_type.clone(),
            raw: link.raw.clone(),
            raw_target: link.raw_target.clone(),
            raw_description: link.raw_description.clone(),
            link_path: link.link_path.clone(),
            search_option: link.search_option.clone(),
            path_absolute: link.path_absolute.clone(),
            target_file_id: link.target_file_id,
            target_heading_id: link.target_heading_id,
            target_custom_id: link.target_custom_id.clone(),
            target_id: link.target_id.clone(),
            resolution_status: link.resolution_status.clone(),
            resolution_diagnostic: link.resolution_diagnostic.clone(),
            location: Location {
                file_path: link.file_path.clone(),
                line: Some(link.line),
                byte_start: Some(link.byte_start),
                byte_end: Some(link.byte_end),
            },
            source_path: source_path.clone(),
            source: self.link_source(link.file_id, link.heading_id)?,
            target: self.link_target_from_stored(link)?,
        })
    }

    fn link_source(&self, file_id: i64, heading_id: i64) -> Result<LinkSource, QueryShapeError> {
        let file = self.file(file_id)?;
        let heading = self.heading(heading_id)?;
        let source_path = self.path_entries_for_link_source(heading_id);
        Ok(LinkSource {
            file: FileRef {
                id: file.id,
                path: file.path.clone(),
                title: file.root_title.clone(),
                title_raw: file.root_title_raw.clone(),
            },
            heading: if heading.level == 0 {
                None
            } else {
                Some(self.heading_ref(heading_id)?)
            },
            source_path,
        })
    }

    fn link_target_from_row(&self, row: &LinkQueryRow) -> Result<LinkTarget, QueryShapeError> {
        self.link_target_fields(
            &row.raw_target,
            row.target_file_id,
            row.target_heading_id,
            row.resolution_status.as_deref(),
            row.resolution_diagnostic.clone(),
        )
    }

    fn link_target_from_stored(&self, link: &StoredLink) -> Result<LinkTarget, QueryShapeError> {
        self.link_target_fields(
            &link.raw_target,
            link.target_file_id,
            link.target_heading_id,
            link.resolution_status.as_deref(),
            link.resolution_diagnostic.clone(),
        )
    }

    fn link_target_fields(
        &self,
        raw_target: &str,
        target_file_id: Option<i64>,
        target_heading_id: Option<i64>,
        resolution_status: Option<&str>,
        resolution_diagnostic: Option<String>,
    ) -> Result<LinkTarget, QueryShapeError> {
        let resolved = resolution_status == Some("resolved");
        let file = if resolved {
            target_file_id
                .map(|file_id| self.file_ref(file_id))
                .transpose()?
        } else {
            None
        };
        let heading = if resolved {
            target_heading_id
                .map(|heading_id| self.heading_ref(heading_id))
                .transpose()?
        } else {
            None
        };
        let resolved_kind = if resolved {
            if target_heading_id.is_some() {
                Some(QueryTarget::Headings)
            } else if target_file_id.is_some() {
                Some(QueryTarget::Files)
            } else {
                None
            }
        } else {
            None
        };

        Ok(LinkTarget {
            resolved_kind,
            file,
            heading,
            raw_target: raw_target.to_string(),
            resolution_status: resolution_status.map(str::to_string),
            resolution_diagnostic,
        })
    }

    fn file_ref(&self, file_id: i64) -> Result<FileRef, QueryShapeError> {
        let file = self.file(file_id)?;
        Ok(FileRef {
            id: file.id,
            path: file.path.clone(),
            title: file.root_title.clone(),
            title_raw: file.root_title_raw.clone(),
        })
    }

    fn heading_ref(&self, heading_id: i64) -> Result<HeadingRef, QueryShapeError> {
        let heading = self.heading(heading_id)?;
        Ok(HeadingRef {
            id: heading.id,
            title: heading.title.clone(),
            title_raw: heading
                .title_raw
                .clone()
                .expect("non-root heading refs should have title_raw"),
            level: heading.level,
            outline_path: strip_root_breadcrumb(&heading.breadcrumbs, heading.level),
        })
    }

    fn path_entries_for_heading(&self, heading_id: i64) -> Vec<PathEntry> {
        let heading = self.heading(heading_id).expect("heading should exist");
        let file = self.file(heading.file_id).expect("file should exist");
        let mut path = vec![PathEntry::File(FilePathEntry {
            id: file.id,
            path: file.path.clone(),
            title: file.root_title.clone(),
            title_raw: file.root_title_raw.clone(),
        })];

        for id in self
            .heading_chain_without_root(heading_id)
            .expect("heading chain should exist")
        {
            let entry = self.heading(id).expect("heading should exist");
            path.push(PathEntry::Heading(HeadingPathEntry {
                id: entry.id,
                title: entry.title.clone(),
                title_raw: entry
                    .title_raw
                    .clone()
                    .expect("non-root heading path entries should have title_raw"),
                level: entry.level,
            }));
        }

        path
    }

    fn path_entries_for_link_source(&self, heading_id: i64) -> Vec<PathEntry> {
        let heading = self.heading(heading_id).expect("heading should exist");
        if heading.level == 0 {
            let file = self.file(heading.file_id).expect("file should exist");
            return vec![PathEntry::File(FilePathEntry {
                id: file.id,
                path: file.path.clone(),
                title: file.root_title.clone(),
                title_raw: file.root_title_raw.clone(),
            })];
        }
        self.path_entries_for_heading(heading_id)
    }

    fn heading_chain_without_root(&self, heading_id: i64) -> Result<Vec<i64>, QueryShapeError> {
        let mut chain = Vec::new();
        let mut current = Some(heading_id);
        while let Some(id) = current {
            let heading = self.heading(id)?;
            if heading.level == 0 {
                break;
            }
            chain.push(heading.id);
            current = heading.parent_id;
        }
        chain.reverse();
        Ok(chain)
    }

    fn file(&self, file_id: i64) -> Result<&StoredFile, QueryShapeError> {
        self.files.get(&file_id).ok_or_else(|| {
            QueryShapeError::missing(format!("missing stored file row for id {file_id}"))
        })
    }

    fn heading(&self, heading_id: i64) -> Result<&StoredHeading, QueryShapeError> {
        self.headings.get(&heading_id).ok_or_else(|| {
            QueryShapeError::missing(format!("missing stored heading row for id {heading_id}"))
        })
    }

    fn file_path_entry(&self, file_id: i64) -> Result<PathEntry, QueryShapeError> {
        let file = self.file(file_id)?;
        Ok(PathEntry::File(FilePathEntry {
            id: file.id,
            path: file.path.clone(),
            title: file.root_title.clone(),
            title_raw: file.root_title_raw.clone(),
        }))
    }
}

fn insert_heading_outline(
    file_node: &mut FileResultNode,
    heading_chain: &[i64],
    matched_heading_id: i64,
    context: &EnrichmentContext,
    includes: &[QueryInclude],
) -> Result<(), QueryShapeError> {
    let parent = ensure_heading_outline_path(file_node, heading_chain, context, includes)?;
    parent.matched = true;
    *parent = context.shape_heading_node(matched_heading_id, true, includes, true)?;
    Ok(())
}

fn ensure_heading_outline_path<'a>(
    file_node: &'a mut FileResultNode,
    heading_chain: &[i64],
    context: &EnrichmentContext,
    _includes: &[QueryInclude],
) -> Result<&'a mut HeadingResultNode, QueryShapeError> {
    ensure_heading_outline_children(
        file_node
            .children
            .as_mut()
            .expect("outline file node should have children"),
        heading_chain,
        context,
    )
}

fn ensure_heading_outline_children<'a>(
    children: &'a mut Vec<QueryResultNode>,
    heading_chain: &[i64],
    context: &EnrichmentContext,
) -> Result<&'a mut HeadingResultNode, QueryShapeError> {
    let (head, tail) = heading_chain
        .split_first()
        .ok_or_else(|| QueryShapeError::missing("missing outline heading chain"))?;
    let index = if let Some(index) = children
        .iter()
        .position(|node| matches!(node, QueryResultNode::Heading(heading) if heading.id == *head))
    {
        index
    } else {
        children.push(QueryResultNode::Heading(context.shape_heading_node(
            *head,
            false,
            &[],
            true,
        )?));
        children.len() - 1
    };
    let node = match &mut children[index] {
        QueryResultNode::Heading(node) => node,
        _ => unreachable!("outline path nodes should be headings"),
    };
    if tail.is_empty() {
        Ok(node)
    } else {
        ensure_heading_outline_children(
            node.children
                .as_mut()
                .expect("outline heading should have children"),
            tail,
            context,
        )
    }
}

fn sort_outline_children(children: Option<&mut Vec<QueryResultNode>>) {
    if let Some(children) = children {
        for child in children.iter_mut() {
            match child {
                QueryResultNode::File(node) => sort_outline_children(node.children.as_mut()),
                QueryResultNode::Heading(node) => sort_outline_children(node.children.as_mut()),
                QueryResultNode::Link(_) => {}
            }
        }
        children.sort_by_key(node_sort_key);
    }
}

fn node_sort_key(node: &QueryResultNode) -> (i64, i64, u8) {
    match node {
        QueryResultNode::File(node) => (node.location.byte_start.unwrap_or(i64::MIN), node.id, 0),
        QueryResultNode::Heading(node) => {
            (node.location.byte_start.unwrap_or(i64::MIN), node.id, 1)
        }
        QueryResultNode::Link(node) => (node.location.byte_start.unwrap_or(i64::MIN), node.id, 2),
    }
}

fn collect_matched_ids(rows: &QueryRows) -> (BTreeSet<i64>, BTreeSet<i64>, Vec<LinkQueryRow>) {
    match rows {
        QueryRows::Headings(rows) => {
            let mut file_ids = BTreeSet::new();
            let mut heading_ids = BTreeSet::new();
            for row in rows {
                match row {
                    HeadingQueryMatch::File(row) => {
                        file_ids.insert(row.id);
                    }
                    HeadingQueryMatch::Heading(row) => {
                        file_ids.insert(row.file_id);
                        heading_ids.insert(row.id);
                    }
                }
            }
            (file_ids, heading_ids, Vec::new())
        }
        QueryRows::Links(rows) => {
            let mut file_ids = BTreeSet::new();
            let mut heading_ids = BTreeSet::new();
            for row in rows {
                file_ids.insert(row.file_id);
                heading_ids.insert(row.heading_id);
            }
            (file_ids, heading_ids, rows.clone())
        }
        QueryRows::Files(rows) => {
            let file_ids = rows.iter().map(|row| row.id).collect::<BTreeSet<_>>();
            (file_ids, BTreeSet::new(), Vec::new())
        }
    }
}

fn load_files(
    connection: &Connection,
    file_ids: &BTreeSet<i64>,
) -> Result<HashMap<i64, StoredFile>, QueryShapeError> {
    if file_ids.is_empty() {
        return Ok(HashMap::new());
    }

    let sql = format!(
        "SELECT
            files.id,
            files.path,
            files.mtime_ns,
            files.size,
            files.content_hash,
            files.indexed_at,
            root.id,
            root.title,
            root.title_raw,
            root.line_number
         FROM files
         INNER JOIN headings AS root ON root.file_id = files.id AND root.level = 0
         WHERE files.id IN ({})
         ORDER BY files.path",
        placeholders(file_ids.len())
    );
    let params = file_ids.iter().copied().collect::<Vec<_>>();
    let mut statement = connection
        .prepare(&sql)
        .map_err(|source| QueryShapeError::database("load_files.prepare", source))?;
    let rows = statement
        .query_map(params_from_iter(params.iter()), |row| {
            let path: String = row.get(1)?;
            let path_ref = Path::new(&path);
            let name = path_ref
                .file_name()
                .and_then(|value| value.to_str())
                .unwrap_or(path.as_str())
                .to_string();
            let dir = path_ref
                .parent()
                .and_then(|value| value.to_str())
                .unwrap_or(".")
                .to_string();
            Ok(StoredFile {
                id: row.get(0)?,
                path,
                name,
                dir,
                mtime_ns: row.get(2)?,
                size: row.get(3)?,
                content_hash: row.get(4)?,
                indexed_at: row.get(5)?,
                root_heading_id: row.get(6)?,
                root_title: row.get(7)?,
                root_title_raw: row.get(8)?,
                root_line_number: row.get(9)?,
            })
        })
        .map_err(|source| QueryShapeError::database("load_files.query", source))?;

    rows.collect::<Result<Vec<_>, _>>()
        .map_err(|source| QueryShapeError::database("load_files.collect", source))
        .map(|rows| rows.into_iter().map(|row| (row.id, row)).collect())
}

fn load_file_ids_for_headings(
    connection: &Connection,
    heading_ids: &[i64],
) -> Result<BTreeSet<i64>, QueryShapeError> {
    let sql = format!(
        "SELECT DISTINCT file_id FROM headings WHERE id IN ({}) ORDER BY file_id",
        placeholders(heading_ids.len())
    );
    let mut statement = connection.prepare(&sql).map_err(|source| {
        QueryShapeError::database("load_file_ids_for_headings.prepare", source)
    })?;
    let rows = statement
        .query_map(params_from_iter(heading_ids.iter()), |row| row.get(0))
        .map_err(|source| QueryShapeError::database("load_file_ids_for_headings.query", source))?;

    rows.collect::<Result<BTreeSet<_>, _>>()
        .map_err(|source| QueryShapeError::database("load_file_ids_for_headings.collect", source))
}

fn load_headings_for_files(
    connection: &Connection,
    file_ids: &BTreeSet<i64>,
) -> Result<HashMap<i64, StoredHeading>, QueryShapeError> {
    if file_ids.is_empty() {
        return Ok(HashMap::new());
    }

    let sql = format!(
        "SELECT
            headings.id,
            headings.file_id,
            headings.parent_id,
            headings.level,
            headings.line_number,
            headings.byte_start,
            headings.byte_end,
            headings.title,
            headings.title_raw,
            headings.todo_keyword,
            headings.todo_type,
            headings.priority,
            headings.scheduled_raw,
            headings.scheduled_ts,
            headings.deadline_raw,
            headings.deadline_ts,
            headings.closed_raw,
            headings.closed_ts,
            headings.archivedp,
            headings.footnote_section_p,
            outline_path.breadcrumbs_json
         FROM headings
         LEFT JOIN outline_path ON outline_path.heading_id = headings.id
         INNER JOIN files ON files.id = headings.file_id
         WHERE headings.file_id IN ({})
         ORDER BY files.path, headings.byte_start, headings.id",
        placeholders(file_ids.len())
    );
    let params = file_ids.iter().copied().collect::<Vec<_>>();
    let mut statement = connection
        .prepare(&sql)
        .map_err(|source| QueryShapeError::database("load_headings.prepare", source))?;
    let rows = statement
        .query_map(params_from_iter(params.iter()), |row| {
            let priority: Option<String> = row.get(11)?;
            Ok((
                row.get::<_, i64>(0)?,
                row.get::<_, Option<String>>(20)?,
                StoredHeading {
                    id: row.get(0)?,
                    file_id: row.get(1)?,
                    parent_id: row.get(2)?,
                    level: row.get(3)?,
                    line_number: row.get(4)?,
                    byte_start: row.get(5)?,
                    byte_end: row.get(6)?,
                    title: row.get(7)?,
                    title_raw: row.get(8)?,
                    todo_keyword: row.get(9)?,
                    todo_type: row.get(10)?,
                    priority,
                    scheduled_raw: row.get(12)?,
                    scheduled_ts: row.get(13)?,
                    deadline_raw: row.get(14)?,
                    deadline_ts: row.get(15)?,
                    closed_raw: row.get(16)?,
                    closed_ts: row.get(17)?,
                    archivedp: row.get::<_, i64>(18)? != 0,
                    footnote_section_p: row.get::<_, i64>(19)? != 0,
                    all_tags: Vec::new(),
                    breadcrumbs: Vec::new(),
                },
            ))
        })
        .map_err(|source| QueryShapeError::database("load_headings.query", source))?;

    let mut headings = HashMap::new();
    for row in rows
        .collect::<Result<Vec<_>, _>>()
        .map_err(|source| QueryShapeError::database("load_headings.collect", source))?
    {
        let (id, breadcrumbs_json, mut heading) = row;
        let breadcrumbs_json = breadcrumbs_json.ok_or_else(|| {
            QueryShapeError::missing(format!(
                "missing outline_path row for stored heading id {id}"
            ))
        })?;
        heading.breadcrumbs = serde_json::from_str(&breadcrumbs_json)
            .map_err(|source| QueryShapeError::invalid_json("breadcrumbs_json", id, source))?;
        headings.insert(id, heading);
    }
    load_effective_tags_for_stored_headings(connection, &mut headings)?;
    Ok(headings)
}

fn load_effective_tags_for_stored_headings(
    connection: &Connection,
    headings: &mut HashMap<i64, StoredHeading>,
) -> Result<(), QueryShapeError> {
    if headings.is_empty() {
        return Ok(());
    }

    const TAG_LOAD_CHUNK_SIZE: usize = 900;
    let heading_ids = headings.keys().copied().collect::<Vec<_>>();
    for chunk in heading_ids.chunks(TAG_LOAD_CHUNK_SIZE) {
        let sql = format!(
            "SELECT heading_id, tag
             FROM effective_tags
             WHERE heading_id IN ({})
             ORDER BY heading_id, position",
            placeholders(chunk.len())
        );
        let mut statement = connection
            .prepare(&sql)
            .map_err(|source| QueryShapeError::database("load_effective_tags.prepare", source))?;
        let rows = statement
            .query_map(params_from_iter(chunk.iter()), |row| {
                Ok((row.get::<_, i64>(0)?, row.get::<_, String>(1)?))
            })
            .map_err(|source| QueryShapeError::database("load_effective_tags.query", source))?;
        for row in rows {
            let (heading_id, tag) = row.map_err(|source| {
                QueryShapeError::database("load_effective_tags.collect", source)
            })?;
            let heading = headings.get_mut(&heading_id).ok_or_else(|| {
                QueryShapeError::missing(format!(
                    "effective_tags references unloaded heading id {heading_id}"
                ))
            })?;
            heading.all_tags.push(tag);
        }
    }
    Ok(())
}

fn load_properties(
    connection: &Connection,
    heading_ids: &BTreeSet<i64>,
    file_ids: &BTreeSet<i64>,
    files: &HashMap<i64, StoredFile>,
    include_all_file_headings: bool,
) -> Result<Vec<StoredProperty>, QueryShapeError> {
    let (sql, params) = if include_all_file_headings {
        if file_ids.is_empty() {
            return Ok(Vec::new());
        }
        (
            format!(
                "SELECT properties.id, properties.heading_id, properties.key, properties.value,
                        properties.source, properties.append, properties.line_number
                 FROM properties
                 INNER JOIN headings ON headings.id = properties.heading_id
                 WHERE headings.file_id IN ({})
                 ORDER BY properties.heading_id, properties.line_number, properties.id",
                placeholders(file_ids.len())
            ),
            file_ids.iter().copied().collect::<Vec<_>>(),
        )
    } else {
        let mut target_ids = heading_ids.clone();
        for file_id in file_ids {
            if let Some(file) = files.get(file_id) {
                target_ids.insert(file.root_heading_id);
            }
        }
        if target_ids.is_empty() {
            return Ok(Vec::new());
        }
        (
            format!(
                "SELECT properties.id, heading_id, key, value, source, append, line_number
                 FROM properties
                 WHERE heading_id IN ({})
                 ORDER BY heading_id, line_number, id",
                placeholders(target_ids.len())
            ),
            target_ids.iter().copied().collect::<Vec<_>>(),
        )
    };
    let mut statement = connection
        .prepare(&sql)
        .map_err(|source| QueryShapeError::database("load_properties.prepare", source))?;
    let rows = statement
        .query_map(params_from_iter(params.iter()), |row| {
            Ok(StoredProperty {
                heading_id: row.get(1)?,
                fact: PropertyFact {
                    key: row.get(2)?,
                    value: row.get(3)?,
                    source: row.get(4)?,
                    append: row.get::<_, i64>(5)? != 0,
                    line_number: row.get(6)?,
                },
            })
        })
        .map_err(|source| QueryShapeError::database("load_properties.query", source))?;
    rows.collect::<Result<Vec<_>, _>>()
        .map_err(|source| QueryShapeError::database("load_properties.collect", source))
}

fn load_effective_properties(
    connection: &Connection,
    heading_ids: &BTreeSet<i64>,
    file_ids: &BTreeSet<i64>,
    files: &HashMap<i64, StoredFile>,
) -> Result<HashMap<i64, Vec<EffectivePropertyFact>>, QueryShapeError> {
    let mut target_ids = heading_ids.clone();
    for file_id in file_ids {
        if let Some(file) = files.get(file_id) {
            target_ids.insert(file.root_heading_id);
        }
    }
    if target_ids.is_empty() {
        return Ok(HashMap::new());
    }
    let sql = format!("SELECT heading_id, key, effective_value FROM effective_properties WHERE heading_id IN ({}) ORDER BY heading_id, key", placeholders(target_ids.len()));
    let mut statement = connection
        .prepare(&sql)
        .map_err(|source| QueryShapeError::database("load_effective_properties.prepare", source))?;
    let rows = statement
        .query_map(params_from_iter(target_ids.iter()), |row| {
            Ok((
                row.get::<_, i64>(0)?,
                EffectivePropertyFact {
                    key: row.get(1)?,
                    value: Some(row.get(2)?),
                },
            ))
        })
        .map_err(|source| QueryShapeError::database("load_effective_properties.query", source))?;
    let mut effective = HashMap::new();
    for heading_id in target_ids {
        effective.insert(heading_id, Vec::new());
    }
    for row in rows
        .collect::<Result<Vec<_>, _>>()
        .map_err(|source| QueryShapeError::database("load_effective_properties.collect", source))?
    {
        effective.entry(row.0).or_insert_with(Vec::new).push(row.1);
    }
    Ok(effective)
}

fn load_keywords(
    connection: &Connection,
    heading_ids: &BTreeSet<i64>,
    file_ids: &BTreeSet<i64>,
    files: &HashMap<i64, StoredFile>,
) -> Result<Vec<StoredKeyword>, QueryShapeError> {
    let mut target_ids = heading_ids.clone();
    for file_id in file_ids {
        if let Some(file) = files.get(file_id) {
            target_ids.insert(file.root_heading_id);
        }
    }
    if target_ids.is_empty() {
        return Ok(Vec::new());
    }

    let sql = format!(
        "SELECT heading_id, keyword, value, line_number
         FROM keywords
         WHERE heading_id IN ({})
         ORDER BY heading_id, line_number, id",
        placeholders(target_ids.len())
    );
    let params = target_ids.iter().copied().collect::<Vec<_>>();
    let mut statement = connection
        .prepare(&sql)
        .map_err(|source| QueryShapeError::database("load_keywords.prepare", source))?;
    let rows = statement
        .query_map(params_from_iter(params.iter()), |row| {
            Ok(StoredKeyword {
                heading_id: row.get(0)?,
                fact: KeywordFact {
                    keyword: row.get(1)?,
                    value: row.get(2)?,
                    line_number: row.get(3)?,
                },
            })
        })
        .map_err(|source| QueryShapeError::database("load_keywords.query", source))?;
    rows.collect::<Result<Vec<_>, _>>()
        .map_err(|source| QueryShapeError::database("load_keywords.collect", source))
}

fn load_links_by_file(
    connection: &Connection,
    file_ids: &BTreeSet<i64>,
) -> Result<HashMap<i64, Vec<StoredLink>>, QueryShapeError> {
    load_link_map(connection, "links.file_id", file_ids)
}

fn load_backlinks_by_file(
    connection: &Connection,
    file_ids: &BTreeSet<i64>,
) -> Result<HashMap<i64, Vec<StoredLink>>, QueryShapeError> {
    load_link_map(connection, "links.target_file_id", file_ids)
}

fn load_links_by_heading(
    connection: &Connection,
    heading_ids: &BTreeSet<i64>,
) -> Result<HashMap<i64, Vec<StoredLink>>, QueryShapeError> {
    load_link_map(connection, "links.heading_id", heading_ids)
}

fn load_backlinks_by_heading(
    connection: &Connection,
    heading_ids: &BTreeSet<i64>,
) -> Result<HashMap<i64, Vec<StoredLink>>, QueryShapeError> {
    load_link_map(connection, "links.target_heading_id", heading_ids)
}

fn load_link_map(
    connection: &Connection,
    id_column: &str,
    ids: &BTreeSet<i64>,
) -> Result<HashMap<i64, Vec<StoredLink>>, QueryShapeError> {
    if ids.is_empty() {
        return Ok(HashMap::new());
    }
    let sql = format!(
        "SELECT
            links.id,
            links.file_id,
            files.path,
            links.heading_id,
            headings.level,
            links.source_context,
            links.format,
            links.link_type,
            links.raw,
            links.raw_target,
            links.raw_description,
            links.path,
            links.search_option,
            links.path_absolute,
            links.target_file_id,
            links.target_heading_id,
            links.target_custom_id,
            links.target_id,
            links.resolution_status,
            links.resolution_diagnostic,
            links.byte_start,
            links.byte_end,
            links.line,
            {id_column}
         FROM links
         INNER JOIN files ON files.id = links.file_id
         INNER JOIN headings ON headings.id = links.heading_id
         WHERE {id_column} IN ({})
         ORDER BY {id_column}, files.path, links.byte_start, links.id",
        placeholders(ids.len())
    );
    let params = ids.iter().copied().collect::<Vec<_>>();
    let mut statement = connection
        .prepare(&sql)
        .map_err(|source| QueryShapeError::database("load_link_map.prepare", source))?;
    let rows = statement
        .query_map(params_from_iter(params.iter()), |row| {
            Ok((
                row.get::<_, i64>(23)?,
                StoredLink {
                    id: row.get(0)?,
                    file_id: row.get(1)?,
                    file_path: row.get(2)?,
                    heading_id: row.get(3)?,
                    source_context: row.get(5)?,
                    format: row.get(6)?,
                    link_type: row.get(7)?,
                    raw: row.get(8)?,
                    raw_target: row.get(9)?,
                    raw_description: row.get(10)?,
                    link_path: row.get(11)?,
                    search_option: row.get(12)?,
                    path_absolute: row.get(13)?,
                    target_file_id: row.get(14)?,
                    target_heading_id: row.get(15)?,
                    target_custom_id: row.get(16)?,
                    target_id: row.get(17)?,
                    resolution_status: row.get(18)?,
                    resolution_diagnostic: row.get(19)?,
                    byte_start: row.get(20)?,
                    byte_end: row.get(21)?,
                    line: row.get(22)?,
                },
            ))
        })
        .map_err(|source| QueryShapeError::database("load_link_map.query", source))?;
    let mut grouped = HashMap::<i64, Vec<StoredLink>>::new();
    for row in rows
        .collect::<Result<Vec<_>, _>>()
        .map_err(|source| QueryShapeError::database("load_link_map.collect", source))?
    {
        grouped.entry(row.0).or_default().push(row.1);
    }
    Ok(grouped)
}

fn placeholders(count: usize) -> String {
    std::iter::repeat_n("?", count)
        .collect::<Vec<_>>()
        .join(", ")
}

fn strip_root_breadcrumb(breadcrumbs: &[String], heading_level: i64) -> Vec<String> {
    if heading_level == 0 {
        Vec::new()
    } else {
        breadcrumbs.iter().skip(1).cloned().collect()
    }
}

#[cfg(test)]
mod tests {
    use super::{
        execute_and_shape_query, shape_query_results, EffectivePropertyFact, QueryExecutionOptions,
        QueryInclude, QueryOutputMode, QueryResponse, QueryResultKind, QueryResultNode,
    };
    use crate::db::{
        open_in_memory_database_with_schema, DbWriter, EffectivePropertyRecord, EffectiveTagRecord,
        FileRecordInput, HeadingRecord, KeywordRecord, LinkRecord, OutlinePathRecord,
        PropertyRecord, SchemaDefinition, TagRecord,
    };
    use crate::property::{derive_effective_properties, PropertyRow};
    use crate::query::{
        execute_sqlite_query, parse_query, validate_query, QueryTarget, QueryValidationOptions,
    };
    use crate::tag::derive_effective_tags;
    use rusqlite::Connection;
    use serde_json::Value;
    use std::path::Path;

    #[test]
    fn query_result_kind_classifies_heading_levels() {
        assert_eq!(
            QueryResultKind::from_heading_level(0),
            QueryResultKind::Root
        );
        assert_eq!(
            QueryResultKind::from_heading_level(1),
            QueryResultKind::Heading
        );
    }

    #[test]
    fn link_output_preserves_omitted_root_sources_and_uses_root_outline_nodes() {
        let connection = seeded_connection();
        let query = validated(r#"(links (status "resolved"))"#);
        let rows = execute_sqlite_query(&connection, &query).expect("query should execute");
        let response = shape_query_results(
            &connection,
            rows,
            &QueryExecutionOptions {
                output_mode: QueryOutputMode::Flat,
                includes: vec![
                    QueryInclude::Path,
                    QueryInclude::Source,
                    QueryInclude::Target,
                ],
                ..QueryExecutionOptions::default()
            },
        )
        .expect("results should shape");

        let json = serde_json::to_value(&response).expect("response should serialize");
        let first = &json["results"][0];
        assert_eq!(first["kind"], "link");
        assert_eq!(first["link_path"], "beta.org");
        assert!(first.get("path").is_none());
        assert!(first["node_path"].is_array());
        assert_eq!(first["node_path"][0]["kind"], "file");
        assert_eq!(first["source"]["heading"], Value::Null);
        assert_eq!(
            first["source"]["source_path"].as_array().map(Vec::len),
            Some(1)
        );
        assert_eq!(first["target"]["resolved_kind"], "files");
        assert_eq!(first["target"]["file"]["path"], "/tmp/query-beta.org");

        let second = &json["results"][1];
        assert_eq!(second["source"]["heading"]["id"], 11);
        assert_eq!(second["source"]["heading"]["title"], "Query Engine");
        assert_eq!(second["target"]["resolved_kind"], "files");
        assert!(second["target"]["heading"].is_null());

        let third = &json["results"][2];
        assert_eq!(third["source"]["heading"]["id"], 12);
        assert_eq!(third["target"]["resolved_kind"], "headings");
        assert_eq!(third["target"]["file"]["path"], "/tmp/query-beta.org");
        assert_eq!(third["target"]["heading"]["id"], 21);
        assert_eq!(third["target"]["heading"]["title"], "Beta Target");

        let outline_rows = execute_sqlite_query(&connection, &query).expect("query should execute");
        let outline = shape_query_results(
            &connection,
            outline_rows,
            &QueryExecutionOptions {
                output_mode: QueryOutputMode::Outline,
                ..QueryExecutionOptions::default()
            },
        )
        .expect("outline results should shape");
        let outline_json = serde_json::to_value(&outline).expect("outline should serialize");
        assert_eq!(outline_json["results"][0]["kind"], "root");
    }

    #[test]
    fn broken_link_target_preserves_status_without_resolved_objects() {
        let connection = seeded_connection();
        let response = execute_and_shape_query(
            &connection,
            &validated(r#"(links (status "broken"))"#),
            &QueryExecutionOptions {
                output_mode: QueryOutputMode::Flat,
                includes: vec![QueryInclude::Source, QueryInclude::Target],
                ..QueryExecutionOptions::default()
            },
        )
        .expect("query should shape");

        let link = link_node(&response.results[0]);
        let target = link.target.as_ref().expect("target include should exist");
        assert_eq!(target.resolution_status.as_deref(), Some("broken"));
        assert_eq!(
            target.resolution_diagnostic.as_deref(),
            Some("missing target")
        );
        assert!(target.file.is_none());
        assert!(target.heading.is_none());
        assert!(target.resolved_kind.is_none());
    }

    #[test]
    fn missing_outline_path_row_reports_outline_specific_error() {
        let connection = seeded_connection();
        connection
            .execute("DELETE FROM outline_path WHERE heading_id = 11", [])
            .expect("outline path row should delete");

        let error = execute_and_shape_query(
            &connection,
            &validated(r#"(headings (title "Query Engine" :exact t))"#),
            &QueryExecutionOptions::default(),
        )
        .expect_err("query shaping should fail when outline_path is missing");

        assert!(
            error
                .to_string()
                .contains("missing outline_path row for stored heading id 11"),
            "expected outline-specific error, got {error}"
        );
    }

    #[test]
    fn file_links_include_is_file_wide() {
        let connection = seeded_connection();
        let response = execute_and_shape_query(
            &connection,
            &validated(r#"(files (file-title "Alpha Index" :exact t))"#),
            &QueryExecutionOptions {
                output_mode: QueryOutputMode::Flat,
                includes: vec![QueryInclude::Links],
                ..QueryExecutionOptions::default()
            },
        )
        .expect("query should shape");

        let file = file_node(&response.results[0]);
        let links = file.links.as_ref().expect("links include should exist");
        let ids = links.iter().map(|link| link.id).collect::<Vec<_>>();
        assert_eq!(ids, vec![102, 100, 101, 105]);
        assert_eq!(links[0].source_path.len(), 1);
        assert_eq!(links[1].source_path.len(), 2);
    }

    #[test]
    fn heading_properties_include_is_exact_and_omitted_by_default() {
        let connection = seeded_connection();
        let plain = execute_and_shape_query(
            &connection,
            &validated(r#"(headings (title "Query Engine" :exact t))"#),
            &QueryExecutionOptions::default(),
        )
        .expect("plain query should shape");
        let plain_json = serde_json::to_value(&plain).expect("plain response should serialize");
        assert!(plain_json["results"][0].get("properties").is_none());

        let included = execute_and_shape_query(
            &connection,
            &validated(r#"(headings (title "Query Engine" :exact t))"#),
            &QueryExecutionOptions {
                output_mode: QueryOutputMode::Flat,
                includes: vec![QueryInclude::Properties],
                ..QueryExecutionOptions::default()
            },
        )
        .expect("included query should shape");
        let heading = heading_node(&included.results[0]);
        let properties = heading
            .properties
            .as_ref()
            .expect("properties include should exist");
        assert_eq!(properties.len(), 1);
        assert_eq!(properties[0].key, "AREA");
        assert_eq!(properties[0].value.as_deref(), Some("infra"));
        assert_eq!(properties[0].source, "property_drawer");
        assert!(!properties[0].append);
        assert_eq!(properties[0].line_number, Some(4));
        assert_eq!(matched_heading_ids(&plain), matched_heading_ids(&included));
    }

    #[test]
    fn effective_properties_include_resolves_inherited_values_and_is_omitted_by_default() {
        let connection = seeded_connection();
        let plain = execute_and_shape_query(
            &connection,
            &validated(r#"(headings (title "Nested" :exact t))"#),
            &QueryExecutionOptions::default(),
        )
        .expect("plain query should shape");
        let plain_json = serde_json::to_value(&plain).expect("plain response should serialize");
        assert!(plain_json["results"][0]
            .get("effective_properties")
            .is_none());

        let included = execute_and_shape_query(
            &connection,
            &validated(r#"(headings (title "Nested" :exact t))"#),
            &QueryExecutionOptions {
                output_mode: QueryOutputMode::Flat,
                includes: vec![QueryInclude::EffectiveProperties],
                ..QueryExecutionOptions::default()
            },
        )
        .expect("included query should shape");
        let heading = heading_node(&included.results[0]);
        assert!(heading.properties.is_none());
        let properties = heading
            .effective_properties
            .as_ref()
            .expect("effective properties include should exist");
        assert_eq!(
            properties,
            &vec![
                EffectivePropertyFact {
                    key: "AREA".to_string(),
                    value: Some("infra".to_string()),
                },
                EffectivePropertyFact {
                    key: "CATEGORY".to_string(),
                    value: Some("work".to_string()),
                },
            ]
        );
        assert_eq!(matched_heading_ids(&plain), matched_heading_ids(&included));
    }

    #[test]
    fn effective_properties_include_uses_last_local_base_plus_all_appends() {
        let connection = seeded_connection();
        let response = execute_and_shape_query(
            &connection,
            &validated(r#"(headings (title "Loose Note" :exact t))"#),
            &QueryExecutionOptions {
                output_mode: QueryOutputMode::Flat,
                includes: vec![QueryInclude::EffectiveProperties],
                ..QueryExecutionOptions::default()
            },
        )
        .expect("query should shape");

        let heading = heading_node(&response.results[0]);
        assert_eq!(
            heading.effective_properties.as_ref(),
            Some(&vec![
                EffectivePropertyFact {
                    key: "APPEND_REPLACED".to_string(),
                    value: Some("second appended".to_string()),
                },
                EffectivePropertyFact {
                    key: "CATEGORY".to_string(),
                    value: Some("work".to_string()),
                },
            ])
        );
    }

    #[test]
    fn file_properties_and_keywords_includes_use_root_stored_facts_and_omit_by_default() {
        let connection = seeded_connection();
        let plain = execute_and_shape_query(
            &connection,
            &validated(r#"(files (file-title "Alpha Index" :exact t))"#),
            &QueryExecutionOptions::default(),
        )
        .expect("plain query should shape");
        let plain_json = serde_json::to_value(&plain).expect("plain response should serialize");
        assert!(plain_json["results"][0].get("properties").is_none());
        assert!(plain_json["results"][0].get("keywords").is_none());

        let included = execute_and_shape_query(
            &connection,
            &validated(r#"(files (file-title "Alpha Index" :exact t))"#),
            &QueryExecutionOptions {
                output_mode: QueryOutputMode::Flat,
                includes: vec![QueryInclude::Properties, QueryInclude::Keywords],
                ..QueryExecutionOptions::default()
            },
        )
        .expect("included query should shape");
        let file = file_node(&included.results[0]);
        let properties = file
            .properties
            .as_ref()
            .expect("properties include should exist");
        assert_eq!(properties.len(), 1);
        assert_eq!(properties[0].key, "CATEGORY");
        assert_eq!(properties[0].value.as_deref(), Some("work"));
        assert_eq!(properties[0].source, "category_keyword");
        let keywords = file
            .keywords
            .as_ref()
            .expect("keywords include should exist");
        assert_eq!(keywords.len(), 1);
        assert_eq!(keywords[0].keyword, "AUTHOR");
        assert_eq!(keywords[0].value.as_deref(), Some("Alice"));
        assert_eq!(keywords[0].line_number, Some(1));
    }

    #[test]
    fn file_effective_properties_include_uses_resolved_root_values() {
        let connection = seeded_connection();
        let response = execute_and_shape_query(
            &connection,
            &validated(r#"(files (file-title "Alpha Index" :exact t))"#),
            &QueryExecutionOptions {
                output_mode: QueryOutputMode::Flat,
                includes: vec![QueryInclude::EffectiveProperties],
                ..QueryExecutionOptions::default()
            },
        )
        .expect("query should shape");

        let file = file_node(&response.results[0]);
        assert!(file.properties.is_none());
        assert_eq!(
            file.effective_properties.as_ref(),
            Some(&vec![EffectivePropertyFact {
                key: "CATEGORY".to_string(),
                value: Some("work".to_string()),
            }])
        );
    }

    #[test]
    fn heading_keywords_include_reflects_current_stored_heading_facts() {
        let connection = seeded_connection();
        let plain = execute_and_shape_query(
            &connection,
            &validated(r#"(headings (title "Query Engine" :exact t))"#),
            &QueryExecutionOptions::default(),
        )
        .expect("plain query should shape");
        let plain_json = serde_json::to_value(&plain).expect("plain response should serialize");
        assert!(plain_json["results"][0].get("keywords").is_none());

        let included = execute_and_shape_query(
            &connection,
            &validated(r#"(headings (title "Query Engine" :exact t))"#),
            &QueryExecutionOptions {
                output_mode: QueryOutputMode::Flat,
                includes: vec![QueryInclude::Keywords],
                ..QueryExecutionOptions::default()
            },
        )
        .expect("included query should shape");
        let heading = heading_node(&included.results[0]);
        let keywords = heading
            .keywords
            .as_ref()
            .expect("keywords include should exist");
        assert!(keywords.is_empty());
        assert_eq!(matched_heading_ids(&plain), matched_heading_ids(&included));
    }

    #[test]
    fn file_backlinks_include_captures_heading_targets_in_file() {
        let connection = seeded_connection();
        let response = execute_and_shape_query(
            &connection,
            &validated(r#"(files (file-title "Alpha Index" :exact t))"#),
            &QueryExecutionOptions {
                output_mode: QueryOutputMode::Flat,
                includes: vec![QueryInclude::Backlinks],
                ..QueryExecutionOptions::default()
            },
        )
        .expect("query should shape");

        let file = file_node(&response.results[0]);
        let backlinks = file
            .backlinks
            .as_ref()
            .expect("backlinks include should exist");
        let ids = backlinks.iter().map(|link| link.id).collect::<Vec<_>>();
        assert_eq!(ids, vec![104, 103]);
        assert_eq!(
            backlinks[0].target.resolution_status.as_deref(),
            Some("resolved")
        );
    }

    #[test]
    fn file_location_omits_internal_root_byte_sentinel() {
        let connection = seeded_connection();
        let response = execute_and_shape_query(
            &connection,
            &validated(r#"(files (file-title "Alpha Index" :exact t))"#),
            &QueryExecutionOptions::default(),
        )
        .expect("query should shape");

        let file = file_node(&response.results[0]);
        assert_eq!(file.location.byte_start, None);
        assert_eq!(file.location.byte_end, None);
    }

    #[test]
    fn outline_mode_marks_context_only_ancestors() {
        let connection = seeded_connection();
        let response = execute_and_shape_query(
            &connection,
            &validated(r#"(headings (title "Nested" :exact t))"#),
            &QueryExecutionOptions {
                output_mode: QueryOutputMode::Outline,
                includes: vec![],
                ..QueryExecutionOptions::default()
            },
        )
        .expect("query should shape");

        let file = file_node(&response.results[0]);
        assert!(!file.matched);
        let parent = heading_node(&file.children.as_ref().expect("children")[0]);
        assert!(!parent.matched);
        let child = heading_node(&parent.children.as_ref().expect("children")[0]);
        assert!(child.matched);
    }

    #[test]
    fn heading_title_root_matches_shape_as_file_nodes_in_flat_and_outline_output() {
        let connection = seeded_connection();

        let flat = execute_and_shape_query(
            &connection,
            &validated(r#"(headings (title "Alpha Index" :exact t))"#),
            &QueryExecutionOptions::default(),
        )
        .expect("flat root title query should shape");
        assert_eq!(flat.target, QueryTarget::Headings);
        let file = file_node(&flat.results[0]);
        assert!(file.matched);
        assert_eq!(file.title, "Alpha Index");
        assert!(file.children.is_none());

        let outline = execute_and_shape_query(
            &connection,
            &validated(r#"(headings (title "Alpha Index" :exact t))"#),
            &QueryExecutionOptions {
                output_mode: QueryOutputMode::Outline,
                includes: vec![],
                ..QueryExecutionOptions::default()
            },
        )
        .expect("outline root title query should shape");
        assert_eq!(outline.target, QueryTarget::Headings);
        let file = file_node(&outline.results[0]);
        assert!(file.matched);
        assert!(file
            .children
            .as_ref()
            .expect("outline file children should exist")
            .is_empty());
    }

    #[test]
    fn heading_file_predicates_shape_matching_file_roots_and_preserve_includes() {
        let connection = seeded_connection();

        let flat = execute_and_shape_query(
            &connection,
            &validated(r#"(headings (file-title "Alpha Index" :exact t))"#),
            &QueryExecutionOptions {
                output_mode: QueryOutputMode::Flat,
                includes: vec![QueryInclude::Properties],
                ..QueryExecutionOptions::default()
            },
        )
        .expect("flat file-title heading query should shape");
        let file = file_node(&flat.results[0]);
        assert!(file.matched);
        assert_eq!(file.level, 0);
        assert_eq!(file.path, "/tmp/query-alpha.org");
        let properties = file
            .properties
            .as_ref()
            .expect("properties include should exist");
        assert_eq!(properties.len(), 1);
        assert_eq!(properties[0].key, "CATEGORY");
        assert_eq!(properties[0].value.as_deref(), Some("work"));
        let heading = heading_node(&flat.results[1]);
        assert!(heading.matched);
        assert!(heading.level > 0);

        let outline = execute_and_shape_query(
            &connection,
            &validated(
                r#"(headings
                    (and
                      (file-title "Alpha Index" :exact t)
                      (todo "NEXT")))"#,
            ),
            &QueryExecutionOptions {
                output_mode: QueryOutputMode::Outline,
                includes: vec![],
                ..QueryExecutionOptions::default()
            },
        )
        .expect("outline file-title heading query should shape");
        let outline_file = file_node(&outline.results[0]);
        assert!(!outline_file.matched);
        let outline_children = outline_file
            .children
            .as_ref()
            .expect("outline file children should exist");
        let outline_heading = heading_node(&outline_children[0]);
        assert!(outline_heading.matched);
    }

    #[test]
    fn bare_headings_query_shapes_file_roots_and_real_headings_in_flat_and_outline_output() {
        let connection = seeded_connection();

        let flat = execute_and_shape_query(
            &connection,
            &validated(r#"(headings)"#),
            &QueryExecutionOptions::default(),
        )
        .expect("flat bare headings query should shape");
        assert_eq!(flat.target, QueryTarget::Headings);
        assert!(matches!(
            flat.results.first(),
            Some(QueryResultNode::File(_))
        ));
        let flat_file = file_node(&flat.results[0]);
        assert!(flat_file.matched);
        assert_eq!(flat_file.level, 0);
        assert_eq!(flat_file.path, "/tmp/query-alpha.org");
        assert_eq!(flat_file.title, "Alpha Index");
        let flat_heading = heading_node(&flat.results[1]);
        assert!(flat_heading.matched);
        assert!(flat_heading.level > 0);

        let outline = execute_and_shape_query(
            &connection,
            &validated(r#"(headings)"#),
            &QueryExecutionOptions {
                output_mode: QueryOutputMode::Outline,
                includes: vec![],
                ..QueryExecutionOptions::default()
            },
        )
        .expect("outline bare headings query should shape");
        assert_eq!(outline.target, QueryTarget::Headings);
        assert_eq!(outline.results.len(), 2);
        let outline_file = file_node(&outline.results[0]);
        assert!(outline_file.matched);
        let outline_children = outline_file
            .children
            .as_ref()
            .expect("outline file children should exist");
        assert!(!outline_children.is_empty());
        let outline_heading = heading_node(&outline_children[0]);
        assert!(outline_heading.matched);
        assert!(outline_heading.level > 0);

        let flat_json = serde_json::to_value(&flat).expect("flat response should serialize");
        assert!(flat_json["results"]
            .as_array()
            .expect("flat results should be an array")
            .iter()
            .all(
                |node| node["kind"] != "heading" || node["level"].as_i64().unwrap_or_default() > 0
            ));
        assert_eq!(flat_json["results"][0]["kind"], "root");
        assert_eq!(flat_json["results"][0]["level"], 0);
        let outline_json =
            serde_json::to_value(&outline).expect("outline response should serialize");
        assert_eq!(outline_json["results"][0]["kind"], "root");
        assert_eq!(matched_heading_ids(&flat), matched_heading_ids(&outline));
    }

    #[test]
    fn output_modes_do_not_change_matched_heading_ids() {
        let connection = seeded_connection();
        let query = validated(r#"(headings (tags "project"))"#);
        let flat = execute_and_shape_query(
            &connection,
            &query,
            &QueryExecutionOptions {
                output_mode: QueryOutputMode::Flat,
                includes: vec![QueryInclude::Path],
                ..QueryExecutionOptions::default()
            },
        )
        .expect("flat query should shape");
        let outline = execute_and_shape_query(
            &connection,
            &query,
            &QueryExecutionOptions {
                output_mode: QueryOutputMode::Outline,
                includes: vec![QueryInclude::Path],
                ..QueryExecutionOptions::default()
            },
        )
        .expect("outline query should shape");

        assert_eq!(matched_heading_ids(&flat), vec![11, 12]);
        assert_eq!(matched_heading_ids(&outline), vec![11, 12]);
    }

    #[test]
    fn includes_do_not_change_matched_ids() {
        let connection = seeded_connection();
        let query = validated(r#"(headings (tags "project"))"#);
        let plain = execute_and_shape_query(&connection, &query, &QueryExecutionOptions::default())
            .expect("plain query should shape");
        let enriched = execute_and_shape_query(
            &connection,
            &query,
            &QueryExecutionOptions {
                output_mode: QueryOutputMode::Flat,
                includes: vec![
                    QueryInclude::Path,
                    QueryInclude::Properties,
                    QueryInclude::Keywords,
                    QueryInclude::Links,
                    QueryInclude::Backlinks,
                ],
                ..QueryExecutionOptions::default()
            },
        )
        .expect("enriched query should shape");

        assert_eq!(matched_heading_ids(&plain), matched_heading_ids(&enriched));
    }

    #[test]
    fn shaping_is_read_only() {
        let connection = seeded_connection();
        let before = table_counts(&connection);
        let _response = execute_and_shape_query(
            &connection,
            &validated(r#"(links (status "resolved"))"#),
            &QueryExecutionOptions {
                output_mode: QueryOutputMode::Flat,
                includes: vec![
                    QueryInclude::Path,
                    QueryInclude::Source,
                    QueryInclude::Target,
                ],
                ..QueryExecutionOptions::default()
            },
        )
        .expect("query should shape");
        let after = table_counts(&connection);
        assert_eq!(before, after);
    }

    fn matched_heading_ids(response: &QueryResponse) -> Vec<i64> {
        let mut ids = Vec::new();
        collect_matched_heading_ids(&response.results, &mut ids);
        ids.sort();
        ids
    }

    fn collect_matched_heading_ids(nodes: &[QueryResultNode], ids: &mut Vec<i64>) {
        for node in nodes {
            match node {
                QueryResultNode::File(node) => {
                    if let Some(children) = &node.children {
                        collect_matched_heading_ids(children, ids);
                    }
                }
                QueryResultNode::Heading(node) => {
                    if node.matched {
                        ids.push(node.id);
                    }
                    if let Some(children) = &node.children {
                        collect_matched_heading_ids(children, ids);
                    }
                }
                QueryResultNode::Link(_) => {}
            }
        }
    }

    fn file_node(node: &QueryResultNode) -> &super::FileResultNode {
        match node {
            QueryResultNode::File(node) => node,
            _ => panic!("expected file node"),
        }
    }

    fn heading_node(node: &QueryResultNode) -> &super::HeadingResultNode {
        match node {
            QueryResultNode::Heading(node) => node,
            _ => panic!("expected heading node"),
        }
    }

    fn link_node(node: &QueryResultNode) -> &super::LinkResultNode {
        match node {
            QueryResultNode::Link(node) => node,
            _ => panic!("expected link node"),
        }
    }

    fn validated(query: &str) -> crate::query::ValidatedQuery {
        let parsed = parse_query(query).expect("query should parse");
        validate_query(parsed, &QueryValidationOptions::default()).expect("query should validate")
    }

    fn seed_effective_tags(connection: &Connection, file_id: i64) {
        let parents = {
            let mut statement = connection
                .prepare("SELECT id, parent_id FROM headings WHERE file_id = ?1 ORDER BY id")
                .expect("seed heading query should prepare");
            statement
                .query_map([file_id], |row| {
                    Ok((row.get::<_, i64>(0)?, row.get::<_, Option<i64>>(1)?))
                })
                .expect("seed heading query should run")
                .collect::<Result<std::collections::HashMap<_, _>, _>>()
                .expect("seed heading rows should decode")
        };
        let direct_tags = {
            let mut statement = connection
                .prepare(
                    "SELECT tags.heading_id, tags.tag
                     FROM tags
                     INNER JOIN headings ON headings.id = tags.heading_id
                     WHERE headings.file_id = ?1
                     ORDER BY tags.heading_id, tags.tag",
                )
                .expect("seed tag query should prepare");
            let rows = statement
                .query_map([file_id], |row| {
                    Ok((row.get::<_, i64>(0)?, row.get::<_, String>(1)?))
                })
                .expect("seed tag query should run")
                .collect::<Result<Vec<_>, _>>()
                .expect("seed tag rows should decode");
            let mut by_heading = std::collections::HashMap::<i64, Vec<String>>::new();
            for (heading_id, tag) in rows {
                by_heading.entry(heading_id).or_default().push(tag);
            }
            by_heading
        };
        let rows = derive_effective_tags(&parents, &direct_tags)
            .into_iter()
            .map(|row| EffectiveTagRecord {
                heading_id: row.heading_id,
                file_id,
                tag: row.tag,
                position: row.position,
            })
            .collect::<Vec<_>>();
        DbWriter::insert_effective_tags(connection, &rows)
            .expect("effective tags should seed through the production writer");
    }

    fn seed_effective_properties(connection: &Connection, file_id: i64) {
        let parents = {
            let mut statement = connection
                .prepare("SELECT id, parent_id FROM headings WHERE file_id = ?1 ORDER BY id")
                .expect("seed heading query should prepare");
            statement
                .query_map([file_id], |row| {
                    Ok((row.get::<_, i64>(0)?, row.get::<_, Option<i64>>(1)?))
                })
                .expect("seed heading query should run")
                .collect::<Result<std::collections::HashMap<_, _>, _>>()
                .expect("seed heading rows should decode")
        };
        let rows_by_heading = {
            let mut statement = connection
                .prepare(
                    "SELECT properties.id, properties.heading_id, properties.key, properties.value,
                            properties.append, properties.line_number
                     FROM properties
                     INNER JOIN headings ON headings.id = properties.heading_id
                     WHERE headings.file_id = ?1
                     ORDER BY properties.line_number, properties.id",
                )
                .expect("seed property query should prepare");
            let rows = statement
                .query_map([file_id], |row| {
                    Ok(PropertyRow {
                        id: row.get(0)?,
                        heading_id: row.get(1)?,
                        key: row.get(2)?,
                        value: row.get(3)?,
                        append: row.get::<_, i64>(4)? != 0,
                        line_number: row.get(5)?,
                    })
                })
                .expect("seed property query should run")
                .collect::<Result<Vec<_>, _>>()
                .expect("seed property rows should decode");
            let mut by_heading = std::collections::HashMap::<i64, Vec<PropertyRow>>::new();
            for row in rows {
                by_heading.entry(row.heading_id).or_default().push(row);
            }
            by_heading
        };
        let rows = derive_effective_properties(&parents, &rows_by_heading)
            .into_iter()
            .map(|row| EffectivePropertyRecord {
                heading_id: row.heading_id,
                file_id,
                key: row.key,
                local_value: row.local_value,
                effective_value: row.effective_value,
            })
            .collect::<Vec<_>>();
        DbWriter::insert_effective_properties(connection, &rows)
            .expect("effective properties should seed through the production writer");
    }

    fn seeded_connection() -> Connection {
        let schema = SchemaDefinition::new(3, false);
        let mut connection =
            open_in_memory_database_with_schema(&schema).expect("database should open");
        seed_database(
            &mut connection,
            Path::new("/tmp/query-alpha.org"),
            Path::new("/tmp/query-beta.org"),
        );
        connection
    }

    fn seed_database(connection: &mut Connection, alpha_path: &Path, beta_path: &Path) {
        let alpha = FileRecordInput {
            path: alpha_path.to_path_buf(),
            identity: None,
            mtime_ns: 1_767_398_400_000_000_000,
            size: 100,
            content_hash: None,
            indexed_at: Some(1_767_398_410),
        };
        let beta = FileRecordInput {
            path: beta_path.to_path_buf(),
            identity: None,
            mtime_ns: 1_767_484_800_000_000_000,
            size: 120,
            content_hash: None,
            indexed_at: Some(1_767_484_810),
        };

        let (beta_file_id, ()) = DbWriter::rebuild_file(connection, &beta, |tx, file_id| {
            let root_id = DbWriter::insert_level0_heading(
                tx,
                &HeadingRecord {
                    id: Some(20),
                    file_id,
                    parent_id: None,
                    level: 0,
                    line_number: None,
                    byte_start: -1,
                    byte_end: 120,
                    title: "Beta Index".to_string(),
                    title_raw: Some("Beta Index".to_string()),
                    todo_keyword: None,
                    todo_type: None,
                    priority: None,
                    scheduled_raw: None,
                    scheduled_ts: None,
                    scheduled_has_time: None,
                    deadline_raw: None,
                    deadline_ts: None,
                    deadline_has_time: None,
                    closed_raw: None,
                    closed_ts: None,
                    closed_has_time: None,
                    archivedp: false,
                    footnote_section_p: false,
                },
            )?;
            DbWriter::insert_outline_path(
                tx,
                &[OutlinePathRecord {
                    heading_id: root_id,
                    file_id,
                    parent_id: None,
                    depth: 0,
                    materialized_path: "0000".to_string(),
                    breadcrumbs_json: "[\"Beta Index\"]".to_string(),
                }],
            )?;
            DbWriter::insert_tags(
                tx,
                &[TagRecord {
                    heading_id: root_id,
                    tag: "archive".to_string(),
                }],
            )?;
            Ok(())
        })
        .expect("beta file should seed");

        let (alpha_file_id, ()) = DbWriter::rebuild_file(connection, &alpha, |tx, file_id| {
            let root_id = DbWriter::insert_level0_heading(
                tx,
                &HeadingRecord {
                    id: Some(10),
                    file_id,
                    parent_id: None,
                    level: 0,
                    line_number: None,
                    byte_start: -1,
                    byte_end: 100,
                    title: "Alpha Index".to_string(),
                    title_raw: Some("Alpha Index".to_string()),
                    todo_keyword: None,
                    todo_type: None,
                    priority: None,
                    scheduled_raw: None,
                    scheduled_ts: None,
                    scheduled_has_time: None,
                    deadline_raw: None,
                    deadline_ts: None,
                    deadline_has_time: None,
                    closed_raw: None,
                    closed_ts: None,
                    closed_has_time: None,
                    archivedp: false,
                    footnote_section_p: false,
                },
            )?;
            DbWriter::insert_headings(
                tx,
                &[
                    HeadingRecord {
                        id: Some(11),
                        file_id,
                        parent_id: Some(root_id),
                        level: 1,
                        line_number: Some(3),
                        byte_start: 10,
                        byte_end: 40,
                        title: "Query Engine".to_string(),
                        title_raw: Some("Query Engine".to_string()),
                        todo_keyword: Some("NEXT".to_string()),
                        todo_type: Some("open".to_string()),
                        priority: Some("A".to_string()),
                        scheduled_raw: None,
                        scheduled_ts: None,
                        scheduled_has_time: None,
                        deadline_raw: None,
                        deadline_ts: None,
                        deadline_has_time: None,
                        closed_raw: None,
                        closed_ts: None,
                        closed_has_time: None,
                        archivedp: false,
                        footnote_section_p: false,
                    },
                    HeadingRecord {
                        id: Some(12),
                        file_id,
                        parent_id: Some(11),
                        level: 2,
                        line_number: Some(6),
                        byte_start: 41,
                        byte_end: 70,
                        title: "Nested".to_string(),
                        title_raw: Some("Nested".to_string()),
                        todo_keyword: None,
                        todo_type: None,
                        priority: None,
                        scheduled_raw: None,
                        scheduled_ts: None,
                        scheduled_has_time: None,
                        deadline_raw: None,
                        deadline_ts: None,
                        deadline_has_time: None,
                        closed_raw: None,
                        closed_ts: None,
                        closed_has_time: None,
                        archivedp: false,
                        footnote_section_p: false,
                    },
                    HeadingRecord {
                        id: Some(13),
                        file_id,
                        parent_id: Some(root_id),
                        level: 1,
                        line_number: Some(8),
                        byte_start: 71,
                        byte_end: 95,
                        title: "Loose Note".to_string(),
                        title_raw: Some("Loose Note".to_string()),
                        todo_keyword: None,
                        todo_type: None,
                        priority: None,
                        scheduled_raw: None,
                        scheduled_ts: None,
                        scheduled_has_time: None,
                        deadline_raw: None,
                        deadline_ts: None,
                        deadline_has_time: None,
                        closed_raw: None,
                        closed_ts: None,
                        closed_has_time: None,
                        archivedp: false,
                        footnote_section_p: false,
                    },
                ],
            )?;
            DbWriter::insert_outline_path(
                tx,
                &[
                    OutlinePathRecord {
                        heading_id: 10,
                        file_id,
                        parent_id: None,
                        depth: 0,
                        materialized_path: "0000".to_string(),
                        breadcrumbs_json: "[\"Alpha Index\"]".to_string(),
                    },
                    OutlinePathRecord {
                        heading_id: 11,
                        file_id,
                        parent_id: Some(10),
                        depth: 1,
                        materialized_path: "0000.0001".to_string(),
                        breadcrumbs_json: "[\"Alpha Index\",\"Query Engine\"]".to_string(),
                    },
                    OutlinePathRecord {
                        heading_id: 12,
                        file_id,
                        parent_id: Some(11),
                        depth: 2,
                        materialized_path: "0000.0001.0001".to_string(),
                        breadcrumbs_json: "[\"Alpha Index\",\"Query Engine\",\"Nested\"]"
                            .to_string(),
                    },
                    OutlinePathRecord {
                        heading_id: 13,
                        file_id,
                        parent_id: Some(10),
                        depth: 1,
                        materialized_path: "0000.0002".to_string(),
                        breadcrumbs_json: "[\"Alpha Index\",\"Loose Note\"]".to_string(),
                    },
                ],
            )?;
            DbWriter::insert_tags(
                tx,
                &[
                    TagRecord {
                        heading_id: 10,
                        tag: "filetag".to_string(),
                    },
                    TagRecord {
                        heading_id: 11,
                        tag: "project".to_string(),
                    },
                    TagRecord {
                        heading_id: 12,
                        tag: "urgent".to_string(),
                    },
                ],
            )?;
            seed_effective_tags(tx, file_id);
            DbWriter::insert_keywords(
                tx,
                &[KeywordRecord {
                    heading_id: 10,
                    keyword: "AUTHOR".to_string(),
                    value: Some("Alice".to_string()),
                    line_number: Some(1),
                }],
            )?;
            DbWriter::insert_properties(
                tx,
                &[
                    PropertyRecord {
                        heading_id: 10,
                        key: "CATEGORY".to_string(),
                        value: Some("work".to_string()),
                        source: "category_keyword".to_string(),
                        append: false,
                        line_number: Some(2),
                    },
                    PropertyRecord {
                        heading_id: 11,
                        key: "AREA".to_string(),
                        value: Some("infra".to_string()),
                        source: "property_drawer".to_string(),
                        append: false,
                        line_number: Some(4),
                    },
                    PropertyRecord {
                        heading_id: 13,
                        key: "APPEND_REPLACED".to_string(),
                        value: Some("first".to_string()),
                        source: "property_drawer".to_string(),
                        append: false,
                        line_number: Some(9),
                    },
                    PropertyRecord {
                        heading_id: 13,
                        key: "APPEND_REPLACED".to_string(),
                        value: Some("appended".to_string()),
                        source: "property_drawer".to_string(),
                        append: true,
                        line_number: Some(10),
                    },
                    PropertyRecord {
                        heading_id: 13,
                        key: "APPEND_REPLACED".to_string(),
                        value: Some("second".to_string()),
                        source: "property_drawer".to_string(),
                        append: false,
                        line_number: Some(11),
                    },
                ],
            )?;
            Ok(())
        })
        .expect("alpha file should seed");
        seed_effective_properties(connection, alpha_file_id);

        connection
            .execute(
                "INSERT INTO headings
                 (id, file_id, parent_id, level, line_number, byte_start, byte_end, title, title_raw,
                  todo_keyword, todo_type, priority, scheduled_raw, scheduled_ts, deadline_raw,
                  deadline_ts, closed_raw, closed_ts, archivedp, footnote_section_p)
                 VALUES
                 (21, ?1, 20, 1, 3, 10, 40, 'Beta Target', 'Beta Target',
                  NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, 0, 0)",
                rusqlite::params![beta_file_id],
            )
            .expect("beta child heading should insert");
        DbWriter::insert_outline_path(
            connection,
            &[OutlinePathRecord {
                heading_id: 21,
                file_id: beta_file_id,
                parent_id: Some(20),
                depth: 1,
                materialized_path: "0000.0001".to_string(),
                breadcrumbs_json: "[\"Beta Index\",\"Beta Target\"]".to_string(),
            }],
        )
        .expect("beta outline should insert");
        DbWriter::insert_tags(
            connection,
            &[TagRecord {
                heading_id: 21,
                tag: "target".to_string(),
            }],
        )
        .expect("beta child tag should insert");
        seed_effective_tags(connection, beta_file_id);

        DbWriter::insert_links(
            connection,
            &[
                LinkRecord {
                    id: Some(100),
                    file_id: alpha_file_id,
                    heading_id: 11,
                    byte_start: 50,
                    byte_end: 80,
                    line: 4,
                    source_context: "normal".to_string(),
                    format: "bracket".to_string(),
                    raw: "[[file:beta.org][Beta notes]]".to_string(),
                    raw_target: "file:beta.org".to_string(),
                    raw_description: Some("Beta notes".to_string()),
                    link_type: "file".to_string(),
                    path: "beta.org".to_string(),
                    search_option: None,
                },
                LinkRecord {
                    id: Some(101),
                    file_id: alpha_file_id,
                    heading_id: 12,
                    byte_start: 60,
                    byte_end: 95,
                    line: 6,
                    source_context: "normal".to_string(),
                    format: "bracket".to_string(),
                    raw: "[[file:beta.org::*Beta Target][Beta heading]]".to_string(),
                    raw_target: "file:beta.org::*Beta Target".to_string(),
                    raw_description: Some("Beta heading".to_string()),
                    link_type: "file".to_string(),
                    path: "beta.org".to_string(),
                    search_option: Some("*Beta Target".to_string()),
                },
                LinkRecord {
                    id: Some(102),
                    file_id: alpha_file_id,
                    heading_id: 10,
                    byte_start: 0,
                    byte_end: 24,
                    line: 1,
                    source_context: "normal".to_string(),
                    format: "bracket".to_string(),
                    raw: "[[file:beta.org][Preamble]]".to_string(),
                    raw_target: "file:beta.org".to_string(),
                    raw_description: Some("Preamble".to_string()),
                    link_type: "file".to_string(),
                    path: "beta.org".to_string(),
                    search_option: None,
                },
                LinkRecord {
                    id: Some(103),
                    file_id: beta_file_id,
                    heading_id: 21,
                    byte_start: 50,
                    byte_end: 84,
                    line: 4,
                    source_context: "normal".to_string(),
                    format: "bracket".to_string(),
                    raw: "[[file:alpha.org::*Query Engine][Backlink]]".to_string(),
                    raw_target: "file:alpha.org::*Query Engine".to_string(),
                    raw_description: Some("Backlink".to_string()),
                    link_type: "file".to_string(),
                    path: "alpha.org".to_string(),
                    search_option: Some("*Query Engine".to_string()),
                },
                LinkRecord {
                    id: Some(104),
                    file_id: beta_file_id,
                    heading_id: 20,
                    byte_start: 0,
                    byte_end: 22,
                    line: 1,
                    source_context: "normal".to_string(),
                    format: "bracket".to_string(),
                    raw: "[[file:alpha.org][Root]]".to_string(),
                    raw_target: "file:alpha.org".to_string(),
                    raw_description: Some("Root".to_string()),
                    link_type: "file".to_string(),
                    path: "alpha.org".to_string(),
                    search_option: None,
                },
                LinkRecord {
                    id: Some(105),
                    file_id: alpha_file_id,
                    heading_id: 13,
                    byte_start: 80,
                    byte_end: 104,
                    line: 8,
                    source_context: "normal".to_string(),
                    format: "bracket".to_string(),
                    raw: "[[file:missing.org]]".to_string(),
                    raw_target: "file:missing.org".to_string(),
                    raw_description: None,
                    link_type: "file".to_string(),
                    path: "missing.org".to_string(),
                    search_option: None,
                },
            ],
        )
        .expect("links should seed");

        connection
            .execute(
                "UPDATE links SET path_absolute = ?1, target_file_id = ?2, resolution_status = 'resolved' WHERE id = 100",
                rusqlite::params![beta_path.to_string_lossy().to_string(), beta_file_id],
            )
            .expect("file link should update");
        connection
            .execute(
                "UPDATE links SET path_absolute = ?1, target_file_id = ?2, target_heading_id = 21, resolution_status = 'resolved' WHERE id = 101",
                rusqlite::params![beta_path.to_string_lossy().to_string(), beta_file_id],
            )
            .expect("heading link should update");
        connection
            .execute(
                "UPDATE links SET path_absolute = ?1, target_file_id = ?2, resolution_status = 'resolved' WHERE id = 102",
                rusqlite::params![beta_path.to_string_lossy().to_string(), beta_file_id],
            )
            .expect("preamble link should update");
        connection
            .execute(
                "UPDATE links SET path_absolute = ?1, target_file_id = ?2, target_heading_id = 11, resolution_status = 'resolved' WHERE id = 103",
                rusqlite::params![alpha_path.to_string_lossy().to_string(), alpha_file_id],
            )
            .expect("backlink should update");
        connection
            .execute(
                "UPDATE links SET path_absolute = ?1, target_file_id = ?2, resolution_status = 'resolved' WHERE id = 104",
                rusqlite::params![alpha_path.to_string_lossy().to_string(), alpha_file_id],
            )
            .expect("root backlink should update");
        connection
            .execute(
                "UPDATE links SET resolution_status = 'broken', resolution_diagnostic = 'missing target' WHERE id = 105",
                [],
            )
            .expect("broken link should update");
    }

    fn table_counts(connection: &Connection) -> Vec<(&'static str, i64)> {
        [
            "files",
            "headings",
            "links",
            "tags",
            "properties",
            "keywords",
        ]
        .into_iter()
        .map(|table| {
            let sql = format!("SELECT COUNT(*) FROM {table}");
            let count = connection
                .query_row(&sql, [], |row| row.get(0))
                .expect("table count should load");
            (table, count)
        })
        .collect()
    }
}
