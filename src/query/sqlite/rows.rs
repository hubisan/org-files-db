use super::*;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CompiledSqlQuery {
    pub target: QueryTarget,
    pub sql: String,
    pub params: Vec<QueryParam>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum QueryParam {
    Integer(i64),
    Text(String),
}

impl ToSql for QueryParam {
    fn to_sql(&self) -> rusqlite::Result<ToSqlOutput<'_>> {
        match self {
            Self::Integer(value) => Ok(ToSqlOutput::Owned(Value::Integer(*value))),
            Self::Text(value) => Ok(ToSqlOutput::Owned(Value::Text(value.clone()))),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub enum QueryRows {
    Headings(Vec<HeadingQueryMatch>),
    Links(Vec<LinkQueryRow>),
    Files(Vec<FileQueryRow>),
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
#[serde(tag = "kind", rename_all = "snake_case")]
#[allow(clippy::large_enum_variant)] // Source-preserving heading fields intentionally remain inline.
pub enum HeadingQueryMatch {
    File(FileQueryRow),
    Heading(HeadingQueryRow),
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct HeadingQueryRow {
    pub id: i64,
    pub file_id: i64,
    pub file_path: String,
    pub parent_id: Option<i64>,
    pub level: i64,
    pub line_number: Option<i64>,
    pub byte_start: i64,
    pub byte_end: i64,
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
    pub all_tags_json: String,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct LinkQueryRow {
    pub id: i64,
    pub file_id: i64,
    pub file_path: String,
    pub heading_id: i64,
    pub heading_level: i64,
    pub source_context: String,
    pub format: String,
    pub link_type: String,
    pub raw: String,
    pub raw_target: String,
    pub raw_description: Option<String>,
    pub path: String,
    pub search_option: Option<String>,
    pub path_absolute: Option<String>,
    pub target_file_id: Option<i64>,
    pub target_heading_id: Option<i64>,
    pub target_custom_id: Option<String>,
    pub target_id: Option<String>,
    pub resolution_status: Option<String>,
    pub resolution_diagnostic: Option<String>,
    pub byte_start: i64,
    pub byte_end: i64,
    pub line: i64,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct FileQueryRow {
    pub id: i64,
    pub path: String,
    pub mtime_ns: i64,
    pub size: i64,
    pub content_hash: Option<String>,
    pub indexed_at: Option<i64>,
    pub root_heading_id: i64,
    pub root_title: String,
    pub root_title_raw: Option<String>,
    pub root_line_number: Option<i64>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(in crate::query::sqlite) enum HeadingMatchKind {
    RealHeading,
    RootFile,
    AllHeadings,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum QueryExecutionErrorKind {
    UnsupportedPredicate,
    UnsupportedBackendFeature,
    DateResolution,
    Database,
}

#[derive(Debug)]
pub struct QueryExecutionError {
    pub kind: QueryExecutionErrorKind,
    pub target: QueryTarget,
    pub predicate: String,
    pub message: String,
    pub(in crate::query::sqlite) source: Option<rusqlite::Error>,
}

impl QueryExecutionError {
    pub(in crate::query::sqlite) fn unsupported_predicate(
        target: QueryTarget,
        predicate: impl Into<String>,
        message: impl Into<String>,
    ) -> Self {
        Self {
            kind: QueryExecutionErrorKind::UnsupportedPredicate,
            target,
            predicate: predicate.into(),
            message: message.into(),
            source: None,
        }
    }

    pub(in crate::query::sqlite) fn unsupported_backend_feature(
        target: QueryTarget,
        predicate: impl Into<String>,
        message: impl Into<String>,
    ) -> Self {
        Self {
            kind: QueryExecutionErrorKind::UnsupportedBackendFeature,
            target,
            predicate: predicate.into(),
            message: message.into(),
            source: None,
        }
    }

    pub(in crate::query::sqlite) fn date_resolution(
        target: QueryTarget,
        predicate: impl Into<String>,
        message: impl Into<String>,
    ) -> Self {
        Self {
            kind: QueryExecutionErrorKind::DateResolution,
            target,
            predicate: predicate.into(),
            message: message.into(),
            source: None,
        }
    }

    pub(in crate::query::sqlite) fn database(
        target: QueryTarget,
        operation: &'static str,
        source: rusqlite::Error,
    ) -> Self {
        Self {
            kind: QueryExecutionErrorKind::Database,
            target,
            predicate: operation.to_string(),
            message: format!("failed to execute SQLite query {operation}: {source}"),
            source: Some(source),
        }
    }
}

impl fmt::Display for QueryExecutionError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{} (target {}, predicate {})",
            self.message,
            target_name(self.target),
            self.predicate
        )
    }
}

impl std::error::Error for QueryExecutionError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        self.source
            .as_ref()
            .map(|source| source as &(dyn std::error::Error + 'static))
    }
}
