use std::{
    collections::HashSet,
    path::{Path, PathBuf},
};

use serde::Serialize;

use super::diagnostics::ParseDiagnostic;

pub trait OrgParser {
    fn parse_document(
        &self,
        path: &Path,
        content: &str,
        options: &ParseOptions,
    ) -> Result<ParsedOrgDocument, ParseDiagnostic>;
}

#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct ParseOptions {
    pub todo_keywords: TodoKeywordConfig,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct TodoKeywordConfig {
    pub open: Vec<TodoKeyword>,
    pub closed: Vec<TodoKeyword>,
}

impl TodoKeywordConfig {
    pub fn all_keywords(&self) -> impl Iterator<Item = &TodoKeyword> {
        self.open.iter().chain(self.closed.iter())
    }

    pub fn deduplicated(self) -> Self {
        let mut seen = HashSet::new();
        let mut open = Vec::new();
        let mut closed = Vec::new();

        for keyword in self.open {
            if seen.insert(keyword.name.clone()) {
                open.push(keyword);
            }
        }
        for keyword in self.closed {
            if seen.insert(keyword.name.clone()) {
                closed.push(keyword);
            }
        }

        Self { open, closed }
    }
}

impl Default for TodoKeywordConfig {
    fn default() -> Self {
        Self {
            open: vec![TodoKeyword::new("TODO")],
            closed: vec![TodoKeyword::new("DONE")],
        }
    }
}

pub fn file_local_todo_keyword_config(keywords: &[ParsedKeyword]) -> Option<TodoKeywordConfig> {
    let mut open = Vec::new();
    let mut closed = Vec::new();
    let mut seen = HashSet::new();

    for keyword in keywords.iter().filter(|keyword| {
        keyword.key.eq_ignore_ascii_case("TODO")
            || keyword.key.eq_ignore_ascii_case("SEQ_TODO")
            || keyword.key.eq_ignore_ascii_case("TYP_TODO")
    }) {
        let Some(value) = keyword.value.as_deref() else {
            continue;
        };
        let Some(line_config) = parse_file_local_todo_keyword_line(value) else {
            continue;
        };

        for todo_keyword in line_config.open {
            if seen.insert(todo_keyword.name.clone()) {
                open.push(todo_keyword);
            }
        }
        for todo_keyword in line_config.closed {
            if seen.insert(todo_keyword.name.clone()) {
                closed.push(todo_keyword);
            }
        }
    }

    if open.is_empty() && closed.is_empty() {
        None
    } else {
        Some(TodoKeywordConfig { open, closed })
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct TodoKeyword {
    pub name: String,
    pub fast_key: Option<char>,
}

fn parse_todo_keyword_token(token: &str) -> Option<TodoKeyword> {
    if let Some((name, suffix)) = token.split_once('(') {
        let name = name.trim();
        if name.is_empty() {
            return None;
        }

        let fast_key = suffix
            .strip_suffix(')')
            .and_then(|value| value.chars().next());
        if let Some(fast_key) = fast_key {
            Some(TodoKeyword::with_fast_key(name, fast_key))
        } else {
            Some(TodoKeyword::new(name))
        }
    } else {
        let name = token.trim();
        if name.is_empty() {
            None
        } else {
            Some(TodoKeyword::new(name))
        }
    }
}

fn parse_file_local_todo_keyword_line(value: &str) -> Option<TodoKeywordConfig> {
    let tokens: Vec<&str> = value.split_whitespace().collect();
    if tokens.is_empty() {
        return None;
    }

    let mut open = Vec::new();
    let mut closed = Vec::new();

    if let Some(separator_index) = tokens.iter().position(|token| *token == "|") {
        for token in &tokens[..separator_index] {
            open.push(parse_todo_keyword_token(token)?);
        }
        for token in &tokens[separator_index + 1..] {
            closed.push(parse_todo_keyword_token(token)?);
        }
    } else {
        let (closed_token, open_tokens) = tokens.split_last()?;
        for token in open_tokens {
            open.push(parse_todo_keyword_token(token)?);
        }
        closed.push(parse_todo_keyword_token(closed_token)?);
    }

    Some(TodoKeywordConfig { open, closed })
}

impl TodoKeyword {
    pub fn new(name: impl Into<String>) -> Self {
        Self {
            name: name.into(),
            fast_key: None,
        }
    }

    pub fn with_fast_key(name: impl Into<String>, fast_key: char) -> Self {
        Self {
            name: name.into(),
            fast_key: Some(fast_key),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct ParsedOrgDocument {
    pub file_path: PathBuf,
    pub metadata: ParsedDocumentMetadata,
    pub headings: Vec<ParsedHeading>,
    pub diagnostics: Vec<ParseDiagnostic>,
}

impl ParsedOrgDocument {
    pub fn new(file_path: impl Into<PathBuf>) -> Self {
        Self {
            file_path: file_path.into(),
            metadata: ParsedDocumentMetadata::default(),
            headings: Vec::new(),
            diagnostics: Vec::new(),
        }
    }
}

#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub struct ParsedDocumentMetadata {
    pub title: Option<String>,
    pub keywords: Vec<ParsedKeyword>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct ParsedKeyword {
    pub key: String,
    pub value: Option<String>,
    pub line_number: Option<u32>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct ParsedHeading {
    pub file_path: PathBuf,
    pub level: u8,
    pub title: String,
    pub title_raw: String,
    pub body_text: Option<String>,
    pub body_byte_start: Option<usize>,
    pub body_byte_end: Option<usize>,
    pub todo_keyword: Option<String>,
    pub todo_type: Option<TodoType>,
    pub priority: Option<char>,
    pub tags: Vec<String>,
    pub properties: Vec<ParsedProperty>,
    pub planning: ParsedPlanning,
    pub timestamps: Vec<ParsedTimestamp>,
    pub line_number: Option<u32>,
    pub byte_start: usize,
    pub byte_end: usize,
    pub parent_index: Option<usize>,
    pub is_archived: bool,
    pub is_root: bool,
}

impl ParsedHeading {
    pub fn new(
        file_path: impl Into<PathBuf>,
        level: u8,
        title: impl Into<String>,
        byte_start: usize,
        byte_end: usize,
    ) -> Self {
        let title = title.into();
        Self {
            file_path: file_path.into(),
            level,
            title_raw: title.clone(),
            title,
            body_text: None,
            body_byte_start: None,
            body_byte_end: None,
            todo_keyword: None,
            todo_type: None,
            priority: None,
            tags: Vec::new(),
            properties: Vec::new(),
            planning: ParsedPlanning::default(),
            timestamps: Vec::new(),
            line_number: None,
            byte_start,
            byte_end,
            parent_index: None,
            is_archived: false,
            is_root: false,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub enum TodoType {
    Open,
    Closed,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct ParsedProperty {
    pub key: String,
    pub value: Option<String>,
    pub source: ParsedPropertySource,
    pub append: bool,
    pub line_number: Option<u32>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum ParsedPropertySource {
    PropertyDrawer,
    PropertyKeyword,
    CategoryKeyword,
}

impl ParsedPropertySource {
    pub fn as_db_str(self) -> &'static str {
        match self {
            Self::PropertyDrawer => "property_drawer",
            Self::PropertyKeyword => "property_keyword",
            Self::CategoryKeyword => "category_keyword",
        }
    }
}

#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub struct ParsedPlanning {
    pub scheduled: Option<ParsedTimestamp>,
    pub deadline: Option<ParsedTimestamp>,
    pub closed: Option<ParsedTimestamp>,
}

impl ParsedPlanning {
    pub fn scheduled_raw(&self) -> Option<&str> {
        self.scheduled
            .as_ref()
            .map(|timestamp| timestamp.raw_value.as_str())
    }

    pub fn scheduled_ts(&self) -> Option<i64> {
        self.scheduled
            .as_ref()
            .and_then(|timestamp| timestamp.start_ts)
    }

    pub fn deadline_raw(&self) -> Option<&str> {
        self.deadline
            .as_ref()
            .map(|timestamp| timestamp.raw_value.as_str())
    }

    pub fn deadline_ts(&self) -> Option<i64> {
        self.deadline
            .as_ref()
            .and_then(|timestamp| timestamp.start_ts)
    }

    pub fn closed_raw(&self) -> Option<&str> {
        self.closed
            .as_ref()
            .map(|timestamp| timestamp.raw_value.as_str())
    }

    pub fn closed_ts(&self) -> Option<i64> {
        self.closed
            .as_ref()
            .and_then(|timestamp| timestamp.start_ts)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct ParsedTimestamp {
    pub role: Option<ParsedTimestampRole>,
    pub raw_value: String,
    pub timestamp_type: ParsedTimestampType,
    pub range_type: ParsedTimestampRangeType,
    pub start_ts: Option<i64>,
    pub end_ts: Option<i64>,
    pub byte_start: usize,
    pub byte_end: usize,
    pub line_number: Option<u32>,
    pub modifiers: Vec<ParsedTimestampModifier>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize)]
pub enum ParsedTimestampRole {
    Scheduled,
    Deadline,
    Closed,
    Body,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize)]
pub enum ParsedTimestampType {
    Active,
    Inactive,
    Diary,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize)]
pub enum ParsedTimestampRangeType {
    None,
    DateRange,
    TimeRange,
    DateTimeRange,
    Unknown,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct ParsedTimestampModifier {
    pub kind: ParsedTimestampModifierKind,
    pub modifier_type: ParsedTimestampModifierType,
    pub value: i64,
    pub unit: ParsedTimestampUnit,
    pub repeater_deadline_value: Option<i64>,
    pub repeater_deadline_unit: Option<ParsedTimestampUnit>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize)]
pub enum ParsedTimestampModifierKind {
    Repeater,
    Warning,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize)]
pub enum ParsedTimestampModifierType {
    Cumulate,
    CatchUp,
    Restart,
    All,
    First,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize)]
pub enum ParsedTimestampUnit {
    Hour,
    Day,
    Week,
    Month,
    Year,
}
