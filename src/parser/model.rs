use std::path::{Path, PathBuf};

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
}

impl Default for TodoKeywordConfig {
    fn default() -> Self {
        Self {
            open: vec![TodoKeyword::new("TODO")],
            closed: vec![TodoKeyword::new("DONE")],
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct TodoKeyword {
    pub name: String,
    pub fast_key: Option<char>,
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
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct ParsedHeading {
    pub file_path: PathBuf,
    pub level: u8,
    pub title: String,
    pub title_raw: String,
    pub todo_keyword: Option<String>,
    pub todo_type: Option<TodoType>,
    pub priority: Option<char>,
    pub tags: Vec<String>,
    pub properties: Vec<ParsedProperty>,
    pub planning: ParsedPlanning,
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
            todo_keyword: None,
            todo_type: None,
            priority: None,
            tags: Vec::new(),
            properties: Vec::new(),
            planning: ParsedPlanning::default(),
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
    pub value: String,
    pub inherited: bool,
}

#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub struct ParsedPlanning {
    pub scheduled: Option<String>,
    pub deadline: Option<String>,
    pub closed: Option<String>,
}
