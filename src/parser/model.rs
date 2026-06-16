use std::path::{Path, PathBuf};

use super::diagnostics::ParseDiagnostic;

pub trait OrgParser {
    fn parse_document(
        &self,
        path: &Path,
        content: &str,
    ) -> Result<ParsedOrgDocument, ParseDiagnostic>;
}

#[derive(Debug, Clone, PartialEq, Eq)]
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

#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct ParsedDocumentMetadata {
    pub title: Option<String>,
    pub keywords: Vec<ParsedKeyword>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ParsedKeyword {
    pub key: String,
    pub value: Option<String>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ParsedHeading {
    pub level: u8,
    pub title: String,
    pub title_raw: String,
    pub todo_keyword: Option<String>,
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
    pub fn new(level: u8, title: impl Into<String>, byte_start: usize, byte_end: usize) -> Self {
        let title = title.into();
        Self {
            level,
            title_raw: title.clone(),
            title,
            todo_keyword: None,
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

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ParsedProperty {
    pub key: String,
    pub value: String,
    pub inherited: bool,
}

#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct ParsedPlanning {
    pub scheduled: Option<String>,
    pub deadline: Option<String>,
    pub closed: Option<String>,
}
