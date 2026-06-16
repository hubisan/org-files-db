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
    pub headings: Vec<ParsedHeading>,
    pub diagnostics: Vec<ParseDiagnostic>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ParsedHeading {
    pub level: u8,
    pub title: String,
    pub todo_keyword: Option<String>,
    pub priority: Option<char>,
    pub tags: Vec<String>,
    pub properties: Vec<(String, String)>,
    pub planning: ParsedPlanning,
    pub line_number: Option<u32>,
    pub byte_start: usize,
    pub byte_end: usize,
    pub parent_index: Option<usize>,
}

#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct ParsedPlanning {
    pub scheduled: Option<String>,
    pub deadline: Option<String>,
    pub closed: Option<String>,
}
