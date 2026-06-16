use std::path::Path;

use super::diagnostics::ParseDiagnostic;
use super::model::{OrgParser, ParsedOrgDocument};

#[derive(Debug, Default, Clone, Copy)]
pub struct OrgizeAdapter;

impl OrgizeAdapter {
    pub fn new() -> Self {
        Self
    }
}

impl OrgParser for OrgizeAdapter {
    fn parse_document(
        &self,
        path: &Path,
        _content: &str,
    ) -> Result<ParsedOrgDocument, ParseDiagnostic> {
        Ok(ParsedOrgDocument::new(path))
    }
}
