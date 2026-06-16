use std::path::Path;

use crate::parser::{OrgParser, ParseDiagnostic, ParsedOrgDocument};

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

    pub fn parse(&self, path: &Path, content: &str) -> Result<ParsedOrgDocument, ParseDiagnostic> {
        self.parser.parse_document(path, content)
    }
}
