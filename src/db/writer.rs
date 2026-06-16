use crate::parser::ParsedOrgDocument;

#[derive(Debug, Default)]
pub struct DbWriter;

impl DbWriter {
    pub fn new() -> Self {
        Self
    }

    pub fn write_document(&self, _document: &ParsedOrgDocument) {}
}
