pub mod diagnostics;
pub(crate) mod line_index;
pub mod line_lexer;
pub mod link_scanner;
pub mod model;
pub mod orgize_adapter;
pub(crate) mod orgize_inline;
pub(crate) mod properties;
pub mod structure_scanner;
pub(crate) mod timestamp_raw;
pub(crate) mod title;

pub use diagnostics::{DiagnosticSeverity, ParseDiagnostic};
pub use link_scanner::{
    scan_links, LinkScanContext, LinkScanner, LinkScannerConfig, DEFAULT_PLAIN_LINK_PROTOCOLS,
};
pub use model::{
    file_local_todo_keyword_config, OrgParser, OrgParserCore, ParseOptions, ParsedDocumentMetadata,
    ParsedHeading, ParsedKeyword, ParsedLink, ParsedLinkSourceContext, ParsedOrgDocument,
    ParsedPlanning, ParsedProperty, ParsedPropertySource, ParsedTimestamp, ParsedTimestampModifier,
    ParsedTimestampModifierKind, ParsedTimestampModifierType, ParsedTimestampRangeType,
    ParsedTimestampRole, ParsedTimestampType, ParsedTimestampUnit, TodoKeyword, TodoKeywordConfig,
    TodoType,
};
pub use orgize_adapter::OrgizeAdapter;

#[cfg(test)]
mod corpus_tests;
