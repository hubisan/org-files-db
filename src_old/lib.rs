// ------------------------------------------------------------
// Top-level module wiring for the Org parser crate
// ------------------------------------------------------------

// Public modules
pub mod parser;
pub mod types;
pub mod config;

// Convenience re-exports (recommended)
pub use parser::parse_org;
pub use types::{OrgDocument, OrgElement, Heading, Property, Link, Block};

// Global helper (used by parser::parse_org)
pub fn ignore_line(line: &str) -> bool {
    line.starts_with(": ") || line.starts_with("# ")
}
