#[cfg(not(unix))]
compile_error!("org-files-db supports Unix-like platforms only; Windows is unsupported");

pub mod benchmark;
pub mod cli;
pub mod config;
pub mod db;
mod exclusions;
mod file_identity;
mod hex_encoding;
pub mod indexer;
mod indexing_context;
mod link_resolver;
pub(crate) mod notify_source;
pub mod parser;
pub mod presentation;
pub mod presentation_benchmark;
#[cfg(feature = "presentation-cache-benchmark")]
pub mod presentation_cache_benchmark;
pub(crate) mod presentation_view;
pub(crate) mod presentation_view_cache;
pub(crate) mod presentation_view_rebuild;
pub(crate) mod property;
pub mod query;
pub mod query_sql_benchmark;
mod source_root_evidence;
pub(crate) mod tag;
pub mod todo_keywords;
pub(crate) mod watcher;
pub(crate) mod watcher_cli;
pub(crate) mod watcher_runtime;

#[cfg(test)]
mod watcher_integration_tests;
