#[cfg(not(unix))]
compile_error!("org-files-db supports Unix-like platforms only; Windows is unsupported");

pub mod benchmark;
pub mod cli;
pub mod config;
pub mod db;
mod exclusions;
mod file_identity;
pub mod indexer;
mod indexing_context;
mod link_resolver;
#[allow(dead_code)]
pub(crate) mod notify_source;
pub mod parser;
pub(crate) mod property;
pub mod query;
pub(crate) mod tag;
pub mod todo_keywords;
#[allow(dead_code)]
pub(crate) mod watcher;
pub(crate) mod watcher_cli;
#[allow(dead_code)]
pub(crate) mod watcher_runtime;

#[cfg(test)]
mod watcher_integration_tests;
