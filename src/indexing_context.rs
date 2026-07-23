//! Versioned indexing context for safe future incremental decisions.
//!
//! The fingerprints deliberately encode only settings that affect indexed
//! facts, the source universe, or derived search state. Query presentation
//! settings, such as the query timezone, are intentionally absent.

// The comparison API is intentionally prepared before the next change-planning
// task consumes it. Full rebuild does not use it as a recovery prerequisite.
#![allow(dead_code)]

use std::{os::unix::ffi::OsStrExt, path::Path};

use rusqlite::{types::ValueRef, Connection, OptionalExtension};
use sha2::{Digest, Sha256};

use crate::{
    config::{Config, ConfiguredDir},
    db::{
        DbWriteError, DbWriter, DB_METADATA_INDEXING_DERIVED_SEARCH_FINGERPRINT_KEY,
        DB_METADATA_INDEXING_DERIVED_SEARCH_VERSION_KEY,
        DB_METADATA_INDEXING_DISCOVERY_FINGERPRINT_KEY, DB_METADATA_INDEXING_DISCOVERY_VERSION_KEY,
        DB_METADATA_INDEXING_SEMANTICS_FINGERPRINT_KEY, DB_METADATA_INDEXING_SEMANTICS_VERSION_KEY,
        FTS_SCHEMA_CONTRACT_VERSION,
    },
    parser::TodoKeyword,
};

pub(crate) const INDEXING_SEMANTICS_CONTRACT_VERSION: &str = "1";
pub(crate) const INDEXING_DISCOVERY_CONTRACT_VERSION: &str = "1";
pub(crate) const INDEXING_DERIVED_SEARCH_CONTRACT_VERSION: &str = "1";
const PARSER_INDEXER_CONTRACT_VERSION: &str = "1";

/// The work a later incremental caller must perform before trusting unchanged
/// source snapshots.
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub(crate) struct IndexInvalidationSet(u8);

impl IndexInvalidationSet {
    pub(crate) const REPARSE_ALL_FILES: Self = Self(1 << 0);
    pub(crate) const RECONCILE_SOURCE_UNIVERSE_AND_LINKS: Self = Self(1 << 1);
    pub(crate) const REBUILD_DERIVED_SEARCH: Self = Self(1 << 2);

    pub(crate) fn contains(self, invalidation: Self) -> bool {
        self.0 & invalidation.0 == invalidation.0
    }

    fn insert(&mut self, invalidation: Self) {
        self.0 |= invalidation.0;
    }

    fn is_empty(self) -> bool {
        self.0 == 0
    }
}

/// Fail-safe comparison of the persisted context with the current process.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum IndexingContextComparison {
    Compatible,
    FullRebuildRequired,
    Invalidations(IndexInvalidationSet),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct IndexingContext {
    semantics_fingerprint: String,
    discovery_fingerprint: String,
    derived_search_fingerprint: String,
}

impl IndexingContext {
    pub(crate) fn from_config(config: &Config, fts_backend_available: bool) -> Self {
        Self {
            semantics_fingerprint: semantics_fingerprint(config),
            discovery_fingerprint: discovery_fingerprint(config),
            derived_search_fingerprint: derived_search_fingerprint(config, fts_backend_available),
        }
    }

    /// Writes all context entries through the caller's existing transaction.
    pub(crate) fn persist(&self, connection: &Connection) -> Result<(), DbWriteError> {
        for (key, value) in [
            (
                DB_METADATA_INDEXING_SEMANTICS_VERSION_KEY,
                INDEXING_SEMANTICS_CONTRACT_VERSION,
            ),
            (
                DB_METADATA_INDEXING_SEMANTICS_FINGERPRINT_KEY,
                self.semantics_fingerprint.as_str(),
            ),
            (
                DB_METADATA_INDEXING_DISCOVERY_VERSION_KEY,
                INDEXING_DISCOVERY_CONTRACT_VERSION,
            ),
            (
                DB_METADATA_INDEXING_DISCOVERY_FINGERPRINT_KEY,
                self.discovery_fingerprint.as_str(),
            ),
            (
                DB_METADATA_INDEXING_DERIVED_SEARCH_VERSION_KEY,
                INDEXING_DERIVED_SEARCH_CONTRACT_VERSION,
            ),
            (
                DB_METADATA_INDEXING_DERIVED_SEARCH_FINGERPRINT_KEY,
                self.derived_search_fingerprint.as_str(),
            ),
        ] {
            DbWriter::set_metadata_value(connection, key, value)?;
        }
        Ok(())
    }

    pub(crate) fn compare(
        &self,
        connection: &Connection,
    ) -> Result<IndexingContextComparison, DbWriteError> {
        let Some(stored) = StoredIndexingContext::load(connection)? else {
            return Ok(IndexingContextComparison::FullRebuildRequired);
        };

        if !stored.has_supported_contracts() {
            return Ok(IndexingContextComparison::FullRebuildRequired);
        }

        let mut invalidations = IndexInvalidationSet::default();
        if stored.semantics_fingerprint != self.semantics_fingerprint {
            invalidations.insert(IndexInvalidationSet::REPARSE_ALL_FILES);
        }
        if stored.discovery_fingerprint != self.discovery_fingerprint {
            invalidations.insert(IndexInvalidationSet::RECONCILE_SOURCE_UNIVERSE_AND_LINKS);
        }
        if stored.derived_search_fingerprint != self.derived_search_fingerprint {
            invalidations.insert(IndexInvalidationSet::REBUILD_DERIVED_SEARCH);
        }

        if invalidations.is_empty() {
            Ok(IndexingContextComparison::Compatible)
        } else {
            Ok(IndexingContextComparison::Invalidations(invalidations))
        }
    }
}

struct StoredIndexingContext {
    semantics_version: String,
    semantics_fingerprint: String,
    discovery_version: String,
    discovery_fingerprint: String,
    derived_search_version: String,
    derived_search_fingerprint: String,
}

impl StoredIndexingContext {
    fn load(connection: &Connection) -> Result<Option<Self>, DbWriteError> {
        let values = [
            load_metadata(connection, DB_METADATA_INDEXING_SEMANTICS_VERSION_KEY)?,
            load_metadata(connection, DB_METADATA_INDEXING_SEMANTICS_FINGERPRINT_KEY)?,
            load_metadata(connection, DB_METADATA_INDEXING_DISCOVERY_VERSION_KEY)?,
            load_metadata(connection, DB_METADATA_INDEXING_DISCOVERY_FINGERPRINT_KEY)?,
            load_metadata(connection, DB_METADATA_INDEXING_DERIVED_SEARCH_VERSION_KEY)?,
            load_metadata(
                connection,
                DB_METADATA_INDEXING_DERIVED_SEARCH_FINGERPRINT_KEY,
            )?,
        ];
        let [LoadedMetadata::Text(semantics_version), LoadedMetadata::Text(semantics_fingerprint), LoadedMetadata::Text(discovery_version), LoadedMetadata::Text(discovery_fingerprint), LoadedMetadata::Text(derived_search_version), LoadedMetadata::Text(derived_search_fingerprint)] =
            values
        else {
            return Ok(None);
        };

        Ok(Some(Self {
            semantics_version,
            semantics_fingerprint,
            discovery_version,
            discovery_fingerprint,
            derived_search_version,
            derived_search_fingerprint,
        }))
    }

    fn has_supported_contracts(&self) -> bool {
        self.semantics_version == INDEXING_SEMANTICS_CONTRACT_VERSION
            && self.discovery_version == INDEXING_DISCOVERY_CONTRACT_VERSION
            && self.derived_search_version == INDEXING_DERIVED_SEARCH_CONTRACT_VERSION
            && valid_fingerprint(&self.semantics_fingerprint)
            && valid_fingerprint(&self.discovery_fingerprint)
            && valid_fingerprint(&self.derived_search_fingerprint)
    }
}

enum LoadedMetadata {
    Missing,
    Text(String),
    Malformed,
}

fn load_metadata(connection: &Connection, key: &str) -> Result<LoadedMetadata, DbWriteError> {
    connection
        .query_row(
            "SELECT value FROM db_metadata WHERE key = ?1",
            [key],
            |row| {
                let value = row.get_ref(0)?;
                Ok(match value {
                    ValueRef::Text(bytes) => match std::str::from_utf8(bytes) {
                        Ok(value) => LoadedMetadata::Text(value.to_string()),
                        Err(_) => LoadedMetadata::Malformed,
                    },
                    ValueRef::Null
                    | ValueRef::Integer(_)
                    | ValueRef::Real(_)
                    | ValueRef::Blob(_) => LoadedMetadata::Malformed,
                })
            },
        )
        .optional()
        .map(|value| value.unwrap_or(LoadedMetadata::Missing))
        .map_err(|source| DbWriteError::ReadBack {
            operation: "load_indexing_context_metadata",
            source,
        })
}

fn valid_fingerprint(value: &str) -> bool {
    value.len() == 64
        && value
            .bytes()
            .all(|byte| byte.is_ascii_digit() || matches!(byte, b'a'..=b'f'))
}

fn semantics_fingerprint(config: &Config) -> String {
    let mut encoder = FingerprintEncoder::new("indexing-semantics");
    encoder.string("contract", INDEXING_SEMANTICS_CONTRACT_VERSION);
    encoder.string("parser-indexer-contract", PARSER_INDEXER_CONTRACT_VERSION);
    encoder.todo_keywords("default-open-todo", &config.todo.default_open_keywords);
    encoder.todo_keywords("default-closed-todo", &config.todo.default_closed_keywords);
    encoder.string_set("plain-link-protocols", &config.links.plain_protocols);
    encoder.bool("body-text-persisted", config.search.index_body_text);
    encoder.finish()
}

fn discovery_fingerprint(config: &Config) -> String {
    let mut encoder = FingerprintEncoder::new("indexing-discovery");
    encoder.string("contract", INDEXING_DISCOVERY_CONTRACT_VERSION);
    encoder.path_set("explicit-files", &config.files);
    encoder.path("config-dir", &config.discovery.config_dir);
    encoder.optional_path("home-dir", config.discovery.home_dir.as_deref());
    encoder.string_set("global-file-exclusions", &config.discovery.files_exclude);

    let mut roots = config.dirs.iter().map(encode_root).collect::<Vec<_>>();
    roots.sort();
    roots.dedup();
    encoder.bytes_set("directory-roots", &roots);
    encoder.finish()
}

fn derived_search_fingerprint(config: &Config, fts_backend_available: bool) -> String {
    let mut encoder = FingerprintEncoder::new("indexing-derived-search");
    encoder.string("contract", INDEXING_DERIVED_SEARCH_CONTRACT_VERSION);
    encoder.bool("fts5-enabled", config.search.fts5_enabled);
    encoder.bool("fts5-backend-available", fts_backend_available);
    encoder.string("fts-schema-contract", FTS_SCHEMA_CONTRACT_VERSION);
    encoder.bool("fts-body-indexed", config.search.index_body_text);
    encoder.finish()
}

fn encode_root(root: &ConfiguredDir) -> Vec<u8> {
    let mut encoder = BinaryEncoder::default();
    encoder.path("logical-root", &root.path);
    encoder.bool("recursive", root.recursive);
    encoder.string_set("local-exclusions", &root.exclude);
    encoder.into_bytes()
}

struct FingerprintEncoder {
    hasher: Sha256,
}

impl FingerprintEncoder {
    fn new(domain: &str) -> Self {
        let mut encoder = Self {
            hasher: Sha256::new(),
        };
        encoder.string("format", "orgfdb-indexing-context-v1");
        encoder.string("domain", domain);
        encoder
    }

    fn string(&mut self, tag: &str, value: &str) {
        self.field(tag, value.as_bytes());
    }

    fn bool(&mut self, tag: &str, value: bool) {
        self.field(tag, &[u8::from(value)]);
    }

    fn path(&mut self, tag: &str, value: &Path) {
        self.field(tag, value.as_os_str().as_bytes());
    }

    fn optional_path(&mut self, tag: &str, value: Option<&Path>) {
        match value {
            Some(path) => self.path(tag, path),
            None => self.field(tag, &[]),
        }
    }

    fn todo_keywords(&mut self, tag: &str, values: &[TodoKeyword]) {
        let mut encoder = BinaryEncoder::default();
        for keyword in values {
            encoder.string("name", &keyword.name);
            match keyword.fast_key {
                Some(fast_key) => encoder.string("fast-key", &fast_key.to_string()),
                None => encoder.bytes("fast-key-none", &[]),
            }
        }
        self.field(tag, &encoder.into_bytes());
    }

    fn string_set(&mut self, tag: &str, values: &[String]) {
        let values = values
            .iter()
            .map(|value| value.as_bytes().to_vec())
            .collect::<Vec<_>>();
        self.bytes_set(tag, &values);
    }

    fn path_set(&mut self, tag: &str, values: &[std::path::PathBuf]) {
        let values = values
            .iter()
            .map(|value| value.as_os_str().as_bytes().to_vec())
            .collect::<Vec<_>>();
        self.bytes_set(tag, &values);
    }

    fn bytes_set(&mut self, tag: &str, values: &[Vec<u8>]) {
        let mut values = values.to_vec();
        values.sort();
        values.dedup();
        let mut encoder = BinaryEncoder::default();
        for value in values {
            encoder.bytes("item", &value);
        }
        self.field(tag, &encoder.into_bytes());
    }

    fn field(&mut self, tag: &str, value: &[u8]) {
        self.hasher.update((tag.len() as u64).to_be_bytes());
        self.hasher.update(tag.as_bytes());
        self.hasher.update((value.len() as u64).to_be_bytes());
        self.hasher.update(value);
    }

    fn finish(self) -> String {
        format!("{:x}", self.hasher.finalize())
    }
}

#[derive(Default)]
struct BinaryEncoder(Vec<u8>);

impl BinaryEncoder {
    fn string(&mut self, tag: &str, value: &str) {
        self.bytes(tag, value.as_bytes());
    }

    fn bool(&mut self, tag: &str, value: bool) {
        self.bytes(tag, &[u8::from(value)]);
    }

    fn path(&mut self, tag: &str, value: &Path) {
        self.bytes(tag, value.as_os_str().as_bytes());
    }

    fn bytes(&mut self, tag: &str, value: &[u8]) {
        self.0.extend_from_slice(&(tag.len() as u64).to_be_bytes());
        self.0.extend_from_slice(tag.as_bytes());
        self.0
            .extend_from_slice(&(value.len() as u64).to_be_bytes());
        self.0.extend_from_slice(value);
    }

    fn string_set(&mut self, tag: &str, values: &[String]) {
        let mut values = values
            .iter()
            .map(|value| value.as_bytes().to_vec())
            .collect::<Vec<_>>();
        values.sort();
        values.dedup();
        for value in values {
            self.bytes(tag, &value);
        }
    }

    fn into_bytes(self) -> Vec<u8> {
        self.0
    }
}

#[cfg(test)]
mod tests {
    use std::{ffi::OsStr, os::unix::ffi::OsStrExt, path::PathBuf};

    use super::{IndexInvalidationSet, IndexingContext, IndexingContextComparison};
    use crate::{
        config::{Config, ConfiguredDir},
        db::{
            open_in_memory_database, DB_METADATA_INDEXING_DERIVED_SEARCH_FINGERPRINT_KEY,
            DB_METADATA_INDEXING_SEMANTICS_FINGERPRINT_KEY,
            DB_METADATA_INDEXING_SEMANTICS_VERSION_KEY,
        },
        parser::TodoKeyword,
    };

    fn persisted(
        config: &Config,
        backend_available: bool,
    ) -> (rusqlite::Connection, IndexingContext) {
        let connection = open_in_memory_database().expect("database should open");
        let context = IndexingContext::from_config(config, backend_available);
        context
            .persist(&connection)
            .expect("context should persist");
        (connection, context)
    }

    fn comparison(config: &Config, changed: &Config) -> IndexingContextComparison {
        let (connection, _) = persisted(config, true);
        IndexingContext::from_config(changed, true)
            .compare(&connection)
            .expect("context comparison should work")
    }

    #[test]
    fn unchanged_context_is_compatible_and_timezone_is_query_only() {
        let config = Config::default();
        let mut timezone_changed = config.clone();
        timezone_changed.query.timezone = Some("Europe/Zurich".to_string());

        assert_eq!(
            comparison(&config, &timezone_changed),
            IndexingContextComparison::Compatible
        );
    }

    #[test]
    fn todo_and_protocol_changes_require_reparsing() {
        let config = Config::default();
        let mut todo_changed = config.clone();
        todo_changed
            .todo
            .default_open_keywords
            .push(TodoKeyword::new("NEXT"));
        let mut protocol_changed = config.clone();
        protocol_changed
            .links
            .plain_protocols
            .push("custom".to_string());

        for changed in [todo_changed, protocol_changed] {
            assert_eq!(
                comparison(&config, &changed),
                IndexingContextComparison::Invalidations(IndexInvalidationSet::REPARSE_ALL_FILES)
            );
        }
    }

    #[test]
    fn set_like_inputs_are_order_independent_but_todo_order_is_semantic() {
        let mut config = Config::default();
        config.links.plain_protocols = vec!["https".to_string(), "file".to_string()];
        config.discovery.files_exclude = vec!["*.org".to_string(), "private.org".to_string()];
        config
            .todo
            .default_open_keywords
            .push(TodoKeyword::new("NEXT"));
        let mut reordered = config.clone();
        reordered.links.plain_protocols.reverse();
        reordered.discovery.files_exclude.reverse();
        assert_eq!(
            comparison(&config, &reordered),
            IndexingContextComparison::Compatible
        );

        let mut todo_reordered = config.clone();
        todo_reordered.todo.default_open_keywords.reverse();
        assert_eq!(
            comparison(&config, &todo_reordered),
            IndexingContextComparison::Invalidations(IndexInvalidationSet::REPARSE_ALL_FILES)
        );
    }

    #[test]
    fn source_universe_changes_require_reconciliation() {
        let config = Config::default();
        let mut changed = config.clone();
        changed.dirs.push(ConfiguredDir {
            path: PathBuf::from("/tmp/notes"),
            recursive: true,
            exclude: vec!["archive/**".to_string()],
        });

        assert_eq!(
            comparison(&config, &changed),
            IndexingContextComparison::Invalidations(
                IndexInvalidationSet::RECONCILE_SOURCE_UNIVERSE_AND_LINKS
            )
        );
    }

    #[test]
    fn body_and_search_backend_changes_combine_invalidations() {
        let config = Config::default();
        let mut body_changed = config.clone();
        body_changed.search.index_body_text = true;
        let IndexingContextComparison::Invalidations(invalidations) =
            comparison(&config, &body_changed)
        else {
            panic!("body indexing should require invalidations");
        };
        assert!(invalidations.contains(IndexInvalidationSet::REPARSE_ALL_FILES));
        assert!(invalidations.contains(IndexInvalidationSet::REBUILD_DERIVED_SEARCH));

        let (connection, _) = persisted(&config, true);
        let changed = IndexingContext::from_config(&config, false);
        assert_eq!(
            changed
                .compare(&connection)
                .expect("comparison should work"),
            IndexingContextComparison::Invalidations(IndexInvalidationSet::REBUILD_DERIVED_SEARCH)
        );
    }

    #[test]
    fn combined_changes_are_composable() {
        let config = Config::default();
        let mut changed = config.clone();
        changed
            .todo
            .default_open_keywords
            .push(TodoKeyword::new("NEXT"));
        changed.files.push(PathBuf::from("/tmp/notes.org"));
        changed.search.fts5_enabled = false;

        let IndexingContextComparison::Invalidations(invalidations) = comparison(&config, &changed)
        else {
            panic!("changes should require invalidations");
        };
        assert!(invalidations.contains(IndexInvalidationSet::REPARSE_ALL_FILES));
        assert!(invalidations.contains(IndexInvalidationSet::RECONCILE_SOURCE_UNIVERSE_AND_LINKS));
        assert!(invalidations.contains(IndexInvalidationSet::REBUILD_DERIVED_SEARCH));
    }

    #[test]
    fn missing_malformed_unsupported_and_non_text_metadata_require_full_rebuild() {
        let config = Config::default();
        let connection = open_in_memory_database().expect("database should open");
        let context = IndexingContext::from_config(&config, true);
        assert_eq!(
            context
                .compare(&connection)
                .expect("comparison should work"),
            IndexingContextComparison::FullRebuildRequired
        );

        context
            .persist(&connection)
            .expect("context should persist");
        connection
            .execute(
                "DELETE FROM db_metadata WHERE key = ?1",
                [DB_METADATA_INDEXING_DERIVED_SEARCH_FINGERPRINT_KEY],
            )
            .expect("metadata should delete");
        assert_eq!(
            context
                .compare(&connection)
                .expect("comparison should work"),
            IndexingContextComparison::FullRebuildRequired
        );

        context
            .persist(&connection)
            .expect("context should persist");
        connection
            .execute(
                "UPDATE db_metadata SET value = 'not-a-fingerprint' WHERE key = ?1",
                [DB_METADATA_INDEXING_SEMANTICS_FINGERPRINT_KEY],
            )
            .expect("metadata should update");
        assert_eq!(
            context
                .compare(&connection)
                .expect("comparison should work"),
            IndexingContextComparison::FullRebuildRequired
        );

        context
            .persist(&connection)
            .expect("context should persist");
        connection
            .execute(
                "UPDATE db_metadata SET value = '999' WHERE key = 'indexing_semantics_version'",
                [],
            )
            .expect("metadata should update");
        assert_eq!(
            context
                .compare(&connection)
                .expect("comparison should work"),
            IndexingContextComparison::FullRebuildRequired
        );

        context
            .persist(&connection)
            .expect("context should persist");
        connection
            .execute(
                "UPDATE db_metadata SET value = CAST(x'0102' AS BLOB) WHERE key = ?1",
                [DB_METADATA_INDEXING_SEMANTICS_FINGERPRINT_KEY],
            )
            .expect("metadata should update");
        assert_eq!(
            context
                .compare(&connection)
                .expect("comparison should work"),
            IndexingContextComparison::FullRebuildRequired
        );

        let integer_connection =
            rusqlite::Connection::open_in_memory().expect("untyped metadata database should open");
        integer_connection
            .execute_batch(
                "CREATE TABLE db_metadata (
                    key TEXT PRIMARY KEY,
                    value
                );",
            )
            .expect("metadata table should create");
        context
            .persist(&integer_connection)
            .expect("context should persist");
        integer_connection
            .execute(
                "UPDATE db_metadata SET value = 1 WHERE key = ?1",
                [DB_METADATA_INDEXING_SEMANTICS_VERSION_KEY],
            )
            .expect("metadata should update");
        assert_eq!(
            context
                .compare(&integer_connection)
                .expect("comparison should work"),
            IndexingContextComparison::FullRebuildRequired
        );
    }

    #[test]
    fn native_unix_path_bytes_are_fingerprinted_without_loss() {
        let mut config = Config::default();
        config
            .files
            .push(PathBuf::from(OsStr::from_bytes(b"/tmp/nonutf-\xff.org")));
        let fingerprint = IndexingContext::from_config(&config, true);
        config.files = vec![PathBuf::from(OsStr::from_bytes(b"/tmp/nonutf-\xfe.org"))];
        let changed = IndexingContext::from_config(&config, true);

        assert_ne!(fingerprint, changed);
    }

    #[test]
    fn persisted_values_are_lowercase_sha256_fingerprints() {
        let config = Config::default();
        let (connection, _) = persisted(&config, true);
        let value: String = connection
            .query_row(
                "SELECT value FROM db_metadata WHERE key = ?1",
                [DB_METADATA_INDEXING_DERIVED_SEARCH_FINGERPRINT_KEY],
                |row| row.get(0),
            )
            .expect("fingerprint should exist");
        assert_eq!(value.len(), 64);
        assert!(value
            .bytes()
            .all(|byte| byte.is_ascii_digit() || matches!(byte, b'a'..=b'f')));
    }
}
