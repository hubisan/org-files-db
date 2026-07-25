use std::{
    cell::RefCell,
    collections::HashMap,
    error::Error,
    fmt,
    path::{Path, PathBuf},
};

use regex::Regex;
use rusqlite::{functions::FunctionFlags, Connection, OpenFlags};

pub(crate) mod reader;
pub mod schema;
pub(crate) mod writer;

pub(crate) use reader::{DbReadError, DbReader, HeadingListRow, LinkListRow};
pub use schema::{sqlite_supports_fts5, SchemaDefinition, CURRENT_SCHEMA_VERSION};
pub use writer::DbWriteError;
pub(crate) use writer::{
    DbWriter, EffectivePropertyRecord, FileRecordInput, HeadingBodyRecord, HeadingRecord,
    KeywordRecord, LinkRecord, OutlinePathRecord, PropertyRecord, TagRecord, TimestampRecord,
    TimestampRepeaterRecord, TodoKeywordRecord,
};

pub const DB_METADATA_BODY_TEXT_AVAILABLE_KEY: &str = "body_text_available";
pub const DB_METADATA_FTS_AVAILABLE_KEY: &str = "fts_available";
pub const DB_METADATA_FTS_BODY_INDEXED_KEY: &str = "fts_body_indexed";
pub const DB_METADATA_FTS_SCHEMA_VERSION_KEY: &str = "fts_schema_version";
pub const DB_METADATA_INDEXING_SEMANTICS_VERSION_KEY: &str = "indexing_semantics_version";
pub const DB_METADATA_INDEXING_SEMANTICS_FINGERPRINT_KEY: &str = "indexing_semantics_fingerprint";
pub const DB_METADATA_INDEXING_DISCOVERY_VERSION_KEY: &str = "indexing_discovery_version";
pub const DB_METADATA_INDEXING_DISCOVERY_FINGERPRINT_KEY: &str = "indexing_discovery_fingerprint";
pub const DB_METADATA_INDEXING_DERIVED_SEARCH_VERSION_KEY: &str = "indexing_derived_search_version";
pub const DB_METADATA_INDEXING_DERIVED_SEARCH_FINGERPRINT_KEY: &str =
    "indexing_derived_search_fingerprint";
pub const FTS_SCHEMA_CONTRACT_VERSION: &str = "3";

#[cfg(test)]
const IN_MEMORY_DATABASE: &str = ":memory:";

pub fn open_database(path: impl AsRef<Path>) -> Result<Connection, DbError> {
    open_database_with_schema(path, &SchemaDefinition::default())
}

pub(crate) fn open_existing_database_read_only(
    path: impl AsRef<Path>,
) -> Result<Connection, DbError> {
    open_existing_database_read_only_with_schema(path, &SchemaDefinition::default())
}

pub(crate) fn open_database_with_schema(
    path: impl AsRef<Path>,
    schema: &SchemaDefinition,
) -> Result<Connection, DbError> {
    let path = path.as_ref();
    let target = path.display().to_string();
    let mut connection = Connection::open(path).map_err(|source| DbError::Open {
        path: path.to_path_buf(),
        source,
    })?;
    initialize_database(&mut connection, &target, schema)?;
    Ok(connection)
}

pub(crate) fn open_existing_database_read_only_with_schema(
    path: impl AsRef<Path>,
    schema: &SchemaDefinition,
) -> Result<Connection, DbError> {
    let path = path.as_ref();
    let target = path.display().to_string();
    let connection =
        Connection::open_with_flags(path, OpenFlags::SQLITE_OPEN_READ_ONLY).map_err(|source| {
            DbError::Open {
                path: path.to_path_buf(),
                source,
            }
        })?;
    register_connection_functions(&connection, &target)?;
    let on_disk_version = read_schema_version(&connection).map_err(|source| DbError::Inspect {
        target: target.clone(),
        source,
    })?;
    validate_schema_version(on_disk_version, schema, &target)?;
    Ok(connection)
}

#[cfg(test)]
pub(crate) fn open_in_memory_database() -> Result<Connection, DbError> {
    open_in_memory_database_with_schema(&SchemaDefinition::default())
}

#[cfg(test)]
pub(crate) fn open_in_memory_database_with_schema(
    schema: &SchemaDefinition,
) -> Result<Connection, DbError> {
    let mut connection =
        Connection::open_in_memory().map_err(|source| DbError::OpenInMemory { source })?;
    initialize_database(&mut connection, IN_MEMORY_DATABASE, schema)?;
    Ok(connection)
}

fn initialize_database(
    connection: &mut Connection,
    target: &str,
    schema: &SchemaDefinition,
) -> Result<(), DbError> {
    register_connection_functions(connection, target)?;
    connection
        .execute_batch(
            r#"
PRAGMA foreign_keys = ON;
PRAGMA journal_mode = WAL;
PRAGMA synchronous = NORMAL;
"#,
        )
        .map_err(|source| DbError::Initialize {
            target: target.to_string(),
            source,
        })?;

    if schema.enable_fts
        && !sqlite_supports_fts5(connection).map_err(|source| DbError::Initialize {
            target: target.to_string(),
            source,
        })?
    {
        return Err(DbError::UnsupportedBackendFeature {
            target: target.to_string(),
            feature: "SQLite FTS5",
            message:
                "SQLite FTS5 was requested, but the opened SQLite connection does not support FTS5"
                    .to_string(),
        });
    }

    let on_disk_version =
        read_schema_version(connection).map_err(|source| DbError::Initialize {
            target: target.to_string(),
            source,
        })?;
    validate_schema_version(on_disk_version, schema, target)?;

    let tx = connection
        .transaction()
        .map_err(|source| DbError::Initialize {
            target: target.to_string(),
            source,
        })?;
    schema.apply(&tx).map_err(|source| DbError::Initialize {
        target: target.to_string(),
        source,
    })?;

    if on_disk_version != schema.version {
        tx.pragma_update(None, "user_version", schema.version)
            .map_err(|source| DbError::Initialize {
                target: target.to_string(),
                source,
            })?;
    }

    tx.commit().map_err(|source| DbError::Initialize {
        target: target.to_string(),
        source,
    })?;

    Ok(())
}

fn register_connection_functions(connection: &Connection, target: &str) -> Result<(), DbError> {
    let cache = RefCell::new(HashMap::<String, Regex>::new());
    connection
        .create_scalar_function(
            "orgfdb_regexp",
            2,
            FunctionFlags::SQLITE_UTF8 | FunctionFlags::SQLITE_DETERMINISTIC,
            move |ctx| {
                let pattern: String = ctx.get(0)?;
                let value: Option<String> = ctx.get(1)?;
                let Some(value) = value else {
                    return Ok(false);
                };

                let regex = {
                    let mut cache = cache.borrow_mut();
                    if let Some(regex) = cache.get(&pattern) {
                        regex.clone()
                    } else {
                        let regex = Regex::new(&pattern).map_err(|error| {
                            rusqlite::Error::UserFunctionError(Box::new(SqliteRegexpError(
                                error.to_string(),
                            )))
                        })?;
                        if cache.len() >= 128 {
                            cache.clear();
                        }
                        cache.insert(pattern.clone(), regex.clone());
                        regex
                    }
                };

                Ok(regex.is_match(&value))
            },
        )
        .map_err(|source| DbError::Initialize {
            target: target.to_string(),
            source,
        })?;

    Ok(())
}

#[derive(Debug)]
struct SqliteRegexpError(String);

impl fmt::Display for SqliteRegexpError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(&self.0)
    }
}

impl Error for SqliteRegexpError {}

fn read_schema_version(connection: &Connection) -> rusqlite::Result<u32> {
    connection.pragma_query_value(None, "user_version", |row| row.get(0))
}

fn validate_schema_version(
    on_disk_version: u32,
    schema: &SchemaDefinition,
    target: &str,
) -> Result<(), DbError> {
    if on_disk_version > schema.version {
        return Err(DbError::UnsupportedFutureSchemaVersion {
            target: target.to_string(),
            on_disk_version,
            supported_version: schema.version,
        });
    }

    Ok(())
}

#[derive(Debug)]
pub enum DbError {
    Open {
        path: PathBuf,
        source: rusqlite::Error,
    },
    OpenInMemory {
        source: rusqlite::Error,
    },
    Initialize {
        target: String,
        source: rusqlite::Error,
    },
    Inspect {
        target: String,
        source: rusqlite::Error,
    },
    UnsupportedFutureSchemaVersion {
        target: String,
        on_disk_version: u32,
        supported_version: u32,
    },
    UnsupportedBackendFeature {
        target: String,
        feature: &'static str,
        message: String,
    },
}

impl fmt::Display for DbError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Open { path, source } => {
                write!(
                    f,
                    "failed to open SQLite database {}: {}",
                    path.display(),
                    source
                )
            }
            Self::OpenInMemory { source } => {
                write!(f, "failed to open in-memory SQLite database: {}", source)
            }
            Self::Initialize { target, source } => {
                write!(
                    f,
                    "failed to initialize SQLite database {}: {}",
                    target, source
                )
            }
            Self::Inspect { target, source } => {
                write!(f, "failed to inspect SQLite database {}: {}", target, source)
            }
            Self::UnsupportedFutureSchemaVersion {
                target,
                on_disk_version,
                supported_version,
            } => write!(
                f,
                "failed to open SQLite database {}: unsupported future schema version {} (this binary supports up to {})",
                target, on_disk_version, supported_version
            ),
            Self::UnsupportedBackendFeature {
                target, message, ..
            } => write!(f, "failed to initialize SQLite database {}: {}", target, message),
        }
    }
}

impl Error for DbError {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        match self {
            Self::Open { source, .. }
            | Self::OpenInMemory { source }
            | Self::Initialize { source, .. }
            | Self::Inspect { source, .. } => Some(source),
            Self::UnsupportedFutureSchemaVersion { .. }
            | Self::UnsupportedBackendFeature { .. } => None,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::{
        initialize_database, open_database, open_in_memory_database,
        open_in_memory_database_with_schema, read_schema_version, sqlite_supports_fts5, DbError,
        DbReader, SchemaDefinition, CURRENT_SCHEMA_VERSION,
    };
    use rusqlite::{params, Connection, OptionalExtension};
    use std::{
        fs,
        path::{Path, PathBuf},
        time::{SystemTime, UNIX_EPOCH},
    };

    type MigratedLegacyRepeaterRow = (
        Option<String>,
        Option<i64>,
        Option<String>,
        Option<String>,
        Option<i64>,
        Option<String>,
    );
    type MigratedExplicitRepeaterRow = (
        i64,
        Option<String>,
        Option<i64>,
        Option<String>,
        Option<i64>,
        Option<String>,
        Option<String>,
        Option<i64>,
        Option<String>,
    );
    struct TestDir {
        path: PathBuf,
    }

    impl TestDir {
        fn new(name: &str) -> Self {
            let unique = SystemTime::now()
                .duration_since(UNIX_EPOCH)
                .expect("system time should be after unix epoch")
                .as_nanos();
            let path = std::env::temp_dir().join(format!(
                "org-files-db-db-tests-{}-{}-{}",
                name,
                std::process::id(),
                unique
            ));
            fs::create_dir_all(&path).expect("test dir should be created");
            Self { path }
        }

        fn path(&self) -> &Path {
            &self.path
        }
    }

    impl Drop for TestDir {
        fn drop(&mut self) {
            let _ = fs::remove_dir_all(&self.path);
        }
    }

    fn count_rows(connection: &Connection, sql: &str) -> i64 {
        connection
            .query_row(sql, [], |row| row.get(0))
            .expect("count query should succeed")
    }

    fn explicit_index_names(connection: &Connection) -> Vec<String> {
        let mut statement = connection
            .prepare(
                "SELECT name
                 FROM sqlite_master
                 WHERE type = 'index' AND sql IS NOT NULL
                 ORDER BY name",
            )
            .expect("explicit index inventory should prepare");
        statement
            .query_map([], |row| row.get(0))
            .expect("explicit index inventory should query")
            .collect::<Result<Vec<_>, _>>()
            .expect("explicit index inventory should decode")
    }

    fn expected_current_explicit_indexes() -> Vec<String> {
        [
            "files_identity_unique",
            "idx_effective_properties_file",
            "idx_files_hash",
            "idx_files_mtime_size",
            "idx_files_path_lower",
            "idx_headings_closed",
            "idx_headings_deadline",
            "idx_headings_parent_id",
            "idx_headings_scheduled",
            "idx_headings_title_lower",
            "idx_headings_todo",
            "idx_headings_todo_type",
            "idx_keywords_keyword",
            "idx_links_heading",
            "idx_links_path",
            "idx_links_target_file",
            "idx_links_target_heading",
            "idx_outline_file_materialized_path",
            "idx_outline_parent",
            "idx_properties_custom_id_lookup",
            "idx_properties_heading_key",
            "idx_properties_id_lookup",
            "idx_properties_key_value",
            "idx_tags_tag",
            "idx_timestamps_heading_id",
            "idx_timestamps_role_start",
            "idx_timestamps_start",
            "idx_todo_keywords_file_state",
            "uq_headings_file_level0",
        ]
        .into_iter()
        .map(str::to_string)
        .collect()
    }

    fn foreign_key_targets(connection: &Connection, table_name: &str) -> Vec<String> {
        let mut statement = connection
            .prepare(&format!("PRAGMA foreign_key_list({table_name})"))
            .expect("foreign_key_list should prepare");
        statement
            .query_map([], |row| row.get(2))
            .expect("foreign_key_list should query")
            .collect::<Result<Vec<String>, _>>()
            .expect("foreign_key_list rows should collect")
    }

    fn foreign_key_check_rows(connection: &Connection) -> Vec<(String, i64, String, i64)> {
        let mut statement = connection
            .prepare("PRAGMA foreign_key_check")
            .expect("foreign_key_check should prepare");
        statement
            .query_map([], |row| {
                Ok((row.get(0)?, row.get(1)?, row.get(2)?, row.get(3)?))
            })
            .expect("foreign_key_check should query")
            .collect::<Result<Vec<_>, _>>()
            .expect("foreign_key_check rows should collect")
    }

    #[test]
    fn opens_in_memory_database_with_required_pragmas() {
        let connection = open_in_memory_database().expect("database should open");

        let foreign_keys: i64 = connection
            .pragma_query_value(None, "foreign_keys", |row| row.get(0))
            .expect("foreign_keys pragma should be readable");
        let synchronous: i64 = connection
            .pragma_query_value(None, "synchronous", |row| row.get(0))
            .expect("synchronous pragma should be readable");
        let user_version: i64 = connection
            .pragma_query_value(None, "user_version", |row| row.get(0))
            .expect("user_version pragma should be readable");

        assert_eq!(foreign_keys, 1);
        assert_eq!(synchronous, 1);
        assert_eq!(user_version, i64::from(CURRENT_SCHEMA_VERSION));
    }

    #[test]
    fn applies_schema_idempotently_without_fts() {
        let schema = SchemaDefinition::new(CURRENT_SCHEMA_VERSION, false);
        let connection =
            open_in_memory_database_with_schema(&schema).expect("database should open");

        schema
            .apply(&connection)
            .expect("schema should be idempotent");

        let heading_fts_exists: i64 = connection
            .query_row(
                "SELECT COUNT(*) FROM sqlite_master WHERE type = 'table' AND name = 'heading_fts'",
                [],
                |row| row.get(0),
            )
            .expect("sqlite_master should be queryable");

        assert_eq!(heading_fts_exists, 0);
    }

    #[test]
    fn fresh_schema_has_the_measured_explicit_index_set() {
        let schema = SchemaDefinition::new(CURRENT_SCHEMA_VERSION, false);
        let connection =
            open_in_memory_database_with_schema(&schema).expect("database should open");

        assert_eq!(
            explicit_index_names(&connection),
            expected_current_explicit_indexes()
        );
    }

    #[test]
    fn migrates_version_8_index_set_to_version_10() {
        let test_dir = TestDir::new("version-8-index-set");
        let db_path = test_dir.path().join("db.sqlite");

        {
            let connection = open_database(&db_path).expect("database should initialize");
            connection
                .execute_batch(
                    r#"
DROP INDEX idx_files_path_lower;
DROP INDEX idx_headings_title_lower;
CREATE INDEX idx_tags_heading ON tags(heading_id);
CREATE INDEX idx_keywords_heading ON keywords(heading_id);
CREATE INDEX idx_timestamp_repeaters_timestamp_id
    ON timestamp_repeaters(timestamp_id);
PRAGMA user_version = 8;
"#,
                )
                .expect("version-8 index set should seed");
        }

        let connection = open_database(&db_path).expect("version-8 database should migrate");
        let version = read_schema_version(&connection).expect("schema version should load");

        assert_eq!(version, CURRENT_SCHEMA_VERSION);
        assert_eq!(CURRENT_SCHEMA_VERSION, 10);
        assert_eq!(
            explicit_index_names(&connection),
            expected_current_explicit_indexes()
        );
        assert!(foreign_key_check_rows(&connection).is_empty());
    }

    #[test]
    fn measured_expression_indexes_match_production_query_shapes() {
        let schema = SchemaDefinition::new(CURRENT_SCHEMA_VERSION, false);
        let connection =
            open_in_memory_database_with_schema(&schema).expect("database should open");

        let mut file_statement = connection
            .prepare("EXPLAIN QUERY PLAN SELECT id FROM files WHERE LOWER(path) = LOWER(?1)")
            .expect("file plan should prepare");
        let file_plan = file_statement
            .query_map(["/tmp/example.org"], |row| row.get::<_, String>(3))
            .expect("file plan should query")
            .collect::<Result<Vec<_>, _>>()
            .expect("file plan should decode");
        assert!(file_plan
            .iter()
            .any(|detail| detail.contains("idx_files_path_lower")));

        let mut title_statement = connection
            .prepare("EXPLAIN QUERY PLAN SELECT id FROM headings WHERE LOWER(title) = LOWER(?1)")
            .expect("title plan should prepare");
        let title_plan = title_statement
            .query_map(["Example"], |row| row.get::<_, String>(3))
            .expect("title plan should query")
            .collect::<Result<Vec<_>, _>>()
            .expect("title plan should decode");
        assert!(title_plan
            .iter()
            .any(|detail| detail.contains("idx_headings_title_lower")));
    }

    #[test]
    fn opens_existing_database_at_current_schema_version() {
        let test_dir = TestDir::new("current-schema-version");
        let db_path = test_dir.path().join("db.sqlite");

        let connection = open_database(&db_path).expect("database should open");
        let initial_version = read_schema_version(&connection).expect("schema version should load");
        assert_eq!(initial_version, CURRENT_SCHEMA_VERSION);
        drop(connection);

        let reopened = open_database(&db_path).expect("database should reopen");
        let reopened_version =
            read_schema_version(&reopened).expect("schema version should load after reopen");
        assert_eq!(reopened_version, CURRENT_SCHEMA_VERSION);
    }

    #[test]
    fn opens_explicitly_unversioned_database_and_sets_current_schema_version() {
        let test_dir = TestDir::new("unversioned-schema-version");
        let db_path = test_dir.path().join("db.sqlite");

        let connection = Connection::open(&db_path).expect("seed database should open");
        connection
            .pragma_update(None, "user_version", 0_i64)
            .expect("user_version should seed");
        drop(connection);

        let opened =
            open_database(&db_path).expect("database should initialize from user_version 0");
        let version = read_schema_version(&opened).expect("schema version should load");
        assert_eq!(version, CURRENT_SCHEMA_VERSION);
    }

    #[test]
    fn opens_legacy_database_and_migrates_it_to_current_schema_version() {
        let test_dir = TestDir::new("legacy-schema-version");
        let db_path = test_dir.path().join("db.sqlite");

        let connection = Connection::open(&db_path).expect("legacy database should open");
        connection
            .execute_batch(
                r#"
PRAGMA user_version = 0;

CREATE TABLE files (
    id              INTEGER PRIMARY KEY,
    path            TEXT NOT NULL UNIQUE,
    mtime_ns        INTEGER NOT NULL,
    size            INTEGER NOT NULL,
    content_hash    TEXT,
    indexed_at      INTEGER
);

CREATE TABLE todo_keywords (
    file_id         INTEGER NOT NULL,
    keyword         TEXT NOT NULL,
    state_type      TEXT NOT NULL CHECK (state_type IN ('open', 'closed')),
    shortcut        TEXT CHECK (shortcut IS NULL OR length(shortcut) = 1),
    sequence_no     INTEGER NOT NULL,
    FOREIGN KEY (file_id)
        REFERENCES files(id)
        ON DELETE CASCADE,
    PRIMARY KEY (file_id, keyword)
);

INSERT INTO files (id, path, mtime_ns, size) VALUES (1, '/tmp/project.org', 1, 1);
INSERT INTO todo_keywords (file_id, keyword, state_type, shortcut, sequence_no)
VALUES (1, 'PLAN', 'open', 'p', 0);
"#,
            )
            .expect("legacy schema should seed");
        drop(connection);

        let opened = open_database(&db_path).expect("legacy database should migrate");
        let version = read_schema_version(&opened).expect("schema version should load");
        assert_eq!(version, CURRENT_SCHEMA_VERSION);

        let metadata_table_exists: i64 = opened
            .query_row(
                "SELECT COUNT(*) FROM sqlite_master WHERE type = 'table' AND name = 'db_metadata'",
                [],
                |row| row.get(0),
            )
            .expect("db_metadata table existence should load");
        assert_eq!(metadata_table_exists, 1);

        let provenance: (String, Option<String>, Option<i64>) = opened
            .query_row(
                "SELECT source_kind, source_keyword, source_line_number
                 FROM todo_keywords
                 WHERE file_id = 1 AND keyword = 'PLAN'",
                [],
                |row| Ok((row.get(0)?, row.get(1)?, row.get(2)?)),
            )
            .expect("migrated todo keyword provenance should remain queryable");
        assert_eq!(provenance, ("config_default".to_string(), None, None));
    }

    #[test]
    fn migrating_legacy_database_does_not_enable_body_text_capability_before_rebuild() {
        let test_dir = TestDir::new("legacy-body-text-capability");
        let db_path = test_dir.path().join("db.sqlite");

        let connection = Connection::open(&db_path).expect("legacy database should open");
        connection
            .execute_batch(
                r#"
PRAGMA user_version = 0;

CREATE TABLE files (
    id              INTEGER PRIMARY KEY,
    path            TEXT NOT NULL UNIQUE,
    mtime_ns        INTEGER NOT NULL,
    size            INTEGER NOT NULL,
    content_hash    TEXT,
    indexed_at      INTEGER
);

CREATE TABLE headings (
    id                  INTEGER PRIMARY KEY,
    file_id             INTEGER NOT NULL,
    parent_id           INTEGER,
    level               INTEGER NOT NULL,
    line_number         INTEGER,
    byte_start          INTEGER NOT NULL,
    byte_end            INTEGER NOT NULL,
    title               TEXT NOT NULL,
    title_raw           TEXT,
    todo_keyword        TEXT,
    todo_type           TEXT,
    priority            TEXT,
    scheduled_raw       TEXT,
    scheduled_ts        INTEGER,
    deadline_raw        TEXT,
    deadline_ts         INTEGER,
    closed_raw          TEXT,
    closed_ts           INTEGER,
    archivedp           INTEGER NOT NULL DEFAULT 0,
    footnote_section_p  INTEGER NOT NULL DEFAULT 0,
    all_tags_json       TEXT NOT NULL DEFAULT '[]'
);

CREATE TABLE heading_bodies (
    heading_id          INTEGER PRIMARY KEY,
    body_text           TEXT NOT NULL,
    body_byte_start     INTEGER,
    body_byte_end       INTEGER
);

INSERT INTO files (id, path, mtime_ns, size) VALUES (1, '/tmp/project.org', 1, 1);
INSERT INTO headings (id, file_id, parent_id, level, byte_start, byte_end, title, title_raw)
VALUES (1, 1, NULL, 0, 0, 10, '/tmp/project.org', '/tmp/project.org');
INSERT INTO heading_bodies (heading_id, body_text, body_byte_start, body_byte_end)
VALUES (1, 'legacy body text', 0, 10);
"#,
            )
            .expect("legacy schema should seed");
        drop(connection);

        let opened = open_database(&db_path).expect("legacy database should migrate");
        let capability_value: Option<String> = opened
            .query_row(
                "SELECT value FROM db_metadata WHERE key = ?1",
                [crate::db::DB_METADATA_BODY_TEXT_AVAILABLE_KEY],
                |row| row.get(0),
            )
            .optional()
            .expect("body-text capability should query cleanly");
        assert_eq!(capability_value, None);
    }

    #[test]
    fn rejects_databases_with_future_schema_versions() {
        let test_dir = TestDir::new("future-schema-version");
        let db_path = test_dir.path().join("db.sqlite");

        let connection = Connection::open(&db_path).expect("future database should open");
        connection
            .pragma_update(None, "user_version", i64::from(CURRENT_SCHEMA_VERSION + 1))
            .expect("future user_version should seed");
        drop(connection);

        let error = open_database(&db_path).expect_err("future schema version should fail closed");
        match error {
            DbError::UnsupportedFutureSchemaVersion {
                on_disk_version,
                supported_version,
                ..
            } => {
                assert_eq!(on_disk_version, CURRENT_SCHEMA_VERSION + 1);
                assert_eq!(supported_version, CURRENT_SCHEMA_VERSION);
            }
            other => panic!("expected UnsupportedFutureSchemaVersion, got {other}"),
        }
    }

    #[test]
    fn does_not_advance_schema_version_when_initialization_fails() {
        let mut connection =
            Connection::open_in_memory().expect("broken legacy database should open");
        connection
            .execute_batch(
                r#"
PRAGMA user_version = 0;

CREATE TABLE todo_keywords (
    file_id         INTEGER NOT NULL,
    keyword         TEXT NOT NULL,
    state_type      TEXT NOT NULL CHECK (state_type IN ('open', 'closed'))
);
"#,
            )
            .expect("broken legacy schema should seed");

        let error = initialize_database(
            &mut connection,
            ":memory:",
            &SchemaDefinition::new(CURRENT_SCHEMA_VERSION, false),
        )
        .expect_err("broken legacy schema should fail initialization");
        assert!(
            matches!(error, DbError::Initialize { .. }),
            "expected initialize error, got {error}"
        );

        let user_version = read_schema_version(&connection).expect("schema version should load");
        assert_eq!(user_version, 0);

        let todo_keywords_exists: i64 = connection
            .query_row(
                "SELECT COUNT(*) FROM sqlite_master WHERE type = 'table' AND name = 'todo_keywords'",
                [],
                |row| row.get(0),
            )
            .expect("todo_keywords existence should load");
        let legacy_todo_keywords_exists: i64 = connection
            .query_row(
                "SELECT COUNT(*) FROM sqlite_master WHERE type = 'table' AND name = 'todo_keywords_legacy'",
                [],
                |row| row.get(0),
            )
            .expect("legacy todo_keywords existence should load");
        assert_eq!(todo_keywords_exists, 1);
        assert_eq!(legacy_todo_keywords_exists, 0);
    }

    #[test]
    fn migrates_legacy_todo_keywords_table_to_store_provenance() {
        let schema = SchemaDefinition::new(CURRENT_SCHEMA_VERSION, false);
        let connection = Connection::open_in_memory().expect("legacy database should open");
        connection
            .execute_batch(
                r#"
CREATE TABLE files (
    id              INTEGER PRIMARY KEY,
    path            TEXT NOT NULL UNIQUE,
    mtime_ns        INTEGER NOT NULL,
    size            INTEGER NOT NULL,
    content_hash    TEXT,
    indexed_at      INTEGER
);

CREATE TABLE todo_keywords (
    file_id         INTEGER NOT NULL,
    keyword         TEXT NOT NULL,
    state_type      TEXT NOT NULL CHECK (state_type IN ('open', 'closed')),
    shortcut        TEXT CHECK (shortcut IS NULL OR length(shortcut) = 1),
    sequence_no     INTEGER NOT NULL,
    FOREIGN KEY (file_id)
        REFERENCES files(id)
        ON DELETE CASCADE,
    PRIMARY KEY (file_id, keyword)
);

INSERT INTO files (id, path, mtime_ns, size) VALUES (1, '/tmp/project.org', 1, 1);
INSERT INTO todo_keywords (file_id, keyword, state_type, shortcut, sequence_no)
VALUES (1, 'PLAN', 'open', 'p', 0);
"#,
            )
            .expect("legacy schema should seed");

        schema
            .apply(&connection)
            .expect("schema migration should succeed");

        let columns: Vec<String> = {
            let mut statement = connection
                .prepare("PRAGMA table_info(todo_keywords)")
                .expect("todo_keywords pragma should prepare");
            statement
                .query_map([], |row| row.get(1))
                .expect("todo_keywords pragma should query")
                .collect::<Result<Vec<_>, _>>()
                .expect("todo_keywords columns should collect")
        };
        assert!(columns.iter().any(|column| column == "source_kind"));
        assert!(columns.iter().any(|column| column == "source_keyword"));
        assert!(columns.iter().any(|column| column == "source_line_number"));

        let provenance: (String, Option<String>, Option<i64>) = connection
            .query_row(
                "SELECT source_kind, source_keyword, source_line_number
                 FROM todo_keywords
                 WHERE file_id = 1 AND keyword = 'PLAN'",
                [],
                |row| Ok((row.get(0)?, row.get(1)?, row.get(2)?)),
            )
            .expect("todo keyword provenance should be queryable");

        assert_eq!(provenance.0, "config_default");
        assert_eq!(provenance.1, None);
        assert_eq!(provenance.2, None);
    }

    #[test]
    fn applies_schema_with_fts_when_supported() {
        let probe = Connection::open_in_memory().expect("probe connection should open");
        if !sqlite_supports_fts5(&probe).expect("fts5 probe should run") {
            return;
        }

        let schema = SchemaDefinition::new(CURRENT_SCHEMA_VERSION, true);
        let connection =
            open_in_memory_database_with_schema(&schema).expect("database should open");

        let heading_fts_exists: i64 = connection
            .query_row(
                "SELECT COUNT(*) FROM sqlite_master WHERE type = 'table' AND name = 'heading_fts'",
                [],
                |row| row.get(0),
            )
            .expect("sqlite_master should be queryable");

        assert_eq!(heading_fts_exists, 1);
    }

    #[test]
    fn sqlite_fts5_probe_leaves_no_temp_schema_artifacts() {
        let connection = open_in_memory_database_with_schema(&SchemaDefinition::new(
            CURRENT_SCHEMA_VERSION,
            false,
        ))
        .expect("database should open");

        let supports_fts5 = sqlite_supports_fts5(&connection).expect("fts5 probe should run");
        let temp_artifacts: i64 = connection
            .query_row(
                "SELECT COUNT(*) FROM sqlite_temp_master WHERE name LIKE 'org_files_db_fts5_probe_%'",
                [],
                |row| row.get(0),
            )
            .expect("temp schema should be queryable");

        assert_eq!(temp_artifacts, 0);
        if !supports_fts5 {
            return;
        }

        assert!(supports_fts5);
    }

    #[test]
    fn sqlite_fts5_probe_works_inside_active_transaction() {
        let mut connection = open_in_memory_database_with_schema(&SchemaDefinition::new(
            CURRENT_SCHEMA_VERSION,
            false,
        ))
        .expect("database should open");
        let tx = connection.transaction().expect("transaction should start");

        let supports_fts5 = sqlite_supports_fts5(&tx).expect("fts5 probe should run");
        let temp_artifacts: i64 = tx
            .query_row(
                "SELECT COUNT(*) FROM sqlite_temp_master WHERE name LIKE 'org_files_db_fts5_probe_%'",
                [],
                |row| row.get(0),
            )
            .expect("temp schema should be queryable");

        assert_eq!(temp_artifacts, 0);
        if !supports_fts5 {
            return;
        }

        assert!(supports_fts5);
    }

    #[test]
    fn timestamp_repeaters_schema_tracks_explicit_repeater_warning_columns() {
        let connection = open_in_memory_database().expect("database should open");

        let timestamps_columns: Vec<String> = {
            let mut statement = connection
                .prepare("PRAGMA table_info(timestamps)")
                .expect("timestamps pragma should prepare");
            statement
                .query_map([], |row| row.get(1))
                .expect("timestamps pragma should query")
                .collect::<Result<Vec<_>, _>>()
                .expect("timestamps columns should collect")
        };
        assert!(
            !timestamps_columns
                .iter()
                .any(|column| column == "has_repeater"),
            "timestamps table should not expose has_repeater"
        );

        let modifier_columns: Vec<String> = {
            let mut statement = connection
                .prepare("PRAGMA table_info(timestamp_repeaters)")
                .expect("timestamp_repeaters pragma should prepare");
            statement
                .query_map([], |row| row.get(1))
                .expect("timestamp_repeaters pragma should query")
                .collect::<Result<Vec<_>, _>>()
                .expect("timestamp_repeaters columns should collect")
        };
        assert!(modifier_columns
            .iter()
            .any(|column| column == "repeater_type"));
        assert!(modifier_columns
            .iter()
            .any(|column| column == "repeater_value"));
        assert!(modifier_columns
            .iter()
            .any(|column| column == "repeater_unit"));
        assert!(modifier_columns
            .iter()
            .any(|column| column == "repeater_deadline_value"));
        assert!(modifier_columns
            .iter()
            .any(|column| column == "repeater_deadline_unit"));
        assert!(modifier_columns
            .iter()
            .any(|column| column == "warning_type"));
        assert!(modifier_columns
            .iter()
            .any(|column| column == "warning_value"));
        assert!(modifier_columns
            .iter()
            .any(|column| column == "warning_unit"));
    }

    #[test]
    fn timestamp_repeaters_constraints_reject_invalid_repeater_warning_shapes() {
        let connection = open_in_memory_database().expect("database should open");

        connection
            .execute(
                "INSERT INTO files (id, path, mtime_ns, size) VALUES (?1, ?2, ?3, ?4)",
                (1_i64, "/tmp/example.org", 10_i64, 20_i64),
            )
            .expect("file insert should succeed");
        connection
            .execute(
                "INSERT INTO headings
                 (id, file_id, parent_id, level, byte_start, byte_end, title, title_raw)
                 VALUES (?1, ?2, ?3, ?4, ?5, ?6, ?7, ?8)",
                (
                    1_i64,
                    1_i64,
                    Option::<i64>::None,
                    0_i64,
                    -1_i64,
                    20_i64,
                    "/tmp/example.org",
                    "/tmp/example.org",
                ),
            )
            .expect("heading insert should succeed");
        connection
            .execute(
                "INSERT INTO timestamps
                 (id, heading_id, role, raw_value, byte_start, byte_end)
                 VALUES (?1, ?2, ?3, ?4, ?5, ?6)",
                (1_i64, 1_i64, "scheduled", "<2024-11-20 Wed>", 0_i64, 16_i64),
            )
            .expect("timestamp insert should succeed");

        let deadline_without_repeater = connection
            .execute(
                "INSERT INTO timestamp_repeaters
                 (timestamp_id, repeater_deadline_value, repeater_deadline_unit, warning_type, warning_value, warning_unit)
                 VALUES (?1, ?2, ?3, ?4, ?5, ?6)",
                (1_i64, 2_i64, "day", "all", 5_i64, "day"),
            )
            .expect_err("repeater deadline should require repeater columns");
        assert!(deadline_without_repeater.to_string().contains("CHECK"));

        connection
            .execute(
                "INSERT INTO timestamp_repeaters
                 (timestamp_id, repeater_type, repeater_value, repeater_unit,
                  repeater_deadline_value, repeater_deadline_unit,
                  warning_type, warning_value, warning_unit)
                 VALUES (?1, ?2, ?3, ?4, ?5, ?6, ?7, ?8, ?9)",
                (
                    1_i64, "catch_up", 1_i64, "month", 2_i64, "day", "all", 5_i64, "day",
                ),
            )
            .expect("combined repeater and warning row should insert");

        let duplicate_timestamp = connection
            .execute(
                "INSERT INTO timestamp_repeaters
                 (timestamp_id, warning_type, warning_value, warning_unit)
                 VALUES (?1, ?2, ?3, ?4)",
                (1_i64, "first", 2_i64, "week"),
            )
            .expect_err("duplicate timestamp row should fail");
        assert!(duplicate_timestamp.to_string().contains("UNIQUE"));

        let empty_row = connection
            .execute(
                "INSERT INTO timestamp_repeaters (timestamp_id) VALUES (?1)",
                (2_i64,),
            )
            .expect_err("timestamp repeater row should require repeater or warning data");
        assert!(empty_row.to_string().contains("CHECK"));
    }

    #[test]
    fn properties_schema_tracks_append_and_allows_duplicate_direct_rows() {
        let connection = open_in_memory_database().expect("database should open");

        let property_columns: Vec<String> = {
            let mut statement = connection
                .prepare("PRAGMA table_info(properties)")
                .expect("properties pragma should prepare");
            statement
                .query_map([], |row| row.get(1))
                .expect("properties pragma should query")
                .collect::<Result<Vec<_>, _>>()
                .expect("properties columns should collect")
        };
        assert!(property_columns.iter().any(|column| column == "append"));
        assert!(!property_columns.iter().any(|column| column == "inherited"));

        connection
            .execute(
                "INSERT INTO files (id, path, mtime_ns, size) VALUES (?1, ?2, ?3, ?4)",
                (1_i64, "/tmp/example.org", 10_i64, 20_i64),
            )
            .expect("file insert should succeed");
        connection
            .execute(
                "INSERT INTO headings
                 (id, file_id, parent_id, level, byte_start, byte_end, title, title_raw)
                 VALUES (?1, ?2, ?3, ?4, ?5, ?6, ?7, ?8)",
                (
                    1_i64,
                    1_i64,
                    Option::<i64>::None,
                    0_i64,
                    -1_i64,
                    20_i64,
                    "/tmp/example.org",
                    "/tmp/example.org",
                ),
            )
            .expect("heading insert should succeed");

        connection
            .execute(
                "INSERT INTO properties (heading_id, key, value, source, append, line_number)
                 VALUES (?1, ?2, ?3, ?4, ?5, ?6)",
                (1_i64, "OWNER", "Alice", "property_drawer", 0_i64, 2_i64),
            )
            .expect("first property insert should succeed");
        connection
            .execute(
                "INSERT INTO properties (heading_id, key, value, source, append, line_number)
                 VALUES (?1, ?2, ?3, ?4, ?5, ?6)",
                (1_i64, "OWNER", "Bob", "property_drawer", 0_i64, 3_i64),
            )
            .expect("duplicate direct property insert should succeed");

        let property_rows: Vec<(String, String, i64)> = {
            let mut statement = connection
                .prepare(
                    "SELECT key, value, append
                     FROM properties
                     ORDER BY line_number, id",
                )
                .expect("properties select should prepare");
            statement
                .query_map([], |row| Ok((row.get(0)?, row.get(1)?, row.get(2)?)))
                .expect("properties select should query")
                .collect::<Result<Vec<_>, _>>()
                .expect("properties select should collect")
        };
        assert_eq!(
            property_rows,
            vec![
                ("OWNER".to_string(), "Alice".to_string(), 0),
                ("OWNER".to_string(), "Bob".to_string(), 0),
            ]
        );
    }

    #[test]
    fn enforces_unique_file_paths() {
        let schema = SchemaDefinition::new(CURRENT_SCHEMA_VERSION, false);
        let connection =
            open_in_memory_database_with_schema(&schema).expect("database should open");

        connection
            .execute(
                "INSERT INTO files (path, mtime_ns, size, content_hash, indexed_at)
                 VALUES (?1, ?2, ?3, ?4, ?5)",
                (
                    "/tmp/example.org",
                    10_i64,
                    20_i64,
                    Option::<String>::None,
                    Option::<i64>::None,
                ),
            )
            .expect("first file insert should succeed");

        let error = connection
            .execute(
                "INSERT INTO files (path, mtime_ns, size, content_hash, indexed_at)
                 VALUES (?1, ?2, ?3, ?4, ?5)",
                (
                    "/tmp/example.org",
                    11_i64,
                    21_i64,
                    Option::<String>::None,
                    Option::<i64>::None,
                ),
            )
            .expect_err("duplicate file path should fail");

        assert!(error.to_string().contains("UNIQUE"));
    }

    #[test]
    fn enforces_level_zero_constraints() {
        let schema = SchemaDefinition::new(CURRENT_SCHEMA_VERSION, false);
        let connection =
            open_in_memory_database_with_schema(&schema).expect("database should open");

        connection
            .execute(
                "INSERT INTO files (id, path, mtime_ns, size) VALUES (?1, ?2, ?3, ?4)",
                (1_i64, "/tmp/example.org", 10_i64, 20_i64),
            )
            .expect("file insert should succeed");

        connection
            .execute(
                "INSERT INTO headings
                 (id, file_id, parent_id, level, byte_start, byte_end, title, title_raw)
                 VALUES (?1, ?2, ?3, ?4, ?5, ?6, ?7, ?8)",
                (
                    1_i64,
                    1_i64,
                    Option::<i64>::None,
                    0_i64,
                    0_i64,
                    20_i64,
                    "/tmp/example.org",
                    "/tmp/example.org",
                ),
            )
            .expect("level 0 heading should insert");

        let duplicate_level_zero = connection
            .execute(
                "INSERT INTO headings
                 (file_id, parent_id, level, byte_start, byte_end, title, title_raw)
                 VALUES (?1, ?2, ?3, ?4, ?5, ?6, ?7)",
                (
                    1_i64,
                    Option::<i64>::None,
                    0_i64,
                    21_i64,
                    30_i64,
                    "duplicate",
                    "duplicate",
                ),
            )
            .expect_err("duplicate level 0 heading should fail");
        assert!(duplicate_level_zero.to_string().contains("UNIQUE"));

        let regular_without_parent = connection
            .execute(
                "INSERT INTO headings
                 (file_id, parent_id, level, byte_start, byte_end, title, title_raw)
                 VALUES (?1, ?2, ?3, ?4, ?5, ?6, ?7)",
                (
                    1_i64,
                    Option::<i64>::None,
                    1_i64,
                    31_i64,
                    40_i64,
                    "child",
                    "child",
                ),
            )
            .expect_err("regular heading without parent should fail");
        assert!(regular_without_parent.to_string().contains("CHECK"));
    }

    #[test]
    fn cascades_source_rows_when_file_is_deleted() {
        let schema = SchemaDefinition::new(CURRENT_SCHEMA_VERSION, false);
        let connection =
            open_in_memory_database_with_schema(&schema).expect("database should open");

        insert_fixture_graph(&connection);

        connection
            .execute("DELETE FROM files WHERE id = 1", [])
            .expect("file delete should succeed");

        let source_file_count: i64 = connection
            .query_row("SELECT COUNT(*) FROM files WHERE id = 1", [], |row| {
                row.get(0)
            })
            .expect("source file count should succeed");
        let remaining_file_count: i64 = connection
            .query_row("SELECT COUNT(*) FROM files", [], |row| row.get(0))
            .expect("remaining file count should succeed");
        let source_heading_count: i64 = connection
            .query_row(
                "SELECT COUNT(*) FROM headings WHERE file_id = 1",
                [],
                |row| row.get(0),
            )
            .expect("source heading count should succeed");
        let source_todo_keyword_count: i64 = connection
            .query_row(
                "SELECT COUNT(*) FROM todo_keywords WHERE file_id = 1",
                [],
                |row| row.get(0),
            )
            .expect("source todo keyword count should succeed");
        let source_link_count: i64 = connection
            .query_row("SELECT COUNT(*) FROM links WHERE file_id = 1", [], |row| {
                row.get(0)
            })
            .expect("source link count should succeed");
        let source_outline_count: i64 = connection
            .query_row(
                "SELECT COUNT(*) FROM outline_path WHERE file_id = 1",
                [],
                |row| row.get(0),
            )
            .expect("source outline path count should succeed");
        let total_keyword_count: i64 = connection
            .query_row("SELECT COUNT(*) FROM keywords", [], |row| row.get(0))
            .expect("keyword count should succeed");
        let total_property_count: i64 = connection
            .query_row("SELECT COUNT(*) FROM properties", [], |row| row.get(0))
            .expect("property count should succeed");
        let total_tag_count: i64 = connection
            .query_row("SELECT COUNT(*) FROM tags", [], |row| row.get(0))
            .expect("tag count should succeed");
        let total_body_count: i64 = connection
            .query_row("SELECT COUNT(*) FROM heading_bodies", [], |row| row.get(0))
            .expect("heading body count should succeed");

        assert_eq!(source_file_count, 0);
        assert_eq!(remaining_file_count, 1);
        assert_eq!(source_heading_count, 0);
        assert_eq!(source_todo_keyword_count, 0);
        assert_eq!(source_link_count, 0);
        assert_eq!(source_outline_count, 0);
        assert_eq!(total_keyword_count, 0);
        assert_eq!(total_property_count, 0);
        assert_eq!(total_tag_count, 0);
        assert_eq!(total_body_count, 0);
    }

    #[test]
    fn deleting_deferred_targets_keeps_source_links() {
        let schema = SchemaDefinition::new(CURRENT_SCHEMA_VERSION, false);
        let connection =
            open_in_memory_database_with_schema(&schema).expect("database should open");

        insert_fixture_graph(&connection);

        connection
            .execute("DELETE FROM headings WHERE id = 3", [])
            .expect("resolved target heading should delete");

        let (source_link_count, target_heading_id, resolution_status): (
            i64,
            Option<i64>,
            Option<String>,
        ) = connection
            .query_row(
                "SELECT COUNT(*), MIN(target_heading_id), MIN(resolution_status)
                 FROM links
                 WHERE id = 1",
                [],
                |row| Ok((row.get(0)?, row.get(1)?, row.get(2)?)),
            )
            .expect("source link should remain queryable");

        assert_eq!(source_link_count, 1);
        assert_eq!(target_heading_id, None);
        assert_eq!(resolution_status.as_deref(), Some("resolved"));

        connection
            .execute("DELETE FROM files WHERE id = 2", [])
            .expect("resolved target file should delete");

        let (target_file_id, source_link_count_after_file_delete): (Option<i64>, i64) = connection
            .query_row(
                "SELECT target_file_id, COUNT(*) FROM links WHERE id = 1",
                [],
                |row| Ok((row.get(0)?, row.get(1)?)),
            )
            .expect("source link should remain queryable");

        assert_eq!(target_file_id, None);
        assert_eq!(source_link_count_after_file_delete, 1);
    }

    #[test]
    fn supports_per_file_rebuild_while_keeping_file_row() {
        let schema = SchemaDefinition::new(CURRENT_SCHEMA_VERSION, false);
        let connection =
            open_in_memory_database_with_schema(&schema).expect("database should open");

        insert_fixture_graph(&connection);

        connection
            .execute("DELETE FROM todo_keywords WHERE file_id = 1", [])
            .expect("todo keyword delete should succeed");
        connection
            .execute("DELETE FROM headings WHERE file_id = 1", [])
            .expect("heading delete should succeed");

        let file_count: i64 = connection
            .query_row("SELECT COUNT(*) FROM files WHERE id = 1", [], |row| {
                row.get(0)
            })
            .expect("file count should succeed");
        assert_eq!(file_count, 1);

        let rebuilt_heading_count: i64 = connection
            .query_row(
                "SELECT COUNT(*) FROM headings WHERE file_id = 1",
                [],
                |row| row.get(0),
            )
            .expect("rebuilt heading count should succeed");
        let rebuilt_todo_keyword_count: i64 = connection
            .query_row(
                "SELECT COUNT(*) FROM todo_keywords WHERE file_id = 1",
                [],
                |row| row.get(0),
            )
            .expect("rebuilt todo keyword count should succeed");
        let rebuilt_link_count: i64 = connection
            .query_row("SELECT COUNT(*) FROM links WHERE file_id = 1", [], |row| {
                row.get(0)
            })
            .expect("rebuilt link count should succeed");

        assert_eq!(rebuilt_heading_count, 0);
        assert_eq!(rebuilt_todo_keyword_count, 0);
        assert_eq!(rebuilt_link_count, 0);

        connection
            .execute(
                "INSERT INTO headings
                 (id, file_id, parent_id, level, byte_start, byte_end, title, title_raw)
                 VALUES (?1, ?2, ?3, ?4, ?5, ?6, ?7, ?8)",
                (
                    10_i64,
                    1_i64,
                    Option::<i64>::None,
                    0_i64,
                    0_i64,
                    50_i64,
                    "/tmp/source.org",
                    "/tmp/source.org",
                ),
            )
            .expect("new level 0 heading should insert");
        connection
            .execute(
                "INSERT INTO headings
                 (id, file_id, parent_id, level, byte_start, byte_end, title, title_raw)
                 VALUES (?1, ?2, ?3, ?4, ?5, ?6, ?7, ?8)",
                (
                    11_i64,
                    1_i64,
                    Some(10_i64),
                    1_i64,
                    1_i64,
                    10_i64,
                    "Fresh",
                    "Fresh",
                ),
            )
            .expect("new child heading should insert");
    }

    #[test]
    fn heading_bodies_exist_without_fts() {
        let schema = SchemaDefinition::new(CURRENT_SCHEMA_VERSION, false);
        let connection =
            open_in_memory_database_with_schema(&schema).expect("database should open");

        connection
            .execute(
                "INSERT INTO files (id, path, mtime_ns, size) VALUES (?1, ?2, ?3, ?4)",
                (1_i64, "/tmp/example.org", 10_i64, 20_i64),
            )
            .expect("file insert should succeed");
        connection
            .execute(
                "INSERT INTO headings
                 (id, file_id, parent_id, level, byte_start, byte_end, title, title_raw)
                 VALUES (?1, ?2, ?3, ?4, ?5, ?6, ?7, ?8)",
                (
                    1_i64,
                    1_i64,
                    Option::<i64>::None,
                    0_i64,
                    0_i64,
                    20_i64,
                    "/tmp/example.org",
                    "/tmp/example.org",
                ),
            )
            .expect("heading insert should succeed");
        connection
            .execute(
                "INSERT INTO heading_bodies (heading_id, body_text, body_byte_start, body_byte_end)
                 VALUES (?1, ?2, ?3, ?4)",
                (1_i64, "preamble", 0_i64, 8_i64),
            )
            .expect("body insert should succeed");

        let body_text: String = connection
            .query_row(
                "SELECT body_text FROM heading_bodies WHERE heading_id = 1",
                [],
                |row| row.get(0),
            )
            .expect("body should be queryable");

        assert_eq!(body_text, "preamble");
    }

    #[test]
    fn supports_title_only_fts_rows() {
        let probe = Connection::open_in_memory().expect("probe connection should open");
        if !sqlite_supports_fts5(&probe).expect("fts5 probe should run") {
            return;
        }

        let schema = SchemaDefinition::new(CURRENT_SCHEMA_VERSION, true);
        let connection =
            open_in_memory_database_with_schema(&schema).expect("database should open");

        connection
            .execute(
                "INSERT INTO heading_fts (rowid, title, body) VALUES (?1, ?2, ?3)",
                (1_i64, "Inbox", ""),
            )
            .expect("title-only fts row should insert");

        let match_count: i64 = connection
            .query_row(
                "SELECT COUNT(*) FROM heading_fts WHERE heading_fts MATCH 'Inbox'",
                [],
                |row| row.get(0),
            )
            .expect("fts query should succeed");
        let stored_row: (Option<String>, Option<String>) = connection
            .query_row(
                "SELECT title, body FROM heading_fts WHERE rowid = 1",
                [],
                |row| Ok((row.get(0)?, row.get(1)?)),
            )
            .expect("contentless row should be readable as null payload");

        assert_eq!(match_count, 1);
        assert_eq!(stored_row, (None, None));
    }

    #[test]
    fn contentless_fts_schema_is_rendered_when_enabled() {
        let probe = Connection::open_in_memory().expect("probe connection should open");
        if !sqlite_supports_fts5(&probe).expect("fts5 probe should run") {
            return;
        }

        let schema = SchemaDefinition::new(CURRENT_SCHEMA_VERSION, true);
        let connection =
            open_in_memory_database_with_schema(&schema).expect("database should open");

        let sql: String = connection
            .query_row(
                "SELECT sql FROM sqlite_master WHERE type = 'table' AND name = 'heading_fts'",
                [],
                |row| row.get(0),
            )
            .expect("heading_fts sql should load");

        assert!(sql.contains("content = ''"));
        assert!(sql.contains("tokenize = 'unicode61'"));
    }

    #[test]
    fn file_backed_database_uses_wal_mode() {
        let test_dir = TestDir::new("wal");
        let database_path = test_dir.path().join("org-files-db.sqlite");

        let connection = open_database(&database_path).expect("database should open");
        let journal_mode: String = connection
            .pragma_query_value(None, "journal_mode", |row| row.get(0))
            .expect("journal_mode pragma should be readable");

        assert_eq!(journal_mode.to_ascii_lowercase(), "wal");
    }

    #[test]
    fn open_database_upgrades_legacy_timestamp_repeaters_table() {
        let test_dir = TestDir::new("legacy-repeaters");
        let database_path = test_dir.path().join("org-files-db.sqlite");

        open_database(&database_path).expect("database should initialize");

        {
            let legacy = Connection::open(&database_path).expect("legacy database should open");
            legacy
                .execute_batch(
                    r#"
INSERT INTO files (id, path, mtime_ns, size) VALUES (1, '/tmp/example.org', 10, 20);
INSERT INTO headings
    (id, file_id, parent_id, level, byte_start, byte_end, title, title_raw)
VALUES
    (1, 1, NULL, 0, -1, 20, '/tmp/example.org', '/tmp/example.org');
INSERT INTO timestamps
    (id, heading_id, role, raw_value, byte_start, byte_end)
VALUES
    (1, 1, 'scheduled', '<2024-11-20 Wed +1w/2d>', 0, 22);

DROP TABLE timestamp_repeaters;

CREATE TABLE timestamp_repeaters (
    id INTEGER PRIMARY KEY,
    timestamp_id INTEGER NOT NULL,
    type TEXT,
    value INTEGER,
    unit TEXT,
    deadline_value INTEGER,
    deadline_unit TEXT
);

INSERT INTO timestamp_repeaters
    (id, timestamp_id, type, value, unit, deadline_value, deadline_unit)
VALUES
    (1, 1, 'cumulate', 1, 'week', 2, 'day');
"#,
                )
                .expect("legacy schema should initialize");
        }

        let connection = open_database(&database_path).expect("database should upgrade");

        let columns: Vec<String> = {
            let mut statement = connection
                .prepare("PRAGMA table_info(timestamp_repeaters)")
                .expect("timestamp_repeaters pragma should prepare");
            statement
                .query_map([], |row| row.get(1))
                .expect("timestamp_repeaters pragma should query")
                .collect::<Result<Vec<_>, _>>()
                .expect("timestamp_repeaters columns should collect")
        };
        assert!(columns.iter().any(|column| column == "repeater_type"));
        assert!(columns
            .iter()
            .any(|column| column == "repeater_deadline_value"));
        assert!(columns
            .iter()
            .any(|column| column == "repeater_deadline_unit"));
        assert!(columns.iter().any(|column| column == "warning_type"));

        let migrated_row: MigratedLegacyRepeaterRow = connection
            .query_row(
                "SELECT repeater_type, repeater_deadline_value, repeater_deadline_unit,
                        warning_type, warning_value, warning_unit
                 FROM timestamp_repeaters
                 WHERE id = 1",
                [],
                |row| {
                    Ok((
                        row.get(0)?,
                        row.get(1)?,
                        row.get(2)?,
                        row.get(3)?,
                        row.get(4)?,
                        row.get(5)?,
                    ))
                },
            )
            .expect("migrated row should be queryable");
        assert_eq!(
            migrated_row,
            (
                Some("cumulate".to_string()),
                Some(2_i64),
                Some("day".to_string()),
                None,
                None,
                None,
            )
        );
    }

    #[test]
    fn open_database_upgrades_generic_modifier_rows_to_explicit_timestamp_row() {
        let test_dir = TestDir::new("generic-repeaters");
        let database_path = test_dir.path().join("org-files-db.sqlite");

        open_database(&database_path).expect("database should initialize");

        {
            let legacy = Connection::open(&database_path).expect("legacy database should open");
            legacy
                .execute_batch(
                    r#"
INSERT INTO files (id, path, mtime_ns, size) VALUES (1, '/tmp/example.org', 10, 20);
INSERT INTO headings
    (id, file_id, parent_id, level, byte_start, byte_end, title, title_raw)
VALUES
    (1, 1, NULL, 0, -1, 20, '/tmp/example.org', '/tmp/example.org');
INSERT INTO timestamps
    (id, heading_id, role, raw_value, byte_start, byte_end)
VALUES
    (1, 1, 'scheduled', '<2024-11-20 Wed ++1m/2d -5d>', 0, 28);

DROP TABLE timestamp_repeaters;

CREATE TABLE timestamp_repeaters (
    id INTEGER PRIMARY KEY,
    timestamp_id INTEGER NOT NULL,
    kind TEXT NOT NULL,
    type TEXT NOT NULL,
    value INTEGER NOT NULL,
    unit TEXT NOT NULL,
    repeater_deadline_value INTEGER,
    repeater_deadline_unit TEXT
);

INSERT INTO timestamp_repeaters
    (id, timestamp_id, kind, type, value, unit, repeater_deadline_value, repeater_deadline_unit)
VALUES
    (1, 1, 'repeater', 'catch_up', 1, 'month', 2, 'day'),
    (2, 1, 'warning', 'all', 5, 'day', NULL, NULL);
"#,
                )
                .expect("generic schema should initialize");
        }

        let connection = open_database(&database_path).expect("database should upgrade");

        let migrated_rows: Vec<MigratedExplicitRepeaterRow> = {
            let mut statement = connection
                .prepare(
                    "SELECT timestamp_id, repeater_type, repeater_value, repeater_unit,
                            repeater_deadline_value, repeater_deadline_unit,
                            warning_type, warning_value, warning_unit
                     FROM timestamp_repeaters",
                )
                .expect("query should prepare");
            statement
                .query_map([], |row| {
                    Ok((
                        row.get(0)?,
                        row.get(1)?,
                        row.get(2)?,
                        row.get(3)?,
                        row.get(4)?,
                        row.get(5)?,
                        row.get(6)?,
                        row.get(7)?,
                        row.get(8)?,
                    ))
                })
                .expect("query should run")
                .collect::<Result<Vec<_>, _>>()
                .expect("rows should collect")
        };

        assert_eq!(
            migrated_rows,
            vec![(
                1_i64,
                Some("catch_up".to_string()),
                Some(1_i64),
                Some("month".to_string()),
                Some(2_i64),
                Some("day".to_string()),
                Some("all".to_string()),
                Some(5_i64),
                Some("day".to_string()),
            )]
        );
    }

    #[test]
    fn open_database_upgrades_legacy_properties_table() {
        let test_dir = TestDir::new("legacy-properties");
        let database_path = test_dir.path().join("org-files-db.sqlite");

        open_database(&database_path).expect("database should initialize");

        {
            let legacy = Connection::open(&database_path).expect("legacy database should open");
            legacy
                .execute_batch(
                    r#"
INSERT INTO files (id, path, mtime_ns, size) VALUES (1, '/tmp/example.org', 10, 20);
INSERT INTO headings
    (id, file_id, parent_id, level, byte_start, byte_end, title, title_raw)
VALUES
    (1, 1, NULL, 0, -1, 20, '/tmp/example.org', '/tmp/example.org');

DROP TABLE properties;

CREATE TABLE properties (
    id              INTEGER PRIMARY KEY,
    heading_id      INTEGER NOT NULL,
    key             TEXT NOT NULL,
    value           TEXT,
    source          TEXT NOT NULL CHECK (
                        source IN ('property_keyword', 'property_drawer', 'category_keyword')
                    ),
    inherited       INTEGER NOT NULL DEFAULT 0 CHECK (inherited IN (0, 1)),
    line_number     INTEGER,
    FOREIGN KEY (heading_id)
        REFERENCES headings(id)
        ON DELETE CASCADE,
    UNIQUE (heading_id, key, source, inherited)
);

INSERT INTO properties (id, heading_id, key, value, source, inherited, line_number)
VALUES
    (1, 1, 'CUSTOM_ID', 'legacy-id', 'property_drawer', 0, 2);
"#,
                )
                .expect("legacy properties schema should initialize");
        }

        let connection = open_database(&database_path).expect("database should upgrade");

        let columns: Vec<String> = {
            let mut statement = connection
                .prepare("PRAGMA table_info(properties)")
                .expect("properties pragma should prepare");
            statement
                .query_map([], |row| row.get(1))
                .expect("properties pragma should query")
                .collect::<Result<Vec<_>, _>>()
                .expect("properties columns should collect")
        };
        assert!(columns.iter().any(|column| column == "append"));
        assert!(!columns.iter().any(|column| column == "inherited"));

        let migrated_row: (String, Option<String>, String, i64, Option<i64>) = connection
            .query_row(
                "SELECT key, value, source, append, line_number
                 FROM properties",
                [],
                |row| {
                    Ok((
                        row.get(0)?,
                        row.get(1)?,
                        row.get(2)?,
                        row.get(3)?,
                        row.get(4)?,
                    ))
                },
            )
            .expect("migrated row should be queryable");
        assert_eq!(
            migrated_row,
            (
                "CUSTOM_ID".to_string(),
                Some("legacy-id".to_string()),
                "property_drawer".to_string(),
                0,
                Some(2),
            )
        );
    }

    #[test]
    fn open_database_upgrades_legacy_tags_table() {
        let test_dir = TestDir::new("legacy-tags");
        let database_path = test_dir.path().join("org-files-db.sqlite");

        open_database(&database_path).expect("database should initialize");

        {
            let legacy = Connection::open(&database_path).expect("legacy database should open");
            legacy
                .execute_batch(
                    r#"
INSERT INTO files (id, path, mtime_ns, size) VALUES (1, '/tmp/example.org', 10, 20);
INSERT INTO headings
    (id, file_id, parent_id, level, byte_start, byte_end, title, title_raw)
VALUES
    (1, 1, NULL, 0, -1, 20, '/tmp/example.org', '/tmp/example.org');

DROP TABLE tags;

CREATE TABLE tags (
    heading_id      INTEGER NOT NULL,
    tag             TEXT NOT NULL,
    inherited       INTEGER NOT NULL DEFAULT 0 CHECK (inherited IN (0, 1)),
    FOREIGN KEY (heading_id)
        REFERENCES headings(id)
        ON DELETE CASCADE,
    PRIMARY KEY (heading_id, tag, inherited)
);

INSERT INTO tags (heading_id, tag, inherited)
VALUES
    (1, 'alpha', 0),
    (1, 'alpha', 1),
    (1, 'beta', 0);
"#,
                )
                .expect("legacy tags schema should initialize");
        }

        let connection = open_database(&database_path).expect("database should upgrade");

        let columns: Vec<String> = {
            let mut statement = connection
                .prepare("PRAGMA table_info(tags)")
                .expect("tags pragma should prepare");
            statement
                .query_map([], |row| row.get(1))
                .expect("tags pragma should query")
                .collect::<Result<Vec<_>, _>>()
                .expect("tags columns should collect")
        };
        assert!(columns.iter().any(|column| column == "heading_id"));
        assert!(columns.iter().any(|column| column == "tag"));
        assert!(!columns.iter().any(|column| column == "inherited"));

        let tag_count: i64 = connection
            .query_row("SELECT COUNT(*) FROM tags", [], |row| row.get(0))
            .expect("tags count should be queryable");
        assert_eq!(tag_count, 2);

        let tag_rows: Vec<(i64, String)> = {
            let mut statement = connection
                .prepare("SELECT heading_id, tag FROM tags ORDER BY heading_id, tag")
                .expect("tags select should prepare");
            statement
                .query_map([], |row| Ok((row.get(0)?, row.get(1)?)))
                .expect("tags select should query")
                .collect::<Result<Vec<_>, _>>()
                .expect("migrated tags should collect")
        };
        assert_eq!(
            tag_rows,
            vec![(1_i64, "alpha".to_string()), (1_i64, "beta".to_string())]
        );
    }

    #[test]
    fn open_database_upgrades_legacy_links_table() {
        let test_dir = TestDir::new("legacy-links");
        let database_path = test_dir.path().join("org-files-db.sqlite");

        open_database(&database_path).expect("database should initialize");

        {
            let legacy = Connection::open(&database_path).expect("legacy database should open");
            legacy
                .execute_batch(
                    r#"
INSERT INTO files (id, path, mtime_ns, size) VALUES (1, '/tmp/example.org', 10, 20);
INSERT INTO headings
    (id, file_id, parent_id, level, byte_start, byte_end, title, title_raw)
VALUES
    (1, 1, NULL, 0, -1, 20, '/tmp/example.org', '/tmp/example.org');

DROP TABLE links;

CREATE TABLE links (
    id                  INTEGER PRIMARY KEY,
    file_id             INTEGER NOT NULL,
    heading_id          INTEGER NOT NULL,
    byte_start          INTEGER NOT NULL,
    byte_end            INTEGER NOT NULL,
    line_number         INTEGER,
    link_type           TEXT,
    target              TEXT NOT NULL,
    target_absolute     TEXT,
    raw_link            TEXT NOT NULL,
    description         TEXT,
    format              TEXT,
    search_option       TEXT,
    relation            TEXT,
    resolved_file_id    INTEGER,
    resolved_heading_id INTEGER,
    resolved            INTEGER NOT NULL DEFAULT 0,
    broken              INTEGER NOT NULL DEFAULT 0,
    diagnostic          TEXT
);

CREATE INDEX idx_links_heading ON links(heading_id);
CREATE INDEX idx_links_target ON links(target);
CREATE INDEX idx_links_resolved_file ON links(resolved_file_id);
CREATE INDEX idx_links_resolved_heading ON links(resolved_heading_id);

INSERT INTO links
    (id, file_id, heading_id, byte_start, byte_end, line_number, link_type, target,
     target_absolute, raw_link, description, format, search_option,
     resolved_file_id, resolved_heading_id, resolved, diagnostic)
VALUES
    (1, 1, 1, 4, 24, 3, 'file', 'notes.org', '/tmp/notes.org',
     '[[file:notes.org::42][Notes]]', 'Notes', 'bracket', '42', NULL, NULL, 1, 'legacy resolved');

PRAGMA user_version = 1;
"#,
                )
                .expect("legacy links schema should initialize");
        }

        let connection = open_database(&database_path).expect("database should upgrade");
        let schema_version = read_schema_version(&connection).expect("schema version should load");
        assert_eq!(schema_version, CURRENT_SCHEMA_VERSION);

        let columns: Vec<String> = {
            let mut statement = connection
                .prepare("PRAGMA table_info(links)")
                .expect("links pragma should prepare");
            statement
                .query_map([], |row| row.get(1))
                .expect("links pragma should query")
                .collect::<Result<Vec<_>, _>>()
                .expect("links columns should collect")
        };
        assert!(columns.iter().any(|column| column == "line"));
        assert!(columns.iter().any(|column| column == "source_context"));
        assert!(columns.iter().any(|column| column == "raw"));
        assert!(columns.iter().any(|column| column == "raw_target"));
        assert!(columns.iter().any(|column| column == "raw_description"));
        assert!(columns.iter().any(|column| column == "path"));
        assert!(columns.iter().any(|column| column == "path_absolute"));
        assert!(columns.iter().any(|column| column == "target_file_id"));
        assert!(columns.iter().any(|column| column == "target_heading_id"));
        assert!(columns.iter().any(|column| column == "resolution_status"));
        assert!(columns
            .iter()
            .any(|column| column == "resolution_diagnostic"));
        assert!(!columns.iter().any(|column| column == "line_number"));
        assert!(!columns.iter().any(|column| column == "raw_link"));
        assert!(!columns.iter().any(|column| column == "resolved_file_id"));
        assert!(columns.iter().any(|column| column == "link_type"));

        let migrated_row = connection
            .query_row(
                "SELECT line, source_context, format, raw, raw_target, raw_description,
                        link_type, path, search_option, path_absolute, target_file_id,
                        target_heading_id, target_custom_id, target_id,
                        resolution_status, resolution_diagnostic
                 FROM links
                 WHERE id = 1",
                [],
                |row| {
                    Ok((
                        row.get::<_, i64>(0)?,
                        row.get::<_, String>(1)?,
                        row.get::<_, String>(2)?,
                        row.get::<_, String>(3)?,
                        row.get::<_, String>(4)?,
                        row.get::<_, Option<String>>(5)?,
                        row.get::<_, String>(6)?,
                        row.get::<_, String>(7)?,
                        row.get::<_, Option<String>>(8)?,
                        row.get::<_, Option<String>>(9)?,
                        row.get::<_, Option<i64>>(10)?,
                        row.get::<_, Option<i64>>(11)?,
                        row.get::<_, Option<String>>(12)?,
                        row.get::<_, Option<String>>(13)?,
                        row.get::<_, Option<String>>(14)?,
                        row.get::<_, Option<String>>(15)?,
                    ))
                },
            )
            .expect("migrated link row should be queryable");
        let (
            line,
            source_context,
            format,
            raw,
            raw_target,
            raw_description,
            link_type,
            path,
            search_option,
            path_absolute,
            target_file_id,
            target_heading_id,
            target_custom_id,
            target_id,
            resolution_status,
            resolution_diagnostic,
        ) = migrated_row;
        assert_eq!(line, 3);
        assert_eq!(source_context, "normal");
        assert_eq!(format, "bracket");
        assert_eq!(raw, "[[file:notes.org::42][Notes]]");
        // Legacy migration reconstructs raw_target on a best-effort basis for typed links.
        assert_eq!(raw_target, "file:notes.org::42");
        assert_eq!(raw_description, Some("Notes".to_string()));
        assert_eq!(link_type, "file".to_string());
        assert_eq!(path, "notes.org");
        assert_eq!(search_option, Some("42".to_string()));
        assert_eq!(path_absolute, Some("/tmp/notes.org".to_string()));
        assert_eq!(target_file_id, None);
        assert_eq!(target_heading_id, None);
        assert_eq!(target_custom_id, None);
        assert_eq!(target_id, None);
        assert_eq!(resolution_status, Some("resolved".to_string()));
        assert_eq!(resolution_diagnostic, Some("legacy resolved".to_string()));

        let migrated_index_names: Vec<String> = {
            let mut statement = connection
                .prepare(
                    "SELECT name
                     FROM sqlite_master
                     WHERE type = 'index' AND tbl_name = 'links'
                     ORDER BY name",
                )
                .expect("links indexes query should prepare");
            statement
                .query_map([], |row| row.get(0))
                .expect("links indexes query should run")
                .collect::<Result<Vec<_>, _>>()
                .expect("links indexes should collect")
        };
        assert!(migrated_index_names.contains(&"idx_links_heading".to_string()));
        assert!(migrated_index_names.contains(&"idx_links_path".to_string()));
        assert!(migrated_index_names.contains(&"idx_links_target_file".to_string()));
        assert!(migrated_index_names.contains(&"idx_links_target_heading".to_string()));
    }

    #[test]
    fn open_database_upgrades_phase3_links_table_to_resolution_state_contract() {
        let test_dir = TestDir::new("phase3-links");
        let database_path = test_dir.path().join("org-files-db.sqlite");

        open_database(&database_path).expect("database should initialize");

        {
            let legacy = Connection::open(&database_path).expect("legacy database should open");
            legacy
                .execute_batch(
                    r#"
INSERT INTO files (id, path, mtime_ns, size) VALUES (1, '/tmp/example.org', 10, 20);
INSERT INTO headings
    (id, file_id, parent_id, level, byte_start, byte_end, title, title_raw)
VALUES
    (1, 1, NULL, 0, -1, 20, '/tmp/example.org', '/tmp/example.org');

DROP TABLE links;

CREATE TABLE links (
    id                  INTEGER PRIMARY KEY,
    file_id             INTEGER NOT NULL,
    heading_id          INTEGER NOT NULL,
    byte_start          INTEGER NOT NULL CHECK (byte_start >= 0),
    byte_end            INTEGER NOT NULL CHECK (byte_end >= byte_start),
    line                INTEGER NOT NULL CHECK (line > 0),
    source_context      TEXT NOT NULL,
    format              TEXT NOT NULL,
    raw                 TEXT NOT NULL,
    raw_target          TEXT NOT NULL,
    raw_description     TEXT,
    link_type           TEXT NOT NULL,
    path                TEXT NOT NULL,
    search_option       TEXT,
    path_absolute       TEXT,
    target_file_id      INTEGER,
    target_heading_id   INTEGER,
    target_custom_id    TEXT,
    target_id           TEXT
);

INSERT INTO links
    (id, file_id, heading_id, byte_start, byte_end, line, source_context, format, raw,
     raw_target, raw_description, link_type, path, search_option, path_absolute,
     target_file_id, target_heading_id, target_custom_id, target_id)
VALUES
    (1, 1, 1, 0, 18, 1, 'normal', 'bracket', '[[id:abc123]]',
     'id:abc123', NULL, 'id', 'abc123', NULL, NULL, NULL, NULL, NULL, NULL);

PRAGMA user_version = 2;
"#,
                )
                .expect("phase3 links schema should initialize");
        }

        let connection = open_database(&database_path).expect("database should upgrade");

        let migrated_row: (Option<String>, Option<String>, String, String) = connection
            .query_row(
                "SELECT resolution_status, resolution_diagnostic, raw, raw_target
                 FROM links
                 WHERE id = 1",
                [],
                |row| Ok((row.get(0)?, row.get(1)?, row.get(2)?, row.get(3)?)),
            )
            .expect("migrated phase3 link row should be queryable");
        assert_eq!(
            migrated_row,
            (
                None,
                None,
                "[[id:abc123]]".to_string(),
                "id:abc123".to_string(),
            )
        );
    }

    #[test]
    fn open_database_upgrades_v4_timestamp_tables_with_explicit_time_columns() {
        let test_dir = TestDir::new("explicit-time-columns");
        let database_path = test_dir.path().join("org-files-db.sqlite");

        open_database(&database_path).expect("database should initialize");

        {
            let legacy = Connection::open(&database_path).expect("legacy database should open");
            legacy
                .execute_batch(
                    r#"
DROP TABLE timestamps;
DROP TABLE headings;

CREATE TABLE headings (
    id                  INTEGER PRIMARY KEY,
    file_id             INTEGER NOT NULL,
    parent_id           INTEGER,
    level               INTEGER NOT NULL CHECK (level >= 0),
    line_number         INTEGER,
    byte_start          INTEGER NOT NULL,
    byte_end            INTEGER NOT NULL CHECK (byte_end >= byte_start),
    title               TEXT NOT NULL,
    title_raw           TEXT,
    todo_keyword        TEXT,
    todo_type           TEXT CHECK (todo_type IN ('open', 'closed') OR todo_type IS NULL),
    priority            TEXT CHECK (priority IS NULL OR length(priority) = 1),
    scheduled_raw       TEXT,
    scheduled_ts        INTEGER,
    deadline_raw        TEXT,
    deadline_ts         INTEGER,
    closed_raw          TEXT,
    closed_ts           INTEGER,
    archivedp           INTEGER NOT NULL DEFAULT 0 CHECK (archivedp IN (0, 1)),
    footnote_section_p  INTEGER NOT NULL DEFAULT 0 CHECK (footnote_section_p IN (0, 1)),
    all_tags_json       TEXT NOT NULL DEFAULT '[]',
    CHECK (
        (level = 0 AND parent_id IS NULL)
        OR
        (level > 0 AND parent_id IS NOT NULL)
    )
);

CREATE TABLE timestamps (
    id              INTEGER PRIMARY KEY,
    heading_id      INTEGER NOT NULL,
    role            TEXT,
    start_ts        INTEGER,
    end_ts          INTEGER,
    type            TEXT,
    range_type      TEXT,
    raw_value       TEXT NOT NULL,
    byte_start      INTEGER NOT NULL,
    byte_end        INTEGER NOT NULL CHECK (byte_end >= byte_start),
    line_number     INTEGER
);

INSERT INTO files (id, path, mtime_ns, size) VALUES (1, '/tmp/example.org', 10, 20);
INSERT INTO headings
    (id, file_id, parent_id, level, byte_start, byte_end, title, title_raw,
     scheduled_raw, scheduled_ts, all_tags_json)
VALUES
    (1, 1, NULL, 0, -1, 20, '/tmp/example.org', NULL, NULL, NULL, '[]'),
    (2, 1, 1, 1, 21, 60, 'Timed task', 'Timed task',
     '<2026-01-03 Fri 00:00>', 1767398400, '[]');

INSERT INTO timestamps
    (id, heading_id, role, start_ts, end_ts, type, range_type, raw_value, byte_start, byte_end, line_number)
VALUES
    (1, 2, 'scheduled', 1767398400, NULL, 'active', 'none', '<2026-01-03 Fri 00:00>', 21, 43, 2);

PRAGMA user_version = 4;
"#,
                )
                .expect("legacy explicit-time schema should initialize");
        }

        let connection = open_database(&database_path).expect("database should upgrade");
        let schema_version = read_schema_version(&connection).expect("schema version should load");
        assert_eq!(schema_version, CURRENT_SCHEMA_VERSION);

        let heading_columns: Vec<String> = {
            let mut statement = connection
                .prepare("PRAGMA table_info(headings)")
                .expect("headings pragma should prepare");
            statement
                .query_map([], |row| row.get(1))
                .expect("headings pragma should query")
                .collect::<Result<Vec<_>, _>>()
                .expect("headings columns should collect")
        };
        assert!(heading_columns
            .iter()
            .any(|column| column == "scheduled_has_time"));
        assert!(heading_columns
            .iter()
            .any(|column| column == "deadline_has_time"));
        assert!(heading_columns
            .iter()
            .any(|column| column == "closed_has_time"));

        let timestamp_columns: Vec<String> = {
            let mut statement = connection
                .prepare("PRAGMA table_info(timestamps)")
                .expect("timestamps pragma should prepare");
            statement
                .query_map([], |row| row.get(1))
                .expect("timestamps pragma should query")
                .collect::<Result<Vec<_>, _>>()
                .expect("timestamps columns should collect")
        };
        assert!(timestamp_columns.iter().any(|column| column == "has_time"));

        let migrated_heading: (Option<i64>, Option<i64>) = connection
            .query_row(
                "SELECT scheduled_ts, scheduled_has_time
                 FROM headings
                 WHERE id = 2",
                [],
                |row| Ok((row.get(0)?, row.get(1)?)),
            )
            .expect("migrated heading should load");
        assert_eq!(migrated_heading, (Some(1_767_398_400), None));

        let migrated_timestamp: (Option<i64>, Option<i64>) = connection
            .query_row(
                "SELECT start_ts, has_time
                 FROM timestamps
                 WHERE id = 1",
                [],
                |row| Ok((row.get(0)?, row.get(1)?)),
            )
            .expect("migrated timestamp should load");
        assert_eq!(migrated_timestamp, (Some(1_767_398_400), None));
    }

    #[test]
    fn open_database_preserves_headings_dependents_during_headings_migration() {
        let test_dir = TestDir::new("headings-dependent-repair");
        let database_path = test_dir.path().join("org-files-db.sqlite");

        open_database(&database_path).expect("database should initialize");

        {
            let legacy = Connection::open(&database_path).expect("legacy database should open");
            legacy
                .execute_batch(
                    r#"
DROP TABLE outline_path;
DROP TABLE heading_bodies;
DROP TABLE timestamp_repeaters;
DROP TABLE timestamps;
DROP TABLE links;
DROP TABLE tags;
DROP TABLE properties;
DROP TABLE keywords;
DROP TABLE headings;

CREATE TABLE headings (
    id                  INTEGER PRIMARY KEY,
    file_id             INTEGER NOT NULL,
    parent_id           INTEGER,
    level               INTEGER NOT NULL CHECK (level >= 0),
    line_number         INTEGER,
    byte_start          INTEGER NOT NULL,
    byte_end            INTEGER NOT NULL CHECK (byte_end >= byte_start),
    title               TEXT NOT NULL,
    title_raw           TEXT,
    todo_keyword        TEXT,
    todo_type           TEXT CHECK (todo_type IN ('open', 'closed') OR todo_type IS NULL),
    priority            TEXT CHECK (priority IS NULL OR length(priority) = 1),
    scheduled_raw       TEXT,
    scheduled_ts        INTEGER,
    deadline_raw        TEXT,
    deadline_ts         INTEGER,
    closed_raw          TEXT,
    closed_ts           INTEGER,
    archivedp           INTEGER NOT NULL DEFAULT 0 CHECK (archivedp IN (0, 1)),
    footnote_section_p  INTEGER NOT NULL DEFAULT 0 CHECK (footnote_section_p IN (0, 1)),
    all_tags_json       TEXT NOT NULL DEFAULT '[]',
    CHECK (
        (level = 0 AND parent_id IS NULL)
        OR
        (level > 0 AND parent_id IS NOT NULL)
    )
);

CREATE TABLE keywords (
    id              INTEGER PRIMARY KEY,
    heading_id      INTEGER NOT NULL,
    keyword         TEXT NOT NULL,
    value           TEXT,
    line_number     INTEGER,
    FOREIGN KEY (heading_id)
        REFERENCES headings(id)
        ON DELETE CASCADE,
    UNIQUE (heading_id, keyword, line_number)
);

CREATE TABLE properties (
    id              INTEGER PRIMARY KEY,
    heading_id      INTEGER NOT NULL,
    key             TEXT NOT NULL,
    value           TEXT,
    source          TEXT NOT NULL CHECK (
                        source IN ('property_keyword', 'property_drawer', 'category_keyword')
                    ),
    append          INTEGER NOT NULL DEFAULT 0 CHECK (append IN (0, 1)),
    line_number     INTEGER,
    FOREIGN KEY (heading_id)
        REFERENCES headings(id)
        ON DELETE CASCADE
);

CREATE TABLE tags (
    heading_id      INTEGER NOT NULL,
    tag             TEXT NOT NULL,
    FOREIGN KEY (heading_id)
        REFERENCES headings(id)
        ON DELETE CASCADE,
    PRIMARY KEY (heading_id, tag)
);

CREATE TABLE timestamps (
    id              INTEGER PRIMARY KEY,
    heading_id      INTEGER NOT NULL,
    role            TEXT,
    start_ts        INTEGER,
    end_ts          INTEGER,
    type            TEXT,
    range_type      TEXT,
    raw_value       TEXT NOT NULL,
    byte_start      INTEGER NOT NULL,
    byte_end        INTEGER NOT NULL CHECK (byte_end >= byte_start),
    line_number     INTEGER,
    FOREIGN KEY (heading_id)
        REFERENCES headings(id)
        ON DELETE CASCADE
);

CREATE TABLE timestamp_repeaters (
    id                          INTEGER PRIMARY KEY,
    timestamp_id                INTEGER NOT NULL UNIQUE,
    repeater_type               TEXT,
    repeater_value              INTEGER,
    repeater_unit               TEXT,
    repeater_deadline_value     INTEGER,
    repeater_deadline_unit      TEXT,
    warning_type                TEXT,
    warning_value               INTEGER,
    warning_unit                TEXT,
    FOREIGN KEY (timestamp_id)
        REFERENCES timestamps(id)
        ON DELETE CASCADE
);

CREATE TABLE links (
    id                  INTEGER PRIMARY KEY,
    file_id             INTEGER NOT NULL,
    heading_id          INTEGER NOT NULL,
    byte_start          INTEGER NOT NULL CHECK (byte_start >= 0),
    byte_end            INTEGER NOT NULL CHECK (byte_end >= byte_start),
    line                INTEGER NOT NULL CHECK (line > 0),
    source_context      TEXT NOT NULL,
    format              TEXT NOT NULL,
    raw                 TEXT NOT NULL,
    raw_target          TEXT NOT NULL,
    raw_description     TEXT,
    link_type           TEXT NOT NULL,
    path                TEXT NOT NULL,
    search_option       TEXT,
    path_absolute       TEXT,
    target_file_id      INTEGER,
    target_heading_id   INTEGER,
    target_custom_id    TEXT,
    target_id           TEXT,
    resolution_status   TEXT,
    resolution_diagnostic TEXT,
    FOREIGN KEY (file_id)
        REFERENCES files(id)
        ON DELETE CASCADE,
    FOREIGN KEY (heading_id)
        REFERENCES headings(id)
        ON DELETE CASCADE,
    FOREIGN KEY (target_file_id)
        REFERENCES files(id)
        ON DELETE SET NULL,
    FOREIGN KEY (target_heading_id)
        REFERENCES headings(id)
        ON DELETE SET NULL,
    UNIQUE (file_id, byte_start)
);

CREATE TABLE heading_bodies (
    heading_id          INTEGER PRIMARY KEY,
    body_text           TEXT NOT NULL,
    body_byte_start     INTEGER,
    body_byte_end       INTEGER,
    FOREIGN KEY (heading_id)
        REFERENCES headings(id)
        ON DELETE CASCADE
);

CREATE TABLE outline_path (
    heading_id          INTEGER PRIMARY KEY,
    file_id             INTEGER NOT NULL,
    parent_id           INTEGER,
    depth               INTEGER NOT NULL CHECK (depth >= 0),
    materialized_path   TEXT NOT NULL,
    breadcrumbs_json    TEXT NOT NULL,
    FOREIGN KEY (heading_id)
        REFERENCES headings(id)
        ON DELETE CASCADE,
    FOREIGN KEY (file_id)
        REFERENCES files(id)
        ON DELETE CASCADE,
    FOREIGN KEY (parent_id)
        REFERENCES headings(id)
        ON DELETE SET NULL
);

INSERT INTO files (id, path, mtime_ns, size) VALUES (1, '/tmp/migrate.org', 10, 90);
INSERT INTO headings
    (id, file_id, parent_id, level, line_number, byte_start, byte_end, title, title_raw,
     scheduled_raw, scheduled_ts, all_tags_json)
VALUES
    (1, 1, NULL, 0, 1, -1, 90, 'Migrate Index', 'Migrate Index', NULL, NULL, '[]'),
    (2, 1, 1, 1, 3, 20, 80, 'Migrated Task', 'Migrated Task',
     '<2026-01-03 Fri 00:00>', 1767398400, '["project"]');

INSERT INTO keywords (id, heading_id, keyword, value, line_number)
VALUES (1, 1, 'TITLE', 'Migrate Index', 1);
INSERT INTO properties (id, heading_id, key, value, source, append, line_number)
VALUES (1, 2, 'CUSTOM_ID', 'migrated-task', 'property_drawer', 0, 4);
INSERT INTO tags (heading_id, tag) VALUES (2, 'project');
INSERT INTO timestamps
    (id, heading_id, role, start_ts, end_ts, type, range_type, raw_value, byte_start, byte_end, line_number)
VALUES
    (1, 2, 'scheduled', 1767398400, NULL, 'active', 'none', '<2026-01-03 Fri 00:00>', 25, 47, 3);
INSERT INTO timestamp_repeaters
    (id, timestamp_id, repeater_type, repeater_value, repeater_unit, repeater_deadline_value,
     repeater_deadline_unit, warning_type, warning_value, warning_unit)
VALUES
    (1, 1, 'restart', 1, 'week', NULL, NULL, NULL, NULL, NULL);
INSERT INTO links
    (id, file_id, heading_id, byte_start, byte_end, line, source_context, format, raw, raw_target,
     raw_description, link_type, path, search_option, path_absolute, target_file_id, target_heading_id,
     target_custom_id, target_id, resolution_status, resolution_diagnostic)
VALUES
    (1, 1, 2, 50, 70, 5, 'normal', 'bracket', '[[file:target.org][Target]]', 'file:target.org',
     'Target', 'file', 'target.org', NULL, '/tmp/target.org', NULL, NULL, NULL, NULL, 'resolved', NULL);
INSERT INTO heading_bodies (heading_id, body_text, body_byte_start, body_byte_end)
VALUES (2, 'Migrated body', 48, 79);
INSERT INTO outline_path
    (heading_id, file_id, parent_id, depth, materialized_path, breadcrumbs_json)
VALUES
    (1, 1, NULL, 0, '0000', '["Migrate Index"]'),
    (2, 1, 1, 1, '0000.0001', '["Migrate Index","Migrated Task"]');

PRAGMA user_version = 4;
"#,
                )
                .expect("legacy headings schema should initialize");
        }

        let connection = open_database(&database_path).expect("database should upgrade");

        connection
            .execute("UPDATE headings SET priority = '10' WHERE id = 2", [])
            .expect("migrated headings schema should preserve multi-digit priorities");
        let priority: Option<String> = connection
            .query_row("SELECT priority FROM headings WHERE id = 2", [], |row| {
                row.get(0)
            })
            .expect("migrated priority should load");
        assert_eq!(priority.as_deref(), Some("10"));

        for table_name in [
            "keywords",
            "properties",
            "tags",
            "timestamps",
            "heading_bodies",
            "links",
            "outline_path",
        ] {
            assert!(
                !foreign_key_targets(&connection, table_name)
                    .iter()
                    .any(|target| target == "headings_legacy"),
                "{table_name} should not retain headings_legacy foreign keys"
            );
        }
        assert!(
            foreign_key_check_rows(&connection).is_empty(),
            "foreign_key_check should be empty after repair"
        );

        assert_eq!(count_rows(&connection, "SELECT COUNT(*) FROM keywords"), 1);
        assert_eq!(
            count_rows(&connection, "SELECT COUNT(*) FROM properties"),
            1
        );
        assert_eq!(count_rows(&connection, "SELECT COUNT(*) FROM tags"), 1);
        assert_eq!(
            count_rows(&connection, "SELECT COUNT(*) FROM timestamps"),
            1
        );
        assert_eq!(
            count_rows(&connection, "SELECT COUNT(*) FROM timestamp_repeaters"),
            1
        );
        assert_eq!(
            count_rows(&connection, "SELECT COUNT(*) FROM heading_bodies"),
            1
        );
        assert_eq!(count_rows(&connection, "SELECT COUNT(*) FROM links"), 1);
        assert_eq!(
            count_rows(&connection, "SELECT COUNT(*) FROM outline_path"),
            2
        );

        let child_outline: (i64, String) = connection
            .query_row(
                "SELECT depth, breadcrumbs_json
                 FROM outline_path
                 WHERE heading_id = 2",
                [],
                |row| Ok((row.get(0)?, row.get(1)?)),
            )
            .expect("rebuilt outline path should load");
        assert_eq!(child_outline.0, 1);
        assert_eq!(child_outline.1, "[\"Migrate Index\",\"Migrated Task\"]");

        let links = DbReader::list_links(&connection).expect("links should remain queryable");
        assert_eq!(links.len(), 1);
        assert_eq!(links[0].heading_id, 2);
    }

    #[test]
    fn open_database_repairs_tables_still_referencing_headings_legacy() {
        let test_dir = TestDir::new("repair-headings-legacy-fks");
        let database_path = test_dir.path().join("org-files-db.sqlite");

        {
            let broken = Connection::open(&database_path).expect("broken database should open");
            broken
                .execute_batch(
                    r#"
PRAGMA foreign_keys = OFF;
PRAGMA user_version = 7;

CREATE TABLE files (
    id              INTEGER PRIMARY KEY,
    path            TEXT NOT NULL UNIQUE,
    mtime_ns        INTEGER NOT NULL,
    size            INTEGER NOT NULL,
    content_hash    TEXT,
    indexed_at      INTEGER
);

CREATE TABLE db_metadata (
    key             TEXT PRIMARY KEY,
    value           TEXT NOT NULL
);

CREATE TABLE headings (
    id                  INTEGER PRIMARY KEY,
    file_id             INTEGER NOT NULL,
    parent_id           INTEGER,
    level               INTEGER NOT NULL CHECK (level >= 0),
    line_number         INTEGER,
    byte_start          INTEGER NOT NULL,
    byte_end            INTEGER NOT NULL CHECK (byte_end >= byte_start),
    title               TEXT NOT NULL,
    title_raw           TEXT,
    todo_keyword        TEXT,
    todo_type           TEXT CHECK (todo_type IN ('open', 'closed') OR todo_type IS NULL),
    priority            TEXT CHECK (priority IS NULL OR length(priority) = 1),
    scheduled_raw       TEXT,
    scheduled_ts        INTEGER,
    scheduled_has_time  INTEGER CHECK (scheduled_has_time IN (0, 1) OR scheduled_has_time IS NULL),
    deadline_raw        TEXT,
    deadline_ts         INTEGER,
    deadline_has_time   INTEGER CHECK (deadline_has_time IN (0, 1) OR deadline_has_time IS NULL),
    closed_raw          TEXT,
    closed_ts           INTEGER,
    closed_has_time     INTEGER CHECK (closed_has_time IN (0, 1) OR closed_has_time IS NULL),
    archivedp           INTEGER NOT NULL DEFAULT 0 CHECK (archivedp IN (0, 1)),
    footnote_section_p  INTEGER NOT NULL DEFAULT 0 CHECK (footnote_section_p IN (0, 1)),
    all_tags_json       TEXT NOT NULL DEFAULT '[]',
    CHECK (
        (level = 0 AND parent_id IS NULL)
        OR
        (level > 0 AND parent_id IS NOT NULL)
    ),
    FOREIGN KEY (file_id)
        REFERENCES files(id)
        ON DELETE CASCADE,
    FOREIGN KEY (parent_id)
        REFERENCES headings(id)
        ON DELETE CASCADE,
    UNIQUE (file_id, byte_start)
);

CREATE TABLE keywords (
    id              INTEGER PRIMARY KEY,
    heading_id      INTEGER NOT NULL,
    keyword         TEXT NOT NULL,
    value           TEXT,
    line_number     INTEGER,
    FOREIGN KEY (heading_id)
        REFERENCES headings_legacy(id)
        ON DELETE CASCADE,
    UNIQUE (heading_id, keyword, line_number)
);

CREATE TABLE properties (
    id              INTEGER PRIMARY KEY,
    heading_id      INTEGER NOT NULL,
    key             TEXT NOT NULL,
    value           TEXT,
    source          TEXT NOT NULL CHECK (
                        source IN ('property_keyword', 'property_drawer', 'category_keyword')
                    ),
    append          INTEGER NOT NULL DEFAULT 0 CHECK (append IN (0, 1)),
    line_number     INTEGER,
    FOREIGN KEY (heading_id)
        REFERENCES headings_legacy(id)
        ON DELETE CASCADE
);

CREATE TABLE tags (
    heading_id      INTEGER NOT NULL,
    tag             TEXT NOT NULL,
    FOREIGN KEY (heading_id)
        REFERENCES headings_legacy(id)
        ON DELETE CASCADE,
    PRIMARY KEY (heading_id, tag)
);

CREATE TABLE timestamps (
    id              INTEGER PRIMARY KEY,
    heading_id      INTEGER NOT NULL,
    role            TEXT CHECK (
                        role IN ('scheduled', 'deadline', 'closed', 'body')
                        OR role IS NULL
                    ),
    has_time        INTEGER CHECK (has_time IN (0, 1) OR has_time IS NULL),
    start_ts        INTEGER,
    end_ts          INTEGER,
    type            TEXT CHECK (
                        type IN ('active', 'inactive', 'diary')
                        OR type IS NULL
                    ),
    range_type      TEXT CHECK (
                        range_type IN ('none', 'date_range', 'time_range', 'datetime_range', 'unknown')
                        OR range_type IS NULL
                    ),
    raw_value       TEXT NOT NULL,
    byte_start      INTEGER NOT NULL,
    byte_end        INTEGER NOT NULL CHECK (byte_end >= byte_start),
    line_number     INTEGER,
    FOREIGN KEY (heading_id)
        REFERENCES headings_legacy(id)
        ON DELETE CASCADE
);

CREATE TABLE timestamp_repeaters (
    id                          INTEGER PRIMARY KEY,
    timestamp_id                INTEGER NOT NULL UNIQUE,
    repeater_type               TEXT,
    repeater_value              INTEGER,
    repeater_unit               TEXT,
    repeater_deadline_value     INTEGER,
    repeater_deadline_unit      TEXT,
    warning_type                TEXT,
    warning_value               INTEGER,
    warning_unit                TEXT,
    FOREIGN KEY (timestamp_id)
        REFERENCES timestamps(id)
        ON DELETE CASCADE
);

CREATE TABLE links (
    id                  INTEGER PRIMARY KEY,
    file_id             INTEGER NOT NULL,
    heading_id          INTEGER NOT NULL,
    byte_start          INTEGER NOT NULL CHECK (byte_start >= 0),
    byte_end            INTEGER NOT NULL CHECK (byte_end >= byte_start),
    line                INTEGER NOT NULL CHECK (line > 0),
    source_context      TEXT NOT NULL,
    format              TEXT NOT NULL,
    raw                 TEXT NOT NULL,
    raw_target          TEXT NOT NULL,
    raw_description     TEXT,
    link_type           TEXT NOT NULL,
    path                TEXT NOT NULL,
    search_option       TEXT,
    path_absolute       TEXT,
    target_file_id      INTEGER,
    target_heading_id   INTEGER,
    target_custom_id    TEXT,
    target_id           TEXT,
    resolution_status   TEXT,
    resolution_diagnostic TEXT,
    FOREIGN KEY (file_id)
        REFERENCES files(id)
        ON DELETE CASCADE,
    FOREIGN KEY (heading_id)
        REFERENCES headings_legacy(id)
        ON DELETE CASCADE,
    FOREIGN KEY (target_file_id)
        REFERENCES files(id)
        ON DELETE SET NULL,
    FOREIGN KEY (target_heading_id)
        REFERENCES headings_legacy(id)
        ON DELETE SET NULL,
    UNIQUE (file_id, byte_start)
);

CREATE TABLE heading_bodies (
    heading_id          INTEGER PRIMARY KEY,
    body_text           TEXT NOT NULL,
    body_byte_start     INTEGER,
    body_byte_end       INTEGER,
    FOREIGN KEY (heading_id)
        REFERENCES headings_legacy(id)
        ON DELETE CASCADE
);

CREATE TABLE outline_path (
    heading_id          INTEGER PRIMARY KEY,
    file_id             INTEGER NOT NULL,
    parent_id           INTEGER,
    depth               INTEGER NOT NULL CHECK (depth >= 0),
    materialized_path   TEXT NOT NULL,
    breadcrumbs_json    TEXT NOT NULL,
    FOREIGN KEY (heading_id)
        REFERENCES headings_legacy(id)
        ON DELETE CASCADE,
    FOREIGN KEY (file_id)
        REFERENCES files(id)
        ON DELETE CASCADE,
    FOREIGN KEY (parent_id)
        REFERENCES headings_legacy(id)
        ON DELETE SET NULL
);

INSERT INTO db_metadata (key, value) VALUES
    ('fts_available', '1'),
    ('fts_body_indexed', '1'),
    ('fts_schema_version', '1');

INSERT INTO files (id, path, mtime_ns, size) VALUES (1, '/tmp/repair.org', 10, 80);
INSERT INTO headings
    (id, file_id, parent_id, level, line_number, byte_start, byte_end, title, title_raw, all_tags_json)
VALUES
    (1, 1, NULL, 0, 1, -1, 80, 'Repair Index', 'Repair Index', '[]'),
    (2, 1, 1, 1, 3, 10, 70, 'Repair Heading', 'Repair Heading', '["repair"]');
INSERT INTO keywords (id, heading_id, keyword, value, line_number)
VALUES (1, 1, 'TITLE', 'Repair Index', 1);
INSERT INTO properties (id, heading_id, key, value, source, append, line_number)
VALUES (1, 2, 'CUSTOM_ID', 'repair-heading', 'property_drawer', 0, 4);
INSERT INTO tags (heading_id, tag) VALUES (2, 'repair');
INSERT INTO timestamps
    (id, heading_id, role, has_time, start_ts, end_ts, type, range_type, raw_value, byte_start, byte_end, line_number)
VALUES
    (1, 2, 'body', NULL, NULL, NULL, 'active', 'none', '<2026-01-03 Fri>', 20, 36, 5);
INSERT INTO timestamp_repeaters
    (id, timestamp_id, repeater_type, repeater_value, repeater_unit, repeater_deadline_value,
     repeater_deadline_unit, warning_type, warning_value, warning_unit)
VALUES
    (1, 1, 'restart', 2, 'week', NULL, NULL, NULL, NULL, NULL);
INSERT INTO links
    (id, file_id, heading_id, byte_start, byte_end, line, source_context, format, raw, raw_target,
     raw_description, link_type, path, search_option, path_absolute, target_file_id, target_heading_id,
     target_custom_id, target_id, resolution_status, resolution_diagnostic)
VALUES
    (1, 1, 2, 40, 60, 6, 'normal', 'plain', 'https://example.com', 'https://example.com',
     NULL, 'https', 'https://example.com', NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL);
INSERT INTO heading_bodies (heading_id, body_text, body_byte_start, body_byte_end)
VALUES (2, 'Repair body', 37, 69);

PRAGMA foreign_keys = ON;
"#,
                )
                .expect("broken headings-dependent schema should initialize");
        }

        let connection = open_database(&database_path).expect("database should repair");

        for table_name in [
            "keywords",
            "properties",
            "tags",
            "timestamps",
            "heading_bodies",
            "links",
            "outline_path",
        ] {
            assert!(
                !foreign_key_targets(&connection, table_name)
                    .iter()
                    .any(|target| target == "headings_legacy"),
                "{table_name} should not retain headings_legacy foreign keys"
            );
        }
        assert!(
            foreign_key_check_rows(&connection).is_empty(),
            "foreign_key_check should be empty after repair"
        );

        assert_eq!(
            count_rows(&connection, "SELECT COUNT(*) FROM outline_path"),
            2
        );
        assert_eq!(count_rows(&connection, "SELECT COUNT(*) FROM keywords"), 1);
        assert_eq!(
            count_rows(&connection, "SELECT COUNT(*) FROM properties"),
            1
        );
        assert_eq!(count_rows(&connection, "SELECT COUNT(*) FROM tags"), 1);
        assert_eq!(
            count_rows(&connection, "SELECT COUNT(*) FROM timestamps"),
            1
        );
        assert_eq!(
            count_rows(&connection, "SELECT COUNT(*) FROM timestamp_repeaters"),
            1
        );
        assert_eq!(
            count_rows(&connection, "SELECT COUNT(*) FROM heading_bodies"),
            1
        );
        assert_eq!(count_rows(&connection, "SELECT COUNT(*) FROM links"), 1);

        let fts_metadata: Vec<(String, String)> = {
            let mut statement = connection
                .prepare(
                    "SELECT key, value
                     FROM db_metadata
                     WHERE key IN ('fts_available', 'fts_body_indexed', 'fts_schema_version')
                     ORDER BY key",
                )
                .expect("fts metadata query should prepare");
            statement
                .query_map([], |row| Ok((row.get(0)?, row.get(1)?)))
                .expect("fts metadata query should run")
                .collect::<Result<Vec<_>, _>>()
                .expect("fts metadata rows should collect")
        };
        assert_eq!(
            fts_metadata,
            vec![
                ("fts_available".to_string(), "0".to_string()),
                ("fts_body_indexed".to_string(), "0".to_string()),
                ("fts_schema_version".to_string(), "0".to_string()),
            ]
        );

        let links = DbReader::list_links(&connection).expect("links should remain queryable");
        assert_eq!(links.len(), 1);
        assert_eq!(
            links[0].heading_breadcrumbs_json,
            "[\"Repair Index\",\"Repair Heading\"]"
        );
    }

    #[test]
    fn open_database_preserves_existing_explicit_time_values_during_table_rebuilds() {
        let test_dir = TestDir::new("preserve-explicit-time-values");
        let database_path = test_dir.path().join("org-files-db.sqlite");

        open_database(&database_path).expect("database should initialize");

        {
            let legacy = Connection::open(&database_path).expect("legacy database should open");
            legacy
                .execute_batch(
                    r#"
DROP TABLE timestamps;
DROP TABLE headings;

CREATE TABLE headings (
    id                  INTEGER PRIMARY KEY,
    file_id             INTEGER NOT NULL,
    parent_id           INTEGER,
    level               INTEGER NOT NULL CHECK (level >= 0),
    line_number         INTEGER,
    byte_start          INTEGER NOT NULL,
    byte_end            INTEGER NOT NULL CHECK (byte_end >= byte_start),
    title               TEXT NOT NULL,
    title_raw           TEXT NOT NULL,
    todo_keyword        TEXT,
    todo_type           TEXT CHECK (todo_type IN ('open', 'closed') OR todo_type IS NULL),
    priority            TEXT CHECK (priority IS NULL OR length(priority) = 1),
    scheduled_raw       TEXT,
    scheduled_ts        INTEGER,
    scheduled_has_time  INTEGER CHECK (scheduled_has_time IN (0, 1) OR scheduled_has_time IS NULL),
    deadline_raw        TEXT,
    deadline_ts         INTEGER,
    deadline_has_time   INTEGER CHECK (deadline_has_time IN (0, 1) OR deadline_has_time IS NULL),
    closed_raw          TEXT,
    closed_ts           INTEGER,
    closed_has_time     INTEGER CHECK (closed_has_time IN (0, 1) OR closed_has_time IS NULL),
    archivedp           INTEGER NOT NULL DEFAULT 0 CHECK (archivedp IN (0, 1)),
    footnote_section_p  INTEGER NOT NULL DEFAULT 0 CHECK (footnote_section_p IN (0, 1)),
    all_tags_json       TEXT NOT NULL DEFAULT '[]',
    CHECK (
        (level = 0 AND parent_id IS NULL)
        OR
        (level > 0 AND parent_id IS NOT NULL)
    )
);

CREATE TABLE timestamps (
    id              INTEGER PRIMARY KEY,
    heading_id      INTEGER NOT NULL,
    role            TEXT,
    has_time        INTEGER CHECK (has_time IN (0, 1) OR has_time IS NULL),
    start_ts        INTEGER,
    end_ts          INTEGER,
    type            TEXT,
    range_type      TEXT,
    raw_value       TEXT NOT NULL,
    byte_start      INTEGER NOT NULL,
    byte_end        INTEGER NOT NULL CHECK (byte_end >= byte_start),
    line_number     INTEGER,
    has_repeater    INTEGER
);

INSERT INTO files (id, path, mtime_ns, size) VALUES (1, '/tmp/example.org', 10, 20);
INSERT INTO headings
    (id, file_id, parent_id, level, byte_start, byte_end, title, title_raw,
     scheduled_raw, scheduled_ts, scheduled_has_time,
     deadline_raw, deadline_ts, deadline_has_time,
     closed_raw, closed_ts, closed_has_time, all_tags_json)
VALUES
    (1, 1, NULL, 0, -1, 20, '/tmp/example.org', '/tmp/example.org',
     NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, '[]'),
    (2, 1, 1, 1, 21, 60, 'Timed task', 'Timed task',
     '<2026-01-03 Fri 00:00>', 1767398400, 1,
     '<2026-01-04 Sat>', 1767484800, 0,
     '[2026-01-05 Sun 09:30]', 1767605400, 1, '[]');

INSERT INTO timestamps
    (id, heading_id, role, has_time, start_ts, end_ts, type, range_type, raw_value, byte_start, byte_end, line_number, has_repeater)
VALUES
    (1, 2, 'scheduled', 1, 1767398400, NULL, 'active', 'none', '<2026-01-03 Fri 00:00>', 21, 43, 2, 0),
    (2, 2, 'deadline', 0, 1767484800, NULL, 'active', 'none', '<2026-01-04 Sat>', 44, 60, 2, 0);

PRAGMA user_version = 4;
"#,
                )
                .expect("legacy explicit-time preservation schema should initialize");
        }

        let connection = open_database(&database_path).expect("database should upgrade");

        let migrated_heading: (Option<i64>, Option<i64>, Option<i64>) = connection
            .query_row(
                "SELECT scheduled_has_time, deadline_has_time, closed_has_time
                 FROM headings
                 WHERE id = 2",
                [],
                |row| Ok((row.get(0)?, row.get(1)?, row.get(2)?)),
            )
            .expect("migrated heading should load");
        assert_eq!(migrated_heading, (Some(1), Some(0), Some(1)));

        let migrated_timestamps: Vec<(String, Option<i64>)> = {
            let mut statement = connection
                .prepare(
                    "SELECT role, has_time
                     FROM timestamps
                     WHERE heading_id = 2
                     ORDER BY id",
                )
                .expect("timestamps query should prepare");
            statement
                .query_map([], |row| Ok((row.get(0)?, row.get(1)?)))
                .expect("timestamps query should run")
                .collect::<Result<Vec<_>, _>>()
                .expect("timestamps should collect")
        };
        assert_eq!(
            migrated_timestamps,
            vec![
                ("scheduled".to_string(), Some(1)),
                ("deadline".to_string(), Some(0)),
            ]
        );
    }

    #[test]
    fn links_resolution_status_accepts_all_allowed_values() {
        let schema = SchemaDefinition::new(CURRENT_SCHEMA_VERSION, false);
        let connection =
            open_in_memory_database_with_schema(&schema).expect("database should open");

        connection
            .execute(
                "INSERT INTO files (id, path, mtime_ns, size) VALUES (?1, ?2, ?3, ?4)",
                (1_i64, "/tmp/status.org", 10_i64, 20_i64),
            )
            .expect("file insert should succeed");
        connection
            .execute(
                "INSERT INTO headings
                 (id, file_id, parent_id, level, byte_start, byte_end, title, title_raw)
                 VALUES (?1, ?2, ?3, ?4, ?5, ?6, ?7, ?8)",
                (
                    1_i64,
                    1_i64,
                    Option::<i64>::None,
                    0_i64,
                    -1_i64,
                    20_i64,
                    "/tmp/status.org",
                    "/tmp/status.org",
                ),
            )
            .expect("heading insert should succeed");

        let statuses = [
            "unresolved",
            "resolved",
            "broken",
            "ambiguous",
            "unsupported",
        ];
        for (index, status) in statuses.iter().enumerate() {
            connection
                .execute(
                    "INSERT INTO links
                     (id, file_id, heading_id, byte_start, byte_end, line, source_context, format,
                      raw, raw_target, raw_description, link_type, path, search_option,
                      resolution_status, resolution_diagnostic)
                     VALUES (?1, ?2, ?3, ?4, ?5, ?6, ?7, ?8, ?9, ?10, ?11, ?12, ?13, ?14,
                             ?15, ?16)",
                    params![
                        (index as i64) + 1,
                        1_i64,
                        1_i64,
                        (index as i64) * 10,
                        (index as i64) * 10 + 8,
                        (index as i64) + 1,
                        "normal",
                        "plain",
                        format!("id:{status}"),
                        format!("id:{status}"),
                        Option::<String>::None,
                        "id",
                        status.to_string(),
                        Option::<String>::None,
                        status.to_string(),
                        Some(format!("diag:{status}")),
                    ],
                )
                .expect("allowed resolution status should insert");
        }

        let stored_statuses: Vec<String> = {
            let mut statement = connection
                .prepare("SELECT resolution_status FROM links ORDER BY id")
                .expect("status query should prepare");
            statement
                .query_map([], |row| row.get(0))
                .expect("status query should run")
                .collect::<Result<Vec<_>, _>>()
                .expect("status rows should collect")
        };
        assert_eq!(
            stored_statuses,
            statuses
                .iter()
                .map(|status| status.to_string())
                .collect::<Vec<_>>()
        );
    }

    #[test]
    fn links_resolution_status_rejects_invalid_non_null_values() {
        let schema = SchemaDefinition::new(CURRENT_SCHEMA_VERSION, false);
        let connection =
            open_in_memory_database_with_schema(&schema).expect("database should open");

        connection
            .execute(
                "INSERT INTO files (id, path, mtime_ns, size) VALUES (?1, ?2, ?3, ?4)",
                (1_i64, "/tmp/invalid-status.org", 10_i64, 20_i64),
            )
            .expect("file insert should succeed");
        connection
            .execute(
                "INSERT INTO headings
                 (id, file_id, parent_id, level, byte_start, byte_end, title, title_raw)
                 VALUES (?1, ?2, ?3, ?4, ?5, ?6, ?7, ?8)",
                (
                    1_i64,
                    1_i64,
                    Option::<i64>::None,
                    0_i64,
                    -1_i64,
                    20_i64,
                    "/tmp/invalid-status.org",
                    "/tmp/invalid-status.org",
                ),
            )
            .expect("heading insert should succeed");

        let error = connection
            .execute(
                "INSERT INTO links
                 (id, file_id, heading_id, byte_start, byte_end, line, source_context, format,
                  raw, raw_target, raw_description, link_type, path, search_option,
                  resolution_status, resolution_diagnostic)
                 VALUES (?1, ?2, ?3, ?4, ?5, ?6, ?7, ?8, ?9, ?10, ?11, ?12, ?13, ?14,
                         ?15, ?16)",
                params![
                    1_i64,
                    1_i64,
                    1_i64,
                    0_i64,
                    10_i64,
                    1_i64,
                    "normal",
                    "plain",
                    "id:stale",
                    "id:stale",
                    Option::<String>::None,
                    "id",
                    "stale",
                    Option::<String>::None,
                    "stale",
                    Option::<String>::None,
                ],
            )
            .expect_err("invalid resolution_status should fail");
        assert!(
            error.to_string().contains("CHECK constraint failed"),
            "expected CHECK constraint failure, got {error}"
        );
    }

    fn insert_fixture_graph(connection: &Connection) {
        connection
            .execute(
                "INSERT INTO files (id, path, mtime_ns, size) VALUES (?1, ?2, ?3, ?4)",
                (1_i64, "/tmp/source.org", 10_i64, 20_i64),
            )
            .expect("source file insert should succeed");
        connection
            .execute(
                "INSERT INTO files (id, path, mtime_ns, size) VALUES (?1, ?2, ?3, ?4)",
                (2_i64, "/tmp/target.org", 30_i64, 40_i64),
            )
            .expect("target file insert should succeed");

        connection
            .execute(
                "INSERT INTO headings
                 (id, file_id, parent_id, level, byte_start, byte_end, title, title_raw)
                 VALUES (?1, ?2, ?3, ?4, ?5, ?6, ?7, ?8)",
                (
                    1_i64,
                    1_i64,
                    Option::<i64>::None,
                    0_i64,
                    0_i64,
                    20_i64,
                    "/tmp/source.org",
                    "/tmp/source.org",
                ),
            )
            .expect("source level 0 heading should insert");
        connection
            .execute(
                "INSERT INTO headings
                 (id, file_id, parent_id, level, byte_start, byte_end, title, title_raw, todo_keyword, todo_type)
                 VALUES (?1, ?2, ?3, ?4, ?5, ?6, ?7, ?8, ?9, ?10)",
                (2_i64, 1_i64, Some(1_i64), 1_i64, 1_i64, 10_i64, "Source child", "Source child", "TODO", "open"),
            )
            .expect("source child heading should insert");
        connection
            .execute(
                "INSERT INTO headings
                 (id, file_id, parent_id, level, byte_start, byte_end, title, title_raw)
                 VALUES (?1, ?2, ?3, ?4, ?5, ?6, ?7, ?8)",
                (
                    3_i64,
                    2_i64,
                    Option::<i64>::None,
                    0_i64,
                    0_i64,
                    20_i64,
                    "/tmp/target.org",
                    "/tmp/target.org",
                ),
            )
            .expect("target level 0 heading should insert");

        connection
            .execute(
                "INSERT INTO todo_keywords
                 (file_id, keyword, state_type, shortcut, sequence_no, source_kind,
                  source_keyword, source_line_number)
                 VALUES (?1, ?2, ?3, ?4, ?5, ?6, ?7, ?8)",
                (
                    1_i64,
                    "TODO",
                    "open",
                    Option::<String>::None,
                    0_i64,
                    "config_default",
                    Option::<String>::None,
                    Option::<i64>::None,
                ),
            )
            .expect("todo keyword insert should succeed");
        connection
            .execute(
                "INSERT INTO keywords (heading_id, keyword, value, line_number)
                 VALUES (?1, ?2, ?3, ?4)",
                (1_i64, "TITLE", "Source", 1_i64),
            )
            .expect("keyword insert should succeed");
        connection
            .execute(
                "INSERT INTO properties (heading_id, key, value, source, append, line_number)
                 VALUES (?1, ?2, ?3, ?4, ?5, ?6)",
                (
                    2_i64,
                    "CUSTOM_ID",
                    "child-1",
                    "property_drawer",
                    0_i64,
                    2_i64,
                ),
            )
            .expect("property insert should succeed");
        connection
            .execute(
                "INSERT INTO tags (heading_id, tag) VALUES (?1, ?2)",
                (2_i64, "project"),
            )
            .expect("tag insert should succeed");
        connection
            .execute(
                "INSERT INTO links
                 (id, file_id, heading_id, byte_start, byte_end, line, source_context, format,
                 raw, raw_target, raw_description, link_type, path, search_option,
                  path_absolute, target_file_id, target_heading_id, target_custom_id, target_id,
                  resolution_status, resolution_diagnostic)
                 VALUES (?1, ?2, ?3, ?4, ?5, ?6, ?7, ?8, ?9, ?10, ?11, ?12, ?13, ?14,
                         ?15, ?16, ?17, ?18, ?19, ?20, ?21)",
                params![
                    1_i64,
                    1_i64,
                    2_i64,
                    5_i64,
                    15_i64,
                    2_i64,
                    "heading",
                    "bracket",
                    "[[file:target.org]]",
                    "file:target.org",
                    Option::<String>::None,
                    Some("file".to_string()),
                    "target.org",
                    Option::<String>::None,
                    Option::<String>::None,
                    Some(2_i64),
                    Some(3_i64),
                    Option::<String>::None,
                    Option::<String>::None,
                    Some("resolved".to_string()),
                    Option::<String>::None,
                ],
            )
            .expect("link insert should succeed");
        connection
            .execute(
                "INSERT INTO heading_bodies (heading_id, body_text, body_byte_start, body_byte_end)
                 VALUES (?1, ?2, ?3, ?4)",
                (2_i64, "Body", 11_i64, 15_i64),
            )
            .expect("heading body insert should succeed");
        connection
            .execute(
                "INSERT INTO outline_path (heading_id, file_id, parent_id, depth, materialized_path, breadcrumbs_json)
                 VALUES (?1, ?2, ?3, ?4, ?5, ?6)",
                (2_i64, 1_i64, Some(1_i64), 1_i64, "0000.0001", "[\"/tmp/source.org\",\"Source child\"]"),
            )
            .expect("outline path insert should succeed");
    }
}
