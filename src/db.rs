use std::{
    error::Error,
    fmt,
    path::{Path, PathBuf},
};

use rusqlite::Connection;

pub mod reader;
pub mod schema;
pub mod writer;

pub use reader::{DbReadError, DbReader, HeadingListRow};
pub use schema::{sqlite_supports_fts5, SchemaDefinition};
pub use writer::{
    DbWriteError, DbWriter, FileRecordInput, HeadingFtsRecord, HeadingRecord, KeywordRecord,
    OutlinePathRecord, PropertyRecord, TagRecord, TimestampRecord, TimestampRepeaterRecord,
    TodoKeywordRecord,
};

pub const IN_MEMORY_DATABASE: &str = ":memory:";

pub fn open_database(path: impl AsRef<Path>) -> Result<Connection, DbError> {
    open_database_with_schema(path, &SchemaDefinition::default())
}

pub fn open_database_with_schema(
    path: impl AsRef<Path>,
    schema: &SchemaDefinition,
) -> Result<Connection, DbError> {
    let path = path.as_ref();
    let target = path.display().to_string();
    let connection = Connection::open(path).map_err(|source| DbError::Open {
        path: path.to_path_buf(),
        source,
    })?;
    initialize_database(&connection, &target, schema)?;
    Ok(connection)
}

pub fn open_in_memory_database() -> Result<Connection, DbError> {
    open_in_memory_database_with_schema(&SchemaDefinition::default())
}

pub fn open_in_memory_database_with_schema(
    schema: &SchemaDefinition,
) -> Result<Connection, DbError> {
    let connection =
        Connection::open_in_memory().map_err(|source| DbError::OpenInMemory { source })?;
    initialize_database(&connection, IN_MEMORY_DATABASE, schema)?;
    Ok(connection)
}

fn initialize_database(
    connection: &Connection,
    target: &str,
    schema: &SchemaDefinition,
) -> Result<(), DbError> {
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
    connection
        .pragma_update(None, "user_version", schema.version)
        .map_err(|source| DbError::Initialize {
            target: target.to_string(),
            source,
        })?;
    schema
        .apply(connection)
        .map_err(|source| DbError::Initialize {
            target: target.to_string(),
            source,
        })?;
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
        }
    }
}

impl Error for DbError {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        match self {
            Self::Open { source, .. }
            | Self::OpenInMemory { source }
            | Self::Initialize { source, .. } => Some(source),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::{
        open_database, open_in_memory_database, open_in_memory_database_with_schema,
        sqlite_supports_fts5, SchemaDefinition,
    };
    use rusqlite::Connection;
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
        assert_eq!(user_version, 1);
    }

    #[test]
    fn applies_schema_idempotently_without_fts() {
        let schema = SchemaDefinition::new(1, false);
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
    fn applies_schema_with_fts_when_supported() {
        let probe = Connection::open_in_memory().expect("probe connection should open");
        if !sqlite_supports_fts5(&probe) {
            return;
        }

        let schema = SchemaDefinition::new(1, true);
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
        let schema = SchemaDefinition::new(1, false);
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
        let schema = SchemaDefinition::new(1, false);
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
        let schema = SchemaDefinition::new(1, false);
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
    fn deleting_resolved_targets_keeps_source_links() {
        let schema = SchemaDefinition::new(1, false);
        let connection =
            open_in_memory_database_with_schema(&schema).expect("database should open");

        insert_fixture_graph(&connection);

        connection
            .execute("DELETE FROM headings WHERE id = 3", [])
            .expect("resolved target heading should delete");

        let (source_link_count, resolved_heading_id): (i64, Option<i64>) = connection
            .query_row(
                "SELECT COUNT(*), MIN(resolved_heading_id) FROM links WHERE id = 1",
                [],
                |row| Ok((row.get(0)?, row.get(1)?)),
            )
            .expect("source link should remain queryable");

        assert_eq!(source_link_count, 1);
        assert_eq!(resolved_heading_id, None);

        connection
            .execute("DELETE FROM files WHERE id = 2", [])
            .expect("resolved target file should delete");

        let resolved_file_id: Option<i64> = connection
            .query_row(
                "SELECT resolved_file_id FROM links WHERE id = 1",
                [],
                |row| row.get(0),
            )
            .expect("source link should remain queryable");

        assert_eq!(resolved_file_id, None);
    }

    #[test]
    fn supports_per_file_rebuild_while_keeping_file_row() {
        let schema = SchemaDefinition::new(1, false);
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
        let schema = SchemaDefinition::new(1, false);
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
        if !sqlite_supports_fts5(&probe) {
            return;
        }

        let schema = SchemaDefinition::new(1, true);
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

        assert_eq!(match_count, 1);
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
                "INSERT INTO todo_keywords (file_id, keyword, state_type, shortcut, sequence_no)
                 VALUES (?1, ?2, ?3, ?4, ?5)",
                (1_i64, "TODO", "open", Option::<String>::None, 0_i64),
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
                "INSERT INTO tags (heading_id, tag, inherited) VALUES (?1, ?2, ?3)",
                (2_i64, "project", 0_i64),
            )
            .expect("tag insert should succeed");
        connection
            .execute(
                "INSERT INTO links
                 (id, file_id, heading_id, byte_start, byte_end, target, raw_link, resolved_file_id, resolved_heading_id, resolved, broken)
                 VALUES (?1, ?2, ?3, ?4, ?5, ?6, ?7, ?8, ?9, ?10, ?11)",
                (
                    1_i64,
                    1_i64,
                    2_i64,
                    5_i64,
                    15_i64,
                    "target.org",
                    "file:target.org",
                    Some(2_i64),
                    Some(3_i64),
                    1_i64,
                    0_i64,
                ),
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
