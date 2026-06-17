use std::{
    error::Error,
    fmt,
    path::{Path, PathBuf},
};

use rusqlite::Connection;

pub mod reader;
pub mod schema;
pub mod writer;

pub use reader::DbReader;
pub use schema::SchemaDefinition;
pub use writer::DbWriter;

pub const IN_MEMORY_DATABASE: &str = ":memory:";

pub fn open_database(path: impl AsRef<Path>) -> Result<Connection, DbError> {
    let path = path.as_ref();
    let target = path.display().to_string();
    let connection = Connection::open(path).map_err(|source| DbError::Open {
        path: path.to_path_buf(),
        source,
    })?;
    initialize_database(&connection, &target, &SchemaDefinition::default())?;
    Ok(connection)
}

pub fn open_in_memory_database() -> Result<Connection, DbError> {
    let connection =
        Connection::open_in_memory().map_err(|source| DbError::OpenInMemory { source })?;
    initialize_database(
        &connection,
        IN_MEMORY_DATABASE,
        &SchemaDefinition::default(),
    )?;
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
    use super::{open_database, open_in_memory_database};
    use std::{
        fs,
        path::{Path, PathBuf},
        time::{SystemTime, UNIX_EPOCH},
    };

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
    fn file_backed_database_uses_wal_mode() {
        let test_dir = TestDir::new("wal");
        let database_path = test_dir.path().join("org-files-db.sqlite");

        let connection = open_database(&database_path).expect("database should open");
        let journal_mode: String = connection
            .pragma_query_value(None, "journal_mode", |row| row.get(0))
            .expect("journal_mode pragma should be readable");

        assert_eq!(journal_mode.to_ascii_lowercase(), "wal");
    }
}
