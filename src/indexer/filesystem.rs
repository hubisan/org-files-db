use super::*;
use rusqlite::{params, Statement};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(super) enum FilesystemKind {
    File,
    Directory,
}

impl FilesystemKind {
    fn as_str(self) -> &'static str {
        match self {
            Self::File => "file",
            Self::Directory => "directory",
        }
    }
}

pub(super) struct FilesystemEntry {
    pub(super) path: String,
    pub(super) kind: FilesystemKind,
    pub(super) name: String,
    pub(super) parent: Option<String>,
    pub(super) extension: Option<String>,
    pub(super) size: Option<i64>,
    pub(super) mtime_ns: Option<i64>,
}

impl FilesystemEntry {
    pub(super) fn read(path: &Path, kind: FilesystemKind) -> Result<Option<Self>, IndexerError> {
        let metadata = match fs::symlink_metadata(path) {
            Ok(metadata) => metadata,
            Err(source)
                if matches!(
                    source.kind(),
                    io::ErrorKind::NotFound | io::ErrorKind::NotADirectory
                ) =>
            {
                return Ok(None);
            }
            Err(source) => {
                return Err(IndexerError::Discover {
                    path: path.to_path_buf(),
                    source,
                });
            }
        };
        let (extension, size, mtime_ns) = match kind {
            FilesystemKind::File if metadata.is_file() => (
                path.extension()
                    .map(|extension| display_path(Path::new(extension))),
                i64::try_from(metadata.len()).ok(),
                metadata.modified().ok().and_then(epoch_nanoseconds),
            ),
            FilesystemKind::Directory if metadata.is_dir() => (None, None, None),
            _ => return Ok(None),
        };
        Ok(Some(Self {
            path: display_path(path),
            kind,
            name: display_path(path.file_name().map_or(path, Path::new)),
            parent: path.parent().map(display_path),
            extension,
            size,
            mtime_ns,
        }))
    }

    pub(super) fn prepare_upsert(connection: &Connection) -> Result<Statement<'_>, IndexerError> {
        connection
            .prepare(
                "INSERT INTO filesystem_entries (path, kind, name, parent, extension, size, mtime_ns)
                 VALUES (?1, ?2, ?3, ?4, ?5, ?6, ?7)
                 ON CONFLICT(path) DO UPDATE SET
                   kind = excluded.kind, name = excluded.name, parent = excluded.parent,
                   extension = excluded.extension, size = excluded.size, mtime_ns = excluded.mtime_ns
                 WHERE kind IS NOT excluded.kind OR name IS NOT excluded.name
                    OR parent IS NOT excluded.parent OR extension IS NOT excluded.extension
                    OR size IS NOT excluded.size OR mtime_ns IS NOT excluded.mtime_ns",
            )
            .map_err(map_upsert_error)
    }

    pub(super) fn upsert(&self, statement: &mut Statement<'_>) -> Result<bool, IndexerError> {
        statement
            .execute(params![
                self.path,
                self.kind.as_str(),
                self.name,
                self.parent,
                self.extension,
                self.size,
                self.mtime_ns,
            ])
            .map(|rows| rows > 0)
            .map_err(map_upsert_error)
    }
}

fn map_upsert_error(source: rusqlite::Error) -> IndexerError {
    IndexerError::Write(DbWriteError::Write {
        operation: "upsert_filesystem_entry",
        source,
    })
}

fn epoch_nanoseconds(time: SystemTime) -> Option<i64> {
    let nanoseconds = match time.duration_since(UNIX_EPOCH) {
        Ok(duration) => i128::try_from(duration.as_nanos()).ok()?,
        Err(error) => -i128::try_from(error.duration().as_nanos()).ok()?,
    };
    i64::try_from(nanoseconds).ok()
}

#[cfg(test)]
#[test]
fn modification_times_are_signed_epoch_nanoseconds() {
    use std::time::Duration;

    assert_eq!(
        epoch_nanoseconds(UNIX_EPOCH - Duration::new(1, 5)),
        Some(-1_000_000_005)
    );
    assert_eq!(epoch_nanoseconds(UNIX_EPOCH), Some(0));
    assert_eq!(
        epoch_nanoseconds(UNIX_EPOCH + Duration::new(1, 5)),
        Some(1_000_000_005)
    );
}

#[derive(Debug)]
pub(super) struct FilesystemInternalPaths {
    database_paths: BTreeSet<PathBuf>,
    cache_directories: Vec<PathBuf>,
}

impl FilesystemInternalPaths {
    pub(super) fn new(config: &Config) -> Self {
        use crate::presentation_view_cache::{
            presentation_view_cache_root, PRESENTATION_VIEWS_DIRECTORY,
        };

        let db_path = &config.db_path;
        let mut databases = BTreeSet::from([db_path.to_path_buf()]);
        databases.extend(fs::canonicalize(db_path));
        if let Some((parent, name)) = db_path.parent().zip(db_path.file_name()) {
            if let Ok(canonical_parent) = fs::canonicalize(parent) {
                databases.insert(canonical_parent.join(name));
            }
        }

        let mut paths = BTreeSet::new();
        for database in databases {
            for suffix in ["", "-wal", "-shm", "-journal"] {
                let mut path = database.as_os_str().to_os_string();
                path.push(suffix);
                let path = PathBuf::from(path);
                paths.extend(fs::canonicalize(&path));
                paths.insert(path);
            }
        }
        let mut cache_directories = Vec::new();
        if let Ok(cache_root) = presentation_view_cache_root(config) {
            let view_cache =
                normalize_syntactic_path(cache_root.join(PRESENTATION_VIEWS_DIRECTORY));
            // The watcher may create the cache after filesystem discovery.
            // Resolve the closest existing ancestor for aliased cache paths.
            for ancestor in view_cache.ancestors() {
                if let Ok(canonical) = fs::canonicalize(ancestor) {
                    let relative = view_cache.strip_prefix(ancestor).expect("ancestor prefix");
                    cache_directories.push(canonical.join(relative));
                    break;
                }
            }
            cache_directories.push(view_cache);
        }
        Self {
            database_paths: paths,
            cache_directories,
        }
    }

    pub(super) fn contains(&self, path: &Path) -> bool {
        self.database_paths.contains(path)
            || self
                .cache_directories
                .iter()
                .any(|directory| path.starts_with(directory))
    }
}
