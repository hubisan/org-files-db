use super::*;

#[derive(Debug, Clone, PartialEq, Eq)]
pub(in crate::indexer) struct FileSnapshot {
    pub(in crate::indexer) mtime_ns: i64,
    pub(in crate::indexer) size: i64,
    pub(in crate::indexer) content_hash: String,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(in crate::indexer) struct CapturedSource {
    pub(in crate::indexer) bytes: Vec<u8>,
    pub(in crate::indexer) snapshot: FileSnapshot,
}

pub(in crate::indexer) fn persisted_file_record(
    persisted: &PersistedFileSnapshot,
    path: &Path,
) -> FileRecordInput {
    FileRecordInput {
        path: path.to_path_buf(),
        identity: persisted
            .identity
            .as_ref()
            .map(|identity| identity.as_bytes().to_vec()),
        mtime_ns: persisted.mtime_ns,
        size: persisted.size,
        content_hash: persisted.content_hash.clone(),
        indexed_at: None,
    }
}

#[derive(Debug, Clone)]
pub(in crate::indexer) struct PersistedFileSnapshot {
    pub(in crate::indexer) file_id: i64,
    pub(in crate::indexer) path: String,
    pub(in crate::indexer) identity: Option<FileIdentity>,
    pub(in crate::indexer) mtime_ns: i64,
    pub(in crate::indexer) size: i64,
    pub(in crate::indexer) content_hash: Option<String>,
}

pub(in crate::indexer) enum PersistedFileSnapshots {
    Valid(Vec<PersistedFileSnapshot>),
    InvalidIdentity,
}

pub(in crate::indexer) trait FileSnapshotReader {
    fn metadata(&mut self, path: &Path) -> Result<FileMetadata, IndexerError>;
    fn read_bytes(&mut self, path: &Path) -> Result<Vec<u8>, IndexerError>;
}

pub(in crate::indexer) struct FilesystemSnapshotReader;

#[derive(Debug, Clone, PartialEq, Eq)]
pub(in crate::indexer) struct FileMetadata {
    pub(in crate::indexer) mtime_ns: i64,
    pub(in crate::indexer) size: i64,
}

impl FileSnapshotReader for FilesystemSnapshotReader {
    fn metadata(&mut self, path: &Path) -> Result<FileMetadata, IndexerError> {
        file_metadata(path)
    }

    fn read_bytes(&mut self, path: &Path) -> Result<Vec<u8>, IndexerError> {
        fs::read(path).map_err(|source| IndexerError::ReadFile {
            path: path.to_path_buf(),
            source,
        })
    }
}

pub(in crate::indexer) fn capture_stable_source(
    path: &Path,
) -> Result<CapturedSource, IndexerError> {
    capture_stable_source_with(&mut FilesystemSnapshotReader, path)
}

pub(in crate::indexer) fn capture_stable_source_with(
    reader: &mut impl FileSnapshotReader,
    path: &Path,
) -> Result<CapturedSource, IndexerError> {
    for _ in 0..2 {
        let before = reader.metadata(path)?;
        let bytes = reader.read_bytes(path)?;
        let after = reader.metadata(path)?;
        if before != after {
            continue;
        }

        let content_hash = format!("sha256:{}", encode_lower(Sha256::digest(&bytes)));
        return Ok(CapturedSource {
            bytes,
            snapshot: FileSnapshot {
                mtime_ns: before.mtime_ns,
                size: before.size,
                content_hash,
            },
        });
    }

    Err(IndexerError::UnstableFileSnapshot {
        path: path.to_path_buf(),
    })
}

pub(in crate::indexer) fn decode_captured_source(
    path: &Path,
    bytes: Vec<u8>,
) -> Result<String, IndexerError> {
    String::from_utf8(bytes).map_err(|source| IndexerError::ReadFile {
        path: path.to_path_buf(),
        source: io::Error::new(io::ErrorKind::InvalidData, source),
    })
}

pub(in crate::indexer) fn qualified_sha256_hash(value: Option<&str>) -> Option<&str> {
    let value = value?;
    let digest = value.strip_prefix("sha256:")?;
    (digest.len() == 64
        && digest
            .bytes()
            .all(|byte| byte.is_ascii_digit() || matches!(byte, b'a'..=b'f')))
    .then_some(value)
}

pub(in crate::indexer) fn sqlite_fts5_available_read_only(
    connection: &Connection,
) -> rusqlite::Result<bool> {
    connection
        .query_row(
            "SELECT EXISTS(SELECT 1 FROM pragma_module_list WHERE name = 'fts5')",
            [],
            |row| row.get::<_, i64>(0),
        )
        .map(|value| value != 0)
}

pub(in crate::indexer) fn load_persisted_file_snapshots(
    connection: &Connection,
) -> Result<PersistedFileSnapshots, IndexerError> {
    let mut statement = connection
        .prepare(
            "SELECT id, path, identity, mtime_ns, size, content_hash
             FROM files",
        )
        .map_err(|source| {
            IndexerError::Database(DbError::Inspect {
                target: "persisted file snapshots".to_string(),
                source,
            })
        })?;
    let mut rows = statement.query([]).map_err(|source| {
        IndexerError::Database(DbError::Inspect {
            target: "persisted file snapshots".to_string(),
            source,
        })
    })?;
    let mut snapshots = Vec::new();
    while let Some(row) = rows.next().map_err(|source| {
        IndexerError::Database(DbError::Inspect {
            target: "persisted file snapshots".to_string(),
            source,
        })
    })? {
        let values = (|| -> rusqlite::Result<_> {
            Ok((
                row.get_ref(0)?,
                row.get_ref(1)?,
                row.get_ref(2)?,
                row.get_ref(3)?,
                row.get_ref(4)?,
                row.get_ref(5)?,
            ))
        })()
        .map_err(|source| {
            IndexerError::Database(DbError::Inspect {
                target: "persisted file snapshots".to_string(),
                source,
            })
        })?;
        let (
            ValueRef::Integer(file_id),
            ValueRef::Text(path),
            identity,
            ValueRef::Integer(mtime_ns),
            ValueRef::Integer(size),
            content_hash,
        ) = values
        else {
            return Ok(PersistedFileSnapshots::InvalidIdentity);
        };
        let Ok(path) = std::str::from_utf8(path) else {
            return Ok(PersistedFileSnapshots::InvalidIdentity);
        };
        if path.is_empty() || size < 0 {
            return Ok(PersistedFileSnapshots::InvalidIdentity);
        }
        let identity = match identity {
            ValueRef::Blob(identity) => match FileIdentity::from_stored_bytes(identity.to_vec()) {
                Some(identity) => Some(identity),
                None => return Ok(PersistedFileSnapshots::InvalidIdentity),
            },
            ValueRef::Null => None,
            ValueRef::Integer(_) | ValueRef::Real(_) | ValueRef::Text(_) => {
                return Ok(PersistedFileSnapshots::InvalidIdentity)
            }
        };
        let content_hash = match content_hash {
            ValueRef::Text(value) => std::str::from_utf8(value).ok().map(str::to_owned),
            ValueRef::Null | ValueRef::Integer(_) | ValueRef::Real(_) | ValueRef::Blob(_) => None,
        };
        snapshots.push(PersistedFileSnapshot {
            file_id,
            path: path.to_owned(),
            identity,
            mtime_ns,
            size,
            content_hash,
        });
    }
    snapshots.sort_by(|left, right| {
        let left = left
            .identity
            .as_ref()
            .map(|identity| identity.as_bytes())
            .unwrap_or(left.path.as_bytes());
        let right = right
            .identity
            .as_ref()
            .map(|identity| identity.as_bytes())
            .unwrap_or(right.path.as_bytes());
        left.cmp(right)
    });
    Ok(PersistedFileSnapshots::Valid(snapshots))
}

pub(in crate::indexer) fn error_path(error: &IndexerError) -> PathBuf {
    match error {
        IndexerError::Discover { path, .. }
        | IndexerError::InvalidFileMetadata { path, .. }
        | IndexerError::UnstableFileSnapshot { path }
        | IndexerError::Parse { path, .. }
        | IndexerError::ReadFile { path, .. } => path.clone(),
        _ => PathBuf::new(),
    }
}

pub(in crate::indexer) fn file_metadata(path: &Path) -> Result<FileMetadata, IndexerError> {
    let metadata = fs::metadata(path).map_err(|source| IndexerError::ReadFile {
        path: path.to_path_buf(),
        source,
    })?;
    let modified = metadata
        .modified()
        .map_err(|source| IndexerError::ReadFile {
            path: path.to_path_buf(),
            source,
        })?;
    let modified_ns = modified
        .duration_since(UNIX_EPOCH)
        .map_err(|_| IndexerError::InvalidFileMetadata {
            path: path.to_path_buf(),
            field: "mtime_ns",
        })?
        .as_nanos();
    let size = metadata.len();
    Ok(FileMetadata {
        mtime_ns: i64::try_from(modified_ns).map_err(|_| IndexerError::InvalidFileMetadata {
            path: path.to_path_buf(),
            field: "mtime_ns",
        })?,
        size: i64::try_from(size).map_err(|_| IndexerError::InvalidFileMetadata {
            path: path.to_path_buf(),
            field: "size",
        })?,
    })
}

pub(in crate::indexer) fn build_file_record(
    path: &Path,
    identity: &FileIdentity,
    snapshot: &FileSnapshot,
) -> Result<FileRecordInput, IndexerError> {
    Ok(FileRecordInput {
        path: path.to_path_buf(),
        identity: Some(identity.as_bytes().to_vec()),
        mtime_ns: snapshot.mtime_ns,
        size: snapshot.size,
        content_hash: Some(snapshot.content_hash.clone()),
        indexed_at: None,
    })
}

pub(in crate::indexer) fn file_record_for_write(
    prepared_record: &FileRecordInput,
    path: &Path,
) -> Result<FileRecordInput, IndexerError> {
    let indexed_at = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .map_err(|_| IndexerError::InvalidFileMetadata {
            path: path.to_path_buf(),
            field: "indexed_at",
        })?
        .as_secs();

    Ok(FileRecordInput {
        indexed_at: Some(i64::try_from(indexed_at).map_err(|_| {
            IndexerError::InvalidFileMetadata {
                path: path.to_path_buf(),
                field: "indexed_at",
            }
        })?),
        ..prepared_record.clone()
    })
}

pub(in crate::indexer) fn snapshot_matches_record(
    path: &Path,
    record: &FileRecordInput,
) -> Result<bool, IndexerError> {
    let captured = capture_stable_source(path)?;
    Ok(captured.snapshot.mtime_ns == record.mtime_ns
        && captured.snapshot.size == record.size
        && record.content_hash.as_deref() == Some(captured.snapshot.content_hash.as_str()))
}

pub(in crate::indexer) fn metadata_matches_record(
    path: &Path,
    record: &FileRecordInput,
) -> Result<bool, IndexerError> {
    let metadata = file_metadata(path)?;
    Ok(metadata.mtime_ns == record.mtime_ns && metadata.size == record.size)
}

pub(in crate::indexer) fn same_persisted_file_baseline(
    expected: &[PersistedFileSnapshot],
    current: &[PersistedFileSnapshot],
) -> bool {
    expected.len() == current.len()
        && expected.iter().zip(current).all(|(left, right)| {
            left.file_id == right.file_id
                && left.path == right.path
                && left.identity == right.identity
                && left.mtime_ns == right.mtime_ns
                && left.size == right.size
                && left.content_hash == right.content_hash
        })
}

pub(in crate::indexer) fn row_matches_file_record(
    connection: &Connection,
    file_id: i64,
    record: &FileRecordInput,
) -> Result<bool, IndexerError> {
    let expected_path = display_path(&record.path);
    let found = connection
        .query_row(
            "SELECT path, identity, mtime_ns, size, content_hash FROM files WHERE id = ?1",
            [file_id],
            |row| {
                Ok((
                    row.get::<_, String>(0)?,
                    row.get::<_, Option<Vec<u8>>>(1)?,
                    row.get::<_, i64>(2)?,
                    row.get::<_, i64>(3)?,
                    row.get::<_, Option<String>>(4)?,
                ))
            },
        )
        .optional()
        .map_err(|source| {
            IndexerError::Write(DbWriteError::ReadBack {
                operation: "apply_change_plan.file_baseline",
                source,
            })
        })?;
    Ok(found.is_some_and(|(path, identity, mtime, size, hash)| {
        path == expected_path
            && identity == record.identity
            && mtime == record.mtime_ns
            && size == record.size
            && hash == record.content_hash
    }))
}
