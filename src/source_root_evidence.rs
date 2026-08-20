use std::{
    collections::BTreeMap, error::Error, fmt, fs, io, os::unix::fs::MetadataExt, path::PathBuf,
};

use rusqlite::{types::ValueRef, Connection, OptionalExtension};
use serde::{Deserialize, Serialize};

use crate::{
    config::{normalize_syntactic_path, Config},
    db::{
        DbWriteError, DbWriter, DB_METADATA_SOURCE_ROOT_EVIDENCE_KEY,
        DB_METADATA_SOURCE_ROOT_EVIDENCE_VERSION_KEY,
    },
    file_identity::{display_path, FileIdentity},
};

pub(crate) const SOURCE_ROOT_EVIDENCE_CONTRACT_VERSION: &str = "1";

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct SourceRootEvidenceSet {
    roots: Vec<SourceRootEvidence>,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
struct SourceRootEvidence {
    logical_path: FileIdentity,
    canonical_path: FileIdentity,
    device: u64,
    inode: u64,
    recursive: bool,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum SourceRootEvidencePolicy {
    Automatic,
    AcceptChanges,
}

impl SourceRootEvidenceSet {
    pub(crate) fn capture(config: &Config) -> Result<Self, SourceRootEvidenceError> {
        let mut roots = Vec::with_capacity(config.dirs.len());
        for configured in &config.dirs {
            let logical_path = normalize_syntactic_path(configured.path.clone());
            let canonical_path = fs::canonicalize(&logical_path).map_err(|source| {
                SourceRootEvidenceError::Inspect {
                    path: logical_path.clone(),
                    source,
                }
            })?;
            let metadata = fs::metadata(&canonical_path).map_err(|source| {
                SourceRootEvidenceError::Inspect {
                    path: logical_path.clone(),
                    source,
                }
            })?;
            if !metadata.is_dir() {
                return Err(SourceRootEvidenceError::NotDirectory { path: logical_path });
            }
            roots.push(SourceRootEvidence {
                logical_path: FileIdentity::from_canonical_path(&logical_path),
                canonical_path: FileIdentity::from_canonical_path(&canonical_path),
                device: metadata.dev(),
                inode: metadata.ino(),
                recursive: configured.recursive,
            });
        }
        roots.sort();
        roots.dedup();
        validate_consistent_logical_roots(&roots)
            .map_err(|path| SourceRootEvidenceError::InconsistentConfiguration { path })?;
        Ok(Self { roots })
    }

    pub(crate) fn validate_committed(
        &self,
        connection: &Connection,
        policy: SourceRootEvidencePolicy,
    ) -> Result<(), SourceRootEvidenceError> {
        if self.roots.is_empty() {
            return Ok(());
        }

        let stored = load_stored_evidence(connection)?;
        let existing_files = indexed_file_count(connection)?;
        match stored {
            StoredSourceRootEvidence::Missing => {
                if existing_files == 0 || policy == SourceRootEvidencePolicy::AcceptChanges {
                    Ok(())
                } else {
                    Err(SourceRootEvidenceError::MissingCommittedEvidence {
                        existing_indexed_files: existing_files,
                    })
                }
            }
            StoredSourceRootEvidence::Invalid => {
                if existing_files == 0 || policy == SourceRootEvidencePolicy::AcceptChanges {
                    Ok(())
                } else {
                    Err(SourceRootEvidenceError::InvalidCommittedEvidence {
                        existing_indexed_files: existing_files,
                    })
                }
            }
            StoredSourceRootEvidence::Valid(previous) => {
                if policy == SourceRootEvidencePolicy::AcceptChanges {
                    Ok(())
                } else {
                    previous.ensure_continuity(self)
                }
            }
        }
    }

    pub(crate) fn ensure_unchanged(&self, current: &Self) -> Result<(), SourceRootEvidenceError> {
        if self == current {
            return Ok(());
        }
        let expected = evidence_by_logical_path(&self.roots);
        let observed = evidence_by_logical_path(&current.roots);
        for (logical_path, expected_root) in &expected {
            let Some(observed_root) = observed.get(logical_path) else {
                return Err(SourceRootEvidenceError::ChangedDuringReconciliation {
                    path: identity_path(&expected_root.logical_path),
                });
            };
            if !same_root_identity(expected_root, observed_root) {
                return Err(SourceRootEvidenceError::ChangedDuringReconciliation {
                    path: identity_path(&expected_root.logical_path),
                });
            }
        }
        for (logical_path, observed_root) in &observed {
            if !expected.contains_key(logical_path) {
                return Err(SourceRootEvidenceError::ChangedDuringReconciliation {
                    path: identity_path(&observed_root.logical_path),
                });
            }
        }
        Err(SourceRootEvidenceError::ChangedDuringReconciliation {
            path: PathBuf::from("<configured source roots>"),
        })
    }

    pub(crate) fn persist(&self, connection: &Connection) -> Result<(), SourceRootEvidenceError> {
        let stored = StoredSourceRootEvidenceSet {
            roots: self.roots.iter().map(StoredSourceRoot::from).collect(),
        };
        let serialized =
            serde_json::to_string(&stored).map_err(SourceRootEvidenceError::Serialize)?;
        DbWriter::set_metadata_value(
            connection,
            DB_METADATA_SOURCE_ROOT_EVIDENCE_VERSION_KEY,
            SOURCE_ROOT_EVIDENCE_CONTRACT_VERSION,
        )
        .map_err(SourceRootEvidenceError::Database)?;
        DbWriter::set_metadata_value(
            connection,
            DB_METADATA_SOURCE_ROOT_EVIDENCE_KEY,
            &serialized,
        )
        .map_err(SourceRootEvidenceError::Database)
    }

    fn ensure_continuity(&self, current: &Self) -> Result<(), SourceRootEvidenceError> {
        let previous = evidence_by_logical_path(&self.roots);
        let current = evidence_by_logical_path(&current.roots);
        for (logical_path, current_root) in current {
            let Some(previous_root) = previous.get(&logical_path) else {
                continue;
            };
            if previous_root.canonical_path != current_root.canonical_path {
                return Err(SourceRootEvidenceError::CommittedRootChanged {
                    path: identity_path(&current_root.logical_path),
                    reason: "canonical target changed",
                });
            }
            if previous_root.device != current_root.device
                || previous_root.inode != current_root.inode
            {
                return Err(SourceRootEvidenceError::CommittedRootChanged {
                    path: identity_path(&current_root.logical_path),
                    reason: "native directory identity changed",
                });
            }
        }
        Ok(())
    }
}

fn same_root_identity(left: &SourceRootEvidence, right: &SourceRootEvidence) -> bool {
    left.logical_path == right.logical_path
        && left.canonical_path == right.canonical_path
        && left.device == right.device
        && left.inode == right.inode
        && left.recursive == right.recursive
}

fn evidence_by_logical_path(
    roots: &[SourceRootEvidence],
) -> BTreeMap<Vec<u8>, &SourceRootEvidence> {
    roots
        .iter()
        .map(|root| (root.logical_path.as_bytes().to_vec(), root))
        .collect()
}

fn validate_consistent_logical_roots(roots: &[SourceRootEvidence]) -> Result<(), PathBuf> {
    let mut by_logical = BTreeMap::<Vec<u8>, &SourceRootEvidence>::new();
    for root in roots {
        let key = root.logical_path.as_bytes().to_vec();
        if let Some(previous) = by_logical.insert(key, root) {
            if previous.canonical_path != root.canonical_path
                || previous.device != root.device
                || previous.inode != root.inode
                || previous.recursive != root.recursive
            {
                return Err(identity_path(&root.logical_path));
            }
        }
    }
    Ok(())
}

fn identity_path(identity: &FileIdentity) -> PathBuf {
    identity
        .to_path()
        .unwrap_or_else(|| PathBuf::from("<invalid stored source-root path>"))
}

#[derive(Debug, Serialize, Deserialize)]
struct StoredSourceRootEvidenceSet {
    roots: Vec<StoredSourceRoot>,
}

#[derive(Debug, Serialize, Deserialize)]
struct StoredSourceRoot {
    logical_path: String,
    canonical_path: String,
    device: u64,
    inode: u64,
    recursive: bool,
}

impl From<&SourceRootEvidence> for StoredSourceRoot {
    fn from(value: &SourceRootEvidence) -> Self {
        Self {
            logical_path: encode_hex(value.logical_path.as_bytes()),
            canonical_path: encode_hex(value.canonical_path.as_bytes()),
            device: value.device,
            inode: value.inode,
            recursive: value.recursive,
        }
    }
}

fn decode_stored_root(value: StoredSourceRoot) -> Option<SourceRootEvidence> {
    Some(SourceRootEvidence {
        logical_path: decode_identity(&value.logical_path)?,
        canonical_path: decode_identity(&value.canonical_path)?,
        device: value.device,
        inode: value.inode,
        recursive: value.recursive,
    })
}

enum StoredSourceRootEvidence {
    Missing,
    Invalid,
    Valid(SourceRootEvidenceSet),
}

enum LoadedMetadata {
    Missing,
    Text(String),
    Malformed,
}

fn load_stored_evidence(
    connection: &Connection,
) -> Result<StoredSourceRootEvidence, SourceRootEvidenceError> {
    let version = load_metadata(connection, DB_METADATA_SOURCE_ROOT_EVIDENCE_VERSION_KEY)?;
    let evidence = load_metadata(connection, DB_METADATA_SOURCE_ROOT_EVIDENCE_KEY)?;
    let (version, evidence) = match (version, evidence) {
        (LoadedMetadata::Missing, LoadedMetadata::Missing) => {
            return Ok(StoredSourceRootEvidence::Missing)
        }
        (LoadedMetadata::Text(version), LoadedMetadata::Text(evidence)) => (version, evidence),
        _ => return Ok(StoredSourceRootEvidence::Invalid),
    };
    if version != SOURCE_ROOT_EVIDENCE_CONTRACT_VERSION {
        return Ok(StoredSourceRootEvidence::Invalid);
    }
    let stored = match serde_json::from_str::<StoredSourceRootEvidenceSet>(&evidence) {
        Ok(stored) => stored,
        Err(_) => return Ok(StoredSourceRootEvidence::Invalid),
    };
    let mut roots = Vec::with_capacity(stored.roots.len());
    for root in stored.roots {
        let Some(root) = decode_stored_root(root) else {
            return Ok(StoredSourceRootEvidence::Invalid);
        };
        roots.push(root);
    }
    roots.sort();
    roots.dedup();
    if validate_consistent_logical_roots(&roots).is_err() {
        return Ok(StoredSourceRootEvidence::Invalid);
    }
    Ok(StoredSourceRootEvidence::Valid(SourceRootEvidenceSet {
        roots,
    }))
}

fn load_metadata(
    connection: &Connection,
    key: &'static str,
) -> Result<LoadedMetadata, SourceRootEvidenceError> {
    connection
        .query_row(
            "SELECT value FROM db_metadata WHERE key = ?1",
            [key],
            |row| {
                let value = row.get_ref(0)?;
                Ok(match value {
                    ValueRef::Text(bytes) => match std::str::from_utf8(bytes) {
                        Ok(value) => LoadedMetadata::Text(value.to_owned()),
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
        .map_err(|source| {
            SourceRootEvidenceError::Database(DbWriteError::ReadBack {
                operation: "load_source_root_evidence",
                source,
            })
        })
}

fn indexed_file_count(connection: &Connection) -> Result<usize, SourceRootEvidenceError> {
    let count = connection
        .query_row("SELECT COUNT(*) FROM files", [], |row| row.get::<_, i64>(0))
        .map_err(|source| {
            SourceRootEvidenceError::Database(DbWriteError::ReadBack {
                operation: "count_files_for_source_root_evidence",
                source,
            })
        })?;
    usize::try_from(count).map_err(|_| SourceRootEvidenceError::InvalidIndexedFileCount { count })
}

fn encode_hex(bytes: &[u8]) -> String {
    let mut output = String::with_capacity(bytes.len() * 2);
    for byte in bytes {
        output.push(hex_digit(byte >> 4));
        output.push(hex_digit(byte & 0x0f));
    }
    output
}

fn decode_identity(value: &str) -> Option<FileIdentity> {
    FileIdentity::from_stored_bytes(decode_hex(value)?)
}

fn decode_hex(value: &str) -> Option<Vec<u8>> {
    if !value.len().is_multiple_of(2) {
        return None;
    }
    value
        .as_bytes()
        .as_chunks::<2>()
        .0
        .iter()
        .map(|pair| Some((hex_value(pair[0])? << 4) | hex_value(pair[1])?))
        .collect()
}

fn hex_digit(value: u8) -> char {
    match value {
        0..=9 => char::from(b'0' + value),
        _ => char::from(b'a' + value - 10),
    }
}

fn hex_value(value: u8) -> Option<u8> {
    match value {
        b'0'..=b'9' => Some(value - b'0'),
        b'a'..=b'f' => Some(value - b'a' + 10),
        b'A'..=b'F' => Some(value - b'A' + 10),
        _ => None,
    }
}

#[derive(Debug)]
pub(crate) enum SourceRootEvidenceError {
    Inspect { path: PathBuf, source: io::Error },
    NotDirectory { path: PathBuf },
    InconsistentConfiguration { path: PathBuf },
    MissingCommittedEvidence { existing_indexed_files: usize },
    InvalidCommittedEvidence { existing_indexed_files: usize },
    CommittedRootChanged { path: PathBuf, reason: &'static str },
    ChangedDuringReconciliation { path: PathBuf },
    InvalidIndexedFileCount { count: i64 },
    Serialize(serde_json::Error),
    Database(DbWriteError),
}

impl fmt::Display for SourceRootEvidenceError {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Inspect { path, source } => write!(
                formatter,
                "failed to inspect configured source root {}: {source}",
                display_path(path)
            ),
            Self::NotDirectory { path } => write!(
                formatter,
                "configured source root {} is not a directory",
                display_path(path)
            ),
            Self::InconsistentConfiguration { path } => write!(
                formatter,
                "configured source root {} resolves inconsistently within the same configuration",
                display_path(path)
            ),
            Self::MissingCommittedEvidence {
                existing_indexed_files,
            } => write!(
                formatter,
                "database contains {existing_indexed_files} indexed file(s) but has no committed source-root identity evidence; rerun the manual rebuild with --accept-source-root-changes to adopt the current configured roots"
            ),
            Self::InvalidCommittedEvidence {
                existing_indexed_files,
            } => write!(
                formatter,
                "database contains {existing_indexed_files} indexed file(s) but its committed source-root identity evidence is invalid; rerun the manual rebuild with --accept-source-root-changes to adopt the current configured roots"
            ),
            Self::CommittedRootChanged { path, reason } => write!(
                formatter,
                "configured source root {} no longer matches the identity from the last successful commit ({reason}); rerun the manual rebuild with --accept-source-root-changes if this replacement is intentional",
                display_path(path)
            ),
            Self::ChangedDuringReconciliation { path } => write!(
                formatter,
                "configured source root {} changed while reconciliation was in progress; the stale reconciliation was rejected",
                display_path(path)
            ),
            Self::InvalidIndexedFileCount { count } => write!(
                formatter,
                "database returned invalid indexed file count {count} while validating source-root identity"
            ),
            Self::Serialize(source) => {
                write!(formatter, "failed to serialize source-root identity evidence: {source}")
            }
            Self::Database(source) => write!(formatter, "{source}"),
        }
    }
}

impl Error for SourceRootEvidenceError {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        match self {
            Self::Inspect { source, .. } => Some(source),
            Self::Serialize(source) => Some(source),
            Self::Database(source) => Some(source),
            Self::NotDirectory { .. }
            | Self::InconsistentConfiguration { .. }
            | Self::MissingCommittedEvidence { .. }
            | Self::InvalidCommittedEvidence { .. }
            | Self::CommittedRootChanged { .. }
            | Self::ChangedDuringReconciliation { .. }
            | Self::InvalidIndexedFileCount { .. } => None,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::{decode_hex, encode_hex};

    #[test]
    fn hex_encoding_round_trips_native_identity_bytes() {
        let bytes = b"orgfdb-path-v1\0unix\0/tmp/root-\xff";
        assert_eq!(decode_hex(&encode_hex(bytes)), Some(bytes.to_vec()));
    }
}
