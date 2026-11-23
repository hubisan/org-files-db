use self::filesystem::{FilesystemEntry, FilesystemKind};
use super::*;

pub(super) fn generation(connection: &Connection) -> Result<i64, IndexerError> {
    connection
        .query_row(
            "SELECT generation FROM index_state WHERE singleton = 1",
            [],
            |row| row.get(0),
        )
        .map_err(|source| {
            IndexerError::Write(DbWriteError::ReadBack {
                operation: "filesystem_reconciliation.generation",
                source,
            })
        })
}

pub(super) struct FilesystemReconciliation {
    upserts: Vec<FilesystemEntry>,
    deletions: Vec<String>,
}

impl FilesystemReconciliation {
    pub(super) fn prepare(
        connection: &Connection,
        discovered: &BTreeMap<PathBuf, FilesystemKind>,
        scope: &PlanningScope,
    ) -> Result<Self, IndexerError> {
        if let PlanningScope::Candidates(paths) = scope {
            if !includes_directory_candidate(connection, discovered, paths)? {
                let mut upserts = Vec::new();
                let mut deletions = Vec::new();
                for path in paths {
                    if let Some(kind) = discovered.get(path) {
                        upserts.push(read_current_entry(path, *kind)?);
                    } else {
                        deletions.push(display_path(path));
                    }
                }
                return Ok(Self { upserts, deletions });
            }
        }

        let mut stale = connection
            .prepare("SELECT path FROM filesystem_entries")
            .and_then(|mut statement| {
                statement
                    .query_map([], |row| row.get::<_, String>(0))?
                    .collect::<Result<BTreeSet<_>, _>>()
            })
            .map_err(|source| {
                IndexerError::Write(DbWriteError::ReadBack {
                    operation: "filesystem_reconciliation.stored_paths",
                    source,
                })
            })?;
        let mut upserts = Vec::with_capacity(discovered.len());
        for (path, kind) in discovered {
            let entry = read_current_entry(path, *kind)?;
            stale.remove(&entry.path);
            upserts.push(entry);
        }
        Ok(Self {
            upserts,
            deletions: stale.into_iter().collect(),
        })
    }

    pub(super) fn apply(
        self,
        connection: &Connection,
        change: &mut IndexGenerationChange,
    ) -> Result<(), IndexerError> {
        let mut upsert = FilesystemEntry::prepare_upsert(connection)?;
        let mut delete = connection
            .prepare("DELETE FROM filesystem_entries WHERE path = ?1")
            .map_err(delete_error)?;
        let org_changes = change
            .files
            .iter()
            .map(|file| (file.path.as_str(), file.action))
            .collect::<BTreeMap<_, _>>();
        let mut full_invalidation = change.full_invalidation;
        for entry in self.upserts {
            if entry.upsert(&mut upsert)?
                && org_changes.get(entry.path.as_str()) != Some(&AffectedFileAction::Upsert)
            {
                full_invalidation = true;
            }
        }
        for path in self.deletions {
            if delete.execute([&path]).map_err(delete_error)? != 0
                && org_changes.get(path.as_str()) != Some(&AffectedFileAction::Delete)
            {
                full_invalidation = true;
            }
        }
        if full_invalidation && !change.full_invalidation {
            *change = IndexGenerationChange::full_invalidation();
        }
        Ok(())
    }
}

fn includes_directory_candidate(
    connection: &Connection,
    discovered: &BTreeMap<PathBuf, FilesystemKind>,
    candidates: &BTreeSet<PathBuf>,
) -> Result<bool, IndexerError> {
    let mut stored_directory = connection
        .prepare("SELECT EXISTS(SELECT 1 FROM filesystem_entries WHERE path = ?1 AND kind = 'directory')")
        .map_err(directory_read_error)?;
    for path in candidates {
        if discovered.get(path) == Some(&FilesystemKind::Directory)
            || stored_directory
                .query_row([display_path(path)], |row| row.get(0))
                .map_err(directory_read_error)?
        {
            return Ok(true);
        }
    }
    Ok(false)
}

fn read_current_entry(path: &Path, kind: FilesystemKind) -> Result<FilesystemEntry, IndexerError> {
    FilesystemEntry::read(path, kind)?.ok_or_else(|| IndexerError::ReadFile {
        path: path.to_path_buf(),
        source: io::Error::new(
            io::ErrorKind::NotFound,
            "filesystem entry changed type or disappeared during discovery",
        ),
    })
}

fn directory_read_error(source: rusqlite::Error) -> IndexerError {
    IndexerError::Write(DbWriteError::ReadBack {
        operation: "filesystem_reconciliation.stored_directory",
        source,
    })
}

fn delete_error(source: rusqlite::Error) -> IndexerError {
    IndexerError::Write(DbWriteError::Write {
        operation: "filesystem_reconciliation.delete",
        source,
    })
}
