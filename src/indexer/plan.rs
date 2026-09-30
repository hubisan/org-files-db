use super::*;

#[derive(Debug)]
pub(in crate::indexer) enum PlanningScope {
    All,
    Candidates(BTreeSet<PathBuf>),
}

impl PlanningScope {
    pub(in crate::indexer) fn is_all(&self) -> bool {
        matches!(self, Self::All)
    }

    pub(in crate::indexer) fn includes(
        &self,
        path: &Path,
        identity: &FileIdentity,
        persisted: Option<&PersistedFileSnapshot>,
    ) -> bool {
        match self {
            Self::All => true,
            Self::Candidates(candidates) => {
                candidates.contains(path)
                    || identity
                        .to_path()
                        .is_some_and(|identity_path| candidates.contains(&identity_path))
                    || persisted.is_some_and(|file| self.includes_persisted(file))
            }
        }
    }

    pub(in crate::indexer) fn includes_persisted(&self, persisted: &PersistedFileSnapshot) -> bool {
        match self {
            Self::All => true,
            Self::Candidates(candidates) => persisted
                .identity
                .as_ref()
                .and_then(FileIdentity::to_path)
                .is_some_and(|path| {
                    candidates.iter().any(|candidate| {
                        path.as_path() == candidate.as_path() || path.starts_with(candidate)
                    })
                }),
        }
    }
}

#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub(crate) struct RebuildOptions {
    pub(crate) allow_empty: bool,
    pub(crate) accept_source_root_changes: bool,
}

#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub(crate) struct ChangePlanningOptions {
    pub(crate) allow_empty: bool,
    pub(crate) verify_hashes: bool,
    pub(crate) accept_source_root_changes: bool,
}

#[derive(Debug)]
pub(crate) enum ChangePlanningResult {
    FullRebuildRequired,
    Ready(Box<ChangePlan>),
}

#[derive(Debug)]
pub(crate) struct ChangePlan {
    pub(crate) invalidations: IndexInvalidationSet,
    pub(in crate::indexer) planning_context: IndexingContext,
    pub(in crate::indexer) fts_backend_available: bool,
    pub(in crate::indexer) source_root_evidence: Option<SourceRootEvidenceSet>,
    pub(in crate::indexer) expected_files: Vec<PersistedFileSnapshot>,
    pub(crate) unchanged: Vec<PlannedFile>,
    pub(crate) metadata_only: Vec<PlannedFile>,
    pub(crate) created: Vec<PlannedPreparedFile>,
    pub(crate) modified: Vec<PlannedPreparedFile>,
    pub(crate) deleted: Vec<DeletedFile>,
    pub(crate) failed: Vec<FailedChange>,
}

impl ChangePlan {
    pub(in crate::indexer) fn current_identities(&self) -> Vec<&FileIdentity> {
        self.unchanged
            .iter()
            .chain(self.metadata_only.iter())
            .map(|file| &file.identity)
            .chain(self.created.iter().map(|file| &file.prepared.identity))
            .chain(self.modified.iter().map(|file| &file.prepared.identity))
            .collect()
    }

    pub(in crate::indexer) fn has_index_changes(&self) -> bool {
        !self.invalidations.is_empty()
            || !self.metadata_only.is_empty()
            || !self.created.is_empty()
            || !self.modified.is_empty()
            || !self.deleted.is_empty()
    }
}

#[derive(Debug)]
pub(crate) struct ActionableChangePlan {
    pub(in crate::indexer) plan: ChangePlan,
}

impl TryFrom<ChangePlan> for ActionableChangePlan {
    type Error = ChangeApplicationRejection;

    fn try_from(plan: ChangePlan) -> Result<Self, Self::Error> {
        if !plan.failed.is_empty() {
            return Err(ChangeApplicationRejection::FailedSources);
        }
        Ok(Self { plan })
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum ChangeApplicationRejection {
    FullRebuildRequired,
    FailedSources,
    Stale,
}

#[derive(Debug, PartialEq, Eq)]
pub(crate) enum ChangeApplicationResult {
    Applied(ChangeApplicationReport),
    Rejected(ChangeApplicationRejection),
}

#[derive(Debug, Default, PartialEq, Eq)]
pub(crate) struct ChangeApplicationReport {
    pub(crate) unchanged: usize,
    pub(crate) metadata_only: usize,
    pub(crate) created: usize,
    pub(crate) modified: usize,
    pub(crate) deleted: usize,
}

impl From<&ChangePlan> for ChangeApplicationReport {
    fn from(plan: &ChangePlan) -> Self {
        Self {
            unchanged: plan.unchanged.len(),
            metadata_only: plan.metadata_only.len(),
            created: plan.created.len(),
            modified: plan.modified.len(),
            deleted: plan.deleted.len(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct PlannedFile {
    /// Existing row selected by identity (or the exact UTF-8 legacy display path).
    pub(crate) existing_file_id: i64,
    pub(crate) path: PathBuf,
    pub(crate) identity: FileIdentity,
    pub(crate) file_record: FileRecordInput,
    pub(in crate::indexer) expected_file_record: FileRecordInput,
}

impl PlannedFile {
    pub(in crate::indexer) fn from_persisted(
        discovered: &DiscoveredOrgFile,
        persisted: &PersistedFileSnapshot,
    ) -> Self {
        Self {
            existing_file_id: persisted.file_id,
            path: discovered.path.clone(),
            identity: discovered.identity.clone(),
            file_record: FileRecordInput {
                path: discovered.path.clone(),
                identity: Some(discovered.identity.as_bytes().to_vec()),
                mtime_ns: persisted.mtime_ns,
                size: persisted.size,
                content_hash: persisted.content_hash.clone(),
                indexed_at: None,
            },
            expected_file_record: persisted_file_record(persisted, &discovered.path),
        }
    }

    pub(in crate::indexer) fn from_persisted_snapshot(
        persisted: &PersistedFileSnapshot,
    ) -> Option<Self> {
        let identity = persisted.identity.clone()?;
        let path = identity.to_path()?;
        Some(Self {
            existing_file_id: persisted.file_id,
            path: path.clone(),
            identity: identity.clone(),
            file_record: persisted_file_record(persisted, &path),
            expected_file_record: persisted_file_record(persisted, &path),
        })
    }

    pub(in crate::indexer) fn from_captured(
        discovered: &DiscoveredOrgFile,
        persisted: &PersistedFileSnapshot,
        captured: &CapturedSource,
    ) -> Result<Self, IndexerError> {
        Ok(Self {
            existing_file_id: persisted.file_id,
            path: discovered.path.clone(),
            identity: discovered.identity.clone(),
            file_record: build_file_record(
                &discovered.path,
                &discovered.identity,
                &captured.snapshot,
            )?,
            expected_file_record: persisted_file_record(persisted, &discovered.path),
        })
    }
}

#[derive(Debug)]
pub(crate) struct PlannedPreparedFile {
    /// `None` denotes a created source; otherwise application must update this row.
    pub(crate) existing_file_id: Option<i64>,
    pub(in crate::indexer) expected_file_record: Option<FileRecordInput>,
    pub(crate) prepared: PreparedFile,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct DeletedFile {
    pub(crate) file_id: i64,
    pub(crate) path: String,
    pub(crate) identity: Option<FileIdentity>,
    pub(in crate::indexer) sort_key: Vec<u8>,
}

impl From<PersistedFileSnapshot> for DeletedFile {
    fn from(value: PersistedFileSnapshot) -> Self {
        let sort_key = value
            .identity
            .as_ref()
            .map(|identity| identity.as_bytes().to_vec())
            .unwrap_or_else(|| value.path.as_bytes().to_vec());
        Self {
            file_id: value.file_id,
            path: value.path,
            identity: value.identity,
            sort_key,
        }
    }
}

#[derive(Debug)]
// `path` and `error` are not reported yet; see #34.
#[allow(dead_code)]
pub(crate) struct FailedChange {
    pub(crate) path: PathBuf,
    pub(crate) error: IndexerError,
}

pub(in crate::indexer) enum PlannedCurrentFile {
    Unchanged(PlannedFile),
    MetadataOnly(PlannedFile),
    Created(PlannedPreparedFile),
    Modified(PlannedPreparedFile),
}

pub(in crate::indexer) fn plan_baseline_matches(
    connection: &Connection,
    plan: &ChangePlan,
) -> Result<bool, IndexerError> {
    let PersistedFileSnapshots::Valid(current_files) = load_persisted_file_snapshots(connection)?
    else {
        return Ok(false);
    };
    if !same_persisted_file_baseline(&plan.expected_files, &current_files) {
        return Ok(false);
    }
    for file in plan.unchanged.iter().chain(plan.metadata_only.iter()) {
        if !row_matches_file_record(
            connection,
            file.existing_file_id,
            &file.expected_file_record,
        )? {
            return Ok(false);
        }
    }
    for file in &plan.modified {
        let Some(file_id) = file.existing_file_id else {
            continue;
        };
        let Some(expected) = file.expected_file_record.as_ref() else {
            return Ok(false);
        };
        if !row_matches_file_record(connection, file_id, expected)? {
            return Ok(false);
        }
    }
    for file in &plan.deleted {
        let found = connection
            .query_row("SELECT 1 FROM files WHERE id = ?1", [file.file_id], |_| {
                Ok(())
            })
            .optional()
            .map_err(|source| {
                IndexerError::Write(DbWriteError::ReadBack {
                    operation: "apply_change_plan.deleted_baseline",
                    source,
                })
            })?;
        if found.is_none() {
            return Ok(false);
        }
    }
    Ok(true)
}
