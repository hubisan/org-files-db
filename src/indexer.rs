use std::{
    collections::{BTreeMap, BTreeSet},
    error::Error,
    fmt, fs, io,
    path::{Path, PathBuf},
    time::{SystemTime, UNIX_EPOCH},
};

use rusqlite::{types::ValueRef, Connection, OptionalExtension, TransactionBehavior};
use sha2::{Digest, Sha256};

use crate::{
    config::{normalize_syntactic_path, Config, ConfigError},
    db::{
        open_database_with_schema, sqlite_supports_fts5, DbError, DbWriteError, DbWriter,
        EffectivePropertyRecord, EffectiveTagRecord, FileRecordInput, HeadingBodyRecord,
        HeadingRecord, KeywordRecord, LinkRecord, OutlinePathRecord, PropertyRecord,
        SchemaDefinition, TagRecord, TimestampRecord, TimestampRepeaterRecord, TodoKeywordRecord,
        CURRENT_SCHEMA_VERSION, DB_METADATA_BODY_TEXT_AVAILABLE_KEY, DB_METADATA_FTS_AVAILABLE_KEY,
        DB_METADATA_FTS_BODY_INDEXED_KEY, DB_METADATA_FTS_SCHEMA_VERSION_KEY,
        FTS_SCHEMA_CONTRACT_VERSION,
    },
    exclusions::ExclusionMatcher,
    file_identity::{display_path, FileIdentity},
    indexing_context::{IndexInvalidationSet, IndexingContext, IndexingContextComparison},
    link_resolver::IndexedUniverse,
    link_resolver::LinkResolver,
    parser::{
        DiagnosticSeverity, OrgParserCore, ParseDiagnostic, ParseOptions, ParsedHeading,
        ParsedLink, ParsedOrgDocument, ParsedTimestamp, ParsedTimestampModifierKind,
        ParsedTimestampModifierType, ParsedTimestampRole, ParsedTimestampUnit, TodoType,
    },
    property::{derive_effective_properties, PropertyRow},
    source_root_evidence::{
        SourceRootEvidenceError, SourceRootEvidencePolicy, SourceRootEvidenceSet,
    },
    tag::derive_effective_tags,
    todo_keywords::{
        resolve_todo_keywords_with_default_source, ResolvedTodoKeywordEntry, ResolvedTodoKeywords,
        TodoKeywordSourceKind,
    },
};

#[derive(Debug)]
pub(crate) struct CandidatePathNormalizer {
    indexed_universe: IndexedUniverse,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum CandidatePathResolution {
    Candidate(PathBuf),
    Ignore,
    Reconcile,
}

impl CandidatePathNormalizer {
    pub(crate) fn from_config(config: &Config) -> Result<Self, IndexerError> {
        let discovery = discover_org_files(config)?;
        Ok(Self {
            indexed_universe: discovery.indexed_universe,
        })
    }

    pub(crate) fn watcher_directory_hints(&self) -> Vec<(PathBuf, bool)> {
        self.indexed_universe.watcher_directory_hints()
    }

    pub(crate) fn resolve(&self, path: &Path) -> CandidatePathResolution {
        let logical_path = normalize_syntactic_path(path.to_path_buf());
        if !logical_path.is_absolute() {
            return CandidatePathResolution::Ignore;
        }

        let symlink_metadata = match fs::symlink_metadata(&logical_path) {
            Ok(metadata) => Some(metadata),
            Err(source) if source.kind() == io::ErrorKind::NotFound => None,
            Err(source) if source.kind() == io::ErrorKind::InvalidInput => {
                return CandidatePathResolution::Ignore;
            }
            Err(_) => return CandidatePathResolution::Reconcile,
        };

        let Some(symlink_metadata) = symlink_metadata else {
            if self.indexed_universe.is_configured_root_path(&logical_path) {
                return CandidatePathResolution::Reconcile;
            }
            if let Some(canonical_directory) = self
                .indexed_universe
                .canonical_directory_for_known_path(&logical_path)
            {
                return CandidatePathResolution::Candidate(canonical_directory.to_path_buf());
            }

            let candidate = self
                .indexed_universe
                .normalize_candidate_path(&logical_path, None)
                .or_else(|| {
                    self.indexed_universe
                        .is_explicit_logical_path(&logical_path)
                        .then(|| logical_path.clone())
                });
            return self.classify_file_candidate(&logical_path, candidate);
        };

        let existing_is_symlink = symlink_metadata.file_type().is_symlink();
        let canonical_path = match fs::canonicalize(&logical_path) {
            Ok(path) => path,
            Err(source) if source.kind() == io::ErrorKind::NotFound => {
                return CandidatePathResolution::Reconcile;
            }
            Err(source) if source.kind() == io::ErrorKind::InvalidInput => {
                return CandidatePathResolution::Ignore;
            }
            Err(_) => return CandidatePathResolution::Reconcile,
        };
        let metadata = match fs::metadata(&canonical_path) {
            Ok(metadata) => metadata,
            Err(source) if source.kind() == io::ErrorKind::InvalidInput => {
                return CandidatePathResolution::Ignore;
            }
            Err(_) => return CandidatePathResolution::Reconcile,
        };

        if metadata.is_dir() {
            return if self.indexed_universe.is_known_directory_path(&logical_path)
                || self
                    .indexed_universe
                    .is_known_directory_path(&canonical_path)
                || self.indexed_universe.includes_logical_file(&logical_path)
                || self.indexed_universe.contains(&canonical_path)
            {
                CandidatePathResolution::Reconcile
            } else {
                CandidatePathResolution::Ignore
            };
        }
        if !metadata.is_file() {
            return CandidatePathResolution::Ignore;
        }

        if let Some(previous_canonical) = self
            .indexed_universe
            .source_mapping_for_logical_path(&logical_path)
        {
            if previous_canonical != canonical_path {
                return CandidatePathResolution::Reconcile;
            }
        }

        let candidate = self
            .indexed_universe
            .normalize_candidate_path(&logical_path, Some(&canonical_path));
        if existing_is_symlink && candidate.is_some() {
            return CandidatePathResolution::Reconcile;
        }

        self.classify_file_candidate(&logical_path, candidate)
    }

    fn classify_file_candidate(
        &self,
        logical_path: &Path,
        candidate: Option<PathBuf>,
    ) -> CandidatePathResolution {
        let Some(candidate) = candidate else {
            return CandidatePathResolution::Ignore;
        };

        if self.indexed_universe.is_known_source(&candidate)
            || self
                .indexed_universe
                .is_explicit_candidate(logical_path, &candidate)
            || is_org_source_path(logical_path)
            || is_org_source_path(&candidate)
        {
            CandidatePathResolution::Candidate(candidate)
        } else {
            CandidatePathResolution::Ignore
        }
    }
}

#[derive(Debug)]
pub struct Indexer<P> {
    parser: P,
}

impl<P> Indexer<P>
where
    P: OrgParserCore,
{
    pub fn new(parser: P) -> Self {
        Self { parser }
    }

    pub fn rebuild_from_config_path(
        &self,
        config_path: impl AsRef<Path>,
    ) -> Result<RebuildReport, IndexerError> {
        self.rebuild_from_config_path_with_options(config_path, false)
    }

    pub(crate) fn rebuild_from_config_path_with_options(
        &self,
        config_path: impl AsRef<Path>,
        allow_empty: bool,
    ) -> Result<RebuildReport, IndexerError> {
        self.rebuild_from_config_path_with_rebuild_options(
            config_path,
            RebuildOptions {
                allow_empty,
                accept_source_root_changes: false,
            },
        )
    }

    pub(crate) fn rebuild_from_config_path_with_rebuild_options(
        &self,
        config_path: impl AsRef<Path>,
        options: RebuildOptions,
    ) -> Result<RebuildReport, IndexerError> {
        let config = Config::load_from_file(config_path).map_err(IndexerError::Config)?;
        let schema = SchemaDefinition::new(CURRENT_SCHEMA_VERSION, config.search.fts5_enabled);
        let mut connection =
            open_database_with_schema(&config.db_path, &schema).map_err(IndexerError::Database)?;
        self.rebuild_with_rebuild_options(&mut connection, &config, options)
    }

    pub fn rebuild(
        &self,
        connection: &mut Connection,
        config: &Config,
    ) -> Result<RebuildReport, IndexerError> {
        self.rebuild_with_options(connection, config, false)
    }

    /// Plans and applies one complete configured-source reconciliation through
    /// the same indexer validation and transactional mutation boundary used by
    /// all incremental callers.
    pub(crate) fn reconcile_configured_sources(
        &self,
        connection: &mut Connection,
        config: &Config,
    ) -> Result<ChangeApplicationResult, IndexerError> {
        let planning = self.plan_changes(connection, config)?;
        self.apply_planning_result(connection, config, planning)
    }

    /// Plans and applies a bounded reconciliation for already-normalized
    /// candidate paths. Candidate event kinds are intentionally absent: the
    /// indexer reclassifies each path from current filesystem and persisted DB
    /// state.
    pub(crate) fn reconcile_candidate_paths<I>(
        &self,
        connection: &mut Connection,
        config: &Config,
        candidates: I,
    ) -> Result<ChangeApplicationResult, IndexerError>
    where
        I: IntoIterator<Item = PathBuf>,
    {
        let candidates = candidates.into_iter().collect::<BTreeSet<_>>();
        if candidates.is_empty() {
            return Ok(ChangeApplicationResult::Applied(
                ChangeApplicationReport::default(),
            ));
        }
        let planning = self.plan_changes_for_scope(
            connection,
            config,
            ChangePlanningOptions {
                allow_empty: true,
                verify_hashes: false,
                accept_source_root_changes: false,
            },
            PlanningScope::Candidates(candidates),
        )?;
        self.apply_planning_result(connection, config, planning)
    }

    fn apply_planning_result(
        &self,
        connection: &mut Connection,
        config: &Config,
        planning: ChangePlanningResult,
    ) -> Result<ChangeApplicationResult, IndexerError> {
        let actionable = match self.actionable_plan(planning) {
            Ok(actionable) => actionable,
            Err(rejection) => return Ok(ChangeApplicationResult::Rejected(rejection)),
        };
        self.apply_change_plan(connection, config, actionable)
    }

    pub(crate) fn plan_changes(
        &self,
        connection: &Connection,
        config: &Config,
    ) -> Result<ChangePlanningResult, IndexerError> {
        self.plan_changes_with_options(connection, config, ChangePlanningOptions::default())
    }

    pub(crate) fn plan_changes_with_options(
        &self,
        connection: &Connection,
        config: &Config,
        options: ChangePlanningOptions,
    ) -> Result<ChangePlanningResult, IndexerError> {
        self.plan_changes_for_scope(connection, config, options, PlanningScope::All)
    }

    fn plan_changes_for_scope(
        &self,
        connection: &Connection,
        config: &Config,
        options: ChangePlanningOptions,
        scope: PlanningScope,
    ) -> Result<ChangePlanningResult, IndexerError> {
        self.plan_changes_for_scope_with_hook(connection, config, options, scope, || {})
    }

    fn plan_changes_for_scope_with_hook<H>(
        &self,
        connection: &Connection,
        config: &Config,
        options: ChangePlanningOptions,
        scope: PlanningScope,
        after_initial_root_capture: H,
    ) -> Result<ChangePlanningResult, IndexerError>
    where
        H: FnOnce(),
    {
        let planned_root_evidence = if scope.is_all() {
            let evidence = SourceRootEvidenceSet::capture(config)
                .map_err(map_initial_source_root_capture_error)?;
            evidence
                .validate_committed(
                    connection,
                    if options.accept_source_root_changes {
                        SourceRootEvidencePolicy::AcceptChanges
                    } else {
                        SourceRootEvidencePolicy::Automatic
                    },
                )
                .map_err(|source| IndexerError::SourceRootEvidence(Box::new(source)))?;
            Some(evidence)
        } else {
            None
        };
        after_initial_root_capture();

        let fts_backend_available =
            sqlite_fts5_available_read_only(connection).map_err(|source| {
                IndexerError::Database(DbError::Inspect {
                    target: config.db_path.display().to_string(),
                    source,
                })
            })?;
        if config.search.fts5_enabled && !fts_backend_available {
            return Err(IndexerError::Write(DbWriteError::UnsupportedBackendFeature {
                feature: "SQLite FTS5",
                message: "SQLite FTS5 was requested, but the opened SQLite connection does not support FTS5".to_string(),
            }));
        }
        let context = IndexingContext::from_config(config, fts_backend_available);
        let context_comparison = context.compare(connection).map_err(IndexerError::Write)?;
        if context_comparison == IndexingContextComparison::FullRebuildRequired {
            return Ok(ChangePlanningResult::FullRebuildRequired);
        }

        let persisted = match load_persisted_file_snapshots(connection)? {
            PersistedFileSnapshots::Valid(files) => files,
            PersistedFileSnapshots::InvalidIdentity => {
                return Ok(ChangePlanningResult::FullRebuildRequired)
            }
        };
        if matches!(&scope, PlanningScope::Candidates(_))
            && persisted.iter().any(|file| file.identity.is_none())
        {
            return Ok(ChangePlanningResult::FullRebuildRequired);
        }
        let discovery = discover_org_files(config)?;
        if let Some(expected) = planned_root_evidence.as_ref() {
            let current = SourceRootEvidenceSet::capture(config)
                .map_err(|source| IndexerError::SourceRootEvidence(Box::new(source)))?;
            expected
                .ensure_unchanged(&current)
                .map_err(|source| IndexerError::SourceRootEvidence(Box::new(source)))?;
        }
        if discovery.files.is_empty() && !discovery.had_exclusion_match && !options.allow_empty {
            let existing_indexed_files = existing_indexed_file_count(connection)?;
            if existing_indexed_files > 0 {
                return Err(IndexerError::RefusedEmptyRebuild {
                    existing_indexed_files,
                });
            }
        }

        let invalidations = match context_comparison {
            IndexingContextComparison::Compatible => IndexInvalidationSet::default(),
            IndexingContextComparison::Invalidations(invalidations) => invalidations,
            IndexingContextComparison::FullRebuildRequired => unreachable!("handled above"),
        };
        let reparse_all = invalidations.contains(IndexInvalidationSet::REPARSE_ALL_FILES);
        if reparse_all && matches!(&scope, PlanningScope::Candidates(_)) {
            return self.plan_changes_for_scope(connection, config, options, PlanningScope::All);
        }
        let expected_files = persisted.clone();
        let mut by_identity = BTreeMap::new();
        let mut legacy_by_path = BTreeMap::new();
        for file in persisted {
            if let Some(identity) = file.identity.clone() {
                by_identity.insert(identity, file);
            } else {
                legacy_by_path.insert(file.path.clone(), file);
            }
        }

        let mut discovery_files = discovery.files;
        discovery_files
            .sort_by(|left, right| left.identity.as_bytes().cmp(right.identity.as_bytes()));
        let mut plan = ChangePlan {
            invalidations,
            indexed_universe: Some(discovery.indexed_universe),
            planning_context: context,
            fts_backend_available,
            verification_policy: options,
            source_root_evidence: planned_root_evidence,
            expected_files,
            unchanged: Vec::new(),
            metadata_only: Vec::new(),
            created: Vec::new(),
            modified: Vec::new(),
            deleted: Vec::new(),
            failed: Vec::new(),
        };
        for discovered in discovery_files {
            let persisted = by_identity.remove(&discovered.identity).or_else(|| {
                discovered
                    .path
                    .to_str()
                    .and_then(|_| legacy_by_path.remove(&display_path(&discovered.path)))
            });
            if !scope.includes(&discovered.path, &discovered.identity, persisted.as_ref()) {
                if let Some(persisted) = persisted.as_ref() {
                    plan.unchanged
                        .push(PlannedFile::from_persisted(&discovered, persisted));
                }
                continue;
            }
            match self.plan_discovered_file(discovered, persisted, config, options, reparse_all) {
                Ok(PlannedCurrentFile::Unchanged(file)) => plan.unchanged.push(file),
                Ok(PlannedCurrentFile::MetadataOnly(file)) => plan.metadata_only.push(file),
                Ok(PlannedCurrentFile::Created(file)) => plan.created.push(file),
                Ok(PlannedCurrentFile::Modified(file)) => plan.modified.push(file),
                Err(error) => plan.failed.push(FailedChange {
                    path: error_path(&error),
                    error,
                }),
            }
        }
        for persisted in by_identity
            .into_values()
            .chain(legacy_by_path.into_values())
        {
            if scope.includes_persisted(&persisted) {
                plan.deleted.push(DeletedFile::from(persisted));
            } else if let Some(file) = PlannedFile::from_persisted_snapshot(&persisted) {
                plan.unchanged.push(file);
            } else {
                return Ok(ChangePlanningResult::FullRebuildRequired);
            }
        }
        plan.deleted
            .sort_by(|left, right| left.sort_key.cmp(&right.sort_key));
        Ok(ChangePlanningResult::Ready(Box::new(plan)))
    }

    /// Converts a successful, failure-free planning result into the only input
    /// accepted by the transactional mutation boundary.
    #[allow(dead_code)]
    pub(crate) fn actionable_plan(
        &self,
        result: ChangePlanningResult,
    ) -> Result<ActionableChangePlan, ChangeApplicationRejection> {
        match result {
            ChangePlanningResult::FullRebuildRequired => {
                Err(ChangeApplicationRejection::FullRebuildRequired)
            }
            ChangePlanningResult::Ready(plan) if !plan.failed.is_empty() => {
                Err(ChangeApplicationRejection::FailedSources)
            }
            ChangePlanningResult::Ready(plan) => ActionableChangePlan::try_from(*plan),
        }
    }

    /// Applies one single-use actionable plan as one immediate SQLite transaction.
    #[allow(dead_code)]
    pub(crate) fn apply_change_plan(
        &self,
        connection: &mut Connection,
        config: &Config,
        actionable: ActionableChangePlan,
    ) -> Result<ChangeApplicationResult, IndexerError> {
        self.apply_change_plan_with_hook(connection, config, actionable, || {})
    }

    fn apply_change_plan_with_hook<H>(
        &self,
        connection: &mut Connection,
        config: &Config,
        actionable: ActionableChangePlan,
        before_final_root_validation: H,
    ) -> Result<ChangeApplicationResult, IndexerError>
    where
        H: FnOnce(),
    {
        let ActionableChangePlan {
            plan,
            indexed_universe,
        } = actionable;
        if !plan.failed.is_empty() {
            return Ok(ChangeApplicationResult::Rejected(
                ChangeApplicationRejection::FailedSources,
            ));
        }
        if let Some(expected) = plan.source_root_evidence.as_ref() {
            let current = SourceRootEvidenceSet::capture(config)
                .map_err(|source| IndexerError::SourceRootEvidence(Box::new(source)))?;
            expected
                .ensure_unchanged(&current)
                .map_err(|source| IndexerError::SourceRootEvidence(Box::new(source)))?;
        }
        // Revalidate every current source before a write transaction. This also
        // rejects plans whose prepared evidence is no longer current.
        for file in plan.unchanged.iter().chain(plan.metadata_only.iter()) {
            if !snapshot_matches_record(&file.path, &file.file_record)? {
                return Ok(ChangeApplicationResult::Rejected(
                    ChangeApplicationRejection::Stale,
                ));
            }
        }
        for file in plan.created.iter().chain(plan.modified.iter()) {
            if !snapshot_matches_record(&file.prepared.path, &file.prepared.file_record)? {
                return Ok(ChangeApplicationResult::Rejected(
                    ChangeApplicationRejection::Stale,
                ));
            }
        }
        let fts_available = sqlite_fts5_available_read_only(connection).map_err(|source| {
            IndexerError::Database(DbError::Inspect {
                target: config.db_path.display().to_string(),
                source,
            })
        })?;
        if config.search.fts5_enabled && !fts_available {
            return Err(IndexerError::Write(DbWriteError::UnsupportedBackendFeature { feature: "SQLite FTS5", message: "SQLite FTS5 was requested, but the opened SQLite connection does not support FTS5".to_string() }));
        }
        let current_context = IndexingContext::from_config(config, fts_available);
        if fts_available != plan.fts_backend_available || current_context != plan.planning_context {
            return Ok(ChangeApplicationResult::Rejected(
                ChangeApplicationRejection::Stale,
            ));
        }
        let current_invalidations = match current_context
            .compare(connection)
            .map_err(IndexerError::Write)?
        {
            IndexingContextComparison::Compatible => IndexInvalidationSet::default(),
            IndexingContextComparison::Invalidations(invalidations) => invalidations,
            IndexingContextComparison::FullRebuildRequired => {
                return Ok(ChangeApplicationResult::Rejected(
                    ChangeApplicationRejection::Stale,
                ))
            }
        };
        if current_invalidations != plan.invalidations {
            return Ok(ChangeApplicationResult::Rejected(
                ChangeApplicationRejection::Stale,
            ));
        }
        let rediscovery = discover_org_files(config)?;
        if let Some(expected) = plan.source_root_evidence.as_ref() {
            let current = SourceRootEvidenceSet::capture(config)
                .map_err(|source| IndexerError::SourceRootEvidence(Box::new(source)))?;
            expected
                .ensure_unchanged(&current)
                .map_err(|source| IndexerError::SourceRootEvidence(Box::new(source)))?;
        }
        let mut observed = rediscovery
            .files
            .iter()
            .map(|file| file.identity.as_bytes().to_vec())
            .collect::<Vec<_>>();
        let mut planned = plan
            .current_identities()
            .into_iter()
            .map(|identity| identity.as_bytes().to_vec())
            .collect::<Vec<_>>();
        observed.sort();
        planned.sort();
        if observed != planned {
            return Ok(ChangeApplicationResult::Rejected(
                ChangeApplicationRejection::Stale,
            ));
        }
        let tx = connection
            .transaction_with_behavior(TransactionBehavior::Immediate)
            .map_err(|source| IndexerError::Write(DbWriteError::Transaction { source }))?;
        if !plan_baseline_matches(&tx, &plan)? {
            return Ok(ChangeApplicationResult::Rejected(
                ChangeApplicationRejection::Stale,
            ));
        }
        for file in plan.unchanged.iter().chain(plan.metadata_only.iter()) {
            if !metadata_matches_record(&file.path, &file.file_record)? {
                return Ok(ChangeApplicationResult::Rejected(
                    ChangeApplicationRejection::Stale,
                ));
            }
        }
        for file in plan.created.iter().chain(plan.modified.iter()) {
            if !metadata_matches_record(&file.prepared.path, &file.prepared.file_record)? {
                return Ok(ChangeApplicationResult::Rejected(
                    ChangeApplicationRejection::Stale,
                ));
            }
        }
        for file in &plan.metadata_only {
            update_existing_file_metadata(&tx, file)?;
        }
        for file in &plan.modified {
            replace_prepared_file(
                &tx,
                file.existing_file_id,
                &file.prepared,
                config.search.index_body_text,
            )?;
        }
        for file in &plan.created {
            replace_prepared_file(&tx, None, &file.prepared, config.search.index_body_text)?;
        }
        for file in &plan.deleted {
            DbWriter::delete_file(&tx, file.file_id).map_err(IndexerError::Write)?;
        }
        if config.search.fts5_enabled {
            DbWriter::rebuild_heading_fts(&tx, config.search.index_body_text)
                .map_err(IndexerError::Write)?;
            persist_search_trust_metadata(&tx, true, config.search.index_body_text)
                .map_err(IndexerError::Write)?;
        } else {
            persist_search_trust_metadata(&tx, false, false).map_err(IndexerError::Write)?;
        }
        let _ = indexed_universe;
        LinkResolver::resolve_all(&tx, &rediscovery.indexed_universe)
            .map_err(IndexerError::Write)?;
        IndexingContext::from_config(config, fts_available)
            .persist(&tx)
            .map_err(IndexerError::Write)?;
        before_final_root_validation();
        if let Some(expected) = plan.source_root_evidence.as_ref() {
            let current = SourceRootEvidenceSet::capture(config)
                .map_err(|source| IndexerError::SourceRootEvidence(Box::new(source)))?;
            expected
                .ensure_unchanged(&current)
                .map_err(|source| IndexerError::SourceRootEvidence(Box::new(source)))?;
            expected
                .persist(&tx)
                .map_err(|source| IndexerError::SourceRootEvidence(Box::new(source)))?;
        }
        tx.commit()
            .map_err(|source| IndexerError::Write(DbWriteError::Transaction { source }))?;
        Ok(ChangeApplicationResult::Applied(
            ChangeApplicationReport::from(&plan),
        ))
    }

    fn plan_discovered_file(
        &self,
        discovered: DiscoveredOrgFile,
        persisted: Option<PersistedFileSnapshot>,
        config: &Config,
        options: ChangePlanningOptions,
        reparse_all: bool,
    ) -> Result<PlannedCurrentFile, IndexerError> {
        self.plan_discovered_file_with_reader(
            discovered,
            persisted,
            config,
            options,
            reparse_all,
            &mut FilesystemSnapshotReader,
        )
    }

    fn plan_discovered_file_with_reader<R>(
        &self,
        discovered: DiscoveredOrgFile,
        persisted: Option<PersistedFileSnapshot>,
        config: &Config,
        options: ChangePlanningOptions,
        reparse_all: bool,
        reader: &mut R,
    ) -> Result<PlannedCurrentFile, IndexerError>
    where
        R: FileSnapshotReader,
    {
        let metadata = reader.metadata(&discovered.path)?;
        let fast_snapshot_matches = persisted
            .as_ref()
            .is_some_and(|file| file.mtime_ns == metadata.mtime_ns && file.size == metadata.size);
        let hash_comparable = persisted
            .as_ref()
            .and_then(|file| qualified_sha256_hash(file.content_hash.as_deref()))
            .is_some();
        if let Some(persisted) = persisted.as_ref() {
            if fast_snapshot_matches && hash_comparable && !options.verify_hashes && !reparse_all {
                return Ok(PlannedCurrentFile::Unchanged(PlannedFile::from_persisted(
                    &discovered,
                    persisted,
                )));
            }
        }

        let captured = capture_stable_source_with(reader, &discovered.path)?;
        if let Some(persisted) = persisted.as_ref() {
            let hash_matches = qualified_sha256_hash(persisted.content_hash.as_deref())
                .is_some_and(|hash| hash == captured.snapshot.content_hash);
            let captured_snapshot_matches = persisted.mtime_ns == captured.snapshot.mtime_ns
                && persisted.size == captured.snapshot.size;
            if !reparse_all && hash_matches {
                let file = PlannedFile::from_captured(&discovered, persisted, &captured)?;
                return Ok(if captured_snapshot_matches {
                    PlannedCurrentFile::Unchanged(file)
                } else {
                    PlannedCurrentFile::MetadataOnly(file)
                });
            }
            let expected_file_record = persisted_file_record(persisted, &discovered.path);
            let prepared = self.prepare_captured_file(discovered, config, captured)?;
            return Ok(PlannedCurrentFile::Modified(PlannedPreparedFile {
                existing_file_id: Some(persisted.file_id),
                expected_file_record: Some(expected_file_record),
                prepared,
            }));
        }

        let prepared = self.prepare_captured_file(discovered, config, captured)?;
        Ok(PlannedCurrentFile::Created(PlannedPreparedFile {
            existing_file_id: None,
            expected_file_record: None,
            prepared,
        }))
    }

    pub(crate) fn rebuild_with_options(
        &self,
        connection: &mut Connection,
        config: &Config,
        allow_empty: bool,
    ) -> Result<RebuildReport, IndexerError> {
        self.rebuild_with_rebuild_options(
            connection,
            config,
            RebuildOptions {
                allow_empty,
                accept_source_root_changes: false,
            },
        )
    }

    pub(crate) fn rebuild_with_rebuild_options(
        &self,
        connection: &mut Connection,
        config: &Config,
        options: RebuildOptions,
    ) -> Result<RebuildReport, IndexerError> {
        let source_root_evidence = SourceRootEvidenceSet::capture(config)
            .map_err(map_initial_source_root_capture_error)?;
        source_root_evidence
            .validate_committed(
                connection,
                if options.accept_source_root_changes {
                    SourceRootEvidencePolicy::AcceptChanges
                } else {
                    SourceRootEvidencePolicy::Automatic
                },
            )
            .map_err(|source| IndexerError::SourceRootEvidence(Box::new(source)))?;

        let fts_backend_available = sqlite_supports_fts5(connection).map_err(|source| {
            IndexerError::Database(DbError::Inspect {
                target: config.db_path.display().to_string(),
                source,
            })
        })?;
        if config.search.fts5_enabled && !fts_backend_available {
            return Err(IndexerError::Write(DbWriteError::UnsupportedBackendFeature {
                feature: "SQLite FTS5",
                message:
                    "SQLite FTS5 was requested, but the opened SQLite connection does not support FTS5"
                        .to_string(),
            }));
        }
        let indexing_context = IndexingContext::from_config(config, fts_backend_available);

        let discovery = discover_org_files(config)?;
        source_root_evidence
            .ensure_unchanged(
                &SourceRootEvidenceSet::capture(config)
                    .map_err(|source| IndexerError::SourceRootEvidence(Box::new(source)))?,
            )
            .map_err(|source| IndexerError::SourceRootEvidence(Box::new(source)))?;
        if discovery.files.is_empty() && !discovery.had_exclusion_match {
            let existing_indexed_files = existing_indexed_file_count(connection)?;
            if existing_indexed_files > 0 && !options.allow_empty {
                return Err(IndexerError::RefusedEmptyRebuild {
                    existing_indexed_files,
                });
            }
        }

        let mut pending = Vec::with_capacity(discovery.files.len());
        let _missing_explicit_files = discovery.missing_explicit_files;
        for discovered in discovery.files {
            pending.push(self.prepare_discovered_file(discovered, config)?);
        }
        source_root_evidence
            .ensure_unchanged(
                &SourceRootEvidenceSet::capture(config)
                    .map_err(|source| IndexerError::SourceRootEvidence(Box::new(source)))?,
            )
            .map_err(|source| IndexerError::SourceRootEvidence(Box::new(source)))?;

        let tx = connection
            .transaction()
            .map_err(|source| IndexerError::Write(DbWriteError::Transaction { source }))?;
        DbWriter::delete_all_indexed_data(&tx).map_err(IndexerError::Write)?;
        DbWriter::set_metadata_flag(
            &tx,
            DB_METADATA_BODY_TEXT_AVAILABLE_KEY,
            config.search.index_body_text,
        )
        .map_err(IndexerError::Write)?;

        let mut report = RebuildReport::default();
        for pending_file in pending {
            debug_assert_eq!(
                pending_file.file_record.identity.as_deref(),
                Some(pending_file.identity.as_bytes())
            );
            let file_record = file_record_for_write(&pending_file.file_record, &pending_file.path)?;
            let file_id = DbWriter::upsert_file(&tx, &file_record).map_err(IndexerError::Write)?;
            let heading_count = index_document(
                &tx,
                file_id,
                &pending_file.document,
                &pending_file.todo_keywords,
                config.search.index_body_text,
            )
            .map_err(IndexerError::Write)?;
            let indexed_file = IndexedFile {
                path: pending_file.path.clone(),
                file_id,
                heading_count,
            };

            report
                .diagnostics
                .extend(pending_file.diagnostics.iter().cloned());
            report.diagnostics.extend(
                pending_file
                    .document
                    .diagnostics
                    .iter()
                    .cloned()
                    .map(IndexDiagnostic::from),
            );
            report.indexed_files.push(indexed_file);
        }

        if config.search.fts5_enabled {
            DbWriter::rebuild_heading_fts(&tx, config.search.index_body_text)
                .map_err(IndexerError::Write)?;
            persist_search_trust_metadata(&tx, true, config.search.index_body_text)
                .map_err(IndexerError::Write)?;
        } else {
            persist_search_trust_metadata(&tx, false, false).map_err(IndexerError::Write)?;
        }

        LinkResolver::resolve_all(&tx, &discovery.indexed_universe).map_err(IndexerError::Write)?;
        indexing_context.persist(&tx).map_err(IndexerError::Write)?;
        source_root_evidence
            .ensure_unchanged(
                &SourceRootEvidenceSet::capture(config)
                    .map_err(|source| IndexerError::SourceRootEvidence(Box::new(source)))?,
            )
            .map_err(|source| IndexerError::SourceRootEvidence(Box::new(source)))?;
        source_root_evidence
            .persist(&tx)
            .map_err(|source| IndexerError::SourceRootEvidence(Box::new(source)))?;

        tx.commit()
            .map_err(|source| IndexerError::Write(DbWriteError::Transaction { source }))?;

        Ok(report)
    }

    fn prepare_discovered_file(
        &self,
        discovered: DiscoveredOrgFile,
        config: &Config,
    ) -> Result<PreparedFile, IndexerError> {
        let captured = capture_stable_source(&discovered.path)?;
        self.prepare_captured_file(discovered, config, captured)
    }

    fn prepare_captured_file(
        &self,
        discovered: DiscoveredOrgFile,
        config: &Config,
        captured: CapturedSource,
    ) -> Result<PreparedFile, IndexerError> {
        let path = discovered.path;
        let identity = discovered.identity;
        let CapturedSource { bytes, snapshot } = captured;
        let content = decode_captured_source(&path, bytes)?;
        let parse_options = config.parse_options();
        let todo_keywords = resolve_todo_keywords_with_default_source(
            &content,
            &parse_options.todo_keywords,
            TodoKeywordSourceKind::ConfigDefault,
        );
        let document = self
            .parser
            .parse_document_core(
                &path,
                &content,
                &ParseOptions {
                    todo_keywords: todo_keywords.effective.clone(),
                    link_scanner: parse_options.link_scanner,
                },
            )
            .map_err(|diagnostic| IndexerError::Parse {
                path: path.clone(),
                diagnostic,
            })?;

        Ok(PreparedFile {
            file_record: build_file_record(&path, &identity, &snapshot)?,
            identity,
            document: normalize_document(document, &path, &content),
            todo_keywords,
            diagnostics: Vec::new(),
            path,
        })
    }
}

fn persist_search_trust_metadata(
    connection: &Connection,
    fts_available: bool,
    body_indexed: bool,
) -> Result<(), DbWriteError> {
    DbWriter::set_metadata_flag(connection, DB_METADATA_FTS_AVAILABLE_KEY, fts_available)?;
    DbWriter::set_metadata_flag(connection, DB_METADATA_FTS_BODY_INDEXED_KEY, body_indexed)?;
    DbWriter::set_metadata_value(
        connection,
        DB_METADATA_FTS_SCHEMA_VERSION_KEY,
        if fts_available {
            FTS_SCHEMA_CONTRACT_VERSION
        } else {
            "0"
        },
    )?;
    Ok(())
}

/// Owned, DB-free preparation output suitable for later change planning and
/// parallel parsing. The file record carries the stable source snapshot.
#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct PreparedFile {
    path: PathBuf,
    identity: FileIdentity,
    document: ParsedOrgDocument,
    todo_keywords: ResolvedTodoKeywords,
    diagnostics: Vec<IndexDiagnostic>,
    file_record: FileRecordInput,
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct FileSnapshot {
    mtime_ns: i64,
    size: i64,
    content_hash: String,
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct CapturedSource {
    bytes: Vec<u8>,
    snapshot: FileSnapshot,
}

#[derive(Debug)]
enum PlanningScope {
    All,
    Candidates(BTreeSet<PathBuf>),
}

impl PlanningScope {
    fn is_all(&self) -> bool {
        matches!(self, Self::All)
    }

    fn includes(
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

    fn includes_persisted(&self, persisted: &PersistedFileSnapshot) -> bool {
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

#[allow(dead_code)]
#[derive(Debug)]
pub(crate) enum ChangePlanningResult {
    FullRebuildRequired,
    Ready(Box<ChangePlan>),
}

#[allow(dead_code)]
#[derive(Debug)]
pub(crate) struct ChangePlan {
    pub(crate) invalidations: IndexInvalidationSet,
    indexed_universe: Option<IndexedUniverse>,
    planning_context: IndexingContext,
    fts_backend_available: bool,
    verification_policy: ChangePlanningOptions,
    source_root_evidence: Option<SourceRootEvidenceSet>,
    expected_files: Vec<PersistedFileSnapshot>,
    pub(crate) unchanged: Vec<PlannedFile>,
    pub(crate) metadata_only: Vec<PlannedFile>,
    pub(crate) created: Vec<PlannedPreparedFile>,
    pub(crate) modified: Vec<PlannedPreparedFile>,
    pub(crate) deleted: Vec<DeletedFile>,
    pub(crate) failed: Vec<FailedChange>,
}

impl ChangePlan {
    fn current_identities(&self) -> Vec<&FileIdentity> {
        self.unchanged
            .iter()
            .chain(self.metadata_only.iter())
            .map(|file| &file.identity)
            .chain(self.created.iter().map(|file| &file.prepared.identity))
            .chain(self.modified.iter().map(|file| &file.prepared.identity))
            .collect()
    }
}

#[allow(dead_code)]
#[derive(Debug)]
pub(crate) struct ActionableChangePlan {
    plan: ChangePlan,
    indexed_universe: IndexedUniverse,
}

impl TryFrom<ChangePlan> for ActionableChangePlan {
    type Error = ChangeApplicationRejection;

    fn try_from(mut plan: ChangePlan) -> Result<Self, Self::Error> {
        if !plan.failed.is_empty() {
            return Err(ChangeApplicationRejection::FailedSources);
        }
        let indexed_universe = plan
            .indexed_universe
            .take()
            .ok_or(ChangeApplicationRejection::InvalidPlan)?;
        Ok(Self {
            plan,
            indexed_universe,
        })
    }
}

#[allow(dead_code)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum ChangeApplicationRejection {
    FullRebuildRequired,
    FailedSources,
    InvalidPlan,
    Stale,
}

#[allow(dead_code)]
#[derive(Debug, PartialEq, Eq)]
pub(crate) enum ChangeApplicationResult {
    Applied(ChangeApplicationReport),
    Rejected(ChangeApplicationRejection),
}

#[allow(dead_code)]
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

#[allow(dead_code)]
#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct PlannedFile {
    /// Existing row selected by identity (or the exact UTF-8 legacy display path).
    pub(crate) existing_file_id: i64,
    pub(crate) path: PathBuf,
    pub(crate) identity: FileIdentity,
    pub(crate) file_record: FileRecordInput,
    expected_file_record: FileRecordInput,
}

#[allow(dead_code)]
impl PlannedFile {
    fn from_persisted(discovered: &DiscoveredOrgFile, persisted: &PersistedFileSnapshot) -> Self {
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

    fn from_persisted_snapshot(persisted: &PersistedFileSnapshot) -> Option<Self> {
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

    fn from_captured(
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

fn persisted_file_record(persisted: &PersistedFileSnapshot, path: &Path) -> FileRecordInput {
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

#[allow(dead_code)]
#[derive(Debug)]
pub(crate) struct PlannedPreparedFile {
    /// `None` denotes a created source; otherwise application must update this row.
    pub(crate) existing_file_id: Option<i64>,
    expected_file_record: Option<FileRecordInput>,
    pub(crate) prepared: PreparedFile,
}

#[allow(dead_code)]
#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct DeletedFile {
    pub(crate) file_id: i64,
    pub(crate) path: String,
    pub(crate) identity: Option<FileIdentity>,
    sort_key: Vec<u8>,
}

#[allow(dead_code)]
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

#[allow(dead_code)]
#[derive(Debug)]
pub(crate) struct FailedChange {
    pub(crate) path: PathBuf,
    pub(crate) error: IndexerError,
}

#[allow(dead_code)]
enum PlannedCurrentFile {
    Unchanged(PlannedFile),
    MetadataOnly(PlannedFile),
    Created(PlannedPreparedFile),
    Modified(PlannedPreparedFile),
}

#[allow(dead_code)]
#[derive(Debug, Clone)]
struct PersistedFileSnapshot {
    file_id: i64,
    path: String,
    identity: Option<FileIdentity>,
    mtime_ns: i64,
    size: i64,
    content_hash: Option<String>,
}

#[allow(dead_code)]
enum PersistedFileSnapshots {
    Valid(Vec<PersistedFileSnapshot>),
    InvalidIdentity,
}

struct DiscoveryResult {
    files: Vec<DiscoveredOrgFile>,
    indexed_universe: IndexedUniverse,
    missing_explicit_files: Vec<PathBuf>,
    had_exclusion_match: bool,
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct DiscoveredOrgFile {
    path: PathBuf,
    identity: FileIdentity,
    scan_root: PathBuf,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
enum ScanRootKind {
    ExplicitFile,
    ConfiguredDir,
}

#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct RebuildReport {
    pub indexed_files: Vec<IndexedFile>,
    pub diagnostics: Vec<IndexDiagnostic>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct IndexedFile {
    pub path: PathBuf,
    pub file_id: i64,
    pub heading_count: usize,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct IndexDiagnostic {
    pub severity: DiagnosticSeverity,
    pub message: String,
    pub file_path: Option<PathBuf>,
    pub line_number: Option<u32>,
    pub byte_range: Option<(usize, usize)>,
}

impl From<ParseDiagnostic> for IndexDiagnostic {
    fn from(value: ParseDiagnostic) -> Self {
        Self {
            severity: value.severity,
            message: value.message,
            file_path: value.file_path,
            line_number: value.line_number,
            byte_range: value.byte_range,
        }
    }
}

#[derive(Debug)]
pub enum IndexerError {
    Config(ConfigError),
    Database(DbError),
    Discover {
        path: PathBuf,
        source: std::io::Error,
    },
    InvalidDocument(&'static str),
    InvalidFileMetadata {
        path: PathBuf,
        field: &'static str,
    },
    UnstableFileSnapshot {
        path: PathBuf,
    },
    Parse {
        path: PathBuf,
        diagnostic: ParseDiagnostic,
    },
    ReadFile {
        path: PathBuf,
        source: std::io::Error,
    },
    Serialize {
        field: &'static str,
        source: serde_json::Error,
    },
    RefusedEmptyRebuild {
        existing_indexed_files: usize,
    },
    SourceRootEvidence(Box<dyn Error + Send + Sync>),
    Write(DbWriteError),
}

fn map_initial_source_root_capture_error(source: SourceRootEvidenceError) -> IndexerError {
    match source {
        SourceRootEvidenceError::Inspect { path, source } => {
            IndexerError::Discover { path, source }
        }
        SourceRootEvidenceError::NotDirectory { path } => IndexerError::Discover {
            path,
            source: io::Error::new(
                io::ErrorKind::InvalidInput,
                "configured path is not a directory",
            ),
        },
        source => IndexerError::SourceRootEvidence(Box::new(source)),
    }
}

impl fmt::Display for IndexerError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Config(source) => write!(f, "{source}"),
            Self::Database(source) => write!(f, "{source}"),
            Self::Discover { path, source } => {
                write!(
                    f,
                    "failed to discover Org files under {}: {}",
                    path.display(),
                    source
                )
            }
            Self::InvalidDocument(message) => write!(f, "invalid parsed document: {message}"),
            Self::InvalidFileMetadata { path, field } => write!(
                f,
                "failed to convert file metadata field {field} for {}",
                path.display()
            ),
            Self::UnstableFileSnapshot { path } => write!(
                f,
                "file changed while preparing {}; retry the indexing operation",
                path.display()
            ),
            Self::Parse { path, diagnostic } => {
                write!(
                    f,
                    "failed to parse {}: {}",
                    path.display(),
                    diagnostic.message
                )
            }
            Self::ReadFile { path, source } => {
                write!(f, "failed to read Org file {}: {}", path.display(), source)
            }
            Self::Serialize { field, source } => {
                write!(f, "failed to serialize {field} for DB write: {source}")
            }
            Self::RefusedEmptyRebuild {
                existing_indexed_files,
            } => write!(
                f,
                "rebuild found zero input Org files and was refused to avoid deleting {existing_indexed_files} indexed file(s); rerun with --allow-empty if this is intentional"
            ),
            Self::SourceRootEvidence(source) => write!(f, "{source}"),
            Self::Write(source) => write!(f, "{source}"),
        }
    }
}

impl Error for IndexerError {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        match self {
            Self::Config(source) => Some(source),
            Self::Database(source) => Some(source),
            Self::Discover { source, .. } => Some(source),
            Self::InvalidDocument(_) => None,
            Self::InvalidFileMetadata { .. } => None,
            Self::UnstableFileSnapshot { .. } => None,
            Self::Parse { .. } => None,
            Self::ReadFile { source, .. } => Some(source),
            Self::Serialize { source, .. } => Some(source),
            Self::RefusedEmptyRebuild { .. } => None,
            Self::SourceRootEvidence(source) => Some(source.as_ref()),
            Self::Write(source) => Some(source),
        }
    }
}

fn discover_org_files(config: &Config) -> Result<DiscoveryResult, IndexerError> {
    let mut paths = BTreeMap::new();
    let mut indexed_universe = IndexedUniverse::default();
    let mut missing_explicit_files = Vec::new();
    let mut globally_excluded = BTreeSet::new();
    let global_exclusions = ExclusionMatcher::global(&config.discovery);
    indexed_universe.set_global_exclusions(ExclusionMatcher::global(&config.discovery));
    let mut had_exclusion_match = false;

    for file in &config.files {
        if global_exclusions.matches_file(file) {
            had_exclusion_match = true;
            continue;
        }
        indexed_universe.add_explicit_logical_path(file.clone());
        let canonical_file = match canonicalize_existing_file(file) {
            Ok(path) => path,
            Err(IndexerError::Discover { source, .. })
                if source.kind() == io::ErrorKind::NotFound =>
            {
                missing_explicit_files.push(file.clone());
                continue;
            }
            Err(error) => return Err(error),
        };
        let scan_root = canonical_file
            .parent()
            .unwrap_or(canonical_file.as_path())
            .to_path_buf();
        indexed_universe.add_explicit_mapping(file.clone(), canonical_file.clone());
        insert_discovered_path(
            &mut paths,
            canonical_file,
            scan_root,
            ScanRootKind::ExplicitFile,
        );
    }

    for dir in &config.dirs {
        let canonical_dir = canonicalize_existing_dir(&dir.path)?;
        let mut visited_directories = BTreeSet::new();
        let local_exclusions = ExclusionMatcher::local(dir, &config.discovery);
        let scope_id = indexed_universe.add_root_scope(
            dir.path.clone(),
            canonical_dir.clone(),
            dir.recursive,
            local_exclusions,
        );
        let mut collector = DirectoryCollector {
            scan_root: &canonical_dir,
            recursive: dir.recursive,
            output: &mut paths,
            indexed_universe: &mut indexed_universe,
            visited_directories: &mut visited_directories,
            global_exclusions: &global_exclusions,
            globally_excluded: &mut globally_excluded,
            had_exclusion_match: &mut had_exclusion_match,
            scope_id,
        };
        collector.collect(&dir.path)?;
    }

    for excluded in &globally_excluded {
        paths.remove(excluded);
    }

    Ok(DiscoveryResult {
        files: paths
            .into_iter()
            .map(|(path, (scan_root, _))| DiscoveredOrgFile {
                identity: FileIdentity::from_canonical_path(&path),
                path,
                scan_root,
            })
            .collect(),
        indexed_universe,
        missing_explicit_files,
        had_exclusion_match,
    })
}

fn existing_indexed_file_count(connection: &Connection) -> Result<usize, IndexerError> {
    let count = connection
        .query_row("SELECT COUNT(*) FROM files", [], |row| row.get::<_, i64>(0))
        .map_err(|source| {
            IndexerError::Database(DbError::Inspect {
                target: "existing indexed files".to_string(),
                source,
            })
        })?;

    Ok(count as usize)
}

struct DirectoryCollector<'a> {
    scan_root: &'a Path,
    recursive: bool,
    output: &'a mut BTreeMap<PathBuf, (PathBuf, ScanRootKind)>,
    indexed_universe: &'a mut IndexedUniverse,
    visited_directories: &'a mut BTreeSet<PathBuf>,
    global_exclusions: &'a ExclusionMatcher,
    globally_excluded: &'a mut BTreeSet<PathBuf>,
    had_exclusion_match: &'a mut bool,
    scope_id: usize,
}

impl DirectoryCollector<'_> {
    fn collect(&mut self, logical_dir: &Path) -> Result<(), IndexerError> {
        let canonical_dir = canonicalize_existing_dir(logical_dir)?;
        if !self.visited_directories.insert(canonical_dir.clone()) {
            return Ok(());
        }
        self.indexed_universe.add_directory_mapping(
            self.scope_id,
            logical_dir.to_path_buf(),
            canonical_dir,
        );
        let mut entries = fs::read_dir(logical_dir)
            .map_err(|source| IndexerError::Discover {
                path: logical_dir.to_path_buf(),
                source,
            })?
            .collect::<Result<Vec<_>, _>>()
            .map_err(|source| IndexerError::Discover {
                path: logical_dir.to_path_buf(),
                source,
            })?;
        entries.sort_by_key(|entry| entry.path());

        for entry in entries {
            let path = entry.path();
            let file_type = entry.file_type().map_err(|source| IndexerError::Discover {
                path: path.clone(),
                source,
            })?;

            let followed_metadata = if file_type.is_symlink() {
                Some(
                    fs::metadata(&path).map_err(|source| IndexerError::Discover {
                        path: path.clone(),
                        source,
                    })?,
                )
            } else {
                None
            };
            let is_file = file_type.is_file()
                || followed_metadata
                    .as_ref()
                    .is_some_and(fs::Metadata::is_file);
            let is_dir =
                file_type.is_dir() || followed_metadata.as_ref().is_some_and(fs::Metadata::is_dir);

            if is_file {
                if is_org_source_path(&path) {
                    let canonical_path = canonicalize_existing_file(&path)?;
                    if self.global_exclusions.matches_file(&path) {
                        self.globally_excluded.insert(canonical_path.clone());
                        self.indexed_universe
                            .add_globally_excluded_path(canonical_path);
                        *self.had_exclusion_match = true;
                        continue;
                    }
                    if self
                        .indexed_universe
                        .root_scope_excludes_file(self.scope_id, &path)
                    {
                        *self.had_exclusion_match = true;
                        continue;
                    }
                    self.indexed_universe
                        .add_source_mapping(path.clone(), canonical_path.clone());
                    insert_discovered_path(
                        self.output,
                        canonical_path,
                        self.scan_root.to_path_buf(),
                        ScanRootKind::ConfiguredDir,
                    );
                }
            } else if self.recursive && is_dir {
                if self
                    .indexed_universe
                    .root_scope_excludes_directory(self.scope_id, &path)
                {
                    *self.had_exclusion_match = true;
                    continue;
                }
                self.collect(&path)?;
            }
        }

        Ok(())
    }
}

fn insert_discovered_path(
    output: &mut BTreeMap<PathBuf, (PathBuf, ScanRootKind)>,
    path: PathBuf,
    scan_root: PathBuf,
    scan_root_kind: ScanRootKind,
) {
    match output.get_mut(&path) {
        Some((existing_root, existing_kind)) => {
            let prefer_new_root = match (scan_root_kind, *existing_kind) {
                (ScanRootKind::ConfiguredDir, ScanRootKind::ExplicitFile) => true,
                (ScanRootKind::ExplicitFile, ScanRootKind::ConfiguredDir) => false,
                _ => path_depth(&scan_root) > path_depth(existing_root),
            };

            if prefer_new_root {
                *existing_root = scan_root;
                *existing_kind = scan_root_kind;
            }
        }
        None => {
            output.insert(path, (scan_root, scan_root_kind));
        }
    }
}

fn path_depth(path: &Path) -> usize {
    path.components().count()
}

fn is_org_source_path(path: &Path) -> bool {
    path.extension()
        .and_then(|value| value.to_str())
        .is_some_and(|value| value.eq_ignore_ascii_case("org"))
}

fn canonicalize_existing_file(path: &Path) -> Result<PathBuf, IndexerError> {
    let canonical = fs::canonicalize(path).map_err(|source| IndexerError::Discover {
        path: path.to_path_buf(),
        source,
    })?;
    let metadata = fs::metadata(&canonical).map_err(|source| IndexerError::Discover {
        path: path.to_path_buf(),
        source,
    })?;
    if !metadata.is_file() {
        return Err(IndexerError::Discover {
            path: path.to_path_buf(),
            source: io::Error::new(io::ErrorKind::InvalidInput, "configured path is not a file"),
        });
    }
    Ok(canonical)
}

fn canonicalize_existing_dir(path: &Path) -> Result<PathBuf, IndexerError> {
    let canonical = fs::canonicalize(path).map_err(|source| IndexerError::Discover {
        path: path.to_path_buf(),
        source,
    })?;
    let metadata = fs::metadata(&canonical).map_err(|source| IndexerError::Discover {
        path: path.to_path_buf(),
        source,
    })?;
    if !metadata.is_dir() {
        return Err(IndexerError::Discover {
            path: path.to_path_buf(),
            source: io::Error::new(
                io::ErrorKind::InvalidInput,
                "configured path is not a directory",
            ),
        });
    }
    Ok(canonical)
}

trait FileSnapshotReader {
    fn metadata(&mut self, path: &Path) -> Result<FileMetadata, IndexerError>;
    fn read_bytes(&mut self, path: &Path) -> Result<Vec<u8>, IndexerError>;
}

struct FilesystemSnapshotReader;

#[derive(Debug, Clone, PartialEq, Eq)]
struct FileMetadata {
    mtime_ns: i64,
    size: i64,
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

fn capture_stable_source(path: &Path) -> Result<CapturedSource, IndexerError> {
    capture_stable_source_with(&mut FilesystemSnapshotReader, path)
}

fn capture_stable_source_with(
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

        let content_hash = format!("sha256:{:x}", Sha256::digest(&bytes));
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

fn decode_captured_source(path: &Path, bytes: Vec<u8>) -> Result<String, IndexerError> {
    String::from_utf8(bytes).map_err(|source| IndexerError::ReadFile {
        path: path.to_path_buf(),
        source: io::Error::new(io::ErrorKind::InvalidData, source),
    })
}

#[allow(dead_code)]
fn qualified_sha256_hash(value: Option<&str>) -> Option<&str> {
    let value = value?;
    let digest = value.strip_prefix("sha256:")?;
    (digest.len() == 64
        && digest
            .bytes()
            .all(|byte| byte.is_ascii_digit() || matches!(byte, b'a'..=b'f')))
    .then_some(value)
}

#[allow(dead_code)]
fn sqlite_fts5_available_read_only(connection: &Connection) -> rusqlite::Result<bool> {
    connection
        .query_row(
            "SELECT EXISTS(SELECT 1 FROM pragma_module_list WHERE name = 'fts5')",
            [],
            |row| row.get::<_, i64>(0),
        )
        .map(|value| value != 0)
}

#[allow(dead_code)]
fn load_persisted_file_snapshots(
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

#[allow(dead_code)]
fn error_path(error: &IndexerError) -> PathBuf {
    match error {
        IndexerError::Discover { path, .. }
        | IndexerError::InvalidFileMetadata { path, .. }
        | IndexerError::UnstableFileSnapshot { path }
        | IndexerError::Parse { path, .. }
        | IndexerError::ReadFile { path, .. } => path.clone(),
        _ => PathBuf::new(),
    }
}

fn file_metadata(path: &Path) -> Result<FileMetadata, IndexerError> {
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

fn build_file_record(
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

fn file_record_for_write(
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

#[allow(dead_code)]
fn snapshot_matches_record(path: &Path, record: &FileRecordInput) -> Result<bool, IndexerError> {
    let captured = capture_stable_source(path)?;
    Ok(captured.snapshot.mtime_ns == record.mtime_ns
        && captured.snapshot.size == record.size
        && record.content_hash.as_deref() == Some(captured.snapshot.content_hash.as_str()))
}

fn metadata_matches_record(path: &Path, record: &FileRecordInput) -> Result<bool, IndexerError> {
    let metadata = file_metadata(path)?;
    Ok(metadata.mtime_ns == record.mtime_ns && metadata.size == record.size)
}

#[allow(dead_code)]
fn plan_baseline_matches(connection: &Connection, plan: &ChangePlan) -> Result<bool, IndexerError> {
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

fn same_persisted_file_baseline(
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

#[allow(dead_code)]
fn row_matches_file_record(
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

#[allow(dead_code)]
fn update_existing_file_metadata(
    connection: &Connection,
    file: &PlannedFile,
) -> Result<(), IndexerError> {
    let record = file_record_for_write(&file.file_record, &file.path)?;
    connection.execute(
        "UPDATE files SET mtime_ns = ?1, size = ?2, content_hash = ?3, indexed_at = ?4 WHERE id = ?5",
        rusqlite::params![record.mtime_ns, record.size, record.content_hash, record.indexed_at, file.existing_file_id],
    ).map_err(|source| IndexerError::Write(DbWriteError::Write { operation: "apply_change_plan.metadata_only", source }))?;
    Ok(())
}

#[allow(dead_code)]
fn replace_prepared_file(
    connection: &Connection,
    expected_file_id: Option<i64>,
    prepared: &PreparedFile,
    index_body_text: bool,
) -> Result<(), IndexerError> {
    let record = file_record_for_write(&prepared.file_record, &prepared.path)?;
    let file_id = DbWriter::upsert_file(connection, &record).map_err(IndexerError::Write)?;
    if expected_file_id.is_some_and(|expected| expected != file_id) {
        return Err(IndexerError::Write(DbWriteError::InvalidInput(
            "planned file identity selected a different file row",
        )));
    }
    DbWriter::delete_file_data(connection, file_id).map_err(IndexerError::Write)?;
    index_document(
        connection,
        file_id,
        &prepared.document,
        &prepared.todo_keywords,
        index_body_text,
    )
    .map_err(IndexerError::Write)?;
    Ok(())
}

fn normalize_document(
    document: ParsedOrgDocument,
    path: &Path,
    content: &str,
) -> ParsedOrgDocument {
    let mut normalized = document;
    let level_zero_title = synthetic_level_zero_title(path, normalized.metadata.title.as_deref());
    let needs_level_zero = normalized
        .headings
        .first()
        .map(|heading| heading.level != 0 || heading.parent_index.is_some())
        .unwrap_or(true);

    if needs_level_zero {
        normalized.headings.insert(
            0,
            synthetic_level_zero_heading(
                path,
                content,
                &level_zero_title,
                normalized.metadata.title.as_deref(),
            ),
        );
    } else {
        let level_zero = &mut normalized.headings[0];
        level_zero.file_path = path.to_path_buf();
        level_zero.level = 0;
        level_zero.parent_index = None;
        level_zero.byte_start = 0;
        level_zero.byte_end = content.len();
        level_zero.line_number = Some(1);
        level_zero.title = level_zero_title;
        level_zero.title_raw = source_document_title(normalized.metadata.title.as_deref());
        level_zero.is_root = true;
    }

    normalize_heading_parent_indexes(&mut normalized.headings);

    for heading in &mut normalized.headings {
        heading.file_path = path.to_path_buf();
    }

    normalized.file_path = path.to_path_buf();
    normalized
}

fn normalize_heading_parent_indexes(headings: &mut [ParsedHeading]) {
    if headings.is_empty() {
        return;
    }

    headings[0].level = 0;
    headings[0].parent_index = None;
    headings[0].is_root = true;

    let mut stack = vec![0usize];
    for index in 1..headings.len() {
        let current_level = headings[index].level;
        headings[index].is_root = false;

        while let Some(&parent_index) = stack.last() {
            if headings[parent_index].level < current_level {
                break;
            }
            stack.pop();
        }

        let parent_index = stack.last().copied().unwrap_or(0);
        headings[index].parent_index = Some(parent_index);
        stack.push(index);
    }
}

fn synthetic_level_zero_heading(
    path: &Path,
    content: &str,
    title: &str,
    source_title: Option<&str>,
) -> ParsedHeading {
    let mut heading = ParsedHeading::new(path, 0, title.to_string(), 0, content.len());
    heading.title_raw = source_document_title(source_title);
    heading.line_number = Some(1);
    heading.is_root = true;
    heading
}

fn synthetic_level_zero_title(path: &Path, document_title: Option<&str>) -> String {
    if let Some(title) = source_document_title(document_title) {
        return title;
    }

    path.file_stem()
        .or_else(|| path.file_name())
        .and_then(|name| name.to_str().map(str::to_string))
        .filter(|name| !name.is_empty())
        .unwrap_or_else(|| display_path(path))
}

fn source_document_title(document_title: Option<&str>) -> Option<String> {
    document_title
        .map(str::trim)
        .filter(|title| !title.is_empty())
        .map(str::to_string)
}

fn index_document(
    connection: &Connection,
    file_id: i64,
    document: &ParsedOrgDocument,
    todo_keywords: &ResolvedTodoKeywords,
    index_body_text: bool,
) -> Result<usize, DbWriteError> {
    if document.headings.is_empty() || document.headings[0].level != 0 {
        return Err(DbWriteError::InvalidInput(
            "normalized documents must start with a level 0 heading",
        ));
    }

    let level0_heading = &document.headings[0];
    let level0_id = DbWriter::insert_level0_heading(
        connection,
        &heading_record(file_id, None, level0_heading).map_err(db_write_invalid_input)?,
    )?;

    let mut heading_ids = vec![level0_id];
    let mut outline_rows = vec![outline_record(
        level0_id,
        file_id,
        None,
        0,
        outline_root_materialized_path(),
        vec![level0_heading.title.clone()],
    )
    .map_err(db_write_invalid_input)?];

    let mut child_ordinals = vec![0usize; document.headings.len()];

    for (heading_index, heading) in document.headings.iter().enumerate().skip(1) {
        let parent_index = heading.parent_index.unwrap_or(0);
        let parent_id = heading_ids.get(parent_index).copied().ok_or_else(|| {
            DbWriteError::InvalidInput("heading parent_index must reference an earlier heading")
        })?;
        let heading_id =
            DbWriter::insert_headings(
                connection,
                &[heading_record(file_id, Some(parent_id), heading)
                    .map_err(db_write_invalid_input)?],
            )?[0];

        if heading_ids.len() != heading_index {
            return Err(DbWriteError::InvalidInput(
                "heading insertion order must match parsed heading order",
            ));
        }

        heading_ids.push(heading_id);
        let parent_outline = &outline_rows[parent_index];
        let sibling_ordinal = child_ordinals[parent_index] + 1;
        child_ordinals[parent_index] = sibling_ordinal;
        outline_rows.push(
            outline_record(
                heading_id,
                file_id,
                Some(parent_id),
                parent_outline.depth + 1,
                outline_child_materialized_path(&parent_outline.materialized_path, sibling_ordinal),
                extend_breadcrumbs(&parent_outline.breadcrumbs_json, &heading.title)
                    .map_err(db_write_invalid_input)?,
            )
            .map_err(db_write_invalid_input)?,
        );
    }

    let keyword_rows = document
        .metadata
        .keywords
        .iter()
        .map(|keyword| KeywordRecord {
            heading_id: level0_id,
            keyword: keyword.key.clone(),
            value: keyword.value.clone(),
            line_number: keyword.line_number.map(i64::from),
        })
        .collect::<Vec<_>>();
    let todo_rows = todo_keyword_rows(file_id, &todo_keywords.entries);
    let tag_rows = document
        .headings
        .iter()
        .enumerate()
        .flat_map(|(index, heading)| {
            let heading_id = heading_ids[index];
            heading
                .tags
                .iter()
                .cloned()
                .map(move |tag| TagRecord { heading_id, tag })
        })
        .collect::<Vec<_>>();
    let property_rows = document
        .headings
        .iter()
        .enumerate()
        .flat_map(|(index, heading)| {
            let heading_id = heading_ids[index];
            heading
                .properties
                .iter()
                .map(move |property| PropertyRecord {
                    heading_id,
                    key: property.key.clone(),
                    value: property.value.clone(),
                    source: property.source.as_db_str().to_string(),
                    append: property.append,
                    line_number: property.line_number.map(i64::from),
                })
        })
        .collect::<Vec<_>>();
    let body_rows = if index_body_text {
        document
            .headings
            .iter()
            .enumerate()
            .map(|(index, heading)| heading_body_record(heading_ids[index], heading))
            .filter_map(Result::transpose)
            .collect::<Result<Vec<_>, _>>()
            .map_err(db_write_invalid_input)?
    } else {
        Vec::new()
    };
    let timestamp_rows = document
        .headings
        .iter()
        .enumerate()
        .flat_map(|(index, heading)| {
            let heading_id = heading_ids[index];
            heading
                .timestamps
                .iter()
                .map(move |timestamp| timestamp_record(heading_id, timestamp))
        })
        .collect::<Result<Vec<_>, _>>()
        .map_err(db_write_invalid_input)?;
    let link_rows = document
        .links
        .iter()
        .map(|link| link_record(file_id, &heading_ids, &document.headings, link))
        .collect::<Result<Vec<_>, _>>()
        .map_err(db_write_invalid_input)?;

    DbWriter::insert_todo_keywords(connection, &todo_rows)?;
    DbWriter::insert_keywords(connection, &keyword_rows)?;
    DbWriter::insert_tags(connection, &tag_rows)?;

    let parents = document
        .headings
        .iter()
        .enumerate()
        .map(|(index, heading)| {
            (
                heading_ids[index],
                heading.parent_index.map(|parent| heading_ids[parent]),
            )
        })
        .collect::<std::collections::HashMap<_, _>>();
    let direct_tags_by_heading = document
        .headings
        .iter()
        .enumerate()
        .map(|(index, heading)| (heading_ids[index], heading.tags.clone()))
        .collect::<std::collections::HashMap<_, _>>();
    let effective_tag_rows = derive_effective_tags(&parents, &direct_tags_by_heading)
        .into_iter()
        .map(|row| EffectiveTagRecord {
            heading_id: row.heading_id,
            file_id,
            tag: row.tag,
            position: row.position,
        })
        .collect::<Vec<_>>();
    DbWriter::insert_effective_tags(connection, &effective_tag_rows)?;

    DbWriter::insert_properties(connection, &property_rows)?;
    let mut properties_by_heading = std::collections::HashMap::<i64, Vec<PropertyRow>>::new();
    for (order, property) in property_rows.iter().enumerate() {
        properties_by_heading
            .entry(property.heading_id)
            .or_default()
            .push(PropertyRow {
                id: order as i64,
                heading_id: property.heading_id,
                key: property.key.clone(),
                value: property.value.clone(),
                append: property.append,
                line_number: property.line_number,
            });
    }
    let projection = derive_effective_properties(&parents, &properties_by_heading)
        .into_iter()
        .map(|row| EffectivePropertyRecord {
            heading_id: row.heading_id,
            file_id,
            key: row.key,
            local_value: row.local_value,
            effective_value: row.effective_value,
        })
        .collect::<Vec<_>>();
    DbWriter::insert_effective_properties(connection, &projection)?;
    DbWriter::insert_outline_path(connection, &outline_rows)?;
    DbWriter::insert_heading_bodies(connection, &body_rows)?;
    DbWriter::insert_links(connection, &link_rows)?;
    let timestamp_ids = DbWriter::insert_timestamps(connection, &timestamp_rows)?;
    let timestamp_repeater_rows = document
        .headings
        .iter()
        .flat_map(|heading| heading.timestamps.iter())
        .zip(timestamp_ids.iter().copied())
        .filter_map(|(timestamp, timestamp_id)| {
            timestamp_repeater_record(timestamp_id, &timestamp.modifiers).transpose()
        })
        .collect::<Result<Vec<_>, _>>()
        .map_err(db_write_invalid_input)?;
    DbWriter::insert_timestamp_repeaters(connection, &timestamp_repeater_rows)?;

    Ok(document.headings.len())
}

fn timestamp_record(
    heading_id: i64,
    timestamp: &ParsedTimestamp,
) -> Result<TimestampRecord, &'static str> {
    Ok(TimestampRecord {
        heading_id,
        role: timestamp.role.map(timestamp_role_name),
        has_time: timestamp.has_time,
        start_ts: timestamp.start_ts,
        end_ts: timestamp.end_ts,
        timestamp_type: Some(timestamp_type_name(timestamp).to_string()),
        range_type: Some(timestamp_range_type_name(timestamp).to_string()),
        raw_value: timestamp.raw_value.clone(),
        byte_start: i64::try_from(timestamp.byte_start)
            .map_err(|_| "timestamp byte_start out of range")?,
        byte_end: i64::try_from(timestamp.byte_end)
            .map_err(|_| "timestamp byte_end out of range")?,
        line_number: timestamp.line_number.map(i64::from),
    })
}

fn link_record(
    file_id: i64,
    heading_ids: &[i64],
    headings: &[ParsedHeading],
    link: &ParsedLink,
) -> Result<LinkRecord, &'static str> {
    let heading_index = owning_heading_index(headings, link.byte_start)
        .ok_or("link byte range must attach to a heading including root")?;

    Ok(LinkRecord {
        id: None,
        file_id,
        heading_id: heading_ids
            .get(heading_index)
            .copied()
            .ok_or("link heading index must reference an inserted heading")?,
        byte_start: i64::try_from(link.byte_start).map_err(|_| "link byte_start out of range")?,
        byte_end: i64::try_from(link.byte_end).map_err(|_| "link byte_end out of range")?,
        line: i64::from(link.line),
        source_context: link.source_context.as_db_str().to_string(),
        format: link.format.clone(),
        raw: link.raw.clone(),
        raw_target: link.raw_target.clone(),
        raw_description: link.raw_description.clone(),
        link_type: link.link_type.clone(),
        path: link.path.clone(),
        search_option: link.search_option.clone(),
    })
}

fn owning_heading_index(headings: &[ParsedHeading], byte_start: usize) -> Option<usize> {
    headings
        .iter()
        .enumerate()
        .rev()
        .find(|(_, heading)| heading.byte_start <= byte_start && byte_start < heading.byte_end)
        .map(|(index, _)| index)
}

fn timestamp_repeater_record(
    timestamp_id: i64,
    modifiers: &[crate::parser::ParsedTimestampModifier],
) -> Result<Option<TimestampRepeaterRecord>, &'static str> {
    let mut row = TimestampRepeaterRecord {
        timestamp_id,
        repeater_type: None,
        repeater_value: None,
        repeater_unit: None,
        repeater_deadline_value: None,
        repeater_deadline_unit: None,
        warning_type: None,
        warning_value: None,
        warning_unit: None,
    };

    for modifier in modifiers {
        match modifier.kind {
            ParsedTimestampModifierKind::Repeater => {
                if row.repeater_type.is_some() {
                    return Err("timestamp modifiers must not contain multiple repeater entries");
                }
                row.repeater_type = Some(repeater_modifier_type_name(modifier.modifier_type)?);
                row.repeater_value = Some(modifier.value);
                row.repeater_unit = Some(timestamp_unit_name(modifier.unit).to_string());
                row.repeater_deadline_value = modifier.repeater_deadline_value;
                row.repeater_deadline_unit = modifier
                    .repeater_deadline_unit
                    .map(|unit| timestamp_unit_name(unit).to_string());
            }
            ParsedTimestampModifierKind::Warning => {
                if row.warning_type.is_some() {
                    return Err("timestamp modifiers must not contain multiple warning entries");
                }
                row.warning_type = Some(warning_modifier_type_name(modifier.modifier_type)?);
                row.warning_value = Some(modifier.value);
                row.warning_unit = Some(timestamp_unit_name(modifier.unit).to_string());
            }
        }
    }

    if row.repeater_type.is_none() && row.warning_type.is_none() {
        return Ok(None);
    }

    Ok(Some(row))
}

fn heading_record(
    file_id: i64,
    parent_id: Option<i64>,
    heading: &ParsedHeading,
) -> Result<HeadingRecord, &'static str> {
    let todo_type = heading.todo_type.as_ref().map(|value| match value {
        TodoType::Open => "open".to_string(),
        TodoType::Closed => "closed".to_string(),
    });

    Ok(HeadingRecord {
        id: None,
        file_id,
        parent_id,
        level: i64::from(heading.level),
        line_number: heading.line_number.map(i64::from),
        byte_start: if heading.level == 0 {
            -1
        } else {
            i64::try_from(heading.byte_start).map_err(|_| "byte_start out of range")?
        },
        byte_end: i64::try_from(heading.byte_end).map_err(|_| "byte_end out of range")?,
        title: heading.title.clone(),
        title_raw: heading.title_raw.clone(),
        todo_keyword: heading.todo_keyword.clone(),
        todo_type,
        priority: heading.priority.clone(),
        scheduled_raw: heading.planning.scheduled_raw().map(str::to_string),
        scheduled_ts: heading.planning.scheduled_ts(),
        scheduled_has_time: heading.planning.scheduled_has_time(),
        deadline_raw: heading.planning.deadline_raw().map(str::to_string),
        deadline_ts: heading.planning.deadline_ts(),
        deadline_has_time: heading.planning.deadline_has_time(),
        closed_raw: heading.planning.closed_raw().map(str::to_string),
        closed_ts: heading.planning.closed_ts(),
        closed_has_time: heading.planning.closed_has_time(),
        archivedp: heading.is_archived,
        footnote_section_p: false,
    })
}

fn timestamp_role_name(role: ParsedTimestampRole) -> String {
    match role {
        ParsedTimestampRole::Scheduled => "scheduled".to_string(),
        ParsedTimestampRole::Deadline => "deadline".to_string(),
        ParsedTimestampRole::Closed => "closed".to_string(),
        ParsedTimestampRole::Body => "body".to_string(),
    }
}

fn timestamp_type_name(timestamp: &ParsedTimestamp) -> &'static str {
    match timestamp.timestamp_type {
        crate::parser::ParsedTimestampType::Active => "active",
        crate::parser::ParsedTimestampType::Inactive => "inactive",
        crate::parser::ParsedTimestampType::Diary => "diary",
    }
}

fn timestamp_range_type_name(timestamp: &ParsedTimestamp) -> &'static str {
    match timestamp.range_type {
        crate::parser::ParsedTimestampRangeType::None => "none",
        crate::parser::ParsedTimestampRangeType::DateRange => "date_range",
        crate::parser::ParsedTimestampRangeType::TimeRange => "time_range",
        crate::parser::ParsedTimestampRangeType::DateTimeRange => "datetime_range",
        crate::parser::ParsedTimestampRangeType::Unknown => "unknown",
    }
}

fn repeater_modifier_type_name(
    modifier_type: ParsedTimestampModifierType,
) -> Result<String, &'static str> {
    match modifier_type {
        ParsedTimestampModifierType::Cumulate => Ok("cumulate".to_string()),
        ParsedTimestampModifierType::CatchUp => Ok("catch_up".to_string()),
        ParsedTimestampModifierType::Restart => Ok("restart".to_string()),
        ParsedTimestampModifierType::All | ParsedTimestampModifierType::First => {
            Err("warning modifier type cannot be stored as a repeater")
        }
    }
}

fn warning_modifier_type_name(
    modifier_type: ParsedTimestampModifierType,
) -> Result<String, &'static str> {
    match modifier_type {
        ParsedTimestampModifierType::All => Ok("all".to_string()),
        ParsedTimestampModifierType::First => Ok("first".to_string()),
        ParsedTimestampModifierType::Cumulate
        | ParsedTimestampModifierType::CatchUp
        | ParsedTimestampModifierType::Restart => {
            Err("repeater modifier type cannot be stored as a warning")
        }
    }
}

fn timestamp_unit_name(unit: ParsedTimestampUnit) -> &'static str {
    match unit {
        ParsedTimestampUnit::Hour => "hour",
        ParsedTimestampUnit::Day => "day",
        ParsedTimestampUnit::Week => "week",
        ParsedTimestampUnit::Month => "month",
        ParsedTimestampUnit::Year => "year",
    }
}

fn todo_keyword_rows(
    file_id: i64,
    todo_keywords: &[ResolvedTodoKeywordEntry],
) -> Vec<TodoKeywordRecord> {
    todo_keywords
        .iter()
        .map(|keyword| TodoKeywordRecord {
            file_id,
            keyword: keyword.keyword.clone(),
            state_type: keyword.state_type.clone(),
            shortcut: keyword.shortcut,
            sequence_no: keyword.sequence_no,
            source_kind: keyword.source_kind.as_db_str().to_string(),
            source_keyword: keyword.source_keyword.clone(),
            source_line_number: keyword.source_line_number.map(i64::from),
        })
        .collect()
}

fn outline_record(
    heading_id: i64,
    file_id: i64,
    parent_id: Option<i64>,
    depth: i64,
    materialized_path: String,
    breadcrumbs: Vec<String>,
) -> Result<OutlinePathRecord, &'static str> {
    Ok(OutlinePathRecord {
        heading_id,
        file_id,
        parent_id,
        depth,
        materialized_path,
        breadcrumbs_json: serde_json::to_string(&breadcrumbs)
            .map_err(|_| "outline breadcrumb serialization failed")?,
    })
}

fn extend_breadcrumbs(breadcrumbs_json: &str, title: &str) -> Result<Vec<String>, &'static str> {
    let mut breadcrumbs: Vec<String> = serde_json::from_str(breadcrumbs_json)
        .map_err(|_| "outline breadcrumb deserialization failed")?;
    breadcrumbs.push(title.to_string());
    Ok(breadcrumbs)
}

fn zero_pad_path_segment(value: usize) -> String {
    format!("{value:04}")
}

fn outline_root_materialized_path() -> String {
    zero_pad_path_segment(0)
}

fn outline_child_materialized_path(parent_path: &str, sibling_ordinal: usize) -> String {
    format!("{parent_path}.{}", zero_pad_path_segment(sibling_ordinal))
}

fn heading_body_record(
    heading_id: i64,
    heading: &ParsedHeading,
) -> Result<Option<HeadingBodyRecord>, &'static str> {
    let Some(body_text) = heading.body_text.clone() else {
        return Ok(None);
    };

    Ok(Some(HeadingBodyRecord {
        heading_id,
        body_text,
        body_byte_start: heading
            .body_byte_start
            .map(i64::try_from)
            .transpose()
            .map_err(|_| "heading body_byte_start out of range")?,
        body_byte_end: heading
            .body_byte_end
            .map(i64::try_from)
            .transpose()
            .map_err(|_| "heading body_byte_end out of range")?,
    }))
}

fn db_write_invalid_input(message: &'static str) -> DbWriteError {
    DbWriteError::InvalidInput(message)
}

#[cfg(test)]
mod tests {
    use super::{
        capture_stable_source_with, discover_org_files, ChangeApplicationRejection,
        ChangeApplicationResult, ChangePlanningOptions, ChangePlanningResult, DiscoveredOrgFile,
        FileMetadata, FileSnapshotReader, IndexInvalidationSet, IndexedFile, Indexer, IndexerError,
        PersistedFileSnapshot, PlannedCurrentFile, PlanningScope, RebuildOptions,
    };
    use crate::{
        config::{Config, ConfiguredDir, SearchConfig},
        db::{
            open_in_memory_database_with_schema, sqlite_supports_fts5, DbReader, DbWriter,
            FileRecordInput, HeadingRecord, SchemaDefinition, CURRENT_SCHEMA_VERSION,
            DB_METADATA_BODY_TEXT_AVAILABLE_KEY,
            DB_METADATA_INDEXING_DERIVED_SEARCH_FINGERPRINT_KEY,
            DB_METADATA_INDEXING_DISCOVERY_FINGERPRINT_KEY,
            DB_METADATA_INDEXING_SEMANTICS_FINGERPRINT_KEY,
            DB_METADATA_INDEXING_SEMANTICS_VERSION_KEY, DB_METADATA_SOURCE_ROOT_EVIDENCE_KEY,
            DB_METADATA_SOURCE_ROOT_EVIDENCE_VERSION_KEY,
        },
        file_identity::FileIdentity,
        link_resolver::{
            CUSTOM_ID_MISSING_DIAGNOSTIC, DUPLICATE_ID_DIAGNOSTIC, FILE_MISSING_DIAGNOSTIC,
            FILE_OUTSIDE_UNIVERSE_DIAGNOSTIC, HEADING_TITLE_MISSING_DIAGNOSTIC,
            ID_MISSING_DIAGNOSTIC, SAME_FILE_STAR_HEADING_MISSING_DIAGNOSTIC,
            UNSUPPORTED_DIAGNOSTIC,
        },
        parser::{OrgParserCore, OrgizeAdapter, ParseDiagnostic, ParseOptions, ParsedOrgDocument},
        query::{
            execute_sqlite_query, parse_query, validate_query, QueryRows, QueryValidationOptions,
        },
    };
    use rusqlite::Connection;
    use sha2::{Digest, Sha256};
    use std::{
        collections::VecDeque,
        fs,
        os::unix::fs::MetadataExt,
        path::{Path, PathBuf},
        time::{SystemTime, UNIX_EPOCH},
    };

    struct ScriptedSnapshotReader {
        metadata: VecDeque<FileMetadata>,
        bytes: VecDeque<Vec<u8>>,
    }

    impl FileSnapshotReader for ScriptedSnapshotReader {
        fn metadata(&mut self, _path: &Path) -> Result<FileMetadata, IndexerError> {
            Ok(self
                .metadata
                .pop_front()
                .expect("test metadata should exist"))
        }

        fn read_bytes(&mut self, _path: &Path) -> Result<Vec<u8>, IndexerError> {
            Ok(self.bytes.pop_front().expect("test bytes should exist"))
        }
    }

    #[derive(Debug, PartialEq, Eq)]
    struct StoredLinkRow {
        heading_title: String,
        format: String,
        raw: String,
        raw_target: String,
        raw_description: Option<String>,
        link_type: String,
        path: String,
        search_option: Option<String>,
        source_context: String,
    }

    type TimestampRow = (String, String, String, String, Option<i64>, Option<i64>);
    type PlanningHasTimeRow = (String, Option<i64>, Option<i64>, Option<i64>, Option<i64>);
    type TimestampHasTimeRow = (String, String, String, Option<i64>, Option<i64>);
    type RepeaterRow = (
        String,
        Option<String>,
        Option<i64>,
        Option<String>,
        Option<i64>,
        Option<String>,
        Option<String>,
        Option<i64>,
        Option<String>,
    );
    type PropertyRow = (i64, String, Option<String>, String, i64, Option<i64>);
    type KeywordRow = (String, Option<String>, Option<i64>);
    type TodoProvenanceRow = (
        String,
        String,
        Option<String>,
        i64,
        String,
        Option<String>,
        Option<i64>,
    );
    type LinkResolutionRow = (String, Option<String>, Option<String>, Option<String>);
    type TargetRemovalLinkRow = (String, String, Option<i64>, Option<String>, Option<String>);
    type FileHeadingSearchResolutionRow = (
        String,
        Option<String>,
        Option<String>,
        Option<String>,
        Option<String>,
        Option<String>,
    );
    type FileOnlyRootResolutionRow = (
        String,
        Option<String>,
        Option<String>,
        Option<String>,
        Option<String>,
    );
    type SameFileStarHeadingResolutionRow = (
        String,
        Option<String>,
        Option<String>,
        Option<String>,
        Option<String>,
        Option<String>,
        Option<String>,
    );
    type SameFileCustomIdResolutionRow = (
        String,
        Option<String>,
        Option<String>,
        Option<String>,
        Option<String>,
        Option<String>,
        Option<String>,
    );
    type FileContextCustomIdResolutionRow = (
        String,
        Option<String>,
        Option<String>,
        Option<String>,
        Option<String>,
        Option<String>,
        Option<String>,
    );
    type OrgIdResolutionRow = (
        String,
        Option<String>,
        Option<String>,
        Option<String>,
        Option<String>,
        Option<String>,
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
                "org-files-db-indexer-tests-{}-{}-{}",
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

    fn write_file(path: &Path, content: &str) {
        if let Some(parent) = path.parent() {
            fs::create_dir_all(parent).expect("parent dir should exist");
        }
        fs::write(path, content).expect("file should be written");
    }

    fn write_config(path: &Path, body: &str) {
        write_file(path, body);
    }

    fn recursive_root_config(test_dir: &TestDir, roots: Vec<PathBuf>) -> Config {
        Config {
            db_path: test_dir.path().join("db.sqlite"),
            files: Vec::new(),
            dirs: roots
                .into_iter()
                .map(|path| ConfiguredDir {
                    path,
                    recursive: true,
                    exclude: Vec::new(),
                })
                .collect(),
            discovery: Default::default(),
            links: Default::default(),
            todo: Default::default(),
            search: SearchConfig {
                fts5_enabled: false,
                index_body_text: false,
            },
            query: Default::default(),
        }
    }

    fn replace_directory_root(root: &Path, files: &[(&str, &str)]) -> PathBuf {
        let previous = root.with_extension("previous");
        fs::rename(root, &previous).expect("original root should move aside");
        fs::create_dir_all(root).expect("replacement root should be created");
        for (relative, content) in files {
            write_file(&root.join(relative), content);
        }
        previous
    }

    fn indexed_titles(connection: &Connection) -> Vec<String> {
        connection
            .prepare("SELECT title FROM headings WHERE level = 1 ORDER BY title")
            .expect("title query should prepare")
            .query_map([], |row| row.get::<_, String>(0))
            .expect("title query should execute")
            .collect::<Result<Vec<_>, _>>()
            .expect("titles should read")
    }

    fn metadata_rows(connection: &Connection) -> Vec<(String, String)> {
        connection
            .prepare("SELECT key, value FROM db_metadata ORDER BY key")
            .expect("metadata query should prepare")
            .query_map([], |row| Ok((row.get(0)?, row.get(1)?)))
            .expect("metadata query should execute")
            .collect::<Result<Vec<_>, _>>()
            .expect("metadata rows should read")
    }

    fn seed_indexed_file(connection: &Connection) {
        let file_id = DbWriter::upsert_file(
            connection,
            &FileRecordInput {
                path: PathBuf::from("/tmp/existing.org"),
                identity: None,
                mtime_ns: 1,
                size: 1,
                content_hash: None,
                indexed_at: None,
            },
        )
        .expect("file should insert");

        DbWriter::insert_level0_heading(
            connection,
            &HeadingRecord {
                id: None,
                file_id,
                parent_id: None,
                level: 0,
                line_number: Some(1),
                byte_start: -1,
                byte_end: 1,
                title: "Existing".to_string(),
                title_raw: Some("Existing".to_string()),
                todo_keyword: None,
                todo_type: None,
                priority: None,
                scheduled_raw: None,
                scheduled_ts: None,
                scheduled_has_time: None,
                deadline_raw: None,
                deadline_ts: None,
                deadline_has_time: None,
                closed_raw: None,
                closed_ts: None,
                closed_has_time: None,
                archivedp: false,
                footnote_section_p: false,
            },
        )
        .expect("heading should insert");
    }

    #[test]
    fn rebuild_processes_org_files_from_loaded_config() {
        let test_dir = TestDir::new("rebuild-from-config");
        let notes_dir = test_dir.path().join("notes");
        let db_path = test_dir.path().join("db.sqlite");
        let config_path = test_dir.path().join("config.toml");
        let org_path = notes_dir.join("project.org");

        write_file(
            &org_path,
            "#+TITLE: Project Notes\n#+TODO: PLAN(p) | DONE(d)\n* PLAN Inbox :rust:\n:PROPERTIES:\n:CUSTOM_ID: inbox\n:END:\nSCHEDULED: <2026-06-18 Thu>\n",
        );
        write_config(
            &config_path,
            r#"
db_path = "db.sqlite"
[[dirs]]
path = "notes/../notes"
recursive = true

[todo]
default_open_keywords = ["TODO(t)"]
default_closed_keywords = ["DONE(d)"]

[search]
fts5_enabled = false
index_body_text = false
"#,
        );

        let report = Indexer::new(OrgizeAdapter::new())
            .rebuild_from_config_path(&config_path)
            .expect("rebuild should succeed");

        assert_eq!(
            report.indexed_files,
            vec![IndexedFile {
                path: org_path.clone(),
                file_id: 1,
                heading_count: 2,
            }]
        );
        assert!(report.diagnostics.is_empty());

        let connection = Connection::open(&db_path).expect("db should open");
        let headings = DbReader::list_headings(&connection).expect("headings should load");
        assert_eq!(headings.len(), 2);
        assert_eq!(headings[0].level, 0);
        assert_eq!(headings[0].title, "Project Notes");
        assert_eq!(headings[1].title, "Inbox");
        assert_eq!(headings[1].todo_keyword.as_deref(), Some("PLAN"));
        assert_eq!(headings[1].todo_type.as_deref(), Some("open"));
        assert_eq!(headings[1].all_tags, vec!["rust".to_string()]);

        let todo_rows: Vec<TodoProvenanceRow> = query_rows(
            &connection,
            "SELECT keyword, state_type, shortcut, sequence_no, source_kind, source_keyword, source_line_number
             FROM todo_keywords
             ORDER BY sequence_no",
            |row| {
                Ok((
                    row.get(0)?,
                    row.get(1)?,
                    row.get(2)?,
                    row.get(3)?,
                    row.get(4)?,
                    row.get(5)?,
                    row.get(6)?,
                ))
            },
        );
        assert_eq!(
            todo_rows,
            vec![
                (
                    "PLAN".to_string(),
                    "open".to_string(),
                    Some("p".to_string()),
                    0,
                    "org_keyword".to_string(),
                    Some("TODO".to_string()),
                    Some(2),
                ),
                (
                    "DONE".to_string(),
                    "closed".to_string(),
                    Some("d".to_string()),
                    1,
                    "org_keyword".to_string(),
                    Some("TODO".to_string()),
                    Some(2),
                ),
            ]
        );

        let keywords: Vec<(String, Option<String>)> = query_rows(
            &connection,
            "SELECT keyword, value FROM keywords ORDER BY keyword",
            |row| Ok((row.get(0)?, row.get(1)?)),
        );
        assert_eq!(
            keywords,
            vec![
                ("TITLE".to_string(), Some("Project Notes".to_string())),
                ("TODO".to_string(), Some("PLAN(p) | DONE(d)".to_string())),
            ]
        );

        let properties: Vec<(String, Option<String>, String)> = query_rows(
            &connection,
            "SELECT key, value, source FROM properties",
            |row| Ok((row.get(0)?, row.get(1)?, row.get(2)?)),
        );
        assert_eq!(
            properties,
            vec![(
                "CUSTOM_ID".to_string(),
                Some("inbox".to_string()),
                "property_drawer".to_string(),
            )]
        );
    }

    #[test]
    fn rebuild_can_mix_recursive_and_non_recursive_directory_roots() {
        let test_dir = TestDir::new("mixed-dir-recursion");
        let recursive_dir = test_dir.path().join("notes");
        let non_recursive_dir = test_dir.path().join("inbox");
        let config_path = test_dir.path().join("config.toml");

        let recursive_root = recursive_dir.join("root.org");
        let recursive_child = recursive_dir.join("nested/child.org");
        let non_recursive_root = non_recursive_dir.join("top.org");
        let non_recursive_child = non_recursive_dir.join("nested/skipped.org");

        write_file(&recursive_root, "* Recursive root\n");
        write_file(&recursive_child, "* Recursive child\n");
        write_file(&non_recursive_root, "* Inbox root\n");
        write_file(&non_recursive_child, "* Inbox child\n");

        write_config(
            &config_path,
            r#"
db_path = "db.sqlite"

[[dirs]]
path = "notes"
recursive = true

[[dirs]]
path = "inbox"
recursive = false

[search]
fts5_enabled = false
index_body_text = false
"#,
        );

        let report = Indexer::new(OrgizeAdapter::new())
            .rebuild_from_config_path(&config_path)
            .expect("rebuild should succeed");

        assert_eq!(
            report
                .indexed_files
                .iter()
                .map(|file| file.path.clone())
                .collect::<Vec<_>>(),
            vec![non_recursive_root, recursive_child, recursive_root]
        );
    }

    #[test]
    fn rebuild_defaults_directory_entries_to_non_recursive() {
        let test_dir = TestDir::new("default-dir-recursion");
        let notes_dir = test_dir.path().join("notes");
        let db_path = test_dir.path().join("db.sqlite");
        let config_path = test_dir.path().join("config.toml");
        let root_file = notes_dir.join("root.org");
        let nested_file = notes_dir.join("nested/skipped.org");

        write_file(&root_file, "* Root\n");
        write_file(&nested_file, "* Nested\n");

        write_config(
            &config_path,
            r#"
db_path = "db.sqlite"

[[dirs]]
path = "notes"

[search]
fts5_enabled = false
index_body_text = false
"#,
        );

        let report = Indexer::new(OrgizeAdapter::new())
            .rebuild_from_config_path(&config_path)
            .expect("rebuild should succeed");

        assert_eq!(
            report
                .indexed_files
                .iter()
                .map(|file| file.path.clone())
                .collect::<Vec<_>>(),
            vec![root_file]
        );

        let connection = Connection::open(&db_path).expect("db should open");
        let file_count: i64 = connection
            .query_row("SELECT COUNT(*) FROM files", [], |row| row.get(0))
            .expect("file count should load");
        assert_eq!(file_count, 1);
    }

    #[test]
    fn rebuild_persists_level_zero_and_duplicate_direct_properties() {
        let test_dir = TestDir::new("rebuild-properties");
        let notes_dir = test_dir.path().join("notes");
        let db_path = test_dir.path().join("db.sqlite");
        let config_path = test_dir.path().join("config.toml");
        let org_path = notes_dir.join("properties.org");

        write_file(
            &org_path,
            ":PROPERTIES:\n:CATEGORY: Level 0 Category Property\n:var+: root\n:END:\n#+TITLE: Project Notes\n#+PROPERTY: Effort_ALL 0:10 0:30 1:00\n* TODO Inbox :rust:\n:PROPERTIES:\n:CUSTOM_ID: inbox\n:Owner: Alice\n:owner: Bob\n:var+: baz=3\n:END:\n#+PROPERTY: var+ bar=2\n#+CATEGORY: project\n",
        );
        write_config(
            &config_path,
            r#"
db_path = "db.sqlite"
[[dirs]]
path = "notes"
recursive = true

[todo]
default_open_keywords = ["TODO(t)"]
default_closed_keywords = ["DONE(d)"]

[search]
fts5_enabled = false
index_body_text = false
"#,
        );

        let report = Indexer::new(OrgizeAdapter::new())
            .rebuild_from_config_path(&config_path)
            .expect("rebuild should succeed");

        assert_eq!(report.indexed_files.len(), 1);
        assert!(report.diagnostics.is_empty());

        let connection = Connection::open(&db_path).expect("db should open");
        let properties: Vec<PropertyRow> = query_rows(
            &connection,
            "SELECT headings.level, properties.key, properties.value, properties.source, properties.append, properties.line_number
             FROM properties
             INNER JOIN headings ON headings.id = properties.heading_id
             ORDER BY headings.level, properties.line_number, properties.id",
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
        );
        assert_eq!(
            properties,
            vec![
                (
                    0,
                    "CATEGORY".to_string(),
                    Some("Level 0 Category Property".to_string()),
                    "property_drawer".to_string(),
                    0,
                    Some(2),
                ),
                (
                    0,
                    "VAR".to_string(),
                    Some("root".to_string()),
                    "property_drawer".to_string(),
                    1,
                    Some(3),
                ),
                (
                    0,
                    "EFFORT_ALL".to_string(),
                    Some("0:10 0:30 1:00".to_string()),
                    "property_keyword".to_string(),
                    0,
                    Some(6),
                ),
                (
                    0,
                    "VAR".to_string(),
                    Some("bar=2".to_string()),
                    "property_keyword".to_string(),
                    1,
                    Some(14),
                ),
                (
                    0,
                    "CATEGORY".to_string(),
                    Some("project".to_string()),
                    "category_keyword".to_string(),
                    0,
                    Some(15),
                ),
                (
                    1,
                    "CUSTOM_ID".to_string(),
                    Some("inbox".to_string()),
                    "property_drawer".to_string(),
                    0,
                    Some(9),
                ),
                (
                    1,
                    "OWNER".to_string(),
                    Some("Alice".to_string()),
                    "property_drawer".to_string(),
                    0,
                    Some(10),
                ),
                (
                    1,
                    "OWNER".to_string(),
                    Some("Bob".to_string()),
                    "property_drawer".to_string(),
                    0,
                    Some(11),
                ),
                (
                    1,
                    "VAR".to_string(),
                    Some("baz=3".to_string()),
                    "property_drawer".to_string(),
                    1,
                    Some(12),
                ),
            ]
        );

        let raw_keywords: Vec<KeywordRow> = query_rows(
            &connection,
            "SELECT keyword, value, line_number
             FROM keywords
             WHERE keyword IN ('PROPERTY', 'CATEGORY')
             ORDER BY line_number, rowid",
            |row| Ok((row.get(0)?, row.get(1)?, row.get(2)?)),
        );
        assert_eq!(
            raw_keywords,
            vec![
                (
                    "PROPERTY".to_string(),
                    Some("Effort_ALL 0:10 0:30 1:00".to_string()),
                    Some(6),
                ),
                (
                    "PROPERTY".to_string(),
                    Some("var+ bar=2".to_string()),
                    Some(14),
                ),
                (
                    "CATEGORY".to_string(),
                    Some("project".to_string()),
                    Some(15),
                ),
            ]
        );
    }

    #[test]
    fn rebuild_indexes_empty_property_drawer_rows_and_property_queries_can_match_them() {
        let test_dir = TestDir::new("rebuild-empty-property-drawers");
        let notes_dir = test_dir.path().join("notes");
        let db_path = test_dir.path().join("db.sqlite");
        let config_path = test_dir.path().join("config.toml");
        let org_path = notes_dir.join("empty-property-drawers.org");

        write_file(
            &org_path,
            "\
#+TITLE: Empty Property Drawer Fixture
* Empty base followed by append
:PROPERTIES:
:VALUE:
:VALUE+: empty base followed by append
:END:

* Base followed by empty append
:PROPERTIES:
:VALUE: base followed by empty append
:VALUE+:
:END:

* Empty base with trailing space
:PROPERTIES:
:VALUE: 
:VALUE+: valid
:END:

* Empty append with trailing space
:PROPERTIES:
:VALUE: valid
:VALUE+: 
:END:

* Append only
:PROPERTIES:
:VALUE+: only
:END:
",
        );
        write_config(
            &config_path,
            r#"
db_path = "db.sqlite"
[[dirs]]
path = "notes"
recursive = true

[search]
fts5_enabled = false
index_body_text = false
"#,
        );

        let report = Indexer::new(OrgizeAdapter::new())
            .rebuild_from_config_path(&config_path)
            .expect("rebuild should succeed");

        assert_eq!(report.indexed_files.len(), 1);
        assert!(report.diagnostics.is_empty());

        let connection = Connection::open(&db_path).expect("db should open");
        let properties: Vec<(String, String, Option<String>, i64)> = query_rows(
            &connection,
            "SELECT headings.title, properties.key, properties.value, properties.append
             FROM properties
             INNER JOIN headings ON headings.id = properties.heading_id
             ORDER BY headings.line_number, properties.line_number, properties.id",
            |row| Ok((row.get(0)?, row.get(1)?, row.get(2)?, row.get(3)?)),
        );
        assert_eq!(
            properties,
            vec![
                (
                    "Empty base followed by append".to_string(),
                    "VALUE".to_string(),
                    Some("".to_string()),
                    0,
                ),
                (
                    "Empty base followed by append".to_string(),
                    "VALUE".to_string(),
                    Some("empty base followed by append".to_string()),
                    1,
                ),
                (
                    "Base followed by empty append".to_string(),
                    "VALUE".to_string(),
                    Some("base followed by empty append".to_string()),
                    0,
                ),
                (
                    "Base followed by empty append".to_string(),
                    "VALUE".to_string(),
                    Some("".to_string()),
                    1,
                ),
                (
                    "Empty base with trailing space".to_string(),
                    "VALUE".to_string(),
                    Some("".to_string()),
                    0,
                ),
                (
                    "Empty base with trailing space".to_string(),
                    "VALUE".to_string(),
                    Some("valid".to_string()),
                    1,
                ),
                (
                    "Empty append with trailing space".to_string(),
                    "VALUE".to_string(),
                    Some("valid".to_string()),
                    0,
                ),
                (
                    "Empty append with trailing space".to_string(),
                    "VALUE".to_string(),
                    Some("".to_string()),
                    1,
                ),
                (
                    "Append only".to_string(),
                    "VALUE".to_string(),
                    Some("only".to_string()),
                    1,
                ),
            ]
        );

        let query = validate_query(
            parse_query(
                r#"(headings
                     (and
                       (title "Empty base followed by append" :exact t)
                       (property "VALUE" "empty base followed by append" :inherit nil)))"#,
            )
            .expect("query should parse"),
            &QueryValidationOptions::default(),
        )
        .expect("query should validate");
        let rows = execute_sqlite_query(&connection, &query).expect("query should execute");
        let QueryRows::Headings(rows) = rows else {
            panic!("expected heading rows");
        };
        assert_eq!(rows.len(), 1);

        let query = validate_query(
            parse_query(
                r#"(headings
                     (and
                       (title "Base followed by empty append" :exact t)
                       (property "VALUE" "base followed by empty append" :inherit nil)))"#,
            )
            .expect("query should parse"),
            &QueryValidationOptions::default(),
        )
        .expect("query should validate");
        let rows = execute_sqlite_query(&connection, &query).expect("query should execute");
        let QueryRows::Headings(rows) = rows else {
            panic!("expected heading rows");
        };
        assert_eq!(rows.len(), 1);
    }

    #[test]
    fn rebuild_indexes_late_file_level_keywords_on_level_zero_heading() {
        let test_dir = TestDir::new("rebuild-late-file-level-keywords");
        let notes_dir = test_dir.path().join("notes");
        let db_path = test_dir.path().join("db.sqlite");
        let config_path = test_dir.path().join("config.toml");
        let org_path = notes_dir.join("late-file-keywords.org");
        let content =
            include_str!("../tests/data/parser/properties/late-file-keywords/fixture.org");

        write_file(&org_path, content);
        write_config(
            &config_path,
            r#"
db_path = "db.sqlite"
[[dirs]]
path = "notes"
recursive = true

[todo]
default_open_keywords = ["TODO(t)"]
default_closed_keywords = ["DONE(d)"]

[search]
fts5_enabled = false
index_body_text = false
"#,
        );

        let report = Indexer::new(OrgizeAdapter::new())
            .rebuild_from_config_path(&config_path)
            .expect("rebuild should succeed");

        assert_eq!(report.indexed_files.len(), 1);
        assert!(report.diagnostics.is_empty());

        let connection = Connection::open(&db_path).expect("db should open");
        let headings = DbReader::list_headings(&connection).expect("headings should load");
        assert_eq!(headings.len(), 4);
        assert_eq!(
            headings
                .iter()
                .map(|heading| heading.title.clone())
                .collect::<Vec<_>>(),
            vec![
                "Keyword and Property Normalization Fixture Later Title".to_string(),
                "First heading".to_string(),
                "Child heading".to_string(),
                "Second heading".to_string(),
            ]
        );
        assert_eq!(headings[1].parent_id, Some(headings[0].id));
        assert_eq!(headings[2].parent_id, Some(headings[1].id));
        assert_eq!(headings[3].parent_id, Some(headings[0].id));

        let properties: Vec<PropertyRow> = query_rows(
            &connection,
            "SELECT headings.level, properties.key, properties.value, properties.source, properties.append, properties.line_number
             FROM properties
             INNER JOIN headings ON headings.id = properties.heading_id
             ORDER BY properties.line_number, properties.id",
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
        );
        assert_eq!(
            properties,
            vec![
                (
                    0,
                    "BEFORE_PROP".to_string(),
                    Some("before-value".to_string()),
                    "property_keyword".to_string(),
                    0,
                    Some(3),
                ),
                (
                    0,
                    "CATEGORY".to_string(),
                    Some("before-category".to_string()),
                    "category_keyword".to_string(),
                    0,
                    Some(4),
                ),
                (
                    0,
                    "AFTER_PROP".to_string(),
                    Some("after-value".to_string()),
                    "property_keyword".to_string(),
                    0,
                    Some(11),
                ),
                (
                    0,
                    "REPEATED_PROP".to_string(),
                    Some("first".to_string()),
                    "property_keyword".to_string(),
                    0,
                    Some(12),
                ),
                (
                    0,
                    "REPEATED_PROP".to_string(),
                    Some("second".to_string()),
                    "property_keyword".to_string(),
                    0,
                    Some(13),
                ),
                (
                    0,
                    "APPENDED_PROP".to_string(),
                    Some("base".to_string()),
                    "property_keyword".to_string(),
                    0,
                    Some(14),
                ),
                (
                    0,
                    "APPENDED_PROP".to_string(),
                    Some("extra".to_string()),
                    "property_keyword".to_string(),
                    1,
                    Some(15),
                ),
                (
                    0,
                    "CATEGORY".to_string(),
                    Some("after-category".to_string()),
                    "category_keyword".to_string(),
                    0,
                    Some(16),
                ),
                (
                    0,
                    "SECOND_AFTER_HEADING".to_string(),
                    Some("works".to_string()),
                    "property_keyword".to_string(),
                    0,
                    Some(27),
                ),
                (
                    0,
                    "CATEGORY".to_string(),
                    Some("second-category".to_string()),
                    "category_keyword".to_string(),
                    0,
                    Some(28),
                ),
            ]
        );

        let raw_keywords: Vec<KeywordRow> = query_rows(
            &connection,
            "SELECT keyword, value, line_number
             FROM keywords
             ORDER BY line_number, rowid",
            |row| Ok((row.get(0)?, row.get(1)?, row.get(2)?)),
        );
        assert_eq!(
            raw_keywords,
            vec![
                (
                    "TITLE".to_string(),
                    Some("Keyword and Property Normalization Fixture".to_string()),
                    Some(1),
                ),
                ("STARTUP".to_string(), Some("showall".to_string()), Some(2)),
                (
                    "PROPERTY".to_string(),
                    Some("before_prop before-value".to_string()),
                    Some(3),
                ),
                (
                    "CATEGORY".to_string(),
                    Some("before-category".to_string()),
                    Some(4),
                ),
                (
                    "AUTHOR".to_string(),
                    Some("Later Author".to_string()),
                    Some(9),
                ),
                (
                    "OPTIONS".to_string(),
                    Some("toc:nil num:t".to_string()),
                    Some(10),
                ),
                (
                    "PROPERTY".to_string(),
                    Some("after_prop after-value".to_string()),
                    Some(11),
                ),
                (
                    "PROPERTY".to_string(),
                    Some("repeated_prop first".to_string()),
                    Some(12),
                ),
                (
                    "PROPERTY".to_string(),
                    Some("repeated_prop second".to_string()),
                    Some(13),
                ),
                (
                    "PROPERTY".to_string(),
                    Some("appended_prop base".to_string()),
                    Some(14),
                ),
                (
                    "PROPERTY".to_string(),
                    Some("appended_prop+ extra".to_string()),
                    Some(15),
                ),
                (
                    "CATEGORY".to_string(),
                    Some("after-category".to_string()),
                    Some(16),
                ),
                (
                    "TITLE".to_string(),
                    Some("Later Title".to_string()),
                    Some(21),
                ),
                (
                    "EXPORT_FILE_NAME".to_string(),
                    Some("later-export-name".to_string()),
                    Some(22),
                ),
                (
                    "PROPERTY".to_string(),
                    Some("second_after_heading works".to_string()),
                    Some(27),
                ),
                (
                    "CATEGORY".to_string(),
                    Some("second-category".to_string()),
                    Some(28),
                ),
            ]
        );
    }

    #[test]
    fn rebuild_persists_one_synthetic_root_row_per_file_with_db_sentinels() {
        let test_dir = TestDir::new("synthetic-root-sentinels");
        let notes_dir = test_dir.path().join("notes");
        let db_path = test_dir.path().join("db.sqlite");
        let config_path = test_dir.path().join("config.toml");
        let alpha_path = notes_dir.join("alpha.org");
        let beta_path = notes_dir.join("nested/beta.org");

        write_file(
            &alpha_path,
            "#+TITLE: Alpha Root\n* Alpha Top\n** Alpha Child\n",
        );
        write_file(&beta_path, "* Beta Top\n");
        write_config(
            &config_path,
            r#"
db_path = "db.sqlite"
[[dirs]]
path = "notes"
recursive = true

[search]
fts5_enabled = false
index_body_text = false
"#,
        );

        let report = Indexer::new(OrgizeAdapter::new())
            .rebuild_from_config_path(&config_path)
            .expect("rebuild should succeed");

        assert_eq!(report.indexed_files.len(), 2);
        assert!(report.diagnostics.is_empty());

        let connection = Connection::open(&db_path).expect("db should open");
        let headings: Vec<(String, i64, Option<i64>, i64, i64, String)> = query_rows(
            &connection,
            "SELECT files.path, headings.id, headings.parent_id, headings.level, headings.byte_start, headings.title
             FROM headings
             INNER JOIN files ON files.id = headings.file_id
             ORDER BY files.path, headings.level, headings.byte_start, headings.id",
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
        );

        let alpha_rows = headings
            .iter()
            .filter(|(path, ..)| path == &alpha_path.display().to_string())
            .collect::<Vec<_>>();
        let beta_rows = headings
            .iter()
            .filter(|(path, ..)| path == &beta_path.display().to_string())
            .collect::<Vec<_>>();

        assert_eq!(alpha_rows.len(), 3);
        assert_eq!(beta_rows.len(), 2);

        let alpha_root = alpha_rows
            .iter()
            .find(|(_, _, _, level, _, _)| *level == 0)
            .expect("alpha root should exist");
        let alpha_top = alpha_rows
            .iter()
            .find(|(_, _, _, level, _, title)| *level == 1 && title == "Alpha Top")
            .expect("alpha top heading should exist");
        let alpha_child = alpha_rows
            .iter()
            .find(|(_, _, _, level, _, title)| *level == 2 && title == "Alpha Child")
            .expect("alpha child heading should exist");
        let beta_root = beta_rows
            .iter()
            .find(|(_, _, _, level, _, _)| *level == 0)
            .expect("beta root should exist");
        let beta_top = beta_rows
            .iter()
            .find(|(_, _, _, level, _, title)| *level == 1 && title == "Beta Top")
            .expect("beta top heading should exist");

        assert_eq!(alpha_root.2, None);
        assert_eq!(alpha_root.4, -1);
        assert_eq!(alpha_root.5, "Alpha Root");
        assert_eq!(alpha_top.2, Some(alpha_root.1));
        assert_eq!(alpha_top.4, 20);
        assert_eq!(alpha_child.2, Some(alpha_top.1));
        assert_eq!(alpha_child.4, 32);

        assert_eq!(beta_root.2, None);
        assert_eq!(beta_root.4, -1);
        assert_eq!(beta_root.5, "beta");
        assert_eq!(beta_top.2, Some(beta_root.1));
        assert_eq!(beta_top.4, 0);

        let root_outline_rows: Vec<(String, i64, Option<i64>, String, String)> = query_rows(
            &connection,
            "SELECT files.path, outline_path.depth, outline_path.parent_id, outline_path.materialized_path, outline_path.breadcrumbs_json
             FROM outline_path
             INNER JOIN files ON files.id = outline_path.file_id
             WHERE outline_path.depth = 0
             ORDER BY files.path",
            |row| {
                Ok((
                    row.get(0)?,
                    row.get(1)?,
                    row.get(2)?,
                    row.get(3)?,
                    row.get(4)?,
                ))
            },
        );

        assert_eq!(
            root_outline_rows,
            vec![
                (
                    alpha_path.display().to_string(),
                    0,
                    None,
                    "0000".to_string(),
                    "[\"Alpha Root\"]".to_string(),
                ),
                (
                    beta_path.display().to_string(),
                    0,
                    None,
                    "0000".to_string(),
                    "[\"beta\"]".to_string(),
                ),
            ]
        );
    }

    #[test]
    fn rebuild_stores_generic_raw_keywords_on_the_synthetic_level_zero_heading() {
        let test_dir = TestDir::new("rebuild-generic-raw-keywords");
        let notes_dir = test_dir.path().join("notes");
        let db_path = test_dir.path().join("db.sqlite");
        let config_path = test_dir.path().join("config.toml");
        let org_path = notes_dir.join("raw-generic-keywords.org");
        let content =
            include_str!("../tests/data/parser/file-scope/raw-generic-keywords/fixture.org");

        write_file(&org_path, content);
        write_config(
            &config_path,
            r#"
db_path = "db.sqlite"
[[dirs]]
path = "notes"
recursive = true

[search]
fts5_enabled = false
index_body_text = false
"#,
        );

        let report = Indexer::new(OrgizeAdapter::new())
            .rebuild_from_config_path(&config_path)
            .expect("rebuild should succeed");

        assert_eq!(report.indexed_files.len(), 1);
        assert!(report.diagnostics.is_empty());

        let connection = Connection::open(&db_path).expect("db should open");
        let headings = DbReader::list_headings(&connection).expect("headings should load");
        assert_eq!(headings.len(), 3);
        assert_eq!(
            headings
                .iter()
                .map(|heading| heading.title.clone())
                .collect::<Vec<_>>(),
            vec![
                "First title Later title".to_string(),
                "First heading".to_string(),
                "Child heading".to_string(),
            ]
        );
        assert_eq!(headings[1].parent_id, Some(headings[0].id));
        assert_eq!(headings[2].parent_id, Some(headings[1].id));

        let raw_keywords: Vec<(i64, String, Option<String>, Option<i64>)> = query_rows(
            &connection,
            "SELECT headings.level, keywords.keyword, keywords.value, keywords.line_number
             FROM keywords
             INNER JOIN headings ON headings.id = keywords.heading_id
             ORDER BY keywords.line_number, keywords.id",
            |row| Ok((row.get(0)?, row.get(1)?, row.get(2)?, row.get(3)?)),
        );
        assert_eq!(
            raw_keywords,
            vec![
                (
                    0,
                    "TITLE".to_string(),
                    Some("First title".to_string()),
                    Some(1),
                ),
                (
                    0,
                    "STARTUP".to_string(),
                    Some("showall".to_string()),
                    Some(2),
                ),
                (
                    0,
                    "AUTHOR".to_string(),
                    Some("Jane Doe".to_string()),
                    Some(7),
                ),
                (
                    0,
                    "OPTIONS".to_string(),
                    Some("toc:nil num:t".to_string()),
                    Some(8),
                ),
                (
                    0,
                    "TITLE".to_string(),
                    Some("Later title".to_string()),
                    Some(13),
                ),
                (
                    0,
                    "EXPORT_FILE_NAME".to_string(),
                    Some("export-name".to_string()),
                    Some(14),
                ),
            ]
        );

        let generic_properties: Vec<(String, Option<String>, String)> = query_rows(
            &connection,
            "SELECT key, value, source
             FROM properties
             WHERE key IN ('TITLE', 'AUTHOR', 'STARTUP', 'OPTIONS', 'EXPORT_FILE_NAME')",
            |row| Ok((row.get(0)?, row.get(1)?, row.get(2)?)),
        );
        assert!(generic_properties.is_empty());
    }

    #[test]
    fn rebuild_handles_overlapping_org_todo_keyword_lines_without_duplicate_rows() {
        let test_dir = TestDir::new("overlapping-org-todo");
        let notes_dir = test_dir.path().join("notes");
        let db_path = test_dir.path().join("db.sqlite");
        let config_path = test_dir.path().join("config.toml");
        let org_path = notes_dir.join("overlapping.org");
        let content = include_str!(
            "../tests/data/parser/todo-keywords/overlapping-file-local-lines/fixture.org"
        );

        write_file(&org_path, content);
        write_config(
            &config_path,
            r#"
db_path = "db.sqlite"
[[dirs]]
path = "notes"
recursive = true

[todo]
default_open_keywords = ["TODO(t)"]
default_closed_keywords = ["DONE(d)"]

[search]
fts5_enabled = false
index_body_text = false
"#,
        );

        let report = Indexer::new(OrgizeAdapter::new())
            .rebuild_from_config_path(&config_path)
            .expect("rebuild should succeed");

        assert_eq!(report.indexed_files.len(), 1);
        assert!(report.diagnostics.is_empty());

        let connection = Connection::open(&db_path).expect("db should open");
        let headings = DbReader::list_headings(&connection).expect("headings should load");
        assert_eq!(headings.len(), 4);
        assert_eq!(headings[1].title, "First heading");
        assert_eq!(headings[1].title_raw.as_deref(), Some("TODO First heading"));
        assert_eq!(headings[1].todo_keyword.as_deref(), Some("TODO"));
        assert_eq!(headings[1].todo_type.as_deref(), Some("open"));
        assert_eq!(headings[2].title, "Second heading");
        assert_eq!(
            headings[2].title_raw.as_deref(),
            Some("NEXT Second heading")
        );
        assert_eq!(headings[2].todo_keyword.as_deref(), Some("NEXT"));
        assert_eq!(headings[2].todo_type.as_deref(), Some("open"));
        assert_eq!(headings[3].title, "Finished heading");
        assert_eq!(
            headings[3].title_raw.as_deref(),
            Some("DONE Finished heading")
        );
        assert_eq!(headings[3].todo_keyword.as_deref(), Some("DONE"));
        assert_eq!(headings[3].todo_type.as_deref(), Some("closed"));

        let todo_rows: Vec<TodoProvenanceRow> = query_rows(
            &connection,
            "SELECT keyword, state_type, shortcut, sequence_no, source_kind, source_keyword, source_line_number
             FROM todo_keywords
             ORDER BY sequence_no",
            |row| Ok((
                row.get(0)?,
                row.get(1)?,
                row.get(2)?,
                row.get(3)?,
                row.get(4)?,
                row.get(5)?,
                row.get(6)?,
            )),
        );
        assert_eq!(
            todo_rows,
            vec![
                (
                    "TODO".to_string(),
                    "open".to_string(),
                    None,
                    0,
                    "org_keyword".to_string(),
                    Some("TODO".to_string()),
                    Some(2),
                ),
                (
                    "NEXT".to_string(),
                    "open".to_string(),
                    None,
                    1,
                    "org_keyword".to_string(),
                    Some("TODO".to_string()),
                    Some(2),
                ),
                (
                    "WAIT".to_string(),
                    "open".to_string(),
                    None,
                    2,
                    "org_keyword".to_string(),
                    Some("SEQ_TODO".to_string()),
                    Some(3),
                ),
                (
                    "DONE".to_string(),
                    "closed".to_string(),
                    None,
                    3,
                    "org_keyword".to_string(),
                    Some("TODO".to_string()),
                    Some(2),
                ),
                (
                    "CANCELED".to_string(),
                    "closed".to_string(),
                    None,
                    4,
                    "org_keyword".to_string(),
                    Some("SEQ_TODO".to_string()),
                    Some(3),
                ),
            ]
        );
    }

    #[test]
    fn rebuild_uses_combined_document_title_for_root_heading() {
        let test_dir = TestDir::new("combined-document-title");
        let notes_dir = test_dir.path().join("notes");
        let db_path = test_dir.path().join("db.sqlite");
        let config_path = test_dir.path().join("config.toml");
        let org_path = notes_dir.join("multiple-title.org");

        write_file(
            &org_path,
            "#+TITLE: Title can span\n#+TITLE: multiple lines,\n#+AUTHOR: Hubisan\n\n* Unfortunately Everywhere\n\n#+TITLE: even here\n#+TITLE:\n\n* Plain Heading\n",
        );
        write_config(
            &config_path,
            r#"
db_path = "db.sqlite"
[[dirs]]
path = "notes"
recursive = true

[search]
fts5_enabled = false
index_body_text = false
"#,
        );

        Indexer::new(OrgizeAdapter::new())
            .rebuild_from_config_path(&config_path)
            .expect("rebuild should succeed");

        let connection = Connection::open(&db_path).expect("db should open");
        let headings = DbReader::list_headings(&connection).expect("headings should load");

        assert_eq!(headings.len(), 3);
        assert_eq!(
            headings[0].title,
            "Title can span multiple lines, even here"
        );
        assert_eq!(
            headings[0].title_raw.as_deref(),
            Some("Title can span multiple lines, even here")
        );
        assert_eq!(headings[1].title, "Unfortunately Everywhere");
        assert_eq!(
            headings[1].title_raw.as_deref(),
            Some("Unfortunately Everywhere")
        );
        assert_eq!(headings[2].title, "Plain Heading");
        assert_eq!(headings[2].title_raw.as_deref(), Some("Plain Heading"));
    }

    #[test]
    fn rebuild_distinguishes_source_titles_from_fallback_file_titles() {
        let test_dir = TestDir::new("fallback-root-title");
        let notes_dir = test_dir.path().join("notes");
        let db_path = test_dir.path().join("db.sqlite");
        let config_path = test_dir.path().join("config.toml");
        let org_path = notes_dir.join("no-title-set.org");

        write_file(&org_path, "Some preamble text.\n* Heading\n");
        write_config(
            &config_path,
            r#"
db_path = "db.sqlite"
[[dirs]]
path = "notes"
recursive = true

[search]
fts5_enabled = false
index_body_text = false
"#,
        );

        Indexer::new(OrgizeAdapter::new())
            .rebuild_from_config_path(&config_path)
            .expect("rebuild should succeed");

        let connection = Connection::open(&db_path).expect("db should open");
        let headings = DbReader::list_headings(&connection).expect("headings should load");

        assert_eq!(headings[0].title, "no-title-set");
        let json = serde_json::to_value(&headings[0]).expect("row should serialize");
        assert!(json["title_raw"].is_null());
        assert_eq!(headings[1].title_raw.as_deref(), Some("Heading"));
    }

    #[test]
    fn rebuild_persists_heading_shortcuts_and_rich_timestamp_rows() {
        let test_dir = TestDir::new("timestamp-rebuild");
        let notes_dir = test_dir.path().join("notes");
        let db_path = test_dir.path().join("db.sqlite");
        let config_path = test_dir.path().join("config.toml");
        let org_path = notes_dir.join("planning-timestamp.org");

        write_file(
            &org_path,
            include_str!("../tests/data/parser/timestamps/planning-timestamp/fixture.org"),
        );
        write_config(
            &config_path,
            r#"
db_path = "db.sqlite"
[[dirs]]
path = "notes"
recursive = true

[search]
fts5_enabled = false
index_body_text = false
"#,
        );

        Indexer::new(OrgizeAdapter::new())
            .rebuild_from_config_path(&config_path)
            .expect("rebuild should succeed");

        let connection = Connection::open(&db_path).expect("db should open");
        let timestamp_columns: Vec<String> =
            query_rows(&connection, "PRAGMA table_info(timestamps)", |row| {
                row.get(1)
            });
        let heading_columns: Vec<String> =
            query_rows(&connection, "PRAGMA table_info(headings)", |row| row.get(1));
        assert!(
            !timestamp_columns
                .iter()
                .any(|column| column == "has_repeater"),
            "timestamps table should not have has_repeater"
        );
        assert!(timestamp_columns.iter().any(|column| column == "has_time"));
        assert!(heading_columns
            .iter()
            .any(|column| column == "scheduled_has_time"));
        assert!(heading_columns
            .iter()
            .any(|column| column == "deadline_has_time"));
        assert!(heading_columns
            .iter()
            .any(|column| column == "closed_has_time"));

        let headings = DbReader::list_headings(&connection).expect("headings should load");
        let scheduled = headings
            .iter()
            .find(|heading| heading.title == "Simple scheduled")
            .expect("scheduled heading should exist");
        assert_eq!(scheduled.scheduled_raw.as_deref(), Some("<2024-11-20 Wed>"));
        assert_eq!(scheduled.scheduled_ts, Some(1_732_060_800));

        let duplicate = headings
            .iter()
            .find(|heading| heading.title == "Multiple same keyword")
            .expect("duplicate heading should exist");
        assert_eq!(duplicate.scheduled_raw.as_deref(), Some("<2024-11-21 Thu>"));
        assert_eq!(duplicate.scheduled_ts, Some(1_732_147_200));

        let diary = headings
            .iter()
            .find(|heading| heading.title == "Diary expression")
            .expect("diary heading should exist");
        assert!(diary.scheduled_ts.is_none());

        let timestamp_rows: Vec<TimestampRow> = query_rows(
            &connection,
            "SELECT h.title, t.role, t.type, t.range_type, t.start_ts, t.end_ts
                 FROM timestamps t
                 JOIN headings h ON h.id = t.heading_id
                 WHERE h.level > 0
                 ORDER BY h.title, t.byte_start",
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
        );
        assert!(timestamp_rows.iter().any(|row| {
            row.0 == "Time range same day"
                && row.1 == "scheduled"
                && row.2 == "active"
                && row.3 == "time_range"
                && row.4 == Some(1_732_095_000)
                && row.5 == Some(1_732_100_400)
        }));
        assert!(timestamp_rows.iter().any(|row| {
            row.0 == "Date range"
                && row.1 == "deadline"
                && row.3 == "date_range"
                && row.4 == Some(1_733_011_200)
                && row.5 == Some(1_733_184_000)
        }));
        assert!(timestamp_rows.iter().any(|row| {
            row.0 == "Task" && row.1 == "body" && row.2 == "active" && row.4 == Some(1_782_172_800)
        }));

        let repeater_rows: Vec<RepeaterRow> = query_rows(
            &connection,
            "SELECT h.title, tr.repeater_type, tr.repeater_value, tr.repeater_unit,
                    tr.repeater_deadline_value, tr.repeater_deadline_unit,
                    tr.warning_type, tr.warning_value, tr.warning_unit
             FROM timestamp_repeaters tr
             JOIN timestamps t ON t.id = tr.timestamp_id
             JOIN headings h ON h.id = t.heading_id
             ORDER BY h.title, tr.id",
            |row| {
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
            },
        );
        assert_eq!(
            repeater_rows,
            vec![
                (
                    "Repeater".to_string(),
                    Some("cumulate".to_string()),
                    Some(1),
                    Some("week".to_string()),
                    None,
                    None,
                    None,
                    None,
                    None,
                ),
                (
                    "Repeater with deadline and warning".to_string(),
                    Some("catch_up".to_string()),
                    Some(1),
                    Some("month".to_string()),
                    Some(2),
                    Some("day".to_string()),
                    Some("all".to_string()),
                    Some(5),
                    Some("day".to_string()),
                ),
                (
                    "Warning only all".to_string(),
                    None,
                    None,
                    None,
                    None,
                    None,
                    Some("all".to_string()),
                    Some(5),
                    Some("day".to_string()),
                ),
                (
                    "Warning only first".to_string(),
                    None,
                    None,
                    None,
                    None,
                    None,
                    Some("first".to_string()),
                    Some(2),
                    Some("week".to_string()),
                ),
            ]
        );
    }

    #[test]
    fn rebuild_persists_explicit_time_metadata_for_planning_and_generic_timestamps() {
        let test_dir = TestDir::new("timestamp-has-time");
        let notes_dir = test_dir.path().join("notes");
        let db_path = test_dir.path().join("db.sqlite");
        let config_path = test_dir.path().join("config.toml");
        let org_path = notes_dir.join("explicit-time.org");

        write_file(
            &org_path,
            r#"#+TITLE: Explicit Time Fixture

* Scheduled Date Only
SCHEDULED: <2026-06-23 Tue>

* Scheduled Midnight
SCHEDULED: <2026-06-24 Wed 00:00>

* Deadline Timed
DEADLINE: <2026-06-25 Thu 09:30>

* Closed Date Only
CLOSED: [2026-06-26 Fri]

* Generic Active Date Only
<2026-06-27 Sat>

* Generic Active Midnight
<2026-06-28 Sun 00:00>

* Generic Inactive Timed
[2026-06-29 Mon 18:45]
"#,
        );
        write_config(
            &config_path,
            r#"
db_path = "db.sqlite"
[[dirs]]
path = "notes"
recursive = true

[search]
fts5_enabled = false
index_body_text = false
"#,
        );

        Indexer::new(OrgizeAdapter::new())
            .rebuild_from_config_path(&config_path)
            .expect("rebuild should succeed");

        let connection = Connection::open(&db_path).expect("db should open");

        let planning_rows: Vec<PlanningHasTimeRow> = query_rows(
            &connection,
            "SELECT title, scheduled_ts, scheduled_has_time, deadline_has_time, closed_has_time
                 FROM headings
                 WHERE level > 0
                 ORDER BY byte_start",
            |row| {
                Ok((
                    row.get(0)?,
                    row.get(1)?,
                    row.get(2)?,
                    row.get(3)?,
                    row.get(4)?,
                ))
            },
        );
        assert!(planning_rows
            .iter()
            .any(|row| { row.0 == "Scheduled Date Only" && row.1.is_some() && row.2 == Some(0) }));
        assert!(planning_rows
            .iter()
            .any(|row| { row.0 == "Scheduled Midnight" && row.1.is_some() && row.2 == Some(1) }));
        assert!(planning_rows
            .iter()
            .any(|row| row.0 == "Deadline Timed" && row.3 == Some(1)));
        assert!(planning_rows
            .iter()
            .any(|row| row.0 == "Closed Date Only" && row.4 == Some(0)));

        let timestamp_rows: Vec<TimestampHasTimeRow> = query_rows(
            &connection,
            "SELECT h.title, t.role, t.raw_value, t.has_time, t.start_ts
             FROM timestamps t
             JOIN headings h ON h.id = t.heading_id
             WHERE h.level > 0
             ORDER BY h.byte_start, t.byte_start",
            |row| {
                Ok((
                    row.get(0)?,
                    row.get(1)?,
                    row.get(2)?,
                    row.get(3)?,
                    row.get(4)?,
                ))
            },
        );
        assert!(timestamp_rows.iter().any(|row| {
            row.0 == "Scheduled Date Only"
                && row.1 == "scheduled"
                && row.2 == "<2026-06-23 Tue>"
                && row.3 == Some(0)
                && row.4 == Some(1_782_172_800)
        }));
        assert!(timestamp_rows.iter().any(|row| {
            row.0 == "Scheduled Midnight"
                && row.1 == "scheduled"
                && row.2 == "<2026-06-24 Wed 00:00>"
                && row.3 == Some(1)
                && row.4 == Some(1_782_259_200)
        }));
        assert!(timestamp_rows.iter().any(|row| {
            row.0 == "Generic Active Date Only"
                && row.1 == "body"
                && row.2 == "<2026-06-27 Sat>"
                && row.3 == Some(0)
        }));
        assert!(timestamp_rows.iter().any(|row| {
            row.0 == "Generic Active Midnight"
                && row.1 == "body"
                && row.2 == "<2026-06-28 Sun 00:00>"
                && row.3 == Some(1)
        }));
        assert!(timestamp_rows.iter().any(|row| {
            row.0 == "Generic Inactive Timed"
                && row.1 == "body"
                && row.2 == "[2026-06-29 Mon 18:45]"
                && row.3 == Some(1)
        }));
    }

    #[test]
    fn rebuild_deduplicates_equivalent_config_file_paths() {
        let test_dir = TestDir::new("equivalent-config-files");
        let notes_dir = test_dir.path().join("files");
        let db_path = test_dir.path().join("db.sqlite");
        let config_path = test_dir.path().join("config.toml");
        let org_path = notes_dir.join("a.org");

        write_file(&org_path, "#+TITLE: Example\n* Heading\n");
        write_config(
            &config_path,
            r#"
db_path = "db.sqlite"
files = ["././files/a.org", "files/../files/a.org"]

[search]
fts5_enabled = false
index_body_text = false
"#,
        );

        let report = Indexer::new(OrgizeAdapter::new())
            .rebuild_from_config_path(&config_path)
            .expect("rebuild should succeed");

        assert_eq!(report.indexed_files.len(), 1);
        assert_eq!(report.indexed_files[0].path, org_path);

        let connection = Connection::open(&db_path).expect("db should open");
        let files_count: i64 = connection
            .query_row("SELECT COUNT(*) FROM files", [], |row| row.get(0))
            .expect("files count should load");
        let headings = DbReader::list_headings(&connection).expect("headings should load");

        assert_eq!(files_count, 1);
        assert_eq!(headings.len(), 2);
        assert_eq!(headings[0].title, "Example");
        assert_eq!(headings[1].title, "Heading");
    }

    #[test]
    fn rebuild_removes_stale_relative_file_rows_for_configured_scope() {
        let test_dir = TestDir::new("stale-relative-rows");
        let files_dir = test_dir.path().join("files");
        let db_path = test_dir.path().join("db.sqlite");
        let config_path = test_dir.path().join("config.toml");
        let org_path = files_dir.join("a.org");
        let absolute_path = org_path.to_string_lossy().to_string();

        write_file(&org_path, "#+TITLE: Current\n* Fresh\n");
        write_config(
            &config_path,
            r#"
db_path = "db.sqlite"
files = ["././files/a.org", "./files/../files/a.org"]

[search]
fts5_enabled = false
index_body_text = false
"#,
        );

        let mut connection = crate::db::open_database_with_schema(
            &db_path,
            &crate::db::SchemaDefinition::new(1, false),
        )
        .expect("db should open");
        connection
            .execute(
                "INSERT INTO files (id, path, mtime_ns, size) VALUES (1, '././files/a.org', 1, 1)",
                [],
            )
            .expect("first stale file row should insert");
        connection
            .execute(
                "INSERT INTO files (id, path, mtime_ns, size) VALUES (2, 'files/a.org', 1, 1)",
                [],
            )
            .expect("second stale file row should insert");
        connection
            .execute(
                "INSERT INTO headings (id, file_id, parent_id, level, line_number, byte_start, byte_end, title, title_raw, archivedp, footnote_section_p)
                 VALUES (1, 1, NULL, 0, 1, -1, 1, 'Old Root', 'Old Root', 0, 0)",
                [],
            )
            .expect("first stale root should insert");
        connection
            .execute(
                "INSERT INTO headings (id, file_id, parent_id, level, line_number, byte_start, byte_end, title, title_raw, archivedp, footnote_section_p)
                 VALUES (2, 1, 1, 1, 1, 0, 1, 'Old Child', 'Old Child', 0, 0)",
                [],
            )
            .expect("first stale child should insert");
        connection
            .execute(
                "INSERT INTO headings (id, file_id, parent_id, level, line_number, byte_start, byte_end, title, title_raw, archivedp, footnote_section_p)
                 VALUES (3, 2, NULL, 0, 1, -1, 1, 'Older Root', 'Older Root', 0, 0)",
                [],
            )
            .expect("second stale root should insert");

        let report = Indexer::new(OrgizeAdapter::new())
            .rebuild(
                &mut connection,
                &Config::load_from_file(&config_path).expect("config should load"),
            )
            .expect("rebuild should succeed");

        assert_eq!(report.indexed_files.len(), 1);
        assert_eq!(report.indexed_files[0].path, org_path);

        let file_rows: Vec<(i64, String)> = query_rows(
            &connection,
            "SELECT id, path FROM files ORDER BY id",
            |row| Ok((row.get(0)?, row.get(1)?)),
        );
        let heading_rows: Vec<(i64, i64, String)> = query_rows(
            &connection,
            "SELECT id, file_id, title FROM headings ORDER BY id",
            |row| Ok((row.get(0)?, row.get(1)?, row.get(2)?)),
        );

        assert_eq!(file_rows.len(), 1);
        assert_eq!(file_rows[0].1, absolute_path);
        assert_eq!(heading_rows.len(), 2);
        assert!(heading_rows
            .iter()
            .all(|(_, file_id, _)| *file_id == file_rows[0].0));
        assert_eq!(
            heading_rows
                .iter()
                .map(|(_, _, title)| title.clone())
                .collect::<Vec<_>>(),
            vec!["Current".to_string(), "Fresh".to_string()]
        );
    }

    #[test]
    fn discovery_classifies_missing_configured_files_without_canonicalizing_them() {
        let test_dir = TestDir::new("missing-configured-file");
        let config_path = test_dir.path().join("config.toml");
        let missing_file = test_dir.path().join("missing.org");

        write_config(
            &config_path,
            r#"
db_path = "db.sqlite"
files = ["missing.org"]

[search]
fts5_enabled = false
index_body_text = false
"#,
        );

        let config = Config::load_from_file(&config_path).expect("config should load");
        let discovery = discover_org_files(&config).expect("missing explicit file is classified");
        assert!(discovery.files.is_empty());
        assert_eq!(discovery.missing_explicit_files, vec![missing_file]);
    }

    #[test]
    fn discovery_ignores_a_missing_explicit_file_matched_by_a_global_exclusion() {
        let test_dir = TestDir::new("excluded-missing-explicit-file");
        let config_path = test_dir.path().join("config.toml");
        write_config(
            &config_path,
            r#"
files = ["missing.org"]
files_exclude = ["missing.org"]
"#,
        );

        let discovery =
            discover_org_files(&Config::load_from_file(&config_path).expect("config should load"))
                .expect("excluded missing file should not be inspected");
        assert!(discovery.files.is_empty());
        assert!(discovery.missing_explicit_files.is_empty());
        assert!(discovery.had_exclusion_match);
    }

    #[test]
    fn rebuild_allows_an_intentionally_empty_pruned_directory() {
        let test_dir = TestDir::new("intentionally-empty-pruned-directory");
        let config_path = test_dir.path().join("config.toml");
        let db_path = test_dir.path().join("db.sqlite");
        write_file(&test_dir.path().join("notes/archive/old.org"), "* Old\n");
        let indexer = Indexer::new(OrgizeAdapter::new());
        write_config(
            &config_path,
            "db_path = \"db.sqlite\"\n[[dirs]]\npath = \"notes\"\nrecursive = true\n[search]\nfts5_enabled = false\n",
        );
        indexer
            .rebuild_from_config_path(&config_path)
            .expect("initial rebuild should succeed");
        write_config(
            &config_path,
            "db_path = \"db.sqlite\"\n[[dirs]]\npath = \"notes\"\nrecursive = true\nexclude = [\"archive/**\"]\n[search]\nfts5_enabled = false\n",
        );
        indexer
            .rebuild_from_config_path(&config_path)
            .expect("pruned exclusion should be intentional");
        let connection = Connection::open(&db_path).expect("database should open");
        let count: i64 = connection
            .query_row("SELECT COUNT(*) FROM files", [], |row| row.get(0))
            .expect("file count should load");
        assert_eq!(count, 0);
    }

    #[test]
    fn link_resolution_marks_a_globally_excluded_target_outside_the_universe() {
        let test_dir = TestDir::new("excluded-link-target");
        let config_path = test_dir.path().join("config.toml");
        let db_path = test_dir.path().join("db.sqlite");
        write_file(
            &test_dir.path().join("notes/source.org"),
            "* Source\n[[file:target.org]]\n",
        );
        write_file(&test_dir.path().join("notes/target.org"), "* Target\n");
        write_config(
            &config_path,
            "db_path = \"db.sqlite\"\nfiles_exclude = [\"notes/target.org\"]\n[[dirs]]\npath = \"notes\"\nrecursive = true\n[search]\nfts5_enabled = false\n",
        );

        Indexer::new(OrgizeAdapter::new())
            .rebuild_from_config_path(&config_path)
            .expect("rebuild should succeed");
        let connection = Connection::open(&db_path).expect("database should open");
        let result: (String, String) = connection
            .query_row(
                "SELECT resolution_status, resolution_diagnostic FROM links",
                [],
                |row| Ok((row.get(0)?, row.get(1)?)),
            )
            .expect("link resolution should load");
        assert_eq!(
            result,
            (
                "unresolved".to_string(),
                "outside indexed universe".to_string()
            )
        );
    }

    #[test]
    fn removing_an_exclusion_makes_the_file_eligible_again() {
        let test_dir = TestDir::new("removing-exclusion");
        let config_path = test_dir.path().join("config.toml");
        let db_path = test_dir.path().join("db.sqlite");
        write_file(&test_dir.path().join("note.org"), "* Note\n");
        let indexer = Indexer::new(OrgizeAdapter::new());
        write_config(
            &config_path,
            "db_path = \"db.sqlite\"\nfiles = [\"note.org\"]\nfiles_exclude = [\"note.org\"]\n[search]\nfts5_enabled = false\n",
        );
        indexer
            .rebuild_from_config_path(&config_path)
            .expect("excluded rebuild should succeed");
        write_config(
            &config_path,
            "db_path = \"db.sqlite\"\nfiles = [\"note.org\"]\n[search]\nfts5_enabled = false\n",
        );
        indexer
            .rebuild_from_config_path(&config_path)
            .expect("rebuild after removal should succeed");
        let connection = Connection::open(&db_path).expect("database should open");
        let counts: (i64, i64) = connection
            .query_row(
                "SELECT (SELECT COUNT(*) FROM files), (SELECT COUNT(*) FROM headings)",
                [],
                |row| Ok((row.get(0)?, row.get(1)?)),
            )
            .expect("rows should exist after exclusion removal");
        assert_eq!(counts, (1, 2));
    }

    #[test]
    fn discovery_applies_global_and_root_local_exclusions() {
        let test_dir = TestDir::new("discovery-exclusions");
        let config_path = test_dir.path().join("config.toml");
        let explicit = test_dir.path().join("explicit.org");
        let root = test_dir.path().join("notes");
        let kept = root.join("kept.org");
        let local_secret = root.join("secret.org");
        let archived = root.join("archive/old.org");
        let private = root.join("nested/private.private.org");
        write_file(&explicit, "* Explicit\n");
        write_file(&kept, "* Kept\n");
        write_file(&local_secret, "* Secret\n");
        write_file(&archived, "* Archived\n");
        write_file(&private, "* Private\n");
        write_config(
            &config_path,
            r#"
files = ["explicit.org"]
files_exclude = ["explicit.org", "**/*.private.org"]

[[dirs]]
path = "notes"
recursive = true
exclude = ["secret.org", "archive/**"]
"#,
        );

        let discovery =
            discover_org_files(&Config::load_from_file(&config_path).expect("config should load"))
                .expect("discovery should succeed");
        assert_eq!(
            discovery
                .files
                .iter()
                .map(|file| file.path.clone())
                .collect::<Vec<_>>(),
            vec![fs::canonicalize(&kept).expect("kept path should canonicalize")]
        );
    }

    #[test]
    fn absolute_global_and_local_exclusion_patterns_apply_to_their_logical_paths() {
        let test_dir = TestDir::new("absolute-discovery-exclusions");
        let config_path = test_dir.path().join("config.toml");
        let explicit = test_dir.path().join("explicit.org");
        let root = test_dir.path().join("notes");
        let global = root.join("global.org");
        let local = root.join("local.org");
        let archived = root.join("archive/old.org");
        let kept = root.join("kept.org");
        for path in [&explicit, &global, &local, &archived, &kept] {
            write_file(path, "* Note\n");
        }
        write_config(
            &config_path,
            &format!(
                "files = [\"explicit.org\"]\nfiles_exclude = [\"{}\", \"{}\"]\n\n[[dirs]]\npath = \"notes\"\nrecursive = true\nexclude = [\"{}\", \"{}/**\"]\n",
                explicit.display(),
                global.display(),
                local.display(),
                root.join("archive").display(),
            ),
        );

        let discovery =
            discover_org_files(&Config::load_from_file(&config_path).expect("config should load"))
                .expect("discovery should succeed");
        assert_eq!(
            discovery
                .files
                .iter()
                .map(|file| file.path.clone())
                .collect::<Vec<_>>(),
            vec![fs::canonicalize(kept).expect("kept path should canonicalize")]
        );
    }

    #[test]
    fn rebuild_removes_files_excluded_by_a_new_configuration() {
        let test_dir = TestDir::new("rebuild-new-exclusion");
        let config_path = test_dir.path().join("config.toml");
        let db_path = test_dir.path().join("db.sqlite");
        let note = test_dir.path().join("note.org");
        write_file(&note, "* Note\n");
        write_config(
            &config_path,
            "db_path = \"db.sqlite\"\nfiles = [\"note.org\"]\n[search]\nfts5_enabled = false\n",
        );
        let indexer = Indexer::new(OrgizeAdapter::new());
        indexer
            .rebuild_from_config_path(&config_path)
            .expect("initial rebuild should succeed");

        write_config(
            &config_path,
            "db_path = \"db.sqlite\"\nfiles = [\"note.org\"]\nfiles_exclude = [\"note.org\"]\n[search]\nfts5_enabled = false\n",
        );
        indexer
            .rebuild_from_config_path(&config_path)
            .expect("all-excluded rebuild should be intentional");

        let connection = Connection::open(&db_path).expect("database should open");
        let count: i64 = connection
            .query_row("SELECT COUNT(*) FROM files", [], |row| row.get(0))
            .expect("file count should load");
        assert_eq!(count, 0);
    }

    #[cfg(unix)]
    #[test]
    fn exclusions_use_logical_symlink_paths_and_global_matches_win_across_roots() {
        use std::os::unix::fs::symlink;

        let test_dir = TestDir::new("logical-symlink-exclusions");
        let aliases = test_dir.path().join("aliases");
        let external = test_dir.path().join("external");
        let config_path = test_dir.path().join("config.toml");
        let target = external.join("target.org");
        write_file(&target, "* Target\n");
        fs::create_dir_all(&aliases).expect("alias directory should be created");
        symlink(&external, aliases.join("linked")).expect("directory symlink should be created");
        write_config(
            &config_path,
            r#"
files_exclude = ["aliases/linked/target.org"]

[[dirs]]
path = "aliases"
recursive = true

[[dirs]]
path = "external"
recursive = true
"#,
        );

        let discovery =
            discover_org_files(&Config::load_from_file(&config_path).expect("config should load"))
                .expect("discovery should succeed");
        assert!(discovery.files.is_empty());
        assert!(!discovery
            .indexed_universe
            .contains(&fs::canonicalize(&target).expect("target should canonicalize")));
    }

    #[cfg(unix)]
    #[test]
    fn local_exclusions_do_not_leak_to_an_overlapping_root() {
        use std::os::unix::fs::symlink;

        let test_dir = TestDir::new("local-overlap-exclusions");
        let aliases = test_dir.path().join("aliases");
        let external = test_dir.path().join("external");
        let config_path = test_dir.path().join("config.toml");
        let target = external.join("target.org");
        write_file(&target, "* Target\n");
        fs::create_dir_all(&aliases).expect("alias directory should be created");
        symlink(&external, aliases.join("linked")).expect("directory symlink should be created");
        write_config(
            &config_path,
            r#"
[[dirs]]
path = "aliases"
recursive = true
exclude = ["linked/**"]

[[dirs]]
path = "external"
recursive = true
"#,
        );

        let discovery =
            discover_org_files(&Config::load_from_file(&config_path).expect("config should load"))
                .expect("discovery should succeed");
        assert_eq!(
            discovery
                .files
                .iter()
                .map(|file| file.path.clone())
                .collect::<Vec<_>>(),
            vec![fs::canonicalize(&target).expect("target should canonicalize")]
        );
        assert!(discovery
            .indexed_universe
            .contains(&fs::canonicalize(&target).expect("target should canonicalize")));
    }

    #[cfg(unix)]
    #[test]
    fn exclusion_globs_match_non_utf8_unix_file_names_without_lossy_conversion() {
        use std::{ffi::OsStr, os::unix::ffi::OsStrExt};

        let test_dir = TestDir::new("non-utf8-exclusion");
        let notes = test_dir.path().join("notes");
        let config_path = test_dir.path().join("config.toml");
        write_file(
            &notes.join(OsStr::from_bytes(b"private-\xff.org")),
            "* Private\n",
        );
        write_config(
            &config_path,
            r#"
[[dirs]]
path = "notes"
recursive = true
exclude = ["*.org"]
"#,
        );

        let discovery =
            discover_org_files(&Config::load_from_file(&config_path).expect("config should load"))
                .expect("discovery should succeed");
        assert!(discovery.files.is_empty());
    }

    #[cfg(unix)]
    #[test]
    fn discovery_follows_file_and_directory_symlinks_without_cycles() {
        use std::os::unix::fs::symlink;

        let test_dir = TestDir::new("symlink-discovery");
        let root = test_dir.path().join("root");
        let external = test_dir.path().join("external");
        let config_path = test_dir.path().join("config.toml");
        let direct = root.join("direct.org");
        let external_file = external.join("external.org");
        write_file(&direct, "* Direct\n");
        write_file(&external_file, "* External\n");
        symlink(&direct, root.join("alias.org")).expect("file symlink should be created");
        symlink(&external, root.join("external-dir")).expect("directory symlink should be created");
        symlink(&root, external.join("cycle")).expect("cycle symlink should be created");
        write_config(
            &config_path,
            r#"
db_path = "db.sqlite"
[[dirs]]
path = "root"
recursive = true
"#,
        );

        let discovery =
            discover_org_files(&Config::load_from_file(&config_path).expect("config should load"))
                .expect("symlink traversal should finish");
        assert_eq!(discovery.files.len(), 2);
        let mut expected = vec![
            fs::canonicalize(&direct).unwrap(),
            fs::canonicalize(&external_file).unwrap(),
        ];
        expected.sort();
        assert_eq!(
            discovery
                .files
                .iter()
                .map(|file| file.path.clone())
                .collect::<Vec<_>>(),
            expected
        );
        assert_ne!(discovery.files[0].identity, discovery.files[1].identity);
    }

    #[cfg(unix)]
    #[test]
    fn rebuild_persists_non_utf8_paths_without_lossy_identity_conversion() {
        use std::{ffi::OsStr, os::unix::ffi::OsStrExt};

        let test_dir = TestDir::new("non-utf8-file-identity");
        let org_path = test_dir.path().join(OsStr::from_bytes(b"notes-\xff.org"));
        write_file(&org_path, "* Heading\n");
        let config = Config {
            db_path: PathBuf::from("db.sqlite"),
            files: vec![org_path],
            dirs: Vec::new(),
            links: Default::default(),
            todo: Default::default(),
            search: SearchConfig {
                fts5_enabled: false,
                index_body_text: false,
            },
            query: Default::default(),
            discovery: Default::default(),
        };
        let mut connection = open_in_memory_database_with_schema(&SchemaDefinition::new(
            CURRENT_SCHEMA_VERSION,
            false,
        ))
        .expect("database should open");

        Indexer::new(OrgizeAdapter::new())
            .rebuild(&mut connection, &config)
            .expect("non-UTF-8 file should rebuild");

        let (path, identity): (String, Vec<u8>) = connection
            .query_row("SELECT path, identity FROM files", [], |row| {
                Ok((row.get(0)?, row.get(1)?))
            })
            .expect("stored path should load");
        assert!(path.starts_with("path-bytes:"));
        assert!(identity.starts_with(b"orgfdb-path-v1\0unix\0"));
        assert!(identity.ends_with(b"notes-\xff.org"));
    }

    #[test]
    fn rebuild_refuses_zero_input_when_existing_indexed_data_would_be_deleted() {
        let mut connection = open_in_memory_database_with_schema(&SchemaDefinition::new(
            CURRENT_SCHEMA_VERSION,
            false,
        ))
        .expect("database should open");
        seed_indexed_file(&connection);

        let config = Config {
            db_path: PathBuf::from("db.sqlite"),
            files: Vec::new(),
            dirs: Vec::new(),
            links: Default::default(),
            todo: Default::default(),
            search: SearchConfig {
                fts5_enabled: false,
                index_body_text: false,
            },
            query: Default::default(),
            discovery: Default::default(),
        };

        let error = Indexer::new(OrgizeAdapter::new())
            .rebuild_with_options(&mut connection, &config, false)
            .expect_err("rebuild should refuse to wipe existing data");

        match &error {
            IndexerError::RefusedEmptyRebuild {
                existing_indexed_files,
            } => {
                assert_eq!(*existing_indexed_files, 1);
            }
            other => panic!("unexpected error: {other}"),
        }

        let message = error.to_string();
        assert!(message.contains("zero input Org files"));
        assert!(message.contains("avoid deleting 1 indexed file"));
        assert!(message.contains("--allow-empty"));

        let files_count: i64 = connection
            .query_row("SELECT COUNT(*) FROM files", [], |row| row.get(0))
            .expect("file count should load");
        let headings_count: i64 = connection
            .query_row("SELECT COUNT(*) FROM headings", [], |row| row.get(0))
            .expect("heading count should load");

        assert_eq!(files_count, 1);
        assert_eq!(headings_count, 1);
    }

    #[test]
    fn rebuild_allows_zero_input_when_existing_database_is_empty() {
        let mut connection = open_in_memory_database_with_schema(&SchemaDefinition::new(
            CURRENT_SCHEMA_VERSION,
            false,
        ))
        .expect("database should open");

        let config = Config {
            db_path: PathBuf::from("db.sqlite"),
            files: Vec::new(),
            dirs: Vec::new(),
            links: Default::default(),
            todo: Default::default(),
            search: SearchConfig {
                fts5_enabled: false,
                index_body_text: false,
            },
            query: Default::default(),
            discovery: Default::default(),
        };

        let report = Indexer::new(OrgizeAdapter::new())
            .rebuild_with_options(&mut connection, &config, false)
            .expect("rebuild should succeed for an empty database");

        assert!(report.indexed_files.is_empty());
        assert!(report.diagnostics.is_empty());

        let files_count: i64 = connection
            .query_row("SELECT COUNT(*) FROM files", [], |row| row.get(0))
            .expect("file count should load");
        let headings_count: i64 = connection
            .query_row("SELECT COUNT(*) FROM headings", [], |row| row.get(0))
            .expect("heading count should load");

        assert_eq!(files_count, 0);
        assert_eq!(headings_count, 0);
    }

    #[test]
    fn rebuild_from_config_path_allows_zero_input_on_fresh_database() {
        let test_dir = TestDir::new("zero-input-fresh-db");
        let config_path = test_dir.path().join("config.toml");
        let db_path = test_dir.path().join("db.sqlite");

        write_config(
            &config_path,
            r#"
db_path = "./db.sqlite"

[search]
fts5_enabled = false
index_body_text = false
"#,
        );

        let report = Indexer::new(OrgizeAdapter::new())
            .rebuild_from_config_path_with_options(&config_path, false)
            .expect("fresh database rebuild should succeed");

        assert!(report.indexed_files.is_empty());
        assert!(report.diagnostics.is_empty());

        let connection = crate::db::open_database(&db_path).expect("database should open");
        let files_count: i64 = connection
            .query_row("SELECT COUNT(*) FROM files", [], |row| row.get(0))
            .expect("file count should load");
        assert_eq!(files_count, 0);
    }

    #[test]
    fn rebuild_with_allow_empty_clears_existing_indexed_data() {
        let mut connection = open_in_memory_database_with_schema(&SchemaDefinition::new(
            CURRENT_SCHEMA_VERSION,
            false,
        ))
        .expect("database should open");
        seed_indexed_file(&connection);

        let config = Config {
            db_path: PathBuf::from("db.sqlite"),
            files: Vec::new(),
            dirs: Vec::new(),
            links: Default::default(),
            todo: Default::default(),
            search: SearchConfig {
                fts5_enabled: false,
                index_body_text: false,
            },
            query: Default::default(),
            discovery: Default::default(),
        };

        let report = Indexer::new(OrgizeAdapter::new())
            .rebuild_with_options(&mut connection, &config, true)
            .expect("rebuild should allow empty input with override");

        assert!(report.indexed_files.is_empty());
        assert!(report.diagnostics.is_empty());

        let files_count: i64 = connection
            .query_row("SELECT COUNT(*) FROM files", [], |row| row.get(0))
            .expect("file count should load");
        let headings_count: i64 = connection
            .query_row("SELECT COUNT(*) FROM headings", [], |row| row.get(0))
            .expect("heading count should load");

        assert_eq!(files_count, 0);
        assert_eq!(headings_count, 0);
    }

    #[test]
    fn rebuild_reports_missing_configured_directories_at_rebuild_time() {
        let test_dir = TestDir::new("missing-configured-dir");
        let config_path = test_dir.path().join("config.toml");
        let missing_dir = test_dir.path().join("missing-dir");

        write_config(
            &config_path,
            r#"
db_path = "db.sqlite"
[[dirs]]
path = "missing-dir"

[search]
fts5_enabled = false
index_body_text = false
"#,
        );

        let config = Config::load_from_file(&config_path).expect("config should load");
        let mut connection = open_in_memory_database_with_schema(&SchemaDefinition::new(
            CURRENT_SCHEMA_VERSION,
            false,
        ))
        .expect("database should open");

        let error = Indexer::new(OrgizeAdapter::new())
            .rebuild(&mut connection, &config)
            .expect_err("rebuild should fail for a missing configured directory");

        match error {
            IndexerError::Discover { path, .. } => {
                assert_eq!(path, missing_dir);
            }
            other => panic!("unexpected error: {other}"),
        }
    }

    #[test]
    fn rebuild_respects_org_todo_keywords_as_overrides() {
        let test_dir = TestDir::new("org-todo-overrides");
        let notes_dir = test_dir.path().join("notes");
        let db_path = test_dir.path().join("db.sqlite");
        let config_path = test_dir.path().join("config.toml");
        let org_path = notes_dir.join("todo.org");

        write_file(
            &org_path,
            "#+TITLE: TODO Overrides\n#+TODO: PLAN(p) | DONE(d)\n* PLAN me\n* DONE me\n* REVIEW Mist\n",
        );
        write_config(
            &config_path,
            r#"
db_path = "db.sqlite"
[[dirs]]
path = "notes"
recursive = true

[todo]
default_open_keywords = ["REVIEW(r)", "TODO(t)"]
default_closed_keywords = ["DONE(d)"]

[search]
fts5_enabled = false
index_body_text = false
"#,
        );

        Indexer::new(OrgizeAdapter::new())
            .rebuild_from_config_path(&config_path)
            .expect("rebuild should succeed");

        let connection = Connection::open(&db_path).expect("db should open");
        let headings = DbReader::list_headings(&connection).expect("headings should load");

        assert_eq!(headings.len(), 4);
        assert_eq!(headings[0].title, "TODO Overrides");
        assert_eq!(headings[1].title, "me");
        assert_eq!(headings[1].title_raw.as_deref(), Some("PLAN me"));
        assert_eq!(headings[1].todo_keyword.as_deref(), Some("PLAN"));
        assert_eq!(headings[1].todo_type.as_deref(), Some("open"));
        assert_eq!(headings[2].title, "me");
        assert_eq!(headings[2].title_raw.as_deref(), Some("DONE me"));
        assert_eq!(headings[2].todo_keyword.as_deref(), Some("DONE"));
        assert_eq!(headings[2].todo_type.as_deref(), Some("closed"));
        assert_eq!(headings[3].title, "REVIEW Mist");
        assert_eq!(headings[3].title_raw.as_deref(), Some("REVIEW Mist"));
        assert_eq!(headings[3].todo_keyword, None);
        assert_eq!(headings[3].todo_type, None);
    }

    #[test]
    fn rebuild_records_config_default_todo_keyword_provenance() {
        let test_dir = TestDir::new("config-default-todo");
        let notes_dir = test_dir.path().join("notes");
        let db_path = test_dir.path().join("db.sqlite");
        let config_path = test_dir.path().join("config.toml");
        let org_path = notes_dir.join("default.org");

        write_file(
            &org_path,
            "#+TITLE: Defaults\n* TODO Inbox\n* DONE Closed\n",
        );
        write_config(
            &config_path,
            r#"
db_path = "db.sqlite"
[[dirs]]
path = "notes"
recursive = true

[todo]
default_open_keywords = ["TODO(t)"]
default_closed_keywords = ["DONE(d)"]

[search]
fts5_enabled = false
index_body_text = false
"#,
        );

        Indexer::new(OrgizeAdapter::new())
            .rebuild_from_config_path(&config_path)
            .expect("rebuild should succeed");

        let connection = Connection::open(&db_path).expect("db should open");
        let todo_rows: Vec<TodoProvenanceRow> = query_rows(
            &connection,
            "SELECT keyword, state_type, shortcut, sequence_no, source_kind, source_keyword, source_line_number
             FROM todo_keywords
             ORDER BY sequence_no",
            |row| Ok((
                row.get(0)?,
                row.get(1)?,
                row.get(2)?,
                row.get(3)?,
                row.get(4)?,
                row.get(5)?,
                row.get(6)?,
            )),
        );

        assert_eq!(
            todo_rows,
            vec![
                (
                    "TODO".to_string(),
                    "open".to_string(),
                    Some("t".to_string()),
                    0,
                    "config_default".to_string(),
                    None,
                    None,
                ),
                (
                    "DONE".to_string(),
                    "closed".to_string(),
                    Some("d".to_string()),
                    1,
                    "config_default".to_string(),
                    None,
                    None,
                ),
            ]
        );
    }

    #[test]
    fn rebuild_handles_manual_org_todo_fixture() {
        let test_dir = TestDir::new("manual-org-todo");
        let org_path = test_dir.path().join("test.org");
        let db_path = test_dir.path().join("db.sqlite");
        let config_path = test_dir.path().join("config.toml");

        write_file(
            &org_path,
            "#+TITLE:\n#+STARTUP: showall\n#+TODO: TODO(t) NEXT(n) PLAN(p) | DONE(d) CANCEL(c)\n\n* REVIEW *Mist*\n\n* PLAN me\n\n* TODO me                                                              :test:\n\n** again                                                                :me:\n\n* DONE me\n",
        );
        write_config(
            &config_path,
            r#"
db_path = "db.sqlite"
files = ["test.org"]

[search]
fts5_enabled = false
index_body_text = false
"#,
        );

        Indexer::new(OrgizeAdapter::new())
            .rebuild_from_config_path(&config_path)
            .expect("rebuild should succeed");

        let connection = Connection::open(&db_path).expect("db should open");
        let headings = DbReader::list_headings(&connection).expect("headings should load");

        assert_eq!(headings.len(), 6);
        assert_eq!(headings[0].title, "test");
        assert_eq!(headings[1].title, "REVIEW Mist");
        assert_eq!(headings[1].title_raw.as_deref(), Some("REVIEW *Mist*"));
        assert_eq!(headings[1].todo_keyword, None);
        assert_eq!(headings[1].todo_type, None);
        assert_eq!(headings[2].title, "me");
        assert_eq!(headings[2].title_raw.as_deref(), Some("PLAN me"));
        assert_eq!(headings[2].todo_keyword.as_deref(), Some("PLAN"));
        assert_eq!(headings[2].todo_type.as_deref(), Some("open"));
        assert_eq!(headings[3].title, "me");
        assert_eq!(headings[3].title_raw.as_deref(), Some("TODO me"));
        assert_eq!(headings[3].todo_keyword.as_deref(), Some("TODO"));
        assert_eq!(headings[3].todo_type.as_deref(), Some("open"));
        assert_eq!(headings[4].title, "again");
        assert_eq!(headings[5].title, "me");
        assert_eq!(headings[5].title_raw.as_deref(), Some("DONE me"));
        assert_eq!(headings[5].todo_keyword.as_deref(), Some("DONE"));
        assert_eq!(headings[5].todo_type.as_deref(), Some("closed"));

        let todo_rows: Vec<TodoProvenanceRow> = query_rows(
            &connection,
            "SELECT keyword, state_type, shortcut, sequence_no, source_kind, source_keyword, source_line_number
             FROM todo_keywords
             ORDER BY sequence_no",
            |row| {
                Ok((
                    row.get(0)?,
                    row.get(1)?,
                    row.get(2)?,
                    row.get(3)?,
                    row.get(4)?,
                    row.get(5)?,
                    row.get(6)?,
                ))
            },
        );
        assert_eq!(
            todo_rows,
            vec![
                (
                    "TODO".to_string(),
                    "open".to_string(),
                    Some("t".to_string()),
                    0,
                    "org_keyword".to_string(),
                    Some("TODO".to_string()),
                    Some(3),
                ),
                (
                    "NEXT".to_string(),
                    "open".to_string(),
                    Some("n".to_string()),
                    1,
                    "org_keyword".to_string(),
                    Some("TODO".to_string()),
                    Some(3),
                ),
                (
                    "PLAN".to_string(),
                    "open".to_string(),
                    Some("p".to_string()),
                    2,
                    "org_keyword".to_string(),
                    Some("TODO".to_string()),
                    Some(3),
                ),
                (
                    "DONE".to_string(),
                    "closed".to_string(),
                    Some("d".to_string()),
                    3,
                    "org_keyword".to_string(),
                    Some("TODO".to_string()),
                    Some(3),
                ),
                (
                    "CANCEL".to_string(),
                    "closed".to_string(),
                    Some("c".to_string()),
                    4,
                    "org_keyword".to_string(),
                    Some("TODO".to_string()),
                    Some(3),
                ),
            ]
        );
    }

    #[test]
    fn rebuild_supports_simplified_org_todo_keyword_lines() {
        let test_dir = TestDir::new("simplified-org-todo");
        let org_path = test_dir.path().join("test.org");
        let db_path = test_dir.path().join("db.sqlite");
        let config_path = test_dir.path().join("config.toml");

        write_file(
            &org_path,
            include_str!(
                "../tests/data/parser/todo-keywords/simplified-file-local-lines/fixture.org"
            ),
        );
        write_config(
            &config_path,
            r#"
db_path = "db.sqlite"
files = ["test.org"]

[todo]
default_open_keywords = ["TODO(t)"]
default_closed_keywords = ["DONE(d)"]

[search]
fts5_enabled = false
index_body_text = false
"#,
        );

        Indexer::new(OrgizeAdapter::new())
            .rebuild_from_config_path(&config_path)
            .expect("rebuild should succeed");

        let connection = Connection::open(&db_path).expect("db should open");
        let headings = DbReader::list_headings(&connection).expect("headings should load");

        assert_eq!(headings.len(), 14);
        assert_eq!(
            headings[1].title,
            "TODO invalid keyword, even though it is a default it is overwritten"
        );
        assert_eq!(headings[1].todo_keyword, None);
        assert_eq!(headings[2].todo_keyword, None);

        let expected_rows = vec![
            ("one", "open", Some("t"), 0),
            ("two", "open", Some("n"), 1),
            ("FIVE", "open", None, 2),
            ("SIX", "open", None, 3),
            ("seven", "open", None, 4),
            ("nine", "open", None, 5),
            ("three", "closed", Some("d"), 6),
            ("four", "closed", Some("w"), 7),
            ("eight", "closed", None, 8),
            ("ten", "closed", None, 9),
            ("eleven", "closed", Some("c"), 10),
        ];

        let todo_rows: Vec<(String, String, Option<String>, i64)> = query_rows(
            &connection,
            "SELECT keyword, state_type, shortcut, sequence_no FROM todo_keywords ORDER BY sequence_no",
            |row| Ok((row.get(0)?, row.get(1)?, row.get(2)?, row.get(3)?)),
        );
        assert_eq!(
            todo_rows,
            expected_rows
                .into_iter()
                .map(|(keyword, state_type, shortcut, sequence_no)| {
                    (
                        keyword.to_string(),
                        state_type.to_string(),
                        shortcut.map(str::to_string),
                        sequence_no,
                    )
                })
                .collect::<Vec<_>>()
        );
    }

    #[test]
    fn rebuild_stores_phase3_links_as_source_facts() {
        let test_dir = TestDir::new("bracket-links");
        let notes_dir = test_dir.path().join("notes");
        let db_path = test_dir.path().join("db.sqlite");
        let config_path = test_dir.path().join("config.toml");
        let org_path = notes_dir.join("links.org");

        write_file(
            &org_path,
            "\
#+TITLE: Bracket Links
[[FILE:notes.org::42]]
[[unknown:foo]]
[[target][description]]
[[./local.org::10]]
[[../parent.org]]
[[~/home.org]]
[[/tmp/system.org]]
[[#custom-id]]
[[*Heading]]
[[dedicated target]]
[[notes.org]]
<https://example.com/some path with spaces>
<file:~/code/main.c::255>
<file:~/xx.org::*My Target>
<file:~/xx.org::#my-custom-id>
<file:~/xx.org::/regexp/>
<file+sys:~/sys/path::7>
<file+emacs:~/emacs/path::*Target>
<unknown:foo>
<jira:ABC-123>
file:~/plain.c::255
attachment:projects.org::10
* Heading
<shell:ls *.org>
[[shell:ls]]
https://example.org
<https://example.org>
",
        );
        write_config(
            &config_path,
            r#"
db_path = "db.sqlite"
files = ["notes/links.org"]

[search]
fts5_enabled = false
index_body_text = false
"#,
        );

        let report = Indexer::new(OrgizeAdapter::new())
            .rebuild_from_config_path(&config_path)
            .expect("rebuild should succeed");

        assert_eq!(report.indexed_files.len(), 1);
        assert_eq!(report.indexed_files[0].path, org_path);

        let connection = Connection::open(&db_path).expect("db should open");
        let rows: Vec<StoredLinkRow> = query_rows(
            &connection,
            "SELECT h.title, l.format, l.raw, l.raw_target, l.raw_description, l.link_type,
                    l.path, l.search_option, l.source_context
             FROM links l
             JOIN headings h ON h.id = l.heading_id
             ORDER BY l.byte_start",
            |row| {
                Ok(StoredLinkRow {
                    heading_title: row.get(0)?,
                    format: row.get(1)?,
                    raw: row.get(2)?,
                    raw_target: row.get(3)?,
                    raw_description: row.get(4)?,
                    link_type: row.get(5)?,
                    path: row.get(6)?,
                    search_option: row.get(7)?,
                    source_context: row.get(8)?,
                })
            },
        );

        assert_eq!(
            rows,
            vec![
                StoredLinkRow {
                    heading_title: "Bracket Links".to_string(),
                    format: "bracket".to_string(),
                    raw: "[[FILE:notes.org::42]]".to_string(),
                    raw_target: "FILE:notes.org::42".to_string(),
                    raw_description: None,
                    link_type: "file".to_string(),
                    path: "notes.org".to_string(),
                    search_option: Some("42".to_string()),
                    source_context: "normal".to_string(),
                },
                StoredLinkRow {
                    heading_title: "Bracket Links".to_string(),
                    format: "bracket".to_string(),
                    raw: "[[unknown:foo]]".to_string(),
                    raw_target: "unknown:foo".to_string(),
                    raw_description: None,
                    link_type: "unknown".to_string(),
                    path: "foo".to_string(),
                    search_option: None,
                    source_context: "normal".to_string(),
                },
                StoredLinkRow {
                    heading_title: "Bracket Links".to_string(),
                    format: "bracket".to_string(),
                    raw: "[[target][description]]".to_string(),
                    raw_target: "target".to_string(),
                    raw_description: Some("description".to_string()),
                    link_type: "fuzzy".to_string(),
                    path: "target".to_string(),
                    search_option: None,
                    source_context: "normal".to_string(),
                },
                StoredLinkRow {
                    heading_title: "Bracket Links".to_string(),
                    format: "bracket".to_string(),
                    raw: "[[./local.org::10]]".to_string(),
                    raw_target: "./local.org::10".to_string(),
                    raw_description: None,
                    link_type: "file".to_string(),
                    path: "./local.org".to_string(),
                    search_option: Some("10".to_string()),
                    source_context: "normal".to_string(),
                },
                StoredLinkRow {
                    heading_title: "Bracket Links".to_string(),
                    format: "bracket".to_string(),
                    raw: "[[../parent.org]]".to_string(),
                    raw_target: "../parent.org".to_string(),
                    raw_description: None,
                    link_type: "file".to_string(),
                    path: "../parent.org".to_string(),
                    search_option: None,
                    source_context: "normal".to_string(),
                },
                StoredLinkRow {
                    heading_title: "Bracket Links".to_string(),
                    format: "bracket".to_string(),
                    raw: "[[~/home.org]]".to_string(),
                    raw_target: "~/home.org".to_string(),
                    raw_description: None,
                    link_type: "file".to_string(),
                    path: "~/home.org".to_string(),
                    search_option: None,
                    source_context: "normal".to_string(),
                },
                StoredLinkRow {
                    heading_title: "Bracket Links".to_string(),
                    format: "bracket".to_string(),
                    raw: "[[/tmp/system.org]]".to_string(),
                    raw_target: "/tmp/system.org".to_string(),
                    raw_description: None,
                    link_type: "file".to_string(),
                    path: "/tmp/system.org".to_string(),
                    search_option: None,
                    source_context: "normal".to_string(),
                },
                StoredLinkRow {
                    heading_title: "Bracket Links".to_string(),
                    format: "bracket".to_string(),
                    raw: "[[#custom-id]]".to_string(),
                    raw_target: "#custom-id".to_string(),
                    raw_description: None,
                    link_type: "custom-id".to_string(),
                    path: "custom-id".to_string(),
                    search_option: None,
                    source_context: "normal".to_string(),
                },
                StoredLinkRow {
                    heading_title: "Bracket Links".to_string(),
                    format: "bracket".to_string(),
                    raw: "[[*Heading]]".to_string(),
                    raw_target: "*Heading".to_string(),
                    raw_description: None,
                    link_type: "fuzzy".to_string(),
                    path: "*Heading".to_string(),
                    search_option: None,
                    source_context: "normal".to_string(),
                },
                StoredLinkRow {
                    heading_title: "Bracket Links".to_string(),
                    format: "bracket".to_string(),
                    raw: "[[dedicated target]]".to_string(),
                    raw_target: "dedicated target".to_string(),
                    raw_description: None,
                    link_type: "fuzzy".to_string(),
                    path: "dedicated target".to_string(),
                    search_option: None,
                    source_context: "normal".to_string(),
                },
                StoredLinkRow {
                    heading_title: "Bracket Links".to_string(),
                    format: "bracket".to_string(),
                    raw: "[[notes.org]]".to_string(),
                    raw_target: "notes.org".to_string(),
                    raw_description: None,
                    link_type: "fuzzy".to_string(),
                    path: "notes.org".to_string(),
                    search_option: None,
                    source_context: "normal".to_string(),
                },
                StoredLinkRow {
                    heading_title: "Bracket Links".to_string(),
                    format: "angle".to_string(),
                    raw: "<https://example.com/some path with spaces>".to_string(),
                    raw_target: "https://example.com/some path with spaces".to_string(),
                    raw_description: None,
                    link_type: "https".to_string(),
                    path: "//example.com/some path with spaces".to_string(),
                    search_option: None,
                    source_context: "normal".to_string(),
                },
                StoredLinkRow {
                    heading_title: "Bracket Links".to_string(),
                    format: "angle".to_string(),
                    raw: "<file:~/code/main.c::255>".to_string(),
                    raw_target: "file:~/code/main.c::255".to_string(),
                    raw_description: None,
                    link_type: "file".to_string(),
                    path: "~/code/main.c".to_string(),
                    search_option: Some("255".to_string()),
                    source_context: "normal".to_string(),
                },
                StoredLinkRow {
                    heading_title: "Bracket Links".to_string(),
                    format: "angle".to_string(),
                    raw: "<file:~/xx.org::*My Target>".to_string(),
                    raw_target: "file:~/xx.org::*My Target".to_string(),
                    raw_description: None,
                    link_type: "file".to_string(),
                    path: "~/xx.org".to_string(),
                    search_option: Some("*My Target".to_string()),
                    source_context: "normal".to_string(),
                },
                StoredLinkRow {
                    heading_title: "Bracket Links".to_string(),
                    format: "angle".to_string(),
                    raw: "<file:~/xx.org::#my-custom-id>".to_string(),
                    raw_target: "file:~/xx.org::#my-custom-id".to_string(),
                    raw_description: None,
                    link_type: "file".to_string(),
                    path: "~/xx.org".to_string(),
                    search_option: Some("#my-custom-id".to_string()),
                    source_context: "normal".to_string(),
                },
                StoredLinkRow {
                    heading_title: "Bracket Links".to_string(),
                    format: "angle".to_string(),
                    raw: "<file:~/xx.org::/regexp/>".to_string(),
                    raw_target: "file:~/xx.org::/regexp/".to_string(),
                    raw_description: None,
                    link_type: "file".to_string(),
                    path: "~/xx.org".to_string(),
                    search_option: Some("/regexp/".to_string()),
                    source_context: "normal".to_string(),
                },
                StoredLinkRow {
                    heading_title: "Bracket Links".to_string(),
                    format: "angle".to_string(),
                    raw: "<file+sys:~/sys/path::7>".to_string(),
                    raw_target: "file+sys:~/sys/path::7".to_string(),
                    raw_description: None,
                    link_type: "file+sys".to_string(),
                    path: "~/sys/path".to_string(),
                    search_option: Some("7".to_string()),
                    source_context: "normal".to_string(),
                },
                StoredLinkRow {
                    heading_title: "Bracket Links".to_string(),
                    format: "angle".to_string(),
                    raw: "<file+emacs:~/emacs/path::*Target>".to_string(),
                    raw_target: "file+emacs:~/emacs/path::*Target".to_string(),
                    raw_description: None,
                    link_type: "file+emacs".to_string(),
                    path: "~/emacs/path".to_string(),
                    search_option: Some("*Target".to_string()),
                    source_context: "normal".to_string(),
                },
                StoredLinkRow {
                    heading_title: "Bracket Links".to_string(),
                    format: "angle".to_string(),
                    raw: "<unknown:foo>".to_string(),
                    raw_target: "unknown:foo".to_string(),
                    raw_description: None,
                    link_type: "unknown".to_string(),
                    path: "foo".to_string(),
                    search_option: None,
                    source_context: "normal".to_string(),
                },
                StoredLinkRow {
                    heading_title: "Bracket Links".to_string(),
                    format: "angle".to_string(),
                    raw: "<jira:ABC-123>".to_string(),
                    raw_target: "jira:ABC-123".to_string(),
                    raw_description: None,
                    link_type: "jira".to_string(),
                    path: "ABC-123".to_string(),
                    search_option: None,
                    source_context: "normal".to_string(),
                },
                StoredLinkRow {
                    heading_title: "Bracket Links".to_string(),
                    format: "plain".to_string(),
                    raw: "file:~/plain.c::255".to_string(),
                    raw_target: "file:~/plain.c::255".to_string(),
                    raw_description: None,
                    link_type: "file".to_string(),
                    path: "~/plain.c".to_string(),
                    search_option: Some("255".to_string()),
                    source_context: "normal".to_string(),
                },
                StoredLinkRow {
                    heading_title: "Bracket Links".to_string(),
                    format: "plain".to_string(),
                    raw: "attachment:projects.org::10".to_string(),
                    raw_target: "attachment:projects.org::10".to_string(),
                    raw_description: None,
                    link_type: "attachment".to_string(),
                    path: "projects.org::10".to_string(),
                    search_option: None,
                    source_context: "normal".to_string(),
                },
                StoredLinkRow {
                    heading_title: "Heading".to_string(),
                    format: "angle".to_string(),
                    raw: "<shell:ls *.org>".to_string(),
                    raw_target: "shell:ls *.org".to_string(),
                    raw_description: None,
                    link_type: "shell".to_string(),
                    path: "ls *.org".to_string(),
                    search_option: None,
                    source_context: "normal".to_string(),
                },
                StoredLinkRow {
                    heading_title: "Heading".to_string(),
                    format: "bracket".to_string(),
                    raw: "[[shell:ls]]".to_string(),
                    raw_target: "shell:ls".to_string(),
                    raw_description: None,
                    link_type: "shell".to_string(),
                    path: "ls".to_string(),
                    search_option: None,
                    source_context: "normal".to_string(),
                },
                StoredLinkRow {
                    heading_title: "Heading".to_string(),
                    format: "plain".to_string(),
                    raw: "https://example.org".to_string(),
                    raw_target: "https://example.org".to_string(),
                    raw_description: None,
                    link_type: "https".to_string(),
                    path: "//example.org".to_string(),
                    search_option: None,
                    source_context: "normal".to_string(),
                },
                StoredLinkRow {
                    heading_title: "Heading".to_string(),
                    format: "angle".to_string(),
                    raw: "<https://example.org>".to_string(),
                    raw_target: "https://example.org".to_string(),
                    raw_description: None,
                    link_type: "https".to_string(),
                    path: "//example.org".to_string(),
                    search_option: None,
                    source_context: "normal".to_string(),
                },
            ]
        );

        let resolution_rows: Vec<(String, Option<String>, Option<String>)> = query_rows(
            &connection,
            "SELECT raw, resolution_status, resolution_diagnostic
             FROM links
             ORDER BY byte_start",
            |row| Ok((row.get(0)?, row.get(1)?, row.get(2)?)),
        );
        assert_eq!(
            resolution_rows,
            vec![
                (
                    "[[FILE:notes.org::42]]".to_string(),
                    Some("unresolved".to_string()),
                    Some(FILE_OUTSIDE_UNIVERSE_DIAGNOSTIC.to_string()),
                ),
                (
                    "[[unknown:foo]]".to_string(),
                    Some("unsupported".to_string()),
                    Some(UNSUPPORTED_DIAGNOSTIC.to_string()),
                ),
                (
                    "[[target][description]]".to_string(),
                    Some("unsupported".to_string()),
                    Some(UNSUPPORTED_DIAGNOSTIC.to_string()),
                ),
                (
                    "[[./local.org::10]]".to_string(),
                    Some("unresolved".to_string()),
                    Some(FILE_OUTSIDE_UNIVERSE_DIAGNOSTIC.to_string()),
                ),
                (
                    "[[../parent.org]]".to_string(),
                    Some("unresolved".to_string()),
                    Some(FILE_OUTSIDE_UNIVERSE_DIAGNOSTIC.to_string()),
                ),
                (
                    "[[~/home.org]]".to_string(),
                    Some("unresolved".to_string()),
                    Some(FILE_OUTSIDE_UNIVERSE_DIAGNOSTIC.to_string()),
                ),
                (
                    "[[/tmp/system.org]]".to_string(),
                    Some("unresolved".to_string()),
                    Some(FILE_OUTSIDE_UNIVERSE_DIAGNOSTIC.to_string()),
                ),
                (
                    "[[#custom-id]]".to_string(),
                    Some("broken".to_string()),
                    Some(CUSTOM_ID_MISSING_DIAGNOSTIC.to_string()),
                ),
                (
                    "[[*Heading]]".to_string(),
                    Some("resolved".to_string()),
                    None,
                ),
                (
                    "[[dedicated target]]".to_string(),
                    Some("unsupported".to_string()),
                    Some(UNSUPPORTED_DIAGNOSTIC.to_string()),
                ),
                (
                    "[[notes.org]]".to_string(),
                    Some("unsupported".to_string()),
                    Some(UNSUPPORTED_DIAGNOSTIC.to_string()),
                ),
                (
                    "<https://example.com/some path with spaces>".to_string(),
                    Some("unsupported".to_string()),
                    Some(UNSUPPORTED_DIAGNOSTIC.to_string()),
                ),
                (
                    "<file:~/code/main.c::255>".to_string(),
                    Some("unresolved".to_string()),
                    Some(FILE_OUTSIDE_UNIVERSE_DIAGNOSTIC.to_string()),
                ),
                (
                    "<file:~/xx.org::*My Target>".to_string(),
                    Some("unresolved".to_string()),
                    Some(FILE_OUTSIDE_UNIVERSE_DIAGNOSTIC.to_string()),
                ),
                (
                    "<file:~/xx.org::#my-custom-id>".to_string(),
                    Some("unresolved".to_string()),
                    Some(FILE_OUTSIDE_UNIVERSE_DIAGNOSTIC.to_string()),
                ),
                (
                    "<file:~/xx.org::/regexp/>".to_string(),
                    Some("unresolved".to_string()),
                    Some(FILE_OUTSIDE_UNIVERSE_DIAGNOSTIC.to_string()),
                ),
                (
                    "<file+sys:~/sys/path::7>".to_string(),
                    Some("unsupported".to_string()),
                    Some(UNSUPPORTED_DIAGNOSTIC.to_string()),
                ),
                (
                    "<file+emacs:~/emacs/path::*Target>".to_string(),
                    Some("unsupported".to_string()),
                    Some(UNSUPPORTED_DIAGNOSTIC.to_string()),
                ),
                (
                    "<unknown:foo>".to_string(),
                    Some("unsupported".to_string()),
                    Some(UNSUPPORTED_DIAGNOSTIC.to_string()),
                ),
                (
                    "<jira:ABC-123>".to_string(),
                    Some("unsupported".to_string()),
                    Some(UNSUPPORTED_DIAGNOSTIC.to_string()),
                ),
                (
                    "file:~/plain.c::255".to_string(),
                    Some("unresolved".to_string()),
                    Some(FILE_OUTSIDE_UNIVERSE_DIAGNOSTIC.to_string()),
                ),
                (
                    "attachment:projects.org::10".to_string(),
                    Some("unsupported".to_string()),
                    Some(UNSUPPORTED_DIAGNOSTIC.to_string()),
                ),
                (
                    "<shell:ls *.org>".to_string(),
                    Some("unsupported".to_string()),
                    Some(UNSUPPORTED_DIAGNOSTIC.to_string()),
                ),
                (
                    "[[shell:ls]]".to_string(),
                    Some("unsupported".to_string()),
                    Some(UNSUPPORTED_DIAGNOSTIC.to_string()),
                ),
                (
                    "https://example.org".to_string(),
                    Some("unsupported".to_string()),
                    Some(UNSUPPORTED_DIAGNOSTIC.to_string()),
                ),
                (
                    "<https://example.org>".to_string(),
                    Some("unsupported".to_string()),
                    Some(UNSUPPORTED_DIAGNOSTIC.to_string()),
                ),
            ]
        );
    }

    #[test]
    fn rebuild_runs_link_resolver_after_all_files_are_indexed() {
        let test_dir = TestDir::new("link-resolution-order");
        let notes_dir = test_dir.path().join("notes");
        let db_path = test_dir.path().join("db.sqlite");
        let config_path = test_dir.path().join("config.toml");
        let source_path = notes_dir.join("a-source.org");
        let target_path = notes_dir.join("z-target.org");

        write_file(&source_path, "#+TITLE: Source\n[[file:z-target.org]]\n");
        write_file(&target_path, "#+TITLE: Target\n* Later file\n");
        write_config(
            &config_path,
            r#"
db_path = "db.sqlite"
dirs = [{ path = "notes", recursive = false }]

[search]
fts5_enabled = false
index_body_text = false
"#,
        );

        let report = Indexer::new(OrgizeAdapter::new())
            .rebuild_from_config_path(&config_path)
            .expect("rebuild should succeed");

        assert_eq!(
            report
                .indexed_files
                .iter()
                .map(|file| file.path.clone())
                .collect::<Vec<_>>(),
            vec![source_path.clone(), target_path.clone()]
        );

        let connection = Connection::open(&db_path).expect("db should open");
        let file_rows: Vec<String> =
            query_rows(&connection, "SELECT path FROM files ORDER BY path", |row| {
                row.get(0)
            });
        let link_rows: Vec<(String, String, Option<String>, Option<String>)> = query_rows(
            &connection,
            "SELECT files.path, links.raw, links.resolution_status, links.resolution_diagnostic
             FROM links
             INNER JOIN files ON files.id = links.file_id
             ORDER BY files.path, links.byte_start, links.id",
            |row| Ok((row.get(0)?, row.get(1)?, row.get(2)?, row.get(3)?)),
        );

        assert_eq!(
            file_rows,
            vec![
                source_path.to_string_lossy().to_string(),
                target_path.to_string_lossy().to_string(),
            ]
        );
        assert_eq!(
            link_rows,
            vec![(
                source_path.to_string_lossy().to_string(),
                "[[file:z-target.org]]".to_string(),
                Some("resolved".to_string()),
                None,
            )]
        );
    }

    #[test]
    fn rebuild_resolves_file_links_to_known_indexed_files_and_marks_missing_and_external_targets() {
        let test_dir = TestDir::new("file-link-resolution");
        let notes_dir = test_dir.path().join("notes");
        let db_path = test_dir.path().join("db.sqlite");
        let config_path = test_dir.path().join("config.toml");
        let source_path = notes_dir.join("source.org");
        let local_path = notes_dir.join("local.org");
        let parent_path = test_dir.path().join("parent.org");
        let absolute_path = test_dir.path().join("absolute.org");
        let external_path = test_dir.path().join("outside").join("external.org");

        write_file(
            &source_path,
            &format!(
                "#+TITLE: Source\n[[./local.org]]\n[[../parent.org]]\n[[{}]]\n[[./missing.org]]\n[[{}]]\n[[unknown:foo]]\n",
                absolute_path.to_string_lossy(),
                external_path.to_string_lossy(),
            ),
        );
        write_file(&local_path, "* Local\n");
        write_file(&parent_path, "* Parent\n");
        write_file(&absolute_path, "* Absolute\n");
        write_config(
            &config_path,
            &format!(
                "db_path = \"db.sqlite\"\nfiles = [\"{}\", \"{}\"]\n\n[[dirs]]\npath = \"notes\"\nrecursive = true\n\n[search]\nfts5_enabled = false\nindex_body_text = false\n",
                parent_path.file_name().expect("parent file name").to_string_lossy(),
                absolute_path.file_name().expect("absolute file name").to_string_lossy(),
            ),
        );

        let report = Indexer::new(OrgizeAdapter::new())
            .rebuild_from_config_path(&config_path)
            .expect("rebuild should succeed");
        assert_eq!(report.indexed_files.len(), 4);

        let connection = Connection::open(&db_path).expect("db should open");
        let rows: Vec<LinkResolutionRow> = query_rows(
            &connection,
            "SELECT links.raw, links.resolution_status, files.path, links.resolution_diagnostic
             FROM links
             LEFT JOIN files ON files.id = links.target_file_id
             ORDER BY byte_start",
            |row| Ok((row.get(0)?, row.get(1)?, row.get(2)?, row.get(3)?)),
        );
        let path_rows: Vec<(String, Option<String>)> = query_rows(
            &connection,
            "SELECT raw, path_absolute
             FROM links
             ORDER BY byte_start",
            |row| Ok((row.get(0)?, row.get(1)?)),
        );

        assert_eq!(
            rows,
            vec![
                (
                    "[[./local.org]]".to_string(),
                    Some("resolved".to_string()),
                    Some(local_path.to_string_lossy().to_string()),
                    None,
                ),
                (
                    "[[../parent.org]]".to_string(),
                    Some("resolved".to_string()),
                    Some(parent_path.to_string_lossy().to_string()),
                    None,
                ),
                (
                    format!("[[{}]]", absolute_path.to_string_lossy()),
                    Some("resolved".to_string()),
                    Some(absolute_path.to_string_lossy().to_string()),
                    None,
                ),
                (
                    "[[./missing.org]]".to_string(),
                    Some("broken".to_string()),
                    None,
                    Some(FILE_MISSING_DIAGNOSTIC.to_string()),
                ),
                (
                    format!("[[{}]]", external_path.to_string_lossy()),
                    Some("unresolved".to_string()),
                    None,
                    Some(FILE_OUTSIDE_UNIVERSE_DIAGNOSTIC.to_string()),
                ),
                (
                    "[[unknown:foo]]".to_string(),
                    Some("unsupported".to_string()),
                    None,
                    Some(UNSUPPORTED_DIAGNOSTIC.to_string()),
                ),
            ]
        );
        assert_eq!(
            path_rows,
            vec![
                (
                    "[[./local.org]]".to_string(),
                    Some(local_path.to_string_lossy().to_string()),
                ),
                (
                    "[[../parent.org]]".to_string(),
                    Some(parent_path.to_string_lossy().to_string()),
                ),
                (
                    format!("[[{}]]", absolute_path.to_string_lossy()),
                    Some(absolute_path.to_string_lossy().to_string()),
                ),
                (
                    "[[./missing.org]]".to_string(),
                    Some(notes_dir.join("missing.org").to_string_lossy().to_string()),
                ),
                (
                    format!("[[{}]]", external_path.to_string_lossy()),
                    Some(external_path.to_string_lossy().to_string()),
                ),
                ("[[unknown:foo]]".to_string(), None),
            ]
        );
    }

    #[test]
    fn rebuild_resolves_file_heading_title_search_options() {
        let test_dir = TestDir::new("file-heading-title-resolution");
        let notes_dir = test_dir.path().join("notes");
        let db_path = test_dir.path().join("db.sqlite");
        let config_path = test_dir.path().join("config.toml");
        let source_path = notes_dir.join("source.org");
        let target_path = notes_dir.join("target.org");

        write_file(
            &source_path,
            "\
#+TITLE: Source
[[file:target.org::*Heading]]
[[file:target.org::*   Main Index   ]]
[[file:target.org::*ärger]]
[[file:target.org::*\\[2026-07-01 Wed\\] Implement deterministic link resolution pass after rebuild]]
[[file:target.org::*Missing]]
[[file:target.org::*Duplicate]]
[[file:target.org::#custom-id]]
",
        );
        write_file(
            &target_path,
            "\
#+TITLE: Target
* TODO [#A] Heading :tag:
* Main index
* Ärger
* [2026-07-01 Wed] Implement deterministic link resolution pass after rebuild
* Duplicate
* Duplicate
",
        );
        write_config(
            &config_path,
            r#"
db_path = "db.sqlite"

[[dirs]]
path = "notes"
recursive = true

[search]
fts5_enabled = false
index_body_text = false
"#,
        );

        Indexer::new(OrgizeAdapter::new())
            .rebuild_from_config_path(&config_path)
            .expect("rebuild should succeed");

        let connection = Connection::open(&db_path).expect("db should open");
        let rows: Vec<FileHeadingSearchResolutionRow> = query_rows(
            &connection,
            "SELECT
                 links.raw,
                 target_files.path,
                 target_headings.title,
                 links.search_option,
                 links.resolution_status,
                 links.resolution_diagnostic
             FROM links
             LEFT JOIN files AS target_files ON target_files.id = links.target_file_id
             LEFT JOIN headings AS target_headings ON target_headings.id = links.target_heading_id
             ORDER BY links.byte_start",
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
        );

        assert_eq!(
            rows,
            vec![
                (
                    "[[file:target.org::*Heading]]".to_string(),
                    Some(target_path.to_string_lossy().to_string()),
                    Some("Heading".to_string()),
                    Some("*Heading".to_string()),
                    Some("resolved".to_string()),
                    None,
                ),
                (
                    "[[file:target.org::*   Main Index   ]]".to_string(),
                    Some(target_path.to_string_lossy().to_string()),
                    Some("Main index".to_string()),
                    Some("*   Main Index   ".to_string()),
                    Some("resolved".to_string()),
                    None,
                ),
                (
                    "[[file:target.org::*ärger]]".to_string(),
                    Some(target_path.to_string_lossy().to_string()),
                    Some("Ärger".to_string()),
                    Some("*ärger".to_string()),
                    Some("resolved".to_string()),
                    None,
                ),
                (
                    "[[file:target.org::*\\[2026-07-01 Wed\\] Implement deterministic link resolution pass after rebuild]]".to_string(),
                    Some(target_path.to_string_lossy().to_string()),
                    Some(
                        "[2026-07-01 Wed] Implement deterministic link resolution pass after rebuild"
                            .to_string(),
                    ),
                    Some(
                        "*\\[2026-07-01 Wed\\] Implement deterministic link resolution pass after rebuild"
                            .to_string(),
                    ),
                    Some("resolved".to_string()),
                    None,
                ),
                (
                    "[[file:target.org::*Missing]]".to_string(),
                    Some(target_path.to_string_lossy().to_string()),
                    None,
                    Some("*Missing".to_string()),
                    Some("broken".to_string()),
                    Some(HEADING_TITLE_MISSING_DIAGNOSTIC.to_string()),
                ),
                (
                    "[[file:target.org::*Duplicate]]".to_string(),
                    Some(target_path.to_string_lossy().to_string()),
                    Some("Duplicate".to_string()),
                    Some("*Duplicate".to_string()),
                    Some("resolved".to_string()),
                    None,
                ),
                (
                    "[[file:target.org::#custom-id]]".to_string(),
                    Some(target_path.to_string_lossy().to_string()),
                    None,
                    Some("#custom-id".to_string()),
                    Some("broken".to_string()),
                    Some(CUSTOM_ID_MISSING_DIAGNOSTIC.to_string()),
                ),
            ]
        );
    }

    #[test]
    fn rebuild_resolves_file_only_links_to_synthetic_root_headings() {
        let test_dir = TestDir::new("file-only-root-heading-resolution");
        let notes_dir = test_dir.path().join("notes");
        let external_dir = test_dir.path().join("external");
        let db_path = test_dir.path().join("db.sqlite");
        let config_path = test_dir.path().join("config.toml");
        let source_path = notes_dir.join("source.org");
        let target_path = notes_dir.join("target.org");
        let child_path = notes_dir.join("child").join("child.org");
        let external_path = external_dir.join("outside.org");

        write_file(
            &source_path,
            &format!(
                "\
#+TITLE: Source
[[file:target.org]]
[[./target.org]]
[[file:child/child.org]]
<file:target.org>
file:target.org
[[{}]]
[[file:target.org::*Explicit heading]]
[[file:target.org::#custom-id]]
[[file:missing.org]]
",
                external_path.to_string_lossy()
            ),
        );
        write_file(
            &target_path,
            "\
#+TITLE: Target
* Explicit heading
:PROPERTIES:
:CUSTOM_ID: custom-id
:END:
",
        );
        write_file(
            &child_path,
            "\
#+TITLE: Child
",
        );
        write_file(
            &external_path,
            "\
#+TITLE: External
",
        );
        write_config(
            &config_path,
            r#"
db_path = "db.sqlite"

[[dirs]]
path = "notes"
recursive = true

[search]
fts5_enabled = false
index_body_text = false
"#,
        );

        Indexer::new(OrgizeAdapter::new())
            .rebuild_from_config_path(&config_path)
            .expect("rebuild should succeed");

        let connection = Connection::open(&db_path).expect("db should open");
        let rows: Vec<FileOnlyRootResolutionRow> = query_rows(
            &connection,
            "SELECT
                 links.raw,
                 target_files.path,
                 target_headings.title,
                 links.resolution_status,
                 links.resolution_diagnostic
             FROM links
             LEFT JOIN files AS target_files ON target_files.id = links.target_file_id
             LEFT JOIN headings AS target_headings ON target_headings.id = links.target_heading_id
             ORDER BY links.byte_start",
            |row| {
                Ok((
                    row.get(0)?,
                    row.get(1)?,
                    row.get(2)?,
                    row.get(3)?,
                    row.get(4)?,
                ))
            },
        );

        assert_eq!(
            rows,
            vec![
                (
                    "[[file:target.org]]".to_string(),
                    Some(target_path.to_string_lossy().to_string()),
                    Some("Target".to_string()),
                    Some("resolved".to_string()),
                    None,
                ),
                (
                    "[[./target.org]]".to_string(),
                    Some(target_path.to_string_lossy().to_string()),
                    Some("Target".to_string()),
                    Some("resolved".to_string()),
                    None,
                ),
                (
                    "[[file:child/child.org]]".to_string(),
                    Some(child_path.to_string_lossy().to_string()),
                    Some("Child".to_string()),
                    Some("resolved".to_string()),
                    None,
                ),
                (
                    "<file:target.org>".to_string(),
                    Some(target_path.to_string_lossy().to_string()),
                    Some("Target".to_string()),
                    Some("resolved".to_string()),
                    None,
                ),
                (
                    "file:target.org".to_string(),
                    Some(target_path.to_string_lossy().to_string()),
                    Some("Target".to_string()),
                    Some("resolved".to_string()),
                    None,
                ),
                (
                    format!("[[{}]]", external_path.to_string_lossy()),
                    None,
                    None,
                    Some("unresolved".to_string()),
                    Some(FILE_OUTSIDE_UNIVERSE_DIAGNOSTIC.to_string()),
                ),
                (
                    "[[file:target.org::*Explicit heading]]".to_string(),
                    Some(target_path.to_string_lossy().to_string()),
                    Some("Explicit heading".to_string()),
                    Some("resolved".to_string()),
                    None,
                ),
                (
                    "[[file:target.org::#custom-id]]".to_string(),
                    Some(target_path.to_string_lossy().to_string()),
                    Some("Explicit heading".to_string()),
                    Some("resolved".to_string()),
                    None,
                ),
                (
                    "[[file:missing.org]]".to_string(),
                    None,
                    None,
                    Some("broken".to_string()),
                    Some(FILE_MISSING_DIAGNOSTIC.to_string()),
                ),
            ]
        );
    }

    #[test]
    fn rebuild_resolves_file_context_custom_id_links() {
        let test_dir = TestDir::new("file-context-custom-id-resolution");
        let notes_dir = test_dir.path().join("notes");
        let db_path = test_dir.path().join("db.sqlite");
        let config_path = test_dir.path().join("config.toml");
        let source_path = notes_dir.join("source.org");
        let target_path = notes_dir.join("target.org");

        write_file(
            &source_path,
            "\
#+TITLE: Source
[[file:target.org::#custom-id]]
[[file:target.org::# Custom-ID ][Description]]
[[file:target.org::# abc ]]
[[file:target.org::# ab]]
[[./target.org::#dup]]
<file:target.org::#angle-id>
file:target.org::#plain-id
[[file:target.org::#missing]]
[[file:missing.org::#custom-id]]
",
        );
        write_file(
            &target_path,
            "\
#+TITLE: Target
* Target heading
:PROPERTIES:
:CUSTOM_ID: custom-id
:END:
* Spaced target
:PROPERTIES:
:CUSTOM_ID:  abc 
:END:
* First duplicate
:PROPERTIES:
:CUSTOM_ID: dup
:END:
* Second duplicate
:PROPERTIES:
:CUSTOM_ID: DUP
:END:
* Angle heading
:PROPERTIES:
:CUSTOM_ID: angle-id
:END:
* Plain heading
:PROPERTIES:
:CUSTOM_ID: plain-id
:END:
",
        );
        write_config(
            &config_path,
            r#"
db_path = "db.sqlite"

[[dirs]]
path = "notes"
recursive = true

[search]
fts5_enabled = false
index_body_text = false
"#,
        );

        Indexer::new(OrgizeAdapter::new())
            .rebuild_from_config_path(&config_path)
            .expect("rebuild should succeed");

        let connection = Connection::open(&db_path).expect("db should open");
        let rows: Vec<FileContextCustomIdResolutionRow> = query_rows(
            &connection,
            "SELECT
                 links.raw,
                 target_files.path,
                 target_headings.title,
                 links.raw_description,
                 links.target_custom_id,
                 links.resolution_status,
                 links.resolution_diagnostic
             FROM links
             LEFT JOIN files AS target_files ON target_files.id = links.target_file_id
             LEFT JOIN headings AS target_headings ON target_headings.id = links.target_heading_id
             ORDER BY links.byte_start",
            |row| {
                Ok((
                    row.get(0)?,
                    row.get(1)?,
                    row.get(2)?,
                    row.get(3)?,
                    row.get(4)?,
                    row.get(5)?,
                    row.get(6)?,
                ))
            },
        );

        assert_eq!(
            rows,
            vec![
                (
                    "[[file:target.org::#custom-id]]".to_string(),
                    Some(target_path.to_string_lossy().to_string()),
                    Some("Target heading".to_string()),
                    None,
                    Some("custom-id".to_string()),
                    Some("resolved".to_string()),
                    None,
                ),
                (
                    "[[file:target.org::# Custom-ID ][Description]]".to_string(),
                    Some(target_path.to_string_lossy().to_string()),
                    None,
                    Some("Description".to_string()),
                    Some(" Custom-ID ".to_string()),
                    Some("broken".to_string()),
                    Some(CUSTOM_ID_MISSING_DIAGNOSTIC.to_string()),
                ),
                (
                    "[[file:target.org::# abc ]]".to_string(),
                    Some(target_path.to_string_lossy().to_string()),
                    Some("Spaced target".to_string()),
                    None,
                    Some(" abc ".to_string()),
                    Some("resolved".to_string()),
                    None,
                ),
                (
                    "[[file:target.org::# ab]]".to_string(),
                    Some(target_path.to_string_lossy().to_string()),
                    None,
                    None,
                    Some(" ab".to_string()),
                    Some("broken".to_string()),
                    Some(CUSTOM_ID_MISSING_DIAGNOSTIC.to_string()),
                ),
                (
                    "[[./target.org::#dup]]".to_string(),
                    Some(target_path.to_string_lossy().to_string()),
                    Some("First duplicate".to_string()),
                    None,
                    Some("dup".to_string()),
                    Some("resolved".to_string()),
                    None,
                ),
                (
                    "<file:target.org::#angle-id>".to_string(),
                    Some(target_path.to_string_lossy().to_string()),
                    Some("Angle heading".to_string()),
                    None,
                    Some("angle-id".to_string()),
                    Some("resolved".to_string()),
                    None,
                ),
                (
                    "file:target.org::#plain-id".to_string(),
                    Some(target_path.to_string_lossy().to_string()),
                    Some("Plain heading".to_string()),
                    None,
                    Some("plain-id".to_string()),
                    Some("resolved".to_string()),
                    None,
                ),
                (
                    "[[file:target.org::#missing]]".to_string(),
                    Some(target_path.to_string_lossy().to_string()),
                    None,
                    None,
                    Some("missing".to_string()),
                    Some("broken".to_string()),
                    Some(CUSTOM_ID_MISSING_DIAGNOSTIC.to_string()),
                ),
                (
                    "[[file:missing.org::#custom-id]]".to_string(),
                    None,
                    None,
                    None,
                    None,
                    Some("broken".to_string()),
                    Some(FILE_MISSING_DIAGNOSTIC.to_string()),
                ),
            ]
        );
    }

    #[test]
    fn rebuild_resolves_org_id_links() {
        let test_dir = TestDir::new("org-id-resolution");
        let notes_dir = test_dir.path().join("notes");
        let db_path = test_dir.path().join("db.sqlite");
        let config_path = test_dir.path().join("config.toml");
        let source_path = notes_dir.join("source.org");
        let target_a_path = notes_dir.join("target-a.org");
        let target_b_path = notes_dir.join("target-b.org");
        let target_c_path = notes_dir.join("target-c.org");

        write_file(
            &source_path,
            "\
#+TITLE: Source
id:foo
[[id: FOO ][Description]]
[[id:123 56 ]]
[[id: 23]]
[[id: ab]]
[[id:ab ]]
<id:angle-id>
id:dup
[[id:missing]]
",
        );
        write_file(
            &target_a_path,
            "\
#+TITLE: Target A
* Exact target
:PROPERTIES:
:ID: foo
:END:
* Spaced tail target
:PROPERTIES:
:ID: 123 56 
:END:
* Spaced head target
:PROPERTIES:
:ID:  23
:END:
* Angle target
:PROPERTIES:
:ID: angle-id
:END:
",
        );
        write_file(
            &target_b_path,
            "\
#+TITLE: Target B
* First duplicate
:PROPERTIES:
:ID: dup
:END:
",
        );
        write_file(
            &target_c_path,
            "\
#+TITLE: Target C
* Second duplicate
:PROPERTIES:
:ID: DUP
:END:
",
        );
        write_config(
            &config_path,
            r#"
db_path = "db.sqlite"

[[dirs]]
path = "notes"
recursive = true

[search]
fts5_enabled = false
index_body_text = false
"#,
        );

        Indexer::new(OrgizeAdapter::new())
            .rebuild_from_config_path(&config_path)
            .expect("rebuild should succeed");

        let connection = Connection::open(&db_path).expect("db should open");
        let rows: Vec<OrgIdResolutionRow> = query_rows(
            &connection,
            "SELECT
                 links.raw,
                 target_files.path,
                 target_headings.title,
                 links.raw_description,
                 links.target_id,
                 links.resolution_status,
                 links.resolution_diagnostic
             FROM links
             LEFT JOIN files AS target_files ON target_files.id = links.target_file_id
             LEFT JOIN headings AS target_headings ON target_headings.id = links.target_heading_id
             ORDER BY links.byte_start",
            |row| {
                Ok((
                    row.get(0)?,
                    row.get(1)?,
                    row.get(2)?,
                    row.get(3)?,
                    row.get(4)?,
                    row.get(5)?,
                    row.get(6)?,
                ))
            },
        );

        assert_eq!(
            rows,
            vec![
                (
                    "id:foo".to_string(),
                    Some(target_a_path.to_string_lossy().to_string()),
                    Some("Exact target".to_string()),
                    None,
                    Some("foo".to_string()),
                    Some("resolved".to_string()),
                    None,
                ),
                (
                    "[[id: FOO ][Description]]".to_string(),
                    None,
                    None,
                    Some("Description".to_string()),
                    Some(" FOO ".to_string()),
                    Some("unresolved".to_string()),
                    Some(ID_MISSING_DIAGNOSTIC.to_string()),
                ),
                (
                    "[[id:123 56 ]]".to_string(),
                    Some(target_a_path.to_string_lossy().to_string()),
                    Some("Spaced tail target".to_string()),
                    None,
                    Some("123 56 ".to_string()),
                    Some("resolved".to_string()),
                    None,
                ),
                (
                    "[[id: 23]]".to_string(),
                    Some(target_a_path.to_string_lossy().to_string()),
                    Some("Spaced head target".to_string()),
                    None,
                    Some(" 23".to_string()),
                    Some("resolved".to_string()),
                    None,
                ),
                (
                    "[[id: ab]]".to_string(),
                    None,
                    None,
                    None,
                    Some(" ab".to_string()),
                    Some("unresolved".to_string()),
                    Some(ID_MISSING_DIAGNOSTIC.to_string()),
                ),
                (
                    "[[id:ab ]]".to_string(),
                    None,
                    None,
                    None,
                    Some("ab ".to_string()),
                    Some("unresolved".to_string()),
                    Some(ID_MISSING_DIAGNOSTIC.to_string()),
                ),
                (
                    "<id:angle-id>".to_string(),
                    Some(target_a_path.to_string_lossy().to_string()),
                    Some("Angle target".to_string()),
                    None,
                    Some("angle-id".to_string()),
                    Some("resolved".to_string()),
                    None,
                ),
                (
                    "id:dup".to_string(),
                    None,
                    None,
                    None,
                    Some("dup".to_string()),
                    Some("ambiguous".to_string()),
                    Some(DUPLICATE_ID_DIAGNOSTIC.to_string()),
                ),
                (
                    "[[id:missing]]".to_string(),
                    None,
                    None,
                    None,
                    Some("missing".to_string()),
                    Some("unresolved".to_string()),
                    Some(ID_MISSING_DIAGNOSTIC.to_string()),
                ),
            ]
        );
    }

    #[test]
    fn rebuild_resolves_same_file_fuzzy_star_heading_links() {
        let test_dir = TestDir::new("same-file-fuzzy-star-heading-resolution");
        let notes_dir = test_dir.path().join("notes");
        let db_path = test_dir.path().join("db.sqlite");
        let config_path = test_dir.path().join("config.toml");
        let source_path = notes_dir.join("source.org");

        write_file(
            &source_path,
            "\
#+TITLE: Source
[[*Heading]]
[[*Heading][Description]]
[[*   Peer heading   ]]
[[*ärger]]
[[*\\[2026-07-01 Wed\\] Implement deterministic link resolution pass after rebuild]]
[[*Missing]]
[[*Duplicate]]
[[Heading]]
* TODO [#A] Heading :tag:
* Peer Heading
* Ärger
* [2026-07-01 Wed] Implement deterministic link resolution pass after rebuild
* Duplicate
* Duplicate
",
        );
        write_config(
            &config_path,
            r#"
db_path = "db.sqlite"

[[dirs]]
path = "notes"
recursive = true

[search]
fts5_enabled = false
index_body_text = false
"#,
        );

        Indexer::new(OrgizeAdapter::new())
            .rebuild_from_config_path(&config_path)
            .expect("rebuild should succeed");

        let connection = Connection::open(&db_path).expect("db should open");
        let rows: Vec<SameFileStarHeadingResolutionRow> = query_rows(
            &connection,
            "SELECT
                 links.raw,
                 target_files.path,
                 target_headings.title,
                 links.raw_description,
                 links.resolution_status,
                 links.resolution_diagnostic,
                 links.search_option
             FROM links
             LEFT JOIN files AS target_files ON target_files.id = links.target_file_id
             LEFT JOIN headings AS target_headings ON target_headings.id = links.target_heading_id
             ORDER BY links.byte_start",
            |row| {
                Ok((
                    row.get(0)?,
                    row.get(1)?,
                    row.get(2)?,
                    row.get(3)?,
                    row.get(4)?,
                    row.get(5)?,
                    row.get(6)?,
                ))
            },
        );

        assert_eq!(
            rows,
            vec![
                (
                    "[[*Heading]]".to_string(),
                    Some(source_path.to_string_lossy().to_string()),
                    Some("Heading".to_string()),
                    None,
                    Some("resolved".to_string()),
                    None,
                    None,
                ),
                (
                    "[[*Heading][Description]]".to_string(),
                    Some(source_path.to_string_lossy().to_string()),
                    Some("Heading".to_string()),
                    Some("Description".to_string()),
                    Some("resolved".to_string()),
                    None,
                    None,
                ),
                (
                    "[[*   Peer heading   ]]".to_string(),
                    Some(source_path.to_string_lossy().to_string()),
                    Some("Peer Heading".to_string()),
                    None,
                    Some("resolved".to_string()),
                    None,
                    None,
                ),
                (
                    "[[*ärger]]".to_string(),
                    Some(source_path.to_string_lossy().to_string()),
                    Some("Ärger".to_string()),
                    None,
                    Some("resolved".to_string()),
                    None,
                    None,
                ),
                (
                    "[[*\\[2026-07-01 Wed\\] Implement deterministic link resolution pass after rebuild]]"
                        .to_string(),
                    Some(source_path.to_string_lossy().to_string()),
                    Some(
                        "[2026-07-01 Wed] Implement deterministic link resolution pass after rebuild"
                            .to_string(),
                    ),
                    None,
                    Some("resolved".to_string()),
                    None,
                    None,
                ),
                (
                    "[[*Missing]]".to_string(),
                    Some(source_path.to_string_lossy().to_string()),
                    None,
                    None,
                    Some("broken".to_string()),
                    Some(SAME_FILE_STAR_HEADING_MISSING_DIAGNOSTIC.to_string()),
                    None,
                ),
                (
                    "[[*Duplicate]]".to_string(),
                    Some(source_path.to_string_lossy().to_string()),
                    Some("Duplicate".to_string()),
                    None,
                    Some("resolved".to_string()),
                    None,
                    None,
                ),
                (
                    "[[Heading]]".to_string(),
                    None,
                    None,
                    None,
                    Some("unsupported".to_string()),
                    Some(UNSUPPORTED_DIAGNOSTIC.to_string()),
                    None,
                ),
            ]
        );
    }

    #[test]
    fn rebuild_resolves_same_file_custom_id_links() {
        let test_dir = TestDir::new("same-file-fuzzy-custom-id-resolution");
        let notes_dir = test_dir.path().join("notes");
        let db_path = test_dir.path().join("db.sqlite");
        let config_path = test_dir.path().join("config.toml");
        let source_path = notes_dir.join("source.org");

        write_file(
            &source_path,
            "\
#+TITLE: Source
[[#custom-id]]
[[# Custom-ID ][Description]]
[[#abc ]]
[[# abc]]
[[# ab]]
[[#dup]]
[[#missing]]
[[Heading]]
* Target heading
:PROPERTIES:
:CUSTOM_ID: custom-id
:END:
* Spaced tail target
:PROPERTIES:
:CUSTOM_ID: abc 
:END:
* Spaced head target
:PROPERTIES:
:CUSTOM_ID:  abc
:END:
* First duplicate
:PROPERTIES:
:CUSTOM_ID: dup
:END:
* Second duplicate
:PROPERTIES:
:CUSTOM_ID: DUP
:END:
",
        );
        write_config(
            &config_path,
            r#"
db_path = "db.sqlite"

[[dirs]]
path = "notes"
recursive = true

[search]
fts5_enabled = false
index_body_text = false
"#,
        );

        Indexer::new(OrgizeAdapter::new())
            .rebuild_from_config_path(&config_path)
            .expect("rebuild should succeed");

        let connection = Connection::open(&db_path).expect("db should open");
        let rows: Vec<SameFileCustomIdResolutionRow> = query_rows(
            &connection,
            "SELECT
                 links.raw,
                 target_files.path,
                 target_headings.title,
                 links.raw_description,
                 links.target_custom_id,
                 links.resolution_status,
                 links.resolution_diagnostic
             FROM links
             LEFT JOIN files AS target_files ON target_files.id = links.target_file_id
             LEFT JOIN headings AS target_headings ON target_headings.id = links.target_heading_id
             ORDER BY links.byte_start",
            |row| {
                Ok((
                    row.get(0)?,
                    row.get(1)?,
                    row.get(2)?,
                    row.get(3)?,
                    row.get(4)?,
                    row.get(5)?,
                    row.get(6)?,
                ))
            },
        );

        assert_eq!(
            rows,
            vec![
                (
                    "[[#custom-id]]".to_string(),
                    Some(source_path.to_string_lossy().to_string()),
                    Some("Target heading".to_string()),
                    None,
                    Some("custom-id".to_string()),
                    Some("resolved".to_string()),
                    None,
                ),
                (
                    "[[# Custom-ID ][Description]]".to_string(),
                    Some(source_path.to_string_lossy().to_string()),
                    None,
                    Some("Description".to_string()),
                    Some(" Custom-ID ".to_string()),
                    Some("broken".to_string()),
                    Some(CUSTOM_ID_MISSING_DIAGNOSTIC.to_string()),
                ),
                (
                    "[[#abc ]]".to_string(),
                    Some(source_path.to_string_lossy().to_string()),
                    Some("Spaced tail target".to_string()),
                    None,
                    Some("abc ".to_string()),
                    Some("resolved".to_string()),
                    None,
                ),
                (
                    "[[# abc]]".to_string(),
                    Some(source_path.to_string_lossy().to_string()),
                    Some("Spaced head target".to_string()),
                    None,
                    Some(" abc".to_string()),
                    Some("resolved".to_string()),
                    None,
                ),
                (
                    "[[# ab]]".to_string(),
                    Some(source_path.to_string_lossy().to_string()),
                    None,
                    None,
                    Some(" ab".to_string()),
                    Some("broken".to_string()),
                    Some(CUSTOM_ID_MISSING_DIAGNOSTIC.to_string()),
                ),
                (
                    "[[#dup]]".to_string(),
                    Some(source_path.to_string_lossy().to_string()),
                    Some("First duplicate".to_string()),
                    None,
                    Some("dup".to_string()),
                    Some("resolved".to_string()),
                    None,
                ),
                (
                    "[[#missing]]".to_string(),
                    Some(source_path.to_string_lossy().to_string()),
                    None,
                    None,
                    Some("missing".to_string()),
                    Some("broken".to_string()),
                    Some(CUSTOM_ID_MISSING_DIAGNOSTIC.to_string()),
                ),
                (
                    "[[Heading]]".to_string(),
                    None,
                    None,
                    None,
                    None,
                    Some("unsupported".to_string()),
                    Some(UNSUPPORTED_DIAGNOSTIC.to_string()),
                ),
            ]
        );
    }

    #[test]
    fn rebuild_keeps_source_link_rows_when_target_file_disappears_from_indexed_set() {
        let test_dir = TestDir::new("file-link-target-removed");
        let notes_dir = test_dir.path().join("notes");
        let db_path = test_dir.path().join("db.sqlite");
        let config_path = test_dir.path().join("config.toml");
        let source_path = notes_dir.join("source.org");
        let target_path = notes_dir.join("target.org");

        write_file(&source_path, "#+TITLE: Source\n[[./target.org]]\n");
        write_file(&target_path, "* Target\n");
        write_config(
            &config_path,
            r#"
db_path = "db.sqlite"

[[dirs]]
path = "notes"
recursive = true

[search]
fts5_enabled = false
index_body_text = false
"#,
        );

        Indexer::new(OrgizeAdapter::new())
            .rebuild_from_config_path(&config_path)
            .expect("first rebuild should succeed");
        std::fs::remove_file(&target_path).expect("target file should delete");

        Indexer::new(OrgizeAdapter::new())
            .rebuild_from_config_path(&config_path)
            .expect("second rebuild should succeed");

        let connection = Connection::open(&db_path).expect("db should open");
        let link_rows: Vec<TargetRemovalLinkRow> = query_rows(
            &connection,
            "SELECT raw, raw_target, target_file_id, resolution_status, resolution_diagnostic
                 FROM links
                 ORDER BY byte_start",
            |row| {
                Ok((
                    row.get(0)?,
                    row.get(1)?,
                    row.get(2)?,
                    row.get(3)?,
                    row.get(4)?,
                ))
            },
        );

        assert_eq!(
            link_rows,
            vec![(
                "[[./target.org]]".to_string(),
                "./target.org".to_string(),
                None,
                Some("broken".to_string()),
                Some(FILE_MISSING_DIAGNOSTIC.to_string()),
            )]
        );
    }

    #[test]
    fn rebuild_stores_plain_links_with_reviewed_boundary_and_end_semantics() {
        let test_dir = TestDir::new("plain-link-boundaries");
        let notes_dir = test_dir.path().join("notes");
        let db_path = test_dir.path().join("db.sqlite");
        let config_path = test_dir.path().join("config.toml");
        let org_path = notes_dir.join("links.org");

        write_file(
            &org_path,
            "\
#+TITLE: Plain Boundaries
!https://www.example.com
\"https://www.example.com
_https://www.example.com
'https://www.example.com
$https://www.example.com
%https://www.example.com
xhttps://www.example.com
Prefix:https://www.example.com
https://example.org/path with text after whitespace
https://example.org/path<balanced-suffix>
https://example.org/path(foo)
https://example.org/path[foo]
https://example.org/path.
https://example.org/path/
https://example.org/path-
https://example.org/path>not-part-of-plain-link
https://example.org/path<not-part-of-plain-link
",
        );
        write_config(
            &config_path,
            r#"
db_path = "db.sqlite"
files = ["notes/links.org"]

[search]
fts5_enabled = false
index_body_text = false
"#,
        );

        Indexer::new(OrgizeAdapter::new())
            .rebuild_from_config_path(&config_path)
            .expect("rebuild should succeed");

        let connection = Connection::open(&db_path).expect("db should open");
        let rows: Vec<String> = query_rows(
            &connection,
            "SELECT raw FROM links ORDER BY byte_start",
            |row| row.get(0),
        );

        assert_eq!(
            rows,
            vec![
                "https://www.example.com".to_string(),
                "https://www.example.com".to_string(),
                "https://www.example.com".to_string(),
                "https://www.example.com".to_string(),
                "https://example.org/path".to_string(),
                "https://example.org/path<balanced-suffix>".to_string(),
                "https://example.org/path(foo)".to_string(),
                "https://example.org/path[foo]".to_string(),
                "https://example.org/path".to_string(),
                "https://example.org/path/".to_string(),
                "https://example.org/path-".to_string(),
                "https://example.org/path".to_string(),
                "https://example.org/path".to_string(),
            ]
        );
    }

    #[test]
    fn rebuild_ignores_links_in_ignored_regions_and_persists_source_contexts() {
        let test_dir = TestDir::new("links-source-context");
        let notes_dir = test_dir.path().join("notes");
        let db_path = test_dir.path().join("db.sqlite");
        let config_path = test_dir.path().join("config.toml");
        let org_path = notes_dir.join("links.org");

        write_file(
            &org_path,
            "\
#+TITLE: Source Contexts
#+PROPERTY: ignored https://example.org/in-property-keyword
Before heading https://example.org/in-root-paragraph

* Heading with https://example.org/in-heading
Inline =https://example.org/in-code= and ~https://example.org/in-verbatim~
src_sh{https://example.org/in-inline-src}
@@html:https://example.org/in-inline-export@@

#+BEGIN_SRC text
https://example.org/in-source-block
#+END_SRC

#+BEGIN_EXAMPLE
https://example.org/in-example-block
#+END_EXAMPLE

: https://example.org/in-colon-example-line

#+BEGIN_COMMENT
https://example.org/in-comment-block
#+END_COMMENT

# https://example.org/in-comment-line

#+BEGIN_EXPORT HTML
https://example.org/in-export-block
#+END_EXPORT

Paragraph https://example.org/in-paragraph

#+BEGIN_VERSE
https://example.org/in-verse
#+END_VERSE

#+BEGIN_QUOTE
https://example.org/in-quote
#+END_QUOTE

#+BEGIN_CENTER
https://example.org/in-center
#+END_CENTER

#+BEGIN_JUSTIFY
https://example.org/in-justify
#+END_JUSTIFY

:PROPERTIES:
:LINK: https://example.org/in-property-drawer
:END:

:A_DRAWER:
https://example.org/in-drawer
:END:
",
        );
        write_config(
            &config_path,
            r#"
db_path = "db.sqlite"
files = ["notes/links.org"]

[search]
fts5_enabled = false
index_body_text = false
"#,
        );

        Indexer::new(OrgizeAdapter::new())
            .rebuild_from_config_path(&config_path)
            .expect("rebuild should succeed");

        let connection = Connection::open(&db_path).expect("db should open");
        let rows: Vec<(String, String, String)> = query_rows(
            &connection,
            "SELECT h.title, l.raw, l.source_context
             FROM links l
             JOIN headings h ON h.id = l.heading_id
             ORDER BY l.byte_start",
            |row| Ok((row.get(0)?, row.get(1)?, row.get(2)?)),
        );

        assert_eq!(
            rows,
            vec![
                (
                    "Source Contexts".to_string(),
                    "https://example.org/in-root-paragraph".to_string(),
                    "normal".to_string(),
                ),
                (
                    "Heading with https://example.org/in-heading".to_string(),
                    "https://example.org/in-heading".to_string(),
                    "heading".to_string(),
                ),
                (
                    "Heading with https://example.org/in-heading".to_string(),
                    "https://example.org/in-paragraph".to_string(),
                    "normal".to_string(),
                ),
                (
                    "Heading with https://example.org/in-heading".to_string(),
                    "https://example.org/in-verse".to_string(),
                    "verse_block".to_string(),
                ),
                (
                    "Heading with https://example.org/in-heading".to_string(),
                    "https://example.org/in-quote".to_string(),
                    "quote_block".to_string(),
                ),
                (
                    "Heading with https://example.org/in-heading".to_string(),
                    "https://example.org/in-center".to_string(),
                    "center_block".to_string(),
                ),
                (
                    "Heading with https://example.org/in-heading".to_string(),
                    "https://example.org/in-justify".to_string(),
                    "justify_block".to_string(),
                ),
                (
                    "Heading with https://example.org/in-heading".to_string(),
                    "https://example.org/in-property-drawer".to_string(),
                    "property_drawer".to_string(),
                ),
                (
                    "Heading with https://example.org/in-heading".to_string(),
                    "https://example.org/in-drawer".to_string(),
                    "drawer".to_string(),
                ),
            ]
        );
    }

    #[test]
    fn child_heading_inherits_parent_tags_in_effective_tags() {
        let test_dir = TestDir::new("inherited-tags");
        let org_path = test_dir.path().join("tags.org");
        let db_path = test_dir.path().join("db.sqlite");
        let config = Config {
            db_path: db_path.clone(),
            files: vec![org_path.clone()],
            dirs: Vec::new(),
            links: Default::default(),
            todo: Default::default(),
            search: crate::config::SearchConfig {
                fts5_enabled: false,
                index_body_text: false,
            },
            query: Default::default(),
            discovery: Default::default(),
        };
        let mut connection = crate::db::open_database_with_schema(
            &db_path,
            &crate::db::SchemaDefinition::new(1, false),
        )
        .expect("db should open");

        write_file(&org_path, "* TODO me :test:\n** again :me:\n");
        Indexer::new(OrgizeAdapter::new())
            .rebuild(&mut connection, &config)
            .expect("rebuild should succeed");

        let headings = DbReader::list_headings(&connection).expect("headings should load");
        assert_eq!(headings[1].all_tags, vec!["test".to_string()]);
        assert_eq!(
            headings[2].all_tags,
            vec!["test".to_string(), "me".to_string()]
        );

        let tag_rows: Vec<String> = query_rows(
            &connection,
            "SELECT tag FROM tags ORDER BY heading_id, tag",
            |row| row.get(0),
        );
        assert_eq!(tag_rows, vec!["test".to_string(), "me".to_string()]);
    }

    #[test]
    fn rebuild_stores_direct_and_effective_heading_tags_and_filetags() {
        let test_dir = TestDir::new("filetags-direct-tags");
        let org_path = test_dir.path().join("tags.org");
        let db_path = test_dir.path().join("db.sqlite");
        let config = Config {
            db_path: db_path.clone(),
            files: vec![org_path.clone()],
            dirs: Vec::new(),
            links: Default::default(),
            todo: Default::default(),
            search: crate::config::SearchConfig {
                fts5_enabled: false,
                index_body_text: false,
            },
            query: Default::default(),
            discovery: Default::default(),
        };
        let mut connection = crate::db::open_database_with_schema(
            &db_path,
            &crate::db::SchemaDefinition::new(1, false),
        )
        .expect("db should open");

        write_file(
            &org_path,
            "#+TITLE: Tags Fixture\n#+FILETAGS: :file:project:\n\n* Parent :parent:\nParent body.\n\n** Child :child:\nChild body.\n\n* Sibling\nSibling body.\n",
        );
        Indexer::new(OrgizeAdapter::new())
            .rebuild(&mut connection, &config)
            .expect("rebuild should succeed");

        let headings = DbReader::list_headings(&connection).expect("headings should load");
        assert_eq!(
            headings[0].all_tags,
            vec!["file".to_string(), "project".to_string()]
        );
        assert_eq!(
            headings[1].all_tags,
            vec![
                "file".to_string(),
                "project".to_string(),
                "parent".to_string()
            ]
        );
        assert_eq!(
            headings[2].all_tags,
            vec![
                "file".to_string(),
                "project".to_string(),
                "parent".to_string(),
                "child".to_string()
            ]
        );
        assert_eq!(
            headings[3].all_tags,
            vec!["file".to_string(), "project".to_string()]
        );

        let tag_rows: Vec<(i64, String)> = query_rows(
            &connection,
            "SELECT headings.level, tags.tag
             FROM tags
             INNER JOIN headings ON headings.id = tags.heading_id
             ORDER BY headings.level, tags.tag",
            |row| Ok((row.get(0)?, row.get(1)?)),
        );
        assert_eq!(
            tag_rows,
            vec![
                (0, "file".to_string()),
                (0, "project".to_string()),
                (1, "parent".to_string()),
                (2, "child".to_string()),
            ]
        );

        let raw_keywords: Vec<(i64, String, Option<String>, Option<i64>)> = query_rows(
            &connection,
            "SELECT headings.level, keywords.keyword, keywords.value, keywords.line_number
             FROM keywords
             INNER JOIN headings ON headings.id = keywords.heading_id
             WHERE keywords.keyword = 'FILETAGS'
             ORDER BY keywords.line_number, keywords.id",
            |row| Ok((row.get(0)?, row.get(1)?, row.get(2)?, row.get(3)?)),
        );
        assert_eq!(
            raw_keywords,
            vec![(
                0,
                "FILETAGS".to_string(),
                Some(":file:project:".to_string()),
                Some(2),
            )]
        );
    }

    #[test]
    fn duplicate_inherited_tags_are_not_repeated_in_effective_tags() {
        let test_dir = TestDir::new("duplicate-inherited-tags");
        let org_path = test_dir.path().join("tags.org");
        let db_path = test_dir.path().join("db.sqlite");
        let config = Config {
            db_path: db_path.clone(),
            files: vec![org_path.clone()],
            dirs: Vec::new(),
            links: Default::default(),
            todo: Default::default(),
            search: crate::config::SearchConfig {
                fts5_enabled: false,
                index_body_text: false,
            },
            query: Default::default(),
            discovery: Default::default(),
        };
        let mut connection = crate::db::open_database_with_schema(
            &db_path,
            &crate::db::SchemaDefinition::new(1, false),
        )
        .expect("db should open");

        write_file(
            &org_path,
            "* Parent :outer:shared:\n** Child :shared:inner:\n*** Grandchild :outer:leaf:\n",
        );
        Indexer::new(OrgizeAdapter::new())
            .rebuild(&mut connection, &config)
            .expect("rebuild should succeed");

        let headings = DbReader::list_headings(&connection).expect("headings should load");
        assert_eq!(
            headings[1].all_tags,
            vec!["outer".to_string(), "shared".to_string()]
        );
        assert_eq!(
            headings[2].all_tags,
            vec![
                "outer".to_string(),
                "shared".to_string(),
                "inner".to_string()
            ]
        );
        assert_eq!(
            headings[3].all_tags,
            vec![
                "outer".to_string(),
                "shared".to_string(),
                "inner".to_string(),
                "leaf".to_string()
            ]
        );

        let tag_rows: Vec<String> = query_rows(
            &connection,
            "SELECT tag FROM tags ORDER BY heading_id, tag",
            |row| row.get(0),
        );
        assert_eq!(
            tag_rows,
            vec![
                "outer".to_string(),
                "shared".to_string(),
                "inner".to_string(),
                "shared".to_string(),
                "leaf".to_string(),
                "outer".to_string(),
            ]
        );
    }

    #[test]
    fn rebuilding_same_file_twice_is_idempotent_and_replaces_old_rows() {
        let test_dir = TestDir::new("idempotent");
        let org_path = test_dir.path().join("notes.org");
        let db_path = test_dir.path().join("db.sqlite");
        let config = Config {
            db_path: db_path.clone(),
            files: vec![org_path.clone()],
            dirs: Vec::new(),
            links: Default::default(),
            todo: Default::default(),
            search: crate::config::SearchConfig {
                fts5_enabled: false,
                index_body_text: false,
            },
            query: Default::default(),
            discovery: Default::default(),
        };
        let mut connection = crate::db::open_database_with_schema(
            &db_path,
            &crate::db::SchemaDefinition::new(1, false),
        )
        .expect("db should open");

        write_file(&org_path, "* TODO First :old:\n");
        let first_report = Indexer::new(OrgizeAdapter::new())
            .rebuild(&mut connection, &config)
            .expect("first rebuild should succeed");
        assert_eq!(first_report.indexed_files.len(), 1);

        write_file(&org_path, "* DONE Second :new:\n");
        let second_report = Indexer::new(OrgizeAdapter::new())
            .rebuild(&mut connection, &config)
            .expect("second rebuild should succeed");
        assert_eq!(second_report.indexed_files.len(), 1);

        let heading_count: i64 = connection
            .query_row("SELECT COUNT(*) FROM headings", [], |row| row.get(0))
            .expect("heading count should load");
        let tag_count: i64 = connection
            .query_row("SELECT COUNT(*) FROM tags", [], |row| row.get(0))
            .expect("tag count should load");
        let effective_tag_count: i64 = connection
            .query_row("SELECT COUNT(*) FROM effective_tags", [], |row| row.get(0))
            .expect("effective tag count should load");
        let titles: Vec<String> = query_rows(
            &connection,
            "SELECT title FROM headings WHERE level > 0 ORDER BY title",
            |row| row.get(0),
        );
        let tags: Vec<String> =
            query_rows(&connection, "SELECT tag FROM tags ORDER BY tag", |row| {
                row.get(0)
            });
        let effective_tags: Vec<String> = query_rows(
            &connection,
            "SELECT tag FROM effective_tags ORDER BY heading_id, position",
            |row| row.get(0),
        );

        assert_eq!(heading_count, 2);
        assert_eq!(tag_count, 1);
        assert_eq!(effective_tag_count, 1);
        assert_eq!(titles, vec!["Second".to_string()]);
        assert_eq!(tags, vec!["new".to_string()]);
        assert_eq!(effective_tags, vec!["new".to_string()]);
    }

    #[test]
    fn rebuilding_same_links_twice_keeps_resolution_fields_deterministic() {
        let test_dir = TestDir::new("link-resolution-repeat");
        let org_path = test_dir.path().join("notes.org");
        let db_path = test_dir.path().join("db.sqlite");
        let config = Config {
            db_path: db_path.clone(),
            files: vec![org_path.clone()],
            dirs: Vec::new(),
            links: Default::default(),
            todo: Default::default(),
            search: crate::config::SearchConfig {
                fts5_enabled: false,
                index_body_text: false,
            },
            query: Default::default(),
            discovery: Default::default(),
        };
        let mut connection = crate::db::open_database_with_schema(
            &db_path,
            &crate::db::SchemaDefinition::new(CURRENT_SCHEMA_VERSION, false),
        )
        .expect("db should open");

        write_file(&org_path, "#+TITLE: Repeat\n[[unknown:foo]]\n");

        let first_report = Indexer::new(OrgizeAdapter::new())
            .rebuild(&mut connection, &config)
            .expect("first rebuild should succeed");
        let first_rows: Vec<(String, String, Option<String>, Option<String>)> = query_rows(
            &connection,
            "SELECT raw, raw_target, resolution_status, resolution_diagnostic
             FROM links
             ORDER BY file_id, byte_start, id",
            |row| Ok((row.get(0)?, row.get(1)?, row.get(2)?, row.get(3)?)),
        );

        let second_report = Indexer::new(OrgizeAdapter::new())
            .rebuild(&mut connection, &config)
            .expect("second rebuild should succeed");
        let second_rows: Vec<(String, String, Option<String>, Option<String>)> = query_rows(
            &connection,
            "SELECT raw, raw_target, resolution_status, resolution_diagnostic
             FROM links
             ORDER BY file_id, byte_start, id",
            |row| Ok((row.get(0)?, row.get(1)?, row.get(2)?, row.get(3)?)),
        );

        assert_eq!(first_report.indexed_files.len(), 1);
        assert_eq!(second_report.indexed_files.len(), 1);
        assert_eq!(
            first_rows,
            vec![(
                "[[unknown:foo]]".to_string(),
                "unknown:foo".to_string(),
                Some("unsupported".to_string()),
                Some(UNSUPPORTED_DIAGNOSTIC.to_string()),
            )]
        );
        assert_eq!(second_rows, first_rows);
    }

    #[test]
    fn rebuild_computes_parent_ids_for_nested_headings() {
        let test_dir = TestDir::new("parent-ids");
        let org_path = test_dir.path().join("tree.org");
        let db_path = test_dir.path().join("db.sqlite");
        let config = Config {
            db_path: db_path.clone(),
            files: vec![org_path.clone()],
            dirs: Vec::new(),
            links: Default::default(),
            todo: Default::default(),
            search: crate::config::SearchConfig {
                fts5_enabled: false,
                index_body_text: false,
            },
            query: Default::default(),
            discovery: Default::default(),
        };
        let mut connection = crate::db::open_database_with_schema(
            &db_path,
            &crate::db::SchemaDefinition::new(1, false),
        )
        .expect("db should open");

        write_file(
            &org_path,
            "#+TITLE: Tree\n* Parent\n** Child\n*** Grandchild\n* Sibling\n** Cousin\n",
        );

        let indexer = Indexer::new(OrgizeAdapter::new());
        indexer
            .rebuild(&mut connection, &config)
            .expect("first rebuild should succeed");
        indexer
            .rebuild(&mut connection, &config)
            .expect("second rebuild should succeed");

        let headings: Vec<(i64, Option<i64>, i64, String)> = query_rows(
            &connection,
            "SELECT id, parent_id, level, title FROM headings ORDER BY byte_start, id",
            |row| Ok((row.get(0)?, row.get(1)?, row.get(2)?, row.get(3)?)),
        );

        let level0_count: i64 = connection
            .query_row("SELECT COUNT(*) FROM headings WHERE level = 0", [], |row| {
                row.get(0)
            })
            .expect("level 0 count should load");

        let level0 = headings
            .iter()
            .find(|(_, _, level, _)| *level == 0)
            .expect("level 0 heading should exist");
        let parent = headings
            .iter()
            .find(|(_, _, _, title)| title == "Parent")
            .expect("parent heading should exist");
        let child = headings
            .iter()
            .find(|(_, _, _, title)| title == "Child")
            .expect("child heading should exist");
        let grandchild = headings
            .iter()
            .find(|(_, _, _, title)| title == "Grandchild")
            .expect("grandchild heading should exist");
        let sibling = headings
            .iter()
            .find(|(_, _, _, title)| title == "Sibling")
            .expect("sibling heading should exist");
        let cousin = headings
            .iter()
            .find(|(_, _, _, title)| title == "Cousin")
            .expect("cousin heading should exist");

        assert_eq!(headings.len(), 6);
        assert_eq!(level0_count, 1);
        assert_eq!(level0.1, None);
        assert_eq!(parent.1, Some(level0.0));
        assert_eq!(child.1, Some(parent.0));
        assert_eq!(grandchild.1, Some(child.0));
        assert_eq!(sibling.1, Some(level0.0));
        assert_eq!(cousin.1, Some(sibling.0));
    }

    #[test]
    fn outline_rows_remain_consistent_after_rebuild() {
        let test_dir = TestDir::new("outline");
        let org_path = test_dir.path().join("outline.org");
        let db_path = test_dir.path().join("db.sqlite");
        let config = Config {
            db_path: db_path.clone(),
            files: vec![org_path.clone()],
            dirs: Vec::new(),
            links: Default::default(),
            todo: Default::default(),
            search: crate::config::SearchConfig {
                fts5_enabled: false,
                index_body_text: false,
            },
            query: Default::default(),
            discovery: Default::default(),
        };
        let mut connection = crate::db::open_database_with_schema(
            &db_path,
            &crate::db::SchemaDefinition::new(1, false),
        )
        .expect("db should open");

        write_file(
            &org_path,
            "#+TITLE: Outline\n* Parent A\n** Child A1\n*** Grandchild A1a\n** Child A2\n* Parent B\n** Child B1\n",
        );
        let indexer = Indexer::new(OrgizeAdapter::new());
        indexer
            .rebuild(&mut connection, &config)
            .expect("first rebuild should succeed");
        let first_outline: Vec<(i64, String, String)> = query_rows(
            &connection,
            "SELECT depth, materialized_path, breadcrumbs_json FROM outline_path ORDER BY materialized_path",
            |row| Ok((row.get(0)?, row.get(1)?, row.get(2)?)),
        );

        indexer
            .rebuild(&mut connection, &config)
            .expect("second rebuild should succeed");
        let second_outline: Vec<(i64, String, String)> = query_rows(
            &connection,
            "SELECT depth, materialized_path, breadcrumbs_json FROM outline_path ORDER BY materialized_path",
            |row| Ok((row.get(0)?, row.get(1)?, row.get(2)?)),
        );
        let heading_count: i64 = connection
            .query_row("SELECT COUNT(*) FROM headings", [], |row| row.get(0))
            .expect("heading count should load");
        let outline_count: i64 = connection
            .query_row("SELECT COUNT(*) FROM outline_path", [], |row| row.get(0))
            .expect("outline count should load");

        assert_eq!(
            first_outline,
            vec![
                (0, "0000".to_string(), "[\"Outline\"]".to_string()),
                (
                    1,
                    "0000.0001".to_string(),
                    "[\"Outline\",\"Parent A\"]".to_string()
                ),
                (
                    2,
                    "0000.0001.0001".to_string(),
                    "[\"Outline\",\"Parent A\",\"Child A1\"]".to_string()
                ),
                (
                    3,
                    "0000.0001.0001.0001".to_string(),
                    "[\"Outline\",\"Parent A\",\"Child A1\",\"Grandchild A1a\"]".to_string()
                ),
                (
                    2,
                    "0000.0001.0002".to_string(),
                    "[\"Outline\",\"Parent A\",\"Child A2\"]".to_string()
                ),
                (
                    1,
                    "0000.0002".to_string(),
                    "[\"Outline\",\"Parent B\"]".to_string()
                ),
                (
                    2,
                    "0000.0002.0001".to_string(),
                    "[\"Outline\",\"Parent B\",\"Child B1\"]".to_string()
                ),
            ]
        );
        assert_eq!(second_outline, first_outline);
        assert_eq!(heading_count, 7);
        assert_eq!(outline_count, 7);
        assert_materialized_paths_are_four_digits(&first_outline);
    }

    #[test]
    fn rebuild_works_when_fts_is_disabled() {
        let test_dir = TestDir::new("fts-disabled");
        let org_path = test_dir.path().join("notes.org");
        let config_path = test_dir.path().join("config.toml");
        let db_path = test_dir.path().join("db.sqlite");

        write_file(&org_path, "* Heading\n");
        write_config(
            &config_path,
            r#"
db_path = "db.sqlite"
files = ["notes.org"]

[search]
fts5_enabled = false
index_body_text = false
"#,
        );

        Indexer::new(OrgizeAdapter::new())
            .rebuild_from_config_path(&config_path)
            .expect("rebuild should succeed");

        let connection = Connection::open(&db_path).expect("db should open");
        let fts_table_count: i64 = connection
            .query_row(
                "SELECT COUNT(*) FROM sqlite_master WHERE type = 'table' AND name = 'heading_fts'",
                [],
                |row| row.get(0),
            )
            .expect("fts table count should load");
        assert_eq!(fts_table_count, 0);
    }

    #[test]
    fn rebuild_populates_fts_rows_when_supported_and_enabled() {
        let probe = Connection::open_in_memory().expect("probe should open");
        if !sqlite_supports_fts5(&probe).expect("fts5 probe should run") {
            return;
        }

        let test_dir = TestDir::new("fts-enabled");
        let org_path = test_dir.path().join("notes.org");
        let config_path = test_dir.path().join("config.toml");
        let db_path = test_dir.path().join("db.sqlite");

        write_file(
            &org_path,
            "* TODO Searchable Heading\nBody phrase for full text search.\n",
        );
        write_config(
            &config_path,
            r#"
db_path = "db.sqlite"
files = ["notes.org"]

[search]
fts5_enabled = true
index_body_text = true
"#,
        );

        Indexer::new(OrgizeAdapter::new())
            .rebuild_from_config_path(&config_path)
            .expect("rebuild should succeed");

        let connection = Connection::open(&db_path).expect("db should open");
        let row_count: i64 = connection
            .query_row("SELECT COUNT(*) FROM heading_fts", [], |row| row.get(0))
            .expect("fts rows should load");
        let indexed_rowid: i64 = connection
            .query_row(
                "SELECT heading_fts.rowid
                 FROM heading_fts
                 INNER JOIN headings ON headings.id = heading_fts.rowid
                 WHERE headings.level > 0",
                [],
                |row| row.get(0),
            )
            .expect("fts rowid should load");
        let match_count: i64 = connection
            .query_row(
                "SELECT COUNT(*) FROM heading_fts WHERE heading_fts MATCH 'Searchable'",
                [],
                |row| row.get(0),
            )
            .expect("fts match should load");
        let body_match_count: i64 = connection
            .query_row(
                "SELECT COUNT(*) FROM heading_fts WHERE heading_fts MATCH 'phrase'",
                [],
                |row| row.get(0),
            )
            .expect("fts body match should load");
        let root_match_count: i64 = connection
            .query_row(
                "SELECT COUNT(*) FROM heading_fts WHERE heading_fts MATCH 'notes'",
                [],
                |row| row.get(0),
            )
            .expect("root title match should load");
        let stored_payload: (Option<String>, Option<String>) = connection
            .query_row(
                "SELECT title, body FROM heading_fts WHERE rowid = ?1",
                [indexed_rowid],
                |row| Ok((row.get(0)?, row.get(1)?)),
            )
            .expect("contentless payload should load as null");

        assert_eq!(row_count, 2);
        assert_eq!(match_count, 1);
        assert_eq!(body_match_count, 1);
        assert_eq!(root_match_count, 1);
        assert_eq!(stored_payload, (None, None));
    }

    #[test]
    fn rebuild_populates_title_only_fts_rows_when_body_indexing_is_disabled() {
        let probe = Connection::open_in_memory().expect("probe should open");
        if !sqlite_supports_fts5(&probe).expect("fts5 probe should run") {
            return;
        }

        let test_dir = TestDir::new("fts-title-only");
        let org_path = test_dir.path().join("notes.org");
        let config_path = test_dir.path().join("config.toml");
        let db_path = test_dir.path().join("db.sqlite");

        write_file(
            &org_path,
            "* Searchable Heading\nBody phrase for full text search.\n",
        );
        write_config(
            &config_path,
            r#"
db_path = "db.sqlite"
files = ["notes.org"]

[search]
fts5_enabled = true
index_body_text = false
"#,
        );

        Indexer::new(OrgizeAdapter::new())
            .rebuild_from_config_path(&config_path)
            .expect("rebuild should succeed");

        let connection = Connection::open(&db_path).expect("db should open");
        let title_match_count: i64 = connection
            .query_row(
                "SELECT COUNT(*) FROM heading_fts WHERE heading_fts MATCH 'Searchable'",
                [],
                |row| row.get(0),
            )
            .expect("fts title match should load");
        let body_match_count: i64 = connection
            .query_row(
                "SELECT COUNT(*) FROM heading_fts WHERE heading_fts MATCH 'phrase'",
                [],
                |row| row.get(0),
            )
            .expect("fts body match should load");

        assert_eq!(title_match_count, 1);
        assert_eq!(body_match_count, 0);
    }

    #[test]
    fn rebuild_recreates_fts_rows_from_canonical_data_without_stale_matches() {
        let probe = Connection::open_in_memory().expect("probe should open");
        if !sqlite_supports_fts5(&probe).expect("fts5 probe should run") {
            return;
        }

        let test_dir = TestDir::new("fts-rebuild-recreate");
        let org_path = test_dir.path().join("notes.org");
        let db_path = test_dir.path().join("db.sqlite");
        let config = Config {
            db_path: db_path.clone(),
            files: vec![org_path.clone()],
            dirs: Vec::new(),
            links: Default::default(),
            todo: Default::default(),
            search: crate::config::SearchConfig {
                fts5_enabled: true,
                index_body_text: true,
            },
            query: Default::default(),
            discovery: Default::default(),
        };
        let mut connection = crate::db::open_database_with_schema(
            &db_path,
            &crate::db::SchemaDefinition::new(crate::db::CURRENT_SCHEMA_VERSION, true),
        )
        .expect("db should open");

        write_file(&org_path, "* First Heading\nAlpha phrase.\n");
        Indexer::new(OrgizeAdapter::new())
            .rebuild(&mut connection, &config)
            .expect("first rebuild should succeed");
        assert_eq!(
            connection
                .query_row(
                    "SELECT COUNT(*) FROM heading_fts WHERE heading_fts MATCH 'Alpha'",
                    [],
                    |row| row.get::<_, i64>(0)
                )
                .expect("alpha match should load"),
            1
        );

        write_file(&org_path, "* Second Heading\nBeta phrase.\n");
        Indexer::new(OrgizeAdapter::new())
            .rebuild(&mut connection, &config)
            .expect("second rebuild should succeed");

        let row_count: i64 = connection
            .query_row("SELECT COUNT(*) FROM heading_fts", [], |row| row.get(0))
            .expect("fts rows should load");
        let alpha_match_count: i64 = connection
            .query_row(
                "SELECT COUNT(*) FROM heading_fts WHERE heading_fts MATCH 'Alpha'",
                [],
                |row| row.get(0),
            )
            .expect("alpha match should load");
        let beta_match_count: i64 = connection
            .query_row(
                "SELECT COUNT(*) FROM heading_fts WHERE heading_fts MATCH 'Beta'",
                [],
                |row| row.get(0),
            )
            .expect("beta match should load");

        assert_eq!(row_count, 2);
        assert_eq!(alpha_match_count, 0);
        assert_eq!(beta_match_count, 1);
    }

    #[test]
    fn empty_rebuild_with_fts_enabled_recreates_empty_heading_fts_state() {
        let probe = Connection::open_in_memory().expect("probe should open");
        if !sqlite_supports_fts5(&probe).expect("fts5 probe should run") {
            return;
        }

        let mut connection = crate::db::open_in_memory_database_with_schema(
            &crate::db::SchemaDefinition::new(crate::db::CURRENT_SCHEMA_VERSION, true),
        )
        .expect("db should open");
        connection
            .execute(
                "INSERT INTO heading_fts (rowid, title, body) VALUES (?1, ?2, ?3)",
                (1_i64, "Stale Heading", "obsoletetoken"),
            )
            .expect("stale fts row should insert");
        DbWriter::set_metadata_flag(&connection, DB_METADATA_BODY_TEXT_AVAILABLE_KEY, false)
            .expect("metadata flag should persist");

        let config = Config {
            db_path: PathBuf::from("db.sqlite"),
            files: Vec::new(),
            dirs: Vec::new(),
            links: Default::default(),
            todo: Default::default(),
            search: crate::config::SearchConfig {
                fts5_enabled: true,
                index_body_text: true,
            },
            query: Default::default(),
            discovery: Default::default(),
        };

        let report = Indexer::new(OrgizeAdapter::new())
            .rebuild_with_options(&mut connection, &config, false)
            .expect("empty rebuild should succeed");

        let heading_fts_exists: i64 = connection
            .query_row(
                "SELECT COUNT(*) FROM sqlite_master WHERE type = 'table' AND name = 'heading_fts'",
                [],
                |row| row.get(0),
            )
            .expect("fts table existence should load");
        let row_count: i64 = connection
            .query_row("SELECT COUNT(*) FROM heading_fts", [], |row| row.get(0))
            .expect("fts row count should load");
        let stale_match_count: i64 = connection
            .query_row(
                "SELECT COUNT(*) FROM heading_fts WHERE heading_fts MATCH 'obsoletetoken'",
                [],
                |row| row.get(0),
            )
            .expect("stale match count should load");
        let body_text_available: String = connection
            .query_row(
                "SELECT value FROM db_metadata WHERE key = ?1",
                [DB_METADATA_BODY_TEXT_AVAILABLE_KEY],
                |row| row.get(0),
            )
            .expect("body text metadata should load");

        assert!(report.indexed_files.is_empty());
        assert!(report.diagnostics.is_empty());
        assert_eq!(heading_fts_exists, 1);
        assert_eq!(row_count, 0);
        assert_eq!(stale_match_count, 0);
        assert_eq!(body_text_available, "1");
    }

    #[test]
    fn rebuild_stores_heading_bodies_only_when_body_indexing_is_enabled() {
        let test_dir = TestDir::new("heading-bodies-enabled");
        let org_path = test_dir.path().join("notes.org");
        let db_path = test_dir.path().join("db.sqlite");
        let config = Config {
            db_path: db_path.clone(),
            files: vec![org_path.clone()],
            dirs: Vec::new(),
            links: Default::default(),
            todo: Default::default(),
            search: crate::config::SearchConfig {
                fts5_enabled: false,
                index_body_text: true,
            },
            query: Default::default(),
            discovery: Default::default(),
        };
        let mut connection = crate::db::open_database_with_schema(
            &db_path,
            &crate::db::SchemaDefinition::new(1, false),
        )
        .expect("db should open");

        write_file(
            &org_path,
            "#+TITLE: Body Text Fixture\nFile-level introduction before the first heading.\n\n* Parent\nParent paragraph one.\n\nParent paragraph two.\n\n** Child\nChild paragraph.\nThis text belongs to Child, not Parent.\n\n*** Grandchild\nGrandchild paragraph.\n\n* Empty Body Parent\n** Child Under Empty Parent\nChild body only.\n\n* Parent With Metadata\nSCHEDULED: <2026-06-23 Tue>\n:PROPERTIES:\n:Owner: Alice\n:END:\n\nBody after planning and property drawer.\n",
        );
        Indexer::new(OrgizeAdapter::new())
            .rebuild(&mut connection, &config)
            .expect("rebuild should succeed");

        let body_text_available: String = connection
            .query_row(
                "SELECT value FROM db_metadata WHERE key = 'body_text_available'",
                [],
                |row| row.get(0),
            )
            .expect("body-text capability should load");
        assert_eq!(body_text_available, "1");

        let level_zero_body: String = connection
            .query_row(
                "SELECT heading_bodies.body_text
                 FROM heading_bodies
                 INNER JOIN headings ON headings.id = heading_bodies.heading_id
                 WHERE headings.level = 0",
                [],
                |row| row.get(0),
            )
            .expect("level 0 body should load");
        assert!(level_zero_body.contains("File-level introduction before the first heading."));

        let parent_body: (String, i64, i64) = connection
            .query_row(
                "SELECT heading_bodies.body_text, heading_bodies.body_byte_start, heading_bodies.body_byte_end
                 FROM heading_bodies
                 INNER JOIN headings ON headings.id = heading_bodies.heading_id
                 WHERE headings.title = 'Parent'",
                [],
                |row| Ok((row.get(0)?, row.get(1)?, row.get(2)?)),
            )
            .expect("parent body should load");
        assert_eq!(
            parent_body.0,
            "Parent paragraph one.\n\nParent paragraph two."
        );
        assert!(parent_body.1 < parent_body.2);

        let child_body: String = connection
            .query_row(
                "SELECT heading_bodies.body_text
                 FROM heading_bodies
                 INNER JOIN headings ON headings.id = heading_bodies.heading_id
                 WHERE headings.title = 'Child'",
                [],
                |row| row.get(0),
            )
            .expect("child body should load");
        assert_eq!(
            child_body,
            "Child paragraph.\nThis text belongs to Child, not Parent."
        );

        let grandchild_body: String = connection
            .query_row(
                "SELECT heading_bodies.body_text
                 FROM heading_bodies
                 INNER JOIN headings ON headings.id = heading_bodies.heading_id
                 WHERE headings.title = 'Grandchild'",
                [],
                |row| row.get(0),
            )
            .expect("grandchild body should load");
        assert_eq!(grandchild_body, "Grandchild paragraph.");

        let empty_parent_rows: i64 = connection
            .query_row(
                "SELECT COUNT(*)
                 FROM heading_bodies
                 INNER JOIN headings ON headings.id = heading_bodies.heading_id
                 WHERE headings.title = 'Empty Body Parent'",
                [],
                |row| row.get(0),
            )
            .expect("empty body parent count should load");
        assert_eq!(empty_parent_rows, 0);

        let child_under_empty_parent_body: String = connection
            .query_row(
                "SELECT heading_bodies.body_text
                 FROM heading_bodies
                 INNER JOIN headings ON headings.id = heading_bodies.heading_id
                 WHERE headings.title = 'Child Under Empty Parent'",
                [],
                |row| row.get(0),
            )
            .expect("child under empty parent body should load");
        assert_eq!(child_under_empty_parent_body, "Child body only.");

        let metadata_body: String = connection
            .query_row(
                "SELECT heading_bodies.body_text
                 FROM heading_bodies
                 INNER JOIN headings ON headings.id = heading_bodies.heading_id
                 WHERE headings.title = 'Parent With Metadata'",
                [],
                |row| row.get(0),
            )
            .expect("metadata body should load");
        assert_eq!(metadata_body, "Body after planning and property drawer.");
    }

    #[test]
    fn rebuild_skips_heading_bodies_when_body_indexing_is_disabled() {
        let test_dir = TestDir::new("heading-bodies-disabled");
        let org_path = test_dir.path().join("notes.org");
        let db_path = test_dir.path().join("db.sqlite");
        let config = Config {
            db_path: db_path.clone(),
            files: vec![org_path.clone()],
            dirs: Vec::new(),
            links: Default::default(),
            todo: Default::default(),
            search: crate::config::SearchConfig {
                fts5_enabled: false,
                index_body_text: false,
            },
            query: Default::default(),
            discovery: Default::default(),
        };
        let mut connection = crate::db::open_database_with_schema(
            &db_path,
            &crate::db::SchemaDefinition::new(1, false),
        )
        .expect("db should open");

        write_file(
            &org_path,
            "#+TITLE: Disabled Bodies\n* Parent\nBody that should not be stored.\n",
        );
        Indexer::new(OrgizeAdapter::new())
            .rebuild(&mut connection, &config)
            .expect("rebuild should succeed");

        let body_text_available: String = connection
            .query_row(
                "SELECT value FROM db_metadata WHERE key = 'body_text_available'",
                [],
                |row| row.get(0),
            )
            .expect("body-text capability should load");
        assert_eq!(body_text_available, "0");

        let body_count: i64 = connection
            .query_row("SELECT COUNT(*) FROM heading_bodies", [], |row| row.get(0))
            .expect("heading body count should load");
        assert_eq!(body_count, 0);
    }

    #[test]
    fn rebuild_updates_body_text_capability_when_configuration_changes() {
        let test_dir = TestDir::new("heading-bodies-capability-toggle");
        let org_path = test_dir.path().join("notes.org");
        let db_path = test_dir.path().join("db.sqlite");
        let mut connection = crate::db::open_database_with_schema(
            &db_path,
            &crate::db::SchemaDefinition::new(1, false),
        )
        .expect("db should open");

        write_file(&org_path, "* Parent\nBody that may be stored.\n");

        let enabled = Config {
            db_path: db_path.clone(),
            files: vec![org_path.clone()],
            dirs: Vec::new(),
            links: Default::default(),
            todo: Default::default(),
            search: crate::config::SearchConfig {
                fts5_enabled: false,
                index_body_text: true,
            },
            query: Default::default(),
            discovery: Default::default(),
        };
        Indexer::new(OrgizeAdapter::new())
            .rebuild(&mut connection, &enabled)
            .expect("enabled rebuild should succeed");

        let enabled_value: String = connection
            .query_row(
                "SELECT value FROM db_metadata WHERE key = 'body_text_available'",
                [],
                |row| row.get(0),
            )
            .expect("enabled capability should load");
        assert_eq!(enabled_value, "1");

        let disabled = Config {
            search: crate::config::SearchConfig {
                fts5_enabled: false,
                index_body_text: false,
            },
            ..enabled
        };
        Indexer::new(OrgizeAdapter::new())
            .rebuild(&mut connection, &disabled)
            .expect("disabled rebuild should succeed");

        let disabled_value: String = connection
            .query_row(
                "SELECT value FROM db_metadata WHERE key = 'body_text_available'",
                [],
                |row| row.get(0),
            )
            .expect("disabled capability should load");
        assert_eq!(disabled_value, "0");

        let body_count: i64 = connection
            .query_row("SELECT COUNT(*) FROM heading_bodies", [], |row| row.get(0))
            .expect("heading body count should load");
        assert_eq!(body_count, 0);
    }

    #[test]
    fn rebuild_excludes_structured_metadata_from_stored_heading_bodies() {
        let test_dir = TestDir::new("heading-bodies-structured-metadata");
        let org_path = test_dir.path().join("notes.org");
        let db_path = test_dir.path().join("db.sqlite");
        let config = Config {
            db_path: db_path.clone(),
            files: vec![org_path.clone()],
            dirs: Vec::new(),
            links: Default::default(),
            todo: Default::default(),
            search: crate::config::SearchConfig {
                fts5_enabled: false,
                index_body_text: true,
            },
            query: Default::default(),
            discovery: Default::default(),
        };
        let mut connection = crate::db::open_database_with_schema(
            &db_path,
            &crate::db::SchemaDefinition::new(1, false),
        )
        .expect("db should open");

        write_file(
            &org_path,
            ":PROPERTIES:\n:CATEGORY: Level 0 Category Property\n:END:\n#+TITLE: Body Metadata Fixture\nIntro before heading.\n\n* Task\nSCHEDULED: <2026-06-23 Tue>\n:PROPERTIES:\n:Owner: Bob\n:END:\nReal body text.\n\n#+AUTHOR: Jane Doe\n\nBody after keyword.\n\n** Child\nChild body.\n\n* Invalid Planning\nSCHEDULED: <%%(diary-float t 42)>\nBody after invalid planning.\n",
        );
        Indexer::new(OrgizeAdapter::new())
            .rebuild(&mut connection, &config)
            .expect("rebuild should succeed");

        let level_zero_body: String = connection
            .query_row(
                "SELECT heading_bodies.body_text
                 FROM heading_bodies
                 INNER JOIN headings ON headings.id = heading_bodies.heading_id
                 WHERE headings.level = 0",
                [],
                |row| row.get(0),
            )
            .expect("level 0 body should load");
        assert_eq!(level_zero_body, "Intro before heading.");

        let task_body: (String, Option<i64>, Option<i64>) = connection
            .query_row(
                "SELECT heading_bodies.body_text, heading_bodies.body_byte_start, heading_bodies.body_byte_end
                 FROM heading_bodies
                 INNER JOIN headings ON headings.id = heading_bodies.heading_id
                 WHERE headings.title = 'Task'",
                [],
                |row| Ok((row.get(0)?, row.get(1)?, row.get(2)?)),
            )
            .expect("task body should load");
        assert_eq!(task_body.0, "Real body text.\n\nBody after keyword.");
        assert_eq!(task_body.1, None);
        assert_eq!(task_body.2, None);

        let invalid_planning_body: String = connection
            .query_row(
                "SELECT heading_bodies.body_text
                 FROM heading_bodies
                 INNER JOIN headings ON headings.id = heading_bodies.heading_id
                 WHERE headings.title = 'Invalid Planning'",
                [],
                |row| row.get(0),
            )
            .expect("invalid planning body should load");
        assert_eq!(
            invalid_planning_body,
            "SCHEDULED: <%%(diary-float t 42)>\nBody after invalid planning."
        );
    }

    #[test]
    fn faulty_file_stops_cleanly_with_clear_error() {
        struct FailingParser;

        impl OrgParserCore for FailingParser {
            fn parse_document_core(
                &self,
                path: &Path,
                _content: &str,
                _options: &ParseOptions,
            ) -> Result<ParsedOrgDocument, ParseDiagnostic> {
                if path
                    .file_name()
                    .and_then(|value| value.to_str())
                    .is_some_and(|value| value.contains("bad"))
                {
                    Err(ParseDiagnostic::error("intentional test parse failure")
                        .with_file_path(path))
                } else {
                    Ok(ParsedOrgDocument::new(path))
                }
            }
        }

        let test_dir = TestDir::new("fault");
        let good_path = test_dir.path().join("a-good.org");
        let bad_path = test_dir.path().join("b-bad.org");
        let db_path = test_dir.path().join("db.sqlite");
        let initial_config = Config {
            db_path: db_path.clone(),
            files: vec![good_path.clone()],
            dirs: Vec::new(),
            links: Default::default(),
            todo: Default::default(),
            search: crate::config::SearchConfig {
                fts5_enabled: false,
                index_body_text: false,
            },
            query: Default::default(),
            discovery: Default::default(),
        };
        let mut connection = crate::db::open_database_with_schema(
            &db_path,
            &crate::db::SchemaDefinition::new(1, false),
        )
        .expect("db should open");

        write_file(&good_path, "* Good\n");
        write_file(&bad_path, "* Bad\n");

        Indexer::new(OrgizeAdapter::new())
            .rebuild(&mut connection, &initial_config)
            .expect("initial rebuild should succeed");
        let persisted_context: (String, String) = connection
            .query_row(
                "SELECT
                    (SELECT value FROM db_metadata WHERE key = ?1),
                    (SELECT value FROM db_metadata WHERE key = ?2)",
                [
                    DB_METADATA_INDEXING_SEMANTICS_FINGERPRINT_KEY,
                    DB_METADATA_INDEXING_DISCOVERY_FINGERPRINT_KEY,
                ],
                |row| Ok((row.get(0)?, row.get(1)?)),
            )
            .expect("persisted context should load");
        let context_entry_count: i64 = connection
            .query_row(
                "SELECT COUNT(*) FROM db_metadata WHERE key LIKE 'indexing_%'",
                [],
                |row| row.get(0),
            )
            .expect("all indexing context entries should persist");
        assert_eq!(context_entry_count, 6);
        let config = Config {
            files: vec![good_path.clone(), bad_path.clone()],
            ..initial_config
        };

        let error = Indexer::new(FailingParser)
            .rebuild(&mut connection, &config)
            .expect_err("rebuild should stop on parser failure");

        match error {
            IndexerError::Parse { path, diagnostic } => {
                assert_eq!(path, bad_path);
                assert_eq!(diagnostic.message, "intentional test parse failure");
            }
            other => panic!("unexpected error: {other}"),
        }

        let files_count: i64 = connection
            .query_row("SELECT COUNT(*) FROM files", [], |row| row.get(0))
            .expect("file count should load");
        let headings_count: i64 = connection
            .query_row("SELECT COUNT(*) FROM headings", [], |row| row.get(0))
            .expect("heading count should load");

        assert_eq!(files_count, 1);
        assert_eq!(headings_count, 2);
        let unchanged_context: (String, String) = connection
            .query_row(
                "SELECT
                    (SELECT value FROM db_metadata WHERE key = ?1),
                    (SELECT value FROM db_metadata WHERE key = ?2)",
                [
                    DB_METADATA_INDEXING_SEMANTICS_FINGERPRINT_KEY,
                    DB_METADATA_INDEXING_DISCOVERY_FINGERPRINT_KEY,
                ],
                |row| Ok((row.get(0)?, row.get(1)?)),
            )
            .expect("persisted context should remain readable");
        assert_eq!(unchanged_context, persisted_context);
    }

    #[test]
    fn full_rebuild_replaces_malformed_previous_indexing_context() {
        let test_dir = TestDir::new("malformed-indexing-context-recovery");
        let org_path = test_dir.path().join("notes.org");
        let db_path = test_dir.path().join("db.sqlite");
        let config = Config {
            db_path: db_path.clone(),
            files: vec![org_path.clone()],
            dirs: Vec::new(),
            links: Default::default(),
            todo: Default::default(),
            search: SearchConfig {
                fts5_enabled: false,
                index_body_text: false,
            },
            query: Default::default(),
            discovery: Default::default(),
        };
        let mut connection = crate::db::open_database_with_schema(
            &db_path,
            &crate::db::SchemaDefinition::new(CURRENT_SCHEMA_VERSION, false),
        )
        .expect("database should open");
        write_file(&org_path, "* Original\n");
        let indexer = Indexer::new(OrgizeAdapter::new());
        indexer
            .rebuild(&mut connection, &config)
            .expect("initial rebuild should succeed");
        connection
            .execute(
                "UPDATE db_metadata SET value = CAST(x'0102' AS BLOB) WHERE key = ?1",
                [DB_METADATA_INDEXING_SEMANTICS_FINGERPRINT_KEY],
            )
            .expect("metadata should corrupt");

        write_file(&org_path, "* Rebuilt\n");
        indexer
            .rebuild(&mut connection, &config)
            .expect("full rebuild should recover from malformed metadata");

        let (storage_type, value): (String, String) = connection
            .query_row(
                "SELECT typeof(value), value FROM db_metadata WHERE key = ?1",
                [DB_METADATA_INDEXING_SEMANTICS_FINGERPRINT_KEY],
                |row| Ok((row.get(0)?, row.get(1)?)),
            )
            .expect("replaced metadata should load");
        assert_eq!(storage_type, "text");
        assert_eq!(value.len(), 64);
        assert!(value
            .bytes()
            .all(|byte| byte.is_ascii_digit() || matches!(byte, b'a'..=b'f')));
    }

    #[test]
    fn failed_context_persistence_rolls_back_all_rebuild_state() {
        let test_dir = TestDir::new("indexing-context-persist-rollback");
        let org_path = test_dir.path().join("notes.org");
        let db_path = test_dir.path().join("db.sqlite");
        let config = Config {
            db_path: db_path.clone(),
            files: vec![org_path.clone()],
            dirs: Vec::new(),
            links: Default::default(),
            todo: Default::default(),
            search: SearchConfig {
                fts5_enabled: false,
                index_body_text: false,
            },
            query: Default::default(),
            discovery: Default::default(),
        };
        let mut connection = crate::db::open_database_with_schema(
            &db_path,
            &crate::db::SchemaDefinition::new(CURRENT_SCHEMA_VERSION, false),
        )
        .expect("database should open");
        write_file(
            &org_path,
            "* Original\n:PROPERTIES:\n:OWNER: Alice\n:END:\n[[https://example.com]]\n",
        );
        let indexer = Indexer::new(OrgizeAdapter::new());
        indexer
            .rebuild(&mut connection, &config)
            .expect("initial rebuild should succeed");

        let snapshot = |connection: &Connection| {
            let files: String = connection
                .query_row(
                    "SELECT COALESCE(group_concat(value, '|'), '')
                     FROM (
                        SELECT path || ':' || mtime_ns || ':' || size || ':' || content_hash AS value
                        FROM files ORDER BY id
                     )",
                    [],
                    |row| row.get(0),
                )
                .expect("file snapshot should load");
            let headings: String = connection
                .query_row(
                    "SELECT COALESCE(group_concat(value, '|'), '')
                     FROM (
                        SELECT level || ':' || title || ':' || byte_start || ':' || byte_end AS value
                        FROM headings ORDER BY id
                     )",
                    [],
                    |row| row.get(0),
                )
                .expect("heading snapshot should load");
            let links: String = connection
                .query_row(
                    "SELECT COALESCE(group_concat(value, '|'), '')
                     FROM (
                        SELECT raw || ':' || COALESCE(resolution_status, '') AS value
                        FROM links ORDER BY id
                     )",
                    [],
                    |row| row.get(0),
                )
                .expect("link snapshot should load");
            let outline: String = connection
                .query_row(
                    "SELECT COALESCE(group_concat(value, '|'), '')
                     FROM (
                        SELECT materialized_path || ':' || breadcrumbs_json AS value
                        FROM outline_path ORDER BY heading_id
                     )",
                    [],
                    |row| row.get(0),
                )
                .expect("outline snapshot should load");
            let effective_properties: String = connection
                .query_row(
                    "SELECT COALESCE(group_concat(value, '|'), '')
                     FROM (
                        SELECT headings.byte_start || ':' || effective.key || ':' ||
                               quote(effective.local_value) || ':' ||
                               quote(effective.effective_value) AS value
                        FROM effective_properties AS effective
                        INNER JOIN headings ON headings.id = effective.heading_id
                        ORDER BY headings.byte_start, effective.key
                     )",
                    [],
                    |row| row.get(0),
                )
                .expect("effective-property snapshot should load");
            let effective_tags: String = connection
                .query_row(
                    "SELECT COALESCE(group_concat(value, '|'), '')
                     FROM (
                        SELECT hex(heading_file.identity) || ':' ||
                               hex(effective_file.identity) || ':' ||
                               headings.byte_start || ':' || effective.position || ':' ||
                               quote(effective.tag) AS value
                        FROM effective_tags AS effective
                        INNER JOIN headings ON headings.id = effective.heading_id
                        INNER JOIN files AS heading_file ON heading_file.id = headings.file_id
                        INNER JOIN files AS effective_file ON effective_file.id = effective.file_id
                        ORDER BY headings.byte_start, effective.position
                     )",
                    [],
                    |row| row.get(0),
                )
                .expect("effective-tag snapshot should load");
            let metadata: String = connection
                .query_row(
                    "SELECT COALESCE(group_concat(value, '|'), '')
                     FROM (
                        SELECT key || ':' || value AS value
                        FROM db_metadata
                        WHERE key IN (
                            'body_text_available', 'fts_available', 'fts_body_indexed',
                            'fts_schema_version'
                        ) OR key LIKE 'indexing_%'
                        ORDER BY key
                     )",
                    [],
                    |row| row.get(0),
                )
                .expect("metadata snapshot should load");
            (
                files,
                headings,
                links,
                outline,
                effective_properties,
                effective_tags,
                metadata,
            )
        };
        let before = snapshot(&connection);

        connection
            .execute_batch(
                "CREATE TRIGGER fail_indexing_context_persist
                 BEFORE UPDATE OF value ON db_metadata
                 WHEN NEW.key = 'indexing_derived_search_fingerprint'
                 BEGIN
                    SELECT RAISE(ABORT, 'forced indexing context persistence failure');
                 END;",
            )
            .expect("failure trigger should install");
        write_file(
            &org_path,
            "* Changed\n:PROPERTIES:\n:OWNER: Bob\n:END:\n[[https://example.invalid]]\n",
        );

        let error = indexer
            .rebuild(&mut connection, &config)
            .expect_err("context persistence trigger should abort rebuild");
        assert!(error.to_string().contains("set_metadata_value"));
        assert_eq!(snapshot(&connection), before);
    }

    #[test]
    fn preparation_hashes_and_normalizes_one_stable_file_snapshot() {
        let test_dir = TestDir::new("prepared-file");
        let path = test_dir.path().join("prepared.org");
        write_file(&path, "* Prepared\n");
        let canonical_path = fs::canonicalize(&path).expect("path should canonicalize");
        let config = Config {
            db_path: test_dir.path().join("db.sqlite"),
            files: Vec::new(),
            dirs: Vec::new(),
            discovery: Default::default(),
            links: Default::default(),
            todo: Default::default(),
            search: SearchConfig {
                fts5_enabled: false,
                index_body_text: false,
            },
            query: Default::default(),
        };
        let prepared = Indexer::new(OrgizeAdapter::new())
            .prepare_discovered_file(
                DiscoveredOrgFile {
                    identity: FileIdentity::from_canonical_path(&canonical_path),
                    path: canonical_path.clone(),
                    scan_root: test_dir.path().to_path_buf(),
                },
                &config,
            )
            .expect("preparation should succeed");

        assert_eq!(prepared.path, canonical_path);
        assert_eq!(
            prepared.file_record.content_hash.as_deref(),
            Some("sha256:40e60c2e39f6e79d31d21d605d33d858cf27fb7ed9a706982e9698a34c725075")
        );
        assert_eq!(prepared.file_record.indexed_at, None);
        assert_eq!(prepared.document.headings.len(), 2);
        assert_eq!(prepared.document.headings[1].title, "Prepared");
    }

    #[test]
    fn unchanged_preparations_have_equal_source_derived_fields() {
        let test_dir = TestDir::new("prepared-file-determinism");
        let path = test_dir.path().join("prepared.org");
        write_file(&path, "* Prepared\n");
        let canonical_path = fs::canonicalize(&path).expect("path should canonicalize");
        let config = Config::default();
        let indexer = Indexer::new(OrgizeAdapter::new());
        let discovered = || DiscoveredOrgFile {
            identity: FileIdentity::from_canonical_path(&canonical_path),
            path: canonical_path.clone(),
            scan_root: test_dir.path().to_path_buf(),
        };

        let first = indexer
            .prepare_discovered_file(discovered(), &config)
            .expect("first preparation should succeed");
        let second = indexer
            .prepare_discovered_file(discovered(), &config)
            .expect("second preparation should succeed");
        assert_eq!(first, second);
    }

    #[test]
    fn rebuild_persists_the_prepared_exact_content_hash() {
        let test_dir = TestDir::new("prepared-content-hash-persistence");
        let config_path = test_dir.path().join("config.toml");
        let path = test_dir.path().join("prepared.org");
        write_file(&path, "* Prepared\n");
        write_config(
            &config_path,
            "db_path = \"db.sqlite\"\nfiles = [\"prepared.org\"]\n[search]\nfts5_enabled = false\n",
        );

        Indexer::new(OrgizeAdapter::new())
            .rebuild_from_config_path(&config_path)
            .expect("rebuild should succeed");
        let connection =
            Connection::open(test_dir.path().join("db.sqlite")).expect("database should open");
        let row: (String, Option<i64>) = connection
            .query_row("SELECT content_hash, indexed_at FROM files", [], |row| {
                Ok((row.get(0)?, row.get(1)?))
            })
            .expect("prepared metadata should persist");
        assert_eq!(
            row.0,
            "sha256:40e60c2e39f6e79d31d21d605d33d858cf27fb7ed9a706982e9698a34c725075"
        );
        assert!(row.1.is_some());
    }

    #[test]
    fn preparation_rejects_invalid_utf8_without_opening_a_database_transaction() {
        let test_dir = TestDir::new("prepared-invalid-utf8");
        let path = test_dir.path().join("invalid.org");
        fs::write(&path, b"* Invalid\n\xff").expect("invalid file should write");
        let canonical_path = fs::canonicalize(&path).expect("path should canonicalize");
        let config = Config::default();

        let error = Indexer::new(OrgizeAdapter::new())
            .prepare_discovered_file(
                DiscoveredOrgFile {
                    identity: FileIdentity::from_canonical_path(&canonical_path),
                    path: canonical_path.clone(),
                    scan_root: test_dir.path().to_path_buf(),
                },
                &config,
            )
            .expect_err("invalid UTF-8 should fail preparation");
        assert!(matches!(error, IndexerError::ReadFile { path, .. } if path == canonical_path));
    }

    #[test]
    fn stable_reader_retries_one_unstable_attempt_then_uses_the_second_snapshot() {
        let path = Path::new("/tmp/retry.org");
        let mut reader = ScriptedSnapshotReader {
            metadata: VecDeque::from([
                FileMetadata {
                    mtime_ns: 1,
                    size: 4,
                },
                FileMetadata {
                    mtime_ns: 2,
                    size: 4,
                },
                FileMetadata {
                    mtime_ns: 3,
                    size: 10,
                },
                FileMetadata {
                    mtime_ns: 3,
                    size: 10,
                },
            ]),
            bytes: VecDeque::from([b"old\n".to_vec(), b"* Stable\n".to_vec()]),
        };

        let captured =
            capture_stable_source_with(&mut reader, path).expect("second attempt should win");
        assert_eq!(captured.bytes, b"* Stable\n");
        assert_eq!(captured.snapshot.mtime_ns, 3);
        assert_eq!(captured.snapshot.size, 10);
        assert_eq!(
            captured.snapshot.content_hash,
            "sha256:9bff91c3ea0353d6792c39f79b35052dfd6379d8fa9a83147e04058ac160d998"
        );
    }

    #[test]
    fn stable_reader_rejects_two_unstable_attempts_deterministically() {
        let path = Path::new("/tmp/unstable.org");
        let mut reader = ScriptedSnapshotReader {
            metadata: VecDeque::from([
                FileMetadata {
                    mtime_ns: 1,
                    size: 1,
                },
                FileMetadata {
                    mtime_ns: 2,
                    size: 1,
                },
                FileMetadata {
                    mtime_ns: 3,
                    size: 1,
                },
                FileMetadata {
                    mtime_ns: 4,
                    size: 1,
                },
            ]),
            bytes: VecDeque::from([b"a".to_vec(), b"b".to_vec()]),
        };

        assert!(matches!(
            capture_stable_source_with(&mut reader, path),
            Err(IndexerError::UnstableFileSnapshot { path: error_path }) if error_path == path
        ));
    }

    #[test]
    fn stable_reader_hashes_crlf_bytes_exactly() {
        let path = Path::new("/tmp/crlf.org");
        let mut reader = ScriptedSnapshotReader {
            metadata: VecDeque::from([
                FileMetadata {
                    mtime_ns: 1,
                    size: 11,
                },
                FileMetadata {
                    mtime_ns: 1,
                    size: 11,
                },
            ]),
            bytes: VecDeque::from([b"* Heading\r\n".to_vec()]),
        };

        let captured =
            capture_stable_source_with(&mut reader, path).expect("snapshot should be stable");
        assert_eq!(
            captured.snapshot.content_hash,
            "sha256:5339665855497512213a47b97f835bad7c112b2b344a73efce829edd9a0f075f"
        );
    }

    #[test]
    fn change_planning_is_read_only_and_uses_the_unchanged_fast_path() {
        struct PanicParser;

        impl OrgParserCore for PanicParser {
            fn parse_document_core(
                &self,
                _path: &Path,
                _content: &str,
                _options: &ParseOptions,
            ) -> Result<ParsedOrgDocument, ParseDiagnostic> {
                panic!("unchanged planning must not parse")
            }
        }

        let test_dir = TestDir::new("change-plan-read-only");
        let path = test_dir.path().join("notes.org");
        let config = Config {
            db_path: test_dir.path().join("db.sqlite"),
            files: vec![path.clone()],
            dirs: Vec::new(),
            discovery: Default::default(),
            links: Default::default(),
            todo: Default::default(),
            search: SearchConfig {
                fts5_enabled: false,
                index_body_text: false,
            },
            query: Default::default(),
        };
        write_file(&path, "* Original\n[[https://example.com]]\n");
        let mut connection = crate::db::open_database_with_schema(
            &config.db_path,
            &SchemaDefinition::new(CURRENT_SCHEMA_VERSION, false),
        )
        .expect("database should open");
        Indexer::new(OrgizeAdapter::new())
            .rebuild(&mut connection, &config)
            .expect("initial rebuild should succeed");
        let before_changes = connection.total_changes();
        let before: (i64, i64, i64, String) = connection
            .query_row(
                "SELECT
                    (SELECT COUNT(*) FROM files),
                    (SELECT COUNT(*) FROM headings),
                    (SELECT COUNT(*) FROM links),
                    (SELECT group_concat(key || ':' || value, '|')
                     FROM (SELECT key, value FROM db_metadata ORDER BY key))",
                [],
                |row| Ok((row.get(0)?, row.get(1)?, row.get(2)?, row.get(3)?)),
            )
            .expect("state should load");

        let result = Indexer::new(PanicParser)
            .plan_changes(&connection, &config)
            .expect("planning should succeed");
        let ChangePlanningResult::Ready(plan) = result else {
            panic!("current context should produce an actionable plan");
        };
        assert_eq!(plan.unchanged.len(), 1);
        assert!(plan.metadata_only.is_empty());
        assert!(plan.created.is_empty());
        assert!(plan.modified.is_empty());
        assert!(plan.deleted.is_empty());
        assert!(plan.failed.is_empty());
        assert_eq!(connection.total_changes(), before_changes);
        let after: (i64, i64, i64, String) = connection
            .query_row(
                "SELECT
                    (SELECT COUNT(*) FROM files),
                    (SELECT COUNT(*) FROM headings),
                    (SELECT COUNT(*) FROM links),
                    (SELECT group_concat(key || ':' || value, '|')
                     FROM (SELECT key, value FROM db_metadata ORDER BY key))",
                [],
                |row| Ok((row.get(0)?, row.get(1)?, row.get(2)?, row.get(3)?)),
            )
            .expect("state should load");
        assert_eq!(after, before);
    }

    #[test]
    fn change_planning_handles_context_invalidation_without_unnecessary_parsing() {
        struct PanicParser;

        impl OrgParserCore for PanicParser {
            fn parse_document_core(
                &self,
                _path: &Path,
                _content: &str,
                _options: &ParseOptions,
            ) -> Result<ParsedOrgDocument, ParseDiagnostic> {
                panic!("derived-search-only planning must not parse")
            }
        }

        let test_dir = TestDir::new("change-plan-invalidations");
        let path = test_dir.path().join("notes.org");
        let config = Config {
            db_path: test_dir.path().join("db.sqlite"),
            files: vec![path.clone()],
            dirs: Vec::new(),
            discovery: Default::default(),
            links: Default::default(),
            todo: Default::default(),
            search: SearchConfig {
                fts5_enabled: false,
                index_body_text: false,
            },
            query: Default::default(),
        };
        write_file(&path, "* Original\n");
        let mut connection = crate::db::open_database_with_schema(
            &config.db_path,
            &SchemaDefinition::new(CURRENT_SCHEMA_VERSION, false),
        )
        .expect("database should open");
        Indexer::new(OrgizeAdapter::new())
            .rebuild(&mut connection, &config)
            .expect("initial rebuild should succeed");

        connection
            .execute(
                "UPDATE db_metadata SET value = ?1 WHERE key = ?2",
                [
                    "0000000000000000000000000000000000000000000000000000000000000000",
                    DB_METADATA_INDEXING_DERIVED_SEARCH_FINGERPRINT_KEY,
                ],
            )
            .expect("derived fingerprint should change");
        let result = Indexer::new(PanicParser)
            .plan_changes(&connection, &config)
            .expect("derived-only planning should succeed");
        let ChangePlanningResult::Ready(plan) = result else {
            panic!("derived-only invalidation remains actionable");
        };
        assert_eq!(plan.unchanged.len(), 1);
        assert!(plan.modified.is_empty());
        assert!(plan
            .invalidations
            .contains(IndexInvalidationSet::REBUILD_DERIVED_SEARCH));

        connection
            .execute(
                "UPDATE db_metadata SET value = ?1 WHERE key = ?2",
                [
                    "0000000000000000000000000000000000000000000000000000000000000000",
                    DB_METADATA_INDEXING_SEMANTICS_FINGERPRINT_KEY,
                ],
            )
            .expect("semantic fingerprint should change");
        let result = Indexer::new(OrgizeAdapter::new())
            .plan_changes(&connection, &config)
            .expect("semantic invalidation should plan");
        let ChangePlanningResult::Ready(plan) = result else {
            panic!("semantic invalidation remains actionable");
        };
        assert_eq!(plan.modified.len(), 1);
        assert!(plan.unchanged.is_empty());
        assert_eq!(plan.modified[0].prepared.path, path);
        assert!(plan
            .invalidations
            .contains(IndexInvalidationSet::REPARSE_ALL_FILES));
    }

    #[test]
    fn terminal_context_or_identity_problems_have_no_actionable_change_groups() {
        let test_dir = TestDir::new("change-plan-terminal");
        let path = test_dir.path().join("notes.org");
        let config = Config {
            db_path: test_dir.path().join("db.sqlite"),
            files: vec![path.clone()],
            dirs: Vec::new(),
            discovery: Default::default(),
            links: Default::default(),
            todo: Default::default(),
            search: SearchConfig {
                fts5_enabled: false,
                index_body_text: false,
            },
            query: Default::default(),
        };
        write_file(&path, "* Original\n");
        let mut connection = crate::db::open_database_with_schema(
            &config.db_path,
            &SchemaDefinition::new(CURRENT_SCHEMA_VERSION, false),
        )
        .expect("database should open");
        Indexer::new(OrgizeAdapter::new())
            .rebuild(&mut connection, &config)
            .expect("initial rebuild should succeed");
        connection
            .execute(
                "UPDATE db_metadata SET value = 'invalid' WHERE key = ?1",
                [DB_METADATA_INDEXING_SEMANTICS_VERSION_KEY],
            )
            .expect("context should corrupt");
        assert!(matches!(
            Indexer::new(OrgizeAdapter::new()).plan_changes(&connection, &config),
            Ok(ChangePlanningResult::FullRebuildRequired)
        ));
    }

    #[test]
    fn change_planning_hashes_metadata_only_files_without_parsing() {
        struct PanicParser;

        impl OrgParserCore for PanicParser {
            fn parse_document_core(
                &self,
                _path: &Path,
                _content: &str,
                _options: &ParseOptions,
            ) -> Result<ParsedOrgDocument, ParseDiagnostic> {
                panic!("metadata-only planning must not parse")
            }
        }

        let test_dir = TestDir::new("change-plan-metadata-only");
        let path = test_dir.path().join("notes.org");
        let config = Config {
            db_path: test_dir.path().join("db.sqlite"),
            files: vec![path.clone()],
            dirs: Vec::new(),
            discovery: Default::default(),
            links: Default::default(),
            todo: Default::default(),
            search: SearchConfig {
                fts5_enabled: false,
                index_body_text: false,
            },
            query: Default::default(),
        };
        write_file(&path, "* Original\n");
        let mut connection = crate::db::open_database_with_schema(
            &config.db_path,
            &SchemaDefinition::new(CURRENT_SCHEMA_VERSION, false),
        )
        .expect("database should open");
        Indexer::new(OrgizeAdapter::new())
            .rebuild(&mut connection, &config)
            .expect("initial rebuild should succeed");
        connection
            .execute("UPDATE files SET mtime_ns = 0", [])
            .expect("stored metadata should change");

        let result = Indexer::new(PanicParser)
            .plan_changes(&connection, &config)
            .expect("planning should hash the source");
        let ChangePlanningResult::Ready(plan) = result else {
            panic!("current context should remain actionable");
        };
        assert_eq!(plan.metadata_only.len(), 1);
        assert!(plan.modified.is_empty());
        assert!(plan.metadata_only[0]
            .file_record
            .content_hash
            .as_deref()
            .is_some_and(|hash| hash.starts_with("sha256:")));
    }

    #[test]
    fn change_planning_uses_captured_metadata_after_the_fast_path() {
        struct PanicParser;

        impl OrgParserCore for PanicParser {
            fn parse_document_core(
                &self,
                _path: &Path,
                _content: &str,
                _options: &ParseOptions,
            ) -> Result<ParsedOrgDocument, ParseDiagnostic> {
                panic!("equal captured bytes must remain metadata-only")
            }
        }

        let path = PathBuf::from("/tmp/change-plan-captured-metadata.org");
        let bytes = b"* Same\n".to_vec();
        let hash = format!("sha256:{:x}", Sha256::digest(&bytes));
        let discovered = DiscoveredOrgFile {
            identity: FileIdentity::from_canonical_path(&path),
            path: path.clone(),
            scan_root: PathBuf::from("/tmp"),
        };
        let persisted = PersistedFileSnapshot {
            file_id: 41,
            path: path.display().to_string(),
            identity: Some(discovered.identity.clone()),
            mtime_ns: 10,
            size: i64::try_from(bytes.len()).expect("test size fits"),
            content_hash: Some(hash),
        };
        let mut reader = ScriptedSnapshotReader {
            // The no-read probe matches persisted metadata. The subsequent
            // stable capture observes the later touched snapshot.
            metadata: VecDeque::from([
                FileMetadata {
                    mtime_ns: 10,
                    size: 7,
                },
                FileMetadata {
                    mtime_ns: 11,
                    size: 7,
                },
                FileMetadata {
                    mtime_ns: 11,
                    size: 7,
                },
            ]),
            bytes: VecDeque::from([bytes]),
        };
        let config = Config::default();
        let result = Indexer::new(PanicParser)
            .plan_discovered_file_with_reader(
                discovered,
                Some(persisted),
                &config,
                ChangePlanningOptions {
                    allow_empty: false,
                    verify_hashes: true,
                    accept_source_root_changes: false,
                },
                false,
                &mut reader,
            )
            .expect("capture should classify deterministically");
        let PlannedCurrentFile::MetadataOnly(file) = result else {
            panic!("captured metadata, rather than the fast probe, decides the result");
        };
        assert_eq!(file.existing_file_id, 41);
        assert_eq!(file.file_record.mtime_ns, 11);
    }

    #[test]
    fn change_planning_treats_unusable_stored_values_conservatively() {
        let test_dir = TestDir::new("change-plan-stored-values");
        let path = test_dir.path().join("notes.org");
        let config = Config {
            db_path: test_dir.path().join("db.sqlite"),
            files: vec![path.clone()],
            dirs: Vec::new(),
            discovery: Default::default(),
            links: Default::default(),
            todo: Default::default(),
            search: SearchConfig {
                fts5_enabled: false,
                index_body_text: false,
            },
            query: Default::default(),
        };
        write_file(&path, "* Original\n");
        let mut connection = crate::db::open_database_with_schema(
            &config.db_path,
            &SchemaDefinition::new(CURRENT_SCHEMA_VERSION, false),
        )
        .expect("database should open");
        Indexer::new(OrgizeAdapter::new())
            .rebuild(&mut connection, &config)
            .expect("initial rebuild should succeed");

        connection
            .execute("UPDATE files SET content_hash = X'00'", [])
            .expect("test fixture should store a blob hash");
        let result = Indexer::new(OrgizeAdapter::new())
            .plan_changes(&connection, &config)
            .expect("wrong hash type is non-comparable, not a planner error");
        let ChangePlanningResult::Ready(plan) = result else {
            panic!("context is valid")
        };
        assert_eq!(plan.modified.len(), 1);
        assert!(plan.modified[0]
            .prepared
            .file_record
            .content_hash
            .as_deref()
            .is_some_and(|value| value.starts_with("sha256:")));

        connection
            .execute("UPDATE files SET identity = X'00'", [])
            .expect("test fixture should corrupt identity");
        assert!(matches!(
            Indexer::new(OrgizeAdapter::new()).plan_changes(&connection, &config),
            Ok(ChangePlanningResult::FullRebuildRequired)
        ));
    }

    #[test]
    fn change_planning_reports_failed_sources_without_deleting_them() {
        let test_dir = TestDir::new("change-plan-failed-source");
        let path = test_dir.path().join("notes.org");
        let config = Config {
            db_path: test_dir.path().join("db.sqlite"),
            files: vec![path.clone()],
            dirs: Vec::new(),
            discovery: Default::default(),
            links: Default::default(),
            todo: Default::default(),
            search: SearchConfig {
                fts5_enabled: false,
                index_body_text: false,
            },
            query: Default::default(),
        };
        write_file(&path, "* Original\n");
        let mut connection = crate::db::open_database_with_schema(
            &config.db_path,
            &SchemaDefinition::new(CURRENT_SCHEMA_VERSION, false),
        )
        .expect("database should open");
        Indexer::new(OrgizeAdapter::new())
            .rebuild(&mut connection, &config)
            .expect("initial rebuild should succeed");
        fs::write(&path, b"* Invalid\n\xff").expect("invalid source should write");

        let result = Indexer::new(OrgizeAdapter::new())
            .plan_changes(&connection, &config)
            .expect("per-file failure should not abort planning");
        let ChangePlanningResult::Ready(plan) = result else {
            panic!("file-level failure should remain actionable for other files");
        };
        assert_eq!(plan.failed.len(), 1);
        assert_eq!(
            plan.failed[0].path,
            fs::canonicalize(&path).expect("path should resolve")
        );
        assert!(matches!(
            plan.failed[0].error,
            IndexerError::ReadFile { .. }
        ));
        assert!(plan.deleted.is_empty());
    }

    #[test]
    fn configured_source_reconciliation_uses_the_indexer_plan_and_apply_boundary() {
        let test_dir = TestDir::new("configured-source-reconciliation");
        let path = test_dir.path().join("notes.org");
        let config = Config {
            db_path: test_dir.path().join("db.sqlite"),
            files: vec![path.clone()],
            dirs: Vec::new(),
            discovery: Default::default(),
            links: Default::default(),
            todo: Default::default(),
            search: SearchConfig {
                fts5_enabled: false,
                index_body_text: false,
            },
            query: Default::default(),
        };
        let indexer = Indexer::new(OrgizeAdapter::new());
        write_file(&path, "* Original\n");
        let mut connection = crate::db::open_database_with_schema(
            &config.db_path,
            &SchemaDefinition::new(CURRENT_SCHEMA_VERSION, false),
        )
        .expect("database should open");
        indexer
            .rebuild(&mut connection, &config)
            .expect("initial rebuild");

        write_file(&path, "* Changed\n");
        assert!(matches!(
            indexer.reconcile_configured_sources(&mut connection, &config),
            Ok(ChangeApplicationResult::Applied(_))
        ));
        let title: String = connection
            .query_row("SELECT title FROM headings WHERE level = 1", [], |row| {
                row.get(0)
            })
            .expect("changed heading should exist");
        assert_eq!(title, "Changed");
    }

    #[test]
    fn candidate_reconciliation_updates_only_the_submitted_existing_path() {
        let test_dir = TestDir::new("candidate-reconciliation-modified");
        let first = test_dir.path().join("first.org");
        let second = test_dir.path().join("second.org");
        let config = Config {
            db_path: test_dir.path().join("db.sqlite"),
            files: vec![first.clone(), second.clone()],
            dirs: Vec::new(),
            discovery: Default::default(),
            links: Default::default(),
            todo: Default::default(),
            search: SearchConfig {
                fts5_enabled: false,
                index_body_text: false,
            },
            query: Default::default(),
        };
        let indexer = Indexer::new(OrgizeAdapter::new());
        write_file(&first, "* First\n");
        write_file(&second, "* Second\n");
        let mut connection = crate::db::open_database_with_schema(
            &config.db_path,
            &SchemaDefinition::new(CURRENT_SCHEMA_VERSION, false),
        )
        .expect("database should open");
        indexer
            .rebuild(&mut connection, &config)
            .expect("initial rebuild");

        write_file(&first, "* Changed\n");
        let candidate = fs::canonicalize(&first).expect("candidate should canonicalize");
        let result = indexer
            .reconcile_candidate_paths(&mut connection, &config, [candidate])
            .expect("candidate reconciliation should execute");
        let ChangeApplicationResult::Applied(report) = result else {
            panic!("candidate plan should apply");
        };
        assert_eq!(report.modified, 1);
        assert_eq!(report.created, 0);
        assert_eq!(report.deleted, 0);
        let titles = connection
            .prepare("SELECT title FROM headings WHERE level = 1 ORDER BY title")
            .expect("query should prepare")
            .query_map([], |row| row.get::<_, String>(0))
            .expect("query should execute")
            .collect::<Result<Vec<_>, _>>()
            .expect("titles should read");
        assert_eq!(titles, vec!["Changed", "Second"]);
    }

    #[test]
    fn candidate_reconciliation_matches_a_deleted_path_by_persisted_identity() {
        let test_dir = TestDir::new("candidate-reconciliation-deleted");
        let path = test_dir.path().join("notes.org");
        let config = Config {
            db_path: test_dir.path().join("db.sqlite"),
            files: vec![path.clone()],
            dirs: Vec::new(),
            discovery: Default::default(),
            links: Default::default(),
            todo: Default::default(),
            search: SearchConfig {
                fts5_enabled: false,
                index_body_text: false,
            },
            query: Default::default(),
        };
        let indexer = Indexer::new(OrgizeAdapter::new());
        write_file(&path, "* Original\n");
        let canonical = fs::canonicalize(&path).expect("path should canonicalize before deletion");
        let mut connection = crate::db::open_database_with_schema(
            &config.db_path,
            &SchemaDefinition::new(CURRENT_SCHEMA_VERSION, false),
        )
        .expect("database should open");
        indexer
            .rebuild(&mut connection, &config)
            .expect("initial rebuild");
        fs::remove_file(&path).expect("source should be removed");

        let result = indexer
            .reconcile_candidate_paths(&mut connection, &config, [canonical])
            .expect("missing candidate should reconcile");
        let ChangeApplicationResult::Applied(report) = result else {
            panic!("deletion plan should apply");
        };
        assert_eq!(report.deleted, 1);
        let file_count: i64 = connection
            .query_row("SELECT COUNT(*) FROM files", [], |row| row.get(0))
            .expect("file count should read");
        assert_eq!(file_count, 0);
    }

    #[test]
    fn candidate_reconciliation_deletes_persisted_descendants_of_a_removed_directory() {
        let test_dir = TestDir::new("candidate-reconciliation-removed-directory");
        let notes = test_dir.path().join("notes");
        let removed = notes.join("removed");
        let first = removed.join("first.org");
        let second = removed.join("deeper/second.org");
        let kept = notes.join("kept.org");
        let config = Config {
            db_path: test_dir.path().join("db.sqlite"),
            files: Vec::new(),
            dirs: vec![ConfiguredDir {
                path: notes,
                recursive: true,
                exclude: Vec::new(),
            }],
            discovery: Default::default(),
            links: Default::default(),
            todo: Default::default(),
            search: SearchConfig {
                fts5_enabled: false,
                index_body_text: false,
            },
            query: Default::default(),
        };
        let indexer = Indexer::new(OrgizeAdapter::new());
        write_file(&first, "* First\n");
        write_file(&second, "* Second\n");
        write_file(&kept, "* Kept\n");
        let removed_canonical =
            fs::canonicalize(&removed).expect("directory should canonicalize before deletion");
        let mut connection = crate::db::open_database_with_schema(
            &config.db_path,
            &SchemaDefinition::new(CURRENT_SCHEMA_VERSION, false),
        )
        .expect("database should open");
        indexer
            .rebuild(&mut connection, &config)
            .expect("initial rebuild");
        fs::remove_dir_all(&removed).expect("directory should be removed");

        let result = indexer
            .reconcile_candidate_paths(&mut connection, &config, [removed_canonical])
            .expect("removed directory should reconcile through persisted descendants");
        let ChangeApplicationResult::Applied(report) = result else {
            panic!("directory deletion plan should apply");
        };
        assert_eq!(report.deleted, 2);
        assert_eq!(report.unchanged, 1);
        let paths = connection
            .prepare("SELECT path FROM files ORDER BY path")
            .expect("query should prepare")
            .query_map([], |row| row.get::<_, String>(0))
            .expect("query should execute")
            .collect::<Result<Vec<_>, _>>()
            .expect("paths should read");
        assert_eq!(paths, vec![kept.display().to_string()]);
    }

    #[test]
    fn actionable_plan_applies_a_modified_file_and_rejects_a_stale_one() {
        let test_dir = TestDir::new("actionable-change-plan");
        let path = test_dir.path().join("notes.org");
        let config = Config {
            db_path: test_dir.path().join("db.sqlite"),
            files: vec![path.clone()],
            dirs: Vec::new(),
            discovery: Default::default(),
            links: Default::default(),
            todo: Default::default(),
            search: SearchConfig {
                fts5_enabled: false,
                index_body_text: false,
            },
            query: Default::default(),
        };
        let indexer = Indexer::new(OrgizeAdapter::new());
        write_file(&path, "* Original\n");
        let mut connection = crate::db::open_database_with_schema(
            &config.db_path,
            &SchemaDefinition::new(CURRENT_SCHEMA_VERSION, false),
        )
        .expect("database should open");
        indexer
            .rebuild(&mut connection, &config)
            .expect("initial rebuild");

        write_file(&path, "* Changed\n");
        let actionable = indexer
            .actionable_plan(indexer.plan_changes(&connection, &config).expect("plan"))
            .expect("modified plan should be actionable");
        assert!(matches!(
            indexer.apply_change_plan(&mut connection, &config, actionable),
            Ok(ChangeApplicationResult::Applied(_))
        ));
        let title: String = connection
            .query_row("SELECT title FROM headings WHERE level = 1", [], |row| {
                row.get(0)
            })
            .expect("changed heading should exist");
        assert_eq!(title, "Changed");

        write_file(&path, "* Planned\n");
        let actionable = indexer
            .actionable_plan(indexer.plan_changes(&connection, &config).expect("plan"))
            .expect("modified plan should be actionable");
        write_file(&path, "* Stale\n");
        assert!(matches!(
            indexer.apply_change_plan(&mut connection, &config, actionable),
            Ok(ChangeApplicationResult::Rejected(
                ChangeApplicationRejection::Stale
            ))
        ));
        let title: String = connection
            .query_row("SELECT title FROM headings WHERE level = 1", [], |row| {
                row.get(0)
            })
            .expect("old heading should remain after rejection");
        assert_eq!(title, "Changed");
    }

    #[test]
    fn actionable_plan_rejects_a_changed_database_baseline_and_terminal_results() {
        let test_dir = TestDir::new("actionable-plan-baseline");
        let path = test_dir.path().join("notes.org");
        let config = Config {
            db_path: test_dir.path().join("db.sqlite"),
            files: vec![path.clone()],
            dirs: Vec::new(),
            discovery: Default::default(),
            links: Default::default(),
            todo: Default::default(),
            search: SearchConfig {
                fts5_enabled: false,
                index_body_text: false,
            },
            query: Default::default(),
        };
        let indexer = Indexer::new(OrgizeAdapter::new());
        write_file(&path, "* Original\n");
        let mut connection = crate::db::open_database_with_schema(
            &config.db_path,
            &SchemaDefinition::new(CURRENT_SCHEMA_VERSION, false),
        )
        .expect("database should open");
        indexer
            .rebuild(&mut connection, &config)
            .expect("initial rebuild");
        write_file(&path, "* Changed\n");
        let actionable = indexer
            .actionable_plan(indexer.plan_changes(&connection, &config).expect("plan"))
            .expect("plan should be actionable");
        connection
            .execute("UPDATE files SET indexed_at = indexed_at + 1", [])
            .expect("out-of-band update should succeed");
        // indexed_at is intentionally operational and does not invalidate the
        // source baseline, so change source evidence instead.
        connection
            .execute("UPDATE files SET content_hash = 'sha256:0000000000000000000000000000000000000000000000000000000000000000'", [])
            .expect("out-of-band baseline update should succeed");
        assert!(matches!(
            indexer.apply_change_plan(&mut connection, &config, actionable),
            Ok(ChangeApplicationResult::Rejected(
                ChangeApplicationRejection::Stale
            ))
        ));
        assert!(matches!(
            indexer.actionable_plan(ChangePlanningResult::FullRebuildRequired),
            Err(ChangeApplicationRejection::FullRebuildRequired)
        ));
    }

    #[test]
    fn preparation_failure_preserves_an_existing_database() {
        let test_dir = TestDir::new("prepared-failure-rollback");
        let config_path = test_dir.path().join("config.toml");
        let path = test_dir.path().join("note.org");
        write_file(&path, "* Original\n");
        write_config(
            &config_path,
            "db_path = \"db.sqlite\"\nfiles = [\"note.org\"]\n[search]\nfts5_enabled = false\n",
        );
        let indexer = Indexer::new(OrgizeAdapter::new());
        indexer
            .rebuild_from_config_path(&config_path)
            .expect("initial rebuild should succeed");
        fs::write(&path, b"* Invalid\n\xff").expect("invalid replacement should write");

        assert!(matches!(
            indexer.rebuild_from_config_path(&config_path),
            Err(IndexerError::ReadFile { .. })
        ));
        let connection =
            Connection::open(test_dir.path().join("db.sqlite")).expect("database should open");
        let title: String = connection
            .query_row("SELECT title FROM headings WHERE level = 1", [], |row| {
                row.get(0)
            })
            .expect("previous heading should remain");
        assert_eq!(title, "Original");
    }

    #[test]
    fn first_rebuild_persists_configured_root_evidence_for_an_empty_database() {
        let test_dir = TestDir::new("root-evidence-first-rebuild");
        let root = test_dir.path().join("notes");
        fs::create_dir_all(&root).expect("source root should exist");
        let config = recursive_root_config(&test_dir, vec![root]);
        let indexer = Indexer::new(OrgizeAdapter::new());
        let mut connection = crate::db::open_database_with_schema(
            &config.db_path,
            &SchemaDefinition::new(CURRENT_SCHEMA_VERSION, false),
        )
        .expect("database should open");

        indexer
            .rebuild(&mut connection, &config)
            .expect("first rebuild should initialise root evidence");

        let version: String = connection
            .query_row(
                "SELECT value FROM db_metadata WHERE key = ?1",
                [DB_METADATA_SOURCE_ROOT_EVIDENCE_VERSION_KEY],
                |row| row.get(0),
            )
            .expect("root evidence version should exist");
        let evidence: String = connection
            .query_row(
                "SELECT value FROM db_metadata WHERE key = ?1",
                [DB_METADATA_SOURCE_ROOT_EVIDENCE_KEY],
                |row| row.get(0),
            )
            .expect("root evidence should exist");
        assert_eq!(version, "1");
        assert!(evidence.contains("logical_path"));
    }

    #[test]
    fn unchanged_root_reconciliation_uses_the_normal_unchanged_path() {
        let test_dir = TestDir::new("root-evidence-unchanged");
        let root = test_dir.path().join("notes");
        let note = root.join("note.org");
        write_file(&note, "* Stable\n");
        let config = recursive_root_config(&test_dir, vec![root]);
        let indexer = Indexer::new(OrgizeAdapter::new());
        let mut connection = crate::db::open_database_with_schema(
            &config.db_path,
            &SchemaDefinition::new(CURRENT_SCHEMA_VERSION, false),
        )
        .expect("database should open");
        indexer
            .rebuild(&mut connection, &config)
            .expect("initial rebuild should succeed");

        let result = indexer
            .reconcile_configured_sources(&mut connection, &config)
            .expect("unchanged root should reconcile");
        let ChangeApplicationResult::Applied(report) = result else {
            panic!("unchanged reconciliation should apply");
        };
        assert_eq!(report.unchanged, 1);
        assert_eq!(indexed_titles(&connection), vec!["Stable"]);
    }

    #[test]
    fn reachable_empty_replacement_root_is_rejected_before_deletion() {
        let test_dir = TestDir::new("root-evidence-empty-replacement");
        let root = test_dir.path().join("notes");
        write_file(&root.join("note.org"), "* Original\n");
        let config = recursive_root_config(&test_dir, vec![root.clone()]);
        let indexer = Indexer::new(OrgizeAdapter::new());
        let mut connection = crate::db::open_database_with_schema(
            &config.db_path,
            &SchemaDefinition::new(CURRENT_SCHEMA_VERSION, false),
        )
        .expect("database should open");
        indexer
            .rebuild(&mut connection, &config)
            .expect("initial rebuild should succeed");
        let _previous = replace_directory_root(&root, &[]);

        let error = indexer
            .reconcile_configured_sources(&mut connection, &config)
            .expect_err("automatic reconciliation must reject a replacement root");
        assert!(matches!(&error, IndexerError::SourceRootEvidence(_)));
        assert!(error.to_string().contains("no longer matches"));
        assert_eq!(indexed_titles(&connection), vec!["Original"]);
    }

    #[test]
    fn partially_populated_replacement_root_is_rejected_before_deletion() {
        let test_dir = TestDir::new("root-evidence-partial-replacement");
        let root = test_dir.path().join("notes");
        write_file(&root.join("first.org"), "* First\n");
        write_file(&root.join("second.org"), "* Second\n");
        let config = recursive_root_config(&test_dir, vec![root.clone()]);
        let indexer = Indexer::new(OrgizeAdapter::new());
        let mut connection = crate::db::open_database_with_schema(
            &config.db_path,
            &SchemaDefinition::new(CURRENT_SCHEMA_VERSION, false),
        )
        .expect("database should open");
        indexer
            .rebuild(&mut connection, &config)
            .expect("initial rebuild should succeed");
        let _previous = replace_directory_root(&root, &[("replacement.org", "* Replacement\n")]);

        let error = indexer
            .reconcile_configured_sources(&mut connection, &config)
            .expect_err("partial replacement must be rejected");
        assert!(matches!(error, IndexerError::SourceRootEvidence(_)));
        assert_eq!(indexed_titles(&connection), vec!["First", "Second"]);
    }

    #[test]
    fn same_device_root_inode_replacement_is_rejected() {
        let test_dir = TestDir::new("root-evidence-same-device-replacement");
        let root = test_dir.path().join("notes");
        write_file(&root.join("note.org"), "* Original\n");
        let config = recursive_root_config(&test_dir, vec![root.clone()]);
        let indexer = Indexer::new(OrgizeAdapter::new());
        let mut connection = crate::db::open_database_with_schema(
            &config.db_path,
            &SchemaDefinition::new(CURRENT_SCHEMA_VERSION, false),
        )
        .expect("database should open");
        indexer
            .rebuild(&mut connection, &config)
            .expect("initial rebuild should succeed");
        let original_metadata = fs::metadata(&root).expect("original root metadata should load");
        let _previous = replace_directory_root(&root, &[("new.org", "* New\n")]);
        let replacement_metadata =
            fs::metadata(&root).expect("replacement root metadata should load");
        assert_eq!(original_metadata.dev(), replacement_metadata.dev());
        assert_ne!(original_metadata.ino(), replacement_metadata.ino());

        let error = indexer
            .reconcile_configured_sources(&mut connection, &config)
            .expect_err("same-device inode replacement must be rejected");
        assert!(matches!(error, IndexerError::SourceRootEvidence(_)));
        assert_eq!(indexed_titles(&connection), vec!["Original"]);
    }

    #[test]
    fn one_replaced_root_rejects_a_multi_root_reconciliation() {
        let test_dir = TestDir::new("root-evidence-multiple-roots");
        let first_root = test_dir.path().join("first");
        let second_root = test_dir.path().join("second");
        write_file(&first_root.join("first.org"), "* First\n");
        write_file(&second_root.join("second.org"), "* Second\n");
        let config =
            recursive_root_config(&test_dir, vec![first_root.clone(), second_root.clone()]);
        let indexer = Indexer::new(OrgizeAdapter::new());
        let mut connection = crate::db::open_database_with_schema(
            &config.db_path,
            &SchemaDefinition::new(CURRENT_SCHEMA_VERSION, false),
        )
        .expect("database should open");
        indexer
            .rebuild(&mut connection, &config)
            .expect("initial rebuild should succeed");
        let _previous = replace_directory_root(&second_root, &[("new.org", "* New\n")]);

        let error = indexer
            .reconcile_configured_sources(&mut connection, &config)
            .expect_err("one changed root must reject the complete reconciliation");
        assert!(matches!(error, IndexerError::SourceRootEvidence(_)));
        assert_eq!(indexed_titles(&connection), vec!["First", "Second"]);
    }

    #[test]
    fn root_replacement_during_discovery_is_rejected_as_stale() {
        let test_dir = TestDir::new("root-evidence-during-discovery");
        let root = test_dir.path().join("notes");
        write_file(&root.join("note.org"), "* Original\n");
        let config = recursive_root_config(&test_dir, vec![root.clone()]);
        let indexer = Indexer::new(OrgizeAdapter::new());
        let mut connection = crate::db::open_database_with_schema(
            &config.db_path,
            &SchemaDefinition::new(CURRENT_SCHEMA_VERSION, false),
        )
        .expect("database should open");
        indexer
            .rebuild(&mut connection, &config)
            .expect("initial rebuild should succeed");

        let error = indexer
            .plan_changes_for_scope_with_hook(
                &connection,
                &config,
                ChangePlanningOptions::default(),
                PlanningScope::All,
                || {
                    let _previous = replace_directory_root(&root, &[("new.org", "* New\n")]);
                },
            )
            .expect_err("root replacement during discovery must be rejected");
        assert!(matches!(&error, IndexerError::SourceRootEvidence(_)));
        assert!(error.to_string().contains("stale reconciliation"));
        assert_eq!(indexed_titles(&connection), vec!["Original"]);
    }

    #[test]
    fn root_replacement_before_commit_rolls_back_all_database_changes() {
        let test_dir = TestDir::new("root-evidence-before-commit");
        let root = test_dir.path().join("notes");
        let note = root.join("note.org");
        write_file(&note, "* Original\n");
        let config = recursive_root_config(&test_dir, vec![root.clone()]);
        let indexer = Indexer::new(OrgizeAdapter::new());
        let mut connection = crate::db::open_database_with_schema(
            &config.db_path,
            &SchemaDefinition::new(CURRENT_SCHEMA_VERSION, false),
        )
        .expect("database should open");
        indexer
            .rebuild(&mut connection, &config)
            .expect("initial rebuild should succeed");
        let metadata_before = metadata_rows(&connection);
        write_file(&note, "* Changed\n");
        let actionable = indexer
            .actionable_plan(indexer.plan_changes(&connection, &config).expect("plan"))
            .expect("plan should be actionable");

        let error = indexer
            .apply_change_plan_with_hook(&mut connection, &config, actionable, || {
                let _previous =
                    replace_directory_root(&root, &[("replacement.org", "* Replacement\n")]);
            })
            .expect_err("final root validation must reject the transaction");
        assert!(matches!(error, IndexerError::SourceRootEvidence(_)));
        assert_eq!(indexed_titles(&connection), vec!["Original"]);
        assert_eq!(metadata_rows(&connection), metadata_before);
    }

    #[test]
    fn indexed_database_without_root_evidence_requires_explicit_adoption() {
        let test_dir = TestDir::new("root-evidence-adoption");
        let root = test_dir.path().join("notes");
        write_file(&root.join("note.org"), "* Original\n");
        let config = recursive_root_config(&test_dir, vec![root]);
        let indexer = Indexer::new(OrgizeAdapter::new());
        let mut connection = crate::db::open_database_with_schema(
            &config.db_path,
            &SchemaDefinition::new(CURRENT_SCHEMA_VERSION, false),
        )
        .expect("database should open");
        indexer
            .rebuild(&mut connection, &config)
            .expect("initial rebuild should succeed");
        connection
            .execute(
                "DELETE FROM db_metadata WHERE key = ?1",
                [DB_METADATA_SOURCE_ROOT_EVIDENCE_VERSION_KEY],
            )
            .expect("version evidence should delete");
        connection
            .execute(
                "DELETE FROM db_metadata WHERE key = ?1",
                [DB_METADATA_SOURCE_ROOT_EVIDENCE_KEY],
            )
            .expect("root evidence should delete");

        let error = indexer
            .rebuild(&mut connection, &config)
            .expect_err("legacy indexed state must require adoption");
        assert!(matches!(error, IndexerError::SourceRootEvidence(_)));
        indexer
            .rebuild_with_rebuild_options(
                &mut connection,
                &config,
                RebuildOptions {
                    allow_empty: false,
                    accept_source_root_changes: true,
                },
            )
            .expect("explicit adoption should succeed");
        let version: String = connection
            .query_row(
                "SELECT value FROM db_metadata WHERE key = ?1",
                [DB_METADATA_SOURCE_ROOT_EVIDENCE_VERSION_KEY],
                |row| row.get(0),
            )
            .expect("adopted evidence should exist");
        assert_eq!(version, "1");
    }

    #[test]
    fn invalid_committed_root_evidence_requires_and_allows_explicit_adoption() {
        let test_dir = TestDir::new("root-evidence-invalid-adoption");
        let root = test_dir.path().join("notes");
        write_file(&root.join("note.org"), "* Original\n");
        let config = recursive_root_config(&test_dir, vec![root]);
        let indexer = Indexer::new(OrgizeAdapter::new());
        let mut connection = crate::db::open_database_with_schema(
            &config.db_path,
            &SchemaDefinition::new(CURRENT_SCHEMA_VERSION, false),
        )
        .expect("database should open");
        indexer
            .rebuild(&mut connection, &config)
            .expect("initial rebuild should succeed");
        connection
            .execute(
                "UPDATE db_metadata SET value = CAST(x'0102' AS BLOB) WHERE key = ?1",
                [DB_METADATA_SOURCE_ROOT_EVIDENCE_KEY],
            )
            .expect("invalid evidence should store");

        let error = indexer
            .rebuild(&mut connection, &config)
            .expect_err("invalid committed evidence must require adoption");
        assert!(matches!(error, IndexerError::SourceRootEvidence(_)));
        indexer
            .rebuild_with_rebuild_options(
                &mut connection,
                &config,
                RebuildOptions {
                    allow_empty: false,
                    accept_source_root_changes: true,
                },
            )
            .expect("manual adoption should replace invalid evidence");
        assert_eq!(indexed_titles(&connection), vec!["Original"]);
    }

    #[test]
    fn explicit_acceptance_commits_replacement_root_evidence() {
        let test_dir = TestDir::new("root-evidence-accept-replacement");
        let root = test_dir.path().join("notes");
        write_file(&root.join("old.org"), "* Old\n");
        let config = recursive_root_config(&test_dir, vec![root.clone()]);
        let indexer = Indexer::new(OrgizeAdapter::new());
        let mut connection = crate::db::open_database_with_schema(
            &config.db_path,
            &SchemaDefinition::new(CURRENT_SCHEMA_VERSION, false),
        )
        .expect("database should open");
        indexer
            .rebuild(&mut connection, &config)
            .expect("initial rebuild should succeed");
        let _previous = replace_directory_root(&root, &[("new.org", "* New\n")]);

        indexer
            .rebuild_with_rebuild_options(
                &mut connection,
                &config,
                RebuildOptions {
                    allow_empty: false,
                    accept_source_root_changes: true,
                },
            )
            .expect("manual acceptance should rebuild the replacement root");
        assert_eq!(indexed_titles(&connection), vec!["New"]);
        indexer
            .rebuild(&mut connection, &config)
            .expect("accepted root should become the committed identity");
    }

    #[test]
    fn accepting_root_changes_does_not_imply_allow_empty() {
        let test_dir = TestDir::new("root-evidence-accept-empty");
        let root = test_dir.path().join("notes");
        write_file(&root.join("old.org"), "* Old\n");
        let config = recursive_root_config(&test_dir, vec![root.clone()]);
        let indexer = Indexer::new(OrgizeAdapter::new());
        let mut connection = crate::db::open_database_with_schema(
            &config.db_path,
            &SchemaDefinition::new(CURRENT_SCHEMA_VERSION, false),
        )
        .expect("database should open");
        indexer
            .rebuild(&mut connection, &config)
            .expect("initial rebuild should succeed");
        let _previous = replace_directory_root(&root, &[]);

        let error = indexer
            .rebuild_with_rebuild_options(
                &mut connection,
                &config,
                RebuildOptions {
                    allow_empty: false,
                    accept_source_root_changes: true,
                },
            )
            .expect_err("root acceptance must not bypass zero-input safety");
        assert!(matches!(error, IndexerError::RefusedEmptyRebuild { .. }));
        assert_eq!(indexed_titles(&connection), vec!["Old"]);
    }

    #[test]
    fn normal_file_deletion_inside_an_unchanged_root_still_reconciles() {
        let test_dir = TestDir::new("root-evidence-normal-deletion");
        let root = test_dir.path().join("notes");
        let first = root.join("first.org");
        let second = root.join("second.org");
        write_file(&first, "* First\n");
        write_file(&second, "* Second\n");
        let config = recursive_root_config(&test_dir, vec![root]);
        let indexer = Indexer::new(OrgizeAdapter::new());
        let mut connection = crate::db::open_database_with_schema(
            &config.db_path,
            &SchemaDefinition::new(CURRENT_SCHEMA_VERSION, false),
        )
        .expect("database should open");
        indexer
            .rebuild(&mut connection, &config)
            .expect("initial rebuild should succeed");
        fs::remove_file(&second).expect("second file should be removed");

        let result = indexer
            .reconcile_configured_sources(&mut connection, &config)
            .expect("ordinary deletion should reconcile");
        let ChangeApplicationResult::Applied(report) = result else {
            panic!("ordinary deletion should apply");
        };
        assert_eq!(report.deleted, 1);
        assert_eq!(indexed_titles(&connection), vec!["First"]);
    }

    fn query_rows<T, F>(connection: &Connection, sql: &str, mut map: F) -> Vec<T>
    where
        F: FnMut(&rusqlite::Row<'_>) -> rusqlite::Result<T>,
    {
        let mut statement = connection.prepare(sql).expect("statement should prepare");
        let rows = statement
            .query_map([], |row| map(row))
            .expect("query should run");
        rows.collect::<Result<Vec<_>, _>>()
            .expect("rows should collect")
    }

    fn assert_materialized_paths_are_four_digits(rows: &[(i64, String, String)]) {
        for (_, path, _) in rows {
            for segment in path.split('.') {
                assert_eq!(
                    segment.len(),
                    4,
                    "outline path segment should be exactly 4 digits: {path}"
                );
            }
        }
    }
}
