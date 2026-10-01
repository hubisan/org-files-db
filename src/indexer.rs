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
        advance_index_generation, open_database_with_schema, sqlite_supports_fts5, AffectedFile,
        DbError, DbWriteError, DbWriter, EffectivePropertyRecord, EffectiveTagRecord,
        FileRecordInput, HeadingBodyRecord, HeadingRecord, IndexGenerationChange, KeywordRecord,
        LinkRecord, OutlinePathRecord, PropertyRecord, SchemaDefinition, TagRecord,
        TimestampRecord, TimestampRepeaterRecord, TodoKeywordRecord, CURRENT_SCHEMA_VERSION,
        DB_METADATA_BODY_TEXT_AVAILABLE_KEY, DB_METADATA_FTS_AVAILABLE_KEY,
        DB_METADATA_FTS_BODY_INDEXED_KEY, DB_METADATA_FTS_SCHEMA_VERSION_KEY,
        FTS_SCHEMA_CONTRACT_VERSION,
    },
    exclusions::ExclusionMatcher,
    file_identity::{display_path, FileIdentity},
    hex_encoding::encode_lower,
    indexing_context::{IndexInvalidationSet, IndexingContext, IndexingContextComparison},
    link_resolver::LinkResolver,
    link_resolver::{IndexedUniverse, ResolutionScope},
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

mod candidate;
mod discovery;
mod plan;
mod rows;
mod snapshot;
#[cfg(test)]
mod tests;

pub(crate) use self::candidate::{CandidatePathNormalizer, CandidatePathResolution};
use self::discovery::*;
pub(crate) use self::plan::*;
use self::rows::*;
use self::snapshot::*;

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
            planning_context: context,
            fts_backend_available,
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
            let failed_path = discovered.path.clone();
            let failed_identity = discovered.identity.clone();
            match self.plan_discovered_file(discovered, persisted, config, options, reparse_all) {
                Ok(PlannedCurrentFile::Unchanged(file)) => plan.unchanged.push(file),
                Ok(PlannedCurrentFile::MetadataOnly(file)) => plan.metadata_only.push(file),
                Ok(PlannedCurrentFile::Created(file)) => plan.created.push(file),
                Ok(PlannedCurrentFile::Modified(file)) => plan.modified.push(file),
                Err(error) => plan.failed.push(FailedChange {
                    path: failed_path,
                    identity: failed_identity,
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
    pub(crate) fn actionable_plan(
        &self,
        result: ChangePlanningResult,
    ) -> Result<ActionableChangePlan, ChangeApplicationRejection> {
        match result {
            ChangePlanningResult::FullRebuildRequired => {
                Err(ChangeApplicationRejection::FullRebuildRequired)
            }
            ChangePlanningResult::Ready(plan) => ActionableChangePlan::try_from(*plan),
        }
    }

    /// Applies one single-use actionable plan as one immediate SQLite transaction.
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
        let ActionableChangePlan { plan } = actionable;
        let should_optimize_planner_statistics = plan.has_index_changes();
        if let Some(expected) = plan.source_root_evidence.as_ref() {
            let current = SourceRootEvidenceSet::capture(config)
                .map_err(|source| IndexerError::SourceRootEvidence(Box::new(source)))?;
            expected
                .ensure_unchanged(&current)
                .map_err(|source| IndexerError::SourceRootEvidence(Box::new(source)))?;
        }
        // Revalidate every current source before a write transaction. This also
        // rejects plans whose prepared evidence is no longer current. Unchanged
        // files were accepted by the planner on modification time and size, so
        // they are revalidated on the same metadata without reading content.
        // Files whose content evidence drives a mutation keep the full hash check.
        for file in &plan.unchanged {
            if !metadata_matches_record(&file.path, &file.file_record)? {
                return Ok(ChangeApplicationResult::Rejected(
                    ChangeApplicationRejection::Stale,
                ));
            }
        }
        for file in &plan.metadata_only {
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
        let mut affected_file_ids = BTreeSet::new();
        for file in &plan.metadata_only {
            update_existing_file_metadata(&tx, file)?;
            affected_file_ids.insert(file.existing_file_id);
        }
        for file in &plan.modified {
            affected_file_ids.insert(replace_prepared_file(
                &tx,
                file.existing_file_id,
                &file.prepared,
                config.search.index_body_text,
            )?);
        }
        for file in &plan.created {
            affected_file_ids.insert(replace_prepared_file(
                &tx,
                None,
                &file.prepared,
                config.search.index_body_text,
            )?);
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
        // Index invalidations can change how every link resolves, so they take the full pass.
        let resolution_report = if plan.invalidations.is_empty() {
            LinkResolver::resolve_scoped(
                &tx,
                &rediscovery.indexed_universe,
                &ResolutionScope { affected_file_ids },
            )
        } else {
            LinkResolver::resolve_all(&tx, &rediscovery.indexed_universe)
        }
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
        let generation_change =
            generation_change_for_plan(&plan, &resolution_report.changed_source_paths);
        if !generation_change.is_empty() {
            advance_index_generation(
                &tx,
                &generation_change,
                config.index.journal_retention_generations,
            )
            .map_err(IndexerError::Write)?;
        }
        tx.commit()
            .map_err(|source| IndexerError::Write(DbWriteError::Transaction { source }))?;
        if should_optimize_planner_statistics {
            optimize_planner_statistics(connection);
        }
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
        let path_changed = persisted
            .as_ref()
            .is_some_and(|file| file.path != display_path(&discovered.path));
        let hash_comparable = persisted
            .as_ref()
            .and_then(|file| qualified_sha256_hash(file.content_hash.as_deref()))
            .is_some();
        if let Some(persisted) = persisted.as_ref() {
            if fast_snapshot_matches && hash_comparable && !options.verify_hashes && !reparse_all {
                let file = PlannedFile::from_persisted(&discovered, persisted);
                return Ok(if path_changed {
                    PlannedCurrentFile::MetadataOnly(file)
                } else {
                    PlannedCurrentFile::Unchanged(file)
                });
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
                return Ok(if captured_snapshot_matches && !path_changed {
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
        let mut skipped_diagnostics = Vec::new();
        let _missing_explicit_files = discovery.missing_explicit_files;
        for discovered in discovery.files {
            let path = discovered.path.clone();
            match self.prepare_discovered_file(discovered, config) {
                Ok(prepared) => pending.push(prepared),
                Err(error) => skipped_diagnostics.push(IndexDiagnostic {
                    severity: DiagnosticSeverity::Warning,
                    message: format!("skipped: {}", skip_cause(&error)),
                    file_path: Some(path),
                    line_number: None,
                    byte_range: None,
                }),
            }
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

        let mut report = RebuildReport {
            diagnostics: skipped_diagnostics,
            ..RebuildReport::default()
        };
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

        let _resolution_report = LinkResolver::resolve_all(&tx, &discovery.indexed_universe)
            .map_err(IndexerError::Write)?;
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
        advance_index_generation(
            &tx,
            &IndexGenerationChange::full_invalidation(),
            config.index.journal_retention_generations,
        )
        .map_err(IndexerError::Write)?;

        tx.commit()
            .map_err(|source| IndexerError::Write(DbWriteError::Transaction { source }))?;
        optimize_planner_statistics(connection);

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

fn optimize_planner_statistics(connection: &Connection) {
    // The indexing transaction is already committed. Planner maintenance is
    // best-effort so that a maintenance failure cannot report a committed
    // index update as failed.
    let _maintenance_result = connection.execute_batch("PRAGMA optimize;");
}

fn generation_change_for_plan(
    plan: &ChangePlan,
    resolution_changed_source_paths: &BTreeSet<String>,
) -> IndexGenerationChange {
    if !plan.invalidations.is_empty() {
        return IndexGenerationChange::full_invalidation();
    }

    let previous_paths = plan
        .expected_files
        .iter()
        .map(|file| (file.file_id, file.path.as_str()))
        .collect::<BTreeMap<_, _>>();
    let mut affected = Vec::new();

    for file in &plan.metadata_only {
        add_upsert_with_previous_path(
            &mut affected,
            previous_paths.get(&file.existing_file_id).copied(),
            &file.path,
        );
    }
    for file in &plan.modified {
        add_upsert_with_previous_path(
            &mut affected,
            file.existing_file_id
                .and_then(|file_id| previous_paths.get(&file_id).copied()),
            &file.prepared.path,
        );
    }
    for file in &plan.created {
        affected.push(AffectedFile::upsert(display_path(&file.prepared.path)));
    }
    for file in &plan.deleted {
        affected.push(AffectedFile::delete(file.path.clone()));
    }
    affected.extend(
        resolution_changed_source_paths
            .iter()
            .cloned()
            .map(AffectedFile::upsert),
    );

    IndexGenerationChange::from_files(affected)
}

fn add_upsert_with_previous_path(
    affected: &mut Vec<AffectedFile>,
    previous_path: Option<&str>,
    current_path: &Path,
) {
    let current_path = display_path(current_path);
    if let Some(previous_path) = previous_path {
        if previous_path != current_path {
            affected.push(AffectedFile::delete(previous_path.to_string()));
        }
    }
    affected.push(AffectedFile::upsert(current_path));
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

impl IndexerError {
    /// A single-source failure that a retry can plausibly resolve without the
    /// file content changing: unstable snapshot, or a file that vanished or
    /// was interrupted while being read.
    pub(crate) fn is_transient_source_failure(&self) -> bool {
        match self {
            Self::UnstableFileSnapshot { .. } => true,
            Self::ReadFile { source, .. } => matches!(
                source.kind(),
                io::ErrorKind::NotFound | io::ErrorKind::Interrupted
            ),
            _ => false,
        }
    }

    /// True for failures of a batch that a retry with backoff can resolve:
    /// SQLite BUSY/LOCKED and transient source failures. Discovery, source-root
    /// and every other error stay fatal.
    pub(crate) fn is_transient(&self) -> bool {
        if self.is_transient_source_failure() {
            return true;
        }
        let mut current: Option<&(dyn Error + 'static)> = self.source();
        while let Some(error) = current {
            if let Some(rusqlite::Error::SqliteFailure(failure, _)) =
                error.downcast_ref::<rusqlite::Error>()
            {
                if matches!(
                    failure.code,
                    rusqlite::ErrorCode::DatabaseBusy | rusqlite::ErrorCode::DatabaseLocked
                ) {
                    return true;
                }
            }
            current = error.source();
        }
        false
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

fn update_existing_file_metadata(
    connection: &Connection,
    file: &PlannedFile,
) -> Result<(), IndexerError> {
    let record = file_record_for_write(&file.file_record, &file.path)?;
    let file_id = DbWriter::upsert_file(connection, &record).map_err(IndexerError::Write)?;
    if file_id != file.existing_file_id {
        return Err(IndexerError::Write(DbWriteError::InvalidInput(
            "metadata-only update selected a different file row",
        )));
    }
    Ok(())
}

fn replace_prepared_file(
    connection: &Connection,
    expected_file_id: Option<i64>,
    prepared: &PreparedFile,
    index_body_text: bool,
) -> Result<i64, IndexerError> {
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
    Ok(file_id)
}
