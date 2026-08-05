use std::{
    collections::BTreeMap,
    error::Error,
    fmt,
    path::PathBuf,
    time::{Duration, Instant},
};

use rusqlite::Connection;

use crate::{
    config::Config,
    file_identity::FileIdentity,
    indexer::{
        CandidatePathNormalizer, CandidatePathResolution, ChangeApplicationRejection,
        ChangeApplicationResult, Indexer, IndexerError,
    },
    parser::OrgParserCore,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum WatcherPathEventKind {
    Create,
    Modify,
    Remove,
    Rename,
    Metadata,
    Other,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum WatcherUncertainty {
    Overflow,
    DroppedEvents,
    Rescan,
    Other,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum WatcherInput {
    Paths {
        kind: WatcherPathEventKind,
        paths: Vec<PathBuf>,
    },
    Uncertain(WatcherUncertainty),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum NormalizedWatcherBatch {
    Candidates(Vec<PathBuf>),
    Reconcile,
}

#[derive(Debug)]
pub(crate) struct WatcherBatchNormalizer {
    candidate_paths: CandidatePathNormalizer,
}

impl WatcherBatchNormalizer {
    pub(crate) fn from_config(config: &Config) -> Result<Self, IndexerError> {
        Ok(Self {
            candidate_paths: CandidatePathNormalizer::from_config(config)?,
        })
    }

    pub(crate) fn normalize<I>(&self, inputs: I) -> NormalizedWatcherBatch
    where
        I: IntoIterator<Item = WatcherInput>,
    {
        let mut candidates = BTreeMap::new();

        for input in inputs {
            let (kind, paths) = match input {
                WatcherInput::Paths { kind, paths } => (kind, paths),
                WatcherInput::Uncertain(_) => return NormalizedWatcherBatch::Reconcile,
            };

            if kind == WatcherPathEventKind::Rename && paths.len() != 2 {
                return NormalizedWatcherBatch::Reconcile;
            }

            for path in paths {
                match self.candidate_paths.resolve(&path) {
                    CandidatePathResolution::Candidate(path) => {
                        if !insert_bounded_candidate(&mut candidates, path) {
                            return NormalizedWatcherBatch::Reconcile;
                        }
                    }
                    CandidatePathResolution::Ignore => {}
                    CandidatePathResolution::Reconcile => {
                        return NormalizedWatcherBatch::Reconcile;
                    }
                }
            }
        }

        NormalizedWatcherBatch::Candidates(candidates.into_values().collect())
    }
}

pub(crate) const DEFAULT_WATCHER_DEBOUNCE_INTERVAL: Duration = Duration::from_millis(250);
pub(crate) const MAX_WATCHER_CANDIDATES_PER_BATCH: usize = 1024;

fn insert_bounded_candidate(
    candidates: &mut BTreeMap<FileIdentity, PathBuf>,
    path: PathBuf,
) -> bool {
    candidates.insert(FileIdentity::from_canonical_path(&path), path);
    candidates.len() <= MAX_WATCHER_CANDIDATES_PER_BATCH
}

impl NormalizedWatcherBatch {
    fn is_empty(&self) -> bool {
        matches!(self, Self::Candidates(paths) if paths.is_empty())
    }

    fn merge(self, other: Self) -> Self {
        match (self, other) {
            (Self::Reconcile, _) | (_, Self::Reconcile) => Self::Reconcile,
            (Self::Candidates(left), Self::Candidates(right)) => {
                let mut candidates = BTreeMap::new();
                for path in left.into_iter().chain(right) {
                    if !insert_bounded_candidate(&mut candidates, path) {
                        return Self::Reconcile;
                    }
                }
                Self::Candidates(candidates.into_values().collect())
            }
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum WatcherControllerError {
    InputClosed,
    NoActiveExecution,
}

impl fmt::Display for WatcherControllerError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::InputClosed => write!(f, "watcher controller is no longer accepting input"),
            Self::NoActiveExecution => write!(f, "watcher controller has no active execution"),
        }
    }
}

impl Error for WatcherControllerError {}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum WatcherExecutionStatus {
    Idle,
    Executed,
}

pub(crate) trait WatcherBatchExecutor {
    type Error;

    fn execute(&mut self, batch: NormalizedWatcherBatch) -> Result<(), Self::Error>;
}

#[derive(Debug)]
pub(crate) enum WatcherExecutionError {
    Indexer(IndexerError),
    Rejected(ChangeApplicationRejection),
}

impl fmt::Display for WatcherExecutionError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Indexer(source) => write!(f, "watcher reconciliation failed: {source}"),
            Self::Rejected(ChangeApplicationRejection::FullRebuildRequired) => write!(
                f,
                "watcher reconciliation was rejected because a full rebuild is required"
            ),
            Self::Rejected(ChangeApplicationRejection::FailedSources) => write!(
                f,
                "watcher reconciliation was rejected because one or more sources failed preparation"
            ),
            Self::Rejected(ChangeApplicationRejection::InvalidPlan) => write!(
                f,
                "watcher reconciliation was rejected because the indexing plan was invalid"
            ),
            Self::Rejected(ChangeApplicationRejection::Stale) => write!(
                f,
                "watcher reconciliation was rejected because the indexing plan became stale"
            ),
        }
    }
}

impl Error for WatcherExecutionError {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        match self {
            Self::Indexer(source) => Some(source),
            Self::Rejected(_) => None,
        }
    }
}

pub(crate) struct IndexerWatcherExecutor<'a, P> {
    indexer: &'a Indexer<P>,
    connection: &'a mut Connection,
    config: &'a Config,
}

impl<'a, P> IndexerWatcherExecutor<'a, P> {
    pub(crate) fn new(
        indexer: &'a Indexer<P>,
        connection: &'a mut Connection,
        config: &'a Config,
    ) -> Self {
        Self {
            indexer,
            connection,
            config,
        }
    }
}

impl<P> WatcherBatchExecutor for IndexerWatcherExecutor<'_, P>
where
    P: OrgParserCore,
{
    type Error = WatcherExecutionError;

    fn execute(&mut self, batch: NormalizedWatcherBatch) -> Result<(), Self::Error> {
        let result = match batch {
            NormalizedWatcherBatch::Candidates(paths) => {
                self.indexer
                    .reconcile_candidate_paths(self.connection, self.config, paths)
            }
            NormalizedWatcherBatch::Reconcile => self
                .indexer
                .reconcile_configured_sources(self.connection, self.config),
        }
        .map_err(WatcherExecutionError::Indexer)?;

        match result {
            ChangeApplicationResult::Applied(_) => Ok(()),
            ChangeApplicationResult::Rejected(rejection) => {
                Err(WatcherExecutionError::Rejected(rejection))
            }
        }
    }
}

#[derive(Debug)]
pub(crate) struct WatcherExecutionController {
    normalizer: WatcherBatchNormalizer,
    debounce_interval: Duration,
    pending: Option<NormalizedWatcherBatch>,
    pending_deadline: Option<Instant>,
    active: Option<NormalizedWatcherBatch>,
    accepting_inputs: bool,
}

impl WatcherExecutionController {
    pub(crate) fn from_config(config: &Config) -> Result<Self, IndexerError> {
        Self::with_debounce_interval(config, DEFAULT_WATCHER_DEBOUNCE_INTERVAL)
    }

    pub(crate) fn with_debounce_interval(
        config: &Config,
        debounce_interval: Duration,
    ) -> Result<Self, IndexerError> {
        Ok(Self {
            normalizer: WatcherBatchNormalizer::from_config(config)?,
            debounce_interval,
            pending: None,
            pending_deadline: None,
            active: None,
            accepting_inputs: true,
        })
    }

    #[cfg(test)]
    pub(crate) fn debounce_interval(&self) -> Duration {
        self.debounce_interval
    }

    pub(crate) fn replace_normalizer(&mut self, normalizer: WatcherBatchNormalizer) {
        self.normalizer = normalizer;
    }

    pub(crate) fn push_input(
        &mut self,
        input: WatcherInput,
        now: Instant,
    ) -> Result<(), WatcherControllerError> {
        self.push_inputs([input], now)
    }

    pub(crate) fn push_inputs<I>(
        &mut self,
        inputs: I,
        now: Instant,
    ) -> Result<(), WatcherControllerError>
    where
        I: IntoIterator<Item = WatcherInput>,
    {
        if !self.accepting_inputs {
            return Err(WatcherControllerError::InputClosed);
        }

        let batch = self.normalizer.normalize(inputs);
        if batch.is_empty() {
            return Ok(());
        }

        self.merge_pending(batch);
        self.pending_deadline = Some(now.checked_add(self.debounce_interval).unwrap_or(now));
        Ok(())
    }

    pub(crate) fn next_deadline(&self) -> Option<Instant> {
        self.pending_deadline
    }

    pub(crate) fn start_ready_execution(&mut self, now: Instant) -> Option<NormalizedWatcherBatch> {
        if self.active.is_some() {
            return None;
        }

        let ready = !self.accepting_inputs
            || self
                .pending_deadline
                .is_some_and(|deadline| now >= deadline);
        if !ready {
            return None;
        }

        let batch = self.pending.take()?;
        self.pending_deadline = None;
        self.active = Some(batch.clone());
        Some(batch)
    }

    pub(crate) fn finish_execution_success(&mut self) -> Result<(), WatcherControllerError> {
        self.take_active()?;
        Ok(())
    }

    pub(crate) fn finish_execution_failure(
        &mut self,
        now: Instant,
    ) -> Result<(), WatcherControllerError> {
        let failed = self.take_active()?;
        self.requeue_failed_batch(failed, now);
        Ok(())
    }

    #[cfg(test)]
    pub(crate) fn execute_ready<E>(
        &mut self,
        now: Instant,
        executor: &mut E,
    ) -> Result<WatcherExecutionStatus, E::Error>
    where
        E: WatcherBatchExecutor,
    {
        let Some(batch) = self.start_ready_execution(now) else {
            return Ok(WatcherExecutionStatus::Idle);
        };

        let retry_batch = batch.clone();
        match executor.execute(batch) {
            Ok(()) => {
                self.active = None;
                Ok(WatcherExecutionStatus::Executed)
            }
            Err(error) => {
                self.active = None;
                self.requeue_failed_batch(retry_batch, now);
                Err(error)
            }
        }
    }

    /// Stops accepting new inputs and flushes all pending work without waiting
    /// for the debounce deadline. An active execution finishes first; any batch
    /// accumulated during it is then eligible immediately.
    pub(crate) fn begin_shutdown(&mut self) {
        self.accepting_inputs = false;
    }

    pub(crate) fn is_accepting_inputs(&self) -> bool {
        self.accepting_inputs
    }

    pub(crate) fn is_shutdown_complete(&self) -> bool {
        !self.accepting_inputs && self.pending.is_none() && self.active.is_none()
    }

    fn merge_pending(&mut self, batch: NormalizedWatcherBatch) {
        self.pending = Some(match self.pending.take() {
            Some(pending) => pending.merge(batch),
            None => batch,
        });
    }

    fn requeue_failed_batch(&mut self, batch: NormalizedWatcherBatch, now: Instant) {
        self.merge_pending(batch);
        self.pending_deadline = Some(if self.accepting_inputs {
            now.checked_add(self.debounce_interval).unwrap_or(now)
        } else {
            now
        });
    }

    fn take_active(&mut self) -> Result<NormalizedWatcherBatch, WatcherControllerError> {
        self.active
            .take()
            .ok_or(WatcherControllerError::NoActiveExecution)
    }
}

#[cfg(test)]
mod tests {
    use super::{
        IndexerWatcherExecutor, NormalizedWatcherBatch, WatcherBatchExecutor,
        WatcherBatchNormalizer, WatcherControllerError, WatcherExecutionController,
        WatcherExecutionStatus, WatcherInput, WatcherPathEventKind, WatcherUncertainty,
        DEFAULT_WATCHER_DEBOUNCE_INTERVAL, MAX_WATCHER_CANDIDATES_PER_BATCH,
    };
    use crate::{
        config::Config,
        db::{
            open_database_with_schema, read_index_state, SchemaDefinition, CURRENT_SCHEMA_VERSION,
        },
        indexer::Indexer,
        parser::OrgizeAdapter,
    };
    use std::{
        ffi::OsStr,
        fs,
        os::unix::ffi::OsStrExt,
        path::{Path, PathBuf},
        time::{Duration, Instant, SystemTime, UNIX_EPOCH},
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
                "org-files-db-watcher-tests-{}-{}-{}",
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

    fn load_config(test_dir: &TestDir, body: &str) -> Config {
        let config_path = test_dir.path().join("config.toml");
        write_file(&config_path, body);
        Config::load_from_file(config_path).expect("config should load")
    }

    fn recursive_config(test_dir: &TestDir) -> Config {
        fs::create_dir_all(test_dir.path().join("notes")).expect("notes root should exist");
        load_config(
            test_dir,
            "db_path = \"db.sqlite\"\n[[dirs]]\npath = \"notes\"\nrecursive = true\n[search]\nfts5_enabled = false\n",
        )
    }

    fn paths(kind: WatcherPathEventKind, paths: impl IntoIterator<Item = PathBuf>) -> WatcherInput {
        WatcherInput::Paths {
            kind,
            paths: paths.into_iter().collect(),
        }
    }

    fn candidates(batch: NormalizedWatcherBatch) -> Vec<PathBuf> {
        match batch {
            NormalizedWatcherBatch::Candidates(paths) => paths,
            NormalizedWatcherBatch::Reconcile => panic!("expected candidate batch"),
        }
    }

    #[derive(Default)]
    struct RecordingExecutor {
        batches: Vec<NormalizedWatcherBatch>,
        fail_next: bool,
    }

    impl WatcherBatchExecutor for RecordingExecutor {
        type Error = &'static str;

        fn execute(&mut self, batch: NormalizedWatcherBatch) -> Result<(), Self::Error> {
            self.batches.push(batch);
            if self.fail_next {
                self.fail_next = false;
                Err("planned watcher execution failure")
            } else {
                Ok(())
            }
        }
    }

    fn controller(config: &Config, debounce_interval: Duration) -> WatcherExecutionController {
        WatcherExecutionController::with_debounce_interval(config, debounce_interval)
            .expect("controller")
    }

    #[test]
    fn indexer_watcher_batch_advances_once_and_noop_batch_does_not_advance() {
        let test_dir = TestDir::new("generation-batch");
        let config = recursive_config(&test_dir);
        let note = test_dir.path().join("notes/note.org");
        write_file(&note, "* Initial\n");
        let schema = SchemaDefinition::new(CURRENT_SCHEMA_VERSION, false);
        let mut connection = open_database_with_schema(&config.db_path, &schema)
            .expect("watcher test database should open");
        let indexer = Indexer::new(OrgizeAdapter::new());
        indexer
            .rebuild(&mut connection, &config)
            .expect("initial rebuild should succeed");
        let before = read_index_state(&connection).expect("initial state should load");

        write_file(&note, "* Changed\n");
        let canonical = fs::canonicalize(&note).expect("candidate should canonicalize");
        IndexerWatcherExecutor::new(&indexer, &mut connection, &config)
            .execute(NormalizedWatcherBatch::Candidates(vec![canonical.clone()]))
            .expect("changed watcher batch should execute");
        let after_change = read_index_state(&connection).expect("changed state should load");
        assert_eq!(after_change.generation, before.generation + 1);

        IndexerWatcherExecutor::new(&indexer, &mut connection, &config)
            .execute(NormalizedWatcherBatch::Candidates(vec![canonical]))
            .expect("no-op watcher batch should execute");
        assert_eq!(
            read_index_state(&connection).expect("no-op state should load"),
            after_change
        );
    }

    #[test]
    fn repeated_modify_events_produce_one_candidate() {
        let test_dir = TestDir::new("repeated-modify");
        let config = recursive_config(&test_dir);
        let note = test_dir.path().join("notes/note.org");
        write_file(&note, "* Note\n");
        let normalizer = WatcherBatchNormalizer::from_config(&config).expect("normalizer");

        let batch = normalizer.normalize([
            paths(WatcherPathEventKind::Modify, [note.clone()]),
            paths(WatcherPathEventKind::Modify, [note.clone()]),
        ]);

        assert_eq!(candidates(batch), vec![fs::canonicalize(note).unwrap()]);
    }

    #[test]
    fn create_followed_by_modify_produces_one_candidate() {
        let test_dir = TestDir::new("create-modify");
        let config = recursive_config(&test_dir);
        let normalizer = WatcherBatchNormalizer::from_config(&config).expect("normalizer");
        let note = test_dir.path().join("notes/new.org");
        write_file(&note, "* New\n");

        let batch = normalizer.normalize([
            paths(WatcherPathEventKind::Create, [note.clone()]),
            paths(WatcherPathEventKind::Modify, [note.clone()]),
        ]);

        assert_eq!(candidates(batch), vec![fs::canonicalize(note).unwrap()]);
    }

    #[test]
    fn modify_followed_by_remove_keeps_the_deleted_candidate() {
        let test_dir = TestDir::new("modify-remove");
        let config = recursive_config(&test_dir);
        let note = test_dir.path().join("notes/removed.org");
        write_file(&note, "* Removed\n");
        let canonical = fs::canonicalize(&note).unwrap();
        let normalizer = WatcherBatchNormalizer::from_config(&config).expect("normalizer");
        fs::remove_file(&note).expect("file should be removed");

        let batch = normalizer.normalize([
            paths(WatcherPathEventKind::Modify, [note.clone()]),
            paths(WatcherPathEventKind::Remove, [note]),
        ]);

        assert_eq!(candidates(batch), vec![canonical]);
    }

    #[test]
    fn remove_followed_by_create_keeps_the_current_candidate() {
        let test_dir = TestDir::new("remove-create");
        let config = recursive_config(&test_dir);
        let note = test_dir.path().join("notes/recreated.org");
        write_file(&note, "* Before\n");
        let normalizer = WatcherBatchNormalizer::from_config(&config).expect("normalizer");
        fs::remove_file(&note).expect("file should be removed");
        write_file(&note, "* After\n");

        let batch = normalizer.normalize([
            paths(WatcherPathEventKind::Remove, [note.clone()]),
            paths(WatcherPathEventKind::Create, [note.clone()]),
        ]);

        assert_eq!(candidates(batch), vec![fs::canonicalize(note).unwrap()]);
    }

    #[test]
    fn rename_with_both_paths_preserves_old_and_new_candidates() {
        let test_dir = TestDir::new("rename-pair");
        let config = recursive_config(&test_dir);
        let old = test_dir.path().join("notes/old.org");
        let new = test_dir.path().join("notes/new.org");
        write_file(&old, "* Note\n");
        let old_canonical = fs::canonicalize(&old).unwrap();
        let normalizer = WatcherBatchNormalizer::from_config(&config).expect("normalizer");
        fs::rename(&old, &new).expect("file should be renamed");
        let new_canonical = fs::canonicalize(&new).unwrap();

        let batch =
            candidates(normalizer.normalize([paths(WatcherPathEventKind::Rename, [old, new])]));
        let mut expected = vec![old_canonical, new_canonical];
        expected.sort();

        assert_eq!(batch, expected);
    }

    #[test]
    fn created_directory_inside_recursive_root_requests_reconciliation() {
        let test_dir = TestDir::new("created-directory");
        let config = recursive_config(&test_dir);
        let normalizer = WatcherBatchNormalizer::from_config(&config).expect("normalizer");
        let directory = test_dir.path().join("notes/new-directory");
        fs::create_dir_all(&directory).expect("directory should be created");

        let batch = normalizer.normalize([paths(WatcherPathEventKind::Create, [directory])]);

        assert_eq!(batch, NormalizedWatcherBatch::Reconcile);
    }

    #[test]
    fn removed_known_directory_keeps_a_bounded_reconciliation_candidate() {
        let test_dir = TestDir::new("removed-directory");
        let directory = test_dir.path().join("notes/nested");
        fs::create_dir_all(&directory).expect("directory should be created");
        write_file(&directory.join("note.org"), "* Note\n");
        let config = recursive_config(&test_dir);
        let normalizer = WatcherBatchNormalizer::from_config(&config).expect("normalizer");
        let canonical = fs::canonicalize(&directory).expect("directory should canonicalize");
        fs::remove_dir_all(&directory).expect("directory should be removed");

        let batch = normalizer.normalize([paths(WatcherPathEventKind::Remove, [directory])]);

        assert_eq!(candidates(batch), vec![canonical]);
    }

    #[test]
    fn removed_configured_root_still_requests_reconciliation() {
        let test_dir = TestDir::new("removed-root");
        let notes = test_dir.path().join("notes");
        let config = recursive_config(&test_dir);
        let normalizer = WatcherBatchNormalizer::from_config(&config).expect("normalizer");
        fs::remove_dir_all(&notes).expect("configured root should be removed");

        let batch = normalizer.normalize([paths(WatcherPathEventKind::Remove, [notes])]);

        assert_eq!(batch, NormalizedWatcherBatch::Reconcile);
    }

    #[test]
    fn retargeted_file_symlink_requests_reconciliation() {
        use std::os::unix::fs::symlink;

        let test_dir = TestDir::new("retargeted-file-symlink");
        let config = recursive_config(&test_dir);
        let first = test_dir.path().join("first.org");
        let second = test_dir.path().join("second.org");
        let alias = test_dir.path().join("notes/alias.org");
        write_file(&first, "* First\n");
        write_file(&second, "* Second\n");
        symlink(&first, &alias).expect("initial symlink should be created");
        let normalizer = WatcherBatchNormalizer::from_config(&config).expect("normalizer");
        fs::remove_file(&alias).expect("initial symlink should be removed");
        symlink(&second, &alias).expect("replacement symlink should be created");

        let batch = normalizer.normalize([paths(WatcherPathEventKind::Create, [alias])]);

        assert_eq!(batch, NormalizedWatcherBatch::Reconcile);
    }

    #[test]
    fn incomplete_rename_requests_reconciliation() {
        let test_dir = TestDir::new("incomplete-rename");
        let config = recursive_config(&test_dir);
        let normalizer = WatcherBatchNormalizer::from_config(&config).expect("normalizer");

        assert_eq!(
            normalizer.normalize([paths(
                WatcherPathEventKind::Rename,
                [test_dir.path().join("notes/old.org")],
            )]),
            NormalizedWatcherBatch::Reconcile
        );
    }

    #[test]
    fn duplicate_and_syntactically_redundant_paths_are_coalesced() {
        let test_dir = TestDir::new("duplicate-paths");
        let config = recursive_config(&test_dir);
        let note = test_dir.path().join("notes/note.org");
        write_file(&note, "* Note\n");
        let normalizer = WatcherBatchNormalizer::from_config(&config).expect("normalizer");
        let redundant = test_dir.path().join("notes/./note.org");

        let batch = normalizer.normalize([paths(
            WatcherPathEventKind::Modify,
            [note.clone(), redundant, note.clone()],
        )]);

        assert_eq!(candidates(batch), vec![fs::canonicalize(note).unwrap()]);
    }

    #[test]
    fn excluded_and_unrelated_paths_are_ignored() {
        let test_dir = TestDir::new("ignored-paths");
        fs::create_dir_all(test_dir.path().join("notes/archive")).unwrap();
        let config = load_config(
            &test_dir,
            "db_path = \"db.sqlite\"\nfiles_exclude = [\"notes/global.org\"]\n[[dirs]]\npath = \"notes\"\nrecursive = true\nexclude = [\"archive/**\", \"local.org\"]\n[search]\nfts5_enabled = false\n",
        );
        let normalizer = WatcherBatchNormalizer::from_config(&config).expect("normalizer");
        let kept = test_dir.path().join("notes/kept.org");
        let global = test_dir.path().join("notes/global.org");
        let local = test_dir.path().join("notes/local.org");
        let archived = test_dir.path().join("notes/archive/old.org");
        let unrelated = test_dir.path().join("outside.org");
        for path in [&kept, &global, &local, &archived, &unrelated] {
            write_file(path, "* Note\n");
        }

        let batch = normalizer.normalize([paths(
            WatcherPathEventKind::Modify,
            [kept.clone(), global, local, archived, unrelated],
        )]);

        assert_eq!(candidates(batch), vec![fs::canonicalize(kept).unwrap()]);
    }

    #[test]
    fn global_exclusion_of_one_alias_suppresses_the_shared_canonical_identity() {
        use std::os::unix::fs::symlink;

        let test_dir = TestDir::new("global-alias-exclusion");
        fs::create_dir_all(test_dir.path().join("kept")).unwrap();
        fs::create_dir_all(test_dir.path().join("excluded")).unwrap();
        let target = test_dir.path().join("target.org");
        let kept = test_dir.path().join("kept/source.org");
        let excluded = test_dir.path().join("excluded/alias.org");
        write_file(&target, "* Target\n");
        symlink(&target, &kept).expect("kept symlink should be created");
        symlink(&target, &excluded).expect("excluded symlink should be created");
        let config = load_config(
            &test_dir,
            "db_path = \"db.sqlite\"\nfiles_exclude = [\"excluded/alias.org\"]\n[[dirs]]\npath = \"excluded\"\nrecursive = true\n[[dirs]]\npath = \"kept\"\nrecursive = true\n[search]\nfts5_enabled = false\n",
        );
        let normalizer = WatcherBatchNormalizer::from_config(&config).expect("normalizer");

        let batch = normalizer.normalize([paths(WatcherPathEventKind::Modify, [kept])]);

        assert_eq!(batch, NormalizedWatcherBatch::Candidates(Vec::new()));
    }

    #[test]
    fn non_recursive_roots_ignore_nested_candidates() {
        let test_dir = TestDir::new("non-recursive");
        fs::create_dir_all(test_dir.path().join("notes/nested")).unwrap();
        let config = load_config(
            &test_dir,
            "db_path = \"db.sqlite\"\n[[dirs]]\npath = \"notes\"\nrecursive = false\n[search]\nfts5_enabled = false\n",
        );
        let normalizer = WatcherBatchNormalizer::from_config(&config).expect("normalizer");
        let direct = test_dir.path().join("notes/direct.org");
        let nested = test_dir.path().join("notes/nested/nested.org");
        write_file(&direct, "* Direct\n");
        write_file(&nested, "* Nested\n");

        let batch = normalizer.normalize([paths(
            WatcherPathEventKind::Modify,
            [nested, direct.clone()],
        )]);

        assert_eq!(candidates(batch), vec![fs::canonicalize(direct).unwrap()]);
    }

    #[test]
    fn reconciliation_requests_supersede_path_candidates() {
        let test_dir = TestDir::new("reconciliation-supersedes");
        let config = recursive_config(&test_dir);
        let note = test_dir.path().join("notes/note.org");
        write_file(&note, "* Note\n");
        let normalizer = WatcherBatchNormalizer::from_config(&config).expect("normalizer");

        for uncertainty in [
            WatcherUncertainty::Overflow,
            WatcherUncertainty::DroppedEvents,
            WatcherUncertainty::Rescan,
            WatcherUncertainty::Other,
        ] {
            assert_eq!(
                normalizer.normalize([
                    paths(WatcherPathEventKind::Modify, [note.clone()]),
                    WatcherInput::Uncertain(uncertainty),
                ]),
                NormalizedWatcherBatch::Reconcile
            );
        }
    }

    #[test]
    fn equivalent_permutations_have_deterministic_identity_order() {
        let test_dir = TestDir::new("deterministic-order");
        let config = recursive_config(&test_dir);
        let alpha = test_dir.path().join("notes/alpha.org");
        let beta = test_dir.path().join("notes/beta.org");
        write_file(&alpha, "* Alpha\n");
        write_file(&beta, "* Beta\n");
        let normalizer = WatcherBatchNormalizer::from_config(&config).expect("normalizer");

        let first = normalizer.normalize([
            paths(WatcherPathEventKind::Metadata, [beta.clone()]),
            paths(WatcherPathEventKind::Modify, [alpha.clone()]),
        ]);
        let second = normalizer.normalize([
            paths(WatcherPathEventKind::Modify, [alpha]),
            paths(WatcherPathEventKind::Metadata, [beta]),
        ]);

        assert_eq!(first, second);
    }

    #[test]
    fn empty_or_irrelevant_batches_produce_no_candidates() {
        let test_dir = TestDir::new("empty-batch");
        let config = recursive_config(&test_dir);
        let normalizer = WatcherBatchNormalizer::from_config(&config).expect("normalizer");

        assert_eq!(
            normalizer.normalize(std::iter::empty::<WatcherInput>()),
            NormalizedWatcherBatch::Candidates(Vec::new())
        );
        assert_eq!(
            normalizer.normalize([paths(
                WatcherPathEventKind::Modify,
                [test_dir.path().join("unrelated.txt")],
            )]),
            NormalizedWatcherBatch::Candidates(Vec::new())
        );
    }

    #[test]
    fn explicit_non_org_files_remain_eligible() {
        let test_dir = TestDir::new("explicit-non-org");
        let explicit = test_dir.path().join("notes.data");
        write_file(&explicit, "* Note\n");
        let config = load_config(
            &test_dir,
            "db_path = \"db.sqlite\"\nfiles = [\"notes.data\"]\n[search]\nfts5_enabled = false\n",
        );
        let normalizer = WatcherBatchNormalizer::from_config(&config).expect("normalizer");

        let batch = normalizer.normalize([paths(WatcherPathEventKind::Modify, [explicit.clone()])]);

        assert_eq!(candidates(batch), vec![fs::canonicalize(explicit).unwrap()]);
    }

    #[test]
    fn deleted_explicit_files_use_the_retained_indexed_identity() {
        let test_dir = TestDir::new("deleted-explicit");
        let explicit = test_dir.path().join("explicit.org");
        write_file(&explicit, "* Note\n");
        let config = load_config(
            &test_dir,
            "db_path = \"db.sqlite\"\nfiles = [\"explicit.org\"]\n[search]\nfts5_enabled = false\n",
        );
        let canonical = fs::canonicalize(&explicit).unwrap();
        let normalizer = WatcherBatchNormalizer::from_config(&config).expect("normalizer");
        fs::remove_file(&explicit).expect("explicit file should be removed");

        let batch = normalizer.normalize([paths(WatcherPathEventKind::Remove, [explicit])]);

        assert_eq!(candidates(batch), vec![canonical]);
    }

    #[test]
    fn deleted_symlink_sources_keep_their_canonical_indexed_identity() {
        use std::os::unix::fs::symlink;

        let test_dir = TestDir::new("deleted-symlink");
        let config = recursive_config(&test_dir);
        let target = test_dir.path().join("target.org");
        let alias = test_dir.path().join("notes/alias.org");
        write_file(&target, "* Target\n");
        symlink(&target, &alias).expect("file symlink should be created");
        let canonical = fs::canonicalize(&alias).unwrap();
        let normalizer = WatcherBatchNormalizer::from_config(&config).expect("normalizer");
        fs::remove_file(&alias).expect("symlink should be removed");

        let batch = normalizer.normalize([paths(WatcherPathEventKind::Remove, [alias])]);

        assert_eq!(candidates(batch), vec![canonical]);
    }

    #[test]
    fn non_utf8_unix_paths_are_preserved_and_ordered_without_lossy_conversion() {
        let test_dir = TestDir::new("non-utf8");
        let config = recursive_config(&test_dir);
        let note = test_dir
            .path()
            .join("notes")
            .join(OsStr::from_bytes(b"note-\xff.org"));
        write_file(&note, "* Note\n");
        let normalizer = WatcherBatchNormalizer::from_config(&config).expect("normalizer");
        let canonical = fs::canonicalize(&note).unwrap();

        let batch = normalizer.normalize([paths(WatcherPathEventKind::Modify, [note])]);

        assert_eq!(candidates(batch), vec![canonical]);
    }

    #[test]
    fn oversized_candidate_burst_collapses_to_reconciliation() {
        let test_dir = TestDir::new("bounded-candidates");
        let config = recursive_config(&test_dir);
        let mut notes = Vec::new();
        for index in 0..=MAX_WATCHER_CANDIDATES_PER_BATCH {
            let note = test_dir
                .path()
                .join("notes")
                .join(format!("note-{index:04}.org"));
            write_file(&note, "* Note\n");
            notes.push(note);
        }
        let normalizer = WatcherBatchNormalizer::from_config(&config).expect("normalizer");

        let batch = normalizer.normalize([paths(WatcherPathEventKind::Modify, notes)]);

        assert_eq!(batch, NormalizedWatcherBatch::Reconcile);
    }

    #[test]
    fn merging_candidate_batches_cannot_exceed_the_bound() {
        let left = NormalizedWatcherBatch::Candidates(
            (0..MAX_WATCHER_CANDIDATES_PER_BATCH)
                .map(|index| PathBuf::from(format!("/tmp/left-{index:04}.org")))
                .collect(),
        );
        let right = NormalizedWatcherBatch::Candidates(vec![PathBuf::from("/tmp/overflow.org")]);

        assert_eq!(left.merge(right), NormalizedWatcherBatch::Reconcile);
    }

    #[test]
    fn default_controller_uses_the_documented_debounce_interval() {
        let test_dir = TestDir::new("default-debounce");
        let config = recursive_config(&test_dir);
        let controller = WatcherExecutionController::from_config(&config).expect("controller");

        assert_eq!(
            controller.debounce_interval(),
            DEFAULT_WATCHER_DEBOUNCE_INTERVAL
        );
    }

    #[test]
    fn one_event_burst_produces_one_execution() {
        let test_dir = TestDir::new("one-burst");
        let config = recursive_config(&test_dir);
        let note = test_dir.path().join("notes/note.org");
        write_file(&note, "* Note\n");
        let canonical = fs::canonicalize(&note).unwrap();
        let debounce = Duration::from_millis(100);
        let mut controller = controller(&config, debounce);
        let mut executor = RecordingExecutor::default();
        let start = Instant::now();

        controller
            .push_input(paths(WatcherPathEventKind::Create, [note.clone()]), start)
            .unwrap();
        controller
            .push_input(
                paths(WatcherPathEventKind::Modify, [note.clone()]),
                start + Duration::from_millis(10),
            )
            .unwrap();
        controller
            .push_input(
                paths(WatcherPathEventKind::Metadata, [note]),
                start + Duration::from_millis(20),
            )
            .unwrap();

        assert_eq!(
            controller
                .execute_ready(start + Duration::from_millis(119), &mut executor)
                .unwrap(),
            WatcherExecutionStatus::Idle
        );
        assert_eq!(
            controller
                .execute_ready(start + Duration::from_millis(120), &mut executor)
                .unwrap(),
            WatcherExecutionStatus::Executed
        );
        assert_eq!(executor.batches.len(), 1);
        assert_eq!(
            executor.batches[0],
            NormalizedWatcherBatch::Candidates(vec![canonical])
        );
    }

    #[test]
    fn separate_bursts_produce_separate_executions() {
        let test_dir = TestDir::new("separate-bursts");
        let config = recursive_config(&test_dir);
        let alpha = test_dir.path().join("notes/alpha.org");
        let beta = test_dir.path().join("notes/beta.org");
        write_file(&alpha, "* Alpha\n");
        write_file(&beta, "* Beta\n");
        let mut controller = controller(&config, Duration::from_millis(50));
        let mut executor = RecordingExecutor::default();
        let start = Instant::now();

        controller
            .push_input(paths(WatcherPathEventKind::Modify, [alpha]), start)
            .unwrap();
        assert_eq!(
            controller
                .execute_ready(start + Duration::from_millis(50), &mut executor)
                .unwrap(),
            WatcherExecutionStatus::Executed
        );

        controller
            .push_input(
                paths(WatcherPathEventKind::Modify, [beta]),
                start + Duration::from_millis(100),
            )
            .unwrap();
        assert_eq!(
            controller
                .execute_ready(start + Duration::from_millis(150), &mut executor)
                .unwrap(),
            WatcherExecutionStatus::Executed
        );

        assert_eq!(executor.batches.len(), 2);
    }

    #[test]
    fn pending_reconciliation_supersedes_candidates() {
        let test_dir = TestDir::new("controller-reconciliation");
        let config = recursive_config(&test_dir);
        let note = test_dir.path().join("notes/note.org");
        write_file(&note, "* Note\n");
        let mut controller = controller(&config, Duration::from_millis(10));
        let mut executor = RecordingExecutor::default();
        let start = Instant::now();

        controller
            .push_input(paths(WatcherPathEventKind::Modify, [note]), start)
            .unwrap();
        controller
            .push_input(
                WatcherInput::Uncertain(WatcherUncertainty::Overflow),
                start + Duration::from_millis(1),
            )
            .unwrap();
        controller
            .execute_ready(start + Duration::from_millis(11), &mut executor)
            .unwrap();

        assert_eq!(executor.batches, vec![NormalizedWatcherBatch::Reconcile]);
    }

    #[test]
    fn events_arriving_during_execution_are_retained_for_the_next_batch() {
        let test_dir = TestDir::new("events-during-execution");
        let config = recursive_config(&test_dir);
        let alpha = test_dir.path().join("notes/alpha.org");
        let beta = test_dir.path().join("notes/beta.org");
        write_file(&alpha, "* Alpha\n");
        write_file(&beta, "* Beta\n");
        let alpha = fs::canonicalize(alpha).unwrap();
        let beta = fs::canonicalize(beta).unwrap();
        let mut controller = controller(&config, Duration::from_millis(100));
        let start = Instant::now();

        controller
            .push_input(paths(WatcherPathEventKind::Modify, [alpha.clone()]), start)
            .unwrap();
        assert_eq!(
            controller.start_ready_execution(start + Duration::from_millis(100)),
            Some(NormalizedWatcherBatch::Candidates(vec![alpha]))
        );

        controller
            .push_input(
                paths(WatcherPathEventKind::Modify, [beta.clone()]),
                start + Duration::from_millis(110),
            )
            .unwrap();
        assert_eq!(
            controller.start_ready_execution(start + Duration::from_secs(1)),
            None
        );
        controller.finish_execution_success().unwrap();

        assert_eq!(
            controller.start_ready_execution(start + Duration::from_millis(209)),
            None
        );
        assert_eq!(
            controller.start_ready_execution(start + Duration::from_millis(210)),
            Some(NormalizedWatcherBatch::Candidates(vec![beta]))
        );
        controller.finish_execution_success().unwrap();
    }

    #[test]
    fn failed_execution_preserves_active_and_later_pending_work() {
        let test_dir = TestDir::new("failed-execution");
        let config = recursive_config(&test_dir);
        let alpha = test_dir.path().join("notes/alpha.org");
        let beta = test_dir.path().join("notes/beta.org");
        write_file(&alpha, "* Alpha\n");
        write_file(&beta, "* Beta\n");
        let alpha = fs::canonicalize(alpha).unwrap();
        let beta = fs::canonicalize(beta).unwrap();
        let mut controller = controller(&config, Duration::from_millis(100));
        let start = Instant::now();

        controller
            .push_input(paths(WatcherPathEventKind::Modify, [alpha.clone()]), start)
            .unwrap();
        assert!(controller
            .start_ready_execution(start + Duration::from_millis(100))
            .is_some());
        controller
            .push_input(
                paths(WatcherPathEventKind::Modify, [beta.clone()]),
                start + Duration::from_millis(110),
            )
            .unwrap();
        controller
            .finish_execution_failure(start + Duration::from_millis(120))
            .unwrap();

        assert_eq!(
            controller.start_ready_execution(start + Duration::from_millis(219)),
            None
        );
        assert_eq!(
            controller.start_ready_execution(start + Duration::from_millis(220)),
            Some(NormalizedWatcherBatch::Candidates(vec![alpha, beta]))
        );
        controller.finish_execution_success().unwrap();
    }

    #[test]
    fn executor_failure_requeues_the_batch_after_the_debounce_interval() {
        let test_dir = TestDir::new("executor-failure");
        let config = recursive_config(&test_dir);
        let note = test_dir.path().join("notes/note.org");
        write_file(&note, "* Note\n");
        let mut controller = controller(&config, Duration::from_millis(25));
        let mut executor = RecordingExecutor {
            batches: Vec::new(),
            fail_next: true,
        };
        let start = Instant::now();

        controller
            .push_input(paths(WatcherPathEventKind::Modify, [note]), start)
            .unwrap();
        assert_eq!(
            controller.execute_ready(start + Duration::from_millis(25), &mut executor),
            Err("planned watcher execution failure")
        );
        assert_eq!(
            controller
                .execute_ready(start + Duration::from_millis(49), &mut executor)
                .unwrap(),
            WatcherExecutionStatus::Idle
        );
        assert_eq!(
            controller
                .execute_ready(start + Duration::from_millis(50), &mut executor)
                .unwrap(),
            WatcherExecutionStatus::Executed
        );
        assert_eq!(executor.batches.len(), 2);
        assert_eq!(executor.batches[0], executor.batches[1]);
    }

    #[test]
    fn shutdown_with_an_empty_queue_is_immediately_complete() {
        let test_dir = TestDir::new("empty-shutdown");
        let config = recursive_config(&test_dir);
        let mut controller = controller(&config, Duration::from_millis(100));
        let start = Instant::now();

        controller.begin_shutdown();

        assert!(!controller.is_accepting_inputs());
        assert!(controller.is_shutdown_complete());
        assert_eq!(controller.start_ready_execution(start), None);
        assert_eq!(
            controller.push_input(WatcherInput::Uncertain(WatcherUncertainty::Rescan), start),
            Err(WatcherControllerError::InputClosed)
        );
    }

    #[test]
    fn shutdown_flushes_a_pending_batch_without_waiting_for_debounce() {
        let test_dir = TestDir::new("pending-shutdown");
        let config = recursive_config(&test_dir);
        let note = test_dir.path().join("notes/note.org");
        write_file(&note, "* Note\n");
        let canonical = fs::canonicalize(&note).unwrap();
        let mut controller = controller(&config, Duration::from_secs(30));
        let start = Instant::now();

        controller
            .push_input(paths(WatcherPathEventKind::Modify, [note]), start)
            .unwrap();
        assert_eq!(
            controller.next_deadline(),
            Some(start + Duration::from_secs(30))
        );
        controller.begin_shutdown();

        assert_eq!(
            controller.start_ready_execution(start + Duration::from_millis(1)),
            Some(NormalizedWatcherBatch::Candidates(vec![canonical]))
        );
        assert!(!controller.is_shutdown_complete());
        controller.finish_execution_success().unwrap();
        assert!(controller.is_shutdown_complete());
    }

    #[test]
    fn completion_without_an_active_execution_is_rejected() {
        let test_dir = TestDir::new("completion-without-execution");
        let config = recursive_config(&test_dir);
        let mut controller = controller(&config, Duration::from_millis(10));

        assert_eq!(
            controller.finish_execution_success(),
            Err(WatcherControllerError::NoActiveExecution)
        );
    }
}
