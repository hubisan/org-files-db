use std::{
    error::Error,
    fmt,
    path::{Path, PathBuf},
    time::Instant,
};

use crate::{
    config::Config,
    indexer::IndexerError,
    notify_source::{
        NotifyBackendFailure, NotifySourceMessage, NotifyWatcherError, NotifyWatcherSource,
    },
    watcher::{
        NormalizedWatcherBatch, WatcherBatchExecutor, WatcherControllerError,
        WatcherExecutionController, WatcherExecutionStatus, WatcherInput, WatcherPathEventKind,
        WatcherUncertainty,
    },
};

pub(crate) trait WatcherMessageSource {
    type Error;

    fn try_recv_message(&mut self) -> Result<Option<NotifySourceMessage>, Self::Error>;
    fn watch_target_paths(&self) -> Vec<PathBuf>;
    fn validate_watch_targets(&self) -> Result<(), Self::Error>;
    fn refresh_watches(&mut self) -> Result<(), Self::Error>;
}

impl WatcherMessageSource for NotifyWatcherSource {
    type Error = NotifyWatcherError;

    fn try_recv_message(&mut self) -> Result<Option<NotifySourceMessage>, Self::Error> {
        self.try_recv()
    }

    fn watch_target_paths(&self) -> Vec<PathBuf> {
        self.watch_targets()
            .iter()
            .map(|target| target.path().to_path_buf())
            .collect()
    }

    fn validate_watch_targets(&self) -> Result<(), Self::Error> {
        NotifyWatcherSource::validate_watch_targets(self)
    }

    fn refresh_watches(&mut self) -> Result<(), Self::Error> {
        NotifyWatcherSource::refresh_watches(self)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum WatcherRuntimeState {
    Running,
    Terminated,
}

#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub(crate) struct WatcherRecoveryContext {
    uncertainty_count: usize,
    latest_watch_target: Option<PathBuf>,
    latest_backend_failure: Option<NotifyBackendFailure>,
}

impl WatcherRecoveryContext {
    pub(crate) fn uncertainty_count(&self) -> usize {
        self.uncertainty_count
    }

    pub(crate) fn latest_watch_target(&self) -> Option<&Path> {
        self.latest_watch_target.as_deref()
    }

    pub(crate) fn latest_backend_failure(&self) -> Option<&NotifyBackendFailure> {
        self.latest_backend_failure.as_ref()
    }

    fn record_uncertainty(&mut self) {
        self.uncertainty_count = self.uncertainty_count.saturating_add(1);
    }

    fn record_watch_target(&mut self, path: PathBuf) {
        self.record_uncertainty();
        self.latest_watch_target = Some(path);
    }

    fn record_backend_failure(&mut self, failure: NotifyBackendFailure) {
        self.record_uncertainty();
        self.latest_backend_failure = Some(failure);
    }

    fn clear(&mut self) {
        *self = Self::default();
    }

    fn is_empty(&self) -> bool {
        self.uncertainty_count == 0
            && self.latest_watch_target.is_none()
            && self.latest_backend_failure.is_none()
    }
}

impl fmt::Display for WatcherRecoveryContext {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.is_empty() {
            return write!(f, "no recorded backend uncertainty");
        }

        write!(f, "{} uncertainty signal(s)", self.uncertainty_count)?;
        if let Some(path) = &self.latest_watch_target {
            write!(f, "; latest affected watch target: {}", path.display())?;
        }
        if let Some(failure) = &self.latest_backend_failure {
            write!(f, "; latest backend failure: {failure}")?;
        }
        Ok(())
    }
}

#[derive(Debug)]
pub(crate) enum WatcherStartupError<SourceError, ExecutionError> {
    Source(SourceError),
    Controller(IndexerError),
    ControllerState(WatcherControllerError),
    Reconciliation(ExecutionError),
}

impl<SourceError, ExecutionError> fmt::Display for WatcherStartupError<SourceError, ExecutionError>
where
    SourceError: fmt::Display,
    ExecutionError: fmt::Display,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Source(source) => write!(f, "watcher startup source failed: {source}"),
            Self::Controller(source) => {
                write!(
                    f,
                    "watcher startup controller initialization failed: {source}"
                )
            }
            Self::ControllerState(source) => {
                write!(f, "watcher startup controller state failed: {source}")
            }
            Self::Reconciliation(source) => write!(
                f,
                "watcher startup reconciliation failed; watcher was not started: {source}"
            ),
        }
    }
}

impl<SourceError, ExecutionError> Error for WatcherStartupError<SourceError, ExecutionError>
where
    SourceError: Error + 'static,
    ExecutionError: Error + 'static,
{
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        match self {
            Self::Source(source) => Some(source),
            Self::Controller(source) => Some(source),
            Self::ControllerState(source) => Some(source),
            Self::Reconciliation(source) => Some(source),
        }
    }
}

#[derive(Debug)]
pub(crate) enum WatcherRuntimeError<SourceError, ExecutionError> {
    Terminated,
    Source(SourceError),
    Controller(WatcherControllerError),
    Execution {
        batch: NormalizedWatcherBatch,
        recovery: WatcherRecoveryContext,
        source: ExecutionError,
    },
}

impl<SourceError, ExecutionError> fmt::Display for WatcherRuntimeError<SourceError, ExecutionError>
where
    SourceError: fmt::Display,
    ExecutionError: fmt::Display,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Terminated => write!(f, "watcher runtime is terminated"),
            Self::Source(source) => {
                write!(
                    f,
                    "watcher source failed; watcher runtime terminated: {source}"
                )
            }
            Self::Controller(source) => {
                write!(
                    f,
                    "watcher controller failed; watcher runtime terminated: {source}"
                )
            }
            Self::Execution {
                batch,
                recovery,
                source,
            } => write!(
                f,
                "watcher {:?} execution failed; watcher runtime terminated ({recovery}): {source}",
                batch
            ),
        }
    }
}

impl<SourceError, ExecutionError> Error for WatcherRuntimeError<SourceError, ExecutionError>
where
    SourceError: Error + 'static,
    ExecutionError: Error + 'static,
{
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        match self {
            Self::Terminated => None,
            Self::Source(source) => Some(source),
            Self::Controller(source) => Some(source),
            Self::Execution { source, .. } => Some(source),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) struct WatcherCycleReport {
    pub(crate) source_messages: usize,
    pub(crate) backend_failures: usize,
    pub(crate) execution_status: WatcherExecutionStatus,
}

pub(crate) struct WatcherRuntime<S> {
    source: S,
    controller: WatcherExecutionController,
    state: WatcherRuntimeState,
    recovery: WatcherRecoveryContext,
    watch_targets: Vec<PathBuf>,
    refresh_required: bool,
}

impl WatcherRuntime<NotifyWatcherSource> {
    pub(crate) fn start_notify<E>(
        config: &Config,
        now: Instant,
        executor: &mut E,
    ) -> Result<Self, WatcherStartupError<NotifyWatcherError, E::Error>>
    where
        E: WatcherBatchExecutor,
    {
        let source =
            NotifyWatcherSource::from_config(config).map_err(WatcherStartupError::Source)?;
        Self::start_registered(source, config, now, executor)
    }
}

impl<S> WatcherRuntime<S>
where
    S: WatcherMessageSource,
{
    pub(crate) fn start_registered<E>(
        source: S,
        config: &Config,
        now: Instant,
        executor: &mut E,
    ) -> Result<Self, WatcherStartupError<S::Error, E::Error>>
    where
        E: WatcherBatchExecutor,
    {
        source
            .validate_watch_targets()
            .map_err(WatcherStartupError::Source)?;
        let controller = WatcherExecutionController::from_config(config)
            .map_err(WatcherStartupError::Controller)?;
        let watch_targets = source.watch_target_paths();
        let mut runtime = Self {
            source,
            controller,
            state: WatcherRuntimeState::Running,
            recovery: WatcherRecoveryContext::default(),
            watch_targets,
            refresh_required: false,
        };

        executor
            .execute(NormalizedWatcherBatch::Reconcile)
            .map_err(WatcherStartupError::Reconciliation)?;
        runtime
            .drain_source::<E::Error>(now)
            .map_err(|error| match error {
                WatcherRuntimeError::Source(source) => WatcherStartupError::Source(source),
                WatcherRuntimeError::Controller(source) => {
                    WatcherStartupError::ControllerState(source)
                }
                WatcherRuntimeError::Terminated | WatcherRuntimeError::Execution { .. } => {
                    unreachable!("startup source drain cannot execute a Phase 6 batch")
                }
            })?;
        Ok(runtime)
    }

    pub(crate) fn state(&self) -> WatcherRuntimeState {
        self.state
    }

    pub(crate) fn next_deadline(&self) -> Option<Instant> {
        self.controller.next_deadline()
    }

    pub(crate) fn recovery_context(&self) -> &WatcherRecoveryContext {
        &self.recovery
    }

    pub(crate) fn begin_shutdown(&mut self) {
        self.controller.begin_shutdown();
    }

    pub(crate) fn is_shutdown_complete(&self) -> bool {
        self.controller.is_shutdown_complete()
    }

    pub(crate) fn process_available<E>(
        &mut self,
        now: Instant,
        executor: &mut E,
    ) -> Result<WatcherCycleReport, WatcherRuntimeError<S::Error, E::Error>>
    where
        E: WatcherBatchExecutor,
    {
        self.ensure_running::<E::Error>()?;
        let (source_messages, backend_failures) = if self.controller.is_accepting_inputs() {
            self.drain_source::<E::Error>(now)?
        } else {
            (0, 0)
        };
        let execution_status = self.execute_ready(now, executor)?;
        Ok(WatcherCycleReport {
            source_messages,
            backend_failures,
            execution_status,
        })
    }

    fn ensure_running<ExecutionError>(
        &self,
    ) -> Result<(), WatcherRuntimeError<S::Error, ExecutionError>> {
        if self.state == WatcherRuntimeState::Terminated {
            Err(WatcherRuntimeError::Terminated)
        } else {
            Ok(())
        }
    }

    fn drain_source<ExecutionError>(
        &mut self,
        now: Instant,
    ) -> Result<(usize, usize), WatcherRuntimeError<S::Error, ExecutionError>> {
        let mut source_messages = 0;
        let mut backend_failures = 0;

        loop {
            let message = match self.source.try_recv_message() {
                Ok(Some(message)) => message,
                Ok(None) => break,
                Err(source) => {
                    self.state = WatcherRuntimeState::Terminated;
                    return Err(WatcherRuntimeError::Source(source));
                }
            };
            source_messages += 1;

            let input = match message {
                NotifySourceMessage::Input(input) => self.prepare_input(input),
                NotifySourceMessage::BackendFailure(failure) => {
                    backend_failures += 1;
                    let input = failure.recovery_input();
                    self.recovery.record_backend_failure(failure);
                    input
                }
            };

            self.controller.push_input(input, now).map_err(|source| {
                self.state = WatcherRuntimeState::Terminated;
                WatcherRuntimeError::Controller(source)
            })?;
        }

        Ok((source_messages, backend_failures))
    }

    fn prepare_input(&mut self, input: WatcherInput) -> WatcherInput {
        if let Some(path) = affected_watch_target(&input, &self.watch_targets) {
            self.recovery.record_watch_target(path);
            self.refresh_required = true;
            return WatcherInput::Uncertain(WatcherUncertainty::Other);
        }

        if matches!(&input, WatcherInput::Uncertain(_)) {
            self.recovery.record_uncertainty();
        }
        input
    }

    fn execute_ready<E>(
        &mut self,
        now: Instant,
        executor: &mut E,
    ) -> Result<WatcherExecutionStatus, WatcherRuntimeError<S::Error, E::Error>>
    where
        E: WatcherBatchExecutor,
    {
        let Some(batch) = self.controller.start_ready_execution(now) else {
            return Ok(WatcherExecutionStatus::Idle);
        };

        if let Err(source) = self.source.validate_watch_targets() {
            self.state = WatcherRuntimeState::Terminated;
            self.controller
                .finish_execution_failure(now)
                .map_err(WatcherRuntimeError::Controller)?;
            return Err(WatcherRuntimeError::Source(source));
        }

        if matches!(&batch, NormalizedWatcherBatch::Reconcile) && self.refresh_required {
            if let Err(source) = self.source.refresh_watches() {
                self.state = WatcherRuntimeState::Terminated;
                self.controller
                    .finish_execution_failure(now)
                    .map_err(WatcherRuntimeError::Controller)?;
                return Err(WatcherRuntimeError::Source(source));
            }
        }

        match executor.execute(batch.clone()) {
            Ok(()) => {
                if let Err(source) = self.controller.finish_execution_success() {
                    self.state = WatcherRuntimeState::Terminated;
                    return Err(WatcherRuntimeError::Controller(source));
                }
                if matches!(&batch, NormalizedWatcherBatch::Reconcile) {
                    self.recovery.clear();
                    self.refresh_required = false;
                }
                Ok(WatcherExecutionStatus::Executed)
            }
            Err(source) => {
                self.state = WatcherRuntimeState::Terminated;
                self.controller
                    .finish_execution_failure(now)
                    .map_err(WatcherRuntimeError::Controller)?;
                Err(WatcherRuntimeError::Execution {
                    batch,
                    recovery: self.recovery.clone(),
                    source,
                })
            }
        }
    }
}

fn affected_watch_target(input: &WatcherInput, watch_targets: &[PathBuf]) -> Option<PathBuf> {
    let WatcherInput::Paths { kind, paths } = input else {
        return None;
    };
    if !matches!(
        kind,
        WatcherPathEventKind::Create
            | WatcherPathEventKind::Remove
            | WatcherPathEventKind::Rename
            | WatcherPathEventKind::Metadata
            | WatcherPathEventKind::Other
    ) {
        return None;
    }

    paths
        .iter()
        .find(|path| {
            watch_targets
                .iter()
                .any(|target| target.as_path() == path.as_path())
        })
        .cloned()
}

#[cfg(test)]
mod tests {
    use super::{
        WatcherCycleReport, WatcherMessageSource, WatcherRuntime, WatcherRuntimeError,
        WatcherRuntimeState,
    };
    use crate::{
        config::{Config, SearchConfig},
        db::{open_database_with_schema, SchemaDefinition, CURRENT_SCHEMA_VERSION},
        indexer::{ChangeApplicationReport, ChangeApplicationResult, Indexer},
        notify_source::{NotifyBackendFailure, NotifySourceMessage},
        parser::OrgizeAdapter,
        watcher::{
            NormalizedWatcherBatch, Phase6WatcherExecutor, WatcherBatchExecutor,
            WatcherExecutionError, WatcherExecutionStatus, WatcherInput, WatcherPathEventKind,
            WatcherUncertainty,
        },
    };
    use rusqlite::Connection;
    use std::{
        cell::{Cell, RefCell},
        collections::VecDeque,
        convert::Infallible,
        error::Error,
        fmt, fs,
        path::{Path, PathBuf},
        rc::Rc,
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
                "org-files-db-watcher-runtime-tests-{}-{}-{}",
                name,
                std::process::id(),
                unique
            ));
            fs::create_dir_all(&path).expect("test directory should be created");
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
            fs::create_dir_all(parent).expect("parent directory should exist");
        }
        fs::write(path, content).expect("file should be written");
    }

    fn search_config() -> SearchConfig {
        SearchConfig {
            fts5_enabled: false,
            index_body_text: false,
        }
    }

    fn explicit_config(test_dir: &TestDir, files: Vec<PathBuf>) -> Config {
        Config {
            db_path: test_dir.path().join("db.sqlite"),
            files,
            dirs: Vec::new(),
            discovery: Default::default(),
            links: Default::default(),
            todo: Default::default(),
            search: search_config(),
            query: Default::default(),
        }
    }

    fn recursive_config(test_dir: &TestDir) -> Config {
        let root = test_dir.path().join("notes");
        fs::create_dir_all(&root).expect("notes root should exist");
        Config {
            db_path: test_dir.path().join("db.sqlite"),
            files: Vec::new(),
            dirs: vec![crate::config::ConfiguredDir {
                path: root,
                recursive: true,
                exclude: Vec::new(),
            }],
            discovery: Default::default(),
            links: Default::default(),
            todo: Default::default(),
            search: search_config(),
            query: Default::default(),
        }
    }

    fn open_database(config: &Config) -> Connection {
        open_database_with_schema(
            &config.db_path,
            &SchemaDefinition::new(CURRENT_SCHEMA_VERSION, false),
        )
        .expect("database should open")
    }

    fn heading_titles(connection: &Connection) -> Vec<String> {
        connection
            .prepare("SELECT title FROM headings WHERE level = 1 ORDER BY title")
            .expect("title query should prepare")
            .query_map([], |row| row.get::<_, String>(0))
            .expect("title query should execute")
            .collect::<Result<Vec<_>, _>>()
            .expect("titles should read")
    }

    #[derive(Debug, Clone, PartialEq, Eq)]
    enum TestSourceError {
        Receive,
        Unavailable(PathBuf),
        Refresh,
    }

    impl fmt::Display for TestSourceError {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            match self {
                Self::Receive => write!(f, "planned source receive failure"),
                Self::Unavailable(path) => {
                    write!(f, "watch target is unavailable: {}", path.display())
                }
                Self::Refresh => write!(f, "planned watch refresh failure"),
            }
        }
    }

    impl Error for TestSourceError {}

    type SourceQueue = Rc<RefCell<VecDeque<Result<NotifySourceMessage, TestSourceError>>>>;

    #[derive(Clone)]
    struct TestSourceHandle {
        queue: SourceQueue,
        refreshes: Rc<Cell<usize>>,
        fail_refresh: Rc<Cell<bool>>,
    }

    impl TestSourceHandle {
        fn push(&self, message: NotifySourceMessage) {
            self.queue.borrow_mut().push_back(Ok(message));
        }

        fn push_error(&self) {
            self.queue
                .borrow_mut()
                .push_back(Err(TestSourceError::Receive));
        }

        fn refresh_count(&self) -> usize {
            self.refreshes.get()
        }

        fn fail_refresh(&self) {
            self.fail_refresh.set(true);
        }
    }

    struct TestSource {
        queue: SourceQueue,
        watch_targets: Vec<PathBuf>,
        refreshes: Rc<Cell<usize>>,
        fail_refresh: Rc<Cell<bool>>,
    }

    impl TestSource {
        fn new(watch_targets: Vec<PathBuf>) -> (Self, TestSourceHandle) {
            let queue = Rc::new(RefCell::new(VecDeque::new()));
            let refreshes = Rc::new(Cell::new(0));
            let fail_refresh = Rc::new(Cell::new(false));
            (
                Self {
                    queue: Rc::clone(&queue),
                    watch_targets,
                    refreshes: Rc::clone(&refreshes),
                    fail_refresh: Rc::clone(&fail_refresh),
                },
                TestSourceHandle {
                    queue,
                    refreshes,
                    fail_refresh,
                },
            )
        }
    }

    impl WatcherMessageSource for TestSource {
        type Error = TestSourceError;

        fn try_recv_message(&mut self) -> Result<Option<NotifySourceMessage>, Self::Error> {
            self.queue.borrow_mut().pop_front().transpose()
        }

        fn watch_target_paths(&self) -> Vec<PathBuf> {
            self.watch_targets.clone()
        }

        fn validate_watch_targets(&self) -> Result<(), Self::Error> {
            for path in &self.watch_targets {
                match fs::metadata(path) {
                    Ok(metadata) if metadata.is_dir() => {}
                    Ok(_) | Err(_) => return Err(TestSourceError::Unavailable(path.clone())),
                }
            }
            Ok(())
        }

        fn refresh_watches(&mut self) -> Result<(), Self::Error> {
            self.validate_watch_targets()?;
            if self.fail_refresh.get() {
                return Err(TestSourceError::Refresh);
            }
            self.refreshes.set(self.refreshes.get() + 1);
            Ok(())
        }
    }

    #[derive(Default)]
    struct RecordingExecutor {
        batches: Vec<NormalizedWatcherBatch>,
    }

    impl WatcherBatchExecutor for RecordingExecutor {
        type Error = Infallible;

        fn execute(&mut self, batch: NormalizedWatcherBatch) -> Result<(), Self::Error> {
            self.batches.push(batch);
            Ok(())
        }
    }

    #[derive(Debug, Clone, Copy, PartialEq, Eq)]
    struct ReportCounts {
        unchanged: usize,
        created: usize,
        modified: usize,
        deleted: usize,
    }

    impl From<ChangeApplicationReport> for ReportCounts {
        fn from(report: ChangeApplicationReport) -> Self {
            Self {
                unchanged: report.unchanged,
                created: report.created,
                modified: report.modified,
                deleted: report.deleted,
            }
        }
    }

    struct ReportingPhase6Executor<'a> {
        indexer: &'a Indexer<OrgizeAdapter>,
        connection: &'a mut Connection,
        config: &'a Config,
        reports: Vec<ReportCounts>,
    }

    impl WatcherBatchExecutor for ReportingPhase6Executor<'_> {
        type Error = WatcherExecutionError;

        fn execute(&mut self, batch: NormalizedWatcherBatch) -> Result<(), Self::Error> {
            let result = match batch {
                NormalizedWatcherBatch::Candidates(paths) => self
                    .indexer
                    .reconcile_candidate_paths(self.connection, self.config, paths),
                NormalizedWatcherBatch::Reconcile => self
                    .indexer
                    .reconcile_configured_sources(self.connection, self.config),
            }
            .map_err(WatcherExecutionError::Indexer)?;

            match result {
                ChangeApplicationResult::Applied(report) => {
                    self.reports.push(report.into());
                    Ok(())
                }
                ChangeApplicationResult::Rejected(rejection) => {
                    Err(WatcherExecutionError::Rejected(rejection))
                }
            }
        }
    }

    struct CreateDuringStartupExecutor<'a> {
        inner: ReportingPhase6Executor<'a>,
        create_path: PathBuf,
        source: TestSourceHandle,
        created: bool,
    }

    impl WatcherBatchExecutor for CreateDuringStartupExecutor<'_> {
        type Error = WatcherExecutionError;

        fn execute(&mut self, batch: NormalizedWatcherBatch) -> Result<(), Self::Error> {
            self.inner.execute(batch)?;
            if !self.created {
                write_file(&self.create_path, "* During\n");
                self.source.push(path_input(
                    WatcherPathEventKind::Create,
                    self.create_path.clone(),
                ));
                self.created = true;
            }
            Ok(())
        }
    }

    fn path_input(kind: WatcherPathEventKind, path: PathBuf) -> NotifySourceMessage {
        NotifySourceMessage::Input(WatcherInput::Paths {
            kind,
            paths: vec![path],
        })
    }

    #[test]
    fn startup_reconciles_a_file_changed_while_watcher_was_stopped() {
        let test_dir = TestDir::new("changed-before-startup");
        let note = test_dir.path().join("note.org");
        let config = explicit_config(&test_dir, vec![note.clone()]);
        write_file(&note, "* Original\n");
        let indexer = Indexer::new(OrgizeAdapter::new());
        let mut connection = open_database(&config);
        indexer
            .rebuild(&mut connection, &config)
            .expect("initial rebuild should succeed");
        write_file(&note, "* Changed\n");
        let (source, _) = TestSource::new(vec![test_dir.path().to_path_buf()]);
        let mut executor = ReportingPhase6Executor {
            indexer: &indexer,
            connection: &mut connection,
            config: &config,
            reports: Vec::new(),
        };

        let runtime =
            WatcherRuntime::start_registered(source, &config, Instant::now(), &mut executor)
                .expect("watcher startup should reconcile");

        assert_eq!(runtime.state(), WatcherRuntimeState::Running);
        assert_eq!(executor.reports[0].modified, 1);
        assert_eq!(heading_titles(&connection), vec!["Changed"]);
    }

    #[test]
    fn startup_reconciles_a_file_deleted_while_watcher_was_stopped() {
        let test_dir = TestDir::new("deleted-before-startup");
        let first = test_dir.path().join("first.org");
        let second = test_dir.path().join("second.org");
        let config = explicit_config(&test_dir, vec![first.clone(), second.clone()]);
        write_file(&first, "* First\n");
        write_file(&second, "* Second\n");
        let indexer = Indexer::new(OrgizeAdapter::new());
        let mut connection = open_database(&config);
        indexer
            .rebuild(&mut connection, &config)
            .expect("initial rebuild should succeed");
        fs::remove_file(&second).expect("second file should be removed");
        let (source, _) = TestSource::new(vec![test_dir.path().to_path_buf()]);
        let mut executor = ReportingPhase6Executor {
            indexer: &indexer,
            connection: &mut connection,
            config: &config,
            reports: Vec::new(),
        };

        WatcherRuntime::start_registered(source, &config, Instant::now(), &mut executor)
            .expect("watcher startup should reconcile");

        assert_eq!(executor.reports[0].deleted, 1);
        assert_eq!(heading_titles(&connection), vec!["First"]);
    }

    #[test]
    fn startup_reconciles_a_file_created_while_watcher_was_stopped() {
        let test_dir = TestDir::new("created-before-startup");
        let first = test_dir.path().join("first.org");
        let second = test_dir.path().join("second.org");
        let config = explicit_config(&test_dir, vec![first.clone(), second.clone()]);
        write_file(&first, "* First\n");
        let indexer = Indexer::new(OrgizeAdapter::new());
        let mut connection = open_database(&config);
        indexer
            .rebuild(&mut connection, &config)
            .expect("initial rebuild should succeed");
        write_file(&second, "* Second\n");
        let (source, _) = TestSource::new(vec![test_dir.path().to_path_buf()]);
        let mut executor = ReportingPhase6Executor {
            indexer: &indexer,
            connection: &mut connection,
            config: &config,
            reports: Vec::new(),
        };

        WatcherRuntime::start_registered(source, &config, Instant::now(), &mut executor)
            .expect("watcher startup should reconcile");

        assert_eq!(executor.reports[0].created, 1);
        assert_eq!(heading_titles(&connection), vec!["First", "Second"]);
    }

    #[test]
    fn no_change_startup_uses_the_phase_6_unchanged_path() {
        let test_dir = TestDir::new("unchanged-startup");
        let note = test_dir.path().join("note.org");
        let config = explicit_config(&test_dir, vec![note.clone()]);
        write_file(&note, "* Original\n");
        let indexer = Indexer::new(OrgizeAdapter::new());
        let mut connection = open_database(&config);
        indexer
            .rebuild(&mut connection, &config)
            .expect("initial rebuild should succeed");
        let (source, _) = TestSource::new(vec![test_dir.path().to_path_buf()]);
        let mut executor = ReportingPhase6Executor {
            indexer: &indexer,
            connection: &mut connection,
            config: &config,
            reports: Vec::new(),
        };

        WatcherRuntime::start_registered(source, &config, Instant::now(), &mut executor)
            .expect("watcher startup should reconcile");

        assert_eq!(executor.reports[0].unchanged, 1);
        assert_eq!(executor.reports[0].created, 0);
        assert_eq!(executor.reports[0].modified, 0);
        assert_eq!(executor.reports[0].deleted, 0);
    }

    #[test]
    fn file_created_during_startup_is_retained_for_the_next_batch() {
        let test_dir = TestDir::new("file-created-during-startup");
        let config = recursive_config(&test_dir);
        let root = config.dirs[0].path.clone();
        let existing = root.join("existing.org");
        let created = root.join("during.org");
        write_file(&existing, "* Existing\n");
        let indexer = Indexer::new(OrgizeAdapter::new());
        let mut connection = open_database(&config);
        indexer
            .rebuild(&mut connection, &config)
            .expect("initial rebuild should succeed");
        let (source, handle) = TestSource::new(vec![root]);
        let now = Instant::now();
        let mut executor = CreateDuringStartupExecutor {
            inner: ReportingPhase6Executor {
                indexer: &indexer,
                connection: &mut connection,
                config: &config,
                reports: Vec::new(),
            },
            create_path: created,
            source: handle,
            created: false,
        };

        let mut runtime = WatcherRuntime::start_registered(source, &config, now, &mut executor)
            .expect("startup should retain the created-file event");

        assert_eq!(executor.inner.reports[0].unchanged, 1);
        let deadline = runtime
            .next_deadline()
            .expect("created file should be pending");
        let report = runtime
            .process_available(deadline, &mut executor)
            .expect("created file should execute after startup");
        assert_eq!(report.execution_status, WatcherExecutionStatus::Executed);
        assert_eq!(executor.inner.reports[1].created, 1);
        assert_eq!(heading_titles(&connection), vec!["During", "Existing"]);
    }

    #[test]
    fn repeated_rescan_requests_collapse_into_one_reconciliation() {
        let test_dir = TestDir::new("repeated-rescan");
        let config = recursive_config(&test_dir);
        let root = config.dirs[0].path.clone();
        let (source, handle) = TestSource::new(vec![root]);
        handle.push(NotifySourceMessage::Input(WatcherInput::Uncertain(
            WatcherUncertainty::Rescan,
        )));
        handle.push(NotifySourceMessage::Input(WatcherInput::Uncertain(
            WatcherUncertainty::Rescan,
        )));
        let now = Instant::now();
        let mut executor = RecordingExecutor::default();
        let mut runtime = WatcherRuntime::start_registered(source, &config, now, &mut executor)
            .expect("startup should succeed");

        assert_eq!(runtime.recovery_context().uncertainty_count(), 2);
        let deadline = runtime
            .next_deadline()
            .expect("reconciliation should be pending");
        let report = runtime
            .process_available(deadline, &mut executor)
            .expect("reconciliation should execute");

        assert_eq!(report.execution_status, WatcherExecutionStatus::Executed);
        assert_eq!(executor.batches.len(), 2);
        assert_eq!(executor.batches[1], NormalizedWatcherBatch::Reconcile);
        assert_eq!(runtime.recovery_context().uncertainty_count(), 0);
    }

    #[test]
    fn backend_failures_keep_bounded_diagnostic_context() {
        let test_dir = TestDir::new("backend-failures");
        let config = recursive_config(&test_dir);
        let root = config.dirs[0].path.clone();
        let (source, handle) = TestSource::new(vec![root.clone()]);
        for message in ["first", "second"] {
            handle.push(NotifySourceMessage::BackendFailure(
                NotifyBackendFailure::new(message.to_string(), vec![root.clone()]),
            ));
        }
        let now = Instant::now();
        let mut executor = RecordingExecutor::default();
        let runtime = WatcherRuntime::start_registered(source, &config, now, &mut executor)
            .expect("startup should succeed");

        assert_eq!(runtime.recovery_context().uncertainty_count(), 2);
        assert_eq!(
            runtime
                .recovery_context()
                .latest_backend_failure()
                .expect("latest failure")
                .message(),
            "second"
        );
    }

    #[test]
    fn watched_root_replacement_refreshes_registration_before_recovery() {
        let test_dir = TestDir::new("root-replacement");
        let config = recursive_config(&test_dir);
        let root = config.dirs[0].path.clone();
        let (source, handle) = TestSource::new(vec![root.clone()]);
        let now = Instant::now();
        let mut executor = RecordingExecutor::default();
        let mut runtime = WatcherRuntime::start_registered(source, &config, now, &mut executor)
            .expect("startup should succeed");
        handle.push(path_input(WatcherPathEventKind::Create, root.clone()));

        let queued = runtime
            .process_available(now, &mut executor)
            .expect("root event should queue recovery");
        assert_eq!(queued.execution_status, WatcherExecutionStatus::Idle);
        assert_eq!(
            runtime.recovery_context().latest_watch_target(),
            Some(root.as_path())
        );
        let deadline = runtime.next_deadline().expect("recovery should be pending");
        runtime
            .process_available(deadline, &mut executor)
            .expect("root recovery should execute");

        assert_eq!(handle.refresh_count(), 1);
        assert_eq!(executor.batches[1], NormalizedWatcherBatch::Reconcile);
    }

    #[test]
    fn unavailable_watch_root_terminates_before_phase_6_can_delete_state() {
        let test_dir = TestDir::new("unavailable-root");
        let config = recursive_config(&test_dir);
        let root = config.dirs[0].path.clone();
        let note = root.join("note.org");
        write_file(&note, "* Original\n");
        let indexer = Indexer::new(OrgizeAdapter::new());
        let mut connection = open_database(&config);
        indexer
            .rebuild(&mut connection, &config)
            .expect("initial rebuild should succeed");
        let (source, handle) = TestSource::new(vec![root.clone()]);
        let now = Instant::now();
        {
            let mut executor = Phase6WatcherExecutor::new(&indexer, &mut connection, &config);
            let mut runtime = WatcherRuntime::start_registered(source, &config, now, &mut executor)
                .expect("startup should succeed");
            fs::remove_dir_all(&root).expect("watch root should become unavailable");
            handle.push(path_input(WatcherPathEventKind::Remove, root));
            runtime
                .process_available(now, &mut executor)
                .expect("root removal should queue recovery");
            let deadline = runtime.next_deadline().expect("recovery should be pending");
            let error = runtime
                .process_available(deadline, &mut executor)
                .expect_err("unavailable root should terminate recovery");
            assert!(matches!(error, WatcherRuntimeError::Source(_)));
            assert_eq!(runtime.state(), WatcherRuntimeState::Terminated);
        }

        let file_count: i64 = connection
            .query_row("SELECT COUNT(*) FROM files", [], |row| row.get(0))
            .expect("file count should read");
        assert_eq!(file_count, 1);
    }

    #[test]
    fn failed_recovery_terminates_and_preserves_last_committed_state() {
        let test_dir = TestDir::new("failed-recovery-rollback");
        let note = test_dir.path().join("note.org");
        let config = explicit_config(&test_dir, vec![note.clone()]);
        write_file(&note, "* Original\n");
        let indexer = Indexer::new(OrgizeAdapter::new());
        let mut connection = open_database(&config);
        indexer
            .rebuild(&mut connection, &config)
            .expect("initial rebuild should succeed");
        let (source, handle) = TestSource::new(vec![test_dir.path().to_path_buf()]);
        let now = Instant::now();
        {
            let mut executor = Phase6WatcherExecutor::new(&indexer, &mut connection, &config);
            let mut runtime = WatcherRuntime::start_registered(source, &config, now, &mut executor)
                .expect("startup should succeed");
            fs::write(&note, b"* Invalid\n\xff").expect("invalid source should be written");
            handle.push(NotifySourceMessage::Input(WatcherInput::Uncertain(
                WatcherUncertainty::Rescan,
            )));
            runtime
                .process_available(now, &mut executor)
                .expect("rescan should queue recovery");
            let deadline = runtime.next_deadline().expect("recovery should be pending");
            let error = runtime
                .process_available(deadline, &mut executor)
                .expect_err("failed Phase 6 recovery should terminate");
            assert!(matches!(error, WatcherRuntimeError::Execution { .. }));
            assert_eq!(runtime.state(), WatcherRuntimeState::Terminated);
        }

        assert_eq!(heading_titles(&connection), vec!["Original"]);
    }

    #[test]
    fn source_receive_failure_terminates_without_executing_another_batch() {
        let test_dir = TestDir::new("source-receive-failure");
        let config = recursive_config(&test_dir);
        let root = config.dirs[0].path.clone();
        let (source, handle) = TestSource::new(vec![root]);
        let now = Instant::now();
        let mut executor = RecordingExecutor::default();
        let mut runtime = WatcherRuntime::start_registered(source, &config, now, &mut executor)
            .expect("startup should succeed");
        handle.push_error();

        let error = runtime
            .process_available(now, &mut executor)
            .expect_err("source failure should terminate");

        assert!(matches!(error, WatcherRuntimeError::Source(_)));
        assert_eq!(runtime.state(), WatcherRuntimeState::Terminated);
        assert_eq!(executor.batches, vec![NormalizedWatcherBatch::Reconcile]);
    }

    #[test]
    fn refresh_failure_terminates_before_reconciliation() {
        let test_dir = TestDir::new("refresh-failure");
        let config = recursive_config(&test_dir);
        let root = config.dirs[0].path.clone();
        let (source, handle) = TestSource::new(vec![root.clone()]);
        let now = Instant::now();
        let mut executor = RecordingExecutor::default();
        let mut runtime = WatcherRuntime::start_registered(source, &config, now, &mut executor)
            .expect("startup should succeed");
        handle.fail_refresh();
        handle.push(path_input(WatcherPathEventKind::Rename, root));
        runtime
            .process_available(now, &mut executor)
            .expect("root event should queue recovery");
        let deadline = runtime.next_deadline().expect("recovery should be pending");

        let error = runtime
            .process_available(deadline, &mut executor)
            .expect_err("refresh failure should terminate");

        assert!(matches!(error, WatcherRuntimeError::Source(_)));
        assert_eq!(executor.batches, vec![NormalizedWatcherBatch::Reconcile]);
    }

    #[test]
    fn cycle_report_counts_backend_messages_without_growing_work_queue() {
        let test_dir = TestDir::new("cycle-report");
        let config = recursive_config(&test_dir);
        let root = config.dirs[0].path.clone();
        let (source, handle) = TestSource::new(vec![root.clone()]);
        let now = Instant::now();
        let mut executor = RecordingExecutor::default();
        let mut runtime = WatcherRuntime::start_registered(source, &config, now, &mut executor)
            .expect("startup should succeed");
        handle.push(NotifySourceMessage::BackendFailure(
            NotifyBackendFailure::new("overflow".to_string(), vec![root]),
        ));

        let report: WatcherCycleReport = runtime
            .process_available(now, &mut executor)
            .expect("backend failure should queue recovery");

        assert_eq!(report.source_messages, 1);
        assert_eq!(report.backend_failures, 1);
        assert_eq!(report.execution_status, WatcherExecutionStatus::Idle);
    }

    #[test]
    fn terminated_runtime_rejects_further_processing() {
        let test_dir = TestDir::new("terminated-runtime");
        let config = recursive_config(&test_dir);
        let root = config.dirs[0].path.clone();
        let (source, handle) = TestSource::new(vec![root]);
        let now = Instant::now();
        let mut executor = RecordingExecutor::default();
        let mut runtime = WatcherRuntime::start_registered(source, &config, now, &mut executor)
            .expect("startup should succeed");
        handle.push_error();
        runtime
            .process_available(now, &mut executor)
            .expect_err("source failure should terminate runtime");

        let error = runtime
            .process_available(now + Duration::from_secs(1), &mut executor)
            .expect_err("terminated runtime should reject work");

        assert!(matches!(error, WatcherRuntimeError::Terminated));
    }
}
