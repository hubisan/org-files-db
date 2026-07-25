use std::{
    error::Error,
    fmt, fs, io,
    path::{Path, PathBuf},
    sync::mpsc::{self, Receiver, RecvTimeoutError, TryRecvError},
    time::{Duration, Instant},
};

use notify::{
    event::ModifyKind, recommended_watcher, Event, EventKind, RecommendedWatcher, RecursiveMode,
    Watcher,
};

use crate::{
    config::{normalize_syntactic_path, Config},
    file_identity::FileIdentity,
    watcher::{WatcherInput, WatcherPathEventKind, WatcherUncertainty},
};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum NotifyWatchMode {
    NonRecursive,
    Recursive,
}

impl NotifyWatchMode {
    fn as_notify_mode(self) -> RecursiveMode {
        match self {
            Self::NonRecursive => RecursiveMode::NonRecursive,
            Self::Recursive => RecursiveMode::Recursive,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct NotifyWatchTarget {
    path: PathBuf,
    mode: NotifyWatchMode,
}

impl NotifyWatchTarget {
    pub(crate) fn path(&self) -> &Path {
        &self.path
    }

    pub(crate) fn mode(&self) -> NotifyWatchMode {
        self.mode
    }

    fn covers(&self, other: &Self) -> bool {
        if self.path == other.path {
            return self.mode == NotifyWatchMode::Recursive
                || other.mode == NotifyWatchMode::NonRecursive;
        }

        self.mode == NotifyWatchMode::Recursive && other.path.starts_with(&self.path)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct NotifyBackendFailure {
    message: String,
    paths: Vec<PathBuf>,
}

impl NotifyBackendFailure {
    pub(crate) fn new(message: String, paths: Vec<PathBuf>) -> Self {
        Self { message, paths }
    }

    pub(crate) fn message(&self) -> &str {
        &self.message
    }

    pub(crate) fn paths(&self) -> &[PathBuf] {
        &self.paths
    }

    pub(crate) fn recovery_input(&self) -> WatcherInput {
        WatcherInput::Uncertain(WatcherUncertainty::DroppedEvents)
    }
}

impl fmt::Display for NotifyBackendFailure {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "notify backend error: {}", self.message)?;
        if !self.paths.is_empty() {
            write!(f, " (paths:")?;
            for path in &self.paths {
                write!(f, " {}", path.display())?;
            }
            write!(f, ")")?;
        }
        write!(f, "; full reconciliation is required")
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum NotifySourceMessage {
    Input(WatcherInput),
    BackendFailure(NotifyBackendFailure),
}

#[derive(Debug)]
pub(crate) enum NotifyWatcherError {
    InspectWatchPath {
        path: PathBuf,
        source: io::Error,
    },
    WatchPathNotDirectory {
        path: PathBuf,
    },
    MissingExplicitParent {
        path: PathBuf,
    },
    CreateBackend {
        source: notify::Error,
    },
    RegisterWatch {
        path: PathBuf,
        mode: NotifyWatchMode,
        source: notify::Error,
    },
    EventChannelDisconnected,
}

impl fmt::Display for NotifyWatcherError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::InspectWatchPath { path, source } => write!(
                f,
                "failed to inspect notify watch path {}: {}",
                path.display(),
                source
            ),
            Self::WatchPathNotDirectory { path } => write!(
                f,
                "notify watch path is not a directory: {}",
                path.display()
            ),
            Self::MissingExplicitParent { path } => write!(
                f,
                "configured explicit file has no parent directory to watch: {}",
                path.display()
            ),
            Self::CreateBackend { source } => {
                write!(f, "failed to create notify filesystem watcher: {source}")
            }
            Self::RegisterWatch { path, mode, source } => write!(
                f,
                "failed to register {:?} notify watch for {}: {}",
                mode,
                path.display(),
                source
            ),
            Self::EventChannelDisconnected => {
                write!(f, "notify event channel disconnected unexpectedly")
            }
        }
    }
}

impl Error for NotifyWatcherError {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        match self {
            Self::InspectWatchPath { source, .. } => Some(source),
            Self::CreateBackend { source } | Self::RegisterWatch { source, .. } => Some(source),
            Self::WatchPathNotDirectory { .. }
            | Self::MissingExplicitParent { .. }
            | Self::EventChannelDisconnected => None,
        }
    }
}

pub(crate) struct NotifyWatcherSource {
    _watcher: RecommendedWatcher,
    receiver: Receiver<notify::Result<Event>>,
    watch_targets: Vec<NotifyWatchTarget>,
}

impl NotifyWatcherSource {
    pub(crate) fn from_config(config: &Config) -> Result<Self, NotifyWatcherError> {
        let watch_targets = notify_watch_targets(config)?;
        let (sender, receiver) = mpsc::channel();
        let watcher = create_registered_watcher(sender, &watch_targets)?;

        Ok(Self {
            _watcher: watcher,
            receiver,
            watch_targets,
        })
    }

    pub(crate) fn watch_targets(&self) -> &[NotifyWatchTarget] {
        &self.watch_targets
    }

    pub(crate) fn validate_watch_targets(&self) -> Result<(), NotifyWatcherError> {
        validate_watch_targets(&self.watch_targets)
    }

    pub(crate) fn refresh_watches(&mut self) -> Result<(), NotifyWatcherError> {
        validate_watch_targets(&self.watch_targets)?;
        let (sender, receiver) = mpsc::channel();
        let watcher = create_registered_watcher(sender, &self.watch_targets)?;
        self._watcher = watcher;
        self.receiver = receiver;
        Ok(())
    }

    pub(crate) fn try_recv(&self) -> Result<Option<NotifySourceMessage>, NotifyWatcherError> {
        loop {
            match self.receiver.try_recv() {
                Ok(result) => {
                    if let Some(message) = translate_notify_result(result) {
                        return Ok(Some(message));
                    }
                }
                Err(TryRecvError::Empty) => return Ok(None),
                Err(TryRecvError::Disconnected) => {
                    return Err(NotifyWatcherError::EventChannelDisconnected);
                }
            }
        }
    }

    pub(crate) fn recv_timeout(
        &self,
        timeout: Duration,
    ) -> Result<Option<NotifySourceMessage>, NotifyWatcherError> {
        let started = Instant::now();
        let mut remaining = timeout;

        loop {
            match self.receiver.recv_timeout(remaining) {
                Ok(result) => {
                    if let Some(message) = translate_notify_result(result) {
                        return Ok(Some(message));
                    }
                }
                Err(RecvTimeoutError::Timeout) => return Ok(None),
                Err(RecvTimeoutError::Disconnected) => {
                    return Err(NotifyWatcherError::EventChannelDisconnected);
                }
            }

            let elapsed = started.elapsed();
            let Some(next_remaining) = timeout.checked_sub(elapsed) else {
                return Ok(None);
            };
            if next_remaining.is_zero() {
                return Ok(None);
            }
            remaining = next_remaining;
        }
    }
}

pub(crate) fn translate_notify_result(
    result: notify::Result<Event>,
) -> Option<NotifySourceMessage> {
    match result {
        Ok(event) => translate_notify_event(event).map(NotifySourceMessage::Input),
        Err(error) => Some(NotifySourceMessage::BackendFailure(notify_backend_failure(
            error,
        ))),
    }
}

fn translate_notify_event(event: Event) -> Option<WatcherInput> {
    if event.need_rescan() {
        return Some(WatcherInput::Uncertain(WatcherUncertainty::Rescan));
    }

    let kind = match event.kind {
        EventKind::Access(_) => return None,
        EventKind::Create(_) => WatcherPathEventKind::Create,
        EventKind::Modify(ModifyKind::Name(_)) => WatcherPathEventKind::Rename,
        EventKind::Modify(ModifyKind::Metadata(_)) => WatcherPathEventKind::Metadata,
        EventKind::Modify(_) => WatcherPathEventKind::Modify,
        EventKind::Remove(_) => WatcherPathEventKind::Remove,
        EventKind::Any | EventKind::Other => WatcherPathEventKind::Other,
    };

    if event.paths.is_empty() {
        return Some(WatcherInput::Uncertain(WatcherUncertainty::Other));
    }

    Some(WatcherInput::Paths {
        kind,
        paths: event.paths,
    })
}

fn notify_backend_failure(error: notify::Error) -> NotifyBackendFailure {
    let message = error.to_string();
    let paths = error.paths;
    NotifyBackendFailure::new(message, paths)
}

fn notify_watch_targets(config: &Config) -> Result<Vec<NotifyWatchTarget>, NotifyWatcherError> {
    let mut targets = Vec::new();

    for configured_dir in &config.dirs {
        let path = normalize_syntactic_path(configured_dir.path.clone());
        validate_watch_directory(&path)?;
        insert_watch_target(
            &mut targets,
            NotifyWatchTarget {
                path,
                mode: if configured_dir.recursive {
                    NotifyWatchMode::Recursive
                } else {
                    NotifyWatchMode::NonRecursive
                },
            },
        );
    }

    for explicit_file in &config.files {
        let logical_file = normalize_syntactic_path(explicit_file.clone());
        let parent =
            logical_file
                .parent()
                .ok_or_else(|| NotifyWatcherError::MissingExplicitParent {
                    path: logical_file.clone(),
                })?;
        let parent = normalize_syntactic_path(parent.to_path_buf());
        validate_watch_directory(&parent)?;
        insert_watch_target(
            &mut targets,
            NotifyWatchTarget {
                path: parent,
                mode: NotifyWatchMode::NonRecursive,
            },
        );
    }

    targets.sort_by_key(|target| FileIdentity::from_canonical_path(target.path()));
    Ok(targets)
}

fn validate_watch_directory(path: &Path) -> Result<(), NotifyWatcherError> {
    let metadata = fs::metadata(path).map_err(|source| NotifyWatcherError::InspectWatchPath {
        path: path.to_path_buf(),
        source,
    })?;
    if !metadata.is_dir() {
        return Err(NotifyWatcherError::WatchPathNotDirectory {
            path: path.to_path_buf(),
        });
    }
    Ok(())
}

fn insert_watch_target(targets: &mut Vec<NotifyWatchTarget>, target: NotifyWatchTarget) {
    if targets.iter().any(|existing| existing.covers(&target)) {
        return;
    }

    targets.retain(|existing| !target.covers(existing));
    targets.push(target);
}

fn create_registered_watcher(
    sender: mpsc::Sender<notify::Result<Event>>,
    targets: &[NotifyWatchTarget],
) -> Result<RecommendedWatcher, NotifyWatcherError> {
    let mut watcher = recommended_watcher(sender)
        .map_err(|source| NotifyWatcherError::CreateBackend { source })?;
    register_watch_targets(targets, |target| {
        watcher.watch(target.path(), target.mode().as_notify_mode())
    })?;
    Ok(watcher)
}

fn validate_watch_targets(targets: &[NotifyWatchTarget]) -> Result<(), NotifyWatcherError> {
    for target in targets {
        validate_watch_directory(target.path())?;
    }
    Ok(())
}

fn register_watch_targets(
    targets: &[NotifyWatchTarget],
    mut register: impl FnMut(&NotifyWatchTarget) -> notify::Result<()>,
) -> Result<(), NotifyWatcherError> {
    for target in targets {
        register(target).map_err(|source| NotifyWatcherError::RegisterWatch {
            path: target.path.clone(),
            mode: target.mode,
            source,
        })?;
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::{
        notify_watch_targets, register_watch_targets, translate_notify_result, NotifySourceMessage,
        NotifyWatchMode, NotifyWatcherError, NotifyWatcherSource,
    };
    use crate::{
        config::Config,
        watcher::{
            NormalizedWatcherBatch, WatcherBatchNormalizer, WatcherInput, WatcherPathEventKind,
            WatcherUncertainty,
        },
    };
    use notify::{
        event::{
            AccessKind, AccessMode, CreateKind, DataChange, Flag, MetadataKind, ModifyKind,
            RemoveKind, RenameMode,
        },
        Event, EventKind,
    };
    use std::{
        fs,
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
                "org-files-db-notify-tests-{}-{}-{}",
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

    fn input(message: Option<NotifySourceMessage>) -> WatcherInput {
        match message.expect("translated message") {
            NotifySourceMessage::Input(input) => input,
            NotifySourceMessage::BackendFailure(error) => {
                panic!("expected watcher input, got {error}")
            }
        }
    }

    #[test]
    fn translates_create_modify_metadata_remove_and_other_events() {
        let path = PathBuf::from("/tmp/note.org");
        let cases = [
            (
                EventKind::Create(CreateKind::File),
                WatcherPathEventKind::Create,
            ),
            (
                EventKind::Modify(ModifyKind::Data(DataChange::Content)),
                WatcherPathEventKind::Modify,
            ),
            (
                EventKind::Modify(ModifyKind::Metadata(MetadataKind::WriteTime)),
                WatcherPathEventKind::Metadata,
            ),
            (
                EventKind::Remove(RemoveKind::File),
                WatcherPathEventKind::Remove,
            ),
            (EventKind::Other, WatcherPathEventKind::Other),
        ];

        for (event_kind, expected_kind) in cases {
            let event = Event::new(event_kind).add_path(path.clone());
            assert_eq!(
                input(translate_notify_result(Ok(event))),
                WatcherInput::Paths {
                    kind: expected_kind,
                    paths: vec![path.clone()],
                }
            );
        }
    }

    #[test]
    fn translates_complete_rename_with_both_paths_in_backend_order() {
        let old = PathBuf::from("/tmp/old.org");
        let new = PathBuf::from("/tmp/new.org");
        let event = Event::new(EventKind::Modify(ModifyKind::Name(RenameMode::Both)))
            .add_path(old.clone())
            .add_path(new.clone());

        assert_eq!(
            input(translate_notify_result(Ok(event))),
            WatcherInput::Paths {
                kind: WatcherPathEventKind::Rename,
                paths: vec![old, new],
            }
        );
    }

    #[test]
    fn translates_incomplete_rename_for_normalizer_reconciliation() {
        let old = PathBuf::from("/tmp/old.org");
        let event =
            Event::new(EventKind::Modify(ModifyKind::Name(RenameMode::From))).add_path(old.clone());

        assert_eq!(
            input(translate_notify_result(Ok(event))),
            WatcherInput::Paths {
                kind: WatcherPathEventKind::Rename,
                paths: vec![old],
            }
        );
    }

    #[test]
    fn rescan_flag_supersedes_event_paths() {
        let event = Event::new(EventKind::Modify(ModifyKind::Any))
            .add_path(PathBuf::from("/tmp/note.org"))
            .set_flag(Flag::Rescan);

        assert_eq!(
            input(translate_notify_result(Ok(event))),
            WatcherInput::Uncertain(WatcherUncertainty::Rescan)
        );
    }

    #[test]
    fn ignores_non_mutating_access_events() {
        let event = Event::new(EventKind::Access(AccessKind::Open(AccessMode::Any)))
            .add_path(PathBuf::from("/tmp/note.org"));

        assert_eq!(translate_notify_result(Ok(event)), None);
    }

    #[test]
    fn pathless_unknown_event_requests_reconciliation() {
        assert_eq!(
            input(translate_notify_result(Ok(Event::new(EventKind::Any)))),
            WatcherInput::Uncertain(WatcherUncertainty::Other)
        );
    }

    #[test]
    fn backend_error_keeps_context_and_requests_reconciliation() {
        let path = PathBuf::from("/tmp/notes");
        let error = notify::Error::generic("backend queue overflow").add_path(path.clone());
        let Some(NotifySourceMessage::BackendFailure(failure)) =
            translate_notify_result(Err(error))
        else {
            panic!("expected backend failure");
        };

        assert!(failure.message().contains("backend queue overflow"));
        assert_eq!(failure.paths(), &[path]);
        assert_eq!(
            failure.recovery_input(),
            WatcherInput::Uncertain(WatcherUncertainty::DroppedEvents)
        );
        assert!(failure.to_string().contains("full reconciliation"));
    }

    #[test]
    fn plans_recursive_roots_and_explicit_file_parents_without_duplicate_watches() {
        let test_dir = TestDir::new("targets");
        fs::create_dir_all(test_dir.path().join("notes/nested")).expect("notes root");
        write_file(
            &test_dir.path().join("notes/nested/explicit.org"),
            "* Explicit\n",
        );
        fs::create_dir_all(test_dir.path().join("outside")).expect("outside root");
        write_file(
            &test_dir.path().join("outside/standalone.org"),
            "* Standalone\n",
        );
        let config = load_config(
            &test_dir,
            "db_path = \"db.sqlite\"\nfiles = [\"notes/nested/explicit.org\", \"outside/standalone.org\"]\n[[dirs]]\npath = \"notes\"\nrecursive = true\n[search]\nfts5_enabled = false\n",
        );

        let targets = notify_watch_targets(&config).expect("watch targets");
        assert_eq!(targets.len(), 2);
        assert!(targets.iter().any(|target| {
            target.path() == test_dir.path().join("notes")
                && target.mode() == NotifyWatchMode::Recursive
        }));
        assert!(targets.iter().any(|target| {
            target.path() == test_dir.path().join("outside")
                && target.mode() == NotifyWatchMode::NonRecursive
        }));
    }

    #[test]
    fn recursive_target_replaces_existing_nested_non_recursive_target() {
        let test_dir = TestDir::new("target-order");
        fs::create_dir_all(test_dir.path().join("notes/nested")).expect("nested root");
        write_file(
            &test_dir.path().join("notes/nested/explicit.org"),
            "* Explicit\n",
        );
        let config = load_config(
            &test_dir,
            "db_path = \"db.sqlite\"\nfiles = [\"notes/nested/explicit.org\"]\n[[dirs]]\npath = \"notes\"\nrecursive = true\n[search]\nfts5_enabled = false\n",
        );

        let targets = notify_watch_targets(&config).expect("watch targets");
        assert_eq!(targets.len(), 1);
        assert_eq!(targets[0].path(), test_dir.path().join("notes"));
        assert_eq!(targets[0].mode(), NotifyWatchMode::Recursive);
    }

    #[test]
    fn inaccessible_watch_root_reports_affected_path() {
        let test_dir = TestDir::new("missing-root");
        let config = load_config(
            &test_dir,
            "db_path = \"db.sqlite\"\n[[dirs]]\npath = \"missing\"\nrecursive = true\n[search]\nfts5_enabled = false\n",
        );

        let error = notify_watch_targets(&config).expect_err("missing root should fail");
        assert!(matches!(
            error,
            NotifyWatcherError::InspectWatchPath { ref path, .. }
                if path == &test_dir.path().join("missing")
        ));
        assert!(error.to_string().contains("missing"));
    }

    #[test]
    fn watch_registration_failure_reports_target_and_mode() {
        let test_dir = TestDir::new("registration-error");
        fs::create_dir_all(test_dir.path().join("notes")).expect("notes root");
        let config = load_config(
            &test_dir,
            "db_path = \"db.sqlite\"\n[[dirs]]\npath = \"notes\"\nrecursive = true\n[search]\nfts5_enabled = false\n",
        );
        let targets = notify_watch_targets(&config).expect("watch targets");

        let error = register_watch_targets(&targets, |_target| {
            Err(notify::Error::generic("planned registration failure"))
        })
        .expect_err("registration should fail");

        assert!(matches!(
            error,
            NotifyWatcherError::RegisterWatch {
                ref path,
                mode: NotifyWatchMode::Recursive,
                ..
            } if path == &test_dir.path().join("notes")
        ));
    }

    #[test]
    fn translated_unrelated_and_sqlite_events_are_filtered_by_phase_6_normalizer() {
        let test_dir = TestDir::new("phase6-filter");
        fs::create_dir_all(test_dir.path().join("notes")).expect("notes root");
        let config = load_config(
            &test_dir,
            "db_path = \"notes/db.sqlite\"\n[[dirs]]\npath = \"notes\"\nrecursive = true\n[search]\nfts5_enabled = false\n",
        );
        let normalizer = WatcherBatchNormalizer::from_config(&config).expect("normalizer");
        let inputs = [
            input(translate_notify_result(Ok(Event::new(EventKind::Modify(
                ModifyKind::Any,
            ))
            .add_path(test_dir.path().join("notes/db.sqlite"))))),
            input(translate_notify_result(Ok(Event::new(EventKind::Modify(
                ModifyKind::Any,
            ))
            .add_path(test_dir.path().join("notes/db.sqlite-wal"))))),
            input(translate_notify_result(Ok(Event::new(EventKind::Modify(
                ModifyKind::Any,
            ))
            .add_path(test_dir.path().join("outside.org"))))),
        ];

        assert_eq!(
            normalizer.normalize(inputs),
            NormalizedWatcherBatch::Candidates(Vec::new())
        );
    }

    #[test]
    fn watch_target_validation_reports_a_removed_root() {
        let test_dir = TestDir::new("removed-watch-root");
        let root = test_dir.path().join("notes");
        fs::create_dir_all(&root).expect("notes root");
        let config = load_config(
            &test_dir,
            "db_path = \"db.sqlite\"\n[[dirs]]\npath = \"notes\"\nrecursive = true\n[search]\nfts5_enabled = false\n",
        );
        let source = NotifyWatcherSource::from_config(&config).expect("notify source");
        fs::remove_dir_all(&root).expect("watch root should be removed");

        let error = source
            .validate_watch_targets()
            .expect_err("removed root should fail validation");

        assert!(matches!(
            error,
            NotifyWatcherError::InspectWatchPath { path, .. } if path == root
        ));
    }

    #[test]
    fn recommended_backend_observes_created_file_below_recursive_root() {
        let test_dir = TestDir::new("real-backend");
        fs::create_dir_all(test_dir.path().join("notes")).expect("notes root");
        let config = load_config(
            &test_dir,
            "db_path = \"db.sqlite\"\n[[dirs]]\npath = \"notes\"\nrecursive = true\n[search]\nfts5_enabled = false\n",
        );
        let mut source = NotifyWatcherSource::from_config(&config).expect("notify source");
        assert_eq!(source.watch_targets().len(), 1);
        source
            .refresh_watches()
            .expect("notify registrations should refresh");
        let normalizer = WatcherBatchNormalizer::from_config(&config).expect("normalizer");
        let note = test_dir.path().join("notes/new.org");
        write_file(&note, "* New\n");
        let canonical_note = fs::canonicalize(&note).expect("canonical note");
        let deadline = Instant::now() + Duration::from_secs(5);

        while Instant::now() < deadline {
            let Some(message) = source
                .recv_timeout(Duration::from_millis(200))
                .expect("receive notify message")
            else {
                continue;
            };

            match message {
                NotifySourceMessage::Input(input) => match normalizer.normalize([input]) {
                    NormalizedWatcherBatch::Candidates(paths) => {
                        if paths.contains(&canonical_note) {
                            return;
                        }
                    }
                    NormalizedWatcherBatch::Reconcile => return,
                },
                NotifySourceMessage::BackendFailure(error) => {
                    panic!("notify backend failed during integration test: {error}");
                }
            }
        }

        panic!(
            "notify backend did not report created Org file {} within timeout",
            note.display()
        );
    }
}
