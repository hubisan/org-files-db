use std::{
    collections::{BTreeMap, BTreeSet},
    error::Error,
    fmt,
    io::{self, Read, Write},
    path::{Path, PathBuf},
    process::{Child, Command, Stdio},
    sync::{
        mpsc::{self, Receiver, RecvTimeoutError, Sender},
        Arc, Mutex,
    },
    thread::{self, JoinHandle},
    time::Duration,
};

use crate::{
    config::Config,
    db::{open_existing_database_read_only, read_index_state},
    presentation::PresentationSpec,
    presentation_view::{
        PresentationViewInclude, PresentationViewOutputMode, PresentationViewRegistryAccessError,
        PresentationViewRegistryHandle, RegisteredPresentationView,
    },
    presentation_view_cache::{
        presentation_view_cache_root, PresentationViewCachePathError, PresentationViewCacheStore,
    },
    query::{
        execute_and_shape_query, parse_query, sqlite_query_validation_options, validate_query,
        QueryExecutionOptions, QueryInclude, QueryOutputMode,
    },
};

const WORKER_POLL_INTERVAL: Duration = Duration::from_millis(10);
const MAX_REBUILD_WORKERS: usize = 3;

pub(crate) fn run_rebuild_worker(
    db_path: &Path,
    cache_root: &Path,
    expected_database_id: &str,
    expected_generation: i64,
    reader: impl Read,
) -> Result<(), String> {
    let view: RegisteredPresentationView =
        serde_json::from_reader(reader).map_err(|source| source.to_string())?;
    let spec_json = serde_json::to_string(&view.definition.presentation_spec)
        .map_err(|source| source.to_string())?;
    let spec = PresentationSpec::parse_json(&spec_json).map_err(|source| source.to_string())?;

    let connection =
        open_existing_database_read_only(db_path).map_err(|source| source.to_string())?;
    connection
        .execute_batch("BEGIN DEFERRED TRANSACTION")
        .map_err(|source| source.to_string())?;
    let state = read_index_state(&connection).map_err(|source| source.to_string())?;
    ensure_worker_target(
        &state.database_id,
        state.generation,
        expected_database_id,
        expected_generation,
    )?;

    let parsed = parse_query(&view.definition.query).map_err(|source| source.to_string())?;
    let validation_options =
        sqlite_query_validation_options(&connection).map_err(|source| source.to_string())?;
    let validated =
        validate_query(parsed, &validation_options).map_err(|source| source.to_string())?;
    let explicit_includes = view
        .definition
        .includes
        .iter()
        .copied()
        .map(query_include)
        .collect::<Vec<_>>();
    let query_includes = spec
        .combined_includes_for_query_target(validated.target, &explicit_includes)
        .map_err(|source| source.to_string())?;
    let options = QueryExecutionOptions {
        output_mode: query_output_mode(view.definition.output),
        includes: query_includes,
        query_timezone: view.definition.query_timezone.clone(),
        now_utc: None,
        restricted_file_paths: None,
    };
    let query_response = execute_and_shape_query(&connection, &validated, &options)
        .map_err(|source| source.to_string())?;
    let response = spec
        .build_response(
            state.database_id.clone(),
            state.generation,
            query_response.results,
        )
        .map_err(|source| source.to_string())?;
    connection
        .execute_batch("COMMIT")
        .map_err(|source| source.to_string())?;

    let mut payload = serde_json::to_vec(&response).map_err(|source| source.to_string())?;
    payload.push(b'\n');

    let latest_connection =
        open_existing_database_read_only(db_path).map_err(|source| source.to_string())?;
    let latest = read_index_state(&latest_connection).map_err(|source| source.to_string())?;
    ensure_worker_target(
        &latest.database_id,
        latest.generation,
        expected_database_id,
        expected_generation,
    )?;

    PresentationViewCacheStore::in_root(cache_root.to_path_buf(), view.session_id.clone())
        .publish(&view, expected_database_id, expected_generation, &payload)
        .map_err(|source| source.to_string())?;
    Ok(())
}

fn ensure_worker_target(
    actual_database_id: &str,
    actual_generation: i64,
    expected_database_id: &str,
    expected_generation: i64,
) -> Result<(), String> {
    if actual_database_id != expected_database_id || actual_generation != expected_generation {
        return Err(format!(
            "presentation view rebuild target changed: expected database {expected_database_id} generation {expected_generation}, found database {actual_database_id} generation {actual_generation}"
        ));
    }
    Ok(())
}

fn query_output_mode(output: PresentationViewOutputMode) -> QueryOutputMode {
    match output {
        PresentationViewOutputMode::Flat => QueryOutputMode::Flat,
        PresentationViewOutputMode::Outline => QueryOutputMode::Outline,
    }
}

fn query_include(include: PresentationViewInclude) -> QueryInclude {
    match include {
        PresentationViewInclude::Path => QueryInclude::Path,
        PresentationViewInclude::Properties => QueryInclude::Properties,
        PresentationViewInclude::EffectiveProperties => QueryInclude::EffectiveProperties,
        PresentationViewInclude::Keywords => QueryInclude::Keywords,
        PresentationViewInclude::Links => QueryInclude::Links,
        PresentationViewInclude::Backlinks => QueryInclude::Backlinks,
        PresentationViewInclude::Source => QueryInclude::Source,
        PresentationViewInclude::Target => QueryInclude::Target,
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct PresentationViewReadyTarget {
    pub view: RegisteredPresentationView,
    pub database_id: String,
    pub generation: i64,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum PresentationViewReadState {
    Pending,
    Ready(PresentationViewReadyTarget),
    Failed(String),
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct RebuildTarget {
    view: RegisteredPresentationView,
    database_id: String,
    generation: i64,
}

impl RebuildTarget {
    fn ready_target(&self) -> PresentationViewReadyTarget {
        PresentationViewReadyTarget {
            view: self.view.clone(),
            database_id: self.database_id.clone(),
            generation: self.generation,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum TargetStatus {
    Queued,
    Building,
    Ready,
    Failed(String),
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct ViewState {
    target: RebuildTarget,
    status: TargetStatus,
}

#[derive(Debug, Default)]
struct SharedState {
    views: BTreeMap<String, ViewState>,
}

#[derive(Debug)]
enum ManagerMessage {
    Refresh,
    Shutdown,
}

#[derive(Clone)]
pub(crate) struct PresentationViewRebuildHandle {
    registry: PresentationViewRegistryHandle,
    db_path: PathBuf,
    sender: Sender<ManagerMessage>,
    shared: Arc<Mutex<SharedState>>,
}

impl PresentationViewRebuildHandle {
    pub(crate) fn request_refresh(&self) {
        let _ = self.sender.send(ManagerMessage::Refresh);
    }

    fn require_refresh(&self) -> Result<(), PresentationViewRebuildReadError> {
        self.sender
            .send(ManagerMessage::Refresh)
            .map_err(|_| PresentationViewRebuildReadError::CoordinatorUnavailable)
    }

    pub(crate) fn read_state(
        &self,
        name: &str,
    ) -> Result<PresentationViewReadState, PresentationViewRebuildReadError> {
        let view = self
            .registry
            .show(name)
            .map_err(PresentationViewRebuildReadError::Registry)?;
        let connection = open_existing_database_read_only(&self.db_path)
            .map_err(|source| PresentationViewRebuildReadError::Database(source.to_string()))?;
        let index_state = read_index_state(&connection)
            .map_err(|source| PresentationViewRebuildReadError::Database(source.to_string()))?;
        let expected = RebuildTarget {
            view,
            database_id: index_state.database_id,
            generation: index_state.generation,
        };

        let state = self
            .shared
            .lock()
            .map_err(|_| PresentationViewRebuildReadError::CoordinatorUnavailable)?;
        let current = state.views.get(name);
        let result = match current {
            Some(current) if current.target == expected => match &current.status {
                TargetStatus::Queued | TargetStatus::Building => PresentationViewReadState::Pending,
                TargetStatus::Ready => {
                    PresentationViewReadState::Ready(current.target.ready_target())
                }
                TargetStatus::Failed(message) => PresentationViewReadState::Failed(message.clone()),
            },
            _ => PresentationViewReadState::Pending,
        };
        drop(state);

        if matches!(&result, PresentationViewReadState::Pending) {
            self.require_refresh()?;
        }
        Ok(result)
    }
}

pub(crate) struct PresentationViewRebuildCoordinator {
    sender: Sender<ManagerMessage>,
    thread: Option<JoinHandle<()>>,
}

impl PresentationViewRebuildCoordinator {
    pub(crate) fn start(
        config: &Config,
        registry: &PresentationViewRegistryHandle,
    ) -> Result<(Self, PresentationViewRebuildHandle), PresentationViewRebuildStartError> {
        let executable = std::env::current_exe()
            .map_err(PresentationViewRebuildStartError::CurrentExecutable)?;
        let cache_root = presentation_view_cache_root(config)
            .map_err(PresentationViewRebuildStartError::CachePath)?;
        let worker_limit = rebuild_worker_limit();
        let (sender, receiver) = mpsc::channel();
        let shared = Arc::new(Mutex::new(SharedState::default()));
        let handle = PresentationViewRebuildHandle {
            registry: registry.clone(),
            db_path: config.db_path.clone(),
            sender: sender.clone(),
            shared: Arc::clone(&shared),
        };
        let thread_registry = registry.clone();
        let thread_db_path = config.db_path.clone();
        let thread = thread::Builder::new()
            .name("orgfdb-view-rebuild".to_string())
            .spawn(move || {
                run_manager(
                    receiver,
                    shared,
                    thread_registry,
                    thread_db_path,
                    cache_root,
                    executable,
                    worker_limit,
                );
            })
            .map_err(PresentationViewRebuildStartError::SpawnManager)?;

        sender
            .send(ManagerMessage::Refresh)
            .map_err(|_| PresentationViewRebuildStartError::ManagerUnavailable)?;

        Ok((
            Self {
                sender,
                thread: Some(thread),
            },
            handle,
        ))
    }
}

impl Drop for PresentationViewRebuildCoordinator {
    fn drop(&mut self) {
        let _ = self.sender.send(ManagerMessage::Shutdown);
        if let Some(thread) = self.thread.take() {
            let _ = thread.join();
        }
    }
}

struct RunningWorker {
    target: RebuildTarget,
    child: Child,
}

fn run_manager(
    receiver: Receiver<ManagerMessage>,
    shared: Arc<Mutex<SharedState>>,
    registry: PresentationViewRegistryHandle,
    db_path: PathBuf,
    cache_root: PathBuf,
    executable: PathBuf,
    worker_limit: usize,
) {
    let mut workers = BTreeMap::<String, RunningWorker>::new();
    let mut refresh_requested = true;

    loop {
        match receiver.recv_timeout(WORKER_POLL_INTERVAL) {
            Ok(ManagerMessage::Refresh) => refresh_requested = true,
            Ok(ManagerMessage::Shutdown) => {
                terminate_all_workers(&mut workers);
                return;
            }
            Err(RecvTimeoutError::Timeout) => {}
            Err(RecvTimeoutError::Disconnected) => {
                terminate_all_workers(&mut workers);
                return;
            }
        }

        while let Ok(message) = receiver.try_recv() {
            match message {
                ManagerMessage::Refresh => refresh_requested = true,
                ManagerMessage::Shutdown => {
                    terminate_all_workers(&mut workers);
                    return;
                }
            }
        }

        if refresh_requested {
            refresh_requested = false;
            if refresh_targets(&shared, &registry, &db_path, &mut workers).is_err() {
                refresh_requested = true;
                continue;
            }
        }

        if poll_workers(&shared, &cache_root, &mut workers) {
            refresh_requested = true;
        }
        spawn_queued_workers(
            &shared,
            &cache_root,
            &db_path,
            &executable,
            worker_limit,
            &mut workers,
        );
    }
}

fn refresh_targets(
    shared: &Arc<Mutex<SharedState>>,
    registry: &PresentationViewRegistryHandle,
    db_path: &Path,
    workers: &mut BTreeMap<String, RunningWorker>,
) -> Result<(), String> {
    let views = registry.list().map_err(|source| source.to_string())?;
    let connection =
        open_existing_database_read_only(db_path).map_err(|source| source.to_string())?;
    let index_state = read_index_state(&connection).map_err(|source| source.to_string())?;
    let desired = views
        .into_iter()
        .map(|view| {
            let name = view.definition.name.clone();
            let target = RebuildTarget {
                view,
                database_id: index_state.database_id.clone(),
                generation: index_state.generation,
            };
            (name, target)
        })
        .collect::<BTreeMap<_, _>>();

    let desired_names = desired.keys().cloned().collect::<BTreeSet<_>>();
    let obsolete_workers = workers
        .keys()
        .filter(|name| !desired_names.contains(*name))
        .cloned()
        .collect::<Vec<_>>();
    for name in obsolete_workers {
        terminate_worker(workers, &name);
    }

    let mut state = shared
        .lock()
        .map_err(|_| "presentation view rebuild state is unavailable".to_string())?;
    state.views.retain(|name, _| desired_names.contains(name));

    for (name, target) in desired {
        let target_changed = state
            .views
            .get(&name)
            .map(|current| current.target != target)
            .unwrap_or(true);
        if !target_changed {
            continue;
        }

        terminate_worker(workers, &name);
        state.views.insert(
            name,
            ViewState {
                target,
                status: TargetStatus::Queued,
            },
        );
    }
    Ok(())
}

fn spawn_queued_workers(
    shared: &Arc<Mutex<SharedState>>,
    cache_root: &Path,
    db_path: &Path,
    executable: &Path,
    worker_limit: usize,
    workers: &mut BTreeMap<String, RunningWorker>,
) {
    while workers.len() < worker_limit {
        let next = match next_queued_target(shared, workers) {
            Ok(Some(next)) => next,
            Ok(None) => return,
            Err(_) => return,
        };
        let name = next.view.definition.name.clone();
        match spawn_worker(executable, db_path, cache_root, &next) {
            Ok(worker) => {
                workers.insert(name.clone(), worker);
                set_status_if_target(shared, &name, &next, TargetStatus::Building);
            }
            Err(message) => {
                set_status_if_target(shared, &name, &next, TargetStatus::Failed(message));
            }
        }
    }
}

fn next_queued_target(
    shared: &Arc<Mutex<SharedState>>,
    workers: &BTreeMap<String, RunningWorker>,
) -> Result<Option<RebuildTarget>, ()> {
    let state = shared.lock().map_err(|_| ())?;
    Ok(state
        .views
        .iter()
        .find(|(name, current)| {
            current.status == TargetStatus::Queued && !workers.contains_key(*name)
        })
        .map(|(_, current)| current.target.clone()))
}

fn spawn_worker(
    executable: &Path,
    db_path: &Path,
    cache_root: &Path,
    target: &RebuildTarget,
) -> Result<RunningWorker, String> {
    let mut child = Command::new(executable)
        .arg("__presentation-view-rebuild-worker")
        .arg("--db")
        .arg(db_path)
        .arg("--cache-root")
        .arg(cache_root)
        .arg("--database-id")
        .arg(&target.database_id)
        .arg("--generation")
        .arg(target.generation.to_string())
        .stdin(Stdio::piped())
        .stdout(Stdio::null())
        .stderr(Stdio::piped())
        .spawn()
        .map_err(|source| format!("failed to start presentation view rebuild worker: {source}"))?;

    let write_result = child
        .stdin
        .take()
        .ok_or_else(|| "presentation view rebuild worker stdin is unavailable".to_string())
        .and_then(|mut stdin| {
            serde_json::to_writer(&mut stdin, &target.view).map_err(|source| source.to_string())?;
            stdin.write_all(b"\n").map_err(|source| source.to_string())
        });
    if let Err(message) = write_result {
        let _ = child.kill();
        let _ = child.wait();
        return Err(format!(
            "failed to send presentation view definition to rebuild worker: {message}"
        ));
    }

    Ok(RunningWorker {
        target: target.clone(),
        child,
    })
}

fn poll_workers(
    shared: &Arc<Mutex<SharedState>>,
    cache_root: &Path,
    workers: &mut BTreeMap<String, RunningWorker>,
) -> bool {
    let names = workers.keys().cloned().collect::<Vec<_>>();
    let mut completed = Vec::new();
    for name in names {
        let Some(worker) = workers.get_mut(&name) else {
            continue;
        };
        match worker.child.try_wait() {
            Ok(Some(status)) => completed.push((name, status.success())),
            Ok(None) => {}
            Err(source) => {
                let target = worker.target.clone();
                let message =
                    format!("failed to inspect presentation view rebuild worker: {source}");
                set_status_if_target(shared, &name, &target, TargetStatus::Failed(message));
                let _ = worker.child.kill();
                let _ = worker.child.wait();
                completed.push((name, false));
            }
        }
    }

    let had_completion = !completed.is_empty();
    for (name, success) in completed {
        let Some(mut worker) = workers.remove(&name) else {
            continue;
        };
        let target = worker.target.clone();
        if success {
            let worker_stderr = read_worker_stderr(&mut worker.child);
            if !worker_stderr.trim().is_empty() {
                eprintln!("{}", worker_stderr.trim_end());
            }
            let store = PresentationViewCacheStore::in_root(
                cache_root.to_path_buf(),
                target.view.session_id.clone(),
            );
            let result = store.open_valid(&target.view, &target.database_id, target.generation);
            match result {
                Ok(_) => set_status_if_target(shared, &name, &target, TargetStatus::Ready),
                Err(source) => set_status_if_target(
                    shared,
                    &name,
                    &target,
                    TargetStatus::Failed(format!(
                        "presentation view rebuild completed without a valid cache: {source}"
                    )),
                ),
            }
        } else if target_is_current(shared, &name, &target) {
            let message = read_worker_stderr(&mut worker.child);
            let message = if message.trim().is_empty() {
                "presentation view rebuild worker failed".to_string()
            } else {
                format!(
                    "presentation view rebuild worker failed: {}",
                    message.trim()
                )
            };
            set_status_if_target(shared, &name, &target, TargetStatus::Failed(message));
        }
    }
    had_completion
}

fn target_is_current(shared: &Arc<Mutex<SharedState>>, name: &str, target: &RebuildTarget) -> bool {
    shared
        .lock()
        .ok()
        .and_then(|state| state.views.get(name).cloned())
        .map(|current| current.target == *target)
        .unwrap_or(false)
}

fn set_status_if_target(
    shared: &Arc<Mutex<SharedState>>,
    name: &str,
    target: &RebuildTarget,
    status: TargetStatus,
) {
    let Ok(mut state) = shared.lock() else {
        return;
    };
    let Some(current) = state.views.get_mut(name) else {
        return;
    };
    if current.target == *target {
        current.status = status;
    }
}

fn terminate_worker(workers: &mut BTreeMap<String, RunningWorker>, name: &str) {
    let Some(mut worker) = workers.remove(name) else {
        return;
    };
    let _ = worker.child.kill();
    let _ = worker.child.wait();
}

fn terminate_all_workers(workers: &mut BTreeMap<String, RunningWorker>) {
    let names = workers.keys().cloned().collect::<Vec<_>>();
    for name in names {
        terminate_worker(workers, &name);
    }
}

fn read_worker_stderr(child: &mut Child) -> String {
    let mut output = String::new();
    if let Some(mut stderr) = child.stderr.take() {
        let _ = stderr.read_to_string(&mut output);
    }
    output
}

fn rebuild_worker_limit() -> usize {
    thread::available_parallelism()
        .map(|count| count.get().min(MAX_REBUILD_WORKERS))
        .unwrap_or(1)
}

#[derive(Debug)]
pub(crate) enum PresentationViewRebuildStartError {
    CurrentExecutable(io::Error),
    CachePath(PresentationViewCachePathError),
    SpawnManager(io::Error),
    ManagerUnavailable,
}

impl fmt::Display for PresentationViewRebuildStartError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::CurrentExecutable(source) => write!(
                f,
                "failed to resolve the orgfdb executable for presentation view rebuild workers: {source}"
            ),
            Self::CachePath(source) => write!(f, "{source}"),
            Self::SpawnManager(source) => write!(
                f,
                "failed to start presentation view rebuild coordinator: {source}"
            ),
            Self::ManagerUnavailable => {
                write!(f, "presentation view rebuild coordinator stopped during startup")
            }
        }
    }
}

impl Error for PresentationViewRebuildStartError {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        match self {
            Self::CurrentExecutable(source) | Self::SpawnManager(source) => Some(source),
            Self::CachePath(source) => Some(source),
            Self::ManagerUnavailable => None,
        }
    }
}

#[derive(Debug)]
pub(crate) enum PresentationViewRebuildReadError {
    Registry(PresentationViewRegistryAccessError),
    Database(String),
    CoordinatorUnavailable,
}

impl fmt::Display for PresentationViewRebuildReadError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Registry(source) => write!(f, "{source}"),
            Self::Database(message) => write!(
                f,
                "failed to read database state for presentation view cache: {message}"
            ),
            Self::CoordinatorUnavailable => {
                write!(f, "presentation view rebuild coordinator is unavailable")
            }
        }
    }
}

impl Error for PresentationViewRebuildReadError {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        match self {
            Self::Registry(source) => Some(source),
            Self::Database(_) | Self::CoordinatorUnavailable => None,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::{
        rebuild_worker_limit, refresh_targets, run_rebuild_worker, RebuildTarget, RunningWorker,
        SharedState, TargetStatus, ViewState,
    };
    use crate::{
        config::Config,
        db::{
            advance_index_generation, open_database_with_schema, read_index_state,
            IndexGenerationChange, SchemaDefinition, CURRENT_SCHEMA_VERSION,
        },
        indexer::Indexer,
        parser::OrgizeAdapter,
        presentation_view::{
            PresentationViewDefinition, PresentationViewOutputMode, PresentationViewRegistryHandle,
            RegisteredPresentationView,
        },
        presentation_view_cache::PresentationViewCacheStore,
    };
    use serde_json::json;
    use std::{
        collections::BTreeMap,
        fs,
        io::Cursor,
        path::PathBuf,
        process::{Command, Stdio},
        sync::{Arc, Mutex},
        time::{SystemTime, UNIX_EPOCH},
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
                "org-files-db-view-rebuild-{name}-{}-{unique}",
                std::process::id()
            ));
            fs::create_dir_all(&path).expect("test directory should be created");
            Self { path }
        }
    }

    impl Drop for TestDir {
        fn drop(&mut self) {
            let _ = fs::remove_dir_all(&self.path);
        }
    }

    fn write_fixture(test_dir: &TestDir) -> Config {
        let org_path = test_dir.path.join("notes.org");
        let config_path = test_dir.path.join("config.toml");
        fs::write(&org_path, "#+TITLE: Notes\n* NEXT Cached heading\n")
            .expect("org fixture should be written");
        fs::write(
            &config_path,
            r#"db_path = "./db.sqlite"
files = ["./notes.org"]

[todo]
default_open_keywords = ["TODO", "NEXT"]
default_closed_keywords = ["DONE"]

[search]
fts5_enabled = false
index_body_text = false
"#,
        )
        .expect("config fixture should be written");
        Indexer::new(OrgizeAdapter::new())
            .rebuild_from_config_path(&config_path)
            .expect("fixture rebuild should succeed");
        Config::load_from_file(&config_path).expect("fixture config should load")
    }

    fn definition(name: &str) -> PresentationViewDefinition {
        PresentationViewDefinition {
            name: name.to_string(),
            query: "(headings)".to_string(),
            output: PresentationViewOutputMode::Flat,
            includes: Vec::new(),
            query_timezone: None,
            presentation_spec: json!({"columns":[{"name":"title"}]}),
        }
    }

    fn target(revision: u64, generation: i64) -> RebuildTarget {
        RebuildTarget {
            view: RegisteredPresentationView {
                session_id: "session".to_string(),
                revision,
                definition: definition("agenda"),
            },
            database_id: "database".to_string(),
            generation,
        }
    }

    fn advance_generation(config: &Config) -> i64 {
        let mut connection = open_database_with_schema(
            &config.db_path,
            &SchemaDefinition::new(CURRENT_SCHEMA_VERSION, false),
        )
        .expect("database should open");
        let transaction = connection.transaction().expect("transaction should start");
        advance_index_generation(&transaction, &IndexGenerationChange::full_invalidation())
            .expect("generation should advance");
        transaction.commit().expect("transaction should commit");
        read_index_state(&connection)
            .expect("state should load")
            .generation
    }

    #[test]
    fn target_changes_for_generation_or_revision() {
        assert_ne!(target(1, 4), target(1, 5));
        assert_ne!(target(1, 4), target(2, 4));
    }

    #[test]
    fn ready_state_keeps_one_target() {
        let state = ViewState {
            target: target(2, 7),
            status: TargetStatus::Ready,
        };
        assert_eq!(state.target.generation, 7);
        assert_eq!(state.target.view.revision, 2);
        assert_eq!(state.status, TargetStatus::Ready);
    }

    #[test]
    fn worker_limit_is_bounded() {
        assert!((1..=3).contains(&rebuild_worker_limit()));
    }

    #[test]
    fn refresh_targets_replaces_an_obsolete_generation_and_stops_its_worker() {
        let test_dir = TestDir::new("obsolete-generation");
        let config = write_fixture(&test_dir);
        let registry = PresentationViewRegistryHandle::for_watcher(&config);
        registry
            .register(definition("agenda"))
            .expect("view should register");
        let shared = Arc::new(Mutex::new(SharedState::default()));
        let mut workers = BTreeMap::new();

        refresh_targets(&shared, &registry, &config.db_path, &mut workers)
            .expect("initial target refresh should succeed");
        let initial = shared.lock().expect("state should lock").views["agenda"]
            .target
            .clone();

        let child = Command::new("sh")
            .arg("-c")
            .arg("sleep 60")
            .stdin(Stdio::null())
            .stdout(Stdio::null())
            .stderr(Stdio::null())
            .spawn()
            .expect("sleep worker should start");
        workers.insert(
            "agenda".to_string(),
            RunningWorker {
                target: initial.clone(),
                child,
            },
        );
        shared
            .lock()
            .expect("state should lock")
            .views
            .get_mut("agenda")
            .expect("view state should exist")
            .status = TargetStatus::Building;

        let new_generation = advance_generation(&config);
        refresh_targets(&shared, &registry, &config.db_path, &mut workers)
            .expect("new target refresh should succeed");

        assert!(workers.is_empty());
        let state = shared.lock().expect("state should lock");
        let current = &state.views["agenda"];
        assert_eq!(current.target.generation, new_generation);
        assert_eq!(current.status, TargetStatus::Queued);
        assert_ne!(current.target, initial);
    }

    #[test]
    fn rebuild_worker_publishes_a_valid_materialized_view() {
        let test_dir = TestDir::new("worker-publish");
        let config = write_fixture(&test_dir);
        let registry = PresentationViewRegistryHandle::for_watcher(&config);
        registry
            .register(definition("agenda"))
            .expect("view should register");
        let view = registry.show("agenda").expect("view should exist");
        let connection = crate::db::open_existing_database_read_only(&config.db_path)
            .expect("database should open");
        let state = read_index_state(&connection).expect("state should load");
        let cache_root = test_dir.path.join("cache");
        let serialized = serde_json::to_vec(&view).expect("view should serialize");

        run_rebuild_worker(
            &config.db_path,
            &cache_root,
            &state.database_id,
            state.generation,
            Cursor::new(serialized),
        )
        .expect("worker should publish cache");

        let store = PresentationViewCacheStore::in_root(cache_root, view.session_id.clone());
        let mut reader = store
            .open_valid(&view, &state.database_id, state.generation)
            .expect("published cache should be valid");
        let mut payload = Vec::new();
        reader
            .copy_payload_to(&mut payload)
            .expect("payload should stream");
        let value: serde_json::Value =
            serde_json::from_slice(&payload).expect("payload should be JSON");
        assert_eq!(value["database_id"], state.database_id);
        assert_eq!(value["generation"], state.generation);
        assert!(value["rows"]
            .as_array()
            .is_some_and(|rows| !rows.is_empty()));
    }
}
