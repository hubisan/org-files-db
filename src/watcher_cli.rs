use std::{
    error::Error,
    fmt,
    io::{self, Write},
    sync::{
        atomic::{AtomicBool, Ordering},
        Arc,
    },
    thread,
    time::{Duration, Instant},
};

use signal_hook::{
    consts::signal::{SIGINT, SIGTERM},
    flag as signal_flag, low_level, SigId,
};

use crate::{
    config::Config,
    db::{open_database_with_schema, DbError, SchemaDefinition, CURRENT_SCHEMA_VERSION},
    indexer::Indexer,
    notify_source::NotifyWatcherError,
    parser::OrgizeAdapter,
    watcher::{Phase6WatcherExecutor, WatcherBatchExecutor, WatcherExecutionError},
    watcher_runtime::{
        WatcherMessageSource, WatcherRuntime, WatcherRuntimeError, WatcherStartupError,
    },
};

const MAX_IDLE_POLL_INTERVAL: Duration = Duration::from_millis(25);

pub(crate) fn run_watch_command(
    config: &Config,
    stderr: &mut impl Write,
) -> Result<(), WatcherCommandError> {
    let shutdown = SignalShutdown::install()?;
    let schema = SchemaDefinition::new(CURRENT_SCHEMA_VERSION, config.search.fts5_enabled);
    let mut connection = open_database_with_schema(&config.db_path, &schema)
        .map_err(WatcherCommandError::Database)?;
    let indexer = Indexer::new(OrgizeAdapter::new());
    let mut executor = Phase6WatcherExecutor::new(&indexer, &mut connection, config);
    let mut runtime = WatcherRuntime::start_notify(config, Instant::now(), &mut executor)
        .map_err(|source| WatcherCommandError::Startup(Box::new(source)))?;

    writeln!(stderr, "watcher ready").map_err(WatcherCommandError::Io)?;
    let mut clock = SystemLoopClock;
    drive_watcher_loop(&mut runtime, &mut executor, &shutdown, &mut clock, stderr)?;
    Ok(())
}

trait ShutdownRequest {
    fn requested(&self) -> bool;
}

struct SignalShutdown {
    requested: Arc<AtomicBool>,
    registrations: Vec<SigId>,
}

impl SignalShutdown {
    fn install() -> Result<Self, WatcherCommandError> {
        let requested = Arc::new(AtomicBool::new(false));
        let mut registrations = Vec::with_capacity(2);

        let sigint = signal_flag::register(SIGINT, Arc::clone(&requested)).map_err(|source| {
            WatcherCommandError::RegisterSignal {
                signal: "SIGINT",
                source,
            }
        })?;
        registrations.push(sigint);

        match signal_flag::register(SIGTERM, Arc::clone(&requested)) {
            Ok(sigterm) => registrations.push(sigterm),
            Err(source) => {
                for registration in registrations.drain(..) {
                    let _ = low_level::unregister(registration);
                }
                return Err(WatcherCommandError::RegisterSignal {
                    signal: "SIGTERM",
                    source,
                });
            }
        }

        Ok(Self {
            requested,
            registrations,
        })
    }
}

impl ShutdownRequest for SignalShutdown {
    fn requested(&self) -> bool {
        self.requested.load(Ordering::Relaxed)
    }
}

impl Drop for SignalShutdown {
    fn drop(&mut self) {
        for registration in self.registrations.drain(..) {
            let _ = low_level::unregister(registration);
        }
    }
}

trait LoopClock {
    fn now(&self) -> Instant;
    fn wait(&mut self, duration: Duration);
}

struct SystemLoopClock;

impl LoopClock for SystemLoopClock {
    fn now(&self) -> Instant {
        Instant::now()
    }

    fn wait(&mut self, duration: Duration) {
        thread::sleep(duration);
    }
}

fn drive_watcher_loop<S, E, R, C, W>(
    runtime: &mut WatcherRuntime<S>,
    executor: &mut E,
    shutdown: &R,
    clock: &mut C,
    stderr: &mut W,
) -> Result<(), WatcherCommandError>
where
    S: WatcherMessageSource<Error = NotifyWatcherError>,
    E: WatcherBatchExecutor<Error = WatcherExecutionError>,
    R: ShutdownRequest,
    C: LoopClock,
    W: Write,
{
    let mut shutdown_started = false;

    loop {
        if !shutdown_started && shutdown.requested() {
            runtime.begin_shutdown();
            shutdown_started = true;
            writeln!(stderr, "watcher stopping").map_err(WatcherCommandError::Io)?;
        }

        let now = clock.now();
        runtime
            .process_available(now, executor)
            .map_err(|source| WatcherCommandError::Runtime(Box::new(source)))?;

        if shutdown_started && runtime.is_shutdown_complete() {
            writeln!(stderr, "watcher stopped").map_err(WatcherCommandError::Io)?;
            return Ok(());
        }

        let wait = next_wait_duration(now, runtime.next_deadline());
        if !wait.is_zero() {
            clock.wait(wait);
        }
    }
}

fn next_wait_duration(now: Instant, deadline: Option<Instant>) -> Duration {
    deadline
        .map(|deadline| deadline.saturating_duration_since(now))
        .unwrap_or(MAX_IDLE_POLL_INTERVAL)
        .min(MAX_IDLE_POLL_INTERVAL)
}

#[derive(Debug)]
pub(crate) enum WatcherCommandError {
    Database(DbError),
    RegisterSignal {
        signal: &'static str,
        source: io::Error,
    },
    Startup(Box<WatcherStartupError<NotifyWatcherError, WatcherExecutionError>>),
    Runtime(Box<WatcherRuntimeError<NotifyWatcherError, WatcherExecutionError>>),
    Io(io::Error),
}

impl fmt::Display for WatcherCommandError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Database(source) => write!(f, "failed to open watcher database: {source}"),
            Self::RegisterSignal { signal, source } => {
                write!(
                    f,
                    "failed to register {signal} watcher shutdown handler: {source}"
                )
            }
            Self::Startup(source) => write!(f, "{source}"),
            Self::Runtime(source) => write!(f, "{source}"),
            Self::Io(source) => write!(f, "failed to write watcher lifecycle output: {source}"),
        }
    }
}

impl Error for WatcherCommandError {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        match self {
            Self::Database(source) => Some(source),
            Self::RegisterSignal { source, .. } => Some(source),
            Self::Startup(source) => Some(source.as_ref()),
            Self::Runtime(source) => Some(source.as_ref()),
            Self::Io(source) => Some(source),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::{
        drive_watcher_loop, next_wait_duration, LoopClock, ShutdownRequest, WatcherCommandError,
        MAX_IDLE_POLL_INTERVAL,
    };
    use crate::{
        config::{Config, ConfiguredDir},
        notify_source::{NotifySourceMessage, NotifyWatcherError},
        watcher::{
            NormalizedWatcherBatch, WatcherBatchExecutor, WatcherExecutionError, WatcherInput,
            WatcherPathEventKind,
        },
        watcher_runtime::{WatcherMessageSource, WatcherRuntime},
    };
    use std::{
        cell::{Cell, RefCell},
        collections::VecDeque,
        fs,
        path::PathBuf,
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
                "org-files-db-watcher-cli-tests-{}-{}-{}",
                name,
                std::process::id(),
                unique
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

    struct TestSource {
        queue: Rc<RefCell<VecDeque<NotifySourceMessage>>>,
        watch_target: PathBuf,
    }

    impl TestSource {
        fn new(watch_target: PathBuf) -> (Self, Rc<RefCell<VecDeque<NotifySourceMessage>>>) {
            let queue = Rc::new(RefCell::new(VecDeque::new()));
            (
                Self {
                    queue: Rc::clone(&queue),
                    watch_target,
                },
                queue,
            )
        }
    }

    impl WatcherMessageSource for TestSource {
        type Error = NotifyWatcherError;

        fn try_recv_message(&mut self) -> Result<Option<NotifySourceMessage>, Self::Error> {
            Ok(self.queue.borrow_mut().pop_front())
        }

        fn watch_target_paths(&self) -> Vec<PathBuf> {
            vec![self.watch_target.clone()]
        }

        fn validate_watch_targets(&self) -> Result<(), Self::Error> {
            Ok(())
        }

        fn refresh_watches(&mut self) -> Result<(), Self::Error> {
            Ok(())
        }
    }

    #[derive(Default)]
    struct TestExecutor {
        batches: Vec<NormalizedWatcherBatch>,
        fail_after: Option<usize>,
    }

    impl WatcherBatchExecutor for TestExecutor {
        type Error = WatcherExecutionError;

        fn execute(&mut self, batch: NormalizedWatcherBatch) -> Result<(), Self::Error> {
            self.batches.push(batch);
            if self
                .fail_after
                .is_some_and(|fail_after| self.batches.len() > fail_after)
            {
                return Err(WatcherExecutionError::Rejected(
                    crate::indexer::ChangeApplicationRejection::Stale,
                ));
            }
            Ok(())
        }
    }

    struct TestShutdown {
        requested_after: usize,
        checks: Cell<usize>,
    }

    impl TestShutdown {
        fn immediate() -> Self {
            Self {
                requested_after: 0,
                checks: Cell::new(0),
            }
        }

        fn after_checks(requested_after: usize) -> Self {
            Self {
                requested_after,
                checks: Cell::new(0),
            }
        }
    }

    impl ShutdownRequest for TestShutdown {
        fn requested(&self) -> bool {
            let checks = self.checks.get();
            self.checks.set(checks + 1);
            checks >= self.requested_after
        }
    }

    struct TestClock {
        now: Instant,
        waits: Vec<Duration>,
    }

    impl TestClock {
        fn new(now: Instant) -> Self {
            Self {
                now,
                waits: Vec::new(),
            }
        }
    }

    impl LoopClock for TestClock {
        fn now(&self) -> Instant {
            self.now
        }

        fn wait(&mut self, duration: Duration) {
            self.waits.push(duration);
            self.now = self.now.checked_add(duration).unwrap_or(self.now);
        }
    }

    fn config(test_dir: &TestDir) -> Config {
        Config {
            db_path: test_dir.path.join("db.sqlite"),
            files: Vec::new(),
            dirs: vec![ConfiguredDir {
                path: test_dir.path.clone(),
                recursive: true,
                exclude: Vec::new(),
            }],
            discovery: Default::default(),
            links: Default::default(),
            todo: Default::default(),
            search: Default::default(),
            query: Default::default(),
        }
    }

    #[test]
    fn idle_shutdown_is_immediate_and_deterministic() {
        let test_dir = TestDir::new("idle-shutdown");
        let config = config(&test_dir);
        let (source, _) = TestSource::new(test_dir.path.clone());
        let started = Instant::now();
        let mut executor = TestExecutor::default();
        let mut runtime = WatcherRuntime::start_registered(source, &config, started, &mut executor)
            .expect("runtime should start");
        let mut clock = TestClock::new(started);
        let mut stderr = Vec::new();

        drive_watcher_loop(
            &mut runtime,
            &mut executor,
            &TestShutdown::immediate(),
            &mut clock,
            &mut stderr,
        )
        .expect("shutdown should succeed");

        assert_eq!(executor.batches, vec![NormalizedWatcherBatch::Reconcile]);
        assert!(clock.waits.is_empty());
        assert_eq!(
            String::from_utf8(stderr).expect("stderr should be utf-8"),
            "watcher stopping\nwatcher stopped\n"
        );
    }

    #[test]
    fn shutdown_stops_accepting_new_source_messages() {
        let test_dir = TestDir::new("stop-input");
        let config = config(&test_dir);
        let candidate = test_dir.path.join("late.org");
        let (source, queue) = TestSource::new(test_dir.path.clone());
        let started = Instant::now();
        let mut executor = TestExecutor::default();
        let mut runtime = WatcherRuntime::start_registered(source, &config, started, &mut executor)
            .expect("runtime should start");
        queue
            .borrow_mut()
            .push_back(NotifySourceMessage::Input(WatcherInput::Paths {
                kind: WatcherPathEventKind::Create,
                paths: vec![candidate],
            }));
        let mut clock = TestClock::new(started);
        let mut stderr = Vec::new();

        drive_watcher_loop(
            &mut runtime,
            &mut executor,
            &TestShutdown::immediate(),
            &mut clock,
            &mut stderr,
        )
        .expect("shutdown should succeed");

        assert_eq!(executor.batches, vec![NormalizedWatcherBatch::Reconcile]);
        assert_eq!(queue.borrow().len(), 1);
    }

    #[test]
    fn shutdown_flushes_work_queued_during_startup() {
        let test_dir = TestDir::new("pending-shutdown");
        let config = config(&test_dir);
        let candidate = test_dir.path.join("note.org");
        let (source, queue) = TestSource::new(test_dir.path.clone());
        queue
            .borrow_mut()
            .push_back(NotifySourceMessage::Input(WatcherInput::Paths {
                kind: WatcherPathEventKind::Modify,
                paths: vec![candidate],
            }));
        let started = Instant::now();
        let mut executor = TestExecutor::default();
        let mut runtime = WatcherRuntime::start_registered(source, &config, started, &mut executor)
            .expect("runtime should start");
        let mut clock = TestClock::new(started);
        let mut stderr = Vec::new();

        drive_watcher_loop(
            &mut runtime,
            &mut executor,
            &TestShutdown::immediate(),
            &mut clock,
            &mut stderr,
        )
        .expect("shutdown should flush pending work");

        assert_eq!(executor.batches.len(), 2);
        assert!(matches!(
            executor.batches[1],
            NormalizedWatcherBatch::Candidates(_)
        ));
        assert!(clock.waits.is_empty());
    }

    #[test]
    fn fatal_execution_failure_is_returned_without_retry_loop() {
        let test_dir = TestDir::new("fatal-execution");
        let config = config(&test_dir);
        let candidate = test_dir.path.join("note.org");
        let (source, queue) = TestSource::new(test_dir.path.clone());
        queue
            .borrow_mut()
            .push_back(NotifySourceMessage::Input(WatcherInput::Paths {
                kind: WatcherPathEventKind::Modify,
                paths: vec![candidate],
            }));
        let started = Instant::now();
        let mut executor = TestExecutor {
            batches: Vec::new(),
            fail_after: Some(1),
        };
        let mut runtime = WatcherRuntime::start_registered(source, &config, started, &mut executor)
            .expect("runtime should start");
        let mut clock = TestClock::new(started);
        let mut stderr = Vec::new();

        let error = drive_watcher_loop(
            &mut runtime,
            &mut executor,
            &TestShutdown::after_checks(usize::MAX),
            &mut clock,
            &mut stderr,
        )
        .expect_err("fatal execution should stop the loop");

        assert!(matches!(error, WatcherCommandError::Runtime(_)));
        assert_eq!(executor.batches.len(), 2);
        assert!(!clock.waits.is_empty());
    }

    #[test]
    fn wait_is_bounded_by_deadline_and_signal_poll_interval() {
        let now = Instant::now();
        assert_eq!(next_wait_duration(now, None), MAX_IDLE_POLL_INTERVAL);
        assert_eq!(
            next_wait_duration(now, Some(now + Duration::from_millis(5))),
            Duration::from_millis(5)
        );
        assert_eq!(
            next_wait_duration(now, Some(now + Duration::from_secs(1))),
            MAX_IDLE_POLL_INTERVAL
        );
        assert_eq!(next_wait_duration(now, Some(now)), Duration::ZERO);
    }
}
