use std::{
    collections::BTreeSet,
    fs,
    path::{Path, PathBuf},
    thread,
    time::{Duration, Instant, SystemTime, UNIX_EPOCH},
};

use rusqlite::{params, Connection, OptionalExtension};

use crate::{
    config::{Config, ConfiguredDir, DiscoveryConfig, SearchConfig},
    db::{
        open_database_with_schema, sqlite_supports_fts5, SchemaDefinition, CURRENT_SCHEMA_VERSION,
    },
    indexer::{ChangeApplicationResult, Indexer},
    notify_source::{NotifyWatcherError, NotifyWatcherSource},
    parser::OrgizeAdapter,
    watcher::{IndexerWatcherExecutor, WatcherExecutionError, WatcherExecutionStatus},
    watcher_runtime::{
        WatcherCycleReport, WatcherRuntime, WatcherRuntimeError, WatcherRuntimeState,
    },
};

const POLL_INTERVAL: Duration = Duration::from_millis(10);
const QUIET_INTERVAL: Duration = Duration::from_millis(500);
const EVENTUAL_TIMEOUT: Duration = Duration::from_secs(8);

type RealRuntimeError = WatcherRuntimeError<NotifyWatcherError, WatcherExecutionError>;

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
            "org-files-db-watcher-e2e-{}-{}-{}",
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

struct RealWatcherHarness {
    config: Config,
    indexer: Indexer<OrgizeAdapter>,
    connection: Connection,
    runtime: Option<WatcherRuntime<NotifyWatcherSource>>,
}

impl RealWatcherHarness {
    fn new_stopped(config: Config) -> Self {
        let schema = SchemaDefinition::new(CURRENT_SCHEMA_VERSION, config.search.fts5_enabled);
        let connection = open_database_with_schema(&config.db_path, &schema)
            .expect("watcher test database should open");
        Self {
            config,
            indexer: Indexer::new(OrgizeAdapter::new()),
            connection,
            runtime: None,
        }
    }

    fn new_started(config: Config) -> Self {
        let mut harness = Self::new_stopped(config);
        harness.rebuild_without_watcher();
        harness.start();
        harness
    }

    fn rebuild_without_watcher(&mut self) {
        self.indexer
            .rebuild(&mut self.connection, &self.config)
            .expect("initial rebuild should succeed");
    }

    fn start(&mut self) {
        assert!(self.runtime.is_none(), "watcher runtime is already running");
        let mut executor =
            IndexerWatcherExecutor::new(&self.indexer, &mut self.connection, &self.config);
        let runtime = WatcherRuntime::start_notify(&self.config, Instant::now(), &mut executor)
            .unwrap_or_else(|error| panic!("real watcher startup failed: {error}"));
        self.runtime = Some(runtime);
    }

    fn stop(&mut self) {
        self.runtime = None;
    }

    fn restart(&mut self) {
        self.stop();
        self.start();
    }

    fn tick(&mut self) -> Result<WatcherCycleReport, Box<RealRuntimeError>> {
        let runtime = self.runtime.as_mut().expect("watcher runtime should run");
        let mut executor =
            IndexerWatcherExecutor::new(&self.indexer, &mut self.connection, &self.config);
        runtime
            .process_available(Instant::now(), &mut executor)
            .map_err(Box::new)
    }

    fn wait_until(&mut self, description: &str, mut predicate: impl FnMut(&Connection) -> bool) {
        let deadline = Instant::now() + EVENTUAL_TIMEOUT;
        let mut last_report = None;

        loop {
            match self.tick() {
                Ok(report) => last_report = Some(report),
                Err(error) => panic!(
                    "{description}: watcher failed before the expected state was reached: {error}; {}",
                    self.diagnostics(last_report)
                ),
            }

            if predicate(&self.connection) {
                return;
            }

            if Instant::now() >= deadline {
                panic!(
                    "{description}: timed out after {:?}; {}",
                    EVENTUAL_TIMEOUT,
                    self.diagnostics(last_report)
                );
            }
            thread::sleep(POLL_INTERVAL);
        }
    }

    fn wait_for_runtime_error(&mut self, description: &str) -> Box<RealRuntimeError> {
        let deadline = Instant::now() + EVENTUAL_TIMEOUT;

        loop {
            let report = match self.tick() {
                Ok(report) => report,
                Err(error) => return error,
            };

            if Instant::now() >= deadline {
                panic!(
                    "{description}: expected a watcher failure within {:?}; {}",
                    EVENTUAL_TIMEOUT,
                    self.diagnostics(Some(report))
                );
            }
            thread::sleep(POLL_INTERVAL);
        }
    }

    fn wait_for_source_message(&mut self, description: &str) {
        let deadline = Instant::now() + EVENTUAL_TIMEOUT;
        let mut last_report = None;

        loop {
            match self.tick() {
                Ok(report) => {
                    last_report = Some(report);
                    if report.source_messages > 0 {
                        return;
                    }
                }
                Err(error) => panic!(
                    "{description}: watcher failed while waiting for a real backend event: {error}; {}",
                    self.diagnostics(last_report)
                ),
            }

            if Instant::now() >= deadline {
                panic!(
                    "{description}: no real backend event arrived within {:?}; {}",
                    EVENTUAL_TIMEOUT,
                    self.diagnostics(last_report)
                );
            }
            thread::sleep(POLL_INTERVAL);
        }
    }

    fn assert_stays_for(
        &mut self,
        description: &str,
        duration: Duration,
        mut predicate: impl FnMut(&Connection) -> bool,
    ) {
        let deadline = Instant::now() + duration;
        let mut last_report = None;

        while Instant::now() < deadline {
            match self.tick() {
                Ok(report) => last_report = Some(report),
                Err(error) => panic!(
                    "{description}: watcher failed while checking a stable state: {error}; {}",
                    self.diagnostics(last_report)
                ),
            }
            assert!(
                predicate(&self.connection),
                "{description}: observed an unexpected DB state; {}",
                self.diagnostics(last_report)
            );
            thread::sleep(POLL_INTERVAL);
        }
    }

    fn settle_quietly(&mut self, description: &str) {
        let deadline = Instant::now() + EVENTUAL_TIMEOUT;
        let mut quiet_since = Instant::now();
        let mut last_report = None;

        loop {
            let report = self.tick().unwrap_or_else(|error| {
                panic!(
                    "{description}: watcher failed while waiting for a quiet period: {error}; {}",
                    self.diagnostics(last_report)
                )
            });
            last_report = Some(report);
            if report.source_messages > 0
                || report.backend_failures > 0
                || report.execution_status == WatcherExecutionStatus::Executed
            {
                quiet_since = Instant::now();
            }

            if Instant::now().duration_since(quiet_since) >= QUIET_INTERVAL {
                return;
            }
            if Instant::now() >= deadline {
                panic!(
                    "{description}: watcher never became quiet within {:?}; {}",
                    EVENTUAL_TIMEOUT,
                    self.diagnostics(last_report)
                );
            }
            thread::sleep(POLL_INTERVAL);
        }
    }

    fn diagnostics(&self, last_report: Option<WatcherCycleReport>) -> String {
        let runtime_state = self
            .runtime
            .as_ref()
            .map(WatcherRuntime::state)
            .unwrap_or(WatcherRuntimeState::Terminated);
        let next_deadline = self
            .runtime
            .as_ref()
            .and_then(WatcherRuntime::next_deadline);
        format!(
            "runtime_state={runtime_state:?}, next_deadline={next_deadline:?}, last_report={last_report:?}, files={:?}, headings={:?}, links={:?}",
            indexed_paths(&self.connection),
            heading_titles(&self.connection),
            link_rows(&self.connection)
        )
    }
}

fn write_file(path: &Path, content: &str) {
    if let Some(parent) = path.parent() {
        fs::create_dir_all(parent).expect("parent directory should exist");
    }
    fs::write(path, content).expect("test file should be written");
}

fn write_bytes(path: &Path, content: &[u8]) {
    if let Some(parent) = path.parent() {
        fs::create_dir_all(parent).expect("parent directory should exist");
    }
    fs::write(path, content).expect("test bytes should be written");
}

fn replace_atomically(path: &Path, content: &str) {
    let file_name = path
        .file_name()
        .and_then(|name| name.to_str())
        .expect("test file name should be UTF-8");
    let temporary = path.with_file_name(format!(".{file_name}.orgfdb-tmp"));
    write_file(&temporary, content);
    fs::rename(&temporary, path).expect("atomic replacement should succeed on Unix");
}

fn rewrite_same_content_with_new_mtime(path: &Path, content: &str) {
    let original = fs::metadata(path)
        .and_then(|metadata| metadata.modified())
        .expect("original modification time should load");
    let deadline = Instant::now() + Duration::from_secs(3);

    loop {
        fs::write(path, content).expect("same content should be rewritten");
        let modified = fs::metadata(path)
            .and_then(|metadata| metadata.modified())
            .expect("new modification time should load");
        if modified != original {
            return;
        }
        assert!(
            Instant::now() < deadline,
            "filesystem did not expose a new mtime for a same-content rewrite"
        );
        thread::sleep(POLL_INTERVAL);
    }
}

fn config_with_sources(
    test_dir: &TestDir,
    db_path: PathBuf,
    files: Vec<PathBuf>,
    dirs: Vec<ConfiguredDir>,
    files_exclude: Vec<String>,
    fts5_enabled: bool,
    index_body_text: bool,
) -> Config {
    Config {
        db_path,
        files,
        dirs,
        discovery: DiscoveryConfig {
            files_exclude,
            config_dir: test_dir.path().to_path_buf(),
            home_dir: None,
        },
        search: SearchConfig {
            fts5_enabled,
            index_body_text,
        },
        ..Config::default()
    }
}

fn recursive_config(test_dir: &TestDir, root: &Path, db_path: PathBuf) -> Config {
    config_with_sources(
        test_dir,
        db_path,
        Vec::new(),
        vec![ConfiguredDir {
            path: root.to_path_buf(),
            recursive: true,
            exclude: Vec::new(),
        }],
        Vec::new(),
        false,
        false,
    )
}

fn heading_titles(connection: &Connection) -> Vec<String> {
    connection
        .prepare("SELECT title FROM headings WHERE level > 0 ORDER BY title")
        .expect("heading title query should prepare")
        .query_map([], |row| row.get::<_, String>(0))
        .expect("heading title query should execute")
        .collect::<Result<Vec<_>, _>>()
        .expect("heading titles should load")
}

fn indexed_paths(connection: &Connection) -> Vec<String> {
    connection
        .prepare("SELECT path FROM files ORDER BY path")
        .expect("file path query should prepare")
        .query_map([], |row| row.get::<_, String>(0))
        .expect("file path query should execute")
        .collect::<Result<Vec<_>, _>>()
        .expect("file paths should load")
}

fn heading_id(connection: &Connection, path: &Path, title: &str) -> Option<i64> {
    connection
        .query_row(
            "SELECT headings.id
             FROM headings
             INNER JOIN files ON files.id = headings.file_id
             WHERE files.path = ?1 AND headings.level > 0 AND headings.title = ?2",
            params![path.display().to_string(), title],
            |row| row.get(0),
        )
        .optional()
        .expect("heading id should load")
}

fn file_fingerprint(connection: &Connection, path: &Path) -> Option<(i64, i64, Option<String>)> {
    connection
        .query_row(
            "SELECT mtime_ns, size, content_hash FROM files WHERE path = ?1",
            [path.display().to_string()],
            |row| Ok((row.get(0)?, row.get(1)?, row.get(2)?)),
        )
        .optional()
        .expect("file fingerprint should load")
}

fn link_rows(connection: &Connection) -> Vec<(String, String, Option<String>)> {
    connection
        .prepare(
            "SELECT source_files.path, links.raw, links.resolution_status
             FROM links
             INNER JOIN files AS source_files ON source_files.id = links.file_id
             ORDER BY source_files.path, links.byte_start",
        )
        .expect("link query should prepare")
        .query_map([], |row| Ok((row.get(0)?, row.get(1)?, row.get(2)?)))
        .expect("link query should execute")
        .collect::<Result<Vec<_>, _>>()
        .expect("links should load")
}

fn resolved_link_target(connection: &Connection) -> Option<(String, String)> {
    connection
        .query_row(
            "SELECT target_files.path, target_headings.title
             FROM links
             INNER JOIN files AS target_files ON target_files.id = links.target_file_id
             INNER JOIN headings AS target_headings ON target_headings.id = links.target_heading_id
             WHERE links.resolution_status = 'resolved'",
            [],
            |row| Ok((row.get(0)?, row.get(1)?)),
        )
        .optional()
        .expect("resolved link target should load")
}

fn fts_match_count(connection: &Connection, expression: &str) -> i64 {
    connection
        .query_row(
            "SELECT COUNT(*) FROM heading_fts WHERE heading_fts MATCH ?1",
            [expression],
            |row| row.get(0),
        )
        .expect("FTS match count should load")
}

#[derive(Debug, PartialEq, Eq)]
struct HeadingSnapshotRow {
    file_path: String,
    level: i64,
    byte_start: i64,
    title: String,
    todo_keyword: Option<String>,
    priority: Option<String>,
}

#[derive(Debug, PartialEq, Eq)]
struct BodySnapshotRow {
    file_path: String,
    level: i64,
    title: String,
    body_text: String,
}

#[derive(Debug, PartialEq, Eq)]
struct LinkSnapshotRow {
    source_path: String,
    raw: String,
    resolution_status: String,
    target_path: Option<String>,
    target_title: Option<String>,
    resolution_diagnostic: Option<String>,
}

#[derive(Debug, PartialEq, Eq)]
struct FtsSnapshotRow {
    file_path: String,
    level: i64,
    title: String,
}

#[derive(Debug, PartialEq, Eq)]
struct SemanticSnapshot {
    files: Vec<String>,
    headings: Vec<HeadingSnapshotRow>,
    bodies: Vec<BodySnapshotRow>,
    links: Vec<LinkSnapshotRow>,
    fts_headings: Vec<FtsSnapshotRow>,
    search_metadata: Vec<(String, String)>,
}

fn semantic_snapshot(connection: &Connection, fts_enabled: bool) -> SemanticSnapshot {
    let files = indexed_paths(connection);
    let headings = connection
        .prepare(
            "SELECT files.path,
                    headings.level,
                    headings.byte_start,
                    headings.title,
                    headings.todo_keyword,
                    headings.priority
             FROM headings
             INNER JOIN files ON files.id = headings.file_id
             ORDER BY files.path, headings.level, headings.byte_start, headings.title",
        )
        .expect("semantic heading query should prepare")
        .query_map([], |row| {
            Ok(HeadingSnapshotRow {
                file_path: row.get(0)?,
                level: row.get(1)?,
                byte_start: row.get(2)?,
                title: row.get(3)?,
                todo_keyword: row.get(4)?,
                priority: row.get(5)?,
            })
        })
        .expect("semantic heading query should execute")
        .collect::<Result<Vec<_>, _>>()
        .expect("semantic headings should load");
    let bodies = connection
        .prepare(
            "SELECT files.path, headings.level, headings.title, heading_bodies.body_text
             FROM heading_bodies
             INNER JOIN headings ON headings.id = heading_bodies.heading_id
             INNER JOIN files ON files.id = headings.file_id
             ORDER BY files.path, headings.level, headings.byte_start, headings.title",
        )
        .expect("semantic body query should prepare")
        .query_map([], |row| {
            Ok(BodySnapshotRow {
                file_path: row.get(0)?,
                level: row.get(1)?,
                title: row.get(2)?,
                body_text: row.get(3)?,
            })
        })
        .expect("semantic body query should execute")
        .collect::<Result<Vec<_>, _>>()
        .expect("semantic bodies should load");
    let links = connection
        .prepare(
            "SELECT source_files.path,
                    links.raw,
                    COALESCE(links.resolution_status, ''),
                    target_files.path,
                    target_headings.title,
                    links.resolution_diagnostic
             FROM links
             INNER JOIN files AS source_files ON source_files.id = links.file_id
             LEFT JOIN files AS target_files ON target_files.id = links.target_file_id
             LEFT JOIN headings AS target_headings ON target_headings.id = links.target_heading_id
             ORDER BY source_files.path, links.byte_start, links.raw",
        )
        .expect("semantic link query should prepare")
        .query_map([], |row| {
            Ok(LinkSnapshotRow {
                source_path: row.get(0)?,
                raw: row.get(1)?,
                resolution_status: row.get(2)?,
                target_path: row.get(3)?,
                target_title: row.get(4)?,
                resolution_diagnostic: row.get(5)?,
            })
        })
        .expect("semantic link query should execute")
        .collect::<Result<Vec<_>, _>>()
        .expect("semantic links should load");
    let fts_headings = if fts_enabled {
        connection
            .prepare(
                "SELECT files.path, headings.level, headings.title
                 FROM heading_fts
                 INNER JOIN headings ON headings.id = heading_fts.rowid
                 INNER JOIN files ON files.id = headings.file_id
                 ORDER BY files.path, headings.level, headings.byte_start, headings.title",
            )
            .expect("semantic FTS query should prepare")
            .query_map([], |row| {
                Ok(FtsSnapshotRow {
                    file_path: row.get(0)?,
                    level: row.get(1)?,
                    title: row.get(2)?,
                })
            })
            .expect("semantic FTS query should execute")
            .collect::<Result<Vec<_>, _>>()
            .expect("semantic FTS headings should load")
    } else {
        Vec::new()
    };
    let search_metadata = connection
        .prepare(
            "SELECT key, value
             FROM db_metadata
             WHERE key IN (
                 'body_text_available',
                 'fts_available',
                 'fts_body_indexed',
                 'fts_schema_version'
             )
             ORDER BY key",
        )
        .expect("search metadata query should prepare")
        .query_map([], |row| Ok((row.get(0)?, row.get(1)?)))
        .expect("search metadata query should execute")
        .collect::<Result<Vec<_>, _>>()
        .expect("search metadata should load");

    SemanticSnapshot {
        files,
        headings,
        bodies,
        links,
        fts_headings,
        search_metadata,
    }
}

#[test]
fn real_backend_no_change_startup_preserves_indexed_rows() {
    let test_dir = TestDir::new("no-change-startup");
    let notes = test_dir.path().join("notes");
    let note = notes.join("note.org");
    fs::create_dir_all(&notes).expect("notes directory should exist");
    write_file(&note, "* Stable\nBody\n");
    let config = recursive_config(&test_dir, &notes, test_dir.path().join("db.sqlite"));
    let mut harness = RealWatcherHarness::new_stopped(config);

    harness.rebuild_without_watcher();
    let heading_before = heading_id(&harness.connection, &note, "Stable");
    let fingerprint_before = file_fingerprint(&harness.connection, &note);

    harness.start();

    assert_eq!(
        heading_id(&harness.connection, &note, "Stable"),
        heading_before
    );
    assert_eq!(
        file_fingerprint(&harness.connection, &note),
        fingerprint_before
    );
    assert_eq!(heading_titles(&harness.connection), vec!["Stable"]);
    harness.settle_quietly("no-change startup");
}

#[test]
fn real_backend_converges_for_recursive_changes_and_event_bursts() {
    let test_dir = TestDir::new("recursive-changes");
    let notes = test_dir.path().join("notes");
    let alpha = notes.join("alpha.org");
    fs::create_dir_all(&notes).expect("notes directory should exist");
    let alpha_initial = "* Alpha\nInitial body\n";
    write_file(&alpha, alpha_initial);
    let config = recursive_config(&test_dir, &notes, test_dir.path().join("db.sqlite"));
    let mut harness = RealWatcherHarness::new_started(config);

    assert_eq!(heading_titles(&harness.connection), vec!["Alpha"]);
    let initial_heading_id =
        heading_id(&harness.connection, &alpha, "Alpha").expect("initial heading should exist");
    let initial_fingerprint =
        file_fingerprint(&harness.connection, &alpha).expect("initial file should exist");

    rewrite_same_content_with_new_mtime(&alpha, alpha_initial);
    harness.wait_until("same-content metadata-only update", |connection| {
        file_fingerprint(connection, &alpha)
            .is_some_and(|fingerprint| fingerprint.0 != initial_fingerprint.0)
    });
    assert_eq!(
        heading_id(&harness.connection, &alpha, "Alpha"),
        Some(initial_heading_id),
        "metadata-only updates must not replace heading rows"
    );
    assert_eq!(
        file_fingerprint(&harness.connection, &alpha).and_then(|fingerprint| fingerprint.2),
        initial_fingerprint.2,
        "same-content rewrites must keep the content hash"
    );

    write_file(&alpha, "* Modified Alpha\nChanged body\n");
    harness.wait_until("modified Org content", |connection| {
        heading_titles(connection) == vec!["Modified Alpha"]
    });

    for index in 0..8 {
        write_file(
            &alpha,
            &format!("* Alpha Burst {index}\nBurst body {index}\n"),
        );
    }
    write_file(&alpha, "* Final Alpha\nFinal alpha body\n");
    harness.wait_until("rapid writes to one file", |connection| {
        heading_titles(connection) == vec!["Final Alpha"]
    });

    let beta = notes.join("beta.org");
    let gamma = notes.join("nested/gamma.org");
    for index in 0..6 {
        write_file(&beta, &format!("* Beta Burst {index}\n"));
        write_file(&gamma, &format!("* Gamma Burst {index}\n"));
    }
    write_file(&beta, "* Final Beta\n");
    write_file(&gamma, "* Final Gamma\n");
    harness.wait_until("rapid writes to multiple files", |connection| {
        heading_titles(connection) == vec!["Final Alpha", "Final Beta", "Final Gamma"]
    });

    let renamed = notes.join("renamed.org");
    fs::rename(&alpha, &renamed).expect("Org file rename should succeed");
    harness.wait_until("renamed Org file", |connection| {
        let paths = indexed_paths(connection);
        paths.contains(&renamed.display().to_string())
            && !paths.contains(&alpha.display().to_string())
            && heading_titles(connection) == vec!["Final Alpha", "Final Beta", "Final Gamma"]
    });

    replace_atomically(&renamed, "* Atomic Replacement\nAtomic body\n");
    harness.wait_until("atomic editor-style replacement", |connection| {
        heading_titles(connection) == vec!["Atomic Replacement", "Final Beta", "Final Gamma"]
    });

    fs::remove_file(&renamed).expect("renamed file should be deleted");
    harness.wait_until("deleted Org file", |connection| {
        heading_titles(connection) == vec!["Final Beta", "Final Gamma"]
            && !indexed_paths(connection).contains(&renamed.display().to_string())
    });
    harness.settle_quietly("recursive change stream");

    let paths = indexed_paths(&harness.connection);
    assert_eq!(
        paths.iter().collect::<BTreeSet<_>>().len(),
        paths.len(),
        "duplicate or redundant backend events must not duplicate indexed files"
    );
}

#[test]
fn real_backend_reconciles_recursive_directory_rename_and_delete() {
    let test_dir = TestDir::new("directory-topology");
    let notes = test_dir.path().join("notes");
    let nested = notes.join("nested");
    let renamed = notes.join("renamed");
    let alpha = nested.join("alpha.org");
    let beta = nested.join("deeper/beta.org");
    fs::create_dir_all(&notes).expect("notes directory should exist");
    write_file(&alpha, "* Alpha\n");
    write_file(&beta, "* Beta\n");
    let config = recursive_config(&test_dir, &notes, test_dir.path().join("db.sqlite"));
    let mut harness = RealWatcherHarness::new_started(config);

    fs::rename(&nested, &renamed).expect("nested directory rename should succeed");
    let renamed_alpha = renamed.join("alpha.org");
    let renamed_beta = renamed.join("deeper/beta.org");
    harness.wait_until("recursive directory rename", |connection| {
        let paths = indexed_paths(connection);
        paths.contains(&renamed_alpha.display().to_string())
            && paths.contains(&renamed_beta.display().to_string())
            && !paths.contains(&alpha.display().to_string())
            && !paths.contains(&beta.display().to_string())
            && heading_titles(connection) == vec!["Alpha", "Beta"]
    });

    fs::remove_dir_all(&renamed).expect("renamed directory should be deleted");
    harness.wait_until("recursive directory deletion", |connection| {
        indexed_paths(connection).is_empty() && heading_titles(connection).is_empty()
    });
}

#[test]
fn real_backend_refreshes_symlink_identity_and_external_target_watches() {
    use std::os::unix::fs::symlink;

    let test_dir = TestDir::new("symlink-retarget");
    let notes = test_dir.path().join("notes");
    let outside = test_dir.path().join("outside");
    let first = outside.join("first.org");
    let second = outside.join("second.org");
    let alias = notes.join("alias.org");
    fs::create_dir_all(&notes).expect("notes directory should exist");
    write_file(&first, "* First\n");
    write_file(&second, "* Second\n");
    symlink(&first, &alias).expect("initial symlink should be created");
    let config = recursive_config(&test_dir, &notes, test_dir.path().join("db.sqlite"));
    let mut harness = RealWatcherHarness::new_started(config);

    assert_eq!(heading_titles(&harness.connection), vec!["First"]);
    fs::remove_file(&alias).expect("old symlink should be removed");
    symlink(&second, &alias).expect("replacement symlink should be created");
    harness.wait_until("file symlink retarget", |connection| {
        heading_titles(connection) == vec!["Second"]
            && indexed_paths(connection) == vec![second.display().to_string()]
    });

    write_file(&second, "* Second Updated\n");
    harness.wait_until("external symlink target modification", |connection| {
        heading_titles(connection) == vec!["Second Updated"]
    });
}

#[test]
fn real_backend_respects_scope_exclusions_and_database_activity() {
    let test_dir = TestDir::new("scope-and-exclusions");
    let recursive = test_dir.path().join("recursive");
    let shallow = test_dir.path().join("shallow");
    fs::create_dir_all(&recursive).expect("recursive root should exist");
    fs::create_dir_all(shallow.join("nested")).expect("shallow nested directory should exist");

    write_file(&recursive.join("direct.org"), "* Recursive Direct\n");
    write_file(
        &recursive.join("nested/initial.org"),
        "* Recursive Nested\n",
    );
    write_file(&recursive.join("archive/old.org"), "* Excluded Archive\n");
    write_file(&recursive.join("ignored.org"), "* Globally Excluded\n");
    write_file(&shallow.join("direct.org"), "* Shallow Direct\n");
    write_file(
        &shallow.join("nested/existing.org"),
        "* Shallow Nested Existing\n",
    );

    let db_path = recursive.join("watcher.sqlite");
    let config = config_with_sources(
        &test_dir,
        db_path.clone(),
        Vec::new(),
        vec![
            ConfiguredDir {
                path: recursive.clone(),
                recursive: true,
                exclude: vec!["archive/**".to_string()],
            },
            ConfiguredDir {
                path: shallow.clone(),
                recursive: false,
                exclude: Vec::new(),
            },
        ],
        vec!["recursive/ignored*.org".to_string()],
        false,
        false,
    );
    let mut harness = RealWatcherHarness::new_started(config);

    assert_eq!(
        heading_titles(&harness.connection),
        vec!["Recursive Direct", "Recursive Nested", "Shallow Direct"]
    );

    write_file(
        &recursive.join("nested/new.org"),
        "* Recursive New Nested\n",
    );
    harness.wait_until("recursive directory discovery", |connection| {
        heading_titles(connection)
            == vec![
                "Recursive Direct",
                "Recursive Nested",
                "Recursive New Nested",
                "Shallow Direct",
            ]
    });

    write_file(&shallow.join("nested/new.org"), "* Shallow New Nested\n");
    write_file(
        &recursive.join("archive/new.org"),
        "* Excluded Archive New\n",
    );
    write_file(
        &recursive.join("ignored-second.org"),
        "* Globally Excluded New\n",
    );
    write_file(&recursive.join("unrelated.txt"), "not Org\n");

    let expected = vec![
        "Recursive Direct".to_string(),
        "Recursive Nested".to_string(),
        "Recursive New Nested".to_string(),
        "Shallow Direct".to_string(),
    ];
    harness.assert_stays_for(
        "non-recursive, excluded, and unrelated events",
        Duration::from_millis(900),
        |connection| heading_titles(connection) == expected,
    );
    harness.settle_quietly("SQLite WAL and sidecar activity inside the watched root");

    assert!(db_path.exists(), "watched database should exist");
    assert_eq!(heading_titles(&harness.connection), expected);
}

#[test]
fn real_backend_handles_explicit_replacement_and_offline_restart_changes() {
    let test_dir = TestDir::new("explicit-and-restart");
    let first = test_dir.path().join("first.org");
    let second = test_dir.path().join("second.org");
    write_file(&first, "* First\n");
    write_file(&second, "* Second\n");
    let config = config_with_sources(
        &test_dir,
        test_dir.path().join("db.sqlite"),
        vec![first.clone(), second.clone()],
        Vec::new(),
        Vec::new(),
        false,
        false,
    );
    let mut harness = RealWatcherHarness::new_started(config);

    replace_atomically(&first, "* First Replaced\n");
    harness.wait_until("explicit-file atomic replacement", |connection| {
        heading_titles(connection) == vec!["First Replaced", "Second"]
    });

    harness.stop();
    write_file(&first, "* First Offline\n");
    fs::remove_file(&second).expect("second explicit file should be removed offline");
    harness.restart();
    assert_eq!(heading_titles(&harness.connection), vec!["First Offline"]);
    assert_eq!(
        indexed_paths(&harness.connection),
        vec![first.display().to_string()]
    );

    harness.stop();
    write_file(&second, "* Second Restored Offline\n");
    harness.restart();
    assert_eq!(
        heading_titles(&harness.connection),
        vec!["First Offline", "Second Restored Offline"]
    );
}

#[test]
fn real_backend_invalid_input_leaves_the_last_committed_state_intact() {
    let test_dir = TestDir::new("invalid-input");
    let notes = test_dir.path().join("notes");
    let note = notes.join("note.org");
    fs::create_dir_all(&notes).expect("notes directory should exist");
    write_file(&note, "* Committed\n** Child\nBody\n");
    let config = recursive_config(&test_dir, &notes, test_dir.path().join("db.sqlite"));
    let mut harness = RealWatcherHarness::new_started(config);
    let committed_paths = indexed_paths(&harness.connection);
    let committed_titles = heading_titles(&harness.connection);

    write_bytes(&note, &[0xff, 0xfe, 0xfd]);
    let error = harness.wait_for_runtime_error("invalid UTF-8 Org input");

    assert!(
        matches!(error.as_ref(), WatcherRuntimeError::Execution { .. }),
        "invalid input should fail indexer execution, got {error}"
    );
    assert_eq!(indexed_paths(&harness.connection), committed_paths);
    assert_eq!(heading_titles(&harness.connection), committed_titles);
    assert_eq!(
        harness
            .runtime
            .as_ref()
            .expect("runtime should remain inspectable")
            .state(),
        WatcherRuntimeState::Terminated
    );
}

#[test]
fn real_backend_root_disruption_does_not_become_mass_deletion() {
    let test_dir = TestDir::new("root-disruption");
    let notes = test_dir.path().join("notes");
    let moved_notes = test_dir.path().join("notes-unavailable");
    let note = notes.join("note.org");
    fs::create_dir_all(&notes).expect("notes directory should exist");
    write_file(&note, "* Preserved\n");
    let config = recursive_config(&test_dir, &notes, test_dir.path().join("db.sqlite"));
    let mut harness = RealWatcherHarness::new_started(config);
    let committed_paths = indexed_paths(&harness.connection);
    let committed_titles = heading_titles(&harness.connection);

    write_file(&notes.join("trigger.org"), "* Pending Trigger\n");
    harness.wait_for_source_message("root disruption trigger");
    fs::rename(&notes, &moved_notes).expect("watched root should become temporarily unavailable");

    let error = harness.wait_for_runtime_error("temporary source-root disruption");
    assert!(
        matches!(error.as_ref(), WatcherRuntimeError::Source(_)),
        "unavailable watch root should fail source validation, got {error}"
    );
    assert_eq!(indexed_paths(&harness.connection), committed_paths);
    assert_eq!(heading_titles(&harness.connection), committed_titles);
}

#[test]
fn real_backend_state_matches_fresh_configured_source_reconciliation_with_derived_state() {
    let test_dir = TestDir::new("reconciliation-comparison");
    let notes = test_dir.path().join("notes");
    let source = notes.join("source.org");
    let target = notes.join("target.org");
    fs::create_dir_all(&notes).expect("notes directory should exist");
    write_file(&source, "* Source\n[[file:target.org::*Target][target]]\n");
    write_file(&target, "* Target\nSearchable initial phrase.\n");

    let probe = Connection::open_in_memory().expect("FTS probe connection should open");
    let fts_enabled = sqlite_supports_fts5(&probe).expect("FTS5 capability probe should run");
    let config = config_with_sources(
        &test_dir,
        test_dir.path().join("watcher.sqlite"),
        Vec::new(),
        vec![ConfiguredDir {
            path: notes.clone(),
            recursive: true,
            exclude: Vec::new(),
        }],
        Vec::new(),
        fts_enabled,
        true,
    );
    let mut harness = RealWatcherHarness::new_started(config.clone());

    write_file(
        &source,
        "* Source Updated\n[[file:target.org::*Renamed Target][target]]\n",
    );
    write_file(
        &target,
        "* Renamed Target\nSearchable final phrase for watcher convergence.\n",
    );
    harness.wait_until("resolved link and derived state", |connection| {
        resolved_link_target(connection)
            == Some((target.display().to_string(), "Renamed Target".to_string()))
            && heading_titles(connection) == vec!["Renamed Target", "Source Updated"]
            && (!fts_enabled
                || (fts_match_count(connection, "Renamed") > 0
                    && fts_match_count(connection, "convergence") > 0))
    });
    harness.settle_quietly("derived state convergence");

    let watcher_snapshot = semantic_snapshot(&harness.connection, fts_enabled);
    let mut fresh_config = config;
    fresh_config.db_path = test_dir.path().join("fresh-reconciliation.sqlite");
    let schema = SchemaDefinition::new(CURRENT_SCHEMA_VERSION, fts_enabled);
    let mut fresh_connection = open_database_with_schema(&fresh_config.db_path, &schema)
        .expect("fresh comparison database should open");
    let fresh_indexer = Indexer::new(OrgizeAdapter::new());
    fresh_indexer
        .rebuild(&mut fresh_connection, &fresh_config)
        .expect("fresh rebuild should succeed");
    match fresh_indexer
        .reconcile_configured_sources(&mut fresh_connection, &fresh_config)
        .expect("fresh configured-source reconciliation should run")
    {
        ChangeApplicationResult::Applied(_) => {}
        ChangeApplicationResult::Rejected(rejection) => {
            panic!("fresh configured-source reconciliation was rejected: {rejection:?}")
        }
    }
    let fresh_snapshot = semantic_snapshot(&fresh_connection, fts_enabled);

    assert_eq!(watcher_snapshot, fresh_snapshot);
}
