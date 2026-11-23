#![cfg(unix)]

use std::{
    fs,
    os::unix::fs::symlink,
    path::PathBuf,
    process::{Child, Command, Stdio},
    thread,
    time::{Duration, Instant},
};

use rusqlite::Connection;
use serde_json::{json, Value};

struct Fixture(PathBuf);

impl Fixture {
    fn new(name: &str) -> Self {
        let dir = std::env::temp_dir().join(format!("orgfdb-hidden-{name}-{}", std::process::id()));
        fs::create_dir(&dir).unwrap();
        fs::create_dir_all(dir.join("files/.private")).unwrap();
        fs::write(dir.join("files/.private/data.txt"), "data").unwrap();
        fs::write(dir.join("files/ordinary.org"), "* Ordinary\n").unwrap();
        symlink(".private", dir.join("files/ignored")).unwrap();
        fs::write(
            dir.join("config.toml"),
            r#"
db_path = "db.sqlite"

[search]
fts5_enabled = false

[[dirs]]
path = "files"
recursive = true
exclude = ["ignored/**"]
"#,
        )
        .unwrap();
        Self(dir)
    }

    fn cli(&self) -> Command {
        let mut cli = Command::new(env!("CARGO_BIN_EXE_orgfdb"));
        cli.current_dir(&self.0)
            .env("XDG_RUNTIME_DIR", self.0.join("run"))
            .env("XDG_CACHE_HOME", self.0.join("cache"));
        cli
    }

    fn output(&self, args: &[&str]) -> Vec<u8> {
        let output = self
            .cli()
            .args(args)
            .args(["--config", "config.toml"])
            .output()
            .unwrap();
        assert!(
            output.status.success(),
            "{args:?}: {}",
            String::from_utf8_lossy(&output.stderr),
        );
        output.stdout
    }

    fn run(&self, args: &[&str]) -> Value {
        serde_json::from_slice(&self.output(args))
            .unwrap_or_else(|error| panic!("{args:?} should emit JSON: {error}"))
    }

    fn start_watcher(&self) -> Watcher {
        let log = self.0.join("watch.log");
        let child = self
            .cli()
            .args(["watch", "--config", "config.toml"])
            .stdout(Stdio::null())
            .stderr(fs::File::create(&log).unwrap())
            .spawn()
            .unwrap();
        let mut watcher = Watcher(child);
        wait_until(|| {
            let stderr = fs::read_to_string(&log).unwrap();
            assert!(watcher.0.try_wait().unwrap().is_none(), "{stderr}");
            stderr.contains("watcher ready")
        });
        watcher
    }
}

impl Drop for Fixture {
    fn drop(&mut self) {
        let _ = fs::remove_dir_all(&self.0);
    }
}

struct Watcher(Child);

impl Drop for Watcher {
    fn drop(&mut self) {
        let _ = self.0.kill();
        let _ = self.0.wait();
    }
}

fn wait_until(mut condition: impl FnMut() -> bool) {
    let deadline = Instant::now() + Duration::from_secs(8);
    while !condition() {
        assert!(Instant::now() < deadline, "timed out waiting for watcher");
        thread::sleep(Duration::from_millis(20));
    }
}

#[test]
fn watcher_upgrades_visibility_and_refreshes_views_when_an_alias_changes() {
    const QUERY: &str = r#"(fs-files (extension "txt"))"#;
    const SPEC: &str = r#"{"columns":[{"name":"file-name"}]}"#;
    let fixture = Fixture::new("upgrade");
    let target = fixture
        .0
        .join("files/.private/data.txt")
        .canonicalize()
        .unwrap();
    let target_paths = json!([target]);
    let no_paths = json!([]);
    fixture.output(&["rebuild"]);
    let before_upgrade = fixture.run(&["status"]);
    {
        let legacy = Connection::open(fixture.0.join("db.sqlite")).unwrap();
        legacy
            .execute_batch(
                "DROP INDEX IF EXISTS idx_filesystem_visibility_pending;
                 ALTER TABLE filesystem_entries DROP COLUMN visible;
                 PRAGMA user_version = 15;",
            )
            .unwrap();
    }

    // A failed indexing command can leave the schema upgraded before rediscovery.
    let root = fixture.0.join("files");
    let moved_root = fixture.0.join("files-moved");
    fs::rename(&root, &moved_root).unwrap();
    let failed_rebuild = fixture
        .cli()
        .args(["rebuild", "--config", "config.toml"])
        .output()
        .unwrap();
    fs::rename(&moved_root, &root).unwrap();
    assert!(!failed_rebuild.status.success());
    assert_eq!(fixture.run(&["status"])["schema_version"], 16);

    let pending_query = fixture
        .cli()
        .args(["query", "--config", "config.toml", QUERY])
        .output()
        .unwrap();
    assert!(!pending_query.status.success());
    assert!(String::from_utf8_lossy(&pending_query.stderr)
        .contains("run an indexing command to finish upgrading filesystem visibility"));
    assert_eq!(
        fixture.run(&["query", "--hidden", QUERY])["results"][0]["path"],
        json!(target),
    );

    let _watcher = fixture.start_watcher();
    let upgraded = fixture.run(&["status"]);
    assert_eq!(upgraded["schema_version"], 16);
    assert_eq!(upgraded["database_id"], before_upgrade["database_id"]);
    assert!(
        upgraded["generation"].as_i64().unwrap() > before_upgrade["generation"].as_i64().unwrap()
    );

    let register = |name, show_hidden| {
        let mut args = vec![
            "view",
            "register",
            "--presentation-spec-json",
            SPEC,
            name,
            QUERY,
        ];
        if show_hidden {
            args.push("--hidden");
        }
        fixture.run(&args)
    };
    let read = |name| fixture.run(&["view", "read", name]);
    let default_registration = register("default", false);
    assert_eq!(read("default")["files"], no_paths);
    let changed = register("default", true);
    assert_eq!(changed["action"], "replaced");
    assert_ne!(changed["revision"], default_registration["revision"]);
    assert_eq!(read("default")["files"], target_paths);
    assert_eq!(register("default", false)["action"], "replaced");
    register("all", true);
    assert_eq!(
        fixture.run(&["view", "show", "all"])["definition"]["show_hidden"],
        true
    );
    assert_eq!(read("default")["files"], no_paths);
    assert_eq!(read("all")["files"], target_paths);

    let mut generation = upgraded["generation"].as_i64().unwrap();
    symlink(".private", root.join("visible")).unwrap();

    for (paths, next_change) in [
        (&target_paths, Some(("visible", ".hidden-alias"))),
        (&no_paths, Some((".hidden-alias", "visible"))),
        (&target_paths, None),
    ] {
        wait_until(|| {
            let current = fixture.run(&["status"])["generation"].as_i64().unwrap();
            if current <= generation {
                return false;
            }
            generation = current;
            true
        });
        let default = read("default");
        assert_eq!(&default["files"], paths);
        assert_eq!(default["generation"], generation);
        assert_eq!(read("all")["files"], target_paths);
        if let Some((from, to)) = next_change {
            fs::rename(root.join(from), root.join(to)).unwrap();
        }
    }

    fs::remove_file(root.join("visible")).unwrap();
    wait_until(|| fixture.run(&["status"])["generation"].as_i64().unwrap() > generation);
    assert_eq!(read("default")["files"], no_paths);
    assert_eq!(read("all")["files"], target_paths);
}

#[test]
fn watcher_refreshes_visibility_when_an_external_intermediate_symlink_changes() {
    const QUERY: &str = r#"(fs-files (extension "txt"))"#;
    let fixture = Fixture::new("indirect");
    let root = fixture.0.join("files");
    let hidden = root.join(".private/data.txt");
    let hidden_path = hidden.canonicalize().unwrap();
    let alternate = root.join(".alternate");
    fs::create_dir(&alternate).unwrap();
    fs::write(alternate.join("data.txt"), "new target").unwrap();
    let alternate_path = alternate.join("data.txt").canonicalize().unwrap();
    let external = fixture.0.join("external");
    fs::create_dir(&external).unwrap();
    let bridge = external.join("bridge");
    // The intermediate dependency is outside the configured root and is
    // neither the visible alias nor the final canonical target.
    symlink("../files/.private", &bridge).unwrap();
    symlink("../external/bridge/data.txt", root.join("visible.txt")).unwrap();

    fixture.output(&["rebuild"]);
    let _watcher = fixture.start_watcher();
    fixture.run(&[
        "view",
        "register",
        "--presentation-spec-json",
        r#"{"columns":[{"name":"file-name"}]}"#,
        "default",
        QUERY,
    ]);
    let read_view = || fixture.run(&["view", "read", "default"]);
    let mut generation = read_view()["generation"].as_i64().unwrap();
    assert_eq!(read_view()["files"], json!([hidden_path]));

    fs::remove_file(&bridge).unwrap();
    wait_until(|| fixture.run(&["status"])["generation"].as_i64().unwrap() > generation);
    let hidden_view = read_view();
    generation = hidden_view["generation"].as_i64().unwrap();
    assert_eq!(hidden_view["files"], json!([]));
    let all = fixture.run(&["query", "--hidden", QUERY]);
    let all_paths = all["results"]
        .as_array()
        .unwrap()
        .iter()
        .map(|row| row["path"].clone())
        .collect::<Vec<_>>();
    assert_eq!(all_paths, vec![json!(alternate_path), json!(hidden_path)]);

    symlink("../files/.alternate", &bridge).unwrap();
    wait_until(|| fixture.run(&["status"])["generation"].as_i64().unwrap() > generation);
    assert_eq!(read_view()["files"], json!([alternate_path]));
}
