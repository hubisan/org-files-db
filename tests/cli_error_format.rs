//! Contract tests for `--error-format` (issue #13).

use std::{path::Path, process::Command};

use serde_json::Value;

fn run(args: &[&str]) -> (i32, String) {
    let output = Command::new(env!("CARGO_BIN_EXE_orgfdb"))
        .args(args)
        .output()
        .expect("run orgfdb");
    (
        output.status.code().expect("exit code"),
        String::from_utf8(output.stderr).expect("utf8 stderr"),
    )
}

fn json_error(stderr: &str) -> Value {
    assert_eq!(stderr.lines().count(), 1, "exactly one line: {stderr:?}");
    let value: Value = serde_json::from_str(stderr).expect("stderr is JSON");
    let error = value.get("error").expect("error object").clone();
    assert!(error["kind"].is_string());
    assert!(error["message"].is_string());
    error
}

fn write_config(dir: &Path, fts: bool) -> String {
    std::fs::write(dir.join("notes.org"), "* Heading\n").unwrap();
    let path = dir.join(format!("config-{fts}.toml"));
    std::fs::write(
        &path,
        format!(
            "db_path = \"./db.sqlite\"\nfiles = [\"notes.org\"]\n\n[search]\nfts5_enabled = {fts}\n"
        ),
    )
    .unwrap();
    path.display().to_string()
}

#[test]
fn headings_config_error_is_json_with_path() {
    let dir = tempdir("headings");
    let missing = dir.join("missing.toml").display().to_string();
    let (code, stderr) = run(&["headings", "--config", &missing, "--error-format", "json"]);
    assert_eq!(code, 1);
    let error = json_error(&stderr);
    assert_eq!(error["kind"], "config");
    assert_eq!(error["path"], missing.as_str());
}

#[test]
fn links_missing_database_is_json_and_flag_position_is_free() {
    let dir = tempdir("links");
    let config = write_config(&dir, true);
    let (code, stderr) = run(&["--error-format=json", "links", "--config", &config]);
    assert_eq!(code, 1);
    assert_eq!(json_error(&stderr)["kind"], "database");
}

#[test]
fn query_invalid_is_json() {
    let dir = tempdir("query");
    let config = build_index(&dir);
    let (code, stderr) = run(&[
        "query",
        "(headings",
        "--config",
        &config,
        "--error-format",
        "json",
    ]);
    assert_eq!(code, 1);
    assert_eq!(json_error(&stderr)["kind"], "query-invalid");
}

#[test]
fn search_disabled_is_json() {
    let dir = tempdir("search");
    build_index(&dir);
    let config = write_config(&dir, false);
    let (code, stderr) = run(&["search", "--config", &config, "x", "--error-format", "json"]);
    assert_eq!(code, 1);
    assert_eq!(json_error(&stderr)["kind"], "search-disabled");
}

#[test]
fn clap_usage_error_honors_json() {
    let (code, stderr) = run(&["headings", "--bogus", "--error-format", "json"]);
    assert_eq!(code, 2);
    assert_eq!(json_error(&stderr)["kind"], "usage");
}

#[test]
fn text_mode_is_unchanged() {
    let dir = tempdir("text");
    let missing = dir.join("missing.toml").display().to_string();
    for args in [
        vec!["headings", "--config", &missing],
        vec!["headings", "--config", &missing, "--error-format", "text"],
    ] {
        let (code, stderr) = run(&args);
        assert_eq!(code, 1);
        assert!(stderr.starts_with("failed to read config file"), "{stderr}");
        assert!(!stderr.starts_with('{'));
    }
    let config = build_index(&dir);
    let (code, stderr) = run(&["query", "(headings", "--config", &config]);
    assert_eq!(code, 1);
    assert!(!stderr.starts_with('{'), "{stderr}");
}

fn build_index(dir: &Path) -> String {
    let config = write_config(dir, true);
    let (code, stderr) = run(&["rebuild", "--config", &config]);
    assert_eq!(code, 0, "{stderr}");
    config
}

fn tempdir(name: &str) -> std::path::PathBuf {
    let dir = std::env::temp_dir().join(format!("orgfdb-errfmt-{name}-{}", std::process::id()));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).unwrap();
    dir
}
