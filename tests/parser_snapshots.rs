//! Snapshot tests for parser output.
//!
//! Every `tests/data/parser/<category>/<name>/fixture.org` is parsed with default options and
//! the serialized `ParsedOrgDocument` is compared with `snapshot.json` in the same directory.
//! Run `UPDATE_SNAPSHOTS=1 cargo test --test parser_snapshots` to rewrite the snapshots, then
//! review the diff. `tests/parser_contract_guard.rs` hashes the snapshots, so a changed
//! snapshot also requires a `PARSER_INDEXER_CONTRACT_VERSION` bump.

use std::{
    fs,
    path::{Path, PathBuf},
};

use org_files_db::parser::{OrgParser, OrgizeAdapter, ParseOptions};

const CATEGORIES: [&str; 9] = [
    "body",
    "timestamps",
    "headings",
    "links",
    "planning",
    "properties",
    "priorities",
    "todo-keywords",
    "file-scope",
];
const MAX_DIFF_LINES: usize = 12;

fn fixture_root() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("tests/data/parser")
}

fn fixture_dirs() -> Vec<PathBuf> {
    let mut fixtures = Vec::new();
    for category in CATEGORIES {
        let category_path = fixture_root().join(category);
        let entries = fs::read_dir(&category_path)
            .unwrap_or_else(|err| panic!("failed to read {}: {err}", category_path.display()));
        for entry in entries {
            let path = entry.expect("fixture entry should load").path();
            if path.is_dir() {
                fixtures.push(path);
            }
        }
    }
    fixtures.sort();
    fixtures
}

/// Deterministic pretty JSON of the parsed document (struct field order, vectors in source order).
fn render_snapshot(dir: &Path) -> String {
    let fixture = dir.join("fixture.org");
    let content = fs::read_to_string(&fixture)
        .unwrap_or_else(|err| panic!("failed to read {}: {err}", fixture.display()));
    let relative = dir
        .strip_prefix(fixture_root())
        .expect("fixture under root")
        .join("fixture.org");
    let document = OrgizeAdapter::new()
        .parse_document(&relative, &content, &ParseOptions::default())
        .unwrap_or_else(|err| panic!("fixture {} should parse: {err:?}", fixture.display()));
    let mut json = serde_json::to_string_pretty(&document).expect("document should serialize");
    json.push('\n');
    json
}

/// Readable line diff: the first differing lines plus the total count.
fn line_diff(expected: &str, actual: &str) -> String {
    let expected: Vec<&str> = expected.lines().collect();
    let actual: Vec<&str> = actual.lines().collect();
    let mut out = String::new();
    let mut shown = 0;
    let mut total = 0;
    for index in 0..expected.len().max(actual.len()) {
        let (old, new) = (expected.get(index), actual.get(index));
        if old == new {
            continue;
        }
        total += 1;
        if shown < MAX_DIFF_LINES {
            shown += 1;
            let line = index + 1;
            if let Some(old) = old {
                out.push_str(&format!("  line {line}: - {old}\n"));
            }
            if let Some(new) = new {
                out.push_str(&format!("  line {line}: + {new}\n"));
            }
        }
    }
    if total > shown {
        out.push_str(&format!("  ... {} more differing lines\n", total - shown));
    }
    out
}

#[test]
fn parser_output_matches_snapshots() {
    let update = std::env::var_os("UPDATE_SNAPSHOTS").is_some_and(|v| v != "0" && !v.is_empty());
    let mut failures = Vec::new();
    for dir in fixture_dirs() {
        let name = dir
            .strip_prefix(fixture_root())
            .unwrap()
            .display()
            .to_string();
        let snapshot_path = dir.join("snapshot.json");
        let actual = render_snapshot(&dir);
        let expected = fs::read_to_string(&snapshot_path).ok();
        if expected.as_deref() == Some(actual.as_str()) {
            continue;
        }
        if update {
            fs::write(&snapshot_path, &actual)
                .unwrap_or_else(|err| panic!("failed to write {}: {err}", snapshot_path.display()));
            eprintln!("updated snapshot {name}");
            continue;
        }
        failures.push(match expected {
            Some(expected) => format!(
                "snapshot mismatch for {name} ({}):\n{}",
                snapshot_path.display(),
                line_diff(&expected, &actual)
            ),
            None => format!("missing snapshot for {name} ({})", snapshot_path.display()),
        });
    }
    assert!(
        failures.is_empty(),
        "{}\nreview the change, then run `UPDATE_SNAPSHOTS=1 cargo test --test parser_snapshots` \
         and bump PARSER_INDEXER_CONTRACT_VERSION (see tests/parser_contract_guard.rs)",
        failures.join("\n")
    );
}

#[test]
fn parser_fixture_directories_follow_expected_layout() {
    for category in CATEGORIES {
        assert!(
            fixture_root().join(category).is_dir(),
            "missing fixture category {category}"
        );
    }
    for dir in fixture_dirs() {
        for file in ["fixture.org", "notes.txt", "snapshot.json"] {
            assert!(
                dir.join(file).is_file(),
                "missing {}",
                dir.join(file).display()
            );
        }
        let notes = fs::read_to_string(dir.join("notes.txt")).expect("notes should read");
        assert!(
            notes.contains("# classification:"),
            "missing classification comment in {}",
            dir.join("notes.txt").display()
        );
    }
}
