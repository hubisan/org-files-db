use super::{search_json_rows, CliSearchScope, SearchError};
use crate::cli::{
    error::CliError,
    rebuild,
    tests::{build_search_fixture, write_search_config},
};
use crate::db::{
    open_database, open_database_with_schema, sqlite_supports_fts5, SchemaDefinition,
    CURRENT_SCHEMA_VERSION, DB_METADATA_FTS_AVAILABLE_KEY, DB_METADATA_FTS_BODY_INDEXED_KEY,
    DB_METADATA_FTS_SCHEMA_VERSION_KEY, FTS_SCHEMA_CONTRACT_VERSION,
};
use crate::test_support::{write_file, TestDir};
use rusqlite::Connection;
use std::{fs, path::Path};

fn trust_metadata_rows(db_path: &Path) -> Vec<(String, String)> {
    let connection = open_database(db_path).expect("database should open");
    let mut statement = connection
        .prepare(
            "SELECT key, value FROM db_metadata
             WHERE key IN (?1, ?2, ?3)
             ORDER BY key",
        )
        .expect("metadata query should prepare");
    let rows = statement
        .query_map(
            [
                DB_METADATA_FTS_AVAILABLE_KEY,
                DB_METADATA_FTS_BODY_INDEXED_KEY,
                DB_METADATA_FTS_SCHEMA_VERSION_KEY,
            ],
            |row| Ok((row.get(0)?, row.get(1)?)),
        )
        .expect("metadata query should run")
        .collect::<Result<Vec<_>, _>>()
        .expect("metadata rows should collect");
    rows
}

fn expected_trust_metadata_rows() -> Vec<(String, String)> {
    vec![
        (DB_METADATA_FTS_AVAILABLE_KEY.to_string(), "1".to_string()),
        (
            DB_METADATA_FTS_BODY_INDEXED_KEY.to_string(),
            "1".to_string(),
        ),
        (
            DB_METADATA_FTS_SCHEMA_VERSION_KEY.to_string(),
            FTS_SCHEMA_CONTRACT_VERSION.to_string(),
        ),
    ]
}

#[test]
fn search_rejects_body_scope_when_trusted_index_is_title_only() {
    let (_test_dir, config_path, _) = build_search_fixture(
        "search-title-only",
        &[(
            "notes.org",
            "* Searchable Heading\nBody phrase for sqlite search.\n",
        )],
        false,
    );

    let error = search_json_rows(CliSearchScope::Body, "phrase", Some(&config_path))
        .expect_err("body scope should fail for title-only index");
    assert!(matches!(
        error,
        CliError::Search(SearchError::BodyScopeUnavailable)
    ));
}

#[test]
fn rebuild_persists_search_trust_metadata_for_search_command() {
    let (_test_dir, _config_path, db_path) = build_search_fixture(
        "search-trust-metadata",
        &[(
            "notes.org",
            "* Searchable Heading\nBody phrase for sqlite search.\n",
        )],
        true,
    );

    assert_eq!(
        trust_metadata_rows(&db_path),
        expected_trust_metadata_rows()
    );
}

#[test]
fn search_rejects_missing_trust_metadata_and_requires_rebuild() {
    let test_dir = TestDir::new("search-missing-metadata");
    let db_path = test_dir.path().join("db.sqlite");
    let config_path = test_dir.path().join("config.toml");
    write_search_config(&config_path, "./db.sqlite", &[], true, true);

    let connection = open_database_with_schema(
        &db_path,
        &SchemaDefinition::new(CURRENT_SCHEMA_VERSION, true),
    )
    .expect("database should open");
    drop(connection);

    let error = search_json_rows(CliSearchScope::All, "sqlite", Some(&config_path))
        .expect_err("missing metadata should fail");
    match error {
        CliError::Search(SearchError::MissingTrustMetadata) => {}
        other => panic!("unexpected error: {other}"),
    }
    assert!(error.to_string().contains("run orgfdb rebuild"));
}

/// Each row tampers with a trusted index in one or more steps; after every step search must
/// fail with the expected error. Rebuild-hint rows must tell the user to run `orgfdb rebuild`.
#[test]
fn search_rejects_tampered_trust_state() {
    type ErrorCheck = fn(&CliError) -> bool;
    let set_version = |value: &str| {
        format!(
            "UPDATE db_metadata SET value = '{value}' WHERE key = '{DB_METADATA_FTS_SCHEMA_VERSION_KEY}'"
        )
    };
    let stale: ErrorCheck =
        |error| matches!(error, CliError::Search(SearchError::MissingTrustedIndex));
    let cases: Vec<(&str, Vec<String>, ErrorCheck, bool)> = vec![
        (
            "non-current numeric FTS contract versions are stale",
            ["0", "1", "2", "4"]
                .iter()
                .map(|v| set_version(v))
                .collect(),
            stale,
            true,
        ),
        (
            "non-numeric FTS contract metadata is invalid",
            vec![set_version("invalid")],
            |error| matches!(error, CliError::Search(SearchError::InvalidTrustMetadata)),
            false,
        ),
        (
            "missing heading_fts table is untrusted",
            vec!["DROP TABLE heading_fts;".to_string()],
            stale,
            false,
        ),
        (
            "incompatible heading_fts schema is rejected",
            vec![
                "DROP TABLE heading_fts; CREATE TABLE heading_fts (title TEXT, body TEXT);"
                    .to_string(),
            ],
            |error| {
                matches!(
                    error,
                    CliError::Search(SearchError::IncompatibleIndexSchema)
                )
            },
            false,
        ),
    ];

    for (label, steps, is_expected, expects_rebuild_hint) in cases {
        let (_test_dir, config_path, db_path) = build_search_fixture(
            "search-tampered-trust",
            &[("notes.org", "* Searchable Heading\nBody phrase.\n")],
            true,
        );
        let connection = open_database(&db_path).expect("database should open");
        for step in &steps {
            connection
                .execute_batch(step)
                .expect("tamper step should apply");
            let error = search_json_rows(CliSearchScope::All, "Searchable", Some(&config_path))
                .expect_err(label);
            assert!(is_expected(&error), "{label}: unexpected error {error}");
            if expects_rebuild_hint {
                assert!(error.to_string().contains("run orgfdb rebuild"), "{label}");
            }
        }
    }
}

#[test]
fn search_accepts_trusted_empty_fts_rebuilds() {
    let probe = Connection::open_in_memory().expect("probe should open");
    if !sqlite_supports_fts5(&probe).expect("fts5 probe should run") {
        return;
    }

    let test_dir = TestDir::new("search-empty-trusted-rebuild");
    let config_path = test_dir.path().join("config.toml");
    let db_path = test_dir.path().join("db.sqlite");
    write_file(
        &config_path,
        r#"
db_path = "./db.sqlite"

[search]
fts5_enabled = true
index_body_text = true
"#,
    );

    let report = rebuild(&config_path).expect("empty rebuild should succeed");
    assert!(report.indexed_files.is_empty());
    assert!(report.diagnostics.is_empty());

    let rows = search_json_rows(CliSearchScope::All, "sqlite", Some(&config_path))
        .expect("search should trust the empty rebuild");
    assert!(rows.is_empty());

    assert_eq!(
        trust_metadata_rows(&db_path),
        expected_trust_metadata_rows()
    );
}

#[test]
fn search_rejects_disabled_config_even_when_index_is_trusted() {
    let (test_dir, trusted_config_path, _db_path) = build_search_fixture(
        "search-disabled-config",
        &[("notes.org", "* Searchable Heading\nBody phrase.\n")],
        true,
    );
    let disabled_config_path = test_dir.path().join("disabled.toml");
    write_search_config(
        &disabled_config_path,
        "./db.sqlite",
        &["notes.org"],
        false,
        true,
    );

    let error = search_json_rows(
        CliSearchScope::All,
        "Searchable",
        Some(&disabled_config_path),
    )
    .expect_err("disabled config should fail");
    assert!(matches!(
        error,
        CliError::Search(SearchError::DisabledByConfig)
    ));

    let trusted_rows = search_json_rows(
        CliSearchScope::All,
        "Searchable",
        Some(&trusted_config_path),
    )
    .expect("trusted config should still work");
    assert_eq!(trusted_rows.len(), 1);
}

#[test]
fn search_rejects_invalid_fts_expression_without_raw_sqlite_leak() {
    let (_test_dir, config_path, _) = build_search_fixture(
        "search-invalid-expression",
        &[("notes.org", "* Searchable Heading\nBody phrase.\n")],
        true,
    );

    let error = search_json_rows(CliSearchScope::All, "AND", Some(&config_path))
        .expect_err("invalid expression should fail");
    match error {
        CliError::Search(SearchError::InvalidExpression { .. }) => {}
        other => panic!("unexpected error: {other}"),
    }
    assert_eq!(error.to_string(), "invalid SQLite FTS5 search expression");
}

#[test]
fn search_is_read_only_and_does_not_scan_org_files() {
    let (test_dir, config_path, db_path) = build_search_fixture(
        "search-read-only",
        &[("notes.org", "* Searchable Heading\nBody phrase.\n")],
        true,
    );

    let writable = open_database(&db_path).expect("database should open");
    let version_before: u32 = writable
        .pragma_query_value(None, "user_version", |row| row.get(0))
        .expect("user_version should load");
    let metadata_before: Vec<(String, String)> = {
        let mut stmt = writable
            .prepare("SELECT key, value FROM db_metadata ORDER BY key")
            .expect("metadata query should prepare");
        stmt.query_map([], |row| Ok((row.get(0)?, row.get(1)?)))
            .expect("metadata query should run")
            .collect::<Result<Vec<_>, _>>()
            .expect("metadata rows should collect")
    };
    let heading_fts_rows_before: i64 = writable
        .query_row("SELECT COUNT(*) FROM heading_fts", [], |row| row.get(0))
        .expect("fts row count should load");
    drop(writable);

    fs::remove_file(test_dir.path().join("notes.org")).expect("source org file should delete");

    let rows = search_json_rows(CliSearchScope::All, "Searchable", Some(&config_path))
        .expect("search should succeed without source file");
    assert_eq!(rows.len(), 1);

    let reopened = Connection::open(&db_path).expect("database should reopen");
    let version_after: u32 = reopened
        .pragma_query_value(None, "user_version", |row| row.get(0))
        .expect("user_version should reload");
    let metadata_after: Vec<(String, String)> = {
        let mut stmt = reopened
            .prepare("SELECT key, value FROM db_metadata ORDER BY key")
            .expect("metadata query should prepare");
        stmt.query_map([], |row| Ok((row.get(0)?, row.get(1)?)))
            .expect("metadata query should run")
            .collect::<Result<Vec<_>, _>>()
            .expect("metadata rows should collect")
    };
    let heading_fts_rows_after: i64 = reopened
        .query_row("SELECT COUNT(*) FROM heading_fts", [], |row| row.get(0))
        .expect("fts row count should reload");

    assert_eq!(version_after, version_before);
    assert_eq!(metadata_after, metadata_before);
    assert_eq!(heading_fts_rows_after, heading_fts_rows_before);
}

#[test]
fn search_orders_equal_rank_results_deterministically_by_heading_id() {
    let (_test_dir, config_path, _) = build_search_fixture(
        "search-deterministic-order",
        &[
            ("a.org", "* Shared\nsqlite\n"),
            ("b.org", "* Shared\nsqlite\n"),
        ],
        true,
    );

    let rows = search_json_rows(CliSearchScope::All, "Shared", Some(&config_path))
        .expect("search should succeed");
    assert_eq!(rows.len(), 2);
    assert!(rows[0].rank <= rows[1].rank);
    if (rows[0].rank - rows[1].rank).abs() < f64::EPSILON {
        assert!(rows[0].heading.id < rows[1].heading.id);
    }
}
