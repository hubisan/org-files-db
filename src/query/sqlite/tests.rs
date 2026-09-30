use super::{
    compile_sqlite_query, compile_sqlite_query_with_metadata_strategy, execute_sqlite_query,
    execute_sqlite_query_with_options, execute_sqlite_query_with_relation_and_metadata_strategy,
    expand_leading_home_path_with_home, heading_matched_relation_cost, params_from_iter,
    sqlite_query_validation_options, FileQueryRow, HeadingQueryMatch, HeadingQueryRow,
    LinkQueryRow, MatchedRelationCost, MetadataPredicateSqlStrategy, QueryExecutionErrorKind,
    QueryParam, QueryRows,
};
use crate::db::{
    open_database, open_in_memory_database_with_schema, DbWriter, EffectivePropertyRecord,
    EffectiveTagRecord, FileRecordInput, HeadingBodyRecord, HeadingRecord, KeywordRecord,
    LinkRecord, OutlinePathRecord, PropertyRecord, SchemaDefinition, TagRecord, TimestampRecord,
};
use crate::property::{derive_effective_properties, PropertyRow};
use crate::query::{
    parse_query, resolve_relative_dates, resolve_temporal_bounds, validate_query,
    QueryDateResolutionOptions, QueryExecutionOptions, QueryTarget, QueryValidationOptions,
};
use crate::tag::derive_effective_tags;
use chrono::NaiveDate;
use rusqlite::{limits::Limit, Connection};
use std::{
    fs,
    path::{Path, PathBuf},
    time::{SystemTime, UNIX_EPOCH},
};

struct PlanningFixture<'a> {
    kind: &'a str,
    timestamp: Option<i64>,
    has_time: Option<bool>,
    raw_value: &'a str,
}

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
            "org-files-db-query-sqlite-{}-{}-{}",
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

fn validation_options() -> QueryValidationOptions {
    QueryValidationOptions {
        body_text_available: true,
        regexp_matching_supported: true,
    }
}

fn validated(query: &str) -> crate::query::ValidatedQuery {
    let parsed = parse_query(query).expect("query should parse");
    validate_query(parsed, &validation_options()).expect("query should validate")
}

fn temporal_resolved(query: &str) -> crate::query::ValidatedQuery {
    resolve_temporal_bounds(
        &validated(query),
        &QueryDateResolutionOptions {
            timezone: Some("UTC".to_string()),
            now_utc: None,
        },
    )
    .expect("temporal bounds should resolve")
}

#[test]
fn matched_relation_cost_requires_measured_predicate_driven_metadata_shape() {
    assert_eq!(
        heading_matched_relation_cost(&validated(r#"(headings (level 1))"#)),
        MatchedRelationCost::Cheap
    );
    assert_eq!(
        heading_matched_relation_cost(&validated(
            r#"(headings (and (level 1) (or (title "Task") (tags "project"))))"#
        )),
        MatchedRelationCost::Cheap
    );
    assert_eq!(
        heading_matched_relation_cost(&validated(
            r#"(headings (and (level 1) (tags "project") (property "GROUP" "group0")))"#
        )),
        MatchedRelationCost::Expensive
    );
    assert_eq!(
        heading_matched_relation_cost(&validated(
            r#"(headings (ancestors (headings (title "Task"))))"#
        )),
        MatchedRelationCost::Cheap
    );
}

#[test]
fn validation_options_read_persisted_body_text_capability() {
    let connection = open_in_memory_database_with_schema(&SchemaDefinition::new(
        crate::db::CURRENT_SCHEMA_VERSION,
        false,
    ))
    .expect("database should open");

    let unavailable =
        sqlite_query_validation_options(&connection).expect("validation options should load");
    assert!(!unavailable.body_text_available);
    assert!(unavailable.regexp_matching_supported);

    DbWriter::set_metadata_flag(
        &connection,
        crate::db::DB_METADATA_BODY_TEXT_AVAILABLE_KEY,
        true,
    )
    .expect("metadata should persist");

    let available =
        sqlite_query_validation_options(&connection).expect("validation options should reload");
    assert!(available.body_text_available);
    assert!(available.regexp_matching_supported);
}

#[test]
fn validation_options_treat_missing_metadata_table_as_body_text_unavailable() {
    let connection = Connection::open_in_memory().expect("database should open");

    let options =
        sqlite_query_validation_options(&connection).expect("validation options should load");
    assert!(!options.body_text_available);
    assert!(options.regexp_matching_supported);
}

#[test]
fn validation_options_treat_missing_heading_bodies_table_as_body_text_unavailable() {
    let connection = reduced_body_text_capability_connection(true, true);

    let options =
        sqlite_query_validation_options(&connection).expect("validation options should load");
    assert!(!options.body_text_available);
    assert!(options.regexp_matching_supported);
}

#[test]
fn validation_options_treat_missing_body_text_metadata_row_as_unavailable() {
    let connection = reduced_body_text_capability_connection(false, true);

    let options =
        sqlite_query_validation_options(&connection).expect("validation options should load");
    assert!(!options.body_text_available);
    assert!(options.regexp_matching_supported);
}

#[test]
fn validation_options_treat_disabled_body_text_metadata_value_as_unavailable() {
    let connection = reduced_body_text_capability_connection(true, false);

    let options =
        sqlite_query_validation_options(&connection).expect("validation options should load");
    assert!(!options.body_text_available);
    assert!(options.regexp_matching_supported);
}

#[test]
fn execution_rejects_has_text_when_body_text_capability_is_unavailable() {
    let connection = open_in_memory_database_with_schema(&SchemaDefinition::new(
        crate::db::CURRENT_SCHEMA_VERSION,
        false,
    ))
    .expect("database should open");
    let parsed = parse_query(r#"(headings (has-text "sqlite"))"#).expect("query should parse");
    let query = validate_query(
        parsed,
        &QueryValidationOptions {
            body_text_available: true,
            regexp_matching_supported: true,
        },
    )
    .expect("query should validate with permissive options");

    let error = execute_sqlite_query(&connection, &query).expect_err("query should fail");
    assert_eq!(
        error.kind,
        QueryExecutionErrorKind::UnsupportedBackendFeature
    );
    assert_eq!(
        error.message,
        "has-text requires body text to be available in the database"
    );
}

#[test]
fn execution_rejects_has_text_when_metadata_claims_capability_but_heading_bodies_is_missing() {
    let connection = reduced_body_text_capability_connection(true, true);
    let parsed = parse_query(r#"(headings (has-text "sqlite"))"#).expect("query should parse");
    let query = validate_query(
        parsed,
        &QueryValidationOptions {
            body_text_available: true,
            regexp_matching_supported: true,
        },
    )
    .expect("query should validate with permissive options");

    let error = execute_sqlite_query(&connection, &query).expect_err("query should fail");
    assert_eq!(
        error.kind,
        QueryExecutionErrorKind::UnsupportedBackendFeature
    );
    assert_eq!(
        error.message,
        "has-text requires body text to be available in the database"
    );
    assert!(!error.to_string().contains("no such table: heading_bodies"));
}

#[test]
fn compile_uses_placeholders_instead_of_inlining_user_payload() {
    let user_value = "x' OR 1=1 --";
    for query in [
        validated(&format!(r#"(headings (title "{user_value}"))"#)),
        validated(&format!(r#"(headings (has-text "{user_value}"))"#)),
        validated(&format!(r#"(headings (title "{user_value}" :regexp t))"#)),
        validated(&format!(
            r#"(headings (has-text "{user_value}" :regexp t))"#
        )),
        validated(&format!(r#"(headings (outline-contains "{user_value}"))"#)),
        validated(&format!(
            r#"(headings (outline-contains "{user_value}" :regexp t))"#
        )),
        validated(&format!(
            r#"(headings (outline-sequence "{user_value}" "nested"))"#
        )),
        validated(&format!(
            r#"(headings (outline-sequence "{user_value}" "nested" :regexp t))"#
        )),
        validated(&format!(r#"(headings (tags "{user_value}" :inherit nil))"#)),
        validated(&format!(
            r#"(headings (tags "{user_value}" :inherit nil :regexp t))"#
        )),
        validated(&format!(r#"(headings (property "OWNER" "{user_value}"))"#)),
        validated(&format!(
            r#"(headings (property "OWNER" "{user_value}" :regexp t))"#
        )),
        validated(&format!(r#"(files (keyword "AUTHOR" "{user_value}"))"#)),
        validated(&format!(
            r#"(files (keyword "AUTHOR" "{user_value}" :regexp t))"#
        )),
        validated(&format!(r#"(files (file-path "{user_value}" :regexp t))"#)),
        validated(&format!(
            r#"(links (link-target "{user_value}" :regexp t))"#
        )),
        validated(&format!(
            r#"(headings (links-to (headings (title "{user_value}"))))"#
        )),
    ] {
        let compiled = compile_sqlite_query(&query).expect("query should compile");
        assert!(compiled.sql.contains('?'));
        assert!(!compiled.sql.contains(user_value));
        assert!(!compiled.params.is_empty());
        assert!(compiled.params.iter().any(|param| matches!(
            param,
            super::QueryParam::Text(value) if value == user_value
        )));
    }
}

#[test]
fn heading_root_branch_is_skipped_when_predicates_cannot_match_level_zero() {
    let level_one = validated(r#"(headings (level 1))"#);
    assert_eq!(
        super::heading_root_truth(level_one.predicate.as_ref()),
        super::StaticTruth::False
    );

    let negated_level_one = validated(r#"(headings (not (level 1)))"#);
    assert_eq!(
        super::heading_root_truth(negated_level_one.predicate.as_ref()),
        super::StaticTruth::True
    );

    let unknown_title = validated(r#"(headings (title "Project" :exact t))"#);
    assert_eq!(
        super::heading_root_truth(unknown_title.predicate.as_ref()),
        super::StaticTruth::Unknown
    );

    let impossible_and = validated(r#"(headings (and (title "Project" :exact t) (level 2 4)))"#);
    assert_eq!(
        super::heading_root_truth(impossible_and.predicate.as_ref()),
        super::StaticTruth::False
    );
}

#[test]
fn compile_omits_unused_heading_root_joins() {
    let simple = compile_sqlite_query(&validated(r#"(headings (level 1))"#))
        .expect("simple heading query should compile");
    assert!(simple.sql.contains("INNER JOIN files AS f0"));
    assert!(!simple.sql.contains("INNER JOIN headings AS r0"));

    let with_file_title =
        compile_sqlite_query(&validated(r#"(headings (file-title "Project" :exact t))"#))
            .expect("file-title query should compile");
    assert!(with_file_title.sql.contains("INNER JOIN headings AS r0"));
}

#[test]
fn compile_top_level_queries_leave_public_order_to_rust() {
    for expression in [
        r#"(headings (level 1))"#,
        r#"(links (link-type "file"))"#,
        "(files)",
    ] {
        let compiled =
            compile_sqlite_query(&validated(expression)).expect("top-level query should compile");
        assert!(!compiled.sql.contains("ORDER BY"), "{}", compiled.sql);
    }
}

#[test]
fn compile_top_level_links_omit_unused_outline_path_join() {
    let compiled = compile_sqlite_query(&validated(r#"(links (link-type "file"))"#))
        .expect("link query should compile");

    assert!(compiled.sql.contains("INNER JOIN files AS f0"));
    assert!(compiled.sql.contains("INNER JOIN headings AS lh0"));
    assert!(!compiled.sql.contains("INNER JOIN outline_path AS op0"));
}

#[test]
fn compile_nested_link_filters_use_only_the_links_table_when_possible() {
    let compiled = compile_sqlite_query(&validated(
        r#"(headings (has-link (links (link-type "file"))))"#,
    ))
    .expect("nested link query should compile");

    assert!(compiled.sql.contains("EXISTS (SELECT 1 FROM links AS l1"));
    assert!(!compiled.sql.contains("INNER JOIN files AS f1"));
    assert!(!compiled.sql.contains("INNER JOIN headings AS lh1"));
    assert!(!compiled.sql.contains("INNER JOIN outline_path AS op1"));
}

#[test]
fn compile_nested_heading_and_file_targets_add_only_required_joins() {
    let heading_title = compile_sqlite_query(&validated(
        r#"(headings (links-to (headings (title "Target" :exact t))))"#,
    ))
    .expect("nested heading title query should compile");
    assert!(heading_title.sql.contains("FROM headings AS h2"));
    assert!(!heading_title.sql.contains("INNER JOIN files AS f2"));
    assert!(!heading_title.sql.contains("INNER JOIN headings AS r2"));

    let heading_file_title = compile_sqlite_query(&validated(
        r#"(headings (links-to (headings (file-title "Project" :exact t))))"#,
    ))
    .expect("nested heading file-title query should compile");
    assert!(heading_file_title.sql.contains("INNER JOIN headings AS r2"));

    let file_path = compile_sqlite_query(&validated(
        r#"(headings (links-to (files (file-path "/tmp/target.org" :exact t))))"#,
    ))
    .expect("nested file path query should compile");
    assert!(file_path.sql.contains("FROM files AS f2"));
    assert!(!file_path.sql.contains("INNER JOIN headings AS r2"));

    let file_title = compile_sqlite_query(&validated(
        r#"(headings (links-to (files (file-title "Project" :exact t))))"#,
    ))
    .expect("nested file title query should compile");
    assert!(file_title.sql.contains("INNER JOIN headings AS r2"));
}

#[test]
fn expands_only_leading_file_path_home_syntax() {
    assert_eq!(
        expand_leading_home_path_with_home("~", Some("/home/tester")),
        Ok("/home/tester".to_string())
    );
    assert_eq!(
        expand_leading_home_path_with_home("~/notes.org", Some("/home/tester")),
        Ok("/home/tester/notes.org".to_string())
    );
    assert_eq!(
        expand_leading_home_path_with_home("projects/~/notes.org", Some("/home/tester")),
        Ok("projects/~/notes.org".to_string())
    );
    assert_eq!(
        expand_leading_home_path_with_home("file~backup.org", Some("/home/tester")),
        Ok("file~backup.org".to_string())
    );
    assert_eq!(
        expand_leading_home_path_with_home("~alice/notes.org", Some("/home/tester")),
        Ok("~alice/notes.org".to_string())
    );
    assert!(expand_leading_home_path_with_home("~/notes.org", None)
        .expect_err("missing HOME should fail")
        .contains("HOME"));
    assert_eq!(
        expand_leading_home_path_with_home("notes.org", None),
        Ok("notes.org".to_string())
    );
}

#[test]
fn compiles_expanded_file_paths_for_all_file_path_contexts() {
    let home = std::env::var("HOME").expect("test environment should provide HOME");
    let expected = format!("{home}/projects/ancestors.org");

    for query in [
        r#"(headings (file-path "~/projects/ancestors.org" :exact t))"#,
        r#"(files (file-path "~/projects/ancestors.org" :exact t))"#,
        r#"(headings (links-to (files (file-path "~/projects/ancestors.org" :exact t))))"#,
        r#"(links (target (files (file-path "~/projects/ancestors.org" :exact t))))"#,
    ] {
        let compiled = compile_sqlite_query(&validated(query)).expect("query should compile");
        assert_eq!(compiled.params, vec![QueryParam::Text(expected.clone())]);
    }
}

#[test]
fn compiles_expanded_file_dirs_for_all_file_dir_contexts() {
    let home = std::env::var("HOME").expect("test environment should provide HOME");
    let expected = format!("{home}/projects");

    for query in [
        r#"(headings (file-dir "~/projects" :exact t))"#,
        r#"(files (file-dir "~/projects" :exact t))"#,
        r#"(headings (links-to (files (file-dir "~/projects" :exact t))))"#,
        r#"(links (target (files (file-dir "~/projects" :exact t))))"#,
    ] {
        let compiled = compile_sqlite_query(&validated(query)).expect("query should compile");
        assert_eq!(compiled.params, vec![QueryParam::Text(expected.clone())]);
    }

    for (query, expected) in [
        (
            r#"(files (file-dir "/var/projects" :exact t))"#,
            "/var/projects",
        ),
        (r#"(files (file-dir "projects" :exact t))"#, "projects"),
    ] {
        let compiled = compile_sqlite_query(&validated(query)).expect("query should compile");
        assert_eq!(
            compiled.params,
            vec![QueryParam::Text(expected.to_string())]
        );
    }
}

#[test]
fn execution_expands_file_path_home_without_changing_returned_paths() {
    let connection = open_in_memory_database_with_schema(&SchemaDefinition::new(
        crate::db::CURRENT_SCHEMA_VERSION,
        false,
    ))
    .expect("database should open");
    let home = std::env::var("HOME").expect("test environment should provide HOME");
    let path = format!("{home}/projects/ancestors.org");
    connection
        .execute(
            "INSERT INTO files (id, path, mtime_ns, size) VALUES (1, ?1, 0, 0)",
            rusqlite::params![path],
        )
        .expect("file should insert");
    connection
        .execute_batch(
            "INSERT INTO headings
             (id, file_id, parent_id, level, byte_start, byte_end, title)
             VALUES
             (1, 1, NULL, 0, -1, 0, 'Index'),
             (2, 1, 1, 1, 0, 0, 'Child');",
        )
        .expect("headings should insert");

    let file_rows = execute_sqlite_query(
        &connection,
        &validated(r#"(files (file-path "~/projects/ancestors.org" :exact t))"#),
    )
    .expect("exact file query should execute");
    assert_eq!(file_paths(file_rows), vec![path.clone()]);

    let heading_rows = execute_sqlite_query(
        &connection,
        &validated(r#"(headings (file-path "~/projects" "ancestors"))"#),
    )
    .expect("substring heading query should execute");
    assert_eq!(heading_ids(heading_rows), vec![2]);

    let regexp_rows = execute_sqlite_query(
        &connection,
        &validated(r#"(files (file-path "~/projects/.*\\.org" :regexp t))"#),
    )
    .expect("regexp file query should execute");
    assert_eq!(file_paths(regexp_rows), vec![path]);
}

#[test]
fn execution_expands_file_dir_home_without_changing_returned_paths() {
    let connection = open_in_memory_database_with_schema(&SchemaDefinition::new(
        crate::db::CURRENT_SCHEMA_VERSION,
        false,
    ))
    .expect("database should open");
    let home = std::env::var("HOME").expect("test environment should provide HOME");
    let path = format!("{home}/projects/ancestors.org");
    connection
        .execute(
            "INSERT INTO files (id, path, mtime_ns, size) VALUES (1, ?1, 0, 0)",
            rusqlite::params![path],
        )
        .expect("file should insert");
    connection
        .execute_batch(
            "INSERT INTO headings
             (id, file_id, parent_id, level, byte_start, byte_end, title)
             VALUES
             (1, 1, NULL, 0, -1, 0, 'Index'),
             (2, 1, 1, 1, 0, 0, 'Child');",
        )
        .expect("headings should insert");

    let file_rows = execute_sqlite_query(
        &connection,
        &validated(r#"(files (file-dir "~/projects" :exact t))"#),
    )
    .expect("exact file directory query should execute");
    assert_eq!(file_paths(file_rows), vec![path.clone()]);

    let heading_rows =
        execute_sqlite_query(&connection, &validated(r#"(headings (file-dir "~/proj"))"#))
            .expect("substring heading directory query should execute");
    assert_eq!(heading_ids(heading_rows), vec![2]);

    let regexp_rows = execute_sqlite_query(
        &connection,
        &validated(r#"(files (file-dir "~/proj.*" :regexp t))"#),
    )
    .expect("regexp file directory query should execute");
    assert_eq!(file_paths(regexp_rows), vec![path]);
}

#[test]
fn execution_file_modified_uses_configured_timezone_for_calendar_boundaries() {
    let connection = open_in_memory_database_with_schema(&SchemaDefinition::new(
        crate::db::CURRENT_SCHEMA_VERSION,
        false,
    ))
    .expect("database should open");
    connection
        .execute(
            "INSERT INTO files (id, path, mtime_ns, size) VALUES (1, '/tmp/late.org', ?1, 0)",
            rusqlite::params![1_784_327_696_000_000_000_i64],
        )
        .expect("file should insert");
    connection
        .execute(
            "INSERT INTO headings (id, file_id, parent_id, level, byte_start, byte_end, title) VALUES (1, 1, NULL, 0, -1, 0, 'Late')",
            [],
        )
        .expect("root heading should insert");
    let options = QueryExecutionOptions {
        query_timezone: Some("Europe/Zurich".to_string()),
        ..QueryExecutionOptions::default()
    };

    for (query, expected) in [
        (r#"(files (file-modified :on "2026-07-17"))"#, Vec::new()),
        (
            r#"(files (file-modified :on "2026-07-18"))"#,
            vec!["/tmp/late.org"],
        ),
        (
            r#"(files (file-modified :from "2026-07-18"))"#,
            vec!["/tmp/late.org"],
        ),
        (r#"(files (file-modified :to "2026-07-17"))"#, Vec::new()),
    ] {
        let rows = execute_sqlite_query_with_options(&connection, &validated(query), &options)
            .expect("file modification query should execute");
        assert_eq!(
            file_paths(rows),
            expected.into_iter().map(str::to_string).collect::<Vec<_>>()
        );
    }
}

#[test]
fn compile_supports_regex_predicates_and_rejects_invalid_patterns() {
    for query in [
        validated(r#"(headings (tags "proj-.*" :regexp t))"#),
        validated(r#"(headings (property "OWNER" "A.*" :regexp t))"#),
        validated(r#"(files (keyword "AUTHOR" "A.*" :regexp t))"#),
        validated(r#"(links (link-target "notes.*" :regexp t))"#),
        validated(r#"(headings (title "Query.*" :regexp t))"#),
        validated(r#"(files (file-path ".*/query-alpha\\.org" :regexp t))"#),
        validated(r#"(headings (outline-contains "Query.*" :regexp t))"#),
        validated(r#"(headings (outline-sequence "Query.*" "Nested.*" :regexp t))"#),
    ] {
        compile_sqlite_query(&query).expect("regexp query should compile");
    }

    for (query, predicate) in [
        (r#"(headings (has-text "(" :regexp t))"#, "has-text"),
        (r#"(headings (title "(" :regexp t))"#, "title"),
        (r#"(headings (tags "(" :regexp t))"#, "tags"),
        (r#"(headings (property "OWNER" "(" :regexp t))"#, "property"),
        (r#"(files (keyword "AUTHOR" "(" :regexp t))"#, "keyword"),
        (
            r#"(headings (outline-contains "(" :regexp t))"#,
            "outline-contains",
        ),
        (
            r#"(headings (outline-sequence "(" "Nested.*" :regexp t))"#,
            "outline-sequence",
        ),
        (r#"(links (link-target "(" :regexp t))"#, "link-target"),
    ] {
        let error = compile_sqlite_query(&validated(query)).expect_err("regexp should fail");
        assert_eq!(
            error.kind,
            QueryExecutionErrorKind::UnsupportedBackendFeature
        );
        assert!(error
            .message
            .contains(&format!("invalid regular expression for {predicate}")));
    }
}

#[test]
fn compile_rejects_unresolved_relative_date_values() {
    for query in [
        validated(r#"(headings (scheduled :on today))"#),
        validated(r#"(files (file-modified :from -7))"#),
    ] {
        let error = compile_sqlite_query(&query).expect_err("unresolved relative date should fail");
        assert_eq!(error.kind, QueryExecutionErrorKind::DateResolution);
        assert!(error
            .to_string()
            .contains("resolve relative dates before SQL compilation"));
    }
}

#[test]
fn execution_file_restriction_filters_structural_targets_and_combines_with_query() {
    let connection = seeded_connection();
    let alpha = "/tmp/query-alpha.org".to_string();
    let beta = "/tmp/query-beta.org".to_string();

    let heading_rows = execute_sqlite_query_with_options(
        &connection,
        &validated("(headings)"),
        &QueryExecutionOptions {
            restricted_file_paths: Some(vec![alpha.clone()]),
            ..QueryExecutionOptions::default()
        },
    )
    .expect("restricted heading query should execute");
    let QueryRows::Headings(heading_rows) = heading_rows else {
        panic!("expected heading rows");
    };
    assert!(!heading_rows.is_empty());
    assert!(heading_rows.iter().all(|row| match row {
        HeadingQueryMatch::File(row) => row.path == alpha,
        HeadingQueryMatch::Heading(row) => row.file_path == alpha,
    }));

    let link_rows = execute_sqlite_query_with_options(
        &connection,
        &validated("(links)"),
        &QueryExecutionOptions {
            restricted_file_paths: Some(vec![alpha.clone()]),
            ..QueryExecutionOptions::default()
        },
    )
    .expect("restricted link query should execute");
    let QueryRows::Links(link_rows) = link_rows else {
        panic!("expected link rows");
    };
    assert!(!link_rows.is_empty());
    assert!(link_rows.iter().all(|row| row.file_path == alpha));

    let file_rows = execute_sqlite_query_with_options(
        &connection,
        &validated("(files)"),
        &QueryExecutionOptions {
            restricted_file_paths: Some(vec![beta.clone()]),
            ..QueryExecutionOptions::default()
        },
    )
    .expect("restricted file query should execute");
    assert_eq!(file_paths(file_rows), vec![beta]);

    let mismatch = execute_sqlite_query_with_options(
        &connection,
        &validated(r#"(files (file-title "Beta Index" :exact t))"#),
        &QueryExecutionOptions {
            restricted_file_paths: Some(vec![alpha]),
            ..QueryExecutionOptions::default()
        },
    )
    .expect("restriction should combine with the user query");
    assert!(file_paths(mismatch).is_empty());

    for query in ["(headings)", "(links)", "(files)"] {
        let rows = execute_sqlite_query_with_options(
            &connection,
            &validated(query),
            &QueryExecutionOptions {
                restricted_file_paths: Some(Vec::new()),
                ..QueryExecutionOptions::default()
            },
        )
        .expect("an empty restriction should execute");
        let empty = match rows {
            QueryRows::Headings(rows) => rows.is_empty(),
            QueryRows::Links(rows) => rows.is_empty(),
            QueryRows::Files(rows) => rows.is_empty(),
        };
        assert!(empty, "empty restriction returned rows for {query}");
    }
}

#[test]
fn execution_file_restriction_preserves_unicode_and_space_paths() {
    let schema = SchemaDefinition::new(3, false);
    let mut connection =
        open_in_memory_database_with_schema(&schema).expect("database should open");
    let selected = Path::new("/tmp/über space.org");
    seed_database(&mut connection, selected, Path::new("/tmp/other.org"));

    let rows = execute_sqlite_query_with_options(
        &connection,
        &validated("(files)"),
        &QueryExecutionOptions {
            restricted_file_paths: Some(vec![selected.display().to_string()]),
            ..QueryExecutionOptions::default()
        },
    )
    .expect("Unicode restriction should execute");
    assert_eq!(file_paths(rows), vec![selected.display().to_string()]);
}

#[test]
fn execution_file_restriction_respects_small_runtime_variable_limit() {
    let connection = seeded_connection();
    let restricted = vec![
        "/tmp/query-alpha.org".to_string(),
        "/tmp/query-beta.org".to_string(),
        "/tmp/query-gamma.org".to_string(),
    ];
    let previous = connection
        .set_limit(Limit::SQLITE_LIMIT_VARIABLE_NUMBER, 2)
        .expect("runtime variable limit should change");

    let rows = execute_sqlite_query_with_options(
        &connection,
        &validated("(files)"),
        &QueryExecutionOptions {
            restricted_file_paths: Some(restricted.clone()),
            ..QueryExecutionOptions::default()
        },
    )
    .expect("restricted query should batch inserts below the variable limit");
    connection
        .set_limit(Limit::SQLITE_LIMIT_VARIABLE_NUMBER, previous)
        .expect("runtime variable limit should restore");

    assert_eq!(file_paths(rows), restricted);
}

#[test]
fn execution_matches_metadata_queries_and_boolean_composition() {
    let connection = seeded_connection();

    let headings_query = validated(
        r#"(headings
            (and
              (todo "NEXT")
              (priority "A")
              (title "Engine")
              (file-path "alpha.org")
              (not (file-title "Beta"))))"#,
    );
    let heading_rows =
        execute_sqlite_query(&connection, &headings_query).expect("heading query should execute");
    assert_eq!(
        heading_rows,
        QueryRows::Headings(vec![HeadingQueryMatch::Heading(HeadingQueryRow {
            id: 11,
            file_id: 2,
            file_path: "/tmp/query-alpha.org".to_string(),
            parent_id: Some(10),
            level: 1,
            line_number: Some(3),
            byte_start: 10,
            byte_end: 40,
            title: "Query Engine".to_string(),
            title_raw: Some("Query Engine".to_string()),
            todo_keyword: Some("NEXT".to_string()),
            todo_type: Some("open".to_string()),
            priority: Some("A".to_string()),
            scheduled_raw: Some("<2026-01-03 Fri>".to_string()),
            scheduled_ts: Some(1_767_398_400),
            deadline_raw: None,
            deadline_ts: None,
            closed_raw: None,
            closed_ts: None,
            archivedp: false,
            footnote_section_p: false,
            all_tags_json: "[\"filetag\",\"project\"]".to_string(),
        })])
    );

    let links_query = validated(
        r#"(links
            (and
              (link-type "file")
              (link-target "beta.org")
              (has-description)
              (status "resolved")
              (source (headings (title "Engine")))
              (target (files (file-title "Beta Index" :exact t)))))"#,
    );
    let link_rows =
        execute_sqlite_query(&connection, &links_query).expect("link query should execute");
    assert_eq!(
        link_rows,
        QueryRows::Links(vec![LinkQueryRow {
            id: 100,
            file_id: 2,
            file_path: "/tmp/query-alpha.org".to_string(),
            heading_id: 11,
            heading_level: 1,
            source_context: "normal".to_string(),
            format: "bracket".to_string(),
            link_type: "file".to_string(),
            raw: "[[file:beta.org][Beta notes]]".to_string(),
            raw_target: "file:beta.org".to_string(),
            raw_description: Some("Beta notes".to_string()),
            path: "beta.org".to_string(),
            search_option: None,
            path_absolute: Some("/tmp/query-beta.org".to_string()),
            target_file_id: Some(1),
            target_heading_id: Some(20),
            target_custom_id: None,
            target_id: None,
            resolution_status: Some("resolved".to_string()),
            resolution_diagnostic: None,
            byte_start: 50,
            byte_end: 80,
            line: 4,
        }])
    );

    let files_query = validated(
        r#"(files
            (or
              (file-title "Beta Index" :exact t)
              (and
                (keyword "AUTHOR" "Alice")
                (property "CATEGORY" "work")
                (tags "filetag"))))"#,
    );
    let file_rows =
        execute_sqlite_query(&connection, &files_query).expect("file query should execute");
    assert_eq!(
        file_rows,
        QueryRows::Files(vec![
            FileQueryRow {
                id: 2,
                path: "/tmp/query-alpha.org".to_string(),
                mtime_ns: 1_767_398_400_000_000_000,
                size: 100,
                content_hash: None,
                indexed_at: Some(1_767_398_410),
                root_heading_id: 10,
                root_title: "Alpha Index".to_string(),
                root_title_raw: Some("Alpha Index".to_string()),
                root_line_number: None,
            },
            FileQueryRow {
                id: 1,
                path: "/tmp/query-beta.org".to_string(),
                mtime_ns: 1_767_484_800_000_000_000,
                size: 120,
                content_hash: None,
                indexed_at: Some(1_767_484_810),
                root_heading_id: 20,
                root_title: "Beta Index".to_string(),
                root_title_raw: Some("Beta Index".to_string()),
                root_line_number: None,
            },
        ])
    );
}

#[test]
fn execution_compares_priorities_by_semantic_rank_and_preserves_source_values() {
    let connection = seeded_connection();
    for (id, priority) in [(101, "1"), (102, "2"), (103, "C"), (104, "3"), (105, "10")] {
        connection
            .execute(
                "INSERT INTO headings
                 (id, file_id, parent_id, level, byte_start, byte_end, title, priority)
                 VALUES (?1, 2, 10, 1, ?2, ?3, ?4, ?5)",
                rusqlite::params![
                    id,
                    id * 10,
                    id * 10 + 5,
                    format!("Priority {priority}"),
                    priority
                ],
            )
            .expect("priority heading should insert");
    }

    let matching_priorities = |query: &str| {
        let QueryRows::Headings(rows) = execute_sqlite_query(&connection, &validated(query))
            .expect("priority query should execute")
        else {
            panic!("headings query should return heading rows");
        };
        rows.into_iter()
            .filter_map(|row| match row {
                HeadingQueryMatch::Heading(row) => row.priority,
                HeadingQueryMatch::File(_) => None,
            })
            .collect::<Vec<_>>()
    };

    assert_eq!(
        matching_priorities(r#"(headings (priority "A"))"#),
        ["A", "1"]
    );
    assert_eq!(
        matching_priorities(r#"(headings (priority "1"))"#),
        ["A", "1"]
    );
    assert_eq!(
        matching_priorities(r#"(headings (priority "B"))"#),
        ["B", "2"]
    );
    assert_eq!(
        matching_priorities(r#"(headings (priority > "B"))"#),
        ["A", "1"]
    );
    assert_eq!(
        matching_priorities(r#"(headings (priority >= "B"))"#),
        ["A", "B", "1", "2"]
    );
    assert_eq!(
        matching_priorities(r#"(headings (priority < "B"))"#),
        ["C", "3", "10"]
    );
    assert_eq!(
        matching_priorities(r#"(headings (priority <= "B"))"#),
        ["B", "2", "C", "3", "10"]
    );
    assert_eq!(
        matching_priorities(r#"(headings (priority > "2"))"#),
        ["A", "1"]
    );
    assert_eq!(
        matching_priorities(r#"(headings (priority >= "2"))"#),
        ["A", "B", "1", "2"]
    );
    assert_eq!(
        matching_priorities(r#"(headings (priority < "2"))"#),
        ["C", "3", "10"]
    );
    assert_eq!(
        matching_priorities(r#"(headings (priority <= "2"))"#),
        ["B", "2", "C", "3", "10"]
    );
}

#[test]
fn execution_case_insensitive_predicates_fold_unicode() {
    let connection = seeded_connection();
    connection
        .execute(
            "UPDATE headings SET title = 'Über Straße' WHERE id = 12",
            [],
        )
        .expect("title update");
    connection
        .execute(
            r#"UPDATE outline_path SET breadcrumbs_json = '["Über Straße"]' WHERE heading_id = 12"#,
            [],
        )
        .expect("outline update");
    connection
        .execute(
            "UPDATE keywords SET keyword = 'Ärger' WHERE heading_id = 10",
            [],
        )
        .expect("keyword update");

    for query in [
        r#"(headings (title "über"))"#,
        r#"(headings (title "ÜBER"))"#,
        r#"(headings (title "straße"))"#,
    ] {
        let rows = execute_sqlite_query(&connection, &validated(query))
            .expect("title query should execute");
        assert_eq!(heading_ids(rows), vec![12], "{query}");
    }

    let rows = execute_sqlite_query(
        &connection,
        &validated(r#"(files (keyword "ärger" "Alice"))"#),
    )
    .expect("keyword query should execute");
    assert_eq!(file_paths(rows), vec!["/tmp/query-alpha.org".to_string()]);

    let rows = execute_sqlite_query(
        &connection,
        &validated(r#"(headings (outline-contains "über"))"#),
    )
    .expect("outline query should execute");
    let ids = heading_ids(rows);
    assert!(ids.contains(&12), "{ids:?}");
}

#[test]
fn execution_matches_hierarchy_predicates() {
    let connection = seeded_connection();

    let parent_rows = execute_sqlite_query(&connection, &validated(r#"(headings (parent))"#))
        .expect("parent query should execute");
    assert_eq!(heading_ids(parent_rows), vec![11, 12, 13, 14, 15, 21, 31]);

    let parent_nested_rows = execute_sqlite_query(
        &connection,
        &validated(r#"(headings (parent (headings (title "Query Engine" :exact t))))"#),
    )
    .expect("nested parent query should execute");
    assert_eq!(heading_ids(parent_nested_rows), vec![12, 14, 15]);

    let ancestor_rows = execute_sqlite_query(
        &connection,
        &validated(r#"(headings (ancestors (headings (title "Query Engine" :exact t))))"#),
    )
    .expect("ancestor query should execute");
    assert_eq!(heading_ids(ancestor_rows), vec![12, 14, 15]);

    let children_rows = execute_sqlite_query(&connection, &validated(r#"(headings (children))"#))
        .expect("children query should execute");
    assert_eq!(heading_ids(children_rows), vec![11]);

    let child_nested_rows = execute_sqlite_query(
        &connection,
        &validated(r#"(headings (children (headings (title "Nested Task" :exact t))))"#),
    )
    .expect("child nested query should execute");
    assert_eq!(heading_ids(child_nested_rows), vec![11]);

    let descendant_rows = execute_sqlite_query(
        &connection,
        &validated(r#"(headings (descendants (headings (title "Nested Task" :exact t))))"#),
    )
    .expect("descendant query should execute");
    assert_eq!(heading_ids(descendant_rows), vec![11]);

    let root_parent_rows = execute_sqlite_query(
        &connection,
        &validated(r#"(headings (parent (headings (level 0))))"#),
    )
    .expect("root parent query should execute");
    assert_eq!(heading_ids(root_parent_rows), vec![11, 13, 21, 31]);

    let root_parent_boolean_rows = execute_sqlite_query(
        &connection,
        &validated(
            r#"(headings
                (parent
                  (headings
                    (and
                      (level 0)
                      (title "Alpha Index" :exact t)))))"#,
        ),
    )
    .expect("boolean root parent query should execute");
    assert_eq!(heading_ids(root_parent_boolean_rows), vec![11, 13]);

    let root_ancestor_rows = execute_sqlite_query(
        &connection,
        &validated(r#"(headings (ancestors (headings (level 0))))"#),
    )
    .expect("root ancestor query should execute");
    assert_eq!(
        heading_ids(root_ancestor_rows),
        vec![11, 12, 13, 14, 15, 21, 31]
    );

    let root_tag_ancestor_rows = execute_sqlite_query(
        &connection,
        &validated(r#"(headings (ancestors (headings (tags "filetag" :inherit nil))))"#),
    )
    .expect("root tag ancestor query should execute");
    assert_eq!(
        heading_ids(root_tag_ancestor_rows),
        vec![11, 12, 13, 14, 15]
    );

    let root_children_rows = execute_sqlite_query(
        &connection,
        &validated(r#"(headings (children (headings (title "Query Engine" :exact t))))"#),
    )
    .expect("root children query should execute");
    assert_eq!(
        heading_file_paths(root_children_rows),
        vec!["/tmp/query-alpha.org"]
    );

    let root_descendant_rows = execute_sqlite_query(
        &connection,
        &validated(r#"(headings (descendants (headings (title "Nested Task" :exact t))))"#),
    )
    .expect("root descendant query should execute");
    assert_eq!(
        heading_file_paths(root_descendant_rows),
        vec!["/tmp/query-alpha.org"]
    );

    let root_parent_outer_rows = execute_sqlite_query(
        &connection,
        &validated(
            r#"(headings
                (and
                  (level 0)
                  (parent (headings (title "Query Engine" :exact t)))))"#,
        ),
    )
    .expect("root parent outer query should execute");
    assert!(heading_file_paths(root_parent_outer_rows).is_empty());

    let root_ancestor_outer_rows = execute_sqlite_query(
        &connection,
        &validated(
            r#"(headings
                (and
                  (level 0)
                  (ancestors (headings (title "Query Engine" :exact t)))))"#,
        ),
    )
    .expect("root ancestor outer query should execute");
    assert!(heading_file_paths(root_ancestor_outer_rows).is_empty());

    let self_descendant_rows = execute_sqlite_query(
        &connection,
        &validated(
            r#"(headings
                (and
                  (title "Nested Task" :exact t)
                  (descendants (headings (title "Nested Task" :exact t)))))"#,
        ),
    )
    .expect("self descendant query should execute");
    assert!(heading_ids(self_descendant_rows).is_empty());
}

#[test]
fn nested_hierarchy_heading_scopes_do_not_filter_out_synthetic_roots() {
    let compiled = compile_sqlite_query(&validated(r#"(headings (parent (headings (level 0))))"#))
        .expect("nested hierarchy query should compile");

    assert!(compiled.sql.contains("h0.level > 0"));
    assert!(!compiled.sql.contains("h1.level > 0"));
    assert!(compiled.sql.contains("h1.level = ?"));
}

#[test]
fn nested_relation_heading_scopes_do_not_filter_out_synthetic_roots() {
    let compiled = compile_sqlite_query(&validated(r#"(links (source (headings (level 0))))"#))
        .expect("nested relation query should compile");

    assert!(!compiled.sql.contains("h1.level > 0"));
    assert!(compiled.sql.contains("h1.level = ?"));
}

#[test]
fn compile_supports_documented_outline_and_hierarchy_heading_predicates() {
    for query in [
        r#"(headings (outline-contains "Query"))"#,
        r#"(headings (outline-sequence "Query" "Nested"))"#,
        r#"(headings (parent))"#,
        r#"(headings (children))"#,
        r#"(headings (ancestors))"#,
        r#"(headings (descendants))"#,
    ] {
        compile_sqlite_query(&validated(query)).expect("query should compile");
    }
}

#[test]
fn execution_matches_outline_predicates() {
    let connection = seeded_connection();

    let contains_rows = execute_sqlite_query(
        &connection,
        &validated(r#"(headings (outline-contains "Query" "Nested"))"#),
    )
    .expect("outline contains query should execute");
    assert_eq!(heading_ids(contains_rows), vec![12]);

    let contains_order_insensitive_rows = execute_sqlite_query(
        &connection,
        &validated(r#"(headings (outline-contains "Nested" "Query"))"#),
    )
    .expect("outline contains query should execute");
    assert_eq!(heading_ids(contains_order_insensitive_rows), vec![12]);

    let sequence_top_level_rows = execute_sqlite_query(
        &connection,
        &validated(
            r#"(headings
                (and
                  (outline-sequence "Query Engine" :exact t)
                  (level 1)))"#,
        ),
    )
    .expect("outline sequence top-level query should execute");
    assert_eq!(heading_ids(sequence_top_level_rows), vec![11]);

    let contains_root_name_rows = execute_sqlite_query(
        &connection,
        &validated(r#"(headings (outline-contains "Alpha Index"))"#),
    )
    .expect("outline contains should execute");
    assert_eq!(
        heading_ids(contains_root_name_rows),
        vec![11, 12, 13, 14, 15]
    );

    let contains_root_and_child_rows = execute_sqlite_query(
        &connection,
        &validated(r#"(headings (outline-contains "Alpha Index" "Nested Task" :regexp nil))"#),
    )
    .expect("outline contains should match root and child components");
    assert_eq!(heading_ids(contains_root_and_child_rows), vec![12]);

    let root_contains_rows = execute_sqlite_query(
        &connection,
        &validated(r#"(headings (and (level 0) (outline-contains "Alpha Index")))"#),
    )
    .expect("root outline contains should execute");
    assert_eq!(
        heading_file_paths(root_contains_rows),
        vec!["/tmp/query-alpha.org"]
    );

    let sequence_exact_rows = execute_sqlite_query(
        &connection,
        &validated(r#"(headings (outline-sequence "Query Engine" "Nested Task" :exact t))"#),
    )
    .expect("outline sequence exact query should execute");
    assert_eq!(heading_ids(sequence_exact_rows), vec![12]);

    let sequence_substring_rows = execute_sqlite_query(
        &connection,
        &validated(r#"(headings (outline-sequence "Query" "Nested"))"#),
    )
    .expect("outline sequence query should execute");
    assert_eq!(heading_ids(sequence_substring_rows), vec![12]);

    let sequence_root_and_parent_rows = execute_sqlite_query(
        &connection,
        &validated(r#"(headings (outline-sequence "Alpha Index" "Query Engine" :exact t))"#),
    )
    .expect("root-leading outline sequence should execute");
    assert_eq!(
        heading_ids(sequence_root_and_parent_rows),
        vec![11, 12, 14, 15]
    );

    let sequence_root_parent_child_rows = execute_sqlite_query(
        &connection,
        &validated(
            r#"(headings (outline-sequence "Alpha Index" "Query Engine" "Nested Task" :exact t))"#,
        ),
    )
    .expect("root-leading child outline sequence should execute");
    assert_eq!(heading_ids(sequence_root_parent_child_rows), vec![12]);

    let sequence_root_only_rows = execute_sqlite_query(
        &connection,
        &validated(r#"(headings (outline-sequence "Alpha Index" :exact t))"#),
    )
    .expect("one-component root sequence should execute");
    assert_eq!(
        heading_ids(sequence_root_only_rows),
        vec![11, 12, 13, 14, 15]
    );

    let root_sequence_rows = execute_sqlite_query(
        &connection,
        &validated(r#"(headings (and (level 0) (outline-sequence "Alpha Index" :exact t)))"#),
    )
    .expect("root outline sequence should execute");
    assert_eq!(
        heading_file_paths(root_sequence_rows),
        vec!["/tmp/query-alpha.org"]
    );

    let sequence_non_contiguous_rows = execute_sqlite_query(
        &connection,
        &validated(r#"(headings (outline-sequence "Query Engine" "Statistic Cookies" :exact t))"#),
    )
    .expect("outline sequence query should execute");
    assert_eq!(heading_ids(sequence_non_contiguous_rows), vec![14]);

    let sequence_missing_rows = execute_sqlite_query(
        &connection,
        &validated(r#"(headings (outline-sequence "Query Engine" "Loose Note" :exact t))"#),
    )
    .expect("outline sequence should execute");
    assert_eq!(heading_ids(sequence_missing_rows), Vec::<i64>::new());

    let regexp_contains_rows = execute_sqlite_query(
        &connection,
        &validated(r#"(headings (outline-contains "Query.*" "Nested.*" :regexp t))"#),
    )
    .expect("outline regexp contains query should execute");
    assert_eq!(heading_ids(regexp_contains_rows), vec![12]);

    let regexp_sequence_rows = execute_sqlite_query(
        &connection,
        &validated(r#"(headings (outline-sequence "Query.*" "Nested.*" :regexp t))"#),
    )
    .expect("outline regexp sequence query should execute");
    assert_eq!(heading_ids(regexp_sequence_rows), vec![12]);

    let regexp_root_rows = execute_sqlite_query(
        &connection,
        &validated(r#"(headings (outline-contains "Alpha.*" :regexp t))"#),
    )
    .expect("outline regexp root query should execute");
    assert_eq!(heading_ids(regexp_root_rows), vec![11, 12, 13, 14, 15]);
}

#[test]
fn compiled_tag_predicates_use_normalized_direct_and_effective_tables() {
    let inherited = compile_sqlite_query(&validated(r#"(headings (tags "project"))"#))
        .expect("inherited tag query should compile");
    assert!(inherited.sql.contains("FROM effective_tags"));
    assert!(!inherited.sql.contains("WITH RECURSIVE lineage"));

    let local = compile_sqlite_query(&validated(r#"(headings (tags "project" :inherit nil))"#))
        .expect("local tag query should compile");
    assert!(local.sql.contains("FROM tags"));
    assert!(!local.sql.contains("FROM effective_tags"));
    assert!(!local.sql.contains("WITH RECURSIVE lineage"));

    let connection = seeded_connection();
    let explain_sql = format!("EXPLAIN QUERY PLAN {}", inherited.sql);
    let mut statement = connection
        .prepare(&explain_sql)
        .expect("inherited tag plan should prepare");
    let plan = statement
        .query_map(params_from_iter(inherited.params.iter()), |row| {
            row.get::<_, String>(3)
        })
        .expect("inherited tag plan should query")
        .collect::<Result<Vec<_>, _>>()
        .expect("inherited tag plan should decode");
    assert!(
        plan.iter()
            .any(|detail| detail.contains("idx_effective_tags_tag_heading")),
        "expected effective tag index in plan: {plan:?}"
    );
}

#[test]
fn compiled_property_predicates_use_materialized_effective_properties() {
    for query in [
        r#"(headings (property "OWNER" "Alice"))"#,
        r#"(headings (property "OWNER" "Alice" :inherit t))"#,
        r#"(headings (property "OWNER" "Alice" :inherit nil))"#,
        r#"(headings (property "OWNER" "A.*" :regexp t))"#,
    ] {
        let compiled = compile_sqlite_query(&validated(query)).expect("query should compile");
        assert!(compiled.sql.contains("FROM effective_properties"));
        assert!(!compiled.sql.contains("lineage_up"));
        assert!(!compiled.sql.contains("local_summary"));
    }
}

#[test]
fn production_predicate_driven_metadata_strategy_preserves_boolean_query_rows() {
    let connection = seeded_connection();
    for query in [
        r#"(headings (and (level 1) (tags "urgent" :inherit nil)))"#,
        r#"(headings (and (level 1) (property "OWNER" "Bob" :inherit nil)))"#,
        r#"(headings (and (level 1) (property "CATEGORY" "work" :inherit t)))"#,
        r#"(headings (and (level 1) (keyword "AUTHOR" "Alice" :inherit nil)))"#,
        r#"(headings (and (level 1) (not (property "OWNER" "Bob" :inherit nil))))"#,
        r#"(headings (and (level 1) (or (tags "urgent" :inherit nil) (property "OWNER" "Bob" :inherit nil))))"#,
    ] {
        let validated = validated(query);
        let legacy = execute_sqlite_query_with_relation_and_metadata_strategy(
            &connection,
            &validated,
            &QueryExecutionOptions::default(),
            MetadataPredicateSqlStrategy::HeadingDrivenExists,
        )
        .expect("heading-driven metadata query should execute")
        .rows;
        let production = execute_sqlite_query(&connection, &validated)
            .expect("production metadata query should execute");
        assert_eq!(legacy, production, "query changed result: {query}");
    }
}

#[test]
fn production_metadata_strategy_compiles_indexable_equality_subqueries() {
    let tag = compile_sqlite_query(&validated(
        r#"(headings (and (level 1) (tags "urgent" :inherit nil)))"#,
    ))
    .expect("production tag query should compile");
    assert!(tag
        .sql
        .contains("IN (SELECT tags.heading_id FROM tags WHERE tags.tag IN"));

    let property = compile_sqlite_query(&validated(
        r#"(headings (and (level 1) (property "OWNER" "Bob" :inherit nil)))"#,
    ))
    .expect("production property query should compile");
    assert!(property.sql.contains(
        "IN (SELECT heading_id FROM effective_properties WHERE key = ? AND local_value IS NOT NULL AND local_value = ?"
    ));

    let keyword = compile_sqlite_query(&validated(
        r#"(headings (and (level 1) (keyword "AUTHOR" "Alice" :inherit nil)))"#,
    ))
    .expect("production keyword query should compile");
    assert!(keyword.sql.contains(
        "IN (SELECT keywords.heading_id FROM keywords WHERE orgfdb_lower(keywords.keyword) = orgfdb_lower(?) AND keywords.value = ?"
    ));

    let regexp = compile_sqlite_query(&validated(
        r#"(headings (property "OWNER" "B.*" :regexp t :inherit nil))"#,
    ))
    .expect("regexp property query should compile");
    assert!(regexp
        .sql
        .contains("EXISTS (SELECT 1 FROM effective_properties"));
}

#[test]
fn predicate_driven_metadata_strategy_compiles_indexable_subqueries() {
    let tag = compile_sqlite_query_with_metadata_strategy(
        &validated(r#"(headings (and (level 1) (tags "urgent" :inherit nil)))"#),
        false,
        MetadataPredicateSqlStrategy::PredicateDrivenIn,
    )
    .expect("predicate-driven tag query should compile");
    assert!(tag
        .sql
        .contains("IN (SELECT tags.heading_id FROM tags WHERE tags.tag IN"));

    let property = compile_sqlite_query_with_metadata_strategy(
        &validated(r#"(headings (and (level 1) (property "OWNER" "Bob" :inherit nil)))"#),
        false,
        MetadataPredicateSqlStrategy::PredicateDrivenIn,
    )
    .expect("predicate-driven property query should compile");
    assert!(property.sql.contains(
        "IN (SELECT heading_id FROM effective_properties WHERE key = ? AND local_value IS NOT NULL AND local_value = ?"
    ));

    let keyword = compile_sqlite_query_with_metadata_strategy(
        &validated(r#"(headings (and (level 1) (keyword "AUTHOR" "Alice" :inherit nil)))"#),
        false,
        MetadataPredicateSqlStrategy::PredicateDrivenIn,
    )
    .expect("predicate-driven keyword query should compile");
    assert!(keyword.sql.contains(
        "IN (SELECT keywords.heading_id FROM keywords WHERE orgfdb_lower(keywords.keyword) = orgfdb_lower(?) AND keywords.value = ?"
    ));
}

#[test]
fn execution_matches_outline_predicates_in_boolean_and_hierarchy_queries() {
    let connection = seeded_connection();

    let boolean_rows = execute_sqlite_query(
        &connection,
        &validated(
            r#"(headings
                (and
                  (outline-sequence "Query Engine" "Statistic Cookies" :exact t)
                  (property "ADD-VALUE" "is valid" :inherit t)))"#,
        ),
    )
    .expect("boolean outline query should execute");
    assert_eq!(heading_ids(boolean_rows), vec![14]);

    let descendant_rows = execute_sqlite_query(
        &connection,
        &validated(
            r#"(headings
                (descendants
                  (headings
                    (outline-sequence "Query Engine" "Nested Task" :exact t))))"#,
        ),
    )
    .expect("hierarchy outline query should execute");
    assert_eq!(heading_ids(descendant_rows), vec![11]);
}

#[test]
fn execution_matches_heading_and_file_link_relation_queries() {
    let connection = seeded_connection();

    let has_link_rows = execute_sqlite_query(&connection, &validated(r#"(headings (has-link))"#))
        .expect("has-link query should execute");
    assert_eq!(heading_ids(has_link_rows), vec![11, 12, 13, 21]);

    let has_file_link_rows = execute_sqlite_query(
        &connection,
        &validated(r#"(headings (has-link (links (link-type "file"))))"#),
    )
    .expect("has-link file query should execute");
    assert_eq!(heading_ids(has_file_link_rows), vec![11, 12, 13, 21]);

    let links_to_file_rows = execute_sqlite_query(
        &connection,
        &validated(r#"(headings (links-to (files (file-title "Beta Index" :exact t))))"#),
    )
    .expect("links-to file query should execute");
    assert_eq!(heading_ids(links_to_file_rows), vec![11, 12]);

    let links_to_heading_rows = execute_sqlite_query(
        &connection,
        &validated(r#"(headings (links-to (headings (title "Beta Target" :exact t))))"#),
    )
    .expect("links-to heading query should execute");
    assert_eq!(heading_ids(links_to_heading_rows), vec![12]);

    let links_to_root_rows = execute_sqlite_query(
        &connection,
        &validated(r#"(headings (and (level 0) (links-to (headings (level 0)))))"#),
    )
    .expect("links-to root heading query should execute");
    let QueryRows::Headings(links_to_root_rows) = links_to_root_rows else {
        panic!("expected heading matches");
    };
    assert!(matches!(
        links_to_root_rows.as_slice(),
        [HeadingQueryMatch::File(alpha), HeadingQueryMatch::File(beta)]
            if alpha.path == "/tmp/query-alpha.org" && beta.path == "/tmp/query-beta.org"
    ));

    let file_links_to_root_rows = execute_sqlite_query(
        &connection,
        &validated(r#"(files (links-to (headings (level 0))))"#),
    )
    .expect("file links-to root heading query should execute");
    assert_eq!(
        file_paths(file_links_to_root_rows),
        vec![
            "/tmp/query-alpha.org".to_string(),
            "/tmp/query-beta.org".to_string()
        ]
    );

    let linked_from_any_rows =
        execute_sqlite_query(&connection, &validated(r#"(headings (linked-from :any))"#))
            .expect("linked-from any query should execute");
    assert_eq!(heading_ids(linked_from_any_rows), vec![11, 21]);

    let linked_from_heading_rows = execute_sqlite_query(
        &connection,
        &validated(r#"(headings (linked-from (headings (title "Nested Task" :exact t))))"#),
    )
    .expect("linked-from heading query should execute");
    assert_eq!(heading_ids(linked_from_heading_rows), vec![21]);

    let linked_from_root_rows = execute_sqlite_query(
        &connection,
        &validated(r#"(headings (and (level 0) (linked-from (headings (level 0)))))"#),
    )
    .expect("linked-from root heading query should execute");
    let QueryRows::Headings(linked_from_root_rows) = linked_from_root_rows else {
        panic!("expected heading matches");
    };
    assert!(matches!(
        linked_from_root_rows.as_slice(),
        [HeadingQueryMatch::File(alpha), HeadingQueryMatch::File(beta)]
            if alpha.path == "/tmp/query-alpha.org" && beta.path == "/tmp/query-beta.org"
    ));

    let file_linked_from_root_rows = execute_sqlite_query(
        &connection,
        &validated(r#"(files (linked-from (headings (level 0))))"#),
    )
    .expect("file linked-from root heading query should execute");
    assert_eq!(
        file_paths(file_linked_from_root_rows),
        vec![
            "/tmp/query-alpha.org".to_string(),
            "/tmp/query-beta.org".to_string()
        ]
    );

    let linked_from_file_rows = execute_sqlite_query(
        &connection,
        &validated(r#"(headings (linked-from (files (file-title "Beta Index" :exact t))))"#),
    )
    .expect("linked-from file query should execute");
    assert_eq!(heading_ids(linked_from_file_rows), vec![11]);

    let file_has_link_rows = execute_sqlite_query(&connection, &validated(r#"(files (has-link))"#))
        .expect("file has-link query should execute");
    assert_eq!(
        file_paths(file_has_link_rows),
        vec![
            "/tmp/query-alpha.org".to_string(),
            "/tmp/query-beta.org".to_string()
        ]
    );

    let file_links_to_heading_rows = execute_sqlite_query(
        &connection,
        &validated(r#"(files (links-to (headings (title "Beta Target" :exact t))))"#),
    )
    .expect("file links-to heading query should execute");
    assert_eq!(
        file_paths(file_links_to_heading_rows),
        vec!["/tmp/query-alpha.org".to_string()]
    );

    let file_linked_from_any_rows =
        execute_sqlite_query(&connection, &validated(r#"(files (linked-from :any))"#))
            .expect("file linked-from any query should execute");
    assert_eq!(
        file_paths(file_linked_from_any_rows),
        vec![
            "/tmp/query-alpha.org".to_string(),
            "/tmp/query-beta.org".to_string()
        ]
    );

    let file_linked_from_heading_rows = execute_sqlite_query(
        &connection,
        &validated(r#"(files (linked-from (headings (title "Beta Target" :exact t))))"#),
    )
    .expect("file linked-from heading query should execute");
    assert_eq!(
        file_paths(file_linked_from_heading_rows),
        vec!["/tmp/query-alpha.org".to_string()]
    );
}

#[test]
fn execution_matches_link_source_target_and_status_queries() {
    let connection = seeded_connection();

    let source_heading_rows = execute_sqlite_query(
        &connection,
        &validated(r#"(links (source (headings (title "Nested Task" :exact t))))"#),
    )
    .expect("source heading query should execute");
    assert_eq!(link_ids(source_heading_rows), vec![101]);

    let source_root_rows = execute_sqlite_query(
        &connection,
        &validated(r#"(links (source (headings (level 0))))"#),
    )
    .expect("source root heading query should execute");
    assert_eq!(link_ids(source_root_rows), vec![102, 104]);

    let source_root_file_name_rows = execute_sqlite_query(
        &connection,
        &validated(
            r#"(links (source (headings (and (level 0) (file-name "query-alpha.org" :exact t)))))"#,
        ),
    )
    .expect("source root file-name query should execute");
    assert_eq!(link_ids(source_root_file_name_rows), vec![102]);

    let source_file_rows = execute_sqlite_query(
        &connection,
        &validated(r#"(links (source (files (file-title "Beta Index" :exact t))))"#),
    )
    .expect("source file query should execute");
    assert_eq!(link_ids(source_file_rows), vec![104, 103, 106]);

    let target_heading_rows = execute_sqlite_query(
        &connection,
        &validated(r#"(links (target (headings (title "Beta Target" :exact t))))"#),
    )
    .expect("target heading query should execute");
    assert_eq!(link_ids(target_heading_rows), vec![101]);

    let target_root_rows = execute_sqlite_query(
        &connection,
        &validated(r#"(links (target (headings (level 0))))"#),
    )
    .expect("target root heading query should execute");
    assert_eq!(link_ids(target_root_rows), vec![102, 100, 104]);

    let target_file_rows = execute_sqlite_query(
        &connection,
        &validated(r#"(links (target (files (file-title "Beta Index" :exact t))))"#),
    )
    .expect("target file query should execute");
    assert_eq!(link_ids(target_file_rows), vec![102, 100, 101]);

    let target_any_rows = execute_sqlite_query(&connection, &validated(r#"(links (target :any))"#))
        .expect("target any query should execute");
    assert_eq!(link_ids(target_any_rows), vec![102, 100, 101, 104, 103]);

    let broken_rows = execute_sqlite_query(
        &connection,
        &validated(r#"(links (and (status "broken") (link-target "file:missing.org" :exact t)))"#),
    )
    .expect("broken status query should execute");
    assert_eq!(link_ids(broken_rows), vec![105]);

    let unresolved_rows =
        execute_sqlite_query(&connection, &validated(r#"(links (status "unresolved"))"#))
            .expect("unresolved status query should execute");
    assert_eq!(link_ids(unresolved_rows), vec![106]);

    let ambiguous_rows =
        execute_sqlite_query(&connection, &validated(r#"(links (status "ambiguous"))"#))
            .expect("ambiguous status query should execute");
    assert_eq!(link_ids(ambiguous_rows), vec![107]);

    let ambiguous_target_rows = execute_sqlite_query(
        &connection,
        &validated(r#"(links (and (status "ambiguous") (target :any)))"#),
    )
    .expect("ambiguous target-any query should execute");
    assert_eq!(link_ids(ambiguous_target_rows), Vec::<i64>::new());

    let ambiguous_target_file_rows = execute_sqlite_query(
        &connection,
        &validated(
            r#"(links
                (and
                  (status "ambiguous")
                  (target (files (file-title "Gamma Index" :exact t)))))"#,
        ),
    )
    .expect("ambiguous target-file query should execute");
    assert_eq!(link_ids(ambiguous_target_file_rows), Vec::<i64>::new());

    let ambiguous_target_heading_rows = execute_sqlite_query(
        &connection,
        &validated(
            r#"(links
                (and
                  (status "ambiguous")
                  (target (headings (title "Gamma Candidate" :exact t)))))"#,
        ),
    )
    .expect("ambiguous target-heading query should execute");
    assert_eq!(link_ids(ambiguous_target_heading_rows), Vec::<i64>::new());

    let ambiguous_links_to_file_rows = execute_sqlite_query(
        &connection,
        &validated(r#"(headings (links-to (files (file-title "Gamma Index" :exact t))))"#),
    )
    .expect("ambiguous links-to file query should execute");
    assert_eq!(heading_ids(ambiguous_links_to_file_rows), Vec::<i64>::new());

    let ambiguous_links_to_heading_rows = execute_sqlite_query(
        &connection,
        &validated(r#"(headings (links-to (headings (title "Gamma Candidate" :exact t))))"#),
    )
    .expect("ambiguous links-to heading query should execute");
    assert_eq!(
        heading_ids(ambiguous_links_to_heading_rows),
        Vec::<i64>::new()
    );

    let ambiguous_file_links_to_heading_rows = execute_sqlite_query(
        &connection,
        &validated(r#"(files (links-to (headings (title "Gamma Candidate" :exact t))))"#),
    )
    .expect("ambiguous file links-to heading query should execute");
    assert_eq!(
        file_paths(ambiguous_file_links_to_heading_rows),
        Vec::<String>::new()
    );

    let ambiguous_file_links_to_file_rows = execute_sqlite_query(
        &connection,
        &validated(r#"(files (links-to (files (file-title "Gamma Index" :exact t))))"#),
    )
    .expect("ambiguous file links-to file query should execute");
    assert_eq!(
        file_paths(ambiguous_file_links_to_file_rows),
        Vec::<String>::new()
    );

    let gamma_heading_backlink_rows = execute_sqlite_query(
        &connection,
        &validated(r#"(headings (linked-from (files (file-title "Gamma Index" :exact t))))"#),
    )
    .expect("gamma heading backlink query should execute");
    assert_eq!(heading_ids(gamma_heading_backlink_rows), Vec::<i64>::new());

    let gamma_file_backlink_rows = execute_sqlite_query(
        &connection,
        &validated(r#"(files (linked-from (headings (title "Gamma Candidate" :exact t))))"#),
    )
    .expect("gamma file backlink query should execute");
    assert_eq!(file_paths(gamma_file_backlink_rows), Vec::<String>::new());
}

#[test]
fn execution_matches_effective_heading_tag_queries() {
    let connection = seeded_connection();

    let local_rows = execute_sqlite_query(
        &connection,
        &validated(
            r#"(headings (and (title "Nested Task" :exact t) (tags "urgent" :inherit nil)))"#,
        ),
    )
    .expect("local tag query should execute");
    assert_eq!(heading_ids(local_rows), vec![12]);

    let inherited_rows = execute_sqlite_query(
        &connection,
        &validated(r#"(headings (and (title "Nested Task" :exact t) (tags "project")))"#),
    )
    .expect("inherited tag query should execute");
    assert_eq!(heading_ids(inherited_rows), vec![12]);

    let explicit_inherited_rows = execute_sqlite_query(
        &connection,
        &validated(
            r#"(headings (and (title "Nested Task" :exact t) (tags "project" :inherit t)))"#,
        ),
    )
    .expect("explicit inherited tag query should execute");
    assert_eq!(heading_ids(explicit_inherited_rows), vec![12]);

    let alias_rows = execute_sqlite_query(
        &connection,
        &validated(
            r#"(headings (and (title "Nested Task" :exact t) (tags-all "project" "urgent")))"#,
        ),
    )
    .expect("tags-all alias query should execute");
    assert_eq!(heading_ids(alias_rows), vec![12]);

    let root_rows = execute_sqlite_query(
        &connection,
        &validated(r#"(headings (and (title "Nested Task" :exact t) (tags "filetag")))"#),
    )
    .expect("root tag query should execute");
    assert_eq!(heading_ids(root_rows), vec![12]);

    let match_all_rows = execute_sqlite_query(
        &connection,
        &validated(
            r#"(headings (and (title "Nested Task" :exact t) (tags "project" "urgent" :match :all)))"#,
        ),
    )
    .expect("match-all tag query should execute");
    assert_eq!(heading_ids(match_all_rows), vec![12]);

    let correlated_rows =
        execute_sqlite_query(&connection, &validated(r#"(headings (tags "project"))"#))
            .expect("correlated tag query should execute");
    assert_eq!(heading_ids(correlated_rows), vec![11, 12, 14, 15]);
    assert_eq!(
        heading_file_paths(
            execute_sqlite_query(&connection, &validated(r#"(headings (tags "filetag"))"#))
                .expect("root filetag heading query should execute")
        ),
        vec!["/tmp/query-alpha.org".to_string()]
    );

    let file_tag_rows =
        execute_sqlite_query(&connection, &validated(r#"(files (tags "filetag"))"#))
            .expect("file tag query should execute");
    assert_eq!(
        file_paths(file_tag_rows),
        vec!["/tmp/query-alpha.org".to_string()]
    );

    let file_heading_tag_rows =
        execute_sqlite_query(&connection, &validated(r#"(files (tags "project"))"#))
            .expect("file direct tag query should execute");
    assert_eq!(file_paths(file_heading_tag_rows), Vec::<String>::new());

    let regexp_local_rows = execute_sqlite_query(
        &connection,
        &validated(
            r#"(headings (and (title "Nested Task" :exact t) (tags "urg.*" :inherit nil :regexp t)))"#,
        ),
    )
    .expect("regexp local tag query should execute");
    assert_eq!(heading_ids(regexp_local_rows), vec![12]);

    let regexp_inherited_rows = execute_sqlite_query(
        &connection,
        &validated(r#"(headings (and (title "Nested Task" :exact t) (tags "proj.*" :regexp t)))"#),
    )
    .expect("regexp inherited tag query should execute");
    assert_eq!(heading_ids(regexp_inherited_rows), vec![12]);

    let regexp_root_rows = execute_sqlite_query(
        &connection,
        &validated(r#"(headings (and (title "Nested Task" :exact t) (tags "file.*" :regexp t)))"#),
    )
    .expect("regexp root tag query should execute");
    assert_eq!(heading_ids(regexp_root_rows), vec![12]);

    let regexp_match_all_rows = execute_sqlite_query(
        &connection,
        &validated(
            r#"(headings (and (title "Nested Task" :exact t) (tags "proj.*" "urg.*" :regexp t :match :all)))"#,
        ),
    )
    .expect("regexp match-all tag query should execute");
    assert_eq!(heading_ids(regexp_match_all_rows), vec![12]);
}

#[test]
fn execution_matches_effective_heading_property_queries() {
    let connection = seeded_connection();
    let before_rows = property_rows(&connection, 11, "LANG");

    let local_rows = execute_sqlite_query(
        &connection,
        &validated(
            r#"(headings (and (title "Nested Task" :exact t) (property "OWNER" "Bob" :inherit nil)))"#,
        ),
    )
    .expect("local property query should execute");
    assert_eq!(heading_ids(local_rows), vec![12]);

    let inherited_rows = execute_sqlite_query(
        &connection,
        &validated(r#"(headings (and (title "Nested Task" :exact t) (property "AREA" "infra")))"#),
    )
    .expect("inherited property query should execute");
    assert_eq!(heading_ids(inherited_rows), vec![12]);

    let root_rows = execute_sqlite_query(
        &connection,
        &validated(
            r#"(headings (and (title "Nested Task" :exact t) (property "CATEGORY" "work")))"#,
        ),
    )
    .expect("root property query should execute");
    assert_eq!(heading_ids(root_rows), vec![12]);

    let file_rows = execute_sqlite_query(
        &connection,
        &validated(r#"(files (property "CATEGORY" "work"))"#),
    )
    .expect("file property query should execute");
    assert_eq!(
        file_paths(file_rows),
        vec!["/tmp/query-alpha.org".to_string()]
    );

    let appended_rows = execute_sqlite_query(
        &connection,
        &validated(
            r#"(headings (and (title "Query Engine" :exact t) (property "LANG" "rust emacs" :inherit nil)))"#,
        ),
    )
    .expect("append property query should execute");
    assert_eq!(heading_ids(appended_rows), vec![11]);

    let non_resolved_component_rows = execute_sqlite_query(
        &connection,
        &validated(
            r#"(headings (and (title "Query Engine" :exact t) (property "LANG" "emacs" :inherit nil)))"#,
        ),
    )
    .expect("append property query should execute");
    assert_eq!(heading_ids(non_resolved_component_rows), Vec::<i64>::new());

    let append_before_rows = execute_sqlite_query(
        &connection,
        &validated(
            r#"(headings (and (title "Query Engine" :exact t) (property "APPEND_BEFORE" "definition appending before" :inherit nil)))"#,
        ),
    )
    .expect("append-before property query should execute");
    assert_eq!(heading_ids(append_before_rows), vec![11]);

    let append_before_inherited_rows = execute_sqlite_query(
        &connection,
        &validated(
            r#"(headings (and (title "Query Engine" :exact t) (property "APPEND_BEFORE" "definition appending before")))"#,
        ),
    )
    .expect("append-before inherited property query should execute");
    assert_eq!(heading_ids(append_before_inherited_rows), vec![11]);

    let append_before_base_only_rows = execute_sqlite_query(
        &connection,
        &validated(
            r#"(headings (and (title "Query Engine" :exact t) (property "APPEND_BEFORE" "definition" :inherit nil)))"#,
        ),
    )
    .expect("append-before base-only query should execute");
    assert_eq!(heading_ids(append_before_base_only_rows), Vec::<i64>::new());

    let append_between_duplicate_rows = execute_sqlite_query(
        &connection,
        &validated(
            r#"(headings (and (title "Loose Note" :exact t) (property "APPEND_REPLACED" "first appended" :inherit nil)))"#,
        ),
    )
    .expect("append-between-duplicates property query should execute");
    assert_eq!(heading_ids(append_between_duplicate_rows), vec![13]);

    let stale_base_rows = execute_sqlite_query(
        &connection,
        &validated(
            r#"(headings (and (title "Loose Note" :exact t) (property "APPEND_REPLACED" "second appended" :inherit nil)))"#,
        ),
    )
    .expect("stale base query should execute");
    assert_eq!(heading_ids(stale_base_rows), Vec::<i64>::new());

    let overwrite_rows = execute_sqlite_query(
        &connection,
        &validated(
            r#"(headings (and (title "Loose Note" :exact t) (property "DEFINED_TWICE" "works" :inherit nil)))"#,
        ),
    )
    .expect("overwrite property query should execute");
    assert_eq!(heading_ids(overwrite_rows), vec![13]);

    let overwritten_value_rows = execute_sqlite_query(
        &connection,
        &validated(
            r#"(headings (and (title "Loose Note" :exact t) (property "DEFINED_TWICE" "second is effective" :inherit nil)))"#,
        ),
    )
    .expect("overwritten value query should execute");
    assert_eq!(heading_ids(overwritten_value_rows), Vec::<i64>::new());

    let add_value_rows = execute_sqlite_query(
        &connection,
        &validated(
            r#"(headings (and (title "Statistic Cookies" :exact t) (property "ADD-VALUE" "is valid" :inherit nil)))"#,
        ),
    )
    .expect("add-value property query should execute");
    assert_eq!(heading_ids(add_value_rows), vec![14]);

    let add_value_fragment_rows = execute_sqlite_query(
        &connection,
        &validated(
            r#"(headings (and (title "Statistic Cookies" :exact t) (property "ADD-VALUE" "valid" :inherit nil)))"#,
        ),
    )
    .expect("add-value fragment property query should execute");
    assert_eq!(heading_ids(add_value_fragment_rows), Vec::<i64>::new());

    let multiple_append_positions_rows = execute_sqlite_query(
        &connection,
        &validated(
            r#"(headings (and (title "Statistic Cookies" :exact t) (property "MULTI_APPEND" "first before middle after" :inherit nil)))"#,
        ),
    )
    .expect("multiple-append-positions property query should execute");
    assert_eq!(heading_ids(multiple_append_positions_rows), vec![14]);

    let partial_multiple_append_rows = execute_sqlite_query(
        &connection,
        &validated(
            r#"(headings (and (title "Statistic Cookies" :exact t) (property "MULTI_APPEND" "first after" :inherit nil)))"#,
        ),
    )
    .expect("partial multiple-append query should execute");
    assert_eq!(heading_ids(partial_multiple_append_rows), Vec::<i64>::new());

    let root_append_rows = execute_sqlite_query(
        &connection,
        &validated(r#"(headings (property "KEYWORD_APPEND" "foo=1 bar=2" :inherit nil))"#),
    )
    .expect("root append property query should execute");
    assert_eq!(
        heading_file_paths(root_append_rows),
        vec!["/tmp/query-alpha.org".to_string()]
    );

    let root_overwrite_rows = execute_sqlite_query(
        &connection,
        &validated(r#"(headings (property "KEYWORD_OVERWRITTEN_BY_SECOND" "valid" :inherit nil))"#),
    )
    .expect("root overwrite property query should execute");
    assert_eq!(
        heading_file_paths(root_overwrite_rows),
        vec!["/tmp/query-alpha.org".to_string()]
    );

    let append_inherited_rows = execute_sqlite_query(
        &connection,
        &validated(
            r#"(headings (and (title "Nested Task" :exact t) (property "APPEND_INHERITED" "parent child")))"#,
        ),
    )
    .expect("append inherited property query should execute");
    assert_eq!(heading_ids(append_inherited_rows), vec![12]);

    let local_base_with_appends_direct_rows = execute_sqlite_query(
        &connection,
        &validated(
            r#"(headings (and (title "Nested Task" :exact t) (property "LOCAL_BASE_APPEND" "child before after" :inherit nil)))"#,
        ),
    )
    .expect("local-base-with-appends direct query should execute");
    assert_eq!(heading_ids(local_base_with_appends_direct_rows), vec![12]);

    let local_base_with_appends_inherited_rows = execute_sqlite_query(
        &connection,
        &validated(
            r#"(headings (and (title "Nested Task" :exact t) (property "LOCAL_BASE_APPEND" "child before after")))"#,
        ),
    )
    .expect("local-base-with-appends inherited query should execute");
    assert_eq!(
        heading_ids(local_base_with_appends_inherited_rows),
        vec![12]
    );

    let override_parent_rows = execute_sqlite_query(
        &connection,
        &validated(r#"(headings (property "OVERRIDE_CHAIN" "parent"))"#),
    )
    .expect("override parent property query should execute");
    assert_eq!(heading_ids(override_parent_rows), vec![11, 12, 14]);

    let override_child_rows = execute_sqlite_query(
        &connection,
        &validated(r#"(headings (property "OVERRIDE_CHAIN" "child"))"#),
    )
    .expect("override child property query should execute");
    assert_eq!(heading_ids(override_child_rows), vec![15]);

    let correlated_rows = execute_sqlite_query(
        &connection,
        &validated(r#"(headings (property "AREA" "infra"))"#),
    )
    .expect("correlated property query should execute");
    assert_eq!(heading_ids(correlated_rows), vec![11, 12, 14, 15]);
    assert_eq!(
        heading_file_paths(
            execute_sqlite_query(
                &connection,
                &validated(r#"(headings (property "CATEGORY" "work"))"#),
            )
            .expect("root property heading query should execute")
        ),
        vec!["/tmp/query-alpha.org".to_string()]
    );

    let after_rows = property_rows(&connection, 11, "LANG");
    assert_eq!(before_rows, after_rows);
    assert_eq!(
        after_rows,
        vec![
            (Some("rust".to_string()), false),
            (Some("emacs".to_string()), true),
        ]
    );

    let regexp_local_rows = execute_sqlite_query(
        &connection,
        &validated(
            r#"(headings (and (title "Query Engine" :exact t) (property "LANG" "rust em.*" :inherit nil :regexp t)))"#,
        ),
    )
    .expect("regexp local property query should execute");
    assert_eq!(heading_ids(regexp_local_rows), vec![11]);

    let regexp_inherited_rows = execute_sqlite_query(
        &connection,
        &validated(
            r#"(headings (and (title "Nested Task" :exact t) (property "APPEND_INHERITED" "parent child" :regexp t)))"#,
        ),
    )
    .expect("regexp inherited property query should execute");
    assert_eq!(heading_ids(regexp_inherited_rows), vec![12]);

    let regexp_overwrite_rows = execute_sqlite_query(
        &connection,
        &validated(
            r#"(headings (and (title "Loose Note" :exact t) (property "DEFINED_TWICE" "works" :inherit nil :regexp t)))"#,
        ),
    )
    .expect("regexp overwrite property query should execute");
    assert_eq!(heading_ids(regexp_overwrite_rows), vec![13]);

    let regexp_stale_rows = execute_sqlite_query(
        &connection,
        &validated(
            r#"(headings (and (title "Loose Note" :exact t) (property "DEFINED_TWICE" "second.*effective" :inherit nil :regexp t)))"#,
        ),
    )
    .expect("regexp stale property query should execute");
    assert_eq!(heading_ids(regexp_stale_rows), Vec::<i64>::new());
}

#[test]
fn execution_matches_keyword_queries_through_root_context() {
    let connection = seeded_connection();

    let heading_rows = execute_sqlite_query(
        &connection,
        &validated(r#"(headings (and (title "Loose Note" :exact t) (keyword "AUTHOR" "Alice")))"#),
    )
    .expect("heading keyword query should execute");
    assert_eq!(heading_ids(heading_rows), vec![13]);

    let file_rows = execute_sqlite_query(
        &connection,
        &validated(r#"(files (keyword "AUTHOR" "Alice"))"#),
    )
    .expect("file keyword query should execute");
    assert_eq!(
        file_paths(file_rows),
        vec!["/tmp/query-alpha.org".to_string()]
    );

    let regexp_heading_rows = execute_sqlite_query(
        &connection,
        &validated(
            r#"(headings (and (title "Loose Note" :exact t) (keyword "AUTHOR" "A.*" :regexp t)))"#,
        ),
    )
    .expect("regexp heading keyword query should execute");
    assert_eq!(heading_ids(regexp_heading_rows), vec![13]);

    let regexp_file_rows = execute_sqlite_query(
        &connection,
        &validated(r#"(files (keyword "AUTHOR" "A.*" :regexp t))"#),
    )
    .expect("regexp file keyword query should execute");
    assert_eq!(
        file_paths(regexp_file_rows),
        vec!["/tmp/query-alpha.org".to_string()]
    );
}

#[test]
fn execution_keyword_inherit_controls_root_and_local_heading_matching() {
    let connection = seeded_connection();

    let inherited = execute_sqlite_query(
        &connection,
        &validated(r#"(headings (keyword "AUTHOR" "Alice"))"#),
    )
    .expect("inherited keyword query should execute");
    let explicit_inherited = execute_sqlite_query(
        &connection,
        &validated(r#"(headings (keyword "AUTHOR" "Alice" :inherit t))"#),
    )
    .expect("explicit inherited keyword query should execute");
    assert_eq!(inherited, explicit_inherited);
    assert_eq!(heading_ids(inherited), vec![11, 12, 13, 14, 15]);

    for (query, expected_file_ids) in [
        (r#"(headings (keyword "AUTHOR" :inherit nil))"#, vec![2, 1]),
        (
            r#"(headings (keyword "AUTHOR" "Alice" :inherit nil))"#,
            vec![2],
        ),
        (
            r#"(headings (keyword "AUTHOR" "A.*" :inherit nil :regexp t))"#,
            vec![2],
        ),
    ] {
        let rows = execute_sqlite_query(&connection, &validated(query))
            .expect("local keyword query should execute");
        let QueryRows::Headings(rows) = rows else {
            panic!("heading query should return heading rows");
        };
        let file_ids = rows
            .into_iter()
            .map(|row| match row {
                HeadingQueryMatch::File(file) => file.id,
                HeadingQueryMatch::Heading(_) => {
                    panic!("local-only query must not match headings")
                }
            })
            .collect::<Vec<_>>();
        assert_eq!(file_ids, expected_file_ids);
    }

    for query in [
        r#"(headings (keyword "AUTHOR" "missing"))"#,
        r#"(headings (keyword "AUTHOR" "missing" :inherit nil))"#,
    ] {
        let rows = execute_sqlite_query(&connection, &validated(query))
            .expect("non-matching keyword query should execute");
        assert!(matches!(rows, QueryRows::Headings(rows) if rows.is_empty()));
    }
}

#[test]
fn execution_title_queries_match_normalized_heading_titles() {
    let connection = seeded_connection();

    let normalized_rows = execute_sqlite_query(
        &connection,
        &validated(r#"(headings (title "Statistic Cookies" :exact t))"#),
    )
    .expect("normalized title query should execute");
    match normalized_rows {
        QueryRows::Headings(rows) => {
            assert_eq!(rows.len(), 1);
            let HeadingQueryMatch::Heading(row) = &rows[0] else {
                panic!("expected heading row");
            };
            assert_eq!(row.id, 14);
            assert_eq!(row.title, "Statistic Cookies");
            assert_eq!(
                row.title_raw.as_deref(),
                Some("REVIEW [#B] Statistic Cookies [0/1]")
            );
        }
        other => panic!("unexpected rows for normalized title query: {other:?}"),
    }

    let raw_rows = execute_sqlite_query(
        &connection,
        &validated(r#"(headings (title "REVIEW [#B] Statistic Cookies [0/1]" :exact t))"#),
    )
    .expect("raw title query should execute");
    match raw_rows {
        QueryRows::Headings(rows) => assert!(rows.is_empty()),
        other => panic!("unexpected rows for raw title query: {other:?}"),
    }

    let regexp_rows = execute_sqlite_query(
        &connection,
        &validated(r#"(headings (title "Query.*" :regexp t))"#),
    )
    .expect("regexp title query should execute");
    assert_eq!(heading_ids(regexp_rows), vec![11]);
}

#[test]
fn execution_title_queries_can_match_root_files_without_propagating_to_headings() {
    let connection = seeded_connection();

    let default_rows = execute_sqlite_query(
        &connection,
        &validated(r#"(headings (title "Alpha Index" :exact t))"#),
    )
    .expect("default root title query should execute");
    match default_rows {
        QueryRows::Headings(rows) => {
            assert_eq!(rows.len(), 1);
            assert!(matches!(rows[0], HeadingQueryMatch::File(_)));
        }
        other => panic!("unexpected default title rows: {other:?}"),
    }

    let file_title_rows = execute_sqlite_query(
        &connection,
        &validated(r#"(headings (file-title "Alpha Index" :exact t))"#),
    )
    .expect("file-title query should execute");
    assert_eq!(heading_ids(file_title_rows), vec![11, 12, 13, 14, 15]);
}

#[test]
fn execution_heading_queries_return_matching_root_rows_across_predicates() {
    let connection = seeded_connection();

    assert_eq!(
        heading_file_paths(
            execute_sqlite_query(
                &connection,
                &validated(r#"(headings (keyword "AUTHOR" "Alice"))"#),
            )
            .expect("keyword heading query should execute")
        ),
        vec!["/tmp/query-alpha.org".to_string()]
    );

    assert_eq!(
        heading_file_paths(
            execute_sqlite_query(&connection, &validated(r#"(headings (level 0))"#))
                .expect("level heading query should execute")
        ),
        vec![
            "/tmp/query-alpha.org".to_string(),
            "/tmp/query-beta.org".to_string(),
            "/tmp/query-gamma.org".to_string(),
        ]
    );
}

#[test]
fn execution_children_and_descendants_can_match_root_rows() {
    let connection = seeded_connection();

    assert_eq!(
        heading_file_paths(
            execute_sqlite_query(&connection, &validated(r#"(headings (children))"#))
                .expect("children heading query should execute")
        ),
        vec![
            "/tmp/query-alpha.org".to_string(),
            "/tmp/query-beta.org".to_string(),
            "/tmp/query-gamma.org".to_string(),
        ]
    );

    assert_eq!(
        heading_file_paths(
            execute_sqlite_query(&connection, &validated(r#"(headings (descendants))"#))
                .expect("descendants heading query should execute")
        ),
        vec![
            "/tmp/query-alpha.org".to_string(),
            "/tmp/query-beta.org".to_string(),
            "/tmp/query-gamma.org".to_string(),
        ]
    );

    let parent_rows = execute_sqlite_query(&connection, &validated(r#"(headings (parent))"#))
        .expect("parent heading query should execute");
    assert!(heading_file_paths(parent_rows).is_empty());
}

#[test]
fn execution_file_predicates_return_matching_roots_in_heading_queries() {
    let connection = seeded_connection();

    let file_path_rows = execute_sqlite_query(
        &connection,
        &validated(r#"(headings (file-path "/tmp/query-alpha.org" :exact t))"#),
    )
    .expect("file-path heading query should execute");
    assert_eq!(
        heading_file_paths(file_path_rows),
        vec!["/tmp/query-alpha.org".to_string()]
    );

    let file_name_rows = execute_sqlite_query(
        &connection,
        &validated(r#"(headings (file-name "query-alpha.org" :exact t))"#),
    )
    .expect("file-name heading query should execute");
    assert_eq!(
        heading_file_paths(file_name_rows),
        vec!["/tmp/query-alpha.org".to_string()]
    );

    let file_dir_rows = execute_sqlite_query(
        &connection,
        &validated(r#"(headings (file-dir "/tmp" :exact t))"#),
    )
    .expect("file-dir heading query should execute");
    assert_eq!(
        heading_file_paths(file_dir_rows),
        vec![
            "/tmp/query-alpha.org".to_string(),
            "/tmp/query-beta.org".to_string(),
            "/tmp/query-gamma.org".to_string(),
        ]
    );

    let file_title_rows = execute_sqlite_query(
        &connection,
        &validated(r#"(headings (file-title "Alpha Index" :exact t))"#),
    )
    .expect("file-title heading query should execute");
    assert_eq!(
        heading_file_paths(file_title_rows),
        vec!["/tmp/query-alpha.org".to_string()]
    );

    let file_modified_rows = execute_sqlite_query(
        &connection,
        &validated(r#"(headings (file-modified :to "2026-01-03"))"#),
    )
    .expect("file-modified heading query should execute");
    assert_eq!(
        heading_file_paths(file_modified_rows),
        vec!["/tmp/query-alpha.org".to_string()]
    );

    let regexp_file_path_rows = execute_sqlite_query(
        &connection,
        &validated(r#"(headings (file-path ".*/query-alpha\\.org" :regexp t))"#),
    )
    .expect("regexp file-path heading query should execute");
    assert_eq!(
        heading_file_paths(regexp_file_path_rows),
        vec!["/tmp/query-alpha.org".to_string()]
    );

    let regexp_file_name_rows = execute_sqlite_query(
        &connection,
        &validated(r#"(headings (file-name "query-(alpha|beta)\\.org" :regexp t))"#),
    )
    .expect("regexp file-name heading query should execute");
    assert_eq!(
        heading_file_paths(regexp_file_name_rows),
        vec![
            "/tmp/query-alpha.org".to_string(),
            "/tmp/query-beta.org".to_string(),
        ]
    );

    let regexp_file_title_rows = execute_sqlite_query(
        &connection,
        &validated(r#"(headings (file-title "Alpha.*" :regexp t))"#),
    )
    .expect("regexp file-title heading query should execute");
    assert_eq!(
        heading_file_paths(regexp_file_title_rows),
        vec!["/tmp/query-alpha.org".to_string()]
    );
}

#[test]
fn execution_file_predicate_roots_respect_boolean_composition() {
    let connection = seeded_connection();

    let and_rows = execute_sqlite_query(
        &connection,
        &validated(
            r#"(headings
                (and
                  (file-path "/tmp/query-alpha.org" :exact t)
                  (todo "NEXT")))"#,
        ),
    )
    .expect("and heading query should execute");
    assert_eq!(heading_ids(and_rows), vec![11]);

    let or_rows = execute_sqlite_query(
        &connection,
        &validated(
            r#"(headings
                (or
                  (file-path "/tmp/query-alpha.org" :exact t)
                  (title "Beta Index" :exact t)))"#,
        ),
    )
    .expect("or heading query should execute");
    assert_eq!(
        heading_file_paths(or_rows),
        vec![
            "/tmp/query-alpha.org".to_string(),
            "/tmp/query-beta.org".to_string(),
        ]
    );
}

#[test]
fn execution_file_queries_support_file_name_and_file_dir() {
    let connection = seeded_connection();

    let file_name_rows = execute_sqlite_query(
        &connection,
        &validated(r#"(files (file-name "query-alpha.org" :exact t))"#),
    )
    .expect("file-name files query should execute");
    assert_eq!(
        file_paths(file_name_rows),
        vec!["/tmp/query-alpha.org".to_string()]
    );

    let file_dir_rows = execute_sqlite_query(
        &connection,
        &validated(r#"(files (file-dir "/tmp" :exact t))"#),
    )
    .expect("file-dir files query should execute");
    assert_eq!(
        file_paths(file_dir_rows),
        vec![
            "/tmp/query-alpha.org".to_string(),
            "/tmp/query-beta.org".to_string(),
            "/tmp/query-gamma.org".to_string(),
        ]
    );

    let regexp_file_rows = execute_sqlite_query(
        &connection,
        &validated(r#"(files (file-path ".*/query-(alpha|beta)\\.org" :regexp t))"#),
    )
    .expect("regexp file query should execute");
    assert_eq!(
        file_paths(regexp_file_rows),
        vec![
            "/tmp/query-alpha.org".to_string(),
            "/tmp/query-beta.org".to_string(),
        ]
    );
}

#[test]
fn bare_headings_query_returns_file_roots_and_real_headings() {
    let connection = seeded_connection();

    let rows = execute_sqlite_query(&connection, &validated(r#"(headings)"#))
        .expect("bare headings query should execute");
    let QueryRows::Headings(rows) = rows else {
        panic!("expected heading rows");
    };

    let expected_file_count = connection
        .query_row("SELECT COUNT(*) FROM files", [], |row| row.get::<_, i64>(0))
        .expect("file count should load");
    let expected_file_count =
        usize::try_from(expected_file_count).expect("file count should fit usize");
    let expected_heading_count = connection
        .query_row("SELECT COUNT(*) FROM headings WHERE level > 0", [], |row| {
            row.get::<_, i64>(0)
        })
        .expect("real heading count should load");
    let expected_heading_count =
        usize::try_from(expected_heading_count).expect("heading count should fit usize");

    let file_rows = rows
        .iter()
        .filter_map(|row| match row {
            HeadingQueryMatch::File(row) => Some(row),
            HeadingQueryMatch::Heading(_) => None,
        })
        .collect::<Vec<_>>();
    let heading_rows = rows
        .iter()
        .filter_map(|row| match row {
            HeadingQueryMatch::Heading(row) => Some(row),
            HeadingQueryMatch::File(_) => None,
        })
        .collect::<Vec<_>>();

    assert_eq!(file_rows.len(), expected_file_count);
    assert_eq!(heading_rows.len(), expected_heading_count);
    assert!(heading_rows.iter().all(|row| row.level > 0));

    let mut unique_file_ids = file_rows.iter().map(|row| row.id).collect::<Vec<_>>();
    unique_file_ids.sort_unstable();
    unique_file_ids.dedup();
    assert_eq!(unique_file_ids.len(), expected_file_count);

    assert!(
        matches!(rows.first(), Some(HeadingQueryMatch::File(row)) if row.path == "/tmp/query-alpha.org")
    );
    assert!(
        matches!(rows.get(1), Some(HeadingQueryMatch::Heading(row)) if row.file_path == "/tmp/query-alpha.org" && row.id == 11)
    );

    let bare_heading_ids = heading_rows.iter().map(|row| row.id).collect::<Vec<_>>();
    let filtered_heading_ids = heading_ids(
        execute_sqlite_query(&connection, &validated(r#"(headings (title "Property"))"#))
            .expect("filtered headings query should execute"),
    );
    assert!(filtered_heading_ids
        .iter()
        .all(|id| bare_heading_ids.contains(id)));
}

#[test]
fn compile_has_text_uses_correlated_exists_with_bound_params() {
    let compiled = compile_sqlite_query(&validated(
        r#"(headings (has-text "sqlite" "fts" "x' OR 1=1 --"))"#,
    ))
    .expect("query should compile");

    assert_eq!(compiled.target, QueryTarget::Headings);
    assert_eq!(compiled.params.len(), 3);
    assert_eq!(
        compiled.params,
        vec![
            super::QueryParam::Text("sqlite".to_string()),
            super::QueryParam::Text("fts".to_string()),
            super::QueryParam::Text("x' OR 1=1 --".to_string()),
        ]
    );
    assert!(compiled.sql.contains("FROM heading_bodies"));
    assert!(compiled.sql.contains("heading_bodies.heading_id = h0.id"));
    assert!(compiled.sql.matches("EXISTS (").count() >= 3);
    assert!(!compiled.sql.contains("x' OR 1=1 --"));
    assert!(!compiled.sql.contains("1=1 --"));
}

#[test]
fn execution_matches_has_text_against_persisted_heading_bodies() {
    let connection = seeded_connection();

    let single_term_rows =
        execute_sqlite_query(&connection, &validated(r#"(headings (has-text "sqlite"))"#))
            .expect("single-term has-text query should execute");
    assert_eq!(heading_ids(single_term_rows), vec![11, 12, 21]);

    let and_rows = execute_sqlite_query(
        &connection,
        &validated(r#"(headings (has-text "sqlite" "fts"))"#),
    )
    .expect("multi-term has-text query should execute");
    assert_eq!(heading_ids(and_rows), vec![11, 21]);

    let case_rows = execute_sqlite_query(
        &connection,
        &validated(r#"(headings (has-text "SQLITE" "FTS"))"#),
    )
    .expect("case-insensitive has-text query should execute");
    assert_eq!(heading_ids(case_rows), vec![11, 21]);

    let no_match_rows = execute_sqlite_query(
        &connection,
        &validated(r#"(headings (has-text "missing phrase"))"#),
    )
    .expect("no-match has-text query should execute");
    assert_eq!(heading_ids(no_match_rows), Vec::<i64>::new());

    let regexp_rows = execute_sqlite_query(
        &connection,
        &validated(r#"(headings (has-text "(?i)sqlite.*fts" :regexp t))"#),
    )
    .expect("regexp has-text query should execute");
    assert_eq!(heading_ids(regexp_rows), vec![11, 21]);

    let regexp_no_match_rows = execute_sqlite_query(
        &connection,
        &validated(r#"(headings (has-text "(?i)fts.*sqlite" :regexp t))"#),
    )
    .expect("regexp no-match has-text query should execute");
    assert_eq!(heading_ids(regexp_no_match_rows), Vec::<i64>::new());
}

#[test]
fn execution_has_text_excludes_empty_or_missing_body_rows_without_mutation() {
    let connection = seeded_connection();
    let body_count_before: i64 = connection
        .query_row("SELECT COUNT(*) FROM heading_bodies", [], |row| row.get(0))
        .expect("body count should load");

    let empty_body_rows = execute_sqlite_query(
        &connection,
        &validated(r#"(headings (and (title "Statistic Cookies" :exact t) (has-text "sqlite")))"#),
    )
    .expect("empty-body query should execute");
    assert_eq!(heading_ids(empty_body_rows), Vec::<i64>::new());

    let missing_body_rows = execute_sqlite_query(
        &connection,
        &validated(r#"(headings (and (title "Gamma Candidate" :exact t) (has-text "sqlite")))"#),
    )
    .expect("missing-body query should execute");
    assert_eq!(heading_ids(missing_body_rows), Vec::<i64>::new());

    let body_count_after: i64 = connection
        .query_row("SELECT COUNT(*) FROM heading_bodies", [], |row| row.get(0))
        .expect("body count should reload");
    assert_eq!(body_count_before, body_count_after);
}

#[test]
fn injection_like_strings_remain_bound_and_do_not_broaden_results() {
    let connection = seeded_connection();

    for query in [
        validated(r#"(headings (title "x' OR 1=1 --"))"#),
        validated(r#"(headings (has-text "x' OR 1=1 --"))"#),
        validated(r#"(headings (title "x' OR 1=1 --" :regexp t))"#),
        validated(r#"(headings (has-text "x' OR 1=1 --" :regexp t))"#),
        validated(r#"(headings (tags "x' OR 1=1 --" :inherit nil))"#),
        validated(r#"(headings (tags "x' OR 1=1 --" :inherit nil :regexp t))"#),
        validated(r#"(headings (property "OWNER" "x' OR 1=1 --"))"#),
        validated(r#"(headings (property "OWNER" "x' OR 1=1 --" :regexp t))"#),
        validated(r#"(files (keyword "AUTHOR" "x' OR 1=1 --"))"#),
        validated(r#"(files (keyword "AUTHOR" "x' OR 1=1 --" :regexp t))"#),
        validated(r#"(links (link-target "x' OR 1=1 --" :regexp t))"#),
        validated(r#"(headings (links-to (headings (title "x' OR 1=1 --"))))"#),
    ] {
        let compiled = compile_sqlite_query(&query).expect("query should compile");
        assert!(!compiled.sql.contains("1=1"));
        assert!(!compiled.params.is_empty());

        match execute_sqlite_query(&connection, &query).expect("query should execute") {
            QueryRows::Headings(rows) => assert!(rows.is_empty()),
            QueryRows::Files(rows) => assert!(rows.is_empty()),
            QueryRows::Links(rows) => assert!(rows.is_empty()),
        }
    }
}

#[test]
fn execution_matches_link_and_file_regex_metadata_predicates() {
    let connection = seeded_connection();

    let link_target_rows = execute_sqlite_query(
        &connection,
        &validated(r#"(links (link-target "file:beta\\.org.*" :regexp t))"#),
    )
    .expect("regexp link-target query should execute");
    assert_eq!(link_ids(link_target_rows), vec![102, 100, 101]);

    let link_description_rows = execute_sqlite_query(
        &connection,
        &validated(r#"(links (link-description "Beta.*" :regexp t))"#),
    )
    .expect("regexp link-description query should execute");
    assert_eq!(link_ids(link_description_rows), vec![100, 101]);
}

#[test]
fn every_validator_predicate_has_an_executed_query() {
    let connection = seeded_connection();
    connection
        .execute_batch(
            "UPDATE headings SET todo_keyword = 'DONE', todo_type = 'closed' WHERE id = 12;
             UPDATE headings SET deadline_raw = '<2026-01-05 Mon>', deadline_ts = 1767571200 WHERE id = 11;",
        )
        .expect("fixture update should apply");
    let cases: &[(&str, &str)] = &[
        ("todo", r#"(headings (todo))"#),
        ("done", r#"(headings (done))"#),
        ("title", r#"(headings (title "Query Engine"))"#),
        ("has-text", r#"(headings (has-text "query"))"#),
        ("level", r#"(headings (level 1))"#),
        ("priority", r#"(headings (priority "A"))"#),
        ("tags", r#"(headings (tags "query"))"#),
        ("property", r#"(headings (property "ID"))"#),
        ("keyword", r#"(headings (keyword "AUTHOR" "Alice"))"#),
        ("file-name", r#"(files (file-name "query-alpha.org"))"#),
        ("file-path", r#"(files (file-path "/tmp/query-alpha.org"))"#),
        ("file-dir", r#"(files (file-dir "/tmp"))"#),
        ("file-title", r#"(files (file-title "Alpha"))"#),
        (
            "file-modified",
            r#"(files (file-modified :from "2000-01-01"))"#,
        ),
        (
            "outline-contains",
            r#"(headings (outline-contains "Query"))"#,
        ),
        (
            "outline-sequence",
            r#"(headings (outline-sequence "Query"))"#,
        ),
        ("ts", r#"(headings (ts :on "2026-01-03"))"#),
        ("ts-active", r#"(headings (ts-active :on "2026-01-03"))"#),
        ("ts-inactive", r#"(headings (ts-inactive))"#),
        ("deadline", r#"(headings (deadline :on "2026-01-05"))"#),
        ("scheduled", r#"(headings (scheduled :on "2026-01-03"))"#),
        ("closed", r#"(headings (closed))"#),
        ("planning", r#"(headings (planning))"#),
        ("parent", r#"(headings (parent (headings (level 0))))"#),
        (
            "ancestors",
            r#"(headings (ancestors (headings (level 0))))"#,
        ),
        ("children", r#"(headings (children (headings (level 1))))"#),
        (
            "descendants",
            r#"(headings (descendants (headings (level 1))))"#,
        ),
        (
            "has-link",
            r#"(headings (has-link (links (status "resolved"))))"#,
        ),
        ("links-to", r#"(headings (links-to (headings (level 1))))"#),
        ("linked-from", r#"(headings (linked-from :any))"#),
        ("link-type", r#"(links (link-type "file"))"#),
        (
            "link-target",
            r#"(links (link-target "file:beta.org" :exact t))"#,
        ),
        (
            "link-description",
            r#"(links (link-description "Beta heading" :exact t))"#,
        ),
        ("has-description", r#"(links (has-description))"#),
        ("status", r#"(links (status "resolved"))"#),
        ("source", r#"(links (source :any))"#),
        ("target", r#"(links (target :any))"#),
    ];

    // The validator's predicate list is the source of truth.
    let source = include_str!("../validate.rs");
    let start = source
        .find("fn predicate_is_known_globally")
        .expect("validator predicate list should exist");
    let body = &source[start..];
    let body = &body[..body.find("\n}\n").expect("function should end")];
    let mut names: Vec<&str> = body.split('"').skip(1).step_by(2).collect();
    names.sort_unstable();
    let mut covered: Vec<&str> = cases.iter().map(|(name, _)| *name).collect();
    covered.sort_unstable();
    assert_eq!(
        covered, names,
        "every validator predicate needs an executed query"
    );

    for (name, query) in cases {
        execute_sqlite_query(&connection, &validated(query))
            .unwrap_or_else(|error| panic!("{name} query {query} should execute: {error}"));
    }

    let ids = |query: &str| {
        heading_ids(execute_sqlite_query(&connection, &validated(query)).expect("query runs"))
    };
    assert_eq!(ids("(headings (done))"), vec![12]);
    assert_eq!(ids("(headings (and (not (done)) (todo)))"), vec![11, 14]);
    assert_eq!(ids(r#"(headings (deadline :on "2026-01-05"))"#), vec![11]);
    assert!(ids(r#"(headings (deadline :on "2026-01-06"))"#).is_empty());
    let links = |query: &str| {
        link_ids(execute_sqlite_query(&connection, &validated(query)).expect("query runs"))
    };
    assert_eq!(
        links(r#"(links (link-description "Beta heading" :exact t))"#),
        vec![101]
    );
    assert!(links(r#"(links (link-description "No such text" :exact t))"#).is_empty());
}

#[test]
fn execution_matches_persisted_scheduled_and_ts_active_predicates() {
    let connection = seeded_connection();

    let scheduled_rows = execute_sqlite_query(
        &connection,
        &validated(r#"(headings (scheduled :on "2026-01-03"))"#),
    )
    .expect("scheduled query should execute");
    match scheduled_rows {
        QueryRows::Headings(rows) => {
            assert_eq!(rows.len(), 1);
            let HeadingQueryMatch::Heading(row) = &rows[0] else {
                panic!("expected heading row");
            };
            assert_eq!(row.id, 11);
            assert_eq!(row.scheduled_ts, Some(1_767_398_400));
        }
        other => panic!("unexpected scheduled rows: {other:?}"),
    }

    let ts_active_rows = execute_sqlite_query(
        &connection,
        &validated(r#"(headings (ts-active :on "2026-01-03"))"#),
    )
    .expect("ts-active query should execute");
    match ts_active_rows {
        QueryRows::Headings(rows) => {
            assert_eq!(rows.len(), 1);
            let HeadingQueryMatch::Heading(row) = &rows[0] else {
                panic!("expected heading row");
            };
            assert_eq!(row.id, 11);
            assert_eq!(row.title, "Query Engine");
        }
        other => panic!("unexpected ts-active rows: {other:?}"),
    }
}

#[test]
fn execution_temporal_predicates_match_date_only_and_timed_rows() {
    let connection = temporal_test_connection();

    assert_eq!(
        heading_ids(
            execute_sqlite_query(
                &connection,
                &validated(r#"(headings (scheduled :on "2026-01-03"))"#),
            )
            .expect("scheduled date query should execute")
        ),
        vec![201, 202, 203, 204]
    );
    assert_eq!(
        heading_ids(
            execute_sqlite_query(&connection, &validated(r#"(headings (scheduled))"#),)
                .expect("scheduled query should execute")
        ),
        vec![201, 202, 203, 204]
    );

    assert_eq!(
        heading_ids(
            execute_sqlite_query(&connection, &validated(r#"(headings (deadline))"#),)
                .expect("deadline query should execute")
        ),
        vec![205, 206, 207, 208]
    );

    assert_eq!(
        heading_ids(
            execute_sqlite_query(&connection, &validated(r#"(headings (closed))"#),)
                .expect("closed query should execute")
        ),
        vec![209, 210, 211, 212]
    );

    assert_eq!(
        heading_ids(
            execute_sqlite_query(&connection, &validated(r#"(headings (planning))"#),)
                .expect("planning query should execute")
        ),
        vec![201, 202, 203, 204, 205, 206, 207, 208, 209, 210, 211, 212]
    );
}

#[test]
fn execution_generic_timestamp_predicates_match_date_only_and_timed_rows() {
    let connection = temporal_test_connection();

    assert_eq!(
        heading_ids(
            execute_sqlite_query(&connection, &validated(r#"(headings (ts))"#))
                .expect("ts query should execute")
        ),
        vec![213, 214, 215, 216, 217, 218, 219, 220]
    );
    assert_eq!(
        heading_ids(
            execute_sqlite_query(&connection, &validated(r#"(headings (ts-active))"#),)
                .expect("ts-active query should execute")
        ),
        vec![213, 214, 215, 216]
    );
    assert_eq!(
        heading_ids(
            execute_sqlite_query(&connection, &validated(r#"(headings (ts-inactive))"#),)
                .expect("ts-inactive query should execute")
        ),
        vec![217, 218, 219, 220]
    );
}

#[test]
fn execution_date_only_to_includes_full_day_and_excludes_following_day() {
    let connection = seeded_connection();

    let file_rows = execute_sqlite_query(
        &connection,
        &validated(r#"(files (file-modified :to "2026-01-03"))"#),
    )
    .expect("file query should execute");

    assert_eq!(
        file_paths(file_rows),
        vec!["/tmp/query-alpha.org".to_string()]
    );
}

#[test]
fn execution_date_only_from_uses_inclusive_start_of_day_boundary() {
    let connection = date_bound_test_connection();

    let rows = execute_sqlite_query(
        &connection,
        &validated(r#"(headings (scheduled :from "2026-01-03"))"#),
    )
    .expect("date-only :from query should execute");

    assert_eq!(
        heading_ids(rows),
        vec![100, 101, 102, 103, 106, 107, 104, 105]
    );
}

#[test]
fn compile_distinguishes_date_only_and_datetime_bounds() {
    let date_only = compile_sqlite_query(&temporal_resolved(
        r#"(headings (scheduled :from "2026-01-03" :to "2026-01-03"))"#,
    ))
    .expect("date-only query should compile");
    let datetime = compile_sqlite_query(&temporal_resolved(
        r#"(headings (scheduled :from "2026-01-03 09:15" :to "2026-01-03 09:15"))"#,
    ))
    .expect("datetime query should compile");

    assert_eq!(
        date_only.params,
        vec![
            super::QueryParam::Integer(1_767_398_400),
            super::QueryParam::Integer(1_767_484_800),
        ]
    );
    assert!(!date_only.sql.contains("unixepoch"));
    assert!(!date_only.sql.contains("localtime"));

    assert_eq!(
        datetime.params,
        vec![
            super::QueryParam::Integer(1_767_431_700),
            super::QueryParam::Integer(1_767_431_760),
        ]
    );
    assert!(!datetime.sql.contains("unixepoch"));
    assert!(!datetime.sql.contains("localtime"));
}

#[test]
fn execution_datetime_bounds_preserve_hour_and_minute_without_timezone_conversion() {
    let connection = date_bound_test_connection();

    let on_rows = execute_sqlite_query(
        &connection,
        &validated(r#"(headings (scheduled :on "2026-01-03 09:15"))"#),
    )
    .expect("datetime :on query should execute");
    assert_eq!(heading_ids(on_rows), vec![101]);

    let from_rows = execute_sqlite_query(
        &connection,
        &validated(r#"(headings (scheduled :from "2026-01-03 09:15"))"#),
    )
    .expect("datetime :from query should execute");
    assert_eq!(
        heading_ids(from_rows),
        vec![101, 102, 103, 106, 107, 104, 105]
    );

    let to_rows = execute_sqlite_query(
        &connection,
        &validated(r#"(headings (scheduled :to "2026-01-03 09:15"))"#),
    )
    .expect("datetime :to query should execute");
    assert_eq!(heading_ids(to_rows), vec![100, 101]);
}

#[test]
fn execution_datetime_bounds_cover_complete_minutes_and_seconds() {
    let connection = date_bound_test_connection();
    connection
        .execute(
            "UPDATE headings SET scheduled_ts = ?1 WHERE id = 101",
            [naive_date_time_seconds(2026, 1, 3, 9, 15) + 45],
        )
        .expect("scheduled timestamp should update");

    let minute_rows = execute_sqlite_query(
        &connection,
        &validated(r#"(headings (scheduled :on "2026-01-03 09:15"))"#),
    )
    .expect("minute query should execute");
    assert_eq!(heading_ids(minute_rows), vec![101]);

    let second_rows = execute_sqlite_query(
        &connection,
        &validated(r#"(headings (scheduled :on "2026-01-03 09:15:45"))"#),
    )
    .expect("second query should execute");
    assert_eq!(heading_ids(second_rows), vec![101]);

    let next_second_rows = execute_sqlite_query(
        &connection,
        &validated(r#"(headings (scheduled :on "2026-01-03 09:15:46"))"#),
    )
    .expect("next-second query should execute");
    assert!(heading_ids(next_second_rows).is_empty());

    for value in ["2026-01-03 09:15", "2026-01-03 09:15:45"] {
        let from_rows = execute_sqlite_query(
            &connection,
            &validated(&format!(r#"(headings (scheduled :from "{value}"))"#)),
        )
        .expect("from query should execute");
        assert!(heading_ids(from_rows).contains(&101));

        let to_rows = execute_sqlite_query(
            &connection,
            &validated(&format!(r#"(headings (scheduled :to "{value}"))"#)),
        )
        .expect("to query should execute");
        assert_eq!(heading_ids(to_rows), vec![100, 101]);

        let equal_range_rows = execute_sqlite_query(
            &connection,
            &validated(&format!(
                r#"(headings (scheduled :from "{value}" :to "{value}"))"#
            )),
        )
        .expect("equal range query should execute");
        assert_eq!(heading_ids(equal_range_rows), vec![101]);
    }
}

#[test]
fn execution_file_modified_datetime_bounds_include_nanoseconds_before_the_next_interval() {
    let connection = date_bound_test_connection();
    let minute_start = naive_date_time_seconds(2026, 1, 3, 9, 15) * 1_000_000_000;
    connection
        .execute(
            "UPDATE files SET mtime_ns = ?1",
            [minute_start + 59_999_999_999],
        )
        .expect("file mtime should update");

    let execution_options = QueryExecutionOptions {
        query_timezone: Some("UTC".to_string()),
        ..QueryExecutionOptions::default()
    };

    let minute_rows = execute_sqlite_query_with_options(
        &connection,
        &validated(r#"(files (file-modified :on "2026-01-03 09:15"))"#),
        &execution_options,
    )
    .expect("minute query should execute");
    assert_eq!(
        file_paths(minute_rows),
        vec!["/tmp/date-bounds.org".to_string()]
    );

    let second_rows = execute_sqlite_query_with_options(
        &connection,
        &validated(r#"(files (file-modified :on "2026-01-03 09:15:59"))"#),
        &execution_options,
    )
    .expect("second query should execute");
    assert_eq!(
        file_paths(second_rows),
        vec!["/tmp/date-bounds.org".to_string()]
    );

    let next_second_rows = execute_sqlite_query_with_options(
        &connection,
        &validated(r#"(files (file-modified :on "2026-01-03 09:16:00"))"#),
        &execution_options,
    )
    .expect("next-second query should execute");
    assert!(file_paths(next_second_rows).is_empty());
}

#[test]
fn execution_distinguishes_date_only_and_explicit_midnight_datetime_bounds() {
    let connection = date_bound_test_connection();

    let day_rows = execute_sqlite_query(
        &connection,
        &validated(r#"(headings (scheduled :on "2026-01-03"))"#),
    )
    .expect("date-only query should execute");
    assert_eq!(heading_ids(day_rows), vec![100, 101, 102]);

    let midnight_rows = execute_sqlite_query(
        &connection,
        &validated(r#"(headings (scheduled :on "2026-01-03 00:00"))"#),
    )
    .expect("midnight datetime query should execute");
    assert_eq!(heading_ids(midnight_rows), vec![100]);

    let midnight_to_rows = execute_sqlite_query(
        &connection,
        &validated(r#"(headings (scheduled :to "2026-01-03 00:00"))"#),
    )
    .expect("midnight datetime :to query should execute");
    assert_eq!(heading_ids(midnight_to_rows), vec![100]);
}

#[test]
fn execution_date_only_next_day_boundaries_cover_month_end_year_end_and_dst_dates() {
    let connection = date_bound_test_connection();

    let month_end_rows = execute_sqlite_query(
        &connection,
        &validated(r#"(headings (scheduled :to "2026-01-31"))"#),
    )
    .expect("month-end query should execute");
    assert_eq!(heading_ids(month_end_rows), vec![100, 101, 102, 106]);

    let february_first_rows = execute_sqlite_query(
        &connection,
        &validated(r#"(headings (scheduled :to "2026-02-01"))"#),
    )
    .expect("february-first query should execute");
    assert_eq!(
        heading_ids(february_first_rows),
        vec![100, 101, 102, 106, 107]
    );

    let year_end_rows = execute_sqlite_query(
        &connection,
        &validated(r#"(headings (scheduled :to "2026-12-31"))"#),
    )
    .expect("year-end query should execute");
    assert_eq!(
        heading_ids(year_end_rows),
        vec![100, 101, 102, 103, 106, 107, 104]
    );

    let dst_day_rows = execute_sqlite_query(
        &connection,
        &validated(r#"(headings (scheduled :on "2026-03-29"))"#),
    )
    .expect("dst date-only query should execute");
    assert_eq!(heading_ids(dst_day_rows), vec![103]);

    let dst_datetime_rows = execute_sqlite_query(
        &connection,
        &validated(r#"(headings (scheduled :on "2026-03-29 02:30"))"#),
    )
    .expect("dst datetime query should execute");
    assert_eq!(heading_ids(dst_datetime_rows), vec![103]);
}

#[test]
fn execution_resolves_relative_dates_before_sql_with_bound_parameters() {
    let query = validated(r#"(headings (scheduled :from today :to 1))"#);
    let resolved = resolve_relative_dates(
        &query,
        &QueryDateResolutionOptions {
            timezone: Some("UTC".to_string()),
            now_utc: Some(
                "2026-01-03T12:00:00Z"
                    .parse()
                    .expect("timestamp should parse"),
            ),
        },
    )
    .expect("query should resolve");
    let resolved = resolve_temporal_bounds(
        &resolved,
        &QueryDateResolutionOptions {
            timezone: Some("UTC".to_string()),
            now_utc: None,
        },
    )
    .expect("temporal bounds should resolve");
    let compiled = compile_sqlite_query(&resolved).expect("query should compile");

    assert!(!compiled.sql.contains("localtime"));
    assert_eq!(
        compiled.params,
        vec![
            super::QueryParam::Integer(1_767_398_400),
            super::QueryParam::Integer(1_767_571_200),
        ]
    );

    let connection = seeded_connection();
    let rows = execute_sqlite_query_with_options(
        &connection,
        &query,
        &QueryExecutionOptions {
            now_utc: Some(
                "2026-01-03T12:00:00Z"
                    .parse()
                    .expect("timestamp should parse"),
            ),
            query_timezone: Some("UTC".to_string()),
            ..QueryExecutionOptions::default()
        },
    )
    .expect("query should execute");

    match rows {
        QueryRows::Headings(rows) => {
            assert_eq!(rows.len(), 1);
            let HeadingQueryMatch::Heading(row) = &rows[0] else {
                panic!("expected heading row");
            };
            assert_eq!(row.id, 11);
            assert_eq!(row.scheduled_ts, Some(1_767_398_400));
        }
        other => panic!("unexpected scheduled rows: {other:?}"),
    }
}

#[test]
fn execution_uses_effective_timezone_for_relative_dates() {
    let connection = seeded_connection();
    let query = validated(r#"(headings (scheduled :on today))"#);

    let utc_rows = execute_sqlite_query_with_options(
        &connection,
        &query,
        &QueryExecutionOptions {
            now_utc: Some(
                "2026-01-03T23:30:00Z"
                    .parse()
                    .expect("timestamp should parse"),
            ),
            query_timezone: Some("UTC".to_string()),
            ..QueryExecutionOptions::default()
        },
    )
    .expect("utc query should execute");
    let zurich_rows = execute_sqlite_query_with_options(
        &connection,
        &query,
        &QueryExecutionOptions {
            now_utc: Some(
                "2026-01-03T23:30:00Z"
                    .parse()
                    .expect("timestamp should parse"),
            ),
            query_timezone: Some("Europe/Zurich".to_string()),
            ..QueryExecutionOptions::default()
        },
    )
    .expect("zurich query should execute");

    assert_eq!(heading_ids(utc_rows), vec![11]);
    assert_eq!(heading_ids(zurich_rows), Vec::<i64>::new());
}

#[test]
fn execution_returns_date_resolution_error_for_invalid_timezone() {
    let connection = seeded_connection();
    let query = validated(r#"(headings (scheduled :on today))"#);

    let error = execute_sqlite_query_with_options(
        &connection,
        &query,
        &QueryExecutionOptions {
            now_utc: Some(
                "2026-01-03T23:30:00Z"
                    .parse()
                    .expect("timestamp should parse"),
            ),
            query_timezone: Some("Mars/Olympus".to_string()),
            ..QueryExecutionOptions::default()
        },
    )
    .expect_err("query should fail");

    assert_eq!(error.kind, QueryExecutionErrorKind::DateResolution);
    assert!(error.to_string().contains("invalid query timezone"));
}

#[test]
fn execution_returns_date_resolution_error_for_out_of_range_offset() {
    let connection = seeded_connection();
    let query = validated(r#"(headings (scheduled :to 9223372036854775807))"#);

    let error = execute_sqlite_query_with_options(
        &connection,
        &query,
        &QueryExecutionOptions {
            now_utc: Some(
                "2026-01-03T23:30:00Z"
                    .parse()
                    .expect("timestamp should parse"),
            ),
            query_timezone: Some("UTC".to_string()),
            ..QueryExecutionOptions::default()
        },
    )
    .expect_err("query should fail");

    assert_eq!(error.kind, QueryExecutionErrorKind::DateResolution);
    assert!(error.to_string().contains("out of range"));
}

#[test]
fn execution_reads_persisted_db_only_and_does_not_mutate_database() {
    let test_dir = TestDir::new("persisted");
    let db_path = test_dir.path().join("query.sqlite");
    let org_path = test_dir.path().join("notes.org");
    fs::write(&org_path, "* changed after indexing\n").expect("org file should write");

    {
        let mut connection = open_database(&db_path).expect("database should open");
        seed_database(
            &mut connection,
            &org_path,
            &test_dir.path().join("beta.org"),
        );
    }

    fs::write(&org_path, "* completely different content\n").expect("org file should rewrite");

    let connection = open_database(&db_path).expect("database should reopen");
    let before_counts = table_counts(&connection);

    let rows = execute_sqlite_query(
        &connection,
        &validated(r#"(headings (links-to (files (file-title "Beta Index" :exact t))))"#),
    )
    .expect("query should execute from stored DB rows");

    match rows {
        QueryRows::Headings(rows) => {
            assert_eq!(rows.len(), 3);
            assert!(matches!(rows[0], HeadingQueryMatch::File(_)));
            let heading_titles = rows
                .iter()
                .filter_map(|row| match row {
                    HeadingQueryMatch::Heading(row) => Some(row.title.as_str()),
                    HeadingQueryMatch::File(_) => None,
                })
                .collect::<Vec<_>>();
            assert_eq!(heading_titles, vec!["Query Engine", "Nested Task"]);
        }
        other => panic!("unexpected query rows: {other:?}"),
    }

    let after_counts = table_counts(&connection);
    assert_eq!(before_counts, after_counts);
}

fn heading_ids(rows: QueryRows) -> Vec<i64> {
    match rows {
        QueryRows::Headings(rows) => rows
            .into_iter()
            .filter_map(|row| match row {
                HeadingQueryMatch::Heading(row) => Some(row.id),
                HeadingQueryMatch::File(_) => None,
            })
            .collect(),
        other => panic!("expected heading rows, got {other:?}"),
    }
}

fn link_ids(rows: QueryRows) -> Vec<i64> {
    match rows {
        QueryRows::Links(rows) => rows.into_iter().map(|row| row.id).collect(),
        other => panic!("expected link rows, got {other:?}"),
    }
}

fn file_paths(rows: QueryRows) -> Vec<String> {
    match rows {
        QueryRows::Files(rows) => rows.into_iter().map(|row| row.path).collect(),
        other => panic!("expected file rows, got {other:?}"),
    }
}

fn heading_file_paths(rows: QueryRows) -> Vec<String> {
    match rows {
        QueryRows::Headings(rows) => rows
            .into_iter()
            .filter_map(|row| match row {
                HeadingQueryMatch::File(row) => Some(row.path),
                HeadingQueryMatch::Heading(_) => None,
            })
            .collect(),
        other => panic!("expected heading rows, got {other:?}"),
    }
}

fn property_rows(
    connection: &Connection,
    heading_id: i64,
    key: &str,
) -> Vec<(Option<String>, bool)> {
    let mut statement = connection
        .prepare(
            "SELECT value, append
             FROM properties
             WHERE heading_id = ?1 AND key = ?2 COLLATE NOCASE
             ORDER BY id",
        )
        .expect("property statement should prepare");
    let rows = statement
        .query_map(rusqlite::params![heading_id, key], |row| {
            Ok((row.get::<_, Option<String>>(0)?, row.get::<_, i64>(1)? != 0))
        })
        .expect("property rows should query");
    rows.collect::<Result<Vec<_>, _>>()
        .expect("property rows should collect")
}

fn seed_effective_tags(connection: &Connection, file_id: i64) {
    let parents = {
        let mut statement = connection
            .prepare("SELECT id, parent_id FROM headings WHERE file_id = ?1 ORDER BY id")
            .expect("seed heading query should prepare");
        statement
            .query_map([file_id], |row| {
                Ok((row.get::<_, i64>(0)?, row.get::<_, Option<i64>>(1)?))
            })
            .expect("seed heading query should run")
            .collect::<Result<std::collections::HashMap<_, _>, _>>()
            .expect("seed heading rows should decode")
    };
    let direct_tags = {
        let mut statement = connection
            .prepare(
                "SELECT tags.heading_id, tags.tag
                 FROM tags
                 INNER JOIN headings ON headings.id = tags.heading_id
                 WHERE headings.file_id = ?1
                 ORDER BY tags.heading_id, tags.tag",
            )
            .expect("seed tag query should prepare");
        let rows = statement
            .query_map([file_id], |row| {
                Ok((row.get::<_, i64>(0)?, row.get::<_, String>(1)?))
            })
            .expect("seed tag query should run")
            .collect::<Result<Vec<_>, _>>()
            .expect("seed tag rows should decode");
        let mut by_heading = std::collections::HashMap::<i64, Vec<String>>::new();
        for (heading_id, tag) in rows {
            by_heading.entry(heading_id).or_default().push(tag);
        }
        by_heading
    };
    let rows = derive_effective_tags(&parents, &direct_tags)
        .into_iter()
        .map(|row| EffectiveTagRecord {
            heading_id: row.heading_id,
            file_id,
            tag: row.tag,
            position: row.position,
        })
        .collect::<Vec<_>>();
    DbWriter::insert_effective_tags(connection, &rows)
        .expect("effective tags should seed through the production writer");
}

fn seed_effective_properties(connection: &Connection, file_id: i64) {
    let parents = {
        let mut statement = connection
            .prepare("SELECT id, parent_id FROM headings WHERE file_id = ?1 ORDER BY id")
            .expect("seed heading query should prepare");
        statement
            .query_map([file_id], |row| {
                Ok((row.get::<_, i64>(0)?, row.get::<_, Option<i64>>(1)?))
            })
            .expect("seed heading query should run")
            .collect::<Result<std::collections::HashMap<_, _>, _>>()
            .expect("seed heading rows should decode")
    };
    let rows_by_heading = {
        let mut statement = connection
            .prepare(
                "SELECT properties.id, properties.heading_id, properties.key, properties.value,
                        properties.append, properties.line_number, properties.source
                 FROM properties
                 INNER JOIN headings ON headings.id = properties.heading_id
                 WHERE headings.file_id = ?1
                 ORDER BY properties.line_number, properties.id",
            )
            .expect("seed property query should prepare");
        let rows = statement
            .query_map([file_id], |row| {
                Ok(PropertyRow {
                    id: row.get(0)?,
                    heading_id: row.get(1)?,
                    key: row.get(2)?,
                    value: row.get(3)?,
                    append: row.get::<_, i64>(4)? != 0,
                    line_number: row.get(5)?,
                    source: row.get(6)?,
                })
            })
            .expect("seed property query should run")
            .collect::<Result<Vec<_>, _>>()
            .expect("seed property rows should decode");
        let mut by_heading = std::collections::HashMap::<i64, Vec<PropertyRow>>::new();
        for row in rows {
            by_heading.entry(row.heading_id).or_default().push(row);
        }
        by_heading
    };
    let rows = derive_effective_properties(&parents, &rows_by_heading)
        .into_iter()
        .map(|row| EffectivePropertyRecord {
            heading_id: row.heading_id,
            file_id,
            key: row.key,
            local_value: row.local_value,
            effective_value: row.effective_value,
        })
        .collect::<Vec<_>>();
    DbWriter::insert_effective_properties(connection, &rows)
        .expect("effective properties should seed through the production writer");
}

fn seeded_connection() -> Connection {
    let schema = SchemaDefinition::new(3, false);
    let mut connection =
        open_in_memory_database_with_schema(&schema).expect("database should open");
    seed_database(
        &mut connection,
        Path::new("/tmp/query-alpha.org"),
        Path::new("/tmp/query-beta.org"),
    );
    connection
}

fn date_bound_test_connection() -> Connection {
    let schema = SchemaDefinition::new(3, false);
    let mut connection =
        open_in_memory_database_with_schema(&schema).expect("database should open");

    let file = FileRecordInput {
        path: Path::new("/tmp/date-bounds.org").to_path_buf(),
        identity: None,
        mtime_ns: naive_date_time_seconds(2026, 1, 3, 0, 0) * 1_000_000_000,
        size: 100,
        content_hash: None,
        indexed_at: Some(naive_date_time_seconds(2026, 1, 3, 0, 1)),
    };

    DbWriter::rebuild_file(&mut connection, &file, |tx, file_id| {
        let root_id = DbWriter::insert_level0_heading(
            tx,
            &HeadingRecord {
                id: Some(90),
                file_id,
                parent_id: None,
                level: 0,
                line_number: None,
                byte_start: -1,
                byte_end: 100,
                title: "Date Bounds".to_string(),
                title_raw: Some("Date Bounds".to_string()),
                todo_keyword: None,
                todo_type: None,
                priority: None,
                scheduled_raw: None,
                scheduled_ts: None,
                scheduled_has_time: None,
                deadline_raw: None,
                deadline_ts: None,
                deadline_has_time: None,
                closed_raw: None,
                closed_ts: None,
                closed_has_time: None,
                archivedp: false,
                footnote_section_p: false,
            },
        )?;

        DbWriter::insert_headings(
            tx,
            &[
                scheduled_heading(
                    file_id,
                    root_id,
                    100,
                    1,
                    "Start Of Day",
                    naive_date_time_seconds(2026, 1, 3, 0, 0),
                    "<2026-01-03 00:00>",
                ),
                scheduled_heading(
                    file_id,
                    root_id,
                    101,
                    2,
                    "Morning Task",
                    naive_date_time_seconds(2026, 1, 3, 9, 15),
                    "<2026-01-03 09:15>",
                ),
                scheduled_heading(
                    file_id,
                    root_id,
                    102,
                    3,
                    "Late Task",
                    naive_date_time_seconds(2026, 1, 3, 23, 59),
                    "<2026-01-03 23:59>",
                ),
                scheduled_heading(
                    file_id,
                    root_id,
                    103,
                    4,
                    "Dst Task",
                    naive_date_time_seconds(2026, 3, 29, 2, 30),
                    "<2026-03-29 02:30>",
                ),
                scheduled_heading(
                    file_id,
                    root_id,
                    106,
                    5,
                    "Month End Task",
                    naive_date_time_seconds(2026, 1, 31, 23, 59),
                    "<2026-01-31 23:59>",
                ),
                scheduled_heading(
                    file_id,
                    root_id,
                    107,
                    6,
                    "February Start Task",
                    naive_date_time_seconds(2026, 2, 1, 0, 0),
                    "<2026-02-01 00:00>",
                ),
                scheduled_heading(
                    file_id,
                    root_id,
                    104,
                    7,
                    "Year End Task",
                    naive_date_time_seconds(2026, 12, 31, 23, 59),
                    "<2026-12-31 23:59>",
                ),
                scheduled_heading(
                    file_id,
                    root_id,
                    105,
                    8,
                    "Next Year Task",
                    naive_date_time_seconds(2027, 1, 1, 0, 0),
                    "<2027-01-01 00:00>",
                ),
            ],
        )?;

        DbWriter::insert_outline_path(
            tx,
            &[
                outline_row(90, file_id, None, 0, "0000", "[\"Date Bounds\"]"),
                outline_row(
                    100,
                    file_id,
                    Some(90),
                    1,
                    "0000.0001",
                    "[\"Date Bounds\",\"Start Of Day\"]",
                ),
                outline_row(
                    101,
                    file_id,
                    Some(90),
                    1,
                    "0000.0002",
                    "[\"Date Bounds\",\"Morning Task\"]",
                ),
                outline_row(
                    102,
                    file_id,
                    Some(90),
                    1,
                    "0000.0003",
                    "[\"Date Bounds\",\"Late Task\"]",
                ),
                outline_row(
                    103,
                    file_id,
                    Some(90),
                    1,
                    "0000.0004",
                    "[\"Date Bounds\",\"Dst Task\"]",
                ),
                outline_row(
                    106,
                    file_id,
                    Some(90),
                    1,
                    "0000.0005",
                    "[\"Date Bounds\",\"Month End Task\"]",
                ),
                outline_row(
                    107,
                    file_id,
                    Some(90),
                    1,
                    "0000.0006",
                    "[\"Date Bounds\",\"February Start Task\"]",
                ),
                outline_row(
                    104,
                    file_id,
                    Some(90),
                    1,
                    "0000.0007",
                    "[\"Date Bounds\",\"Year End Task\"]",
                ),
                outline_row(
                    105,
                    file_id,
                    Some(90),
                    1,
                    "0000.0008",
                    "[\"Date Bounds\",\"Next Year Task\"]",
                ),
            ],
        )?;

        DbWriter::insert_timestamps(
            tx,
            &[
                scheduled_timestamp(100, 2026, 1, 3, 0, 0, "<2026-01-03 Sat 00:00>"),
                scheduled_timestamp(101, 2026, 1, 3, 9, 15, "<2026-01-03 Sat 09:15>"),
                scheduled_timestamp(102, 2026, 1, 3, 23, 59, "<2026-01-03 Sat 23:59>"),
                scheduled_timestamp(103, 2026, 3, 29, 2, 30, "<2026-03-29 Sun 02:30>"),
                scheduled_timestamp(106, 2026, 1, 31, 23, 59, "<2026-01-31 Sat 23:59>"),
                scheduled_timestamp(107, 2026, 2, 1, 0, 0, "<2026-02-01 Sun 00:00>"),
                scheduled_timestamp(104, 2026, 12, 31, 23, 59, "<2026-12-31 Thu 23:59>"),
                scheduled_timestamp(105, 2027, 1, 1, 0, 0, "<2027-01-01 Fri 00:00>"),
            ],
        )?;

        Ok(())
    })
    .expect("date bound fixture should seed");

    connection
}

fn temporal_test_connection() -> Connection {
    let mut connection = open_in_memory_database_with_schema(&SchemaDefinition::default())
        .expect("database should open");
    let file = FileRecordInput {
        path: Path::new("/tmp/temporal-predicates.org").to_path_buf(),
        identity: None,
        mtime_ns: naive_date_time_seconds(2026, 1, 3, 0, 0) * 1_000_000_000,
        size: 100,
        content_hash: None,
        indexed_at: Some(naive_date_time_seconds(2026, 1, 3, 0, 1)),
    };

    DbWriter::rebuild_file(&mut connection, &file, |tx, file_id| {
        let root_id =
            DbWriter::insert_level0_heading(tx, &base_heading(file_id, None, 200, 0, "With Time"))?;

        let headings = vec![
            planning_heading(
                file_id,
                root_id,
                201,
                1,
                "Scheduled Date Only",
                PlanningFixture {
                    kind: "scheduled",
                    timestamp: Some(naive_date_time_seconds(2026, 1, 3, 0, 0)),
                    has_time: Some(false),
                    raw_value: "<2026-01-03 Sat>",
                },
            ),
            planning_heading(
                file_id,
                root_id,
                202,
                2,
                "Scheduled Timed",
                PlanningFixture {
                    kind: "scheduled",
                    timestamp: Some(naive_date_time_seconds(2026, 1, 3, 9, 15)),
                    has_time: Some(true),
                    raw_value: "<2026-01-03 Sat 09:15>",
                },
            ),
            planning_heading(
                file_id,
                root_id,
                203,
                3,
                "Scheduled Midnight",
                PlanningFixture {
                    kind: "scheduled",
                    timestamp: Some(naive_date_time_seconds(2026, 1, 3, 0, 0)),
                    has_time: Some(true),
                    raw_value: "<2026-01-03 Sat 00:00>",
                },
            ),
            planning_heading(
                file_id,
                root_id,
                204,
                4,
                "Scheduled Unknown",
                PlanningFixture {
                    kind: "scheduled",
                    timestamp: Some(naive_date_time_seconds(2026, 1, 3, 12, 0)),
                    has_time: None,
                    raw_value: "<2026-01-03 Sat 12:00>",
                },
            ),
            planning_heading(
                file_id,
                root_id,
                205,
                5,
                "Deadline Date Only",
                PlanningFixture {
                    kind: "deadline",
                    timestamp: Some(naive_date_time_seconds(2026, 1, 3, 0, 0)),
                    has_time: Some(false),
                    raw_value: "<2026-01-03 Sat>",
                },
            ),
            planning_heading(
                file_id,
                root_id,
                206,
                6,
                "Deadline Timed",
                PlanningFixture {
                    kind: "deadline",
                    timestamp: Some(naive_date_time_seconds(2026, 1, 3, 10, 45)),
                    has_time: Some(true),
                    raw_value: "<2026-01-03 Sat 10:45>",
                },
            ),
            planning_heading(
                file_id,
                root_id,
                207,
                7,
                "Deadline Midnight",
                PlanningFixture {
                    kind: "deadline",
                    timestamp: Some(naive_date_time_seconds(2026, 1, 3, 0, 0)),
                    has_time: Some(true),
                    raw_value: "<2026-01-03 Sat 00:00>",
                },
            ),
            planning_heading(
                file_id,
                root_id,
                208,
                8,
                "Deadline Unknown",
                PlanningFixture {
                    kind: "deadline",
                    timestamp: Some(naive_date_time_seconds(2026, 1, 3, 18, 0)),
                    has_time: None,
                    raw_value: "<2026-01-03 Sat 18:00>",
                },
            ),
            planning_heading(
                file_id,
                root_id,
                209,
                9,
                "Closed Date Only",
                PlanningFixture {
                    kind: "closed",
                    timestamp: Some(naive_date_time_seconds(2026, 1, 3, 0, 0)),
                    has_time: Some(false),
                    raw_value: "[2026-01-03 Sat]",
                },
            ),
            planning_heading(
                file_id,
                root_id,
                210,
                10,
                "Closed Timed",
                PlanningFixture {
                    kind: "closed",
                    timestamp: Some(naive_date_time_seconds(2026, 1, 3, 11, 30)),
                    has_time: Some(true),
                    raw_value: "[2026-01-03 Sat 11:30]",
                },
            ),
            planning_heading(
                file_id,
                root_id,
                211,
                11,
                "Closed Midnight",
                PlanningFixture {
                    kind: "closed",
                    timestamp: Some(naive_date_time_seconds(2026, 1, 3, 0, 0)),
                    has_time: Some(true),
                    raw_value: "[2026-01-03 Sat 00:00]",
                },
            ),
            planning_heading(
                file_id,
                root_id,
                212,
                12,
                "Closed Unknown",
                PlanningFixture {
                    kind: "closed",
                    timestamp: Some(naive_date_time_seconds(2026, 1, 3, 16, 0)),
                    has_time: None,
                    raw_value: "[2026-01-03 Sat 16:00]",
                },
            ),
            base_heading(file_id, Some(root_id), 213, 13, "Active Date Only"),
            base_heading(file_id, Some(root_id), 214, 14, "Active Timed"),
            base_heading(file_id, Some(root_id), 215, 15, "Active Midnight"),
            base_heading(file_id, Some(root_id), 216, 16, "Active Unknown"),
            base_heading(file_id, Some(root_id), 217, 17, "Inactive Date Only"),
            base_heading(file_id, Some(root_id), 218, 18, "Inactive Timed"),
            base_heading(file_id, Some(root_id), 219, 19, "Inactive Midnight"),
            base_heading(file_id, Some(root_id), 220, 20, "Inactive Unknown"),
        ];
        DbWriter::insert_headings(tx, &headings)?;

        let outline_rows = (1_i64..=20_i64)
            .map(|line_number| {
                let heading_id = 200 + line_number;
                outline_row(
                    heading_id,
                    file_id,
                    Some(root_id),
                    1,
                    &format!("0000.{line_number:04}"),
                    &format!("[\"With Time\",\"{}\"]", heading_title(heading_id)),
                )
            })
            .collect::<Vec<_>>();
        let mut outline_rows_with_root = vec![outline_row(
            200,
            file_id,
            None,
            0,
            "0000",
            "[\"With Time\"]",
        )];
        outline_rows_with_root.extend(outline_rows);
        DbWriter::insert_outline_path(tx, &outline_rows_with_root)?;

        DbWriter::insert_timestamps(
            tx,
            &[
                generic_timestamp(
                    213,
                    Some(false),
                    naive_date_time_seconds(2026, 1, 3, 0, 0),
                    "active",
                    "<2026-01-03 Sat>",
                ),
                generic_timestamp(
                    214,
                    Some(true),
                    naive_date_time_seconds(2026, 1, 3, 9, 15),
                    "active",
                    "<2026-01-03 Sat 09:15>",
                ),
                generic_timestamp(
                    215,
                    Some(true),
                    naive_date_time_seconds(2026, 1, 3, 0, 0),
                    "active",
                    "<2026-01-03 Sat 00:00>",
                ),
                generic_timestamp(
                    216,
                    None,
                    naive_date_time_seconds(2026, 1, 3, 12, 0),
                    "active",
                    "<2026-01-03 Sat 12:00>",
                ),
                generic_timestamp(
                    217,
                    Some(false),
                    naive_date_time_seconds(2026, 1, 3, 0, 0),
                    "inactive",
                    "[2026-01-03 Sat]",
                ),
                generic_timestamp(
                    218,
                    Some(true),
                    naive_date_time_seconds(2026, 1, 3, 13, 45),
                    "inactive",
                    "[2026-01-03 Sat 13:45]",
                ),
                generic_timestamp(
                    219,
                    Some(true),
                    naive_date_time_seconds(2026, 1, 3, 0, 0),
                    "inactive",
                    "[2026-01-03 Sat 00:00]",
                ),
                generic_timestamp(
                    220,
                    None,
                    naive_date_time_seconds(2026, 1, 3, 17, 0),
                    "inactive",
                    "[2026-01-03 Sat 17:00]",
                ),
            ],
        )?;

        Ok(())
    })
    .expect("temporal predicate fixture should seed");

    connection
}

fn reduced_body_text_capability_connection(
    include_metadata_row: bool,
    metadata_value: bool,
) -> Connection {
    let connection = Connection::open_in_memory().expect("reduced schema should open");
    connection
        .execute_batch(
            r#"
CREATE TABLE db_metadata (
key     TEXT PRIMARY KEY,
value   TEXT NOT NULL
);
"#,
        )
        .expect("reduced body-text schema should initialize");

    if include_metadata_row {
        connection
            .execute(
                "INSERT INTO db_metadata (key, value) VALUES (?1, ?2)",
                rusqlite::params![
                    crate::db::DB_METADATA_BODY_TEXT_AVAILABLE_KEY,
                    if metadata_value { "1" } else { "0" }
                ],
            )
            .expect("body-text metadata should insert");
    }

    connection
}

fn base_heading(
    file_id: i64,
    parent_id: Option<i64>,
    id: i64,
    line_number: i64,
    title: &str,
) -> HeadingRecord {
    HeadingRecord {
        id: Some(id),
        file_id,
        parent_id,
        level: if parent_id.is_some() { 1 } else { 0 },
        line_number: if line_number > 0 {
            Some(line_number)
        } else {
            None
        },
        byte_start: if line_number > 0 {
            line_number * 10
        } else {
            -1
        },
        byte_end: if line_number > 0 {
            line_number * 10 + 5
        } else {
            100
        },
        title: title.to_string(),
        title_raw: Some(title.to_string()),
        todo_keyword: None,
        todo_type: None,
        priority: None,
        scheduled_raw: None,
        scheduled_ts: None,
        scheduled_has_time: None,
        deadline_raw: None,
        deadline_ts: None,
        deadline_has_time: None,
        closed_raw: None,
        closed_ts: None,
        closed_has_time: None,
        archivedp: false,
        footnote_section_p: false,
    }
}

fn planning_heading(
    file_id: i64,
    root_id: i64,
    id: i64,
    line_number: i64,
    title: &str,
    planning: PlanningFixture<'_>,
) -> HeadingRecord {
    let mut heading = base_heading(file_id, Some(root_id), id, line_number, title);
    match planning.kind {
        "scheduled" => {
            heading.scheduled_raw = Some(planning.raw_value.to_string());
            heading.scheduled_ts = planning.timestamp;
            heading.scheduled_has_time = planning.has_time;
        }
        "deadline" => {
            heading.deadline_raw = Some(planning.raw_value.to_string());
            heading.deadline_ts = planning.timestamp;
            heading.deadline_has_time = planning.has_time;
        }
        "closed" => {
            heading.closed_raw = Some(planning.raw_value.to_string());
            heading.closed_ts = planning.timestamp;
            heading.closed_has_time = planning.has_time;
        }
        _ => unreachable!("unexpected planning heading kind"),
    }
    heading
}

fn generic_timestamp(
    heading_id: i64,
    has_time: Option<bool>,
    start_ts: i64,
    timestamp_type: &str,
    raw_value: &str,
) -> TimestampRecord {
    TimestampRecord {
        heading_id,
        role: None,
        has_time,
        start_ts: Some(start_ts),
        end_ts: None,
        timestamp_type: Some(timestamp_type.to_string()),
        range_type: Some("none".to_string()),
        raw_value: raw_value.to_string(),
        byte_start: heading_id,
        byte_end: heading_id + 1,
        line_number: Some(heading_id - 200),
    }
}

fn heading_title(heading_id: i64) -> &'static str {
    match heading_id {
        201 => "Scheduled Date Only",
        202 => "Scheduled Timed",
        203 => "Scheduled Midnight",
        204 => "Scheduled Unknown",
        205 => "Deadline Date Only",
        206 => "Deadline Timed",
        207 => "Deadline Midnight",
        208 => "Deadline Unknown",
        209 => "Closed Date Only",
        210 => "Closed Timed",
        211 => "Closed Midnight",
        212 => "Closed Unknown",
        213 => "Active Date Only",
        214 => "Active Timed",
        215 => "Active Midnight",
        216 => "Active Unknown",
        217 => "Inactive Date Only",
        218 => "Inactive Timed",
        219 => "Inactive Midnight",
        220 => "Inactive Unknown",
        _ => unreachable!("unexpected temporal predicate fixture heading"),
    }
}

fn naive_date_time_seconds(year: i32, month: u32, day: u32, hour: u32, minute: u32) -> i64 {
    NaiveDate::from_ymd_opt(year, month, day)
        .expect("date should be valid")
        .and_hms_opt(hour, minute, 0)
        .expect("time should be valid")
        .and_utc()
        .timestamp()
}

fn scheduled_heading(
    file_id: i64,
    root_id: i64,
    id: i64,
    line_number: i64,
    title: &str,
    scheduled_ts: i64,
    scheduled_raw: &str,
) -> HeadingRecord {
    HeadingRecord {
        id: Some(id),
        file_id,
        parent_id: Some(root_id),
        level: 1,
        line_number: Some(line_number),
        byte_start: line_number * 10,
        byte_end: line_number * 10 + 5,
        title: title.to_string(),
        title_raw: Some(title.to_string()),
        todo_keyword: None,
        todo_type: None,
        priority: None,
        scheduled_raw: Some(scheduled_raw.to_string()),
        scheduled_ts: Some(scheduled_ts),
        scheduled_has_time: None,
        deadline_raw: None,
        deadline_ts: None,
        deadline_has_time: None,
        closed_raw: None,
        closed_ts: None,
        closed_has_time: None,
        archivedp: false,
        footnote_section_p: false,
    }
}

fn scheduled_timestamp(
    heading_id: i64,
    year: i32,
    month: u32,
    day: u32,
    hour: u32,
    minute: u32,
    raw_value: &str,
) -> TimestampRecord {
    TimestampRecord {
        heading_id,
        role: Some("scheduled".to_string()),
        has_time: None,
        start_ts: Some(naive_date_time_seconds(year, month, day, hour, minute)),
        end_ts: None,
        timestamp_type: Some("active".to_string()),
        range_type: Some("none".to_string()),
        raw_value: raw_value.to_string(),
        byte_start: heading_id,
        byte_end: heading_id + 1,
        line_number: Some(heading_id - 99),
    }
}

fn outline_row(
    heading_id: i64,
    file_id: i64,
    parent_id: Option<i64>,
    depth: i64,
    materialized_path: &str,
    breadcrumbs_json: &str,
) -> OutlinePathRecord {
    OutlinePathRecord {
        heading_id,
        file_id,
        parent_id,
        depth,
        materialized_path: materialized_path.to_string(),
        breadcrumbs_json: breadcrumbs_json.to_string(),
    }
}

fn seed_database(connection: &mut Connection, alpha_path: &Path, beta_path: &Path) {
    let alpha = FileRecordInput {
        path: alpha_path.to_path_buf(),
        identity: None,
        mtime_ns: 1_767_398_400_000_000_000,
        size: 100,
        content_hash: None,
        indexed_at: Some(1_767_398_410),
    };
    let beta = FileRecordInput {
        path: beta_path.to_path_buf(),
        identity: None,
        mtime_ns: 1_767_484_800_000_000_000,
        size: 120,
        content_hash: None,
        indexed_at: Some(1_767_484_810),
    };
    let gamma = FileRecordInput {
        path: Path::new("/tmp/query-gamma.org").to_path_buf(),
        identity: None,
        mtime_ns: 1_767_571_200_000_000_000,
        size: 80,
        content_hash: None,
        indexed_at: Some(1_767_571_210),
    };

    let (beta_file_id, ()) = DbWriter::rebuild_file(connection, &beta, |tx, file_id| {
        let root_id = DbWriter::insert_level0_heading(
            tx,
            &HeadingRecord {
                id: Some(20),
                file_id,
                parent_id: None,
                level: 0,
                line_number: None,
                byte_start: -1,
                byte_end: 120,
                title: "Beta Index".to_string(),
                title_raw: Some("Beta Index".to_string()),
                todo_keyword: None,
                todo_type: None,
                priority: None,
                scheduled_raw: None,
                scheduled_ts: None,
                scheduled_has_time: None,
                deadline_raw: None,
                deadline_ts: None,
                deadline_has_time: None,
                closed_raw: None,
                closed_ts: None,
                closed_has_time: None,
                archivedp: false,
                footnote_section_p: false,
            },
        )?;
        DbWriter::insert_outline_path(
            tx,
            &[OutlinePathRecord {
                heading_id: root_id,
                file_id,
                parent_id: None,
                depth: 0,
                materialized_path: "0000".to_string(),
                breadcrumbs_json: "[\"Beta Index\"]".to_string(),
            }],
        )?;
        DbWriter::insert_tags(
            tx,
            &[TagRecord {
                heading_id: root_id,
                tag: "archive".to_string(),
            }],
        )?;
        DbWriter::insert_keywords(
            tx,
            &[KeywordRecord {
                heading_id: root_id,
                keyword: "AUTHOR".to_string(),
                value: Some("Bob".to_string()),
                line_number: Some(1),
            }],
        )?;
        Ok(())
    })
    .expect("beta file should seed");

    let (alpha_file_id, ()) = DbWriter::rebuild_file(connection, &alpha, |tx, file_id| {
        let root_id = DbWriter::insert_level0_heading(
            tx,
            &HeadingRecord {
                id: Some(10),
                file_id,
                parent_id: None,
                level: 0,
                line_number: None,
                byte_start: -1,
                byte_end: 100,
                title: "Alpha Index".to_string(),
                title_raw: Some("Alpha Index".to_string()),
                todo_keyword: None,
                todo_type: None,
                priority: None,
                scheduled_raw: None,
                scheduled_ts: None,
                scheduled_has_time: None,
                deadline_raw: None,
                deadline_ts: None,
                deadline_has_time: None,
                closed_raw: None,
                closed_ts: None,
                closed_has_time: None,
                archivedp: false,
                footnote_section_p: false,
            },
        )?;
        DbWriter::insert_headings(
            tx,
            &[
                HeadingRecord {
                    id: Some(11),
                    file_id,
                    parent_id: Some(root_id),
                    level: 1,
                    line_number: Some(3),
                    byte_start: 10,
                    byte_end: 40,
                    title: "Query Engine".to_string(),
                    title_raw: Some("Query Engine".to_string()),
                    todo_keyword: Some("NEXT".to_string()),
                    todo_type: Some("open".to_string()),
                    priority: Some("A".to_string()),
                    scheduled_raw: Some("<2026-01-03 Fri>".to_string()),
                    scheduled_ts: Some(1_767_398_400),
                    scheduled_has_time: None,
                    deadline_raw: None,
                    deadline_ts: None,
                    deadline_has_time: None,
                    closed_raw: None,
                    closed_ts: None,
                    closed_has_time: None,
                    archivedp: false,
                    footnote_section_p: false,
                },
                HeadingRecord {
                    id: Some(12),
                    file_id,
                    parent_id: Some(11),
                    level: 2,
                    line_number: Some(6),
                    byte_start: 41,
                    byte_end: 70,
                    title: "Nested Task".to_string(),
                    title_raw: Some("Nested Task".to_string()),
                    todo_keyword: None,
                    todo_type: None,
                    priority: None,
                    scheduled_raw: None,
                    scheduled_ts: None,
                    scheduled_has_time: None,
                    deadline_raw: None,
                    deadline_ts: None,
                    deadline_has_time: None,
                    closed_raw: None,
                    closed_ts: None,
                    closed_has_time: None,
                    archivedp: false,
                    footnote_section_p: false,
                },
                HeadingRecord {
                    id: Some(13),
                    file_id,
                    parent_id: Some(root_id),
                    level: 1,
                    line_number: Some(8),
                    byte_start: 71,
                    byte_end: 95,
                    title: "Loose Note".to_string(),
                    title_raw: Some("Loose Note".to_string()),
                    todo_keyword: None,
                    todo_type: None,
                    priority: None,
                    scheduled_raw: None,
                    scheduled_ts: None,
                    scheduled_has_time: None,
                    deadline_raw: None,
                    deadline_ts: None,
                    deadline_has_time: None,
                    closed_raw: None,
                    closed_ts: None,
                    closed_has_time: None,
                    archivedp: false,
                    footnote_section_p: false,
                },
                HeadingRecord {
                    id: Some(14),
                    file_id,
                    parent_id: Some(11),
                    level: 2,
                    line_number: Some(10),
                    byte_start: 96,
                    byte_end: 130,
                    title: "Statistic Cookies".to_string(),
                    title_raw: Some("REVIEW [#B] Statistic Cookies [0/1]".to_string()),
                    todo_keyword: Some("REVIEW".to_string()),
                    todo_type: Some("open".to_string()),
                    priority: Some("B".to_string()),
                    scheduled_raw: None,
                    scheduled_ts: None,
                    scheduled_has_time: None,
                    deadline_raw: None,
                    deadline_ts: None,
                    deadline_has_time: None,
                    closed_raw: None,
                    closed_ts: None,
                    closed_has_time: None,
                    archivedp: false,
                    footnote_section_p: false,
                },
                HeadingRecord {
                    id: Some(15),
                    file_id,
                    parent_id: Some(11),
                    level: 2,
                    line_number: Some(12),
                    byte_start: 131,
                    byte_end: 160,
                    title: "Overriding Child".to_string(),
                    title_raw: Some("Overriding Child".to_string()),
                    todo_keyword: None,
                    todo_type: None,
                    priority: None,
                    scheduled_raw: None,
                    scheduled_ts: None,
                    scheduled_has_time: None,
                    deadline_raw: None,
                    deadline_ts: None,
                    deadline_has_time: None,
                    closed_raw: None,
                    closed_ts: None,
                    closed_has_time: None,
                    archivedp: false,
                    footnote_section_p: false,
                },
            ],
        )?;
        DbWriter::insert_outline_path(
            tx,
            &[
                OutlinePathRecord {
                    heading_id: 10,
                    file_id,
                    parent_id: None,
                    depth: 0,
                    materialized_path: "0000".to_string(),
                    breadcrumbs_json: "[\"Alpha Index\"]".to_string(),
                },
                OutlinePathRecord {
                    heading_id: 11,
                    file_id,
                    parent_id: Some(10),
                    depth: 1,
                    materialized_path: "0000.0001".to_string(),
                    breadcrumbs_json: "[\"Alpha Index\",\"Query Engine\"]".to_string(),
                },
                OutlinePathRecord {
                    heading_id: 12,
                    file_id,
                    parent_id: Some(11),
                    depth: 2,
                    materialized_path: "0000.0001.0001".to_string(),
                    breadcrumbs_json: "[\"Alpha Index\",\"Query Engine\",\"Nested Task\"]"
                        .to_string(),
                },
                OutlinePathRecord {
                    heading_id: 13,
                    file_id,
                    parent_id: Some(10),
                    depth: 1,
                    materialized_path: "0000.0002".to_string(),
                    breadcrumbs_json: "[\"Alpha Index\",\"Loose Note\"]".to_string(),
                },
                OutlinePathRecord {
                    heading_id: 14,
                    file_id,
                    parent_id: Some(11),
                    depth: 2,
                    materialized_path: "0000.0001.0002".to_string(),
                    breadcrumbs_json: "[\"Alpha Index\",\"Query Engine\",\"Statistic Cookies\"]"
                        .to_string(),
                },
                OutlinePathRecord {
                    heading_id: 15,
                    file_id,
                    parent_id: Some(11),
                    depth: 2,
                    materialized_path: "0000.0001.0003".to_string(),
                    breadcrumbs_json: "[\"Alpha Index\",\"Query Engine\",\"Overriding Child\"]"
                        .to_string(),
                },
            ],
        )?;
        DbWriter::insert_tags(
            tx,
            &[
                TagRecord {
                    heading_id: 10,
                    tag: "filetag".to_string(),
                },
                TagRecord {
                    heading_id: 11,
                    tag: "project".to_string(),
                },
                TagRecord {
                    heading_id: 12,
                    tag: "urgent".to_string(),
                },
                TagRecord {
                    heading_id: 13,
                    tag: "misc".to_string(),
                },
                TagRecord {
                    heading_id: 14,
                    tag: "filetag".to_string(),
                },
            ],
        )?;
        seed_effective_tags(tx, file_id);
        DbWriter::insert_keywords(
            tx,
            &[KeywordRecord {
                heading_id: 10,
                keyword: "AUTHOR".to_string(),
                value: Some("Alice".to_string()),
                line_number: Some(1),
            }],
        )?;
        DbWriter::insert_properties(
            tx,
            &[
                PropertyRecord {
                    heading_id: 10,
                    key: "CATEGORY".to_string(),
                    value: Some("work".to_string()),
                    source: "category_keyword".to_string(),
                    append: false,
                    line_number: Some(2),
                },
                PropertyRecord {
                    heading_id: 10,
                    key: "OWNER".to_string(),
                    value: Some("Alice".to_string()),
                    source: "property_keyword".to_string(),
                    append: false,
                    line_number: Some(2),
                },
                PropertyRecord {
                    heading_id: 10,
                    key: "KEYWORD_APPEND".to_string(),
                    value: Some("foo=1".to_string()),
                    source: "property_keyword".to_string(),
                    append: false,
                    line_number: Some(2),
                },
                PropertyRecord {
                    heading_id: 10,
                    key: "KEYWORD_APPEND".to_string(),
                    value: Some("bar=2".to_string()),
                    source: "property_keyword".to_string(),
                    append: true,
                    line_number: Some(3),
                },
                PropertyRecord {
                    heading_id: 10,
                    key: "KEYWORD_OVERWRITTEN_BY_SECOND".to_string(),
                    value: Some("invalid".to_string()),
                    source: "property_keyword".to_string(),
                    append: false,
                    line_number: Some(4),
                },
                PropertyRecord {
                    heading_id: 10,
                    key: "KEYWORD_OVERWRITTEN_BY_SECOND".to_string(),
                    value: Some("valid".to_string()),
                    source: "property_keyword".to_string(),
                    append: false,
                    line_number: Some(5),
                },
                PropertyRecord {
                    heading_id: 10,
                    key: "ROOT_ONLY".to_string(),
                    value: Some("root".to_string()),
                    source: "property_drawer".to_string(),
                    append: false,
                    line_number: Some(6),
                },
                PropertyRecord {
                    heading_id: 10,
                    key: "OVERRIDE_CHAIN".to_string(),
                    value: Some("root".to_string()),
                    source: "property_drawer".to_string(),
                    append: false,
                    line_number: Some(7),
                },
                PropertyRecord {
                    heading_id: 11,
                    key: "AREA".to_string(),
                    value: Some("infra".to_string()),
                    source: "property_drawer".to_string(),
                    append: false,
                    line_number: Some(4),
                },
                PropertyRecord {
                    heading_id: 11,
                    key: "LANG".to_string(),
                    value: Some("rust".to_string()),
                    source: "property_drawer".to_string(),
                    append: false,
                    line_number: Some(5),
                },
                PropertyRecord {
                    heading_id: 11,
                    key: "LANG".to_string(),
                    value: Some("emacs".to_string()),
                    source: "property_drawer".to_string(),
                    append: true,
                    line_number: Some(6),
                },
                PropertyRecord {
                    heading_id: 11,
                    key: "OVERRIDE_CHAIN".to_string(),
                    value: Some("parent".to_string()),
                    source: "property_drawer".to_string(),
                    append: false,
                    line_number: Some(7),
                },
                PropertyRecord {
                    heading_id: 11,
                    key: "APPEND_INHERITED".to_string(),
                    value: Some("parent".to_string()),
                    source: "property_drawer".to_string(),
                    append: false,
                    line_number: Some(8),
                },
                PropertyRecord {
                    heading_id: 11,
                    key: "APPEND_BEFORE".to_string(),
                    value: Some("appending before".to_string()),
                    source: "property_drawer".to_string(),
                    append: true,
                    line_number: Some(9),
                },
                PropertyRecord {
                    heading_id: 11,
                    key: "APPEND_BEFORE".to_string(),
                    value: Some("definition".to_string()),
                    source: "property_drawer".to_string(),
                    append: false,
                    line_number: Some(10),
                },
                PropertyRecord {
                    heading_id: 11,
                    key: "LOCAL_BASE_APPEND".to_string(),
                    value: Some("parent".to_string()),
                    source: "property_drawer".to_string(),
                    append: false,
                    line_number: Some(11),
                },
                PropertyRecord {
                    heading_id: 12,
                    key: "OWNER".to_string(),
                    value: Some("Bob".to_string()),
                    source: "property_drawer".to_string(),
                    append: false,
                    line_number: Some(9),
                },
                PropertyRecord {
                    heading_id: 12,
                    key: "APPEND_INHERITED".to_string(),
                    value: Some("child".to_string()),
                    source: "property_drawer".to_string(),
                    append: true,
                    line_number: Some(10),
                },
                PropertyRecord {
                    heading_id: 12,
                    key: "LOCAL_BASE_APPEND".to_string(),
                    value: Some("before".to_string()),
                    source: "property_drawer".to_string(),
                    append: true,
                    line_number: Some(11),
                },
                PropertyRecord {
                    heading_id: 12,
                    key: "LOCAL_BASE_APPEND".to_string(),
                    value: Some("child".to_string()),
                    source: "property_drawer".to_string(),
                    append: false,
                    line_number: Some(12),
                },
                PropertyRecord {
                    heading_id: 12,
                    key: "LOCAL_BASE_APPEND".to_string(),
                    value: Some("after".to_string()),
                    source: "property_drawer".to_string(),
                    append: true,
                    line_number: Some(13),
                },
                PropertyRecord {
                    heading_id: 13,
                    key: "DEFINED_TWICE".to_string(),
                    value: Some("works".to_string()),
                    source: "property_drawer".to_string(),
                    append: false,
                    line_number: Some(11),
                },
                PropertyRecord {
                    heading_id: 13,
                    key: "DEFINED_TWICE".to_string(),
                    value: Some("second is effective".to_string()),
                    source: "property_drawer".to_string(),
                    append: false,
                    line_number: Some(12),
                },
                PropertyRecord {
                    heading_id: 13,
                    key: "APPEND_REPLACED".to_string(),
                    value: Some("first".to_string()),
                    source: "property_drawer".to_string(),
                    append: false,
                    line_number: Some(13),
                },
                PropertyRecord {
                    heading_id: 13,
                    key: "APPEND_REPLACED".to_string(),
                    value: Some("appended".to_string()),
                    source: "property_drawer".to_string(),
                    append: true,
                    line_number: Some(14),
                },
                PropertyRecord {
                    heading_id: 13,
                    key: "APPEND_REPLACED".to_string(),
                    value: Some("second".to_string()),
                    source: "property_drawer".to_string(),
                    append: false,
                    line_number: Some(15),
                },
                PropertyRecord {
                    heading_id: 14,
                    key: "ADD-VALUE".to_string(),
                    value: Some("is".to_string()),
                    source: "property_drawer".to_string(),
                    append: false,
                    line_number: Some(13),
                },
                PropertyRecord {
                    heading_id: 14,
                    key: "ADD-VALUE".to_string(),
                    value: Some("valid".to_string()),
                    source: "property_drawer".to_string(),
                    append: true,
                    line_number: Some(14),
                },
                PropertyRecord {
                    heading_id: 14,
                    key: "MULTI_APPEND".to_string(),
                    value: Some("before".to_string()),
                    source: "property_drawer".to_string(),
                    append: true,
                    line_number: Some(15),
                },
                PropertyRecord {
                    heading_id: 14,
                    key: "MULTI_APPEND".to_string(),
                    value: Some("first".to_string()),
                    source: "property_drawer".to_string(),
                    append: false,
                    line_number: Some(16),
                },
                PropertyRecord {
                    heading_id: 14,
                    key: "MULTI_APPEND".to_string(),
                    value: Some("middle".to_string()),
                    source: "property_drawer".to_string(),
                    append: true,
                    line_number: Some(17),
                },
                PropertyRecord {
                    heading_id: 14,
                    key: "MULTI_APPEND".to_string(),
                    value: Some("second".to_string()),
                    source: "property_drawer".to_string(),
                    append: false,
                    line_number: Some(18),
                },
                PropertyRecord {
                    heading_id: 14,
                    key: "MULTI_APPEND".to_string(),
                    value: Some("after".to_string()),
                    source: "property_drawer".to_string(),
                    append: true,
                    line_number: Some(19),
                },
                PropertyRecord {
                    heading_id: 15,
                    key: "OVERRIDE_CHAIN".to_string(),
                    value: Some("child".to_string()),
                    source: "property_drawer".to_string(),
                    append: false,
                    line_number: Some(15),
                },
            ],
        )?;
        DbWriter::insert_timestamps(
            tx,
            &[TimestampRecord {
                heading_id: 11,
                role: Some("scheduled".to_string()),
                has_time: None,
                start_ts: Some(1_767_398_400),
                end_ts: None,
                timestamp_type: Some("active".to_string()),
                range_type: Some("none".to_string()),
                raw_value: "<2026-01-03 Fri>".to_string(),
                byte_start: 12,
                byte_end: 28,
                line_number: Some(3),
            }],
        )?;
        DbWriter::insert_links(
            tx,
            &[LinkRecord {
                id: Some(100),
                file_id,
                heading_id: 11,
                byte_start: 50,
                byte_end: 80,
                line: 4,
                source_context: "normal".to_string(),
                format: "bracket".to_string(),
                raw: "[[file:beta.org][Beta notes]]".to_string(),
                raw_target: "file:beta.org".to_string(),
                raw_description: Some("Beta notes".to_string()),
                link_type: "file".to_string(),
                path: "beta.org".to_string(),
                search_option: None,
            }],
        )?;
        tx.execute(
            "UPDATE links
             SET path_absolute = ?1,
                 target_file_id = ?2,
                 target_heading_id = ?3,
                 resolution_status = 'resolved'
             WHERE id = 100",
            rusqlite::params![beta_path.to_string_lossy().to_string(), beta_file_id, 20],
        )
        .expect("link target should update");
        Ok(())
    })
    .expect("alpha file should seed");
    seed_effective_properties(connection, alpha_file_id);

    let (gamma_file_id, ()) = DbWriter::rebuild_file(connection, &gamma, |tx, file_id| {
        let root_id = DbWriter::insert_level0_heading(
            tx,
            &HeadingRecord {
                id: Some(30),
                file_id,
                parent_id: None,
                level: 0,
                line_number: None,
                byte_start: -1,
                byte_end: 80,
                title: "Gamma Index".to_string(),
                title_raw: Some("Gamma Index".to_string()),
                todo_keyword: None,
                todo_type: None,
                priority: None,
                scheduled_raw: None,
                scheduled_ts: None,
                scheduled_has_time: None,
                deadline_raw: None,
                deadline_ts: None,
                deadline_has_time: None,
                closed_raw: None,
                closed_ts: None,
                closed_has_time: None,
                archivedp: false,
                footnote_section_p: false,
            },
        )?;
        DbWriter::insert_headings(
            tx,
            &[HeadingRecord {
                id: Some(31),
                file_id,
                parent_id: Some(root_id),
                level: 1,
                line_number: Some(2),
                byte_start: 10,
                byte_end: 28,
                title: "Gamma Candidate".to_string(),
                title_raw: Some("Gamma Candidate".to_string()),
                todo_keyword: None,
                todo_type: None,
                priority: None,
                scheduled_raw: None,
                scheduled_ts: None,
                scheduled_has_time: None,
                deadline_raw: None,
                deadline_ts: None,
                deadline_has_time: None,
                closed_raw: None,
                closed_ts: None,
                closed_has_time: None,
                archivedp: false,
                footnote_section_p: false,
            }],
        )?;
        DbWriter::insert_outline_path(
            tx,
            &[
                OutlinePathRecord {
                    heading_id: root_id,
                    file_id,
                    parent_id: None,
                    depth: 0,
                    materialized_path: "0000".to_string(),
                    breadcrumbs_json: "[\"Gamma Index\"]".to_string(),
                },
                OutlinePathRecord {
                    heading_id: 31,
                    file_id,
                    parent_id: Some(root_id),
                    depth: 1,
                    materialized_path: "0000.0001".to_string(),
                    breadcrumbs_json: "[\"Gamma Index\",\"Gamma Candidate\"]".to_string(),
                },
            ],
        )?;
        Ok(())
    })
    .expect("gamma file should seed");

    connection
        .execute(
            "INSERT INTO headings
             (id, file_id, parent_id, level, line_number, byte_start, byte_end, title, title_raw,
              todo_keyword, todo_type, priority, scheduled_raw, scheduled_ts, deadline_raw,
              deadline_ts, closed_raw, closed_ts, archivedp, footnote_section_p)
             VALUES
             (21, ?1, 20, 1, 3, 10, 40, 'Beta Target', 'Beta Target',
              NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, 0, 0)",
            rusqlite::params![beta_file_id],
        )
        .expect("beta child heading should insert");
    DbWriter::insert_outline_path(
        connection,
        &[OutlinePathRecord {
            heading_id: 21,
            file_id: beta_file_id,
            parent_id: Some(20),
            depth: 1,
            materialized_path: "0000.0001".to_string(),
            breadcrumbs_json: "[\"Beta Index\",\"Beta Target\"]".to_string(),
        }],
    )
    .expect("beta child outline should insert");
    DbWriter::insert_tags(
        connection,
        &[TagRecord {
            heading_id: 21,
            tag: "target".to_string(),
        }],
    )
    .expect("beta child tag should insert");
    seed_effective_tags(connection, beta_file_id);

    DbWriter::set_metadata_flag(
        connection,
        crate::db::DB_METADATA_BODY_TEXT_AVAILABLE_KEY,
        true,
    )
    .expect("body-text capability should persist");
    DbWriter::insert_heading_bodies(
        connection,
        &[
            HeadingBodyRecord {
                heading_id: 11,
                body_text: "SQLite index notes with FTS fallback guidance.".to_string(),
                body_byte_start: Some(16),
                body_byte_end: Some(62),
            },
            HeadingBodyRecord {
                heading_id: 12,
                body_text: "Nested sqlite implementation checklist.".to_string(),
                body_byte_start: Some(63),
                body_byte_end: Some(101),
            },
            HeadingBodyRecord {
                heading_id: 13,
                body_text: "General notes without the keyword.".to_string(),
                body_byte_start: Some(102),
                body_byte_end: Some(136),
            },
            HeadingBodyRecord {
                heading_id: 14,
                body_text: String::new(),
                body_byte_start: Some(137),
                body_byte_end: Some(137),
            },
            HeadingBodyRecord {
                heading_id: 21,
                body_text: "BETA target body mentions sqlite and fts together.".to_string(),
                body_byte_start: Some(12),
                body_byte_end: Some(60),
            },
        ],
    )
    .expect("heading body rows should insert");

    DbWriter::insert_links(
        connection,
        &[
            LinkRecord {
                id: Some(101),
                file_id: alpha_file_id,
                heading_id: 12,
                byte_start: 60,
                byte_end: 95,
                line: 6,
                source_context: "normal".to_string(),
                format: "bracket".to_string(),
                raw: "[[file:beta.org::*Beta Target][Beta heading]]".to_string(),
                raw_target: "file:beta.org::*Beta Target".to_string(),
                raw_description: Some("Beta heading".to_string()),
                link_type: "file".to_string(),
                path: "beta.org".to_string(),
                search_option: Some("*Beta Target".to_string()),
            },
            LinkRecord {
                id: Some(102),
                file_id: alpha_file_id,
                heading_id: 10,
                byte_start: 0,
                byte_end: 24,
                line: 1,
                source_context: "normal".to_string(),
                format: "bracket".to_string(),
                raw: "[[file:beta.org][Preamble]]".to_string(),
                raw_target: "file:beta.org".to_string(),
                raw_description: Some("Preamble".to_string()),
                link_type: "file".to_string(),
                path: "beta.org".to_string(),
                search_option: None,
            },
            LinkRecord {
                id: Some(103),
                file_id: beta_file_id,
                heading_id: 21,
                byte_start: 50,
                byte_end: 84,
                line: 4,
                source_context: "normal".to_string(),
                format: "bracket".to_string(),
                raw: "[[file:alpha.org::*Query Engine][Backlink]]".to_string(),
                raw_target: "file:alpha.org::*Query Engine".to_string(),
                raw_description: Some("Backlink".to_string()),
                link_type: "file".to_string(),
                path: "alpha.org".to_string(),
                search_option: Some("*Query Engine".to_string()),
            },
            LinkRecord {
                id: Some(104),
                file_id: beta_file_id,
                heading_id: 20,
                byte_start: 0,
                byte_end: 22,
                line: 1,
                source_context: "normal".to_string(),
                format: "bracket".to_string(),
                raw: "[[file:alpha.org][Root]]".to_string(),
                raw_target: "file:alpha.org".to_string(),
                raw_description: Some("Root".to_string()),
                link_type: "file".to_string(),
                path: "alpha.org".to_string(),
                search_option: None,
            },
            LinkRecord {
                id: Some(105),
                file_id: alpha_file_id,
                heading_id: 13,
                byte_start: 80,
                byte_end: 104,
                line: 8,
                source_context: "normal".to_string(),
                format: "bracket".to_string(),
                raw: "[[file:missing.org]]".to_string(),
                raw_target: "file:missing.org".to_string(),
                raw_description: None,
                link_type: "file".to_string(),
                path: "missing.org".to_string(),
                search_option: None,
            },
            LinkRecord {
                id: Some(106),
                file_id: beta_file_id,
                heading_id: 21,
                byte_start: 85,
                byte_end: 100,
                line: 5,
                source_context: "normal".to_string(),
                format: "bracket".to_string(),
                raw: "[[id:missing-id]]".to_string(),
                raw_target: "id:missing-id".to_string(),
                raw_description: None,
                link_type: "id".to_string(),
                path: "missing-id".to_string(),
                search_option: None,
            },
            LinkRecord {
                id: Some(107),
                file_id: alpha_file_id,
                heading_id: 11,
                byte_start: 81,
                byte_end: 92,
                line: 5,
                source_context: "normal".to_string(),
                format: "bracket".to_string(),
                raw: "[[id:dup-id]]".to_string(),
                raw_target: "id:dup-id".to_string(),
                raw_description: None,
                link_type: "id".to_string(),
                path: "dup-id".to_string(),
                search_option: None,
            },
        ],
    )
    .expect("relation links should seed");

    connection
        .execute(
            "UPDATE links
             SET path_absolute = ?1,
                 target_file_id = ?2,
                 target_heading_id = ?3,
                 resolution_status = 'resolved'
             WHERE id = 101",
            rusqlite::params![beta_path.to_string_lossy().to_string(), beta_file_id, 21],
        )
        .expect("heading target should update");
    connection
        .execute(
            "UPDATE links
             SET path_absolute = ?1,
                 target_file_id = ?2,
                 target_heading_id = ?3,
                 resolution_status = 'resolved'
             WHERE id = 102",
            rusqlite::params![beta_path.to_string_lossy().to_string(), beta_file_id, 20],
        )
        .expect("preamble file target should update");
    connection
        .execute(
            "UPDATE links
             SET path_absolute = ?1,
                 target_file_id = ?2,
                 target_heading_id = ?3,
                 resolution_status = 'resolved'
             WHERE id = 103",
            rusqlite::params![alpha_path.to_string_lossy().to_string(), alpha_file_id, 11],
        )
        .expect("backlink heading target should update");
    connection
        .execute(
            "UPDATE links
             SET path_absolute = ?1,
                 target_file_id = ?2,
                 target_heading_id = ?3,
                 resolution_status = 'resolved'
             WHERE id = 104",
            rusqlite::params![alpha_path.to_string_lossy().to_string(), alpha_file_id, 10],
        )
        .expect("backlink root target should update");
    connection
        .execute(
            "UPDATE links
             SET resolution_status = 'broken',
                 resolution_diagnostic = 'missing target'
             WHERE id = 105",
            [],
        )
        .expect("broken link should update");
    connection
        .execute(
            "UPDATE links
             SET target_id = 'missing-id',
                 resolution_status = 'unresolved',
                 resolution_diagnostic = 'missing org id'
             WHERE id = 106",
            [],
        )
        .expect("unresolved link should update");
    connection
        .execute(
            "UPDATE links
             SET target_file_id = ?1,
                 target_heading_id = ?2,
                 target_id = 'dup-id',
                 resolution_status = 'ambiguous',
                 resolution_diagnostic = 'duplicate org id'
             WHERE id = 107",
            rusqlite::params![gamma_file_id, 31],
        )
        .expect("ambiguous link should update");
}

fn table_counts(connection: &Connection) -> Vec<(&'static str, i64)> {
    [
        "files",
        "headings",
        "links",
        "tags",
        "properties",
        "keywords",
        "timestamps",
    ]
    .into_iter()
    .map(|table| {
        let sql = format!("SELECT COUNT(*) FROM {table}");
        let count = connection
            .query_row(&sql, [], |row| row.get(0))
            .expect("table count should load");
        (table, count)
    })
    .collect()
}
