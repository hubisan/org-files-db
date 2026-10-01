use super::{
    execute_and_shape_query, load_heading_paths_from_relation, shape_query_results,
    EffectivePropertyFact, QueryExecutionOptions, QueryInclude, QueryOutputMode, QueryResponse,
    QueryResultKind, QueryResultNode, QueryShapeErrorKind,
};
use crate::db::{
    open_in_memory_database_with_schema, DbWriter, EffectivePropertyRecord, EffectiveTagRecord,
    FileRecordInput, HeadingRecord, KeywordRecord, LinkRecord, OutlinePathRecord, PropertyRecord,
    SchemaDefinition, TagRecord,
};
use crate::property::{derive_effective_properties, PropertyRow};
use crate::query::sqlite::execute_sqlite_query_with_relation;
use crate::query::{
    execute_sqlite_query, parse_query, validate_query, HeadingQueryMatch, QueryRows, QueryTarget,
    QueryValidationOptions,
};
use crate::tag::derive_effective_tags;
use rusqlite::{limits::Limit, Connection};
use serde_json::Value;
use std::path::Path;

#[test]
fn query_result_kind_classifies_heading_levels() {
    assert_eq!(
        QueryResultKind::from_heading_level(0),
        QueryResultKind::Root
    );
    assert_eq!(
        QueryResultKind::from_heading_level(1),
        QueryResultKind::Heading
    );
}

#[test]
fn link_output_preserves_omitted_root_sources_and_uses_root_outline_nodes() {
    let connection = seeded_connection();
    let query = validated(r#"(links (status "resolved"))"#);
    let rows = execute_sqlite_query(&connection, &query).expect("query should execute");
    let response = shape_query_results(
        &connection,
        rows,
        &QueryExecutionOptions {
            output_mode: QueryOutputMode::Flat,
            includes: vec![
                QueryInclude::Path,
                QueryInclude::Source,
                QueryInclude::Target,
            ],
            ..QueryExecutionOptions::default()
        },
    )
    .expect("results should shape");

    let json = serde_json::to_value(&response).expect("response should serialize");
    let first = &json["results"][0];
    assert_eq!(first["kind"], "link");
    assert_eq!(first["link_path"], "beta.org");
    assert!(first.get("path").is_none());
    assert!(first["node_path"].is_array());
    assert_eq!(first["node_path"][0]["kind"], "file");
    assert_eq!(first["source"]["heading"], Value::Null);
    assert_eq!(
        first["source"]["source_path"].as_array().map(Vec::len),
        Some(1)
    );
    assert_eq!(first["target"]["resolved_kind"], "files");
    assert_eq!(first["target"]["file"]["path"], "/tmp/query-beta.org");

    let second = &json["results"][1];
    assert_eq!(second["source"]["heading"]["id"], 11);
    assert_eq!(second["source"]["heading"]["title"], "Query Engine");
    assert_eq!(second["target"]["resolved_kind"], "files");
    assert!(second["target"]["heading"].is_null());

    let third = &json["results"][2];
    assert_eq!(third["source"]["heading"]["id"], 12);
    assert_eq!(third["target"]["resolved_kind"], "headings");
    assert_eq!(third["target"]["file"]["path"], "/tmp/query-beta.org");
    assert_eq!(third["target"]["heading"]["id"], 21);
    assert_eq!(third["target"]["heading"]["title"], "Beta Target");

    let outline_rows = execute_sqlite_query(&connection, &query).expect("query should execute");
    let outline = shape_query_results(
        &connection,
        outline_rows,
        &QueryExecutionOptions {
            output_mode: QueryOutputMode::Outline,
            ..QueryExecutionOptions::default()
        },
    )
    .expect("outline results should shape");
    let outline_json = serde_json::to_value(&outline).expect("outline should serialize");
    assert_eq!(outline_json["results"][0]["kind"], "root");
}

#[test]
fn file_root_target_include_is_shaped_as_file_without_root_title_raw() {
    let connection = seeded_connection();
    connection
        .execute("UPDATE headings SET title_raw = NULL WHERE id = 20", [])
        .expect("root title_raw should clear");
    connection
        .execute("UPDATE links SET target_heading_id = 20 WHERE id = 100", [])
        .expect("file link should reference the synthetic root heading");

    let response = execute_and_shape_query(
        &connection,
        &validated(r#"(links (status "resolved"))"#),
        &QueryExecutionOptions {
            output_mode: QueryOutputMode::Flat,
            includes: vec![QueryInclude::Target],
            ..QueryExecutionOptions::default()
        },
    )
    .expect("file root target should shape");

    let link = response
        .results
        .iter()
        .find_map(|node| match node {
            QueryResultNode::Link(link) if link.id == 100 => Some(link.as_ref()),
            _ => None,
        })
        .expect("resolved file link should exist");
    let target = link.target.as_ref().expect("target include should exist");

    assert_eq!(link.target_heading_id, Some(20));
    assert_eq!(target.resolved_kind, Some(QueryTarget::Files));
    assert!(target.heading.is_none());
    assert_eq!(
        target
            .file
            .as_ref()
            .and_then(|file| file.title_raw.as_deref()),
        None
    );
}

#[test]
fn missing_heading_title_raw_in_path_returns_shape_error() {
    let connection = seeded_connection();
    connection
        .execute("UPDATE headings SET title_raw = NULL WHERE id = 11", [])
        .expect("heading title_raw should clear");

    let error = execute_and_shape_query(
        &connection,
        &validated(r#"(headings (title "Nested" :exact t))"#),
        &QueryExecutionOptions {
            output_mode: QueryOutputMode::Flat,
            includes: vec![QueryInclude::Path],
            ..QueryExecutionOptions::default()
        },
    )
    .expect_err("missing path title_raw should return a shape error");

    assert_eq!(error.kind, QueryShapeErrorKind::MissingStoredData);
    assert!(error.message.contains("stored heading row 11"));
}

#[test]
fn missing_heading_title_raw_in_included_link_returns_shape_error() {
    let connection = seeded_connection();
    connection
        .execute("UPDATE headings SET title_raw = NULL WHERE id = 11", [])
        .expect("heading title_raw should clear");

    let error = execute_and_shape_query(
        &connection,
        &validated(r#"(headings (title "Query Engine" :exact t))"#),
        &QueryExecutionOptions {
            output_mode: QueryOutputMode::Flat,
            includes: vec![QueryInclude::Links],
            ..QueryExecutionOptions::default()
        },
    )
    .expect_err("malformed included link source should return a shape error");

    assert_eq!(error.kind, QueryShapeErrorKind::MissingStoredData);
    assert!(error.message.contains("stored heading row 11"));
}

#[test]
fn missing_heading_title_raw_in_resolved_target_returns_shape_error() {
    let connection = seeded_connection();
    connection
        .execute("UPDATE headings SET title_raw = NULL WHERE id = 21", [])
        .expect("target heading title_raw should clear");

    let error = execute_and_shape_query(
        &connection,
        &validated(r#"(links (status "resolved"))"#),
        &QueryExecutionOptions {
            output_mode: QueryOutputMode::Flat,
            includes: vec![QueryInclude::Target],
            ..QueryExecutionOptions::default()
        },
    )
    .expect_err("malformed resolved target should return a shape error");

    assert_eq!(error.kind, QueryShapeErrorKind::MissingStoredData);
    assert!(error.message.contains("stored heading row 21"));
}

#[test]
fn inconsistent_outline_file_path_returns_shape_error() {
    let connection = seeded_connection();
    let query = validated(r#"(headings (title "Nested" :exact t))"#);
    let mut rows = execute_sqlite_query(&connection, &query).expect("query should execute");

    match &mut rows {
        QueryRows::Headings(rows) => match rows.first_mut() {
            Some(HeadingQueryMatch::Heading(row)) => {
                row.file_path = "/tmp/inconsistent-query-path.org".to_string();
            }
            _ => panic!("expected a heading query row"),
        },
        _ => panic!("expected heading query rows"),
    }

    let error = shape_query_results(
        &connection,
        rows,
        &QueryExecutionOptions {
            output_mode: QueryOutputMode::Outline,
            ..QueryExecutionOptions::default()
        },
    )
    .expect_err("inconsistent outline path should return a shape error");

    assert_eq!(error.kind, QueryShapeErrorKind::MissingStoredData);
    assert!(error.message.contains("missing outline file root"));
}

#[test]
fn broken_link_target_preserves_status_without_resolved_objects() {
    let connection = seeded_connection();
    let response = execute_and_shape_query(
        &connection,
        &validated(r#"(links (status "broken"))"#),
        &QueryExecutionOptions {
            output_mode: QueryOutputMode::Flat,
            includes: vec![QueryInclude::Source, QueryInclude::Target],
            ..QueryExecutionOptions::default()
        },
    )
    .expect("query should shape");

    let link = link_node(&response.results[0]);
    let target = link.target.as_ref().expect("target include should exist");
    assert_eq!(target.resolution_status.as_deref(), Some("broken"));
    assert_eq!(
        target.resolution_diagnostic.as_deref(),
        Some("missing target")
    );
    assert!(target.file.is_none());
    assert!(target.heading.is_none());
    assert!(target.resolved_kind.is_none());
}

#[test]
fn missing_outline_path_row_reports_outline_specific_error() {
    let connection = seeded_connection();
    connection
        .execute("DELETE FROM outline_path WHERE heading_id = 11", [])
        .expect("outline path row should delete");

    let error = execute_and_shape_query(
        &connection,
        &validated(r#"(headings (title "Query Engine" :exact t))"#),
        &QueryExecutionOptions::default(),
    )
    .expect_err("query shaping should fail when outline_path is missing");

    assert!(
        error
            .to_string()
            .contains("missing outline_path row for stored heading id 11"),
        "expected outline-specific error, got {error}"
    );
}

#[test]
fn file_links_include_is_file_wide() {
    let connection = seeded_connection();
    let response = execute_and_shape_query(
        &connection,
        &validated(r#"(files (file-title "Alpha Index" :exact t))"#),
        &QueryExecutionOptions {
            output_mode: QueryOutputMode::Flat,
            includes: vec![QueryInclude::Links],
            ..QueryExecutionOptions::default()
        },
    )
    .expect("query should shape");

    let file = file_node(&response.results[0]);
    let links = file.links.as_ref().expect("links include should exist");
    let ids = links.iter().map(|link| link.id).collect::<Vec<_>>();
    assert_eq!(ids, vec![102, 100, 101, 105]);
    assert_eq!(links[0].source_path.len(), 1);
    assert_eq!(links[1].source_path.len(), 2);
}

#[test]
fn heading_properties_include_is_exact_and_omitted_by_default() {
    let connection = seeded_connection();
    let plain = execute_and_shape_query(
        &connection,
        &validated(r#"(headings (title "Query Engine" :exact t))"#),
        &QueryExecutionOptions::default(),
    )
    .expect("plain query should shape");
    let plain_json = serde_json::to_value(&plain).expect("plain response should serialize");
    assert!(plain_json["results"][0].get("properties").is_none());

    let included = execute_and_shape_query(
        &connection,
        &validated(r#"(headings (title "Query Engine" :exact t))"#),
        &QueryExecutionOptions {
            output_mode: QueryOutputMode::Flat,
            includes: vec![QueryInclude::Properties],
            ..QueryExecutionOptions::default()
        },
    )
    .expect("included query should shape");
    let heading = heading_node(&included.results[0]);
    let properties = heading
        .properties
        .as_ref()
        .expect("properties include should exist");
    assert_eq!(properties.len(), 1);
    assert_eq!(properties[0].key, "AREA");
    assert_eq!(properties[0].value.as_deref(), Some("infra"));
    assert_eq!(properties[0].source, "property_drawer");
    assert!(!properties[0].append);
    assert_eq!(properties[0].line_number, Some(4));
    assert_eq!(matched_heading_ids(&plain), matched_heading_ids(&included));
}

#[test]
fn effective_properties_include_resolves_inherited_values_and_is_omitted_by_default() {
    let connection = seeded_connection();
    let plain = execute_and_shape_query(
        &connection,
        &validated(r#"(headings (title "Nested" :exact t))"#),
        &QueryExecutionOptions::default(),
    )
    .expect("plain query should shape");
    let plain_json = serde_json::to_value(&plain).expect("plain response should serialize");
    assert!(plain_json["results"][0]
        .get("effective_properties")
        .is_none());

    let included = execute_and_shape_query(
        &connection,
        &validated(r#"(headings (title "Nested" :exact t))"#),
        &QueryExecutionOptions {
            output_mode: QueryOutputMode::Flat,
            includes: vec![QueryInclude::EffectiveProperties],
            ..QueryExecutionOptions::default()
        },
    )
    .expect("included query should shape");
    let heading = heading_node(&included.results[0]);
    assert!(heading.properties.is_none());
    let properties = heading
        .effective_properties
        .as_ref()
        .expect("effective properties include should exist");
    assert_eq!(
        properties,
        &vec![
            EffectivePropertyFact {
                key: "AREA".to_string(),
                value: Some("infra".to_string()),
            },
            EffectivePropertyFact {
                key: "CATEGORY".to_string(),
                value: Some("work".to_string()),
            },
        ]
    );
    assert_eq!(matched_heading_ids(&plain), matched_heading_ids(&included));
}

#[test]
fn effective_properties_include_uses_first_local_base_plus_all_appends() {
    let connection = seeded_connection();
    let response = execute_and_shape_query(
        &connection,
        &validated(r#"(headings (title "Loose Note" :exact t))"#),
        &QueryExecutionOptions {
            output_mode: QueryOutputMode::Flat,
            includes: vec![QueryInclude::EffectiveProperties],
            ..QueryExecutionOptions::default()
        },
    )
    .expect("query should shape");

    let heading = heading_node(&response.results[0]);
    assert_eq!(
        heading.effective_properties.as_ref(),
        Some(&vec![
            EffectivePropertyFact {
                key: "APPEND_REPLACED".to_string(),
                value: Some("first appended".to_string()),
            },
            EffectivePropertyFact {
                key: "CATEGORY".to_string(),
                value: Some("work".to_string()),
            },
        ])
    );
}

#[test]
fn file_properties_and_keywords_includes_use_root_stored_facts_and_omit_by_default() {
    let connection = seeded_connection();
    let plain = execute_and_shape_query(
        &connection,
        &validated(r#"(files (file-title "Alpha Index" :exact t))"#),
        &QueryExecutionOptions::default(),
    )
    .expect("plain query should shape");
    let plain_json = serde_json::to_value(&plain).expect("plain response should serialize");
    assert!(plain_json["results"][0].get("properties").is_none());
    assert!(plain_json["results"][0].get("keywords").is_none());

    let included = execute_and_shape_query(
        &connection,
        &validated(r#"(files (file-title "Alpha Index" :exact t))"#),
        &QueryExecutionOptions {
            output_mode: QueryOutputMode::Flat,
            includes: vec![QueryInclude::Properties, QueryInclude::Keywords],
            ..QueryExecutionOptions::default()
        },
    )
    .expect("included query should shape");
    let file = file_node(&included.results[0]);
    let properties = file
        .properties
        .as_ref()
        .expect("properties include should exist");
    assert_eq!(properties.len(), 1);
    assert_eq!(properties[0].key, "CATEGORY");
    assert_eq!(properties[0].value.as_deref(), Some("work"));
    assert_eq!(properties[0].source, "category_keyword");
    let keywords = file
        .keywords
        .as_ref()
        .expect("keywords include should exist");
    assert_eq!(keywords.len(), 1);
    assert_eq!(keywords[0].keyword, "AUTHOR");
    assert_eq!(keywords[0].value.as_deref(), Some("Alice"));
    assert_eq!(keywords[0].line_number, Some(1));
}

#[test]
fn file_effective_properties_include_uses_resolved_root_values() {
    let connection = seeded_connection();
    let response = execute_and_shape_query(
        &connection,
        &validated(r#"(files (file-title "Alpha Index" :exact t))"#),
        &QueryExecutionOptions {
            output_mode: QueryOutputMode::Flat,
            includes: vec![QueryInclude::EffectiveProperties],
            ..QueryExecutionOptions::default()
        },
    )
    .expect("query should shape");

    let file = file_node(&response.results[0]);
    assert!(file.properties.is_none());
    assert_eq!(
        file.effective_properties.as_ref(),
        Some(&vec![EffectivePropertyFact {
            key: "CATEGORY".to_string(),
            value: Some("work".to_string()),
        }])
    );
}

#[test]
fn heading_keywords_include_reflects_current_stored_heading_facts() {
    let connection = seeded_connection();
    let plain = execute_and_shape_query(
        &connection,
        &validated(r#"(headings (title "Query Engine" :exact t))"#),
        &QueryExecutionOptions::default(),
    )
    .expect("plain query should shape");
    let plain_json = serde_json::to_value(&plain).expect("plain response should serialize");
    assert!(plain_json["results"][0].get("keywords").is_none());

    let included = execute_and_shape_query(
        &connection,
        &validated(r#"(headings (title "Query Engine" :exact t))"#),
        &QueryExecutionOptions {
            output_mode: QueryOutputMode::Flat,
            includes: vec![QueryInclude::Keywords],
            ..QueryExecutionOptions::default()
        },
    )
    .expect("included query should shape");
    let heading = heading_node(&included.results[0]);
    let keywords = heading
        .keywords
        .as_ref()
        .expect("keywords include should exist");
    assert!(keywords.is_empty());
    assert_eq!(matched_heading_ids(&plain), matched_heading_ids(&included));
}

#[test]
fn file_backlinks_include_captures_heading_targets_in_file() {
    let connection = seeded_connection();
    let response = execute_and_shape_query(
        &connection,
        &validated(r#"(files (file-title "Alpha Index" :exact t))"#),
        &QueryExecutionOptions {
            output_mode: QueryOutputMode::Flat,
            includes: vec![QueryInclude::Backlinks],
            ..QueryExecutionOptions::default()
        },
    )
    .expect("query should shape");

    let file = file_node(&response.results[0]);
    let backlinks = file
        .backlinks
        .as_ref()
        .expect("backlinks include should exist");
    let ids = backlinks.iter().map(|link| link.id).collect::<Vec<_>>();
    assert_eq!(ids, vec![104, 103]);
    assert_eq!(
        backlinks[0].target.resolution_status.as_deref(),
        Some("resolved")
    );
}

#[test]
fn file_location_omits_internal_root_byte_sentinel() {
    let connection = seeded_connection();
    let response = execute_and_shape_query(
        &connection,
        &validated(r#"(files (file-title "Alpha Index" :exact t))"#),
        &QueryExecutionOptions::default(),
    )
    .expect("query should shape");

    let file = file_node(&response.results[0]);
    assert_eq!(file.location.byte_start, None);
    assert_eq!(file.location.byte_end, None);
}

#[test]
fn plain_flat_file_results_load_only_their_required_root_heading() {
    let connection = seeded_connection();
    connection
        .execute("DELETE FROM outline_path WHERE heading_id = 13", [])
        .expect("unrelated outline path should delete");

    let response = execute_and_shape_query(
        &connection,
        &validated(r#"(files (file-title "Alpha Index" :exact t))"#),
        &QueryExecutionOptions::default(),
    )
    .expect("plain flat file query should shape without unrelated headings");

    let file = file_node(&response.results[0]);
    assert_eq!(file.path, "/tmp/query-alpha.org");
    assert_eq!(file.tags, vec!["filetag".to_string()]);
}

#[test]
fn plain_flat_heading_results_do_not_load_unrelated_headings_from_the_same_file() {
    let connection = seeded_connection();
    connection
        .execute("DELETE FROM outline_path WHERE heading_id = 13", [])
        .expect("unrelated outline path should delete");

    let response = execute_and_shape_query(
        &connection,
        &validated(r#"(headings (title "Query Engine" :exact t))"#),
        &QueryExecutionOptions::default(),
    )
    .expect("plain flat heading query should shape without unrelated headings");

    let heading = heading_node(&response.results[0]);
    assert_eq!(heading.id, 11);
    assert_eq!(heading.title, "Query Engine");
}

#[test]
fn relation_backed_flat_path_matches_standalone_shaping() {
    let connection = seeded_connection();
    let query = validated(r#"(headings (level 1))"#);
    let options = QueryExecutionOptions {
        includes: vec![QueryInclude::Path],
        ..QueryExecutionOptions::default()
    };
    let rows = execute_sqlite_query(&connection, &query).expect("query should execute");
    let expected =
        shape_query_results(&connection, rows, &options).expect("standalone shaping should work");
    let actual = execute_and_shape_query(&connection, &query, &options)
        .expect("relation-backed shaping should work");

    assert_eq!(actual, expected);
}

#[test]
fn relation_backed_metadata_matches_standalone_shaping() {
    let connection = seeded_connection();
    let query = validated(r#"(headings (level 1))"#);
    let options = QueryExecutionOptions {
        includes: vec![
            QueryInclude::Properties,
            QueryInclude::EffectiveProperties,
            QueryInclude::Keywords,
        ],
        ..QueryExecutionOptions::default()
    };
    let rows = execute_sqlite_query(&connection, &query).expect("query should execute");
    let expected =
        shape_query_results(&connection, rows, &options).expect("standalone shaping should work");
    let actual = execute_and_shape_query(&connection, &query, &options)
        .expect("relation-backed shaping should work");

    assert_eq!(actual, expected);
}

#[test]
fn relation_backed_complex_match_metadata_matches_standalone_shaping() {
    let connection = seeded_connection();
    let query = validated(r#"(headings (tags "project"))"#);
    let options = QueryExecutionOptions {
        includes: vec![QueryInclude::EffectiveProperties],
        ..QueryExecutionOptions::default()
    };
    let rows = execute_sqlite_query(&connection, &query).expect("query should execute");
    let expected =
        shape_query_results(&connection, rows, &options).expect("standalone shaping should work");
    let actual = execute_and_shape_query(&connection, &query, &options)
        .expect("relation-backed shaping should work");

    assert_eq!(actual, expected);
}

#[test]
fn relation_backed_nested_path_matches_standalone_shaping() {
    let connection = seeded_connection();
    let query = validated(r#"(headings (title "Nested" :exact t))"#);
    let options = QueryExecutionOptions {
        includes: vec![QueryInclude::Path],
        ..QueryExecutionOptions::default()
    };
    let rows = execute_sqlite_query(&connection, &query).expect("query should execute");
    let expected =
        shape_query_results(&connection, rows, &options).expect("standalone shaping should work");
    let actual = execute_and_shape_query(&connection, &query, &options)
        .expect("relation-backed shaping should work");

    assert_eq!(actual, expected);
    let path = heading_node(&actual.results[0])
        .node_path
        .as_ref()
        .expect("path should exist");
    assert_eq!(path.len(), 3);
}

#[test]
fn rust_driven_path_respects_small_runtime_variable_limit() {
    let connection = seeded_connection();
    let query = validated(r#"(headings (level 1))"#);
    let options = QueryExecutionOptions {
        includes: vec![QueryInclude::Path],
        ..QueryExecutionOptions::default()
    };
    let executed = execute_sqlite_query_with_relation(&connection, &query, &options)
        .expect("query should execute");
    let rows = match &executed.rows {
        QueryRows::Headings(rows) => rows,
        QueryRows::Links(_) | QueryRows::Files(_) => {
            panic!("heading query should return headings")
        }
    };
    let expected = load_heading_paths_from_relation(&connection, &executed.relation, rows)
        .expect("path loader should load");

    let previous = connection
        .set_limit(Limit::SQLITE_LIMIT_VARIABLE_NUMBER, 2)
        .expect("runtime variable limit should change");
    let actual = load_heading_paths_from_relation(&connection, &executed.relation, rows)
        .expect("path loader should respect the small variable limit");
    connection
        .set_limit(Limit::SQLITE_LIMIT_VARIABLE_NUMBER, previous)
        .expect("runtime variable limit should restore");

    assert_eq!(actual, expected);
}

#[test]
fn rust_driven_path_rejects_cross_file_parent() {
    let connection = seeded_connection();
    connection
        .execute("UPDATE headings SET parent_id = 21 WHERE id = 12", [])
        .expect("cross-file parent should update");
    let query = validated(r#"(headings (title "Nested" :exact t))"#);
    let options = QueryExecutionOptions {
        includes: vec![QueryInclude::Path],
        ..QueryExecutionOptions::default()
    };
    let executed = execute_sqlite_query_with_relation(&connection, &query, &options)
        .expect("query should execute");
    let rows = match &executed.rows {
        QueryRows::Headings(rows) => rows,
        QueryRows::Links(_) | QueryRows::Files(_) => {
            panic!("heading query should return headings")
        }
    };

    let error = load_heading_paths_from_relation(&connection, &executed.relation, rows)
        .expect_err("path loader should reject a cross-file parent");

    assert_eq!(error.kind, QueryShapeErrorKind::MissingStoredData);
    assert!(error
        .message
        .contains("missing stored heading row for id 21"));
}

#[test]
fn rust_driven_path_keeps_ancestor_outline_validation() {
    let connection = seeded_connection();
    connection
        .execute("DELETE FROM outline_path WHERE heading_id = 11", [])
        .expect("ancestor outline path should delete");
    let query = validated(r#"(headings (title "Nested" :exact t))"#);
    let options = QueryExecutionOptions {
        includes: vec![QueryInclude::Path],
        ..QueryExecutionOptions::default()
    };
    let executed = execute_sqlite_query_with_relation(&connection, &query, &options)
        .expect("query should execute");
    let rows = match &executed.rows {
        QueryRows::Headings(rows) => rows,
        QueryRows::Links(_) | QueryRows::Files(_) => {
            panic!("heading query should return headings")
        }
    };

    let error = load_heading_paths_from_relation(&connection, &executed.relation, rows)
        .expect_err("path loader should validate ancestor outline rows");

    assert_eq!(error.kind, QueryShapeErrorKind::MissingStoredData);
    assert!(error
        .message
        .contains("missing outline_path row for stored heading id 11"));
}

#[test]
fn relation_backed_root_heading_metadata_matches_standalone_shaping() {
    let connection = seeded_connection();
    let query = validated(r#"(headings (title "Alpha Index" :exact t))"#);
    let options = QueryExecutionOptions {
        includes: vec![
            QueryInclude::Path,
            QueryInclude::Properties,
            QueryInclude::EffectiveProperties,
            QueryInclude::Keywords,
        ],
        ..QueryExecutionOptions::default()
    };
    let rows = execute_sqlite_query(&connection, &query).expect("query should execute");
    let expected =
        shape_query_results(&connection, rows, &options).expect("standalone shaping should work");
    let actual = execute_and_shape_query(&connection, &query, &options)
        .expect("relation-backed shaping should work");

    assert_eq!(actual, expected);
}

#[test]
fn relation_backed_file_metadata_matches_standalone_shaping() {
    let connection = seeded_connection();
    let query = validated(r#"(files (file-title "Alpha Index" :exact t))"#);
    let options = QueryExecutionOptions {
        includes: vec![
            QueryInclude::Path,
            QueryInclude::Properties,
            QueryInclude::EffectiveProperties,
            QueryInclude::Keywords,
        ],
        ..QueryExecutionOptions::default()
    };
    let rows = execute_sqlite_query(&connection, &query).expect("query should execute");
    let expected =
        shape_query_results(&connection, rows, &options).expect("standalone shaping should work");
    let actual = execute_and_shape_query(&connection, &query, &options)
        .expect("relation-backed shaping should work");

    assert_eq!(actual, expected);
}

#[test]
fn relation_backed_path_rejects_cross_file_parent() {
    let connection = seeded_connection();
    connection
        .execute("UPDATE headings SET parent_id = 21 WHERE id = 12", [])
        .expect("cross-file parent should update");
    let query = validated(r#"(headings (title "Nested" :exact t))"#);
    let options = QueryExecutionOptions {
        includes: vec![QueryInclude::Path],
        ..QueryExecutionOptions::default()
    };

    let error = execute_and_shape_query(&connection, &query, &options)
        .expect_err("path shaping should reject a cross-file parent");

    assert_eq!(error.kind, QueryShapeErrorKind::MissingStoredData);
    assert!(error
        .message
        .contains("missing stored heading row for id 21"));
}

#[test]
fn relation_backed_path_keeps_ancestor_outline_validation() {
    let connection = seeded_connection();
    connection
        .execute("DELETE FROM outline_path WHERE heading_id = 11", [])
        .expect("ancestor outline path should delete");
    let query = validated(r#"(headings (title "Nested" :exact t))"#);
    let options = QueryExecutionOptions {
        includes: vec![QueryInclude::Path],
        ..QueryExecutionOptions::default()
    };

    let error = execute_and_shape_query(&connection, &query, &options)
        .expect_err("path shaping should validate the ancestor outline row");

    assert_eq!(error.kind, QueryShapeErrorKind::MissingStoredData);
    assert!(error
        .message
        .contains("missing outline_path row for stored heading id 11"));
    assert!(connection.is_autocommit());
}

#[test]
fn relation_backed_path_rejects_missing_ancestor() {
    let connection = seeded_connection();
    connection
        .execute_batch(
            r#"
PRAGMA foreign_keys = OFF;
UPDATE headings SET parent_id = 999 WHERE id = 12;
PRAGMA foreign_keys = ON;
"#,
        )
        .expect("missing ancestor parent should update");
    let query = validated(r#"(headings (title "Nested" :exact t))"#);
    let options = QueryExecutionOptions {
        includes: vec![QueryInclude::Path],
        ..QueryExecutionOptions::default()
    };

    let error = execute_and_shape_query(&connection, &query, &options)
        .expect_err("path shaping should reject a missing ancestor");

    assert_eq!(error.kind, QueryShapeErrorKind::MissingStoredData);
    assert!(error
        .message
        .contains("missing stored heading row for id 999"));
    assert!(connection.is_autocommit());
}

#[test]
fn relation_backed_path_rejects_malformed_ancestor_outline_json() {
    let connection = seeded_connection();
    connection
        .execute(
            "UPDATE outline_path SET breadcrumbs_json = 'not json' WHERE heading_id = 11",
            [],
        )
        .expect("ancestor outline path should update");
    let query = validated(r#"(headings (title "Nested" :exact t))"#);
    let options = QueryExecutionOptions {
        includes: vec![QueryInclude::Path],
        ..QueryExecutionOptions::default()
    };

    let error = execute_and_shape_query(&connection, &query, &options)
        .expect_err("path shaping should reject malformed ancestor outline JSON");

    assert_eq!(error.kind, QueryShapeErrorKind::InvalidStoredJson);
    assert!(error
        .message
        .contains("failed to decode stored JSON field breadcrumbs_json for row 11"));
    assert!(connection.is_autocommit());
}

#[test]
fn flat_enrichment_respects_small_runtime_variable_limit() {
    let connection = seeded_connection();
    let query = validated(r#"(headings (level 1))"#);
    let options = QueryExecutionOptions {
        includes: vec![QueryInclude::EffectiveProperties],
        ..QueryExecutionOptions::default()
    };
    let expected = execute_and_shape_query(&connection, &query, &options)
        .expect("baseline query should shape");

    let previous = connection
        .set_limit(Limit::SQLITE_LIMIT_VARIABLE_NUMBER, 2)
        .expect("runtime variable limit should change");
    let actual = execute_and_shape_query(&connection, &query, &options)
        .expect("small variable limit should use more chunks");
    connection
        .set_limit(Limit::SQLITE_LIMIT_VARIABLE_NUMBER, previous)
        .expect("runtime variable limit should restore");

    assert_eq!(actual, expected);
}

#[test]
fn full_enrichment_respects_small_runtime_variable_limit() {
    let connection = seeded_connection();
    let query = validated(r#"(headings (level 1))"#);
    let options = QueryExecutionOptions {
        includes: vec![
            QueryInclude::Path,
            QueryInclude::Properties,
            QueryInclude::EffectiveProperties,
            QueryInclude::Keywords,
            QueryInclude::Links,
            QueryInclude::Backlinks,
        ],
        ..QueryExecutionOptions::default()
    };
    let expected = execute_and_shape_query(&connection, &query, &options)
        .expect("baseline full enrichment should shape");

    let previous = connection
        .set_limit(Limit::SQLITE_LIMIT_VARIABLE_NUMBER, 2)
        .expect("runtime variable limit should change");
    let actual = execute_and_shape_query(&connection, &query, &options)
        .expect("full enrichment should respect the small variable limit");
    connection
        .set_limit(Limit::SQLITE_LIMIT_VARIABLE_NUMBER, previous)
        .expect("runtime variable limit should restore");

    assert_eq!(actual, expected);
}

#[test]
fn plain_flat_link_results_do_not_load_source_headings() {
    let connection = seeded_connection();
    let query = validated(r#"(links (status "resolved"))"#);
    let rows = execute_sqlite_query(&connection, &query).expect("query should execute");

    connection
        .execute("DELETE FROM outline_path WHERE heading_id = 11", [])
        .expect("source outline path should delete after query execution");

    let response = shape_query_results(&connection, rows, &QueryExecutionOptions::default())
        .expect("plain flat link rows should shape without stored heading enrichment");

    assert!(response
        .results
        .iter()
        .any(|node| matches!(node, QueryResultNode::Link(link) if link.id == 100)));
}

#[test]
fn outline_mode_marks_context_only_ancestors() {
    let connection = seeded_connection();
    let response = execute_and_shape_query(
        &connection,
        &validated(r#"(headings (title "Nested" :exact t))"#),
        &QueryExecutionOptions {
            output_mode: QueryOutputMode::Outline,
            includes: vec![],
            ..QueryExecutionOptions::default()
        },
    )
    .expect("query should shape");

    let file = file_node(&response.results[0]);
    assert!(!file.matched);
    let parent = heading_node(&file.children.as_ref().expect("children")[0]);
    assert!(!parent.matched);
    let child = heading_node(&parent.children.as_ref().expect("children")[0]);
    assert!(child.matched);
}

#[test]
fn heading_title_root_matches_shape_as_file_nodes_in_flat_and_outline_output() {
    let connection = seeded_connection();

    let flat = execute_and_shape_query(
        &connection,
        &validated(r#"(headings (title "Alpha Index" :exact t))"#),
        &QueryExecutionOptions::default(),
    )
    .expect("flat root title query should shape");
    assert_eq!(flat.target, QueryTarget::Headings);
    let file = file_node(&flat.results[0]);
    assert!(file.matched);
    assert_eq!(file.title, "Alpha Index");
    assert!(file.children.is_none());

    let outline = execute_and_shape_query(
        &connection,
        &validated(r#"(headings (title "Alpha Index" :exact t))"#),
        &QueryExecutionOptions {
            output_mode: QueryOutputMode::Outline,
            includes: vec![],
            ..QueryExecutionOptions::default()
        },
    )
    .expect("outline root title query should shape");
    assert_eq!(outline.target, QueryTarget::Headings);
    let file = file_node(&outline.results[0]);
    assert!(file.matched);
    assert!(file
        .children
        .as_ref()
        .expect("outline file children should exist")
        .is_empty());
}

#[test]
fn heading_file_predicates_shape_matching_file_roots_and_preserve_includes() {
    let connection = seeded_connection();

    let flat = execute_and_shape_query(
        &connection,
        &validated(r#"(headings (file-title "Alpha Index" :exact t))"#),
        &QueryExecutionOptions {
            output_mode: QueryOutputMode::Flat,
            includes: vec![QueryInclude::Properties],
            ..QueryExecutionOptions::default()
        },
    )
    .expect("flat file-title heading query should shape");
    let file = file_node(&flat.results[0]);
    assert!(file.matched);
    assert_eq!(file.level, 0);
    assert_eq!(file.path, "/tmp/query-alpha.org");
    let properties = file
        .properties
        .as_ref()
        .expect("properties include should exist");
    assert_eq!(properties.len(), 1);
    assert_eq!(properties[0].key, "CATEGORY");
    assert_eq!(properties[0].value.as_deref(), Some("work"));
    let heading = heading_node(&flat.results[1]);
    assert!(heading.matched);
    assert!(heading.level > 0);

    let outline = execute_and_shape_query(
        &connection,
        &validated(
            r#"(headings
                (and
                  (file-title "Alpha Index" :exact t)
                  (todo "NEXT")))"#,
        ),
        &QueryExecutionOptions {
            output_mode: QueryOutputMode::Outline,
            includes: vec![],
            ..QueryExecutionOptions::default()
        },
    )
    .expect("outline file-title heading query should shape");
    let outline_file = file_node(&outline.results[0]);
    assert!(!outline_file.matched);
    let outline_children = outline_file
        .children
        .as_ref()
        .expect("outline file children should exist");
    let outline_heading = heading_node(&outline_children[0]);
    assert!(outline_heading.matched);
}

#[test]
fn bare_headings_query_shapes_file_roots_and_real_headings_in_flat_and_outline_output() {
    let connection = seeded_connection();

    let flat = execute_and_shape_query(
        &connection,
        &validated(r#"(headings)"#),
        &QueryExecutionOptions::default(),
    )
    .expect("flat bare headings query should shape");
    assert_eq!(flat.target, QueryTarget::Headings);
    assert!(matches!(
        flat.results.first(),
        Some(QueryResultNode::File(_))
    ));
    let flat_file = file_node(&flat.results[0]);
    assert!(flat_file.matched);
    assert_eq!(flat_file.level, 0);
    assert_eq!(flat_file.path, "/tmp/query-alpha.org");
    assert_eq!(flat_file.title, "Alpha Index");
    let flat_heading = heading_node(&flat.results[1]);
    assert!(flat_heading.matched);
    assert!(flat_heading.level > 0);

    let outline = execute_and_shape_query(
        &connection,
        &validated(r#"(headings)"#),
        &QueryExecutionOptions {
            output_mode: QueryOutputMode::Outline,
            includes: vec![],
            ..QueryExecutionOptions::default()
        },
    )
    .expect("outline bare headings query should shape");
    assert_eq!(outline.target, QueryTarget::Headings);
    assert_eq!(outline.results.len(), 2);
    let outline_file = file_node(&outline.results[0]);
    assert!(outline_file.matched);
    let outline_children = outline_file
        .children
        .as_ref()
        .expect("outline file children should exist");
    assert!(!outline_children.is_empty());
    let outline_heading = heading_node(&outline_children[0]);
    assert!(outline_heading.matched);
    assert!(outline_heading.level > 0);

    let flat_json = serde_json::to_value(&flat).expect("flat response should serialize");
    assert!(flat_json["results"]
        .as_array()
        .expect("flat results should be an array")
        .iter()
        .all(|node| node["kind"] != "heading" || node["level"].as_i64().unwrap_or_default() > 0));
    assert_eq!(flat_json["results"][0]["kind"], "root");
    assert_eq!(flat_json["results"][0]["level"], 0);
    let outline_json = serde_json::to_value(&outline).expect("outline response should serialize");
    assert_eq!(outline_json["results"][0]["kind"], "root");
    assert_eq!(matched_heading_ids(&flat), matched_heading_ids(&outline));
}

#[test]
fn output_modes_do_not_change_matched_heading_ids() {
    let connection = seeded_connection();
    let query = validated(r#"(headings (tags "project"))"#);
    let flat = execute_and_shape_query(
        &connection,
        &query,
        &QueryExecutionOptions {
            output_mode: QueryOutputMode::Flat,
            includes: vec![QueryInclude::Path],
            ..QueryExecutionOptions::default()
        },
    )
    .expect("flat query should shape");
    let outline = execute_and_shape_query(
        &connection,
        &query,
        &QueryExecutionOptions {
            output_mode: QueryOutputMode::Outline,
            includes: vec![QueryInclude::Path],
            ..QueryExecutionOptions::default()
        },
    )
    .expect("outline query should shape");

    assert_eq!(matched_heading_ids(&flat), vec![11, 12]);
    assert_eq!(matched_heading_ids(&outline), vec![11, 12]);
}

#[test]
fn includes_do_not_change_matched_ids() {
    let connection = seeded_connection();
    let query = validated(r#"(headings (tags "project"))"#);
    let plain = execute_and_shape_query(&connection, &query, &QueryExecutionOptions::default())
        .expect("plain query should shape");
    let enriched = execute_and_shape_query(
        &connection,
        &query,
        &QueryExecutionOptions {
            output_mode: QueryOutputMode::Flat,
            includes: vec![
                QueryInclude::Path,
                QueryInclude::Properties,
                QueryInclude::Keywords,
                QueryInclude::Links,
                QueryInclude::Backlinks,
            ],
            ..QueryExecutionOptions::default()
        },
    )
    .expect("enriched query should shape");

    assert_eq!(matched_heading_ids(&plain), matched_heading_ids(&enriched));
}

#[test]
fn selective_temp_relation_shapes_and_cleans_up() {
    let connection = seeded_connection();
    let query = validated(r#"(headings (and (tags "project") (property "AREA" "infra")))"#);
    let options = QueryExecutionOptions {
        output_mode: QueryOutputMode::Flat,
        includes: vec![
            QueryInclude::Properties,
            QueryInclude::EffectiveProperties,
            QueryInclude::Keywords,
        ],
        ..QueryExecutionOptions::default()
    };

    let selective = execute_and_shape_query(&connection, &query, &options)
        .expect("selective TEMP result should shape");

    assert!(!selective.results.is_empty());
    let temp_count: i64 = connection
        .query_row(
            "SELECT COUNT(*) FROM sqlite_temp_master WHERE type = 'table' AND name = 'orgfdb_query_matched_headings'",
            [],
            |row| row.get(0),
        )
        .expect("TEMP catalog query should work");
    assert_eq!(temp_count, 0);
}

#[test]
fn selective_temp_relation_cleans_up_after_shaping_error() {
    let connection = seeded_connection();
    connection
        .execute("DELETE FROM outline_path WHERE heading_id = 11", [])
        .expect("outline row should delete");
    let query = validated(r#"(headings (and (tags "project") (property "AREA" "infra")))"#);
    let error = execute_and_shape_query(
        &connection,
        &query,
        &QueryExecutionOptions {
            output_mode: QueryOutputMode::Flat,
            includes: vec![QueryInclude::EffectiveProperties],
            ..QueryExecutionOptions::default()
        },
    )
    .expect_err("missing outline data should still fail shaping");
    assert_eq!(error.kind, QueryShapeErrorKind::MissingStoredData);

    let temp_count: i64 = connection
        .query_row(
            "SELECT COUNT(*) FROM sqlite_temp_master WHERE type = 'table' AND name = 'orgfdb_query_matched_headings'",
            [],
            |row| row.get(0),
        )
        .expect("TEMP catalog query should work");
    assert_eq!(temp_count, 0);
}

#[test]
fn shaping_is_read_only() {
    let connection = seeded_connection();
    let before = table_counts(&connection);
    let _response = execute_and_shape_query(
        &connection,
        &validated(r#"(links (status "resolved"))"#),
        &QueryExecutionOptions {
            output_mode: QueryOutputMode::Flat,
            includes: vec![
                QueryInclude::Path,
                QueryInclude::Source,
                QueryInclude::Target,
            ],
            ..QueryExecutionOptions::default()
        },
    )
    .expect("query should shape");
    let after = table_counts(&connection);
    assert_eq!(before, after);
}

fn matched_heading_ids(response: &QueryResponse) -> Vec<i64> {
    let mut ids = Vec::new();
    collect_matched_heading_ids(&response.results, &mut ids);
    ids.sort();
    ids
}

fn collect_matched_heading_ids(nodes: &[QueryResultNode], ids: &mut Vec<i64>) {
    for node in nodes {
        match node {
            QueryResultNode::File(node) => {
                if let Some(children) = &node.children {
                    collect_matched_heading_ids(children, ids);
                }
            }
            QueryResultNode::Heading(node) => {
                if node.matched {
                    ids.push(node.id);
                }
                if let Some(children) = &node.children {
                    collect_matched_heading_ids(children, ids);
                }
            }
            QueryResultNode::Link(_) => {}
        }
    }
}

fn file_node(node: &QueryResultNode) -> &super::FileResultNode {
    match node {
        QueryResultNode::File(node) => node,
        _ => panic!("expected file node"),
    }
}

fn heading_node(node: &QueryResultNode) -> &super::HeadingResultNode {
    match node {
        QueryResultNode::Heading(node) => node,
        _ => panic!("expected heading node"),
    }
}

fn link_node(node: &QueryResultNode) -> &super::LinkResultNode {
    match node {
        QueryResultNode::Link(node) => node,
        _ => panic!("expected link node"),
    }
}

fn validated(query: &str) -> crate::query::ValidatedQuery {
    let parsed = parse_query(query).expect("query should parse");
    validate_query(parsed, &QueryValidationOptions::default()).expect("query should validate")
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
                    id: Some(12),
                    file_id,
                    parent_id: Some(11),
                    level: 2,
                    line_number: Some(6),
                    byte_start: 41,
                    byte_end: 70,
                    title: "Nested".to_string(),
                    title_raw: Some("Nested".to_string()),
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
                    breadcrumbs_json: "[\"Alpha Index\",\"Query Engine\",\"Nested\"]".to_string(),
                },
                OutlinePathRecord {
                    heading_id: 13,
                    file_id,
                    parent_id: Some(10),
                    depth: 1,
                    materialized_path: "0000.0002".to_string(),
                    breadcrumbs_json: "[\"Alpha Index\",\"Loose Note\"]".to_string(),
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
                    heading_id: 11,
                    key: "AREA".to_string(),
                    value: Some("infra".to_string()),
                    source: "property_drawer".to_string(),
                    append: false,
                    line_number: Some(4),
                },
                PropertyRecord {
                    heading_id: 13,
                    key: "APPEND_REPLACED".to_string(),
                    value: Some("first".to_string()),
                    source: "property_drawer".to_string(),
                    append: false,
                    line_number: Some(9),
                },
                PropertyRecord {
                    heading_id: 13,
                    key: "APPEND_REPLACED".to_string(),
                    value: Some("appended".to_string()),
                    source: "property_drawer".to_string(),
                    append: true,
                    line_number: Some(10),
                },
                PropertyRecord {
                    heading_id: 13,
                    key: "APPEND_REPLACED".to_string(),
                    value: Some("second".to_string()),
                    source: "property_drawer".to_string(),
                    append: false,
                    line_number: Some(11),
                },
            ],
        )?;
        Ok(())
    })
    .expect("alpha file should seed");
    seed_effective_properties(connection, alpha_file_id);

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
    .expect("beta outline should insert");
    DbWriter::insert_tags(
        connection,
        &[TagRecord {
            heading_id: 21,
            tag: "target".to_string(),
        }],
    )
    .expect("beta child tag should insert");
    seed_effective_tags(connection, beta_file_id);

    DbWriter::insert_links(
        connection,
        &[
            LinkRecord {
                id: Some(100),
                file_id: alpha_file_id,
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
            },
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
        ],
    )
    .expect("links should seed");

    connection
        .execute(
            "UPDATE links SET path_absolute = ?1, target_file_id = ?2, resolution_status = 'resolved' WHERE id = 100",
            rusqlite::params![beta_path.to_string_lossy().to_string(), beta_file_id],
        )
        .expect("file link should update");
    connection
        .execute(
            "UPDATE links SET path_absolute = ?1, target_file_id = ?2, target_heading_id = 21, resolution_status = 'resolved' WHERE id = 101",
            rusqlite::params![beta_path.to_string_lossy().to_string(), beta_file_id],
        )
        .expect("heading link should update");
    connection
        .execute(
            "UPDATE links SET path_absolute = ?1, target_file_id = ?2, resolution_status = 'resolved' WHERE id = 102",
            rusqlite::params![beta_path.to_string_lossy().to_string(), beta_file_id],
        )
        .expect("preamble link should update");
    connection
        .execute(
            "UPDATE links SET path_absolute = ?1, target_file_id = ?2, target_heading_id = 11, resolution_status = 'resolved' WHERE id = 103",
            rusqlite::params![alpha_path.to_string_lossy().to_string(), alpha_file_id],
        )
        .expect("backlink should update");
    connection
        .execute(
            "UPDATE links SET path_absolute = ?1, target_file_id = ?2, resolution_status = 'resolved' WHERE id = 104",
            rusqlite::params![alpha_path.to_string_lossy().to_string(), alpha_file_id],
        )
        .expect("root backlink should update");
    connection
        .execute(
            "UPDATE links SET resolution_status = 'broken', resolution_diagnostic = 'missing target' WHERE id = 105",
            [],
        )
        .expect("broken link should update");
}

fn table_counts(connection: &Connection) -> Vec<(&'static str, i64)> {
    [
        "files",
        "headings",
        "links",
        "tags",
        "properties",
        "keywords",
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
