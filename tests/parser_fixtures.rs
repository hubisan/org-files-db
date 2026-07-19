use std::{
    collections::HashMap,
    fs,
    path::{Path, PathBuf},
};

use org_files_db::parser::{OrgParser, OrgizeAdapter, ParseOptions};

fn fixture_root() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("tests/data/parser")
}

fn collect_fixture_dirs() -> Vec<PathBuf> {
    let categories = [
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

    let mut fixtures = Vec::new();
    for category in categories {
        let category_path = fixture_root().join(category);
        let entries = fs::read_dir(&category_path)
            .unwrap_or_else(|err| panic!("failed to read {}: {err}", category_path.display()));

        for entry in entries {
            let path = entry.expect("fixture category entry should load").path();
            if path.is_dir() {
                fixtures.push(path);
            }
        }
    }

    fixtures.sort();
    fixtures
}

fn load_expectations(path: &Path) -> HashMap<String, String> {
    let content = fs::read_to_string(path)
        .unwrap_or_else(|err| panic!("failed to read {}: {err}", path.display()));
    let mut expectations = HashMap::new();

    for line in content.lines() {
        let trimmed = line.trim();
        if trimmed.is_empty() || trimmed.starts_with('#') {
            continue;
        }

        let (key, value) = trimmed.split_once('=').unwrap_or_else(|| {
            panic!(
                "malformed expectation line in {}: {trimmed}",
                path.display()
            )
        });
        expectations.insert(key.to_string(), value.to_string());
    }

    expectations
}

fn decode_expected_text(value: &str) -> String {
    value.replace("\\n", "\n")
}

fn assert_expectations(path: &Path, expectations: &HashMap<String, String>) {
    let content = fs::read_to_string(path.join("fixture.org"))
        .unwrap_or_else(|err| panic!("failed to read fixture {}: {err}", path.display()));
    let parser = OrgizeAdapter::new();
    let document = parser
        .parse_document(
            Path::new(
                path.strip_prefix(fixture_root())
                    .expect("fixture path should be relative"),
            ),
            &content,
            &ParseOptions::default(),
        )
        .unwrap_or_else(|err| {
            panic!(
                "fixture {} should parse successfully: {err:?}",
                path.display()
            )
        });

    for (key, value) in expectations {
        match key.as_str() {
            "document.title" => assert_eq!(
                document.metadata.title.as_deref(),
                Some(value.as_str()),
                "unexpected title for {}",
                path.display()
            ),
            "document.keyword_count" => assert_eq!(
                document.metadata.keywords.len(),
                value
                    .parse::<usize>()
                    .expect("document.keyword_count should be numeric"),
                "unexpected keyword count for {}",
                path.display()
            ),
            "heading_count" => assert_eq!(
                document.headings.len(),
                value
                    .parse::<usize>()
                    .expect("heading_count should be numeric"),
                "unexpected heading count for {}",
                path.display()
            ),
            "diagnostic_count" => assert_eq!(
                document.diagnostics.len(),
                value
                    .parse::<usize>()
                    .expect("diagnostic_count should be numeric"),
                "unexpected diagnostic count for {}",
                path.display()
            ),
            key if key.starts_with("document.keyword.") => {
                assert_keyword_expectation(path, &document.metadata.keywords, key, value);
            }
            key if key.starts_with("heading.") => {
                assert_heading_expectation(path, &document.headings, key, value);
            }
            _ => panic!("unsupported expectation key {key} in {}", path.display()),
        }
    }
}

fn assert_keyword_expectation(
    path: &Path,
    keywords: &[org_files_db::parser::ParsedKeyword],
    key: &str,
    value: &str,
) {
    let remainder = key.trim_start_matches("document.keyword.");

    if let Some((index, field)) = remainder.split_once('.') {
        let keyword = &keywords[index
            .parse::<usize>()
            .expect("document keyword index should be numeric")];

        match field {
            "key" => assert_eq!(
                keyword.key,
                value,
                "unexpected keyword key for {}",
                path.display()
            ),
            "value" => {
                let expected = if value == "NULL" { None } else { Some(value) };
                assert_eq!(
                    keyword.value.as_deref(),
                    expected,
                    "unexpected keyword value for {}",
                    path.display()
                );
            }
            "line_number" => assert_eq!(
                keyword.line_number,
                Some(
                    value
                        .parse::<u32>()
                        .expect("document keyword line_number should be numeric")
                ),
                "unexpected keyword line number for {}",
                path.display()
            ),
            _ => panic!(
                "unsupported document keyword expectation key {key} in {}",
                path.display()
            ),
        }
    } else {
        let actual = keywords
            .iter()
            .find(|keyword| keyword.key == remainder)
            .and_then(|keyword| keyword.value.as_deref());
        assert_eq!(
            actual,
            Some(value),
            "unexpected keyword {remainder} for {}",
            path.display()
        );
    }
}

fn assert_heading_expectation(
    path: &Path,
    headings: &[org_files_db::parser::ParsedHeading],
    key: &str,
    value: &str,
) {
    let remainder = key.trim_start_matches("heading.");
    let (index, field) = remainder.split_once('.').unwrap_or_else(|| {
        panic!(
            "malformed heading expectation key {key} in {}",
            path.display()
        )
    });
    let heading = &headings[index
        .parse::<usize>()
        .expect("heading index should be numeric")];

    match field {
        "level" => assert_eq!(
            heading.level,
            value
                .parse::<u8>()
                .expect("heading level should be numeric"),
            "unexpected heading level for {}",
            path.display()
        ),
        "title" => assert_eq!(
            heading.title,
            value,
            "unexpected heading title for {}",
            path.display()
        ),
        "body_text" => assert!(
            {
                let expected = decode_expected_text(value);
                heading.body_text.as_deref() == Some(expected.as_str())
            },
            "unexpected heading body text for {}",
            path.display()
        ),
        "body_byte_start" => assert_eq!(
            heading.body_byte_start,
            Some(
                value
                    .parse::<usize>()
                    .expect("body_byte_start should be numeric")
            ),
            "unexpected heading body byte start for {}",
            path.display()
        ),
        "body_byte_end" => assert_eq!(
            heading.body_byte_end,
            Some(
                value
                    .parse::<usize>()
                    .expect("body_byte_end should be numeric")
            ),
            "unexpected heading body byte end for {}",
            path.display()
        ),
        "todo_keyword" => assert_eq!(
            heading.todo_keyword.as_deref(),
            Some(value),
            "unexpected TODO keyword for {}",
            path.display()
        ),
        "priority" => assert_eq!(
            heading.priority,
            Some(value.to_string()),
            "unexpected priority for {}",
            path.display()
        ),
        "tags" => {
            let expected: Vec<String> = if value.is_empty() {
                Vec::new()
            } else {
                value.split(',').map(|tag| tag.to_string()).collect()
            };
            assert_eq!(
                heading.tags,
                expected,
                "unexpected tags for {}",
                path.display()
            );
        }
        "is_root" => assert_eq!(
            heading.is_root,
            value.parse::<bool>().expect("is_root should be a boolean"),
            "unexpected root flag for {}",
            path.display()
        ),
        "line_number" => assert_eq!(
            heading.line_number,
            Some(value.parse::<u32>().expect("line_number should be numeric")),
            "unexpected line number for {}",
            path.display()
        ),
        "parent_index" => assert_eq!(
            heading.parent_index,
            Some(
                value
                    .parse::<usize>()
                    .expect("parent_index should be numeric")
            ),
            "unexpected parent index for {}",
            path.display()
        ),
        "property_count" => assert_eq!(
            heading.properties.len(),
            value
                .parse::<usize>()
                .expect("property_count should be numeric"),
            "unexpected property count for {}",
            path.display()
        ),
        field if field.starts_with("property.") => {
            assert_property_expectation(path, heading, field, value);
        }
        "timestamp_count" => assert_eq!(
            heading.timestamps.len(),
            value
                .parse::<usize>()
                .expect("timestamp_count should be numeric"),
            "unexpected timestamp count for {}",
            path.display()
        ),
        "planning.scheduled" | "planning.scheduled_raw" => assert_eq!(
            heading.planning.scheduled_raw(),
            Some(value),
            "unexpected scheduled value for {}",
            path.display()
        ),
        "planning.scheduled_ts" => assert_eq!(
            heading.planning.scheduled_ts(),
            Some(
                value
                    .parse::<i64>()
                    .expect("scheduled_ts should be numeric")
            ),
            "unexpected scheduled timestamp for {}",
            path.display()
        ),
        "planning.deadline" | "planning.deadline_raw" => assert_eq!(
            heading.planning.deadline_raw(),
            Some(value),
            "unexpected deadline value for {}",
            path.display()
        ),
        "planning.deadline_ts" => assert_eq!(
            heading.planning.deadline_ts(),
            Some(value.parse::<i64>().expect("deadline_ts should be numeric")),
            "unexpected deadline timestamp for {}",
            path.display()
        ),
        "planning.closed" | "planning.closed_raw" => assert_eq!(
            heading.planning.closed_raw(),
            Some(value),
            "unexpected closed value for {}",
            path.display()
        ),
        "planning.closed_ts" => assert_eq!(
            heading.planning.closed_ts(),
            Some(value.parse::<i64>().expect("closed_ts should be numeric")),
            "unexpected closed timestamp for {}",
            path.display()
        ),
        field if field.starts_with("timestamp.") => {
            assert_timestamp_expectation(path, heading, field, value);
        }
        _ => panic!(
            "unsupported heading expectation key {key} in {}",
            path.display()
        ),
    }
}

fn assert_property_expectation(
    path: &Path,
    heading: &org_files_db::parser::ParsedHeading,
    field: &str,
    value: &str,
) {
    let remainder = field.trim_start_matches("property.");
    let (index, field) = remainder.split_once('.').unwrap_or_else(|| {
        panic!(
            "malformed property expectation key heading.{field} in {}",
            path.display()
        )
    });
    let property = &heading.properties[index
        .parse::<usize>()
        .expect("property index should be numeric")];

    match field {
        "key" => assert_eq!(
            property.key,
            value,
            "unexpected property key for {}",
            path.display()
        ),
        "value" => {
            let expected = if value == "NULL" { None } else { Some(value) };
            assert_eq!(
                property.value.as_deref(),
                expected,
                "unexpected property value for {}",
                path.display()
            );
        }
        "source" => assert_eq!(
            normalize_property_source_name(property.source),
            value,
            "unexpected property source for {}",
            path.display()
        ),
        "append" => assert_eq!(
            property.append,
            value.parse::<bool>().expect("append should be a boolean"),
            "unexpected property append flag for {}",
            path.display()
        ),
        "line_number" => assert_eq!(
            property.line_number,
            Some(
                value
                    .parse::<u32>()
                    .expect("property line_number should be numeric")
            ),
            "unexpected property line number for {}",
            path.display()
        ),
        _ => panic!(
            "unsupported property expectation key heading.{field} in {}",
            path.display()
        ),
    }
}

fn assert_timestamp_expectation(
    path: &Path,
    heading: &org_files_db::parser::ParsedHeading,
    field: &str,
    value: &str,
) {
    let remainder = field.trim_start_matches("timestamp.");
    let (index, field) = remainder.split_once('.').unwrap_or_else(|| {
        panic!(
            "malformed timestamp expectation key heading.{field} in {}",
            path.display()
        )
    });
    let timestamp = &heading.timestamps[index
        .parse::<usize>()
        .expect("timestamp index should be numeric")];

    match field {
        "role" => assert_eq!(
            timestamp.role.map(normalize_role_name),
            Some(value),
            "unexpected timestamp role for {}",
            path.display()
        ),
        "type" => assert_eq!(
            normalize_type_name(timestamp.timestamp_type),
            value,
            "unexpected timestamp type for {}",
            path.display()
        ),
        "range_type" => assert_eq!(
            normalize_range_type_name(timestamp.range_type),
            value,
            "unexpected timestamp range_type for {}",
            path.display()
        ),
        "raw_value" => assert_eq!(
            timestamp.raw_value,
            value,
            "unexpected timestamp raw_value for {}",
            path.display()
        ),
        "start_ts" => assert_eq!(
            timestamp.start_ts,
            Some(value.parse::<i64>().expect("start_ts should be numeric")),
            "unexpected timestamp start_ts for {}",
            path.display()
        ),
        "end_ts" => assert_eq!(
            timestamp.end_ts,
            Some(value.parse::<i64>().expect("end_ts should be numeric")),
            "unexpected timestamp end_ts for {}",
            path.display()
        ),
        "modifier_count" => assert_eq!(
            timestamp.modifiers.len(),
            value
                .parse::<usize>()
                .expect("modifier_count should be numeric"),
            "unexpected timestamp modifier count for {}",
            path.display()
        ),
        field if field.starts_with("modifier.") => {
            assert_modifier_expectation(path, timestamp, field, value);
        }
        _ => panic!(
            "unsupported timestamp expectation key heading.{field} in {}",
            path.display()
        ),
    }
}

fn assert_modifier_expectation(
    path: &Path,
    timestamp: &org_files_db::parser::ParsedTimestamp,
    field: &str,
    value: &str,
) {
    let remainder = field.trim_start_matches("modifier.");
    let (index, field) = remainder.split_once('.').unwrap_or_else(|| {
        panic!(
            "malformed modifier expectation key heading.timestamp.{field} in {}",
            path.display()
        )
    });
    let modifier = &timestamp.modifiers[index
        .parse::<usize>()
        .expect("modifier index should be numeric")];

    match field {
        "kind" => assert_eq!(
            normalize_modifier_kind_name(modifier.kind),
            value,
            "unexpected modifier kind for {}",
            path.display()
        ),
        "type" => assert_eq!(
            normalize_modifier_type_name(modifier.modifier_type),
            value,
            "unexpected modifier type for {}",
            path.display()
        ),
        "value" => assert_eq!(
            modifier.value,
            value
                .parse::<i64>()
                .expect("modifier value should be numeric"),
            "unexpected modifier value for {}",
            path.display()
        ),
        "unit" => assert_eq!(
            normalize_modifier_unit_name(modifier.unit),
            value,
            "unexpected modifier unit for {}",
            path.display()
        ),
        "repeater_deadline_value" => assert_eq!(
            modifier.repeater_deadline_value,
            Some(
                value
                    .parse::<i64>()
                    .expect("repeater_deadline_value should be numeric")
            ),
            "unexpected modifier repeater_deadline_value for {}",
            path.display()
        ),
        "repeater_deadline_unit" => assert_eq!(
            modifier
                .repeater_deadline_unit
                .map(normalize_modifier_unit_name),
            Some(value),
            "unexpected modifier repeater_deadline_unit for {}",
            path.display()
        ),
        _ => panic!(
            "unsupported modifier expectation key heading.timestamp.{field} in {}",
            path.display()
        ),
    }
}

fn normalize_role_name(role: org_files_db::parser::ParsedTimestampRole) -> &'static str {
    match role {
        org_files_db::parser::ParsedTimestampRole::Scheduled => "scheduled",
        org_files_db::parser::ParsedTimestampRole::Deadline => "deadline",
        org_files_db::parser::ParsedTimestampRole::Closed => "closed",
        org_files_db::parser::ParsedTimestampRole::Body => "body",
    }
}

fn normalize_type_name(timestamp_type: org_files_db::parser::ParsedTimestampType) -> &'static str {
    match timestamp_type {
        org_files_db::parser::ParsedTimestampType::Active => "active",
        org_files_db::parser::ParsedTimestampType::Inactive => "inactive",
        org_files_db::parser::ParsedTimestampType::Diary => "diary",
    }
}

fn normalize_range_type_name(
    range_type: org_files_db::parser::ParsedTimestampRangeType,
) -> &'static str {
    match range_type {
        org_files_db::parser::ParsedTimestampRangeType::None => "none",
        org_files_db::parser::ParsedTimestampRangeType::DateRange => "date_range",
        org_files_db::parser::ParsedTimestampRangeType::TimeRange => "time_range",
        org_files_db::parser::ParsedTimestampRangeType::DateTimeRange => "datetime_range",
        org_files_db::parser::ParsedTimestampRangeType::Unknown => "unknown",
    }
}

fn normalize_modifier_kind_name(
    kind: org_files_db::parser::ParsedTimestampModifierKind,
) -> &'static str {
    match kind {
        org_files_db::parser::ParsedTimestampModifierKind::Repeater => "repeater",
        org_files_db::parser::ParsedTimestampModifierKind::Warning => "warning",
    }
}

fn normalize_modifier_type_name(
    modifier_type: org_files_db::parser::ParsedTimestampModifierType,
) -> String {
    match modifier_type {
        org_files_db::parser::ParsedTimestampModifierType::Cumulate => "cumulate".to_string(),
        org_files_db::parser::ParsedTimestampModifierType::CatchUp => "catch_up".to_string(),
        org_files_db::parser::ParsedTimestampModifierType::Restart => "restart".to_string(),
        org_files_db::parser::ParsedTimestampModifierType::All => "all".to_string(),
        org_files_db::parser::ParsedTimestampModifierType::First => "first".to_string(),
    }
}

fn normalize_modifier_unit_name(unit: org_files_db::parser::ParsedTimestampUnit) -> &'static str {
    match unit {
        org_files_db::parser::ParsedTimestampUnit::Hour => "hour",
        org_files_db::parser::ParsedTimestampUnit::Day => "day",
        org_files_db::parser::ParsedTimestampUnit::Week => "week",
        org_files_db::parser::ParsedTimestampUnit::Month => "month",
        org_files_db::parser::ParsedTimestampUnit::Year => "year",
    }
}

fn normalize_property_source_name(
    source: org_files_db::parser::ParsedPropertySource,
) -> &'static str {
    match source {
        org_files_db::parser::ParsedPropertySource::PropertyDrawer => "property_drawer",
        org_files_db::parser::ParsedPropertySource::PropertyKeyword => "property_keyword",
        org_files_db::parser::ParsedPropertySource::CategoryKeyword => "category_keyword",
    }
}

#[test]
fn parser_fixture_directories_follow_expected_layout() {
    for category in [
        "body",
        "timestamps",
        "headings",
        "links",
        "planning",
        "properties",
        "priorities",
        "todo-keywords",
        "file-scope",
    ] {
        assert!(
            fixture_root().join(category).is_dir(),
            "missing fixture category {}",
            category
        );
    }

    for fixture_dir in collect_fixture_dirs() {
        let org_file = fixture_dir.join("fixture.org");
        let expected_file = fixture_dir.join("expected.txt");
        assert!(org_file.is_file(), "missing {}", org_file.display());
        assert!(
            expected_file.is_file(),
            "missing {}",
            expected_file.display()
        );

        let expected_content = fs::read_to_string(&expected_file)
            .unwrap_or_else(|err| panic!("failed to read {}: {err}", expected_file.display()));
        assert!(
            expected_content.contains("# classification:"),
            "missing classification comment in {}",
            expected_file.display()
        );
        assert!(
            expected_content.lines().any(|line| line.contains('=')),
            "missing machine-readable expectations in {}",
            expected_file.display()
        );
    }
}

#[test]
fn orgize_adapter_smoke_tests_structured_fixtures() {
    for fixture_dir in collect_fixture_dirs() {
        let expectations = load_expectations(&fixture_dir.join("expected.txt"));
        assert_expectations(&fixture_dir, &expectations);
    }
}
