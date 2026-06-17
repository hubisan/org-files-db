use std::{
    collections::HashMap,
    fs,
    path::{Path, PathBuf},
};

use org_files_db::parser::{OrgParser, OrgizeAdapter};

fn fixture_root() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("tests/data/parser")
}

fn collect_fixture_dirs() -> Vec<PathBuf> {
    let categories = [
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
                let wanted_key = key.trim_start_matches("document.keyword.");
                let actual = document
                    .metadata
                    .keywords
                    .iter()
                    .find(|keyword| keyword.key == wanted_key)
                    .and_then(|keyword| keyword.value.as_deref());
                assert_eq!(
                    actual,
                    Some(value.as_str()),
                    "unexpected keyword {wanted_key} for {}",
                    path.display()
                );
            }
            key if key.starts_with("heading.") => {
                assert_heading_expectation(path, &document.headings, key, value);
            }
            _ => panic!("unsupported expectation key {key} in {}", path.display()),
        }
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
        "todo_keyword" => assert_eq!(
            heading.todo_keyword.as_deref(),
            Some(value),
            "unexpected TODO keyword for {}",
            path.display()
        ),
        "priority" => assert_eq!(
            heading.priority,
            value.chars().next(),
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
        "planning.scheduled" => assert_eq!(
            heading.planning.scheduled.as_deref(),
            Some(value),
            "unexpected scheduled value for {}",
            path.display()
        ),
        "planning.deadline" => assert_eq!(
            heading.planning.deadline.as_deref(),
            Some(value),
            "unexpected deadline value for {}",
            path.display()
        ),
        "planning.closed" => assert_eq!(
            heading.planning.closed.as_deref(),
            Some(value),
            "unexpected closed value for {}",
            path.display()
        ),
        _ => panic!(
            "unsupported heading expectation key {key} in {}",
            path.display()
        ),
    }
}

#[test]
fn parser_fixture_directories_follow_expected_layout() {
    for category in [
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
