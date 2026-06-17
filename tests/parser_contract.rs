use std::path::{Path, PathBuf};

use org_files_db::parser::{
    DiagnosticSeverity, OrgParser, OrgizeAdapter, ParseDiagnostic, ParseOptions,
    ParsedDocumentMetadata, ParsedHeading, ParsedKeyword, ParsedOrgDocument, ParsedPlanning,
    ParsedProperty, TodoKeyword, TodoKeywordConfig, TodoType,
};

struct ParserFixture {
    path: PathBuf,
    content: &'static str,
    expected_title: Option<&'static str>,
    expected_heading_titles: Vec<&'static str>,
    expected_priorities: Vec<Option<char>>,
    expected_tags: Vec<Vec<&'static str>>,
    expected_diagnostics: usize,
}

fn assert_fixture<P>(parser: &P, fixture: &ParserFixture)
where
    P: OrgParser,
{
    let document = parser
        .parse_document(&fixture.path, fixture.content, &ParseOptions::default())
        .expect("fixture parser should succeed");

    assert_eq!(
        document.metadata.title.as_deref(),
        fixture.expected_title,
        "unexpected document title"
    );
    assert_eq!(
        document.headings.len(),
        fixture.expected_heading_titles.len(),
        "unexpected heading count"
    );
    assert_eq!(
        document.diagnostics.len(),
        fixture.expected_diagnostics,
        "unexpected diagnostics count"
    );

    for (index, heading) in document.headings.iter().enumerate() {
        assert_eq!(
            heading.title, fixture.expected_heading_titles[index],
            "unexpected heading title at index {index}"
        );
        assert_eq!(
            heading.priority, fixture.expected_priorities[index],
            "unexpected priority at index {index}"
        );

        let expected_tags: Vec<String> = fixture.expected_tags[index]
            .iter()
            .map(|tag| (*tag).to_string())
            .collect();
        assert_eq!(
            heading.tags, expected_tags,
            "unexpected tags at index {index}"
        );
    }
}

struct StubFixtureParser;

impl OrgParser for StubFixtureParser {
    fn parse_document(
        &self,
        path: &Path,
        content: &str,
        _options: &ParseOptions,
    ) -> Result<ParsedOrgDocument, ParseDiagnostic> {
        let mut document = ParsedOrgDocument::new(path);

        if let Some(title) = content
            .lines()
            .find_map(|line| line.strip_prefix("#+TITLE: "))
            .map(str::to_string)
        {
            document.metadata.title = Some(title);
        }

        let mut heading = ParsedHeading::new(path, 1, "Inbox", 14, content.len());
        heading.title_raw = "TODO [#A] Inbox :rust:parser:".to_string();
        heading.todo_keyword = Some("TODO".to_string());
        heading.todo_type = Some(TodoType::Open);
        heading.priority = Some('A');
        heading.tags = vec!["rust".to_string(), "parser".to_string()];
        heading.properties = vec![ParsedProperty {
            key: "CUSTOM_ID".to_string(),
            value: "parser-inbox".to_string(),
            inherited: false,
        }];
        heading.planning = ParsedPlanning {
            scheduled: Some("<2026-06-16 Tue>".to_string()),
            deadline: None,
            closed: None,
        };
        heading.line_number = Some(2);
        heading.is_root = true;

        document.metadata.keywords.push(ParsedKeyword {
            key: "TITLE".to_string(),
            value: document.metadata.title.clone(),
        });
        document.headings.push(heading);
        document.diagnostics.push(
            ParseDiagnostic::warning("fixture parser ignores body content")
                .with_file_path(path)
                .with_line_number(2)
                .with_byte_range(14, content.len()),
        );

        Ok(document)
    }
}

#[test]
fn parsed_org_document_supports_schema_near_metadata() {
    let mut document = ParsedOrgDocument::new("notes/project.org");
    document.metadata = ParsedDocumentMetadata {
        title: Some("Project Notes".to_string()),
        keywords: vec![ParsedKeyword {
            key: "FILETAGS".to_string(),
            value: Some(":project:rust:".to_string()),
        }],
    };

    let mut heading = ParsedHeading::new("notes/project.org", 2, "Parser model", 32, 58);
    heading.title_raw = "TODO [#B] Parser model".to_string();
    heading.todo_keyword = Some("TODO".to_string());
    heading.todo_type = Some(TodoType::Open);
    heading.priority = Some('B');
    heading.tags = vec!["project".to_string(), "rust".to_string()];
    heading.properties = vec![ParsedProperty {
        key: "OWNER".to_string(),
        value: "hubisan".to_string(),
        inherited: false,
    }];
    heading.planning = ParsedPlanning {
        scheduled: Some("<2026-06-17 Wed>".to_string()),
        deadline: Some("<2026-06-20 Sat>".to_string()),
        closed: None,
    };
    heading.parent_index = Some(0);
    heading.is_archived = true;

    document.headings.push(heading.clone());

    assert_eq!(document.file_path, PathBuf::from("notes/project.org"));
    assert_eq!(
        document.headings[0].file_path,
        PathBuf::from("notes/project.org")
    );
    assert_eq!(document.metadata.title.as_deref(), Some("Project Notes"));
    assert_eq!(document.metadata.keywords.len(), 1);
    assert_eq!(document.headings[0], heading);
    assert_eq!(
        document.headings[0].planning.deadline.as_deref(),
        Some("<2026-06-20 Sat>")
    );
}

#[test]
fn diagnostics_can_be_collected_on_successful_parse() {
    let fixture = ParserFixture {
        path: PathBuf::from("tests/data/parser/headings/basic-heading/fixture.org"),
        content: include_str!("data/parser/headings/basic-heading/fixture.org"),
        expected_title: Some("Basic Heading Fixture"),
        expected_heading_titles: vec!["Inbox"],
        expected_priorities: vec![Some('A')],
        expected_tags: vec![vec!["rust", "parser"]],
        expected_diagnostics: 1,
    };

    assert_fixture(&StubFixtureParser, &fixture);
}

#[test]
fn diagnostics_builders_preserve_location_information() {
    let diagnostic = ParseDiagnostic::error("unsupported planning syntax")
        .with_file_path("notes/project.org")
        .with_line_number(7)
        .with_byte_range(120, 140);

    assert_eq!(diagnostic.severity, DiagnosticSeverity::Error);
    assert_eq!(
        diagnostic.file_path,
        Some(PathBuf::from("notes/project.org"))
    );
    assert_eq!(diagnostic.line_number, Some(7));
    assert_eq!(diagnostic.byte_range, Some((120, 140)));
}

#[test]
fn orgize_adapter_returns_internal_document_type() {
    let document = OrgizeAdapter::new()
        .parse_document(
            Path::new("notes/project.org"),
            "* Heading",
            &ParseOptions::default(),
        )
        .expect("adapter placeholder should succeed");

    assert_eq!(document.file_path, PathBuf::from("notes/project.org"));
    assert!(document.metadata.title.is_none());
    assert_eq!(document.headings.len(), 2);
    assert_eq!(document.headings[0].level, 0);
    assert_eq!(document.headings[0].title, "notes/project.org");
    assert_eq!(document.headings[1].title, "Heading");
    assert_eq!(document.headings[1].parent_index, Some(0));
    assert!(document.headings[0].is_root);
    assert!(document.diagnostics.is_empty());
}

#[test]
fn parse_options_default_to_org_mode_todo_keywords() {
    let options = ParseOptions::default();

    assert_eq!(
        options.todo_keywords,
        TodoKeywordConfig {
            open: vec![TodoKeyword::new("TODO")],
            closed: vec![TodoKeyword::new("DONE")],
        }
    );
}

#[test]
fn parsed_heading_serializes_with_expected_field_names() {
    let mut heading = ParsedHeading::new("notes/project.org", 0, "Project", 0, 12);
    heading.title_raw = "Project".to_string();
    heading.todo_keyword = Some("TODO".to_string());
    heading.todo_type = Some(TodoType::Open);
    heading.priority = Some('A');
    heading.tags = vec!["project".to_string(), "root".to_string()];
    heading.line_number = Some(1);
    heading.is_root = true;

    let json = serde_json::to_value(&heading).expect("heading should serialize");

    assert_eq!(json["file_path"], "notes/project.org");
    assert_eq!(json["level"], 0);
    assert_eq!(json["title"], "Project");
    assert_eq!(json["title_raw"], "Project");
    assert_eq!(json["todo_keyword"], "TODO");
    assert_eq!(json["todo_type"], "Open");
    assert_eq!(json["priority"], "A");
    assert_eq!(json["tags"], serde_json::json!(["project", "root"]));
    assert_eq!(json["line_number"], 1);
    assert_eq!(json["byte_start"], 0);
    assert_eq!(json["byte_end"], 12);
    assert!(json.get("begin").is_none());
    assert!(json.get("end").is_none());
}

#[test]
fn parsed_heading_can_represent_synthetic_level_zero_heading() {
    let heading = ParsedHeading::new("notes/project.org", 0, "notes/project.org", 0, 0);

    assert_eq!(heading.level, 0);
    assert_eq!(heading.parent_index, None);
    assert_eq!(heading.file_path, PathBuf::from("notes/project.org"));
}
