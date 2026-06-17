use std::path::Path;

use org_files_db::parser::{
    OrgParser, OrgizeAdapter, ParseOptions, TodoKeyword, TodoKeywordConfig, TodoType,
};

#[test]
fn orgize_adapter_extracts_heading_basics_from_old_fixture() {
    let content = include_str!("data/parser/headings/nested-planning-lines/fixture.org");
    let document = OrgizeAdapter::new()
        .parse_document(
            Path::new("tests/data/parser/headings/nested-planning-lines/fixture.org"),
            content,
            &ParseOptions::default(),
        )
        .expect("orgize adapter should parse old heading fixture");

    assert_eq!(
        document.metadata.title.as_deref(),
        Some("Org Tests for Headings")
    );
    assert_eq!(document.headings.len(), 3);

    assert_eq!(document.headings[0].title, "Priortiy");
    assert_eq!(
        document.headings[0].file_path,
        Path::new("tests/data/parser/headings/nested-planning-lines/fixture.org")
    );
    assert_eq!(document.headings[0].level, 1);
    assert!(document.headings[0].is_root);
    assert_eq!(document.headings[0].line_number, Some(5));
    assert!(document.headings[0].byte_end > document.headings[0].byte_start);

    assert_eq!(document.headings[1].title, "Planning Info");
    assert_eq!(document.headings[1].level, 1);
    assert_eq!(document.headings[2].title, "Each on one Line");
    assert_eq!(document.headings[2].level, 2);
    assert_eq!(document.headings[2].parent_index, Some(1));
    assert!(document.headings[2].planning.scheduled.is_some());
    assert!(document.headings[2].planning.deadline.is_none());
    assert!(document.headings[2].planning.closed.is_none());
    assert!(document.diagnostics.is_empty());
}

#[test]
fn orgize_adapter_extracts_todo_priority_and_tags() {
    let content = include_str!("data/parser/priorities/todo-priority-tags/fixture.org");
    let document = OrgizeAdapter::new()
        .parse_document(
            Path::new("tests/data/parser/priorities/todo-priority-tags/fixture.org"),
            content,
            &ParseOptions::default(),
        )
        .expect("orgize adapter should parse todo/priority/tag fixture");

    assert_eq!(
        document.metadata.title.as_deref(),
        Some("Orgize Priority Fixture")
    );
    assert_eq!(document.headings.len(), 2);

    let first = &document.headings[0];
    assert_eq!(first.title, "Inbox");
    assert_eq!(first.todo_keyword.as_deref(), Some("TODO"));
    assert_eq!(first.todo_type, Some(TodoType::Open));
    assert_eq!(first.priority, Some('A'));
    assert_eq!(first.tags, vec!["rust".to_string(), "parser".to_string()]);
    assert!(first.is_root);
    assert_eq!(first.parent_index, None);

    let second = &document.headings[1];
    assert_eq!(second.title, "Child");
    assert_eq!(second.parent_index, Some(0));
    assert_eq!(second.todo_keyword.as_deref(), Some("DONE"));
    assert_eq!(second.todo_type, Some(TodoType::Closed));
    assert_eq!(second.priority, Some('B'));
    assert_eq!(second.tags, vec!["child".to_string()]);

    assert!(document.diagnostics.is_empty());
}

#[test]
fn orgize_adapter_uses_configured_project_todo_keywords() {
    let content = "* PLAN Parser fixture\n** DONE Implemented\n";
    let options = ParseOptions {
        todo_keywords: TodoKeywordConfig {
            open: vec![TodoKeyword::with_fast_key("PLAN", 'p')],
            closed: vec![TodoKeyword::with_fast_key("DONE", 'd')],
        },
    };

    let document = OrgizeAdapter::new()
        .parse_document(Path::new("notes/custom-todo.org"), content, &options)
        .expect("orgize adapter should parse configured todo keywords");

    assert_eq!(document.headings.len(), 2);
    assert_eq!(document.headings[0].todo_keyword.as_deref(), Some("PLAN"));
    assert_eq!(document.headings[0].todo_type, Some(TodoType::Open));
    assert_eq!(document.headings[0].title, "Parser fixture");
    assert_eq!(document.headings[1].todo_keyword.as_deref(), Some("DONE"));
    assert_eq!(document.headings[1].todo_type, Some(TodoType::Closed));
    assert_eq!(document.headings[1].title, "Implemented");
}
