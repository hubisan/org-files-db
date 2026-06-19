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
    assert_eq!(document.headings.len(), 4);

    assert_eq!(document.headings[0].level, 0);
    assert_eq!(document.headings[0].title, "Org Tests for Headings");
    assert_eq!(document.headings[0].title_raw, "Org Tests for Headings");
    assert_eq!(document.headings[0].byte_start, 0);
    assert_eq!(document.headings[0].byte_end, content.len());
    assert!(document.headings[0].is_root);

    assert_eq!(document.headings[1].title, "Priortiy");
    assert_eq!(
        document.headings[1].file_path,
        Path::new("tests/data/parser/headings/nested-planning-lines/fixture.org")
    );
    assert_eq!(document.headings[1].level, 1);
    assert!(!document.headings[1].is_root);
    assert_eq!(document.headings[1].parent_index, Some(0));
    assert_eq!(document.headings[1].line_number, Some(5));
    assert!(document.headings[1].byte_end > document.headings[1].byte_start);

    assert_eq!(document.headings[2].title, "Planning Info");
    assert_eq!(document.headings[2].level, 1);
    assert_eq!(document.headings[2].parent_index, Some(0));
    assert_eq!(document.headings[3].title, "Each on one Line");
    assert_eq!(document.headings[3].level, 2);
    assert_eq!(document.headings[3].parent_index, Some(2));
    assert!(document.headings[3].planning.scheduled.is_some());
    assert!(document.headings[3].planning.deadline.is_none());
    assert!(document.headings[3].planning.closed.is_none());
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
    assert_eq!(document.headings.len(), 3);

    let level0 = &document.headings[0];
    assert_eq!(level0.level, 0);
    assert_eq!(level0.title, "Orgize Priority Fixture");
    assert_eq!(level0.title_raw, "Orgize Priority Fixture");

    let first = &document.headings[1];
    assert_eq!(first.title, "Inbox");
    assert_eq!(first.todo_keyword.as_deref(), Some("TODO"));
    assert_eq!(first.todo_type, Some(TodoType::Open));
    assert_eq!(first.priority, Some('A'));
    assert_eq!(first.tags, vec!["rust".to_string(), "parser".to_string()]);
    assert!(!first.is_root);
    assert_eq!(first.parent_index, Some(0));

    let second = &document.headings[2];
    assert_eq!(second.title, "Child");
    assert_eq!(second.parent_index, Some(1));
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

    assert_eq!(document.headings.len(), 3);
    assert_eq!(document.headings[1].todo_keyword.as_deref(), Some("PLAN"));
    assert_eq!(document.headings[1].todo_type, Some(TodoType::Open));
    assert_eq!(document.headings[1].title, "Parser fixture");
    assert_eq!(document.headings[1].title_raw, "Parser fixture");
    assert_eq!(document.headings[2].todo_keyword.as_deref(), Some("DONE"));
    assert_eq!(document.headings[2].todo_type, Some(TodoType::Closed));
    assert_eq!(document.headings[2].title, "Implemented");
}

#[test]
fn orgize_adapter_prefers_file_local_todo_keywords_over_configured_defaults() {
    let content = include_str!("data/parser/todo-keywords/custom-sequence/fixture.org");
    let options = ParseOptions {
        todo_keywords: TodoKeywordConfig {
            open: vec![TodoKeyword::with_fast_key("TODO", 't')],
            closed: vec![TodoKeyword::with_fast_key("DONE", 'd')],
        },
    };

    let document = OrgizeAdapter::new()
        .parse_document(
            Path::new("tests/data/parser/todo-keywords/custom-sequence/fixture.org"),
            content,
            &options,
        )
        .expect("orgize adapter should respect file-local todo keywords");

    assert_eq!(document.headings.len(), 3);
    assert_eq!(document.headings[1].todo_keyword.as_deref(), Some("PLAN"));
    assert_eq!(document.headings[1].todo_type, Some(TodoType::Open));
    assert_eq!(document.headings[1].title, "Parser fixture");
    assert_eq!(document.headings[1].title_raw, "Parser fixture");
    assert_eq!(document.headings[2].todo_keyword.as_deref(), Some("DONE"));
    assert_eq!(document.headings[2].todo_type, Some(TodoType::Closed));
    assert_eq!(document.headings[2].title, "Implemented");
}

#[test]
fn orgize_adapter_treats_file_local_todo_keywords_as_overrides() {
    let content = "#+TODO: PLAN(p) | DONE(d)\n* PLAN me\n* DONE me\n* REVIEW Mist\n";
    let options = ParseOptions {
        todo_keywords: TodoKeywordConfig {
            open: vec![TodoKeyword::with_fast_key("REVIEW", 'r')],
            closed: vec![TodoKeyword::with_fast_key("DONE", 'd')],
        },
    };

    let document = OrgizeAdapter::new()
        .parse_document(Path::new("notes/override.org"), content, &options)
        .expect("orgize adapter should respect file-local todo keywords");

    assert_eq!(document.headings.len(), 4);
    assert_eq!(document.headings[1].todo_keyword.as_deref(), Some("PLAN"));
    assert_eq!(document.headings[1].todo_type, Some(TodoType::Open));
    assert_eq!(document.headings[1].title, "me");
    assert_eq!(document.headings[1].title_raw, "me");
    assert_eq!(document.headings[2].todo_keyword.as_deref(), Some("DONE"));
    assert_eq!(document.headings[2].todo_type, Some(TodoType::Closed));
    assert_eq!(document.headings[2].title, "me");
    assert_eq!(document.headings[2].title_raw, "me");
    assert_eq!(document.headings[3].todo_keyword, None);
    assert_eq!(document.headings[3].todo_type, None);
    assert_eq!(document.headings[3].title, "REVIEW Mist");
    assert_eq!(document.headings[3].title_raw, "REVIEW Mist");
}

#[test]
fn orgize_adapter_ignores_empty_document_title_keywords() {
    let content = "#+TITLE:\n#+TITLE:   Project Notes   \n#+STARTUP: showall\n#+TODO: TODO(t) NEXT(n) PLAN(p) | DONE(d) CANCEL(c)\n\n* REVIEW *Mist*\n\n* PLAN me\n\n* TODO me                                                              :test:\n\n** again                                                                :me:\n\n* DONE me\n";

    let document = OrgizeAdapter::new()
        .parse_document(
            Path::new("notes/manual-fixture.org"),
            content,
            &ParseOptions::default(),
        )
        .expect("manual fixture should parse");

    assert_eq!(document.metadata.title.as_deref(), Some("Project Notes"));
    assert_eq!(document.headings.len(), 6);
    assert_eq!(document.headings[1].title, "REVIEW Mist");
    assert_eq!(document.headings[1].title_raw, "REVIEW *Mist*");
    assert_eq!(document.headings[1].todo_keyword, None);
    assert_eq!(document.headings[1].todo_type, None);
    assert_eq!(document.headings[2].title, "me");
    assert_eq!(document.headings[2].title_raw, "me");
    assert_eq!(document.headings[2].todo_keyword.as_deref(), Some("PLAN"));
    assert_eq!(document.headings[2].todo_type, Some(TodoType::Open));
    assert_eq!(document.headings[3].title, "me");
    assert_eq!(document.headings[3].title_raw, "me");
    assert_eq!(document.headings[3].todo_keyword.as_deref(), Some("TODO"));
    assert_eq!(document.headings[3].todo_type, Some(TodoType::Open));
    assert_eq!(document.headings[4].title, "again");
    assert_eq!(document.headings[5].title, "me");
    assert_eq!(document.headings[5].title_raw, "me");
    assert_eq!(document.headings[5].todo_keyword.as_deref(), Some("DONE"));
    assert_eq!(document.headings[5].todo_type, Some(TodoType::Closed));
}

#[test]
fn orgize_adapter_combines_multiple_document_title_keywords_in_order() {
    let content = "#+TITLE: Title can span\n#+TITLE: multiple lines,\n#+AUTHOR: Hubisan\n\n* Unfortunately Everywhere\n\n#+TITLE: even here\n#+TITLE:   \n\n* Plain Heading\n";

    let document = OrgizeAdapter::new()
        .parse_document(
            Path::new("notes/multiple-title.org"),
            content,
            &ParseOptions::default(),
        )
        .expect("document with multiple titles should parse");

    assert_eq!(
        document.metadata.title.as_deref(),
        Some("Title can span multiple lines, even here")
    );
    assert_eq!(document.headings.len(), 3);
    assert_eq!(
        document.headings[0].title,
        "Title can span multiple lines, even here"
    );
    assert_eq!(
        document.headings[0].title_raw,
        "Title can span multiple lines, even here"
    );
    assert_eq!(document.headings[1].title, "Unfortunately Everywhere");
    assert_eq!(document.headings[1].title_raw, "Unfortunately Everywhere");
    assert_eq!(document.headings[2].title, "Plain Heading");
    assert_eq!(document.headings[2].title_raw, "Plain Heading");
}

#[test]
fn orgize_adapter_supports_simplified_file_local_todo_keyword_lines() {
    let content = include_str!("data/parser/todo-keywords/simplified-file-local-lines/fixture.org");
    let options = ParseOptions {
        todo_keywords: TodoKeywordConfig {
            open: vec![TodoKeyword::with_fast_key("TODO", 't')],
            closed: vec![TodoKeyword::with_fast_key("DONE", 'd')],
        },
    };

    let document = OrgizeAdapter::new()
        .parse_document(
            Path::new("tests/data/parser/todo-keywords/simplified-file-local-lines/fixture.org"),
            content,
            &options,
        )
        .expect("simplified file-local todo lines should parse");

    assert_eq!(document.headings.len(), 14);

    assert_eq!(document.headings[1].todo_keyword, None);
    assert_eq!(document.headings[1].todo_type, None);
    assert_eq!(
        document.headings[1].title,
        "TODO invalid keyword, even though it is a default it is overwritten"
    );
    assert_eq!(
        document.headings[1].title_raw,
        "TODO invalid keyword, even though it is a default it is overwritten"
    );

    assert_eq!(document.headings[2].todo_keyword, None);
    assert_eq!(document.headings[2].todo_type, None);
    assert_eq!(
        document.headings[2].title,
        "DONE invalid keyword, even though it is a default it is overwritten"
    );

    let expected = [
        (3, "one", TodoType::Open, "valid, type open"),
        (4, "two", TodoType::Open, "valid, type open"),
        (5, "three", TodoType::Closed, "valid, type closed"),
        (6, "four", TodoType::Closed, "valid, type closed"),
        (7, "FIVE", TodoType::Open, "valid, type open"),
        (8, "SIX", TodoType::Open, "valid, type open"),
        (9, "seven", TodoType::Open, "valid, type open"),
        (10, "eight", TodoType::Closed, "valid, type closed"),
        (11, "nine", TodoType::Open, "valid, type open"),
        (12, "ten", TodoType::Closed, "valid, type closed"),
        (13, "eleven", TodoType::Closed, "valid, type closed"),
    ];

    for (index, keyword, todo_type, expected_title) in expected {
        assert_eq!(
            document.headings[index].todo_keyword.as_deref(),
            Some(keyword)
        );
        assert_eq!(document.headings[index].todo_type, Some(todo_type));
        assert_eq!(document.headings[index].title, expected_title);
        assert_eq!(document.headings[index].title_raw, expected_title);
    }
}

#[test]
fn orgize_adapter_normalizes_described_link_titles() {
    let document = OrgizeAdapter::new()
        .parse_document(
            Path::new("notes/links.org"),
            "* [[file:natural/hausarzt-krebs-thomas.org][Thomas Krebs - Hausarzt]]\n",
            &ParseOptions::default(),
        )
        .expect("described link heading should parse");

    assert_eq!(document.headings[1].title, "Thomas Krebs - Hausarzt");
    assert_eq!(
        document.headings[1].title_raw,
        "[[file:natural/hausarzt-krebs-thomas.org][Thomas Krebs - Hausarzt]]"
    );
}

#[test]
fn orgize_adapter_normalizes_undescribed_link_titles() {
    let document = OrgizeAdapter::new()
        .parse_document(
            Path::new("notes/links.org"),
            "* [[file:natural/hausarzt-krebs-thomas.org]]\n",
            &ParseOptions::default(),
        )
        .expect("undescribed link heading should parse");

    assert_eq!(
        document.headings[1].title,
        "file:natural/hausarzt-krebs-thomas.org"
    );
    assert_eq!(
        document.headings[1].title_raw,
        "[[file:natural/hausarzt-krebs-thomas.org]]"
    );
}

#[test]
fn orgize_adapter_removes_basic_markup_from_display_title() {
    let document = OrgizeAdapter::new()
        .parse_document(
            Path::new("notes/markup.org"),
            "* *bold* /italic/ _underline_ =code= ~verbatim~\n",
            &ParseOptions::default(),
        )
        .expect("markup heading should parse");

    assert_eq!(
        document.headings[1].title,
        "bold italic underline code verbatim"
    );
    assert_eq!(
        document.headings[1].title_raw,
        "*bold* /italic/ _underline_ =code= ~verbatim~"
    );
}

#[test]
fn orgize_adapter_leaves_plain_heading_titles_unchanged() {
    let document = OrgizeAdapter::new()
        .parse_document(
            Path::new("notes/plain.org"),
            "* Plain Heading\n",
            &ParseOptions::default(),
        )
        .expect("plain heading should parse");

    assert_eq!(document.headings[1].title, "Plain Heading");
    assert_eq!(document.headings[1].title_raw, "Plain Heading");
}

#[test]
fn orgize_adapter_uses_document_title_for_synthetic_level_zero_heading() {
    let document = OrgizeAdapter::new()
        .parse_document(
            Path::new("notes/project.org"),
            "#+TITLE: Project Dashboard\n* Heading\n",
            &ParseOptions::default(),
        )
        .expect("document title should parse");

    assert_eq!(document.headings[0].title, "Project Dashboard");
    assert_eq!(document.headings[0].title_raw, "Project Dashboard");
}

#[test]
fn orgize_adapter_falls_back_to_file_stem_for_synthetic_level_zero_heading() {
    let document = OrgizeAdapter::new()
        .parse_document(
            Path::new("notes/project.org"),
            "* Heading\n",
            &ParseOptions::default(),
        )
        .expect("document without title should parse");

    assert_eq!(document.headings[0].title, "project");
    assert_eq!(document.headings[0].title_raw, "project");
}
