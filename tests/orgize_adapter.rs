use std::path::Path;

use org_files_db::parser::{
    LinkScannerConfig, OrgParser, OrgParserCore, OrgizeAdapter, ParseOptions,
    ParsedLinkSourceContext, ParsedPropertySource, ParsedTimestampModifierKind,
    ParsedTimestampModifierType, ParsedTimestampRangeType, ParsedTimestampRole,
    ParsedTimestampType, ParsedTimestampUnit, TodoKeyword, TodoKeywordConfig, TodoType,
};
use org_files_db::todo_keywords::resolve_todo_keywords;

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
    assert_eq!(
        document.headings[0].title_raw.as_deref(),
        Some("Org Tests for Headings")
    );
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
    assert_eq!(
        document.headings[3].planning.scheduled_raw(),
        Some("<2023-11-10>")
    );
    assert_eq!(
        document.headings[3].planning.scheduled_ts(),
        Some(1_699_574_400)
    );
    assert!(document.headings[3].planning.deadline_raw().is_none());
    assert!(document.headings[3].planning.deadline_ts().is_none());
    assert!(document.headings[3].planning.closed_raw().is_none());
    assert!(document.headings[3].planning.closed_ts().is_none());
    assert!(document.diagnostics.is_empty());
}

#[test]
fn orgize_adapter_extracts_each_planning_keyword_with_normalized_timestamps() {
    let content = "#+TITLE: Planning Keywords\n* Scheduled\nSCHEDULED: <2024-11-20 Wed>\n* Deadline\nDEADLINE: <2024-12-01 Sun 10:30>\n* Closed\nCLOSED: [2024-12-02 Mon]\n";

    let document = OrgizeAdapter::new()
        .parse_document(
            Path::new("notes/planning-keywords.org"),
            content,
            &ParseOptions::default(),
        )
        .expect("planning keywords should parse");

    assert_eq!(document.headings.len(), 4);
    assert_eq!(document.headings[0].title, "Planning Keywords");

    assert_eq!(document.headings[1].title, "Scheduled");
    assert_eq!(
        document.headings[1].planning.scheduled_raw(),
        Some("<2024-11-20 Wed>")
    );
    assert_eq!(
        document.headings[1].planning.scheduled_ts(),
        Some(1_732_060_800)
    );
    assert!(document.headings[1].planning.deadline_raw().is_none());
    assert!(document.headings[1].planning.closed_raw().is_none());

    assert_eq!(document.headings[2].title, "Deadline");
    assert_eq!(
        document.headings[2].planning.deadline_raw(),
        Some("<2024-12-01 Sun 10:30>")
    );
    assert_eq!(
        document.headings[2].planning.deadline_ts(),
        Some(1_733_049_000)
    );
    assert!(document.headings[2].planning.scheduled_raw().is_none());
    assert!(document.headings[2].planning.closed_raw().is_none());

    assert_eq!(document.headings[3].title, "Closed");
    assert_eq!(
        document.headings[3].planning.closed_raw(),
        Some("[2024-12-02 Mon]")
    );
    assert_eq!(
        document.headings[3].planning.closed_ts(),
        Some(1_733_097_600)
    );
    assert!(document.headings[3].planning.scheduled_raw().is_none());
    assert!(document.headings[3].planning.deadline_raw().is_none());
}

#[test]
fn orgize_adapter_parses_one_line_planning_with_all_keywords() {
    let content = "#+TITLE: One Line Planning\n* Test\nDEADLINE: <2024-12-01 Sun> SCHEDULED: <2024-11-20 Wed 09:15> CLOSED: [2024-12-02 Mon]\n";

    let document = OrgizeAdapter::new()
        .parse_document(
            Path::new("notes/one-line-planning.org"),
            content,
            &ParseOptions::default(),
        )
        .expect("one-line planning should parse");

    assert_eq!(document.headings.len(), 2);
    let heading = &document.headings[1];
    assert_eq!(heading.title, "Test");
    assert_eq!(heading.planning.deadline_raw(), Some("<2024-12-01 Sun>"));
    assert_eq!(heading.planning.deadline_ts(), Some(1_733_011_200));
    assert_eq!(
        heading.planning.scheduled_raw(),
        Some("<2024-11-20 Wed 09:15>")
    );
    assert_eq!(heading.planning.scheduled_ts(), Some(1_732_094_100));
    assert_eq!(heading.planning.closed_raw(), Some("[2024-12-02 Mon]"));
    assert_eq!(heading.planning.closed_ts(), Some(1_733_097_600));
}

#[test]
fn orgize_adapter_ignores_body_planning_lines_and_preserves_unsupported_raw_values() {
    let content =
        "* Warning delay\nDEADLINE: <2024-12-01 Sun -5d>\nscheduled: <2024-11-20 Wed -2d>\n";

    let document = OrgizeAdapter::new()
        .parse_document(
            Path::new("notes/warning-delay.org"),
            content,
            &ParseOptions::default(),
        )
        .expect("warning-delay planning should parse");

    assert_eq!(document.headings.len(), 2);
    let heading = &document.headings[1];
    assert_eq!(heading.title, "Warning delay");
    assert_eq!(
        heading.planning.deadline_raw(),
        Some("<2024-12-01 Sun -5d>")
    );
    assert_eq!(heading.planning.deadline_ts(), Some(1_733_011_200));
    assert!(heading.planning.scheduled_raw().is_none());
    assert!(heading.planning.scheduled_ts().is_none());
    assert!(heading.planning.closed_raw().is_none());
    assert!(heading.planning.closed_ts().is_none());
    assert_eq!(heading.timestamps.len(), 2);
    assert_eq!(heading.timestamps[0].modifiers.len(), 1);
    assert_eq!(heading.timestamps[1].role, Some(ParsedTimestampRole::Body));
}

#[test]
fn orgize_adapter_uses_last_value_for_duplicate_planning_keyword_on_one_line() {
    let content = "* Duplicate\nSCHEDULED: <2024-11-20 Wed> SCHEDULED: <2024-11-21 Thu 09:15>\n";

    let document = OrgizeAdapter::new()
        .parse_document(
            Path::new("notes/duplicate-planning.org"),
            content,
            &ParseOptions::default(),
        )
        .expect("duplicate planning keyword should parse");

    assert_eq!(document.headings.len(), 2);
    let heading = &document.headings[1];
    assert_eq!(heading.title, "Duplicate");
    assert_eq!(
        heading.planning.scheduled_raw(),
        Some("<2024-11-21 Thu 09:15>")
    );
    assert_eq!(heading.planning.scheduled_ts(), Some(1_732_180_500));
    assert!(heading.planning.deadline_raw().is_none());
    assert!(heading.planning.closed_raw().is_none());
    assert_eq!(heading.timestamps.len(), 2);
    assert_eq!(
        heading.timestamps[0].role,
        Some(ParsedTimestampRole::Scheduled)
    );
    assert_eq!(heading.timestamps[0].raw_value, "<2024-11-20 Wed>");
    assert_eq!(
        heading.timestamps[1].role,
        Some(ParsedTimestampRole::Scheduled)
    );
    assert_eq!(heading.timestamps[1].raw_value, "<2024-11-21 Thu 09:15>");
}

#[test]
fn orgize_adapter_collects_ranges_repeaters_and_body_timestamps() {
    let content = include_str!("data/parser/timestamps/planning-timestamp/fixture.org");

    let document = OrgizeAdapter::new()
        .parse_document(
            Path::new("tests/data/parser/timestamps/planning-timestamp/fixture.org"),
            content,
            &ParseOptions::default(),
        )
        .expect("timestamp fixture should parse");

    let time_range = &document.headings[7];
    assert_eq!(time_range.title, "Time range same day");
    assert_eq!(time_range.timestamps.len(), 1);
    assert_eq!(
        time_range.timestamps[0].role,
        Some(ParsedTimestampRole::Scheduled)
    );
    assert_eq!(
        time_range.timestamps[0].range_type,
        ParsedTimestampRangeType::TimeRange
    );
    assert_eq!(time_range.timestamps[0].start_ts, Some(1_732_095_000));
    assert_eq!(time_range.timestamps[0].end_ts, Some(1_732_100_400));

    let date_range = &document.headings[8];
    assert_eq!(date_range.title, "Date range");
    assert_eq!(
        date_range.timestamps[0].range_type,
        ParsedTimestampRangeType::DateRange
    );
    assert_eq!(date_range.timestamps[0].start_ts, Some(1_733_011_200));
    assert_eq!(date_range.timestamps[0].end_ts, Some(1_733_184_000));

    let repeater = &document.headings[9];
    assert_eq!(repeater.title, "Repeater");
    assert_eq!(repeater.timestamps[0].modifiers.len(), 1);
    assert_eq!(repeater.timestamps[0].modifiers[0].value, 1);

    let combined = &document.headings[10];
    assert_eq!(combined.title, "Repeater with deadline and warning");
    assert_eq!(combined.timestamps[0].modifiers.len(), 2);
    assert_eq!(
        combined.timestamps[0].modifiers[0].modifier_type,
        ParsedTimestampModifierType::CatchUp
    );
    assert_eq!(combined.timestamps[0].modifiers[0].value, 1);
    assert_eq!(
        combined.timestamps[0].modifiers[0].unit,
        ParsedTimestampUnit::Month
    );
    assert_eq!(
        combined.timestamps[0].modifiers[0].repeater_deadline_value,
        Some(2)
    );
    assert_eq!(
        combined.timestamps[0].modifiers[0].repeater_deadline_unit,
        Some(ParsedTimestampUnit::Day)
    );
    assert_eq!(
        combined.timestamps[0].modifiers[1].kind,
        ParsedTimestampModifierKind::Warning
    );
    assert_eq!(
        combined.timestamps[0].modifiers[1].modifier_type,
        ParsedTimestampModifierType::All
    );

    let warning_first = &document.headings[12];
    assert_eq!(warning_first.title, "Warning only first");
    assert_eq!(warning_first.timestamps[0].modifiers.len(), 1);
    assert_eq!(
        warning_first.timestamps[0].modifiers[0].modifier_type,
        ParsedTimestampModifierType::First
    );
    assert_eq!(warning_first.timestamps[0].modifiers[0].value, 2);
    assert_eq!(
        warning_first.timestamps[0].modifiers[0].unit,
        ParsedTimestampUnit::Week
    );

    let body_only = &document.headings[22];
    assert_eq!(body_only.title, "Another Task");
    assert_eq!(body_only.timestamps.len(), 2);
    assert_eq!(
        body_only.timestamps[0].role,
        Some(ParsedTimestampRole::Body)
    );
    assert_eq!(
        body_only.timestamps[1].role,
        Some(ParsedTimestampRole::Body)
    );
}

#[test]
fn orgize_adapter_preserves_empty_property_values_exposed_by_orgize() {
    let content = "#+TITLE: Empty Property\n* Task\n:PROPERTIES:\n:EMPTY: \n:END:\n";

    let document = OrgizeAdapter::new()
        .parse_document(
            Path::new("notes/empty-property.org"),
            content,
            &ParseOptions::default(),
        )
        .expect("empty property should parse");

    assert_eq!(document.headings.len(), 2);
    let heading = &document.headings[1];
    assert_eq!(heading.properties.len(), 1);
    assert_eq!(heading.properties[0].key, "EMPTY");
    assert_eq!(heading.properties[0].value.as_deref(), Some(""));
    assert_eq!(
        heading.properties[0].source,
        ParsedPropertySource::PropertyDrawer
    );
    assert!(!heading.properties[0].append);
    assert!(document.diagnostics.is_empty());
}

#[test]
fn orgize_adapter_falls_back_to_empty_property_drawer_rows_omitted_by_orgize() {
    let content = "\
#+TITLE: Empty Drawer Rows
* Task
:PROPERTIES:
:VALUE:
:VALUE+: empty base followed by append
:OTHER: base followed by empty append
:OTHER+:
:TRAILING_BASE: 
:TRAILING_BASE+: valid
:TRAILING_APPEND: valid
:TRAILING_APPEND+: 
:END:
";

    let document = OrgizeAdapter::new()
        .parse_document(
            Path::new("notes/empty-drawer-rows.org"),
            content,
            &ParseOptions::default(),
        )
        .expect("empty drawer rows should parse");

    let heading = &document.headings[1];
    let rows: Vec<(String, Option<String>, bool)> = heading
        .properties
        .iter()
        .map(|property| {
            (
                property.key.clone(),
                property.value.clone(),
                property.append,
            )
        })
        .collect();
    assert_eq!(
        rows,
        vec![
            ("VALUE".to_string(), Some("".to_string()), false),
            (
                "VALUE".to_string(),
                Some("empty base followed by append".to_string()),
                true,
            ),
            (
                "OTHER".to_string(),
                Some("base followed by empty append".to_string()),
                false,
            ),
            ("OTHER".to_string(), Some("".to_string()), true),
            ("TRAILING_BASE".to_string(), Some("".to_string()), false),
            ("TRAILING_BASE".to_string(), Some("valid".to_string()), true),
            (
                "TRAILING_APPEND".to_string(),
                Some("valid".to_string()),
                false
            ),
            ("TRAILING_APPEND".to_string(), Some("".to_string()), true),
        ]
    );
    assert!(document.diagnostics.is_empty());
}

#[test]
fn orgize_adapter_preserves_significant_whitespace_in_property_drawer_values() {
    let content = "\
#+TITLE: Property Whitespace
* Task
:PROPERTIES:
:CUSTOM_ID: abc 
:ID:  23
:END:
";

    let document = OrgizeAdapter::new()
        .parse_document(
            Path::new("notes/property-whitespace.org"),
            content,
            &ParseOptions::default(),
        )
        .expect("property whitespace should parse");

    assert_eq!(document.headings.len(), 2);
    let heading = &document.headings[1];
    assert_eq!(heading.properties.len(), 2);
    assert_eq!(heading.properties[0].key, "CUSTOM_ID");
    assert_eq!(heading.properties[0].value.as_deref(), Some("abc "));
    assert_eq!(heading.properties[1].key, "ID");
    assert_eq!(heading.properties[1].value.as_deref(), Some(" 23"));
    assert!(document.diagnostics.is_empty());
}

#[test]
fn orgize_adapter_collects_file_level_property_keywords_anywhere_in_buffer() {
    let content = include_str!("data/parser/properties/late-file-keywords/fixture.org");

    let document = OrgizeAdapter::new()
        .parse_document(
            Path::new("notes/late-file-keywords.org"),
            content,
            &ParseOptions::default(),
        )
        .expect("file-level properties should parse");

    assert_eq!(
        document.metadata.title.as_deref(),
        Some("Keyword and Property Normalization Fixture Later Title")
    );
    assert_eq!(document.headings.len(), 4);

    let keyword_rows: Vec<(String, Option<String>, Option<u32>)> = document
        .metadata
        .keywords
        .iter()
        .map(|keyword| {
            (
                keyword.key.clone(),
                keyword.value.clone(),
                keyword.line_number,
            )
        })
        .collect();
    assert_eq!(
        keyword_rows,
        vec![
            (
                "TITLE".to_string(),
                Some("Keyword and Property Normalization Fixture".to_string()),
                Some(1)
            ),
            ("STARTUP".to_string(), Some("showall".to_string()), Some(2)),
            (
                "PROPERTY".to_string(),
                Some("before_prop before-value".to_string()),
                Some(3)
            ),
            (
                "CATEGORY".to_string(),
                Some("before-category".to_string()),
                Some(4)
            ),
            (
                "AUTHOR".to_string(),
                Some("Later Author".to_string()),
                Some(9)
            ),
            (
                "OPTIONS".to_string(),
                Some("toc:nil num:t".to_string()),
                Some(10)
            ),
            (
                "PROPERTY".to_string(),
                Some("after_prop after-value".to_string()),
                Some(11)
            ),
            (
                "PROPERTY".to_string(),
                Some("repeated_prop first".to_string()),
                Some(12)
            ),
            (
                "PROPERTY".to_string(),
                Some("repeated_prop second".to_string()),
                Some(13)
            ),
            (
                "PROPERTY".to_string(),
                Some("appended_prop base".to_string()),
                Some(14)
            ),
            (
                "PROPERTY".to_string(),
                Some("appended_prop+ extra".to_string()),
                Some(15)
            ),
            (
                "CATEGORY".to_string(),
                Some("after-category".to_string()),
                Some(16)
            ),
            (
                "TITLE".to_string(),
                Some("Later Title".to_string()),
                Some(21)
            ),
            (
                "EXPORT_FILE_NAME".to_string(),
                Some("later-export-name".to_string()),
                Some(22)
            ),
            (
                "PROPERTY".to_string(),
                Some("second_after_heading works".to_string()),
                Some(27)
            ),
            (
                "CATEGORY".to_string(),
                Some("second-category".to_string()),
                Some(28)
            ),
        ]
    );

    let root = &document.headings[0];
    assert_eq!(root.level, 0);
    assert_eq!(
        root.title,
        "Keyword and Property Normalization Fixture Later Title"
    );
    assert!(root.is_root);
    assert_eq!(
        root.properties
            .iter()
            .map(|property| {
                (
                    property.key.clone(),
                    property.value.clone(),
                    property.source,
                    property.append,
                    property.line_number,
                )
            })
            .collect::<Vec<_>>(),
        vec![
            (
                "BEFORE_PROP".to_string(),
                Some("before-value".to_string()),
                ParsedPropertySource::PropertyKeyword,
                false,
                Some(3)
            ),
            (
                "CATEGORY".to_string(),
                Some("before-category".to_string()),
                ParsedPropertySource::CategoryKeyword,
                false,
                Some(4)
            ),
            (
                "AFTER_PROP".to_string(),
                Some("after-value".to_string()),
                ParsedPropertySource::PropertyKeyword,
                false,
                Some(11)
            ),
            (
                "REPEATED_PROP".to_string(),
                Some("first".to_string()),
                ParsedPropertySource::PropertyKeyword,
                false,
                Some(12)
            ),
            (
                "REPEATED_PROP".to_string(),
                Some("second".to_string()),
                ParsedPropertySource::PropertyKeyword,
                false,
                Some(13)
            ),
            (
                "APPENDED_PROP".to_string(),
                Some("base".to_string()),
                ParsedPropertySource::PropertyKeyword,
                false,
                Some(14)
            ),
            (
                "APPENDED_PROP".to_string(),
                Some("extra".to_string()),
                ParsedPropertySource::PropertyKeyword,
                true,
                Some(15)
            ),
            (
                "CATEGORY".to_string(),
                Some("after-category".to_string()),
                ParsedPropertySource::CategoryKeyword,
                false,
                Some(16)
            ),
            (
                "SECOND_AFTER_HEADING".to_string(),
                Some("works".to_string()),
                ParsedPropertySource::PropertyKeyword,
                false,
                Some(27)
            ),
            (
                "CATEGORY".to_string(),
                Some("second-category".to_string()),
                ParsedPropertySource::CategoryKeyword,
                false,
                Some(28)
            ),
        ]
    );

    assert_eq!(document.headings[1].title, "First heading");
    assert_eq!(document.headings[1].parent_index, Some(0));
    assert!(document.headings[1].properties.is_empty());

    assert_eq!(document.headings[2].title, "Child heading");
    assert_eq!(document.headings[2].parent_index, Some(1));
    assert!(document.headings[2].properties.is_empty());

    assert_eq!(document.headings[3].title, "Second heading");
    assert_eq!(document.headings[3].parent_index, Some(0));
    assert!(document.headings[3].properties.is_empty());
    assert!(document.diagnostics.is_empty());
}

#[test]
fn orgize_adapter_only_populates_shortcuts_when_parser_exposes_planning() {
    let content = "* Not valid\n** Planning not immediately after headline\nSome body text first.\nSCHEDULED: <2024-11-20 Wed>\n** Looks like planning in body\nThis mentions DEADLINE: <2024-12-01 Sun> inside text.\n** Lowercase\nscheduled: <2024-11-20 Wed>\n** Diary expression\nSCHEDULED: <%%(diary-float t 42)>\n";

    let document = OrgizeAdapter::new()
        .parse_document(
            Path::new("notes/planning-ownership.org"),
            content,
            &ParseOptions::default(),
        )
        .expect("planning ownership cases should parse");

    let delayed = &document.headings[2];
    assert!(delayed.planning.scheduled_raw().is_none());
    assert_eq!(delayed.timestamps.len(), 1);
    assert_eq!(delayed.timestamps[0].role, Some(ParsedTimestampRole::Body));

    let body_like = &document.headings[3];
    assert!(body_like.planning.deadline_raw().is_none());
    assert_eq!(body_like.timestamps.len(), 1);
    assert_eq!(
        body_like.timestamps[0].role,
        Some(ParsedTimestampRole::Body)
    );

    let lowercase = &document.headings[4];
    let lowercase_shortcuts = [
        lowercase.planning.scheduled.is_some(),
        lowercase.planning.deadline.is_some(),
        lowercase.planning.closed.is_some(),
    ];
    assert_eq!(lowercase.timestamps.len(), 1);
    if lowercase_shortcuts.iter().any(|value| *value) {
        assert_ne!(
            lowercase.timestamps[0].role,
            Some(ParsedTimestampRole::Body)
        );
    } else {
        assert_eq!(
            lowercase.timestamps[0].role,
            Some(ParsedTimestampRole::Body)
        );
    }

    let diary = &document.headings[5];
    assert_eq!(diary.timestamps.len(), 1);
    assert_eq!(
        diary.timestamps[0].timestamp_type,
        ParsedTimestampType::Diary
    );
    assert!(diary.timestamps[0].start_ts.is_none());
    assert!(diary.timestamps[0].end_ts.is_none());
    if let Some(scheduled) = &diary.planning.scheduled {
        assert_eq!(scheduled.raw_value, "<%%(diary-float t 42)>");
        assert!(scheduled.start_ts.is_none());
        assert_eq!(
            diary.timestamps[0].role,
            Some(ParsedTimestampRole::Scheduled)
        );
    } else {
        assert_eq!(diary.timestamps[0].role, Some(ParsedTimestampRole::Body));
    }
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
    assert_eq!(level0.title_raw.as_deref(), Some("Orgize Priority Fixture"));

    let first = &document.headings[1];
    assert_eq!(first.title, "Inbox");
    assert_eq!(first.todo_keyword.as_deref(), Some("TODO"));
    assert_eq!(first.todo_type, Some(TodoType::Open));
    assert_eq!(first.priority.as_deref(), Some("A"));
    assert_eq!(first.tags, vec!["rust".to_string(), "parser".to_string()]);
    assert!(!first.is_root);
    assert_eq!(first.parent_index, Some(0));

    let second = &document.headings[2];
    assert_eq!(second.title, "Child");
    assert_eq!(second.parent_index, Some(1));
    assert_eq!(second.todo_keyword.as_deref(), Some("DONE"));
    assert_eq!(second.todo_type, Some(TodoType::Closed));
    assert_eq!(second.priority.as_deref(), Some("B"));
    assert_eq!(second.tags, vec!["child".to_string()]);

    assert!(document.diagnostics.is_empty());
}

#[test]
fn orgize_adapter_stores_filetags_as_level_zero_direct_tags() {
    let content = "#+TITLE: Tags Fixture\n#+FILETAGS: :file:project:\n\n* Parent :parent:\nParent body.\n\n** Child :child:\nChild body.\n\n* Sibling\nSibling body.\n";

    let document = OrgizeAdapter::new()
        .parse_document(
            Path::new("notes/filetags.org"),
            content,
            &ParseOptions::default(),
        )
        .expect("filetags fixture should parse");

    assert_eq!(document.headings.len(), 4);
    assert_eq!(document.headings[0].tags, vec!["file", "project"]);
    assert_eq!(document.headings[1].tags, vec!["parent"]);
    assert_eq!(document.headings[2].tags, vec!["child"]);
    assert!(document.headings[3].tags.is_empty());

    let raw_keywords: Vec<(&str, Option<&str>)> = document
        .metadata
        .keywords
        .iter()
        .map(|keyword| (keyword.key.as_str(), keyword.value.as_deref()))
        .collect();
    assert_eq!(
        raw_keywords,
        vec![
            ("TITLE", Some("Tags Fixture")),
            ("FILETAGS", Some(":file:project:")),
        ]
    );
}

#[test]
fn orgize_adapter_preserves_empty_and_trailing_space_property_rows_across_multiple_headings() {
    type PropertyRow = (String, Option<String>, bool);
    type HeadingPropertyRows = (String, Vec<PropertyRow>);

    let content = "\
#+TITLE: Empty Property Drawer Fixture
* Empty base followed by append
:PROPERTIES:
:VALUE:
:VALUE+: empty base followed by append
:END:

* Base followed by empty append
:PROPERTIES:
:VALUE: base followed by empty append
:VALUE+:
:END:

* Empty base with trailing space
:PROPERTIES:
:VALUE: 
:VALUE+: valid
:END:

* Empty append with trailing space
:PROPERTIES:
:VALUE: valid
:VALUE+: 
:END:

* Append only
:PROPERTIES:
:VALUE+: only
:END:
";

    let document = OrgizeAdapter::new()
        .parse_document(
            Path::new("notes/empty-property-drawers.org"),
            content,
            &ParseOptions::default(),
        )
        .expect("multi-heading property drawers should parse");

    let rows: Vec<HeadingPropertyRows> = document
        .headings
        .iter()
        .skip(1)
        .map(|heading| {
            (
                heading.title.clone(),
                heading
                    .properties
                    .iter()
                    .map(|property| {
                        (
                            property.key.clone(),
                            property.value.clone(),
                            property.append,
                        )
                    })
                    .collect(),
            )
        })
        .collect();

    assert_eq!(
        rows,
        vec![
            (
                "Empty base followed by append".to_string(),
                vec![
                    ("VALUE".to_string(), Some("".to_string()), false),
                    (
                        "VALUE".to_string(),
                        Some("empty base followed by append".to_string()),
                        true,
                    ),
                ],
            ),
            (
                "Base followed by empty append".to_string(),
                vec![
                    (
                        "VALUE".to_string(),
                        Some("base followed by empty append".to_string()),
                        false,
                    ),
                    ("VALUE".to_string(), Some("".to_string()), true),
                ],
            ),
            (
                "Empty base with trailing space".to_string(),
                vec![
                    ("VALUE".to_string(), Some("".to_string()), false),
                    ("VALUE".to_string(), Some("valid".to_string()), true),
                ],
            ),
            (
                "Empty append with trailing space".to_string(),
                vec![
                    ("VALUE".to_string(), Some("valid".to_string()), false),
                    ("VALUE".to_string(), Some("".to_string()), true),
                ],
            ),
            (
                "Append only".to_string(),
                vec![("VALUE".to_string(), Some("only".to_string()), true)],
            ),
        ]
    );
}

#[test]
fn orgize_adapter_uses_configured_project_todo_keywords() {
    let content = "* PLAN Parser fixture\n** DONE Implemented\n";
    let options = ParseOptions {
        todo_keywords: TodoKeywordConfig {
            open: vec![TodoKeyword::with_fast_key("PLAN", 'p')],
            closed: vec![TodoKeyword::with_fast_key("DONE", 'd')],
        },
        ..ParseOptions::default()
    };

    let document = OrgizeAdapter::new()
        .parse_document(Path::new("notes/custom-todo.org"), content, &options)
        .expect("orgize adapter should parse configured todo keywords");

    assert_eq!(document.headings.len(), 3);
    assert_eq!(document.headings[1].todo_keyword.as_deref(), Some("PLAN"));
    assert_eq!(document.headings[1].todo_type, Some(TodoType::Open));
    assert_eq!(document.headings[1].title, "Parser fixture");
    assert_eq!(
        document.headings[1].title_raw.as_deref(),
        Some("PLAN Parser fixture")
    );
    assert_eq!(document.headings[2].todo_keyword.as_deref(), Some("DONE"));
    assert_eq!(document.headings[2].todo_type, Some(TodoType::Closed));
    assert_eq!(document.headings[2].title, "Implemented");
    assert_eq!(
        document.headings[2].title_raw.as_deref(),
        Some("DONE Implemented")
    );
}

#[test]
fn orgize_adapter_uses_configured_project_todo_keywords_with_priority() {
    let content = "* PLAN [#A] Parser fixture\n** DONE [#B] Implemented\n";
    let options = ParseOptions {
        todo_keywords: TodoKeywordConfig {
            open: vec![TodoKeyword::with_fast_key("PLAN", 'p')],
            closed: vec![TodoKeyword::with_fast_key("DONE", 'd')],
        },
        ..ParseOptions::default()
    };

    let document = OrgizeAdapter::new()
        .parse_document(
            Path::new("notes/custom-todo-priority.org"),
            content,
            &options,
        )
        .expect("orgize adapter should parse configured todo keyword priorities");

    assert_eq!(document.headings.len(), 3);
    assert_eq!(document.headings[1].todo_keyword.as_deref(), Some("PLAN"));
    assert_eq!(document.headings[1].todo_type, Some(TodoType::Open));
    assert_eq!(document.headings[1].priority.as_deref(), Some("A"));
    assert_eq!(document.headings[1].title, "Parser fixture");
    assert_eq!(
        document.headings[1].title_raw.as_deref(),
        Some("PLAN [#A] Parser fixture")
    );
    assert_eq!(document.headings[2].todo_keyword.as_deref(), Some("DONE"));
    assert_eq!(document.headings[2].todo_type, Some(TodoType::Closed));
    assert_eq!(document.headings[2].priority.as_deref(), Some("B"));
    assert_eq!(document.headings[2].title, "Implemented");
    assert_eq!(
        document.headings[2].title_raw.as_deref(),
        Some("DONE [#B] Implemented")
    );
}

#[test]
fn orgize_adapter_preserves_multi_digit_numeric_priority() {
    let document = OrgizeAdapter::new()
        .parse_document(
            Path::new("notes/numeric-priority.org"),
            "* [#10] Numeric priority\n",
            &ParseOptions::default(),
        )
        .expect("numeric priority fixture should parse");

    assert_eq!(document.headings[1].priority.as_deref(), Some("10"));
    assert_eq!(document.headings[1].title, "Numeric priority");
}

#[test]
fn orgize_adapter_prefers_org_todo_keywords_over_configured_defaults() {
    let content = include_str!("data/parser/todo-keywords/custom-sequence/fixture.org");
    let options = ParseOptions {
        todo_keywords: TodoKeywordConfig {
            open: vec![TodoKeyword::with_fast_key("TODO", 't')],
            closed: vec![TodoKeyword::with_fast_key("DONE", 'd')],
        },
        ..ParseOptions::default()
    };

    let document = OrgizeAdapter::new()
        .parse_document(
            Path::new("tests/data/parser/todo-keywords/custom-sequence/fixture.org"),
            content,
            &options,
        )
        .expect("orgize adapter should respect org todo keywords");

    assert_eq!(document.headings.len(), 3);
    assert_eq!(document.headings[1].todo_keyword.as_deref(), Some("PLAN"));
    assert_eq!(document.headings[1].todo_type, Some(TodoType::Open));
    assert_eq!(document.headings[1].title, "Parser fixture");
    assert_eq!(
        document.headings[1].title_raw.as_deref(),
        Some("PLAN Parser fixture")
    );
    assert_eq!(document.headings[2].todo_keyword.as_deref(), Some("DONE"));
    assert_eq!(document.headings[2].todo_type, Some(TodoType::Closed));
    assert_eq!(document.headings[2].title, "Implemented");
    assert_eq!(
        document.headings[2].title_raw.as_deref(),
        Some("DONE Implemented")
    );
}

#[test]
fn orgize_adapter_prefers_org_todo_keywords_over_configured_defaults_for_priority() {
    let content = "#+TODO: NEXT(n) REVIEW(r) BUILD(b) | DONE(d) CANCEL(c)\n\
* NEXT [#A] Priority Test\n\
* REVIEW [#B] Review query CLI\n\
* BUILD [#C] Build query backend\n\
* DONE [#B] Completed task\n";
    let options = ParseOptions {
        todo_keywords: TodoKeywordConfig {
            open: vec![TodoKeyword::with_fast_key("TODO", 't')],
            closed: vec![TodoKeyword::with_fast_key("DONE", 'd')],
        },
        ..ParseOptions::default()
    };

    let document = OrgizeAdapter::new()
        .parse_document(
            Path::new("notes/file-local-priority.org"),
            content,
            &options,
        )
        .expect("orgize adapter should parse priorities from file-local todo keywords");

    assert_eq!(document.headings.len(), 5);
    assert_eq!(document.headings[1].todo_keyword.as_deref(), Some("NEXT"));
    assert_eq!(document.headings[1].todo_type, Some(TodoType::Open));
    assert_eq!(document.headings[1].priority.as_deref(), Some("A"));
    assert_eq!(document.headings[1].title, "Priority Test");
    assert_eq!(
        document.headings[1].title_raw.as_deref(),
        Some("NEXT [#A] Priority Test")
    );
    assert_eq!(document.headings[2].todo_keyword.as_deref(), Some("REVIEW"));
    assert_eq!(document.headings[2].todo_type, Some(TodoType::Open));
    assert_eq!(document.headings[2].priority.as_deref(), Some("B"));
    assert_eq!(document.headings[2].title, "Review query CLI");
    assert_eq!(
        document.headings[2].title_raw.as_deref(),
        Some("REVIEW [#B] Review query CLI")
    );
    assert_eq!(document.headings[3].todo_keyword.as_deref(), Some("BUILD"));
    assert_eq!(document.headings[3].todo_type, Some(TodoType::Open));
    assert_eq!(document.headings[3].priority.as_deref(), Some("C"));
    assert_eq!(document.headings[3].title, "Build query backend");
    assert_eq!(
        document.headings[3].title_raw.as_deref(),
        Some("BUILD [#C] Build query backend")
    );
    assert_eq!(document.headings[4].todo_keyword.as_deref(), Some("DONE"));
    assert_eq!(document.headings[4].todo_type, Some(TodoType::Closed));
    assert_eq!(document.headings[4].priority.as_deref(), Some("B"));
    assert_eq!(document.headings[4].title, "Completed task");
    assert_eq!(
        document.headings[4].title_raw.as_deref(),
        Some("DONE [#B] Completed task")
    );
}

#[test]
fn orgize_adapter_treats_org_todo_keywords_as_overrides() {
    let content = "#+TODO: PLAN(p) | DONE(d)\n* PLAN me\n* DONE me\n* REVIEW Mist\n";
    let options = ParseOptions {
        todo_keywords: TodoKeywordConfig {
            open: vec![TodoKeyword::with_fast_key("REVIEW", 'r')],
            closed: vec![TodoKeyword::with_fast_key("DONE", 'd')],
        },
        ..ParseOptions::default()
    };

    let document = OrgizeAdapter::new()
        .parse_document(Path::new("notes/override.org"), content, &options)
        .expect("orgize adapter should respect org todo keywords");

    assert_eq!(document.headings.len(), 4);
    assert_eq!(document.headings[1].todo_keyword.as_deref(), Some("PLAN"));
    assert_eq!(document.headings[1].todo_type, Some(TodoType::Open));
    assert_eq!(document.headings[1].title, "me");
    assert_eq!(document.headings[1].title_raw.as_deref(), Some("PLAN me"));
    assert_eq!(document.headings[2].todo_keyword.as_deref(), Some("DONE"));
    assert_eq!(document.headings[2].todo_type, Some(TodoType::Closed));
    assert_eq!(document.headings[2].title, "me");
    assert_eq!(document.headings[2].title_raw.as_deref(), Some("DONE me"));
    assert_eq!(document.headings[3].todo_keyword, None);
    assert_eq!(document.headings[3].todo_type, None);
    assert_eq!(document.headings[3].title, "REVIEW Mist");
    assert_eq!(
        document.headings[3].title_raw.as_deref(),
        Some("REVIEW Mist")
    );
}

#[test]
fn orgize_adapter_core_parser_uses_only_effective_todo_keywords() {
    let content = "#+TODO: PLAN(p) | DONE(d)\n* PLAN me\n* DONE me\n";

    let parsed = OrgizeAdapter::new()
        .parse_document_core(
            Path::new("notes/core-todo.org"),
            content,
            &ParseOptions::default(),
        )
        .expect("core parser should parse");

    assert_eq!(parsed.headings[1].todo_keyword, None);
    assert_eq!(parsed.headings[1].title, "PLAN me");
    assert_eq!(parsed.headings[2].todo_keyword.as_deref(), Some("DONE"));
    assert_eq!(parsed.headings[2].todo_type, Some(TodoType::Closed));
    assert_eq!(parsed.headings[2].title, "me");

    let resolved = resolve_todo_keywords(content, &ParseOptions::default().todo_keywords);
    let parsed = OrgizeAdapter::new()
        .parse_document_core(
            Path::new("notes/core-todo.org"),
            content,
            &ParseOptions {
                todo_keywords: resolved.effective,
                ..ParseOptions::default()
            },
        )
        .expect("core parser should use effective TODO keywords");

    assert_eq!(parsed.headings[1].todo_keyword.as_deref(), Some("PLAN"));
    assert_eq!(parsed.headings[1].todo_type, Some(TodoType::Open));
    assert_eq!(parsed.headings[1].title, "me");
    assert_eq!(parsed.headings[2].todo_keyword.as_deref(), Some("DONE"));
    assert_eq!(parsed.headings[2].todo_type, Some(TodoType::Closed));
    assert_eq!(parsed.headings[2].title, "me");
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
    assert_eq!(
        document.headings[1].title_raw.as_deref(),
        Some("REVIEW *Mist*")
    );
    assert_eq!(document.headings[1].todo_keyword, None);
    assert_eq!(document.headings[1].todo_type, None);
    assert_eq!(document.headings[2].title, "me");
    assert_eq!(document.headings[2].title_raw.as_deref(), Some("PLAN me"));
    assert_eq!(document.headings[2].todo_keyword.as_deref(), Some("PLAN"));
    assert_eq!(document.headings[2].todo_type, Some(TodoType::Open));
    assert_eq!(document.headings[3].title, "me");
    assert_eq!(document.headings[3].title_raw.as_deref(), Some("TODO me"));
    assert_eq!(document.headings[3].todo_keyword.as_deref(), Some("TODO"));
    assert_eq!(document.headings[3].todo_type, Some(TodoType::Open));
    assert_eq!(document.headings[4].title, "again");
    assert_eq!(document.headings[5].title, "me");
    assert_eq!(document.headings[5].title_raw.as_deref(), Some("DONE me"));
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
        document.headings[0].title_raw.as_deref(),
        Some("Title can span multiple lines, even here")
    );
    assert_eq!(document.headings[1].title, "Unfortunately Everywhere");
    assert_eq!(
        document.headings[1].title_raw.as_deref(),
        Some("Unfortunately Everywhere")
    );
    assert_eq!(document.headings[2].title, "Plain Heading");
    assert_eq!(
        document.headings[2].title_raw.as_deref(),
        Some("Plain Heading")
    );
}

#[test]
fn orgize_adapter_collects_generic_keywords_anywhere_in_document_as_raw_keyword_rows() {
    let content = include_str!("data/parser/file-scope/raw-generic-keywords/fixture.org");

    let document = OrgizeAdapter::new()
        .parse_document(
            Path::new("notes/raw-generic-keywords.org"),
            content,
            &ParseOptions::default(),
        )
        .expect("generic keywords anywhere in document should parse");

    assert_eq!(
        document.metadata.title.as_deref(),
        Some("First title Later title")
    );
    assert_eq!(document.headings.len(), 3);
    assert_eq!(document.headings[0].title, "First title Later title");
    assert!(document.headings[0].is_root);
    assert!(document.headings[1].properties.is_empty());
    assert!(document.headings[2].properties.is_empty());

    let keyword_rows: Vec<(String, Option<String>, Option<u32>)> = document
        .metadata
        .keywords
        .iter()
        .map(|keyword| {
            (
                keyword.key.clone(),
                keyword.value.clone(),
                keyword.line_number,
            )
        })
        .collect();
    assert_eq!(
        keyword_rows,
        vec![
            (
                "TITLE".to_string(),
                Some("First title".to_string()),
                Some(1)
            ),
            ("STARTUP".to_string(), Some("showall".to_string()), Some(2)),
            ("AUTHOR".to_string(), Some("Jane Doe".to_string()), Some(7)),
            (
                "OPTIONS".to_string(),
                Some("toc:nil num:t".to_string()),
                Some(8)
            ),
            (
                "TITLE".to_string(),
                Some("Later title".to_string()),
                Some(13)
            ),
            (
                "EXPORT_FILE_NAME".to_string(),
                Some("export-name".to_string()),
                Some(14)
            ),
        ]
    );
}

#[test]
fn orgize_adapter_does_not_treat_keyword_like_body_text_as_a_raw_keyword_row() {
    let content = "* Heading with keyword-looking body text\nThis line mentions #+TITLE: but should only become a keyword row if Orgize exposes it as a keyword node in this context.\n\n#+TITLE: Real keyword if Orgize exposes it as a keyword node\n";

    let document = OrgizeAdapter::new()
        .parse_document(
            Path::new("notes/keyword-looking-body-text.org"),
            content,
            &ParseOptions::default(),
        )
        .expect("keyword-like body text should parse");

    assert_eq!(
        document.metadata.title.as_deref(),
        Some("Real keyword if Orgize exposes it as a keyword node")
    );
    assert_eq!(document.headings.len(), 2);
    assert_eq!(
        document.headings[0].title,
        "Real keyword if Orgize exposes it as a keyword node"
    );
    assert_eq!(
        document.headings[1].title,
        "Heading with keyword-looking body text"
    );
    assert_eq!(
        document
            .metadata
            .keywords
            .iter()
            .map(|keyword| (
                keyword.key.as_str(),
                keyword.value.as_deref(),
                keyword.line_number
            ))
            .collect::<Vec<_>>(),
        vec![(
            "TITLE",
            Some("Real keyword if Orgize exposes it as a keyword node"),
            Some(4)
        )]
    );
}

#[test]
fn orgize_adapter_extracts_heading_bodies_without_child_subtrees() {
    let content = "#+TITLE: Body Text Fixture\nFile-level introduction before the first heading.\nThis belongs to the synthetic level 0 heading if body indexing is enabled.\n\n* Parent\nParent paragraph one.\n\nParent paragraph two.\n\n** Child\nChild paragraph.\nThis text belongs to Child, not Parent.\n\n*** Grandchild\nGrandchild paragraph.\n\n* Empty Body Parent\n** Child Under Empty Parent\nChild body only.\n\n* Parent With Metadata\nSCHEDULED: <2026-06-23 Tue>\n:PROPERTIES:\n:Owner: Alice\n:END:\n\nBody after planning and property drawer.\n";

    let document = OrgizeAdapter::new()
        .parse_document(
            Path::new("notes/body-text-fixture.org"),
            content,
            &ParseOptions::default(),
        )
        .expect("body text fixture should parse");

    assert_eq!(document.headings.len(), 7);

    let level_zero_body = document.headings[0]
        .body_text
        .as_deref()
        .expect("level 0 body should be present");
    assert!(level_zero_body.contains("File-level introduction before the first heading."));
    assert!(!level_zero_body.contains("Parent paragraph one."));
    assert!(document.headings[0].body_byte_end > document.headings[0].body_byte_start);

    assert_eq!(
        document.headings[1].body_text.as_deref(),
        Some("Parent paragraph one.\n\nParent paragraph two.")
    );
    assert_eq!(
        document.headings[2].body_text.as_deref(),
        Some("Child paragraph.\nThis text belongs to Child, not Parent.")
    );
    assert_eq!(
        document.headings[3].body_text.as_deref(),
        Some("Grandchild paragraph.")
    );
    assert!(document.headings[4].body_text.is_none());
    assert_eq!(
        document.headings[5].body_text.as_deref(),
        Some("Child body only.")
    );

    let metadata_body = document.headings[6]
        .body_text
        .as_deref()
        .expect("metadata heading body should be present");
    assert_eq!(metadata_body, "Body after planning and property drawer.");
}

#[test]
fn orgize_adapter_excludes_structured_metadata_from_body_text() {
    let content = ":PROPERTIES:\n:CATEGORY: Level 0 Category Property\n:END:\n#+TITLE: Body Metadata Fixture\nIntro before heading.\n\n* Task\nSCHEDULED: <2026-06-23 Tue>\n:PROPERTIES:\n:Owner: Bob\n:END:\nReal body text.\n\n#+AUTHOR: Jane Doe\n\nBody after keyword.\n\n** Child\nChild body.\n\n* Invalid Planning\nSCHEDULED: <%%(diary-float t 42)>\nBody after invalid planning.\n";

    let document = OrgizeAdapter::new()
        .parse_document(
            Path::new("notes/body-metadata-fixture.org"),
            content,
            &ParseOptions::default(),
        )
        .expect("body metadata fixture should parse");

    assert_eq!(document.headings.len(), 4);
    assert_eq!(
        document.headings[0].body_text.as_deref(),
        Some("Intro before heading.")
    );
    assert_eq!(
        document.headings[1].body_text.as_deref(),
        Some("Real body text.\n\nBody after keyword.")
    );
    assert_eq!(document.headings[1].body_byte_start, None);
    assert_eq!(document.headings[1].body_byte_end, None);
    assert_eq!(
        document.headings[2].body_text.as_deref(),
        Some("Child body.")
    );
    assert_eq!(
        document.headings[3].body_text.as_deref(),
        Some("SCHEDULED: <%%(diary-float t 42)>\nBody after invalid planning.")
    );
}

#[test]
fn orgize_adapter_supports_simplified_org_todo_keyword_lines() {
    let content = include_str!("data/parser/todo-keywords/simplified-file-local-lines/fixture.org");
    let options = ParseOptions {
        todo_keywords: TodoKeywordConfig {
            open: vec![TodoKeyword::with_fast_key("TODO", 't')],
            closed: vec![TodoKeyword::with_fast_key("DONE", 'd')],
        },
        ..ParseOptions::default()
    };

    let document = OrgizeAdapter::new()
        .parse_document(
            Path::new("tests/data/parser/todo-keywords/simplified-file-local-lines/fixture.org"),
            content,
            &options,
        )
        .expect("simplified org todo lines should parse");

    assert_eq!(document.headings.len(), 14);

    assert_eq!(document.headings[1].todo_keyword, None);
    assert_eq!(document.headings[1].todo_type, None);
    assert_eq!(
        document.headings[1].title,
        "TODO invalid keyword, even though it is a default it is overwritten"
    );
    assert_eq!(
        document.headings[1].title_raw.as_deref(),
        Some("TODO invalid keyword, even though it is a default it is overwritten")
    );

    assert_eq!(document.headings[2].todo_keyword, None);
    assert_eq!(document.headings[2].todo_type, None);
    assert_eq!(
        document.headings[2].title,
        "DONE invalid keyword, even though it is a default it is overwritten"
    );

    let expected = [
        (
            3,
            "one",
            TodoType::Open,
            "valid, type open",
            "one valid, type open",
        ),
        (
            4,
            "two",
            TodoType::Open,
            "valid, type open",
            "two valid, type open",
        ),
        (
            5,
            "three",
            TodoType::Closed,
            "valid, type closed",
            "three valid, type closed",
        ),
        (
            6,
            "four",
            TodoType::Closed,
            "valid, type closed",
            "four valid, type closed",
        ),
        (
            7,
            "FIVE",
            TodoType::Open,
            "valid, type open",
            "FIVE valid, type open",
        ),
        (
            8,
            "SIX",
            TodoType::Open,
            "valid, type open",
            "SIX valid, type open",
        ),
        (
            9,
            "seven",
            TodoType::Open,
            "valid, type open",
            "seven valid, type open",
        ),
        (
            10,
            "eight",
            TodoType::Closed,
            "valid, type closed",
            "eight valid, type closed",
        ),
        (
            11,
            "nine",
            TodoType::Open,
            "valid, type open",
            "nine valid, type open",
        ),
        (
            12,
            "ten",
            TodoType::Closed,
            "valid, type closed",
            "ten valid, type closed",
        ),
        (
            13,
            "eleven",
            TodoType::Closed,
            "valid, type closed",
            "eleven valid, type closed",
        ),
    ];

    for (index, keyword, todo_type, expected_title, expected_title_raw) in expected {
        assert_eq!(
            document.headings[index].todo_keyword.as_deref(),
            Some(keyword)
        );
        assert_eq!(document.headings[index].todo_type, Some(todo_type));
        assert_eq!(document.headings[index].title, expected_title);
        assert_eq!(
            document.headings[index].title_raw.as_deref(),
            Some(expected_title_raw)
        );
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
        document.headings[1].title_raw.as_deref(),
        Some("[[file:natural/hausarzt-krebs-thomas.org][Thomas Krebs - Hausarzt]]")
    );
}

#[test]
fn orgize_adapter_removes_priority_and_statistics_cookies_from_titles() {
    let content = "\
#+TODO: TODO(t) NEXT(n) REVIEW(r) | DONE(d)
* TODO [#A] Prepare release
* REVIEW [#B] Statistic Cookies [0/1]
* NEXT Progress [50%]
* TODO Support [Linux]
";

    let document = OrgizeAdapter::new()
        .parse_document(
            Path::new("notes/title-cookies.org"),
            content,
            &ParseOptions::default(),
        )
        .expect("title cookie fixture should parse");

    assert_eq!(document.headings.len(), 5);

    assert_eq!(document.headings[1].priority.as_deref(), Some("A"));
    assert_eq!(document.headings[1].title, "Prepare release");
    assert_eq!(
        document.headings[1].title_raw.as_deref(),
        Some("TODO [#A] Prepare release")
    );

    assert_eq!(document.headings[2].priority.as_deref(), Some("B"));
    assert_eq!(document.headings[2].title, "Statistic Cookies");
    assert_eq!(
        document.headings[2].title_raw.as_deref(),
        Some("REVIEW [#B] Statistic Cookies [0/1]")
    );

    assert_eq!(document.headings[3].priority, None);
    assert_eq!(document.headings[3].title, "Progress");
    assert_eq!(
        document.headings[3].title_raw.as_deref(),
        Some("NEXT Progress [50%]")
    );

    assert_eq!(document.headings[4].priority, None);
    assert_eq!(document.headings[4].title, "Support [Linux]");
    assert_eq!(
        document.headings[4].title_raw.as_deref(),
        Some("TODO Support [Linux]")
    );
}

#[test]
fn orgize_adapter_collects_project_owned_links_including_plain_file_splits() {
    let content = "\
#+TITLE: Links
[[FILE:notes.org::42]]
[[target][description]]
<file:~/code/main.c::255>
<shell:ls *.org>
https://example.org
file:~/plain.c::255
* Heading
[[shell:ls]]";

    let document = OrgizeAdapter::new()
        .parse_document(
            Path::new("notes/links.org"),
            content,
            &ParseOptions::default(),
        )
        .expect("adapter should parse link fixture");

    assert_eq!(document.links.len(), 7);
    assert_eq!(document.links[0].raw, "[[FILE:notes.org::42]]");
    assert_eq!(document.links[0].raw_target, "FILE:notes.org::42");
    assert_eq!(document.links[0].link_type, "file");
    assert_eq!(document.links[0].path, "notes.org");
    assert_eq!(document.links[0].search_option.as_deref(), Some("42"));
    assert_eq!(
        document.links[0].source_context,
        ParsedLinkSourceContext::Normal
    );
    assert_eq!(
        document.links[1].raw_description.as_deref(),
        Some("description")
    );
    assert_eq!(document.links[2].format, "angle");
    assert_eq!(document.links[2].raw, "<file:~/code/main.c::255>");
    assert_eq!(document.links[2].search_option.as_deref(), Some("255"));
    assert_eq!(document.links[3].format, "angle");
    assert_eq!(document.links[3].link_type, "shell");
    assert_eq!(document.links[3].path, "ls *.org");
    assert_eq!(document.links[4].format, "plain");
    assert_eq!(document.links[4].raw, "https://example.org");
    assert_eq!(document.links[4].path, "//example.org");
    assert_eq!(document.links[5].format, "plain");
    assert_eq!(document.links[5].raw, "file:~/plain.c::255");
    assert_eq!(document.links[5].path, "~/plain.c");
    assert_eq!(document.links[5].search_option.as_deref(), Some("255"));
    assert_eq!(document.links[6].format, "bracket");
    assert_eq!(document.links[6].link_type, "shell");
    assert_eq!(document.links[6].path, "ls");
    assert_eq!(
        document.links[6].source_context,
        ParsedLinkSourceContext::Normal
    );
}

#[test]
fn orgize_adapter_ignores_links_in_ignored_regions_and_marks_source_contexts() {
    let content = "\
#+TITLE: Link contexts
#+PROPERTY: ignored https://example.org/in-property-keyword
Paragraph with https://example.org/in-root-paragraph.

* Heading with https://example.org/in-heading
Inside code =https://example.org/in-code= and verbatim ~https://example.org/in-verbatim~.
Inline source src_sh{https://example.org/in-inline-src}
Inline export @@html:https://example.org/in-inline-export@@

#+BEGIN_SRC text
https://example.org/in-source-block
#+END_SRC

#+BEGIN_EXAMPLE
https://example.org/in-example-block
#+END_EXAMPLE

: https://example.org/in-colon-example-line

#+BEGIN_COMMENT
https://example.org/in-comment-block
#+END_COMMENT

# https://example.org/in-comment-line

#+BEGIN_EXPORT HTML
https://example.org/in-export-block
#+END_EXPORT

Paragraph with https://example.org/in-paragraph.

#+BEGIN_VERSE
https://example.org/in-verse
#+END_VERSE

#+BEGIN_QUOTE
https://example.org/in-quote
#+END_QUOTE

#+BEGIN_CENTER
https://example.org/in-center
#+END_CENTER

#+BEGIN_JUSTIFY
https://example.org/in-justify
#+END_JUSTIFY

:PROPERTIES:
:LINK: https://example.org/in-property-drawer
:END:

:A_DRAWER:
https://example.org/in-drawer
:END:
";

    let document = OrgizeAdapter::new()
        .parse_document(
            Path::new("notes/link-contexts.org"),
            content,
            &ParseOptions::default(),
        )
        .expect("adapter should parse ignored-region fixture");

    assert_eq!(
        document
            .links
            .iter()
            .map(|link| (link.raw.as_str(), link.source_context.clone()))
            .collect::<Vec<_>>(),
        vec![
            (
                "https://example.org/in-root-paragraph",
                ParsedLinkSourceContext::Normal,
            ),
            (
                "https://example.org/in-heading",
                ParsedLinkSourceContext::Heading,
            ),
            (
                "https://example.org/in-paragraph",
                ParsedLinkSourceContext::Normal,
            ),
            (
                "https://example.org/in-verse",
                ParsedLinkSourceContext::VerseBlock,
            ),
            (
                "https://example.org/in-quote",
                ParsedLinkSourceContext::QuoteBlock,
            ),
            (
                "https://example.org/in-center",
                ParsedLinkSourceContext::CenterBlock,
            ),
            (
                "https://example.org/in-justify",
                ParsedLinkSourceContext::JustifyBlock,
            ),
            (
                "https://example.org/in-property-drawer",
                ParsedLinkSourceContext::PropertyDrawer,
            ),
            (
                "https://example.org/in-drawer",
                ParsedLinkSourceContext::Drawer,
            ),
        ]
    );
}

#[test]
fn orgize_adapter_can_disable_plain_links_without_affecting_bracket_or_angle_links() {
    let content = "jira:ABC-123 <jira:ABC-123> [[jira:ABC-123]]";
    let options = ParseOptions {
        link_scanner: LinkScannerConfig {
            plain_link_protocols: Vec::new(),
        },
        ..ParseOptions::default()
    };

    let document = OrgizeAdapter::new()
        .parse_document(Path::new("notes/plain-disabled.org"), content, &options)
        .expect("adapter should parse link fixture");

    assert_eq!(document.links.len(), 2);
    assert_eq!(document.links[0].format, "angle");
    assert_eq!(document.links[0].link_type, "jira");
    assert_eq!(document.links[1].format, "bracket");
    assert_eq!(document.links[1].link_type, "jira");
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
        document.headings[1].title_raw.as_deref(),
        Some("[[file:natural/hausarzt-krebs-thomas.org]]")
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
        document.headings[1].title_raw.as_deref(),
        Some("*bold* /italic/ _underline_ =code= ~verbatim~")
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
    assert_eq!(
        document.headings[1].title_raw.as_deref(),
        Some("Plain Heading")
    );
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
    assert_eq!(
        document.headings[0].title_raw.as_deref(),
        Some("Project Dashboard")
    );
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
    let json = serde_json::to_value(&document.headings[0]).expect("heading should serialize");
    assert!(json["title_raw"].is_null());
}
