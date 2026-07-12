use org_files_db::query::{
    parse_query, validate_query, QueryTarget, QueryValidationOptions, QueryValue, ValidatedArg,
    ValidatedExpr,
};

fn full_capabilities() -> QueryValidationOptions {
    QueryValidationOptions {
        body_text_available: true,
        regexp_body_matching_supported: true,
    }
}

#[test]
fn validates_heading_predicate_families() {
    let query = parse_query(
        r#"(headings
            (and
              (todo "NEXT" "WAITING")
              (done)
              (title "Query" :exact t)
              (has-text "sqlite")
              (level >= 2)
              (priority "A" "B")
              (tags "project" "urgent" :match :all :inherit t :without-root nil :regexp nil)
              (property "OWNER" "Alice" :inherit nil)
              (property "CATEGORY" "work" :without-root t)
              (keyword "TITLE" "Projects")
              (file-name "notes.org" :exact t)
              (file-path "projects" "notes")
              (file-dir "projects")
              (file-title "Projects" :regexp t)
              (file-modified :from -7)
              (outline-contains "Projects" "Query")
              (outline-sequence "Projects" "Org database" :exact t)
              (ts)
              (ts-active :from today :to 7)
              (ts-inactive :from -30 :to today)
              (deadline)
              (scheduled :to 7)
              (closed :from -7)
              (planning :on today)
              (parent)
              (ancestors (headings (tags "project")))
              (children)
              (descendants (headings (todo "NEXT")))
              (has-link)
              (links-to (files (file-path "notes.org" :exact t)))
              (linked-from :any)))"#,
    )
    .expect("query should parse");

    let validated = validate_query(query, &full_capabilities()).expect("query should validate");
    assert_eq!(validated.target, QueryTarget::Headings);
    assert!(matches!(validated.predicate, Some(ValidatedExpr::And(_))));
}

#[test]
fn validates_link_predicate_families() {
    let query = parse_query(
        r#"(links
            (and
              (link-type "file" "id")
              (link-target "notes.org" :exact t)
              (link-description "Project" "Notes")
              (has-description)
              (status "broken" "unresolved")
              (source :any)
              (target (files (file-path "notes.org" :exact t)))))"#,
    )
    .expect("query should parse");

    let validated = validate_query(query, &full_capabilities()).expect("query should validate");
    assert_eq!(validated.target, QueryTarget::Links);
}

#[test]
fn validates_file_predicate_families() {
    let query = parse_query(
        r#"(files
            (and
              (file-name "notes")
              (file-path "projects" "notes")
              (file-dir "projects")
              (file-title "Projects" :exact t)
              (file-modified :from -7)
              (keyword "TITLE" "Projects")
              (property "CATEGORY" "work")
              (tags "project" "archive" :match :all)
              (has-link (links (link-type "file")))
              (links-to (headings (tags "project")))
              (linked-from (files (file-path "index.org" :exact t)))))"#,
    )
    .expect("query should parse");

    let validated = validate_query(query, &full_capabilities()).expect("query should validate");
    assert_eq!(validated.target, QueryTarget::Files);
}

#[test]
fn parse_and_validate_normalizes_tags_all_to_canonical_tags() {
    let query = parse_query(r#"(headings (tags-all "a" "b"))"#).expect("query should parse");
    let validated = validate_query(query, &full_capabilities()).expect("query should validate");

    let Some(ValidatedExpr::Predicate(predicate)) = validated.predicate else {
        panic!("expected validated predicate");
    };
    assert_eq!(predicate.name, "tags");
    assert_eq!(predicate.args.len(), 2);
    assert_eq!(predicate.options.len(), 1);
    assert_eq!(predicate.options[0].name, "match");
    assert_eq!(
        predicate.options[0].value,
        QueryValue::Keyword("all".to_string())
    );
}

#[test]
fn validates_source_and_target_any_forms() {
    let source_query = parse_query("(links (source :any))").expect("query should parse");
    let source = validate_query(source_query, &full_capabilities()).expect("query should validate");
    let Some(ValidatedExpr::Predicate(predicate)) = source.predicate else {
        panic!("expected predicate");
    };
    assert_eq!(
        predicate.args,
        vec![ValidatedArg::Scalar(QueryValue::Keyword("any".to_string()))]
    );

    let target_query = parse_query("(links (target :any))").expect("query should parse");
    validate_query(target_query, &full_capabilities()).expect("query should validate");
}
