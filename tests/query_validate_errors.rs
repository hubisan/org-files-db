use org_files_db::query::{
    validate_query, Expr, PredicateArg, PredicateCall, QueryAst, QueryTarget,
    QueryValidationErrorKind, QueryValidationOptions, QueryValue,
};

fn no_body_capabilities() -> QueryValidationOptions {
    QueryValidationOptions {
        body_text_available: false,
        regexp_body_matching_supported: false,
    }
}

fn body_no_regexp_capabilities() -> QueryValidationOptions {
    QueryValidationOptions {
        body_text_available: true,
        regexp_body_matching_supported: false,
    }
}

fn full_capabilities() -> QueryValidationOptions {
    QueryValidationOptions {
        body_text_available: true,
        regexp_body_matching_supported: true,
    }
}

#[test]
fn rejects_tags_regexp_with_clear_guidance() {
    let query = QueryAst {
        target: QueryTarget::Headings,
        predicate: Some(Expr::Call(PredicateCall {
            name: "tags-regexp".to_string(),
            args: vec![PredicateArg::Scalar(QueryValue::String(
                "proj.*".to_string(),
            ))],
            options: Vec::new(),
        })),
    };

    let error = validate_query(query, &full_capabilities()).expect_err("query should fail");
    assert_eq!(error.kind, QueryValidationErrorKind::UnsupportedOperator);
    assert!(error.message.contains("use (tags ... :regexp t) instead"));
}

#[test]
fn rejects_generic_unknown_operator() {
    let query = org_files_db::query::parse_query(r#"(headings (not-a-v0-predicate "x"))"#)
        .expect("query should parse");
    let error = validate_query(query, &full_capabilities()).expect_err("query should fail");
    assert_eq!(error.kind, QueryValidationErrorKind::UnsupportedOperator);
}

#[test]
fn rejects_predicates_on_invalid_targets() {
    let query =
        org_files_db::query::parse_query(r#"(links (todo "NEXT"))"#).expect("query should parse");
    let error = validate_query(query, &full_capabilities()).expect_err("query should fail");
    assert_eq!(error.kind, QueryValidationErrorKind::InvalidTarget);

    let query =
        org_files_db::query::parse_query(r#"(files (title "X"))"#).expect("query should parse");
    let error = validate_query(query, &full_capabilities()).expect_err("query should fail");
    assert_eq!(error.kind, QueryValidationErrorKind::InvalidTarget);
}

#[test]
fn rejects_invalid_boolean_option_value_types() {
    let query = org_files_db::query::parse_query(r#"(headings (title "x" :regexp "yes"))"#)
        .expect("query should parse");
    let error = validate_query(query, &full_capabilities()).expect_err("query should fail");
    assert_eq!(error.kind, QueryValidationErrorKind::InvalidValue);

    let query = org_files_db::query::parse_query(r#"(headings (title "x" :exact "yes"))"#)
        .expect("query should parse");
    let error = validate_query(query, &full_capabilities()).expect_err("query should fail");
    assert_eq!(error.kind, QueryValidationErrorKind::InvalidValue);

    let query = org_files_db::query::parse_query(r#"(headings (tags "project" :regexp "yes"))"#)
        .expect("query should parse");
    let error = validate_query(query, &full_capabilities()).expect_err("query should fail");
    assert_eq!(error.kind, QueryValidationErrorKind::InvalidValue);
}

#[test]
fn rejects_invalid_keyword_option_value_type() {
    let query = org_files_db::query::parse_query(r#"(headings (tags "project" :match "all"))"#)
        .expect("query should parse");
    let error = validate_query(query, &full_capabilities()).expect_err("query should fail");
    assert_eq!(error.kind, QueryValidationErrorKind::InvalidValue);
}

#[test]
fn rejects_duplicate_options() {
    let query = org_files_db::query::parse_query(r#"(headings (title "x" :exact t :exact nil))"#)
        .expect("query should parse");
    let error = validate_query(query, &full_capabilities()).expect_err("query should fail");
    assert_eq!(error.kind, QueryValidationErrorKind::InvalidOption);
}

#[test]
fn rejects_wrong_arity() {
    let query = org_files_db::query::parse_query(r#"(files (property "OWNER" "Alice" "Bob"))"#)
        .expect("query should parse");
    let error = validate_query(query, &full_capabilities()).expect_err("query should fail");
    assert_eq!(error.kind, QueryValidationErrorKind::WrongArity);

    let query =
        org_files_db::query::parse_query(r#"(files (file-modified))"#).expect("query should parse");
    let error = validate_query(query, &full_capabilities()).expect_err("query should fail");
    assert_eq!(error.kind, QueryValidationErrorKind::WrongArity);
}

#[test]
fn rejects_unknown_options() {
    let query = org_files_db::query::parse_query(r#"(headings (todo "NEXT" :regexp t))"#)
        .expect("query should parse");
    let error = validate_query(query, &full_capabilities()).expect_err("query should fail");
    assert_eq!(error.kind, QueryValidationErrorKind::InvalidOption);
}

#[test]
fn rejects_invalid_option_combinations() {
    let query =
        org_files_db::query::parse_query(r#"(headings (title "Query" :regexp t :exact t))"#)
            .expect("query should parse");
    let error = validate_query(query, &full_capabilities()).expect_err("query should fail");
    assert_eq!(
        error.kind,
        QueryValidationErrorKind::InvalidOptionCombination
    );

    let query = org_files_db::query::parse_query(r#"(headings (title "a" "b" :exact t))"#)
        .expect("query should parse");
    let error = validate_query(query, &full_capabilities()).expect_err("query should fail");
    assert_eq!(
        error.kind,
        QueryValidationErrorKind::InvalidOptionCombination
    );
}

#[test]
fn rejects_target_aware_inheritance_options_in_file_queries() {
    let query =
        org_files_db::query::parse_query(r#"(files (property "CATEGORY" "work" :inherit t))"#)
            .expect("query should parse");
    let error = validate_query(query, &full_capabilities()).expect_err("query should fail");
    assert_eq!(error.kind, QueryValidationErrorKind::InvalidOption);

    let query = org_files_db::query::parse_query(r#"(files (tags "project" :with-root t))"#)
        .expect("query should parse");
    let error = validate_query(query, &full_capabilities()).expect_err("query should fail");
    assert_eq!(error.kind, QueryValidationErrorKind::InvalidOption);

    let query =
        org_files_db::query::parse_query(r#"(headings (keyword "TITLE" "Projects" :inherit t))"#)
            .expect("query should parse");
    let error = validate_query(query, &full_capabilities()).expect_err("query should fail");
    assert_eq!(error.kind, QueryValidationErrorKind::InvalidOption);
}

#[test]
fn rejects_invalid_match_values() {
    let query = org_files_db::query::parse_query(r#"(headings (tags "project" :match :one))"#)
        .expect("query should parse");
    let error = validate_query(query, &full_capabilities()).expect_err("query should fail");
    assert_eq!(error.kind, QueryValidationErrorKind::InvalidValue);
}

#[test]
fn rejects_has_text_when_body_text_is_unavailable() {
    let query = org_files_db::query::parse_query(r#"(headings (has-text "sqlite"))"#)
        .expect("query should parse");
    let error = validate_query(query, &no_body_capabilities()).expect_err("query should fail");
    assert_eq!(error.kind, QueryValidationErrorKind::UnavailableIndexedData);
}

#[test]
fn rejects_has_text_regexp_when_backend_lacks_regexp_body_matching() {
    let query =
        org_files_db::query::parse_query(r#"(headings (has-text "sqlite.*fts" :regexp t))"#)
            .expect("query should parse");
    let error =
        validate_query(query, &body_no_regexp_capabilities()).expect_err("query should fail");
    assert_eq!(
        error.kind,
        QueryValidationErrorKind::UnsupportedBackendFeature
    );
}

#[test]
fn rejects_semantically_invalid_nested_relation_queries() {
    let query = QueryAst {
        target: QueryTarget::Headings,
        predicate: Some(Expr::Call(PredicateCall {
            name: "has-link".to_string(),
            args: vec![PredicateArg::NestedQuery(Box::new(QueryAst {
                target: QueryTarget::Headings,
                predicate: Some(Expr::Call(PredicateCall {
                    name: "link-type".to_string(),
                    args: vec![PredicateArg::Scalar(QueryValue::String("file".to_string()))],
                    options: Vec::new(),
                })),
            }))],
            options: Vec::new(),
        })),
    };

    let error = validate_query(query, &full_capabilities()).expect_err("query should fail");
    assert_eq!(error.kind, QueryValidationErrorKind::InvalidTarget);
}

#[test]
fn rejects_links_to_links_wrapper_at_parser_layer() {
    let error =
        org_files_db::query::parse_query(r#"(headings (links-to (links (link-type "file"))))"#)
            .expect_err("query should fail");
    assert_eq!(
        error.kind,
        org_files_db::query::QueryParseErrorKind::InvalidTargetForm
    );
}
