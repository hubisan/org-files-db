use org_files_db::query::{parse_query, QueryParseErrorKind};

#[test]
fn rejects_nested_heading_shorthand_inside_source_query() {
    let error = parse_query("(links (source (tags \"project\")))").expect_err("query should fail");
    assert_eq!(
        error.kind,
        QueryParseErrorKind::MissingExplicitTargetWrapper
    );
}

#[test]
fn rejects_wrong_nested_wrapper_for_has_link() {
    let error = parse_query("(headings (has-link (headings (todo \"NEXT\"))))")
        .expect_err("query should fail");
    assert_eq!(error.kind, QueryParseErrorKind::InvalidTargetForm);
}

#[test]
fn rejects_ancestors_nested_heading_shorthand() {
    let error =
        parse_query("(headings (ancestors (tags \"project\")))").expect_err("query should fail");
    assert_eq!(
        error.kind,
        QueryParseErrorKind::MissingExplicitTargetWrapper
    );
}

#[test]
fn rejects_ancestors_with_non_heading_target_wrapper() {
    let error = parse_query("(headings (ancestors (files (file-path \"x.org\"))))")
        .expect_err("query should fail");
    assert_eq!(error.kind, QueryParseErrorKind::InvalidTargetForm);
}

#[test]
fn rejects_tags_all_with_conflicting_match_option() {
    let error =
        parse_query("(headings (tags-all \"a\" :match :any))").expect_err("query should fail");
    assert_eq!(error.kind, QueryParseErrorKind::InvalidOptionSyntax);
}

#[test]
fn rejects_multiple_expressions_inside_target_wrapper() {
    let error = parse_query("(headings (todo \"NEXT\") (tags \"project\"))")
        .expect_err("query should fail");
    assert_eq!(error.kind, QueryParseErrorKind::StructuralArity);
}

#[test]
fn rejects_ancestors_with_multiple_arguments() {
    let error = parse_query(
        "(headings (ancestors (headings (tags \"project\")) (headings (todo \"NEXT\"))))",
    )
    .expect_err("query should fail");
    assert_eq!(error.kind, QueryParseErrorKind::StructuralArity);
}

#[test]
fn rejects_not_with_zero_children() {
    let error = parse_query("(not)").expect_err("query should fail");
    assert_eq!(error.kind, QueryParseErrorKind::InvalidBooleanForm);
}

#[test]
fn rejects_not_with_multiple_children() {
    let error = parse_query("(not (todo) (priority \"A\"))").expect_err("query should fail");
    assert_eq!(error.kind, QueryParseErrorKind::InvalidBooleanForm);
}

#[test]
fn rejects_empty_and_expression() {
    let error = parse_query("(and)").expect_err("query should fail");
    assert_eq!(error.kind, QueryParseErrorKind::InvalidBooleanForm);
}

#[test]
fn rejects_dangling_option_keyword() {
    let error = parse_query("(title \"Query\" :regexp)").expect_err("query should fail");
    assert_eq!(error.kind, QueryParseErrorKind::InvalidOptionSyntax);
}

#[test]
fn rejects_positional_argument_after_keyword_option() {
    let error =
        parse_query("(title \"Query\" :regexp t \"extra\")").expect_err("query should fail");
    assert_eq!(error.kind, QueryParseErrorKind::InvalidOptionSyntax);
}

#[test]
fn rejects_empty_form() {
    let error = parse_query("()").expect_err("query should fail");
    assert_eq!(error.kind, QueryParseErrorKind::InvalidTopLevelForm);
}

#[test]
fn rejects_unterminated_string_literal() {
    let error = parse_query("(title \"Query)").expect_err("query should fail");
    assert_eq!(error.kind, QueryParseErrorKind::Lexical);
}
