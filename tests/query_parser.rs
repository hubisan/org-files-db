use org_files_db::query::{
    parse_query, Expr, PredicateArg, PredicateCall, QueryAst, QueryOption, QueryTarget, QueryValue,
};

#[test]
fn parses_top_level_heading_shorthand_as_headings_query() {
    let query = parse_query("(todo \"NEXT\")").expect("query should parse");

    assert_eq!(
        query,
        QueryAst {
            target: QueryTarget::Headings,
            predicate: Some(Expr::Call(PredicateCall {
                name: "todo".to_string(),
                args: vec![PredicateArg::Scalar(QueryValue::String("NEXT".to_string()))],
                options: Vec::new(),
            })),
        }
    );
}

#[test]
fn parses_top_level_boolean_shorthand_as_headings_query() {
    let query =
        parse_query("(and (todo \"NEXT\") (tags \"project\"))").expect("query should parse");

    assert_eq!(query.target, QueryTarget::Headings);
    let Expr::And(children) = query.predicate.expect("predicate should exist") else {
        panic!("expected and expression");
    };
    assert_eq!(children.len(), 2);
}

#[test]
fn parses_explicit_empty_target_queries() {
    let links = parse_query("(links)").expect("links query should parse");
    assert_eq!(
        links,
        QueryAst {
            target: QueryTarget::Links,
            predicate: None,
        }
    );

    let files = parse_query("(files)").expect("files query should parse");
    assert_eq!(
        files,
        QueryAst {
            target: QueryTarget::Files,
            predicate: None,
        }
    );
}

#[test]
fn normalizes_tags_all_to_tags_with_match_all() {
    let query =
        parse_query("(headings (tags-all \"project\" \"urgent\"))").expect("query should parse");

    let Expr::Call(call) = query.predicate.expect("predicate should exist") else {
        panic!("expected predicate call");
    };
    assert_eq!(call.name, "tags");
    assert_eq!(
        call.args,
        vec![
            PredicateArg::Scalar(QueryValue::String("project".to_string())),
            PredicateArg::Scalar(QueryValue::String("urgent".to_string())),
        ]
    );
    assert_eq!(
        call.options,
        vec![QueryOption {
            name: "match".to_string(),
            value: QueryValue::Keyword("all".to_string()),
        }]
    );
}

#[test]
fn preserves_non_conflicting_tags_all_options_while_normalizing_match_all() {
    let query =
        parse_query("(headings (tags-all \"project\" :regexp t))").expect("query should parse");

    let Expr::Call(call) = query.predicate.expect("predicate should exist") else {
        panic!("expected predicate call");
    };
    assert_eq!(call.name, "tags");
    assert_eq!(
        call.options,
        vec![
            QueryOption {
                name: "regexp".to_string(),
                value: QueryValue::Bool(true),
            },
            QueryOption {
                name: "match".to_string(),
                value: QueryValue::Keyword("all".to_string()),
            },
        ]
    );
}

#[test]
fn parses_generic_predicate_options_without_canonicalizing_them() {
    let query = parse_query("(files (tags \"project\" :regexp t :match :all))")
        .expect("query should parse");

    let Expr::Call(call) = query.predicate.expect("predicate should exist") else {
        panic!("expected predicate call");
    };
    assert_eq!(call.name, "tags");
    assert_eq!(
        call.options,
        vec![
            QueryOption {
                name: "regexp".to_string(),
                value: QueryValue::Bool(true),
            },
            QueryOption {
                name: "match".to_string(),
                value: QueryValue::Keyword("all".to_string()),
            },
        ]
    );
}

#[test]
fn parses_nested_source_query_with_explicit_wrapper() {
    let query =
        parse_query("(links (source (headings (tags \"project\"))))").expect("query should parse");

    let Expr::Call(call) = query.predicate.expect("predicate should exist") else {
        panic!("expected predicate call");
    };
    assert_eq!(call.name, "source");
    assert_eq!(call.args.len(), 1);

    let PredicateArg::NestedQuery(nested) = &call.args[0] else {
        panic!("expected nested query");
    };
    assert_eq!(nested.target, QueryTarget::Headings);
}

#[test]
fn parses_has_link_with_nested_links_query() {
    let query = parse_query("(headings (has-link (links (status \"broken\"))))")
        .expect("query should parse");

    let Expr::Call(call) = query.predicate.expect("predicate should exist") else {
        panic!("expected predicate call");
    };
    assert_eq!(call.name, "has-link");

    let PredicateArg::NestedQuery(nested) = &call.args[0] else {
        panic!("expected nested query");
    };
    assert_eq!(nested.target, QueryTarget::Links);
}

#[test]
fn keeps_linked_from_any_as_keyword_value() {
    let query = parse_query("(headings (linked-from :any))").expect("query should parse");

    let Expr::Call(call) = query.predicate.expect("predicate should exist") else {
        panic!("expected predicate call");
    };
    assert_eq!(call.name, "linked-from");
    assert_eq!(
        call.args,
        vec![PredicateArg::Scalar(QueryValue::Keyword("any".to_string()))]
    );
}

#[test]
fn supports_source_any_as_keyword_value() {
    let query = parse_query("(links (source :any))").expect("query should parse");

    let Expr::Call(call) = query.predicate.expect("predicate should exist") else {
        panic!("expected predicate call");
    };
    assert_eq!(call.name, "source");
    assert_eq!(
        call.args,
        vec![PredicateArg::Scalar(QueryValue::Keyword("any".to_string()))]
    );
}

#[test]
fn supports_target_any_as_keyword_value() {
    let query = parse_query("(links (target :any))").expect("query should parse");

    let Expr::Call(call) = query.predicate.expect("predicate should exist") else {
        panic!("expected predicate call");
    };
    assert_eq!(call.name, "target");
    assert_eq!(
        call.args,
        vec![PredicateArg::Scalar(QueryValue::Keyword("any".to_string()))]
    );
}

#[test]
fn parses_parent_existence_form() {
    assert_hierarchy_existence_form("(headings (parent))", "parent");
}

#[test]
fn parses_ancestors_existence_form() {
    assert_hierarchy_existence_form("(headings (ancestors))", "ancestors");
}

#[test]
fn parses_children_existence_form() {
    assert_hierarchy_existence_form("(headings (children))", "children");
}

#[test]
fn parses_descendants_existence_form() {
    assert_hierarchy_existence_form("(headings (descendants))", "descendants");
}

#[test]
fn parses_ancestors_with_explicit_nested_headings_query() {
    let query = parse_query("(headings (ancestors (headings (tags \"project\"))))")
        .expect("query should parse");

    let Expr::Call(call) = query.predicate.expect("predicate should exist") else {
        panic!("expected predicate call");
    };
    assert_eq!(call.name, "ancestors");
    let PredicateArg::NestedQuery(nested) = &call.args[0] else {
        panic!("expected nested query");
    };
    assert_eq!(nested.target, QueryTarget::Headings);
}

#[test]
fn parses_scalar_symbol_and_integer_arguments() {
    let query = parse_query("(headings (level >= 2))").expect("query should parse");

    let Expr::Call(call) = query.predicate.expect("predicate should exist") else {
        panic!("expected predicate call");
    };
    assert_eq!(
        call.args,
        vec![
            PredicateArg::Scalar(QueryValue::Symbol(">=".to_string())),
            PredicateArg::Scalar(QueryValue::Integer(2)),
        ]
    );
}

fn assert_hierarchy_existence_form(input: &str, expected_name: &str) {
    let query = parse_query(input).expect("query should parse");

    let Expr::Call(call) = query.predicate.expect("predicate should exist") else {
        panic!("expected predicate call");
    };
    assert_eq!(call.name, expected_name);
    assert!(call.args.is_empty());
    assert!(call.options.is_empty());
}
