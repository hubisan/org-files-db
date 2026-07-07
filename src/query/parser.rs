use super::ast::{
    Expr, PredicateArg, PredicateCall, QueryAst, QueryOption, QueryTarget, QueryValue,
};
use super::error::{QueryParseError, QueryParseErrorKind};

#[derive(Debug, Clone, PartialEq, Eq)]
enum RawNode {
    Atom(RawAtom),
    List(RawList),
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct RawAtom {
    kind: RawAtomKind,
    value: String,
    span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct RawList {
    items: Vec<RawNode>,
    span: Span,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum RawAtomKind {
    String,
    Symbol,
    Keyword,
    Integer,
    Bool,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct Span {
    start: usize,
    end: usize,
}

impl Span {
    fn tuple(self) -> (usize, usize) {
        (self.start, self.end)
    }
}

pub fn parse_query(input: &str) -> Result<QueryAst, QueryParseError> {
    let nodes = tokenize_to_nodes(input)?;
    if nodes.len() != 1 {
        return Err(QueryParseError::new(
            QueryParseErrorKind::InvalidTopLevelForm,
            "query must contain exactly one top-level form",
            None,
        ));
    }

    parse_query_node(&nodes[0], true)
}

fn tokenize_to_nodes(input: &str) -> Result<Vec<RawNode>, QueryParseError> {
    let mut parser = RawParser::new(input);
    let mut nodes = Vec::new();
    parser.skip_whitespace();

    while !parser.is_eof() {
        nodes.push(parser.parse_node()?);
        parser.skip_whitespace();
    }

    Ok(nodes)
}

fn parse_query_node(
    node: &RawNode,
    allow_heading_shorthand: bool,
) -> Result<QueryAst, QueryParseError> {
    let list = expect_list(
        node,
        QueryParseErrorKind::InvalidTopLevelForm,
        "query must be a list",
    )?;
    let Some(operator) = list.items.first() else {
        return Err(QueryParseError::new(
            QueryParseErrorKind::InvalidTopLevelForm,
            "query list must not be empty",
            Some(list.span.tuple()),
        ));
    };

    let operator_name = expect_symbol_name(
        operator,
        QueryParseErrorKind::InvalidTopLevelForm,
        "query form must start with a symbol",
    )?;

    if let Some(target) = parse_target_name(operator_name) {
        parse_explicit_target_query(list, target)
    } else if allow_heading_shorthand {
        Ok(QueryAst {
            target: QueryTarget::Headings,
            predicate: Some(parse_expr(node, false)?),
        })
    } else {
        Err(QueryParseError::new(
            QueryParseErrorKind::MissingExplicitTargetWrapper,
            "nested query must use an explicit target wrapper",
            Some(list.span.tuple()),
        ))
    }
}

fn parse_explicit_target_query(
    list: &RawList,
    target: QueryTarget,
) -> Result<QueryAst, QueryParseError> {
    let predicate = match list.items.len() {
        1 => None,
        2 => Some(parse_expr(&list.items[1], false)?),
        _ => {
            return Err(QueryParseError::new(
                QueryParseErrorKind::StructuralArity,
                format!(
                    "target query {} expects zero or one predicate expression",
                    target_name(target)
                ),
                Some(list.span.tuple()),
            ))
        }
    };

    Ok(QueryAst { target, predicate })
}

fn parse_expr(node: &RawNode, allow_heading_shorthand: bool) -> Result<Expr, QueryParseError> {
    let list = expect_list(
        node,
        QueryParseErrorKind::InvalidPredicateForm,
        "predicate expression must be a list",
    )?;
    let Some(head) = list.items.first() else {
        return Err(QueryParseError::new(
            QueryParseErrorKind::InvalidPredicateForm,
            "predicate expression must not be empty",
            Some(list.span.tuple()),
        ));
    };

    let operator = expect_symbol_name(
        head,
        QueryParseErrorKind::InvalidPredicateForm,
        "predicate expression must start with a symbol",
    )?;

    match operator {
        "and" => parse_boolean_expr("and", &list.items[1..], list.span, Expr::And),
        "or" => parse_boolean_expr("or", &list.items[1..], list.span, Expr::Or),
        "not" => parse_not_expr(&list.items[1..], list.span),
        "source" => parse_relation_query_or_any_call(
            "source",
            &list.items[1..],
            list.span,
            &[QueryTarget::Headings, QueryTarget::Files],
        ),
        "target" => parse_relation_query_or_any_call(
            "target",
            &list.items[1..],
            list.span,
            &[QueryTarget::Headings, QueryTarget::Files],
        ),
        "has-link" => parse_has_link_call(&list.items[1..], list.span),
        "links-to" => parse_relation_query_call(
            "links-to",
            &list.items[1..],
            list.span,
            &[QueryTarget::Headings, QueryTarget::Files],
        ),
        "linked-from" => parse_linked_from_call(&list.items[1..], list.span),
        "parent" | "ancestors" | "children" | "descendants" => parse_optional_relation_query_call(
            operator,
            &list.items[1..],
            list.span,
            &[QueryTarget::Headings],
        ),
        _ => parse_generic_call(
            operator,
            &list.items[1..],
            list.span,
            allow_heading_shorthand,
        ),
    }
}

fn parse_boolean_expr<F>(
    name: &str,
    args: &[RawNode],
    span: Span,
    build: F,
) -> Result<Expr, QueryParseError>
where
    F: FnOnce(Vec<Expr>) -> Expr,
{
    if args.is_empty() {
        return Err(QueryParseError::new(
            QueryParseErrorKind::InvalidBooleanForm,
            format!("{name} expects at least one expression"),
            Some(span.tuple()),
        ));
    }

    let mut parsed = Vec::with_capacity(args.len());
    for arg in args {
        parsed.push(parse_expr(arg, false)?);
    }

    Ok(build(parsed))
}

fn parse_not_expr(args: &[RawNode], span: Span) -> Result<Expr, QueryParseError> {
    if args.len() != 1 {
        return Err(QueryParseError::new(
            QueryParseErrorKind::InvalidBooleanForm,
            "not expects exactly one expression",
            Some(span.tuple()),
        ));
    }

    Ok(Expr::Not(Box::new(parse_expr(&args[0], false)?)))
}

fn parse_relation_query_call(
    name: &str,
    args: &[RawNode],
    span: Span,
    allowed_targets: &[QueryTarget],
) -> Result<Expr, QueryParseError> {
    if args.len() != 1 {
        return Err(QueryParseError::new(
            QueryParseErrorKind::StructuralArity,
            format!("{name} expects exactly one nested query"),
            Some(span.tuple()),
        ));
    }

    let nested = parse_required_nested_query(&args[0], allowed_targets)?;
    Ok(Expr::Call(PredicateCall {
        name: name.to_string(),
        args: vec![PredicateArg::NestedQuery(Box::new(nested))],
        options: Vec::new(),
    }))
}

fn parse_relation_query_or_any_call(
    name: &str,
    args: &[RawNode],
    span: Span,
    allowed_targets: &[QueryTarget],
) -> Result<Expr, QueryParseError> {
    if args.len() != 1 {
        return Err(QueryParseError::new(
            QueryParseErrorKind::StructuralArity,
            format!("{name} expects exactly one argument"),
            Some(span.tuple()),
        ));
    }

    let argument = match &args[0] {
        RawNode::Atom(atom) if atom.kind == RawAtomKind::Keyword && atom.value == "any" => {
            PredicateArg::Scalar(QueryValue::Keyword(atom.value.clone()))
        }
        _ => PredicateArg::NestedQuery(Box::new(parse_required_nested_query(
            &args[0],
            allowed_targets,
        )?)),
    };

    Ok(Expr::Call(PredicateCall {
        name: name.to_string(),
        args: vec![argument],
        options: Vec::new(),
    }))
}

fn parse_optional_relation_query_call(
    name: &str,
    args: &[RawNode],
    span: Span,
    allowed_targets: &[QueryTarget],
) -> Result<Expr, QueryParseError> {
    let parsed_args = match args.len() {
        0 => Vec::new(),
        1 => vec![PredicateArg::NestedQuery(Box::new(
            parse_required_nested_query(&args[0], allowed_targets)?,
        ))],
        _ => {
            return Err(QueryParseError::new(
                QueryParseErrorKind::StructuralArity,
                format!("{name} expects zero or one nested headings query"),
                Some(span.tuple()),
            ))
        }
    };

    Ok(Expr::Call(PredicateCall {
        name: name.to_string(),
        args: parsed_args,
        options: Vec::new(),
    }))
}

fn parse_has_link_call(args: &[RawNode], span: Span) -> Result<Expr, QueryParseError> {
    let parsed_args = match args.len() {
        0 => Vec::new(),
        1 => vec![PredicateArg::NestedQuery(Box::new(
            parse_required_nested_query(&args[0], &[QueryTarget::Links])?,
        ))],
        _ => {
            return Err(QueryParseError::new(
                QueryParseErrorKind::StructuralArity,
                "has-link expects zero or one nested links query",
                Some(span.tuple()),
            ))
        }
    };

    Ok(Expr::Call(PredicateCall {
        name: "has-link".to_string(),
        args: parsed_args,
        options: Vec::new(),
    }))
}

fn parse_linked_from_call(args: &[RawNode], span: Span) -> Result<Expr, QueryParseError> {
    if args.len() != 1 {
        return Err(QueryParseError::new(
            QueryParseErrorKind::StructuralArity,
            "linked-from expects exactly one argument",
            Some(span.tuple()),
        ));
    }

    let argument = match &args[0] {
        RawNode::Atom(atom) if atom.kind == RawAtomKind::Keyword => {
            PredicateArg::Scalar(QueryValue::Keyword(atom.value.clone()))
        }
        _ => PredicateArg::NestedQuery(Box::new(parse_required_nested_query(
            &args[0],
            &[QueryTarget::Headings, QueryTarget::Files],
        )?)),
    };

    Ok(Expr::Call(PredicateCall {
        name: "linked-from".to_string(),
        args: vec![argument],
        options: Vec::new(),
    }))
}

fn parse_required_nested_query(
    node: &RawNode,
    allowed_targets: &[QueryTarget],
) -> Result<QueryAst, QueryParseError> {
    let nested = parse_query_node(node, false)?;
    if allowed_targets.contains(&nested.target) {
        Ok(nested)
    } else {
        Err(QueryParseError::new(
            QueryParseErrorKind::InvalidTargetForm,
            format!(
                "nested query expects one of: {}",
                allowed_targets
                    .iter()
                    .map(|target| target_name(*target))
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
            Some(node_span(node).tuple()),
        ))
    }
}

fn parse_generic_call(
    name: &str,
    args: &[RawNode],
    _span: Span,
    _allow_heading_shorthand: bool,
) -> Result<Expr, QueryParseError> {
    let (positional, options) = split_positional_args_and_options(args)?;
    if name == "tags-all" {
        return parse_tags_all_call(positional, options);
    }

    let mut parsed_args = Vec::with_capacity(positional.len());
    for arg in positional {
        parsed_args.push(PredicateArg::Scalar(parse_scalar_value(arg)?));
    }

    Ok(Expr::Call(PredicateCall {
        name: name.to_string(),
        args: parsed_args,
        options,
    }))
}

fn parse_tags_all_call(
    positional: Vec<&RawNode>,
    mut options: Vec<QueryOption>,
) -> Result<Expr, QueryParseError> {
    if options.iter().any(|option| option.name == "match") {
        return Err(QueryParseError::new(
            QueryParseErrorKind::InvalidOptionSyntax,
            "tags-all does not accept an explicit :match option",
            None,
        ));
    }

    let mut parsed_args = Vec::with_capacity(positional.len());
    for arg in positional {
        parsed_args.push(PredicateArg::Scalar(parse_scalar_value(arg)?));
    }

    options.push(QueryOption {
        name: "match".to_string(),
        value: QueryValue::Keyword("all".to_string()),
    });

    Ok(Expr::Call(PredicateCall {
        name: "tags".to_string(),
        args: parsed_args,
        options,
    }))
}

fn split_positional_args_and_options(
    args: &[RawNode],
) -> Result<(Vec<&RawNode>, Vec<QueryOption>), QueryParseError> {
    let mut positional = Vec::new();
    let mut options = Vec::new();
    let mut index = 0;
    let mut in_options = false;

    while index < args.len() {
        match &args[index] {
            RawNode::Atom(atom) if atom.kind == RawAtomKind::Keyword => {
                in_options = true;
                if index + 1 >= args.len() {
                    return Err(QueryParseError::new(
                        QueryParseErrorKind::InvalidOptionSyntax,
                        format!("option :{} is missing a value", atom.value),
                        Some(atom.span.tuple()),
                    ));
                }

                let value = parse_scalar_value(&args[index + 1]).map_err(|err| {
                    if err.kind == QueryParseErrorKind::InvalidPredicateForm {
                        QueryParseError::new(
                            QueryParseErrorKind::InvalidOptionSyntax,
                            "option values must be scalar atoms",
                            err.byte_range,
                        )
                    } else {
                        err
                    }
                })?;
                options.push(QueryOption {
                    name: atom.value.clone(),
                    value,
                });
                index += 2;
            }
            node if in_options => {
                return Err(QueryParseError::new(
                    QueryParseErrorKind::InvalidOptionSyntax,
                    "positional arguments must not appear after keyword options",
                    Some(node_span(node).tuple()),
                ));
            }
            node => {
                positional.push(node);
                index += 1;
            }
        }
    }

    Ok((positional, options))
}

fn parse_scalar_value(node: &RawNode) -> Result<QueryValue, QueryParseError> {
    match node {
        RawNode::Atom(atom) => Ok(match atom.kind {
            RawAtomKind::String => QueryValue::String(atom.value.clone()),
            RawAtomKind::Integer => {
                QueryValue::Integer(atom.value.parse::<i64>().map_err(|_| {
                    QueryParseError::new(
                        QueryParseErrorKind::Lexical,
                        format!("invalid integer literal {}", atom.value),
                        Some(atom.span.tuple()),
                    )
                })?)
            }
            RawAtomKind::Bool => QueryValue::Bool(atom.value == "t"),
            RawAtomKind::Keyword => QueryValue::Keyword(atom.value.clone()),
            RawAtomKind::Symbol => QueryValue::Symbol(atom.value.clone()),
        }),
        RawNode::List(list) => Err(QueryParseError::new(
            QueryParseErrorKind::InvalidPredicateForm,
            "expected a scalar atom, found a list",
            Some(list.span.tuple()),
        )),
    }
}

fn parse_target_name(value: &str) -> Option<QueryTarget> {
    match value {
        "headings" => Some(QueryTarget::Headings),
        "links" => Some(QueryTarget::Links),
        "files" => Some(QueryTarget::Files),
        _ => None,
    }
}

fn target_name(target: QueryTarget) -> &'static str {
    match target {
        QueryTarget::Headings => "headings",
        QueryTarget::Links => "links",
        QueryTarget::Files => "files",
    }
}

fn expect_list<'a>(
    node: &'a RawNode,
    kind: QueryParseErrorKind,
    message: &str,
) -> Result<&'a RawList, QueryParseError> {
    match node {
        RawNode::List(list) => Ok(list),
        RawNode::Atom(atom) => Err(QueryParseError::new(kind, message, Some(atom.span.tuple()))),
    }
}

fn expect_symbol_name<'a>(
    node: &'a RawNode,
    kind: QueryParseErrorKind,
    message: &str,
) -> Result<&'a str, QueryParseError> {
    match node {
        RawNode::Atom(atom) if atom.kind == RawAtomKind::Symbol => Ok(&atom.value),
        RawNode::Atom(atom) => Err(QueryParseError::new(kind, message, Some(atom.span.tuple()))),
        RawNode::List(list) => Err(QueryParseError::new(kind, message, Some(list.span.tuple()))),
    }
}

fn node_span(node: &RawNode) -> Span {
    match node {
        RawNode::Atom(atom) => atom.span,
        RawNode::List(list) => list.span,
    }
}

struct RawParser<'a> {
    input: &'a str,
    index: usize,
}

impl<'a> RawParser<'a> {
    fn new(input: &'a str) -> Self {
        Self { input, index: 0 }
    }

    fn is_eof(&self) -> bool {
        self.index >= self.input.len()
    }

    fn skip_whitespace(&mut self) {
        while let Some(ch) = self.peek_char() {
            if ch.is_whitespace() {
                self.index += ch.len_utf8();
            } else {
                break;
            }
        }
    }

    fn parse_node(&mut self) -> Result<RawNode, QueryParseError> {
        self.skip_whitespace();
        let Some(ch) = self.peek_char() else {
            return Err(QueryParseError::new(
                QueryParseErrorKind::UnexpectedEof,
                "unexpected end of input",
                None,
            ));
        };

        match ch {
            '(' => self.parse_list(),
            ')' => Err(QueryParseError::new(
                QueryParseErrorKind::UnexpectedToken,
                "unexpected closing parenthesis",
                Some((self.index, self.index + 1)),
            )),
            '"' => self.parse_string(),
            _ => self.parse_atom(),
        }
    }

    fn parse_list(&mut self) -> Result<RawNode, QueryParseError> {
        let start = self.index;
        self.index += 1;
        let mut items = Vec::new();

        loop {
            self.skip_whitespace();
            let Some(ch) = self.peek_char() else {
                return Err(QueryParseError::new(
                    QueryParseErrorKind::UnexpectedEof,
                    "unterminated list",
                    Some((start, self.input.len())),
                ));
            };

            if ch == ')' {
                self.index += 1;
                return Ok(RawNode::List(RawList {
                    items,
                    span: Span {
                        start,
                        end: self.index,
                    },
                }));
            }

            items.push(self.parse_node()?);
        }
    }

    fn parse_string(&mut self) -> Result<RawNode, QueryParseError> {
        let start = self.index;
        self.index += 1;
        let mut value = String::new();

        while let Some(ch) = self.peek_char() {
            self.index += ch.len_utf8();
            match ch {
                '"' => {
                    return Ok(RawNode::Atom(RawAtom {
                        kind: RawAtomKind::String,
                        value,
                        span: Span {
                            start,
                            end: self.index,
                        },
                    }))
                }
                '\\' => {
                    let Some(escaped) = self.peek_char() else {
                        return Err(QueryParseError::new(
                            QueryParseErrorKind::Lexical,
                            "unterminated string escape",
                            Some((start, self.input.len())),
                        ));
                    };
                    self.index += escaped.len_utf8();
                    value.push(match escaped {
                        '\\' => '\\',
                        '"' => '"',
                        'n' => '\n',
                        'r' => '\r',
                        't' => '\t',
                        other => {
                            return Err(QueryParseError::new(
                                QueryParseErrorKind::Lexical,
                                format!("unsupported escape sequence \\{other}"),
                                Some((self.index - escaped.len_utf8() - 1, self.index)),
                            ))
                        }
                    });
                }
                other => value.push(other),
            }
        }

        Err(QueryParseError::new(
            QueryParseErrorKind::Lexical,
            "unterminated string literal",
            Some((start, self.input.len())),
        ))
    }

    fn parse_atom(&mut self) -> Result<RawNode, QueryParseError> {
        let start = self.index;
        while let Some(ch) = self.peek_char() {
            if ch.is_whitespace() || ch == '(' || ch == ')' {
                break;
            }
            self.index += ch.len_utf8();
        }

        let raw = &self.input[start..self.index];
        let kind = classify_atom(raw);
        Ok(RawNode::Atom(RawAtom {
            kind,
            value: match kind {
                RawAtomKind::Keyword => raw.trim_start_matches(':').to_string(),
                _ => raw.to_string(),
            },
            span: Span {
                start,
                end: self.index,
            },
        }))
    }

    fn peek_char(&self) -> Option<char> {
        self.input[self.index..].chars().next()
    }
}

fn classify_atom(value: &str) -> RawAtomKind {
    if value == "t" || value == "nil" {
        RawAtomKind::Bool
    } else if value.starts_with(':') && value.len() > 1 {
        RawAtomKind::Keyword
    } else if is_integer(value) {
        RawAtomKind::Integer
    } else {
        RawAtomKind::Symbol
    }
}

fn is_integer(value: &str) -> bool {
    let digits = value.strip_prefix('-').unwrap_or(value);
    !digits.is_empty() && digits.chars().all(|ch| ch.is_ascii_digit())
}

#[cfg(test)]
mod tests {
    use super::parse_query;
    use crate::query::{Expr, PredicateArg, QueryOption, QueryTarget, QueryValue};

    #[test]
    fn normalizes_tags_all_to_tags_with_match_all() {
        let query = parse_query("(tags-all \"project\" \"urgent\")").expect("query should parse");
        assert_eq!(query.target, QueryTarget::Headings);

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
}
