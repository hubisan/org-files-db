use std::{collections::HashSet, fmt};

use serde::Serialize;

use super::ast::{
    Expr, PredicateArg, PredicateCall, QueryAst, QueryOption, QueryTarget, QueryValue,
};
use super::priority::normalize_priority_value;

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct ValidatedQuery {
    pub target: QueryTarget,
    pub predicate: Option<ValidatedExpr>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub enum ValidatedExpr {
    And(Vec<ValidatedExpr>),
    Or(Vec<ValidatedExpr>),
    Not(Box<ValidatedExpr>),
    Predicate(ValidatedPredicate),
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct ValidatedPredicate {
    pub target: QueryTarget,
    pub name: String,
    pub args: Vec<ValidatedArg>,
    pub options: Vec<ValidatedOption>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub enum ValidatedArg {
    Scalar(QueryValue),
    NestedQuery(Box<ValidatedQuery>),
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct ValidatedOption {
    pub name: String,
    pub value: QueryValue,
}

#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct QueryValidationOptions {
    pub body_text_available: bool,
    pub regexp_matching_supported: bool,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum QueryValidationErrorKind {
    UnsupportedOperator,
    InvalidTarget,
    WrongArity,
    InvalidOption,
    InvalidOptionCombination,
    InvalidValue,
    UnsupportedBackendFeature,
    UnavailableIndexedData,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct QueryValidationError {
    pub kind: QueryValidationErrorKind,
    pub target: QueryTarget,
    pub predicate: String,
    pub message: String,
}

impl QueryValidationError {
    fn new(
        kind: QueryValidationErrorKind,
        target: QueryTarget,
        predicate: impl Into<String>,
        message: impl Into<String>,
    ) -> Self {
        Self {
            kind,
            target,
            predicate: predicate.into(),
            message: message.into(),
        }
    }
}

impl fmt::Display for QueryValidationError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{} (target {}, predicate {})",
            self.message,
            target_name(self.target),
            self.predicate
        )
    }
}

impl std::error::Error for QueryValidationError {}

pub fn validate_query(
    ast: QueryAst,
    options: &QueryValidationOptions,
) -> Result<ValidatedQuery, QueryValidationError> {
    Ok(ValidatedQuery {
        target: ast.target,
        predicate: match ast.predicate {
            Some(predicate) => Some(validate_expr(predicate, ast.target, options)?),
            None => None,
        },
    })
}

fn validate_expr(
    expr: Expr,
    target: QueryTarget,
    options: &QueryValidationOptions,
) -> Result<ValidatedExpr, QueryValidationError> {
    match expr {
        Expr::And(children) => Ok(ValidatedExpr::And(
            children
                .into_iter()
                .map(|child| validate_expr(child, target, options))
                .collect::<Result<Vec<_>, _>>()?,
        )),
        Expr::Or(children) => Ok(ValidatedExpr::Or(
            children
                .into_iter()
                .map(|child| validate_expr(child, target, options))
                .collect::<Result<Vec<_>, _>>()?,
        )),
        Expr::Not(child) => Ok(ValidatedExpr::Not(Box::new(validate_expr(
            *child, target, options,
        )?))),
        Expr::Call(call) => Ok(ValidatedExpr::Predicate(validate_call(
            call, target, options,
        )?)),
    }
}

fn validate_call(
    call: PredicateCall,
    target: QueryTarget,
    options: &QueryValidationOptions,
) -> Result<ValidatedPredicate, QueryValidationError> {
    match call.name.as_str() {
        "todo" => validate_todo(call, target),
        "done" => validate_done(call, target),
        "title" => match target {
            QueryTarget::Headings => validate_text_predicate(
                call,
                target,
                options,
                &["regexp", "exact"],
                ExactRule::SingleArg,
            ),
            QueryTarget::Files | QueryTarget::Links => validate_text_predicate(
                call,
                target,
                options,
                &["regexp", "exact"],
                ExactRule::SingleArg,
            ),
        },
        "has-text" => validate_has_text(call, target, options),
        "level" => validate_level(call, target),
        "priority" => validate_priority(call, target),
        "tags" => validate_tags(call, target, options),
        "property" => validate_property(call, target, options),
        "keyword" => validate_keyword(call, target, options),
        "file-name" | "file-path" | "file-dir" | "file-title" => validate_text_predicate(
            call,
            target,
            options,
            &["regexp", "exact"],
            ExactRule::SingleArg,
        ),
        "file-modified" => validate_keyword_only_date_predicate(call, target, false),
        "outline-contains" => {
            validate_text_predicate(call, target, options, &["regexp"], ExactRule::NotSupported)
        }
        "outline-sequence" => validate_text_predicate(
            call,
            target,
            options,
            &["regexp", "exact"],
            ExactRule::PerSegmentAllowed,
        ),
        "ts" | "ts-active" | "ts-inactive" | "deadline" | "scheduled" | "closed" | "planning" => {
            validate_keyword_only_date_predicate(call, target, true)
        }
        "parent" | "ancestors" | "children" | "descendants" => {
            validate_hierarchy(call, target, options)
        }
        "has-link" => validate_has_link(call, target, options),
        "links-to" => validate_links_to(call, target, options),
        "linked-from" => validate_linked_from(call, target, options),
        "link-type" => validate_link_type(call, target),
        "link-target" | "link-description" => validate_text_predicate(
            call,
            target,
            options,
            &["regexp", "exact"],
            ExactRule::SingleArg,
        ),
        "has-description" => validate_has_description(call, target),
        "status" => validate_status(call, target),
        "source" | "target" => validate_source_or_target(call, target, options),
        "tags-regexp" => Err(QueryValidationError::new(
            QueryValidationErrorKind::UnsupportedOperator,
            target,
            "tags-regexp",
            "unsupported operator: tags-regexp; use (tags ... :regexp t) instead",
        )),
        other if predicate_is_known_globally(other) => Err(QueryValidationError::new(
            QueryValidationErrorKind::InvalidTarget,
            target,
            other,
            format!(
                "predicate {other} is not valid for target {}",
                target_name(target)
            ),
        )),
        other => Err(QueryValidationError::new(
            QueryValidationErrorKind::UnsupportedOperator,
            target,
            other,
            format!("unsupported operator: {other}"),
        )),
    }
}

fn validate_todo(
    call: PredicateCall,
    target: QueryTarget,
) -> Result<ValidatedPredicate, QueryValidationError> {
    ensure_target(target, &call.name, &[QueryTarget::Headings])?;
    let options = validate_options(target, &call.name, &call.options, &[])?;
    let args = validate_scalar_strings(target, &call.name, &call.args, Arity::ZeroOrMore)?;
    Ok(validated_predicate(target, call.name, args, options))
}

fn validate_done(
    call: PredicateCall,
    target: QueryTarget,
) -> Result<ValidatedPredicate, QueryValidationError> {
    ensure_target(target, &call.name, &[QueryTarget::Headings])?;
    let options = validate_options(target, &call.name, &call.options, &[])?;
    ensure_arg_count(target, &call.name, &call.args, 0, 0)?;
    Ok(validated_predicate(target, call.name, Vec::new(), options))
}

fn validate_has_text(
    call: PredicateCall,
    target: QueryTarget,
    validation: &QueryValidationOptions,
) -> Result<ValidatedPredicate, QueryValidationError> {
    ensure_target(target, &call.name, &[QueryTarget::Headings])?;
    let options = validate_options(target, &call.name, &call.options, &["regexp"])?;
    let regexp = bool_option(target, &call.name, &options, "regexp")?.unwrap_or(false);
    let args = validate_scalar_strings(target, &call.name, &call.args, Arity::OneOrMore)?;

    if !validation.body_text_available {
        return Err(QueryValidationError::new(
            QueryValidationErrorKind::UnavailableIndexedData,
            target,
            call.name,
            "has-text requires body text to be available in the database",
        ));
    }
    if regexp && !validation.regexp_matching_supported {
        return Err(QueryValidationError::new(
            QueryValidationErrorKind::UnsupportedBackendFeature,
            target,
            "has-text",
            "has-text with :regexp t is not supported by the current backend",
        ));
    }

    Ok(validated_predicate(target, call.name, args, options))
}

fn validate_level(
    call: PredicateCall,
    target: QueryTarget,
) -> Result<ValidatedPredicate, QueryValidationError> {
    ensure_target(target, &call.name, &[QueryTarget::Headings])?;
    let options = validate_options(target, &call.name, &call.options, &[])?;
    let args = validate_all_scalar(target, &call.name, &call.args)?;

    match args.as_slice() {
        [ValidatedArg::Scalar(QueryValue::Integer(_))] => {}
        [ValidatedArg::Scalar(QueryValue::Integer(_)), ValidatedArg::Scalar(QueryValue::Integer(_))] =>
            {}
        [ValidatedArg::Scalar(QueryValue::Symbol(symbol)), ValidatedArg::Scalar(QueryValue::Integer(_))]
            if is_comparator(symbol) => {}
        _ => {
            return Err(QueryValidationError::new(
                QueryValidationErrorKind::WrongArity,
                target,
                "level",
                "level expects (level NUMBER), (level MIN MAX), or (level COMPARATOR NUMBER)",
            ))
        }
    }

    Ok(validated_predicate(target, call.name, args, options))
}

fn validate_priority(
    call: PredicateCall,
    target: QueryTarget,
) -> Result<ValidatedPredicate, QueryValidationError> {
    ensure_target(target, &call.name, &[QueryTarget::Headings])?;
    let options = validate_options(target, &call.name, &call.options, &[])?;
    let args = validate_all_scalar(target, &call.name, &call.args)?;

    match args.as_slice() {
        [] => {
            return Err(QueryValidationError::new(
                QueryValidationErrorKind::WrongArity,
                target,
                "priority",
                "priority expects one or more priority values, or (priority COMPARATOR PRIORITY)",
            ))
        }
        [ValidatedArg::Scalar(QueryValue::Symbol(symbol)), value] if is_comparator(symbol) => {
            normalize_priority_arg(target, value)?;
        }
        values => {
            for value in values {
                normalize_priority_arg(target, value)?;
            }
        }
    }

    Ok(validated_predicate(target, call.name, args, options))
}

fn normalize_priority_arg(
    target: QueryTarget,
    value: &ValidatedArg,
) -> Result<(), QueryValidationError> {
    let ValidatedArg::Scalar(value) = value else {
        unreachable!("priority arguments are scalar after validation");
    };
    normalize_priority_value(value)
        .map(|_| ())
        .map_err(|message| {
            QueryValidationError::new(
                QueryValidationErrorKind::InvalidValue,
                target,
                "priority",
                message,
            )
        })
}

fn validate_tags(
    call: PredicateCall,
    target: QueryTarget,
    validation: &QueryValidationOptions,
) -> Result<ValidatedPredicate, QueryValidationError> {
    ensure_target(
        target,
        &call.name,
        &[QueryTarget::Headings, QueryTarget::Files],
    )?;
    let allowed = match target {
        QueryTarget::Headings => &["inherit", "regexp", "match"][..],
        QueryTarget::Files => &["regexp", "match"][..],
        QueryTarget::Links => &[][..],
    };
    let options = validate_options(target, &call.name, &call.options, allowed)?;
    let regexp = bool_option(target, &call.name, &options, "regexp")?.unwrap_or(false);
    let _inherit = optional_bool_option(target, &call.name, &options, "inherit")?;
    let match_mode = keyword_option(target, &call.name, &options, "match")?;
    if let Some(match_mode) = match_mode.as_deref() {
        if match_mode != "any" && match_mode != "all" {
            return Err(QueryValidationError::new(
                QueryValidationErrorKind::InvalidValue,
                target,
                "tags",
                "tags :match must be :any or :all",
            ));
        }
    }

    let args = validate_scalar_strings(target, &call.name, &call.args, Arity::OneOrMore)?;
    if regexp && !validation.regexp_matching_supported {
        return Err(QueryValidationError::new(
            QueryValidationErrorKind::UnsupportedBackendFeature,
            target,
            "tags",
            "tags with :regexp t are not supported by the current backend",
        ));
    }
    Ok(validated_predicate(target, call.name, args, options))
}

fn validate_property(
    call: PredicateCall,
    target: QueryTarget,
    validation: &QueryValidationOptions,
) -> Result<ValidatedPredicate, QueryValidationError> {
    ensure_target(
        target,
        &call.name,
        &[QueryTarget::Headings, QueryTarget::Files],
    )?;
    let allowed = match target {
        QueryTarget::Headings => &["inherit", "regexp"][..],
        QueryTarget::Files => &["regexp"][..],
        QueryTarget::Links => &[][..],
    };
    let options = validate_options(target, &call.name, &call.options, allowed)?;
    let _regexp = optional_bool_option(target, &call.name, &options, "regexp")?;
    let _inherit = optional_bool_option(target, &call.name, &options, "inherit")?;
    let args = validate_scalar_strings(target, &call.name, &call.args, Arity::OneOrTwo)?;

    if bool_option(target, &call.name, &options, "regexp")?.unwrap_or(false) && args.len() != 2 {
        return Err(QueryValidationError::new(
            QueryValidationErrorKind::InvalidOptionCombination,
            target,
            "property",
            "property :regexp t requires a property value argument",
        ));
    }
    if bool_option(target, &call.name, &options, "regexp")?.unwrap_or(false)
        && !validation.regexp_matching_supported
    {
        return Err(QueryValidationError::new(
            QueryValidationErrorKind::UnsupportedBackendFeature,
            target,
            "property",
            "property with :regexp t is not supported by the current backend",
        ));
    }

    Ok(validated_predicate(target, call.name, args, options))
}

fn validate_keyword(
    call: PredicateCall,
    target: QueryTarget,
    validation: &QueryValidationOptions,
) -> Result<ValidatedPredicate, QueryValidationError> {
    ensure_target(
        target,
        &call.name,
        &[QueryTarget::Headings, QueryTarget::Files],
    )?;
    let allowed_options = match target {
        QueryTarget::Headings => &["regexp", "inherit"][..],
        QueryTarget::Files => &["regexp"][..],
        QueryTarget::Links => unreachable!("keyword target was validated above"),
    };
    let options = validate_options(target, &call.name, &call.options, allowed_options)?;
    if target == QueryTarget::Headings {
        bool_option(target, &call.name, &options, "inherit")?;
    }
    let args = validate_scalar_strings(target, &call.name, &call.args, Arity::OneOrTwo)?;

    if bool_option(target, &call.name, &options, "regexp")?.unwrap_or(false) && args.len() != 2 {
        return Err(QueryValidationError::new(
            QueryValidationErrorKind::InvalidOptionCombination,
            target,
            "keyword",
            "keyword :regexp t requires a keyword value argument",
        ));
    }
    if bool_option(target, &call.name, &options, "regexp")?.unwrap_or(false)
        && !validation.regexp_matching_supported
    {
        return Err(QueryValidationError::new(
            QueryValidationErrorKind::UnsupportedBackendFeature,
            target,
            "keyword",
            "keyword with :regexp t is not supported by the current backend",
        ));
    }

    Ok(validated_predicate(target, call.name, args, options))
}

fn validate_text_predicate(
    call: PredicateCall,
    target: QueryTarget,
    validation: &QueryValidationOptions,
    allowed_options: &[&str],
    exact_rule: ExactRule,
) -> Result<ValidatedPredicate, QueryValidationError> {
    ensure_text_predicate_target(target, &call.name)?;
    let options = validate_options(target, &call.name, &call.options, allowed_options)?;
    let regexp = bool_option(target, &call.name, &options, "regexp")?.unwrap_or(false);
    let exact = bool_option(target, &call.name, &options, "exact")?.unwrap_or(false);
    let args = validate_scalar_strings(target, &call.name, &call.args, Arity::OneOrMore)?;

    if regexp && exact {
        return Err(QueryValidationError::new(
            QueryValidationErrorKind::InvalidOptionCombination,
            target,
            &call.name,
            format!(
                "{} does not allow :regexp t together with :exact t",
                call.name
            ),
        ));
    }
    if regexp && !validation.regexp_matching_supported {
        return Err(QueryValidationError::new(
            QueryValidationErrorKind::UnsupportedBackendFeature,
            target,
            &call.name,
            format!(
                "{} with :regexp t is not supported by the current backend",
                call.name
            ),
        ));
    }

    if exact {
        match exact_rule {
            ExactRule::NotSupported => {
                return Err(QueryValidationError::new(
                    QueryValidationErrorKind::InvalidOption,
                    target,
                    &call.name,
                    format!("{} does not support :exact", call.name),
                ))
            }
            ExactRule::SingleArg if args.len() != 1 => {
                return Err(QueryValidationError::new(
                    QueryValidationErrorKind::InvalidOptionCombination,
                    target,
                    &call.name,
                    ":exact t accepts exactly one text argument",
                ))
            }
            ExactRule::PerSegmentAllowed => {}
            ExactRule::SingleArg => {}
        }
    }

    Ok(validated_predicate(target, call.name, args, options))
}

fn validate_keyword_only_date_predicate(
    call: PredicateCall,
    target: QueryTarget,
    allow_zero_options: bool,
) -> Result<ValidatedPredicate, QueryValidationError> {
    ensure_date_predicate_target(target, &call.name)?;
    let options = validate_options(target, &call.name, &call.options, &["from", "to", "on"])?;
    ensure_arg_count(target, &call.name, &call.args, 0, 0)?;
    validate_date_options(target, &call.name, &options)?;

    let has_date_bound = options
        .iter()
        .any(|option| matches!(option.name.as_str(), "from" | "to" | "on"));
    if !allow_zero_options && !has_date_bound {
        return Err(QueryValidationError::new(
            QueryValidationErrorKind::WrongArity,
            target,
            &call.name,
            format!("{} requires at least one of :from, :to, or :on", call.name),
        ));
    }

    Ok(validated_predicate(target, call.name, Vec::new(), options))
}

fn validate_hierarchy(
    call: PredicateCall,
    target: QueryTarget,
    options: &QueryValidationOptions,
) -> Result<ValidatedPredicate, QueryValidationError> {
    ensure_target(target, &call.name, &[QueryTarget::Headings])?;
    let validated_options = validate_options(target, &call.name, &call.options, &[])?;

    let args = match call.args.len() {
        0 => Vec::new(),
        1 => vec![validate_nested_headings_arg(
            target,
            &call.name,
            call.args.into_iter().next().unwrap(),
            options,
        )?],
        _ => {
            return Err(QueryValidationError::new(
                QueryValidationErrorKind::WrongArity,
                target,
                &call.name,
                format!("{} accepts zero args or one headings query", call.name),
            ))
        }
    };

    Ok(validated_predicate(
        target,
        call.name,
        args,
        validated_options,
    ))
}

fn validate_has_link(
    call: PredicateCall,
    target: QueryTarget,
    options: &QueryValidationOptions,
) -> Result<ValidatedPredicate, QueryValidationError> {
    ensure_target(
        target,
        &call.name,
        &[QueryTarget::Headings, QueryTarget::Files],
    )?;
    let validated_options = validate_options(target, &call.name, &call.options, &[])?;

    let args = match call.args.len() {
        0 => Vec::new(),
        1 => vec![validate_nested_target_arg(
            target,
            &call.name,
            call.args.into_iter().next().unwrap(),
            &[QueryTarget::Links],
            options,
        )?],
        _ => {
            return Err(QueryValidationError::new(
                QueryValidationErrorKind::WrongArity,
                target,
                "has-link",
                "has-link accepts zero args or one links query",
            ))
        }
    };

    Ok(validated_predicate(
        target,
        call.name,
        args,
        validated_options,
    ))
}

fn validate_links_to(
    call: PredicateCall,
    target: QueryTarget,
    options: &QueryValidationOptions,
) -> Result<ValidatedPredicate, QueryValidationError> {
    ensure_target(
        target,
        &call.name,
        &[QueryTarget::Headings, QueryTarget::Files],
    )?;
    let validated_options = validate_options(target, &call.name, &call.options, &[])?;
    ensure_arg_count(target, &call.name, &call.args, 1, 1)?;
    let arg = validate_nested_target_arg(
        target,
        &call.name,
        call.args.into_iter().next().unwrap(),
        &[QueryTarget::Headings, QueryTarget::Files],
        options,
    )?;

    Ok(validated_predicate(
        target,
        call.name,
        vec![arg],
        validated_options,
    ))
}

fn validate_linked_from(
    call: PredicateCall,
    target: QueryTarget,
    options: &QueryValidationOptions,
) -> Result<ValidatedPredicate, QueryValidationError> {
    ensure_target(
        target,
        &call.name,
        &[QueryTarget::Headings, QueryTarget::Files],
    )?;
    let validated_options = validate_options(target, &call.name, &call.options, &[])?;
    ensure_arg_count(target, &call.name, &call.args, 1, 1)?;
    let arg = validate_any_or_nested_targets(
        target,
        &call.name,
        call.args.into_iter().next().unwrap(),
        &[QueryTarget::Headings, QueryTarget::Files],
        options,
    )?;

    Ok(validated_predicate(
        target,
        call.name,
        vec![arg],
        validated_options,
    ))
}

fn validate_link_type(
    call: PredicateCall,
    target: QueryTarget,
) -> Result<ValidatedPredicate, QueryValidationError> {
    ensure_target(target, &call.name, &[QueryTarget::Links])?;
    let options = validate_options(target, &call.name, &call.options, &[])?;
    let args = validate_scalar_strings(target, &call.name, &call.args, Arity::OneOrMore)?;
    Ok(validated_predicate(target, call.name, args, options))
}

fn validate_has_description(
    call: PredicateCall,
    target: QueryTarget,
) -> Result<ValidatedPredicate, QueryValidationError> {
    ensure_target(target, &call.name, &[QueryTarget::Links])?;
    let options = validate_options(target, &call.name, &call.options, &[])?;
    ensure_arg_count(target, &call.name, &call.args, 0, 0)?;
    Ok(validated_predicate(target, call.name, Vec::new(), options))
}

fn validate_status(
    call: PredicateCall,
    target: QueryTarget,
) -> Result<ValidatedPredicate, QueryValidationError> {
    ensure_target(target, &call.name, &[QueryTarget::Links])?;
    let options = validate_options(target, &call.name, &call.options, &[])?;
    let args = validate_scalar_strings(target, &call.name, &call.args, Arity::OneOrMore)?;
    Ok(validated_predicate(target, call.name, args, options))
}

fn validate_source_or_target(
    call: PredicateCall,
    target: QueryTarget,
    options: &QueryValidationOptions,
) -> Result<ValidatedPredicate, QueryValidationError> {
    ensure_target(target, &call.name, &[QueryTarget::Links])?;
    let validated_options = validate_options(target, &call.name, &call.options, &[])?;
    ensure_arg_count(target, &call.name, &call.args, 1, 1)?;
    let arg = validate_any_or_nested_targets(
        target,
        &call.name,
        call.args.into_iter().next().unwrap(),
        &[QueryTarget::Headings, QueryTarget::Files],
        options,
    )?;
    Ok(validated_predicate(
        target,
        call.name,
        vec![arg],
        validated_options,
    ))
}

fn validate_nested_headings_arg(
    target: QueryTarget,
    predicate: &str,
    arg: PredicateArg,
    options: &QueryValidationOptions,
) -> Result<ValidatedArg, QueryValidationError> {
    validate_nested_target_arg(target, predicate, arg, &[QueryTarget::Headings], options)
}

fn validate_nested_target_arg(
    target: QueryTarget,
    predicate: &str,
    arg: PredicateArg,
    allowed_targets: &[QueryTarget],
    options: &QueryValidationOptions,
) -> Result<ValidatedArg, QueryValidationError> {
    match arg {
        PredicateArg::NestedQuery(query) => {
            if !allowed_targets.contains(&query.target) {
                return Err(QueryValidationError::new(
                    QueryValidationErrorKind::InvalidTarget,
                    target,
                    predicate,
                    format!(
                        "{} requires a nested query targeting {}",
                        predicate,
                        allowed_targets
                            .iter()
                            .map(|target| target_name(*target))
                            .collect::<Vec<_>>()
                            .join(" or ")
                    ),
                ));
            }
            Ok(ValidatedArg::NestedQuery(Box::new(validate_query(
                *query, options,
            )?)))
        }
        PredicateArg::Scalar(_) => Err(QueryValidationError::new(
            QueryValidationErrorKind::InvalidValue,
            target,
            predicate,
            format!("{predicate} requires a nested query argument"),
        )),
    }
}

fn validate_any_or_nested_targets(
    target: QueryTarget,
    predicate: &str,
    arg: PredicateArg,
    allowed_targets: &[QueryTarget],
    options: &QueryValidationOptions,
) -> Result<ValidatedArg, QueryValidationError> {
    match arg {
        PredicateArg::Scalar(QueryValue::Keyword(value)) if value == "any" => {
            Ok(ValidatedArg::Scalar(QueryValue::Keyword(value)))
        }
        other => validate_nested_target_arg(target, predicate, other, allowed_targets, options),
    }
}

fn ensure_target(
    target: QueryTarget,
    predicate: &str,
    allowed_targets: &[QueryTarget],
) -> Result<(), QueryValidationError> {
    if allowed_targets.contains(&target) {
        Ok(())
    } else {
        Err(QueryValidationError::new(
            QueryValidationErrorKind::InvalidTarget,
            target,
            predicate,
            format!(
                "predicate {predicate} is not valid for target {}",
                target_name(target)
            ),
        ))
    }
}

fn ensure_text_predicate_target(
    target: QueryTarget,
    predicate: &str,
) -> Result<(), QueryValidationError> {
    let allowed = match predicate {
        "title" | "has-text" | "outline-contains" | "outline-sequence" => {
            &[QueryTarget::Headings][..]
        }
        "link-target" | "link-description" => &[QueryTarget::Links][..],
        "file-name" | "file-path" | "file-dir" | "file-title" => {
            &[QueryTarget::Headings, QueryTarget::Files][..]
        }
        _ => &[
            QueryTarget::Headings,
            QueryTarget::Links,
            QueryTarget::Files,
        ][..],
    };
    ensure_target(target, predicate, allowed)
}

fn ensure_date_predicate_target(
    target: QueryTarget,
    predicate: &str,
) -> Result<(), QueryValidationError> {
    let allowed = match predicate {
        "file-modified" => &[QueryTarget::Headings, QueryTarget::Files][..],
        _ => &[QueryTarget::Headings][..],
    };
    ensure_target(target, predicate, allowed)
}

fn validate_options(
    target: QueryTarget,
    predicate: &str,
    options: &[QueryOption],
    allowed: &[&str],
) -> Result<Vec<ValidatedOption>, QueryValidationError> {
    let allowed: HashSet<&str> = allowed.iter().copied().collect();
    let mut seen = HashSet::new();
    let mut validated = Vec::with_capacity(options.len());

    for option in options {
        if option.name == "with-root" {
            return Err(QueryValidationError::new(
                QueryValidationErrorKind::InvalidOption,
                target,
                predicate,
                format!(
                    "unknown option for {predicate}: :with-root; heading queries always include file/root participation where the predicate semantics allow it"
                ),
            ));
        }
        if !allowed.contains(option.name.as_str()) {
            return Err(QueryValidationError::new(
                QueryValidationErrorKind::InvalidOption,
                target,
                predicate,
                format!("unknown option for {predicate}: :{}", option.name),
            ));
        }
        if !seen.insert(option.name.as_str()) {
            return Err(QueryValidationError::new(
                QueryValidationErrorKind::InvalidOption,
                target,
                predicate,
                format!("duplicate option for {predicate}: :{}", option.name),
            ));
        }
        validated.push(ValidatedOption {
            name: option.name.clone(),
            value: option.value.clone(),
        });
    }

    Ok(validated)
}

fn ensure_arg_count(
    target: QueryTarget,
    predicate: &str,
    args: &[PredicateArg],
    min: usize,
    max: usize,
) -> Result<(), QueryValidationError> {
    if args.len() < min || args.len() > max {
        Err(QueryValidationError::new(
            QueryValidationErrorKind::WrongArity,
            target,
            predicate,
            if min == max {
                format!("{predicate} expects exactly {min} argument(s)")
            } else {
                format!("{predicate} expects between {min} and {max} arguments")
            },
        ))
    } else {
        Ok(())
    }
}

fn validate_scalar_strings(
    target: QueryTarget,
    predicate: &str,
    args: &[PredicateArg],
    arity: Arity,
) -> Result<Vec<ValidatedArg>, QueryValidationError> {
    validate_scalar_values(
        target,
        predicate,
        args,
        arity,
        |value| matches!(value, QueryValue::String(_)),
        "string",
    )
}

fn validate_all_scalar(
    target: QueryTarget,
    predicate: &str,
    args: &[PredicateArg],
) -> Result<Vec<ValidatedArg>, QueryValidationError> {
    args.iter()
        .map(|arg| match arg {
            PredicateArg::Scalar(value) => Ok(ValidatedArg::Scalar(value.clone())),
            PredicateArg::NestedQuery(_) => Err(QueryValidationError::new(
                QueryValidationErrorKind::InvalidValue,
                target,
                predicate,
                format!("{predicate} does not accept nested query arguments"),
            )),
        })
        .collect()
}

fn validate_scalar_values<F>(
    target: QueryTarget,
    predicate: &str,
    args: &[PredicateArg],
    arity: Arity,
    predicate_fn: F,
    expected: &str,
) -> Result<Vec<ValidatedArg>, QueryValidationError>
where
    F: Fn(&QueryValue) -> bool,
{
    let count_ok = match arity {
        Arity::ZeroOrMore => true,
        Arity::OneOrMore => !args.is_empty(),
        Arity::OneOrTwo => (1..=2).contains(&args.len()),
    };
    if !count_ok {
        return Err(QueryValidationError::new(
            QueryValidationErrorKind::WrongArity,
            target,
            predicate,
            match arity {
                Arity::ZeroOrMore => {
                    format!("{predicate} accepts zero or more {expected} arguments")
                }
                Arity::OneOrMore => format!("{predicate} expects one or more {expected} arguments"),
                Arity::OneOrTwo => format!("{predicate} expects one or two {expected} arguments"),
            },
        ));
    }

    args.iter()
        .map(|arg| match arg {
            PredicateArg::Scalar(value) if predicate_fn(value) => {
                Ok(ValidatedArg::Scalar(value.clone()))
            }
            PredicateArg::Scalar(_) => Err(QueryValidationError::new(
                QueryValidationErrorKind::InvalidValue,
                target,
                predicate,
                format!("{predicate} expects {expected} arguments"),
            )),
            PredicateArg::NestedQuery(_) => Err(QueryValidationError::new(
                QueryValidationErrorKind::InvalidValue,
                target,
                predicate,
                format!("{predicate} does not accept nested query arguments"),
            )),
        })
        .collect()
}

fn bool_option(
    target: QueryTarget,
    predicate: &str,
    options: &[ValidatedOption],
    name: &str,
) -> Result<Option<bool>, QueryValidationError> {
    optional_bool_option(target, predicate, options, name)
}

fn optional_bool_option(
    target: QueryTarget,
    predicate: &str,
    options: &[ValidatedOption],
    name: &str,
) -> Result<Option<bool>, QueryValidationError> {
    match options.iter().find(|option| option.name == name) {
        Some(option) => match option.value {
            QueryValue::Bool(value) => Ok(Some(value)),
            _ => Err(QueryValidationError::new(
                QueryValidationErrorKind::InvalidValue,
                target,
                predicate,
                format!(":{} for {} must be t or nil", name, predicate),
            )),
        },
        None => Ok(None),
    }
}

fn keyword_option(
    target: QueryTarget,
    predicate: &str,
    options: &[ValidatedOption],
    name: &str,
) -> Result<Option<String>, QueryValidationError> {
    match options.iter().find(|option| option.name == name) {
        Some(option) => match &option.value {
            QueryValue::Keyword(value) => Ok(Some(value.clone())),
            _ => Err(QueryValidationError::new(
                QueryValidationErrorKind::InvalidValue,
                target,
                predicate,
                format!(":{} for {} must be a keyword value", name, predicate),
            )),
        },
        None => Ok(None),
    }
}

fn validate_date_options(
    target: QueryTarget,
    predicate: &str,
    options: &[ValidatedOption],
) -> Result<(), QueryValidationError> {
    for option in options {
        match option.name.as_str() {
            "from" | "to" | "on" => {
                validate_date_value(target, predicate, &option.name, &option.value)?
            }
            _ => {}
        }
    }
    Ok(())
}

fn validate_date_value(
    target: QueryTarget,
    predicate: &str,
    option_name: &str,
    value: &QueryValue,
) -> Result<(), QueryValidationError> {
    match value {
        QueryValue::String(_) | QueryValue::Integer(_) => Ok(()),
        QueryValue::Symbol(symbol) if symbol == "today" => Ok(()),
        _ => Err(QueryValidationError::new(
            QueryValidationErrorKind::InvalidValue,
            target,
            predicate,
            format!(
                ":{} for {} must be today, an integer day offset, or a date/datetime string",
                option_name, predicate
            ),
        )),
    }
}

fn validated_predicate(
    target: QueryTarget,
    name: String,
    args: Vec<ValidatedArg>,
    options: Vec<ValidatedOption>,
) -> ValidatedPredicate {
    ValidatedPredicate {
        target,
        name,
        args,
        options,
    }
}

fn predicate_is_known_globally(name: &str) -> bool {
    matches!(
        name,
        "todo"
            | "done"
            | "title"
            | "has-text"
            | "level"
            | "priority"
            | "tags"
            | "property"
            | "keyword"
            | "file-name"
            | "file-path"
            | "file-dir"
            | "file-title"
            | "file-modified"
            | "outline-contains"
            | "outline-sequence"
            | "ts"
            | "ts-active"
            | "ts-inactive"
            | "deadline"
            | "scheduled"
            | "closed"
            | "planning"
            | "parent"
            | "ancestors"
            | "children"
            | "descendants"
            | "has-link"
            | "links-to"
            | "linked-from"
            | "link-type"
            | "link-target"
            | "link-description"
            | "has-description"
            | "status"
            | "source"
            | "target"
    )
}

fn is_comparator(value: &str) -> bool {
    matches!(value, "<" | "<=" | ">" | ">=")
}

fn target_name(target: QueryTarget) -> &'static str {
    match target {
        QueryTarget::Headings => "headings",
        QueryTarget::Links => "links",
        QueryTarget::Files => "files",
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ExactRule {
    NotSupported,
    SingleArg,
    PerSegmentAllowed,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Arity {
    ZeroOrMore,
    OneOrMore,
    OneOrTwo,
}
