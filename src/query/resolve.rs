use std::{fmt, str::FromStr};

use chrono::{DateTime, Days, Local, NaiveDate, Utc};
use chrono_tz::Tz;

use super::{
    QueryValue, ValidatedArg, ValidatedExpr, ValidatedOption, ValidatedPredicate, ValidatedQuery,
};

#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct QueryDateResolutionOptions {
    pub timezone: Option<String>,
    pub now_utc: Option<DateTime<Utc>>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum QueryDateResolutionErrorKind {
    InvalidTimezone,
    UnresolvedRelativeDate,
    DateOutOfRange,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct QueryDateResolutionError {
    pub kind: QueryDateResolutionErrorKind,
    pub message: String,
}

impl fmt::Display for QueryDateResolutionError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(&self.message)
    }
}

impl std::error::Error for QueryDateResolutionError {}

pub fn resolve_relative_dates(
    query: &ValidatedQuery,
    options: &QueryDateResolutionOptions,
) -> Result<ValidatedQuery, QueryDateResolutionError> {
    let today = effective_today(options)?;
    Ok(ValidatedQuery {
        target: query.target,
        predicate: query
            .predicate
            .as_ref()
            .map(|predicate| resolve_expr(predicate, today))
            .transpose()?,
    })
}

pub fn ensure_relative_dates_resolved(
    query: &ValidatedQuery,
) -> Result<(), QueryDateResolutionError> {
    if let Some(predicate) = query.predicate.as_ref() {
        ensure_expr_relative_dates_resolved(predicate)?;
    }
    Ok(())
}

fn effective_today(
    options: &QueryDateResolutionOptions,
) -> Result<NaiveDate, QueryDateResolutionError> {
    let now = options.now_utc.unwrap_or_else(Utc::now);
    match options.timezone.as_deref() {
        Some(timezone) => {
            let timezone = Tz::from_str(timezone).map_err(|_| QueryDateResolutionError {
                kind: QueryDateResolutionErrorKind::InvalidTimezone,
                message: format!(
                    "invalid query timezone `{timezone}`: expected an IANA timezone name such as `UTC` or `Europe/Zurich`"
                ),
            })?;
            Ok(now.with_timezone(&timezone).date_naive())
        }
        None => Ok(now.with_timezone(&Local).date_naive()),
    }
}

fn resolve_expr(
    expr: &ValidatedExpr,
    today: NaiveDate,
) -> Result<ValidatedExpr, QueryDateResolutionError> {
    match expr {
        ValidatedExpr::And(children) => Ok(ValidatedExpr::And(
            children
                .iter()
                .map(|child| resolve_expr(child, today))
                .collect::<Result<Vec<_>, _>>()?,
        )),
        ValidatedExpr::Or(children) => Ok(ValidatedExpr::Or(
            children
                .iter()
                .map(|child| resolve_expr(child, today))
                .collect::<Result<Vec<_>, _>>()?,
        )),
        ValidatedExpr::Not(child) => Ok(ValidatedExpr::Not(Box::new(resolve_expr(child, today)?))),
        ValidatedExpr::Predicate(predicate) => Ok(ValidatedExpr::Predicate(resolve_predicate(
            predicate, today,
        )?)),
    }
}

fn resolve_predicate(
    predicate: &ValidatedPredicate,
    today: NaiveDate,
) -> Result<ValidatedPredicate, QueryDateResolutionError> {
    Ok(ValidatedPredicate {
        target: predicate.target,
        name: predicate.name.clone(),
        args: predicate
            .args
            .iter()
            .map(|arg| resolve_arg(arg, today))
            .collect::<Result<Vec<_>, _>>()?,
        options: predicate
            .options
            .iter()
            .map(|option| resolve_option(option, today))
            .collect::<Result<Vec<_>, _>>()?,
    })
}

fn resolve_arg(
    arg: &ValidatedArg,
    today: NaiveDate,
) -> Result<ValidatedArg, QueryDateResolutionError> {
    match arg {
        ValidatedArg::Scalar(value) => Ok(ValidatedArg::Scalar(value.clone())),
        ValidatedArg::NestedQuery(query) => {
            Ok(ValidatedArg::NestedQuery(Box::new(ValidatedQuery {
                target: query.target,
                predicate: query
                    .predicate
                    .as_ref()
                    .map(|predicate| resolve_expr(predicate, today))
                    .transpose()?,
            })))
        }
    }
}

fn resolve_option(
    option: &ValidatedOption,
    today: NaiveDate,
) -> Result<ValidatedOption, QueryDateResolutionError> {
    let value = match option.name.as_str() {
        "from" | "to" | "on" => resolve_date_value(&option.value, today)?,
        _ => option.value.clone(),
    };
    Ok(ValidatedOption {
        name: option.name.clone(),
        value,
    })
}

fn resolve_date_value(
    value: &QueryValue,
    today: NaiveDate,
) -> Result<QueryValue, QueryDateResolutionError> {
    match value {
        QueryValue::Symbol(symbol) if symbol == "today" => {
            Ok(QueryValue::String(today.to_string()))
        }
        QueryValue::Integer(days) => Ok(QueryValue::String(
            checked_day_offset(today, *days)?.to_string(),
        )),
        _ => Ok(value.clone()),
    }
}

fn checked_day_offset(today: NaiveDate, days: i64) -> Result<NaiveDate, QueryDateResolutionError> {
    if days >= 0 {
        today.checked_add_days(Days::new(days as u64))
    } else {
        today.checked_sub_days(Days::new(days.unsigned_abs()))
    }
    .ok_or_else(|| QueryDateResolutionError {
        kind: QueryDateResolutionErrorKind::DateOutOfRange,
        message: format!("relative date offset {days} days is out of range"),
    })
}

fn ensure_expr_relative_dates_resolved(
    expr: &ValidatedExpr,
) -> Result<(), QueryDateResolutionError> {
    match expr {
        ValidatedExpr::And(children) | ValidatedExpr::Or(children) => {
            for child in children {
                ensure_expr_relative_dates_resolved(child)?;
            }
            Ok(())
        }
        ValidatedExpr::Not(child) => ensure_expr_relative_dates_resolved(child),
        ValidatedExpr::Predicate(predicate) => ensure_predicate_relative_dates_resolved(predicate),
    }
}

fn ensure_predicate_relative_dates_resolved(
    predicate: &ValidatedPredicate,
) -> Result<(), QueryDateResolutionError> {
    for arg in &predicate.args {
        if let ValidatedArg::NestedQuery(query) = arg {
            if let Some(predicate) = query.predicate.as_ref() {
                ensure_expr_relative_dates_resolved(predicate)?;
            }
        }
    }
    for option in &predicate.options {
        if matches!(option.name.as_str(), "from" | "to" | "on") {
            ensure_date_value_resolved(&predicate.name, &option.name, &option.value)?;
        }
    }
    Ok(())
}

fn ensure_date_value_resolved(
    predicate: &str,
    option_name: &str,
    value: &QueryValue,
) -> Result<(), QueryDateResolutionError> {
    match value {
        QueryValue::Symbol(symbol) if symbol == "today" => Err(QueryDateResolutionError {
            kind: QueryDateResolutionErrorKind::UnresolvedRelativeDate,
            message: format!(
                "cannot compile unresolved relative date value `today` for {}:{}; resolve relative dates before SQL compilation",
                predicate, option_name
            ),
        }),
        QueryValue::Integer(days) => Err(QueryDateResolutionError {
            kind: QueryDateResolutionErrorKind::UnresolvedRelativeDate,
            message: format!(
                "cannot compile unresolved relative date offset {} for {}:{}; resolve relative dates before SQL compilation",
                days, predicate, option_name
            ),
        }),
        _ => Ok(()),
    }
}

#[cfg(test)]
mod tests {
    use std::sync::{Mutex, OnceLock};

    use chrono::{DateTime, Utc};

    use super::{
        ensure_relative_dates_resolved, resolve_relative_dates, QueryDateResolutionErrorKind,
        QueryDateResolutionOptions,
    };
    use crate::query::{
        parse_query, validate_query, QueryValidationOptions, QueryValue, ValidatedExpr,
    };

    fn validated(query: &str) -> crate::query::ValidatedQuery {
        let parsed = parse_query(query).expect("query should parse");
        validate_query(
            parsed,
            &QueryValidationOptions {
                body_text_available: true,
                regexp_matching_supported: true,
            },
        )
        .expect("query should validate")
    }

    fn parse_utc(value: &str) -> DateTime<Utc> {
        value.parse().expect("timestamp should parse")
    }

    #[test]
    fn resolves_today_in_configured_utc_timezone() {
        let resolved = resolve_relative_dates(
            &validated(r#"(headings (deadline :on today))"#),
            &QueryDateResolutionOptions {
                timezone: Some("UTC".to_string()),
                now_utc: Some(parse_utc("2026-07-12T23:30:00Z")),
            },
        )
        .expect("query should resolve");

        let Some(ValidatedExpr::Predicate(predicate)) = resolved.predicate else {
            panic!("expected predicate");
        };
        assert_eq!(
            predicate.options[0].value,
            QueryValue::String("2026-07-12".to_string())
        );
    }

    #[test]
    fn resolves_today_and_offsets_in_configured_europe_zurich_timezone() {
        let resolved = resolve_relative_dates(
            &validated(r#"(headings (and (deadline :on today) (scheduled :from -1 :to 2)))"#),
            &QueryDateResolutionOptions {
                timezone: Some("Europe/Zurich".to_string()),
                now_utc: Some(parse_utc("2026-07-12T23:30:00Z")),
            },
        )
        .expect("query should resolve");

        let Some(ValidatedExpr::And(predicates)) = resolved.predicate else {
            panic!("expected and predicate");
        };

        let ValidatedExpr::Predicate(deadline) = &predicates[0] else {
            panic!("expected deadline predicate");
        };
        let ValidatedExpr::Predicate(scheduled) = &predicates[1] else {
            panic!("expected scheduled predicate");
        };

        assert_eq!(
            deadline.options[0].value,
            QueryValue::String("2026-07-13".to_string())
        );
        assert_eq!(
            scheduled.options[0].value,
            QueryValue::String("2026-07-12".to_string())
        );
        assert_eq!(
            scheduled.options[1].value,
            QueryValue::String("2026-07-15".to_string())
        );
    }

    #[test]
    fn resolves_with_local_timezone_fallback() {
        let _guard = timezone_test_guard("UTC");
        let resolved = resolve_relative_dates(
            &validated(r#"(headings (deadline :on today))"#),
            &QueryDateResolutionOptions {
                timezone: None,
                now_utc: Some(parse_utc("2026-07-12T23:30:00Z")),
            },
        )
        .expect("query should resolve");

        let Some(ValidatedExpr::Predicate(predicate)) = resolved.predicate else {
            panic!("expected predicate");
        };
        assert_eq!(
            predicate.options[0].value,
            QueryValue::String("2026-07-12".to_string())
        );
    }

    #[test]
    fn resolves_around_local_midnight_using_effective_timezone() {
        let utc = resolve_relative_dates(
            &validated(r#"(headings (deadline :on today))"#),
            &QueryDateResolutionOptions {
                timezone: Some("UTC".to_string()),
                now_utc: Some(parse_utc("2026-07-12T23:30:00Z")),
            },
        )
        .expect("query should resolve");
        let zurich = resolve_relative_dates(
            &validated(r#"(headings (deadline :on today))"#),
            &QueryDateResolutionOptions {
                timezone: Some("Europe/Zurich".to_string()),
                now_utc: Some(parse_utc("2026-07-12T23:30:00Z")),
            },
        )
        .expect("query should resolve");

        let Some(ValidatedExpr::Predicate(utc_predicate)) = utc.predicate else {
            panic!("expected utc predicate");
        };
        let Some(ValidatedExpr::Predicate(zurich_predicate)) = zurich.predicate else {
            panic!("expected zurich predicate");
        };

        assert_eq!(
            utc_predicate.options[0].value,
            QueryValue::String("2026-07-12".to_string())
        );
        assert_eq!(
            zurich_predicate.options[0].value,
            QueryValue::String("2026-07-13".to_string())
        );
    }

    #[test]
    fn resolves_across_daylight_saving_transition() {
        let resolved = resolve_relative_dates(
            &validated(r#"(headings (deadline :from -1 :to 1))"#),
            &QueryDateResolutionOptions {
                timezone: Some("Europe/Zurich".to_string()),
                now_utc: Some(parse_utc("2026-03-29T00:30:00Z")),
            },
        )
        .expect("query should resolve");

        let Some(ValidatedExpr::Predicate(predicate)) = resolved.predicate else {
            panic!("expected predicate");
        };
        assert_eq!(
            predicate.options[0].value,
            QueryValue::String("2026-03-28".to_string())
        );
        assert_eq!(
            predicate.options[1].value,
            QueryValue::String("2026-03-30".to_string())
        );
    }

    #[test]
    fn rejects_invalid_timezone_name() {
        let error = resolve_relative_dates(
            &validated(r#"(headings (deadline :on today))"#),
            &QueryDateResolutionOptions {
                timezone: Some("Mars/Olympus".to_string()),
                now_utc: Some(parse_utc("2026-07-12T23:30:00Z")),
            },
        )
        .expect_err("query should fail");

        assert!(error.to_string().contains("invalid query timezone"));
    }

    #[test]
    fn rejects_unresolved_relative_dates_in_compilation_precheck() {
        let error =
            ensure_relative_dates_resolved(&validated(r#"(headings (deadline :to today))"#))
                .expect_err("precheck should fail");

        assert_eq!(
            error.kind,
            QueryDateResolutionErrorKind::UnresolvedRelativeDate
        );
        assert!(error
            .to_string()
            .contains("resolve relative dates before SQL compilation"));
    }

    #[test]
    fn rejects_out_of_range_integer_day_offsets_without_panicking() {
        let error = resolve_relative_dates(
            &validated(r#"(headings (deadline :to 9223372036854775807))"#),
            &QueryDateResolutionOptions {
                timezone: Some("UTC".to_string()),
                now_utc: Some(parse_utc("2026-07-12T23:30:00Z")),
            },
        )
        .expect_err("query should fail");

        assert_eq!(error.kind, QueryDateResolutionErrorKind::DateOutOfRange);
        assert!(error.to_string().contains("out of range"));
    }

    fn timezone_mutex() -> &'static Mutex<()> {
        static MUTEX: OnceLock<Mutex<()>> = OnceLock::new();
        MUTEX.get_or_init(|| Mutex::new(()))
    }

    struct TimezoneGuard {
        _lock: std::sync::MutexGuard<'static, ()>,
        original: Option<String>,
    }

    impl Drop for TimezoneGuard {
        fn drop(&mut self) {
            match &self.original {
                Some(value) => std::env::set_var("TZ", value),
                None => std::env::remove_var("TZ"),
            }
            tzset();
        }
    }

    fn timezone_test_guard(timezone: &str) -> TimezoneGuard {
        let lock = timezone_mutex().lock().expect("timezone mutex should lock");
        let original = std::env::var("TZ").ok();
        std::env::set_var("TZ", timezone);
        tzset();
        TimezoneGuard {
            _lock: lock,
            original,
        }
    }

    #[cfg(unix)]
    fn tzset() {
        unsafe extern "C" {
            fn tzset();
        }
        unsafe {
            tzset();
        }
    }

    #[cfg(not(unix))]
    fn tzset() {}
}
