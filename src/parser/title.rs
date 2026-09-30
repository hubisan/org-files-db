//! Orgize-free headline title helpers working on raw source lines.

use std::ops::Range;

use super::model::{ParsedLink, TodoKeywordConfig, TodoType};

pub(super) fn infer_todo_keyword(
    title_raw: &str,
    todo_keywords: &TodoKeywordConfig,
) -> Option<(String, String)> {
    for keyword in todo_keywords.all_keywords() {
        let Some(remainder) = title_raw.strip_prefix(&keyword.name) else {
            continue;
        };
        if remainder.is_empty() {
            continue;
        }

        let stripped = remainder.trim_start();
        if stripped.len() == remainder.len() {
            continue;
        }

        return Some((keyword.name.clone(), stripped.to_string()));
    }

    None
}

pub(super) fn placeholder_title_after_todo_prefix<'a>(
    source_title: &str,
    placeholder_title: &'a str,
    stripped_source_title: &str,
) -> Option<&'a str> {
    let prefix_len = source_title
        .len()
        .checked_sub(stripped_source_title.len())?;
    let source_prefix = source_title.get(..prefix_len)?;
    let placeholder_prefix = placeholder_title.get(..prefix_len)?;
    if placeholder_prefix != source_prefix {
        return None;
    }
    placeholder_title.get(prefix_len..)
}

pub(super) fn strip_leading_priority_cookie<'a>(
    title_raw: &'a str,
    priority: Option<&str>,
) -> &'a str {
    let Some(priority) = priority else {
        return title_raw;
    };

    let trimmed = title_raw.trim_start();
    let prefix = format!("[#{priority}]");
    let Some(remainder) = trimmed.strip_prefix(&prefix) else {
        return title_raw;
    };
    if remainder.is_empty() {
        ""
    } else if remainder.starts_with(char::is_whitespace) {
        remainder.trim_start()
    } else {
        title_raw
    }
}

/// True for `[TODO] COMMENT [#A] ...`, where Org does not read a priority.
pub(super) fn cookie_follows_comment(title_raw: &str, todo_keyword: Option<&str>) -> bool {
    let mut rest = title_raw.trim_start();
    if let Some(keyword) = todo_keyword {
        let Some(after_keyword) = rest.strip_prefix(keyword) else {
            return false;
        };
        rest = after_keyword.trim_start();
    }
    let Some(after_comment) = rest.strip_prefix("COMMENT") else {
        return false;
    };
    (after_comment.is_empty() || after_comment.starts_with(char::is_whitespace))
        && priority_cookie_value(after_comment.trim_start()).is_some()
}

pub(super) fn priority_from_source_title(title_raw: &str) -> Option<String> {
    let trimmed = title_raw.trim_start();
    let candidate = priority_cookie_value(trimmed).or_else(|| {
        trimmed
            .split_once(char::is_whitespace)
            .and_then(|(_, remainder)| priority_cookie_value(remainder.trim_start()))
    })?;
    if candidate.bytes().all(|byte| byte.is_ascii_uppercase())
        || candidate.bytes().all(|byte| byte.is_ascii_digit())
    {
        Some(candidate.to_string())
    } else {
        None
    }
}

fn priority_cookie_value(value: &str) -> Option<&str> {
    let value = value.strip_prefix("[#")?;
    let (candidate, remainder) = value.split_once(']')?;
    if candidate.is_empty()
        || (!remainder.is_empty() && !remainder.starts_with(char::is_whitespace))
    {
        return None;
    }
    Some(candidate)
}

pub(super) fn todo_type_for_keyword(
    keyword: &str,
    todo_keywords: &TodoKeywordConfig,
) -> Option<TodoType> {
    if todo_keywords
        .open
        .iter()
        .any(|candidate| candidate.name == keyword)
    {
        Some(TodoType::Open)
    } else if todo_keywords
        .closed
        .iter()
        .any(|candidate| candidate.name == keyword)
    {
        Some(TodoType::Closed)
    } else {
        None
    }
}

pub(super) fn todo_keyword_is_active(keyword: &str, todo_keywords: &TodoKeywordConfig) -> bool {
    todo_keywords
        .all_keywords()
        .any(|candidate| candidate.name == keyword)
}

pub(super) struct SourceHeadlineTitle {
    pub(super) raw: String,
    pub(super) range: Range<usize>,
}

pub(super) fn source_title_from_content_line(content: &str, start: usize) -> SourceHeadlineTitle {
    let line_start = content[..start]
        .rfind('\n')
        .map(|offset| offset + 1)
        .unwrap_or(0);
    let line_end = content[start..]
        .find('\n')
        .map(|offset| start + offset)
        .unwrap_or(content.len());
    let line = &content[line_start..line_end];
    let without_stars = line.trim_start_matches('*').trim_start();
    let without_tags = strip_trailing_org_tags(without_stars);
    let raw = without_tags.trim();
    let raw_start = line_start
        + (line.len() - without_stars.len())
        + (without_tags.len() - without_tags.trim_start().len());
    SourceHeadlineTitle {
        raw: raw.to_string(),
        range: raw_start..raw_start + raw.len(),
    }
}

pub(super) fn links_in_range<'a>(
    links: &'a [ParsedLink],
    range: &Range<usize>,
) -> &'a [ParsedLink] {
    let first = links.partition_point(|link| link.byte_end <= range.start);
    let last = first + links[first..].partition_point(|link| link.byte_start < range.end);
    &links[first..last]
}

pub(super) fn link_contains_range(links: &[ParsedLink], range: &Range<usize>) -> bool {
    let index = links.partition_point(|link| link.byte_start <= range.start);
    index
        .checked_sub(1)
        .and_then(|index| links.get(index))
        .is_some_and(|link| {
            link.format == "bracket" && link.byte_start <= range.start && range.end <= link.byte_end
        })
}

pub(super) fn restore_title_link_placeholders(
    mut title: String,
    placeholders: &[(String, String)],
) -> String {
    for (token, visible) in placeholders {
        title = title.replace(token, visible);
    }
    title
}

fn strip_trailing_org_tags(value: &str) -> &str {
    let trimmed = value.trim_end();
    let mut parts = trimmed.rsplitn(2, char::is_whitespace);
    let last = parts.next().unwrap_or(trimmed);

    if is_org_tag_block(last) {
        parts.next().unwrap_or("").trim_end()
    } else {
        trimmed
    }
}

fn is_org_tag_block(value: &str) -> bool {
    value.starts_with(':')
        && value.ends_with(':')
        && value.len() > 2
        && value[1..value.len() - 1].split(':').all(|segment| {
            !segment.is_empty()
                && segment
                    .chars()
                    .all(|c| c.is_alphanumeric() || matches!(c, '_' | '@' | '#' | '%'))
        })
}

#[cfg(test)]
mod range_tests {
    use super::{link_contains_range, links_in_range};
    use crate::parser::{ParsedLink, ParsedLinkSourceContext};

    fn link(start: usize, end: usize, format: &str) -> ParsedLink {
        ParsedLink {
            source_context: ParsedLinkSourceContext::Normal,
            format: format.to_string(),
            raw: String::new(),
            raw_target: String::new(),
            logical_target: String::new(),
            raw_description: None,
            link_type: String::new(),
            path: String::new(),
            search_option: None,
            byte_start: start,
            byte_end: end,
            target_byte_start: start,
            target_byte_end: end,
            description_byte_start: None,
            description_byte_end: None,
            line: 1,
        }
    }

    #[test]
    fn source_ordered_range_helpers_respect_boundaries_and_link_formats() {
        let links = vec![
            link(10, 20, "bracket"),
            link(20, 30, "angle"),
            link(30, 40, "bracket"),
        ];
        assert!(links_in_range(&[], &(0..1)).is_empty());
        assert!(links_in_range(&links, &(0..10)).is_empty());
        assert!(links_in_range(&links, &(40..50)).is_empty());
        assert_eq!(links_in_range(&links, &(10..20)).len(), 1);
        assert_eq!(links_in_range(&links, &(20..30))[0].format, "angle");
        assert_eq!(links_in_range(&links, &(19..31)).len(), 3);
        assert!(link_contains_range(&links, &(10..20)));
        assert!(link_contains_range(&links, &(11..19)));
        assert!(!link_contains_range(&links, &(20..30)));
        assert!(link_contains_range(&links, &(30..40)));
        assert!(!link_contains_range(&links, &(9..10)));
        assert!(!link_contains_range(&links, &(40..41)));
    }
}
