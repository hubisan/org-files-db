//! Orgize-free headline title helpers working on raw source lines.

use std::ops::Range;

use super::model::{ParsedLink, TodoKeywordConfig, TodoType};

/// Org's title separators (`[ \t]`); other Unicode whitespace such as NBSP is title text.
fn is_org_blank(character: char) -> bool {
    matches!(character, ' ' | '\t')
}

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

        // Org needs one space after the keyword; tabs and other blanks do not count.
        if !remainder.starts_with(' ') {
            continue;
        }
        let stripped = remainder.trim_start_matches(is_org_blank);

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

    let trimmed = title_raw.trim_start_matches(is_org_blank);
    match priority_cookie(trimmed) {
        Some((value, remainder)) if value == priority => remainder.trim_start_matches(is_org_blank),
        _ => title_raw,
    }
}

/// True for `[TODO] COMMENT [#A] ...`, where Org does not read a priority.
pub(super) fn cookie_follows_comment(title_raw: &str, todo_keyword: Option<&str>) -> bool {
    let mut rest = title_raw.trim_start_matches(is_org_blank);
    if let Some(keyword) = todo_keyword {
        let Some(after_keyword) = rest.strip_prefix(keyword) else {
            return false;
        };
        rest = after_keyword.trim_start_matches(is_org_blank);
    }
    let Some(after_comment) = rest.strip_prefix("COMMENT") else {
        return false;
    };
    (after_comment.is_empty() || after_comment.starts_with(is_org_blank))
        && priority_cookie(after_comment.trim_start_matches(is_org_blank)).is_some()
}

pub(super) fn priority_from_source_title(title_raw: &str) -> Option<String> {
    let trimmed = title_raw.trim_start_matches(is_org_blank);
    let (value, _) = priority_cookie(trimmed).or_else(|| {
        trimmed
            .split_once(' ')
            .and_then(|(_, remainder)| priority_cookie(remainder.trim_start_matches(is_org_blank)))
    })?;
    Some(value.to_string())
}

/// Reads a leading `[#X]` cookie and returns its value and the text after it. Org (`[#.]`)
/// takes any single character, with or without a blank after it. A multi-digit value such as
/// `[#10]` is a project extension (#80) and needs a blank or the end after it.
fn priority_cookie(value: &str) -> Option<(&str, &str)> {
    let value = value.strip_prefix("[#")?;
    let (candidate, remainder) = value.split_once(']')?;
    let mut chars = candidate.chars();
    let single = chars.next().is_some() && chars.next().is_none();
    let numeric = !candidate.is_empty() && candidate.bytes().all(|byte| byte.is_ascii_digit());
    let boundary = remainder.is_empty() || remainder.starts_with(is_org_blank);
    (single || (numeric && boundary)).then_some((candidate, remainder))
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
    let without_stars = line
        .trim_start_matches('*')
        .trim_start_matches(is_org_blank);
    let without_tags = strip_trailing_org_tags(without_stars);
    let raw = without_tags.trim_matches(is_org_blank);
    let raw_start = line_start
        + (line.len() - without_stars.len())
        + (without_tags.len() - without_tags.trim_start_matches(is_org_blank).len());
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
    let trimmed = value.trim_end_matches(is_org_blank);
    let mut parts = trimmed.rsplitn(2, is_org_blank);
    let last = parts.next().unwrap_or(trimmed);

    if is_org_tag_block(last) {
        parts.next().unwrap_or("").trim_end_matches(is_org_blank)
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

#[cfg(test)]
mod tests {
    use super::*;

    const ADVERSARIAL: &[&str] = &[
        "",
        " ",
        "\t\n",
        "[",
        "[#",
        "[#]",
        "[#é]",
        "é[#A]é",
        "é [#A] é",
        "[#A]é",
        "[#Aé]",
        "TODOé [#A]",
        "TODO\u{a0}[#A] x",
        "COMMENT\u{a0}[#A]",
        "é:tag:é",
        ":é:",
        "é :a:",
        "x :é:",
        ":::",
        "::",
        ":a::b:",
        "日本語 :日本:",
        "😀[#A]😀 :😀:",
    ];

    fn assert_boundary(s: &str, i: usize) {
        assert!(s.is_char_boundary(i), "offset {i} splits {s:?}");
    }

    #[test]
    fn infer_todo_keyword_needs_whitespace_after_keyword() {
        let cfg = TodoKeywordConfig::default();
        let rows = [
            ("TODO write", Some(("TODO", "write"))),
            ("DONE \t x", Some(("DONE", "x"))),
            ("DONE\tx", None),
            ("TODO\u{a0}x", None),
            ("TODO", None),
            ("TODO ", Some(("TODO", ""))),
            ("TODOx y", None),
            ("todo x", None),
            ("", None),
            ("TODOé x", None),
        ];
        for (input, want) in rows {
            let want = want.map(|(k, t)| (k.to_string(), t.to_string()));
            assert_eq!(infer_todo_keyword(input, &cfg), want, "{input:?}");
        }
    }

    #[test]
    fn placeholder_title_after_todo_prefix_slices_by_prefix_len() {
        let rows = [
            ("TODO x", "TODO y", "x", Some("y")),
            ("TODO x", "TODO ", "x", Some("")),
            ("TODO x", "TODO", "x", None),
            ("TODO x", "DONE y", "x", None),
            ("é x", "é y", "x", Some("y")),
            ("é x", "e\u{301} y", "x", None),
            ("é x", "ab y", "x", None),
            ("x", "x", "longer than source", None),
            ("", "", "", Some("")),
            ("TODO x", "", "x", None),
        ];
        for (source, placeholder, stripped, want) in rows {
            let got = placeholder_title_after_todo_prefix(source, placeholder, stripped);
            assert_eq!(got, want, "{source:?} {placeholder:?}");
        }
    }

    #[test]
    fn priority_cookie_helpers_follow_org_rules() {
        let rows = [
            ("[#A] x", Some("A")),
            ("  [#A]", Some("A")),
            ("TODO [#B] x", Some("B")),
            ("[#10] x", Some("10")),
            ("[#0]", Some("0")),
            ("[#] x", None),
            ("[#Ab] x", None),
            ("[#AB] x", None),
            ("[#a] x", Some("a")),
            ("[#1] x", Some("1")),
            ("[#!] x", Some("!")),
            ("[#A]x", Some("A")),
            ("[#A]\tx", Some("A")),
            ("[#A]\u{a0}x", Some("A")),
            ("[#10]x", None),
            ("TODO\u{a0}[#A] y", None),
            ("[#A", None),
            ("x [#A]", Some("A")), // first word may be a TODO keyword
            ("A B [#C]", None),
            ("[#é] x", Some("é")),
            ("", None),
            ("   ", None),
        ];
        for (input, want) in rows {
            let want = want.map(str::to_string);
            assert_eq!(priority_from_source_title(input), want, "{input:?}");
        }
    }

    #[test]
    fn strip_leading_priority_cookie_needs_matching_cookie_and_space() {
        let rows = [
            (" [#A]  x y", Some("A"), "x y"),
            ("[#A]", Some("A"), ""),
            ("[#A]x", Some("A"), "x"),
            ("[#a]\tx", Some("a"), "x"),
            ("[#A]\u{a0}x", Some("A"), "\u{a0}x"),
            ("[#10]x", Some("10"), "[#10]x"),
            ("[#B] x", Some("A"), "[#B] x"),
            ("[#A] x", None, "[#A] x"),
            ("[#10] x", Some("10"), "x"),
            ("[#é] é", Some("é"), "é"),
            ("", Some("A"), ""),
        ];
        for (input, priority, want) in rows {
            assert_eq!(
                strip_leading_priority_cookie(input, priority),
                want,
                "{input:?}"
            );
        }
    }

    #[test]
    fn cookie_follows_comment_only_after_comment_word() {
        let rows = [
            ("COMMENT [#A] x", None, true),
            ("COMMENT [#A]", None, true),
            ("TODO COMMENT [#A] x", Some("TODO"), true),
            ("COMMENT [#A] x", Some("TODO"), false),
            ("COMMENT x [#A]", None, false),
            ("COMMENT [#] x", None, false),
            ("COMMENT [#a]x", None, true),
            ("COMMENT\t[#!] x", None, true),
            ("COMMENT\u{a0}[#A] x", None, false),
            ("TODO\u{a0}COMMENT [#A]", Some("TODO"), false),
            ("TODO COMMENT\u{a0}[#A]", Some("TODO"), false),
            ("COMMENT", None, false),
            ("COMMENTS [#A]", None, false),
            ("[#A] COMMENT", None, false),
            ("", None, false),
            ("é", Some("é"), false),
        ];
        for (input, keyword, want) in rows {
            assert_eq!(cookie_follows_comment(input, keyword), want, "{input:?}");
        }
    }

    #[test]
    fn strip_trailing_org_tags_only_removes_valid_tag_block() {
        let rows = [
            ("Title :a:b:", "Title"),
            ("Title   :a_b@c#d%e:  ", "Title"),
            ("Title :日本:", "Title"),
            (":a:", ""),
            ("Title :a b:", "Title :a b:"),
            ("Title\t:a:\t", "Title"),
            ("Title\u{a0}:a:", "Title\u{a0}:a:"),
            ("Title :a:\u{a0}", "Title :a:\u{a0}"),
            ("Title :a-b:", "Title :a-b:"),
            ("Title ::", "Title ::"),
            ("Title :::", "Title :::"),
            ("Title :a::b:", "Title :a::b:"),
            ("Title :a:b", "Title :a:b"),
            ("Title:a:", "Title:a:"),
            ("Title : a:", "Title : a:"),
            ("é :é:", "é"),
            ("", ""),
            ("   ", ""),
        ];
        for (input, want) in rows {
            assert_eq!(strip_trailing_org_tags(input), want, "{input:?}");
        }
        let long = format!("{} :{}:", "x".repeat(50_000), "t".repeat(50_000));
        assert_eq!(strip_trailing_org_tags(&long).len(), 50_000);
    }

    #[test]
    fn helpers_never_panic_and_return_char_boundaries() {
        let cfg = TodoKeywordConfig::default();
        for s in ADVERSARIAL {
            infer_todo_keyword(s, &cfg);
            priority_from_source_title(s);
            strip_leading_priority_cookie(s, Some("A"));
            strip_leading_priority_cookie(s, Some("é"));
            cookie_follows_comment(s, Some("TODO"));
            cookie_follows_comment(s, None);
            let tail = strip_trailing_org_tags(s);
            assert!(s.contains(tail));
            for (i, _) in s.char_indices().chain([(s.len(), ' ')]) {
                for other in ADVERSARIAL {
                    if let Some(rest) = placeholder_title_after_todo_prefix(s, other, &s[i..]) {
                        assert_boundary(other, other.len() - rest.len());
                    }
                }
            }
            // Non-boundary-aligned lengths must yield None rather than panic.
            for stripped in ADVERSARIAL {
                let _ = placeholder_title_after_todo_prefix(s, s, stripped);
            }
        }
    }
}
