//! Headline title helpers working on raw source lines.

use std::ops::Range;

use super::model::{ParsedLink, TodoKeywordConfig, TodoType};

/// Org's title separators (`[ \t]`); other Unicode whitespace such as NBSP is title text.
fn is_org_blank(character: char) -> bool {
    matches!(character, ' ' | '\t')
}

/// Facts read from the raw title of a headline line (stars and tags already removed).
#[derive(Debug, PartialEq, Eq)]
pub(super) struct HeadlineParts {
    pub(super) todo_keyword: Option<String>,
    pub(super) priority: Option<String>,
    /// Byte offset in the raw title where the title text starts, after the keyword, the
    /// priority cookie and the blanks behind them. `COMMENT` belongs to the text.
    pub(super) text_start: usize,
}

/// Splits the raw title like `org-element-headline-parser`: a TODO keyword, then a priority
/// cookie, directly at the start. Org needs one space after the keyword (tabs and other
/// blanks do not count); a keyword alone is a TODO state for `org-entry-get` and the
/// agenda, and so it is here. A cookie later in the title is text.
pub(super) fn split_headline_title(
    title_raw: &str,
    todo_keywords: &TodoKeywordConfig,
) -> HeadlineParts {
    let mut text_start = 0;
    let mut todo_keyword = None;
    for keyword in todo_keywords.all_keywords() {
        let Some(remainder) = title_raw.strip_prefix(&keyword.name) else {
            continue;
        };
        if remainder.is_empty() || remainder.starts_with(' ') {
            todo_keyword = Some(keyword.name.clone());
            text_start = title_raw.len() - remainder.trim_start_matches(is_org_blank).len();
            break;
        }
    }
    let after_keyword = &title_raw[text_start..];
    let mut priority = None;
    if let Some((value, remainder)) = priority_cookie(after_keyword) {
        priority = Some(value.to_string());
        text_start = title_raw.len() - remainder.trim_start_matches(is_org_blank).len();
    }
    HeadlineParts {
        todo_keyword,
        priority,
        text_start,
    }
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

pub(super) struct SourceHeadlineTitle {
    pub(super) raw: String,
    pub(super) range: Range<usize>,
}

/// The headline line at `start` without its line terminator (`\n` or `\r\n`).
fn headline_line(content: &str, start: usize) -> (usize, &str) {
    let line_start = content[..start].rfind('\n').map_or(0, |offset| offset + 1);
    let line_end = content[start..]
        .find('\n')
        .map_or(content.len(), |offset| start + offset);
    let line = &content[line_start..line_end];
    (line_start, line.strip_suffix('\r').unwrap_or(line))
}

pub(super) fn source_title_from_content_line(content: &str, start: usize) -> SourceHeadlineTitle {
    let (line_start, line) = headline_line(content, start);
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

pub(super) fn link_contains_range(links: &[ParsedLink], range: &Range<usize>) -> bool {
    let index = links.partition_point(|link| link.byte_start <= range.start);
    index
        .checked_sub(1)
        .and_then(|index| links.get(index))
        .is_some_and(|link| {
            link.format == "bracket" && link.byte_start <= range.start && range.end <= link.byte_end
        })
}

/// Non-empty tags of the last tag block on the headline line at `start`. Org reads only that
/// block (`* T :a: :b:` has the tag `b`).
pub(super) fn source_title_tags(content: &str, start: usize) -> Vec<String> {
    let without_stars = headline_line(content, start)
        .1
        .trim_start_matches('*')
        .trim_start_matches(is_org_blank);
    let trimmed = without_stars.trim_end_matches(is_org_blank);
    let last = trimmed.rsplit(is_org_blank).next().unwrap_or(trimmed);
    if !is_org_tag_block(last) {
        return Vec::new();
    }
    last[1..last.len() - 1]
        .split(':')
        .filter(|tag| !tag.is_empty())
        .map(str::to_string)
        .collect()
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

/// Org's tag block (`:[[:alnum:]_@#%:]+:`): empty segments (`::a::`, `:a::b:`) still count, so
/// the block is stripped from the title like `org-element` does (`:::` is a block, `::` is not).
fn is_org_tag_block(value: &str) -> bool {
    value.len() > 2
        && value.starts_with(':')
        && value.ends_with(':')
        && value[1..value.len() - 1]
            .chars()
            .all(|c| c.is_alphanumeric() || matches!(c, '_' | '@' | '#' | '%' | ':'))
}

#[cfg(test)]
mod range_tests {
    use super::link_contains_range;
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
    fn link_contains_range_respects_boundaries_and_link_formats() {
        let links = vec![
            link(10, 20, "bracket"),
            link(20, 30, "angle"),
            link(30, 40, "bracket"),
        ];
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
    fn split_headline_title_reads_keyword_then_priority_like_org() {
        let cfg = TodoKeywordConfig::default();
        // (raw title, keyword, priority, title text)
        let rows: &[(&str, Option<&str>, Option<&str>, &str)] = &[
            ("TODO write", Some("TODO"), None, "write"),
            ("DONE \t x", Some("DONE"), None, "x"),
            ("DONE\tx", None, None, "DONE\tx"),
            ("TODO\u{a0}x", None, None, "TODO\u{a0}x"),
            ("TODO", Some("TODO"), None, ""),
            ("TODO ", Some("TODO"), None, ""),
            ("TODOx y", None, None, "TODOx y"),
            ("TODOé x", None, None, "TODOé x"),
            ("todo x", None, None, "todo x"),
            ("", None, None, ""),
            ("[#A] x", None, Some("A"), "x"),
            ("TODO [#B] x", Some("TODO"), Some("B"), "x"),
            ("TODO [#B]", Some("TODO"), Some("B"), ""),
            ("TODO \t[#B]x", Some("TODO"), Some("B"), "x"),
            ("[#10] x", None, Some("10"), "x"),
            ("[#0]", None, Some("0"), ""),
            ("[#] x", None, None, "[#] x"),
            ("[#Ab] x", None, None, "[#Ab] x"),
            ("[#a]x", None, Some("a"), "x"),
            ("[#1] x", None, Some("1"), "x"),
            ("[#!] x", None, Some("!"), "x"),
            ("[#A]\tx", None, Some("A"), "x"),
            ("[#A]\u{a0}x", None, Some("A"), "\u{a0}x"),
            ("[#10]x", None, None, "[#10]x"),
            ("[#é] x", None, Some("é"), "x"),
            ("[#A", None, None, "[#A"),
            // A cookie after other text or after COMMENT is title text.
            ("x [#A]", None, None, "x [#A]"),
            ("A B [#C]", None, None, "A B [#C]"),
            ("COMMENT [#A] x", None, None, "COMMENT [#A] x"),
            ("TODO COMMENT [#A] x", Some("TODO"), None, "COMMENT [#A] x"),
            ("TODO\u{a0}[#A] y", None, None, "TODO\u{a0}[#A] y"),
        ];
        for (input, keyword, priority, text) in rows {
            let parts = split_headline_title(input, &cfg);
            assert_eq!(parts.todo_keyword.as_deref(), *keyword, "{input:?}");
            assert_eq!(parts.priority.as_deref(), *priority, "{input:?}");
            assert_eq!(&input[parts.text_start..], *text, "{input:?}");
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
            ("Title :::", "Title"),
            ("Title :a::b:", "Title"),
            ("Title ::a::", "Title"),
            ("Title :a::", "Title"),
            ("Title : :", "Title : :"),
            ("Title ::a", "Title ::a"),
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
            assert_boundary(s, split_headline_title(s, &cfg).text_start);
            let tail = strip_trailing_org_tags(s);
            assert!(s.contains(tail));
        }
    }
}
