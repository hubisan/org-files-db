use std::{collections::HashSet, ops::Range};

use crate::parser::{ParsedLink, ParsedLinkSourceContext};

pub const DEFAULT_PLAIN_LINK_PROTOCOLS: &[&str] = &[
    "http",
    "https",
    "file",
    "file+sys",
    "file+emacs",
    "ftp",
    "attachment",
    "bbdb",
    "docview",
    "doi",
    "gnus",
    "rmail",
    "mhe",
    "help",
    "id",
    "info",
    "irc",
    "mailto",
    "news",
    "shortdoc",
];

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LinkScannerConfig {
    pub plain_link_protocols: Vec<String>,
}

impl Default for LinkScannerConfig {
    fn default() -> Self {
        Self {
            plain_link_protocols: DEFAULT_PLAIN_LINK_PROTOCOLS
                .iter()
                .map(|protocol| (*protocol).to_string())
                .collect(),
        }
    }
}

#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct LinkScanContext {
    pub ignored_byte_ranges: Vec<Range<usize>>,
}

#[derive(Debug, Default)]
pub struct LinkScanner;

impl LinkScanner {
    pub fn new() -> Self {
        Self
    }

    pub fn scan(
        &self,
        content: &str,
        config: &LinkScannerConfig,
        context: &LinkScanContext,
    ) -> Vec<ParsedLink> {
        let enabled_protocols = normalized_protocols(&config.plain_link_protocols);
        let bytes = content.as_bytes();
        let mut links = Vec::new();
        let mut offset = 0;
        let mut line = 1;

        while offset < bytes.len() {
            if let Some(range_end) = ignored_range_end(offset, &context.ignored_byte_ranges) {
                line += newline_count(&bytes[offset..range_end]) as u32;
                offset = range_end;
                continue;
            }

            if bytes[offset] == b'[' && bytes.get(offset + 1) == Some(&b'[') {
                if let Some(link) = try_parse_bracket_link(content, offset, line) {
                    offset = link.byte_end;
                    links.push(link);
                    continue;
                }
            }

            if bytes[offset] == b'<' {
                if let Some(link) = try_parse_angle_link(content, offset, line) {
                    offset = link.byte_end;
                    links.push(link);
                    continue;
                }
            }

            if let Some(link) = try_parse_plain_link(content, offset, line, &enabled_protocols) {
                offset = link.byte_end;
                links.push(link);
                continue;
            }

            let char_len = content[offset..]
                .chars()
                .next()
                .expect("offset should remain on a char boundary")
                .len_utf8();
            if bytes[offset] == b'\n' {
                line += 1;
            }
            offset += char_len;
        }

        links
    }
}

pub fn scan_links(
    content: &str,
    config: &LinkScannerConfig,
    context: &LinkScanContext,
) -> Vec<ParsedLink> {
    LinkScanner::new().scan(content, config, context)
}

fn try_parse_bracket_link(content: &str, offset: usize, line: u32) -> Option<ParsedLink> {
    let line_end = line_end_offset(content, offset);
    let candidate = content.get(offset + 2..line_end)?;
    let parsed = parse_bracket_inner(candidate)?;
    let (link_type, path, search_option) = classify_bracket_target(&parsed.raw_target);

    Some(ParsedLink {
        source_context: ParsedLinkSourceContext::Normal,
        format: "bracket".to_string(),
        raw: content[offset..offset + 2 + parsed.byte_len].to_string(),
        raw_target: parsed.raw_target.clone(),
        raw_description: parsed.raw_description.clone(),
        link_type,
        path,
        search_option,
        byte_start: offset,
        byte_end: offset + 2 + parsed.byte_len,
        line,
    })
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct ParsedBracketInner {
    raw_target: String,
    raw_description: Option<String>,
    byte_len: usize,
}

fn parse_bracket_inner(candidate: &str) -> Option<ParsedBracketInner> {
    for (index, ch) in candidate.char_indices() {
        match ch {
            '[' => {
                if !is_escaped(candidate, index) {
                    return None;
                }
            }
            ']' => {
                if is_escaped(candidate, index) {
                    continue;
                }

                match candidate.as_bytes().get(index + 1).copied() {
                    Some(b']') => {
                        if index == 0 {
                            return None;
                        }

                        return Some(ParsedBracketInner {
                            raw_target: candidate[..index].to_string(),
                            raw_description: None,
                            byte_len: index + 2,
                        });
                    }
                    Some(b'[') => {
                        if index == 0 {
                            return None;
                        }

                        let description = &candidate[index + 2..];
                        let close = description.find("]]")?;
                        return Some(ParsedBracketInner {
                            raw_target: candidate[..index].to_string(),
                            raw_description: Some(description[..close].to_string()),
                            byte_len: index + 2 + close + 2,
                        });
                    }
                    _ => return None,
                }
            }
            _ => {}
        }
    }

    None
}

fn try_parse_angle_link(content: &str, offset: usize, line: u32) -> Option<ParsedLink> {
    let line_end = line_end_offset(content, offset);
    let slice = &content[offset..line_end];
    let close = slice.find('>')?;
    if close == 0 {
        return None;
    }

    let byte_end = offset + close + 1;
    let raw_target = &content[offset + 1..byte_end - 1];
    let (link_type, path) = split_explicit_type(raw_target)?;
    let (path, search_option) = finalize_explicit_target(&link_type, path);

    Some(ParsedLink {
        source_context: ParsedLinkSourceContext::Normal,
        format: "angle".to_string(),
        raw: content[offset..byte_end].to_string(),
        raw_target: raw_target.to_string(),
        raw_description: None,
        link_type,
        path,
        search_option,
        byte_start: offset,
        byte_end,
        line,
    })
}

fn try_parse_plain_link(
    content: &str,
    offset: usize,
    line: u32,
    enabled_protocols: &HashSet<String>,
) -> Option<ParsedLink> {
    if !plain_link_has_valid_left_boundary(content, offset) {
        return None;
    }

    let line_end = line_end_offset(content, offset);
    let slice = &content[offset..line_end];
    let colon = slice.find(':')?;
    if colon == 0 {
        return None;
    }

    let protocol = &slice[..colon];
    if !protocol.bytes().all(protocol_char) {
        return None;
    }

    let normalized = protocol.to_ascii_lowercase();
    if !enabled_protocols.contains(&normalized) {
        return None;
    }

    let end = plain_link_candidate_end(slice, colon);

    if end <= colon + 1 {
        return None;
    }

    let trimmed_end = trim_plain_link_end(&slice[..end]);
    if trimmed_end <= colon + 1 {
        return None;
    }

    let byte_end = offset + trimmed_end;
    let raw = &content[offset..byte_end];
    let (path, search_option) =
        finalize_explicit_target(&normalized, raw[protocol.len() + 1..].to_string());

    Some(ParsedLink {
        source_context: ParsedLinkSourceContext::Normal,
        format: "plain".to_string(),
        raw: raw.to_string(),
        raw_target: raw.to_string(),
        raw_description: None,
        link_type: normalized,
        path,
        search_option,
        byte_start: offset,
        byte_end,
        line,
    })
}

fn classify_bracket_target(target: &str) -> (String, String, Option<String>) {
    if let Some((link_type, path)) = split_explicit_type(target) {
        let (path, search_option) = finalize_explicit_target(&link_type, path);
        return (link_type, path, search_option);
    }

    if target.starts_with("./")
        || target.starts_with("../")
        || target.starts_with("~/")
        || target.starts_with('/')
    {
        let (path, search_option) = split_search_option(target.to_string());
        return ("file".to_string(), path, search_option);
    }

    if target.starts_with('#') {
        return (
            "custom-id".to_string(),
            target.trim_start_matches('#').to_string(),
            None,
        );
    }

    ("fuzzy".to_string(), target.to_string(), None)
}

fn link_type_is_file_like(link_type: &str) -> bool {
    matches!(link_type, "file" | "file+sys" | "file+emacs")
}

fn finalize_explicit_target(link_type: &str, path: String) -> (String, Option<String>) {
    if link_type_is_file_like(link_type) {
        split_search_option(path)
    } else {
        (path, None)
    }
}

fn split_search_option(path: String) -> (String, Option<String>) {
    match path.split_once("::") {
        Some((path, search_option)) => (path.to_string(), Some(search_option.to_string())),
        None => (path, None),
    }
}

fn split_explicit_type(raw_target: &str) -> Option<(String, String)> {
    let (prefix, path) = raw_target.split_once(':')?;
    if prefix.is_empty() || !prefix.bytes().all(protocol_char) {
        return None;
    }
    Some((prefix.to_ascii_lowercase(), path.to_string()))
}

fn is_escaped(slice: &str, index: usize) -> bool {
    if index == 0 {
        return false;
    }

    let bytes = slice.as_bytes();
    let mut backslashes = 0usize;
    let mut cursor = index;
    while cursor > 0 && bytes[cursor - 1] == b'\\' {
        backslashes += 1;
        cursor -= 1;
    }

    backslashes % 2 == 1
}

fn normalized_protocols(protocols: &[String]) -> HashSet<String> {
    protocols
        .iter()
        .map(|protocol| protocol.to_ascii_lowercase())
        .collect()
}

fn protocol_char(byte: u8) -> bool {
    byte.is_ascii_alphanumeric() || matches!(byte, b'+' | b'-' | b'.')
}

fn plain_link_has_valid_left_boundary(content: &str, offset: usize) -> bool {
    let Some(previous) = content[..offset].chars().next_back() else {
        return true;
    };

    !matches!(previous, '\'' | '$' | '%') && !previous.is_alphanumeric()
}

fn plain_link_candidate_end(slice: &str, colon: usize) -> usize {
    let mut cursor = colon + 1;

    while cursor < slice.len() {
        let ch = slice[cursor..]
            .chars()
            .next()
            .expect("cursor should remain on a char boundary");

        if ch.is_whitespace() || matches!(ch, '"' | '\'') {
            break;
        }

        if let Some(group_end) = balanced_plain_link_group_end(&slice[cursor..]) {
            cursor += group_end;
            continue;
        }

        if is_plain_link_group_opener(ch) || is_plain_link_group_closer(ch) {
            break;
        }

        cursor += ch.len_utf8();
    }

    cursor
}

fn balanced_plain_link_group_end(slice: &str) -> Option<usize> {
    let opener = slice.chars().next()?;
    let mut expected_closers = vec![plain_link_group_closer(opener)?];

    for (index, ch) in slice.char_indices().skip(1) {
        if ch.is_whitespace() || matches!(ch, '"' | '\'') {
            return None;
        }

        if let Some(expected) = plain_link_group_closer(ch) {
            expected_closers.push(expected);
            continue;
        }

        if Some(&ch) == expected_closers.last() {
            expected_closers.pop();
            if expected_closers.is_empty() {
                return Some(index + ch.len_utf8());
            }
            continue;
        }

        if is_plain_link_group_closer(ch) {
            return None;
        }
    }

    None
}

fn plain_link_group_closer(ch: char) -> Option<char> {
    match ch {
        '(' => Some(')'),
        '[' => Some(']'),
        '<' => Some('>'),
        _ => None,
    }
}

fn is_plain_link_group_opener(ch: char) -> bool {
    plain_link_group_closer(ch).is_some()
}

fn is_plain_link_group_closer(ch: char) -> bool {
    matches!(ch, ')' | ']' | '>')
}

fn trim_plain_link_end(candidate: &str) -> usize {
    let mut end = candidate.len();

    while end > 0 {
        let ch = candidate[..end]
            .chars()
            .next_back()
            .expect("candidate should be non-empty while trimming");
        if matches!(ch, ')' | ']' | '}') && ends_with_balanced_plain_link_group(&candidate[..end]) {
            break;
        }
        if matches!(ch, '.' | ',' | ';' | ':' | '!' | '?' | ')' | ']' | '}') {
            end -= ch.len_utf8();
            continue;
        }
        break;
    }

    end
}

fn ends_with_balanced_plain_link_group(candidate: &str) -> bool {
    candidate.char_indices().any(|(index, ch)| {
        is_plain_link_group_opener(ch)
            && balanced_plain_link_group_end(&candidate[index..])
                .is_some_and(|group_end| index + group_end == candidate.len())
    })
}

fn line_end_offset(content: &str, offset: usize) -> usize {
    content[offset..]
        .find('\n')
        .map(|relative| offset + relative)
        .unwrap_or(content.len())
}

fn ignored_range_end(offset: usize, ranges: &[Range<usize>]) -> Option<usize> {
    ranges
        .iter()
        .find(|range| range.start <= offset && offset < range.end)
        .map(|range| range.end)
}

fn newline_count(bytes: &[u8]) -> usize {
    bytes.iter().filter(|byte| **byte == b'\n').count()
}

#[cfg(test)]
mod tests {
    use super::{
        scan_links, LinkScanContext, LinkScanner, LinkScannerConfig, DEFAULT_PLAIN_LINK_PROTOCOLS,
    };

    #[test]
    fn scans_basic_bracket_angle_and_plain_links_in_priority_order() {
        let content = "[[file:notes.org]] <https://example.com/a path> https://example.com";

        let links = scan_links(
            content,
            &LinkScannerConfig::default(),
            &LinkScanContext::default(),
        );

        assert_eq!(links.len(), 3);
        assert_eq!(links[0].format, "bracket");
        assert_eq!(links[0].raw, "[[file:notes.org]]");
        assert_eq!(links[0].raw_target, "file:notes.org");
        assert_eq!(links[0].link_type, "file");
        assert_eq!(links[0].path, "notes.org");
        assert_eq!(links[0].line, 1);

        assert_eq!(links[1].format, "angle");
        assert_eq!(links[1].raw, "<https://example.com/a path>");
        assert_eq!(links[1].raw_target, "https://example.com/a path");
        assert_eq!(links[1].link_type, "https");
        assert_eq!(links[1].path, "//example.com/a path");
        assert_eq!(links[1].search_option, None);

        assert_eq!(links[2].format, "plain");
        assert_eq!(links[2].raw, "https://example.com");
        assert_eq!(links[2].raw_target, "https://example.com");
        assert_eq!(links[2].link_type, "https");
        assert_eq!(links[2].path, "//example.com");
    }

    #[test]
    fn failed_bracket_and_angle_candidates_advance_safely() {
        let content = "[[broken\nhttps://example.com\n<broken\n<mailto:person@example.com>";

        let links = LinkScanner::new().scan(
            content,
            &LinkScannerConfig::default(),
            &LinkScanContext::default(),
        );

        assert_eq!(links.len(), 2);
        assert_eq!(links[0].format, "plain");
        assert_eq!(links[0].raw, "https://example.com");
        assert_eq!(links[0].line, 2);
        assert_eq!(links[1].format, "angle");
        assert_eq!(links[1].raw, "<mailto:person@example.com>");
        assert_eq!(links[1].line, 4);
    }

    #[test]
    fn angle_links_support_spaces_and_file_like_search_options() {
        let content = "\
<https://example.com/some path with spaces>
<file:~/code/main.c::255>
<file:~/xx.org::*My Target>
<file:~/xx.org::#my-custom-id>
<file:~/xx.org::/regexp/>
<file:::find me>
<file+sys:~/sys/path::7>
<file+emacs:~/emacs/path::*Target>";

        let links = scan_links(
            content,
            &LinkScannerConfig::default(),
            &LinkScanContext::default(),
        );
        let angle_links = links
            .into_iter()
            .filter(|link| link.format == "angle")
            .collect::<Vec<_>>();

        assert_eq!(
            angle_links
                .iter()
                .map(|link| {
                    (
                        link.raw.clone(),
                        link.raw_target.clone(),
                        link.link_type.clone(),
                        link.path.clone(),
                        link.search_option.clone(),
                    )
                })
                .collect::<Vec<_>>(),
            vec![
                (
                    "<https://example.com/some path with spaces>".to_string(),
                    "https://example.com/some path with spaces".to_string(),
                    "https".to_string(),
                    "//example.com/some path with spaces".to_string(),
                    None,
                ),
                (
                    "<file:~/code/main.c::255>".to_string(),
                    "file:~/code/main.c::255".to_string(),
                    "file".to_string(),
                    "~/code/main.c".to_string(),
                    Some("255".to_string()),
                ),
                (
                    "<file:~/xx.org::*My Target>".to_string(),
                    "file:~/xx.org::*My Target".to_string(),
                    "file".to_string(),
                    "~/xx.org".to_string(),
                    Some("*My Target".to_string()),
                ),
                (
                    "<file:~/xx.org::#my-custom-id>".to_string(),
                    "file:~/xx.org::#my-custom-id".to_string(),
                    "file".to_string(),
                    "~/xx.org".to_string(),
                    Some("#my-custom-id".to_string()),
                ),
                (
                    "<file:~/xx.org::/regexp/>".to_string(),
                    "file:~/xx.org::/regexp/".to_string(),
                    "file".to_string(),
                    "~/xx.org".to_string(),
                    Some("/regexp/".to_string()),
                ),
                (
                    "<file:::find me>".to_string(),
                    "file:::find me".to_string(),
                    "file".to_string(),
                    "".to_string(),
                    Some("find me".to_string()),
                ),
                (
                    "<file+sys:~/sys/path::7>".to_string(),
                    "file+sys:~/sys/path::7".to_string(),
                    "file+sys".to_string(),
                    "~/sys/path".to_string(),
                    Some("7".to_string()),
                ),
                (
                    "<file+emacs:~/emacs/path::*Target>".to_string(),
                    "file+emacs:~/emacs/path::*Target".to_string(),
                    "file+emacs".to_string(),
                    "~/emacs/path".to_string(),
                    Some("*Target".to_string()),
                ),
            ]
        );
    }

    #[test]
    fn angle_links_preserve_unknown_and_action_like_types_without_splitting() {
        let content = "<unknown:foo> <jira:ABC-123> <shell:ls *.org>";

        let links = scan_links(
            content,
            &LinkScannerConfig::default(),
            &LinkScanContext::default(),
        );
        let angle_links = links
            .into_iter()
            .filter(|link| link.format == "angle")
            .collect::<Vec<_>>();

        assert_eq!(angle_links.len(), 3);
        assert_eq!(angle_links[0].link_type, "unknown");
        assert_eq!(angle_links[0].path, "foo");
        assert_eq!(angle_links[0].search_option, None);
        assert_eq!(angle_links[1].link_type, "jira");
        assert_eq!(angle_links[1].path, "ABC-123");
        assert_eq!(angle_links[1].search_option, None);
        assert_eq!(angle_links[2].link_type, "shell");
        assert_eq!(angle_links[2].path, "ls *.org");
        assert_eq!(angle_links[2].search_option, None);
    }

    #[test]
    fn angle_candidates_ignore_unterminated_and_multiline_cases_and_recover() {
        let content = "<https://example.com\n<broken\n<https://example.org/ok>";

        let links = scan_links(
            content,
            &LinkScannerConfig::default(),
            &LinkScanContext::default(),
        );
        let angle_links = links
            .into_iter()
            .filter(|link| link.format == "angle")
            .collect::<Vec<_>>();

        assert_eq!(angle_links.len(), 1);
        assert_eq!(angle_links[0].raw, "<https://example.org/ok>");
        assert_eq!(angle_links[0].line, 3);
    }

    #[test]
    fn unicode_before_link_preserves_byte_offsets() {
        let content = "ä\nhttps://example.org";

        let links = scan_links(
            content,
            &LinkScannerConfig::default(),
            &LinkScanContext::default(),
        );

        assert_eq!(links.len(), 1);
        assert_eq!(links[0].byte_start, "ä\n".len());
        assert_eq!(links[0].byte_end, content.len());
        assert_eq!(links[0].line, 2);
    }

    #[test]
    fn plain_link_detection_uses_configured_protocols() {
        let content = "jira:ABC-123 https://example.org";
        let config = LinkScannerConfig {
            plain_link_protocols: vec!["jira".to_string()],
        };

        let links = scan_links(content, &config, &LinkScanContext::default());

        assert_eq!(links.len(), 1);
        assert_eq!(links[0].format, "plain");
        assert_eq!(links[0].raw, "jira:ABC-123");
        assert_eq!(links[0].link_type, "jira");
        assert_eq!(links[0].path, "ABC-123");
    }

    #[test]
    fn plain_link_detection_uses_expanded_default_protocols_and_excludes_actions() {
        let content = "\
attachment:projects.org doi:10.1000/182 irc:/irc.com/#emacs/bob bbdb:R.*Stallman shell:ls elisp:org-todo";

        let links = scan_links(
            content,
            &LinkScannerConfig::default(),
            &LinkScanContext::default(),
        );

        assert_eq!(
            links
                .iter()
                .map(|link| (link.raw.clone(), link.link_type.clone(), link.path.clone()))
                .collect::<Vec<_>>(),
            vec![
                (
                    "attachment:projects.org".to_string(),
                    "attachment".to_string(),
                    "projects.org".to_string(),
                ),
                (
                    "doi:10.1000/182".to_string(),
                    "doi".to_string(),
                    "10.1000/182".to_string(),
                ),
                (
                    "irc:/irc.com/#emacs/bob".to_string(),
                    "irc".to_string(),
                    "/irc.com/#emacs/bob".to_string(),
                ),
                (
                    "bbdb:R.*Stallman".to_string(),
                    "bbdb".to_string(),
                    "R.*Stallman".to_string(),
                ),
            ]
        );
    }

    #[test]
    fn plain_link_detection_accepts_explicit_action_protocols_case_insensitively() {
        let content = "shell:ls elisp:org-todo";
        let config = LinkScannerConfig {
            plain_link_protocols: vec!["SHELL".to_string(), "ElIsP".to_string()],
        };

        let links = scan_links(content, &config, &LinkScanContext::default());

        assert_eq!(
            links
                .iter()
                .map(|link| (link.raw.clone(), link.link_type.clone(), link.path.clone()))
                .collect::<Vec<_>>(),
            vec![
                (
                    "shell:ls".to_string(),
                    "shell".to_string(),
                    "ls".to_string(),
                ),
                (
                    "elisp:org-todo".to_string(),
                    "elisp".to_string(),
                    "org-todo".to_string(),
                ),
            ]
        );
    }

    #[test]
    fn plain_file_links_split_search_options_only_for_file_like_types() {
        let content = "\
file:~/code/main.c::255 file+sys:~/sys/path::*Target file+emacs:~/emacs/path::#custom-id file:::find attachment:projects.org::10 id:abc123::10 docview:paper.pdf::12 jira:file.org::10";
        let config = LinkScannerConfig {
            plain_link_protocols: vec![
                "file".to_string(),
                "file+sys".to_string(),
                "file+emacs".to_string(),
                "attachment".to_string(),
                "id".to_string(),
                "docview".to_string(),
                "jira".to_string(),
            ],
        };

        let links = scan_links(content, &config, &LinkScanContext::default());

        assert_eq!(
            links
                .iter()
                .map(|link| {
                    (
                        link.raw.clone(),
                        link.link_type.clone(),
                        link.path.clone(),
                        link.search_option.clone(),
                    )
                })
                .collect::<Vec<_>>(),
            vec![
                (
                    "file:~/code/main.c::255".to_string(),
                    "file".to_string(),
                    "~/code/main.c".to_string(),
                    Some("255".to_string()),
                ),
                (
                    "file+sys:~/sys/path::*Target".to_string(),
                    "file+sys".to_string(),
                    "~/sys/path".to_string(),
                    Some("*Target".to_string()),
                ),
                (
                    "file+emacs:~/emacs/path::#custom-id".to_string(),
                    "file+emacs".to_string(),
                    "~/emacs/path".to_string(),
                    Some("#custom-id".to_string()),
                ),
                (
                    "file:::find".to_string(),
                    "file".to_string(),
                    "".to_string(),
                    Some("find".to_string()),
                ),
                (
                    "attachment:projects.org::10".to_string(),
                    "attachment".to_string(),
                    "projects.org::10".to_string(),
                    None,
                ),
                (
                    "id:abc123::10".to_string(),
                    "id".to_string(),
                    "abc123::10".to_string(),
                    None,
                ),
                (
                    "docview:paper.pdf::12".to_string(),
                    "docview".to_string(),
                    "paper.pdf::12".to_string(),
                    None,
                ),
                (
                    "jira:file.org::10".to_string(),
                    "jira".to_string(),
                    "file.org::10".to_string(),
                    None,
                ),
            ]
        );
    }

    #[test]
    fn ignores_links_inside_configured_ignored_ranges() {
        let content = "https://example.org [[file:kept.org]]";
        let ignored_end = "https://example.org".len();
        let context = LinkScanContext {
            ignored_byte_ranges: std::iter::once(0..ignored_end).collect(),
        };

        let links = scan_links(content, &LinkScannerConfig::default(), &context);

        assert_eq!(links.len(), 1);
        assert_eq!(links[0].format, "bracket");
        assert_eq!(links[0].raw, "[[file:kept.org]]");
    }

    #[test]
    fn bracket_links_preserve_basic_description_field() {
        let content = "[[https://example.org][Example]]";

        let links = scan_links(
            content,
            &LinkScannerConfig::default(),
            &LinkScanContext::default(),
        );

        assert_eq!(links.len(), 1);
        assert_eq!(links[0].format, "bracket");
        assert_eq!(links[0].raw_target, "https://example.org");
        assert_eq!(links[0].raw_description.as_deref(), Some("Example"));
        assert_eq!(links[0].path, "//example.org");
        assert_eq!(links[0].search_option, None);
    }

    #[test]
    fn bracket_links_preserve_escaped_target_brackets() {
        let content = r"[[https://example.org/\[section\]][desc]]";

        let links = scan_links(
            content,
            &LinkScannerConfig::default(),
            &LinkScanContext::default(),
        );

        assert_eq!(links.len(), 1);
        assert_eq!(links[0].raw_target, r"https://example.org/\[section\]");
        assert_eq!(links[0].raw_description.as_deref(), Some("desc"));
    }

    #[test]
    fn invalid_unescaped_target_brackets_are_rejected() {
        let content = "[[bad[target]]";

        let links = scan_links(
            content,
            &LinkScannerConfig::default(),
            &LinkScanContext::default(),
        );

        assert!(links.is_empty());
    }

    #[test]
    fn odd_and_even_backslashes_control_target_delimiters() {
        let content = "[[target\\]]] [[target\\\\]]";

        let links = scan_links(
            content,
            &LinkScannerConfig::default(),
            &LinkScanContext::default(),
        );
        let bracket_links = links
            .into_iter()
            .filter(|link| link.format == "bracket")
            .collect::<Vec<_>>();

        assert_eq!(bracket_links.len(), 2);
        assert_eq!(bracket_links[0].raw, "[[target\\]]]");
        assert_eq!(bracket_links[0].raw_target, r"target\]");
        assert_eq!(bracket_links[1].raw, "[[target\\\\]]");
        assert_eq!(bracket_links[1].raw_target, r"target\\");
    }

    #[test]
    fn description_parsing_is_permissive_with_unescaped_brackets() {
        let content = "[[https://example.org][desc with [brackets]]";

        let links = scan_links(
            content,
            &LinkScannerConfig::default(),
            &LinkScanContext::default(),
        );

        assert_eq!(links.len(), 1);
        assert_eq!(
            links[0].raw_description.as_deref(),
            Some("desc with [brackets")
        );
    }

    #[test]
    fn nested_looking_description_closes_at_first_same_line_delimiter() {
        let content = "[[https://example.org][text [[https://nested.example]] more]]";

        let links = scan_links(
            content,
            &LinkScannerConfig::default(),
            &LinkScanContext::default(),
        );

        assert_eq!(links.len(), 1);
        assert_eq!(
            links[0].raw,
            "[[https://example.org][text [[https://nested.example]]"
        );
        assert_eq!(
            links[0].raw_description.as_deref(),
            Some("text [[https://nested.example")
        );
    }

    #[test]
    fn zero_width_space_between_brackets_does_not_close_candidate() {
        let content = "ä [[https://example.org][desc]\u{200B}]\n[[target]]";

        let links = scan_links(
            content,
            &LinkScannerConfig::default(),
            &LinkScanContext::default(),
        );
        let bracket_links = links
            .into_iter()
            .filter(|link| link.format == "bracket")
            .collect::<Vec<_>>();

        assert_eq!(bracket_links.len(), 1);
        assert_eq!(bracket_links[0].raw, "[[target]]");
        assert_eq!(
            bracket_links[0].byte_start,
            content.find("[[target]]").unwrap()
        );
    }

    #[test]
    fn multiline_bracket_candidates_are_ignored_without_crossing_newlines() {
        let content = "[[target][desc\n[[target]]";

        let links = scan_links(
            content,
            &LinkScannerConfig::default(),
            &LinkScanContext::default(),
        );
        let bracket_links = links
            .into_iter()
            .filter(|link| link.format == "bracket")
            .collect::<Vec<_>>();

        assert_eq!(bracket_links.len(), 1);
        assert_eq!(bracket_links[0].raw, "[[target]]");
        assert_eq!(bracket_links[0].line, 2);
    }

    #[test]
    fn invalid_bracket_candidate_does_not_block_later_valid_link() {
        let content = "[[bad[target]] [[target]]";

        let links = scan_links(
            content,
            &LinkScannerConfig::default(),
            &LinkScanContext::default(),
        );

        assert_eq!(links.len(), 1);
        assert_eq!(links[0].raw, "[[target]]");
        assert_eq!(links[0].byte_start, content.rfind("[[target]]").unwrap());
    }

    #[test]
    fn unicode_before_and_inside_bracket_links_preserves_byte_positions() {
        let content = "ä [[https://example.org/ü][dësc]]";

        let links = scan_links(
            content,
            &LinkScannerConfig::default(),
            &LinkScanContext::default(),
        );

        assert_eq!(links.len(), 1);
        assert_eq!(links[0].raw_target, "https://example.org/ü");
        assert_eq!(links[0].raw_description.as_deref(), Some("dësc"));
        assert_eq!(links[0].byte_start, "ä ".len());
        assert_eq!(links[0].byte_end, content.len());
    }

    #[test]
    fn bracket_links_classify_supported_targets_and_split_file_like_search_options() {
        let content = "\
[[file:notes.org::42]]
[[unknown:foo]]
[[shell:ls]]
[[target]]
[[./notes.org::10]]
[[../notes.org]]
[[~/notes.org]]
[[/tmp/notes.org]]
[[#custom-id]]
[[*Heading]]
[[dedicated target]]
[[notes.org]]";

        let links = scan_links(
            content,
            &LinkScannerConfig::default(),
            &LinkScanContext::default(),
        );
        let bracket_links = links
            .into_iter()
            .filter(|link| link.format == "bracket")
            .collect::<Vec<_>>();

        assert_eq!(bracket_links.len(), 12);
        assert_eq!(
            bracket_links
                .iter()
                .map(|link| {
                    (
                        link.raw_target.clone(),
                        link.link_type.clone(),
                        link.path.clone(),
                        link.search_option.clone(),
                    )
                })
                .collect::<Vec<_>>(),
            vec![
                (
                    "file:notes.org::42".to_string(),
                    "file".to_string(),
                    "notes.org".to_string(),
                    Some("42".to_string()),
                ),
                (
                    "unknown:foo".to_string(),
                    "unknown".to_string(),
                    "foo".to_string(),
                    None,
                ),
                (
                    "shell:ls".to_string(),
                    "shell".to_string(),
                    "ls".to_string(),
                    None,
                ),
                (
                    "target".to_string(),
                    "fuzzy".to_string(),
                    "target".to_string(),
                    None,
                ),
                (
                    "./notes.org::10".to_string(),
                    "file".to_string(),
                    "./notes.org".to_string(),
                    Some("10".to_string()),
                ),
                (
                    "../notes.org".to_string(),
                    "file".to_string(),
                    "../notes.org".to_string(),
                    None,
                ),
                (
                    "~/notes.org".to_string(),
                    "file".to_string(),
                    "~/notes.org".to_string(),
                    None,
                ),
                (
                    "/tmp/notes.org".to_string(),
                    "file".to_string(),
                    "/tmp/notes.org".to_string(),
                    None,
                ),
                (
                    "#custom-id".to_string(),
                    "custom-id".to_string(),
                    "custom-id".to_string(),
                    None,
                ),
                (
                    "*Heading".to_string(),
                    "fuzzy".to_string(),
                    "*Heading".to_string(),
                    None,
                ),
                (
                    "dedicated target".to_string(),
                    "fuzzy".to_string(),
                    "dedicated target".to_string(),
                    None,
                ),
                (
                    "notes.org".to_string(),
                    "fuzzy".to_string(),
                    "notes.org".to_string(),
                    None,
                ),
            ]
        );
    }

    #[test]
    fn default_plain_protocols_include_phase3_defaults() {
        assert_eq!(
            DEFAULT_PLAIN_LINK_PROTOCOLS,
            &[
                "http",
                "https",
                "file",
                "file+sys",
                "file+emacs",
                "ftp",
                "attachment",
                "bbdb",
                "docview",
                "doi",
                "gnus",
                "rmail",
                "mhe",
                "help",
                "id",
                "info",
                "irc",
                "mailto",
                "news",
                "shortdoc",
            ]
        );
    }

    #[test]
    fn plain_links_trim_deterministic_trailing_punctuation() {
        let content = "See https://example.org/test.]";

        let links = scan_links(
            content,
            &LinkScannerConfig::default(),
            &LinkScanContext::default(),
        );

        assert_eq!(links.len(), 1);
        assert_eq!(links[0].raw, "https://example.org/test");
        assert_eq!(links[0].byte_end, content.len() - 2);
    }

    #[test]
    fn plain_links_follow_reviewed_org_boundary_examples() {
        let content = "\
!https://www.example.com
\"https://www.example.com
_https://www.example.com
'https://www.example.com
$https://www.example.com
%https://www.example.com
xhttps://www.example.com
Prefix:https://www.example.com";

        let links = scan_links(
            content,
            &LinkScannerConfig::default(),
            &LinkScanContext::default(),
        );

        assert_eq!(
            links
                .iter()
                .map(|link| link.raw.as_str())
                .collect::<Vec<_>>(),
            vec![
                "https://www.example.com",
                "https://www.example.com",
                "https://www.example.com",
                "https://www.example.com",
            ]
        );
        assert_eq!(links[0].line, 1);
        assert_eq!(links[1].line, 2);
        assert_eq!(links[2].line, 3);
        assert_eq!(links[3].line, 8);
    }

    #[test]
    fn plain_links_accept_underscore_as_left_boundary_and_exclude_it_from_span() {
        let content = "_https://www.example.com";

        let links = scan_links(
            content,
            &LinkScannerConfig::default(),
            &LinkScanContext::default(),
        );

        assert_eq!(links.len(), 1);
        assert_eq!(links[0].raw, "https://www.example.com");
        assert_eq!(links[0].line, 1);
        assert_eq!(links[0].byte_start, 1);
        assert_eq!(links[0].byte_end, content.len());
    }

    #[test]
    fn plain_links_exclude_prefix_and_trailing_punctuation_from_byte_ranges() {
        let bang_content = "!https://www.example.com";
        let bang_links = scan_links(
            bang_content,
            &LinkScannerConfig::default(),
            &LinkScanContext::default(),
        );

        assert_eq!(bang_links.len(), 1);
        assert_eq!(bang_links[0].raw, "https://www.example.com");
        assert_eq!(bang_links[0].byte_start, 1);
        assert_eq!(bang_links[0].byte_end, bang_content.len());

        let dot_content = "https://example.org/path.";
        let dot_links = scan_links(
            dot_content,
            &LinkScannerConfig::default(),
            &LinkScanContext::default(),
        );

        assert_eq!(dot_links.len(), 1);
        assert_eq!(dot_links[0].raw, "https://example.org/path");
        assert_eq!(dot_links[0].byte_start, 0);
        assert_eq!(dot_links[0].byte_end, dot_content.len() - 1);
    }

    #[test]
    fn plain_links_follow_reviewed_org_end_examples() {
        let content = "\
https://example.org/path with text after whitespace
https://example.org/path<balanced-suffix>
https://example.org/path(foo)
https://example.org/path[foo]
https://example.org/path.
https://example.org/path,
https://example.org/path;
https://example.org/path:
https://example.org/path!
https://example.org/path?
https://example.org/path/
https://example.org/path-
https://example.org/path>not-part-of-plain-link
https://example.org/path<not-part-of-plain-link";

        let links = scan_links(
            content,
            &LinkScannerConfig::default(),
            &LinkScanContext::default(),
        );

        assert_eq!(
            links
                .iter()
                .map(|link| link.raw.as_str())
                .collect::<Vec<_>>(),
            vec![
                "https://example.org/path",
                "https://example.org/path<balanced-suffix>",
                "https://example.org/path(foo)",
                "https://example.org/path[foo]",
                "https://example.org/path",
                "https://example.org/path",
                "https://example.org/path",
                "https://example.org/path",
                "https://example.org/path",
                "https://example.org/path",
                "https://example.org/path/",
                "https://example.org/path-",
                "https://example.org/path",
                "https://example.org/path",
            ]
        );
    }
}
