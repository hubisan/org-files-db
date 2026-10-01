use std::collections::HashSet;

use crate::parser::structure_scanner::{parsed_keywords, scan_structure};
use crate::parser::{ParsedKeyword, TodoKeyword, TodoKeywordConfig};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ResolvedTodoKeywords {
    pub effective: TodoKeywordConfig,
    pub entries: Vec<ResolvedTodoKeywordEntry>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ResolvedTodoKeywordEntry {
    pub keyword: String,
    pub state_type: String,
    pub shortcut: Option<char>,
    pub sequence_no: i64,
    pub source_kind: TodoKeywordSourceKind,
    pub source_keyword: Option<String>,
    pub source_line_number: Option<u32>,
}

#[derive(Debug, Clone)]
struct KeywordSource {
    keyword: Option<String>,
    line_number: Option<u32>,
    kind: TodoKeywordSourceKind,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TodoKeywordSourceKind {
    ConfigDefault,
    OrgKeyword,
}

impl TodoKeywordSourceKind {
    pub fn as_db_str(self) -> &'static str {
        match self {
            Self::ConfigDefault => "config_default",
            Self::OrgKeyword => "org_keyword",
        }
    }
}

pub(crate) fn parse_todo_keyword_spec(spec: &str) -> TodoKeyword {
    let spec = spec.trim();
    if let Some((name, fast_key)) = split_todo_keyword_spec(spec) {
        match fast_key {
            Some(fast_key) => TodoKeyword::with_fast_key(name, fast_key),
            None => TodoKeyword::new(name),
        }
    } else {
        TodoKeyword::new(spec)
    }
}

pub fn resolve_todo_keywords(
    content: &str,
    default_keywords: &TodoKeywordConfig,
) -> ResolvedTodoKeywords {
    resolve_todo_keywords_with_default_source(
        content,
        default_keywords,
        TodoKeywordSourceKind::ConfigDefault,
    )
}

pub fn resolve_todo_keywords_with_default_source(
    content: &str,
    default_keywords: &TodoKeywordConfig,
    default_source_kind: TodoKeywordSourceKind,
) -> ResolvedTodoKeywords {
    // Cheap pre-scan: without any candidate TODO keyword line the structure scan cannot
    // yield a TODO keyword, so skip it.
    if !may_contain_todo_keyword_line(content) {
        return resolved_from_default_keywords(default_keywords, default_source_kind);
    }
    let keywords = parsed_keywords(content, &scan_structure(content));
    resolve_todo_keywords_from_keywords(&keywords)
        .unwrap_or_else(|| resolved_from_default_keywords(default_keywords, default_source_kind))
}

fn may_contain_todo_keyword_line(content: &str) -> bool {
    content.lines().any(|line| {
        let line = line.trim_start();
        let Some(rest) = line.as_bytes().strip_prefix(b"#+") else {
            return false;
        };
        ["todo", "seq_todo", "typ_todo"].iter().any(|key| {
            rest.get(..key.len())
                .is_some_and(|head| head.eq_ignore_ascii_case(key.as_bytes()))
        })
    })
}

pub(crate) fn resolve_todo_keywords_from_keywords(
    keywords: &[ParsedKeyword],
) -> Option<ResolvedTodoKeywords> {
    let mut open = Vec::new();
    let mut closed = Vec::new();
    let mut open_entries = Vec::new();
    let mut closed_entries = Vec::new();
    let mut seen = HashSet::new();

    for keyword in keywords.iter().filter(|keyword| {
        keyword.key.eq_ignore_ascii_case("TODO")
            || keyword.key.eq_ignore_ascii_case("SEQ_TODO")
            || keyword.key.eq_ignore_ascii_case("TYP_TODO")
    }) {
        let Some(value) = keyword.value.as_deref() else {
            continue;
        };
        let Some(line_config) = parse_todo_keyword_line(value) else {
            continue;
        };
        let source_keyword = Some(keyword.key.to_ascii_uppercase());
        let source_line_number = keyword.line_number;

        let source = KeywordSource {
            keyword: source_keyword,
            line_number: source_line_number,
            kind: TodoKeywordSourceKind::OrgKeyword,
        };

        append_keyword_entries(
            &line_config.open,
            "open",
            &source,
            &mut seen,
            &mut open,
            &mut open_entries,
        );
        append_keyword_entries(
            &line_config.closed,
            "closed",
            &source,
            &mut seen,
            &mut closed,
            &mut closed_entries,
        );
    }

    if open.is_empty() && closed.is_empty() {
        None
    } else {
        Some(ResolvedTodoKeywords {
            effective: TodoKeywordConfig { open, closed },
            entries: finalize_entries(open_entries, closed_entries),
        })
    }
}

fn resolved_from_default_keywords(
    default_keywords: &TodoKeywordConfig,
    source_kind: TodoKeywordSourceKind,
) -> ResolvedTodoKeywords {
    let default_keywords = default_keywords.clone().deduplicated();
    let mut open_entries = Vec::with_capacity(default_keywords.open.len());
    let mut closed_entries = Vec::with_capacity(default_keywords.closed.len());

    for keyword in &default_keywords.open {
        open_entries.push(ResolvedTodoKeywordEntryDraft {
            keyword: keyword.name.clone(),
            state_type: "open".to_string(),
            shortcut: keyword.fast_key,
            source_kind,
            source_keyword: None,
            source_line_number: None,
        });
    }
    for keyword in &default_keywords.closed {
        closed_entries.push(ResolvedTodoKeywordEntryDraft {
            keyword: keyword.name.clone(),
            state_type: "closed".to_string(),
            shortcut: keyword.fast_key,
            source_kind,
            source_keyword: None,
            source_line_number: None,
        });
    }

    ResolvedTodoKeywords {
        effective: default_keywords,
        entries: finalize_entries(open_entries, closed_entries),
    }
}

fn parse_todo_keyword_line(value: &str) -> Option<TodoKeywordConfig> {
    let tokens: Vec<&str> = value.split_whitespace().collect();
    if tokens.is_empty() {
        return None;
    }

    let mut open = Vec::new();
    let mut closed = Vec::new();

    if let Some(separator_index) = tokens.iter().position(|token| *token == "|") {
        for token in &tokens[..separator_index] {
            open.push(parse_todo_keyword_spec(token));
        }
        for token in &tokens[separator_index + 1..] {
            closed.push(parse_todo_keyword_spec(token));
        }
    } else {
        let (closed_token, open_tokens) = tokens.split_last()?;
        for token in open_tokens {
            open.push(parse_todo_keyword_spec(token));
        }
        closed.push(parse_todo_keyword_spec(closed_token));
    }

    Some(TodoKeywordConfig { open, closed })
}

fn append_keyword_entries(
    keywords: &[TodoKeyword],
    state_type: &str,
    source: &KeywordSource,
    seen: &mut HashSet<String>,
    keywords_out: &mut Vec<TodoKeyword>,
    entries_out: &mut Vec<ResolvedTodoKeywordEntryDraft>,
) {
    for keyword in keywords {
        if !seen.insert(keyword.name.clone()) {
            continue;
        }

        keywords_out.push(keyword.clone());
        entries_out.push(ResolvedTodoKeywordEntryDraft {
            keyword: keyword.name.clone(),
            state_type: state_type.to_string(),
            shortcut: keyword.fast_key,
            source_kind: source.kind,
            source_keyword: source.keyword.clone(),
            source_line_number: source.line_number,
        });
    }
}

fn finalize_entries(
    open_entries: Vec<ResolvedTodoKeywordEntryDraft>,
    closed_entries: Vec<ResolvedTodoKeywordEntryDraft>,
) -> Vec<ResolvedTodoKeywordEntry> {
    open_entries
        .into_iter()
        .chain(closed_entries)
        .enumerate()
        .map(|(sequence_no, draft)| ResolvedTodoKeywordEntry {
            keyword: draft.keyword,
            state_type: draft.state_type,
            shortcut: draft.shortcut,
            sequence_no: sequence_no as i64,
            source_kind: draft.source_kind,
            source_keyword: draft.source_keyword,
            source_line_number: draft.source_line_number,
        })
        .collect()
}

/// Splits a keyword spec like Org's `org-set-regexps-and-options`: the name is everything
/// before the first `(` when the spec ends with `)`; the fast key is the character right
/// after `(` unless it is `!`, `@` or `/` (log settings without a key) or the `)` itself.
/// The log part is ignored. Specs without a trailing `)` are literal names.
fn split_todo_keyword_spec(spec: &str) -> Option<(&str, Option<char>)> {
    let body = spec.strip_suffix(')')?;
    let open_paren = body.find('(')?;
    let name = &body[..open_paren];
    if name.is_empty() {
        return None;
    }
    let fast_key = body[open_paren + 1..]
        .chars()
        .next()
        .filter(|c| !matches!(c, '!' | '@' | '/'));
    Some((name, fast_key))
}

/// True when the parenthesized part follows Org's grammar `(KEY?[!@]?(/[!@])?)`, with at
/// least one element. Used to reject malformed config specs that Org would read oddly.
pub(crate) fn is_valid_todo_keyword_spec_suffix(spec: &str) -> bool {
    let Some(body) = spec.strip_suffix(')') else {
        return !spec.contains(['(', ')']);
    };
    let Some(open_paren) = body.find('(') else {
        return false;
    };
    let mut rest = body[open_paren + 1..].chars().peekable();
    let mut seen = 0;
    if rest
        .next_if(|c| !c.is_whitespace() && !matches!(c, '!' | '@' | '/' | '(' | ')'))
        .is_some()
    {
        seen += 1;
    }
    if rest.next_if(|c| matches!(c, '!' | '@')).is_some() {
        seen += 1;
    }
    if rest.next_if(|c| *c == '/').is_some() {
        if rest.next_if(|c| matches!(c, '!' | '@')).is_none() {
            return false;
        }
        seen += 1;
    }
    seen > 0 && rest.next().is_none() && !body[..open_paren].contains(['(', ')'])
}

#[cfg(test)]
mod tests {
    use super::{is_valid_todo_keyword_spec_suffix, parse_todo_keyword_spec, TodoKeyword};

    #[test]
    fn parse_todo_keyword_spec_trims_outer_whitespace() {
        assert_eq!(
            parse_todo_keyword_spec(" TODO(t) "),
            TodoKeyword::with_fast_key("TODO", 't')
        );
    }

    /// Expected values come from Emacs 29.3 / Org 9.6.15 (`org-todo-keywords-1` and the
    /// explicit keys in `org-todo-key-alist`).
    #[test]
    fn parse_todo_keyword_spec_matches_org() {
        let cases: &[(&str, &str, Option<char>)] = &[
            ("ZZZ", "ZZZ", None),
            ("ZZZ(x)", "ZZZ", Some('x')),
            ("ZZZ(x!)", "ZZZ", Some('x')),
            ("ZZZ(x@)", "ZZZ", Some('x')),
            ("ZZZ(x/!)", "ZZZ", Some('x')),
            ("ZZZ(x@/!)", "ZZZ", Some('x')),
            ("ZZZ(x!/@)", "ZZZ", Some('x')),
            ("ZZZ(@/!)", "ZZZ", None),
            ("ZZZ(!)", "ZZZ", None),
            ("ZZZ(@)", "ZZZ", None),
            ("ZZZ(/!)", "ZZZ", None),
            ("ZZZ(ww)", "ZZZ", Some('w')),
            ("ZZZ()", "ZZZ", None),
            ("ZZZ(x)(y)", "ZZZ", Some('x')),
            ("ZZZ(w", "ZZZ(w", None),
            ("ZZZ(x)y", "ZZZ(x)y", None),
        ];
        for (spec, name, key) in cases {
            let keyword = parse_todo_keyword_spec(spec);
            assert_eq!(
                (keyword.name.as_str(), keyword.fast_key),
                (*name, *key),
                "{spec}"
            );
        }
    }

    #[test]
    fn strict_spec_suffix_accepts_only_org_forms() {
        for ok in [
            "A", "A(x)", "A(x!)", "A(x@)", "A(x/!)", "A(x@/!)", "A(x!/@)", "A(@/!)", "A(!)",
            "A(@)", "A(/!)",
        ] {
            assert!(is_valid_todo_keyword_spec_suffix(ok), "{ok}");
        }
        for bad in [
            "A(ww)", "A()", "A(w", "A)w", "A(x)y", "A(x)(y)", "A(x/)", "A(/)", "A(x!!)", "A())",
        ] {
            assert!(!is_valid_todo_keyword_spec_suffix(bad), "{bad}");
        }
    }
}

#[derive(Debug, Clone)]
struct ResolvedTodoKeywordEntryDraft {
    keyword: String,
    state_type: String,
    shortcut: Option<char>,
    source_kind: TodoKeywordSourceKind,
    source_keyword: Option<String>,
    source_line_number: Option<u32>,
}
