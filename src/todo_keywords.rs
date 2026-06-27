use std::collections::HashSet;

use orgize::{
    ast::{Document as OrgDocument, Keyword},
    rowan::ast::AstNode,
    Org,
};

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
        TodoKeyword::with_fast_key(name, fast_key)
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
    let org = Org::parse(content);
    let keywords = collect_document_keywords(&org.document(), content);
    resolve_todo_keywords_from_keywords(&keywords)
        .unwrap_or_else(|| resolved_from_default_keywords(default_keywords, default_source_kind))
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

pub(crate) fn collect_document_keywords(
    document: &OrgDocument,
    content: &str,
) -> Vec<ParsedKeyword> {
    document
        .syntax()
        .descendants()
        .filter_map(Keyword::cast)
        .map(|keyword| ParsedKeyword {
            key: keyword.key().to_string(),
            value: Some(keyword.value().trim().to_string()).filter(|value| !value.is_empty()),
            line_number: Some(line_number_for_offset(
                content,
                usize::from(keyword.start()),
            )),
        })
        .collect()
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

fn split_todo_keyword_spec(spec: &str) -> Option<(&str, char)> {
    let open_paren = spec.rfind('(')?;
    let close_paren = spec.rfind(')')?;
    if close_paren != spec.len() - 1 || open_paren >= close_paren {
        return None;
    }

    let name = spec[..open_paren].trim();
    if name.is_empty() {
        return None;
    }

    let mut suffix_chars = spec[open_paren + 1..close_paren].chars();
    let fast_key = suffix_chars.next()?;
    match suffix_chars.next() {
        None => {}
        Some('!') | Some('@') if suffix_chars.next().is_none() => {}
        _ => return None,
    }

    Some((name, fast_key))
}

fn line_number_for_offset(content: &str, offset: usize) -> u32 {
    content[..offset]
        .bytes()
        .filter(|byte| *byte == b'\n')
        .count() as u32
        + 1
}

#[cfg(test)]
mod tests {
    use super::{parse_todo_keyword_spec, TodoKeyword};

    #[test]
    fn parse_todo_keyword_spec_trims_outer_whitespace() {
        assert_eq!(
            parse_todo_keyword_spec(" TODO(t) "),
            TodoKeyword::with_fast_key("TODO", 't')
        );
    }

    #[test]
    fn parse_todo_keyword_spec_leaves_malformed_suffixes_literal() {
        assert_eq!(
            parse_todo_keyword_spec("TODO(w@)"),
            TodoKeyword::with_fast_key("TODO", 'w')
        );
        assert_eq!(
            parse_todo_keyword_spec("TODO()"),
            TodoKeyword::new("TODO()")
        );
        assert_eq!(
            parse_todo_keyword_spec("TODO(ab)"),
            TodoKeyword::new("TODO(ab)")
        );
        assert_eq!(
            parse_todo_keyword_spec("TODO(t"),
            TodoKeyword::new("TODO(t")
        );
        assert_eq!(
            parse_todo_keyword_spec("TODO)t"),
            TodoKeyword::new("TODO)t")
        );
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
