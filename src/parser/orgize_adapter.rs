use std::path::Path;

use orgize::{
    ast::{Headline, Link},
    rowan::{ast::AstNode, NodeOrToken},
    Org, SyntaxElement, SyntaxKind, SyntaxNode,
};

use super::diagnostics::ParseDiagnostic;
use super::model::{
    file_local_todo_keyword_config, OrgParser, ParseOptions, ParsedHeading, ParsedKeyword,
    ParsedOrgDocument, ParsedProperty, TodoKeywordConfig, TodoType,
};

#[derive(Debug, Default, Clone, Copy)]
pub struct OrgizeAdapter;

impl OrgizeAdapter {
    pub fn new() -> Self {
        Self
    }
}

impl OrgParser for OrgizeAdapter {
    fn parse_document(
        &self,
        path: &Path,
        content: &str,
        options: &ParseOptions,
    ) -> Result<ParsedOrgDocument, ParseDiagnostic> {
        let org = Org::parse(content);
        let document = org.document();
        let mut parsed = ParsedOrgDocument::new(path);

        parsed.metadata.title = document.title();
        parsed.metadata.keywords = document
            .keywords()
            .map(|keyword| ParsedKeyword {
                key: keyword.key().to_string(),
                value: Some(keyword.value().trim().to_string()).filter(|value| !value.is_empty()),
            })
            .collect();
        merge_file_local_todo_keywords_from_content(&mut parsed.metadata.keywords, content);

        let active_todo_keywords = file_local_todo_keyword_config(&parsed.metadata.keywords)
            .unwrap_or_else(|| options.todo_keywords.clone());
        parsed.headings.push(level_zero_heading(
            path,
            content,
            parsed.metadata.title.as_deref(),
        ));

        collect_headlines(
            document.headlines(),
            path,
            content,
            &active_todo_keywords,
            &mut parsed.headings,
            &mut parsed.diagnostics,
            Some(0),
        );

        Ok(parsed)
    }
}

fn collect_headlines(
    headlines: impl Iterator<Item = Headline>,
    path: &Path,
    content: &str,
    todo_keywords: &TodoKeywordConfig,
    output: &mut Vec<ParsedHeading>,
    diagnostics: &mut Vec<ParseDiagnostic>,
    parent_index: Option<usize>,
) {
    for headline in headlines {
        let start = usize::from(headline.start());
        let end = usize::from(headline.end());

        let original_title_raw = headline.title_raw().trim_end().to_string();
        let source_title_raw = source_title_raw_from_content_line(content, start);
        let normalized_title = normalize_title_elements(headline.title());
        let mut parsed =
            ParsedHeading::new(path, headline.level() as u8, normalized_title, start, end);
        parsed.title_raw = original_title_raw.trim().to_string();
        parsed.todo_keyword = headline.todo_keyword().map(|token| token.to_string());
        if parsed
            .todo_keyword
            .as_deref()
            .filter(|keyword| !todo_keyword_is_active(keyword, todo_keywords))
            .is_some()
        {
            parsed.todo_keyword = None;
            parsed.title_raw = source_title_raw.clone();
            parsed.title = normalize_title_preserving_leading_keyword(&source_title_raw);
        }
        if parsed.todo_keyword.is_none() {
            if source_title_raw != original_title_raw.trim() {
                parsed.title_raw = source_title_raw.clone();
                parsed.title = normalize_title_preserving_leading_keyword(&source_title_raw);
            }
            if let Some((keyword, stripped_title_raw)) =
                infer_todo_keyword(&source_title_raw, todo_keywords)
            {
                parsed.todo_keyword = Some(keyword);
                parsed.title_raw = stripped_title_raw.clone();
                parsed.title = normalize_title_from_raw(&stripped_title_raw);
            }
        }
        parsed.todo_type = parsed
            .todo_keyword
            .as_deref()
            .and_then(|keyword| todo_type_for_keyword(keyword, todo_keywords));
        parsed.priority = headline.priority().and_then(|token| token.chars().next());
        parsed.tags = headline.tags().map(|tag| tag.to_string()).collect();
        parsed.line_number = Some(line_number_for_offset(content, start));
        parsed.parent_index = parent_index;
        parsed.is_archived = headline.is_archived();
        parsed.is_root = parent_index.is_none();

        if headline.planning().is_some() {
            parsed.planning.scheduled = headline.scheduled().map(|ts| ts.syntax().to_string());
            parsed.planning.deadline = headline.deadline().map(|ts| ts.syntax().to_string());
            parsed.planning.closed = headline.closed().map(|ts| ts.syntax().to_string());
        }

        if let Some(properties) = headline.properties() {
            parsed.properties = properties
                .iter()
                .map(|(key, value)| ParsedProperty {
                    key: key.to_string(),
                    value: value.to_string(),
                    inherited: false,
                })
                .collect();

            diagnostics.push(
                ParseDiagnostic::warning(
                    "Orgize adapter property extraction is currently local-only and does not handle inheritance",
                )
                .with_file_path(path)
                .with_line_number(line_number_for_offset(content, start))
                .with_byte_range(
                    usize::from(properties.start()),
                    usize::from(properties.end()),
                ),
            );
        }

        output.push(parsed);
        let current_index = output.len() - 1;

        collect_headlines(
            headline.headlines(),
            path,
            content,
            todo_keywords,
            output,
            diagnostics,
            Some(current_index),
        );
    }
}

fn level_zero_heading(path: &Path, content: &str, document_title: Option<&str>) -> ParsedHeading {
    let title = synthetic_level_zero_title(path, document_title);
    let mut heading = ParsedHeading::new(path, 0, title.clone(), 0, content.len());
    heading.title_raw = title;
    heading.line_number = Some(1);
    heading.is_root = true;
    heading
}

fn synthetic_level_zero_title(path: &Path, document_title: Option<&str>) -> String {
    if let Some(title) = document_title
        .map(str::trim)
        .filter(|title| !title.is_empty())
    {
        return title.to_string();
    }

    path.file_stem()
        .or_else(|| path.file_name())
        .map(|name| name.to_string_lossy().into_owned())
        .filter(|name| !name.is_empty())
        .unwrap_or_else(|| path.display().to_string())
}

fn normalize_title_from_raw(title_raw: &str) -> String {
    let parsed = Org::parse(format!("* {title_raw}\n"));
    parsed
        .document()
        .headlines()
        .next()
        .map(|headline| normalize_title_elements(headline.title()))
        .unwrap_or_else(|| title_raw.trim().to_string())
}

fn normalize_title_preserving_leading_keyword(title_raw: &str) -> String {
    const SENTINEL: &str = "ORG_FILES_DB_SENTINEL ";
    let parsed = Org::parse(format!("* {SENTINEL}{title_raw}\n"));
    parsed
        .document()
        .headlines()
        .next()
        .map(|headline| normalize_title_elements(headline.title()))
        .and_then(|title| title.strip_prefix(SENTINEL).map(str::to_string))
        .unwrap_or_else(|| title_raw.trim().to_string())
}

fn normalize_title_elements(elements: impl Iterator<Item = SyntaxElement>) -> String {
    let mut normalized = String::new();

    for element in elements {
        push_normalized_element(&mut normalized, element);
    }

    normalized.trim().to_string()
}

fn push_normalized_element(output: &mut String, element: SyntaxElement) {
    match element {
        NodeOrToken::Node(node) => match node.kind() {
            SyntaxKind::LINK => {
                if let Some(link) = Link::cast(node.clone()) {
                    if link.has_description() {
                        output.push_str(&normalize_title_elements(link.description()));
                    } else {
                        output.push_str(link.path().to_string().trim());
                    }
                } else {
                    push_normalized_children(output, &node);
                }
            }
            kind if is_supported_title_markup(kind) => {
                push_markup_contents(output, &node);
            }
            _ => push_normalized_children(output, &node),
        },
        NodeOrToken::Token(token) => output.push_str(token.text()),
    }
}

fn push_normalized_children(output: &mut String, node: &SyntaxNode) {
    for child in node.children_with_tokens() {
        push_normalized_element(output, child);
    }
}

fn push_markup_contents(output: &mut String, node: &SyntaxNode) {
    let children: Vec<_> = node.children_with_tokens().collect();
    let child_count = children.len();

    for (index, child) in children.into_iter().enumerate() {
        if index == 0 || index + 1 == child_count {
            continue;
        }
        push_normalized_element(output, child);
    }
}

fn is_supported_title_markup(kind: SyntaxKind) -> bool {
    matches!(
        kind,
        SyntaxKind::BOLD
            | SyntaxKind::ITALIC
            | SyntaxKind::UNDERLINE
            | SyntaxKind::VERBATIM
            | SyntaxKind::CODE
    )
}

fn merge_file_local_todo_keywords_from_content(keywords: &mut Vec<ParsedKeyword>, content: &str) {
    keywords.retain(|keyword| !is_file_local_todo_keyword_name(&keyword.key));
    keywords.extend(file_local_todo_keywords_from_content(content));
}

fn file_local_todo_keywords_from_content(content: &str) -> Vec<ParsedKeyword> {
    content
        .lines()
        .filter_map(|line| {
            let remainder = line.strip_prefix("#+")?;
            let (key, value) = remainder.split_once(':')?;
            if !is_file_local_todo_keyword_name(key) {
                return None;
            }

            Some(ParsedKeyword {
                key: key.to_string(),
                value: Some(value.trim().to_string()).filter(|value| !value.is_empty()),
            })
        })
        .collect()
}

fn is_file_local_todo_keyword_name(key: &str) -> bool {
    key.eq_ignore_ascii_case("TODO")
        || key.eq_ignore_ascii_case("SEQ_TODO")
        || key.eq_ignore_ascii_case("TYP_TODO")
}

fn line_number_for_offset(content: &str, offset: usize) -> u32 {
    content[..offset]
        .bytes()
        .filter(|byte| *byte == b'\n')
        .count() as u32
        + 1
}

fn infer_todo_keyword(
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

fn todo_type_for_keyword(keyword: &str, todo_keywords: &TodoKeywordConfig) -> Option<TodoType> {
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

fn todo_keyword_is_active(keyword: &str, todo_keywords: &TodoKeywordConfig) -> bool {
    todo_keywords
        .all_keywords()
        .any(|candidate| candidate.name == keyword)
}

fn source_title_raw_from_content_line(content: &str, start: usize) -> String {
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
    strip_trailing_org_tags(without_stars).trim().to_string()
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
        && value[1..value.len() - 1]
            .split(':')
            .all(|segment| !segment.is_empty())
}
