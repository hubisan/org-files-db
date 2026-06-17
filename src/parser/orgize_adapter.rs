use std::path::Path;

use orgize::{ast::Headline, rowan::ast::AstNode, Org};

use super::diagnostics::ParseDiagnostic;
use super::model::{
    OrgParser, ParseOptions, ParsedHeading, ParsedKeyword, ParsedOrgDocument, ParsedProperty,
    TodoKeywordConfig, TodoType,
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

        let active_todo_keywords = file_local_todo_keywords(&parsed.metadata.keywords)
            .unwrap_or_else(|| options.todo_keywords.clone());
        parsed.headings.push(level_zero_heading(path, content));

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
        let mut parsed = ParsedHeading::new(
            path,
            headline.level() as u8,
            original_title_raw.trim().to_string(),
            start,
            end,
        );
        parsed.title_raw = original_title_raw.clone();
        parsed.todo_keyword = headline.todo_keyword().map(|token| token.to_string());
        if parsed.todo_keyword.is_none() {
            if let Some((keyword, normalized_title)) =
                infer_todo_keyword(&original_title_raw, todo_keywords)
            {
                parsed.todo_keyword = Some(keyword);
                parsed.title = normalized_title;
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

fn level_zero_heading(path: &Path, content: &str) -> ParsedHeading {
    let path_title = path.display().to_string();
    let mut heading = ParsedHeading::new(path, 0, path_title.clone(), 0, content.len());
    heading.title_raw = path_title;
    heading.line_number = Some(1);
    heading.is_root = true;
    heading
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
        let remainder = title_raw.strip_prefix(&keyword.name)?;
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

fn file_local_todo_keywords(keywords: &[ParsedKeyword]) -> Option<TodoKeywordConfig> {
    let todo_value = keywords
        .iter()
        .find(|keyword| keyword.key.eq_ignore_ascii_case("TODO"))
        .and_then(|keyword| keyword.value.as_deref())?;

    let mut open = Vec::new();
    let mut closed = Vec::new();
    let mut in_closed_section = false;

    for token in todo_value.split_whitespace() {
        if token == "|" {
            in_closed_section = true;
            continue;
        }

        let parsed = parse_todo_keyword_token(token)?;
        if in_closed_section {
            closed.push(parsed);
        } else {
            open.push(parsed);
        }
    }

    if open.is_empty() && closed.is_empty() {
        None
    } else {
        Some(TodoKeywordConfig { open, closed })
    }
}

fn parse_todo_keyword_token(token: &str) -> Option<super::model::TodoKeyword> {
    if let Some((name, suffix)) = token.split_once('(') {
        let fast_key = suffix.strip_suffix(')')?.chars().next()?;
        Some(super::model::TodoKeyword::with_fast_key(
            name.trim(),
            fast_key,
        ))
    } else {
        Some(super::model::TodoKeyword::new(token.trim()))
    }
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
