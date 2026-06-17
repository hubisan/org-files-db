use std::path::Path;

use orgize::{ast::Headline, rowan::ast::AstNode, Org};

use super::diagnostics::ParseDiagnostic;
use super::model::{
    OrgParser, ParseOptions, ParsedHeading, ParsedKeyword, ParsedOrgDocument, ParsedProperty,
    TodoKeywordConfig,
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

        collect_headlines(
            document.headlines(),
            content,
            options,
            &mut parsed.headings,
            &mut parsed.diagnostics,
            None,
        );

        Ok(parsed)
    }
}

fn collect_headlines(
    headlines: impl Iterator<Item = Headline>,
    content: &str,
    options: &ParseOptions,
    output: &mut Vec<ParsedHeading>,
    diagnostics: &mut Vec<ParseDiagnostic>,
    parent_index: Option<usize>,
) {
    for headline in headlines {
        let start = usize::from(headline.start());
        let end = usize::from(headline.end());

        let original_title_raw = headline.title_raw().trim_end().to_string();
        let mut parsed = ParsedHeading::new(
            headline.level() as u8,
            original_title_raw.trim().to_string(),
            start,
            end,
        );
        parsed.title_raw = original_title_raw.clone();
        parsed.todo_keyword = headline.todo_keyword().map(|token| token.to_string());
        if parsed.todo_keyword.is_none() {
            if let Some((keyword, normalized_title)) =
                infer_todo_keyword(&original_title_raw, &options.todo_keywords)
            {
                parsed.todo_keyword = Some(keyword);
                parsed.title = normalized_title;
            }
        }
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
            content,
            options,
            output,
            diagnostics,
            Some(current_index),
        );
    }
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
