//! Property and file keyword parsing helpers.

use crate::property::normalize_property_key;

use super::model::{ParsedKeyword, ParsedProperty, ParsedPropertySource};

pub(super) fn file_level_properties_from_keywords(
    keywords: &[ParsedKeyword],
) -> Vec<ParsedProperty> {
    keywords
        .iter()
        .filter_map(parsed_property_from_keyword)
        .collect()
}

pub(super) fn file_level_tags_from_keywords(keywords: &[ParsedKeyword]) -> Vec<String> {
    let mut tags = Vec::new();

    for keyword in keywords {
        if !keyword.key.eq_ignore_ascii_case("FILETAGS") {
            continue;
        }

        let Some(value) = keyword.value.as_deref() else {
            continue;
        };
        for tag in parse_filetags_keyword_value(value) {
            if !tags.iter().any(|existing| existing == &tag) {
                tags.push(tag);
            }
        }
    }

    tags
}

fn parse_filetags_keyword_value(value: &str) -> Vec<String> {
    value
        .split_whitespace()
        .flat_map(|part| {
            part.trim_matches(':')
                .split(':')
                .filter(|tag| !tag.is_empty())
                .map(str::to_string)
                .collect::<Vec<_>>()
        })
        .collect()
}

fn parsed_property_from_keyword(keyword: &ParsedKeyword) -> Option<ParsedProperty> {
    if keyword.key.eq_ignore_ascii_case("PROPERTY") {
        let (key, value, append) = parse_property_keyword_value(keyword.value.as_deref()?)?;
        Some(ParsedProperty {
            key,
            value,
            source: ParsedPropertySource::PropertyKeyword,
            append,
            line_number: keyword.line_number,
        })
    } else if keyword.key.eq_ignore_ascii_case("CATEGORY") {
        Some(ParsedProperty {
            key: "CATEGORY".to_string(),
            value: keyword.value.clone(),
            source: ParsedPropertySource::CategoryKeyword,
            append: false,
            line_number: keyword.line_number,
        })
    } else {
        None
    }
}

fn parse_property_keyword_value(value: &str) -> Option<(String, Option<String>, bool)> {
    let trimmed = value.trim();
    if trimmed.is_empty() {
        return None;
    }

    let mut parts = trimmed.splitn(2, char::is_whitespace);
    let raw_key = parts.next()?.trim();
    if raw_key.is_empty() {
        return None;
    }
    let raw_value = parts
        .next()
        .map(str::trim)
        .filter(|value| !value.is_empty())
        .map(str::to_string);
    let (key, append) = normalize_property_key(raw_key);
    Some((key, raw_value, append))
}

pub(super) fn parsed_property_from_raw_line(
    raw_line: &str,
    source: ParsedPropertySource,
    line_number: u32,
) -> Option<ParsedProperty> {
    // Org (`org-property-re`): `^[ \t]*:KEY:\(?:[ \t].*\)?[ \t]*$` with a KEY of non-blank
    // characters, so `:a b: c` and `:ID:x` are no properties and `:a:b: c` has the key `a:b`.
    let rest = raw_line.trim_start_matches([' ', '\t']).strip_prefix(':')?;
    let token_end = rest.find([' ', '\t']).unwrap_or(rest.len());
    let raw_key = rest[..token_end].strip_suffix(':')?;
    if raw_key.is_empty() {
        return None;
    }
    let value = Some(rest[token_end..].trim_matches([' ', '\t']).to_string());
    let (key, normalized_append) = normalize_property_key(raw_key);

    Some(ParsedProperty {
        key,
        value,
        source,
        append: normalized_append,
        line_number: Some(line_number),
    })
}
