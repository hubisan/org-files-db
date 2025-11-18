// ------------------------------------------------------------
// Org-mode metadata parsing: drawer properties, inline properties,
// and file keywords (LEVEL-0 only).
// ------------------------------------------------------------

use crate::types::Property;

/// Parses drawer properties of the form:
/// :KEY: value
///
/// Must be used only when inside a PROPERTIES drawer.
/// Returns Some(Property) or None if the line is not a drawer property.
pub fn parse_drawer_property(line: &str) -> Option<Property> {
    let trimmed = line.trim();

    if trimmed.starts_with(':') && trimmed.contains(':') {
        if let Some((key, value)) = trimmed[1..].split_once(':') {
            let key = key.trim().to_ascii_uppercase();
            let value = value.trim().to_string();

            return Some(Property { key, value });
        }
    }

    None
}

/// Parses inline properties of the form:
/// #+PROPERTY: KEY value
///
/// Only valid before the first heading (LEVEL-0).
pub fn parse_inline_property(line: &str) -> Option<Property> {
    let trimmed = line.trim();

    if let Some(rest) = trimmed.strip_prefix("#+PROPERTY:") {
        let mut parts = rest.trim().splitn(2, ' ');

        if let (Some(key), Some(value)) = (parts.next(), parts.next()) {
            return Some(Property {
                key: key.trim().to_ascii_uppercase(),
                value: value.trim().to_string(),
            });
        }
    }

    None
}

/// Parsed keyword can produce:
/// - Generic property     (KEYVALUE)
/// - Filetags             (Vec<String>)
/// - Category             (String)
pub enum ParsedKeyword {
    Property(Property),
    FileTags(Vec<String>),
    Category(String),
}

/// Parse Org file-level keywords:
/// #+KEY: value
/// 
/// Only valid before the first heading.
/// FILETAGS, PROPERTY and CATEGORY have special handling.
/// All are stored as keyword.
/// FILETAGS is stored as tags in addition: 
///   splits value into multiple tags and store as tags.
/// CATEGORY is stored as property in addition.
/// PROPERTY ist stored as property in addition.
pub fn parse_keywords(line: &str) -> Option<ParsedKeyword> {
    let trimmed = line.trim();

    if line.starts_with("#+") {
        return None;
    }

    // remove "#+"
    let content = &line[2..];

    if let Some((key, value)) = content.split_once(':') {
        let key = key.trim().to_ascii_uppercase();
        let value = value.trim().to_string();

        match key.as_str() {
            "FILETAGS" => {
                let tags = value
                    .split_whitespace()
                    .map(|s| s.to_string())
                    .collect();
                return Some(ParsedKeyword::FileTags(tags));
            }
            "CATEGORY" => {
                return Some(ParsedKeyword::Category(value));
            }
            "PROPERTY" => return None, // handled in inline property parser
            _ => {
                return Some(ParsedKeyword::Property(Property {
                    key,
                    value,
                }));
            }
        }
    }

    None
}
