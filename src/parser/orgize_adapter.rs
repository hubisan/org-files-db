use std::{collections::HashSet, path::Path};

use orgize::{
    ast::{
        DelayType, Document as OrgDocument, Headline, Keyword, Link, NodeProperty, PropertyDrawer,
        RepeaterType, TimeUnit, Timestamp,
    },
    rowan::{ast::AstNode, NodeOrToken},
    Org, SyntaxElement, SyntaxKind, SyntaxNode,
};

use super::diagnostics::ParseDiagnostic;
use super::model::{
    file_local_todo_keyword_config, OrgParser, ParseOptions, ParsedHeading, ParsedKeyword,
    ParsedOrgDocument, ParsedProperty, ParsedPropertySource, ParsedTimestamp,
    ParsedTimestampModifier, ParsedTimestampModifierKind, ParsedTimestampModifierType,
    ParsedTimestampRangeType, ParsedTimestampRole, ParsedTimestampType, ParsedTimestampUnit,
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

        parsed.metadata.title = combined_document_title(&document);
        parsed.metadata.keywords = collect_document_keywords(&document, content);

        let active_todo_keywords = file_local_todo_keyword_config(&parsed.metadata.keywords)
            .unwrap_or_else(|| options.todo_keywords.clone());
        let mut level_zero = level_zero_heading(path, content, parsed.metadata.title.as_deref());
        if let Some(properties) = document.properties() {
            level_zero.properties.extend(parsed_properties_from_drawer(
                &properties,
                content,
                ParsedPropertySource::PropertyDrawer,
            ));
        }
        level_zero
            .properties
            .extend(file_level_properties_from_keywords(
                &parsed.metadata.keywords,
            ));
        parsed.headings.push(level_zero);

        collect_headlines(
            document.headlines(),
            path,
            content,
            &active_todo_keywords,
            &mut parsed.headings,
            Some(0),
        );

        Ok(parsed)
    }
}

fn combined_document_title(document: &OrgDocument) -> Option<String> {
    document
        .syntax()
        .descendants()
        .filter_map(Keyword::cast)
        .filter(|keyword| keyword.key().eq_ignore_ascii_case("TITLE"))
        .map(|keyword| keyword.value().trim().to_string())
        .filter(|title| !title.is_empty())
        .fold(None, |acc, title| match acc {
            Some(mut existing) => {
                if !existing.is_empty() {
                    existing.push(' ');
                }
                existing.push_str(&title);
                Some(existing)
            }
            None => Some(title),
        })
}

fn collect_headlines(
    headlines: impl Iterator<Item = Headline>,
    path: &Path,
    content: &str,
    todo_keywords: &TodoKeywordConfig,
    output: &mut Vec<ParsedHeading>,
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

        populate_heading_timestamps(&headline, content, &mut parsed);

        if let Some(properties) = headline.properties() {
            parsed.properties = parsed_properties_from_drawer(
                &properties,
                content,
                ParsedPropertySource::PropertyDrawer,
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

fn populate_heading_timestamps(headline: &Headline, content: &str, parsed: &mut ParsedHeading) {
    let mut seen_ranges = HashSet::new();
    let planning_node = headline.planning();

    if let Some(ref planning) = planning_node {
        for child in planning.syntax().children() {
            let role = match child.kind() {
                SyntaxKind::PLANNING_SCHEDULED => ParsedTimestampRole::Scheduled,
                SyntaxKind::PLANNING_DEADLINE => ParsedTimestampRole::Deadline,
                SyntaxKind::PLANNING_CLOSED => ParsedTimestampRole::Closed,
                _ => continue,
            };
            let parsed_timestamp = if let Some(timestamp) =
                child.children().find_map(Timestamp::cast)
            {
                parsed_timestamp_from_orgize(&timestamp, Some(role), content)
            } else if let Some(timestamp) =
                parsed_timestamp_from_planning_fallback(&child.to_string(), &child, role, content)
            {
                timestamp
            } else {
                continue;
            };
            seen_ranges.insert((parsed_timestamp.byte_start, parsed_timestamp.byte_end));
            parsed.timestamps.push(parsed_timestamp.clone());

            match role {
                ParsedTimestampRole::Scheduled => {
                    parsed.planning.scheduled = Some(parsed_timestamp)
                }
                ParsedTimestampRole::Deadline => parsed.planning.deadline = Some(parsed_timestamp),
                ParsedTimestampRole::Closed => parsed.planning.closed = Some(parsed_timestamp),
                ParsedTimestampRole::Body => {}
            }
        }
    }

    if planning_node.is_none() {
        populate_repeater_deadline_planning_fallback(content, parsed, &mut seen_ranges);
    }

    if let Some(title_node) = headline
        .syntax()
        .children()
        .find(|node| node.kind() == SyntaxKind::HEADLINE_TITLE)
    {
        for timestamp in title_node.descendants().filter_map(Timestamp::cast) {
            push_body_timestamp_if_new(&timestamp, content, parsed, &mut seen_ranges);
        }
    }

    for section in headline
        .syntax()
        .children()
        .filter(|node| node.kind() == SyntaxKind::SECTION)
    {
        for timestamp in section.descendants().filter_map(Timestamp::cast) {
            if timestamp
                .syntax()
                .ancestors()
                .any(|ancestor| ancestor.kind() == SyntaxKind::PLANNING)
            {
                continue;
            }
            push_body_timestamp_if_new(&timestamp, content, parsed, &mut seen_ranges);
        }
    }
}

fn push_body_timestamp_if_new(
    timestamp: &Timestamp,
    content: &str,
    parsed: &mut ParsedHeading,
    seen_ranges: &mut HashSet<(usize, usize)>,
) {
    let parsed_timestamp =
        parsed_timestamp_from_orgize(timestamp, Some(ParsedTimestampRole::Body), content);
    if seen_ranges.insert((parsed_timestamp.byte_start, parsed_timestamp.byte_end)) {
        parsed.timestamps.push(parsed_timestamp);
    }
}

fn parsed_timestamp_from_orgize(
    timestamp: &Timestamp,
    role: Option<ParsedTimestampRole>,
    content: &str,
) -> ParsedTimestamp {
    let raw_value = timestamp.raw();
    let byte_start = usize::from(timestamp.start());
    let byte_end = usize::from(timestamp.end());
    let range_type = timestamp_range_type(timestamp, &raw_value);
    let (start_ts, end_ts) = normalize_timestamp_bounds(timestamp, range_type);

    ParsedTimestamp {
        role,
        raw_value,
        timestamp_type: timestamp_type(timestamp),
        range_type,
        start_ts,
        end_ts,
        byte_start,
        byte_end,
        line_number: Some(line_number_for_offset(content, byte_start)),
        modifiers: timestamp_modifiers(timestamp),
    }
}

fn parsed_timestamp_from_planning_fallback(
    planning_text: &str,
    planning_node: &SyntaxNode,
    role: ParsedTimestampRole,
    content: &str,
) -> Option<ParsedTimestamp> {
    let (relative_start, raw_value) = extract_first_raw_timestamp(planning_text)?;
    let byte_start = usize::from(planning_node.text_range().start()) + relative_start;
    let byte_end = byte_start + raw_value.len();
    let timestamp_type = timestamp_type_from_raw(&raw_value)?;
    let (start_ts, end_ts, range_type) = normalize_raw_timestamp_bounds(&raw_value);

    Some(ParsedTimestamp {
        role: Some(role),
        raw_value: raw_value.clone(),
        timestamp_type,
        range_type,
        start_ts,
        end_ts,
        byte_start,
        byte_end,
        line_number: Some(line_number_for_offset(content, byte_start)),
        modifiers: parse_timestamp_modifiers_from_raw(&raw_value)?,
    })
}

fn populate_repeater_deadline_planning_fallback(
    content: &str,
    parsed: &mut ParsedHeading,
    seen_ranges: &mut HashSet<(usize, usize)>,
) {
    let Some(heading_text) = content.get(parsed.byte_start..parsed.byte_end) else {
        return;
    };
    let Some(first_newline) = heading_text.find('\n') else {
        return;
    };
    let after_heading = &heading_text[first_newline + 1..];
    let planning_line = after_heading
        .split_once('\n')
        .map(|(line, _)| line)
        .unwrap_or(after_heading);

    if !planning_line.contains('/') {
        return;
    }

    let line_offset = parsed.byte_start + first_newline + 1;
    for (role, raw_value, relative_start) in parse_planning_fallback_entries(planning_line) {
        let byte_start = line_offset + relative_start;
        let byte_end = byte_start + raw_value.len();
        let (start_ts, end_ts, range_type) = normalize_raw_timestamp_bounds(&raw_value);
        let parsed_timestamp = ParsedTimestamp {
            role: Some(role),
            raw_value: raw_value.clone(),
            timestamp_type: timestamp_type_from_raw(&raw_value)
                .unwrap_or(ParsedTimestampType::Active),
            range_type,
            start_ts,
            end_ts,
            byte_start,
            byte_end,
            line_number: Some(line_number_for_offset(content, byte_start)),
            modifiers: parse_timestamp_modifiers_from_raw(&raw_value).unwrap_or_default(),
        };

        if seen_ranges.insert((byte_start, byte_end)) {
            parsed.timestamps.push(parsed_timestamp.clone());
        }

        match role {
            ParsedTimestampRole::Scheduled => parsed.planning.scheduled = Some(parsed_timestamp),
            ParsedTimestampRole::Deadline => parsed.planning.deadline = Some(parsed_timestamp),
            ParsedTimestampRole::Closed => parsed.planning.closed = Some(parsed_timestamp),
            ParsedTimestampRole::Body => {}
        }
    }
}

fn parse_planning_fallback_entries(line: &str) -> Vec<(ParsedTimestampRole, String, usize)> {
    let mut entries = Vec::new();
    let mut offset = 0;

    while offset < line.len() {
        let remaining = &line[offset..];
        let trimmed = remaining.trim_start();
        let leading_ws = remaining.len() - trimmed.len();
        let entry_offset = offset + leading_ws;

        let (role, rest) = if let Some(rest) = trimmed.strip_prefix("SCHEDULED:") {
            (ParsedTimestampRole::Scheduled, rest)
        } else if let Some(rest) = trimmed.strip_prefix("DEADLINE:") {
            (ParsedTimestampRole::Deadline, rest)
        } else if let Some(rest) = trimmed.strip_prefix("CLOSED:") {
            (ParsedTimestampRole::Closed, rest)
        } else {
            break;
        };

        let timestamp_text = rest.trim_start();
        let Some((timestamp_start, raw_value)) = extract_first_raw_timestamp(timestamp_text) else {
            break;
        };
        let absolute_start =
            entry_offset + (trimmed.len() - timestamp_text.len()) + timestamp_start;
        offset = absolute_start + raw_value.len();
        entries.push((role, raw_value, absolute_start));
    }

    entries
}

fn extract_first_raw_timestamp(text: &str) -> Option<(usize, String)> {
    for (index, character) in text.char_indices() {
        let closing = match character {
            '<' => '>',
            '[' => ']',
            _ => continue,
        };
        let end = text[index..].find(closing)? + index + closing.len_utf8();
        return Some((index, text[index..end].to_string()));
    }
    None
}

fn timestamp_type_from_raw(raw_value: &str) -> Option<ParsedTimestampType> {
    if raw_value.starts_with("<%%(") || raw_value.starts_with("[%%(") {
        Some(ParsedTimestampType::Diary)
    } else if raw_value.starts_with('<') {
        Some(ParsedTimestampType::Active)
    } else if raw_value.starts_with('[') {
        Some(ParsedTimestampType::Inactive)
    } else {
        None
    }
}

fn normalize_raw_timestamp_bounds(
    raw_value: &str,
) -> (Option<i64>, Option<i64>, ParsedTimestampRangeType) {
    if matches!(
        timestamp_type_from_raw(raw_value),
        Some(ParsedTimestampType::Diary)
    ) {
        return (None, None, ParsedTimestampRangeType::None);
    }

    let inner = raw_value
        .strip_prefix(['<', '['])
        .and_then(|value| value.strip_suffix(['>', ']']))
        .unwrap_or(raw_value);
    let mut tokens = inner.split_whitespace();
    let Some(date_token) = tokens.next() else {
        return (None, None, ParsedTimestampRangeType::Unknown);
    };
    let Some((year, month, day)) = parse_date_token(date_token) else {
        return (None, None, ParsedTimestampRangeType::Unknown);
    };

    let mut start_hour = None;
    let mut start_minute = None;
    if let Some(token) = tokens.next() {
        if let Some((hour, minute)) = parse_time_token(token) {
            start_hour = Some(hour);
            start_minute = Some(minute);
        } else if let Some(time_token) = tokens.next() {
            if let Some((hour, minute)) = parse_time_token(time_token) {
                start_hour = Some(hour);
                start_minute = Some(minute);
            }
        }
    }

    (
        unix_seconds_from_utc_date_time(
            year,
            month,
            day,
            start_hour.unwrap_or(0),
            start_minute.unwrap_or(0),
        ),
        None,
        ParsedTimestampRangeType::None,
    )
}

fn parse_date_token(token: &str) -> Option<(i32, u32, u32)> {
    let mut parts = token.split('-');
    let year = parts.next()?.parse().ok()?;
    let month = parts.next()?.parse().ok()?;
    let day = parts.next()?.parse().ok()?;
    if parts.next().is_some() {
        return None;
    }
    Some((year, month, day))
}

fn parse_time_token(token: &str) -> Option<(u32, u32)> {
    let (hour, minute) = token.split_once(':')?;
    Some((hour.parse().ok()?, minute.parse().ok()?))
}

fn timestamp_type(timestamp: &Timestamp) -> ParsedTimestampType {
    if timestamp.is_diary() {
        ParsedTimestampType::Diary
    } else if timestamp.is_inactive() {
        ParsedTimestampType::Inactive
    } else {
        ParsedTimestampType::Active
    }
}

fn timestamp_range_type(timestamp: &Timestamp, raw_value: &str) -> ParsedTimestampRangeType {
    if !timestamp.is_range() {
        return ParsedTimestampRangeType::None;
    }

    let start_has_time = timestamp.hour_start().is_some() && timestamp.minute_start().is_some();
    let end_has_time = timestamp.hour_end().is_some() && timestamp.minute_end().is_some();
    let explicit_end = raw_value.contains("--");

    if explicit_end {
        if !start_has_time && !end_has_time {
            ParsedTimestampRangeType::DateRange
        } else {
            ParsedTimestampRangeType::DateTimeRange
        }
    } else if start_has_time && end_has_time {
        ParsedTimestampRangeType::TimeRange
    } else {
        ParsedTimestampRangeType::Unknown
    }
}

fn normalize_timestamp_bounds(
    timestamp: &Timestamp,
    range_type: ParsedTimestampRangeType,
) -> (Option<i64>, Option<i64>) {
    if timestamp.is_diary() {
        return (None, None);
    }

    let start = normalize_timestamp_part(
        timestamp.year_start(),
        timestamp.month_start(),
        timestamp.day_start(),
        timestamp.hour_start(),
        timestamp.minute_start(),
        true,
    );

    let end = match range_type {
        ParsedTimestampRangeType::None => None,
        ParsedTimestampRangeType::TimeRange => normalize_timestamp_part(
            timestamp.year_start(),
            timestamp.month_start(),
            timestamp.day_start(),
            timestamp.hour_end(),
            timestamp.minute_end(),
            false,
        ),
        ParsedTimestampRangeType::DateRange | ParsedTimestampRangeType::DateTimeRange => {
            normalize_timestamp_part(
                timestamp.year_end(),
                timestamp.month_end(),
                timestamp.day_end(),
                timestamp.hour_end(),
                timestamp.minute_end(),
                true,
            )
        }
        ParsedTimestampRangeType::Unknown => normalize_timestamp_part(
            timestamp.year_end(),
            timestamp.month_end(),
            timestamp.day_end(),
            timestamp.hour_end(),
            timestamp.minute_end(),
            true,
        ),
    };

    (start, end)
}

fn normalize_timestamp_part(
    year: Option<orgize::ast::Token>,
    month: Option<orgize::ast::Token>,
    day: Option<orgize::ast::Token>,
    hour: Option<orgize::ast::Token>,
    minute: Option<orgize::ast::Token>,
    default_to_midnight: bool,
) -> Option<i64> {
    let year = year?.to_string().parse().ok()?;
    let month = month?.to_string().parse().ok()?;
    let day = day?.to_string().parse().ok()?;
    let (hour, minute) = match (hour, minute) {
        (Some(hour), Some(minute)) => (
            hour.to_string().parse().ok()?,
            minute.to_string().parse().ok()?,
        ),
        (None, None) if default_to_midnight => (0, 0),
        _ => return None,
    };

    unix_seconds_from_utc_date_time(year, month, day, hour, minute)
}

fn timestamp_modifiers(timestamp: &Timestamp) -> Vec<ParsedTimestampModifier> {
    parse_timestamp_modifiers_from_raw(&timestamp.raw())
        .unwrap_or_else(|| timestamp_modifiers_from_orgize(timestamp))
}

fn timestamp_modifiers_from_orgize(timestamp: &Timestamp) -> Vec<ParsedTimestampModifier> {
    let mut modifiers = Vec::new();

    if let (Some(modifier_type), Some(value), Some(unit)) = (
        timestamp.repeater_type().map(parsed_repeater_type),
        timestamp.repeater_value(),
        timestamp.repeater_unit().map(parsed_timestamp_unit),
    ) {
        modifiers.push(ParsedTimestampModifier {
            kind: ParsedTimestampModifierKind::Repeater,
            modifier_type,
            value: i64::from(value),
            unit,
            repeater_deadline_value: None,
            repeater_deadline_unit: None,
        });
    }

    if let (Some(modifier_type), Some(value), Some(unit)) = (
        timestamp.warning_type().map(parsed_warning_type),
        timestamp.warning_value(),
        timestamp.warning_unit().map(parsed_timestamp_unit),
    ) {
        modifiers.push(ParsedTimestampModifier {
            kind: ParsedTimestampModifierKind::Warning,
            modifier_type,
            value: i64::from(value),
            unit,
            repeater_deadline_value: None,
            repeater_deadline_unit: None,
        });
    }

    modifiers
}

fn parse_timestamp_modifiers_from_raw(raw_value: &str) -> Option<Vec<ParsedTimestampModifier>> {
    if raw_value.starts_with("<%%(") || raw_value.starts_with("[%%(") {
        return Some(Vec::new());
    }

    let mut modifiers = Vec::new();

    for token in raw_value.split_whitespace() {
        let token = token.trim_matches(|character| matches!(character, '<' | '>' | '[' | ']'));
        if token.is_empty() {
            continue;
        }

        if let Some(modifier) = parse_repeater_modifier_token(token) {
            modifiers.push(modifier);
            continue;
        }

        if let Some(modifier) = parse_warning_modifier_token(token) {
            modifiers.push(modifier);
        }
    }

    Some(modifiers)
}

fn parse_repeater_modifier_token(token: &str) -> Option<ParsedTimestampModifier> {
    let (modifier_type, remainder) = if let Some(remainder) = token.strip_prefix("++") {
        (ParsedTimestampModifierType::CatchUp, remainder)
    } else if let Some(remainder) = token.strip_prefix(".+") {
        (ParsedTimestampModifierType::Restart, remainder)
    } else if let Some(remainder) = token.strip_prefix('+') {
        (ParsedTimestampModifierType::Cumulate, remainder)
    } else {
        return None;
    };

    let (value, unit, remainder) = parse_modifier_value_unit(remainder)?;
    let (repeater_deadline_value, repeater_deadline_unit) =
        if let Some(remainder) = remainder.strip_prefix('/') {
            let (value, unit, remainder) = parse_modifier_value_unit(remainder)?;
            if !remainder.is_empty() {
                return None;
            }
            (Some(value), Some(unit))
        } else {
            if !remainder.is_empty() {
                return None;
            }
            (None, None)
        };

    Some(ParsedTimestampModifier {
        kind: ParsedTimestampModifierKind::Repeater,
        modifier_type,
        value,
        unit,
        repeater_deadline_value,
        repeater_deadline_unit,
    })
}

fn parse_warning_modifier_token(token: &str) -> Option<ParsedTimestampModifier> {
    let (modifier_type, remainder) = if let Some(remainder) = token.strip_prefix("--") {
        (ParsedTimestampModifierType::First, remainder)
    } else if let Some(remainder) = token.strip_prefix('-') {
        (ParsedTimestampModifierType::All, remainder)
    } else {
        return None;
    };

    let (value, unit, remainder) = parse_modifier_value_unit(remainder)?;
    if !remainder.is_empty() {
        return None;
    }

    Some(ParsedTimestampModifier {
        kind: ParsedTimestampModifierKind::Warning,
        modifier_type,
        value,
        unit,
        repeater_deadline_value: None,
        repeater_deadline_unit: None,
    })
}

fn parse_modifier_value_unit(input: &str) -> Option<(i64, ParsedTimestampUnit, &str)> {
    let digits_end = input
        .find(|character: char| !character.is_ascii_digit())
        .unwrap_or(input.len());
    if digits_end == 0 {
        return None;
    }

    let value = input[..digits_end].parse::<i64>().ok()?;
    if value <= 0 {
        return None;
    }

    let remainder = &input[digits_end..];
    let unit = remainder
        .chars()
        .next()
        .and_then(parsed_timestamp_unit_from_char)?;
    Some((value, unit, &remainder[1..]))
}

fn parsed_timestamp_unit_from_char(character: char) -> Option<ParsedTimestampUnit> {
    match character {
        'h' => Some(ParsedTimestampUnit::Hour),
        'd' => Some(ParsedTimestampUnit::Day),
        'w' => Some(ParsedTimestampUnit::Week),
        'm' => Some(ParsedTimestampUnit::Month),
        'y' => Some(ParsedTimestampUnit::Year),
        _ => None,
    }
}

fn parsed_repeater_type(repeater_type: RepeaterType) -> ParsedTimestampModifierType {
    match repeater_type {
        RepeaterType::Cumulate => ParsedTimestampModifierType::Cumulate,
        RepeaterType::CatchUp => ParsedTimestampModifierType::CatchUp,
        RepeaterType::Restart => ParsedTimestampModifierType::Restart,
    }
}

fn parsed_warning_type(delay_type: DelayType) -> ParsedTimestampModifierType {
    match delay_type {
        DelayType::All => ParsedTimestampModifierType::All,
        DelayType::First => ParsedTimestampModifierType::First,
    }
}

fn parsed_timestamp_unit(unit: TimeUnit) -> ParsedTimestampUnit {
    match unit {
        TimeUnit::Hour => ParsedTimestampUnit::Hour,
        TimeUnit::Day => ParsedTimestampUnit::Day,
        TimeUnit::Week => ParsedTimestampUnit::Week,
        TimeUnit::Month => ParsedTimestampUnit::Month,
        TimeUnit::Year => ParsedTimestampUnit::Year,
    }
}

fn unix_seconds_from_utc_date_time(
    year: i32,
    month: u32,
    day: u32,
    hour: u32,
    minute: u32,
) -> Option<i64> {
    // The project does not model time zones yet, so planning timestamps are
    // normalized as UTC-naive Unix seconds.
    let days = days_from_civil(year, month, day)?;
    let seconds = i64::from(hour) * 3_600 + i64::from(minute) * 60;
    days.checked_mul(86_400)?.checked_add(seconds)
}

fn days_from_civil(year: i32, month: u32, day: u32) -> Option<i64> {
    let mut year = i64::from(year);
    let month = i64::from(month);
    let day = i64::from(day);

    year -= if month <= 2 { 1 } else { 0 };
    let era = if year >= 0 { year } else { year - 399 } / 400;
    let year_of_era = year - era * 400;
    let month_index = month + if month > 2 { -3 } else { 9 };
    let day_of_year = (153 * month_index + 2) / 5 + day - 1;
    let day_of_era = year_of_era * 365 + year_of_era / 4 - year_of_era / 100 + day_of_year;
    Some(era * 146_097 + day_of_era - 719_468)
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

fn collect_document_keywords(document: &OrgDocument, content: &str) -> Vec<ParsedKeyword> {
    let mut keywords = document
        .keywords()
        .map(|keyword| parsed_keyword_from_orgize(&keyword, content))
        .collect::<Vec<_>>();
    merge_special_keywords_from_content(&mut keywords, content);
    keywords
}

fn parsed_keyword_from_orgize(keyword: &Keyword, content: &str) -> ParsedKeyword {
    ParsedKeyword {
        key: keyword.key().to_string(),
        value: Some(keyword.value().trim().to_string()).filter(|value| !value.is_empty()),
        line_number: Some(line_number_for_offset(
            content,
            usize::from(keyword.start()),
        )),
    }
}

fn merge_special_keywords_from_content(keywords: &mut Vec<ParsedKeyword>, content: &str) {
    keywords.retain(|keyword| !is_full_buffer_special_keyword_name(&keyword.key));
    keywords.extend(special_keywords_from_content(content));
}

fn special_keywords_from_content(content: &str) -> Vec<ParsedKeyword> {
    content
        .lines()
        .enumerate()
        .filter_map(|(index, line)| {
            let remainder = line.strip_prefix("#+")?;
            let (key, value) = remainder.split_once(':')?;
            if !is_full_buffer_special_keyword_name(key) {
                return None;
            }

            Some(ParsedKeyword {
                key: key.to_string(),
                value: Some(value.trim().to_string()).filter(|value| !value.is_empty()),
                line_number: Some(index as u32 + 1),
            })
        })
        .collect()
}

fn is_full_buffer_special_keyword_name(key: &str) -> bool {
    is_file_local_todo_keyword_name(key)
        || key.eq_ignore_ascii_case("PROPERTY")
        || key.eq_ignore_ascii_case("CATEGORY")
}

fn is_file_local_todo_keyword_name(key: &str) -> bool {
    key.eq_ignore_ascii_case("TODO")
        || key.eq_ignore_ascii_case("SEQ_TODO")
        || key.eq_ignore_ascii_case("TYP_TODO")
}

fn file_level_properties_from_keywords(keywords: &[ParsedKeyword]) -> Vec<ParsedProperty> {
    keywords
        .iter()
        .filter_map(parsed_property_from_keyword)
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

fn parsed_properties_from_drawer(
    drawer: &PropertyDrawer,
    content: &str,
    source: ParsedPropertySource,
) -> Vec<ParsedProperty> {
    drawer
        .node_properties()
        .filter_map(|property| parsed_property_from_node(&property, content, source))
        .collect()
}

fn parsed_property_from_node(
    property: &NodeProperty,
    content: &str,
    source: ParsedPropertySource,
) -> Option<ParsedProperty> {
    let mut text_tokens = property
        .syntax()
        .children_with_tokens()
        .filter_map(|element| element.into_token())
        .filter(|token| token.kind() == SyntaxKind::TEXT)
        .map(|token| token.to_string());

    let raw_key = text_tokens.next()?;
    let value = Some(text_tokens.next().unwrap_or_default());
    let append = property
        .syntax()
        .children_with_tokens()
        .filter_map(|element| element.into_token())
        .any(|token| token.kind() == SyntaxKind::PLUS);
    let (key, normalized_append) = normalize_property_key(&raw_key);

    Some(ParsedProperty {
        key,
        value,
        source,
        append: append || normalized_append,
        line_number: Some(line_number_for_offset(
            content,
            usize::from(property.start()),
        )),
    })
}

fn normalize_property_key(raw_key: &str) -> (String, bool) {
    let (key, append) = if let Some(key) = raw_key.strip_suffix('+') {
        (key, true)
    } else {
        (raw_key, false)
    };
    (key.to_uppercase(), append)
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
