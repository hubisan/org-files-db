//! The only place that still reads Orgize (stage 2 of #41, #103).
//!
//! The structure (headings, planning, drawers, keywords, blocks) comes from
//! `structure_scanner`. Orgize is fed only text that cannot carry structure and is
//! used for inline details:
//!
//! - timestamps in headline titles and body text (`scan_inline`),
//! - inline code, verbatim, inline source and export snippets, which link scanning skips
//!   (`scan_inline`),
//! - title normalization: markup stripped, links replaced by their description
//!   (`normalize_title_text`, `render_link_description_for_title`).
//!
//! Stage 3 replaces these three with own code and removes this module and the dependency.

use std::ops::Range;

use orgize::{
    ast::{DelayType, Link, RepeaterType, TimeUnit, Timestamp},
    rowan::{ast::AstNode, NodeOrToken},
    Org, SyntaxElement, SyntaxKind, SyntaxNode,
};

use super::line_index::LineIndex;
use super::line_lexer::{classify_line, lines, LineClass};
use super::link_scanner::{scan_links, LinkScanContext, LinkScannerConfig};
use super::model::{
    ParsedTimestamp, ParsedTimestampModifier, ParsedTimestampModifierKind,
    ParsedTimestampModifierType, ParsedTimestampRangeType, ParsedTimestampRole,
    ParsedTimestampType, ParsedTimestampUnit,
};
use super::structure_scanner::{RegionKind, Structure};
use super::timestamp_raw::unix_seconds_from_utc_date_time;
use super::timestamp_raw::{parse_timestamp_modifiers_from_raw, raw_timestamp_has_explicit_time};

/// Inline facts Orgize reads from text.
#[derive(Debug, Default)]
pub(super) struct InlineFacts {
    /// Inline code, verbatim, inline source and snippet ranges.
    pub(super) ignored_ranges: Vec<Range<usize>>,
    /// Timestamps with the `Body` role, in source order.
    pub(super) timestamps: Vec<ParsedTimestamp>,
}

impl InlineFacts {
    fn sort(&mut self) {
        self.timestamps.sort_by_key(|t| (t.byte_start, t.byte_end));
        self.timestamps.dedup_by_key(|t| (t.byte_start, t.byte_end));
    }
}

/// True when `text` holds a character that can start a timestamp or an inline code,
/// verbatim, snippet or inline source object. Without one Orgize has nothing to report.
fn may_hold_inline_objects(text: &str) -> bool {
    text.bytes()
        .any(|byte| matches!(byte, b'<' | b'[' | b'=' | b'~' | b'@'))
        || text.contains("src_")
}

/// Reads the inline facts of the whole document. Orgize only sees a copy of the source
/// in which everything the scanner knows to be structure is blanked out (same byte
/// offsets), so that it builds no headline tree, no drawers and no blocks: nesting depth
/// cannot grow its stack, and an empty quote, center or special block, which makes
/// Orgize assert, never reaches it. Headline lines are read one by one, because
/// emphasis may span lines in a paragraph and a title must not join the body.
pub(super) fn scan_inline(content: &str, structure: &Structure, index: &LineIndex) -> InlineFacts {
    let mut facts = InlineFacts::default();
    let body = blanked_body_text(content, structure);
    if may_hold_inline_objects(&body) {
        collect_inline(&Org::parse(&body), 0, index, &mut facts);
    }
    for heading in &structure.headings {
        let line = &content[heading.headline.clone()];
        if !may_hold_inline_objects(line) {
            continue;
        }
        // Dots for the stars: the line stays a plain paragraph line for Orgize.
        let line = format!("{}{}", ".".repeat(heading.level), &line[heading.level..]);
        collect_inline(
            &Org::parse(&line),
            heading.headline.start,
            index,
            &mut facts,
        );
    }
    facts.sort();
    facts
}

fn collect_inline(org: &Org, offset: usize, index: &LineIndex, facts: &mut InlineFacts) {
    for node in org.document().syntax().descendants() {
        match node.kind() {
            SyntaxKind::CODE
            | SyntaxKind::VERBATIM
            | SyntaxKind::INLINE_SRC
            | SyntaxKind::SNIPPET => {
                let range = node_byte_range(&node);
                if range.start < range.end {
                    facts
                        .ignored_ranges
                        .push(range.start + offset..range.end + offset);
                }
            }
            _ => {
                if let Some(timestamp) = Timestamp::cast(node) {
                    facts.timestamps.push(parsed_timestamp_from_orgize(
                        &timestamp,
                        Some(ParsedTimestampRole::Body),
                        offset,
                        index,
                    ));
                }
            }
        }
    }
}

fn node_byte_range(node: &SyntaxNode) -> Range<usize> {
    usize::from(node.text_range().start())..usize::from(node.text_range().end())
}

/// Copy of `content` with the same byte offsets in which structure is blanked (spaces) or,
/// where a line must stay text, defused. What stays is text that Org reads as paragraphs,
/// lists, tables, clocks and the content of quote, center, special, verse blocks and
/// drawers.
pub(super) fn blanked_body_text(content: &str, structure: &Structure) -> String {
    let mut buf = content.as_bytes().to_vec();
    // Lines Orgize would read as structure but the scanner did not: unclosed or
    // mismatched begin and end lines, keywords inside verse, stray drawer lines, and
    // `*` followed by a tab, which Orgize takes for a headline.
    for line in lines(content) {
        let text = line.text(content);
        let indent = text.len() - text.trim_start_matches([' ', '\t']).len();
        match classify_line(text) {
            LineClass::BlockBegin { .. }
            | LineClass::BlockEnd { .. }
            | LineClass::DynBlockBegin { .. }
            | LineClass::Keyword { .. } => {
                buf[line.start + indent] = b'.';
                buf[line.start + indent + 1] = b'.';
            }
            LineClass::Drawer { .. } | LineClass::DrawerEnd => buf[line.start + indent] = b'.',
            LineClass::Text if text.starts_with('*') => {
                let stars = text.bytes().take_while(|byte| *byte == b'*').count();
                if matches!(text.as_bytes().get(stars), None | Some(b'\t')) {
                    buf[line.start..line.start + stars].fill(b'.');
                }
            }
            _ => {}
        }
    }
    let mut blank = |range: Range<usize>| {
        for byte in &mut buf[range] {
            if *byte != b'\n' && *byte != b'\r' {
                *byte = b' ';
            }
        }
    };
    for heading in &structure.headings {
        blank(heading.headline.clone());
        if let Some(planning) = &heading.planning {
            blank(planning.range.clone());
        }
        if let Some(drawer) = &heading.properties {
            blank(drawer.range.clone());
        }
    }
    if let Some(drawer) = &structure.file_properties {
        blank(drawer.range.clone());
    }
    for keyword in structure.keywords.iter().chain(&structure.affiliated) {
        blank(keyword.line.clone());
    }
    for region in &structure.regions {
        match region.kind {
            RegionKind::Comment | RegionKind::FixedWidth => blank(region.range.clone()),
            RegionKind::Block if hides_content(&region.name) => blank(region.range.clone()),
            RegionKind::Block | RegionKind::Drawer | RegionKind::DynamicBlock => {
                blank(region.range.start..region.content.start);
                blank(region.content.end..region.range.end);
            }
        }
    }
    // Only ASCII bytes were written over whole characters or ASCII bytes.
    String::from_utf8(buf).unwrap_or_default()
}

/// Blocks whose content is no Org markup at all (`verse` keeps its objects).
pub(super) fn hides_content(block_name: &str) -> bool {
    ["src", "example", "export", "comment"]
        .iter()
        .any(|name| block_name.eq_ignore_ascii_case(name))
}

/// Org's title blanks are spaces and tabs; other Unicode whitespace such as NBSP is text.
fn trim_org_blanks(text: &str) -> String {
    text.trim_matches([' ', '\t']).to_string()
}

// No underscores or other markup characters: `_` could pair with one in the title.
const SENTINEL: &str = "ORGFILESDBSENTINEL ";
const TAIL: &str = " ORGFILESDBTAIL";

/// Visible text of a headline title: markup characters dropped, bracket links replaced by
/// their description or path, statistics cookies removed. `text` must be the title
/// without TODO keyword and priority cookie.
pub(super) fn normalize_title_text(text: &str) -> String {
    if !text
        .bytes()
        .any(|byte| matches!(byte, b'*' | b'/' | b'_' | b'=' | b'~' | b'['))
    {
        return trim_org_blanks(text);
    }
    // The prefix keeps Orgize from reading a leading TODO or DONE, the suffix from
    // stripping a tag-like word that Org leaves in the title.
    let parsed = Org::parse(format!("* {SENTINEL}{text}{TAIL}\n"));
    parsed
        .document()
        .headlines()
        .next()
        .map(|headline| normalize_title_elements(headline.title()))
        .and_then(|title| title.strip_prefix(SENTINEL).map(str::to_string))
        .and_then(|title| title.strip_suffix(TAIL).map(str::to_string))
        .map_or_else(|| trim_org_blanks(text), |title| trim_org_blanks(&title))
}

fn parsed_timestamp_from_orgize(
    timestamp: &Timestamp,
    role: Option<ParsedTimestampRole>,
    offset: usize,
    lines: &LineIndex,
) -> ParsedTimestamp {
    let raw_value = timestamp.raw();
    let byte_start = usize::from(timestamp.start()) + offset;
    let byte_end = usize::from(timestamp.end()) + offset;
    let range_type = timestamp_range_type(timestamp, &raw_value);
    let has_time = timestamp_has_explicit_time(timestamp, &raw_value);
    let (start_ts, end_ts) = normalize_timestamp_bounds(timestamp, range_type);

    ParsedTimestamp {
        role,
        raw_value,
        timestamp_type: timestamp_type(timestamp),
        range_type,
        has_time,
        start_ts,
        end_ts,
        byte_start,
        byte_end,
        line_number: Some(lines.line_for(byte_start)),
        modifiers: timestamp_modifiers(timestamp),
    }
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

fn timestamp_has_explicit_time(timestamp: &Timestamp, raw_value: &str) -> Option<bool> {
    if timestamp.is_diary() {
        return None;
    }

    Some(
        (timestamp.hour_start().is_some() && timestamp.minute_start().is_some())
            || (timestamp.hour_end().is_some() && timestamp.minute_end().is_some())
            || raw_timestamp_has_explicit_time(raw_value).unwrap_or(false),
    )
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
            SyntaxKind::COOKIE => {}
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

pub(super) fn render_link_description_for_title(raw_description: &str) -> String {
    let mut rendered = raw_description.to_string();
    let mut sentinel_index = 0usize;
    let (prefix, suffix) = loop {
        let prefix = format!("ORGFILESDBLINKDESCRIPTIONPREFIX{sentinel_index}X ");
        let suffix = format!(" ORGFILESDBLINKDESCRIPTIONSUFFIX{sentinel_index}X");
        if !raw_description.contains(&prefix) && !raw_description.contains(&suffix) {
            break (prefix, suffix);
        }
        sentinel_index += 1;
    };
    let nested_links = scan_links(
        raw_description,
        &LinkScannerConfig::default(),
        &LinkScanContext::default(),
    );
    let mut nested_token_index = 0usize;
    let mut nested_placeholders = nested_links
        .into_iter()
        .filter(|link| link.format == "bracket")
        .map(|link| {
            let token = loop {
                let candidate = format!("ORGFILESDBNESTEDLINK{nested_token_index}X");
                nested_token_index += 1;
                if !raw_description.contains(&candidate) {
                    break candidate;
                }
            };
            (link.byte_start..link.byte_end, token, link.raw)
        })
        .collect::<Vec<_>>();
    nested_placeholders.sort_by_key(|(range, _, _)| std::cmp::Reverse(range.start));
    for (range, token, _) in &nested_placeholders {
        rendered.replace_range(range.clone(), token);
    }

    let parsed = Org::parse(format!("* {prefix}{rendered}{suffix}\n"));
    let mut visible = parsed
        .document()
        .headlines()
        .next()
        .map(|headline| normalize_title_elements(headline.title()))
        .and_then(|title| {
            title
                .strip_prefix(&prefix)
                .and_then(|title| title.strip_suffix(&suffix))
                .map(str::to_string)
        })
        .unwrap_or(rendered);
    for (_, token, raw) in nested_placeholders {
        visible = visible.replace(&token, &raw);
    }
    visible
}
