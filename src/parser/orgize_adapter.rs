use std::{collections::HashSet, ops::Range, path::Path};

use orgize::{
    ast::{
        CenterBlock, CommentBlock, DelayType, Document as OrgDocument, Drawer, ExampleBlock,
        ExportBlock, Headline, Keyword, Link, NodeProperty, PropertyDrawer, QuoteBlock,
        RepeaterType, Section, SourceBlock, SpecialBlock, TimeUnit, Timestamp, VerseBlock,
    },
    config::ParseConfig,
    rowan::{ast::AstNode, NodeOrToken},
    Org, SyntaxElement, SyntaxKind, SyntaxNode,
};

use super::diagnostics::ParseDiagnostic;
use super::line_index::LineIndex;
use super::link_scanner::{scan_links, LinkScanContext, LinkScannerConfig};
use super::model::{
    OrgParserCore, ParseOptions, ParsedHeading, ParsedLink, ParsedLinkSourceContext,
    ParsedOrgDocument, ParsedProperty, ParsedPropertySource, ParsedTimestamp,
    ParsedTimestampModifier, ParsedTimestampModifierKind, ParsedTimestampModifierType,
    ParsedTimestampRangeType, ParsedTimestampRole, ParsedTimestampType, ParsedTimestampUnit,
    TodoKeywordConfig,
};
use super::properties::{
    file_level_properties_from_keywords, file_level_tags_from_keywords, is_org_comment_line,
    parsed_property_from_raw_line,
};
use super::timestamp_raw::{
    extract_first_raw_timestamp, normalize_raw_timestamp_bounds,
    parse_timestamp_modifiers_from_raw, populate_text_planning_fallback,
    raw_timestamp_has_explicit_time, timestamp_type_from_raw, unix_seconds_from_utc_date_time,
};
use super::title::{
    cookie_follows_comment, infer_todo_keyword, link_contains_range, links_in_range,
    placeholder_title_after_todo_prefix, priority_from_source_title,
    restore_title_link_placeholders, source_title_from_content_line, source_title_tags,
    strip_leading_priority_cookie, todo_keyword_is_active, todo_type_for_keyword,
};
use crate::todo_keywords::collect_document_keywords;

#[derive(Debug, Default, Clone, Copy)]
pub struct OrgizeAdapter;

#[derive(Debug, Clone, PartialEq, Eq)]
struct ContextSpan {
    range: Range<usize>,
    source_context: ParsedLinkSourceContext,
}

#[derive(Debug, Clone, Default, PartialEq, Eq)]
struct LinkStructuralContext {
    ignored_byte_ranges: Vec<Range<usize>>,
    context_spans: Vec<ContextSpan>,
}

impl OrgizeAdapter {
    pub fn new() -> Self {
        Self
    }
}

impl OrgParserCore for OrgizeAdapter {
    fn parse_document_core(
        &self,
        path: &Path,
        content: &str,
        options: &ParseOptions,
    ) -> Result<ParsedOrgDocument, ParseDiagnostic> {
        if let Some((level, line_number)) = first_heading_over_depth_limit(content) {
            return Err(ParseDiagnostic::error(format!(
                "heading nesting level {level} exceeds the supported maximum of \
                 {MAX_HEADING_LEVEL}"
            ))
            .with_file_path(path)
            .with_line_number(line_number));
        }
        let org = parse_org_document(content, &options.todo_keywords);
        let document = org.document();
        let mut parsed = ParsedOrgDocument::new(path);
        let lines = LineIndex::new(content);

        parsed.metadata.title = combined_document_title(&document);
        parsed.metadata.keywords = collect_document_keywords(&document, &lines);
        let structural_context = collect_link_structural_context(&document);
        parsed.links = scan_links(
            content,
            &options.link_scanner,
            &LinkScanContext {
                ignored_byte_ranges: structural_context.ignored_byte_ranges,
            },
        );
        annotate_links_source_context(&mut parsed.links, &structural_context.context_spans);

        let mut level_zero = level_zero_heading(path, content, parsed.metadata.title.as_deref());
        level_zero.tags = file_level_tags_from_keywords(&parsed.metadata.keywords);
        populate_heading_body(document.section(), content, &mut level_zero, &[]);
        if let Some(properties) = properties_drawer_node_in_document(&document, content) {
            level_zero.properties.extend(parsed_properties_from_drawer(
                &properties,
                content,
                &lines,
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
            &lines,
            &options.todo_keywords,
            &parsed.links,
            &mut parsed.headings,
            Some(0),
        );

        Ok(parsed)
    }
}

/// Deepest heading level accepted. Orgize builds its headline tree recursively, so
/// unbounded depth overflows the stack (an abort, not a catchable error). 100 is
/// far beyond real documents and stays safe on a 2 MiB debug-build thread stack,
/// where Orgize overflows between levels 400 and 500.
pub const MAX_HEADING_LEVEL: usize = 100;

/// Cheap pre-scan for the first line starting with more than `MAX_HEADING_LEVEL`
/// asterisks followed by a space or tab. Returns `(level, 1-based line number)`.
/// Lines inside blocks are counted too; that only makes the check stricter.
fn first_heading_over_depth_limit(content: &str) -> Option<(usize, u32)> {
    content.lines().enumerate().find_map(|(index, line)| {
        let level = line.bytes().take_while(|byte| *byte == b'*').count();
        (level > MAX_HEADING_LEVEL && matches!(line.as_bytes().get(level), Some(b' ' | b'\t')))
            .then(|| (level, index as u32 + 1))
    })
}

fn parse_org_document(content: &str, todo_keywords: &TodoKeywordConfig) -> Org {
    ParseConfig {
        todo_keywords: (
            todo_keywords
                .open
                .iter()
                .map(|keyword| keyword.name.clone())
                .collect(),
            todo_keywords
                .closed
                .iter()
                .map(|keyword| keyword.name.clone())
                .collect(),
        ),
        ..ParseConfig::default()
    }
    .parse(content)
}

fn collect_link_structural_context(document: &OrgDocument) -> LinkStructuralContext {
    let mut ignored_byte_ranges = Vec::new();
    let mut context_spans = Vec::new();

    for node in document.syntax().descendants() {
        match node.kind() {
            SyntaxKind::KEYWORD
            | SyntaxKind::COMMENT
            | SyntaxKind::FIXED_WIDTH
            | SyntaxKind::CODE
            | SyntaxKind::VERBATIM
            | SyntaxKind::INLINE_SRC
            | SyntaxKind::SNIPPET => {
                push_range(&mut ignored_byte_ranges, node_byte_range(&node));
            }
            SyntaxKind::SOURCE_BLOCK => {
                if let Some(block) = SourceBlock::cast(node.clone()) {
                    push_range(&mut ignored_byte_ranges, block_byte_range(&block));
                }
            }
            SyntaxKind::COMMENT_BLOCK => {
                if let Some(block) = CommentBlock::cast(node.clone()) {
                    push_range(&mut ignored_byte_ranges, block_byte_range(&block));
                }
            }
            SyntaxKind::EXAMPLE_BLOCK => {
                if let Some(block) = ExampleBlock::cast(node.clone()) {
                    push_range(&mut ignored_byte_ranges, block_byte_range(&block));
                }
            }
            SyntaxKind::EXPORT_BLOCK => {
                if let Some(block) = ExportBlock::cast(node.clone()) {
                    push_range(&mut ignored_byte_ranges, block_byte_range(&block));
                }
            }
            SyntaxKind::PROPERTY_DRAWER => {
                if let Some(drawer) = PropertyDrawer::cast(node.clone()) {
                    push_context_span(
                        &mut context_spans,
                        block_byte_range(&drawer),
                        ParsedLinkSourceContext::PropertyDrawer,
                    );
                }
            }
            SyntaxKind::DRAWER => {
                if let Some(drawer) = Drawer::cast(node.clone()) {
                    let source_context = if drawer.name().eq_ignore_ascii_case("PROPERTIES") {
                        ParsedLinkSourceContext::PropertyDrawer
                    } else {
                        ParsedLinkSourceContext::Drawer
                    };
                    push_context_span(
                        &mut context_spans,
                        drawer.content_start().into()..drawer.content_end().into(),
                        source_context,
                    );
                }
            }
            SyntaxKind::VERSE_BLOCK => {
                if let Some(block) = VerseBlock::cast(node.clone()) {
                    push_context_span(
                        &mut context_spans,
                        block.content_start().into()..block.content_end().into(),
                        ParsedLinkSourceContext::VerseBlock,
                    );
                }
            }
            SyntaxKind::QUOTE_BLOCK => {
                if let Some(block) = QuoteBlock::cast(node.clone()) {
                    push_context_span(
                        &mut context_spans,
                        block.content_start().into()..block.content_end().into(),
                        ParsedLinkSourceContext::QuoteBlock,
                    );
                }
            }
            SyntaxKind::CENTER_BLOCK => {
                if let Some(block) = CenterBlock::cast(node.clone()) {
                    push_context_span(
                        &mut context_spans,
                        block.content_start().into()..block.content_end().into(),
                        ParsedLinkSourceContext::CenterBlock,
                    );
                }
            }
            SyntaxKind::SPECIAL_BLOCK => {
                if let Some(block) = SpecialBlock::cast(node.clone()) {
                    if special_block_is_justify(&block) {
                        push_context_span(
                            &mut context_spans,
                            block.content_start().into()..block.content_end().into(),
                            ParsedLinkSourceContext::JustifyBlock,
                        );
                    }
                }
            }
            SyntaxKind::HEADLINE_TITLE => {
                push_context_span(
                    &mut context_spans,
                    node_byte_range(&node),
                    ParsedLinkSourceContext::Heading,
                );
            }
            _ => {}
        }
    }

    LinkStructuralContext {
        ignored_byte_ranges: merge_ranges(ignored_byte_ranges),
        context_spans,
    }
}

fn annotate_links_source_context(links: &mut [ParsedLink], context_spans: &[ContextSpan]) {
    const PRIORITY: [ParsedLinkSourceContext; 7] = [
        ParsedLinkSourceContext::PropertyDrawer,
        ParsedLinkSourceContext::Drawer,
        ParsedLinkSourceContext::VerseBlock,
        ParsedLinkSourceContext::QuoteBlock,
        ParsedLinkSourceContext::CenterBlock,
        ParsedLinkSourceContext::JustifyBlock,
        ParsedLinkSourceContext::Heading,
    ];

    // Merge spans once per context kind so each link needs one binary search
    // per kind instead of a scan over every span.
    let merged_by_kind: Vec<(ParsedLinkSourceContext, Vec<Range<usize>>)> = PRIORITY
        .into_iter()
        .map(|expected| {
            let ranges = context_spans
                .iter()
                .filter(|span| span.source_context == expected)
                .map(|span| span.range.clone())
                .collect();
            (expected, merge_ranges(ranges))
        })
        .collect();

    for link in links {
        link.source_context = merged_by_kind
            .iter()
            .find(|(_, ranges)| merged_ranges_contain(ranges, link.byte_start))
            .map(|(expected, _)| expected.clone())
            .unwrap_or(ParsedLinkSourceContext::Normal);
    }
}

fn merged_ranges_contain(ranges: &[Range<usize>], offset: usize) -> bool {
    let index = ranges.partition_point(|range| range.start <= offset);
    index > 0 && offset < ranges[index - 1].end
}

fn node_byte_range(node: &SyntaxNode) -> Range<usize> {
    usize::from(node.text_range().start())..usize::from(node.text_range().end())
}

fn block_byte_range<T: AstNode>(block: &T) -> Range<usize> {
    let range = block.syntax().text_range();
    usize::from(range.start())..usize::from(range.end())
}

fn push_range(ranges: &mut Vec<Range<usize>>, range: Range<usize>) {
    if range.start < range.end {
        ranges.push(range);
    }
}

fn push_context_span(
    spans: &mut Vec<ContextSpan>,
    range: Range<usize>,
    source_context: ParsedLinkSourceContext,
) {
    if range.start < range.end {
        spans.push(ContextSpan {
            range,
            source_context,
        });
    }
}

fn merge_ranges(mut ranges: Vec<Range<usize>>) -> Vec<Range<usize>> {
    ranges.sort_by_key(|range| (range.start, range.end));

    let mut merged: Vec<Range<usize>> = Vec::new();
    for range in ranges {
        if let Some(last) = merged.last_mut() {
            if range.start <= last.end {
                last.end = last.end.max(range.end);
                continue;
            }
        }
        merged.push(range);
    }

    merged
}

fn special_block_is_justify(block: &SpecialBlock) -> bool {
    block
        .syntax()
        .children()
        .find(|node| node.kind() == SyntaxKind::BLOCK_BEGIN)
        .map(|begin| begin.to_string().trim().to_ascii_uppercase())
        .is_some_and(|begin| begin.starts_with("#+BEGIN_JUSTIFY"))
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

/// Collects headlines depth-first in document order using an explicit stack, so
/// heading depth does not grow the call stack.
#[allow(clippy::too_many_arguments)]
fn collect_headlines(
    headlines: impl Iterator<Item = Headline>,
    path: &Path,
    content: &str,
    lines: &LineIndex,
    todo_keywords: &TodoKeywordConfig,
    links: &[ParsedLink],
    output: &mut Vec<ParsedHeading>,
    parent_index: Option<usize>,
) {
    let mut stack = vec![(headlines.collect::<Vec<_>>().into_iter(), parent_index)];
    while let Some((iter, parent_index)) = stack.last_mut() {
        let parent_index = *parent_index;
        let Some(headline) = iter.next() else {
            stack.pop();
            continue;
        };
        let parsed = parse_headline(
            &headline,
            path,
            content,
            lines,
            todo_keywords,
            links,
            parent_index,
        );
        output.push(parsed);
        let current_index = output.len() - 1;
        stack.push((
            headline.headlines().collect::<Vec<_>>().into_iter(),
            Some(current_index),
        ));
    }
}

fn parse_headline(
    headline: &Headline,
    path: &Path,
    content: &str,
    lines: &LineIndex,
    todo_keywords: &TodoKeywordConfig,
    links: &[ParsedLink],
    parent_index: Option<usize>,
) -> ParsedHeading {
    let start = usize::from(headline.start());
    let end = usize::from(headline.end());

    let original_title_raw = headline.title_raw().trim_end().to_string();
    let source_title = source_title_from_content_line(content, start);
    let source_title_raw = source_title.raw;
    let (title_for_normalization, title_placeholders) =
        placeholder_bracket_links_in_title(&source_title_raw, &source_title.range, links);
    let headline_priority = headline.priority().map(|token| token.to_string());
    let parsed_priority = headline_priority
        .clone()
        .or_else(|| priority_from_source_title(&source_title_raw));
    let normalized_title = if !title_placeholders.is_empty()
        || (headline_priority.is_none() && parsed_priority.is_some())
    {
        normalize_title_from_raw(&title_for_normalization, parsed_priority.as_deref())
    } else {
        normalize_title_elements(headline.title())
    };
    let mut parsed = ParsedHeading::new(path, headline.level() as u8, normalized_title, start, end);
    parsed.title_raw = Some(source_title_raw.clone());
    parsed.priority = parsed_priority;
    parsed.todo_keyword = headline.todo_keyword().map(|token| token.to_string());
    // Orgize also accepts a tab after the keyword; Org needs a space (parser-risks.org R21).
    let keyword_bad_separator = parsed.todo_keyword.as_deref().is_some_and(|keyword| {
        source_title_raw
            .strip_prefix(keyword)
            .is_some_and(|rest| !rest.is_empty() && !rest.starts_with(' '))
    });
    if keyword_bad_separator {
        parsed.priority = None;
    }
    if keyword_bad_separator
        || parsed
            .todo_keyword
            .as_deref()
            .filter(|keyword| !todo_keyword_is_active(keyword, todo_keywords))
            .is_some()
    {
        parsed.todo_keyword = None;
        parsed.title_raw = Some(source_title_raw.clone());
        parsed.title = normalize_title_preserving_leading_keyword(
            &title_for_normalization,
            parsed.priority.as_deref(),
        );
    }
    if !title_placeholders.is_empty() {
        if let Some(keyword) = parsed.todo_keyword.as_deref() {
            let stripped_source = source_title_raw
                .strip_prefix(keyword)
                .filter(|remainder| remainder.starts_with(' '))
                .map(|remainder| remainder.trim_start_matches([' ', '\t']));
            if let Some(stripped_source) = stripped_source {
                let stripped = placeholder_title_after_todo_prefix(
                    &source_title_raw,
                    &title_for_normalization,
                    stripped_source,
                )
                .unwrap_or(stripped_source);
                parsed.title = normalize_title_from_raw(stripped, parsed.priority.as_deref());
            }
        }
    }
    if parsed.todo_keyword.is_none() {
        if source_title_raw != original_title_raw.trim() {
            parsed.title_raw = Some(source_title_raw.clone());
            parsed.title = normalize_title_preserving_leading_keyword(
                &title_for_normalization,
                parsed.priority.as_deref(),
            );
        }
        if let Some((keyword, stripped_title_raw)) =
            infer_todo_keyword(&source_title_raw, todo_keywords)
        {
            parsed.todo_keyword = Some(keyword);
            let stripped_placeholder_title = placeholder_title_after_todo_prefix(
                &source_title_raw,
                &title_for_normalization,
                &stripped_title_raw,
            )
            .unwrap_or(&stripped_title_raw);
            parsed.title =
                normalize_title_from_raw(stripped_placeholder_title, parsed.priority.as_deref());
        }
    }
    if cookie_follows_comment(&source_title_raw, parsed.todo_keyword.as_deref()) {
        // Org reads the priority cookie only before COMMENT; after it the cookie is title text.
        parsed.priority = None;
    }
    parsed.todo_type = parsed
        .todo_keyword
        .as_deref()
        .and_then(|keyword| todo_type_for_keyword(keyword, todo_keywords));
    parsed.tags = source_title_tags(content, start);
    parsed.line_number = Some(lines.line_for(start));
    parsed.parent_index = parent_index;
    parsed.is_archived = headline.is_archived();
    parsed.is_root = parent_index.is_none();

    parsed.title = restore_title_link_placeholders(parsed.title, &title_placeholders);
    let text_planning_line =
        populate_heading_timestamps(headline, content, lines, links, &mut parsed);
    let properties_drawer =
        properties_drawer_node_in_headline(headline, text_planning_line.as_ref());
    let mut excluded = text_planning_line.into_iter().collect::<Vec<_>>();
    excluded.extend(properties_drawer.as_ref().map(node_byte_range));
    populate_heading_body(headline.section(), content, &mut parsed, &excluded);

    if let Some(properties) = properties_drawer {
        parsed.properties = parsed_properties_from_drawer(
            &properties,
            content,
            lines,
            ParsedPropertySource::PropertyDrawer,
        );
    }
    parsed
}

fn level_zero_heading(path: &Path, content: &str, document_title: Option<&str>) -> ParsedHeading {
    let title = synthetic_level_zero_title(path, document_title);
    let mut heading = ParsedHeading::new(path, 0, title.clone(), 0, content.len());
    heading.title_raw = source_document_title(document_title);
    heading.line_number = Some(1);
    heading.is_root = true;
    heading
}

fn synthetic_level_zero_title(path: &Path, document_title: Option<&str>) -> String {
    if let Some(title) = source_document_title(document_title) {
        return title.to_string();
    }

    path.file_stem()
        .or_else(|| path.file_name())
        .map(|name| name.to_string_lossy().into_owned())
        .filter(|name| !name.is_empty())
        .unwrap_or_else(|| path.display().to_string())
}

fn source_document_title(document_title: Option<&str>) -> Option<String> {
    document_title
        .map(str::trim)
        .filter(|title| !title.is_empty())
        .map(str::to_string)
}

fn populate_heading_body(
    section: Option<Section>,
    content: &str,
    parsed: &mut ParsedHeading,
    excluded: &[Range<usize>],
) {
    let Some(section) = section else {
        return;
    };

    if let Some((body_text, body_byte_start, body_byte_end)) =
        filtered_section_body(&section, content, excluded)
    {
        parsed.body_text = Some(body_text);
        parsed.body_byte_start = body_byte_start;
        parsed.body_byte_end = body_byte_end;
    }
}

fn filtered_section_body(
    section: &Section,
    content: &str,
    excluded: &[Range<usize>],
) -> Option<(String, Option<usize>, Option<usize>)> {
    let children = section.syntax().children().collect::<Vec<_>>();
    let mut included_ranges = Vec::new();
    for child in children
        .iter()
        .filter(|child| !body_metadata_kind(child.kind()))
    {
        let start = usize::from(child.text_range().start());
        let end = usize::from(child.text_range().end());
        included_ranges.extend(subtract_ranges((start, end), excluded));
    }

    if included_ranges.is_empty() {
        return None;
    }

    let raw = included_ranges
        .iter()
        .map(|(start, end)| &content[*start..*end])
        .collect::<String>();
    let without_leading = raw.trim_start_matches(char::is_whitespace);
    let leading_trim = raw.len() - without_leading.len();
    let trimmed = without_leading.trim_end_matches(char::is_whitespace);
    if trimmed.is_empty() {
        return None;
    }

    let trailing_trim = without_leading.len() - trimmed.len();
    let first_start = included_ranges[0].0;
    let last_end = included_ranges[included_ranges.len() - 1].1;
    let exact_range = if content[first_start..last_end] == raw {
        Some((first_start + leading_trim, last_end - trailing_trim))
    } else {
        None
    };

    Some((
        trimmed.to_string(),
        exact_range.map(|(start, _)| start),
        exact_range.map(|(_, end)| end),
    ))
}

/// `range` without the parts covered by `excluded`, for metadata Orgize left inside body
/// nodes (a planning line kept as paragraph text, a property drawer typed as generic drawer).
fn subtract_ranges(range: (usize, usize), excluded: &[Range<usize>]) -> Vec<(usize, usize)> {
    let mut pieces = vec![range];
    for cut in excluded {
        pieces = pieces
            .into_iter()
            .flat_map(|(start, end)| {
                if start >= cut.end || cut.start >= end {
                    return vec![(start, end)];
                }
                let mut kept = Vec::new();
                if start < cut.start {
                    kept.push((start, cut.start));
                }
                if cut.end < end {
                    kept.push((cut.end, end));
                }
                kept
            })
            .collect();
    }
    pieces
}

fn body_metadata_kind(kind: SyntaxKind) -> bool {
    matches!(
        kind,
        SyntaxKind::KEYWORD | SyntaxKind::PROPERTY_DRAWER | SyntaxKind::PLANNING
    )
}

const TAIL: &str = " ORG_FILES_DB_TAIL";

fn normalize_title_from_raw(title_raw: &str, priority: Option<&str>) -> String {
    let stripped = strip_leading_priority_cookie(title_raw, priority);
    // The suffix keeps Orgize from stripping a tag-like word that Org leaves in the title.
    let parsed = Org::parse(format!("* {stripped}{TAIL}\n"));
    parsed
        .document()
        .headlines()
        .next()
        .map(|headline| normalize_title_elements(headline.title()))
        .and_then(|title| title.strip_suffix(TAIL).map(str::to_string))
        .unwrap_or_else(|| stripped.trim().to_string())
}

fn normalize_title_preserving_leading_keyword(title_raw: &str, priority: Option<&str>) -> String {
    const SENTINEL: &str = "ORG_FILES_DB_SENTINEL ";
    let stripped = strip_leading_priority_cookie(title_raw, priority);
    let parsed = Org::parse(format!("* {SENTINEL}{stripped}{TAIL}\n"));
    parsed
        .document()
        .headlines()
        .next()
        .map(|headline| normalize_title_elements(headline.title()))
        .and_then(|title| title.strip_prefix(SENTINEL).map(str::to_string))
        .and_then(|title| title.strip_suffix(TAIL).map(str::to_string))
        .unwrap_or_else(|| stripped.trim().to_string())
}

fn populate_heading_timestamps(
    headline: &Headline,
    content: &str,
    lines: &LineIndex,
    links: &[ParsedLink],
    parsed: &mut ParsedHeading,
) -> Option<Range<usize>> {
    let mut seen_ranges = HashSet::new();
    let planning_node = headline.planning();
    let mut text_planning_line = None;

    if let Some(ref planning) = planning_node {
        for child in planning.syntax().children() {
            let role = match child.kind() {
                SyntaxKind::PLANNING_SCHEDULED => ParsedTimestampRole::Scheduled,
                SyntaxKind::PLANNING_DEADLINE => ParsedTimestampRole::Deadline,
                SyntaxKind::PLANNING_CLOSED => ParsedTimestampRole::Closed,
                _ => continue,
            };
            let parsed_timestamp =
                if let Some(timestamp) = child.children().find_map(Timestamp::cast) {
                    parsed_timestamp_from_orgize(&timestamp, Some(role), lines)
                } else if let Some(timestamp) =
                    parsed_timestamp_from_planning_fallback(&child.to_string(), &child, role, lines)
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
        text_planning_line =
            populate_text_planning_fallback(content, lines, parsed, &mut seen_ranges);
    }

    if let Some(title_node) = headline
        .syntax()
        .children()
        .find(|node| node.kind() == SyntaxKind::HEADLINE_TITLE)
    {
        for timestamp in title_node.descendants().filter_map(Timestamp::cast) {
            push_body_timestamp_if_new(&timestamp, lines, links, parsed, &mut seen_ranges);
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
            push_body_timestamp_if_new(&timestamp, lines, links, parsed, &mut seen_ranges);
        }
    }
    text_planning_line
}

fn push_body_timestamp_if_new(
    timestamp: &Timestamp,
    lines: &LineIndex,
    links: &[ParsedLink],
    parsed: &mut ParsedHeading,
    seen_ranges: &mut HashSet<(usize, usize)>,
) {
    let parsed_timestamp =
        parsed_timestamp_from_orgize(timestamp, Some(ParsedTimestampRole::Body), lines);
    if link_contains_range(
        links,
        &(parsed_timestamp.byte_start..parsed_timestamp.byte_end),
    ) {
        return;
    }
    if seen_ranges.insert((parsed_timestamp.byte_start, parsed_timestamp.byte_end)) {
        parsed.timestamps.push(parsed_timestamp);
    }
}

fn parsed_timestamp_from_orgize(
    timestamp: &Timestamp,
    role: Option<ParsedTimestampRole>,
    lines: &LineIndex,
) -> ParsedTimestamp {
    let raw_value = timestamp.raw();
    let byte_start = usize::from(timestamp.start());
    let byte_end = usize::from(timestamp.end());
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

fn parsed_timestamp_from_planning_fallback(
    planning_text: &str,
    planning_node: &SyntaxNode,
    role: ParsedTimestampRole,
    lines: &LineIndex,
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
        has_time: raw_timestamp_has_explicit_time(&raw_value),
        start_ts,
        end_ts,
        byte_start,
        byte_end,
        line_number: Some(lines.line_for(byte_start)),
        modifiers: parse_timestamp_modifiers_from_raw(&raw_value)?,
    })
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

fn properties_drawer_node_in_document(document: &OrgDocument, content: &str) -> Option<SyntaxNode> {
    let candidate = document
        .properties()
        .map(|drawer| drawer.syntax().clone())
        .or_else(|| {
            let section = document.section()?;
            let first = section
                .syntax()
                .children()
                .find(|node| node.kind() != SyntaxKind::COMMENT)?;
            is_properties_drawer(&first).then_some(first)
        })?;
    // Org: only comment lines may precede the file-level drawer, not even blank lines.
    let before = &content[..usize::from(candidate.text_range().start())];
    before.lines().all(is_org_comment_line).then_some(candidate)
}

fn properties_drawer_node_in_headline(
    headline: &Headline,
    text_planning_line: Option<&Range<usize>>,
) -> Option<SyntaxNode> {
    headline
        .properties()
        .map(|drawer| drawer.syntax().clone())
        .or_else(|| {
            // Org: the drawer must directly follow the headline or its planning line, so
            // it has to be the very first element of the section (blank lines and
            // comments in front disqualify it). A planning line that Orgize left as a
            // paragraph counts as the planning line when it is that whole paragraph.
            let section = headline.section()?;
            let mut children = section.syntax().children();
            let mut first = children.next()?;
            if let Some(line) = text_planning_line {
                let range = first.text_range();
                if first.kind() == SyntaxKind::PARAGRAPH
                    && usize::from(range.start()) >= line.start
                    && usize::from(range.end()) <= line.end
                {
                    first = children.next()?;
                }
            }
            is_properties_drawer(&first).then_some(first)
        })
}

fn is_properties_drawer(node: &SyntaxNode) -> bool {
    node.kind() == SyntaxKind::PROPERTY_DRAWER
        || Drawer::cast(node.clone())
            .is_some_and(|drawer| drawer.name().eq_ignore_ascii_case("PROPERTIES"))
}

fn parsed_properties_from_drawer(
    drawer: &SyntaxNode,
    content: &str,
    lines: &LineIndex,
    source: ParsedPropertySource,
) -> Vec<ParsedProperty> {
    let mut properties = PropertyDrawer::cast(drawer.clone())
        .map(|property_drawer| {
            property_drawer
                .node_properties()
                .filter_map(|property| parsed_property_from_node(&property, content, lines, source))
                .collect::<Vec<_>>()
        })
        .unwrap_or_default();

    let seen_lines = properties
        .iter()
        .filter_map(|property| property.line_number)
        .collect::<HashSet<_>>();

    properties.extend(parsed_properties_from_drawer_fallback(
        drawer,
        content,
        lines,
        source,
        &seen_lines,
    ));
    properties.sort_by_key(|property| property.line_number.unwrap_or(0));
    properties
}

fn parsed_property_from_node(
    property: &NodeProperty,
    content: &str,
    lines: &LineIndex,
    source: ParsedPropertySource,
) -> Option<ParsedProperty> {
    let start = usize::from(property.start());
    let end = usize::from(property.end());
    let raw_line = content.get(start..end)?.trim_end_matches(['\n', '\r']);
    parsed_property_from_raw_line(
        raw_line,
        source,
        lines.line_for(usize::from(property.start())),
    )
}

fn parsed_properties_from_drawer_fallback(
    drawer: &SyntaxNode,
    content: &str,
    lines: &LineIndex,
    source: ParsedPropertySource,
    seen_lines: &HashSet<u32>,
) -> Vec<ParsedProperty> {
    let Some((start, end)) = property_drawer_content_bounds(drawer) else {
        return Vec::new();
    };
    let Some(drawer_content) = content.get(start..end) else {
        return Vec::new();
    };

    let mut properties = Vec::new();
    let mut line_start = start;
    for segment in drawer_content.split_inclusive('\n') {
        let raw_line = segment.trim_end_matches(['\n', '\r']);
        let line_number = lines.line_for(line_start);
        if !seen_lines.contains(&line_number) {
            if let Some(property) = parsed_property_from_raw_line(raw_line, source, line_number) {
                properties.push(property);
            }
        }
        line_start += segment.len();
    }
    properties
}

fn property_drawer_content_bounds(drawer: &SyntaxNode) -> Option<(usize, usize)> {
    if let Some(property_drawer) = PropertyDrawer::cast(drawer.clone()) {
        return Some((
            usize::from(property_drawer.content_start()),
            usize::from(property_drawer.content_end()),
        ));
    }

    Drawer::cast(drawer.clone()).and_then(|generic_drawer| {
        generic_drawer
            .name()
            .eq_ignore_ascii_case("PROPERTIES")
            .then_some((
                usize::from(generic_drawer.content_start()),
                usize::from(generic_drawer.content_end()),
            ))
    })
}

fn placeholder_bracket_links_in_title(
    title_raw: &str,
    title_range: &Range<usize>,
    links: &[ParsedLink],
) -> (String, Vec<(String, String)>) {
    let mut title = title_raw.to_string();
    let relevant_links = links_in_range(links, title_range);
    let mut replacements = relevant_links
        .iter()
        .filter(|link| {
            link.format == "bracket"
                && title_range.start <= link.byte_start
                && link.byte_end <= title_range.end
        })
        .enumerate()
        .map(|(index, link)| {
            let visible = link
                .raw_description
                .as_deref()
                .map(render_link_description_for_title)
                .unwrap_or_else(|| link.logical_target.clone());
            let mut token_index = index;
            let token = loop {
                let candidate = format!("ORGFILESDBLINKTOKEN{token_index}X");
                if !title_raw.contains(&candidate)
                    && !relevant_links.iter().any(|link| {
                        link.raw_description
                            .as_deref()
                            .is_some_and(|description| description.contains(&candidate))
                            || link.logical_target.contains(&candidate)
                    })
                {
                    break candidate;
                }
                token_index += links.len().max(1);
            };
            (
                link.byte_start - title_range.start..link.byte_end - title_range.start,
                token,
                visible,
            )
        })
        .collect::<Vec<_>>();
    replacements.sort_by_key(|(range, _, _)| std::cmp::Reverse(range.start));

    let mut placeholders = Vec::with_capacity(replacements.len());
    for (range, token, visible) in replacements {
        title.replace_range(range, &token);
        placeholders.push((token, visible));
    }
    (title, placeholders)
}

fn render_link_description_for_title(raw_description: &str) -> String {
    let mut rendered = raw_description.to_string();
    let mut sentinel_index = 0usize;
    let (prefix, suffix) = loop {
        let prefix = format!("ORG_FILES_DB_LINK_DESCRIPTION_PREFIX{sentinel_index}X ");
        let suffix = format!(" ORG_FILES_DB_LINK_DESCRIPTION_SUFFIX{sentinel_index}X");
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
