//! The production parse entry: `ParsedOrgDocument` from the own scanners (#103, #105).
//!
//! Headings, tree, section ranges, planning, property drawers, keywords, body text and the
//! regions that link scanning skips all come from `structure_scanner`; titles, tags, TODO
//! keywords and priorities from the raw headline line (`title`), planning entries from the
//! raw planning line (`timestamp_raw`), timestamps, inline code ranges and the visible
//! title text from `inline_scanner`. The type keeps the name of the Orgize backend it once
//! wrapped, for the public API.

use std::{collections::HashSet, ops::Range, path::Path};

use super::diagnostics::ParseDiagnostic;
use super::inline_scanner::{hides_content, normalize_title_text, scan_inline, InlineFacts};
use super::line_index::LineIndex;
use super::link_scanner::{plain_link_protocol_set, scan_links, LinkScanContext};
use super::model::{
    OrgParserCore, ParseOptions, ParsedHeading, ParsedKeyword, ParsedLink, ParsedLinkSourceContext,
    ParsedOrgDocument, ParsedTimestampRole, TodoKeywordConfig,
};
use super::properties::{file_level_properties_from_keywords, file_level_tags_from_keywords};
use super::structure_scanner::{
    parsed_keywords, scan_structure, HeadingNode, KeywordLine, RegionKind, Structure,
};
use super::timestamp_raw::planning_timestamps;
use super::title::{
    link_contains_range, source_title_from_content_line, source_title_tags, split_headline_title,
    todo_type_for_keyword,
};

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
        let structure = scan_structure(content);
        let lines = LineIndex::new(content);
        let mut parsed = ParsedOrgDocument::new(path);

        parsed.metadata.keywords = parsed_keywords(content, &structure);
        parsed.metadata.title = combined_document_title(&parsed.metadata.keywords);
        let protocols = plain_link_protocol_set(&options.link_scanner);
        let inline = scan_inline(
            content,
            &structure,
            &lines,
            &protocols,
            &options.todo_keywords,
        );
        let context = link_structural_context(content, &structure, &inline);
        parsed.links = scan_links(
            content,
            &options.link_scanner,
            &LinkScanContext {
                ignored_byte_ranges: context.ignored_byte_ranges,
            },
        );
        annotate_links_source_context(&mut parsed.links, &context.context_spans);

        let mut level_zero = level_zero_heading(path, content, parsed.metadata.title.as_deref());
        level_zero.tags = file_level_tags_from_keywords(&parsed.metadata.keywords);
        let preamble_end = structure
            .headings
            .first()
            .map_or(content.len(), |heading| heading.headline.start);
        let mut excluded: Vec<Range<usize>> = structure
            .file_properties
            .iter()
            .map(|drawer| drawer.range.clone())
            .collect();
        excluded.extend(top_level_keyword_lines(&structure, 0..preamble_end));
        populate_body(content, 0..preamble_end, &excluded, &mut level_zero);
        if let Some(drawer) = &structure.file_properties {
            level_zero.properties.extend(drawer.rows.iter().cloned());
        }
        level_zero
            .properties
            .extend(file_level_properties_from_keywords(
                &parsed.metadata.keywords,
            ));
        parsed.headings.push(level_zero);

        for node in &structure.headings {
            let heading = parse_heading(
                node,
                &HeadingSources {
                    path,
                    content,
                    lines: &lines,
                    todo_keywords: &options.todo_keywords,
                    links: &parsed.links,
                    structure: &structure,
                    inline: &inline,
                    protocols: &protocols,
                },
            );
            parsed.headings.push(heading);
        }

        Ok(parsed)
    }
}

/// Everything `parse_heading` reads besides the heading node itself.
struct HeadingSources<'a> {
    path: &'a Path,
    content: &'a str,
    lines: &'a LineIndex,
    todo_keywords: &'a TodoKeywordConfig,
    links: &'a [ParsedLink],
    structure: &'a Structure,
    inline: &'a InlineFacts,
    protocols: &'a HashSet<String>,
}

/// Link ranges to skip and the context spans that classify the rest.
fn link_structural_context(
    content: &str,
    structure: &Structure,
    inline: &InlineFacts,
) -> LinkStructuralContext {
    let mut ignored_byte_ranges = inline.ignored_ranges.clone();
    let mut context_spans = Vec::new();

    for keyword in structure.keywords.iter().chain(&structure.affiliated) {
        push_range(&mut ignored_byte_ranges, keyword.line.clone());
    }
    for heading in &structure.headings {
        if let Some(planning) = &heading.planning {
            push_range(&mut ignored_byte_ranges, planning.range.clone());
        }
        if let Some(drawer) = &heading.properties {
            push_context_span(
                &mut context_spans,
                drawer.range.clone(),
                ParsedLinkSourceContext::PropertyDrawer,
            );
        }
        let title = source_title_from_content_line(content, heading.headline.start).range;
        push_context_span(&mut context_spans, title, ParsedLinkSourceContext::Heading);
    }
    if let Some(drawer) = &structure.file_properties {
        push_context_span(
            &mut context_spans,
            drawer.range.clone(),
            ParsedLinkSourceContext::PropertyDrawer,
        );
    }
    for region in &structure.regions {
        match region.kind {
            RegionKind::Comment | RegionKind::FixedWidth => {
                push_range(&mut ignored_byte_ranges, region.range.clone());
            }
            RegionKind::DynamicBlock => {}
            RegionKind::Drawer => {
                let source_context = if region.name.eq_ignore_ascii_case("PROPERTIES") {
                    ParsedLinkSourceContext::PropertyDrawer
                } else {
                    ParsedLinkSourceContext::Drawer
                };
                push_context_span(&mut context_spans, region.content.clone(), source_context);
            }
            RegionKind::Block => {
                if hides_content(&region.name) {
                    push_range(&mut ignored_byte_ranges, region.range.clone());
                }
                let source_context = [
                    ("verse", ParsedLinkSourceContext::VerseBlock),
                    ("quote", ParsedLinkSourceContext::QuoteBlock),
                    ("center", ParsedLinkSourceContext::CenterBlock),
                    ("justify", ParsedLinkSourceContext::JustifyBlock),
                ]
                .into_iter()
                .find(|(name, _)| region.name.eq_ignore_ascii_case(name))
                .map(|(_, context)| context);
                if let Some(source_context) = source_context {
                    push_context_span(&mut context_spans, region.content.clone(), source_context);
                }
            }
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

/// Joined `#+TITLE` values in file order; empty values are skipped.
fn combined_document_title(keywords: &[ParsedKeyword]) -> Option<String> {
    let titles = keywords
        .iter()
        .filter(|keyword| keyword.key.eq_ignore_ascii_case("TITLE"))
        .filter_map(|keyword| keyword.value.as_deref())
        .collect::<Vec<_>>();
    (!titles.is_empty()).then(|| titles.join(" "))
}

fn parse_heading(node: &HeadingNode, sources: &HeadingSources) -> ParsedHeading {
    let HeadingSources {
        path,
        content,
        lines,
        todo_keywords,
        links,
        structure,
        inline,
        protocols,
    } = sources;
    let start = node.headline.start;
    let source_title = source_title_from_content_line(content, start);
    let title_raw = source_title.raw;
    let parts = split_headline_title(&title_raw, todo_keywords);
    let title = normalize_title_text(&title_raw[parts.text_start..], protocols);

    let mut parsed = ParsedHeading::new(path, node.level as u32, title, start, node.subtree.end);
    parsed.title_raw = Some(title_raw);
    parsed.todo_type = parts
        .todo_keyword
        .as_deref()
        .and_then(|keyword| todo_type_for_keyword(keyword, todo_keywords));
    parsed.todo_keyword = parts.todo_keyword;
    parsed.priority = parts.priority;
    parsed.tags = source_title_tags(content, start);
    parsed.is_archived = parsed.tags.iter().any(|tag| tag == "ARCHIVE");
    parsed.line_number = Some(node.line_number);
    parsed.parent_index = Some(node.parent.map_or(0, |parent| parent + 1));

    if let Some(planning) = &node.planning {
        for timestamp in planning_timestamps(
            &content[planning.range.clone()],
            planning.range.start,
            lines,
        ) {
            match timestamp.role {
                Some(ParsedTimestampRole::Scheduled) => {
                    parsed.planning.scheduled = Some(timestamp.clone())
                }
                Some(ParsedTimestampRole::Deadline) => {
                    parsed.planning.deadline = Some(timestamp.clone())
                }
                Some(ParsedTimestampRole::Closed) => {
                    parsed.planning.closed = Some(timestamp.clone())
                }
                _ => {}
            }
            parsed.timestamps.push(timestamp);
        }
    }
    // Title and section timestamps: the heading owns `headline.start..section.end`.
    let span = node.headline.start..node.section.end;
    let first = inline
        .timestamps
        .partition_point(|timestamp| timestamp.byte_start < span.start);
    parsed.timestamps.extend(
        inline.timestamps[first..]
            .iter()
            .take_while(|timestamp| timestamp.byte_start < span.end)
            .filter(|timestamp| {
                !link_contains_range(links, &(timestamp.byte_start..timestamp.byte_end))
            })
            .cloned(),
    );

    if let Some(drawer) = &node.properties {
        parsed.properties = drawer.rows.clone();
    }
    let mut excluded: Vec<Range<usize>> = node
        .planning
        .iter()
        .map(|planning| planning.range.start..planning.next)
        .collect();
    excluded.extend(node.properties.iter().map(|drawer| drawer.range.clone()));
    excluded.extend(top_level_keyword_lines(structure, node.section.clone()));
    populate_body(content, node.section.clone(), &excluded, &mut parsed);
    parsed
}

/// Keyword lines (also affiliated ones) in `section` that are not nested in a block or
/// drawer. They are metadata, not body text.
fn top_level_keyword_lines(
    structure: &Structure,
    section: Range<usize>,
) -> impl Iterator<Item = Range<usize>> + '_ {
    let within = move |keywords: &'_ [KeywordLine]| {
        let first = keywords.partition_point(|keyword| keyword.line.start < section.start);
        let end = keywords.partition_point(|keyword| keyword.line.start < section.end);
        (first, end)
    };
    let (keyword_first, keyword_end) = within(&structure.keywords);
    let (affiliated_first, affiliated_end) = within(&structure.affiliated);
    structure.keywords[keyword_first..keyword_end]
        .iter()
        .chain(&structure.affiliated[affiliated_first..affiliated_end])
        .filter(|keyword| keyword.top_level)
        .map(|keyword| keyword.line.start..keyword.next)
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

/// Body text of `section` without the `excluded` metadata, trimmed. The byte range is set
/// only when the kept text is one piece of the source.
fn populate_body(
    content: &str,
    section: Range<usize>,
    excluded: &[Range<usize>],
    parsed: &mut ParsedHeading,
) {
    // An element owns the blank lines after it (Org's `:post-blank`).
    let excluded = excluded
        .iter()
        .map(|range| range.start..skip_blank_lines(content, range.end, section.end))
        .collect::<Vec<_>>();
    let included_ranges = subtract_ranges((section.start, section.end), &excluded);
    if included_ranges.is_empty() {
        return;
    }
    let raw = included_ranges
        .iter()
        .map(|(start, end)| &content[*start..*end])
        .collect::<String>();
    let without_leading = raw.trim_start_matches(char::is_whitespace);
    let leading_trim = raw.len() - without_leading.len();
    let trimmed = without_leading.trim_end_matches(char::is_whitespace);
    if trimmed.is_empty() {
        return;
    }
    let trailing_trim = without_leading.len() - trimmed.len();
    let first_start = included_ranges[0].0;
    let last_end = included_ranges[included_ranges.len() - 1].1;
    parsed.body_text = Some(trimmed.to_string());
    if content[first_start..last_end] == raw {
        parsed.body_byte_start = Some(first_start + leading_trim);
        parsed.body_byte_end = Some(last_end - trailing_trim);
    }
}

/// `from`, moved over the whole blank lines (only spaces, tabs, CRs) that follow up to `limit`.
fn skip_blank_lines(content: &str, from: usize, limit: usize) -> usize {
    let mut end = from;
    while end < limit {
        let line_end = content[end..limit]
            .find('\n')
            .map_or(limit, |offset| end + offset + 1);
        if !content[end..line_end]
            .trim_matches([' ', '\t', '\r', '\n'])
            .is_empty()
        {
            break;
        }
        end = line_end;
    }
    end
}

/// `range` without the parts covered by `excluded`.
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
