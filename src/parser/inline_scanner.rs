//! Inline objects of headline titles and body text (stage 3 of #41, #105).
//!
//! The structure (headings, planning, drawers, keywords, blocks) comes from
//! `structure_scanner`. This module reads what is left, like `org-element` reads objects:
//!
//! - timestamps in headline titles and body text (`scan_inline`),
//! - inline code, verbatim, inline source and export snippets, which link scanning skips
//!   (`scan_inline`),
//! - title normalization: markup stripped, links replaced by their description
//!   (`normalize_title_text`, `render_link_description_for_title`).
//!
//! Rules and Emacs references: `docs/design/line-scanner.org` (stage 3). In short: the text
//! is cut into paragraphs, table cells and verse contents like Org cuts it; in each, a lexer
//! walks forward and tries an object at every character that can start one
//! (`org-element--object-lex`), so an object that starts earlier hides what lies inside it.
//! Emphasis follows `org-element--parse-generic-emphasis`, which has no limit on the number
//! of lines. Nothing here recurses deeper than `MAX_NESTING`, and every search is linear.

use std::{collections::HashSet, ops::Range};

use super::inline_entities::is_entity;
use super::line_index::LineIndex;
use super::line_lexer::{classify_line, lines, Line, LineClass};
use super::link_scanner::bracket_link_at;
use super::model::{ParsedTimestamp, TodoKeywordConfig};
use super::structure_scanner::{RegionKind, Structure};
use super::timestamp_raw::{body_timestamp, inline_timestamp_length};
use super::title::{source_title_from_content_line, split_headline_title};

/// Objects nest at most this deep; text inside is read as plain text.
const MAX_NESTING: usize = 48;

/// Inline facts of a document.
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
/// verbatim, snippet or inline source object. Without one there is nothing to report.
fn may_hold_inline_facts(text: &str) -> bool {
    text.bytes()
        .any(|byte| matches!(byte, b'<' | b'[' | b'=' | b'~' | b'@'))
        || text.contains("src_")
}

/// Reads the inline facts of the whole document.
pub(super) fn scan_inline(
    content: &str,
    structure: &Structure,
    index: &LineIndex,
    protocols: &HashSet<String>,
    todo_keywords: &TodoKeywordConfig,
) -> InlineFacts {
    let mut facts = InlineFacts::default();
    let body = blanked_body_text(content, structure);
    let mut lexer = Lexer::new(&body, protocols);
    for (unit, context) in body_units(&body, structure) {
        if may_hold_inline_facts(&body[unit.clone()]) {
            let nodes = lexer.parse_unit(unit, context);
            collect_facts(&body, &nodes, index, &mut facts);
        }
    }
    let mut lexer = Lexer::new(content, protocols);
    for heading in &structure.headings {
        // The title: after the stars, the TODO keyword and the priority cookie, before the
        // tags. Org parses its objects as a text of its own that starts there.
        let title = source_title_from_content_line(content, heading.headline.start);
        let mut start =
            title.range.start + split_headline_title(&title.raw, todo_keywords).text_start;
        // Org drops the word `COMMENT` from the title, with no blank needed behind it.
        if let Some(after) = content[start..title.range.end].strip_prefix("COMMENT") {
            start = title.range.end - after.trim_start_matches([' ', '\t']).len();
        }
        let unit = start..title.range.end;
        if may_hold_inline_facts(&content[unit.clone()]) {
            let nodes = lexer.parse_unit(unit, Context::Standard);
            collect_facts(content, &nodes, index, &mut facts);
        }
    }
    facts.sort();
    facts
}

fn collect_facts(text: &str, nodes: &[Node], index: &LineIndex, facts: &mut InlineFacts) {
    for node in nodes {
        match node.kind {
            Kind::Code | Kind::Verbatim | Kind::InlineSource | Kind::Snippet => {
                facts.ignored_ranges.push(node.range.clone());
            }
            Kind::Timestamp => facts.timestamps.push(body_timestamp(
                &text[node.range.clone()],
                node.range.start,
                index,
            )),
            _ => {}
        }
        collect_facts(text, &node.children, index, facts);
    }
}

// ---------------------------------------------------------------------------------------
// Paragraphs, table cells and verse contents

/// Copy of `content` with the same byte offsets in which structure is blanked (spaces) or,
/// where a line must stay text, defused. What stays is text that Org reads as paragraphs,
/// lists, tables, clocks and the content of quote, center, special, verse blocks and
/// drawers.
pub(super) fn blanked_body_text(content: &str, structure: &Structure) -> String {
    let mut buf = content.as_bytes().to_vec();
    // Lines Org reads as structure but the scanner did not: unclosed or mismatched begin
    // and end lines, keywords inside verse, stray drawer lines, and `*` followed by a
    // tab. Dots keep them text.
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

/// Width of the leading blanks of `text`, a tab reaching the next multiple of 8 (Org's
/// `current-indentation`).
fn indentation(text: &str) -> usize {
    let mut width = 0;
    for byte in text.bytes() {
        match byte {
            b' ' => width += 1,
            b'\t' => width += 8 - width % 8,
            _ => break,
        }
    }
    width
}

/// Bytes of the list bullet (`-`, `+`, `*`, `1.`, `1)`) that starts `rest`, with the blanks
/// behind it, when `rest` is a list item line.
fn list_bullet_len(rest: &str) -> Option<usize> {
    let bytes = rest.as_bytes();
    let mut end = match bytes.first()? {
        b'-' | b'+' | b'*' => 1,
        b'0'..=b'9' => {
            let digits = bytes
                .iter()
                .take_while(|byte| byte.is_ascii_digit())
                .count();
            match bytes.get(digits) {
                Some(b'.' | b')') => digits + 1,
                _ => return None,
            }
        }
        _ => return None,
    };
    match bytes.get(end) {
        None => {}
        Some(b' ' | b'\t') => {
            while matches!(bytes.get(end), Some(b' ' | b'\t')) {
                end += 1;
            }
        }
        Some(_) => return None,
    }
    Some(end)
}

/// Where the parts of a list item line begin, relative to the line without its indentation.
struct ItemParts {
    /// The tag of a description item (`- tag :: text`).
    tag: Option<Range<usize>>,
    /// First byte of the text of the item.
    content: usize,
}

/// Counter (`[@3]`), checkbox (`[X]`) and tag of the item line `rest` whose bullet and the
/// blanks behind it are `bullet` bytes long (`org-list-full-item-re`). Only `-`, `+` and
/// `*` items have tags; the tag ends at the last ` ::` that is followed by a blank or the
/// end of the line.
fn item_parts(rest: &str, bullet: usize) -> ItemParts {
    let skip_blanks = |from: usize| rest.len() - rest[from..].trim_start_matches([' ', '\t']).len();
    let mut at = bullet;
    if let Some(after) = rest[at..].strip_prefix("[@") {
        let label = after.strip_prefix("start:").unwrap_or(after);
        let length = match label.bytes().take_while(u8::is_ascii_digit).count() {
            0 => usize::from(
                label
                    .as_bytes()
                    .first()
                    .is_some_and(u8::is_ascii_alphabetic),
            ),
            digits => digits,
        };
        if length > 0 && label[length..].starts_with(']') {
            at = skip_blanks(rest.len() - label.len() + length + 1);
        }
    }
    let checkbox = &rest.as_bytes()[at..];
    if checkbox.len() >= 3
        && checkbox[0] == b'['
        && matches!(checkbox[1], b' ' | b'X' | b'-')
        && checkbox[2] == b']'
        && checkbox
            .get(3)
            .is_none_or(|byte| matches!(byte, b' ' | b'\t'))
    {
        at = skip_blanks(at + 3);
    }
    if !rest.as_bytes()[0].is_ascii_digit() {
        let body = &rest[at..];
        let tag_end = body.rmatch_indices("::").find_map(|(index, _)| {
            let blank_before = index > 0 && matches!(body.as_bytes()[index - 1], b' ' | b'\t');
            let blank_after = body
                .as_bytes()
                .get(index + 2)
                .is_none_or(|byte| matches!(byte, b' ' | b'\t'));
            (blank_before && blank_after).then_some(index)
        });
        if let Some(index) = tag_end {
            let tag = at..at + body[..index].trim_end_matches([' ', '\t']).len();
            return ItemParts {
                tag: (tag.start < tag.end).then_some(tag),
                content: skip_blanks(at + index + 2),
            };
        }
    }
    ItemParts {
        tag: None,
        content: at,
    }
}

/// `[fn:LABEL]` at the start of `text`: its length.
fn footnote_definition_len(text: &str) -> Option<usize> {
    let rest = text.strip_prefix("[fn:")?;
    let label = rest
        .char_indices()
        .take_while(|(_, c)| is_word(*c) || matches!(c, '-' | '_'))
        .last()
        .map(|(index, c)| index + c.len_utf8())?;
    rest[label..]
        .starts_with(']')
        .then_some("[fn:".len() + label + 1)
}

/// `-----` and more, then blanks only.
fn is_horizontal_rule(rest: &str) -> bool {
    let trimmed = rest.trim_end_matches([' ', '\t']);
    trimmed.len() >= 5 && trimmed.bytes().all(|byte| byte == b'-')
}

/// `+---+---+` (a table.el rule): `\+\(-+\+\)+` and blanks.
fn is_table_el_rule(rest: &str) -> bool {
    let Some(mut cells) = rest.trim_end_matches([' ', '\t']).strip_prefix('+') else {
        return false;
    };
    let mut groups = 0;
    while !cells.is_empty() {
        let dashes = cells.bytes().take_while(|byte| *byte == b'-').count();
        if dashes == 0 || cells.as_bytes().get(dashes) != Some(&b'+') {
            return false;
        }
        cells = &cells[dashes + 1..];
        groups += 1;
    }
    groups > 0
}

/// The last line of the table.el table that starts with the rule line `first`, if it is
/// one (`org-element--current-element`): the lines after the rule start with `|` or `+`
/// up to a blank line or another character, the last of them is a rule again and there is
/// more than the one rule. Otherwise the last line of that run of lines.
fn table_el_end(body: &str, all_lines: &[Line], first: usize) -> Result<usize, usize> {
    let is_table_line = |line: &Line| {
        line.text(body)
            .trim_start_matches([' ', '\t'])
            .starts_with(['|', '+'])
    };
    let mut last = first;
    while all_lines.get(last + 1).is_some_and(is_table_line) {
        last += 1;
    }
    let last_text = all_lines[last].text(body).trim_start_matches([' ', '\t']);
    if last > first && is_table_el_rule(last_text) {
        Ok(last)
    } else {
        Err(last)
    }
}

/// `\begin{NAME}`: the NAME.
fn latex_environment_name(rest: &str) -> Option<&str> {
    let name = rest.strip_prefix("\\begin{")?;
    let close = name.find('}')?;
    let name = &name[..close];
    (!name.is_empty()
        && name
            .bytes()
            .all(|byte| byte.is_ascii_alphanumeric() || byte == b'*'))
    .then_some(name)
}

/// A clock line (`org-element-clock-line-re`): `CLOCK: [ts]` with an optional `--[ts]` and
/// duration, nothing else.
fn is_clock_line(rest: &str) -> bool {
    /// `[YYYY-MM-DD` and optionally ` ...` up to `]`; what follows it.
    fn bracket(text: &str) -> Option<&str> {
        let inner = text.strip_prefix('[')?;
        let date = inner.as_bytes().get(..10)?;
        let is_date = date.iter().enumerate().all(|(index, byte)| match index {
            4 | 7 => *byte == b'-',
            _ => byte.is_ascii_digit(),
        });
        if !is_date || !matches!(inner.as_bytes().get(10), Some(b' ' | b']')) {
            return None;
        }
        let close = inner.find(']')?;
        Some(&inner[close + 1..])
    }
    let Some(after) = rest.strip_prefix("CLOCK: ").and_then(bracket) else {
        return false;
    };
    if after.trim_matches([' ', '\t']).is_empty() {
        return true;
    }
    let Some(after) = after.strip_prefix("--").and_then(bracket) else {
        return false;
    };
    let Some(duration) = after
        .strip_prefix([' ', '\t'])
        .map(|text| text.trim_start_matches([' ', '\t']))
        .and_then(|text| text.strip_prefix("=>"))
        .and_then(|text| text.strip_prefix([' ', '\t']))
    else {
        return false;
    };
    duration
        .trim_matches([' ', '\t'])
        .split_once(':')
        .is_some_and(|(hours, minutes)| {
            !hours.is_empty()
                && hours.bytes().all(|byte| byte.is_ascii_digit())
                && minutes.len() == 2
                && minutes.bytes().all(|byte| byte.is_ascii_digit())
        })
}

/// The text units of the blanked body that Org parses objects in: paragraphs (cut at
/// blank lines, list items, tables and other elements, and where a line leaves the list
/// item that holds the paragraph), table cells, verse contents and clock lines.
fn body_units(body: &str, structure: &Structure) -> Vec<(Range<usize>, Context)> {
    let mut verses: Vec<&Range<usize>> = structure
        .regions
        .iter()
        .filter(|region| region.kind == RegionKind::Block)
        .filter(|region| region.name.eq_ignore_ascii_case("verse"))
        .map(|region| &region.content)
        .collect();
    verses.sort_by_key(|range| range.start);
    let mut verses = verses.into_iter().peekable();

    let mut units = Vec::new();
    let mut paragraph: Option<Range<usize>> = None;
    // Indentation of the open list items, innermost last.
    let mut items: Vec<usize> = Vec::new();
    let mut blank_lines = 0;
    // Searches for the end of a table.el table or a LaTeX environment that failed: later
    // ones cannot succeed where an earlier one did not.
    let mut no_table_el_until = 0;
    let mut no_environment_end: HashSet<&str> = HashSet::new();
    let all_lines: Vec<_> = lines(body).collect();
    let mut index = 0;

    macro_rules! close {
        () => {
            if let Some(range) = paragraph.take() {
                units.push((range, Context::Standard));
            }
        };
    }

    while index < all_lines.len() {
        let line = all_lines[index];
        index += 1;
        while verses.peek().is_some_and(|verse| verse.end <= line.start) {
            verses.next();
        }
        if let Some(verse) = verses.peek() {
            if verse.start <= line.start && line.start < verse.end {
                close!();
                items.clear();
                units.push(((*verse).clone(), Context::Standard));
                while index < all_lines.len() && all_lines[index].start < verse.end {
                    index += 1;
                }
                continue;
            }
        }
        let text = line.text(body);
        let rest = text.trim_start_matches([' ', '\t']);
        if rest.is_empty() {
            close!();
            blank_lines += 1;
            if blank_lines >= 2 {
                items.clear();
            }
            continue;
        }
        blank_lines = 0;
        let indent = indentation(text);
        let rest_start = line.start + (text.len() - rest.len());

        let bullet = list_bullet_len(rest);
        let mut left_item = false;
        while items.last().is_some_and(|top| *top >= indent) {
            items.pop();
            left_item = true;
        }
        if bullet.is_some() {
            close!();
            items.push(indent);
        } else if left_item {
            close!();
        }

        if let Some(bullet) = bullet {
            let parts = item_parts(rest, bullet);
            if let Some(tag) = parts.tag {
                units.push((
                    rest_start + tag.start..rest_start + tag.end,
                    Context::Standard,
                ));
            }
            if rest.len() > parts.content {
                paragraph = Some(rest_start + parts.content..line.end);
            }
        } else if rest.starts_with('|') {
            close!();
            push_table_cells(rest_start, rest, &mut units);
        } else if is_table_el_rule(rest) {
            close!();
            let table_end = if index - 1 <= no_table_el_until {
                Err(no_table_el_until)
            } else {
                table_el_end(body, &all_lines, index - 1)
            };
            match table_end {
                Ok(last) => index = last + 1,
                Err(run_end) => {
                    no_table_el_until = run_end;
                    paragraph = Some(line.start..line.end);
                }
            }
        } else if is_horizontal_rule(rest) || (indent == 0 && text.starts_with("%%(")) {
            close!();
        } else if let Some(length) = (indent == 0)
            .then(|| footnote_definition_len(text))
            .flatten()
        {
            close!();
            let after = &text[length..];
            let skipped = after.len() - after.trim_start_matches([' ', '\t']).len();
            if after.len() > skipped {
                paragraph = Some(line.start + length + skipped..line.end);
            }
        } else if let Some(end) = latex_environment_name(rest)
            .filter(|name| !no_environment_end.contains(name))
            .and_then(|name| {
                let closing = format!("\\end{{{name}}}");
                let end = all_lines[index..]
                    .iter()
                    .position(|later| later.text(body).trim_matches([' ', '\t']) == closing);
                if end.is_none() {
                    no_environment_end.insert(name);
                }
                end
            })
        {
            close!();
            index += end + 1;
        } else if is_clock_line(rest) {
            close!();
            units.push((rest_start..line.end, Context::Clock));
        } else if let Some(open) = &mut paragraph {
            open.end = line.end;
        } else {
            paragraph = Some(line.start..line.end);
        }
    }
    close!();
    units
}

/// The cells of one table row (`org-element-table-row-parser`): text between the bars,
/// without the blanks around it. Rule rows (`|---+---|`) have none.
fn push_table_cells(start: usize, row: &str, units: &mut Vec<(Range<usize>, Context)>) {
    if row.starts_with("|-") {
        return;
    }
    let mut cursor = 1;
    while cursor < row.len() {
        let end = row[cursor..]
            .find('|')
            .map_or(row.len(), |bar| cursor + bar);
        let cell = &row[cursor..end];
        let trimmed = cell.trim_matches([' ', '\t']);
        if !trimmed.is_empty() {
            let offset = cursor + (cell.len() - cell.trim_start_matches([' ', '\t']).len());
            units.push((
                start + offset..start + offset + trimmed.len(),
                Context::TableCell,
            ));
        }
        cursor = end + 1;
    }
}

// ---------------------------------------------------------------------------------------
// Object lexer

/// What the objects of a container may be (`org-element-object-restrictions`).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Context {
    /// Paragraphs, verse, headline titles and the contents of emphasis, sub- and
    /// superscripts and inline footnote definitions: every object.
    Standard,
    /// A link description: no links, timestamps, footnotes or targets.
    LinkDescription,
    /// A table cell: no inline source and no statistics cookie.
    TableCell,
    /// A clock line: its timestamps.
    Clock,
    /// The contents of a radio target: emphasis, code, sub- and superscripts and LaTeX.
    Minimal,
}

#[derive(Debug, Clone, Copy)]
enum Object {
    /// Emphasis, code, verbatim, sub- and superscripts, LaTeX fragments.
    Minimal,
    Snippet,
    InlineSource,
    Macro,
    Cookie,
    Timestamp,
    Link,
    Footnote,
    Target,
}

impl Context {
    fn allows(self, object: Object) -> bool {
        use Context::{Clock, LinkDescription, Minimal, Standard, TableCell};
        match (self, object) {
            (Standard, _) => true,
            (Clock, Object::Timestamp) => true,
            (Clock, _) => false,
            (Minimal, Object::Minimal) => true,
            (Minimal, _) => false,
            (_, Object::Minimal | Object::Snippet | Object::Macro) => true,
            (LinkDescription, Object::InlineSource | Object::Cookie) => true,
            (LinkDescription, _) => false,
            (TableCell, Object::Timestamp | Object::Link | Object::Footnote | Object::Target) => {
                true
            }
            (TableCell, _) => false,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Kind {
    Bold,
    Italic,
    Underline,
    Strike,
    Code,
    Verbatim,
    Subscript,
    Superscript,
    /// Inline footnote definition `[fn:label:definition]`.
    Footnote,
    /// Bracket link `[[target][description]]`.
    Link,
    Timestamp,
    Cookie,
    Snippet,
    InlineSource,
    /// An object that stays text: angle and plain links, macros, targets, LaTeX
    /// fragments, footnote references without definition.
    Other,
}

#[derive(Debug)]
struct Node {
    kind: Kind,
    range: Range<usize>,
    /// Contents of emphasis and the like, the description of a link.
    inner: Range<usize>,
    children: Vec<Node>,
    /// Target of a bracket link without description, with its escapes resolved.
    logical_target: Option<String>,
}

impl Node {
    fn new(kind: Kind, range: Range<usize>) -> Self {
        Self {
            kind,
            inner: range.start..range.start,
            range,
            children: Vec::new(),
            logical_target: None,
        }
    }
}

/// `[[:space:]]` in an Org buffer: what `org-element` calls space around emphasis.
fn is_org_space(c: char) -> bool {
    matches!(
        c,
        '\t' | '\n' | '\u{c}' | '\r' | ' ' | '\u{a0}' | '\u{2000}'
            ..='\u{200b}' | '\u{202f}' | '\u{205f}' | '\u{3000}'
    )
}

/// Word syntax in an Org buffer: letters, digits, `$`, `%` and `'`.
fn is_word(c: char) -> bool {
    c.is_alphanumeric() || matches!(c, '$' | '%' | '\'')
}

/// Characters that may follow the opening mark's `(not space)`: before an opening mark
/// stand a space, `-`, `(`, `'`, `"` or `{`.
fn is_emphasis_pre(c: char) -> bool {
    is_org_space(c) || matches!(c, '-' | '(' | '\'' | '"' | '{')
}

/// After a closing mark stand a space, `-`, `.`, `,`, `;`, `:`, `!`, `?`, `'`, `"`, `)`,
/// `}`, `\` or `[`.
fn is_emphasis_post(c: char) -> bool {
    is_org_space(c)
        || matches!(
            c,
            '-' | '.' | ',' | ';' | ':' | '!' | '?' | '\'' | '"' | ')' | '}' | '\\' | '['
        )
}

const MARKS: [u8; 6] = [b'*', b'/', b'_', b'+', b'~', b'='];

fn mark_index(mark: u8) -> usize {
    MARKS.iter().position(|m| *m == mark).unwrap_or(0)
}

/// Bytes at which some object may start.
fn can_start_object(byte: u8) -> bool {
    byte.is_ascii_alphanumeric()
        || matches!(
            byte,
            b'_' | b'^'
                | b'*'
                | b'/'
                | b'+'
                | b'~'
                | b'='
                | b'['
                | b'<'
                | b'@'
                | b'{'
                | b'$'
                | b'\\'
        )
}

#[derive(Default)]
struct Pairs {
    /// `(open, close)` of balanced brackets, sorted by `open`.
    pairs: Vec<(usize, usize)>,
}

impl Pairs {
    fn build(text: &str, unit: &Range<usize>, open: u8, close: u8) -> Self {
        let bytes = text.as_bytes();
        let mut stack = Vec::new();
        let mut pairs = Vec::new();
        for (offset, byte) in bytes[unit.clone()].iter().enumerate() {
            if *byte == open {
                stack.push(unit.start + offset);
            } else if *byte == close {
                if let Some(start) = stack.pop() {
                    pairs.push((start, unit.start + offset));
                }
            }
        }
        pairs.sort_unstable();
        Self { pairs }
    }

    fn partner(&self, open: usize) -> Option<usize> {
        self.pairs
            .binary_search_by_key(&open, |pair| pair.0)
            .ok()
            .map(|index| self.pairs[index].1)
    }
}

/// What a forward search looks for. The offsets of every match in the unit are collected
/// once per unit (`Lexer::first_at`), so that searches that fail over and over, like
/// thousands of `<2024-04-01` without a `>`, cost a lookup each and the scan stays linear.
#[derive(Debug, Clone, Copy)]
enum Needle {
    /// `]`, `>`, `\r` or `\n`: where a bracket timestamp ends or breaks.
    TimestampEnd,
    /// `>` or `\n`: where a diary sexp or an angle link ends or breaks.
    AngleEnd,
    /// A blank, line break, `[` or `{`: the end of the language of an inline source block.
    LanguageEnd,
    /// `)}}}`: the end of a macro with arguments.
    MacroEnd,
    /// `@@`: the end of an export snippet.
    Snippet,
    /// `$$`.
    DoubleDollar,
    /// `$`.
    Dollar,
    /// `\)`.
    LatexParen,
    /// `\]`.
    LatexBracket,
}

impl Needle {
    fn length(self) -> usize {
        match self {
            Needle::MacroEnd => 4,
            Needle::Snippet | Needle::DoubleDollar | Needle::LatexParen | Needle::LatexBracket => 2,
            _ => 1,
        }
    }

    fn matches(self, rest: &[u8]) -> bool {
        match self {
            Needle::TimestampEnd => matches!(rest[0], b']' | b'>' | b'\r' | b'\n'),
            Needle::AngleEnd => matches!(rest[0], b'>' | b'\n'),
            Needle::LanguageEnd => matches!(rest[0], b' ' | b'\t' | b'\n' | b'[' | b'{'),
            Needle::MacroEnd => rest.starts_with(b")}}}"),
            Needle::Snippet => rest.starts_with(b"@@"),
            Needle::DoubleDollar => rest.starts_with(b"$$"),
            Needle::Dollar => rest[0] == b'$',
            Needle::LatexParen => rest.starts_with(b"\\)"),
            Needle::LatexBracket => rest.starts_with(b"\\]"),
        }
    }
}

struct Lexer<'a> {
    text: &'a str,
    bytes: &'a [u8],
    protocols: &'a HashSet<String>,
    /// Length of the longest protocol.
    longest_protocol: usize,
    /// Per needle: sorted offsets of its matches in the unit.
    found: [Option<Vec<usize>>; 9],
    /// Line breaks in angle links that no `>` follows, so that a link that starts earlier
    /// fails at once when it reaches one.
    dead_breaks: HashSet<usize>,
    /// The unit (paragraph, cell, title) that is being read; all regions lie inside it.
    unit: Range<usize>,
    /// Per emphasis mark: sorted offsets of marks that can close an emphasis.
    closers: [Option<Vec<usize>>; 6],
    square: Option<Pairs>,
    curly: Option<Pairs>,
}

impl<'a> Lexer<'a> {
    fn new(text: &'a str, protocols: &'a HashSet<String>) -> Self {
        Self {
            text,
            bytes: text.as_bytes(),
            protocols,
            longest_protocol: protocols.iter().map(String::len).max().unwrap_or(0),
            found: Default::default(),
            dead_breaks: HashSet::new(),
            unit: 0..0,
            closers: Default::default(),
            square: None,
            curly: None,
        }
    }

    /// First match of `needle` that starts at or after `from` and ends before `limit`.
    fn first_at(&mut self, needle: Needle, from: usize, limit: usize) -> Option<usize> {
        let slot = needle as usize;
        if self.found[slot].is_none() {
            let unit = self.unit.clone();
            let found = (unit.start..unit.end)
                .filter(|index| {
                    unit.end - index >= needle.length()
                        && needle.matches(&self.bytes[*index..unit.end])
                })
                .collect();
            self.found[slot] = Some(found);
        }
        let found = self.found[slot].as_deref()?;
        let first = found.partition_point(|index| *index < from);
        found
            .get(first)
            .copied()
            .filter(|index| index + needle.length() <= limit)
    }

    fn parse_unit(&mut self, unit: Range<usize>, context: Context) -> Vec<Node> {
        self.unit = unit.clone();
        self.closers = Default::default();
        self.found = Default::default();
        self.dead_breaks.clear();
        self.square = None;
        self.curly = None;
        self.parse_region(unit, context, 0)
    }

    fn char_before(&self, pos: usize) -> Option<char> {
        self.text[..pos].chars().next_back()
    }

    fn char_at(&self, pos: usize) -> Option<char> {
        self.text.get(pos..)?.chars().next()
    }

    fn parse_region(&mut self, region: Range<usize>, context: Context, depth: usize) -> Vec<Node> {
        let mut nodes = Vec::new();
        let mut pos = region.start;
        while pos < region.end {
            if can_start_object(self.bytes[pos]) {
                if let Some(node) = self.lex_at(pos, &region, context, depth) {
                    pos = node.range.end.max(pos + 1);
                    nodes.push(node);
                    continue;
                }
            }
            pos += 1;
        }
        nodes
    }

    /// Parses the contents of a container, unless objects nest too deep.
    fn contents(&mut self, inner: Range<usize>, context: Context, depth: usize) -> Vec<Node> {
        if depth + 1 >= MAX_NESTING {
            return Vec::new();
        }
        self.parse_region(inner, context, depth + 1)
    }

    fn lex_at(
        &mut self,
        pos: usize,
        region: &Range<usize>,
        context: Context,
        depth: usize,
    ) -> Option<Node> {
        let byte = self.bytes[pos];
        match byte {
            b'_' | b'^' => {
                if context.allows(Object::Minimal) {
                    if let Some(node) = self.script(pos, region, context, depth) {
                        return Some(node);
                    }
                }
                if byte == b'_' {
                    return self.emphasis(pos, region, context, depth);
                }
                None
            }
            b'*' | b'/' | b'+' | b'~' | b'=' => self.emphasis(pos, region, context, depth),
            b'[' => self.bracket_object(pos, region, context, depth),
            b'<' => self.angle_object(pos, region, context, depth),
            b'@' => self.snippet(pos, region, context),
            b'{' => self.macro_call(pos, region, context),
            b'$' | b'\\' => self.latex_fragment(pos, region, context),
            _ => {
                if byte == b's'
                    && self.text[pos..region.end].starts_with("src_")
                    && context.allows(Object::InlineSource)
                {
                    if let Some(node) = self.inline_source(pos, region) {
                        return Some(node);
                    }
                }
                self.plain_link(pos, region, context)
            }
        }
    }

    // --- emphasis ---

    fn emphasis(
        &mut self,
        pos: usize,
        region: &Range<usize>,
        context: Context,
        depth: usize,
    ) -> Option<Node> {
        if !context.allows(Object::Minimal) {
            return None;
        }
        let mark = self.bytes[pos];
        // `(not space)` after the mark, a space, `-`, `(`, `'`, `"`, `{` or a line start
        // before it.
        let next = self.char_at(pos + 1).filter(|_| pos + 1 < region.end)?;
        if is_org_space(next) {
            return None;
        }
        let at_line_start = pos == region.start || self.bytes[pos - 1] == b'\n';
        if !at_line_start && !self.char_before(pos).is_some_and(is_emphasis_pre) {
            return None;
        }
        let closing = self.find_closing(mark, pos, region.end)?;
        let inner = pos + 1..closing;
        let range = pos..closing + 1;
        let kind = match mark {
            b'*' => Kind::Bold,
            b'/' => Kind::Italic,
            b'_' => Kind::Underline,
            b'+' => Kind::Strike,
            b'~' => Kind::Code,
            _ => Kind::Verbatim,
        };
        let mut node = Node::new(kind, range);
        node.inner = inner.clone();
        if !matches!(kind, Kind::Code | Kind::Verbatim) {
            node.children = self.contents(inner, Context::Standard, depth);
        }
        Some(node)
    }

    /// First mark at or after `origin + 2` that has a non-space before it and a space or
    /// one of `-.,;:!?'")}\[` behind it or the end of the region, and ends before `limit`.
    fn find_closing(&mut self, mark: u8, origin: usize, limit: usize) -> Option<usize> {
        let slot = mark_index(mark);
        if self.closers[slot].is_none() {
            let mut found = Vec::new();
            let unit = self.unit.clone();
            for index in unit.start + 1..unit.end {
                if self.bytes[index] == mark
                    && self.closer_follows(index, unit.end)
                    && self.char_before(index).is_some_and(|c| !is_org_space(c))
                {
                    found.push(index);
                }
            }
            self.closers[slot] = Some(found);
        }
        let from = origin + 2;
        let found = self.closers[slot].as_deref().unwrap_or(&[]);
        let first = found.partition_point(|index| *index < from);
        if let Some(index) = found.get(first).copied().filter(|index| *index < limit) {
            return Some(index);
        }
        // The region ends right behind the mark.
        let last = limit.checked_sub(1)?;
        (last >= from
            && self.bytes[last] == mark
            && self.char_before(last).is_some_and(|c| !is_org_space(c)))
        .then_some(last)
    }

    fn closer_follows(&self, index: usize, end: usize) -> bool {
        index + 1 >= end || self.char_at(index + 1).is_some_and(is_emphasis_post)
    }

    // --- subscripts and superscripts ---

    fn script(
        &mut self,
        pos: usize,
        region: &Range<usize>,
        _context: Context,
        depth: usize,
    ) -> Option<Node> {
        // The lexer only tries after `-`, `{`, `(`, `*`, `+`, `.`, `,` or a letter/digit.
        let next = self.char_at(pos + 1).filter(|_| pos + 1 < region.end)?;
        if !(matches!(next, '-' | '{' | '(' | '*' | '+' | '.' | ',') || next.is_alphanumeric()) {
            return None;
        }
        // `\S-` before the mark, and not at the start of a line.
        if pos == region.start || self.bytes[pos - 1] == b'\n' {
            return None;
        }
        if self.char_before(pos).is_none_or(is_org_space) {
            return None;
        }
        let kind = if self.bytes[pos] == b'_' {
            Kind::Subscript
        } else {
            Kind::Superscript
        };
        let script = pos + 1;
        let (end, inner) = match next {
            '{' => {
                let close = self.balanced(b'{', script, region.end)?;
                (close + 1, script + 1..close)
            }
            '(' => {
                let close = self.balanced(b'(', script, region.end)?;
                (close + 1, script..close + 1)
            }
            _ => {
                let end = self.simple_script_end(script, region.end)?;
                (end, script..script)
            }
        };
        let mut node = Node::new(kind, pos..end);
        node.inner = inner.clone();
        if next == '{' || next == '(' {
            node.children = self.contents(inner, Context::Standard, depth);
        }
        Some(node)
    }

    /// `*` or `[+-]?[[:alnum:].,\\]*[[:alnum:]]` at `start`: the end.
    fn simple_script_end(&self, start: usize, limit: usize) -> Option<usize> {
        let text = &self.text[start..limit];
        if text.starts_with('*') {
            return Some(start + 1);
        }
        let body = text.strip_prefix(['+', '-']).unwrap_or(text);
        let skipped = text.len() - body.len();
        let run = body
            .char_indices()
            .take_while(|(_, c)| c.is_alphanumeric() || matches!(c, '.' | ',' | '\\'))
            .last()
            .map(|(index, c)| index + c.len_utf8())?;
        let trimmed = body[..run].trim_end_matches(|c: char| !c.is_alphanumeric());
        (!trimmed.is_empty()).then_some(start + skipped + trimmed.len())
    }

    /// Index of the bracket that closes the `open` bracket at `start` within `limit`,
    /// with at most three stacked brackets inside (`org-match-sexp-depth`).
    fn balanced(&self, open: u8, start: usize, limit: usize) -> Option<usize> {
        let close = if open == b'{' { b'}' } else { b')' };
        let mut depth = 0usize;
        for index in start..limit {
            let byte = self.bytes[index];
            if byte == open {
                depth += 1;
                if depth > 3 {
                    return None;
                }
            } else if byte == close {
                depth -= 1;
                if depth == 0 {
                    return Some(index);
                }
            }
        }
        None
    }

    // --- objects that start with `[` ---

    fn bracket_object(
        &mut self,
        pos: usize,
        region: &Range<usize>,
        context: Context,
        depth: usize,
    ) -> Option<Node> {
        let rest = &self.text[pos..region.end];
        let second = *rest.as_bytes().get(1)?;
        if second == b'[' {
            if context.allows(Object::Link) {
                return self.bracket_link(pos, region, depth);
            }
            return None;
        }
        if rest.starts_with("[fn:") {
            if context.allows(Object::Footnote) {
                return self.footnote(pos, region, depth);
            }
            return None;
        }
        if rest.starts_with("[cite:") || rest.starts_with("[cite/") {
            // Citations are not read; their text stays text.
            return None;
        }
        let cookie_first = matches!(second, b'%' | b'/') && context.allows(Object::Cookie);
        if cookie_first || second.is_ascii_digit() {
            if !cookie_first {
                if let Some(node) = self.timestamp(pos, region, context) {
                    return Some(node);
                }
            }
            if context.allows(Object::Cookie) {
                return statistics_cookie(rest).map(|len| Node::new(Kind::Cookie, pos..pos + len));
            }
            return None;
        }
        None
    }

    fn timestamp(&mut self, pos: usize, region: &Range<usize>, context: Context) -> Option<Node> {
        if !context.allows(Object::Timestamp) {
            return None;
        }
        // Rule out the timestamps that break at a line end or lack a closing bracket, so
        // that the length below is only computed for a timestamp that will be one.
        if self.text[pos..region.end].starts_with("<%%(") {
            let end = self.first_at(Needle::AngleEnd, pos + 4, region.end)?;
            if self.bytes[end] != b'>' || self.bytes[end - 1] != b')' || end == pos + 4 {
                return None;
            }
        } else {
            let end = self.first_at(Needle::TimestampEnd, pos + 1, region.end)?;
            if !matches!(self.bytes[end], b']' | b'>') {
                return None;
            }
        }
        let length = inline_timestamp_length(&self.text[pos..region.end])?;
        Some(Node::new(Kind::Timestamp, pos..pos + length))
    }

    fn bracket_link(&mut self, pos: usize, region: &Range<usize>, depth: usize) -> Option<Node> {
        let link = bracket_link_at(self.text, pos, self.protocols)?;
        if link.end > region.end {
            return None;
        }
        let mut node = Node::new(Kind::Link, pos..link.end);
        match link.description {
            Some(description) => {
                node.inner = description.clone();
                node.children = self.contents(description, Context::LinkDescription, depth);
            }
            None => node.logical_target = Some(link.logical_target),
        }
        Some(node)
    }

    fn footnote(&mut self, pos: usize, region: &Range<usize>, depth: usize) -> Option<Node> {
        let after = pos + "[fn:".len();
        let label_end = self.text[after..region.end]
            .char_indices()
            .find(|(_, c)| !(is_word(*c) || matches!(c, '-' | '_')))
            .map_or(region.end, |(index, _)| after + index);
        let inline = match self.bytes.get(label_end) {
            Some(b':') if label_end < region.end => true,
            Some(b']') if label_end > after && label_end < region.end => false,
            _ => return None,
        };
        let close = self
            .square_partner(pos)
            .filter(|close| *close < region.end)?;
        if !inline {
            return Some(Node::new(Kind::Other, pos..close + 1));
        }
        let mut node = Node::new(Kind::Footnote, pos..close + 1);
        let inner = label_end + 1..close;
        node.inner = inner.clone();
        node.children = self.contents(inner, Context::Standard, depth);
        Some(node)
    }

    fn square_partner(&mut self, open: usize) -> Option<usize> {
        let (text, unit) = (self.text, self.unit.clone());
        self.square
            .get_or_insert_with(|| Pairs::build(text, &unit, b'[', b']'))
            .partner(open)
    }

    fn curly_partner(&mut self, open: usize) -> Option<usize> {
        let (text, unit) = (self.text, self.unit.clone());
        self.curly
            .get_or_insert_with(|| Pairs::build(text, &unit, b'{', b'}'))
            .partner(open)
    }

    // --- objects that start with `<` ---

    fn angle_object(
        &mut self,
        pos: usize,
        region: &Range<usize>,
        context: Context,
        depth: usize,
    ) -> Option<Node> {
        let rest = &self.text[pos..region.end];
        if rest.starts_with("<<") {
            if !context.allows(Object::Target) {
                return None;
            }
            let (length, open) = target(rest)?;
            let mut node = Node::new(Kind::Other, pos..pos + length);
            if open == 3 {
                // The contents of a radio target hold emphasis, code and the like.
                let inner = pos + open..pos + length - open;
                node.children = self.contents(inner.clone(), Context::Minimal, depth);
                node.inner = inner;
            }
            return Some(node);
        }
        if let Some(node) = self.timestamp(pos, region, context) {
            return Some(node);
        }
        if !context.allows(Object::Link) {
            return None;
        }
        let end = self.angle_link_end(pos, region.end)?;
        Some(Node::new(Kind::Other, pos..end))
    }

    /// End of the angle link `<type:path>` at `pos`. As in `org-link-angle-re` the path may
    /// run over lines: a line break must be followed by blanks and a character other than
    /// `>`.
    fn angle_link_end(&mut self, pos: usize, limit: usize) -> Option<usize> {
        let protocol = link_protocol(
            &self.text[pos + 1..limit],
            self.protocols,
            self.longest_protocol,
        )?;
        let mut cursor = pos + 1 + protocol.len() + 1;
        let mut visited = Vec::new();
        let shared = limit == self.unit.end;
        let end = loop {
            let Some(at) = self.first_at(Needle::AngleEnd, cursor, limit) else {
                break None;
            };
            if self.bytes[at] == b'>' {
                return Some(at + 1);
            }
            if self.dead_breaks.contains(&at) {
                break None;
            }
            let next = self.text[at + 1..limit].trim_start_matches([' ', '\t']);
            visited.push(at);
            if next.is_empty() || next.starts_with(['>', '\n']) {
                break None;
            }
            cursor = limit - next.len();
        };
        if shared {
            self.dead_breaks.extend(visited);
        }
        end
    }

    // --- plain links ---

    fn plain_link(&self, pos: usize, region: &Range<usize>, context: Context) -> Option<Node> {
        if !context.allows(Object::Link) || !self.bytes[pos].is_ascii_alphanumeric() {
            return None;
        }
        // `\<`: no word character before.
        if pos > region.start && self.char_before(pos).is_some_and(is_word) {
            return None;
        }
        let length = plain_link_len(
            &self.text[pos..region.end],
            self.protocols,
            self.longest_protocol,
        )?;
        Some(Node::new(Kind::Other, pos..pos + length))
    }

    // --- export snippets, macros, inline source blocks, LaTeX fragments ---

    fn snippet(&mut self, pos: usize, region: &Range<usize>, context: Context) -> Option<Node> {
        if !context.allows(Object::Snippet) {
            return None;
        }
        let rest = &self.text[pos..region.end];
        let name = rest.strip_prefix("@@")?;
        let length = name
            .bytes()
            .take_while(|byte| byte.is_ascii_alphanumeric() || *byte == b'-')
            .count();
        if length == 0 || name.as_bytes().get(length) != Some(&b':') {
            return None;
        }
        let close = self.first_at(Needle::Snippet, pos + 2 + length + 1, region.end)?;
        Some(Node::new(Kind::Snippet, pos..close + 2))
    }

    fn macro_call(&mut self, pos: usize, region: &Range<usize>, context: Context) -> Option<Node> {
        if !context.allows(Object::Macro) {
            return None;
        }
        let rest = self.text[pos..region.end].strip_prefix("{{{")?;
        let name = rest.as_bytes();
        if !name.first()?.is_ascii_alphabetic() {
            return None;
        }
        let length = name
            .iter()
            .take_while(|byte| byte.is_ascii_alphanumeric() || matches!(byte, b'-' | b'_'))
            .count();
        let after = &rest[length..];
        let end = if after.starts_with('(') {
            self.first_at(Needle::MacroEnd, pos + 3 + length + 1, region.end)? + ")}}}".len()
        } else if after.starts_with("}}}") {
            pos + 3 + length + 3
        } else {
            return None;
        };
        Some(Node::new(Kind::Other, pos..end))
    }

    fn inline_source(&mut self, pos: usize, region: &Range<usize>) -> Option<Node> {
        // `\<src_`: no word character before.
        if pos > region.start && self.char_before(pos).is_some_and(is_word) {
            return None;
        }
        let language = pos + "src_".len();
        let end = self.first_at(Needle::LanguageEnd, language, region.end)?;
        if end == language {
            return None;
        }
        let mut cursor = end;
        if self.bytes[cursor] == b'[' {
            let close = self.square_partner(cursor).filter(|c| *c < region.end)?;
            cursor = close + 1;
        }
        if self.bytes.get(cursor) != Some(&b'{') || cursor >= region.end {
            return None;
        }
        let close = self.curly_partner(cursor).filter(|c| *c < region.end)?;
        Some(Node::new(Kind::InlineSource, pos..close + 1))
    }

    fn latex_fragment(
        &mut self,
        pos: usize,
        region: &Range<usize>,
        context: Context,
    ) -> Option<Node> {
        if !context.allows(Object::Minimal) {
            return None;
        }
        let rest = &self.text[pos..region.end];
        let end = if rest.starts_with("$$") {
            self.first_at(Needle::DoubleDollar, pos + 2, region.end)? + 2
        } else if rest.starts_with('$') {
            self.dollar_fragment(pos, region)?
        } else if rest.starts_with("\\(") {
            self.first_at(Needle::LatexParen, pos + 2, region.end)? + 2
        } else if rest.starts_with("\\[") {
            self.first_at(Needle::LatexBracket, pos + 2, region.end)? + 2
        } else {
            pos + entity_len(rest).or_else(|| latex_macro_len(rest))?
        };
        Some(Node::new(Kind::Other, pos..end))
    }

    /// `$x$` as `org-element-latex-fragment-parser` reads it: no `$` before, no blank or
    /// `,.;` after the first, no blank or `,.` before the second, and a space, punctuation,
    /// quote, bracket or the end of the line behind it.
    fn dollar_fragment(&mut self, pos: usize, region: &Range<usize>) -> Option<usize> {
        if pos > region.start && self.bytes[pos - 1] == b'$' {
            return None;
        }
        let first = self.char_at(pos + 1).filter(|_| pos + 1 < region.end);
        if first.is_some_and(|c| matches!(c, ' ' | '\t' | '\n' | ',' | '.' | ';')) {
            return None;
        }
        let close = self.first_at(Needle::Dollar, pos + 1, region.end)?;
        if self
            .char_before(close)
            .is_some_and(|c| matches!(c, ' ' | '\t' | '\n' | ',' | '.'))
        {
            return None;
        }
        let behind = self.char_at(close + 1).filter(|_| close + 1 < region.end);
        let fits = behind.is_none_or(|c| {
            c == '\n'
                || is_org_space(c)
                || if c.is_ascii() {
                    matches!(
                        c,
                        '!' | '"'
                            | '#'
                            | '\''
                            | '('
                            | ')'
                            | ','
                            | '.'
                            | ':'
                            | ';'
                            | '<'
                            | '>'
                            | '?'
                            | '@'
                            | '['
                            | ']'
                            | '^'
                            | '`'
                            | '{'
                            | '}'
                    )
                } else {
                    !c.is_alphanumeric()
                }
        });
        fits.then_some(close + 1)
    }
}

/// The protocol of a link at the start of `text` (`http` of `http:x`), lowercased, when it
/// is one of `protocols`.
fn link_protocol<'t>(
    text: &'t str,
    protocols: &HashSet<String>,
    longest: usize,
) -> Option<&'t str> {
    let length = text
        .bytes()
        .take(longest + 1)
        .position(|byte| !(byte.is_ascii_alphanumeric() || matches!(byte, b'+' | b'-' | b'.')))?;
    (text.as_bytes()[length] == b':'
        && length > 0
        && protocols.contains(&text[..length].to_ascii_lowercase()))
    .then_some(&text[..length])
}

/// A plain link at the start of `text` as `org-link-plain-re` (Org 9.6) reads it: the
/// protocol, `:`, and a path of characters other than blanks, brackets and angle brackets,
/// with parentheses only in pairs (two levels deep). The path has at least two items and
/// ends in a letter, digit, `/` or a parenthesized group.
fn plain_link_len(text: &str, protocols: &HashSet<String>, longest: usize) -> Option<usize> {
    let protocol = link_protocol(text, protocols, longest)?;
    let start = protocol.len() + 1;
    let mut cursor = start;
    // End offset and whether it may end the link, per item.
    let mut items: Vec<(usize, bool)> = Vec::new();
    while let Some(c) = text[cursor..].chars().next() {
        let (end, may_end) = match c {
            '(' => match paren_group_len(&text[cursor..]) {
                Some(length) => (cursor + length, true),
                None => break,
            },
            '[' | ']' | ')' | '<' | '>' | ' ' | '\t' | '\n' => break,
            _ => {
                let end = cursor + c.len_utf8();
                (end, c.is_alphanumeric() || c == '/')
            }
        };
        items.push((end, may_end));
        cursor = end;
    }
    let last = items
        .iter()
        .enumerate()
        .rev()
        .find(|(index, (_, may_end))| *index >= 1 && *may_end)?;
    Some((last.1).0)
}

/// `(...)` at the start of `text`, with at most one more level of parentheses and no
/// blanks or brackets inside: its length.
fn paren_group_len(text: &str) -> Option<usize> {
    let mut depth = 0;
    for (index, c) in text.char_indices() {
        match c {
            '(' => {
                depth += 1;
                if depth > 2 {
                    return None;
                }
            }
            ')' => {
                depth -= 1;
                if depth == 0 {
                    return Some(index + 1);
                }
            }
            '[' | ']' | '<' | '>' | ' ' | '\t' | '\n' => return None,
            _ => {}
        }
    }
    None
}

/// `[N%]`, `[N/M]` (digits optional) at the start of `text`: its length.
fn statistics_cookie(text: &str) -> Option<usize> {
    let inner = text.strip_prefix('[')?;
    let digits = inner.bytes().take_while(u8::is_ascii_digit).count();
    let rest = &inner[digits..];
    let tail = if rest.starts_with('%') {
        1
    } else if let Some(denominator) = rest.strip_prefix('/') {
        1 + denominator.bytes().take_while(u8::is_ascii_digit).count()
    } else {
        return None;
    };
    (rest.as_bytes().get(tail) == Some(&b']')).then_some(1 + digits + tail + 1)
}

/// `<<target>>` and `<<<radio target>>>` at the start of `text`: its length and the number
/// of brackets on each side. The target has no `<`, `>` or line break and neither starts
/// nor ends with a blank.
fn target(text: &str) -> Option<(usize, usize)> {
    let radio = text.starts_with("<<<");
    let open = if radio { 3 } else { 2 };
    let rest = text.get(open..)?;
    let end = rest.find(['<', '>', '\n'])?;
    let inside = &rest[..end];
    if inside.is_empty()
        || inside.starts_with([' ', '\t'])
        || inside.ends_with([' ', '\t'])
        || !rest[end..].starts_with(if radio { ">>>" } else { ">>" })
    {
        return None;
    }
    Some((open + end + open, open))
}

/// `\name` or `\name{}` of an entity at the start of `text` (`org-element-entity-parser`):
/// its length. The name is followed by a non-letter or the end of the text.
fn entity_len(text: &str) -> Option<usize> {
    let after = text.strip_prefix('\\')?;
    let special = [
        "there4", "sup1", "sup2", "sup3", "frac12", "frac14", "frac32", "frac34",
    ]
    .into_iter()
    .find(|name| after.starts_with(name));
    let name = special.map_or_else(
        || &after[..after.bytes().take_while(u8::is_ascii_alphabetic).count()],
        |name| &after[..name.len()],
    );
    let follow = &after[name.len()..];
    if name.is_empty() || follow.starts_with(|c: char| c.is_alphabetic()) || !is_entity(name) {
        return None;
    }
    Some(1 + name.len() + if follow.starts_with("{}") { 2 } else { 0 })
}

/// `\name`, optionally followed by `[...]` and `{...}` arguments without nesting or line
/// breaks, at the start of `text`: its length.
fn latex_macro_len(text: &str) -> Option<usize> {
    let rest = text.strip_prefix('\\')?;
    let name = rest.bytes().take_while(u8::is_ascii_alphabetic).count();
    if name == 0 {
        return None;
    }
    let mut end = 1 + name;
    if text[end..].starts_with('*') {
        end += 1;
    }
    loop {
        let rest = &text[end..];
        let (open, close, forbidden): (char, char, &[char]) = match rest.chars().next() {
            Some('[') => ('[', ']', &['[', ']', '\n', '{', '}']),
            Some('{') => ('{', '}', &['{', '}', '\n']),
            _ => return Some(end),
        };
        let inside = &rest[open.len_utf8()..];
        match inside.find(|c| forbidden.contains(&c) || c == close) {
            Some(index) if inside[index..].starts_with(close) => {
                end += open.len_utf8() + index + close.len_utf8();
            }
            _ => return Some(end),
        }
    }
}

// ---------------------------------------------------------------------------------------
// Title normalization

/// Org's title blanks are spaces and tabs; other Unicode whitespace such as NBSP is text.
fn trim_org_blanks(text: &str) -> String {
    text.trim_matches([' ', '\t']).to_string()
}

/// Visible text of a headline title: markup characters dropped, bracket links replaced by
/// their description or path, statistics cookies removed. `text` must be the title
/// without TODO keyword and priority cookie.
pub(super) fn normalize_title_text(text: &str, protocols: &HashSet<String>) -> String {
    if !text
        .bytes()
        .any(|byte| matches!(byte, b'*' | b'/' | b'_' | b'=' | b'~' | b'['))
    {
        return trim_org_blanks(text);
    }
    trim_org_blanks(&visible_text(text, Context::Standard, protocols))
}

/// `text` without markup characters, links replaced, cookies dropped.
fn visible_text(text: &str, context: Context, protocols: &HashSet<String>) -> String {
    let mut lexer = Lexer::new(text, protocols);
    let nodes = lexer.parse_unit(0..text.len(), context);
    let mut out = String::with_capacity(text.len());
    render(text, 0..text.len(), &nodes, &mut out);
    out
}

fn render(text: &str, range: Range<usize>, nodes: &[Node], out: &mut String) {
    let mut cursor = range.start;
    for node in nodes {
        out.push_str(&text[cursor..node.range.start]);
        cursor = node.range.end;
        match node.kind {
            Kind::Bold | Kind::Italic | Kind::Underline => {
                render(text, node.inner.clone(), &node.children, out);
            }
            Kind::Code | Kind::Verbatim => out.push_str(&text[node.inner.clone()]),
            Kind::Cookie => {}
            Kind::Link if node.inner.start < node.inner.end => {
                render(text, node.inner.clone(), &node.children, out);
            }
            Kind::Link => out.push_str(node.logical_target.as_deref().unwrap_or_default()),
            Kind::Strike | Kind::Subscript | Kind::Superscript | Kind::Footnote => {
                out.push_str(&text[node.range.start..node.inner.start]);
                render(text, node.inner.clone(), &node.children, out);
                out.push_str(&text[node.inner.end..node.range.end]);
            }
            Kind::Timestamp | Kind::Snippet | Kind::InlineSource | Kind::Other => {
                out.push_str(&text[node.range.clone()]);
            }
        }
    }
    out.push_str(&text[cursor..range.end]);
}
