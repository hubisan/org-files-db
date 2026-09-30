//! Stage B of the own Org scanner (#101, #103): the structure automaton.
//!
//! Turns the per-line classes of `line_lexer` into the heading tree, per-section
//! planning and property drawers, block and drawer regions, keyword lines, and
//! comment and fixed-width runs. The parse entry (`orgize_adapter`) builds every
//! structural fact from this output; `inline_scanner` reads the text between.
//! States and transitions: `docs/design/line-scanner.org`.

use std::{collections::HashSet, ops::Range};

use super::line_lexer::{classify_line, lines, Line, LineClass};
use super::model::{ParsedKeyword, ParsedProperty, ParsedPropertySource};
use super::properties::parsed_property_from_raw_line;

/// Deepest heading level accepted. A product rule (#44) that started as a stack guard
/// for the recursive tree of the former Orgize backend; the scanner itself never recurses,
/// so it only protects against pathological files now. It counts every `^\*+ ` line,
/// because Org reads such a line as a headline wherever it stands.
pub const MAX_HEADING_LEVEL: usize = 100;

/// Heading deeper than `MAX_HEADING_LEVEL`.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DepthError {
    pub level: usize,
    pub line_number: u32,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Structure {
    /// Real headings in document order; the synthetic file root is not included.
    pub headings: Vec<HeadingNode>,
    /// Property drawer of the file-level section.
    pub file_properties: Option<PropertyDrawerNode>,
    /// Keyword lines outside raw blocks, in document order. Affiliated keywords are not
    /// keyword elements in Org and are listed in `affiliated` instead.
    pub keywords: Vec<KeywordLine>,
    /// Affiliated keyword lines (`#+NAME:`, `#+CAPTION:`, ...) that belong to the next
    /// element, in document order.
    pub affiliated: Vec<KeywordLine>,
    /// Blocks, drawers, comment and fixed-width runs, ordered by start offset.
    pub regions: Vec<Region>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct HeadingNode {
    pub level: usize,
    /// Index into `Structure::headings`; `None` for a top-level heading.
    pub parent: Option<usize>,
    pub line_number: u32,
    /// The headline line, without its line terminator.
    pub headline: Range<usize>,
    /// Headline start to the start of the next heading of the same or a lower level.
    pub subtree: Range<usize>,
    /// Text after the headline line up to the next heading of any level.
    pub section: Range<usize>,
    pub planning: Option<PlanningLine>,
    pub properties: Option<PropertyDrawerNode>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PlanningLine {
    /// The planning line, without its line terminator.
    pub range: Range<usize>,
    /// First byte after the line terminator.
    pub next: usize,
    pub line_number: u32,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PropertyDrawerNode {
    /// From the `:PROPERTIES:` line start to the end of the `:END:` line terminator.
    pub range: Range<usize>,
    /// Rows between the two lines, read with the same helper as the adapter.
    pub rows: Vec<ParsedProperty>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct KeywordLine {
    pub key: Range<usize>,
    pub value: Range<usize>,
    pub line: Range<usize>,
    /// First byte after the line terminator.
    pub next: usize,
    /// Not inside a block or drawer.
    pub top_level: bool,
    pub line_number: u32,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RegionKind {
    /// `#+begin_NAME` to `#+end_NAME`.
    Block,
    /// `:NAME:` to `:END:`, not the property drawer.
    Drawer,
    /// `#+BEGIN: NAME` to `#+END:`; the content is parsed like a drawer's.
    DynamicBlock,
    /// Consecutive `# ` comment lines.
    Comment,
    /// Consecutive `: ` fixed-width lines.
    FixedWidth,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Region {
    pub kind: RegionKind,
    /// Block or drawer name as written; empty for comment and fixed-width runs.
    pub name: String,
    /// Whole region including the terminator of its last line.
    pub range: Range<usize>,
    /// Between the begin and end lines; equals `range` for comment and fixed-width runs.
    pub content: Range<usize>,
}

impl Region {
    /// Block whose content is raw text: nothing inside is structure.
    pub fn is_raw_block(&self) -> bool {
        self.kind == RegionKind::Block
            && ["src", "example", "export", "comment", "verse"]
                .iter()
                .any(|name| self.name.eq_ignore_ascii_case(name))
    }
}

/// Where the automaton is inside a section; decides whether planning or a property
/// drawer is still allowed.
#[derive(Clone, Copy, PartialEq, Eq)]
enum Position {
    /// Before the first headline, only comment lines seen so far.
    FileStart,
    /// Directly after a headline line.
    AfterHeadline,
    /// Directly after the planning line.
    AfterPlanning,
    Body,
}

/// Open greater element (quote, center or special block, drawer): its content ends at
/// `end_line`, and nothing inside may look past it.
struct Frame {
    end_line: usize,
}

struct Scan<'a> {
    content: &'a str,
    lines: Vec<(Line, LineClass)>,
    /// Index of the next headline line at or after each index.
    next_headline: Vec<usize>,
    /// `(name, limit)` pairs already known to have no end before `limit`, so that a run of
    /// unclosed begins is not searched again for each one.
    unclosed: HashSet<(String, usize)>,
    /// Affiliated keyword lines before this index end in no element.
    dangling_until: usize,
}

pub fn scan_structure(content: &str) -> Result<Structure, DepthError> {
    let lines: Vec<(Line, LineClass)> = lines(content)
        .map(|line| {
            let class = classify_line(line.text(content));
            (line, class)
        })
        .collect();
    let mut next_headline = vec![lines.len(); lines.len() + 1];
    for index in (0..lines.len()).rev() {
        next_headline[index] = if matches!(lines[index].1, LineClass::Headline { .. }) {
            index
        } else {
            next_headline[index + 1]
        };
    }
    let mut scan = Scan {
        content,
        lines,
        next_headline,
        unclosed: HashSet::new(),
        dangling_until: 0,
    };
    scan.run()
}

impl Scan<'_> {
    fn text(&self, index: usize) -> &str {
        self.lines[index].0.text(self.content)
    }

    fn name(&self, index: usize, range: &Range<usize>) -> &str {
        &self.text(index)[range.clone()]
    }

    fn run(&mut self) -> Result<Structure, DepthError> {
        let content_len = self.content.len();
        let mut out = Structure {
            headings: Vec::new(),
            file_properties: None,
            keywords: Vec::new(),
            affiliated: Vec::new(),
            regions: Vec::new(),
        };
        // Open headings as indexes into `out.headings`, innermost last.
        let mut open: Vec<usize> = Vec::new();
        let mut frames: Vec<Frame> = Vec::new();
        let mut position = Position::FileStart;
        let mut index = 0;
        // Start of the affiliated keyword lines waiting for their element.
        let mut affiliated_start: Option<usize> = None;

        while index < self.lines.len() {
            if frames.last().is_some_and(|frame| frame.end_line == index) {
                frames.pop();
                index += 1;
                continue;
            }
            let line = self.lines[index].0;
            let class = self.lines[index].1.clone();
            let affiliated = affiliated_start.take();
            let element_start = affiliated.unwrap_or(line.start);
            let line_number = index as u32 + 1;
            // Content search limit: the enclosing element's end, else the next headline.
            let limit = frames
                .last()
                .map_or(self.next_headline[index], |frame| frame.end_line);

            if let LineClass::Headline { level } = class {
                if level > MAX_HEADING_LEVEL {
                    return Err(DepthError { level, line_number });
                }
                while let Some(&top) = open.last() {
                    if out.headings[top].level < level {
                        break;
                    }
                    out.headings[top].subtree.end = line.start;
                    open.pop();
                }
                close_section(&mut out.headings, line.start);
                out.headings.push(HeadingNode {
                    level,
                    parent: open.last().copied(),
                    line_number,
                    headline: line.start..line.end,
                    subtree: line.start..content_len,
                    section: line.next..usize::MAX,
                    planning: None,
                    properties: None,
                });
                open.push(out.headings.len() - 1);
                position = Position::AfterHeadline;
                index += 1;
                continue;
            }

            let current = open.last().copied();
            match class {
                LineClass::Planning if position == Position::AfterHeadline => {
                    if let Some(heading) = current {
                        out.headings[heading].planning = Some(PlanningLine {
                            range: line.start..line.end,
                            next: line.next,
                            line_number,
                        });
                    }
                    position = Position::AfterPlanning;
                    index += 1;
                }
                LineClass::Drawer { ref name }
                    if self.name(index, name).eq_ignore_ascii_case("PROPERTIES")
                        && matches!(
                            position,
                            Position::FileStart | Position::AfterHeadline | Position::AfterPlanning
                        ) =>
                {
                    position = Position::Body;
                    match self.find_drawer_end(index, limit) {
                        Some(end) if (index + 1..end).all(|i| is_property_line(self.text(i))) => {
                            let node = self.property_drawer(index, end);
                            match current {
                                Some(heading) => out.headings[heading].properties = Some(node),
                                None => out.file_properties = Some(node),
                            }
                            index = end + 1;
                        }
                        Some(end) => {
                            // Org's property-drawer regexp needs every row to be a property.
                            // Otherwise org-element reads a generic drawer and parses its
                            // content, and org-get-property-block finds no properties.
                            out.regions.push(Region {
                                kind: RegionKind::Drawer,
                                name: self.name(index, name).to_string(),
                                range: line.start..self.lines[end].0.next,
                                content: line.next..self.lines[end].0.start,
                            });
                            frames.push(Frame { end_line: end });
                            index += 1;
                        }
                        None => index += 1,
                    }
                }
                LineClass::Drawer { .. } | LineClass::DrawerEnd => {
                    // `:END:` outside a drawer is a drawer begin too (Emacs, see the doc).
                    position = Position::Body;
                    if let Some(end) = self.find_drawer_end(index, limit) {
                        let name = self.text(index).trim_matches([' ', '\t']);
                        out.regions.push(Region {
                            kind: RegionKind::Drawer,
                            name: name[1..name.len() - 1].to_string(),
                            range: element_start..self.lines[end].0.next,
                            content: line.next..self.lines[end].0.start,
                        });
                        frames.push(Frame { end_line: end });
                    }
                    index += 1;
                }
                LineClass::DynBlockBegin { ref name } => {
                    position = Position::Body;
                    if let Some(end) = self.find_dyn_block_end(index, limit) {
                        out.regions.push(Region {
                            kind: RegionKind::DynamicBlock,
                            name: self.name(index, name).to_string(),
                            range: element_start..self.lines[end].0.next,
                            content: line.next..self.lines[end].0.start,
                        });
                        frames.push(Frame { end_line: end });
                    }
                    index += 1;
                }
                LineClass::BlockBegin { ref name } => {
                    position = Position::Body;
                    let name = self.name(index, name).to_string();
                    if let Some(end) = self.find_block_end(index, limit, &name) {
                        let region = Region {
                            kind: RegionKind::Block,
                            name,
                            range: element_start..self.lines[end].0.next,
                            content: line.next..self.lines[end].0.start,
                        };
                        if region.is_raw_block() {
                            index = end + 1;
                        } else {
                            frames.push(Frame { end_line: end });
                            index += 1;
                        }
                        out.regions.push(region);
                    } else {
                        index += 1;
                    }
                }
                LineClass::Keyword { key, value } => {
                    position = Position::Body;
                    // Attached only when the chain of affiliated keywords ends in an element
                    // (Emacs: `#+CAPTION:` and `#+NAME:` right before a headline are keywords).
                    let affiliated_keyword = is_affiliated_key(&self.text(index)[key.clone()])
                        && (affiliated.is_some() || self.chain_has_element(index, limit));
                    let keyword = KeywordLine {
                        key: line.start + key.start..line.start + key.end,
                        value: line.start + value.start..line.start + value.end,
                        line: line.start..line.end,
                        next: line.next,
                        top_level: frames.is_empty(),
                        line_number,
                    };
                    if affiliated_keyword {
                        // Org attaches it to the next element, it is not a keyword element.
                        out.affiliated.push(keyword);
                        affiliated_start = Some(element_start);
                    } else {
                        out.keywords.push(keyword);
                    }
                    index += 1;
                }
                // Emacs reads `#+NAME: x` followed by `# c` as one paragraph.
                LineClass::Comment if affiliated.is_some() => {
                    position = Position::Body;
                    index += 1;
                }
                LineClass::Comment | LineClass::FixedWidth => {
                    let kind = if class == LineClass::Comment {
                        RegionKind::Comment
                    } else {
                        RegionKind::FixedWidth
                    };
                    if class != LineClass::Comment || position != Position::FileStart {
                        position = Position::Body;
                    }
                    match out.regions.last_mut() {
                        Some(last)
                            if last.kind == kind
                                && last.range.end == line.start
                                && affiliated.is_none() =>
                        {
                            last.range.end = line.next;
                            last.content.end = line.next;
                        }
                        _ => out.regions.push(Region {
                            kind,
                            name: String::new(),
                            range: element_start..line.next,
                            content: line.start..line.next,
                        }),
                    }
                    index += 1;
                }
                _ => {
                    position = Position::Body;
                    index += 1;
                }
            }
        }
        close_section(&mut out.headings, content_len);
        for top in open {
            out.headings[top].subtree.end = content_len;
        }
        out.regions.sort_by_key(|region| region.range.start);
        Ok(out)
    }

    /// First `:END:` line after `begin` and before `limit`.
    fn find_drawer_end(&mut self, begin: usize, limit: usize) -> Option<usize> {
        let key = (String::new(), limit);
        if self.unclosed.contains(&key) {
            return None;
        }
        let found = (begin + 1..limit).find(|&i| self.lines[i].1 == LineClass::DrawerEnd);
        if found.is_none() {
            self.unclosed.insert(key);
        }
        found
    }

    /// True when the affiliated keyword lines from `first` on are followed by a non-empty line
    /// before `limit`. A dangling chain is remembered, so a run of them stays linear.
    fn chain_has_element(&mut self, first: usize, limit: usize) -> bool {
        if first < self.dangling_until {
            return false;
        }
        let mut next = first + 1;
        while next < limit
            && matches!(&self.lines[next].1, LineClass::Keyword { key, .. }
                if is_affiliated_key(&self.text(next)[key.clone()]))
        {
            next += 1;
        }
        let found = next < limit && self.lines[next].1 != LineClass::Empty;
        if !found {
            self.dangling_until = next;
        }
        found
    }

    /// First `#+END:` or `#+END` line after `begin` and before `limit`.
    fn find_dyn_block_end(&mut self, begin: usize, limit: usize) -> Option<usize> {
        let key = ("\0dynamic".to_string(), limit);
        if self.unclosed.contains(&key) {
            return None;
        }
        let found = (begin + 1..limit).find(|&i| is_dyn_block_end(self.text(i)));
        if found.is_none() {
            self.unclosed.insert(key);
        }
        found
    }

    /// First `#+end_NAME` line (name compared case-insensitively) after `begin` and
    /// before `limit`.
    fn find_block_end(&mut self, begin: usize, limit: usize, name: &str) -> Option<usize> {
        let key = (name.to_lowercase(), limit);
        if self.unclosed.contains(&key) {
            return None;
        }
        let found = (begin + 1..limit).find(|&i| match &self.lines[i].1 {
            LineClass::BlockEnd { name: end } => self.name(i, end).to_lowercase() == key.0,
            _ => false,
        });
        if found.is_none() {
            self.unclosed.insert(key);
        }
        found
    }

    fn property_drawer(&self, begin: usize, end: usize) -> PropertyDrawerNode {
        let rows = (begin + 1..end)
            .filter_map(|i| {
                parsed_property_from_raw_line(
                    self.text(i),
                    ParsedPropertySource::PropertyDrawer,
                    i as u32 + 1,
                )
            })
            .collect();
        PropertyDrawerNode {
            range: self.lines[begin].0.start..self.lines[end].0.next,
            rows,
        }
    }
}

/// Document keywords (`#+KEY: value`) of `structure`, in source order. The value is trimmed;
/// an empty value is `None`.
pub fn parsed_keywords(content: &str, structure: &Structure) -> Vec<ParsedKeyword> {
    structure
        .keywords
        .iter()
        .map(|keyword| {
            let value = content[keyword.value.clone()].trim();
            ParsedKeyword {
                key: content[keyword.key.clone()].to_string(),
                value: Some(value.to_string()).filter(|value| !value.is_empty()),
                line_number: Some(keyword.line_number),
            }
        })
        .collect()
}

/// Ends the section of the heading that was most recently pushed, if still open.
fn close_section(headings: &mut [HeadingNode], end: usize) {
    if let Some(last) = headings.last_mut() {
        if last.section.end == usize::MAX {
            last.section.end = end;
        }
    }
}

/// Org's dynamic block end (`org-dblock-end-re`): `#+END:` or `#+END`, any case, blanks around.
fn is_dyn_block_end(line: &str) -> bool {
    let line = line.trim_matches([' ', '\t']);
    line.get(.."#+end".len())
        .is_some_and(|head| head.eq_ignore_ascii_case("#+end"))
        && matches!(&line["#+end".len()..], "" | ":")
}

/// Org's node-property line: `:KEY:` alone or followed by a blank and a value
/// (`[ \t]*:\S-+:\(?:[ \t].*\)?[ \t]*$`).
fn is_property_line(line: &str) -> bool {
    let Some(rest) = line.trim_start_matches([' ', '\t']).strip_prefix(':') else {
        return false;
    };
    let token = rest.split([' ', '\t']).next().unwrap_or("");
    token.len() >= 2 && token.ends_with(':')
}

/// Keywords that Org attaches to the following element (`org-element-affiliated-keywords`
/// and `ATTR_*`). An optional `[...]` suffix (`#+CAPTION[short]:`) is ignored.
fn is_affiliated_key(key: &str) -> bool {
    let key = key.split('[').next().unwrap_or(key).to_ascii_uppercase();
    const AFFILIATED: [&str; 13] = [
        "CAPTION", "DATA", "HEADER", "HEADERS", "LABEL", "NAME", "PLOT", "RESNAME", "RESULT",
        "RESULTS", "SOURCE", "SRCNAME", "TBLNAME",
    ];
    AFFILIATED.contains(&key.as_str())
        || key.strip_prefix("ATTR_").is_some_and(|rest| {
            !rest.is_empty()
                && rest
                    .chars()
                    .all(|c| c.is_ascii_alphanumeric() || c == '-' || c == '_')
        })
}

#[cfg(test)]
mod tests {
    use super::*;

    /// One token per fact: `H<level>@<line>[^<parent>][ P][ D[keys]]`, `F[keys]` for the
    /// file drawer, `K:<key>`, `<kind>:<name>@<start>`, `comment`, `fixed`.
    fn summary(content: &str) -> String {
        let scan = scan_structure(content).expect("depth is fine");
        let keys = |drawer: &PropertyDrawerNode| {
            drawer
                .rows
                .iter()
                .map(|row| row.key.as_str())
                .collect::<Vec<_>>()
                .join(",")
        };
        let mut tokens = Vec::new();
        if let Some(drawer) = &scan.file_properties {
            tokens.push(format!("F[{}]", keys(drawer)));
        }
        for heading in &scan.headings {
            let mut token = format!("H{}@{}", heading.level, heading.line_number);
            if let Some(parent) = heading.parent {
                token.push_str(&format!("^{parent}"));
            }
            if heading.planning.is_some() {
                token.push('P');
            }
            if let Some(drawer) = &heading.properties {
                token.push_str(&format!("D[{}]", keys(drawer)));
            }
            tokens.push(token);
        }
        for keyword in &scan.keywords {
            tokens.push(format!("K:{}", &content[keyword.key.clone()]));
        }
        for region in &scan.regions {
            tokens.push(match region.kind {
                RegionKind::Block => format!("block:{}@{}", region.name, region.range.start),
                RegionKind::Drawer => format!("drawer:{}@{}", region.name, region.range.start),
                RegionKind::DynamicBlock => format!("dyn:{}@{}", region.name, region.range.start),
                RegionKind::Comment => "comment".to_string(),
                RegionKind::FixedWidth => "fixed".to_string(),
            });
        }
        tokens.join(" ")
    }

    #[test]
    fn scans_structure_like_org() {
        let table: &[(&str, &str)] = &[
            ("* A\n** B\n* C\n", "H1@1 H2@2^0 H1@3"),
            (
                "* A\n*** C\n** B\n**** D\n* E\n",
                "H1@1 H3@2^0 H2@3^0 H4@4^2 H1@5",
            ),
            (
                "* H\nSCHEDULED: <2024-01-01 Mon>\n:PROPERTIES:\n:ID: x\n:A+: y\n:END:\ntext\n",
                "H1@1PD[ID,A]",
            ),
            ("* H\nfoo\nSCHEDULED: <2024-01-01 Mon>\n", "H1@1"),
            (
                "* H\n\n:PROPERTIES:\n:ID: x\n:END:\n",
                "H1@1 drawer:PROPERTIES@5",
            ),
            (
                "# c\n:PROPERTIES:\n:ID: x\n:END:\n* H\n",
                "F[ID] H1@5 comment",
            ),
            ("\n:PROPERTIES:\n:ID: x\n:END:\n", "drawer:PROPERTIES@1"),
            ("* H\r\n:PROPERTIES:\r\n:ID: x\r\n:END:\r\n", "H1@1D[ID]"),
            // Headlines cut every section, so a block or drawer is closed before the next one.
            ("* H\n#+begin_src\n* x\n#+end_src\n", "H1@1 H1@3"),
            ("* H\n:LOG:\n* x\n:END:\n", "H1@1 H1@3"),
            // Raw blocks hide their content; greater blocks and drawers parse it.
            ("* H\n#+begin_SRC\n:LOG:\n#+End_src\n", "H1@1 block:SRC@4"),
            (
                "* H\n#+begin_quote\n:D:\nx\n:END:\n#+end_quote\n",
                "H1@1 block:quote@4 drawer:D@18",
            ),
            // Limits: a drawer cannot end outside its block, nor a block inside a drawer.
            (
                "* H\n#+begin_quote\n:D:\nx\n#+end_quote\n:END:\n",
                "H1@1 block:quote@4",
            ),
            (
                "* H\n:A:\n#+begin_quote\n:END:\n#+end_quote\n",
                "H1@1 drawer:A@4",
            ),
            ("* H\n:END:\nx\n:END:\n", "H1@1 drawer:END@4"),
            ("* H\n#+begin_x\n#+begin_x\nend\n", "H1@1"),
            // Affiliated keywords attach to the next element and start its range.
            (
                "* H\n#+NAME: n\n#+begin_src\nx\n#+end_src\n#+K: v\n",
                "H1@1 K:K block:src@4",
            ),
            ("* H\n#+NAME: x\n\n#+K: v\n", "H1@1 K:NAME K:K"),
            ("# a\n# b\n: c\n: d\n", "comment fixed"),
            // Property drawer with a non-property row is a generic drawer without properties.
            (
                "* H\n:PROPERTIES:\n# c\n:ID: x\n:END:\n",
                "H1@1 drawer:PROPERTIES@4 comment",
            ),
            (
                "* H\n:PROPERTIES:\n:a b: c\n:END:\n",
                "H1@1 drawer:PROPERTIES@4",
            ),
            (
                "* H\n:PROPERTIES:\n:ID:x\n:END:\n",
                "H1@1 drawer:PROPERTIES@4",
            ),
            ("* H\n:PROPERTIES:\n::\n:END:\n", "H1@1 drawer:PROPERTIES@4"),
            ("* H\n:PROPERTIES:\n:::  v\n:END:\n", "H1@1D[:]"),
            // An empty or garbage planning line is an empty planning element (Emacs:
            // planning L2-2, property-drawer L3-5), so the drawer after it still counts.
            (
                "* H\nSCHEDULED: garbage\n:PROPERTIES:\n:ID: x\n:END:\n",
                "H1@1PD[ID]",
            ),
            ("* H\nCLOSED:\n:PROPERTIES:\n:ID: x\n:END:\n", "H1@1PD[ID]"),
            // A block ends at the first matching end line, names compare without case
            // (Emacs: src-block L2-5 with a non-matching `#+end_example` inside; src-block
            // L2-3 for `#+begin_SRC` ... `#+End_src`).
            (
                "* H\n#+begin_src a\n#+K: hidden\n#+end_example\n#+END_SRC\n#+K2: v\n",
                "H1@1 K:K2 block:src@4",
            ),
            (
                "* H\n#+begin_SRC\n#+End_src\n#+BEGIN_Quote\nq\n#+end_QUOTE\n",
                "H1@1 block:SRC@4 block:Quote@26",
            ),
            // Drawer names may hold hyphens and non-ASCII letters (Emacs: drawer
            // name=my-drawer, name=äö).
            (
                "* H\n:my-drawer:\nx\n:END:\n:äö:\ny\n:END:\n",
                "H1@1 drawer:my-drawer@4 drawer:äö@24",
            ),
            // A drawer after an affiliated keyword starts at the keyword (Emacs: drawer
            // L2-5 for `#+NAME: d` + `:LOG:` ... `:END:`).
            ("* H\n#+NAME: d\n:LOG:\nx\n:END:\n", "H1@1 drawer:LOG@4"),
            // Affiliated keywords are no keyword elements; dangling ones are.
            ("#+NAME: n\n#+TITLE: t\n", "K:TITLE"),
            ("#+NAME: n\n#+END_SRC\n", ""),
            ("#+NAME: n\n#+begin_src\nx\n", ""),
            ("#+NAME: n\n# c\n# d\n", "comment"),
            // A chain that ends in no element (headline, end of file, blank line) is keywords.
            ("#+CAPTION: c\n#+NAME: n\n* H\n", "H1@3 K:CAPTION K:NAME"),
            (
                "#+CAPTION: c\n#+NAME: n\n\n#+K: v\n",
                "K:CAPTION K:NAME K:K",
            ),
            ("#+CAPTION: c\n#+NAME: n\ntext\n", ""),
            // Dynamic blocks (Emacs: dynamic-block, no BEGIN or END keyword).
            (
                "* H\n#+BEGIN: clocktable :scope file\n#+K: v\n#+END:\n",
                "H1@1 K:K dyn:clocktable@4",
            ),
            ("#+begin:x\n#+K: v\n  #+end\n", "K:K dyn:x@0"),
            ("#+BEGIN: x\n#+K: v\n", "K:K"),
            ("#+BEGIN: x\n* H\n#+END:\n", "H1@2 K:END"),
            ("#+END:\n", "K:END"),
            ("#+BEGIN: x\n#+BEGIN: y\n#+END:\n#+END:\n", "K:END dyn:x@0"),
            ("#+BEGIN:\n#+END:\n", "K:BEGIN K:END"),
            // An empty quote block is a block (Emacs: quote-block L1-2).
            ("#+begin_quote\n#+end_quote\n", "block:quote@0"),
        ];
        for (content, expected) in table {
            assert_eq!(summary(content), *expected, "input {content:?}");
        }
    }

    #[test]
    fn rejects_headings_over_the_depth_limit() {
        let deepest = format!("{} x\n", "*".repeat(MAX_HEADING_LEVEL));
        assert!(scan_structure(&deepest).is_ok());
        let too_deep = format!("a\n{} x\n", "*".repeat(MAX_HEADING_LEVEL + 1));
        let error = scan_structure(&too_deep).unwrap_err();
        assert_eq!((error.level, error.line_number), (MAX_HEADING_LEVEL + 1, 2));
    }
}
