//! Differential test for the line scanner (#101): every input is parsed by the current
//! Orgize-based adapter and by `structure_scanner`, and the shared structural facts
//! must agree. A difference must be a scanner bug or an entry of `ALLOWED` (a case
//! where the adapter deviates from Emacs Org 9.6, verified with `emacs --batch -Q`).
//!
//! Set `STRUCTURE_DIFF_REPORT=1` and run with `--nocapture` to print every difference
//! instead of asserting.

use std::{collections::HashSet, fs, ops::Range, path::Path};

use orgize::{rowan::ast::AstNode, Org, SyntaxKind};

use super::model::{
    OrgParser, ParsedHeading, ParsedOrgDocument, ParsedProperty, ParsedPropertySource,
    ParsedTimestamp, ParsedTimestampRole,
};
use super::structure_scanner::{scan_structure, RegionKind, Structure};
use super::timestamp_raw::parse_planning_fallback_entries;
use super::title::{source_title_from_content_line, source_title_tags};
use super::{OrgizeAdapter, ParseDiagnostic, ParseOptions};

/// Differences that are adapter deviations from Emacs Org 9.6 (Emacs 29.3,
/// `emacs --batch -Q`, `org-element-parse-buffer` unless noted). Entries are
/// `(input name, category)`; the evidence sits next to each entry.
const ALLOWED: &[(&str, &str)] = &[
    // `* H\nSCHEDULED: garbage\n:PROPERTIES:...`: Emacs parses a planning element (empty)
    // followed by a property-drawer, and `org-entry-get` returns the ID. Orgize and the
    // adapter need a timestamp, treat the line as body text and lose the drawer.
    (
        "adversarial: planning garbage",
        "planning-line-without-timestamp",
    ),
    ("adversarial: planning garbage", "property-rows"),
    (
        "adversarial: planning bare keyword",
        "planning-line-without-timestamp",
    ),
    ("adversarial: planning bare keyword", "property-rows"),
    // `#+begin_src a` ... `#+end_example` ... `#+END_SRC`: Emacs ends the block at the
    // first `#+END_SRC` (src-block L2-5); Orgize finds no block when a non-matching
    // `#+end_` line sits in between.
    ("adversarial: block name mismatch", "blocks"),
    // `#+begin_SRC` ... `#+End_src`, `#+BEGIN_Quote` ... `#+end_QUOTE`: Emacs matches names
    // case-insensitively (src-block L2-3); Orgize does not see a block for mixed case.
    ("adversarial: block name case", "blocks"),
    // `:my-drawer:` and `:äö:` are drawers in Emacs (drawer name=my-drawer / name=äö);
    // Orgize accepts neither hyphens nor non-ASCII letters in drawer names.
    ("adversarial: drawer hyphen", "drawers"),
    ("adversarial: drawer unicode", "drawers"),
    // `#+NAME: d` directly before `:LOG:` ... `:END:`: Emacs gives one drawer that starts at
    // the affiliated keyword (drawer L2-5); Orgize builds no drawer there.
    ("adversarial: affiliated before drawer", "drawers"),
    // `#+NAME: n` followed by a keyword, a stray `#+END_SRC`, an unclosed `#+begin_src`
    // or a comment: Emacs attaches it to the next element (keyword TITLE L1-2, paragraph
    // L1-2, paragraph L1-3), so it is no keyword element and `# c` after it is no
    // comment (paragraph L1-2). Orgize reports NAME as a keyword and keeps the comment.
    ("adversarial: affiliated before keyword", "keywords"),
    ("adversarial: affiliated before stray end", "keywords"),
    ("adversarial: affiliated before unclosed block", "keywords"),
    ("adversarial: affiliated before comment", "keywords"),
    ("adversarial: affiliated before comment", "comments"),
];

struct Diff {
    category: &'static str,
    detail: String,
}

fn diff(category: &'static str, detail: impl Into<String>) -> Diff {
    Diff {
        category,
        detail: detail.into(),
    }
}

/// `None` when Orgize panics on the input.
fn parse_adapter(content: &str) -> Option<Result<ParsedOrgDocument, ParseDiagnostic>> {
    std::panic::catch_unwind(|| {
        OrgizeAdapter::new().parse_document(Path::new("t.org"), content, &ParseOptions::default())
    })
    .ok()
}

/// Orgize (like Emacs) attaches blank lines after an element to it; drop them so that
/// ranges compare by their text.
fn trim_range(content: &str, range: &Range<usize>) -> Range<usize> {
    range.start..range.start + content[range.clone()].trim_end().len()
}

fn merge(mut ranges: Vec<Range<usize>>) -> Vec<Range<usize>> {
    ranges.sort_by_key(|range| (range.start, range.end));
    let mut merged: Vec<Range<usize>> = Vec::new();
    for range in ranges {
        match merged.last_mut() {
            Some(last) if range.start <= last.end => last.end = last.end.max(range.end),
            _ => merged.push(range),
        }
    }
    merged
}

/// Block kind as a lowercase word, `special` for names Org does not know.
fn block_kind(name: &str) -> String {
    let lower = name.to_lowercase();
    match lower.as_str() {
        "src" | "example" | "export" | "comment" | "verse" | "quote" | "center" => lower,
        _ => "special".to_string(),
    }
}

struct OrgizeRegions {
    blocks: Vec<(String, Range<usize>)>,
    drawers: Vec<Range<usize>>,
    comments: Vec<Range<usize>>,
    fixed_width: Vec<Range<usize>>,
}

fn orgize_regions(content: &str) -> OrgizeRegions {
    let org = Org::parse(content);
    let mut regions = OrgizeRegions {
        blocks: Vec::new(),
        drawers: Vec::new(),
        comments: Vec::new(),
        fixed_width: Vec::new(),
    };
    for node in org.document().syntax().descendants() {
        let range = usize::from(node.text_range().start())..usize::from(node.text_range().end());
        let block = match node.kind() {
            SyntaxKind::SOURCE_BLOCK => Some("src"),
            SyntaxKind::COMMENT_BLOCK => Some("comment"),
            SyntaxKind::EXAMPLE_BLOCK => Some("example"),
            SyntaxKind::EXPORT_BLOCK => Some("export"),
            SyntaxKind::VERSE_BLOCK => Some("verse"),
            SyntaxKind::QUOTE_BLOCK => Some("quote"),
            SyntaxKind::CENTER_BLOCK => Some("center"),
            SyntaxKind::SPECIAL_BLOCK => Some("special"),
            _ => None,
        };
        if let Some(kind) = block {
            regions.blocks.push((kind.to_string(), range));
        } else if matches!(
            node.kind(),
            SyntaxKind::DRAWER | SyntaxKind::PROPERTY_DRAWER
        ) {
            regions.drawers.push(range);
        } else if node.kind() == SyntaxKind::COMMENT {
            regions.comments.push(range);
        } else if node.kind() == SyntaxKind::FIXED_WIDTH {
            regions.fixed_width.push(range);
        }
    }
    regions.blocks.sort_by_key(|(_, range)| range.start);
    regions.drawers.sort_by_key(|range| range.start);
    regions
}

fn compare_headings(
    content: &str,
    scan: &Structure,
    adapter: &[ParsedHeading],
    out: &mut Vec<Diff>,
) {
    if scan.headings.len() != adapter.len() {
        out.push(diff(
            "heading-count",
            format!("scanner {} adapter {}", scan.headings.len(), adapter.len()),
        ));
        return;
    }
    for (index, (node, heading)) in scan.headings.iter().zip(adapter).enumerate() {
        let at = format!("heading {} (line {})", index + 1, node.line_number);
        if node.level != heading.level as usize {
            out.push(diff(
                "heading-level",
                format!("{at}: {} vs {}", node.level, heading.level),
            ));
        }
        if node.subtree != (heading.byte_start..heading.byte_end) {
            out.push(diff(
                "heading-range",
                format!(
                    "{at}: {:?} vs {}..{}",
                    node.subtree, heading.byte_start, heading.byte_end
                ),
            ));
        }
        if Some(node.line_number) != heading.line_number {
            out.push(diff(
                "heading-line",
                format!("{at}: {:?}", heading.line_number),
            ));
        }
        // The adapter's root heading sits at index 0, so its parent indexes are shifted.
        let adapter_parent = heading
            .parent_index
            .and_then(|parent| parent.checked_sub(1));
        if node.parent != adapter_parent {
            out.push(diff(
                "heading-parent",
                format!("{at}: {:?} vs {adapter_parent:?}", node.parent),
            ));
        }
        let start = node.headline.start;
        let title = source_title_from_content_line(content, start).raw;
        if Some(&title) != heading.title_raw.as_ref() {
            out.push(diff(
                "heading-title",
                format!("{at}: {title:?} vs {:?}", heading.title_raw),
            ));
        }
        if source_title_tags(content, start) != heading.tags {
            out.push(diff("heading-tags", at.clone()));
        }
        let planning = node.planning.as_ref().map(|planning| &planning.range);
        compare_planning(content, &at, planning, heading, out);
        let rows = node
            .properties
            .as_ref()
            .map(|drawer| drawer.rows.as_slice())
            .unwrap_or_default();
        if rows != drawer_rows(heading) {
            out.push(diff(
                "property-rows",
                format!("{at}: {rows:?} vs {:?}", drawer_rows(heading)),
            ));
        }
    }
}

fn drawer_rows(heading: &ParsedHeading) -> Vec<ParsedProperty> {
    heading
        .properties
        .iter()
        .filter(|property| property.source == ParsedPropertySource::PropertyDrawer)
        .cloned()
        .collect()
}

fn compare_planning(
    content: &str,
    at: &str,
    planning: Option<&Range<usize>>,
    heading: &ParsedHeading,
    out: &mut Vec<Diff>,
) {
    let mut entries = Vec::new();
    if let Some(range) = planning {
        for (role, raw, relative) in parse_planning_fallback_entries(&content[range.clone()]) {
            entries.push((role, raw, range.start + relative));
        }
    }
    let scanner_of = |role| {
        entries
            .iter()
            .rev()
            .find(|(candidate, _, _)| *candidate == role)
            .map(|(_, raw, start)| (raw.clone(), *start..*start + raw.len()))
    };
    // The scanner cuts the first timestamp of the entry; Orgize also takes a `--` range
    // end, so compare the start and accept a raw value that extends the scanner's.
    let adapter_of = |timestamp: &Option<ParsedTimestamp>| {
        timestamp
            .as_ref()
            .map(|t| (t.raw_value.clone(), t.byte_start..t.byte_end))
    };
    let mut adapter_has_any = false;
    for (role, name, adapter) in [
        (
            ParsedTimestampRole::Scheduled,
            "scheduled",
            adapter_of(&heading.planning.scheduled),
        ),
        (
            ParsedTimestampRole::Deadline,
            "deadline",
            adapter_of(&heading.planning.deadline),
        ),
        (
            ParsedTimestampRole::Closed,
            "closed",
            adapter_of(&heading.planning.closed),
        ),
    ] {
        adapter_has_any |= adapter.is_some();
        let scanner = scanner_of(role);
        let same = match (&scanner, &adapter) {
            (Some((raw, range)), Some((adapter_raw, adapter_range))) => {
                range.start == adapter_range.start && adapter_raw.starts_with(raw.as_str())
            }
            (None, None) => true,
            _ => false,
        };
        if !same {
            out.push(diff(
                "planning",
                format!("{at} {name}: {scanner:?} vs {adapter:?}"),
            ));
        }
    }
    if planning.is_some() && entries.is_empty() && !adapter_has_any {
        // Same facts, but the adapter did not treat the line as planning (see ALLOWED).
        out.push(diff("planning-line-without-timestamp", at.to_string()));
    }
}

fn differences(content: &str) -> Vec<Diff> {
    let mut out = Vec::new();
    let Some(adapter) = parse_adapter(content) else {
        // Orgize 0.10.0-alpha.10 panics (`assertion failed: !input.is_empty()` in
        // `element_nodes`) on a quote, center or special block whose content is empty or
        // only blank lines, for example `#+begin_quote\n#+end_quote`. Emacs parses an empty
        // quote-block. Such inputs cannot be compared; any other panic is unexplained.
        let empty_greater_block = scan_structure(content).is_ok_and(|scan| {
            scan.regions.iter().any(|region| {
                region.kind == RegionKind::Block
                    && !region.is_raw_block()
                    && content[region.content.clone()].trim().is_empty()
            })
        });
        if !empty_greater_block {
            out.push(diff("adapter-panic", "Orgize panics on this input"));
        }
        return out;
    };
    let adapter = match adapter {
        Ok(document) => document,
        Err(error) => {
            match scan_structure(content) {
                Err(depth) if Some(depth.line_number) == error.line_number => {}
                other => out.push(diff(
                    "depth-error",
                    format!("{error:?} vs {:?}", other.err()),
                )),
            }
            return out;
        }
    };
    let scan = match scan_structure(content) {
        Ok(scan) => scan,
        Err(depth) => {
            out.push(diff("depth-error", format!("scanner only: {depth:?}")));
            return out;
        }
    };

    compare_headings(content, &scan, &adapter.headings[1..], &mut out);

    let root_rows = drawer_rows(&adapter.headings[0]);
    let file_rows = scan
        .file_properties
        .as_ref()
        .map(|drawer| drawer.rows.clone())
        .unwrap_or_default();
    if file_rows != root_rows {
        out.push(diff(
            "file-property-rows",
            format!("{file_rows:?} vs {root_rows:?}"),
        ));
    }

    let keywords: Vec<_> = scan
        .keywords
        .iter()
        .map(|keyword| {
            let value = content[keyword.value.clone()].trim();
            (
                content[keyword.key.clone()].to_string(),
                Some(value.to_string()).filter(|value| !value.is_empty()),
                keyword.line_number,
            )
        })
        .collect();
    let adapter_keywords: Vec<_> = adapter
        .metadata
        .keywords
        .iter()
        .map(|k| (k.key.clone(), k.value.clone(), k.line_number.unwrap_or(0)))
        .collect();
    if keywords != adapter_keywords {
        out.push(diff(
            "keywords",
            format!("{keywords:?} vs {adapter_keywords:?}"),
        ));
    }

    let mut orgize = orgize_regions(content);
    for (_, range) in &mut orgize.blocks {
        *range = trim_range(content, range);
    }
    for range in orgize
        .drawers
        .iter_mut()
        .chain(&mut orgize.comments)
        .chain(&mut orgize.fixed_width)
    {
        *range = trim_range(content, range);
    }
    let mut blocks: Vec<(String, Range<usize>)> = scan
        .regions
        .iter()
        .filter(|region| region.kind == RegionKind::Block)
        .map(|region| (block_kind(&region.name), trim_range(content, &region.range)))
        .collect();
    blocks.sort_by_key(|(_, range)| range.start);
    if blocks != orgize.blocks {
        out.push(diff("blocks", format!("{blocks:?} vs {:?}", orgize.blocks)));
    }
    let mut drawers: Vec<Range<usize>> = scan
        .regions
        .iter()
        .filter(|region| region.kind == RegionKind::Drawer)
        .map(|region| region.range.clone())
        .chain(
            scan.file_properties
                .iter()
                .map(|drawer| drawer.range.clone()),
        )
        .chain(scan.headings.iter().filter_map(|heading| {
            heading
                .properties
                .as_ref()
                .map(|drawer| drawer.range.clone())
        }))
        .map(|range| trim_range(content, &range))
        .collect();
    drawers.sort_by_key(|range| range.start);
    // A property drawer with non-property rows is also a generic drawer region.
    drawers.dedup();
    if drawers != orgize.drawers {
        out.push(diff(
            "drawers",
            format!("{drawers:?} vs {:?}", orgize.drawers),
        ));
    }
    for (kind, category, orgize) in [
        (RegionKind::Comment, "comments", &orgize.comments),
        (RegionKind::FixedWidth, "fixed-width", &orgize.fixed_width),
    ] {
        let ranges: Vec<_> = scan
            .regions
            .iter()
            .filter(|region| region.kind == kind)
            .map(|region| trim_range(content, &region.range))
            .collect();
        if merge(ranges.clone()) != merge(orgize.clone()) {
            out.push(diff(category, format!("{ranges:?} vs {orgize:?}")));
        }
    }
    out
}

fn collect_org_files(dir: &Path, out: &mut Vec<std::path::PathBuf>, only_fixtures: bool) {
    let Ok(entries) = fs::read_dir(dir) else {
        return;
    };
    for entry in entries.flatten() {
        let path = entry.path();
        if path.is_dir() {
            collect_org_files(&path, out, only_fixtures);
        } else if path.extension().is_some_and(|ext| ext == "org")
            && (!only_fixtures || path.file_name().is_some_and(|name| name == "fixture.org"))
        {
            out.push(path);
        }
    }
}

fn corpus() -> Vec<(String, String)> {
    let root = Path::new(env!("CARGO_MANIFEST_DIR"));
    let mut files = Vec::new();
    collect_org_files(&root.join("tests/data/parser"), &mut files, true);
    collect_org_files(&root.join("tests/data/emacs-oracle"), &mut files, false);
    collect_org_files(&root.join("docs"), &mut files, false);
    files.push(root.join("README.org"));
    files.push(root.join("CHANGELOG.org"));
    files.sort();
    let mut inputs: Vec<(String, String)> = files
        .into_iter()
        .map(|path| {
            let content = fs::read_to_string(&path).expect("corpus file should be readable");
            (
                path.strip_prefix(root).unwrap().display().to_string(),
                content,
            )
        })
        .collect();
    inputs.extend(adversarial());
    inputs.extend(generated());
    inputs
}

/// Deterministic random line soup from a vocabulary of structural lines, to reach
/// combinations the hand-written cases miss.
fn generated() -> Vec<(String, String)> {
    const LINES: &[&str] = &[
        "* H",
        "** H :t:",
        "*** S",
        "* TODO t",
        "*",
        "  * not",
        "text",
        "text [[x]]",
        "",
        "# c",
        "#",
        ": f",
        ":",
        ":PROPERTIES:",
        ":properties:",
        ":ID: 1",
        ":A+:",
        ":END:",
        ":end:",
        "  :END:",
        ":LOG:",
        "  :PROPERTIES:",
        "#+begin_src",
        "#+end_src",
        "#+begin_quote",
        "#+end_quote",
        "#+begin_verse",
        "#+end_verse",
        "#+begin_center",
        "#+end_center",
        "#+begin_example",
        "#+end_example",
        "#+begin_export html",
        "#+end_export",
        "#+begin_comment",
        "#+end_comment",
        "#+begin_foo x",
        "#+end_foo",
        "  #+begin_src",
        "  #+end_src  ",
        "SCHEDULED: <2024-01-01 Mon>",
        "DEADLINE: <2024-01-02 Tue>",
        "CLOSED: [2024-01-01 Mon 10:00] SCHEDULED: <2024-01-01 Mon>",
        "\tSCHEDULED: <2024-01-01 Mon>",
        "#+TITLE: t",
        "#+k: v",
    ];
    let mut state: u64 = 0x9E37_79B9_7F4A_7C15;
    let mut next = move |bound: usize| {
        state = state
            .wrapping_mul(6_364_136_223_846_793_005)
            .wrapping_add(1_442_695_040_888_963_407);
        ((state >> 33) as usize) % bound
    };
    let cases = std::env::var("STRUCTURE_DIFF_CASES")
        .ok()
        .and_then(|value| value.parse().ok())
        .unwrap_or(1000);
    (0..cases)
        .map(|case| {
            let count = 1 + next(14);
            let newline = if case % 4 == 3 { "\r\n" } else { "\n" };
            let content: String = (0..count)
                .map(|_| format!("{}{newline}", LINES[next(LINES.len())]))
                .collect();
            (format!("generated: {case}"), content)
        })
        .collect()
}

/// Small inputs for placement, nesting and termination rules. Names show up in reports.
fn adversarial() -> Vec<(String, String)> {
    const D: &str = ":PROPERTIES:\n:ID: x\n:END:\n";
    const S: &str = "SCHEDULED: <2024-01-01 Mon>\n";
    let deep: String = (1..=100)
        .map(|level| format!("{} H{level}\n", "*".repeat(level)))
        .collect();
    let cases: Vec<(&str, String)> = vec![
        // Drawer and planning placement, as in property_drawer_placement_matches_org.
        ("file top", format!("{D}#+TITLE: T\n* H\n")),
        ("file after comments", format!("# c\n# d\n{D}* H\n")),
        ("file after blank", format!("\n\n{D}* H\n")),
        ("file blank then comment", format!("\n# c\n{D}* H\n")),
        ("file comment then blank", format!("# c\n\n{D}* H\n")),
        ("file comment planning", format!("# c\n{S}{D}* H\n")),
        ("file after keyword", format!("#+TITLE: T\n{D}* H\n")),
        ("file after text", format!("Intro\n\n{D}* H\n")),
        ("heading direct", format!("* H\n{D}")),
        ("heading after comment", format!("* H\n# c\n{D}")),
        ("heading after blank", format!("* H\n\n{D}")),
        ("heading after text", format!("* H\ntext\n{D}")),
        ("heading in block", format!("* H\n#+begin_quote\n{D}#+end_quote\n")),
        ("heading after planning", format!("* H\n{S}{D}")),
        (
            "heading after closed",
            format!("* H\nCLOSED: [2024-01-01 Mon 10:00]\n{D}"),
        ),
        ("heading two planning lines", format!("* H\n{S}{S}{D}")),
        ("heading planning blank", format!("* H\n{S}\n{D}")),
        ("heading planning comment", format!("* H\n{S}# c\n{D}")),
        ("heading indented planning", format!("* H\n  \t{S}{D}")),
        (
            "heading lowercase planning",
            "* H\nscheduled: <2024-01-01 Mon>\n".into(),
        ),
        (
            "planning not directly after",
            "* H\nfoo\nSCHEDULED: <2024-01-01 Mon>\n".into(),
        ),
        ("planning garbage", format!("* H\nSCHEDULED: garbage\n{D}")),
        ("planning bare keyword", format!("* H\nCLOSED:\n{D}")),
        (
            "planning repeater",
            "* H\nDEADLINE: <2024-01-01 Mon +1w/2d>\n".into(),
        ),
        ("two headings planning", format!("* A\n{S}** B\n{S}{D}* C\n")),
        (
            "lowercase properties",
            "* H\n:properties:\n:ID: 1\n:END:\n".into(),
        ),
        ("empty property drawer", "* H\n:PROPERTIES:\n:END:\n".into()),
        (
            "property drawer unclosed",
            "* H\n:PROPERTIES:\n:ID: 1\n* I\n:PROPERTIES:\n:A: b\n:END:\n".into(),
        ),
        (
            "property rows",
            "* H\n:PROPERTIES:\n:EMPTY:\n:A+: x\n  :B:   y  \nnot a row\n:END:\n".into(),
        ),
        (
            "indented drawer",
            "* H\n  :PROPERTIES:\n  :ID: x\n  :END:\n".into(),
        ),
        // Drawers.
        ("drawer end lowercase", "* H\n:LOG:\nx\n:end:\n".into()),
        ("drawer end with text", "* H\n:LOG:\nx\n:END: y\n:END:\n".into()),
        ("drawer hyphen", "* H\n:a-b_c1:\nx\n:END:\n".into()),
        ("drawer unicode", "* H\n:äö:\nx\n:END:\n".into()),
        ("drawer name with space", "* H\n:A B:\ny\n:END:\n".into()),
        ("drawer empty name", "* H\n::\ny\n:END:\n:::\nz\n:END:\n".into()),
        ("stray end", "* H\n:END:\nx\n:END:\n".into()),
        ("affiliated before drawer", "* H\n#+NAME: d\n:LOG:\nx\n:END:\n".into()),
        ("affiliated before keyword", "#+NAME: n\n#+TITLE: t\n".into()),
        ("affiliated before stray end", "#+NAME: n\n#+END_SRC\n".into()),
        ("affiliated before unclosed block", "#+NAME: n\n#+begin_src\nx\n".into()),
        ("affiliated before comment", "#+NAME: n\n# c\n# d\n".into()),
        ("affiliated dangling", "* H\n#+NAME: x\n\n#+CAPTION: c\n".into()),
        ("affiliated chain", "* H\n#+NAME: x\n#+CAPTION[s]: c\n#+ATTR_HTML: :a b\n: fixed\n".into()),
        ("unclosed drawer", "* H\n:LOG:\nx\n* I\n:END:\n".into()),
        ("drawer in drawer", "* H\n:A:\n:B:\nx\n:END:\n:END:\n".into()),
        ("drawer after text", "* H\ntext\n:LOG:\nx\n:END:\n".into()),
        (
            "drawer end outside quote",
            "* H\n#+begin_quote\n:D:\nx\n#+end_quote\n:END:\n".into(),
        ),
        (
            "quote in drawer",
            "* H\n:A:\n#+begin_quote\nx\n:END:\n#+end_quote\n".into(),
        ),
        (
            "drawer in center",
            "* H\n#+begin_center\n:D:\nx\n:END:\n#+end_center\n".into(),
        ),
        // Blocks.
        ("src with stars", "* H\n#+begin_src org\n* inside\n#+end_src\n".into()),
        (
            "example with stars",
            "* H\n#+begin_example\n* inside\n#+end_example\nafter\n".into(),
        ),
        (
            "quote with stars",
            "* H\n#+begin_quote\n* inside\n#+end_quote\n".into(),
        ),
        ("drawer with stars", "* H\n:LOG:\n* inside\n:END:\n".into()),
        (
            "comment block with stars",
            "* H\n#+begin_comment\n* inside\n#+end_comment\n".into(),
        ),
        (
            "unclosed src",
            "* H\n#+begin_src\n:D:\nx\n:END:\n* I\n".into(),
        ),
        (
            "unclosed quote",
            "* H\n#+begin_quote\n#+begin_src\nx\n#+end_src\n".into(),
        ),
        (
            "block name mismatch",
            "* H\n#+begin_src a\nx\n#+end_example\n#+END_SRC\nz\n".into(),
        ),
        (
            "block name case",
            "* H\n#+begin_SRC\n#+End_src\n#+BEGIN_Quote\nq\n#+end_QUOTE\n".into(),
        ),
        ("block args", "* H\n#+begin_x y z\n#+end_x q\n#+end_x\n".into()),
        ("empty block name", "* H\n#+begin_\nx\n#+end_\n".into()),
        (
            "nested end name",
            "* H\n#+begin_quote\n#+begin_src\n#+end_quote\n#+end_src\n#+end_quote\n".into(),
        ),
        (
            "verse raw",
            "* H\n#+begin_verse\n:D:\nx\n:END:\n#+end_verse\n".into(),
        ),
        (
            "special block",
            "* H\n#+begin_foo\n# c\n#+K: v\n#+end_foo\n#+begin_justify\nj\n#+end_justify\n".into(),
        ),
        (
            "export block",
            "* H\n#+begin_export html\n<b>\n#+end_export\n".into(),
        ),
        (
            "indented blocks",
            "* H\n  #+BEGIN_SRC x\n  :a:\n  #+END_SRC  \n".into(),
        ),
        (
            "unclosed block run",
            "* H\n#+begin_x\n#+begin_x\n#+begin_x\nend\n".into(),
        ),
        // Keywords, comments, fixed-width, headline forms, encodings.
        (
            "keywords",
            "#+TITLE: T\n#+KEY:value\n#+KEY2:\n#+a b: c\n#+: x\n\t #+key4: w\n#+key5\n* H\n#+K: v\n"
                .into(),
        ),
        (
            "affiliated keywords",
            "* H\n#+NAME: x\n#+begin_src\nx\n#+end_src\n#+CAPTION: c\ntext\n".into(),
        ),
        (
            "dynamic block",
            "* H\n#+BEGIN: clocktable\n#+END:\n#+BEGIN: bar\n".into(),
        ),
        (
            "comments and fixed",
            "# a\n# b\n#c\n: x\n: y\n:\n#\n* H\n  # i\n  : j\n".into(),
        ),
        (
            "headline forms",
            "* \n*  \n* a\n  * indented\n*\n**\n*\tx\n**x\n".into(),
        ),
        ("headline only star space", "* ".into()),
        ("no final newline", "* H\n:PROPERTIES:\n:A: b\n:END:".into()),
        ("empty", String::new()),
        ("only newline", "\n".into()),
        (
            "crlf",
            "#+TITLE: T\r\n* H :t:\r\nSCHEDULED: <2024-01-01 Mon>\r\n:PROPERTIES:\r\n:ID: 1\r\n:END:\r\n#+begin_src\r\n* x\r\n#+end_src\r\n** I\r\n"
                .into(),
        ),
        (
            "multibyte",
            "* Überschrift ä :tag:\n:PROPERTIES:\n:ÄÖ: ü\n:END:\n#+begin_src\n日本語 [[x]]\n#+end_src\n** 日本\n"
                .into(),
        ),
        (
            "tags and todo",
            "* TODO [#A] Task :a:b:\n* DONE x\n** Not :a: :b:\n* T ::a::\n".into(),
        ),
        ("level skip", "* A\n*** C\n** B\n**** D\n* E\n".into()),
        ("deep", deep),
        ("too deep", format!("{} x\n", "*".repeat(101))),
        (
            "too deep in src",
            format!("#+begin_src\n{} x\n#+end_src\n", "*".repeat(101)),
        ),
        (
            "deep siblings",
            format!("{} a\n{} b\n", "*".repeat(100), "*".repeat(100)),
        ),
    ];
    cases
        .into_iter()
        .map(|(name, content)| (format!("adversarial: {name}"), content))
        .collect()
}

#[test]
fn scanner_matches_adapter_on_corpus() {
    let report = std::env::var_os("STRUCTURE_DIFF_REPORT").is_some();
    let mut unexplained = Vec::new();
    let mut seen_allowed = HashSet::new();
    for (name, content) in corpus() {
        for found in differences(&content) {
            if report {
                println!("[{name}] {}: {}", found.category, found.detail);
                if std::env::var("STRUCTURE_DIFF_REPORT").is_ok_and(|value| value == "2") {
                    println!("    input: {content:?}");
                }
            }
            if let Some(entry) = ALLOWED
                .iter()
                .find(|(input, category)| *input == name && *category == found.category)
            {
                seen_allowed.insert(*entry);
            } else {
                unexplained.push(format!("[{name}] {}: {}", found.category, found.detail));
            }
        }
    }
    if !report {
        assert!(
            unexplained.is_empty(),
            "unexplained differences:\n{}",
            unexplained.join("\n")
        );
        for entry in ALLOWED {
            assert!(
                seen_allowed.contains(entry),
                "allow-list entry {entry:?} is never hit"
            );
        }
    }
}
