//! Regression corpus for the production parse path (#103). It replaces the differential test
//! of the scanner stage (#101): the adapter is the scanner now, so the corpus is checked
//! against invariants instead of against Orgize. Every input, hand-written adversarial
//! cases, random line soup over the structural vocabulary, the parser fixtures and the docs,
//! must parse without a panic (this runs in a debug build, where Orgize asserts on an empty
//! quote block), keep every range on the source, and never hand Orgize any structure.
//! Facts that Emacs decides are checked against Emacs in `scripts/emacs-oracle.py`.
//! Set `ADAPTER_CORPUS_CASES` for more random inputs.

use std::{fs, path::Path};

use super::line_lexer::{classify_line, lines, LineClass};
use super::model::{OrgParser, ParsedOrgDocument};
use super::orgize_inline::blanked_body_text;
use super::structure_scanner::scan_structure;
use super::{OrgizeAdapter, ParseOptions};

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
    let cases = std::env::var("ADAPTER_CORPUS_CASES")
        .ok()
        .and_then(|value| value.parse().ok())
        .unwrap_or(1500);
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

fn parse(content: &str) -> Result<ParsedOrgDocument, String> {
    OrgizeAdapter::new()
        .parse_document(Path::new("t.org"), content, &ParseOptions::default())
        .map_err(|diagnostic| diagnostic.message)
}

fn check_range(content: &str, what: &str, start: usize, end: usize) {
    assert!(
        start <= end && end <= content.len(),
        "{what}: {start}..{end} in {} bytes",
        content.len()
    );
    assert!(
        content.is_char_boundary(start) && content.is_char_boundary(end),
        "{what}: {start}..{end} splits a character"
    );
}

#[test]
fn parsed_documents_keep_their_ranges_on_the_source() {
    for (name, content) in corpus() {
        let document = match parse(&content) {
            Ok(document) => document,
            Err(message) => {
                assert!(
                    message.contains("exceeds the supported maximum"),
                    "[{name}] {message}"
                );
                continue;
            }
        };
        for (index, heading) in document.headings.iter().enumerate() {
            let at = format!("[{name}] heading {index}");
            check_range(&content, &at, heading.byte_start, heading.byte_end);
            if let Some(parent) = heading.parent_index {
                let parent = &document.headings[parent];
                assert!(
                    parent.byte_start <= heading.byte_start && heading.byte_end <= parent.byte_end,
                    "{at} is not inside its parent"
                );
            }
            if let (Some(start), Some(end)) = (heading.body_byte_start, heading.body_byte_end) {
                check_range(&content, &at, start, end);
                assert!(
                    heading.byte_start <= start && end <= heading.byte_end,
                    "{at} body"
                );
                assert_eq!(
                    Some(&content[start..end]),
                    heading.body_text.as_deref(),
                    "{at}"
                );
            }
            for timestamp in &heading.timestamps {
                check_range(&content, &at, timestamp.byte_start, timestamp.byte_end);
                assert_eq!(
                    &content[timestamp.byte_start..timestamp.byte_end],
                    timestamp.raw_value,
                    "{at}"
                );
                if index > 0 {
                    assert!(
                        heading.byte_start <= timestamp.byte_start
                            && timestamp.byte_end <= heading.byte_end,
                        "{at} timestamp outside the heading"
                    );
                }
            }
        }
        for link in &document.links {
            check_range(
                &content,
                &format!("[{name}] link"),
                link.byte_start,
                link.byte_end,
            );
        }
    }
}

/// Orgize only ever sees text in which every structural line is blanked or defused.
#[test]
fn orgize_never_sees_structure() {
    for (name, content) in corpus() {
        let Ok(structure) = scan_structure(&content) else {
            continue;
        };
        let blanked = blanked_body_text(&content, &structure);
        assert_eq!(blanked.len(), content.len(), "[{name}]");
        assert_eq!(lines(&blanked).count(), lines(&content).count(), "[{name}]");
        for line in lines(&blanked) {
            let text = line.text(&blanked);
            let class = classify_line(text);
            assert!(
                !matches!(
                    class,
                    LineClass::Headline { .. }
                        | LineClass::BlockBegin { .. }
                        | LineClass::BlockEnd { .. }
                        | LineClass::DynBlockBegin { .. }
                        | LineClass::Keyword { .. }
                        | LineClass::Drawer { .. }
                        | LineClass::DrawerEnd
                ),
                "[{name}] {text:?} is structure"
            );
            // Orgize also takes `*` and a tab for a headline.
            let stars = text.bytes().take_while(|byte| *byte == b'*').count();
            assert!(
                stars == 0 || !matches!(text.as_bytes().get(stars), None | Some(b'\t' | b' ')),
                "[{name}] {text:?} is a headline for Orgize"
            );
        }
    }
}
