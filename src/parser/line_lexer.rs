//! Stage A of the Orgize replacement (#101): a pure, per-line classifier.
//!
//! Every function here looks at one line only. Anything that needs context (is this
//! `:NAME:` a drawer or a property row, is the block closed) belongs to
//! `structure_scanner`. Rules and Emacs references: `docs/design/line-scanner.org`.

use std::ops::Range;

/// One physical line of the source, as byte offsets into the whole buffer.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Line {
    /// First byte of the line.
    pub start: usize,
    /// End of the text, before `\n` or `\r\n`.
    pub end: usize,
    /// First byte of the next line (`content.len()` after the last line).
    pub next: usize,
}

impl Line {
    pub fn text(self, content: &str) -> &str {
        &content[self.start..self.end]
    }
}

/// Splits `content` into lines. A trailing newline does not start an empty line. Only
/// `\n` separates lines; a `\r` right before it is not part of the text.
pub fn lines(content: &str) -> impl Iterator<Item = Line> + '_ {
    let bytes = content.as_bytes();
    let mut start = 0;
    std::iter::from_fn(move || {
        if start >= bytes.len() {
            return None;
        }
        let (mut end, next) = match bytes[start..].iter().position(|byte| *byte == b'\n') {
            Some(offset) => (start + offset, start + offset + 1),
            None => (bytes.len(), bytes.len()),
        };
        if end > start && bytes[end - 1] == b'\r' {
            end -= 1;
        }
        let line = Line { start, end, next };
        start = next;
        Some(line)
    })
}

/// Class of one line. Ranges are relative to the line text.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum LineClass {
    /// `^\*+ `, the only headline form Org recognises (a bare `*` or `*\t` is text).
    Headline {
        level: usize,
    },
    /// Starts with `SCHEDULED:`, `DEADLINE:` or `CLOSED:` (any case) after blanks.
    Planning,
    /// `:NAME:` alone on the line. Stage B decides between drawer and property row.
    Drawer {
        name: Range<usize>,
    },
    /// `:END:` (any case) alone on the line.
    DrawerEnd,
    /// `#+begin_NAME` with optional arguments.
    BlockBegin {
        name: Range<usize>,
    },
    /// `#+end_NAME` alone on the line.
    BlockEnd {
        name: Range<usize>,
    },
    /// `#+BEGIN: name` with optional arguments, the start of a dynamic block. Without a name
    /// it is a keyword line. The end is `#+END:` or `#+END`, checked by the scanner.
    DynBlockBegin {
        name: Range<usize>,
    },
    /// `#+KEY:` with optional value. `BEGIN_x` and `END_x` lines are block lines first.
    Keyword {
        key: Range<usize>,
        value: Range<usize>,
    },
    /// `#` followed by a space or the line end.
    Comment,
    /// `:` followed by a space or the line end.
    FixedWidth,
    /// Only spaces and tabs.
    Empty,
    Text,
}

const fn is_blank(byte: u8) -> bool {
    byte == b' ' || byte == b'\t'
}

fn is_drawer_name_char(c: char) -> bool {
    c.is_alphanumeric() || c == '_' || c == '-'
}

/// Classifies one line without its line terminator.
pub fn classify_line(line: &str) -> LineClass {
    let bytes = line.as_bytes();
    if bytes.first() == Some(&b'*') {
        let stars = bytes.iter().take_while(|byte| **byte == b'*').count();
        if bytes.get(stars) == Some(&b' ') {
            return LineClass::Headline { level: stars };
        }
    }
    let indent = bytes.iter().take_while(|byte| is_blank(**byte)).count();
    let rest = &line[indent..];
    if rest.is_empty() {
        return LineClass::Empty;
    }
    let after = |prefix: usize| &rest[prefix..];
    match rest.as_bytes()[0] {
        b'#' => {
            if rest.len() == 1 || rest.as_bytes()[1] == b' ' {
                LineClass::Comment
            } else if rest.as_bytes()[1] == b'+' {
                classify_hash_plus(rest, indent)
            } else {
                LineClass::Text
            }
        }
        b':' => {
            if rest.len() == 1 || rest.as_bytes()[1] == b' ' {
                return LineClass::FixedWidth;
            }
            let body = after(1);
            let Some(close) = body.find(':') else {
                return LineClass::Text;
            };
            let name = &body[..close];
            let tail = &body[close + 1..];
            if name.is_empty()
                || !name.chars().all(is_drawer_name_char)
                || !tail.bytes().all(is_blank)
            {
                return LineClass::Text;
            }
            if name.eq_ignore_ascii_case("END") {
                LineClass::DrawerEnd
            } else {
                LineClass::Drawer {
                    name: indent + 1..indent + 1 + close,
                }
            }
        }
        _ if starts_with_planning_keyword(rest) => LineClass::Planning,
        _ => LineClass::Text,
    }
}

fn starts_with_planning_keyword(rest: &str) -> bool {
    ["SCHEDULED:", "DEADLINE:", "CLOSED:"]
        .iter()
        .any(|keyword| {
            rest.get(..keyword.len())
                .is_some_and(|head| head.eq_ignore_ascii_case(keyword))
        })
}

/// `rest` starts with `#+`; `indent` is the width of the blanks stripped before it.
fn classify_hash_plus(rest: &str, indent: usize) -> LineClass {
    let body = &rest[2..];
    let base = indent + 2;
    for (prefix, is_begin) in [("begin_", true), ("end_", false)] {
        let Some(head) = body.get(..prefix.len()) else {
            continue;
        };
        if !head.eq_ignore_ascii_case(prefix) {
            continue;
        }
        let name_len = body[prefix.len()..]
            .bytes()
            .take_while(|byte| !byte.is_ascii_whitespace())
            .count();
        if name_len == 0 {
            continue;
        }
        let name = base + prefix.len()..base + prefix.len() + name_len;
        let tail = &body[prefix.len() + name_len..];
        if is_begin {
            // Arguments are free text, so anything after the name is accepted.
            return LineClass::BlockBegin { name };
        }
        if tail.bytes().all(is_blank) {
            return LineClass::BlockEnd { name };
        }
        return LineClass::Text;
    }
    if body
        .get(.."begin:".len())
        .is_some_and(|head| head.eq_ignore_ascii_case("begin:"))
    {
        let after = &body["begin:".len()..];
        let blanks = after.bytes().take_while(|byte| is_blank(*byte)).count();
        let name_len = after[blanks..]
            .bytes()
            .take_while(|byte| !byte.is_ascii_whitespace())
            .count();
        if name_len > 0 {
            let start = base + "begin:".len() + blanks;
            return LineClass::DynBlockBegin {
                name: start..start + name_len,
            };
        }
    }
    // `#+KEY:` with a key of at least one non-blank character.
    let key_len = body
        .bytes()
        .take_while(|byte| !byte.is_ascii_whitespace() && *byte != b':')
        .count();
    if key_len == 0 || body.as_bytes().get(key_len) != Some(&b':') {
        return LineClass::Text;
    }
    let after_colon = &body[key_len + 1..];
    let skipped = after_colon
        .bytes()
        .take_while(|byte| is_blank(*byte))
        .count();
    let value_start = base + key_len + 1 + skipped;
    LineClass::Keyword {
        key: base..base + key_len,
        value: value_start..indent + rest.len(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn kind(line: &str) -> String {
        let class = classify_line(line);
        let text = |range: &Range<usize>| &line[range.clone()];
        match &class {
            LineClass::Headline { level } => format!("headline {level}"),
            LineClass::Drawer { name } => format!("drawer {}", text(name)),
            LineClass::BlockBegin { name } => format!("begin {}", text(name)),
            LineClass::BlockEnd { name } => format!("end {}", text(name)),
            LineClass::DynBlockBegin { name } => format!("dynamic {}", text(name)),
            LineClass::Keyword { key, value } => format!("keyword {}={}", text(key), text(value)),
            other => format!("{other:?}").to_lowercase(),
        }
    }

    #[test]
    fn classifies_lines_like_org() {
        let table: &[(&str, &str)] = &[
            ("* H", "headline 1"),
            ("*** ", "headline 3"),
            ("*", "text"),
            ("*\tH", "text"),
            ("**H", "text"),
            (" * H", "text"),
            ("", "empty"),
            (" \t", "empty"),
            ("SCHEDULED: <2024-01-01>", "planning"),
            ("  \tscheduled: x", "planning"),
            ("Closed:", "planning"),
            ("SCHEDULEDX: x", "text"),
            ("SCHEDULED", "text"),
            (":PROPERTIES:", "drawer PROPERTIES"),
            (" :a-b_ä1: \t", "drawer a-b_ä1"),
            (":END:", "drawerend"),
            ("  :end:  ", "drawerend"),
            (":END: x", "text"),
            (":A B:", "text"),
            ("::", "text"),
            (":::", "text"),
            (":x:y:", "text"),
            (":ID: value", "text"),
            (": fixed", "fixedwidth"),
            (":", "fixedwidth"),
            ("#", "comment"),
            ("  # note", "comment"),
            ("#note", "text"),
            ("#+begin_src rust", "begin src"),
            ("  #+BEGIN_Quote", "begin Quote"),
            ("#+begin_", "text"),
            ("#+begin_x:", "begin x:"),
            ("#+end_src", "end src"),
            ("#+END_SRC \t", "end SRC"),
            ("#+end_src x", "text"),
            ("#+end_", "text"),
            ("#+TITLE: A b", "keyword TITLE=A b"),
            ("#+KEY:value", "keyword KEY=value"),
            ("  #+key:", "keyword key="),
            ("#+a:b: c", "keyword a=b: c"),
            ("#+a b: c", "text"),
            ("#+: x", "text"),
            ("#+key", "text"),
            ("#+BEGIN: clocktable", "dynamic clocktable"),
            ("#+begin:x :a b", "dynamic x"),
            ("  #+BEGIN:\t clocktable", "dynamic clocktable"),
            ("#+BEGIN:", "keyword BEGIN="),
            ("#+BEGIN:  ", "keyword BEGIN="),
            ("#+END:", "keyword END="),
            ("text", "text"),
            ("ä: ü", "text"),
        ];
        for (line, expected) in table {
            assert_eq!(kind(line), *expected, "line {line:?}");
        }
    }

    #[test]
    fn splits_lines_with_offsets_and_crlf() {
        let content = "a\r\nb\n\nlast";
        let got: Vec<_> = lines(content)
            .map(|line| (line.start, line.end, line.next))
            .collect();
        assert_eq!(got, [(0, 1, 3), (3, 4, 5), (5, 5, 6), (6, 10, 10)]);
        assert_eq!(lines("x\n").count(), 1);
        assert_eq!(lines("").count(), 0);
        assert_eq!(lines("ä\r\n").next().unwrap().text("ä\r\n"), "ä");
    }
}
