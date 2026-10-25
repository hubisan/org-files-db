//! Locates the matched text of a search hit in the source file.
//!
//! FTS5 `highlight()` marks every matched token run with the query's own
//! tokenizer and phrase, prefix and NEAR semantics. The marked offsets in the
//! indexed title or body are then mapped to source positions through
//! `heading_text_segments`.

use std::collections::HashMap;

use rusqlite::{params, Connection};
use serde::Serialize;

const OPEN_MARKER: &str = "\u{E000}\u{1}";
const CLOSE_MARKER: &str = "\u{1}\u{E001}";
/// Snippets of long lines keep this many characters before the match.
const SNIPPET_CONTEXT_BEFORE: usize = 60;
/// Longest snippet in characters, unless the match itself is longer.
const SNIPPET_MAX_CHARS: usize = 200;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize)]
#[serde(rename_all = "kebab-case")]
pub(crate) enum OccurrenceField {
    Title,
    Body,
}

/// One matched token run. Positions are `null` when the indexed text has no source position
/// (a file title taken from the file name).
#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub(crate) struct SearchOccurrence {
    pub(crate) field: OccurrenceField,
    /// 1-based source line of the match start.
    pub(crate) line: Option<u32>,
    /// 0-based character column of the match start in its source line.
    pub(crate) column: Option<u32>,
    /// UTF-8 byte offsets of the match in the source file.
    pub(crate) byte_start: Option<usize>,
    pub(crate) byte_end: Option<usize>,
    /// The indexed text line that holds the match, shortened around the match when long.
    pub(crate) snippet: String,
    /// Character offsets of the match in `snippet`.
    pub(crate) snippet_start: usize,
    pub(crate) snippet_end: usize,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct Segment {
    text_start: usize,
    byte_start: usize,
    line: u32,
    column: u32,
}

/// Occurrences per heading id for every row that `expression` matches.
pub(crate) fn load_occurrences(
    connection: &Connection,
    expression: &str,
    heading_ids: &[i64],
) -> rusqlite::Result<HashMap<i64, Vec<SearchOccurrence>>> {
    let mut highlight = connection.prepare(
        "SELECT highlight(heading_fts, 0, ?2, ?3), highlight(heading_fts, 1, ?2, ?3)
         FROM heading_fts
         WHERE heading_fts MATCH ?1 AND rowid = ?4",
    )?;
    let mut segments = connection.prepare(
        "SELECT field, text_start, byte_start, line, column_number
         FROM heading_text_segments
         WHERE heading_id = ?1
         ORDER BY field, text_start",
    )?;
    let mut result = HashMap::with_capacity(heading_ids.len());
    for &heading_id in heading_ids {
        let marked = highlight
            .query_row(
                params![expression, OPEN_MARKER, CLOSE_MARKER, heading_id],
                |row| {
                    Ok((
                        row.get::<_, Option<String>>(0)?,
                        row.get::<_, Option<String>>(1)?,
                    ))
                },
            )
            .map(Some)
            .or_else(|error| match error {
                rusqlite::Error::QueryReturnedNoRows => Ok(None),
                other => Err(other),
            })?;
        let Some((title, body)) = marked else {
            result.insert(heading_id, Vec::new());
            continue;
        };
        let mut title_segments = Vec::new();
        let mut body_segments = Vec::new();
        let rows = segments.query_map([heading_id], |row| {
            Ok((
                row.get::<_, i64>(0)?,
                Segment {
                    text_start: row.get::<_, i64>(1)? as usize,
                    byte_start: row.get::<_, i64>(2)? as usize,
                    line: row.get::<_, i64>(3)? as u32,
                    column: row.get::<_, i64>(4)? as u32,
                },
            ))
        })?;
        for row in rows {
            let (field, segment) = row?;
            if field == 0 {
                title_segments.push(segment);
            } else {
                body_segments.push(segment);
            }
        }
        let mut occurrences = Vec::new();
        if let Some(title) = title {
            occurrences.extend(occurrences_in(
                OccurrenceField::Title,
                &title,
                &title_segments,
            ));
        }
        if let Some(body) = body {
            occurrences.extend(occurrences_in(OccurrenceField::Body, &body, &body_segments));
        }
        result.insert(heading_id, occurrences);
    }
    Ok(result)
}

/// Splits `marked` into the plain text and the byte ranges between the markers.
fn strip_markers(marked: &str) -> (String, Vec<(usize, usize)>) {
    let mut text = String::with_capacity(marked.len());
    let mut ranges = Vec::new();
    let mut rest = marked;
    let mut open = None;
    loop {
        let next_open = rest.find(OPEN_MARKER);
        let next_close = rest.find(CLOSE_MARKER);
        let (index, is_open) = match (next_open, next_close, open.is_some()) {
            (_, Some(close), true) => (close, false),
            (Some(start), _, false) => (start, true),
            _ => {
                text.push_str(rest);
                break;
            }
        };
        text.push_str(&rest[..index]);
        if is_open {
            open = Some(text.len());
            rest = &rest[index + OPEN_MARKER.len()..];
        } else {
            if let Some(start) = open.take() {
                if start < text.len() {
                    ranges.push((start, text.len()));
                }
            }
            rest = &rest[index + CLOSE_MARKER.len()..];
        }
    }
    (text, ranges)
}

fn occurrences_in(
    field: OccurrenceField,
    marked: &str,
    segments: &[Segment],
) -> Vec<SearchOccurrence> {
    let (text, ranges) = strip_markers(marked);
    ranges
        .into_iter()
        .map(|(start, end)| {
            let position = source_position(&text, segments, start, false);
            let byte_end = source_position(&text, segments, end, true).map(|(byte, _, _)| byte);
            let (snippet, snippet_start, snippet_end) = snippet(&text, start, end);
            SearchOccurrence {
                field,
                line: position.map(|(_, line, _)| line),
                column: position.map(|(_, _, column)| column),
                byte_start: position.map(|(byte, _, _)| byte),
                byte_end,
                snippet,
                snippet_start,
                snippet_end,
            }
        })
        .collect()
}

/// Source byte, line and column of the text byte `offset`. A match end (`is_end`) at the end
/// of a piece stays in that piece instead of jumping to the start of the next one.
fn source_position(
    text: &str,
    segments: &[Segment],
    offset: usize,
    is_end: bool,
) -> Option<(usize, u32, u32)> {
    let index = segments
        .partition_point(|segment| {
            segment.text_start < offset || (!is_end && segment.text_start == offset)
        })
        .checked_sub(1)?;
    let segment = segments[index];
    if offset < segment.text_start {
        return None;
    }
    let before = &text[segment.text_start..offset];
    let newlines = before.matches('\n').count() as u32;
    let column = match before.rfind('\n') {
        Some(newline) => before[newline + 1..].chars().count() as u32,
        None => segment.column + before.chars().count() as u32,
    };
    Some((
        segment.byte_start + (offset - segment.text_start),
        segment.line + newlines,
        column,
    ))
}

/// The text line that holds `start..end`, shortened to `SNIPPET_MAX_CHARS` around the match,
/// with the match as character offsets.
fn snippet(text: &str, start: usize, end: usize) -> (String, usize, usize) {
    let line_start = text[..start].rfind('\n').map_or(0, |index| index + 1);
    let line_end = text[end..]
        .find('\n')
        .map_or(text.len(), |index| end + index);
    let line = text[line_start..line_end].trim_end_matches('\r');
    let match_start = text[line_start..start].chars().count();
    let match_end = match_start
        + text[start..end.min(line_start + line.len())]
            .chars()
            .count();
    let total = line.chars().count();
    if total <= SNIPPET_MAX_CHARS {
        return (line.to_string(), match_start, match_end);
    }
    let first = match_start.saturating_sub(SNIPPET_CONTEXT_BEFORE);
    let last = (first + SNIPPET_MAX_CHARS).max(match_end).min(total);
    let shortened = line.chars().skip(first).take(last - first).collect();
    (shortened, match_start - first, match_end - first)
}

#[cfg(test)]
mod tests {
    use super::{snippet, source_position, strip_markers, Segment, CLOSE_MARKER, OPEN_MARKER};

    fn mark(text: &str) -> String {
        text.replace('[', OPEN_MARKER).replace(']', CLOSE_MARKER)
    }

    #[test]
    fn markers_become_byte_ranges_of_the_plain_text() {
        let (text, ranges) = strip_markers(&mark("a [test] and [Grüße] x"));
        assert_eq!(text, "a test and Grüße x");
        assert_eq!(ranges, vec![(2, 6), (11, 18)]);
    }

    #[test]
    fn positions_follow_segments_lines_and_characters() {
        // Body "ä x\n\nnach" from two source pieces: line 12 and, after metadata, line 16.
        let text = "ä x\n\nnach";
        let segments = [
            Segment {
                text_start: 0,
                byte_start: 100,
                line: 12,
                column: 0,
            },
            Segment {
                text_start: 6,
                byte_start: 140,
                line: 16,
                column: 0,
            },
        ];
        assert_eq!(
            source_position(text, &segments, 3, false),
            Some((103, 12, 2))
        );
        assert_eq!(
            source_position(text, &segments, 6, false),
            Some((140, 16, 0))
        );
        assert_eq!(
            source_position(text, &segments, 8, false),
            Some((142, 16, 2))
        );
        // A match end at the end of the first piece stays in it.
        assert_eq!(
            source_position(text, &segments, 4, true),
            Some((104, 12, 3))
        );
        assert_eq!(source_position(text, &[], 0, false), None);
    }

    #[test]
    fn title_positions_add_the_segment_column() {
        let segments = [Segment {
            text_start: 0,
            byte_start: 52,
            line: 7,
            column: 7,
        }];
        assert_eq!(
            source_position("TODO Task", &segments, 5, false),
            Some((57, 7, 12))
        );
    }

    #[test]
    fn snippets_are_the_matched_line_and_shorten_long_lines() {
        assert_eq!(
            snippet("one\ntwo test\nthree", 8, 12),
            ("two test".to_string(), 4, 8)
        );
        let long = format!("{}test{}", "a".repeat(300), "b".repeat(300));
        let (text, start, end) = snippet(&long, 300, 304);
        assert_eq!(text.chars().count(), 200);
        assert_eq!(&text[start..end], "test");
        assert_eq!(start, 60);
    }
}
