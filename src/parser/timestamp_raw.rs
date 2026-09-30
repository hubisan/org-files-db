//! Orgize-free parsing of raw timestamp and planning-line text.

use std::{collections::HashSet, ops::Range};

use super::line_index::LineIndex;
use super::model::{
    ParsedHeading, ParsedTimestamp, ParsedTimestampModifier, ParsedTimestampModifierKind,
    ParsedTimestampModifierType, ParsedTimestampRangeType, ParsedTimestampRole,
    ParsedTimestampType, ParsedTimestampUnit,
};

/// Reads the planning line from the source text when Orgize did not build a planning node
/// (repeater deadlines and diary sexps such as `SCHEDULED: <%%(diary-float t 42)>`). Returns
/// the byte range of that line, newline included, when it holds at least one planning entry.
pub(super) fn populate_text_planning_fallback(
    content: &str,
    lines: &LineIndex,
    parsed: &mut ParsedHeading,
    seen_ranges: &mut HashSet<(usize, usize)>,
) -> Option<Range<usize>> {
    let heading_text = content.get(parsed.byte_start..parsed.byte_end)?;
    let first_newline = heading_text.find('\n')?;
    let after_heading = &heading_text[first_newline + 1..];
    let planning_line = after_heading
        .split_once('\n')
        .map(|(line, _)| line)
        .unwrap_or(after_heading);

    if !planning_line.contains('/')
        && !planning_line.contains("%%(")
        && !starts_with_planning_keyword(planning_line)
    {
        return None;
    }

    let line_offset = parsed.byte_start + first_newline + 1;
    let entries = parse_planning_fallback_entries(planning_line);
    if entries.is_empty() {
        return None;
    }
    let mut line_end = line_offset + planning_line.len();
    if content.as_bytes().get(line_end) == Some(&b'\n') {
        line_end += 1;
    }
    for (role, raw_value, relative_start) in entries {
        let byte_start = line_offset + relative_start;
        let byte_end = byte_start + raw_value.len();
        let (start_ts, end_ts, range_type) = normalize_raw_timestamp_bounds(&raw_value);
        let parsed_timestamp = ParsedTimestamp {
            role: Some(role),
            raw_value: raw_value.clone(),
            timestamp_type: timestamp_type_from_raw(&raw_value)
                .unwrap_or(ParsedTimestampType::Active),
            range_type,
            has_time: raw_timestamp_has_explicit_time(&raw_value),
            start_ts,
            end_ts,
            byte_start,
            byte_end,
            line_number: Some(lines.line_for(byte_start)),
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
    Some(line_offset..line_end)
}

/// Org matches the planning keywords case-insensitively (`scheduled:`, `Deadline:`).
fn strip_planning_keyword<'a>(text: &'a str, keyword: &str) -> Option<&'a str> {
    let head = text.get(..keyword.len())?;
    head.eq_ignore_ascii_case(keyword)
        .then(|| &text[keyword.len()..])
}

fn starts_with_planning_keyword(line: &str) -> bool {
    let trimmed = line.trim_start();
    ["SCHEDULED:", "DEADLINE:", "CLOSED:"]
        .iter()
        .any(|keyword| strip_planning_keyword(trimmed, keyword).is_some())
}

fn parse_planning_fallback_entries(line: &str) -> Vec<(ParsedTimestampRole, String, usize)> {
    let mut entries = Vec::new();
    let mut offset = 0;

    while offset < line.len() {
        let remaining = &line[offset..];
        let trimmed = remaining.trim_start();
        let leading_ws = remaining.len() - trimmed.len();
        let entry_offset = offset + leading_ws;

        let (role, rest) = if let Some(rest) = strip_planning_keyword(trimmed, "SCHEDULED:") {
            (ParsedTimestampRole::Scheduled, rest)
        } else if let Some(rest) = strip_planning_keyword(trimmed, "DEADLINE:") {
            (ParsedTimestampRole::Deadline, rest)
        } else if let Some(rest) = strip_planning_keyword(trimmed, "CLOSED:") {
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

pub(super) fn extract_first_raw_timestamp(text: &str) -> Option<(usize, String)> {
    for (index, character) in text.char_indices() {
        if character == '<' {
            if let Some(length) = diary_timestamp_length(&text[index..]) {
                return Some((index, text[index..index + length].to_string()));
            }
        }
        let closing = match character {
            '<' => '>',
            '[' => ']',
            _ => continue,
        };
        let rest = &text[index + 1..];
        if !starts_with_iso_date(rest) {
            continue;
        }
        let line_end = rest.find('\n').unwrap_or(rest.len());
        let Some(close) = rest[..line_end].find(closing) else {
            continue;
        };
        let end = index + 1 + close + closing.len_utf8();
        return Some((index, text[index..end].to_string()));
    }
    None
}

/// Length of a diary sexp timestamp `<%%(SEXP)>` at the start of `text`. Org (`org-element`):
/// the sexp is non-empty, holds no `>` or newline, and the first `>` closes it after `)`.
fn diary_timestamp_length(text: &str) -> Option<usize> {
    let rest = text.strip_prefix("<%%(")?;
    let close = rest.find(['>', '\n'])?;
    let sexp = rest[..close].strip_suffix(')')?;
    if sexp.is_empty() || rest.as_bytes()[close] != b'>' {
        return None;
    }
    Some("<%%(".len() + close + 1)
}

fn starts_with_iso_date(text: &str) -> bool {
    let bytes = text.as_bytes();
    bytes.len() >= 10
        && bytes[..10].iter().enumerate().all(|(i, b)| match i {
            4 | 7 => *b == b'-',
            _ => b.is_ascii_digit(),
        })
}

pub(super) fn timestamp_type_from_raw(raw_value: &str) -> Option<ParsedTimestampType> {
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

pub(super) fn normalize_raw_timestamp_bounds(
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

    let mut start_time = None;
    let mut end_time = None;
    if let Some(token) = tokens.next() {
        if let Some(range) = parse_time_range_token(token) {
            (start_time, end_time) = range;
        } else if let Some(time_token) = tokens.next() {
            if let Some(range) = parse_time_range_token(time_token) {
                (start_time, end_time) = range;
            }
        }
    }

    let (start_hour, start_minute) = start_time.unwrap_or((0, 0));
    let end_ts = end_time
        .and_then(|(hour, minute)| unix_seconds_from_utc_date_time(year, month, day, hour, minute));
    let range_type = if end_ts.is_some() {
        ParsedTimestampRangeType::TimeRange
    } else {
        ParsedTimestampRangeType::None
    };

    (
        unix_seconds_from_utc_date_time(year, month, day, start_hour, start_minute),
        end_ts,
        range_type,
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

type TimeOfDay = (u32, u32);

fn parse_time_range_token(token: &str) -> Option<(Option<TimeOfDay>, Option<TimeOfDay>)> {
    match token.split_once('-') {
        Some((start, end)) => Some((Some(parse_time_token(start)?), Some(parse_time_token(end)?))),
        None => Some((Some(parse_time_token(token)?), None)),
    }
}

fn parse_time_token(token: &str) -> Option<(u32, u32)> {
    let (hour, minute) = token.split_once(':')?;
    Some((hour.parse().ok()?, minute.parse().ok()?))
}

pub(super) fn raw_timestamp_has_explicit_time(raw_value: &str) -> Option<bool> {
    if matches!(
        timestamp_type_from_raw(raw_value),
        Some(ParsedTimestampType::Diary)
    ) {
        return None;
    }

    let inner = raw_value
        .strip_prefix(['<', '['])
        .and_then(|value| value.strip_suffix(['>', ']']))
        .unwrap_or(raw_value);

    Some(
        inner
            .split_whitespace()
            .any(|token| parse_time_range_token(token).is_some()),
    )
}

pub(super) fn parse_timestamp_modifiers_from_raw(
    raw_value: &str,
) -> Option<Vec<ParsedTimestampModifier>> {
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
    } else {
        let remainder = token.strip_prefix('+')?;
        (ParsedTimestampModifierType::Cumulate, remainder)
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
    } else {
        let remainder = token.strip_prefix('-')?;
        (ParsedTimestampModifierType::All, remainder)
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

pub(super) fn unix_seconds_from_utc_date_time(
    year: i32,
    month: u32,
    day: u32,
    hour: u32,
    minute: u32,
) -> Option<i64> {
    // The project does not model time zones yet, so planning timestamps are
    // normalized as timezone-naive Unix seconds.
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

#[cfg(test)]
mod tests {
    use super::*;
    use ParsedTimestampModifierType as T;
    use ParsedTimestampRangeType as R;
    use ParsedTimestampRole as Role;
    use ParsedTimestampUnit as U;

    type Entry = (Role, &'static str, usize);
    type Sig = (ParsedTimestampModifierKind, T, i64, U, Option<(i64, U)>);

    const ADVERSARIAL: &[&str] = &[
        "",
        " ",
        "<",
        ">",
        "[",
        "]",
        "<>",
        "<é>",
        "é<2024-01-01>é",
        "<2024-01-01",
        "<2024-01-01 é>",
        "<2024-01-01 10:é>",
        "<2024-01-01 é:30>",
        "<2024-01-01 10:00-é>",
        "<2024-01-01 é+1w>",
        "<2024-01-01 +1é>",
        "<2024-01-01 +1\u{e9}>",
        "<2024-01-01 -é>",
        "<2024-01-01 +1w/é>",
        "<%%(é)>",
        "<%%(",
        "<%%()>",
        "[%%(é)]",
        "<é-01-01>",
        "SCHEDULED:é<2024-01-01>",
        "SCHEDULED: é<2024-01-01>",
        "é SCHEDULED: <2024-01-01>",
        "SCHEDULED: <2024-01-01>é DEADLINE: [2024-01-02]é",
        "CLOSED: [2024-01-01 Mon]\nDEADLINE: <2024-01-01>",
        "日本語<2024-01-01>日本語",
        "😀<2024-01-01 +1w>😀",
        "<99999999999999999999-01-01>",
        "<2024-01-01 +99999999999999999999d>",
    ];

    fn sig(m: &ParsedTimestampModifier) -> Sig {
        let deadline = m.repeater_deadline_value.zip(m.repeater_deadline_unit);
        (m.kind, m.modifier_type, m.value, m.unit, deadline)
    }

    #[test]
    fn planning_fallback_entries_report_role_raw_and_offset() {
        let sched = "<%%(diary-float t 42)>";
        let rows: [(&str, Vec<Entry>); 13] = [
            ("", vec![]),
            ("   ", vec![]),
            ("nothing", vec![]),
            (
                "SCHEDULED: <2024-01-01 Mon +1w>",
                vec![(Role::Scheduled, "<2024-01-01 Mon +1w>", 11)],
            ),
            (
                "  DEADLINE: <2024-01-01> CLOSED: [2024-01-02]",
                vec![
                    (Role::Deadline, "<2024-01-01>", 12),
                    (Role::Closed, "[2024-01-02]", 33),
                ],
            ),
            (
                "SCHEDULED:<2024-01-01>",
                vec![(Role::Scheduled, "<2024-01-01>", 10)],
            ),
            (
                "SCHEDULED: <%%(diary-float t 42)>",
                vec![(Role::Scheduled, sched, 11)],
            ),
            ("SCHEDULED: garbage", vec![]),
            (
                "SCHEDULED: <2024-01-01> junk DEADLINE: <2024-01-02>",
                vec![(Role::Scheduled, "<2024-01-01>", 11)],
            ),
            ("SCHEDULED: <2024-01-01", vec![]),
            // Emacs 29.3 / Org 9.6.15 detects the line case-insensitively; orgfdb maps each
            // keyword by name (Emacs itself files lowercase scheduled:/deadline: under :closed).
            (
                "scheduled: <2024-01-01>",
                vec![(Role::Scheduled, "<2024-01-01>", 11)],
            ),
            (
                "Deadline: <2024-01-01> closed: [2024-01-02]",
                vec![
                    (Role::Deadline, "<2024-01-01>", 10),
                    (Role::Closed, "[2024-01-02]", 31),
                ],
            ),
            ("sCHEDULEDx: <2024-01-01>", vec![]),
        ];
        for (line, want) in rows {
            let got = parse_planning_fallback_entries(line);
            let want: Vec<_> = want
                .into_iter()
                .map(|(r, s, o)| (r, s.to_string(), o))
                .collect();
            assert_eq!(got, want, "{line:?}");
            for (_, raw, off) in &got {
                assert_eq!(&line[*off..*off + raw.len()], raw);
            }
        }
        let long = format!("SCHEDULED: <2024-01-01> {}", "x".repeat(100_000));
        assert_eq!(parse_planning_fallback_entries(&long).len(), 1);
    }

    #[test]
    fn raw_timestamp_bounds_cover_time_ranges_and_unknowns() {
        let day = 1_704_067_200;
        let rows = [
            ("<2024-01-01 Mon>", Some(day), None, R::None),
            ("[2024-01-01]", Some(day), None, R::None),
            ("<2024-01-01 Mon 10:30>", Some(day + 37_800), None, R::None),
            ("<2024-01-01 10:30>", Some(day + 37_800), None, R::None),
            (
                "<2024-01-01 Mon 10:30-11:45>",
                Some(day + 37_800),
                Some(day + 42_300),
                R::TimeRange,
            ),
            (
                "<2024-01-01 Mon 10:30 +1w -3d>",
                Some(day + 37_800),
                None,
                R::None,
            ),
            ("<2024-01-01 Mon +1w>", Some(day), None, R::None),
            ("<%%(diary-float t 42)>", None, None, R::None),
            ("[%%(x)]", None, None, R::None),
            ("<>", None, None, R::Unknown),
            ("", None, None, R::Unknown),
            ("   ", None, None, R::Unknown),
            ("<garbage>", None, None, R::Unknown),
            ("<2024-01>", None, None, R::Unknown),
            ("<2024-01-01-02>", None, None, R::Unknown),
            ("<é>", None, None, R::Unknown),
            ("<2024-01-01 Mon 10:xx>", Some(day), None, R::None),
        ];
        for (raw, start, end, range) in rows {
            assert_eq!(
                normalize_raw_timestamp_bounds(raw),
                (start, end, range),
                "{raw:?}"
            );
        }
        let long = format!("<2024-01-01 {}>", "9".repeat(100_000));
        let _ = normalize_raw_timestamp_bounds(&long);
    }

    #[test]
    fn time_range_token_and_explicit_time() {
        let t = |h, m| Some((h, m));
        let rows = [
            ("10:30", Some((t(10, 30), None))),
            ("10:30-11:45", Some((t(10, 30), t(11, 45)))),
            ("9:05", Some((t(9, 5), None))),
            ("10:30-", None),
            ("-11:45", None),
            ("10", None),
            ("10:", None),
            (":30", None),
            ("Mon", None),
            ("", None),
            ("é:30", None),
            ("10:30-11:45-12:00", None),
        ];
        for (token, want) in rows {
            assert_eq!(parse_time_range_token(token), want, "{token:?}");
        }
        let rows = [
            ("<2024-01-01 Mon>", Some(false)),
            ("<2024-01-01 Mon 10:30>", Some(true)),
            ("<2024-01-01 Mon 10:30-11:00 +1w>", Some(true)),
            ("<%%(x)>", None),
            ("", Some(false)),
        ];
        for (raw, want) in rows {
            assert_eq!(raw_timestamp_has_explicit_time(raw), want, "{raw:?}");
        }
    }

    #[test]
    fn modifiers_parse_repeaters_and_warnings() {
        use ParsedTimestampModifierKind::{Repeater as Rep, Warning as Warn};
        let rows: [(&str, Vec<_>); 14] = [
            ("<2024-01-01 Mon>", vec![]),
            (
                "<2024-01-01 Mon +1w>",
                vec![(Rep, T::Cumulate, 1, U::Week, None)],
            ),
            (
                "<2024-01-01 .+1d>",
                vec![(Rep, T::Restart, 1, U::Day, None)],
            ),
            (
                "<2024-01-01 ++2m>",
                vec![(Rep, T::CatchUp, 2, U::Month, None)],
            ),
            ("<2024-01-01 -3d>", vec![(Warn, T::All, 3, U::Day, None)]),
            (
                "<2024-01-01 --1w>",
                vec![(Warn, T::First, 1, U::Week, None)],
            ),
            (
                "<2024-01-01 +1w/2d>",
                vec![(Rep, T::Cumulate, 1, U::Week, Some((2, U::Day)))],
            ),
            (
                "<2024-01-01 10:00 +1y -2h>",
                vec![
                    (Rep, T::Cumulate, 1, U::Year, None),
                    (Warn, T::All, 2, U::Hour, None),
                ],
            ),
            ("<2024-01-01 +0d>", vec![]),
            ("<2024-01-01 +1x>", vec![]),
            ("<2024-01-01 +1w/>", vec![]),
            ("<2024-01-01 +1wx +w +>", vec![]),
            ("<%%(diary +1w)>", vec![]),
            ("", vec![]),
        ];
        for (raw, want) in rows {
            let got: Vec<_> = parse_timestamp_modifiers_from_raw(raw)
                .unwrap()
                .iter()
                .map(sig)
                .collect();
            assert_eq!(got, want, "{raw:?}");
        }
    }

    #[test]
    fn helpers_never_panic_and_return_char_boundaries() {
        for s in ADVERSARIAL {
            if let Some((i, raw)) = extract_first_raw_timestamp(s) {
                assert!(s.is_char_boundary(i) && s.is_char_boundary(i + raw.len()));
                assert_eq!(&s[i..i + raw.len()], raw);
            }
            for (role, raw, off) in parse_planning_fallback_entries(s) {
                let _ = role;
                assert!(
                    s.is_char_boundary(off) && s.is_char_boundary(off + raw.len()),
                    "{s:?}"
                );
            }
            normalize_raw_timestamp_bounds(s);
            raw_timestamp_has_explicit_time(s);
            parse_timestamp_modifiers_from_raw(s);
            timestamp_type_from_raw(s);
            for token in s.split_whitespace() {
                parse_time_range_token(token);
            }
        }
    }
}
