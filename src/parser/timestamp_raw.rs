//! Parsing of raw timestamp and planning-line text.

use super::line_index::LineIndex;
use super::model::{
    ParsedTimestamp, ParsedTimestampModifier, ParsedTimestampModifierKind,
    ParsedTimestampModifierType, ParsedTimestampRangeType, ParsedTimestampRole,
    ParsedTimestampType, ParsedTimestampUnit,
};

/// Planning entries of the planning line `line` (without its terminator), which starts at
/// byte `line_offset` of the source, in line order.
pub(super) fn planning_timestamps(
    line: &str,
    line_offset: usize,
    lines: &LineIndex,
) -> Vec<ParsedTimestamp> {
    parse_planning_fallback_entries(line)
        .into_iter()
        .map(|(role, raw_value, relative_start)| {
            let byte_start = line_offset + relative_start;
            let byte_end = byte_start + raw_value.len();
            let (start_ts, end_ts, range_type) = normalize_raw_timestamp_bounds(&raw_value);
            ParsedTimestamp {
                role: Some(role),
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
                raw_value,
            }
        })
        .collect()
}

/// Org matches the planning keywords case-insensitively (`scheduled:`, `Deadline:`).
fn strip_planning_keyword<'a>(text: &'a str, keyword: &str) -> Option<&'a str> {
    let head = text.get(..keyword.len())?;
    head.eq_ignore_ascii_case(keyword)
        .then(|| &text[keyword.len()..])
}

/// Planning entries of one planning line like `org-element-planning-parser`: every
/// `SCHEDULED:`, `DEADLINE:` or `CLOSED:` anywhere in the line (Org searches for them) whose
/// timestamp follows after spaces or tabs. Text between entries is ignored, and a keyword
/// without a timestamp yields no entry. Returns role, raw value and offset in `line`.
pub(super) fn parse_planning_fallback_entries(
    line: &str,
) -> Vec<(ParsedTimestampRole, String, usize)> {
    let mut entries = Vec::new();
    let mut offset = 0;

    while let Some((role, keyword_end)) = next_planning_keyword(line, offset) {
        let timestamp_start =
            line.len() - line[keyword_end..].trim_start_matches([' ', '\t']).len();
        match timestamp_length(&line[timestamp_start..]) {
            Some(length) => {
                let raw_value = line[timestamp_start..timestamp_start + length].to_string();
                offset = timestamp_start + length;
                entries.push((role, raw_value, timestamp_start));
            }
            None => offset = keyword_end,
        }
    }

    entries
}

/// First planning keyword at or after byte `from`: its role and the offset behind it.
fn next_planning_keyword(line: &str, from: usize) -> Option<(ParsedTimestampRole, usize)> {
    line[from..].char_indices().find_map(|(index, _)| {
        let rest = &line[from + index..];
        [
            ("SCHEDULED:", ParsedTimestampRole::Scheduled),
            ("DEADLINE:", ParsedTimestampRole::Deadline),
            ("CLOSED:", ParsedTimestampRole::Closed),
        ]
        .into_iter()
        .find_map(|(keyword, role)| {
            strip_planning_keyword(rest, keyword).map(|_| (role, from + index + keyword.len()))
        })
    })
}

/// Length of the timestamp at the start of `text`, as `org-element-timestamp-parser` reads
/// it: a diary sexp, or a bracket timestamp (`org-ts-regexp-both`) with an optional
/// `--` and a second one for a range (`<a>--[b]`).
fn timestamp_length(text: &str) -> Option<usize> {
    if let Some(length) = diary_timestamp_length(text) {
        return Some(length);
    }
    let first = bracket_timestamp_length(text)?;
    match text[first..]
        .strip_prefix("--")
        .map(bracket_timestamp_length)
    {
        Some(Some(second)) => Some(first + 2 + second),
        _ => Some(first),
    }
}

/// Length of the timestamp object at the start of `text` as `org-element-timestamp-parser`
/// reads it inside running text. A bracket timestamp is `timestamp_length`; a diary sexp
/// (`<%%(...)>`, accepted by `org-element--timestamp-regexp`) ends at the first `]` or `>`
/// of the line, which is the `.*?` of the parser; an active timestamp with a repeater whose
/// date is not followed by a blank (`<2024-04-01<2024-05-05 Mon +1d>`) is accepted by the
/// same regexp and ends at the first `]` or `>` as well. A second timestamp may follow `--`.
pub(super) fn inline_timestamp_length(text: &str) -> Option<usize> {
    let first = if text.starts_with("<%%") {
        diary_timestamp_length(text)?;
        raw_end(text)?
    } else if let Some(first) = bracket_timestamp_length(text) {
        first
    } else if loose_repeater_timestamp(text) {
        raw_end(text)?
    } else {
        return None;
    };
    match text[first..]
        .strip_prefix("--")
        .map(bracket_timestamp_length)
    {
        Some(Some(second)) => Some(first + 2 + second),
        _ => Some(first),
    }
}

/// Offset behind the first `]` or `>` of the line of `text`.
fn raw_end(text: &str) -> Option<usize> {
    let end = text.find([']', '>', '\n'])?;
    matches!(text.as_bytes()[end], b']' | b'>').then_some(end + 1)
}

/// `<[0-9]+-[0-9]+-[0-9]+[^>\n]+?\+[0-9]+[dwmy]>` (the third alternative of
/// `org-element--timestamp-regexp`) that holds a date as `org-parse-time-string` reads it,
/// which Org needs to make a timestamp of it.
fn loose_repeater_timestamp(text: &str) -> bool {
    let Some(rest) = text.strip_prefix('<') else {
        return false;
    };
    let digits = |text: &str| text.bytes().take_while(u8::is_ascii_digit).count();
    // Two digit groups with a `-` behind each, then the digits of the third.
    let mut third = 0;
    for _ in 0..2 {
        let length = digits(&rest[third..]);
        if length == 0 || rest.as_bytes().get(third + length) != Some(&b'-') {
            return false;
        }
        third += length + 1;
    }
    if digits(&rest[third..]) == 0 {
        return false;
    }
    let Some(end) = rest
        .find(['>', '\n'])
        .filter(|end| rest.as_bytes()[*end] == b'>')
    else {
        return false;
    };
    let inner = &rest[..end];
    // `+N` and a unit at the end, behind at least one character after a digit of the date.
    let Some(before_unit) = inner.strip_suffix(['d', 'w', 'm', 'y']) else {
        return false;
    };
    let repeater = before_unit.trim_end_matches(|c: char| c.is_ascii_digit());
    repeater.len() < before_unit.len()
        && repeater.ends_with('+')
        && repeater.len() > third + 2
        && inner.as_bytes().windows(10).any(is_iso_date_bytes)
}

fn is_iso_date_bytes(bytes: &[u8]) -> bool {
    bytes.iter().enumerate().all(|(index, byte)| match index {
        4 | 7 => *byte == b'-',
        _ => byte.is_ascii_digit(),
    })
}

/// The body timestamp object for the raw value `raw_value`, which starts at `byte_start`.
pub(super) fn body_timestamp(
    raw_value: &str,
    byte_start: usize,
    lines: &LineIndex,
) -> ParsedTimestamp {
    let (start_ts, end_ts, range_type) = normalize_raw_timestamp_bounds(raw_value);
    ParsedTimestamp {
        role: Some(ParsedTimestampRole::Body),
        timestamp_type: timestamp_type_from_raw(raw_value).unwrap_or(ParsedTimestampType::Active),
        range_type,
        has_time: raw_timestamp_has_explicit_time(raw_value),
        start_ts,
        end_ts,
        byte_start,
        byte_end: byte_start + raw_value.len(),
        line_number: Some(lines.line_for(byte_start)),
        modifiers: parse_timestamp_modifiers_from_raw(raw_value).unwrap_or_default(),
        raw_value: raw_value.to_string(),
    }
}

/// Length of `[<[]YYYY-MM-DD\(?: +[^]\r\n>]*?\)?[]>]` at the start of `text`.
fn bracket_timestamp_length(text: &str) -> Option<usize> {
    let rest = text.strip_prefix(['<', '['])?;
    if !starts_with_iso_date(rest) {
        return None;
    }
    let after_date = &rest[10..];
    let close = after_date.find([']', '>', '\r', '\n'])?;
    if !matches!(after_date.as_bytes()[close], b']' | b'>') {
        return None;
    }
    // Between the date and the closing bracket: nothing, or blanks first.
    if close > 0 && !after_date.starts_with(' ') {
        return None;
    }
    Some(1 + 10 + close + 1)
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

/// Splits `<a>--<b>` (either bracket kind) into its two timestamps.
fn split_timestamp_range(raw_value: &str) -> Option<(&str, &str)> {
    let first = bracket_timestamp_length(raw_value)?;
    let second = raw_value[first..].strip_prefix("--")?;
    (bracket_timestamp_length(second)? == second.len()).then(|| (&raw_value[..first], second))
}

/// Start and end as unix seconds (UTC) and the range type. A `--` range runs from the
/// start of the first to the start of the second timestamp.
pub(super) fn normalize_raw_timestamp_bounds(
    raw_value: &str,
) -> (Option<i64>, Option<i64>, ParsedTimestampRangeType) {
    let Some((first, second)) = split_timestamp_range(raw_value) else {
        return single_timestamp_bounds(raw_value);
    };
    let timed = raw_timestamp_has_explicit_time(raw_value) == Some(true);
    (
        single_timestamp_bounds(first).0,
        single_timestamp_bounds(second).0,
        if timed {
            ParsedTimestampRangeType::DateTimeRange
        } else {
            ParsedTimestampRangeType::DateRange
        },
    )
}

fn single_timestamp_bounds(
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
    if let Some((first, second)) = split_timestamp_range(raw_value) {
        return Some(
            raw_timestamp_has_explicit_time(first) == Some(true)
                || raw_timestamp_has_explicit_time(second) == Some(true),
        );
    }
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
    if let Some((first, second)) = split_timestamp_range(raw_value) {
        // Org reads the first repeater and the first warning of the whole raw value.
        let mut modifiers = parse_timestamp_modifiers_from_raw(first)?;
        for modifier in parse_timestamp_modifiers_from_raw(second)? {
            if modifiers.iter().all(|known| known.kind != modifier.kind) {
                modifiers.push(modifier);
            }
        }
        return Some(modifiers);
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
        let rows: Vec<(&str, Vec<Entry>)> = vec![
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
            // Org searches the whole line for keywords, so text in between is ignored.
            (
                "SCHEDULED: <2024-01-01> junk DEADLINE: <2024-01-02>",
                vec![
                    (Role::Scheduled, "<2024-01-01>", 11),
                    (Role::Deadline, "<2024-01-02>", 39),
                ],
            ),
            // The timestamp has to follow the keyword directly, after blanks.
            ("SCHEDULED: foo <2024-01-01>", vec![]),
            (
                "SCHEDULED: foo DEADLINE: <2024-01-01>",
                vec![(Role::Deadline, "<2024-01-01>", 25)],
            ),
            ("SCHEDULED: <2024-01-01x>", vec![]),
            ("SCHEDULED: <2024-01-01Mon>", vec![]),
            // `--` joins two timestamps of either bracket kind; a single dash does not.
            (
                "SCHEDULED: <2024-01-01 Mon>--<2024-01-03 Wed> DEADLINE: <2024-02-02>",
                vec![
                    (Role::Scheduled, "<2024-01-01 Mon>--<2024-01-03 Wed>", 11),
                    (Role::Deadline, "<2024-02-02>", 56),
                ],
            ),
            (
                "CLOSED: [2024-01-01 Mon 10:00]--<2024-01-01 Mon 11:00>",
                vec![(
                    Role::Closed,
                    "[2024-01-01 Mon 10:00]--<2024-01-01 Mon 11:00>",
                    8,
                )],
            ),
            (
                "SCHEDULED: <2024-01-01>-<2024-01-02>",
                vec![(Role::Scheduled, "<2024-01-01>", 11)],
            ),
            (
                "SCHEDULED: <2024-01-01>--garbage",
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
            // A `--` range runs from the start of the first to the start of the second.
            (
                "<2024-01-01 Mon>--<2024-01-03 Wed>",
                Some(day),
                Some(day + 2 * 86_400),
                R::DateRange,
            ),
            (
                "[2024-01-01 Mon 10:30]--<2024-01-03 Wed>",
                Some(day + 37_800),
                Some(day + 2 * 86_400),
                R::DateTimeRange,
            ),
            (
                "<2024-01-01 Mon>--[2024-01-03 Wed 08:00]",
                Some(day),
                Some(day + 2 * 86_400 + 28_800),
                R::DateTimeRange,
            ),
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
            if let Some(length) = timestamp_length(s) {
                assert!(s.is_char_boundary(length));
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
