/// Returns `true` if the line is an Org-mode example line (`: `)
/// or a comment line (`# `). Such lines are ignored entirely and
/// should not participate in parsing.
///
/// # Arguments
/// * `line` – A single line of text from the Org document.
///
/// # Examples
/// ```
/// assert!(ignore_line(": example line"));
/// assert!(ignore_line("# comment line"));
/// assert!(!ignore_line("Normal content"));
/// ```
///
/// # Returns
/// `true` if the line should be ignored.
pub fn ignore_line(line: &str) -> bool {
    line.starts_with(": ") || line.starts_with("# ")
}

/// Returns `true` if the line is an Org-mode heading.
///
/// A valid Org heading must:
/// 1. start with a `*` as the very first character (no leading whitespace)
/// 2. optionally contain additional `*` characters
/// 3. be followed by a space character
///
/// # Arguments
/// * `line` – A single line of text from the Org document.
///
/// # Returns
/// `true` if the line matches the Org-mode heading syntax.
pub fn is_heading_line(line: &str) -> bool {
    let mut chars = line.chars();
    let mut star_count = 0;

    // Count consecutive '*'
    while let Some(c) = chars.next() {
        if c == '*' {
            star_count += 1;
        } else {
            break;
        }
    }

    // Must have at least one '*' and then a space
    star_count > 0 && chars.next() == Some(' ')
}

/// Parses an Org-mode document line by line and dispatches
/// to the appropriate parsing modules.
///
/// # Arguments
/// * `input` – The entire Org document as a string slice.
pub fn parse_org(input: &str) {
    let mut ctx = Context::default();

    for line in input.lines() {
        if ignore_line(line) {
            continue;
        }

        ctx.maybe_end_fast_skip(line);
        if ctx.fast_skip {
            continue;
        }

        ctx.update(line);

        if is_heading_line(line) {
            ctx.has_seen_heading = true;
            parse_heading(line);
            continue;
        }

        if !ctx.has_seen_heading {
            parse_heading_level_0(line);
            continue;
        }

        if ctx.in_properties_drawer() {
            parse_properties(line);
            continue;
        }

        if ctx.allows_links(line) {
            scan_links(line);
        }
    }
}
