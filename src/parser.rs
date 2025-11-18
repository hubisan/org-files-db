// ------------------------------------------------------------
// Block types to skip completely
// ------------------------------------------------------------

const SKIPPED_BLOCKS: &[&str] = &[
    "SRC",
    "EXAMPLE",
    "EXPORT",
    "COMMENT",
];

// ------------------------------------------------------------
// Parser context
// ------------------------------------------------------------

#[derive(Default)]
pub struct Context {
    // Normal block states
    pub in_src: bool,
    pub in_example: bool,
    pub in_export: bool,
    pub in_comment_block: bool,
    pub drawer_stack: Vec<String>,
    pub in_properties: bool,

    // Fast-skip state
    pub fast_skip: bool,
    pub fast_skip_block_type: Option<String>,

    // File-level content tracking
    pub has_seen_heading: bool,
}

// ------------------------------------------------------------
// Helper functions
// ------------------------------------------------------------

/// Returns `true` if the line is an Org-mode example line (`: `)
/// or a comment line (`# `). Such lines are ignored entirely and
/// should not participate in parsing.
///
/// # Examples
/// ```
/// assert!(ignore_line(": example line"));
/// assert!(ignore_line("# comment line"));
/// assert!(!ignore_line("Normal content"));
/// ```
pub fn ignore_line(line: &str) -> bool {
    line.starts_with(": ") || line.starts_with("# ")
}

/// Returns `true` if the line is an Org-mode heading.
///
/// A valid Org heading must:
/// 1. start with a `*` as the very first character (no leading whitespace)
/// 2. optionally contain additional `*` characters
/// 3. be followed by a space character
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

/// Returns `true` if the parser should skip the current line because
/// we are inside a block that should be ignored or this line starts
/// such a block.
///
/// Blocks skipped entirely:
/// - #+BEGIN_SRC … #+END_SRC
/// - #+BEGIN_EXAMPLE … #+END_EXAMPLE
/// - #+BEGIN_EXPORT … #+END_EXPORT
/// - #+BEGIN_COMMENT … #+END_COMMENT
pub fn should_fast_skip(line: &str, ctx: &mut Context) -> bool {
    let trimmed = line.trim();

    // Already skipping → check for END
    if ctx.fast_skip {
        if let Some(block) = &ctx.fast_skip_block_type {
            let ends = trimmed
                .strip_prefix("#+END_")
                .map(|b| b.eq_ignore_ascii_case(block))
                .unwrap_or(false);

            if ends {
                ctx.fast_skip = false;
                ctx.fast_skip_block_type = None;
            }
        }
        return true;
    }

    // Check for BEGIN block
    if let Some(rest) = trimmed.strip_prefix("#+BEGIN_") {
        let block = rest.trim().to_ascii_uppercase();

        if SKIPPED_BLOCKS.contains(&block.as_str()) {
            ctx.fast_skip = true;
            ctx.fast_skip_block_type = Some(block);
            return true;
        }
    }

    false
}

// ------------------------------------------------------------
// Parser modules (your interfaces)
// ------------------------------------------------------------

fn parse_heading(_line: &str) {}
fn parse_heading_level_0(_line: &str) {}
fn parse_properties(_line: &str) {}
fn scan_links(_line: &str) {}

// ------------------------------------------------------------
// Main Org parser
// ------------------------------------------------------------

/// Parses an Org-mode document line by line and dispatches
/// to the appropriate parsing modules.
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
