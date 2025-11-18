// ------------------------------------------------------------
// Heading parsing utilities
// ------------------------------------------------------------

use crate::types::OrgHeading;

/// Returns true if the line is an Org-mode heading.
/// A valid heading:
/// - starts at column 0 (no leading spaces)
/// - has one or more '*' characters
/// - followed immediately by a space
pub fn is_heading_line(line: &str) -> bool {
    // Leading whitespace → not a heading
    if line.starts_with(' ') || line.starts_with('\t') {
        return false;
    }

    let mut chars = line.chars();
    let mut stars = 0;

    // Count leading '*'
    while let Some(c) = chars.next() {
        if c == '*' {
            stars += 1;
        } else {
            break;
        }
    }

    // Need at least 1 star, and next char must be space
    stars > 0 && chars.next() == Some(' ')
}

/// Parses a heading line into an OrgHeading.
/// This function receives the raw heading line such as:
/// "* TODO [#A] Title :tag1:tag2:"
///
/// NOTE: Currently minimal. Full parsing (TODO keywords,
/// priority, tags, cookie, etc.) can be added later.
pub fn parse_heading(line: &str) -> OrgHeading {
    let level = heading_level(line);

    // Extract title text (after leading stars + one space)
    let title = extract_title(line, level);

    OrgHeading {
        level,
        title,
        // TODO: parse TODO, priority, raw_title, tags etc.
        ..Default::default()
    }
}

/// Creates the level-0 (file-level) virtual heading.
pub fn create_level0_heading() -> OrgHeading {
    OrgHeading {
        level: 0,
        file: true,
        ..Default::default()
    }
}

/// Helper: count number of leading '*' to determine heading level.
fn heading_level(line: &str) -> u8 {
    line.chars()
        .take_while(|&c| c == '*')
        .count()
        .min(255) as u8
}

/// Extracts the title text after the stars and space.
fn extract_title(line: &str, level: u8) -> String {
    let prefix_len = level as usize + 1; // stars + required space
    line[prefix_len..].trim().to_string()
}
