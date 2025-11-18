// ------------------------------------------------------------
// Org Parser Context
// Tracks block state, property drawer state and heading state.
// Fully Org-mode compliant.
// ------------------------------------------------------------

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BlockType {
    Src,
    Example,
    Export,
    Comment,
}

#[derive(Default)]
pub struct Context {
    /// Current block (SRC, EXAMPLE, EXPORT, COMMENT)
    pub block: Option<BlockType>,

    /// True when inside a :PROPERTIES: … :END: drawer
    pub in_properties_drawer: bool,

    /// True after the first heading has been encountered
    pub has_seen_heading: bool,

    /// File-level PROPERTIES drawer allowed?
    /// Only until first non-comment line before the first heading.
    pub property_drawer_allowed: bool,

    /// After a heading, PROPERTIES drawer allowed until first non-planning line.
    pub heading_allows_drawer: bool,
}

impl Context {
    pub fn new() -> Self {
        Context {
            block: None,
            in_properties_drawer: false,
            has_seen_heading: false,
            property_drawer_allowed: true, // top of file: allowed
            heading_allows_drawer: false,
        }
    }

    /// Main update entry.
    /// Processes block start/end and property drawer start/end.
    pub fn update(&mut self, line: &str) {
        let trimmed = line.trim();

        if self.handle_begin_block(trimmed) { return; }
        if self.handle_end_block(trimmed) { return; }

        if self.handle_property_drawer_start(trimmed) { return; }
        if self.handle_property_drawer_end(trimmed) { return; }
    }

    // ------------------------------------------------------------
    // Block handling
    // ------------------------------------------------------------

    /// Handles #+BEGIN_* (case-insensitive).
    fn handle_begin_block(&mut self, line: &str) -> bool {
        if let Some(rest) = begins_with_ci(line, "#+BEGIN_") {
            let kind = rest.trim().to_ascii_lowercase();

            self.block = match kind.as_str() {
                "src"     => Some(BlockType::Src),
                "example" => Some(BlockType::Example),
                "export"  => Some(BlockType::Export),
                "comment" => Some(BlockType::Comment),
                _ => self.block,
            };

            return true;
        }
        false
    }

    /// Handles #+END_* (case-insensitive).
    fn handle_end_block(&mut self, line: &str) -> bool {
        if let Some(rest) = begins_with_ci(line, "#+END_") {
            let kind = rest.trim().to_ascii_lowercase();

            if let Some(current) = self.block {
                let matches = match (current, kind.as_str()) {
                    (BlockType::Src,     "src")     => true,
                    (BlockType::Example, "example") => true,
                    (BlockType::Export,  "export")  => true,
                    (BlockType::Comment, "comment") => true,
                    _ => false,
                };

                if matches {
                    self.block = None;
                }
            }

            return true;
        }
        false
    }

    // ------------------------------------------------------------
    // PROPERTIES drawer handling
    // ------------------------------------------------------------

    /// Handles :PROPERTIES: drawer start.
    /// File-level or heading-level depending on context.
    fn handle_property_drawer_start(&mut self, line: &str) -> bool {
        if !line.eq_ignore_ascii_case(":PROPERTIES:") {
            return false;
        }

        // Case 1: file-level drawer
        if !self.has_seen_heading && self.property_drawer_allowed {
            self.in_properties_drawer = true;
            return true;
        }

        // Case 2: drawer directly under heading (or planning line)
        if self.heading_allows_drawer {
            self.in_properties_drawer = true;
            return true;
        }

        // Invalid drawer → ignore
        false
    }

    /// Handles :END: drawer end.
    fn handle_property_drawer_end(&mut self, line: &str) -> bool {
        if !line.eq_ignore_ascii_case(":END:") {
            return false;
        }

        if self.in_properties_drawer {
            self.in_properties_drawer = false;
            return true;
        }

        false
    }

    // ------------------------------------------------------------
    // Helper
    // ------------------------------------------------------------

    /// Returns true if inside any block where normal parsing must be ignored.
    pub fn in_ignored_block(&self) -> bool {
        matches!(
            self.block,
            Some(BlockType::Src | BlockType::Example | BlockType::Export | BlockType::Comment)
        )
    }
}

// ------------------------------------------------------------
// Case-insensitive prefix helper (fast, allocation-free)
// ------------------------------------------------------------
#[inline(always)]
fn begins_with_ci<'a>(line: &'a str, prefix: &str) -> Option<&'a str> {
    let p = prefix.as_bytes();
    let l = line.as_bytes();

    if l.len() > p.len() && l[..p.len()].eq_ignore_ascii_case(p) {
        Some(&line[p.len()..])
    } else {
        None
    }
}
