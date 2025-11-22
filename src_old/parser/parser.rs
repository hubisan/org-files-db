// ------------------------------------------------------------
// Org Parser (minimal, correct, context-driven)
// ------------------------------------------------------------

use crate::parser::context::Context;
use crate::parser::headings::{is_heading_line, parse_heading, create_level0_heading};
use crate::parser::keywords_properties::{parse_keyword, parse_inline_property, parse_drawer_property};
use crate::parser::links::scan_links;
use crate::ignore_line;

use crate::types::{OrgDocument, OrgHeading};

pub fn parse_org(input: &str) -> OrgDocument {
    let mut ctx = Context::new();
    let mut doc = OrgDocument::default();

    // create a virtual level-0 heading
    let mut level0 = create_level0_heading();

    // pointer to current heading (starts at level0)
    let mut current_heading: &mut OrgHeading = &mut level0;

    for line in input.lines() {

        // ------------------------------------------------------------
        // 0. Skip simple ignorable lines
        // ------------------------------------------------------------
        if ignore_line(line) {
            continue;
        }

        // ------------------------------------------------------------
        // 1. Track file-level rules BEFORE calling update()
        // (Org: file-level drawer allowed only until first non-comment line)
        // ------------------------------------------------------------
        if !ctx.has_seen_heading && ctx.property_drawer_allowed {
            if !line.trim().is_empty()
                && !line.trim_start().starts_with("# ")
            {
                // first non-comment, non-empty content → disable
                // file-level property drawer
                ctx.property_drawer_allowed = false;
            }
        }

        // ------------------------------------------------------------
        // 2. Update block/drawer context
        // ------------------------------------------------------------
        ctx.update(line);

        // ------------------------------------------------------------
        // 3. Skip everything inside ignored blocks
        // ------------------------------------------------------------
        if ctx.in_ignored_block() {
            continue;
        }

        // ------------------------------------------------------------
        // 4. Heading?
        // ------------------------------------------------------------
        if is_heading_line(line) {
            ctx.has_seen_heading = true;
            ctx.heading_allows_drawer = true; // drawer allowed right after heading

            // finalize previous heading and start a new one (TODO AST)
            parse_heading(line);

            // TODO: assign new heading to current_heading pointer

            continue;
        }

        // ------------------------------------------------------------
        // 5. Properties drawer
        // ------------------------------------------------------------
        if ctx.in_properties_drawer {
            if let Some(prop) = parse_drawer_property(line) {

                if !ctx.has_seen_heading {
                    // file-level properties
                    current_heading.properties.push(prop);
                } else {
                    // heading-level properties
                    current_heading.properties.push(prop);
                }

                continue;
            }

            // drawer content finished later by context::handle_property_drawer_end()
            continue;
        }

        // ------------------------------------------------------------
        // 6. Planning lines → allow drawer
        // ------------------------------------------------------------
        if is_planning_line(line) {
            ctx.heading_allows_drawer = true;
            // TODO parse planning
            continue;
        }

        // After first non-planning line → drawer not allowed
        if ctx.has_seen_heading && !line.trim().is_empty() {
            ctx.heading_allows_drawer = false;
        }

        // ------------------------------------------------------------
        // 7. Before first heading → Level-0 mode
        // ------------------------------------------------------------
        if !ctx.has_seen_heading {

            // Inline property?
            if let Some(prop) = parse_inline_property(line) {
                current_heading.properties.push(prop);
                // this counts as content, property drawer not allowed anymore
                ctx.property_drawer_allowed = false;
                continue;
            }

            // Keyword?
            if let Some(keyword) = parse_keyword(line) {
                // special case: TITLE keyword → heading title
                if keyword.key == "TITLE" {
                    current_heading.title = keyword.value.clone();
                }

                current_heading.keywords.push(keyword);
                ctx.property_drawer_allowed = false;
                continue;
            }

            // Links in Level-0 (only outside ignored blocks)
            scan_links(line);

            // normal content of Level-0 (TODO handle body if needed)
            continue;
        }

        // ------------------------------------------------------------
        // 8. After first heading → normal body processing
        // ------------------------------------------------------------

        // Inline property
        if let Some(prop) = parse_inline_property(line) {
            current_heading.properties.push(prop);
            continue;
        }

        // Keywords apply at heading-level
        if let Some(keyword) = parse_keyword(line) {
            current_heading.keywords.push(keyword);
            continue;
        }

        // Links
        scan_links(line);

        // TODO body lines
    }

    // store level-0 heading in document AST
    doc.root = level0;
    doc
}

// ------------------------------------------------------------
// Utility: planning line detection
// ------------------------------------------------------------
fn is_planning_line(line: &str) -> bool {
    let t = line.trim_start().to_ascii_uppercase();

    t.starts_with("SCHEDULED:")
        || t.starts_with("DEADLINE:")
        || t.starts_with("CLOSED:")
}
