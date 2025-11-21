use crate::types::OrgLineType;
use lazy_static::lazy_static;
use regex::Regex;

/// Line classifier with full org-element--current-element precedence
pub fn classify_line(
    line: &str,
    mode: ParserMode,    // item, table-row, node-property, section, first-section, planning, …
    at_task: bool,       // inlinetask detection
) -> OrgLineType {

    // ---------------------------------------------------------
    // 1–3. Mode-based early exits
    // ---------------------------------------------------------
    match mode {
        ParserMode::Item => return OrgLineType::Item,
        ParserMode::TableRow => return OrgLineType::TableRow,
        ParserMode::NodeProperty => return OrgLineType::NodeProperty,
        _ => {}
    }

    // ---------------------------------------------------------
    // 4. Headline
    // ---------------------------------------------------------
    if HEADLINE_RE.is_match(line) && !is_inlinetask(line, mode) {
        return OrgLineType::Headline;
    }

    // ---------------------------------------------------------
    // 5. Section (just a marker, not an element)
    // ---------------------------------------------------------
    if matches!(mode, ParserMode::Section | ParserMode::FirstSection) {
        return OrgLineType::Section;
    }

    // ---------------------------------------------------------
    // 6. Comment line
    // ---------------------------------------------------------
    if COMMENT_RE.is_match(line) {
        return OrgLineType::Comment;
    }

    // ---------------------------------------------------------
    // 7. Planning line
    // ---------------------------------------------------------
    if matches!(mode, ParserMode::Planning)
        && HEADLINE_STAR_RE.is_match(line)
        && PLANNING_RE.is_match(line)
    {
        return OrgLineType::Planning;
    }

    // ---------------------------------------------------------
    // 8. Property drawer
    // ---------------------------------------------------------
    if property_drawer_allowed(mode) && PROPERTY_DRAWER_RE.is_match(line) {
        return OrgLineType::PropertyDrawer;
    }

    // ---------------------------------------------------------
    // 9. Not at beginning-of-line → Paragraph
    // ---------------------------------------------------------
    if !line.starts_with(|c| c == '*' || c == ' ' || c == '\t') {
        return OrgLineType::Paragraph;
    }

    // ---------------------------------------------------------
    // 10. Clock line
    // ---------------------------------------------------------
    if CLOCK_RE.is_match(line) {
        return OrgLineType::Clock;
    }

    // ---------------------------------------------------------
    // 11. Inlinetask
    // ---------------------------------------------------------
    if at_task {
        return OrgLineType::InlineTask;
    }

    // ---------------------------------------------------------
    // 12. Affiliated keywords occur here in Emacs
    //     (you can decide whether you want to classify them here)
    // ---------------------------------------------------------

    // ---------------------------------------------------------
    // 13. The big org-element--current-element-re
    // ---------------------------------------------------------
    if let Some(t) = classify_big_regex(line) {
        return t;
    }

    // ---------------------------------------------------------
    // 14. Table
    // ---------------------------------------------------------
    if TABLE_START_RE.is_match(line) {
        return OrgLineType::Table;
    }

    // ---------------------------------------------------------
    // 15. List
    // ---------------------------------------------------------
    if LIST_RE.is_match(line) {
        return OrgLineType::List;
    }

    // ---------------------------------------------------------
    // 16. Default: paragraph
    // ---------------------------------------------------------
    OrgLineType::Paragraph
}
