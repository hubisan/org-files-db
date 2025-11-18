// ------------------------------------------------------------
// Link scanning (inline links, file links, URL links, etc.)
// ------------------------------------------------------------

/// Scans a line for Org-mode style links.
///
/// Org inline links typically look like:
/// - [[file:notes.org]]
/// - [[file:notes.org][description]]
/// - [[https://example.com]]
/// - [[https://example.com][Example Site]]
///
/// This placeholder function is where you will later:
/// - detect link boundaries
/// - extract URLs
/// - extract descriptions
/// - build link nodes for your AST
///
/// Called only when Context::allows_links(line) returns true.
pub fn scan_links(_line: &str) {
    // TODO: Implement link scanning

    // Example stub:
    // if let Some(start) = line.find("[[") {
    //     if let Some(end) = line[start + 2..].find("]]") {
    //         let link = &line[start + 2 .. start + 2 + end];
    //         println!("Found link: {}", link);
    //     }
    // }
}
