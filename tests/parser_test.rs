use org_files_db::parser::{parse_org};
use org_files_db::types::OrgLink;

#[test]
fn test_link_parsing_from_file() {
    // Load test file
    let org_content = std::fs::read_to_string("test.org").expect("Failed to read test.org");

    // Use your actual parser API
    let headings = parse_org(&org_content, "test.org", None, None);

    // Flatten all links
    let all_links: Vec<OrgLink> = headings
        .iter()
        .flat_map(|h| h.links.clone())
        .collect();

    // 1) Bracket-Link mit Beschreibung
    let link1 = all_links.iter()
        .find(|l| l.raw == "[[https://emacs.org][Emacs Website]]")
        .expect("Link 1 not found");

    assert_eq!(link1.link_type, "https");
    assert_eq!(link1.path, "//emacs.org");
    assert_eq!(link1.description, Some("Emacs Website".to_string()));
    assert_eq!(link1.format, "bracket");
    assert!(link1.pos > 150 && link1.pos < 250);

    // 2) Bracket-Link ohne Beschreibung
    let link2 = all_links.iter()
        .find(|l| l.raw == "[[https://gnu.org]]")
        .expect("Link 2 not found");

    assert_eq!(link2.link_type, "https");
    assert_eq!(link2.path, "//gnu.org");
    assert_eq!(link2.description, None);
    assert_eq!(link2.format, "bracket");

    // 3) Plain http link
    let link3 = all_links.iter()
        .find(|l| l.raw == "https://example.com/plain")
        .expect("Link 3 not found");

    assert_eq!(link3.link_type, "https");
    assert_eq!(link3.path, "//example.com/plain");
    assert_eq!(link3.format, "plain");

    // 6) Relative file link: [[../../../../../otd/1-actions.org]]
    let link6 = all_links.iter()
        .find(|l| l.raw == "[[../../../../../otd/1-actions.org]]")
        .expect("Link 6 not found");

    assert_eq!(link6.link_type, "file");
    assert_eq!(link6.path, "../../../../../otd/1-actions.org");
    assert!(link6.path_absolute.is_some());

    // 7) File+search: [[file:../notes/...::Sibling section]]
    let link7 = all_links.iter()
        .find(|l| l.raw == "[[file:../notes/familie/test/test.org::Sibling section]]")
        .expect("Link 7 not found");

    assert_eq!(link7.link_type, "file");
    assert_eq!(link7.path, "../notes/familie/test/test.org");
    assert_eq!(link7.search_option, Some("Sibling section".into()));
    assert_eq!(link7.description, None);
}
