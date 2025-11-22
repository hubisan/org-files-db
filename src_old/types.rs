// ------------------------------------------------------------
// Org-mode AST types (links, headings, blocks, properties…)
// ------------------------------------------------------------

use serde::Serialize;

// ------------------------------------------------------------
// Link type
// ------------------------------------------------------------

/// A single Org-mode link inside headings, properties or body text.
#[derive(Debug, Clone, Serialize)]
pub struct OrgLink {
    /// The original link string as found in the file.
    /// e.g. `[[file:~/test.org][Test]]`
    /// or   `https://google.com`
    pub raw: String,

    /// Link type such as: "http", "https", "file", "id", "anchor", …
    pub link_type: String,

    /// Path exactly as written in the Org file.
    pub path: String,

    /// Absolute path (for file links) but:
    pub path_absolute: Option<String>,

    /// If this is an `id:` or `custom-id:` link, this stores the target
    /// heading ID so that resolving is possible later.
    pub target_heading_id: Option<String>,

    /// Search option such as: `#anchor`, `*heading`, or text search.
    /// Like `./org-test-links.org::dedicated target`
    pub search_option: Option<String>,

    /// Description part of a bracket link: `[[link][description]]`
    pub description: Option<String>,

    /// Either `"plain"` or `"bracket"`.
    pub format: String,

    /// Byte position inside the input buffer.
    pub pos: usize,
}

// ------------------------------------------------------------
// Heading type
// ------------------------------------------------------------

/// A single Org-mode heading in the outline tree.
///
/// This structure contains both "raw" fields (directly from the file)
/// and cleaned/normalized fields.
#[derive(Debug, Clone, Serialize)]
pub struct OrgHeading {
    /// Heading level = number of stars `*`.
    pub level: u8,

    /// TODO keyword if present: TODO / DONE / NEXT / …
    pub todo: Option<String>,

    /// Priority cookie without brackets, e.g. `"A"`.
    pub priority: Option<String>,

    /// Cleaned title:
    /// - no stars
    /// - no TODO keyword
    /// - no priority
    /// - no status cookies
    /// - no tags
    /// - links normalized (description or if none, the path)
    pub title: String,

    /// Raw title:
    /// - no stars
    /// - no tag group at the end
    /// - TODO + priority kept
    /// - status cookie removed
    /// - links kept as-is
    pub title_raw: String,

    /// Tags directly attached to this heading.
    pub tags: Vec<String>,

    /// Tags inherited from ancestors and file-level tags.
    pub inherited_tags: Vec<String>,

    /// Direct properties from this heading’s :PROPERTIES: drawer.
    pub properties: Vec<(String, String)>,

    /// Properties inherited from parents and file-level.
    pub inherited_properties: Vec<(String, String)>,

    /// Planning timestamps
    pub scheduled: Option<String>,
    pub deadline: Option<String>,
    pub closed: Option<String>,

    /// Links detected in the body or title (outside ignored regions).
    pub links: Vec<OrgLink>,

    /// Index of parent inside the global headings vector.
    pub parent_id: Option<usize>,

    /// Outline path of parent headings (titles only).
    pub outline: Vec<String>,

    /// Whether this heading is the virtual file-root (level 0).
    pub file: bool,
}

// ------------------------------------------------------------
// Block types
// ------------------------------------------------------------

#[derive(Debug, Clone, Serialize)]
pub enum BlockKind {
    Src,
    Example,
    Export,
    Comment,
    Unknown(String),
}

#[derive(Debug, Clone, Serialize)]
pub struct Block {
    pub kind: BlockKind,
    pub content: Vec<String>,
}

// ------------------------------------------------------------
// Simple property type (for standalone properties)
// ------------------------------------------------------------

#[derive(Debug, Clone, Serialize)]
pub struct Property {
    pub key: String,
    pub value: String,
}

// ------------------------------------------------------------
// AST element enum
// ------------------------------------------------------------

#[derive(Debug, Clone, Serialize)]
pub enum OrgElement {
    Heading(OrgHeading),
    Property(Property),
    Link(OrgLink),
    Block(Block),
    Text(String),
}

// ------------------------------------------------------------
// Entire document representation
// ------------------------------------------------------------

#[derive(Debug, Clone, Serialize)]
pub struct OrgDocument {
    pub elements: Vec<OrgElement>,
}

impl OrgDocument {
    pub fn new() -> Self {
        Self {
            elements: Vec::new(),
        }
    }

    pub fn push(&mut self, element: OrgElement) {
        self.elements.push(element);
    }
}
