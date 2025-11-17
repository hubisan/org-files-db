use serde::Serialize;

/// Ein einzelner Link im Body oder im Header.
#[derive(Debug, Clone, Serialize)]
pub struct OrgLink {
    /// Der unveränderte Original-Link-String.
    /// z.B. "[[file:~/test.org][Test]]" oder "https://google.com"
    pub raw: String,

    /// Link-Typ: "http", "https", "file", "id", "anchor", …
    pub link_type: String,

    /// Path wie er im Org-File steht (unverändert).
    pub path: String,

    /// Absoluter Pfad (falls file-Link), aber:
    /// - KEINE ~-Expansion
    /// - OHNE canonicalize()
    /// - relativ zum Org-File berechnet
    pub path_absolute: Option<String>,

    // If custom id or id then store the link to be able to link to the heading
    // or file later on.
    pub heading_id: Option<String>,

    /// Search-Option (#anchor, *heading, Text…)
    pub search_option: Option<String>,

    /// Beschreibung bei [[tgt][desc]].
    pub description: Option<String>,

    /// "plain" oder "bracket"
    pub format: String,

    /// Absolute Byte-Position im Inputfile.
    pub pos: usize,
}

/// Ein Org-Heading / Node im Outline.
#[derive(Debug, Clone, Serialize)]
pub struct OrgHeading {
    /// Stern-Level (* count)
    pub level: u8,

    /// TODO-Keyword, falls erkannt.
    pub todo: Option<String>,

    /// Priority (ohne Klammern), z. B. "A".
    pub priority: Option<String>,

    /// Bereinigter Titel (ohne TODO, Priority, Stat-Cookie, Tags;
    /// Links in desc/target umgewandelt).
    pub title: String,

    /// Roher Titel ohne Stern/Tags/Cookie:
    /// - Links bleiben 1:1 erhalten
    /// - TODO & Priority bleiben enthalten
    /// - Stat-Cookie entfernt
    /// - Tag-Group am Ende entfernt
    pub title_raw: String,

    /// Tags direkt am Heading (keine File-Level-Tags)
    pub tags: Vec<String>,

    /// Vererbte Tags (aus Parent + File-Level)
    pub inherited_tags: Vec<String>,

    /// Direkte Properties aus :PROPERTIES:-Drawer
    pub properties: Vec<(String, String)>,

    /// Vererbte Properties aus Parent + File-Level
    pub inherited_properties: Vec<(String, String)>,

    /// Planungstimestamps
    pub scheduled: Option<String>,
    pub deadline: Option<String>,
    pub closed: Option<String>,

    /// Links im Body & im Titel (alle außer in SRC/RESULTS/PROPERTIES drawer)
    pub links: Vec<OrgLink>,

    /// Parent-ID im headings[]-Vektor
    pub parent_id: Option<usize>,

    /// Outline-Pfad der Eltern-Titel (ohne self).
    pub outline: Vec<String>,

    /// Ist dies der virtuelle Datei-Root ("level 0")?
    pub file: bool,
}
