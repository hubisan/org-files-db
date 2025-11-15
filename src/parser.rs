// src/parser.rs
//
// FINAL VERSION — clean, fast, no body, full link parsing,
// file-level tags + properties inherited, TODO system dynamic,
// no keywords, no keyword-properties, correct title/title_raw logic,
// absolute file-link paths with ~ expansion.
// ---------------------------------------------------------------

use crate::config::{is_uppercase_word, TodoMode};
use crate::types::{OrgHeading, OrgLink};
use dirs;
use once_cell::sync::Lazy;
use regex::Regex;
use std::path::{Component, Path, PathBuf};

fn normalize_path(path: &Path) -> PathBuf {
    let mut components = path.components().peekable();
    let mut ret = if let Some(c @ Component::RootDir) = components.peek().cloned() {
        components.next();
        PathBuf::from(c.as_os_str())
    } else {
        PathBuf::new()
    };

    for component in components {
        match component {
            Component::Normal(c) => ret.push(c),
            Component::CurDir => {}
            Component::ParentDir => {
                ret.pop();
            }
            Component::RootDir => unreachable!(),
            Component::Prefix(p) => ret.push(p.as_os_str()),
        }
    }
    ret
}

//
// ─────────────────────────────────────────────
//   HEADING REGEX (Minimal — keine TODO/PRIO/Tags/Cookie hier!)
// ─────────────────────────────────────────────
//
static HEADING_RE: Lazy<Regex> =
    Lazy::new(|| Regex::new(r"^(?P<stars>\*+)\s*(?P<title_raw>.*)$").unwrap());

//
// ─────────────────────────────────────────────
//   FILE-LEVEL DIRECTIVES (case-insensitive)
// ─────────────────────────────────────────────
//
static FILETITLE_RE: Lazy<Regex> = Lazy::new(|| Regex::new(r"(?i)^#\+title:\s*(.*)$").unwrap());

static FILETAGS_RE: Lazy<Regex> = Lazy::new(|| Regex::new(r"(?i)^#\+filetags:\s*(.*)$").unwrap());

static FILEPROP_RE: Lazy<Regex> =
    Lazy::new(|| Regex::new(r"(?i)^#\+property:\s*([A-Za-z0-9_-]+)\s+(.*)$").unwrap());

//
// ─────────────────────────────────────────────
//   DRAWERS (case-insensitive)
// ─────────────────────────────────────────────
//
static DRAWER_START_RE: Lazy<Regex> =
    Lazy::new(|| Regex::new(r"(?i)^:([A-Za-z0-9_-]+):\s*$").unwrap());

static DRAWER_END_RE: Lazy<Regex> = Lazy::new(|| Regex::new(r"(?i)^:end:\s*$").unwrap());

// property key/value inside drawer (case-insensitive key match)
static PROP_LINE: Lazy<Regex> =
    Lazy::new(|| Regex::new(r"(?i)^\s*:([A-Za-z0-9_+-]+):\s*(.*?)\s*$").unwrap());

//
// ─────────────────────────────────────────────
//   PLANNING
// ─────────────────────────────────────────────
//
static PLAN_SCHEDULED: Lazy<Regex> = Lazy::new(|| Regex::new(r"SCHEDULED:\s*(<.*?>)").unwrap());

static PLAN_DEADLINE: Lazy<Regex> = Lazy::new(|| Regex::new(r"DEADLINE:\s*(<.*?>)").unwrap());

static PLAN_CLOSED: Lazy<Regex> = Lazy::new(|| Regex::new(r"CLOSED:\s*(<.*?>)").unwrap());

//
// ─────────────────────────────────────────────
//   BLOCKS: GENERIC BEGIN/END + RESULTS
// ─────────────────────────────────────────────
//

// #+BEGIN_<NAME> / #+END_<NAME>, case-insensitive
static BEGIN_BLOCK_RE: Lazy<Regex> =
    Lazy::new(|| Regex::new(r"(?i)^#\+begin_([A-Za-z0-9_-]+)").unwrap());

static END_BLOCK_RE: Lazy<Regex> =
    Lazy::new(|| Regex::new(r"(?i)^#\+end_([A-Za-z0-9_-]+)").unwrap());

// #+RESULTS:, case-insensitive
static RESULTS_BEGIN_RE: Lazy<Regex> = Lazy::new(|| Regex::new(r"(?i)^#\+results:").unwrap());

//
// ─────────────────────────────────────────────
//   LINK PARSER
// ─────────────────────────────────────────────
//

// [[target][desc]] oder [[target]]
static BRACKET_LINK_RE: Lazy<Regex> =
    Lazy::new(|| Regex::new(r"\[\[([^\]\[]+)(?:\]\[([^\]]*))?\]\]").unwrap());

// plain links (Org-mode compliant)
static PLAIN_LINK_RE: Lazy<Regex> = Lazy::new(|| {
    Regex::new(
        r"(?x)
        (
            (?:https?|ftp|mailto|news|id|file):[^\s\]]+
            | \#[A-Za-z0-9_\-]+
        )
    ",
    )
    .unwrap()
});

// Statistik-Cookie [0/3], [75%]
static STAT_COOKIE_RE: Lazy<Regex> =
    Lazy::new(|| Regex::new(r"\s*\[(?:\d+/\d+|\d+%)\]\s*$").unwrap());

// [[x][desc]] → desc
static TITLE_LINK_DESC_RE: Lazy<Regex> =
    Lazy::new(|| Regex::new(r"\[\[[^\]]+\]\[(.*?)\]\]").unwrap());

// [[x]] → x
static TITLE_LINK_TARGET_RE: Lazy<Regex> = Lazy::new(|| Regex::new(r"\[\[([^\]]+)\]\]").unwrap());

//
// ─────────────────────────────────────────────
//   RELATIVE → ABSOLUTE PATH (WITH ~ expansion)
// ─────────────────────────────────────────────
//
fn make_absolute_path(raw: &str, org_file: &str) -> Option<String> {
    // 1) ~ expansion -> absoluter Home-Pfad
    if let Some(stripped) = raw.strip_prefix("~/") {
        if let Some(home) = dirs::home_dir() {
            let expanded = home.join(stripped);
            return Some(normalize_path(&expanded).to_string_lossy().to_string());
        }
    }

    let p = Path::new(raw);

    // 2) already absolute?
    if p.is_absolute() {
        return Some(normalize_path(p).to_string_lossy().to_string());
    }

    // 3) base dir of the org file
    let org_file_path = Path::new(org_file);
    let absolute_org_path = if org_file_path.is_absolute() {
        org_file_path.to_path_buf()
    } else {
        let cwd = std::env::current_dir().unwrap_or_default();
        cwd.join(org_file_path)
    };

    let org_dir = absolute_org_path.parent().unwrap_or_else(|| Path::new("/"));

    // 4) relative -> absolute (no canonicalize)
    let combined = org_dir.join(raw);

    Some(normalize_path(&combined).to_string_lossy().to_string())
}

//
// ─────────────────────────────────────────────
//   LINK SCANNING
// ─────────────────────────────────────────────
//
fn parse_target_and_search(target: &str) -> (String, Option<String>) {
    if let Some(idx) = target.find("::") {
        let (left, rest) = target.split_at(idx);
        let right = &rest[2..];
        return (left.to_string(), Some(right.to_string()));
    }
    (target.to_string(), None)
}

fn scan_links(line: &str, offset: usize, org_file: &str) -> Vec<OrgLink> {
    let mut out = vec![];
    let mut used = vec![];

    // bracket links
    for cap in BRACKET_LINK_RE.captures_iter(line) {
        let m = cap.get(0).unwrap();
        used.push((m.start(), m.end()));

        let raw_target = cap.get(1).unwrap().as_str().to_string();
        let desc = cap.get(2).map(|m| m.as_str().to_string());
        let (path_raw, search_option) = parse_target_and_search(&raw_target);

        let (link_type, path) = if path_raw.starts_with('#') {
            ("anchor".to_string(), path_raw.clone())
        } else if let Some(idx) = path_raw.find(':') {
            let (l_type, p_val) = path_raw.split_at(idx);
            (l_type.to_string(), p_val[1..].to_string())
        } else {
            ("file".to_string(), path_raw.clone())
        };

        let path_absolute = if link_type == "file" {
            make_absolute_path(&path, org_file)
        } else {
            None
        };

        out.push(OrgLink {
            raw: m.as_str().to_string(),
            link_type,
            path,
            path_absolute,
            search_option,
            description: desc,
            format: "bracket".into(),
            pos: offset + m.start(),
        });
    }

    // plain links
    for cap in PLAIN_LINK_RE.captures_iter(line) {
        let m = cap.get(0).unwrap();

        // skip overlaps
        if used.iter().any(|(s, e)| m.start() >= *s && m.start() < *e) {
            continue;
        }

        let raw = m.as_str().to_string();
        let (path_raw, search_option) = parse_target_and_search(&raw);

        let (link_type, path) = if path_raw.starts_with('#') {
            ("anchor".to_string(), path_raw.clone())
        } else if let Some(idx) = path_raw.find(':') {
            let (l_type, p_val) = path_raw.split_at(idx);
            (l_type.to_string(), p_val[1..].to_string())
        } else {
            // Should not happen with the current regex for plain links
            ("unknown".to_string(), path_raw.clone())
        };

        let path_absolute = if link_type == "file" {
            make_absolute_path(&path, org_file)
        } else {
            None
        };

        out.push(OrgLink {
            raw: m.as_str().to_string(),
            link_type,
            path,
            path_absolute,
            search_option,
            description: None,
            format: "plain".into(),
            pos: offset + m.start(),
        });
    }

    out
}

//
// ─────────────────────────────────────────────
//   TITLE NORMALIZATION
// ─────────────────────────────────────────────
//
fn normalize_title(raw: &str) -> String {
    let mut t = raw.trim().to_string();

    t = TITLE_LINK_DESC_RE.replace_all(&t, "$1").to_string();
    t = TITLE_LINK_TARGET_RE.replace_all(&t, "$1").to_string();

    t.trim().to_string()
}

//
// ─────────────────────────────────────────────
//   TODO + PRIORITY EXTRACTION
// ─────────────────────────────────────────────
//
fn extract_todo(raw: &str, mode: &TodoMode) -> (Option<String>, String) {
    let mut parts = raw.split_whitespace();
    let first = parts.next().unwrap_or("").to_string();
    let remaining = parts.collect::<Vec<_>>().join(" ");

    match mode {
        TodoMode::UserDefined(list) => {
            if list.contains(&first) {
                return (Some(first), remaining);
            }
            (None, raw.to_string())
        }
        TodoMode::AutoUppercase => {
            if is_uppercase_word(&first) {
                (Some(first), remaining)
            } else {
                (None, raw.to_string())
            }
        }
    }
}

fn extract_priority(s: &str) -> (Option<String>, String) {
    if let Some(rest) = s.strip_prefix("[#") {
        if let Some(end) = rest.find(']') {
            let prio = rest[..end].to_string();
            let remaining = rest[end + 1..].trim().to_string();
            return (Some(prio), remaining);
        }
    }
    (None, s.to_string())
}

//
// ─────────────────────────────────────────────
//   TAG GROUP REMOVAL (:tag1:tag2:)
// ─────────────────────────────────────────────
//
fn strip_taggroup(raw: &str) -> (String, Vec<String>) {
    let s = raw.trim().to_string();
    let mut tags = vec![];

    if let Some(pos) = s.rfind(" :") {
        let (before, maybe_tags) = s.split_at(pos + 1);
        let trimmed = maybe_tags.trim();

        if trimmed.starts_with(':') && trimmed.ends_with(':') {
            for t in trimmed.split(':') {
                if !t.trim().is_empty() {
                    tags.push(t.trim().to_string());
                }
            }
            return (before.trim().to_string(), tags);
        }
    }

    (s, tags)
}

//
// ─────────────────────────────────────────────
//   COOKIE REMOVAL
// ─────────────────────────────────────────────
//
fn strip_cookie(s: &str) -> String {
    STAT_COOKIE_RE.replace(s, "").to_string().trim().to_string()
}

//
// ─────────────────────────────────────────────
//   MAIN PARSER ENTRY
// ─────────────────────────────────────────────
//
pub fn parse_org_from_file(
    path: &str,
    todo_cli: Option<&str>,
    todo_file: Option<&str>,
) -> std::io::Result<Vec<OrgHeading>> {
    let content = std::fs::read_to_string(path)?;
    Ok(parse_org(&content, path, todo_cli, todo_file))
}

fn parse_file_metadata(input: &str) -> (Option<String>, Vec<String>, Vec<(String, String)>) {
    let mut filetitle = None;
    let mut filetags = vec![];
    let mut fileprops = vec![];

    for line in input.lines() {
        if HEADING_RE.is_match(line) {
            break;
        }
        if let Some(c) = FILETITLE_RE.captures(line) {
            filetitle = Some(c[1].trim().to_string());
            continue;
        }
        if let Some(c) = FILETAGS_RE.captures(line) {
            for t in c[1].split(':') {
                if !t.trim().is_empty() {
                    filetags.push(t.trim().to_string());
                }
            }
            continue;
        }
        if let Some(c) = FILEPROP_RE.captures(line) {
            fileprops.push((c[1].to_string(), c[2].to_string()));
            continue;
        }
    }

    (filetitle, filetags, fileprops)
}

pub fn parse_org(
    input: &str,
    filename: &str,
    todo_cli: Option<&str>,
    todo_file: Option<&str>,
) -> Vec<OrgHeading> {
    // TODO rules
    let todo_mode = crate::config::load_todo_keywords(todo_cli, todo_file);

    // FILE metadata
    let (filetitle, filetags, fileprops) = parse_file_metadata(input);

    let default_title = Path::new(filename)
        .file_stem()
        .unwrap()
        .to_string_lossy()
        .to_string();

    // ROOT heading
    let mut headings = vec![];

    headings.push(OrgHeading {
        level: 0,
        parent_id: None,
        title_raw: filetitle.clone().unwrap_or_else(|| default_title.clone()),
        title: filetitle.unwrap_or(default_title),
        tags: filetags,
        inherited_tags: vec![],
        properties: fileprops,
        inherited_properties: vec![],
        scheduled: None,
        deadline: None,
        closed: None,
        todo: None,
        priority: None,
        links: vec![],
        outline: vec![],
        file: true,
    });

    // parse loop
    let mut body_headings = parse_body(input, filename, &todo_mode);
    headings.append(&mut body_headings);

    // INHERITANCE + PATH BUILDING
    build_inheritance(&mut headings);

    headings
}

fn parse_body(input: &str, filename: &str, todo_mode: &TodoMode) -> Vec<OrgHeading> {
    let mut headings = vec![];
    let mut current: Option<OrgHeading> = None;
    let mut in_src = false;
    let mut in_example = false;
    let mut in_comment_block = false;
    let mut drawer_stack: Vec<String> = vec![];
    let mut temp_props = vec![];
    let mut file_pos = 0;

    for line in input.lines() {
        let trimmed = line.trim();

        // ─────────────────────────────
        // BEGIN_/END_ blocks (case-insensitive)
        // ─────────────────────────────
        if let Some(c) = BEGIN_BLOCK_RE.captures(trimmed) {
            let block = c[1].to_ascii_lowercase();
            match block.as_str() {
                "src" => in_src = true,
                "example" => in_example = true,
                "comment" => in_comment_block = true,
                _ => {}
            }
            file_pos += line.len() + 1;
            continue;
        }

        if let Some(c) = END_BLOCK_RE.captures(trimmed) {
            let block = c[1].to_ascii_lowercase();
            match block.as_str() {
                "src" => in_src = false,
                "example" => in_example = false,
                "comment" => in_comment_block = false,
                _ => {}
            }
            file_pos += line.len() + 1;
            continue;
        }

        // If we are inside a comment/src/example block: ignore everything until END_
        if in_src || in_example || in_comment_block {
            file_pos += line.len() + 1;
            continue;
        }

        // #+RESULTS: line itself → überspringen, aber Folgezeilen normal parsen
        if RESULTS_BEGIN_RE.is_match(trimmed) {
            file_pos += line.len() + 1;
            continue;
        }

        // DRAWER start
        if let Some(cap) = DRAWER_START_RE.captures(trimmed) {
            drawer_stack.push(cap[1].to_ascii_uppercase());
            file_pos += line.len() + 1;
            continue;
        }

        // DRAWER end
        if DRAWER_END_RE.is_match(trimmed) {
            if let Some(name) = drawer_stack.pop() {
                if name == "PROPERTIES" {
                    if let Some(h) = current.as_mut() {
                        h.properties.extend(temp_props.clone());
                    }
                    temp_props.clear();
                }
            }
            file_pos += line.len() + 1;
            continue;
        }

        // PROPERTIES drawer content
        let in_properties = drawer_stack
            .last()
            .map(|d| d == "PROPERTIES")
            .unwrap_or(false);

        if in_properties {
            if let Some(c) = PROP_LINE.captures(line) {
                let key = c[1].trim_end_matches('+').to_string();
                let val = c[2].to_string();
                temp_props.push((key, val));
            }
            file_pos += line.len() + 1;
            continue;
        }

        // Fixed-width: Zeile startet mit ":" und wir sind NICHT in einem Drawer
        if trimmed.starts_with(':') && drawer_stack.is_empty() {
            file_pos += line.len() + 1;
            continue;
        }

        // Kommentar-Zeilen: "# ..." aber NICHT "#+..."
        if trimmed.starts_with('#') && !trimmed.starts_with("#+") {
            file_pos += line.len() + 1;
            continue;
        }

        // HEADING
        if let Some(cap) = HEADING_RE.captures(line) {
            if let Some(h) = current.take() {
                headings.push(h);
            }

            let level = cap["stars"].len() as u8;

            let full = cap["title_raw"].trim();
            let no_cookie = strip_cookie(full);
            let (no_tags, tags) = strip_taggroup(&no_cookie);

            let (todo, after_todo) = extract_todo(&no_tags, todo_mode);
            let (priority, after_prio) = extract_priority(&after_todo);

            let title_raw = after_prio.trim().to_string();
            let title = normalize_title(&title_raw);

            let links = scan_links(line, file_pos, filename);

            current = Some(OrgHeading {
                level,
                todo,
                priority,
                title_raw,
                title,
                tags,
                inherited_tags: vec![],
                properties: vec![],
                inherited_properties: vec![],
                scheduled: None,
                deadline: None,
                closed: None,
                links,
                parent_id: None,
                outline: vec![],
                file: false,
            });

            file_pos += line.len() + 1;
            continue;
        }

        // PLANNING
        if let Some(h) = current.as_mut() {
            if let Some(c) = PLAN_SCHEDULED.captures(line) {
                h.scheduled = Some(c[1].to_string());
            }
            if let Some(c) = PLAN_DEADLINE.captures(line) {
                h.deadline = Some(c[1].to_string());
            }
            if let Some(c) = PLAN_CLOSED.captures(line) {
                h.closed = Some(c[1].to_string());
            }
        }

        // BODY LINKS ONLY (body not stored)
        if let Some(h) = current.as_mut() {
            // Wir sind hier garantiert NICHT in src/example/comment/properties-blocken
            h.links.extend(scan_links(line, file_pos, filename));
        }

        file_pos += line.len() + 1;
    }

    if let Some(h) = current {
        headings.push(h);
    }

    headings
}

fn build_inheritance(headings: &mut [OrgHeading]) {
    let mut stack = vec![0];

    for i in 1..headings.len() {
        let lvl = headings[i].level;

        while let Some(&top) = stack.last() {
            if headings[top].level < lvl {
                break;
            }
            stack.pop();
        }

        if let Some(&p) = stack.last() {
            let mut parent_outline = headings[p].outline.clone();
            let parent_title = headings[p].title.clone();

            let mut inh_tags = headings[p].inherited_tags.clone();
            inh_tags.extend(headings[p].tags.clone());

            let mut inh_props = headings[p].inherited_properties.clone();
            inh_props.extend(headings[p].properties.clone());

            let h = &mut headings[i];

            h.parent_id = Some(p);
            parent_outline.push(parent_title);
            h.outline = parent_outline;
            h.inherited_tags = inh_tags;
            h.inherited_properties = inh_props;
        }

        stack.push(i);
    }
}
