/*
  TODO
  Recheck wit AI if this is the fastest system for querying.
  It should be a datawarehouse as its sole purpose is querying.
*/
BEGIN TRANSACTION;

PRAGMA foreign_keys = ON;

--------------------------------------------------
-- FILES
--------------------------------------------------
CREATE TABLE IF NOT EXISTS files (
    id                  INTEGER PRIMARY KEY AUTOINCREMENT,
    path                TEXT NOT NULL,

    -- optional fields (kannst du löschen, wenn nicht benötigt)
    title               TEXT,
    created_at          INTEGER NOT NULL DEFAULT (unixepoch()),
    updated_at          INTEGER NOT NULL DEFAULT (unixepoch()),
    md5_hash            TEXT,
    -- TODO ist mtime nicht float?
    modification_time   INTEGER NOT NULL,
    -- TODO kann bei Prüfen auf Veränderung
    -- verwendet werden. wenn mtime anders und size ist es bestimmt
    -- nicht mehr das Gleiche
    size               INTEGER NOT NULL,

    CONSTRAINT uq_files_path UNIQUE (path)
);

--------------------------------------------------
-- HEADINGS
--------------------------------------------------
CREATE TABLE IF NOT EXISTS headings (
    id                    INTEGER PRIMARY KEY AUTOINCREMENT,
    file_id               INTEGER NOT NULL,
    external_id           INTEGER,

    level                 INTEGER NOT NULL,
    line                  INTEGER NOT NULL,
    begin                 INTEGER NOT NULL,

    title                 TEXT,
    title_raw             TEXT,
    todo_keyword          TEXT,
    priority              TEXT,
    scheduled_ts          INTEGER,
    deadline_ts           INTEGER,
    closed_ts             INTEGER,
    parent_id             INTEGER,

    is_root               INTEGER NOT NULL DEFAULT 0,
    is_archived           INTEGER NOT NULL DEFAULT 0,
    is_footnote_section   INTEGER NOT NULL DEFAULT 0,

    -- TODO like in org :tag1:tag2:tag3: or as json?
    all_tags              TEXT,

    CONSTRAINT uq_headings_external_id UNIQUE (external_id),

    CONSTRAINT fk_headings_file
        FOREIGN KEY (file_id) REFERENCES files(id) ON DELETE CASCADE,

    CONSTRAINT fk_headings_parent
        FOREIGN KEY (parent_id) REFERENCES headings(id) ON DELETE CASCADE
);

/*
--------------------------------------------------------------------------------
TABLE: outline

PURPOSE:
    The 'outline' table exists purely to speed up hierarchical queries by
    materializing the structural relationships: parent linkage, depth,
    materialized path, and breadcrumb titles (JSON).

    If a file changes it is dropped and reparsed. Therefore `file_id` has ON
    DELETE CASCADE, all headings and outline rows belonging to that file are
    automatically removed.

--------------------------------------------------------------------------------
*/

CREATE TABLE outline_path (
    file_id           INTEGER NOT NULL,
    heading_id        INTEGER NOT NULL,
    parent_id         INTEGER,
    -- Depth in the hierarchy: 0 = top-level, 1 = child, 2 = grandchild...
    depth             INTEGER NOT NULL,

    -- Zero-padded numeric hierarchical path.
    -- Examples:
    --   "0001"
    --   "0001.0002"
    --   "0001.0002.0001"
    materialized_path TEXT NOT NULL,

    -- JSON array containing breadcrumb titles from root to this heading
    -- including this heading: ["Project","Phase 1","Analysis"]
    breadcrumbs       TEXT NOT NULL,

    CONSTRAINT fk_outline_file
        FOREIGN KEY (file_id)
        REFERENCES files (id)
        ON DELETE CASCADE,
    CONSTRAINT fk_outline_heading
        FOREIGN KEY (heading_id)
        REFERENCES headings (id),
    CONSTRAINT fk_outline_parent
        FOREIGN KEY (parent_id)
        REFERENCES headings (id)
);

--------------------------------------------------
-- TAGS
--------------------------------------------------
CREATE TABLE IF NOT EXISTS tags (
    heading_id   INTEGER NOT NULL,
    tag          TEXT NOT NULL,
    inherited    BOOLEAN NOT NULL DEFAULT 0,   -- 0=lokal, 1=vererbt

    CONSTRAINT fk_tags_heading
        FOREIGN KEY (heading_id)
            REFERENCES headings(id)
            ON DELETE CASCADE,

    CONSTRAINT pk_tags
        PRIMARY KEY (heading_id, tag, inherited)
);

--------------------------------------------------
-- KEYWORDS + PROPERTIES (vereinheitlicht)
--------------------------------------------------
CREATE TABLE IF NOT EXISTS keywords_properties (
    id          INTEGER PRIMARY KEY AUTOINCREMENT,
    heading_id  INTEGER NOT NULL,

    type        TEXT NOT NULL,     -- 'keyword' oder 'property'
    -- Properties can be set by keyword or in drawer
    prop_from   TEXT NOT NULL,
    key         TEXT NOT NULL,
    value       TEXT,
    inherited   BOOLEAN NOT NULL DEFAULT 0,    -- 0=lokal, 1=vererbt

    CONSTRAINT ck_kp_type
        CHECK (type IN ('keyword', 'property')),

    CONSTRAINT ck_kp_prop_from
        CHECK (prop_from IN ('keyword', 'drawer')),

    CONSTRAINT fk_kp_heading
        FOREIGN KEY (heading_id)
            REFERENCES headings(id)
            ON DELETE CASCADE,

    CONSTRAINT uq_kp
        UNIQUE (heading_id, key, type, inherited)
);

--------------------------------------------------
-- LINKS
--------------------------------------------------
CREATE TABLE IF NOT EXISTS links (
    id             INTEGER PRIMARY KEY AUTOINCREMENT,
    -- File in dem sich der Link befindet
    file_id        INTEGER NOT NULL,
    -- Heading unter welcher sich der link befindet. Null
    -- wenn file level
    heading_id        INTEGER,
    begin            INTEGER NOT NULL,
    end            INTEGER NOT NULL,
    line                  INTEGER NOT NULL,

    type           TEXT,
    path           TEXT NOT NULL,

    -- only for type file
    path_absolute  TEXT,
    link_broken    BOOLEAN NOT NULL DEFAULT 0, -- 0=file/folder exists, 1=link is broken
    target_file_id        INTEGER,
    target_heading_id     INTEGER,

    description    TEXT,

    format         TEXT,
    search_option  TEXT,

    -- Create a possibility to store the relation in the link.
    -- [[https://www.example.com][Example (->Inhaber)]]
    relation TEXT,

    CONSTRAINT ck_links_format CHECK (format IN ('plain', 'bracket', 'angle')),

    CONSTRAINT fk_links_file
    FOREIGN KEY (file_id) REFERENCES files(id) ON DELETE CASCADE,

    CONSTRAINT fk_links_heading
    FOREIGN KEY (heading_id) REFERENCES headings(id) ON DELETE SET NULL,

    CONSTRAINT fk_links_target_file
    FOREIGN KEY (target_file_id) REFERENCES files(id) ON DELETE SET NULL,

    CONSTRAINT fk_links_target_heading
    FOREIGN KEY (target_heading_id) REFERENCES headings(id) ON DELETE SET NULL,

    CONSTRAINT uq_links_file_pos UNIQUE (file_id, begin)
);

--------------------------------------------------
-- INDEXES
--------------------------------------------------

-- FILES
CREATE INDEX IF NOT EXISTS idx_files_path
    ON files(path);

CREATE INDEX IF NOT EXISTS idx_files_mtime
    ON files(modification_time);

CREATE INDEX IF NOT EXISTS idx_files_md5
    ON files(md5_hash);

-- HEADINGS
CREATE INDEX IF NOT EXISTS idx_headings_file_id
    ON headings(file_id);

CREATE INDEX IF NOT EXISTS idx_headings_parent_id
    ON headings(parent_id);

CREATE INDEX IF NOT EXISTS idx_headings_todo
    ON headings(todo_keyword);

CREATE INDEX IF NOT EXISTS idx_headings_external_id
    ON headings(external_id);

CREATE INDEX IF NOT EXISTS idx_headings_scheduled
    ON headings(scheduled_ts);

CREATE INDEX IF NOT EXISTS idx_headings_deadline
    ON headings(deadline_ts);

-- TAGS
CREATE INDEX IF NOT EXISTS idx_tags_tag ON tags(tag);
CREATE INDEX IF NOT EXISTS idx_tags_inherited ON tags(inherited);

-- KEYWORDS/PROPERTIES
CREATE INDEX IF NOT EXISTS idx_kp_heading ON keywords_properties(heading_id);
CREATE INDEX IF NOT EXISTS idx_kp_key ON keywords_properties(key);
CREATE INDEX IF NOT EXISTS idx_kp_inherited ON keywords_properties(inherited);

-- LINKS
CREATE INDEX IF NOT EXISTS idx_links_file
    ON links(file_id);

CREATE INDEX IF NOT EXISTS idx_links_heading
    ON links(heading_id);

CREATE INDEX IF NOT EXISTS idx_links_path
    ON links(path);

CREATE INDEX IF NOT EXISTS idx_links_type
  ON links(type);

-- HEADING PATH

CREATE INDEX IF NOT EXISTS idx_outline_file_id
  ON outline_path(file_id);
CREATE INDEX IF NOT EXISTS idx_outline_heading_id
  ON outline_path(heading_id);
CREATE INDEX IF NOT EXISTS idx_outline_parent_id
  ON outline_path(parent_id);
CREATE INDEX IF NOT EXISTS idx_outline_materialized_path
  ON outline_path(materialized_path);

COMMIT;
