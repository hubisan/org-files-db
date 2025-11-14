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
    modification_time   INTEGER NOT NULL,
    -- TODO noch Typ anpassen, kann bei Prüfen auf Veränderung
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

    all_tags              TEXT,

    CONSTRAINT uq_headings_external_id UNIQUE (external_id),

    CONSTRAINT fk_headings_file
        FOREIGN KEY (file_id) REFERENCES files(id) ON DELETE CASCADE,

    CONSTRAINT fk_headings_parent
        FOREIGN KEY (parent_id) REFERENCES headings(id) ON DELETE CASCADE
);

--------------------------------------------------
-- HEADING_PATH
-- Materialized Path
--
-- Speichert den vollständigen Pfad eines Headings als
-- normalisierte Liste.
--
-- Beispiel:
--   Pfad: ["Meine Testdatei", "Hauptaufgabe", "Subtask"]
--
-- Wird gespeichert als:
--   heading_id | depth | title
--        5     |   0   | "Meine Testdatei"
--        5     |   1   | "Hauptaufgabe"
--        5     |   2   | "Subtask"        <-- self
--
-- Warum getrennt?
--   - Pfade müssen sortiert werden → depth notwendig
--   - Kein JSON-Parsing: viel schneller
--   - JOINs sind trivial (GROUP_CONCAT)
--   - parent_id allein würde rekursive SQL-CTEs erfordern (langsam)
-- is_self = 0 → ancestor
-- is_self = 1 → das Heading selbst
--------------------------------------------------
CREATE TABLE IF NOT EXISTS heading_path (
    heading_id  INTEGER NOT NULL,
    depth       INTEGER NOT NULL,
    title       TEXT NOT NULL,
    is_self     INTEGER NOT NULL DEFAULT 0,

    CONSTRAINT fk_hp_heading
        FOREIGN KEY (heading_id)
            REFERENCES headings(id)
            ON DELETE CASCADE,

    PRIMARY KEY (heading_id, depth)
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
    key         TEXT NOT NULL,
    value       TEXT,
    inherited   BOOLEAN NOT NULL DEFAULT 0,    -- 0=lokal, 1=vererbt

    CONSTRAINT ck_kp_type
        CHECK (type IN ('keyword', 'property')),

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
    file_id        INTEGER NOT NULL,
    heading_id     INTEGER,
    pos            INTEGER NOT NULL,

    type           TEXT,
    path           TEXT NOT NULL,
    path_absolute  TEXT,
    description    TEXT,

    format         TEXT,
    search_option  TEXT,

    CONSTRAINT ck_links_format CHECK (format IN ('plain', 'bracket')),

    CONSTRAINT fk_links_file
        FOREIGN KEY (file_id) REFERENCES files(id) ON DELETE CASCADE,

    CONSTRAINT fk_links_heading
        FOREIGN KEY (heading_id) REFERENCES headings(id)
            ON DELETE SET NULL,

    CONSTRAINT uq_links_file_pos UNIQUE (file_id, pos)
);

--------------------------------------------------
-- CITATIONS
--------------------------------------------------
CREATE TABLE IF NOT EXISTS citations (
    id             INTEGER PRIMARY KEY AUTOINCREMENT,
    file_id        INTEGER NOT NULL,
    heading_id     INTEGER,
    pos            INTEGER NOT NULL,
    cite_key       TEXT NOT NULL,

    CONSTRAINT fk_citations_file
        FOREIGN KEY (file_id) REFERENCES files(id) ON DELETE CASCADE,

    CONSTRAINT fk_citations_heading
        FOREIGN KEY (heading_id) REFERENCES headings(id)
            ON DELETE SET NULL,

    CONSTRAINT uq_citations UNIQUE (file_id, pos, cite_key)
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

-- CITATIONS
CREATE INDEX IF NOT EXISTS idx_citations_key
    ON citations(cite_key);

-- HEADING PATH
CREATE INDEX IF NOT EXISTS idx_hp_heading
    ON heading_path(heading_id);

CREATE INDEX IF NOT EXISTS idx_hp_depth
    ON heading_path(depth);

CREATE INDEX IF NOT EXISTS idx_hp_self
    ON heading_path(is_self);

COMMIT;
