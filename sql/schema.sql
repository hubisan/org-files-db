/*
  Authoritative SQLite schema for org-files-db.

  Notes:

  - Every file gets exactly one synthetic level 0 heading.
  - File-level keywords, properties, file tags, and file-level links attach to
    the level 0 heading.
  - Rich timestamp tables are deferred for now.
  - heading_fts is created conditionally by replacing the marker block below.
*/

CREATE TABLE IF NOT EXISTS files (
    id              INTEGER PRIMARY KEY,
    path            TEXT NOT NULL UNIQUE,
    mtime_ns        INTEGER NOT NULL,
    size            INTEGER NOT NULL,
    content_hash    TEXT,
    indexed_at      INTEGER
);

CREATE TABLE IF NOT EXISTS headings (
    id                  INTEGER PRIMARY KEY,
    file_id             INTEGER NOT NULL,
    parent_id           INTEGER,
    level               INTEGER NOT NULL CHECK (level >= 0),
    line_number         INTEGER,
    byte_start          INTEGER NOT NULL,
    byte_end            INTEGER NOT NULL CHECK (byte_end >= byte_start),
    title               TEXT NOT NULL,
    title_raw           TEXT NOT NULL,
    todo_keyword        TEXT,
    todo_type           TEXT CHECK (todo_type IN ('open', 'closed') OR todo_type IS NULL),
    priority            TEXT CHECK (priority IS NULL OR length(priority) = 1),
    scheduled_raw       TEXT,
    scheduled_ts        INTEGER,
    deadline_raw        TEXT,
    deadline_ts         INTEGER,
    closed_raw          TEXT,
    closed_ts           INTEGER,
    archivedp           INTEGER NOT NULL DEFAULT 0 CHECK (archivedp IN (0, 1)),
    footnote_section_p  INTEGER NOT NULL DEFAULT 0 CHECK (footnote_section_p IN (0, 1)),
    all_tags_json       TEXT NOT NULL DEFAULT '[]',
    CHECK (
        (level = 0 AND parent_id IS NULL)
        OR
        (level > 0 AND parent_id IS NOT NULL)
    ),
    FOREIGN KEY (file_id)
        REFERENCES files(id)
        ON DELETE CASCADE,
    FOREIGN KEY (parent_id)
        REFERENCES headings(id)
        ON DELETE CASCADE,
    UNIQUE (file_id, byte_start)
);

CREATE UNIQUE INDEX IF NOT EXISTS uq_headings_file_level0
    ON headings(file_id)
    WHERE level = 0;

CREATE TABLE IF NOT EXISTS todo_keywords (
    file_id         INTEGER NOT NULL,
    keyword         TEXT NOT NULL,
    state_type      TEXT NOT NULL CHECK (state_type IN ('open', 'closed')),
    shortcut        TEXT CHECK (shortcut IS NULL OR length(shortcut) = 1),
    sequence_no     INTEGER NOT NULL,
    FOREIGN KEY (file_id)
        REFERENCES files(id)
        ON DELETE CASCADE,
    PRIMARY KEY (file_id, keyword)
);

CREATE TABLE IF NOT EXISTS keywords (
    id              INTEGER PRIMARY KEY,
    heading_id      INTEGER NOT NULL,
    keyword         TEXT NOT NULL,
    value           TEXT,
    line_number     INTEGER,
    FOREIGN KEY (heading_id)
        REFERENCES headings(id)
        ON DELETE CASCADE,
    UNIQUE (heading_id, keyword, line_number)
);

CREATE TABLE IF NOT EXISTS properties (
    id              INTEGER PRIMARY KEY,
    heading_id      INTEGER NOT NULL,
    key             TEXT NOT NULL,
    value           TEXT,
    source          TEXT NOT NULL CHECK (
                        source IN ('property_keyword', 'property_drawer', 'category_keyword')
                    ),
    inherited       INTEGER NOT NULL DEFAULT 0 CHECK (inherited IN (0, 1)),
    line_number     INTEGER,
    FOREIGN KEY (heading_id)
        REFERENCES headings(id)
        ON DELETE CASCADE,
    UNIQUE (heading_id, key, source, inherited)
);

CREATE TABLE IF NOT EXISTS tags (
    heading_id      INTEGER NOT NULL,
    tag             TEXT NOT NULL,
    inherited       INTEGER NOT NULL DEFAULT 0 CHECK (inherited IN (0, 1)),
    FOREIGN KEY (heading_id)
        REFERENCES headings(id)
        ON DELETE CASCADE,
    PRIMARY KEY (heading_id, tag, inherited)
);

CREATE TABLE IF NOT EXISTS links (
    id                  INTEGER PRIMARY KEY,
    file_id             INTEGER NOT NULL,
    heading_id          INTEGER NOT NULL,
    byte_start          INTEGER NOT NULL,
    byte_end            INTEGER NOT NULL CHECK (byte_end >= byte_start),
    line_number         INTEGER,
    link_type           TEXT,
    target              TEXT NOT NULL,
    target_absolute     TEXT,
    raw_link            TEXT NOT NULL,
    description         TEXT,
    format              TEXT CHECK (format IN ('plain', 'bracket', 'angle') OR format IS NULL),
    search_option       TEXT,
    relation            TEXT,
    resolved_file_id    INTEGER,
    resolved_heading_id INTEGER,
    resolved            INTEGER NOT NULL DEFAULT 0 CHECK (resolved IN (0, 1)),
    broken              INTEGER NOT NULL DEFAULT 0 CHECK (broken IN (0, 1)),
    diagnostic          TEXT,
    FOREIGN KEY (file_id)
        REFERENCES files(id)
        ON DELETE CASCADE,
    FOREIGN KEY (heading_id)
        REFERENCES headings(id)
        ON DELETE CASCADE,
    FOREIGN KEY (resolved_file_id)
        REFERENCES files(id)
        ON DELETE SET NULL,
    FOREIGN KEY (resolved_heading_id)
        REFERENCES headings(id)
        ON DELETE SET NULL,
    UNIQUE (file_id, byte_start)
);

CREATE TABLE IF NOT EXISTS heading_bodies (
    heading_id          INTEGER PRIMARY KEY,
    body_text           TEXT NOT NULL,
    body_byte_start     INTEGER,
    body_byte_end       INTEGER,
    FOREIGN KEY (heading_id)
        REFERENCES headings(id)
        ON DELETE CASCADE
);

CREATE TABLE IF NOT EXISTS outline_path (
    heading_id          INTEGER PRIMARY KEY,
    file_id             INTEGER NOT NULL,
    parent_id           INTEGER,
    depth               INTEGER NOT NULL CHECK (depth >= 0),
    materialized_path   TEXT NOT NULL,
    breadcrumbs_json    TEXT NOT NULL,
    FOREIGN KEY (heading_id)
        REFERENCES headings(id)
        ON DELETE CASCADE,
    FOREIGN KEY (file_id)
        REFERENCES files(id)
        ON DELETE CASCADE,
    FOREIGN KEY (parent_id)
        REFERENCES headings(id)
        ON DELETE SET NULL
);

-- heading_fts placeholder
/*__HEADING_FTS__*/;

CREATE INDEX IF NOT EXISTS idx_files_mtime_size
    ON files(mtime_ns, size);

CREATE INDEX IF NOT EXISTS idx_files_hash
    ON files(content_hash);

CREATE INDEX IF NOT EXISTS idx_headings_parent_id
    ON headings(parent_id);

CREATE INDEX IF NOT EXISTS idx_headings_todo
    ON headings(todo_keyword);

CREATE INDEX IF NOT EXISTS idx_headings_todo_type
    ON headings(todo_type);

CREATE INDEX IF NOT EXISTS idx_headings_scheduled
    ON headings(scheduled_ts);

CREATE INDEX IF NOT EXISTS idx_headings_deadline
    ON headings(deadline_ts);

CREATE INDEX IF NOT EXISTS idx_headings_closed
    ON headings(closed_ts);

CREATE INDEX IF NOT EXISTS idx_todo_keywords_file_state
    ON todo_keywords(file_id, state_type);

CREATE INDEX IF NOT EXISTS idx_keywords_heading
    ON keywords(heading_id);

CREATE INDEX IF NOT EXISTS idx_keywords_keyword
    ON keywords(keyword);

CREATE INDEX IF NOT EXISTS idx_properties_heading
    ON properties(heading_id);

CREATE INDEX IF NOT EXISTS idx_properties_key_value
    ON properties(key, value);

CREATE INDEX IF NOT EXISTS idx_properties_id
    ON properties(value)
    WHERE key = 'ID';

CREATE INDEX IF NOT EXISTS idx_properties_custom_id
    ON properties(value)
    WHERE key = 'CUSTOM_ID';

CREATE INDEX IF NOT EXISTS idx_tags_tag
    ON tags(tag);

CREATE INDEX IF NOT EXISTS idx_tags_heading
    ON tags(heading_id);

CREATE INDEX IF NOT EXISTS idx_links_heading
    ON links(heading_id);

CREATE INDEX IF NOT EXISTS idx_links_target
    ON links(target);

CREATE INDEX IF NOT EXISTS idx_links_resolved_file
    ON links(resolved_file_id);

CREATE INDEX IF NOT EXISTS idx_links_resolved_heading
    ON links(resolved_heading_id);

CREATE INDEX IF NOT EXISTS idx_outline_file_materialized_path
    ON outline_path(file_id, materialized_path);

CREATE INDEX IF NOT EXISTS idx_outline_parent
    ON outline_path(parent_id);
