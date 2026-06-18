/*
  SQLite Database Schema for org-files-db

  This schema is a discussion draft.

  Main design decisions:

  - Every Org file gets one synthetic level 0 heading.
    This level 0 heading represents the file-level scope.

  - File-level keywords, file-level properties, FILETAGS, links outside
    regular headings, and full-text-search content can all be attached to
    this level 0 heading.

  - Regular Org headings start at level 1.

  - byte_start and byte_end are UTF-8 byte offsets into the original file.
    byte_start is the main position used by editor integrations such as Emacs
    to jump to a heading or link location.

  - outline_path and heading_fts are derived/index tables. They can be rebuilt
    from the core tables when a file is reparsed.

  - The schema is optimized for querying and full rebuilds per changed file.
*/

PRAGMA foreign_keys = ON;
PRAGMA user_version = 1;

BEGIN TRANSACTION;

--------------------------------------------------
-- FILES
--------------------------------------------------
/*
  Each row represents one indexed Org file.

  path:
    Absolute normalized path. Must be unique.

  mtime_ns:
    Last modification time in nanoseconds since Unix epoch.
    This avoids ambiguity from filesystems with subsecond precision.

  size:
    File size in bytes. Used with mtime_ns as a fast change-detection signal.

  content_hash:
    Optional content hash. Used as a safer change-detection signal when needed.
    The column name does not specify MD5/SHA/BLAKE3 so the algorithm can change
    without a schema rename.

  created_at / updated_at / indexed_at:
    Unix timestamps in seconds.
*/
CREATE TABLE IF NOT EXISTS files (
    id              INTEGER PRIMARY KEY,
    path            TEXT NOT NULL UNIQUE,

    title           TEXT,

    created_at      INTEGER NOT NULL DEFAULT (unixepoch()),
    updated_at      INTEGER NOT NULL DEFAULT (unixepoch()),
    indexed_at      INTEGER,

    mtime_ns        INTEGER NOT NULL,
    size            INTEGER NOT NULL,
    content_hash    TEXT
);

--------------------------------------------------
-- HEADINGS
--------------------------------------------------
/*
  Each row represents either a real Org heading or the synthetic file-level
  heading.

  Level 0 heading:

    A level 0 heading is created for every file. It represents the file-level
    scope. This allows file-wide elements such as keywords, file tags,
    properties, file-level links, and file-level full-text content to be stored
    in the same hierarchy as regular headings.

    For each file, exactly one level 0 heading should exist.

  level:
    0 = synthetic file-level heading
    1 = top-level Org heading
    2 = child heading
    etc.

  byte_start / byte_end:
    UTF-8 byte offsets into the original file.
    For regular headings, byte_start points to the beginning of the heading.
    For the level 0 heading, byte_start should normally be 0 and byte_end should
    normally be the file length.

  title:
    Normalized display title. For regular headings this excludes TODO keyword,
    priority, tags, statistic cookies, and markup details where normalization is
    supported. Links should be converted to text, using the description if
    present, otherwise the link itself.

  title_raw:
    Raw heading title text, excluding the TODO keyword but preserving more of
    the original Org syntax.

  todo_keyword:
    The TODO state found on the heading, for example TODO, NEXT, PLAN, DONE.

  todo_type:
    open or closed, resolved from the active TODO keyword configuration.

  parent_id:
    Parent heading. NULL for the level 0 heading. Top-level Org headings should
    normally have the level 0 heading as parent.

  all_tags_json:
    JSON array of all tags visible on this heading, including inherited tags.
    This is a query/output convenience cache. The normalized tag rows are stored
    in tags.

  archivedp / footnote_section_p:
    Stored as 0/1 integers.
*/
CREATE TABLE IF NOT EXISTS headings (
    id                  INTEGER PRIMARY KEY,
    file_id             INTEGER NOT NULL,

    parent_id           INTEGER,

    level               INTEGER NOT NULL CHECK (level >= 0),
    line_number         INTEGER,
    byte_start          INTEGER NOT NULL,
    byte_end            INTEGER NOT NULL,

    title               TEXT NOT NULL,
    title_raw           TEXT,

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

    FOREIGN KEY (file_id)
        REFERENCES files(id)
        ON DELETE CASCADE,

    FOREIGN KEY (parent_id)
        REFERENCES headings(id)
        ON DELETE CASCADE,

    UNIQUE (file_id, byte_start)
);

/*
  Enforce exactly one synthetic level 0 heading per file.

  SQLite partial unique indexes are used because normal headings may have many
  rows per file, but level 0 must be unique.
*/
CREATE UNIQUE INDEX IF NOT EXISTS uq_headings_file_level0
    ON headings(file_id)
    WHERE level = 0;

--------------------------------------------------
-- TODO KEYWORDS
--------------------------------------------------
/*
  Each row represents a TODO keyword that is active for one file.

  These rows are derived from file-local #+TODO lines if present, otherwise from
  the configured project defaults.

  Example:

    #+TODO: TODO(t) NEXT(n) PLAN(p) BUILD(b) REVIEW(r) CONTINUE(C) | DONE(d) CANCEL(c)

  Stored as:

    TODO      open    t
    NEXT      open    n
    PLAN      open    p
    BUILD     open    b
    REVIEW    open    r
    CONTINUE  open    C
    DONE      closed  d
    CANCEL    closed  c

  This table explains how headings.todo_keyword was interpreted.
*/
CREATE TABLE IF NOT EXISTS todo_keywords (
    file_id         INTEGER NOT NULL,
    keyword         TEXT NOT NULL,
    state_type      TEXT NOT NULL CHECK (state_type IN ('open', 'closed')),
    shortcut        TEXT,
    sequence_no     INTEGER NOT NULL,

    FOREIGN KEY (file_id)
        REFERENCES files(id)
        ON DELETE CASCADE,

    PRIMARY KEY (file_id, keyword)
);

--------------------------------------------------
-- KEYWORDS
--------------------------------------------------
/*
  Each row represents an Org keyword line.

  File-level keywords such as #+TITLE, #+AUTHOR, #+STARTUP, #+OPTIONS and
  #+EXPORT_FILE_NAME are attached to the level 0 heading.

  Keywords that are semantically relevant to parser behavior, especially
  #+TODO, may also be represented in specialized tables such as todo_keywords.
  The raw keyword can still be stored here for inspection/debugging.

  heading_id:
    Usually the level 0 heading.

  keyword:
    Keyword name without #+ and without trailing colon, for example TITLE,
    STARTUP, TODO, OPTIONS.

  value:
    Raw keyword value after the colon.
*/
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

--------------------------------------------------
-- PROPERTIES
--------------------------------------------------
/*
  Each row represents a property associated with a heading.

  File-level properties:

    File-level properties defined with #+PROPERTY are stored as properties of
    the level 0 heading.

    A file-level #+CATEGORY can also be stored as a property of the level 0
    heading so that categories can be queried uniformly from level 1 onward.

  Heading properties:

    Properties from :PROPERTIES: drawers are attached to the corresponding
    regular heading.

  inherited:
    0 = directly defined on this heading
    1 = inherited/effective value
*/
CREATE TABLE IF NOT EXISTS properties (
    id              INTEGER PRIMARY KEY,
    heading_id      INTEGER NOT NULL,

    key             TEXT NOT NULL,
    value           TEXT,

    source          TEXT NOT NULL CHECK (source IN ('property_keyword', 'property_drawer', 'category_keyword')),
    inherited       INTEGER NOT NULL DEFAULT 0 CHECK (inherited IN (0, 1)),

    line_number     INTEGER,

    FOREIGN KEY (heading_id)
        REFERENCES headings(id)
        ON DELETE CASCADE,

    UNIQUE (heading_id, key, source, inherited)
);

--------------------------------------------------
-- TAGS
--------------------------------------------------
/*
  Each row represents a tag associated with a heading.

  FILETAGS are stored as tags on the level 0 heading.

  inherited:
    0 = directly defined on this heading
    1 = inherited/effective tag
*/
CREATE TABLE IF NOT EXISTS tags (
    heading_id      INTEGER NOT NULL,
    tag             TEXT NOT NULL,
    inherited       INTEGER NOT NULL DEFAULT 0 CHECK (inherited IN (0, 1)),

    FOREIGN KEY (heading_id)
        REFERENCES headings(id)
        ON DELETE CASCADE,

    PRIMARY KEY (heading_id, tag, inherited)
);

--------------------------------------------------
-- TIMESTAMPS
--------------------------------------------------
/*
  Each row represents a timestamp associated with a heading.

  Planning timestamps such as SCHEDULED, DEADLINE, and CLOSED are also stored
  directly on headings for fast common queries. This table is for richer
  timestamp representation and for timestamps found in body text.

  role:
    Optional semantic role, for example scheduled, deadline, closed, body.

  type:
    active or inactive.

  range_type:
    none, date_range, time_range, datetime_range, or another normalized value
    decided by the parser model.

  start_ts / end_ts:
    Unix timestamps in seconds. NULL if the timestamp cannot be normalized.

  raw_value:
    Original Org timestamp string.
*/
CREATE TABLE IF NOT EXISTS timestamps (
    id              INTEGER PRIMARY KEY,
    heading_id      INTEGER NOT NULL,

    role            TEXT CHECK (
                        role IN ('scheduled', 'deadline', 'closed', 'body')
                        OR role IS NULL
                    ),

    start_ts        INTEGER,
    end_ts          INTEGER,

    type            TEXT CHECK (type IN ('active', 'inactive') OR type IS NULL),
    range_type      TEXT,

    raw_value       TEXT NOT NULL,

    byte_start      INTEGER,
    byte_end        INTEGER,
    line_number     INTEGER,

    FOREIGN KEY (heading_id)
        REFERENCES headings(id)
        ON DELETE CASCADE
);

--------------------------------------------------
-- TIMESTAMP REPEATERS
--------------------------------------------------
/*
  Each row represents repeater/warning information for one timestamp.

  type:
    repeat, restart, cumulate, catch_up, warning, or another parser-normalized
    value.

  unit:
    hour, day, week, month, year.
*/
CREATE TABLE IF NOT EXISTS timestamp_repeaters (
    id                  INTEGER PRIMARY KEY,
    timestamp_id         INTEGER NOT NULL,

    type                TEXT,
    value               INTEGER,
    unit                TEXT CHECK (
                            unit IN ('hour', 'day', 'week', 'month', 'year')
                            OR unit IS NULL
                        ),

    deadline_value      INTEGER,
    deadline_unit       TEXT CHECK (
                            deadline_unit IN ('hour', 'day', 'week', 'month', 'year')
                            OR deadline_unit IS NULL
                        ),

    FOREIGN KEY (timestamp_id)
        REFERENCES timestamps(id)
        ON DELETE CASCADE
);

--------------------------------------------------
-- LINKS
--------------------------------------------------
/*
  Each row represents one Org link.

  heading_id:
    The heading that contains the link. Links outside regular headings are
    attached to the level 0 heading.

  byte_start / byte_end:
    UTF-8 byte offsets into the original file. byte_start can be used by Emacs
    or other editor integrations to jump to the link.

  link_type:
    Link type/protocol, for example file, https, http, id, custom-id,
    attachment. May be NULL for links where the parser has not classified the
    type yet.

  target:
    Link target/path without description.
    Examples:
      [[https://www.example.com][Example]] -> https://www.example.com
      [[file:example.org]]                 -> example.org
      [[file:./example.org]]               -> ./example.org

  target_absolute:
    Absolute path for file links when resolvable.

  raw_link:
    Original link target with protocol syntax where applicable.
    Examples:
      [[https://www.example.com][Example]] -> https://www.example.com
      [[file:example.org][Example]]      -> file:example.org
      [[example.org]]                    -> example.org

  description:
    Optional link description.
    Example:
      [[https://www.example.com][Example]] -> Example

  format:
    plain, bracket, or angle.

  search_option:
    Search option after :: in file links.
    Example:
      [[file:~/example.org::255]] -> 255

  relation:
    Optional project-specific relation extracted from the description.
    Example:
      [[https://www.example.com][Example (->Owner)]] -> Owner

  resolved_file_id / resolved_heading_id:
    Resolution targets if the link can be resolved to known indexed data.

  broken:
    1 if the link target is known to be broken, otherwise 0.
*/
CREATE TABLE IF NOT EXISTS links (
    id                  INTEGER PRIMARY KEY,

    file_id             INTEGER NOT NULL,
    heading_id          INTEGER NOT NULL,

    byte_start          INTEGER NOT NULL,
    byte_end            INTEGER NOT NULL,
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

--------------------------------------------------
-- HEADING BODIES
--------------------------------------------------
/*
  Body text belonging to a heading.

  This table exists so heading body text can be optional and so the headings
  table stays compact.

  For the level 0 heading, body_text may contain file-level text before the
  first regular heading, or it may be empty depending on the parser decision.

  For regular headings, body_text should normally exclude child subtrees unless
  the parser model explicitly decides otherwise.

  heading_fts can index this body text.
*/
CREATE TABLE IF NOT EXISTS heading_bodies (
    heading_id          INTEGER PRIMARY KEY,

    body_text           TEXT NOT NULL,
    body_byte_start     INTEGER,
    body_byte_end       INTEGER,

    FOREIGN KEY (heading_id)
        REFERENCES headings(id)
        ON DELETE CASCADE
);

--------------------------------------------------
-- OUTLINE PATH
--------------------------------------------------
/*
  Derived/cache table for fast hierarchical queries.

  This table materializes structural relationships that can be recomputed from
  headings.

  heading_id:
    One outline_path row per heading.

  depth:
    0 for the level 0 file heading.
    1 for top-level Org headings.
    2 for child headings.
    etc.

  materialized_path:
    Zero-padded numeric hierarchical path.

    Examples:
      0000
      0000.0001
      0000.0001.0002
      0000.0001.0002.0001

  breadcrumbs_json:
    JSON array containing breadcrumb titles from the level 0 heading to this
    heading.

    Example:
      ["todo.org", "Project", "Phase 1", "Analysis"]

  This table should be rebuilt when headings for a file are rebuilt.
*/
CREATE TABLE IF NOT EXISTS outline_path (
    heading_id          INTEGER PRIMARY KEY,
    file_id             INTEGER NOT NULL,
    parent_id           INTEGER,

    depth               INTEGER NOT NULL CHECK (depth >= 0),
    materialized_path   TEXT NOT NULL,
    breadcrumbs_json    TEXT NOT NULL,

    FOREIGN KEY (file_id)
        REFERENCES files(id)
        ON DELETE CASCADE,

    FOREIGN KEY (heading_id)
        REFERENCES headings(id)
        ON DELETE CASCADE,

    FOREIGN KEY (parent_id)
        REFERENCES headings(id)
        ON DELETE SET NULL
);

--------------------------------------------------
-- FULL TEXT SEARCH
--------------------------------------------------
/*
  FTS5 table for heading title and body search.

  This is a manually maintained FTS table.

  The indexer should insert/update/delete rows together with headings and
  heading_bodies during rebuild.

  rowid:
    Must match headings.id.

  title:
    Normalized heading title.

  body:
    Heading body text, if available.

  Note:
    SQLite virtual tables cannot enforce normal foreign keys here. The indexer
    is responsible for keeping heading_fts in sync with headings.
*/
CREATE VIRTUAL TABLE IF NOT EXISTS heading_fts
USING fts5(
    title,
    body,
    tokenize = 'unicode61'
);

--------------------------------------------------
-- INDEXES: FILES
--------------------------------------------------
CREATE INDEX IF NOT EXISTS idx_files_path
    ON files(path);

CREATE INDEX IF NOT EXISTS idx_files_mtime_size
    ON files(mtime_ns, size);

CREATE INDEX IF NOT EXISTS idx_files_hash
    ON files(content_hash);

--------------------------------------------------
-- INDEXES: HEADINGS
--------------------------------------------------
CREATE INDEX IF NOT EXISTS idx_headings_file_id
    ON headings(file_id);

CREATE INDEX IF NOT EXISTS idx_headings_parent_id
    ON headings(parent_id);

CREATE INDEX IF NOT EXISTS idx_headings_file_position
    ON headings(file_id, byte_start);

CREATE INDEX IF NOT EXISTS idx_headings_level
    ON headings(level);

CREATE INDEX IF NOT EXISTS idx_headings_todo
    ON headings(todo_keyword);

CREATE INDEX IF NOT EXISTS idx_headings_todo_type
    ON headings(todo_type);

CREATE INDEX IF NOT EXISTS idx_headings_priority
    ON headings(priority);

CREATE INDEX IF NOT EXISTS idx_headings_scheduled
    ON headings(scheduled_ts);

CREATE INDEX IF NOT EXISTS idx_headings_deadline
    ON headings(deadline_ts);

CREATE INDEX IF NOT EXISTS idx_headings_closed
    ON headings(closed_ts);

--------------------------------------------------
-- INDEXES: TODO KEYWORDS
--------------------------------------------------
CREATE INDEX IF NOT EXISTS idx_todo_keywords_file_state
    ON todo_keywords(file_id, state_type);

--------------------------------------------------
-- INDEXES: KEYWORDS
--------------------------------------------------
CREATE INDEX IF NOT EXISTS idx_keywords_heading
    ON keywords(heading_id);

CREATE INDEX IF NOT EXISTS idx_keywords_keyword
    ON keywords(keyword);

CREATE INDEX IF NOT EXISTS idx_keywords_keyword_value
    ON keywords(keyword, value);

--------------------------------------------------
-- INDEXES: PROPERTIES
--------------------------------------------------
CREATE INDEX IF NOT EXISTS idx_properties_heading
    ON properties(heading_id);

CREATE INDEX IF NOT EXISTS idx_properties_key
    ON properties(key);

CREATE INDEX IF NOT EXISTS idx_properties_key_value
    ON properties(key, value);

CREATE INDEX IF NOT EXISTS idx_properties_id
    ON properties(value)
    WHERE key = 'ID';

CREATE INDEX IF NOT EXISTS idx_properties_custom_id
    ON properties(value)
    WHERE key = 'CUSTOM_ID';

--------------------------------------------------
-- INDEXES: TAGS
--------------------------------------------------
CREATE INDEX IF NOT EXISTS idx_tags_tag
    ON tags(tag);

CREATE INDEX IF NOT EXISTS idx_tags_heading
    ON tags(heading_id);

CREATE INDEX IF NOT EXISTS idx_tags_inherited
    ON tags(inherited);

--------------------------------------------------
-- INDEXES: TIMESTAMPS
--------------------------------------------------
CREATE INDEX IF NOT EXISTS idx_timestamps_heading
    ON timestamps(heading_id);

CREATE INDEX IF NOT EXISTS idx_timestamps_role
    ON timestamps(role);

CREATE INDEX IF NOT EXISTS idx_timestamps_start
    ON timestamps(start_ts);

CREATE INDEX IF NOT EXISTS idx_timestamps_type
    ON timestamps(type);

CREATE INDEX IF NOT EXISTS idx_timestamp_repeaters_timestamp
    ON timestamp_repeaters(timestamp_id);

--------------------------------------------------
-- INDEXES: LINKS
--------------------------------------------------
CREATE INDEX IF NOT EXISTS idx_links_file
    ON links(file_id);

CREATE INDEX IF NOT EXISTS idx_links_heading
    ON links(heading_id);

CREATE INDEX IF NOT EXISTS idx_links_file_position
    ON links(file_id, byte_start);

CREATE INDEX IF NOT EXISTS idx_links_type
    ON links(link_type);

CREATE INDEX IF NOT EXISTS idx_links_target
    ON links(target);

CREATE INDEX IF NOT EXISTS idx_links_resolved_file
    ON links(resolved_file_id);

CREATE INDEX IF NOT EXISTS idx_links_resolved_heading
    ON links(resolved_heading_id);

CREATE INDEX IF NOT EXISTS idx_links_broken
    ON links(broken);

--------------------------------------------------
-- INDEXES: HEADING BODIES
--------------------------------------------------
CREATE INDEX IF NOT EXISTS idx_heading_bodies_body_range
    ON heading_bodies(body_byte_start, body_byte_end);

--------------------------------------------------
-- INDEXES: OUTLINE PATH
--------------------------------------------------
CREATE INDEX IF NOT EXISTS idx_outline_file
    ON outline_path(file_id);

CREATE INDEX IF NOT EXISTS idx_outline_parent
    ON outline_path(parent_id);

CREATE INDEX IF NOT EXISTS idx_outline_materialized_path
    ON outline_path(materialized_path);

CREATE INDEX IF NOT EXISTS idx_outline_file_materialized_path
    ON outline_path(file_id, materialized_path);

COMMIT;
