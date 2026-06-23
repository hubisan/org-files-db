/*
  Authoritative SQLite schema for org-files-db.

  Notes:

  - Every Org file gets exactly one synthetic level 0 heading.
  - File-level keywords, properties, file tags, and file-level links attach to
    the level 0 heading.
  - Regular Org headings start at level 1.
  - byte_start and byte_end are UTF-8 byte offsets into the original file.
    byte_start is the main position used by editor integrations such as Emacs
    to jump to a heading or link location.
  - outline_path and heading_fts are derived/index tables. They can be rebuilt
    from the core tables when a file is reparsed.
  - Planning timestamps such as SCHEDULED, DEADLINE, and CLOSED are stored
    directly on headings for fast common queries and also mirrored into the
    richer timestamps table.
  - heading_fts is created conditionally by replacing the marker block below.
  - The schema is optimized for querying and full rebuilds per changed file.
*/

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

  indexed_at:
    Unix timestamp in seconds for when this file was last indexed.
*/
CREATE TABLE IF NOT EXISTS files (
    id              INTEGER PRIMARY KEY,
    path            TEXT NOT NULL UNIQUE,
    mtime_ns        INTEGER NOT NULL,
    size            INTEGER NOT NULL,
    content_hash    TEXT,
    indexed_at      INTEGER
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

    For the level 0 heading, title/title_raw represent the file display title,
    not the full file path. Prefer the file-level #+TITLE value when present.
    If no #+TITLE exists, use the file name without its directory path and,
    for normal Org files, without the .org extension. The full file path is
    available through the files table.

  level:
    0 = synthetic file-level heading
    1 = top-level Org heading
    2 = child heading
    etc.

  parent_id:
    Parent heading. NULL for the level 0 heading. Top-level Org headings should
    normally have the level 0 heading as parent.

  line_number:
    Source line number for the heading when available. For the level 0 heading,
    this normally points at the beginning of the file or the file-level title
    keyword if the implementation chooses to associate it with #+TITLE.

  byte_start / byte_end:
    UTF-8 byte offsets into the original file.
    For regular headings, byte_start points to the beginning of the heading.
    For the level 0 heading, byte_start should normally be 0 and byte_end should
    normally be the file length.

  title:
    Normalized display title.

    For regular headings this excludes TODO keyword, priority, tags, statistic
    cookies, and markup details where normalization is supported. Links should
    be converted to text, using the description if present, otherwise the link
    itself.

    For the level 0 heading this is the normalized file display title. Prefer
    #+TITLE when present. Otherwise use the file name without path and normally
    without the .org extension.

  title_raw:
    Raw heading title text, excluding the TODO keyword but preserving more of
    the original Org syntax.

    For the level 0 heading this should match the raw #+TITLE value when a
    #+TITLE keyword is present. Otherwise it should use the same file-name
    fallback as title.

  todo_keyword:
    The TODO state found on the heading, for example TODO, NEXT, PLAN, DONE.

  todo_type:
    open or closed, resolved from the active TODO keyword configuration.

  priority:
    Org priority marker without brackets, for example A, B, or C.

  scheduled_raw / deadline_raw / closed_raw:
    Original Org planning timestamp strings when present.

  scheduled_ts / deadline_ts / closed_ts:
    Nullable Unix timestamp seconds in UTC.
    Populated only when the corresponding planning timestamp is a simple date or
    date-time timestamp. NULL when absent or when the Org timestamp contains
    unsupported syntax such as ranges, repeaters, warning delays, diary
    expressions, or other rich timestamp forms.

  archivedp / footnote_section_p:
    Stored as 0/1 integers.

  all_tags_json:
    JSON array of all tags visible on this heading, including inherited tags.
    This is a query/output convenience cache. The normalized tag rows are stored
    in tags.
*/
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

  keyword:
    TODO keyword text, for example TODO or DONE.

  state_type:
    open or closed.

  shortcut:
    Optional fast selection key from Org TODO syntax. One character when set.

  sequence_no:
    Order of the keyword within the active TODO keyword configuration.
*/
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

--------------------------------------------------
-- TIMESTAMPS
--------------------------------------------------
/*
  Each row represents one parsed timestamp occurrence associated with a
  heading.

  Planning timestamps are mirrored here with role scheduled, deadline, or
  closed. Generic timestamps in heading titles or section/body content use role
  body.

  type:
    active, inactive, or diary.

  range_type:
    none, date_range, time_range, datetime_range, or unknown.

  start_ts / end_ts:
    Nullable Unix timestamp seconds in UTC. Diary expressions are preserved as
    raw values and leave these columns NULL.
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
    type            TEXT CHECK (
                        type IN ('active', 'inactive', 'diary')
                        OR type IS NULL
                    ),
    range_type      TEXT CHECK (
                        range_type IN ('none', 'date_range', 'time_range', 'datetime_range', 'unknown')
                        OR range_type IS NULL
                    ),
    raw_value       TEXT NOT NULL,
    byte_start      INTEGER NOT NULL,
    byte_end        INTEGER NOT NULL CHECK (byte_end >= byte_start),
    line_number     INTEGER,
    FOREIGN KEY (heading_id)
        REFERENCES headings(id)
        ON DELETE CASCADE
);

/*
  Timestamp repeater and warning rows emitted from parsed Org timestamps.

  The legacy table name timestamp_repeaters is kept for compatibility with the
  earlier schema draft.

  Each row represents the Org/Emacs repeater and warning properties for one
  timestamp:

  - :repeater-type
  - :repeater-value
  - :repeater-unit
  - :repeater-deadline-value
  - :repeater-deadline-unit
  - :warning-type
  - :warning-value
  - :warning-unit
*/
CREATE TABLE IF NOT EXISTS timestamp_repeaters (
    id                          INTEGER PRIMARY KEY,
    timestamp_id                INTEGER NOT NULL UNIQUE,
    repeater_type               TEXT CHECK (
                                    repeater_type IN ('cumulate', 'catch_up', 'restart')
                                    OR repeater_type IS NULL
                                ),
    repeater_value              INTEGER CHECK (
                                    repeater_value IS NULL
                                    OR repeater_value > 0
                                ),
    repeater_unit               TEXT CHECK (
                                    repeater_unit IN ('hour', 'day', 'week', 'month', 'year')
                                    OR repeater_unit IS NULL
                                ),
    repeater_deadline_value     INTEGER CHECK (
                                    repeater_deadline_value IS NULL
                                    OR repeater_deadline_value > 0
                                ),
    repeater_deadline_unit      TEXT CHECK (
                                    repeater_deadline_unit IN ('hour', 'day', 'week', 'month', 'year')
                                    OR repeater_deadline_unit IS NULL
                                ),
    warning_type                TEXT CHECK (
                                    warning_type IN ('all', 'first')
                                    OR warning_type IS NULL
                                ),
    warning_value               INTEGER CHECK (
                                    warning_value IS NULL
                                    OR warning_value > 0
                                ),
    warning_unit                TEXT CHECK (
                                    warning_unit IN ('hour', 'day', 'week', 'month', 'year')
                                    OR warning_unit IS NULL
                                ),
    FOREIGN KEY (timestamp_id)
        REFERENCES timestamps(id)
        ON DELETE CASCADE,
    CHECK (
        (repeater_type IS NULL AND repeater_value IS NULL AND repeater_unit IS NULL)
        OR
        (repeater_type IS NOT NULL AND repeater_value IS NOT NULL AND repeater_unit IS NOT NULL)
    ),
    CHECK (
        (repeater_deadline_value IS NULL AND repeater_deadline_unit IS NULL)
        OR
        (repeater_deadline_value IS NOT NULL AND repeater_deadline_unit IS NOT NULL)
    ),
    CHECK (
        repeater_deadline_value IS NULL
        OR
        repeater_type IS NOT NULL
    ),
    CHECK (
        (warning_type IS NULL AND warning_value IS NULL AND warning_unit IS NULL)
        OR
        (warning_type IS NOT NULL AND warning_value IS NOT NULL AND warning_unit IS NOT NULL)
    ),
    CHECK (
        repeater_type IS NOT NULL
        OR warning_type IS NOT NULL
    )
);

CREATE INDEX IF NOT EXISTS idx_timestamps_heading_id
    ON timestamps(heading_id);

CREATE INDEX IF NOT EXISTS idx_timestamps_role_start
    ON timestamps(role, start_ts);

CREATE INDEX IF NOT EXISTS idx_timestamps_start
    ON timestamps(start_ts);

CREATE INDEX IF NOT EXISTS idx_timestamp_repeaters_timestamp_id
    ON timestamp_repeaters(timestamp_id);

--------------------------------------------------
-- KEYWORDS
--------------------------------------------------
/*
  Each row represents an Org keyword line.

  File-level keywords such as #+TITLE, #+AUTHOR, #+STARTUP, #+OPTIONS and
  #+EXPORT_FILE_NAME are attached to the synthetic level 0 heading, even when
  the keyword line appears later in the document after regular headings.

  Keywords that are semantically relevant to parser behavior, especially
  #+TODO, may also be represented in specialized tables such as todo_keywords.
  The raw keyword can still be stored here for inspection/debugging.

  heading_id:
    Usually the level 0 heading for file-level keywords.

  keyword:
    Keyword name without #+ and without trailing colon, for example TITLE,
    STARTUP, TODO, OPTIONS.

  value:
    Raw keyword value after the colon.

  line_number:
    Source line number for the keyword when available.
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

  key:
    Property key/name.

  value:
    Property value. NULL if present without a value.

  source:
    property_keyword = file-level #+PROPERTY
    property_drawer = heading :PROPERTIES: drawer
    category_keyword = file-level #+CATEGORY

  append:
    0 = normal definition
    1 = key used the trailing + append operator

  line_number:
    Source line number for the property when available.
*/
CREATE TABLE IF NOT EXISTS properties (
    id              INTEGER PRIMARY KEY,
    heading_id      INTEGER NOT NULL,
    key             TEXT NOT NULL,
    value           TEXT,
    source          TEXT NOT NULL CHECK (
                        source IN ('property_keyword', 'property_drawer', 'category_keyword')
                    ),
    append          INTEGER NOT NULL DEFAULT 0 CHECK (append IN (0, 1)),
    line_number     INTEGER,
    FOREIGN KEY (heading_id)
        REFERENCES headings(id)
        ON DELETE CASCADE
);

--------------------------------------------------
-- TAGS
--------------------------------------------------
/*
  Each row represents a tag associated with a heading.

  Tags are stored as direct facts only.

  Regular heading tags are stored on their actual heading.

  FILETAGS are stored as tags on the synthetic level 0 heading. This is
  sufficient to distinguish them from regular heading tags without adding a
  separate source column.

  tag:
    Tag name without surrounding colons.

  inherited:
    Deprecated compatibility column.
    Current writes store only direct tag facts, so this remains 0.
    Effective/inherited tags are represented in headings.all_tags_json.
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

  line_number:
    Source line number for the link when available.

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

  resolved:
    1 if the link target was resolved to indexed data, otherwise 0.

  broken:
    1 if the link target is known to be broken, otherwise 0.

  diagnostic:
    Optional diagnostic explaining resolution or parse issues for this link.
*/
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

  body_text:
    Plain or normalized body text according to the parser/indexer model.

  body_byte_start / body_byte_end:
    UTF-8 byte offsets for the body range when available.
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

  file_id:
    File that owns the heading.

  parent_id:
    Parent heading. NULL for the level 0 heading.

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
      ["todo", "Project", "Phase 1", "Analysis"]

  This table should be rebuilt when headings for a file are rebuilt.
*/
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

  This schema file keeps FTS optional. The marker below is replaced with the
  CREATE VIRTUAL TABLE statement only when FTS5 is enabled.
*/
-- heading_fts placeholder
/*__HEADING_FTS__*/;

--------------------------------------------------
-- INDEXES: FILES
--------------------------------------------------
CREATE INDEX IF NOT EXISTS idx_files_mtime_size
    ON files(mtime_ns, size);

CREATE INDEX IF NOT EXISTS idx_files_hash
    ON files(content_hash);

--------------------------------------------------
-- INDEXES: HEADINGS
--------------------------------------------------
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

--------------------------------------------------
-- INDEXES: PROPERTIES
--------------------------------------------------
CREATE INDEX IF NOT EXISTS idx_properties_heading_key
    ON properties(heading_id, key);

CREATE INDEX IF NOT EXISTS idx_properties_key_value
    ON properties(key, value);

CREATE INDEX IF NOT EXISTS idx_properties_id_lookup
    ON properties(value)
    WHERE key = 'ID';

CREATE INDEX IF NOT EXISTS idx_properties_custom_id_lookup
    ON properties(value)
    WHERE key = 'CUSTOM_ID';

--------------------------------------------------
-- INDEXES: TAGS
--------------------------------------------------
CREATE INDEX IF NOT EXISTS idx_tags_tag
    ON tags(tag);

CREATE INDEX IF NOT EXISTS idx_tags_heading
    ON tags(heading_id);

--------------------------------------------------
-- INDEXES: LINKS
--------------------------------------------------
CREATE INDEX IF NOT EXISTS idx_links_heading
    ON links(heading_id);

CREATE INDEX IF NOT EXISTS idx_links_target
    ON links(target);

CREATE INDEX IF NOT EXISTS idx_links_resolved_file
    ON links(resolved_file_id);

CREATE INDEX IF NOT EXISTS idx_links_resolved_heading
    ON links(resolved_heading_id);

--------------------------------------------------
-- INDEXES: OUTLINE PATH
--------------------------------------------------
CREATE INDEX IF NOT EXISTS idx_outline_file_materialized_path
    ON outline_path(file_id, materialized_path);

CREATE INDEX IF NOT EXISTS idx_outline_parent
    ON outline_path(parent_id);
