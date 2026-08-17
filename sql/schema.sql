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
    Reversible display representation of the absolute normalized path. Must be unique.

  identity:
    Version-tagged, byte-preserving canonical Unix path identity. Production
    index writes populate it after discovery; legacy TEXT-only rows retain NULL
    until successful rediscovery or reconciliation. Non-NULL values are unique
    through the named partial files_identity_unique index.

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
    identity        BLOB,
    mtime_ns        INTEGER NOT NULL,
    size            INTEGER NOT NULL,
    content_hash    TEXT,
    indexed_at      INTEGER
);

CREATE UNIQUE INDEX IF NOT EXISTS files_identity_unique
ON files(identity)
WHERE identity IS NOT NULL;

--------------------------------------------------
-- DATABASE METADATA
--------------------------------------------------
/*
  Small persisted capability/configuration facts about the indexed database
  instance.

  key:
    Stable metadata key.

  value:
    Canonical string representation written by the indexer/database layer.

  Current keys:

  body_text_available:
    "1" when the database instance was last rebuilt with canonical heading body
    text persistence enabled.
    "0" when the database instance was last rebuilt with body-text persistence
    disabled.

  fts_available:
    "1" when the database instance was last rebuilt successfully with FTS
    enabled and a trusted ~=heading_fts= index was recreated transactionally.
    "0" when the database instance was last rebuilt successfully with FTS
    disabled, so any on-disk ~=heading_fts= table is intentionally untrusted.

  fts_body_indexed:
    "1" when the trusted ~=heading_fts= index contains canonical body text.
    "0" when the trusted index is title-only, or when no trusted FTS index is
    currently available.

  fts_schema_version:
    Search-specific trusted FTS contract version as a canonical string.
    "1" is the current contract for the contentless ~=heading_fts= layout used
    by the ~=orgfdb search --json= command.
    "0" indicates that no trusted search index is currently available.
*/
CREATE TABLE IF NOT EXISTS db_metadata (
    key             TEXT PRIMARY KEY,
    value           TEXT NOT NULL
);

--------------------------------------------------
-- INDEX STATE AND GENERATION JOURNAL
--------------------------------------------------
/*
  Transactionally published logical index state for external caches.

  index_state contains exactly one row. generation advances once per committed
  query-visible indexing batch. index_generations and index_generation_files
  retain the affected-file journal required for cached-view delta refresh.
*/
CREATE TABLE IF NOT EXISTS index_state (
    singleton       INTEGER PRIMARY KEY CHECK (singleton = 1),
    database_id     TEXT NOT NULL CHECK (length(database_id) > 0),
    generation      INTEGER NOT NULL CHECK (generation >= 0),
    last_changed_at TEXT NOT NULL CHECK (length(last_changed_at) > 0)
);

CREATE TABLE IF NOT EXISTS index_generations (
    generation        INTEGER PRIMARY KEY CHECK (generation > 0),
    committed_at      TEXT NOT NULL CHECK (length(committed_at) > 0),
    full_invalidation INTEGER NOT NULL CHECK (full_invalidation IN (0, 1))
);

CREATE TABLE IF NOT EXISTS index_generation_files (
    generation INTEGER NOT NULL,
    path       TEXT NOT NULL CHECK (length(path) > 0),
    action     TEXT NOT NULL CHECK (action IN ('upsert', 'delete')),
    PRIMARY KEY (generation, path),
    FOREIGN KEY (generation)
        REFERENCES index_generations(generation)
        ON DELETE CASCADE
);

CREATE INDEX IF NOT EXISTS index_generation_files_path_idx
ON index_generation_files(path, generation);

--------------------------------------------------
-- HEADINGS
--------------------------------------------------
/*
  Each row represents either a real Org heading or the synthetic file-level
  heading.

  Level 0 heading:

    A level 0 heading is created by indexing for every file. It is a
    DB/internal synthetic file-root row, not parser-level Org syntax. This row
    represents the file-level scope. It allows file-wide elements such as
    keywords, file tags, properties, file-level links, and file-level full-text
    content to be stored in the same hierarchy as regular headings.

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
    For the level 0 heading, byte_start uses the DB sentinel value -1 because
    it does not correspond to a parser heading position in the file. byte_end
    normally spans the file length.

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
    #+TITLE keyword is present. Otherwise it should be NULL so fallback file
    title generation remains distinguishable from source-provided title text.

  todo_keyword:
    The TODO state found on the heading, for example TODO, NEXT, PLAN, DONE.

  todo_type:
    open or closed, resolved from the active TODO keyword configuration.

  priority:
    Org priority marker without brackets, preserved as its complete source value,
    for example A, B, C, 1, or 10.

  scheduled_raw / deadline_raw / closed_raw:
    Original Org planning timestamp strings when present.

  scheduled_ts / deadline_ts / closed_ts:
    Nullable timezone-naive Unix timestamp seconds.
    Populated only when the corresponding planning timestamp is a simple date or
    date-time timestamp. NULL when absent or when the Org timestamp contains
    unsupported syntax such as ranges, repeaters, warning delays, diary
    expressions, or other rich timestamp forms.

  scheduled_has_time / deadline_has_time / closed_has_time:
    Nullable 0/1 flags that record whether the original Org timestamp contained
    an explicit hour and minute. Date-only timestamps use 0. Explicit midnight
    uses 1.

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
    byte_end            INTEGER NOT NULL CHECK (byte_end >= byte_start),
    title               TEXT NOT NULL,
    title_raw           TEXT,
    todo_keyword        TEXT,
    todo_type           TEXT CHECK (todo_type IN ('open', 'closed') OR todo_type IS NULL),
    priority            TEXT,
    scheduled_raw       TEXT,
    scheduled_ts        INTEGER,
    scheduled_has_time  INTEGER CHECK (scheduled_has_time IN (0, 1) OR scheduled_has_time IS NULL),
    deadline_raw        TEXT,
    deadline_ts         INTEGER,
    deadline_has_time   INTEGER CHECK (deadline_has_time IN (0, 1) OR deadline_has_time IS NULL),
    closed_raw          TEXT,
    closed_ts           INTEGER,
    closed_has_time     INTEGER CHECK (closed_has_time IN (0, 1) OR closed_has_time IS NULL),
    archivedp           INTEGER NOT NULL DEFAULT 0 CHECK (archivedp IN (0, 1)),
    footnote_section_p  INTEGER NOT NULL DEFAULT 0 CHECK (footnote_section_p IN (0, 1)),
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
  rows per file, but level 0 must be unique. The SQL schema also enforces
  level = 0 => parent_id IS NULL. The byte_start = -1 sentinel is an
  indexer-level invariant documented here and covered by tests, not a SQL CHECK
  constraint.
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

  source_kind:
    config_default or org_keyword.

  source_keyword:
    The source keyword line that produced the row, for example TODO, SEQ_TODO,
    or TYP_TODO. NULL for config defaults.

  source_line_number:
    1-based line number of the source keyword line. NULL for config defaults.
*/
CREATE TABLE IF NOT EXISTS todo_keywords (
    file_id         INTEGER NOT NULL,
    keyword         TEXT NOT NULL,
    state_type      TEXT NOT NULL CHECK (state_type IN ('open', 'closed')),
    shortcut        TEXT CHECK (shortcut IS NULL OR length(shortcut) = 1),
    sequence_no     INTEGER NOT NULL,
    source_kind     TEXT NOT NULL CHECK (
                        source_kind IN ('config_default', 'org_keyword')
                    ),
    source_keyword  TEXT CHECK (
                        source_keyword IN ('TODO', 'SEQ_TODO', 'TYP_TODO')
                        OR source_keyword IS NULL
                    ),
    source_line_number INTEGER CHECK (
                        source_line_number IS NULL
                        OR source_line_number > 0
                    ),
    CHECK (
        (source_kind = 'config_default' AND source_keyword IS NULL AND source_line_number IS NULL)
        OR
        (source_kind = 'org_keyword' AND source_keyword IS NOT NULL AND source_line_number IS NOT NULL)
    ),
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

  has_time:
    Nullable 0/1 flag that records whether the original Org timestamp contained
    an explicit hour and minute. Date-only timestamps use 0. Explicit midnight
    uses 1. Diary expressions may leave this column NULL.

  start_ts / end_ts:
    Nullable timezone-naive Unix timestamp seconds. Diary expressions are
    preserved as raw values and leave these columns NULL.
*/
CREATE TABLE IF NOT EXISTS timestamps (
    id              INTEGER PRIMARY KEY,
    heading_id      INTEGER NOT NULL,
    role            TEXT CHECK (
                        role IN ('scheduled', 'deadline', 'closed', 'body')
                        OR role IS NULL
                    ),
    has_time        INTEGER CHECK (has_time IN (0, 1) OR has_time IS NULL),
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

/*
  Mandatory, rebuildable projection of resolved property values.

  Canonical source rows remain in properties, including duplicate base
  definitions and append rows.

  One row exists for each property key visible at a heading.

  local_value:
    The value resolved from definitions on this heading only, after applying
    the local duplicate-base and append rules. NULL means that the heading has
    no local definition for the key, even though the key may still be inherited
    and therefore have a non-NULL effective_value.. An empty string is an explicit
    empty local value.

  effective_value:
    The final value visible at the heading after inheriting the resolved value
    from the file root or parent headings and then applying this heading's
    local replacement and append rules.

  Property inheritance never crosses file boundaries.
*/
CREATE TABLE IF NOT EXISTS effective_properties (
    heading_id                   INTEGER NOT NULL,
    file_id                      INTEGER NOT NULL,
    key                          TEXT NOT NULL,
    local_value                  TEXT,
    effective_value              TEXT NOT NULL,
    PRIMARY KEY (heading_id, key),
    FOREIGN KEY (heading_id) REFERENCES headings(id) ON DELETE CASCADE,
    FOREIGN KEY (file_id) REFERENCES files(id) ON DELETE CASCADE
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
*/
CREATE TABLE IF NOT EXISTS tags (
    heading_id      INTEGER NOT NULL,
    tag             TEXT NOT NULL,
    FOREIGN KEY (heading_id)
        REFERENCES headings(id)
        ON DELETE CASCADE,
    PRIMARY KEY (heading_id, tag)
);

/*
  Mandatory, rebuildable projection of the tags visible at each heading.

  Canonical direct tag facts remain in tags. FILETAGS are direct tags of the
  synthetic level-0 heading and therefore participate in normal inheritance.

  position preserves the public root-to-leaf, first-occurrence order after
  inherited and local duplicate tags have been removed. Tag inheritance never
  crosses file boundaries.
*/
CREATE TABLE IF NOT EXISTS effective_tags (
    heading_id      INTEGER NOT NULL,
    file_id         INTEGER NOT NULL,
    tag             TEXT NOT NULL,
    position        INTEGER NOT NULL CHECK (position >= 0),
    PRIMARY KEY (heading_id, tag),
    UNIQUE (heading_id, position),
    FOREIGN KEY (heading_id)
        REFERENCES headings(id)
        ON DELETE CASCADE,
    FOREIGN KEY (file_id)
        REFERENCES files(id)
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

  line:
    1-based source line number for the start of the link.

  link_type:
    Link type/protocol, for example file, https, http, id, custom-id,
    attachment. May be NULL for links where the parser has not classified the
    type yet.

  source_context:
    Structural source region the link came from.
    Supported Phase 3 values:
      normal
      heading
      property_drawer
      drawer
      verse_block
      quote_block
      center_block
      justify_block

  raw:
    Original raw link text exactly as it appeared in the source.
    Examples:
      [[https://www.example.com][Example]] -> [[https://www.example.com][Example]]
      <file:example.org::255>              -> <file:example.org::255>
      https://example.org                  -> https://example.org

  raw_target:
    Original target portion exactly as it appeared inside the link syntax.
    Examples:
      [[https://www.example.com][Example]] -> https://www.example.com
      [[file:example.org][Example]]        -> file:example.org
      [[example.org]]                      -> example.org

  raw_description:
    Optional raw link description preserved exactly.
    Example:
      [[https://www.example.com][Example]] -> Example

  format:
    plain, bracket, or angle.

  search_option:
    Search option after :: in file links.
    Example:
      [[file:~/example.org::255]] -> 255

  path:
    Target path portion used for later resolution work.
    For explicit typed links this is the part after the first colon.
    For minimally classified fuzzy links this preserves the raw target.

  path_absolute:
    Absolute path for file-like links when later resolution populates it.

  path_absolute / target_file_id / target_heading_id / target_custom_id / target_id:
    Deferred nullable target fields updated by the Phase 4 resolver.

  resolution_status:
    Explicit resolver state.
    NULL means no resolution pass has populated this row yet.
    When non-NULL it must be one of:
      unresolved
      resolved
      broken
      ambiguous
      unsupported

  resolution_diagnostic:
    Optional stable diagnostic text written by the resolver.
*/
CREATE TABLE IF NOT EXISTS links (
    id                  INTEGER PRIMARY KEY,
    file_id             INTEGER NOT NULL,
    heading_id          INTEGER NOT NULL,
    byte_start          INTEGER NOT NULL CHECK (byte_start >= 0),
    byte_end            INTEGER NOT NULL CHECK (byte_end >= byte_start),
    line                INTEGER NOT NULL CHECK (line > 0),
    source_context      TEXT NOT NULL CHECK (
                            source_context IN (
                                'normal',
                                'heading',
                                'property_drawer',
                                'drawer',
                                'verse_block',
                                'quote_block',
                                'center_block',
                                'justify_block'
                            )
                        ),
    format              TEXT NOT NULL CHECK (format IN ('plain', 'bracket', 'angle')),
    raw                 TEXT NOT NULL,
    raw_target          TEXT NOT NULL,
    raw_description     TEXT,
    link_type           TEXT NOT NULL,
    path                TEXT NOT NULL,
    search_option       TEXT,
    path_absolute       TEXT,
    target_file_id      INTEGER,
    target_heading_id   INTEGER,
    target_custom_id    TEXT,
    target_id           TEXT,
    resolution_status   TEXT CHECK (
                            resolution_status IN (
                                'unresolved',
                                'resolved',
                                'broken',
                                'ambiguous',
                                'unsupported'
                            )
                            OR resolution_status IS NULL
                        ),
    resolution_diagnostic TEXT,
    FOREIGN KEY (file_id)
        REFERENCES files(id)
        ON DELETE CASCADE,
    FOREIGN KEY (heading_id)
        REFERENCES headings(id)
        ON DELETE CASCADE,
    FOREIGN KEY (target_file_id)
        REFERENCES files(id)
        ON DELETE SET NULL,
    FOREIGN KEY (target_heading_id)
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
    heading. Synthetic level 0 rows participate in outline_path, so the root
    row has depth 0, materialized_path 0000, and a one-element breadcrumb array
    containing the file/document root title.

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

  This is a manually maintained derived/cache FTS table.

  The indexer recreates and bulk-populates it from canonical relational facts
  during FTS-enabled full rebuilds.

  rowid:
    Must match headings.id.
    Only real headings (level > 0) are indexed.

  title:
    Normalized heading title.

  body:
    Heading body text, if available; otherwise the empty string.

  content:
    Contentless. Canonical title/body storage remains in headings and
    heading_bodies rather than in the FTS virtual table.

  Note:
    SQLite virtual tables cannot enforce normal foreign keys here. The indexer
    is responsible for rebuilding heading_fts from canonical facts.

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

CREATE INDEX IF NOT EXISTS idx_files_path_lower
    ON files(LOWER(path));

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

CREATE INDEX IF NOT EXISTS idx_headings_title_lower
    ON headings(LOWER(title));

--------------------------------------------------
-- INDEXES: TODO KEYWORDS
--------------------------------------------------
CREATE INDEX IF NOT EXISTS idx_todo_keywords_file_state
    ON todo_keywords(file_id, state_type);

--------------------------------------------------
-- INDEXES: KEYWORDS
--------------------------------------------------
CREATE INDEX IF NOT EXISTS idx_keywords_keyword
    ON keywords(keyword);

CREATE INDEX IF NOT EXISTS idx_keywords_keyword_value_heading
    ON keywords(keyword COLLATE NOCASE, value, heading_id);

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

CREATE INDEX IF NOT EXISTS idx_effective_properties_file
    ON effective_properties(file_id);

CREATE INDEX IF NOT EXISTS idx_effective_properties_key_local_heading
    ON effective_properties(key, local_value, heading_id)
    WHERE local_value IS NOT NULL;

CREATE INDEX IF NOT EXISTS idx_effective_properties_key_effective_heading
    ON effective_properties(key, effective_value, heading_id);

--------------------------------------------------
-- INDEXES: TAGS
--------------------------------------------------
CREATE INDEX IF NOT EXISTS idx_tags_tag
    ON tags(tag);

CREATE INDEX IF NOT EXISTS idx_effective_tags_tag_heading
    ON effective_tags(tag, heading_id);

CREATE INDEX IF NOT EXISTS idx_effective_tags_file
    ON effective_tags(file_id);

--------------------------------------------------
-- INDEXES: LINKS
--------------------------------------------------
CREATE INDEX IF NOT EXISTS idx_links_heading
    ON links(heading_id);

CREATE INDEX IF NOT EXISTS idx_links_path
    ON links(path);

CREATE INDEX IF NOT EXISTS idx_links_target_file
    ON links(target_file_id);

CREATE INDEX IF NOT EXISTS idx_links_target_heading
    ON links(target_heading_id);

--------------------------------------------------
-- INDEXES: OUTLINE PATH
--------------------------------------------------
CREATE INDEX IF NOT EXISTS idx_outline_file_materialized_path
    ON outline_path(file_id, materialized_path);

CREATE INDEX IF NOT EXISTS idx_outline_parent
    ON outline_path(parent_id);
