BEGIN TRANSACTION;

-- Table to store metadata of Org files
CREATE TABLE IF NOT EXISTS files (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  -- Absolute path of the file
  path TEXT NOT NULL UNIQUE,
  -- Timestamp of when the file record was created, in seconds since the epoch
  created_at REAL NOT NULL DEFAULT (strftime('%s', 'now')),
  -- Timestamp of when the file record was last updated, in seconds since the epoch
  updated_at REAL NOT NULL DEFAULT (strftime('%s', 'now')),
  -- Store the hash, mtime is not enough for instance when using Git.
  md5_hash TEXT NOT NULL,
  -- Last modification time of the file, in seconds since the epoch
  modification_time REAL NOT NULL
);

-- Table to store headings extracted from Org files
CREATE TABLE IF NOT EXISTS headings (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  file_id INTEGER NOT NULL,
  level INTEGER NOT NULL,
  begin INTEGER NOT NULL,
  title TEXT,
  title_raw TEXT,
  priority TEXT,
  todo_keyword TEXT,
  todo_type TEXT,
  archivedp INTEGER,
  footnote_section_p INTEGER,
  outline TEXT,
  all_tags TEXT,
  parent_id INTEGER,
  FOREIGN KEY (file_id) REFERENCES files (id) ON DELETE CASCADE,
  FOREIGN KEY (parent_id) REFERENCES headings (id) ON DELETE CASCADE
);

--  Table to store tags for headings.
CREATE TABLE IF NOT EXISTS tags (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  heading_id INTEGER NOT NULL,
  tag TEXT NOT NULL,
  FOREIGN KEY (heading_id) REFERENCES headings (id) ON DELETE CASCADE,
  UNIQUE (heading_id, tag)
);

-- Table to store file-level keywords
CREATE TABLE IF NOT EXISTS keywords (
  id integer PRIMARY KEY AUTOINCREMENT,
  heading_id INTEGER NOT NULL,
  keyword TEXT NOT NULL,
  value TEXT,
  FOREIGN KEY (heading_id) REFERENCES headings (id) ON DELETE CASCADE,
  UNIQUE (heading_id, keyword)
);

-- Table to store properties for headings
CREATE TABLE IF NOT EXISTS properties (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  heading_id INTEGER NOT NULL,
  key TEXT NOT NULL,
  value TEXT,
  FOREIGN KEY (heading_id) REFERENCES headings(id) ON DELETE CASCADE,
  UNIQUE (heading_id, key)
);

-- Table to store links associated with headings
CREATE TABLE IF NOT EXISTS links (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  heading_id INTEGER NOT NULL,
  begin INTEGER NOT NULL,
  type TEXT,
  path TEXT,
  path_absolute TEXT,
  raw_link TEXT,
  description TEXT,
  format TEXT,
  search_option TEXT,
  FOREIGN KEY (heading_id) REFERENCES headings(id) ON DELETE CASCADE
);

-- Table to store timestamp information
CREATE TABLE IF NOT EXISTS timestamps (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    heading_id INTEGER NOT NULL,
    start_timestamp REAL NOT NULL,
    end_timestamp REAL,
    type TEXT NOT NULL,
    range_type TEXT,
    raw_value TEXT NOT NULL,
    FOREIGN KEY (heading_id) REFERENCES headings(id) ON DELETE CASCADE
);

-- Table to store repeater information for timestamps
CREATE TABLE IF NOT EXISTS repeater_timestamps (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    timestamp_id INTEGER NOT NULL,
    type TEXT NOT NULL,
    value INTEGER,
    unit TEXT,
    deadline_value INTEGER,
    deadline_unit TEXT,
    FOREIGN KEY (timestamp_id) REFERENCES timestamps(id) ON DELETE CASCADE
);

-- ===== FILES =====
CREATE INDEX IF NOT EXISTS idx_files_path ON files(path);
CREATE INDEX IF NOT EXISTS idx_files_hash ON files(hash);
CREATE INDEX IF NOT EXISTS idx_files_modification_time ON files(modification_time);

-- ===== HEADINGS =====
CREATE INDEX IF NOT EXISTS idx_headings_file_id ON headings(file_id);
CREATE INDEX IF NOT EXISTS idx_headings_parent_id ON headings(parent_id);
CREATE INDEX IF NOT EXISTS idx_headings_todo_keyword ON headings(todo_keyword);
CREATE INDEX IF NOT EXISTS idx_headings_todo_type ON headings(todo_type);
CREATE INDEX IF NOT EXISTS idx_headings_level ON headings(level);
CREATE INDEX IF NOT EXISTS idx_headings_all_tags ON headings(all_tags);

-- ===== TAGS =====
CREATE INDEX IF NOT EXISTS idx_tags_heading_id ON tags(heading_id);
CREATE INDEX IF NOT EXISTS idx_tags_tag ON tags(tag);

-- ===== KEYWORDS =====
CREATE INDEX IF NOT EXISTS idx_keywords_heading_id ON keywords(heading_id);
CREATE INDEX IF NOT EXISTS idx_keywords_keyword ON keywords(keyword);

-- ===== PROPERTIES =====
CREATE INDEX IF NOT EXISTS idx_properties_heading_id ON properties(heading_id);
CREATE INDEX IF NOT EXISTS idx_properties_key ON properties(key);

-- ===== LINKS =====
CREATE INDEX IF NOT EXISTS idx_links_heading_id ON links(heading_id);

-- ===== TIMESTAMPS =====
CREATE INDEX IF NOT EXISTS idx_timestamps_heading_id ON timestamps(heading_id);

COMMIT;

