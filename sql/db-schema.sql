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
  footnote_section_p INTEGER
  parent_id INTEGER,
  FOREIGN KEY (file_id) REFERENCES files (id) ON DELETE CASCADE,
  FOREIGN KEY (parent_id) REFERENCES headings (id) ON DELETE CASCADE
);

--  Table to store tags for headings.
CREATE TABLE IF NOT EXISTS tags (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  heading_id INTEGER NOT NULL,
  tag TEXT NOT NULL,
  FOREIGN KEY (heading_id) REFERENCES headings (id) ON DELETE CASCADE
);

-- Table to store file-level keywords
CREATE TABLE IF NOT EXISTS keywords (
  id integer PRIMARY KEY AUTOINCREMENT,
  heading_id INTEGER NOT NULL,
  keyword TEXT NOT NULL,
  value TEXT,
  FOREIGN KEY (heading_id) REFERENCES headings (id) ON DELETE CASCADE
);

-- Table to store properties for headings
CREATE TABLE IF NOT EXISTS properties (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  heading_id INTEGER NOT NULL,
  key TEXT NOT NULL,
  value TEXT,
  FOREIGN KEY (heading_id) REFERENCES headings(id) ON DELETE CASCADE
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

-- Table to store general timestamp information for headings
CREATE TABLE IF NOT EXISTS timestamps (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    heading_id INTEGER NOT NULL,
    type TEXT NOT NULL,
    range_type TEXT,
    raw_value TEXT NOT NULL,
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
    FOREIGN KEY (timestamps) REFERENCES timestamps(id) ON DELETE CASCADE
);

COMMIT;

