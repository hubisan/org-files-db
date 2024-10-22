BEGIN TRANSACTION;

-- Table to store metadata of Org files
CREATE TABLE IF NOT EXISTS files (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  -- Absolute path of the file
  path TEXT NOT NULL,
  -- Timestamp of when the file record was created, in seconds since the epoch
  created_at REAL DEFAULT (strftime('%s', 'now')),
  -- Timestamp of when the file record was last updated, in seconds since the epoch
  updated_at REAL DEFAULT (strftime('%s', 'now')),
  -- Last modification time of the file, in seconds since the epoch
  modification_time REAL
);

-- Table to store headings extracted from Org files
CREATE TABLE IF NOT EXISTS headings (
  id integer PRIMARY KEY AUTOINCREMENT,
  -- Foreign key referencing the 'files' table.
  file_id integer NOT NULL,
  -- Heading level (0 for file-level, 1 for top-level, 2 for subheading, etc.).
  level INTEGER NOT NULL,
  -- Point of where the headings begins in the file.
  begin INTEGER NOT NULL,
  -- Components of heading.
  title_raw TEXT,
  --Title without statistic cookies and emphasis markers, and links converted to text.
  title TEXT,
  priority TEXT,
  todo_keyword TEXT,
  todo_type TEXT,
  statistic_cookies TEXT,
  -- Parent heading ID, referencing another entry in this table.
  parent_id INTEGER,
  UNIQUE (file_id, point),
  FOREIGN KEY (file_id) REFERENCES files (id) ON DELETE CASCADE,
  FOREIGN KEY (parent_id) REFERENCES headings (id) ON DELETE CASCADE
);

--  Table to store tags for headings.
CREATE TABLE IF NOT EXISTS tags (
  id integer PRIMARY KEY AUTOINCREMENT,
  -- Foreign key referencing the heading containing these tags.
  heading_id integer NOT NULL,
  tag text NOT NULL,
  -- Set up cascading delete for referenced heading.
  FOREIGN KEY (heading_id) REFERENCES headings (id) ON DELETE CASCADE
);

CREATE INDEX tags_tag_idx ON tags (tag);

--  Table to store properties for headings.
CREATE TABLE IF NOT EXISTS properties (
  id integer PRIMARY KEY AUTOINCREMENT,
  -- Foreign key referencing the heading containing these properties.
  heading_id integer NOT NULL,
  property text NOT NULL,
  value text,
  -- Set up cascading delete for referenced heading.
  FOREIGN KEY (heading_id) REFERENCES headings (id) ON DELETE CASCADE
);

--  Table to store links in files.
CREATE TABLE IF NOT EXISTS links (
  id integer PRIMARY KEY AUTOINCREMENT,
  -- Foreign key referencing the file containing these links.
  file_id integer NOT NULL,
  point integer NOT NULL,
  full_link text NOT NULL,
  type text,
  link text NOT NULL,
  description text,
  -- Set up unique constraint on file and point.
  UNIQUE (file_id, point),
  -- Set up cascading delete for referenced file.
  FOREIGN KEY (file_id) REFERENCES files (id) ON DELETE CASCADE
);

COMMIT;

