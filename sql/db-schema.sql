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
-- A level 0 heading is used at file-level.
CREATE TABLE IF NOT EXISTS headings (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  -- Foreign key referencing the 'files' table.
  file_id INTEGER NOT NULL,
  -- Heading level (0 for file-level, 1 for top-level, 2 for subheading, etc.).
  level INTEGER NOT NULL,
  -- Point of where the headings begins in the file.
  begin INTEGER NOT NULL,
  --Title without statistic cookies and emphasis markers, and links converted to text.
  title TEXT,
  -- Raw title of the heading, excludes only TODO keyword.
  title_raw TEXT,
  -- Other components.
  priority TEXT,
  todo_keyword TEXT,
  todo_type TEXT,
  archivedp INTEGER,
  footnote_section_p INTEGER
  -- Parent heading ID, referencing another entry in this table.
  parent_id INTEGER,
  UNIQUE (file_id, point),
  FOREIGN KEY (file_id) REFERENCES files (id) ON DELETE CASCADE,
  FOREIGN KEY (parent_id) REFERENCES headings (id) ON DELETE CASCADE
);


--  Table to store tags for headings.
CREATE TABLE IF NOT EXISTS tags (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  -- Foreign key referencing the 'headings' table.
  heading_id INTEGER NOT NULL,
  -- Tag name.
  tag TEXT NOT NULL,
  -- Set up cascading delete for referenced heading.
  FOREIGN KEY (heading_id) REFERENCES headings (id) ON DELETE CASCADE
);

-- Table to store file-level keywords
CREATE TABLE IF NOT EXISTS keywords (
  id integer PRIMARY KEY AUTOINCREMENT,
  -- Foreign key referencing the 'headings' table.
  -- Keywords are stored in a heading with level 0.
  heading_id INTEGER NOT NULL,
  -- Keyword name.
  keyword TEXT NOT NULL,
  -- Associated value for the keyword.
  value TEXT,
  FOREIGN KEY (heading_id) REFERENCES headings (id) ON DELETE CASCADE
);

-- Table to store properties for headings
CREATE TABLE IF NOT EXISTS properties (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  -- Foreign key referencing the 'headings' table.
  heading_id INTEGER NOT NULL,
  -- Property key name.
  key TEXT NOT NULL,
  -- Property value.
  value TEXT NOT NULL,
  FOREIGN KEY (heading_id) REFERENCES headings(id) ON DELETE CASCADE
);

-- Table to store links associated with headings
CREATE TABLE IF NOT EXISTS links (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  -- Foreign key referencing the 'headings' table.
  heading_id INTEGER NOT NULL,
  -- Point of where the headings begins in the file.
  begin INTEGER NOT NULL,
  -- Type of link (e.g., file, http, etc.).
  -- Example: [[https://www.example.com][Example]] > https
  -- Example: https://www.example.com > https
  -- Example: [[file:../test.org]]  > file
  -- Example: [[../test.org]]  > file
  type TEXT,
  -- Path. Can be a relative one for files.
  -- Example: [[https://www.example.com][Example]] > //www.example.com
  -- Example: [[../test.org]]  > ../test.org
  path TEXT,
  -- Absolute path if it is of type file.
  -- Example: [[../example.org]]  > ~/example/example.org
  path_absolute TEXT,
  -- Description, if there is one.
  -- Example: [[https://www.example.com][Example]] > Example
  description TEXT,
  -- Format of the link (plain or bracket).
  -- Example: [[https://www.example.com][Example]] > bracket
  -- Example: https://www.example.com > plain
  format TEXT,
  -- The raw link without the description.
  -- Example: [[https://www.example.com][Example]] > https://www.example.com
  raw_link TEXT,
  -- Search option. Example: [[file:~/code/main.c::255]] > 255.
  search_option TEXT,
  -- Set up unique constraint on file and point.
  UNIQUE (heading_id, begin),
  -- Optional description of the link.
  description TEXT,
  FOREIGN KEY (heading_id) REFERENCES headings(id) ON DELETE CASCADE
);

COMMIT;

