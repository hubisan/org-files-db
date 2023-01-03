BEGIN TRANSACTION;

--  Table to store directories containing org-files to be parsed.
CREATE TABLE IF NOT EXISTS directories (
  id integer PRIMARY KEY AUTOINCREMENT,
  -- Absolute path of the directory.
  directory text UNIQUE NOT NULL,
  -- Last time directory was updated, stored as seconds since the epoch.
  updated integer NOT NULL
);

CREATE INDEX directories_directory_idx ON directories (directory);

--  Table to store metadata of org files in directories.
CREATE TABLE IF NOT EXISTS files (
  id integer PRIMARY KEY AUTOINCREMENT,
  -- Foreign key referencing the directory containing this file.
  directory_id integer NOT NULL,
  -- Absolute path of the file.
  filename text NOT NULL,
  -- Last time file was updated, stored as seconds since the epoch.
  updated integer NOT NULL,file
  inode integer NOT NULL,
  -- Modification time as seconds since the epoch.
  mtime integer NOT NULL,
  -- Size in bytes reported by stat.
  size integer NOT NULL,
  -- Set up cascading delete for referenced directory.
  FOREIGN KEY (directory_id) REFERENCES directories (id) ON DELETE CASCADE
);

CREATE INDEX files_filename_idx ON files (filename);

--  Table to store metadata of headings in org files.
CREATE TABLE IF NOT EXISTS headings (
  id integer PRIMARY KEY AUTOINCREMENT,
  -- Foreign key referencing the file containing this heading.
  file_id integer NOT NULL,
  /* Level of the heading. An artificial level 0 heading is added to store
   file level properties and metadata. */
  level integer NOT NULL,
  point integer NOT NULL,
  -- Full line text of heading including stars.
  full_text text,
  -- Components of heading.
  priority text,
  todo_keyword text,
  title text,
  statistic_cookies text,
  -- Store planning info as float to store date and time.
  scheduled real,
  deadline real,
  closed real,
  -- Self reference to the parent id.
  parent_id integer,
  -- Set up unique constraint on file and point.
  UNIQUE (file_id, point),
  -- Set up cascading delete for referenced file.
  FOREIGN KEY (file_id) REFERENCES files (id) ON DELETE CASCADE,
  -- Set up cascading delete for referenced parent heading.
  FOREIGN KEY (parent_id) REFERENCES headings (id) ON DELETE CASCADE
);

CREATE INDEX headings_title_idx ON headings (title);

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

