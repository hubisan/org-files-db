BEGIN TRANSACTION;

--  All the directories in which the org-files are parsed.
CREATE TABLE IF NOT EXISTS directories (
    directory text NOT NULL PRIMARY KEY,
    -- Updated, mtime and size are used to make sure the directory is not
    -- dirty. Updated and mtime stored as seconds since the epoch.
    updated integer NOT NULL,
    mtime integer NOT NULL,
    size integer NOT NULL
);

--  Metadata of the org files in those directories.
CREATE TABLE IF NOT EXISTS files (
    filename text NOT NULL PRIMARY KEY,
    directory text NOT NULL,
    -- Updated, mtime and size are used to make sure the file is not dirty.
    -- Updated and mtime stored as seconds since the epoch.
    updated integer NOT NULL,
    mtime integer NOT NULL,
    size integer NOT NULL,
    title text,
    FOREIGN KEY (directory) REFERENCES directories (directory) ON DELETE CASCADE
);

--  Metadata of the headings in the org files.
CREATE TABLE IF NOT EXISTS headings (
    id integer NOT NULL PRIMARY KEY,
    file text NOT NULL,
    -- The level of the heading. An artificial level 0 heading
    -- is added to store file level properties and metadata.
    level integer NOT NULL,
    position integer NOT NULL,
    -- Store the full line text of the heading including stars.
    full_text text,
    -- Components of the heading.
    priority text NOT NULL,
    todo_keyword text,
    title text,
    statistic_cookies text,
    -- Store planning info as float to be able to store date and time.
    scheduled real,
    deadline real,
    closed real,
    -- Self reference to the parent id.
    parent_id integer,
    UNIQUE (file, position),
    FOREIGN KEY (file) REFERENCES files (file) ON DELETE CASCADE,
    FOREIGN KEY (parent_id) REFERENCES headings (id) ON DELETE CASCADE
);

--  Tags per heading.
CREATE TABLE IF NOT EXISTS tags (
    heading_id integer NOT NULL,
    tag text NOT NULL,
    PRIMARY KEY (heading_id, tag),
    FOREIGN KEY (heading_id) REFERENCES headings (id) ON DELETE CASCADE
);

--  Properties per heading.
CREATE TABLE IF NOT EXISTS properties (
    heading_id integer NOT NULL,
    property text NOT NULL,
    value text,
    PRIMARY KEY (heading_id, property),
    FOREIGN KEY (heading_id) REFERENCES headings (id) ON DELETE CASCADE
);

--  Links in the files.
CREATE TABLE IF NOT EXISTS links (
    file text NOT NULL,
    position integer NOT NULL,
    full_link text NOT NULL,
    type text,
    link text NOT NULL,
    description text,
    PRIMARY KEY (file, position),
    FOREIGN KEY (file) REFERENCES files (file) ON DELETE CASCADE
);

CREATE INDEX headings_title_id ON headings(title);

COMMIT;
