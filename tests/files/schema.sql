CREATE TABLE IF NOT EXISTS directories (
    directory text NOT NULL PRIMARY KEY,
    updated integer NOT NULL,
    mtime integer NOT NULL,
    size integer NOT NULL
);
