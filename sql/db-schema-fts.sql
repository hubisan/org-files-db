-- BEGIN TRANSACTION;

CREATE VIRUTAL TABLE IF NOT EXISTS files_fts
USING fts5 (
    file, content
);

-- COMMIT;

