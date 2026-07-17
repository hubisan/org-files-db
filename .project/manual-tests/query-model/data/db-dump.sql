PRAGMA foreign_keys=OFF;
BEGIN TRANSACTION;
CREATE TABLE files (
    id              INTEGER PRIMARY KEY,
    path            TEXT NOT NULL UNIQUE,
    mtime_ns        INTEGER NOT NULL,
    size            INTEGER NOT NULL,
    content_hash    TEXT,
    indexed_at      INTEGER
);
INSERT INTO files VALUES(1,'/home/hubisan/projects/coding/org-files-db/.project/manual-tests/query-model/org/properties.org',1783029353235070183,2783,NULL,1784245955);
INSERT INTO files VALUES(2,'/home/hubisan/projects/coding/org-files-db/.project/manual-tests/query-model/org/todo-keywords.org',1784242830730251043,0,NULL,1784245955);
CREATE TABLE todo_keywords (
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
INSERT INTO todo_keywords VALUES(1,'TODO','open',NULL,0,'config_default',NULL,NULL);
INSERT INTO todo_keywords VALUES(1,'NEXT','open',NULL,1,'config_default',NULL,NULL);
INSERT INTO todo_keywords VALUES(1,'DONE','closed',NULL,2,'config_default',NULL,NULL);
INSERT INTO todo_keywords VALUES(1,'CANCEL','closed',NULL,3,'config_default',NULL,NULL);
INSERT INTO todo_keywords VALUES(2,'TODO','open',NULL,0,'config_default',NULL,NULL);
INSERT INTO todo_keywords VALUES(2,'NEXT','open',NULL,1,'config_default',NULL,NULL);
INSERT INTO todo_keywords VALUES(2,'DONE','closed',NULL,2,'config_default',NULL,NULL);
INSERT INTO todo_keywords VALUES(2,'CANCEL','closed',NULL,3,'config_default',NULL,NULL);
CREATE TABLE db_metadata (
    key             TEXT PRIMARY KEY,
    value           TEXT NOT NULL
);
INSERT INTO db_metadata VALUES('fts_available','0');
INSERT INTO db_metadata VALUES('fts_body_indexed','0');
INSERT INTO db_metadata VALUES('fts_schema_version','0');
INSERT INTO db_metadata VALUES('body_text_available','0');
CREATE TABLE headings (
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
    priority            TEXT CHECK (priority IS NULL OR length(priority) = 1),
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
INSERT INTO headings VALUES(1,1,NULL,0,1,-1,2783,'Org Property and Keyword Test','Org Property and Keyword Test',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'["project","work"]');
INSERT INTO headings VALUES(2,1,1,1,16,465,638,'Task with multiple drawer properties','Task with multiple drawer properties',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'["project","work"]');
INSERT INTO headings VALUES(3,1,1,1,26,638,858,'Task with duplicate drawer properties','Task with duplicate drawer properties',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'["project","work"]');
INSERT INTO headings VALUES(4,1,1,1,35,858,1094,'Task with append operator in drawer','Task with append operator in drawer',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'["project","work"]');
INSERT INTO headings VALUES(5,1,1,1,45,1094,1304,'Task with mixed-case keys','Task with mixed-case keys',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'["project","work"]');
INSERT INTO headings VALUES(6,1,1,1,58,1304,1437,'Task with empty property accepted by Orgize','Task with empty property accepted by Orgize',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'["project","work"]');
INSERT INTO headings VALUES(7,1,1,1,67,1437,1718,'Task with Orgize empty-property limitation','Task with Orgize empty-property limitation',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'["project","work"]');
INSERT INTO headings VALUES(8,1,1,1,76,1718,1984,'Task after file-level property keywords','Task after file-level property keywords',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'["project","work"]');
INSERT INTO headings VALUES(9,1,1,1,83,1984,2264,'Task after later file-level keywords','Task after later file-level keywords',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'["project","work"]');
INSERT INTO headings VALUES(10,1,1,1,89,2264,2783,'Boundary: property-like but not properties','Boundary: property-like but not properties',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'["project","work"]');
INSERT INTO headings VALUES(11,2,NULL,0,1,-1,0,'todo-keywords',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
CREATE TABLE timestamps (
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
CREATE TABLE timestamp_repeaters (
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
CREATE TABLE keywords (
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
INSERT INTO keywords VALUES(1,1,'TITLE','Org Property and Keyword Test',6);
INSERT INTO keywords VALUES(2,1,'STARTUP','showall',7);
INSERT INTO keywords VALUES(3,1,'CATEGORY','category_keyword_value',8);
INSERT INTO keywords VALUES(4,1,'PROPERTY','Effort_ALL 0:10 0:30 1:00',9);
INSERT INTO keywords VALUES(5,1,'PROPERTY','keyword_property valid',10);
INSERT INTO keywords VALUES(6,1,'PROPERTY','keyword_overwritten_by_second invalid',11);
INSERT INTO keywords VALUES(7,1,'PROPERTY','keyword_overwritten_by_second valid',12);
INSERT INTO keywords VALUES(8,1,'PROPERTY','keyword_append foo=1',13);
INSERT INTO keywords VALUES(9,1,'PROPERTY','keyword_append+ bar=2',14);
INSERT INTO keywords VALUES(10,1,'PROPERTY','later_keyword_property works_everywhere',80);
INSERT INTO keywords VALUES(11,1,'CATEGORY','later_category_keyword',81);
INSERT INTO keywords VALUES(12,1,'FILETAGS',':project:work:',90);
INSERT INTO keywords VALUES(13,1,'TAGS','work(w) home(h)',91);
INSERT INTO keywords VALUES(14,1,'COLUMNS','%TODO %50ITEM %Effort{:} %CLOCKSUM',92);
INSERT INTO keywords VALUES(15,1,'CONSTANTS','c=299792458',93);
INSERT INTO keywords VALUES(16,1,'AUTHOR','Jane Doe',94);
INSERT INTO keywords VALUES(17,1,'OPTIONS','toc:nil num:t',95);
CREATE TABLE properties (
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
INSERT INTO properties VALUES(1,1,'CATEGORY','Level 0 Category Property','property_drawer',0,2);
INSERT INTO properties VALUES(2,1,'WHATEVER','level 0 drawer property','property_drawer',0,3);
INSERT INTO properties VALUES(3,1,'ID','7dad9b62-a3cc-43ec-a60f-e650bdaeae6d','property_drawer',0,4);
INSERT INTO properties VALUES(4,1,'CATEGORY','category_keyword_value','category_keyword',0,8);
INSERT INTO properties VALUES(5,1,'EFFORT_ALL','0:10 0:30 1:00','property_keyword',0,9);
INSERT INTO properties VALUES(6,1,'KEYWORD_PROPERTY','valid','property_keyword',0,10);
INSERT INTO properties VALUES(7,1,'KEYWORD_OVERWRITTEN_BY_SECOND','invalid','property_keyword',0,11);
INSERT INTO properties VALUES(8,1,'KEYWORD_OVERWRITTEN_BY_SECOND','valid','property_keyword',0,12);
INSERT INTO properties VALUES(9,1,'KEYWORD_APPEND','foo=1','property_keyword',0,13);
INSERT INTO properties VALUES(10,1,'KEYWORD_APPEND','bar=2','property_keyword',1,14);
INSERT INTO properties VALUES(11,1,'LATER_KEYWORD_PROPERTY','works_everywhere','property_keyword',0,80);
INSERT INTO properties VALUES(12,1,'CATEGORY','later_category_keyword','category_keyword',0,81);
INSERT INTO properties VALUES(13,2,'ID','abc','property_drawer',0,18);
INSERT INTO properties VALUES(14,2,'CUSTOM_ID','task-custom-id','property_drawer',0,19);
INSERT INTO properties VALUES(15,2,'EFFORT','0:30','property_drawer',0,20);
INSERT INTO properties VALUES(16,2,'OWNER','Alice','property_drawer',0,21);
INSERT INTO properties VALUES(17,2,'DRAWER_PROP','valid','property_drawer',0,22);
INSERT INTO properties VALUES(18,3,'DEFINED_TWICE','invalid','property_drawer',0,28);
INSERT INTO properties VALUES(19,3,'DEFINED_TWICE','valid','property_drawer',0,29);
INSERT INTO properties VALUES(20,4,'ADD-VALUE','is','property_drawer',0,37);
INSERT INTO properties VALUES(21,4,'ADD-VALUE','valid','property_drawer',1,38);
INSERT INTO properties VALUES(22,5,'ID','lowercase-id','property_drawer',0,47);
INSERT INTO properties VALUES(23,5,'CUSTOM_ID','mixed-case-custom-id','property_drawer',0,48);
INSERT INTO properties VALUES(24,5,'DRAWER_PROP','valid','property_drawer',0,49);
INSERT INTO properties VALUES(25,5,'ADD-VALUE','appended','property_drawer',1,50);
INSERT INTO properties VALUES(26,6,'EMPTY','','property_drawer',0,60);
CREATE TABLE tags (
    heading_id      INTEGER NOT NULL,
    tag             TEXT NOT NULL,
    FOREIGN KEY (heading_id)
        REFERENCES headings(id)
        ON DELETE CASCADE,
    PRIMARY KEY (heading_id, tag)
);
INSERT INTO tags VALUES(1,'project');
INSERT INTO tags VALUES(1,'work');
CREATE TABLE links (
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
CREATE TABLE heading_bodies (
    heading_id          INTEGER PRIMARY KEY,
    body_text           TEXT NOT NULL,
    body_byte_start     INTEGER,
    body_byte_end       INTEGER,
    FOREIGN KEY (heading_id)
        REFERENCES headings(id)
        ON DELETE CASCADE
);
CREATE TABLE outline_path (
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
INSERT INTO outline_path VALUES(1,1,NULL,0,'0000','["Org Property and Keyword Test"]');
INSERT INTO outline_path VALUES(2,1,1,1,'0000.0001','["Org Property and Keyword Test","Task with multiple drawer properties"]');
INSERT INTO outline_path VALUES(3,1,1,1,'0000.0002','["Org Property and Keyword Test","Task with duplicate drawer properties"]');
INSERT INTO outline_path VALUES(4,1,1,1,'0000.0003','["Org Property and Keyword Test","Task with append operator in drawer"]');
INSERT INTO outline_path VALUES(5,1,1,1,'0000.0004','["Org Property and Keyword Test","Task with mixed-case keys"]');
INSERT INTO outline_path VALUES(6,1,1,1,'0000.0005','["Org Property and Keyword Test","Task with empty property accepted by Orgize"]');
INSERT INTO outline_path VALUES(7,1,1,1,'0000.0006','["Org Property and Keyword Test","Task with Orgize empty-property limitation"]');
INSERT INTO outline_path VALUES(8,1,1,1,'0000.0007','["Org Property and Keyword Test","Task after file-level property keywords"]');
INSERT INTO outline_path VALUES(9,1,1,1,'0000.0008','["Org Property and Keyword Test","Task after later file-level keywords"]');
INSERT INTO outline_path VALUES(10,1,1,1,'0000.0009','["Org Property and Keyword Test","Boundary: property-like but not properties"]');
INSERT INTO outline_path VALUES(11,2,NULL,0,'0000','["todo-keywords"]');
CREATE INDEX idx_files_mtime_size
    ON files(mtime_ns, size);
CREATE INDEX idx_files_hash
    ON files(content_hash);
CREATE INDEX idx_todo_keywords_file_state
    ON todo_keywords(file_id, state_type);
CREATE UNIQUE INDEX uq_headings_file_level0
    ON headings(file_id)
    WHERE level = 0;
CREATE INDEX idx_timestamps_heading_id
    ON timestamps(heading_id);
CREATE INDEX idx_timestamps_role_start
    ON timestamps(role, start_ts);
CREATE INDEX idx_timestamps_start
    ON timestamps(start_ts);
CREATE INDEX idx_timestamp_repeaters_timestamp_id
    ON timestamp_repeaters(timestamp_id);
CREATE INDEX idx_headings_parent_id
    ON headings(parent_id);
CREATE INDEX idx_headings_todo
    ON headings(todo_keyword);
CREATE INDEX idx_headings_todo_type
    ON headings(todo_type);
CREATE INDEX idx_headings_scheduled
    ON headings(scheduled_ts);
CREATE INDEX idx_headings_deadline
    ON headings(deadline_ts);
CREATE INDEX idx_headings_closed
    ON headings(closed_ts);
CREATE INDEX idx_keywords_heading
    ON keywords(heading_id);
CREATE INDEX idx_keywords_keyword
    ON keywords(keyword);
CREATE INDEX idx_properties_heading_key
    ON properties(heading_id, key);
CREATE INDEX idx_properties_key_value
    ON properties(key, value);
CREATE INDEX idx_properties_id_lookup
    ON properties(value)
    WHERE key = 'ID';
CREATE INDEX idx_properties_custom_id_lookup
    ON properties(value)
    WHERE key = 'CUSTOM_ID';
CREATE INDEX idx_tags_tag
    ON tags(tag);
CREATE INDEX idx_tags_heading
    ON tags(heading_id);
CREATE INDEX idx_links_heading
    ON links(heading_id);
CREATE INDEX idx_links_path
    ON links(path);
CREATE INDEX idx_links_target_file
    ON links(target_file_id);
CREATE INDEX idx_links_target_heading
    ON links(target_heading_id);
CREATE INDEX idx_outline_file_materialized_path
    ON outline_path(file_id, materialized_path);
CREATE INDEX idx_outline_parent
    ON outline_path(parent_id);
COMMIT;
