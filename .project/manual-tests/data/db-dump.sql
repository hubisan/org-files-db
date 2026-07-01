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
INSERT INTO files VALUES(1,'/home/hubisan/projects/coding/org-files-db/.project/manual-tests/files/link-file-resolve/notes/index.org',1782941524859457724,837,NULL,1782941682);
INSERT INTO files VALUES(2,'/home/hubisan/projects/coding/org-files-db/.project/manual-tests/files/link-file-resolve/notes/other/peer.org',1782941542323623984,343,NULL,1782941682);
INSERT INTO files VALUES(3,'/home/hubisan/projects/coding/org-files-db/.project/manual-tests/files/link-file-resolve/notes/sub/child.org',1782941537827580578,428,NULL,1782941682);
INSERT INTO files VALUES(4,'/home/hubisan/projects/coding/org-files-db/.project/manual-tests/files/link-file-resolve/notes/target.org',1782941530491511278,358,NULL,1782941682);
CREATE TABLE headings (
    id                  INTEGER PRIMARY KEY,
    file_id             INTEGER NOT NULL,
    parent_id           INTEGER,
    level               INTEGER NOT NULL CHECK (level >= 0),
    line_number         INTEGER,
    byte_start          INTEGER NOT NULL,
    byte_end            INTEGER NOT NULL CHECK (byte_end >= byte_start),
    title               TEXT NOT NULL,
    title_raw           TEXT NOT NULL,
    todo_keyword        TEXT,
    todo_type           TEXT CHECK (todo_type IN ('open', 'closed') OR todo_type IS NULL),
    priority            TEXT CHECK (priority IS NULL OR length(priority) = 1),
    scheduled_raw       TEXT,
    scheduled_ts        INTEGER,
    deadline_raw        TEXT,
    deadline_ts         INTEGER,
    closed_raw          TEXT,
    closed_ts           INTEGER,
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
INSERT INTO headings VALUES(1,1,NULL,0,1,-1,837,'Index','Index',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(2,1,1,1,4,35,837,'Main index','Main index',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(3,2,NULL,0,1,-1,343,'Peer','Peer',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(4,2,3,1,4,34,343,'Peer heading','Peer heading',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(5,3,NULL,0,1,-1,428,'Child','Child',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(6,3,5,1,4,35,428,'Child heading','Child heading',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(7,4,NULL,0,1,-1,358,'Target','Target',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(8,4,7,1,4,36,222,'Target heading','Target heading',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(9,4,7,1,16,222,296,'Duplicate title','Duplicate title',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(10,4,7,1,20,296,358,'Duplicate title','Duplicate title',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
CREATE TABLE timestamps (
    id              INTEGER PRIMARY KEY,
    heading_id      INTEGER NOT NULL,
    role            TEXT CHECK (
                        role IN ('scheduled', 'deadline', 'closed', 'body')
                        OR role IS NULL
                    ),
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
INSERT INTO keywords VALUES(1,1,'TITLE','Index',1);
INSERT INTO keywords VALUES(2,1,'STARTUP','showall',2);
INSERT INTO keywords VALUES(3,3,'TITLE','Peer',1);
INSERT INTO keywords VALUES(4,3,'STARTUP','showall',2);
INSERT INTO keywords VALUES(5,5,'TITLE','Child',1);
INSERT INTO keywords VALUES(6,5,'STARTUP','showall',2);
INSERT INTO keywords VALUES(7,7,'TITLE','Target',1);
INSERT INTO keywords VALUES(8,7,'STARTUP','showall',2);
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
INSERT INTO properties VALUES(1,2,'CUSTOM_ID','main-index','property_drawer',0,6);
INSERT INTO properties VALUES(2,2,'ID','index-id-001','property_drawer',0,7);
INSERT INTO properties VALUES(3,4,'CUSTOM_ID','peer-custom-id','property_drawer',0,6);
INSERT INTO properties VALUES(4,4,'ID','peer-id-001','property_drawer',0,7);
INSERT INTO properties VALUES(5,6,'CUSTOM_ID','child-custom-id','property_drawer',0,6);
INSERT INTO properties VALUES(6,6,'ID','child-id-001','property_drawer',0,7);
INSERT INTO properties VALUES(7,8,'CUSTOM_ID','target-custom-id','property_drawer',0,6);
INSERT INTO properties VALUES(8,8,'ID','target-id-001','property_drawer',0,7);
CREATE TABLE tags (
    heading_id      INTEGER NOT NULL,
    tag             TEXT NOT NULL,
    FOREIGN KEY (heading_id)
        REFERENCES headings(id)
        ON DELETE CASCADE,
    PRIMARY KEY (heading_id, tag)
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
INSERT INTO outline_path VALUES(1,1,NULL,0,'0000','["Index"]');
INSERT INTO outline_path VALUES(2,1,1,1,'0000.0001','["Index","Main index"]');
INSERT INTO outline_path VALUES(3,2,NULL,0,'0000','["Peer"]');
INSERT INTO outline_path VALUES(4,2,3,1,'0000.0001','["Peer","Peer heading"]');
INSERT INTO outline_path VALUES(5,3,NULL,0,'0000','["Child"]');
INSERT INTO outline_path VALUES(6,3,5,1,'0000.0001','["Child","Child heading"]');
INSERT INTO outline_path VALUES(7,4,NULL,0,'0000','["Target"]');
INSERT INTO outline_path VALUES(8,4,7,1,'0000.0001','["Target","Target heading"]');
INSERT INTO outline_path VALUES(9,4,7,1,'0000.0002','["Target","Duplicate title"]');
INSERT INTO outline_path VALUES(10,4,7,1,'0000.0003','["Target","Duplicate title"]');
CREATE TABLE todo_keywords (
    file_id             INTEGER NOT NULL,
    keyword             TEXT NOT NULL,
    state_type          TEXT NOT NULL CHECK (state_type IN ('open', 'closed')),
    shortcut            TEXT CHECK (shortcut IS NULL OR length(shortcut) = 1),
    sequence_no         INTEGER NOT NULL,
    source_kind         TEXT NOT NULL CHECK (
                            source_kind IN ('config_default', 'org_keyword')
                        ),
    source_keyword      TEXT CHECK (
                            source_keyword IN ('TODO', 'SEQ_TODO', 'TYP_TODO')
                            OR source_keyword IS NULL
                        ),
    source_line_number  INTEGER CHECK (
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
INSERT INTO todo_keywords VALUES(1,'DONE','closed',NULL,1,'config_default',NULL,NULL);
INSERT INTO todo_keywords VALUES(2,'TODO','open',NULL,0,'config_default',NULL,NULL);
INSERT INTO todo_keywords VALUES(2,'DONE','closed',NULL,1,'config_default',NULL,NULL);
INSERT INTO todo_keywords VALUES(3,'TODO','open',NULL,0,'config_default',NULL,NULL);
INSERT INTO todo_keywords VALUES(3,'DONE','closed',NULL,1,'config_default',NULL,NULL);
INSERT INTO todo_keywords VALUES(4,'TODO','open',NULL,0,'config_default',NULL,NULL);
INSERT INTO todo_keywords VALUES(4,'DONE','closed',NULL,1,'config_default',NULL,NULL);
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
INSERT INTO links VALUES(1,1,2,133,152,12,'normal','bracket','[[file:target.org]]','file:target.org',NULL,'file','target.org',NULL,'/home/hubisan/projects/coding/org-files-db/.project/manual-tests/files/link-file-resolve/notes/target.org',4,NULL,NULL,NULL,'resolved',NULL);
INSERT INTO links VALUES(2,1,2,155,177,13,'normal','bracket','[[file:sub/child.org]]','file:sub/child.org',NULL,'file','sub/child.org',NULL,'/home/hubisan/projects/coding/org-files-db/.project/manual-tests/files/link-file-resolve/notes/sub/child.org',3,NULL,NULL,NULL,'resolved',NULL);
INSERT INTO links VALUES(3,1,2,180,203,14,'normal','bracket','[[file:other/peer.org]]','file:other/peer.org',NULL,'file','other/peer.org',NULL,'/home/hubisan/projects/coding/org-files-db/.project/manual-tests/files/link-file-resolve/notes/other/peer.org',2,NULL,NULL,NULL,'resolved',NULL);
INSERT INTO links VALUES(4,1,2,206,222,15,'normal','bracket','[[./target.org]]','./target.org',NULL,'file','./target.org',NULL,'/home/hubisan/projects/coding/org-files-db/.project/manual-tests/files/link-file-resolve/notes/target.org',4,NULL,NULL,NULL,'resolved',NULL);
INSERT INTO links VALUES(5,1,2,225,244,16,'normal','bracket','[[./sub/child.org]]','./sub/child.org',NULL,'file','./sub/child.org',NULL,'/home/hubisan/projects/coding/org-files-db/.project/manual-tests/files/link-file-resolve/notes/sub/child.org',3,NULL,NULL,NULL,'resolved',NULL);
INSERT INTO links VALUES(6,1,2,321,357,20,'normal','bracket','[[file:target.org::*Target heading]]','file:target.org::*Target heading',NULL,'file','target.org','*Target heading','/home/hubisan/projects/coding/org-files-db/.project/manual-tests/files/link-file-resolve/notes/target.org',4,NULL,NULL,NULL,'resolved',NULL);
INSERT INTO links VALUES(7,1,2,360,398,21,'normal','bracket','[[file:target.org::#target-custom-id]]','file:target.org::#target-custom-id',NULL,'file','target.org','#target-custom-id','/home/hubisan/projects/coding/org-files-db/.project/manual-tests/files/link-file-resolve/notes/target.org',4,NULL,NULL,NULL,'resolved',NULL);
INSERT INTO links VALUES(8,1,2,401,439,22,'normal','bracket','[[file:sub/child.org::*Child heading]]','file:sub/child.org::*Child heading',NULL,'file','sub/child.org','*Child heading','/home/hubisan/projects/coding/org-files-db/.project/manual-tests/files/link-file-resolve/notes/sub/child.org',3,NULL,NULL,NULL,'resolved',NULL);
INSERT INTO links VALUES(9,1,2,442,482,23,'normal','bracket','[[file:sub/child.org::#child-custom-id]]','file:sub/child.org::#child-custom-id',NULL,'file','sub/child.org','#child-custom-id','/home/hubisan/projects/coding/org-files-db/.project/manual-tests/files/link-file-resolve/notes/sub/child.org',3,NULL,NULL,NULL,'resolved',NULL);
INSERT INTO links VALUES(10,1,2,553,573,27,'normal','bracket','[[file:missing.org]]','file:missing.org',NULL,'file','missing.org',NULL,'/home/hubisan/projects/coding/org-files-db/.project/manual-tests/files/link-file-resolve/notes/missing.org',NULL,NULL,NULL,NULL,'broken','file target is missing from the indexed universe');
INSERT INTO links VALUES(11,1,2,576,606,28,'normal','bracket','[[file:sub/missing-child.org]]','file:sub/missing-child.org',NULL,'file','sub/missing-child.org',NULL,'/home/hubisan/projects/coding/org-files-db/.project/manual-tests/files/link-file-resolve/notes/sub/missing-child.org',NULL,NULL,NULL,NULL,'broken','file target is missing from the indexed universe');
INSERT INTO links VALUES(12,1,2,609,637,29,'normal','bracket','[[./other/missing-peer.org]]','./other/missing-peer.org',NULL,'file','./other/missing-peer.org',NULL,'/home/hubisan/projects/coding/org-files-db/.project/manual-tests/files/link-file-resolve/notes/other/missing-peer.org',NULL,NULL,NULL,NULL,'broken','file target is missing from the indexed universe');
INSERT INTO links VALUES(13,1,2,704,736,33,'normal','bracket','[[file:../external/outside.org]]','file:../external/outside.org',NULL,'file','../external/outside.org',NULL,'/home/hubisan/projects/coding/org-files-db/.project/manual-tests/files/link-file-resolve/external/outside.org',NULL,NULL,NULL,NULL,'unresolved','file target is outside the indexed universe');
INSERT INTO links VALUES(14,1,2,772,792,37,'normal','bracket','[[id:target-id-001]]','id:target-id-001',NULL,'id','target-id-001',NULL,NULL,NULL,NULL,NULL,NULL,'unsupported','no resolver implemented for link_type');
INSERT INTO links VALUES(15,1,2,795,810,38,'normal','bracket','[[#main-index]]','#main-index',NULL,'custom-id','#main-index',NULL,NULL,NULL,NULL,NULL,NULL,'unsupported','no resolver implemented for link_type');
INSERT INTO links VALUES(16,1,2,813,836,39,'normal','bracket','[[https://example.org]]','https://example.org',NULL,'https','//example.org',NULL,NULL,NULL,NULL,NULL,NULL,'unsupported','no resolver implemented for link_type');
INSERT INTO links VALUES(17,2,4,146,167,12,'normal','bracket','[[file:../index.org]]','file:../index.org',NULL,'file','../index.org',NULL,'/home/hubisan/projects/coding/org-files-db/.project/manual-tests/files/link-file-resolve/notes/index.org',1,NULL,NULL,NULL,'resolved',NULL);
INSERT INTO links VALUES(18,2,4,170,192,13,'normal','bracket','[[file:../target.org]]','file:../target.org',NULL,'file','../target.org',NULL,'/home/hubisan/projects/coding/org-files-db/.project/manual-tests/files/link-file-resolve/notes/target.org',4,NULL,NULL,NULL,'resolved',NULL);
INSERT INTO links VALUES(19,2,4,195,220,14,'normal','bracket','[[file:../sub/child.org]]','file:../sub/child.org',NULL,'file','../sub/child.org',NULL,'/home/hubisan/projects/coding/org-files-db/.project/manual-tests/files/link-file-resolve/notes/sub/child.org',3,NULL,NULL,NULL,'resolved',NULL);
INSERT INTO links VALUES(20,2,4,257,296,18,'normal','bracket','[[file:../target.org::*Target heading]]','file:../target.org::*Target heading',NULL,'file','../target.org','*Target heading','/home/hubisan/projects/coding/org-files-db/.project/manual-tests/files/link-file-resolve/notes/target.org',4,NULL,NULL,NULL,'resolved',NULL);
INSERT INTO links VALUES(21,2,4,299,342,19,'normal','bracket','[[file:../sub/child.org::#child-custom-id]]','file:../sub/child.org::#child-custom-id',NULL,'file','../sub/child.org','#child-custom-id','/home/hubisan/projects/coding/org-files-db/.project/manual-tests/files/link-file-resolve/notes/sub/child.org',3,NULL,NULL,NULL,'resolved',NULL);
INSERT INTO links VALUES(22,3,6,156,177,12,'normal','bracket','[[file:../index.org]]','file:../index.org',NULL,'file','../index.org',NULL,'/home/hubisan/projects/coding/org-files-db/.project/manual-tests/files/link-file-resolve/notes/index.org',1,NULL,NULL,NULL,'resolved',NULL);
INSERT INTO links VALUES(23,3,6,180,202,13,'normal','bracket','[[file:../target.org]]','file:../target.org',NULL,'file','../target.org',NULL,'/home/hubisan/projects/coding/org-files-db/.project/manual-tests/files/link-file-resolve/notes/target.org',4,NULL,NULL,NULL,'resolved',NULL);
INSERT INTO links VALUES(24,3,6,205,231,14,'normal','bracket','[[file:../other/peer.org]]','file:../other/peer.org',NULL,'file','../other/peer.org',NULL,'/home/hubisan/projects/coding/org-files-db/.project/manual-tests/files/link-file-resolve/notes/other/peer.org',2,NULL,NULL,NULL,'resolved',NULL);
INSERT INTO links VALUES(25,3,6,234,251,15,'normal','bracket','[[../target.org]]','../target.org',NULL,'file','../target.org',NULL,'/home/hubisan/projects/coding/org-files-db/.project/manual-tests/files/link-file-resolve/notes/target.org',4,NULL,NULL,NULL,'resolved',NULL);
INSERT INTO links VALUES(26,3,6,295,322,19,'normal','bracket','[[file:missing-in-sub.org]]','file:missing-in-sub.org',NULL,'file','missing-in-sub.org',NULL,'/home/hubisan/projects/coding/org-files-db/.project/manual-tests/files/link-file-resolve/notes/sub/missing-in-sub.org',NULL,NULL,NULL,NULL,'broken','file target is missing from the indexed universe');
INSERT INTO links VALUES(27,3,6,325,359,20,'normal','bracket','[[file:../missing-from-child.org]]','file:../missing-from-child.org',NULL,'file','../missing-from-child.org',NULL,'/home/hubisan/projects/coding/org-files-db/.project/manual-tests/files/link-file-resolve/notes/missing-from-child.org',NULL,NULL,NULL,NULL,'broken','file target is missing from the indexed universe');
INSERT INTO links VALUES(28,3,6,392,427,24,'normal','bracket','[[file:../../external/outside.org]]','file:../../external/outside.org',NULL,'file','../../external/outside.org',NULL,'/home/hubisan/projects/coding/org-files-db/.project/manual-tests/files/link-file-resolve/external/outside.org',NULL,NULL,NULL,NULL,'unresolved','file target is outside the indexed universe');
INSERT INTO links VALUES(29,4,8,136,154,12,'normal','bracket','[[file:index.org]]','file:index.org',NULL,'file','index.org',NULL,'/home/hubisan/projects/coding/org-files-db/.project/manual-tests/files/link-file-resolve/notes/index.org',1,NULL,NULL,NULL,'resolved',NULL);
INSERT INTO links VALUES(30,4,8,157,179,13,'normal','bracket','[[file:sub/child.org]]','file:sub/child.org',NULL,'file','sub/child.org',NULL,'/home/hubisan/projects/coding/org-files-db/.project/manual-tests/files/link-file-resolve/notes/sub/child.org',3,NULL,NULL,NULL,'resolved',NULL);
INSERT INTO links VALUES(31,4,8,182,220,14,'normal','bracket','[[file:other/peer.org::*Peer heading]]','file:other/peer.org::*Peer heading',NULL,'file','other/peer.org','*Peer heading','/home/hubisan/projects/coding/org-files-db/.project/manual-tests/files/link-file-resolve/notes/other/peer.org',2,NULL,NULL,NULL,'resolved',NULL);
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
CREATE INDEX idx_files_mtime_size
    ON files(mtime_ns, size);
CREATE INDEX idx_files_hash
    ON files(content_hash);
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
CREATE INDEX idx_outline_file_materialized_path
    ON outline_path(file_id, materialized_path);
CREATE INDEX idx_outline_parent
    ON outline_path(parent_id);
CREATE INDEX idx_todo_keywords_file_state
    ON todo_keywords(file_id, state_type);
CREATE INDEX idx_links_heading
    ON links(heading_id);
CREATE INDEX idx_links_path
    ON links(path);
CREATE INDEX idx_links_target_file
    ON links(target_file_id);
CREATE INDEX idx_links_target_heading
    ON links(target_heading_id);
COMMIT;
