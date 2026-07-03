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
INSERT INTO files VALUES(1,'/home/hubisan/projects/coding/org-files-db/.project/manual-tests/files/link-file-resolve/notes/index.org',1783114181649212594,2629,NULL,1783114189);
INSERT INTO files VALUES(2,'/home/hubisan/projects/coding/org-files-db/.project/manual-tests/files/link-file-resolve/notes/other/peer.org',1783094478000000000,549,NULL,1783114189);
INSERT INTO files VALUES(3,'/home/hubisan/projects/coding/org-files-db/.project/manual-tests/files/link-file-resolve/notes/sub/child.org',1783094478000000000,725,NULL,1783114189);
INSERT INTO files VALUES(4,'/home/hubisan/projects/coding/org-files-db/.project/manual-tests/files/link-file-resolve/notes/target.org',1783094478000000000,1034,NULL,1783114189);
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
INSERT INTO headings VALUES(1,1,NULL,0,1,-1,2629,'Index','Index',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(2,1,1,1,4,35,713,'Main Index','Main Index',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(3,1,2,2,10,109,331,'Same-file targets','Same-file targets',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(4,1,2,2,20,331,438,'Brackets in Title [2026-07-03 Fr]','Brackets in Title [2026-07-03 Fr]',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(5,1,2,2,26,438,596,'Duplicate Custom','Duplicate Custom',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(6,1,2,2,33,596,713,'Duplicate Custom Later','Duplicate Custom Later',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(7,1,1,1,40,713,1041,'File target links','File target links',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(8,1,1,1,60,1041,1356,'File heading-title links','File heading-title links',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(9,1,1,1,70,1356,1515,'Same-file star heading links','Same-file star heading links',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(10,1,1,1,77,1515,1860,'Same-file CUSTOM_ID links','Same-file CUSTOM_ID links',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(11,1,1,1,87,1860,2312,'File-context CUSTOM_ID links','File-context CUSTOM_ID links',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(12,1,1,1,99,2312,2629,'Org ID links','Org ID links',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(13,2,NULL,0,1,-1,549,'Peer','Peer',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(14,2,13,1,4,34,417,'Peer Heading','Peer Heading',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(15,2,13,1,20,417,549,'Duplicate ID Second','Duplicate ID Second',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(16,3,NULL,0,1,-1,725,'Child','Child',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(17,3,16,1,4,35,725,'Child Heading','Child Heading',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(18,4,NULL,0,1,-1,1034,'Target','Target',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(19,4,18,1,4,36,267,'Target Heading','Target Heading',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(20,4,18,1,17,267,440,'Bracket Title [A/B]','Bracket Title [A/B]',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(21,4,18,1,25,440,566,'Duplicate Custom First','Duplicate Custom First',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(22,4,18,1,32,566,707,'Duplicate Custom Second','Duplicate Custom Second',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(23,4,18,1,39,707,841,'Duplicate ID First','Duplicate ID First',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(24,4,18,1,46,841,1034,'Missing examples from target','Missing examples from target',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
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
INSERT INTO timestamps VALUES(1,4,'body',1783036800,NULL,'inactive','none','[2026-07-03 Fr]',352,367,20);
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
INSERT INTO keywords VALUES(3,13,'TITLE','Peer',1);
INSERT INTO keywords VALUES(4,13,'STARTUP','showall',2);
INSERT INTO keywords VALUES(5,16,'TITLE','Child',1);
INSERT INTO keywords VALUES(6,16,'STARTUP','showall',2);
INSERT INTO keywords VALUES(7,18,'TITLE','Target',1);
INSERT INTO keywords VALUES(8,18,'STARTUP','showall',2);
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
INSERT INTO properties VALUES(3,3,'CUSTOM_ID','same-file-target','property_drawer',0,12);
INSERT INTO properties VALUES(4,3,'CUSTOM_ID',' whitespace-in-custom-id ','property_drawer',0,13);
INSERT INTO properties VALUES(5,3,'ID',' id-with-whitespace ','property_drawer',0,14);
INSERT INTO properties VALUES(6,3,'ID','same-file-id-001','property_drawer',0,15);
INSERT INTO properties VALUES(7,4,'CUSTOM_ID','brackets-heading','property_drawer',0,22);
INSERT INTO properties VALUES(8,4,'ID','brackets-id-001','property_drawer',0,23);
INSERT INTO properties VALUES(9,5,'CUSTOM_ID','duplicate-custom','property_drawer',0,28);
INSERT INTO properties VALUES(10,6,'CUSTOM_ID','DUPLICATE-CUSTOM','property_drawer',0,35);
INSERT INTO properties VALUES(11,14,'CUSTOM_ID','peer-custom-id','property_drawer',0,6);
INSERT INTO properties VALUES(12,14,'ID','peer-id-001','property_drawer',0,7);
INSERT INTO properties VALUES(13,15,'ID','DUPLICATE-ID-001','property_drawer',0,22);
INSERT INTO properties VALUES(14,17,'CUSTOM_ID','child-custom-id','property_drawer',0,6);
INSERT INTO properties VALUES(15,17,'ID','child-id-001','property_drawer',0,7);
INSERT INTO properties VALUES(16,19,'CUSTOM_ID','target-custom-id','property_drawer',0,6);
INSERT INTO properties VALUES(17,19,'ID','target-id-001','property_drawer',0,7);
INSERT INTO properties VALUES(18,20,'CUSTOM_ID','bracket-title-custom-id','property_drawer',0,19);
INSERT INTO properties VALUES(19,20,'ID','bracket-title-id-001','property_drawer',0,20);
INSERT INTO properties VALUES(20,21,'CUSTOM_ID','duplicate-custom','property_drawer',0,27);
INSERT INTO properties VALUES(21,22,'CUSTOM_ID','DUPLICATE-CUSTOM','property_drawer',0,34);
INSERT INTO properties VALUES(22,23,'ID','duplicate-id-001','property_drawer',0,41);
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
INSERT INTO outline_path VALUES(2,1,1,1,'0000.0001','["Index","Main Index"]');
INSERT INTO outline_path VALUES(3,1,2,2,'0000.0001.0001','["Index","Main Index","Same-file targets"]');
INSERT INTO outline_path VALUES(4,1,2,2,'0000.0001.0002','["Index","Main Index","Brackets in Title [2026-07-03 Fr]"]');
INSERT INTO outline_path VALUES(5,1,2,2,'0000.0001.0003','["Index","Main Index","Duplicate Custom"]');
INSERT INTO outline_path VALUES(6,1,2,2,'0000.0001.0004','["Index","Main Index","Duplicate Custom Later"]');
INSERT INTO outline_path VALUES(7,1,1,1,'0000.0002','["Index","File target links"]');
INSERT INTO outline_path VALUES(8,1,1,1,'0000.0003','["Index","File heading-title links"]');
INSERT INTO outline_path VALUES(9,1,1,1,'0000.0004','["Index","Same-file star heading links"]');
INSERT INTO outline_path VALUES(10,1,1,1,'0000.0005','["Index","Same-file CUSTOM_ID links"]');
INSERT INTO outline_path VALUES(11,1,1,1,'0000.0006','["Index","File-context CUSTOM_ID links"]');
INSERT INTO outline_path VALUES(12,1,1,1,'0000.0007','["Index","Org ID links"]');
INSERT INTO outline_path VALUES(13,2,NULL,0,'0000','["Peer"]');
INSERT INTO outline_path VALUES(14,2,13,1,'0000.0001','["Peer","Peer Heading"]');
INSERT INTO outline_path VALUES(15,2,13,1,'0000.0002','["Peer","Duplicate ID Second"]');
INSERT INTO outline_path VALUES(16,3,NULL,0,'0000','["Child"]');
INSERT INTO outline_path VALUES(17,3,16,1,'0000.0001','["Child","Child Heading"]');
INSERT INTO outline_path VALUES(18,4,NULL,0,'0000','["Target"]');
INSERT INTO outline_path VALUES(19,4,18,1,'0000.0001','["Target","Target Heading"]');
INSERT INTO outline_path VALUES(20,4,18,1,'0000.0002','["Target","Bracket Title [A/B]"]');
INSERT INTO outline_path VALUES(21,4,18,1,'0000.0003','["Target","Duplicate Custom First"]');
INSERT INTO outline_path VALUES(22,4,18,1,'0000.0004','["Target","Duplicate Custom Second"]');
INSERT INTO outline_path VALUES(23,4,18,1,'0000.0005','["Target","Duplicate ID First"]');
INSERT INTO outline_path VALUES(24,4,18,1,'0000.0006','["Target","Missing examples from target"]');
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
INSERT INTO links VALUES(1,1,7,757,776,44,'normal','bracket','[[file:target.org]]','file:target.org',NULL,'file','target.org',NULL,'/home/hubisan/projects/coding/org-files-db/.project/manual-tests/files/link-file-resolve/notes/target.org',4,NULL,NULL,NULL,'resolved',NULL);
INSERT INTO links VALUES(2,1,7,779,801,45,'normal','bracket','[[file:sub/child.org]]','file:sub/child.org',NULL,'file','sub/child.org',NULL,'/home/hubisan/projects/coding/org-files-db/.project/manual-tests/files/link-file-resolve/notes/sub/child.org',3,NULL,NULL,NULL,'resolved',NULL);
INSERT INTO links VALUES(3,1,7,804,827,46,'normal','bracket','[[file:other/peer.org]]','file:other/peer.org',NULL,'file','other/peer.org',NULL,'/home/hubisan/projects/coding/org-files-db/.project/manual-tests/files/link-file-resolve/notes/other/peer.org',2,NULL,NULL,NULL,'resolved',NULL);
INSERT INTO links VALUES(4,1,7,830,846,47,'normal','bracket','[[./target.org]]','./target.org',NULL,'file','./target.org',NULL,'/home/hubisan/projects/coding/org-files-db/.project/manual-tests/files/link-file-resolve/notes/target.org',4,NULL,NULL,NULL,'resolved',NULL);
INSERT INTO links VALUES(5,1,7,879,896,51,'normal','angle','<file:target.org>','file:target.org',NULL,'file','target.org',NULL,'/home/hubisan/projects/coding/org-files-db/.project/manual-tests/files/link-file-resolve/notes/target.org',4,NULL,NULL,NULL,'resolved',NULL);
INSERT INTO links VALUES(6,1,7,899,914,52,'normal','plain','file:target.org','file:target.org',NULL,'file','target.org',NULL,'/home/hubisan/projects/coding/org-files-db/.project/manual-tests/files/link-file-resolve/notes/target.org',4,NULL,NULL,NULL,'resolved',NULL);
INSERT INTO links VALUES(7,1,7,951,971,56,'normal','bracket','[[file:missing.org]]','file:missing.org',NULL,'file','missing.org',NULL,'/home/hubisan/projects/coding/org-files-db/.project/manual-tests/files/link-file-resolve/notes/missing.org',NULL,NULL,NULL,NULL,'broken','missing in indexed universe');
INSERT INTO links VALUES(8,1,7,974,1004,57,'normal','bracket','[[file:sub/missing-child.org]]','file:sub/missing-child.org',NULL,'file','sub/missing-child.org',NULL,'/home/hubisan/projects/coding/org-files-db/.project/manual-tests/files/link-file-resolve/notes/sub/missing-child.org',NULL,NULL,NULL,NULL,'broken','missing in indexed universe');
INSERT INTO links VALUES(9,1,7,1007,1039,58,'normal','bracket','[[file:../external/outside.org]]','file:../external/outside.org',NULL,'file','../external/outside.org',NULL,'/home/hubisan/projects/coding/org-files-db/.project/manual-tests/files/link-file-resolve/external/outside.org',NULL,NULL,NULL,NULL,'unresolved','outside indexed universe');
INSERT INTO links VALUES(10,1,8,1071,1107,62,'normal','bracket','[[file:target.org::*Target Heading]]','file:target.org::*Target Heading',NULL,'file','target.org','*Target Heading','/home/hubisan/projects/coding/org-files-db/.project/manual-tests/files/link-file-resolve/notes/target.org',4,19,NULL,NULL,'resolved',NULL);
INSERT INTO links VALUES(11,1,8,1110,1146,63,'normal','bracket','[[file:target.org::*target heading]]','file:target.org::*target heading',NULL,'file','target.org','*target heading','/home/hubisan/projects/coding/org-files-db/.project/manual-tests/files/link-file-resolve/notes/target.org',4,19,NULL,NULL,'resolved',NULL);
INSERT INTO links VALUES(12,1,8,1149,1191,64,'normal','bracket','[[file:target.org::*   Target Heading   ]]','file:target.org::*   Target Heading   ',NULL,'file','target.org','*   Target Heading   ','/home/hubisan/projects/coding/org-files-db/.project/manual-tests/files/link-file-resolve/notes/target.org',4,19,NULL,NULL,'resolved',NULL);
INSERT INTO links VALUES(13,1,8,1194,1237,65,'normal','bracket','[[file:target.org::*Bracket Title \[A/B\]]]','file:target.org::*Bracket Title \[A/B\]',NULL,'file','target.org','*Bracket Title \[A/B\]','/home/hubisan/projects/coding/org-files-db/.project/manual-tests/files/link-file-resolve/notes/target.org',4,20,NULL,NULL,'resolved',NULL);
INSERT INTO links VALUES(14,1,8,1240,1279,66,'normal','angle','<file:target.org::*Bracket Title [A/B]>','file:target.org::*Bracket Title [A/B]',NULL,'file','target.org','*Bracket Title [A/B]','/home/hubisan/projects/coding/org-files-db/.project/manual-tests/files/link-file-resolve/notes/target.org',4,20,NULL,NULL,'resolved',NULL);
INSERT INTO links VALUES(15,1,8,1282,1306,67,'normal','plain','file:target.org::*Target','file:target.org::*Target',NULL,'file','target.org','*Target','/home/hubisan/projects/coding/org-files-db/.project/manual-tests/files/link-file-resolve/notes/target.org',4,NULL,NULL,NULL,'broken','heading not found');
INSERT INTO links VALUES(16,1,8,1317,1354,68,'normal','bracket','[[file:target.org::*Missing Heading]]','file:target.org::*Missing Heading',NULL,'file','target.org','*Missing Heading','/home/hubisan/projects/coding/org-files-db/.project/manual-tests/files/link-file-resolve/notes/target.org',4,NULL,NULL,NULL,'broken','heading not found');
INSERT INTO links VALUES(17,1,9,1390,1412,72,'normal','bracket','[[*Same-file targets]]','*Same-file targets',NULL,'fuzzy','*Same-file targets',NULL,NULL,1,3,NULL,NULL,'resolved',NULL);
INSERT INTO links VALUES(18,1,9,1415,1437,73,'normal','bracket','[[*same-file targets]]','*same-file targets',NULL,'fuzzy','*same-file targets',NULL,NULL,1,3,NULL,NULL,'resolved',NULL);
INSERT INTO links VALUES(19,1,9,1440,1480,74,'normal','bracket','[[*Brackets in Title \[2026-07-03 Fr\]]]','*Brackets in Title \[2026-07-03 Fr\]',NULL,'fuzzy','*Brackets in Title \[2026-07-03 Fr\]',NULL,NULL,1,4,NULL,NULL,'resolved',NULL);
INSERT INTO links VALUES(20,1,9,1483,1513,75,'normal','bracket','[[*Missing Same File Heading]]','*Missing Same File Heading',NULL,'fuzzy','*Missing Same File Heading',NULL,NULL,1,NULL,NULL,NULL,'broken','same-file heading not found');
INSERT INTO links VALUES(21,1,10,1546,1561,79,'normal','bracket','[[#main-index]]','#main-index',NULL,'custom-id','main-index',NULL,NULL,1,2,'main-index',NULL,'resolved',NULL);
INSERT INTO links VALUES(22,1,10,1564,1623,80,'normal','bracket','[[#same-file-target][same-file custom-id with description]]','#same-file-target','same-file custom-id with description','custom-id','same-file-target',NULL,NULL,1,3,'same-file-target',NULL,'resolved',NULL);
INSERT INTO links VALUES(23,1,10,1626,1676,81,'normal','bracket','[[#SAME-FILE-TARGET][custom-id: case-insensitive]]','#SAME-FILE-TARGET','custom-id: case-insensitive','custom-id','SAME-FILE-TARGET',NULL,NULL,1,3,'SAME-FILE-TARGET',NULL,'resolved',NULL);
INSERT INTO links VALUES(24,1,10,1679,1736,82,'normal','bracket','[[# SAME-FILE-TARGET ][custom-id: whitepace not allowed]]','# SAME-FILE-TARGET ','custom-id: whitepace not allowed','custom-id',' SAME-FILE-TARGET ',NULL,NULL,1,NULL,' SAME-FILE-TARGET ',NULL,'broken','custom id not found');
INSERT INTO links VALUES(25,1,10,1739,1809,83,'normal','bracket','[[# whitespace-in-custom-id ][custom-id: whitepace is part of the id]]','# whitespace-in-custom-id ','custom-id: whitepace is part of the id','custom-id',' whitespace-in-custom-id ',NULL,NULL,1,3,' whitespace-in-custom-id ',NULL,'resolved',NULL);
INSERT INTO links VALUES(26,1,10,1812,1833,84,'normal','bracket','[[#duplicate-custom]]','#duplicate-custom',NULL,'custom-id','duplicate-custom',NULL,NULL,1,5,'duplicate-custom',NULL,'resolved',NULL);
INSERT INTO links VALUES(27,1,10,1836,1858,85,'normal','bracket','[[#missing-custom-id]]','#missing-custom-id',NULL,'custom-id','missing-custom-id',NULL,NULL,1,NULL,'missing-custom-id',NULL,'broken','custom id not found');
INSERT INTO links VALUES(28,1,11,1894,1932,89,'normal','bracket','[[file:target.org::#target-custom-id]]','file:target.org::#target-custom-id',NULL,'file','target.org','#target-custom-id','/home/hubisan/projects/coding/org-files-db/.project/manual-tests/files/link-file-resolve/notes/target.org',4,19,'target-custom-id',NULL,'resolved',NULL);
INSERT INTO links VALUES(29,1,11,1935,2010,90,'normal','bracket','[[file:target.org::# TARGET-CUSTOM-ID ][target custom id with description]]','file:target.org::# TARGET-CUSTOM-ID ','target custom id with description','file','target.org','# TARGET-CUSTOM-ID ','/home/hubisan/projects/coding/org-files-db/.project/manual-tests/files/link-file-resolve/notes/target.org',4,NULL,' TARGET-CUSTOM-ID ',NULL,'broken','custom id not found');
INSERT INTO links VALUES(30,1,11,2013,2051,91,'normal','bracket','[[file:target.org::#duplicate-custom]]','file:target.org::#duplicate-custom',NULL,'file','target.org','#duplicate-custom','/home/hubisan/projects/coding/org-files-db/.project/manual-tests/files/link-file-resolve/notes/target.org',4,21,'duplicate-custom',NULL,'resolved',NULL);
INSERT INTO links VALUES(31,1,11,2054,2093,92,'normal','bracket','[[file:target.org::#missing-custom-id]]','file:target.org::#missing-custom-id',NULL,'file','target.org','#missing-custom-id','/home/hubisan/projects/coding/org-files-db/.project/manual-tests/files/link-file-resolve/notes/target.org',4,NULL,'missing-custom-id',NULL,'broken','custom id not found');
INSERT INTO links VALUES(32,1,11,2096,2132,93,'normal','angle','<file:target.org::#target-custom-id>','file:target.org::#target-custom-id',NULL,'file','target.org','#target-custom-id','/home/hubisan/projects/coding/org-files-db/.project/manual-tests/files/link-file-resolve/notes/target.org',4,19,'target-custom-id',NULL,'resolved',NULL);
INSERT INTO links VALUES(33,1,11,2135,2169,94,'normal','plain','file:target.org::#target-custom-id','file:target.org::#target-custom-id',NULL,'file','target.org','#target-custom-id','/home/hubisan/projects/coding/org-files-db/.project/manual-tests/files/link-file-resolve/notes/target.org',4,19,'target-custom-id',NULL,'resolved',NULL);
INSERT INTO links VALUES(34,1,11,2172,2212,95,'normal','bracket','[[file:sub/child.org::#child-custom-id]]','file:sub/child.org::#child-custom-id',NULL,'file','sub/child.org','#child-custom-id','/home/hubisan/projects/coding/org-files-db/.project/manual-tests/files/link-file-resolve/notes/sub/child.org',3,17,'child-custom-id',NULL,'resolved',NULL);
INSERT INTO links VALUES(35,1,11,2215,2255,96,'normal','bracket','[[file:other/peer.org::#peer-custom-id]]','file:other/peer.org::#peer-custom-id',NULL,'file','other/peer.org','#peer-custom-id','/home/hubisan/projects/coding/org-files-db/.project/manual-tests/files/link-file-resolve/notes/other/peer.org',2,14,'peer-custom-id',NULL,'resolved',NULL);
INSERT INTO links VALUES(36,1,11,2258,2310,97,'normal','bracket','[[file:../external/outside.org::#outside-custom-id]]','file:../external/outside.org::#outside-custom-id',NULL,'file','../external/outside.org','#outside-custom-id','/home/hubisan/projects/coding/org-files-db/.project/manual-tests/files/link-file-resolve/external/outside.org',NULL,NULL,NULL,NULL,'unresolved','outside indexed universe');
INSERT INTO links VALUES(37,1,12,2330,2350,101,'normal','bracket','[[id:target-id-001]]','id:target-id-001',NULL,'id','target-id-001',NULL,NULL,4,19,NULL,'target-id-001','resolved',NULL);
INSERT INTO links VALUES(38,1,12,2353,2395,102,'normal','bracket','[[id:TARGET-ID-001][id: case-insensitive]]','id:TARGET-ID-001','id: case-insensitive','id','TARGET-ID-001',NULL,NULL,4,19,NULL,'TARGET-ID-001','resolved',NULL);
INSERT INTO links VALUES(39,1,12,2398,2451,103,'normal','bracket','[[id: TARGET-ID-001 ][id: whitespace is not allowed]]','id: TARGET-ID-001 ','id: whitespace is not allowed','id',' TARGET-ID-001 ',NULL,NULL,NULL,NULL,NULL,' TARGET-ID-001 ','unresolved','id not found');
INSERT INTO links VALUES(40,1,12,2454,2515,104,'normal','bracket','[[id: id-with-whitespace ][id: whitespace is part of the id]]','id: id-with-whitespace ','id: whitespace is part of the id','id',' id-with-whitespace ',NULL,NULL,1,3,NULL,' id-with-whitespace ','resolved',NULL);
INSERT INTO links VALUES(41,1,12,2518,2535,105,'normal','angle','<id:child-id-001>','id:child-id-001',NULL,'id','child-id-001',NULL,NULL,3,17,NULL,'child-id-001','resolved',NULL);
INSERT INTO links VALUES(42,1,12,2538,2552,106,'normal','plain','id:peer-id-001','id:peer-id-001',NULL,'id','peer-id-001',NULL,NULL,2,14,NULL,'peer-id-001','resolved',NULL);
INSERT INTO links VALUES(43,1,12,2555,2578,107,'normal','bracket','[[id:same-file-id-001]]','id:same-file-id-001',NULL,'id','same-file-id-001',NULL,NULL,1,3,NULL,'same-file-id-001','resolved',NULL);
INSERT INTO links VALUES(44,1,12,2581,2602,108,'normal','bracket','[[id:missing-id-001]]','id:missing-id-001',NULL,'id','missing-id-001',NULL,NULL,NULL,NULL,NULL,'missing-id-001','unresolved','id not found');
INSERT INTO links VALUES(45,1,12,2605,2628,109,'normal','bracket','[[id:duplicate-id-001]]','id:duplicate-id-001',NULL,'id','duplicate-id-001',NULL,NULL,NULL,NULL,NULL,'duplicate-id-001','ambiguous','duplicate id');
INSERT INTO links VALUES(46,2,14,175,196,12,'normal','bracket','[[file:../index.org]]','file:../index.org',NULL,'file','../index.org',NULL,'/home/hubisan/projects/coding/org-files-db/.project/manual-tests/files/link-file-resolve/notes/index.org',1,NULL,NULL,NULL,'resolved',NULL);
INSERT INTO links VALUES(47,2,14,199,233,13,'normal','bracket','[[file:../index.org::#main-index]]','file:../index.org::#main-index',NULL,'file','../index.org','#main-index','/home/hubisan/projects/coding/org-files-db/.project/manual-tests/files/link-file-resolve/notes/index.org',1,2,'main-index',NULL,'resolved',NULL);
INSERT INTO links VALUES(48,2,14,236,275,14,'normal','bracket','[[file:../target.org::*Target Heading]]','file:../target.org::*Target Heading',NULL,'file','../target.org','*Target Heading','/home/hubisan/projects/coding/org-files-db/.project/manual-tests/files/link-file-resolve/notes/target.org',4,19,NULL,NULL,'resolved',NULL);
INSERT INTO links VALUES(49,2,14,278,319,15,'normal','bracket','[[file:../target.org::#target-custom-id]]','file:../target.org::#target-custom-id',NULL,'file','../target.org','#target-custom-id','/home/hubisan/projects/coding/org-files-db/.project/manual-tests/files/link-file-resolve/notes/target.org',4,19,'target-custom-id',NULL,'resolved',NULL);
INSERT INTO links VALUES(50,2,14,322,347,16,'normal','bracket','[[file:../sub/child.org]]','file:../sub/child.org',NULL,'file','../sub/child.org',NULL,'/home/hubisan/projects/coding/org-files-db/.project/manual-tests/files/link-file-resolve/notes/sub/child.org',3,NULL,NULL,NULL,'resolved',NULL);
INSERT INTO links VALUES(51,2,14,350,393,17,'normal','bracket','[[file:../sub/child.org::#child-custom-id]]','file:../sub/child.org::#child-custom-id',NULL,'file','../sub/child.org','#child-custom-id','/home/hubisan/projects/coding/org-files-db/.project/manual-tests/files/link-file-resolve/notes/sub/child.org',3,17,'child-custom-id',NULL,'resolved',NULL);
INSERT INTO links VALUES(52,2,14,396,415,18,'normal','bracket','[[id:child-id-001]]','id:child-id-001',NULL,'id','child-id-001',NULL,NULL,3,17,NULL,'child-id-001','resolved',NULL);
INSERT INTO links VALUES(53,3,17,169,190,12,'normal','bracket','[[file:../index.org]]','file:../index.org',NULL,'file','../index.org',NULL,'/home/hubisan/projects/coding/org-files-db/.project/manual-tests/files/link-file-resolve/notes/index.org',1,NULL,NULL,NULL,'resolved',NULL);
INSERT INTO links VALUES(54,3,17,193,227,13,'normal','bracket','[[file:../index.org::*Main Index]]','file:../index.org::*Main Index',NULL,'file','../index.org','*Main Index','/home/hubisan/projects/coding/org-files-db/.project/manual-tests/files/link-file-resolve/notes/index.org',1,2,NULL,NULL,'resolved',NULL);
INSERT INTO links VALUES(55,3,17,230,264,14,'normal','bracket','[[file:../index.org::#main-index]]','file:../index.org::#main-index',NULL,'file','../index.org','#main-index','/home/hubisan/projects/coding/org-files-db/.project/manual-tests/files/link-file-resolve/notes/index.org',1,2,'main-index',NULL,'resolved',NULL);
INSERT INTO links VALUES(56,3,17,267,289,15,'normal','bracket','[[file:../target.org]]','file:../target.org',NULL,'file','../target.org',NULL,'/home/hubisan/projects/coding/org-files-db/.project/manual-tests/files/link-file-resolve/notes/target.org',4,NULL,NULL,NULL,'resolved',NULL);
INSERT INTO links VALUES(57,3,17,292,331,16,'normal','bracket','[[file:../target.org::*Target Heading]]','file:../target.org::*Target Heading',NULL,'file','../target.org','*Target Heading','/home/hubisan/projects/coding/org-files-db/.project/manual-tests/files/link-file-resolve/notes/target.org',4,19,NULL,NULL,'resolved',NULL);
INSERT INTO links VALUES(58,3,17,334,375,17,'normal','bracket','[[file:../target.org::#target-custom-id]]','file:../target.org::#target-custom-id',NULL,'file','../target.org','#target-custom-id','/home/hubisan/projects/coding/org-files-db/.project/manual-tests/files/link-file-resolve/notes/target.org',4,19,'target-custom-id',NULL,'resolved',NULL);
INSERT INTO links VALUES(59,3,17,378,404,18,'normal','bracket','[[file:../other/peer.org]]','file:../other/peer.org',NULL,'file','../other/peer.org',NULL,'/home/hubisan/projects/coding/org-files-db/.project/manual-tests/files/link-file-resolve/notes/other/peer.org',2,NULL,NULL,NULL,'resolved',NULL);
INSERT INTO links VALUES(60,3,17,437,476,22,'normal','angle','<file:../target.org::#target-custom-id>','file:../target.org::#target-custom-id',NULL,'file','../target.org','#target-custom-id','/home/hubisan/projects/coding/org-files-db/.project/manual-tests/files/link-file-resolve/notes/target.org',4,19,'target-custom-id',NULL,'resolved',NULL);
INSERT INTO links VALUES(61,3,17,479,506,23,'normal','plain','file:../target.org::*Target','file:../target.org::*Target',NULL,'file','../target.org','*Target','/home/hubisan/projects/coding/org-files-db/.project/manual-tests/files/link-file-resolve/notes/target.org',4,NULL,NULL,NULL,'broken','heading not found');
INSERT INTO links VALUES(62,3,17,517,533,24,'normal','plain','id:target-id-001','id:target-id-001',NULL,'id','target-id-001',NULL,NULL,4,19,NULL,'target-id-001','resolved',NULL);
INSERT INTO links VALUES(63,3,17,570,599,28,'normal','bracket','[[file:missing-in-child.org]]','file:missing-in-child.org',NULL,'file','missing-in-child.org',NULL,'/home/hubisan/projects/coding/org-files-db/.project/manual-tests/files/link-file-resolve/notes/sub/missing-in-child.org',NULL,NULL,NULL,NULL,'broken','missing in indexed universe');
INSERT INTO links VALUES(64,3,17,602,637,29,'normal','bracket','[[file:../../external/outside.org]]','file:../../external/outside.org',NULL,'file','../../external/outside.org',NULL,'/home/hubisan/projects/coding/org-files-db/.project/manual-tests/files/link-file-resolve/external/outside.org',NULL,NULL,NULL,NULL,'unresolved','outside indexed universe');
INSERT INTO links VALUES(65,3,17,640,693,30,'normal','bracket','[[file:../../external/outside.org::*Outside heading]]','file:../../external/outside.org::*Outside heading',NULL,'file','../../external/outside.org','*Outside heading','/home/hubisan/projects/coding/org-files-db/.project/manual-tests/files/link-file-resolve/external/outside.org',NULL,NULL,NULL,NULL,'unresolved','outside indexed universe');
INSERT INTO links VALUES(66,3,17,696,724,31,'normal','bracket','[[id:missing-from-child-id]]','id:missing-from-child-id',NULL,'id','missing-from-child-id',NULL,NULL,NULL,NULL,NULL,'missing-from-child-id','unresolved','id not found');
INSERT INTO links VALUES(67,4,19,157,175,12,'normal','bracket','[[file:index.org]]','file:index.org',NULL,'file','index.org',NULL,'/home/hubisan/projects/coding/org-files-db/.project/manual-tests/files/link-file-resolve/notes/index.org',1,NULL,NULL,NULL,'resolved',NULL);
INSERT INTO links VALUES(68,4,19,178,209,13,'normal','bracket','[[file:index.org::*Main Index]]','file:index.org::*Main Index',NULL,'file','index.org','*Main Index','/home/hubisan/projects/coding/org-files-db/.project/manual-tests/files/link-file-resolve/notes/index.org',1,2,NULL,NULL,'resolved',NULL);
INSERT INTO links VALUES(69,4,19,212,243,14,'normal','bracket','[[file:index.org::#main-index]]','file:index.org::#main-index',NULL,'file','index.org','#main-index','/home/hubisan/projects/coding/org-files-db/.project/manual-tests/files/link-file-resolve/notes/index.org',1,2,'main-index',NULL,'resolved',NULL);
INSERT INTO links VALUES(70,4,19,246,265,15,'normal','bracket','[[id:index-id-001]]','id:index-id-001',NULL,'id','index-id-001',NULL,NULL,1,2,NULL,'index-id-001','resolved',NULL);
INSERT INTO links VALUES(71,4,24,875,907,48,'normal','bracket','[[file:missing-from-target.org]]','file:missing-from-target.org',NULL,'file','missing-from-target.org',NULL,'/home/hubisan/projects/coding/org-files-db/.project/manual-tests/files/link-file-resolve/notes/missing-from-target.org',NULL,NULL,NULL,NULL,'broken','missing in indexed universe');
INSERT INTO links VALUES(72,4,24,910,953,49,'normal','bracket','[[file:index.org::*Missing Parent Heading]]','file:index.org::*Missing Parent Heading',NULL,'file','index.org','*Missing Parent Heading','/home/hubisan/projects/coding/org-files-db/.project/manual-tests/files/link-file-resolve/notes/index.org',1,NULL,NULL,NULL,'broken','heading not found');
INSERT INTO links VALUES(73,4,24,956,1001,50,'normal','bracket','[[file:index.org::#missing-parent-custom-id]]','file:index.org::#missing-parent-custom-id',NULL,'file','index.org','#missing-parent-custom-id','/home/hubisan/projects/coding/org-files-db/.project/manual-tests/files/link-file-resolve/notes/index.org',1,NULL,'missing-parent-custom-id',NULL,'broken','custom id not found');
INSERT INTO links VALUES(74,4,24,1004,1033,51,'normal','bracket','[[id:missing-from-target-id]]','id:missing-from-target-id',NULL,'id','missing-from-target-id',NULL,NULL,NULL,NULL,NULL,'missing-from-target-id','unresolved','id not found');
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
