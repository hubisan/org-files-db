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
INSERT INTO files VALUES(1,'/home/hubisan/projects/coding/org-files-db/.project/manual-tests/query-model/org/00-index.org',1783801546374100331,1253,NULL,1784233408);
INSERT INTO files VALUES(2,'/home/hubisan/projects/coding/org-files-db/.project/manual-tests/query-model/org/archive/2025.org',1783796759000000000,229,NULL,1784233408);
INSERT INTO files VALUES(3,'/home/hubisan/projects/coding/org-files-db/.project/manual-tests/query-model/org/duplicate-ids.org',1783796759000000000,176,NULL,1784233408);
INSERT INTO files VALUES(4,'/home/hubisan/projects/coding/org-files-db/.project/manual-tests/query-model/org/notes.org',1783796759000000000,903,NULL,1784233408);
INSERT INTO files VALUES(5,'/home/hubisan/projects/coding/org-files-db/.project/manual-tests/query-model/org/people.org',1783796759000000000,345,NULL,1784233408);
INSERT INTO files VALUES(6,'/home/hubisan/projects/coding/org-files-db/.project/manual-tests/query-model/org/prio.org',1783802108961568962,80,NULL,1784233408);
INSERT INTO files VALUES(7,'/home/hubisan/projects/coding/org-files-db/.project/manual-tests/query-model/org/projects.org',1783796759000000000,944,NULL,1784233408);
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
INSERT INTO todo_keywords VALUES(3,'TODO','open',NULL,0,'config_default',NULL,NULL);
INSERT INTO todo_keywords VALUES(3,'NEXT','open',NULL,1,'config_default',NULL,NULL);
INSERT INTO todo_keywords VALUES(3,'DONE','closed',NULL,2,'config_default',NULL,NULL);
INSERT INTO todo_keywords VALUES(3,'CANCEL','closed',NULL,3,'config_default',NULL,NULL);
INSERT INTO todo_keywords VALUES(4,'TODO','open',NULL,0,'config_default',NULL,NULL);
INSERT INTO todo_keywords VALUES(4,'NEXT','open',NULL,1,'config_default',NULL,NULL);
INSERT INTO todo_keywords VALUES(4,'DONE','closed',NULL,2,'config_default',NULL,NULL);
INSERT INTO todo_keywords VALUES(4,'CANCEL','closed',NULL,3,'config_default',NULL,NULL);
INSERT INTO todo_keywords VALUES(5,'TODO','open',NULL,0,'config_default',NULL,NULL);
INSERT INTO todo_keywords VALUES(5,'NEXT','open',NULL,1,'config_default',NULL,NULL);
INSERT INTO todo_keywords VALUES(5,'DONE','closed',NULL,2,'config_default',NULL,NULL);
INSERT INTO todo_keywords VALUES(5,'CANCEL','closed',NULL,3,'config_default',NULL,NULL);
INSERT INTO todo_keywords VALUES(6,'TODO','open',NULL,0,'config_default',NULL,NULL);
INSERT INTO todo_keywords VALUES(6,'NEXT','open',NULL,1,'config_default',NULL,NULL);
INSERT INTO todo_keywords VALUES(6,'DONE','closed',NULL,2,'config_default',NULL,NULL);
INSERT INTO todo_keywords VALUES(6,'CANCEL','closed',NULL,3,'config_default',NULL,NULL);
INSERT INTO todo_keywords VALUES(7,'TODO','open','t',0,'org_keyword','TODO',5);
INSERT INTO todo_keywords VALUES(7,'NEXT','open','n',1,'org_keyword','TODO',5);
INSERT INTO todo_keywords VALUES(7,'WAIT','open','w',2,'org_keyword','TODO',5);
INSERT INTO todo_keywords VALUES(7,'REVIEW','open','r',3,'org_keyword','TODO',5);
INSERT INTO todo_keywords VALUES(7,'DONE','closed','d',4,'org_keyword','TODO',5);
INSERT INTO todo_keywords VALUES(7,'CANCEL','closed','c',5,'org_keyword','TODO',5);
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
INSERT INTO headings VALUES(1,1,NULL,0,1,-1,1253,'Org Files Test Index','Org Files Test Index',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'["dashboard","index"]');
INSERT INTO headings VALUES(2,1,1,1,13,340,386,'Statistic Cookies','[#B] Statistic Cookies [0/1]',NULL,NULL,'B',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'["dashboard","index"]');
INSERT INTO headings VALUES(3,1,2,2,15,372,386,'test','TODO test','TODO','open',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'["dashboard","index"]');
INSERT INTO headings VALUES(4,1,1,1,17,386,413,'Statistic Cookies','Statistic Cookies [0/0]',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'["dashboard","index"]');
INSERT INTO headings VALUES(5,1,1,1,19,413,443,'Review query CLI','NEXT [#A] Review query CLI','NEXT','open','A',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'["dashboard","index"]');
INSERT INTO headings VALUES(6,1,1,1,21,443,893,'Review query CLI','NEXT [#A] Review query CLI','NEXT','open','A',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'["dashboard","index","project"]');
INSERT INTO headings VALUES(7,1,6,2,31,749,827,'Add documentation examples','TODO Add documentation examples','TODO','open',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'["dashboard","index","project","docs"]');
INSERT INTO headings VALUES(8,1,6,2,36,827,893,'Verify JSON output','DONE Verify JSON output','DONE','closed',NULL,NULL,NULL,NULL,NULL,NULL,NULL,'[2026-07-10 Fri 17:30]',1783704600,1,0,0,'["dashboard","index","project","test"]');
INSERT INTO headings VALUES(9,1,1,1,39,893,1005,'Inbox item','TODO Inbox item','TODO','open',NULL,NULL,NULL,NULL,'<2026-12-31 Thu 23:59>',1798761540,1,NULL,NULL,NULL,0,0,'["dashboard","index","inbox"]');
INSERT INTO headings VALUES(10,1,1,1,42,1005,1253,'Reference links','Reference links',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'["dashboard","index","links"]');
INSERT INTO headings VALUES(11,2,NULL,0,1,-1,229,'Archive 2025','Archive 2025',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'["archive"]');
INSERT INTO headings VALUES(12,2,11,1,5,65,162,'Legacy migration','DONE Legacy migration','DONE','closed',NULL,NULL,NULL,NULL,NULL,NULL,NULL,'[2025-12-20 Sat 12:00]',1766232000,1,0,0,'["archive","legacy"]');
INSERT INTO headings VALUES(13,2,11,1,11,162,229,'Obsolete experiment','CANCEL Obsolete experiment','CANCEL','closed',NULL,NULL,NULL,NULL,NULL,NULL,NULL,'[2025-11-01 Sat]',1761955200,0,0,0,'["archive","experiment"]');
INSERT INTO headings VALUES(14,3,NULL,0,1,-1,176,'Duplicate IDs','Duplicate IDs',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'["test","duplicates"]');
INSERT INTO headings VALUES(15,3,14,1,4,54,115,'First duplicate','First duplicate',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'["test","duplicates"]');
INSERT INTO headings VALUES(16,3,14,1,9,115,176,'Second duplicate','Second duplicate',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'["test","duplicates"]');
INSERT INTO headings VALUES(17,4,NULL,0,1,-1,903,'Technical Notes','Technical Notes',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'["notes","reference"]');
INSERT INTO headings VALUES(18,4,17,1,6,103,741,'Query Model','Query Model',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'["notes","reference","query","spec"]');
INSERT INTO headings VALUES(19,4,18,2,15,277,438,'Timestamp semantics','Timestamp semantics',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'["notes","reference","query","spec","time"]');
INSERT INTO headings VALUES(20,4,18,2,20,438,741,'Link semantics','Link semantics',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'["notes","reference","query","spec","links"]');
INSERT INTO headings VALUES(21,4,17,1,27,741,856,'SQLite Notes','SQLite Notes',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'["notes","reference","sqlite","database"]');
INSERT INTO headings VALUES(22,4,17,1,34,856,903,'Empty Description Link','Empty Description Link',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'["notes","reference"]');
INSERT INTO headings VALUES(23,5,NULL,0,1,-1,345,'People','People',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'["people"]');
INSERT INTO headings VALUES(24,5,23,1,5,59,160,'Hubi','Hubi',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'["people","person","maintainer"]');
INSERT INTO headings VALUES(25,5,23,1,12,160,251,'Alex','Alex',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'["people","person","docs"]');
INSERT INTO headings VALUES(26,5,23,1,19,251,345,'Sam','Sam',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'["people","person","security"]');
INSERT INTO headings VALUES(27,6,NULL,0,1,-1,80,'prio',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(28,6,27,1,1,0,27,'Priority Test','NEXT [#A] Priority Test','NEXT','open','A',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(29,6,27,1,3,27,54,'Priority Test','TODO [#B] Priority Test','TODO','open','B',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(30,6,27,1,5,54,80,'Priority Test','DONE [#B] Priority Test','DONE','closed','B',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(31,7,NULL,0,1,-1,944,'Projects','Projects',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'["work","project"]');
INSERT INTO headings VALUES(32,7,31,1,7,158,818,'Query Engine','NEXT [#A] Query Engine','NEXT','open','A',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'["work","project","rust","sqlite"]');
INSERT INTO headings VALUES(33,7,32,2,24,559,658,'Safe SQL translation','TODO Safe SQL translation','TODO','open',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'["work","project","rust","sqlite","security"]');
INSERT INTO headings VALUES(34,7,32,2,30,658,758,'Result shaping','REVIEW Result shaping','REVIEW','open',NULL,'<2026-07-15 Wed>',1784073600,0,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'["work","project","rust","sqlite","json"]');
INSERT INTO headings VALUES(35,7,34,3,33,719,758,'Outline context nodes','TODO Outline context nodes','TODO','open',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'["work","project","rust","sqlite","json","tree"]');
INSERT INTO headings VALUES(36,7,32,2,35,758,818,'AST parser','DONE AST parser','DONE','closed',NULL,NULL,NULL,NULL,NULL,NULL,NULL,'[2026-07-07 Tue 18:15]',1783448100,1,0,0,'["work","project","rust","sqlite","parser"]');
INSERT INTO headings VALUES(37,7,31,1,38,818,886,'Mobile integration','WAIT Mobile integration','WAIT','open',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'["work","project","mobile"]');
INSERT INTO headings VALUES(38,7,31,1,43,886,944,'Old prototype','CANCEL Old prototype','CANCEL','closed',NULL,NULL,NULL,NULL,NULL,NULL,NULL,'[2026-06-01 Mon]',1780272000,0,0,0,'["work","project","archive"]');
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
INSERT INTO timestamps VALUES(1,6,'body',1,1783760400,NULL,'active','none','<2026-07-11 Sat 09:00>',596,618,26);
INSERT INTO timestamps VALUES(2,6,'body',0,1784332800,NULL,'active','none','<2026-07-18 Sat>',629,645,27);
INSERT INTO timestamps VALUES(3,8,'closed',1,1783704600,NULL,'inactive','none','[2026-07-10 Fri 17:30]',869,891,37);
INSERT INTO timestamps VALUES(4,9,'deadline',1,1798761540,NULL,'active','none','<2026-12-31 Thu 23:59>',981,1003,40);
INSERT INTO timestamps VALUES(5,12,'closed',1,1766232000,NULL,'inactive','none','[2025-12-20 Sat 12:00]',106,128,6);
INSERT INTO timestamps VALUES(6,13,'closed',0,1761955200,NULL,'inactive','none','[2025-11-01 Sat]',212,228,12);
INSERT INTO timestamps VALUES(7,19,'body',1,1783764900,NULL,'active','none','<2026-07-11 Sat 10:15>',328,350,16);
INSERT INTO timestamps VALUES(8,19,'body',1,1783716300,NULL,'inactive','none','[2026-07-10 Fri 20:45]',374,396,17);
INSERT INTO timestamps VALUES(9,19,'body',0,1783900800,NULL,'active','none','<2026-07-13 Mon>',420,436,18);
INSERT INTO timestamps VALUES(10,32,'body',1,1783845000,NULL,'active','none','<2026-07-12 Sun 08:30>',306,328,14);
INSERT INTO timestamps VALUES(11,32,'body',1,1784563200,NULL,'active','none','<2026-07-20 Mon 16:00>',339,361,15);
INSERT INTO timestamps VALUES(12,33,'body',0,1783987200,NULL,'active','none','<2026-07-14 Tue>',640,656,28);
INSERT INTO timestamps VALUES(13,34,'scheduled',0,1784073600,NULL,'active','none','<2026-07-15 Wed>',701,717,31);
INSERT INTO timestamps VALUES(14,36,'closed',1,1783448100,NULL,'inactive','none','[2026-07-07 Tue 18:15]',794,816,36);
INSERT INTO timestamps VALUES(15,38,'closed',0,1780272000,NULL,'inactive','none','[2026-06-01 Mon]',927,943,44);
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
INSERT INTO keywords VALUES(1,1,'TITLE','Org Files Test Index',1);
INSERT INTO keywords VALUES(2,1,'FILETAGS',':dashboard:index:',2);
INSERT INTO keywords VALUES(3,1,'CATEGORY','dashboard',3);
INSERT INTO keywords VALUES(4,1,'PROPERTY','OWNER Hubi',4);
INSERT INTO keywords VALUES(5,11,'TITLE','Archive 2025',1);
INSERT INTO keywords VALUES(6,11,'FILETAGS',':archive:',2);
INSERT INTO keywords VALUES(7,11,'CATEGORY','archive',3);
INSERT INTO keywords VALUES(8,14,'TITLE','Duplicate IDs',1);
INSERT INTO keywords VALUES(9,14,'FILETAGS',':test:duplicates:',2);
INSERT INTO keywords VALUES(10,17,'TITLE','Technical Notes',1);
INSERT INTO keywords VALUES(11,17,'FILETAGS',':notes:reference:',2);
INSERT INTO keywords VALUES(12,17,'CATEGORY','notes',3);
INSERT INTO keywords VALUES(13,17,'PROPERTY','LANGUAGE English',4);
INSERT INTO keywords VALUES(14,23,'TITLE','People',1);
INSERT INTO keywords VALUES(15,23,'FILETAGS',':people:',2);
INSERT INTO keywords VALUES(16,23,'CATEGORY','contacts',3);
INSERT INTO keywords VALUES(17,31,'TITLE','Projects',1);
INSERT INTO keywords VALUES(18,31,'FILETAGS',':work:project:',2);
INSERT INTO keywords VALUES(19,31,'CATEGORY','projects',3);
INSERT INTO keywords VALUES(20,31,'PROPERTY','AREA Engineering',4);
INSERT INTO keywords VALUES(21,31,'TODO','TODO(t) NEXT(n) WAIT(w) REVIEW(r) | DONE(d) CANCEL(c)',5);
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
INSERT INTO properties VALUES(1,1,'CATEGORY','dashboard','category_keyword',0,3);
INSERT INTO properties VALUES(2,1,'OWNER','Hubi','property_keyword',0,4);
INSERT INTO properties VALUES(3,6,'CUSTOM_ID','review-query-cli','property_drawer',0,23);
INSERT INTO properties VALUES(4,6,'OWNER','   Hubi','property_drawer',0,24);
INSERT INTO properties VALUES(5,7,'OWNER','   Alex','property_drawer',0,33);
INSERT INTO properties VALUES(6,11,'CATEGORY','archive','category_keyword',0,3);
INSERT INTO properties VALUES(7,12,'OWNER','Hubi','property_drawer',0,8);
INSERT INTO properties VALUES(8,15,'ID','duplicate-test-id','property_drawer',0,6);
INSERT INTO properties VALUES(9,16,'ID','duplicate-test-id','property_drawer',0,11);
INSERT INTO properties VALUES(10,17,'CATEGORY','notes','category_keyword',0,3);
INSERT INTO properties VALUES(11,17,'LANGUAGE','English','property_keyword',0,4);
INSERT INTO properties VALUES(12,18,'CUSTOM_ID','query-model','property_drawer',0,8);
INSERT INTO properties VALUES(13,18,'ID','query-model-note','property_drawer',0,9);
INSERT INTO properties VALUES(14,18,'OWNER','Hubi','property_drawer',0,10);
INSERT INTO properties VALUES(15,21,'OWNER','Sam','property_drawer',0,29);
INSERT INTO properties VALUES(16,23,'CATEGORY','contacts','category_keyword',0,3);
INSERT INTO properties VALUES(17,24,'CUSTOM_ID','person-hubi','property_drawer',0,7);
INSERT INTO properties VALUES(18,24,'ROLE','Maintainer','property_drawer',0,8);
INSERT INTO properties VALUES(19,24,'TEAM','Core','property_drawer',0,9);
INSERT INTO properties VALUES(20,25,'CUSTOM_ID','person-alex','property_drawer',0,14);
INSERT INTO properties VALUES(21,25,'ROLE','Writer','property_drawer',0,15);
INSERT INTO properties VALUES(22,25,'TEAM','Docs','property_drawer',0,16);
INSERT INTO properties VALUES(23,26,'CUSTOM_ID','person-sam','property_drawer',0,21);
INSERT INTO properties VALUES(24,26,'ROLE','Reviewer','property_drawer',0,22);
INSERT INTO properties VALUES(25,26,'TEAM','Core','property_drawer',0,23);
INSERT INTO properties VALUES(26,31,'CATEGORY','projects','category_keyword',0,3);
INSERT INTO properties VALUES(27,31,'AREA','Engineering','property_keyword',0,4);
INSERT INTO properties VALUES(28,32,'ID','project-query-engine','property_drawer',0,9);
INSERT INTO properties VALUES(29,32,'CUSTOM_ID','query-engine','property_drawer',0,10);
INSERT INTO properties VALUES(30,32,'OWNER','Hubi','property_drawer',0,11);
INSERT INTO properties VALUES(31,32,'EFFORT','12:00','property_drawer',0,12);
INSERT INTO properties VALUES(32,33,'OWNER','Sam','property_drawer',0,26);
INSERT INTO properties VALUES(33,37,'OWNER','Alex','property_drawer',0,40);
CREATE TABLE tags (
    heading_id      INTEGER NOT NULL,
    tag             TEXT NOT NULL,
    FOREIGN KEY (heading_id)
        REFERENCES headings(id)
        ON DELETE CASCADE,
    PRIMARY KEY (heading_id, tag)
);
INSERT INTO tags VALUES(1,'dashboard');
INSERT INTO tags VALUES(1,'index');
INSERT INTO tags VALUES(6,'project');
INSERT INTO tags VALUES(6,'dashboard');
INSERT INTO tags VALUES(7,'docs');
INSERT INTO tags VALUES(8,'test');
INSERT INTO tags VALUES(9,'inbox');
INSERT INTO tags VALUES(10,'links');
INSERT INTO tags VALUES(11,'archive');
INSERT INTO tags VALUES(12,'legacy');
INSERT INTO tags VALUES(13,'experiment');
INSERT INTO tags VALUES(14,'test');
INSERT INTO tags VALUES(14,'duplicates');
INSERT INTO tags VALUES(17,'notes');
INSERT INTO tags VALUES(17,'reference');
INSERT INTO tags VALUES(18,'query');
INSERT INTO tags VALUES(18,'spec');
INSERT INTO tags VALUES(19,'time');
INSERT INTO tags VALUES(20,'links');
INSERT INTO tags VALUES(21,'sqlite');
INSERT INTO tags VALUES(21,'database');
INSERT INTO tags VALUES(23,'people');
INSERT INTO tags VALUES(24,'person');
INSERT INTO tags VALUES(24,'maintainer');
INSERT INTO tags VALUES(25,'person');
INSERT INTO tags VALUES(25,'docs');
INSERT INTO tags VALUES(26,'person');
INSERT INTO tags VALUES(26,'security');
INSERT INTO tags VALUES(31,'work');
INSERT INTO tags VALUES(31,'project');
INSERT INTO tags VALUES(32,'rust');
INSERT INTO tags VALUES(32,'sqlite');
INSERT INTO tags VALUES(33,'security');
INSERT INTO tags VALUES(34,'json');
INSERT INTO tags VALUES(35,'tree');
INSERT INTO tags VALUES(36,'parser');
INSERT INTO tags VALUES(37,'mobile');
INSERT INTO tags VALUES(38,'archive');
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
INSERT INTO links VALUES(1,1,1,124,155,7,'normal','bracket','[[file:projects.org][Projects]]','file:projects.org','Projects','file','projects.org',NULL,'/home/hubisan/projects/coding/org-files-db/.project/manual-tests/query-model/org/projects.org',7,31,NULL,NULL,'resolved',NULL);
INSERT INTO links VALUES(2,1,1,158,211,8,'normal','bracket','[[file:notes.org::*Query Model][Query model heading]]','file:notes.org::*Query Model','Query model heading','file','notes.org','*Query Model','/home/hubisan/projects/coding/org-files-db/.project/manual-tests/query-model/org/notes.org',4,18,NULL,NULL,'resolved',NULL);
INSERT INTO links VALUES(3,1,1,214,272,9,'normal','bracket','[[file:notes.org::*Does Not Exist][Broken heading target]]','file:notes.org::*Does Not Exist','Broken heading target','file','notes.org','*Does Not Exist','/home/hubisan/projects/coding/org-files-db/.project/manual-tests/query-model/org/notes.org',4,NULL,NULL,NULL,'broken','heading not found');
INSERT INTO links VALUES(4,1,1,275,314,10,'normal','bracket','[[file:not-indexed.org][Outside index]]','file:not-indexed.org','Outside index','file','not-indexed.org',NULL,'/home/hubisan/projects/coding/org-files-db/.project/manual-tests/query-model/org/not-indexed.org',NULL,NULL,NULL,NULL,'broken','missing in indexed universe');
INSERT INTO links VALUES(5,1,1,317,336,11,'normal','plain','https://example.com','https://example.com',NULL,'https','//example.com',NULL,NULL,NULL,NULL,NULL,NULL,'unsupported','unsupported link type');
INSERT INTO links VALUES(6,1,6,652,706,29,'normal','bracket','[[file:projects.org::*Query Engine][the query engine]]','file:projects.org::*Query Engine','the query engine','file','projects.org','*Query Engine','/home/hubisan/projects/coding/org-files-db/.project/manual-tests/query-model/org/projects.org',7,32,NULL,NULL,'resolved',NULL);
INSERT INTO links VALUES(7,1,6,711,746,29,'normal','bracket','[[id:project-query-engine][its ID]]','id:project-query-engine','its ID','id','project-query-engine',NULL,NULL,7,32,NULL,'project-query-engine','resolved',NULL);
INSERT INTO links VALUES(8,1,10,1085,1123,43,'normal','bracket','[[#review-query-cli][Local custom ID]]','#review-query-cli','Local custom ID','custom-id','review-query-cli',NULL,NULL,1,6,'review-query-cli',NULL,'resolved',NULL);
INSERT INTO links VALUES(9,1,10,1126,1164,44,'normal','bracket','[[id:duplicate-test-id][Ambiguous ID]]','id:duplicate-test-id','Ambiguous ID','id','duplicate-test-id',NULL,NULL,NULL,NULL,NULL,'duplicate-test-id','ambiguous','duplicate id');
INSERT INTO links VALUES(10,1,10,1167,1221,45,'normal','bracket','[[file:notes.org::42][Unsupported file search option]]','file:notes.org::42','Unsupported file search option','file','notes.org','42','/home/hubisan/projects/coding/org-files-db/.project/manual-tests/query-model/org/notes.org',4,NULL,NULL,NULL,'resolved',NULL);
INSERT INTO links VALUES(11,1,10,1224,1252,46,'normal','bracket','[[jira:ORG-42][Jira ticket]]','jira:ORG-42','Jira ticket','jira','ORG-42',NULL,NULL,NULL,NULL,NULL,NULL,'unsupported','unsupported link type');
INSERT INTO links VALUES(12,4,20,466,502,21,'normal','bracket','[[file:projects.org][Projects file]]','file:projects.org','Projects file','file','projects.org',NULL,'/home/hubisan/projects/coding/org-files-db/.project/manual-tests/query-model/org/projects.org',7,31,NULL,NULL,'resolved',NULL);
INSERT INTO links VALUES(13,4,20,505,563,22,'normal','bracket','[[file:projects.org::*Query Engine][Query Engine heading]]','file:projects.org::*Query Engine','Query Engine heading','file','projects.org','*Query Engine','/home/hubisan/projects/coding/org-files-db/.project/manual-tests/query-model/org/projects.org',7,32,NULL,NULL,'resolved',NULL);
INSERT INTO links VALUES(14,4,20,566,626,23,'normal','bracket','[[file:projects.org::#query-engine][Query Engine custom ID]]','file:projects.org::#query-engine','Query Engine custom ID','file','projects.org','#query-engine','/home/hubisan/projects/coding/org-files-db/.project/manual-tests/query-model/org/projects.org',7,32,'query-engine',NULL,'resolved',NULL);
INSERT INTO links VALUES(15,4,20,629,673,24,'normal','bracket','[[id:project-query-engine][Query Engine ID]]','id:project-query-engine','Query Engine ID','id','project-query-engine',NULL,NULL,7,32,NULL,'project-query-engine','resolved',NULL);
INSERT INTO links VALUES(16,4,20,676,739,25,'normal','bracket','[[file:projects.org::*Missing Project][Broken project heading]]','file:projects.org::*Missing Project','Broken project heading','file','projects.org','*Missing Project','/home/hubisan/projects/coding/org-files-db/.project/manual-tests/query-model/org/projects.org',7,NULL,NULL,NULL,'broken','heading not found');
INSERT INTO links VALUES(17,4,22,881,902,35,'normal','bracket','[[file:projects.org]]','file:projects.org',NULL,'file','projects.org',NULL,'/home/hubisan/projects/coding/org-files-db/.project/manual-tests/query-model/org/projects.org',7,31,NULL,NULL,'resolved',NULL);
INSERT INTO links VALUES(18,7,32,411,464,20,'normal','bracket','[[file:notes.org::*Query Model][Specification notes]]','file:notes.org::*Query Model','Specification notes','file','notes.org','*Query Model','/home/hubisan/projects/coding/org-files-db/.project/manual-tests/query-model/org/notes.org',4,18,NULL,NULL,'resolved',NULL);
INSERT INTO links VALUES(19,7,32,467,524,21,'normal','bracket','[[file:notes.org::#query-model][Specification custom ID]]','file:notes.org::#query-model','Specification custom ID','file','notes.org','#query-model','/home/hubisan/projects/coding/org-files-db/.project/manual-tests/query-model/org/notes.org',4,18,'query-model',NULL,'resolved',NULL);
INSERT INTO links VALUES(20,7,32,527,557,22,'normal','bracket','[[https://sqlite.org][SQLite]]','https://sqlite.org','SQLite','https','//sqlite.org',NULL,NULL,NULL,NULL,NULL,NULL,'unsupported','unsupported link type');
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
INSERT INTO outline_path VALUES(1,1,NULL,0,'0000','["Org Files Test Index"]');
INSERT INTO outline_path VALUES(2,1,1,1,'0000.0001','["Org Files Test Index","Statistic Cookies"]');
INSERT INTO outline_path VALUES(3,1,2,2,'0000.0001.0001','["Org Files Test Index","Statistic Cookies","test"]');
INSERT INTO outline_path VALUES(4,1,1,1,'0000.0002','["Org Files Test Index","Statistic Cookies"]');
INSERT INTO outline_path VALUES(5,1,1,1,'0000.0003','["Org Files Test Index","Review query CLI"]');
INSERT INTO outline_path VALUES(6,1,1,1,'0000.0004','["Org Files Test Index","Review query CLI"]');
INSERT INTO outline_path VALUES(7,1,6,2,'0000.0004.0001','["Org Files Test Index","Review query CLI","Add documentation examples"]');
INSERT INTO outline_path VALUES(8,1,6,2,'0000.0004.0002','["Org Files Test Index","Review query CLI","Verify JSON output"]');
INSERT INTO outline_path VALUES(9,1,1,1,'0000.0005','["Org Files Test Index","Inbox item"]');
INSERT INTO outline_path VALUES(10,1,1,1,'0000.0006','["Org Files Test Index","Reference links"]');
INSERT INTO outline_path VALUES(11,2,NULL,0,'0000','["Archive 2025"]');
INSERT INTO outline_path VALUES(12,2,11,1,'0000.0001','["Archive 2025","Legacy migration"]');
INSERT INTO outline_path VALUES(13,2,11,1,'0000.0002','["Archive 2025","Obsolete experiment"]');
INSERT INTO outline_path VALUES(14,3,NULL,0,'0000','["Duplicate IDs"]');
INSERT INTO outline_path VALUES(15,3,14,1,'0000.0001','["Duplicate IDs","First duplicate"]');
INSERT INTO outline_path VALUES(16,3,14,1,'0000.0002','["Duplicate IDs","Second duplicate"]');
INSERT INTO outline_path VALUES(17,4,NULL,0,'0000','["Technical Notes"]');
INSERT INTO outline_path VALUES(18,4,17,1,'0000.0001','["Technical Notes","Query Model"]');
INSERT INTO outline_path VALUES(19,4,18,2,'0000.0001.0001','["Technical Notes","Query Model","Timestamp semantics"]');
INSERT INTO outline_path VALUES(20,4,18,2,'0000.0001.0002','["Technical Notes","Query Model","Link semantics"]');
INSERT INTO outline_path VALUES(21,4,17,1,'0000.0002','["Technical Notes","SQLite Notes"]');
INSERT INTO outline_path VALUES(22,4,17,1,'0000.0003','["Technical Notes","Empty Description Link"]');
INSERT INTO outline_path VALUES(23,5,NULL,0,'0000','["People"]');
INSERT INTO outline_path VALUES(24,5,23,1,'0000.0001','["People","Hubi"]');
INSERT INTO outline_path VALUES(25,5,23,1,'0000.0002','["People","Alex"]');
INSERT INTO outline_path VALUES(26,5,23,1,'0000.0003','["People","Sam"]');
INSERT INTO outline_path VALUES(27,6,NULL,0,'0000','["prio"]');
INSERT INTO outline_path VALUES(28,6,27,1,'0000.0001','["prio","Priority Test"]');
INSERT INTO outline_path VALUES(29,6,27,1,'0000.0002','["prio","Priority Test"]');
INSERT INTO outline_path VALUES(30,6,27,1,'0000.0003','["prio","Priority Test"]');
INSERT INTO outline_path VALUES(31,7,NULL,0,'0000','["Projects"]');
INSERT INTO outline_path VALUES(32,7,31,1,'0000.0001','["Projects","Query Engine"]');
INSERT INTO outline_path VALUES(33,7,32,2,'0000.0001.0001','["Projects","Query Engine","Safe SQL translation"]');
INSERT INTO outline_path VALUES(34,7,32,2,'0000.0001.0002','["Projects","Query Engine","Result shaping"]');
INSERT INTO outline_path VALUES(35,7,34,3,'0000.0001.0002.0001','["Projects","Query Engine","Result shaping","Outline context nodes"]');
INSERT INTO outline_path VALUES(36,7,32,2,'0000.0001.0003','["Projects","Query Engine","AST parser"]');
INSERT INTO outline_path VALUES(37,7,31,1,'0000.0002','["Projects","Mobile integration"]');
INSERT INTO outline_path VALUES(38,7,31,1,'0000.0003','["Projects","Old prototype"]');
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
