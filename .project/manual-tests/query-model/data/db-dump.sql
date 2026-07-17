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
INSERT INTO files VALUES(1,'/home/hubisan/projects/coding/org-files-db/.project/manual-tests/query-model/org/properties.org',1784301238008799537,15518,NULL,1784313560);
INSERT INTO files VALUES(2,'/home/hubisan/projects/coding/org-files-db/.project/manual-tests/query-model/org/todo-keywords.org',1784242830730251043,0,NULL,1784313560);
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
INSERT INTO headings VALUES(1,1,NULL,0,1,-1,15518,'Org Property and Keyword Test','Org Property and Keyword Test',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(2,1,1,1,38,1065,1115,'Empty Property','Empty Property',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(3,1,1,1,43,1115,2674,'Expected file/root values','Expected file/root values',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(4,1,1,1,73,2674,3084,'Local append on the same heading','Local append on the same heading',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(5,1,1,1,87,3084,3422,'Multiple local append rows','Multiple local append rows',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(6,1,1,1,101,3422,3839,'Local append without a base value','Local append without a base value',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(7,1,1,1,114,3839,4251,'Later local definition replaces the earlier definition','Later local definition replaces the earlier definition',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(8,1,1,1,129,4251,4759,'Local append followed by replacement','Local append followed by replacement',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(9,1,1,1,144,4759,5062,'Empty base followed by append','Empty base followed by append',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(10,1,1,1,157,5062,5366,'Base followed by empty append','Base followed by empty append',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(11,1,1,1,170,5366,6824,'Parent append inheritance','Parent append inheritance',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(12,1,11,2,175,5429,5813,'Child with append only','Child with append only',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(13,1,11,2,188,5813,6047,'Child without local value','Child without local value',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(14,1,11,2,197,6047,6407,'Child with local replacement','Child with local replacement',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(15,1,11,2,210,6407,6824,'Child with local replacement and append','Child with local replacement and append',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(16,1,1,1,224,6824,7854,'Parent with appended effective value','Parent with appended effective value',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(17,1,16,2,230,6916,7162,'Inheriting child','Inheriting child',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(18,1,16,2,239,7162,7517,'Child overriding appended parent value','Child overriding appended parent value',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(19,1,16,2,252,7517,7854,'Child appending to appended parent value','Child appending to appended parent value',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(20,1,1,1,264,7854,8893,'Nearest ancestor wins','Nearest ancestor wins',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(21,1,20,2,269,7918,8893,'Parent override','Parent override',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(22,1,21,3,274,7972,8283,'Child inheriting nearest value','Child inheriting nearest value',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(23,1,21,3,284,8283,8597,'Child appending to nearest value','Child appending to nearest value',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(24,1,21,3,296,8597,8893,'Child replacing nearest value','Child replacing nearest value',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(25,1,1,1,308,8893,9937,'Root drawer inheritance','Root drawer inheritance',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(26,1,25,2,310,8920,9210,'Child inheriting root drawer base','Child inheriting root drawer base',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(27,1,25,2,319,9210,9580,'Child appending to root drawer base','Child appending to root drawer base',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(28,1,25,2,331,9580,9937,'Child replacing root drawer base','Child replacing root drawer base',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(29,1,1,1,343,9937,11477,'File keyword inheritance','File keyword inheritance',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(30,1,29,2,345,9965,10276,'Child inheriting appended keyword','Child inheriting appended keyword',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(31,1,29,2,354,10276,10715,'Child appending to file keyword','Child appending to file keyword',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(32,1,29,2,366,10715,11060,'Child replacing file keyword','Child replacing file keyword',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(33,1,29,2,378,11060,11477,'Child replacing and appending file keyword','Child replacing and appending file keyword',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(34,1,1,1,391,11477,11778,'Append before','Append before',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(35,1,1,1,403,11778,12373,'Duplicate property definitions in one drawer','Duplicate property definitions in one drawer',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(36,1,1,1,419,12373,12678,'Duplicate definition followed by append','Duplicate definition followed by append',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(37,1,1,1,432,12678,13278,'Append followed by duplicate replacement','Append followed by duplicate replacement',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(38,1,1,1,450,13278,13584,'Append followed by duplicate replacement','Append followed by duplicate replacement',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(39,1,1,1,463,13584,14236,'Parent duplicate definition','Parent duplicate definition',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(40,1,39,2,469,13663,13914,'Child inheriting duplicate parent value','Child inheriting duplicate parent value',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(41,1,39,2,478,13914,14236,'Child appending to duplicate parent value','Child appending to duplicate parent value',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(42,1,1,1,490,14236,14726,'Parent definition overridden by child duplicate definitions','Parent definition overridden by child duplicate definitions',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(43,1,42,2,495,14333,14726,'Child with two local definitions','Child with two local definitions',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(44,1,1,1,509,14726,15099,'Independent property keys','Independent property keys',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(45,1,1,1,525,15099,15518,'Mixed-case property keys','Mixed-case property keys',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(46,2,NULL,0,1,-1,0,'todo-keywords',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
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
INSERT INTO keywords VALUES(1,1,'TITLE','Org Property and Keyword Test',20);
INSERT INTO keywords VALUES(2,1,'STARTUP','showall',21);
INSERT INTO keywords VALUES(3,1,'CATEGORY','category_keyword_value',22);
INSERT INTO keywords VALUES(4,1,'PROPERTY','KEYWORD_APPEND foo=1',23);
INSERT INTO keywords VALUES(5,1,'PROPERTY','KEYWORD_APPEND+ bar=2',24);
INSERT INTO keywords VALUES(6,1,'PROPERTY','KEYWORD_APPEND+ baz=3',25);
INSERT INTO keywords VALUES(7,1,'PROPERTY','KEYWORD_DUPLICATE first',26);
INSERT INTO keywords VALUES(8,1,'PROPERTY','KEYWORD_DUPLICATE second',27);
INSERT INTO keywords VALUES(9,1,'PROPERTY','KEYWORD_RESET old',28);
INSERT INTO keywords VALUES(10,1,'PROPERTY','KEYWORD_RESET+ appended-before-reset',29);
INSERT INTO keywords VALUES(11,1,'PROPERTY','KEYWORD_RESET replacement',30);
INSERT INTO keywords VALUES(12,1,'PROPERTY','KEYWORD_RESET+ appended-after-reset',31);
INSERT INTO keywords VALUES(13,1,'PROPERTY','KEYWORD_APPEND_ONLY only',32);
INSERT INTO keywords VALUES(14,1,'PROPERTY','KEYWORD_APPEND_ONLY+ appended',33);
INSERT INTO keywords VALUES(15,1,'PROPERTY','KEYWORD_EMPTY',34);
INSERT INTO keywords VALUES(16,1,'PROPERTY','KEYWORD_EMPTY+ valid',35);
INSERT INTO keywords VALUES(17,1,'PROPERTY','KEYWORD_VERY_EMPTY',36);
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
INSERT INTO properties VALUES(3,1,'OVERWRITE','this one works','property_drawer',0,4);
INSERT INTO properties VALUES(4,1,'ID','7dad9b62-a3cc-43ec-a60f-e650bdaeae6d','property_drawer',0,5);
INSERT INTO properties VALUES(5,1,'ROOT_DRAWER_BASE','root','property_drawer',0,6);
INSERT INTO properties VALUES(6,1,'ROOT_DRAWER_APPEND','root','property_drawer',0,7);
INSERT INTO properties VALUES(7,1,'ROOT_DRAWER_APPEND','appended','property_drawer',1,8);
INSERT INTO properties VALUES(8,1,'ROOT_DRAWER_DUPLICATE','first','property_drawer',0,9);
INSERT INTO properties VALUES(9,1,'ROOT_DRAWER_DUPLICATE','second','property_drawer',0,10);
INSERT INTO properties VALUES(10,1,'ROOT_OVERRIDE_CHAIN','this','property_drawer',0,11);
INSERT INTO properties VALUES(11,1,'ROOT_OVERRIDE_CHAIN','is','property_drawer',0,12);
INSERT INTO properties VALUES(12,1,'ROOT_OVERRIDE_CHAIN','the','property_drawer',0,13);
INSERT INTO properties VALUES(13,1,'ROOT_OVERRIDE_CHAIN','root','property_drawer',0,14);
INSERT INTO properties VALUES(14,1,'TEST','old','property_drawer',0,15);
INSERT INTO properties VALUES(15,1,'TEST','append 1','property_drawer',1,16);
INSERT INTO properties VALUES(16,1,'TEST','new','property_drawer',0,17);
INSERT INTO properties VALUES(17,1,'TEST','append 2','property_drawer',1,18);
INSERT INTO properties VALUES(18,1,'CATEGORY','category_keyword_value','category_keyword',0,22);
INSERT INTO properties VALUES(19,1,'KEYWORD_APPEND','foo=1','property_keyword',0,23);
INSERT INTO properties VALUES(20,1,'KEYWORD_APPEND','bar=2','property_keyword',1,24);
INSERT INTO properties VALUES(21,1,'KEYWORD_APPEND','baz=3','property_keyword',1,25);
INSERT INTO properties VALUES(22,1,'KEYWORD_DUPLICATE','first','property_keyword',0,26);
INSERT INTO properties VALUES(23,1,'KEYWORD_DUPLICATE','second','property_keyword',0,27);
INSERT INTO properties VALUES(24,1,'KEYWORD_RESET','old','property_keyword',0,28);
INSERT INTO properties VALUES(25,1,'KEYWORD_RESET','appended-before-reset','property_keyword',1,29);
INSERT INTO properties VALUES(26,1,'KEYWORD_RESET','replacement','property_keyword',0,30);
INSERT INTO properties VALUES(27,1,'KEYWORD_RESET','appended-after-reset','property_keyword',1,31);
INSERT INTO properties VALUES(28,1,'KEYWORD_APPEND_ONLY','only','property_keyword',0,32);
INSERT INTO properties VALUES(29,1,'KEYWORD_APPEND_ONLY','appended','property_keyword',1,33);
INSERT INTO properties VALUES(30,1,'KEYWORD_EMPTY',NULL,'property_keyword',0,34);
INSERT INTO properties VALUES(31,1,'KEYWORD_EMPTY','valid','property_keyword',1,35);
INSERT INTO properties VALUES(32,1,'KEYWORD_VERY_EMPTY',NULL,'property_keyword',0,36);
INSERT INTO properties VALUES(33,2,'EMPTY_PROP','','property_drawer',0,40);
INSERT INTO properties VALUES(34,4,'VALUE','first','property_drawer',0,75);
INSERT INTO properties VALUES(35,4,'VALUE','second','property_drawer',1,76);
INSERT INTO properties VALUES(36,5,'VALUE','base','property_drawer',0,89);
INSERT INTO properties VALUES(37,5,'VALUE','one','property_drawer',1,90);
INSERT INTO properties VALUES(38,5,'VALUE','two','property_drawer',1,91);
INSERT INTO properties VALUES(39,6,'VALUE','only','property_drawer',1,103);
INSERT INTO properties VALUES(40,7,'VALUE','first','property_drawer',0,116);
INSERT INTO properties VALUES(41,7,'VALUE','second','property_drawer',0,117);
INSERT INTO properties VALUES(42,8,'VALUE','old','property_drawer',0,131);
INSERT INTO properties VALUES(43,8,'VALUE','appended-before-reset','property_drawer',1,132);
INSERT INTO properties VALUES(44,8,'VALUE','replacement','property_drawer',0,133);
INSERT INTO properties VALUES(45,8,'VALUE','appended-after-reset','property_drawer',1,134);
INSERT INTO properties VALUES(46,9,'VALUE','','property_drawer',0,146);
INSERT INTO properties VALUES(47,9,'VALUE','empty base followed by append','property_drawer',1,147);
INSERT INTO properties VALUES(48,10,'VALUE','base followed by empty append','property_drawer',0,159);
INSERT INTO properties VALUES(49,10,'VALUE','','property_drawer',1,160);
INSERT INTO properties VALUES(50,11,'VALUE','parent','property_drawer',0,172);
INSERT INTO properties VALUES(51,12,'VALUE','child','property_drawer',1,177);
INSERT INTO properties VALUES(52,14,'VALUE','child','property_drawer',0,199);
INSERT INTO properties VALUES(53,15,'VALUE','child','property_drawer',0,212);
INSERT INTO properties VALUES(54,15,'VALUE','appended','property_drawer',1,213);
INSERT INTO properties VALUES(55,16,'VALUE','parent','property_drawer',0,226);
INSERT INTO properties VALUES(56,16,'VALUE','appended','property_drawer',1,227);
INSERT INTO properties VALUES(57,18,'VALUE','child','property_drawer',0,241);
INSERT INTO properties VALUES(58,19,'VALUE','child','property_drawer',1,254);
INSERT INTO properties VALUES(59,20,'VALUE','grandparent','property_drawer',0,266);
INSERT INTO properties VALUES(60,21,'VALUE','parent','property_drawer',0,271);
INSERT INTO properties VALUES(61,23,'VALUE','child','property_drawer',1,286);
INSERT INTO properties VALUES(62,24,'VALUE','child','property_drawer',0,298);
INSERT INTO properties VALUES(63,27,'ROOT_DRAWER_BASE','child','property_drawer',1,321);
INSERT INTO properties VALUES(64,28,'ROOT_DRAWER_BASE','child','property_drawer',0,333);
INSERT INTO properties VALUES(65,31,'KEYWORD_APPEND','child=4','property_drawer',1,356);
INSERT INTO properties VALUES(66,32,'KEYWORD_APPEND','local=1','property_drawer',0,368);
INSERT INTO properties VALUES(67,33,'KEYWORD_APPEND','local=1','property_drawer',0,380);
INSERT INTO properties VALUES(68,33,'KEYWORD_APPEND','local=2','property_drawer',1,381);
INSERT INTO properties VALUES(69,34,'VALUE','appending before','property_drawer',1,393);
INSERT INTO properties VALUES(70,34,'VALUE','definition','property_drawer',0,394);
INSERT INTO properties VALUES(71,35,'DEFINED_TWICE','works','property_drawer',0,405);
INSERT INTO properties VALUES(72,35,'DEFINED_TWICE','second is effective','property_drawer',0,406);
INSERT INTO properties VALUES(73,36,'VALUE','first','property_drawer',0,421);
INSERT INTO properties VALUES(74,36,'VALUE','second','property_drawer',0,422);
INSERT INTO properties VALUES(75,36,'VALUE','appended','property_drawer',1,423);
INSERT INTO properties VALUES(76,37,'VALUE','first','property_drawer',0,434);
INSERT INTO properties VALUES(77,37,'VALUE','appended','property_drawer',1,435);
INSERT INTO properties VALUES(78,37,'VALUE','another','property_drawer',1,436);
INSERT INTO properties VALUES(79,37,'VALUE','second','property_drawer',0,437);
INSERT INTO properties VALUES(80,37,'VALUE','value','property_drawer',1,438);
INSERT INTO properties VALUES(81,38,'VALUE','first','property_drawer',0,452);
INSERT INTO properties VALUES(82,38,'VALUE','appended','property_drawer',1,453);
INSERT INTO properties VALUES(83,38,'VALUE','second','property_drawer',0,454);
INSERT INTO properties VALUES(84,39,'VALUE','first','property_drawer',0,465);
INSERT INTO properties VALUES(85,39,'VALUE','second','property_drawer',0,466);
INSERT INTO properties VALUES(86,41,'VALUE','child','property_drawer',1,480);
INSERT INTO properties VALUES(87,42,'VALUE','parent','property_drawer',0,492);
INSERT INTO properties VALUES(88,43,'VALUE','first child','property_drawer',0,497);
INSERT INTO properties VALUES(89,43,'VALUE','second child','property_drawer',0,498);
INSERT INTO properties VALUES(90,44,'VALUE_A','one','property_drawer',0,511);
INSERT INTO properties VALUES(91,44,'VALUE_A','two','property_drawer',1,512);
INSERT INTO properties VALUES(92,44,'VALUE_B','first','property_drawer',0,513);
INSERT INTO properties VALUES(93,44,'VALUE_B','second','property_drawer',0,514);
INSERT INTO properties VALUES(94,45,'MIXED_KEY','first','property_drawer',0,527);
INSERT INTO properties VALUES(95,45,'MIXED_KEY','second','property_drawer',1,528);
INSERT INTO properties VALUES(96,45,'MIXED_KEY','replacement','property_drawer',0,529);
INSERT INTO properties VALUES(97,45,'MIXED_KEY','final','property_drawer',1,530);
CREATE TABLE tags (
    heading_id      INTEGER NOT NULL,
    tag             TEXT NOT NULL,
    FOREIGN KEY (heading_id)
        REFERENCES headings(id)
        ON DELETE CASCADE,
    PRIMARY KEY (heading_id, tag)
);
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
INSERT INTO outline_path VALUES(2,1,1,1,'0000.0001','["Org Property and Keyword Test","Empty Property"]');
INSERT INTO outline_path VALUES(3,1,1,1,'0000.0002','["Org Property and Keyword Test","Expected file/root values"]');
INSERT INTO outline_path VALUES(4,1,1,1,'0000.0003','["Org Property and Keyword Test","Local append on the same heading"]');
INSERT INTO outline_path VALUES(5,1,1,1,'0000.0004','["Org Property and Keyword Test","Multiple local append rows"]');
INSERT INTO outline_path VALUES(6,1,1,1,'0000.0005','["Org Property and Keyword Test","Local append without a base value"]');
INSERT INTO outline_path VALUES(7,1,1,1,'0000.0006','["Org Property and Keyword Test","Later local definition replaces the earlier definition"]');
INSERT INTO outline_path VALUES(8,1,1,1,'0000.0007','["Org Property and Keyword Test","Local append followed by replacement"]');
INSERT INTO outline_path VALUES(9,1,1,1,'0000.0008','["Org Property and Keyword Test","Empty base followed by append"]');
INSERT INTO outline_path VALUES(10,1,1,1,'0000.0009','["Org Property and Keyword Test","Base followed by empty append"]');
INSERT INTO outline_path VALUES(11,1,1,1,'0000.0010','["Org Property and Keyword Test","Parent append inheritance"]');
INSERT INTO outline_path VALUES(12,1,11,2,'0000.0010.0001','["Org Property and Keyword Test","Parent append inheritance","Child with append only"]');
INSERT INTO outline_path VALUES(13,1,11,2,'0000.0010.0002','["Org Property and Keyword Test","Parent append inheritance","Child without local value"]');
INSERT INTO outline_path VALUES(14,1,11,2,'0000.0010.0003','["Org Property and Keyword Test","Parent append inheritance","Child with local replacement"]');
INSERT INTO outline_path VALUES(15,1,11,2,'0000.0010.0004','["Org Property and Keyword Test","Parent append inheritance","Child with local replacement and append"]');
INSERT INTO outline_path VALUES(16,1,1,1,'0000.0011','["Org Property and Keyword Test","Parent with appended effective value"]');
INSERT INTO outline_path VALUES(17,1,16,2,'0000.0011.0001','["Org Property and Keyword Test","Parent with appended effective value","Inheriting child"]');
INSERT INTO outline_path VALUES(18,1,16,2,'0000.0011.0002','["Org Property and Keyword Test","Parent with appended effective value","Child overriding appended parent value"]');
INSERT INTO outline_path VALUES(19,1,16,2,'0000.0011.0003','["Org Property and Keyword Test","Parent with appended effective value","Child appending to appended parent value"]');
INSERT INTO outline_path VALUES(20,1,1,1,'0000.0012','["Org Property and Keyword Test","Nearest ancestor wins"]');
INSERT INTO outline_path VALUES(21,1,20,2,'0000.0012.0001','["Org Property and Keyword Test","Nearest ancestor wins","Parent override"]');
INSERT INTO outline_path VALUES(22,1,21,3,'0000.0012.0001.0001','["Org Property and Keyword Test","Nearest ancestor wins","Parent override","Child inheriting nearest value"]');
INSERT INTO outline_path VALUES(23,1,21,3,'0000.0012.0001.0002','["Org Property and Keyword Test","Nearest ancestor wins","Parent override","Child appending to nearest value"]');
INSERT INTO outline_path VALUES(24,1,21,3,'0000.0012.0001.0003','["Org Property and Keyword Test","Nearest ancestor wins","Parent override","Child replacing nearest value"]');
INSERT INTO outline_path VALUES(25,1,1,1,'0000.0013','["Org Property and Keyword Test","Root drawer inheritance"]');
INSERT INTO outline_path VALUES(26,1,25,2,'0000.0013.0001','["Org Property and Keyword Test","Root drawer inheritance","Child inheriting root drawer base"]');
INSERT INTO outline_path VALUES(27,1,25,2,'0000.0013.0002','["Org Property and Keyword Test","Root drawer inheritance","Child appending to root drawer base"]');
INSERT INTO outline_path VALUES(28,1,25,2,'0000.0013.0003','["Org Property and Keyword Test","Root drawer inheritance","Child replacing root drawer base"]');
INSERT INTO outline_path VALUES(29,1,1,1,'0000.0014','["Org Property and Keyword Test","File keyword inheritance"]');
INSERT INTO outline_path VALUES(30,1,29,2,'0000.0014.0001','["Org Property and Keyword Test","File keyword inheritance","Child inheriting appended keyword"]');
INSERT INTO outline_path VALUES(31,1,29,2,'0000.0014.0002','["Org Property and Keyword Test","File keyword inheritance","Child appending to file keyword"]');
INSERT INTO outline_path VALUES(32,1,29,2,'0000.0014.0003','["Org Property and Keyword Test","File keyword inheritance","Child replacing file keyword"]');
INSERT INTO outline_path VALUES(33,1,29,2,'0000.0014.0004','["Org Property and Keyword Test","File keyword inheritance","Child replacing and appending file keyword"]');
INSERT INTO outline_path VALUES(34,1,1,1,'0000.0015','["Org Property and Keyword Test","Append before"]');
INSERT INTO outline_path VALUES(35,1,1,1,'0000.0016','["Org Property and Keyword Test","Duplicate property definitions in one drawer"]');
INSERT INTO outline_path VALUES(36,1,1,1,'0000.0017','["Org Property and Keyword Test","Duplicate definition followed by append"]');
INSERT INTO outline_path VALUES(37,1,1,1,'0000.0018','["Org Property and Keyword Test","Append followed by duplicate replacement"]');
INSERT INTO outline_path VALUES(38,1,1,1,'0000.0019','["Org Property and Keyword Test","Append followed by duplicate replacement"]');
INSERT INTO outline_path VALUES(39,1,1,1,'0000.0020','["Org Property and Keyword Test","Parent duplicate definition"]');
INSERT INTO outline_path VALUES(40,1,39,2,'0000.0020.0001','["Org Property and Keyword Test","Parent duplicate definition","Child inheriting duplicate parent value"]');
INSERT INTO outline_path VALUES(41,1,39,2,'0000.0020.0002','["Org Property and Keyword Test","Parent duplicate definition","Child appending to duplicate parent value"]');
INSERT INTO outline_path VALUES(42,1,1,1,'0000.0021','["Org Property and Keyword Test","Parent definition overridden by child duplicate definitions"]');
INSERT INTO outline_path VALUES(43,1,42,2,'0000.0021.0001','["Org Property and Keyword Test","Parent definition overridden by child duplicate definitions","Child with two local definitions"]');
INSERT INTO outline_path VALUES(44,1,1,1,'0000.0022','["Org Property and Keyword Test","Independent property keys"]');
INSERT INTO outline_path VALUES(45,1,1,1,'0000.0023','["Org Property and Keyword Test","Mixed-case property keys"]');
INSERT INTO outline_path VALUES(46,2,NULL,0,'0000','["todo-keywords"]');
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
