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
INSERT INTO files VALUES(1,'/home/hubisan/projects/coding/org-files-db/.project/manual-tests/query-model/org/headings/ancestors.org',1782986400000000000,363,NULL,1784477089);
INSERT INTO files VALUES(2,'/home/hubisan/projects/coding/org-files-db/.project/manual-tests/query-model/org/headings/children.org',1782986400000000000,399,NULL,1784477089);
INSERT INTO files VALUES(3,'/home/hubisan/projects/coding/org-files-db/.project/manual-tests/query-model/org/headings/closed.org',1782986400000000000,396,NULL,1784477089);
INSERT INTO files VALUES(4,'/home/hubisan/projects/coding/org-files-db/.project/manual-tests/query-model/org/headings/deadline.org',1782986400000000000,416,NULL,1784477089);
INSERT INTO files VALUES(5,'/home/hubisan/projects/coding/org-files-db/.project/manual-tests/query-model/org/headings/descendants.org',1782986400000000000,474,NULL,1784477089);
INSERT INTO files VALUES(6,'/home/hubisan/projects/coding/org-files-db/.project/manual-tests/query-model/org/headings/done.org',1782986400000000000,243,NULL,1784477089);
INSERT INTO files VALUES(7,'/home/hubisan/projects/coding/org-files-db/.project/manual-tests/query-model/org/headings/file-modified.org',1782900000000000000,151,NULL,1784477089);
INSERT INTO files VALUES(8,'/home/hubisan/projects/coding/org-files-db/.project/manual-tests/query-model/org/headings/file-name.org',1782986400000000000,129,NULL,1784477089);
INSERT INTO files VALUES(9,'/home/hubisan/projects/coding/org-files-db/.project/manual-tests/query-model/org/headings/file-title.org',1782986400000000000,155,NULL,1784477089);
INSERT INTO files VALUES(10,'/home/hubisan/projects/coding/org-files-db/.project/manual-tests/query-model/org/headings/has-link.org',1782986400000000000,465,NULL,1784477089);
INSERT INTO files VALUES(11,'/home/hubisan/projects/coding/org-files-db/.project/manual-tests/query-model/org/headings/has-text.org',1784363795131232051,226,NULL,1784477089);
INSERT INTO files VALUES(12,'/home/hubisan/projects/coding/org-files-db/.project/manual-tests/query-model/org/headings/keyword.org',1782986400000000000,215,NULL,1784477089);
INSERT INTO files VALUES(13,'/home/hubisan/projects/coding/org-files-db/.project/manual-tests/query-model/org/headings/level.org',1782986400000000000,213,NULL,1784477089);
INSERT INTO files VALUES(14,'/home/hubisan/projects/coding/org-files-db/.project/manual-tests/query-model/org/headings/linked-from.org',1782986400000000000,576,NULL,1784477089);
INSERT INTO files VALUES(15,'/home/hubisan/projects/coding/org-files-db/.project/manual-tests/query-model/org/headings/links-to.org',1782986400000000000,574,NULL,1784477089);
INSERT INTO files VALUES(16,'/home/hubisan/projects/coding/org-files-db/.project/manual-tests/query-model/org/headings/multipe-title-keywords.org',1781826194942471483,321,NULL,1784477089);
INSERT INTO files VALUES(17,'/home/hubisan/projects/coding/org-files-db/.project/manual-tests/query-model/org/headings/no-title-set.org',1783850095870831478,44,NULL,1784477089);
INSERT INTO files VALUES(18,'/home/hubisan/projects/coding/org-files-db/.project/manual-tests/query-model/org/headings/outline-contains.org',1782986400000000000,403,NULL,1784477089);
INSERT INTO files VALUES(19,'/home/hubisan/projects/coding/org-files-db/.project/manual-tests/query-model/org/headings/outline-sequence.org',1782986400000000000,427,NULL,1784477089);
INSERT INTO files VALUES(20,'/home/hubisan/projects/coding/org-files-db/.project/manual-tests/query-model/org/headings/parent.org',1782986400000000000,374,NULL,1784477089);
INSERT INTO files VALUES(21,'/home/hubisan/projects/coding/org-files-db/.project/manual-tests/query-model/org/headings/path-zone/deep-zone/file-dir.org',1782986400000000000,128,NULL,1784477089);
INSERT INTO files VALUES(22,'/home/hubisan/projects/coding/org-files-db/.project/manual-tests/query-model/org/headings/path-zone/file-path.org',1782986400000000000,121,NULL,1784477089);
INSERT INTO files VALUES(23,'/home/hubisan/projects/coding/org-files-db/.project/manual-tests/query-model/org/headings/planning.org',1782986400000000000,510,NULL,1784477089);
INSERT INTO files VALUES(24,'/home/hubisan/projects/coding/org-files-db/.project/manual-tests/query-model/org/headings/priority.org',1784455889916262025,340,NULL,1784477089);
INSERT INTO files VALUES(25,'/home/hubisan/projects/coding/org-files-db/.project/manual-tests/query-model/org/headings/properties.org',1784317456755088878,15145,NULL,1784477089);
INSERT INTO files VALUES(26,'/home/hubisan/projects/coding/org-files-db/.project/manual-tests/query-model/org/headings/scheduled.org',1782986400000000000,424,NULL,1784477089);
INSERT INTO files VALUES(27,'/home/hubisan/projects/coding/org-files-db/.project/manual-tests/query-model/org/headings/tags.org',1784460355904105294,1896,NULL,1784477089);
INSERT INTO files VALUES(28,'/home/hubisan/projects/coding/org-files-db/.project/manual-tests/query-model/org/headings/title.org',1784332287046809796,115,NULL,1784477089);
INSERT INTO files VALUES(29,'/home/hubisan/projects/coding/org-files-db/.project/manual-tests/query-model/org/headings/todo-keywords-file-local.org',1784331445500650156,1118,NULL,1784477089);
INSERT INTO files VALUES(30,'/home/hubisan/projects/coding/org-files-db/.project/manual-tests/query-model/org/headings/todo-keywords.org',1784331431167729427,245,NULL,1784477089);
INSERT INTO files VALUES(31,'/home/hubisan/projects/coding/org-files-db/.project/manual-tests/query-model/org/headings/ts-active.org',1782986400000000000,321,NULL,1784477089);
INSERT INTO files VALUES(32,'/home/hubisan/projects/coding/org-files-db/.project/manual-tests/query-model/org/headings/ts-inactive.org',1782986400000000000,331,NULL,1784477089);
INSERT INTO files VALUES(33,'/home/hubisan/projects/coding/org-files-db/.project/manual-tests/query-model/org/headings/ts.org',1782986400000000000,402,NULL,1784477089);
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
INSERT INTO todo_keywords VALUES(2,'TODO','open',NULL,0,'org_keyword','TODO',3);
INSERT INTO todo_keywords VALUES(2,'NEXT','open',NULL,1,'org_keyword','TODO',3);
INSERT INTO todo_keywords VALUES(2,'DONE','closed',NULL,2,'org_keyword','TODO',3);
INSERT INTO todo_keywords VALUES(3,'TODO','open',NULL,0,'org_keyword','TODO',3);
INSERT INTO todo_keywords VALUES(3,'DONE','closed',NULL,1,'org_keyword','TODO',3);
INSERT INTO todo_keywords VALUES(4,'TODO','open',NULL,0,'org_keyword','TODO',3);
INSERT INTO todo_keywords VALUES(4,'DONE','closed',NULL,1,'org_keyword','TODO',3);
INSERT INTO todo_keywords VALUES(5,'TODO','open',NULL,0,'org_keyword','TODO',3);
INSERT INTO todo_keywords VALUES(5,'NEXT','open',NULL,1,'org_keyword','TODO',3);
INSERT INTO todo_keywords VALUES(5,'DONE','closed',NULL,2,'org_keyword','TODO',3);
INSERT INTO todo_keywords VALUES(6,'TODO','open',NULL,0,'org_keyword','TODO',3);
INSERT INTO todo_keywords VALUES(6,'NEXT','open',NULL,1,'org_keyword','TODO',3);
INSERT INTO todo_keywords VALUES(6,'DONE','closed',NULL,2,'org_keyword','TODO',3);
INSERT INTO todo_keywords VALUES(6,'CANCELLED','closed',NULL,3,'org_keyword','TODO',3);
INSERT INTO todo_keywords VALUES(7,'TODO','open',NULL,0,'config_default',NULL,NULL);
INSERT INTO todo_keywords VALUES(7,'NEXT','open',NULL,1,'config_default',NULL,NULL);
INSERT INTO todo_keywords VALUES(7,'DONE','closed',NULL,2,'config_default',NULL,NULL);
INSERT INTO todo_keywords VALUES(7,'CANCEL','closed',NULL,3,'config_default',NULL,NULL);
INSERT INTO todo_keywords VALUES(8,'TODO','open',NULL,0,'config_default',NULL,NULL);
INSERT INTO todo_keywords VALUES(8,'NEXT','open',NULL,1,'config_default',NULL,NULL);
INSERT INTO todo_keywords VALUES(8,'DONE','closed',NULL,2,'config_default',NULL,NULL);
INSERT INTO todo_keywords VALUES(8,'CANCEL','closed',NULL,3,'config_default',NULL,NULL);
INSERT INTO todo_keywords VALUES(9,'TODO','open',NULL,0,'config_default',NULL,NULL);
INSERT INTO todo_keywords VALUES(9,'NEXT','open',NULL,1,'config_default',NULL,NULL);
INSERT INTO todo_keywords VALUES(9,'DONE','closed',NULL,2,'config_default',NULL,NULL);
INSERT INTO todo_keywords VALUES(9,'CANCEL','closed',NULL,3,'config_default',NULL,NULL);
INSERT INTO todo_keywords VALUES(10,'TODO','open',NULL,0,'config_default',NULL,NULL);
INSERT INTO todo_keywords VALUES(10,'NEXT','open',NULL,1,'config_default',NULL,NULL);
INSERT INTO todo_keywords VALUES(10,'DONE','closed',NULL,2,'config_default',NULL,NULL);
INSERT INTO todo_keywords VALUES(10,'CANCEL','closed',NULL,3,'config_default',NULL,NULL);
INSERT INTO todo_keywords VALUES(11,'TODO','open',NULL,0,'config_default',NULL,NULL);
INSERT INTO todo_keywords VALUES(11,'NEXT','open',NULL,1,'config_default',NULL,NULL);
INSERT INTO todo_keywords VALUES(11,'DONE','closed',NULL,2,'config_default',NULL,NULL);
INSERT INTO todo_keywords VALUES(11,'CANCEL','closed',NULL,3,'config_default',NULL,NULL);
INSERT INTO todo_keywords VALUES(12,'TODO','open',NULL,0,'config_default',NULL,NULL);
INSERT INTO todo_keywords VALUES(12,'NEXT','open',NULL,1,'config_default',NULL,NULL);
INSERT INTO todo_keywords VALUES(12,'DONE','closed',NULL,2,'config_default',NULL,NULL);
INSERT INTO todo_keywords VALUES(12,'CANCEL','closed',NULL,3,'config_default',NULL,NULL);
INSERT INTO todo_keywords VALUES(13,'TODO','open',NULL,0,'config_default',NULL,NULL);
INSERT INTO todo_keywords VALUES(13,'NEXT','open',NULL,1,'config_default',NULL,NULL);
INSERT INTO todo_keywords VALUES(13,'DONE','closed',NULL,2,'config_default',NULL,NULL);
INSERT INTO todo_keywords VALUES(13,'CANCEL','closed',NULL,3,'config_default',NULL,NULL);
INSERT INTO todo_keywords VALUES(14,'TODO','open',NULL,0,'config_default',NULL,NULL);
INSERT INTO todo_keywords VALUES(14,'NEXT','open',NULL,1,'config_default',NULL,NULL);
INSERT INTO todo_keywords VALUES(14,'DONE','closed',NULL,2,'config_default',NULL,NULL);
INSERT INTO todo_keywords VALUES(14,'CANCEL','closed',NULL,3,'config_default',NULL,NULL);
INSERT INTO todo_keywords VALUES(15,'TODO','open',NULL,0,'config_default',NULL,NULL);
INSERT INTO todo_keywords VALUES(15,'NEXT','open',NULL,1,'config_default',NULL,NULL);
INSERT INTO todo_keywords VALUES(15,'DONE','closed',NULL,2,'config_default',NULL,NULL);
INSERT INTO todo_keywords VALUES(15,'CANCEL','closed',NULL,3,'config_default',NULL,NULL);
INSERT INTO todo_keywords VALUES(16,'TODO','open',NULL,0,'config_default',NULL,NULL);
INSERT INTO todo_keywords VALUES(16,'NEXT','open',NULL,1,'config_default',NULL,NULL);
INSERT INTO todo_keywords VALUES(16,'DONE','closed',NULL,2,'config_default',NULL,NULL);
INSERT INTO todo_keywords VALUES(16,'CANCEL','closed',NULL,3,'config_default',NULL,NULL);
INSERT INTO todo_keywords VALUES(17,'TODO','open',NULL,0,'config_default',NULL,NULL);
INSERT INTO todo_keywords VALUES(17,'NEXT','open',NULL,1,'config_default',NULL,NULL);
INSERT INTO todo_keywords VALUES(17,'DONE','closed',NULL,2,'config_default',NULL,NULL);
INSERT INTO todo_keywords VALUES(17,'CANCEL','closed',NULL,3,'config_default',NULL,NULL);
INSERT INTO todo_keywords VALUES(18,'TODO','open',NULL,0,'config_default',NULL,NULL);
INSERT INTO todo_keywords VALUES(18,'NEXT','open',NULL,1,'config_default',NULL,NULL);
INSERT INTO todo_keywords VALUES(18,'DONE','closed',NULL,2,'config_default',NULL,NULL);
INSERT INTO todo_keywords VALUES(18,'CANCEL','closed',NULL,3,'config_default',NULL,NULL);
INSERT INTO todo_keywords VALUES(19,'TODO','open',NULL,0,'config_default',NULL,NULL);
INSERT INTO todo_keywords VALUES(19,'NEXT','open',NULL,1,'config_default',NULL,NULL);
INSERT INTO todo_keywords VALUES(19,'DONE','closed',NULL,2,'config_default',NULL,NULL);
INSERT INTO todo_keywords VALUES(19,'CANCEL','closed',NULL,3,'config_default',NULL,NULL);
INSERT INTO todo_keywords VALUES(20,'TODO','open',NULL,0,'config_default',NULL,NULL);
INSERT INTO todo_keywords VALUES(20,'NEXT','open',NULL,1,'config_default',NULL,NULL);
INSERT INTO todo_keywords VALUES(20,'DONE','closed',NULL,2,'config_default',NULL,NULL);
INSERT INTO todo_keywords VALUES(20,'CANCEL','closed',NULL,3,'config_default',NULL,NULL);
INSERT INTO todo_keywords VALUES(21,'TODO','open',NULL,0,'config_default',NULL,NULL);
INSERT INTO todo_keywords VALUES(21,'NEXT','open',NULL,1,'config_default',NULL,NULL);
INSERT INTO todo_keywords VALUES(21,'DONE','closed',NULL,2,'config_default',NULL,NULL);
INSERT INTO todo_keywords VALUES(21,'CANCEL','closed',NULL,3,'config_default',NULL,NULL);
INSERT INTO todo_keywords VALUES(22,'TODO','open',NULL,0,'config_default',NULL,NULL);
INSERT INTO todo_keywords VALUES(22,'NEXT','open',NULL,1,'config_default',NULL,NULL);
INSERT INTO todo_keywords VALUES(22,'DONE','closed',NULL,2,'config_default',NULL,NULL);
INSERT INTO todo_keywords VALUES(22,'CANCEL','closed',NULL,3,'config_default',NULL,NULL);
INSERT INTO todo_keywords VALUES(23,'TODO','open',NULL,0,'org_keyword','TODO',3);
INSERT INTO todo_keywords VALUES(23,'DONE','closed',NULL,1,'org_keyword','TODO',3);
INSERT INTO todo_keywords VALUES(24,'TODO','open',NULL,0,'org_keyword','TODO',3);
INSERT INTO todo_keywords VALUES(24,'NEXT','open',NULL,1,'org_keyword','TODO',3);
INSERT INTO todo_keywords VALUES(24,'DONE','closed',NULL,2,'org_keyword','TODO',3);
INSERT INTO todo_keywords VALUES(25,'TODO','open',NULL,0,'config_default',NULL,NULL);
INSERT INTO todo_keywords VALUES(25,'NEXT','open',NULL,1,'config_default',NULL,NULL);
INSERT INTO todo_keywords VALUES(25,'DONE','closed',NULL,2,'config_default',NULL,NULL);
INSERT INTO todo_keywords VALUES(25,'CANCEL','closed',NULL,3,'config_default',NULL,NULL);
INSERT INTO todo_keywords VALUES(26,'TODO','open',NULL,0,'org_keyword','TODO',3);
INSERT INTO todo_keywords VALUES(26,'DONE','closed',NULL,1,'org_keyword','TODO',3);
INSERT INTO todo_keywords VALUES(27,'TODO','open',NULL,0,'config_default',NULL,NULL);
INSERT INTO todo_keywords VALUES(27,'NEXT','open',NULL,1,'config_default',NULL,NULL);
INSERT INTO todo_keywords VALUES(27,'DONE','closed',NULL,2,'config_default',NULL,NULL);
INSERT INTO todo_keywords VALUES(27,'CANCEL','closed',NULL,3,'config_default',NULL,NULL);
INSERT INTO todo_keywords VALUES(28,'TODO','open',NULL,0,'config_default',NULL,NULL);
INSERT INTO todo_keywords VALUES(28,'NEXT','open',NULL,1,'config_default',NULL,NULL);
INSERT INTO todo_keywords VALUES(28,'DONE','closed',NULL,2,'config_default',NULL,NULL);
INSERT INTO todo_keywords VALUES(28,'CANCEL','closed',NULL,3,'config_default',NULL,NULL);
INSERT INTO todo_keywords VALUES(29,'one','open','t',0,'org_keyword','TODO',3);
INSERT INTO todo_keywords VALUES(29,'two','open','n',1,'org_keyword','TODO',3);
INSERT INTO todo_keywords VALUES(29,'FIVE','open',NULL,2,'org_keyword','TODO',4);
INSERT INTO todo_keywords VALUES(29,'SIX','open',NULL,3,'org_keyword','TODO',4);
INSERT INTO todo_keywords VALUES(29,'seven','open',NULL,4,'org_keyword','TYP_TODO',5);
INSERT INTO todo_keywords VALUES(29,'nine','open',NULL,5,'org_keyword','SEQ_TODO',6);
INSERT INTO todo_keywords VALUES(29,'late_open','open',NULL,6,'org_keyword','TODO',32);
INSERT INTO todo_keywords VALUES(29,'three','closed','d',7,'org_keyword','TODO',3);
INSERT INTO todo_keywords VALUES(29,'four','closed','w',8,'org_keyword','TODO',3);
INSERT INTO todo_keywords VALUES(29,'eight','closed',NULL,9,'org_keyword','TYP_TODO',5);
INSERT INTO todo_keywords VALUES(29,'ten','closed',NULL,10,'org_keyword','SEQ_TODO',6);
INSERT INTO todo_keywords VALUES(29,'eleven','closed','c',11,'org_keyword','TODO',28);
INSERT INTO todo_keywords VALUES(29,'late_done','closed',NULL,12,'org_keyword','TODO',32);
INSERT INTO todo_keywords VALUES(30,'TODO','open',NULL,0,'config_default',NULL,NULL);
INSERT INTO todo_keywords VALUES(30,'NEXT','open',NULL,1,'config_default',NULL,NULL);
INSERT INTO todo_keywords VALUES(30,'DONE','closed',NULL,2,'config_default',NULL,NULL);
INSERT INTO todo_keywords VALUES(30,'CANCEL','closed',NULL,3,'config_default',NULL,NULL);
INSERT INTO todo_keywords VALUES(31,'TODO','open',NULL,0,'config_default',NULL,NULL);
INSERT INTO todo_keywords VALUES(31,'NEXT','open',NULL,1,'config_default',NULL,NULL);
INSERT INTO todo_keywords VALUES(31,'DONE','closed',NULL,2,'config_default',NULL,NULL);
INSERT INTO todo_keywords VALUES(31,'CANCEL','closed',NULL,3,'config_default',NULL,NULL);
INSERT INTO todo_keywords VALUES(32,'TODO','open',NULL,0,'config_default',NULL,NULL);
INSERT INTO todo_keywords VALUES(32,'NEXT','open',NULL,1,'config_default',NULL,NULL);
INSERT INTO todo_keywords VALUES(32,'DONE','closed',NULL,2,'config_default',NULL,NULL);
INSERT INTO todo_keywords VALUES(32,'CANCEL','closed',NULL,3,'config_default',NULL,NULL);
INSERT INTO todo_keywords VALUES(33,'TODO','open',NULL,0,'config_default',NULL,NULL);
INSERT INTO todo_keywords VALUES(33,'NEXT','open',NULL,1,'config_default',NULL,NULL);
INSERT INTO todo_keywords VALUES(33,'DONE','closed',NULL,2,'config_default',NULL,NULL);
INSERT INTO todo_keywords VALUES(33,'CANCEL','closed',NULL,3,'config_default',NULL,NULL);
CREATE TABLE db_metadata (
    key             TEXT PRIMARY KEY,
    value           TEXT NOT NULL
);
INSERT INTO db_metadata VALUES('body_text_available','1');
INSERT INTO db_metadata VALUES('fts_available','1');
INSERT INTO db_metadata VALUES('fts_body_indexed','1');
INSERT INTO db_metadata VALUES('fts_schema_version','1');
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
    priority            TEXT,
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
INSERT INTO headings VALUES(1,1,NULL,0,1,-1,363,'ancestors-fixture-anc3','ancestors-fixture-anc3',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(2,1,1,1,4,52,198,'ancestors: Tagged Ancestor anc3-root','ancestors: Tagged Ancestor anc3-root',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(3,1,2,2,5,130,198,'ancestors: Middle anc3-mid','ancestors: Middle anc3-mid',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(4,1,3,3,6,160,198,'ancestors: Deep Descendant anc3-a','ancestors: Deep Descendant anc3-a',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(5,1,1,1,7,198,321,'ancestors: Property Ancestor anc3-prop','ancestors: Property Ancestor anc3-prop',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(6,1,5,2,11,280,321,'ancestors: Property Descendant anc3-b','ancestors: Property Descendant anc3-b',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(7,1,1,1,12,321,363,'ancestors: Top Level No Ancestor anc3-c','ancestors: Top Level No Ancestor anc3-c',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(8,2,NULL,0,1,-1,399,'children-fixture-chd2','children-fixture-chd2',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(9,2,8,1,5,76,161,'children: Has Direct Child chd2-a','children: Has Direct Child chd2-a',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(10,2,9,2,6,112,161,'children: Direct TODO Child chd2-child-a','TODO children: Direct TODO Child chd2-child-a','TODO','open',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(11,2,8,1,7,161,283,'children: Has Nested NEXT chd2-b','children: Has Nested NEXT chd2-b',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(12,2,11,2,8,196,283,'children: Intermediate Child chd2-mid','children: Intermediate Child chd2-mid',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(13,2,12,3,9,237,283,'children: Grandchild NEXT chd2-grand','NEXT children: Grandchild NEXT chd2-grand','NEXT','open',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(14,2,8,1,10,283,314,'children: No Children chd2-c','children: No Children chd2-c',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(15,2,8,1,11,314,399,'children: Direct DONE Only chd2-d','children: Direct DONE Only chd2-d',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(16,2,15,2,12,350,399,'children: Direct DONE Child chd2-child-d','DONE children: Direct DONE Child chd2-child-d','DONE','closed',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(17,3,NULL,0,1,-1,396,'closed-fixture-cls8','closed-fixture-cls8',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(18,3,17,1,5,69,127,'closed: Date Only cls8-a','DONE closed: Date Only cls8-a','DONE','closed',NULL,NULL,NULL,NULL,NULL,NULL,NULL,'[2036-07-16 Wed]',2099779200,0,0,0,'[]');
INSERT INTO headings VALUES(19,3,17,1,8,127,191,'closed: With Time cls8-b','DONE closed: With Time cls8-b','DONE','closed',NULL,NULL,NULL,NULL,NULL,NULL,NULL,'[2036-07-16 Wed 12:40]',2099824800,1,0,0,'[]');
INSERT INTO headings VALUES(20,3,17,1,11,191,253,'closed: Range Earlier cls8-c','DONE closed: Range Earlier cls8-c','DONE','closed',NULL,NULL,NULL,NULL,NULL,NULL,NULL,'[2036-07-01 Tue]',2098483200,0,0,0,'[]');
INSERT INTO headings VALUES(21,3,17,1,14,253,319,'closed: Range Later cls8-d','DONE closed: Range Later cls8-d','DONE','closed',NULL,NULL,NULL,NULL,NULL,NULL,NULL,'[2036-07-31 Thu 20:15]',2101148100,1,0,0,'[]');
INSERT INTO headings VALUES(22,3,17,1,17,319,396,'closed: No Closed Negative Control cls8-e','TODO closed: No Closed Negative Control cls8-e','TODO','open',NULL,'<2036-07-16 Wed>',2099779200,0,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(23,4,NULL,0,1,-1,416,'deadline-fixture-dln6','deadline-fixture-dln6',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(24,4,23,1,5,71,133,'deadline: Date Only dln6-a','TODO deadline: Date Only dln6-a','TODO','open',NULL,NULL,NULL,NULL,'<2034-05-14 Sun>',2031177600,0,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(25,4,23,1,8,133,201,'deadline: With Time dln6-b','TODO deadline: With Time dln6-b','TODO','open',NULL,NULL,NULL,NULL,'<2034-05-14 Sun 10:30>',2031215400,1,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(26,4,23,1,11,201,267,'deadline: Range Earlier dln6-c','TODO deadline: Range Earlier dln6-c','TODO','open',NULL,NULL,NULL,NULL,'<2034-05-01 Mon>',2030054400,0,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(27,4,23,1,14,267,337,'deadline: Range Later dln6-d','TODO deadline: Range Later dln6-d','TODO','open',NULL,NULL,NULL,NULL,'<2034-05-31 Wed 17:45>',2032710300,1,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(28,4,23,1,17,337,416,'deadline: Scheduled Negative Control dln6-e','TODO deadline: Scheduled Negative Control dln6-e','TODO','open',NULL,'<2034-05-14 Sun>',2031177600,0,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(29,5,NULL,0,1,-1,474,'descendants-fixture-des4','descendants-fixture-des4',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(30,5,29,1,5,79,176,'descendants: Direct NEXT Descendant des4-a','descendants: Direct NEXT Descendant des4-a',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(31,5,30,2,6,124,176,'descendants: Direct NEXT Child des4-child-a','NEXT descendants: Direct NEXT Child des4-child-a','NEXT','open',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(32,5,29,1,7,176,349,'descendants: Deep Blocked Descendant des4-b','descendants: Deep Blocked Descendant des4-b',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(33,5,32,2,8,222,349,'descendants: Intermediate des4-mid','descendants: Intermediate des4-mid',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(34,5,33,3,9,260,349,'descendants: Deep Blocked TODO des4-deep                         :des4-blocked:','TODO descendants: Deep Blocked TODO des4-deep','TODO','open',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(35,5,29,1,10,349,437,'descendants: DONE Descendant Only des4-c','descendants: DONE Descendant Only des4-c',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(36,5,35,2,11,392,437,'descendants: Done Child des4-child-c','DONE descendants: Done Child des4-child-c','DONE','closed',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(37,5,29,1,12,437,474,'descendants: No Descendants des4-d','descendants: No Descendants des4-d',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(38,6,NULL,0,1,-1,243,'done-fixture-dne4','done-fixture-dne4',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(39,6,38,1,5,82,115,'done: Done Keyword dne4-a','DONE done: Done Keyword dne4-a','DONE','closed',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(40,6,38,1,6,115,158,'done: Cancelled Keyword dne4-b','CANCELLED done: Cancelled Keyword dne4-b','CANCELLED','closed',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(41,6,38,1,7,158,200,'done: Open Negative Control dne4-c','TODO done: Open Negative Control dne4-c','TODO','open',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(42,6,38,1,8,200,243,'done: No Keyword Negative Control dne4-d','done: No Keyword Negative Control dne4-d',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(43,7,NULL,0,1,-1,151,'file-modified-fixture-fmd8','file-modified-fixture-fmd8',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(44,7,43,1,4,56,100,'file-modified: Modified Time Match fmd8-a','file-modified: Modified Time Match fmd8-a',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(45,7,43,1,5,100,151,'file-modified: Modified Time Second Match fmd8-b','file-modified: Modified Time Second Match fmd8-b',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(46,8,NULL,0,1,-1,129,'file-name-fixture-fnm3','file-name-fixture-fnm3',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(47,8,46,1,4,52,87,'file-name: Filename Match fnm3-a','file-name: Filename Match fnm3-a',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(48,8,46,1,5,87,129,'file-name: Filename Second Match fnm3-b','file-name: Filename Second Match fnm3-b',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(49,9,NULL,0,1,-1,155,'Effective File Title FTL6 Unique','Effective File Title FTL6 Unique',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(50,9,49,1,4,62,105,'file-title: Effective Title Match ftl6-a','file-title: Effective Title Match ftl6-a',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(51,9,49,1,5,105,155,'file-title: Effective Title Second Match ftl6-b','file-title: Effective Title Second Match ftl6-b',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(52,10,NULL,0,1,-1,465,'has-link-fixture-hln5','has-link-fixture-hln5',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(53,10,52,1,4,51,141,'has-link: File Link hln5-a','has-link: File Link hln5-a',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(54,10,52,1,7,141,241,'has-link: HTTPS Link hln5-b','has-link: HTTPS Link hln5-b',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(55,10,52,1,10,241,319,'has-link: ID Link hln5-c','has-link: ID Link hln5-c',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(56,10,52,1,13,319,387,'has-link: No Link Control hln5-d','has-link: No Link Control hln5-d',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(57,10,52,1,16,387,465,'has-link: Target Heading hln5-target','has-link: Target Heading hln5-target',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(58,11,NULL,0,1,-1,226,'has-text','has-text',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(59,11,58,1,4,38,105,'has-text: Single Has Text','has-text: Single Has Text',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(60,11,58,1,9,105,226,'Multiple Machtes','Multiple Machtes',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(61,11,60,2,11,125,176,'has-text: Multiple Matches 1','has-text: Multiple Matches 1',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(62,11,60,2,15,176,226,'has-text: Multiple Matches 2','has-text: Multiple Matches 2',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(63,12,NULL,0,1,-1,215,'keyword-fixture-kwd7','keyword-fixture-kwd7',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(64,12,63,1,6,116,148,'keyword: First Heading kwd7-a','keyword: First Heading kwd7-a',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(65,12,63,1,7,148,215,'keyword: Second Heading kwd7-b','keyword: Second Heading kwd7-b',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(66,12,65,2,8,181,215,'keyword: Nested Heading kwd7-c','keyword: Nested Heading kwd7-c',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(67,13,NULL,0,1,-1,213,'level-fixture-lvl8','level-fixture-lvl8',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(68,13,67,1,4,48,150,'level: Level 1 lvl8-a','level: Level 1 lvl8-a',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(69,13,68,2,5,72,150,'level: Level 2 lvl8-b','level: Level 2 lvl8-b',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(70,13,69,3,6,97,150,'level: Level 3 lvl8-c','level: Level 3 lvl8-c',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(71,13,70,4,7,123,150,'level: Level 4 lvl8-d','level: Level 4 lvl8-d',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(72,13,67,1,8,150,213,'level: Second Level 1 lvl8-e','level: Second Level 1 lvl8-e',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(73,13,72,2,9,181,213,'level: Second Level 2 lvl8-f','level: Second Level 2 lvl8-f',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(74,14,NULL,0,1,-1,576,'linked-from-fixture-lfr7','linked-from-fixture-lfr7',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(75,14,74,1,4,54,190,'linked-from: Tagged Source lfr7-source-a','linked-from: Tagged Source lfr7-source-a',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(76,14,74,1,7,190,286,'linked-from: Plain Source lfr7-source-b','linked-from: Plain Source lfr7-source-b',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(77,14,74,1,10,286,384,'linked-from: Target With Backlink A lfr7-target-a','linked-from: Target With Backlink A lfr7-target-a',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(78,14,74,1,15,384,478,'linked-from: Target With Backlink B lfr7-target-b','linked-from: Target With Backlink B lfr7-target-b',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(79,14,74,1,20,478,576,'linked-from: Target Without Backlink lfr7-target-c','linked-from: Target Without Backlink lfr7-target-c',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(80,15,NULL,0,1,-1,574,'links-to-fixture-lto6','links-to-fixture-lto6',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(81,15,80,1,4,51,147,'links-to: Source To Custom ID lto6-a','links-to: Source To Custom ID lto6-a',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(82,15,80,1,7,147,230,'links-to: Source To ID lto6-b','links-to: Source To ID lto6-b',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(83,15,80,1,10,230,327,'links-to: Source To Other Target lto6-c','links-to: Source To Other Target lto6-c',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(84,15,80,1,13,327,481,'links-to: Target Heading lto6-target','links-to: Target Heading lto6-target',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(85,15,80,1,19,481,574,'links-to: Other Target Heading lto6-other','links-to: Other Target Heading lto6-other',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(86,16,NULL,0,1,-1,321,'Title can span multiple lines, even here','Title can span multiple lines, even here',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(87,16,86,1,7,136,321,'Unfortunately Everywhere','Unfortunately Everywhere',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(88,17,NULL,0,1,-1,44,'no-title-set',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(89,17,88,1,2,1,44,'The parent title should be the file name','The parent title should be the file name',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(90,18,NULL,0,1,-1,403,'outline-contains-fixture-olc1','outline-contains-fixture-olc1',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(91,18,90,1,4,59,253,'outline-contains: Project Branch olc1-project','outline-contains: Project Branch olc1-project',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(92,18,91,2,5,107,202,'outline-contains: Database Branch olc1-database','outline-contains: Database Branch olc1-database',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(93,18,92,3,6,158,202,'outline-contains: Query Leaf olc1-query','outline-contains: Query Leaf olc1-query',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(94,18,91,2,7,202,253,'outline-contains: Unrelated Leaf olc1-unrelated','outline-contains: Unrelated Leaf olc1-unrelated',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(95,18,90,1,8,253,403,'outline-contains: Other Root olc1-other','outline-contains: Other Root olc1-other',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(96,18,95,2,9,295,403,'outline-contains: Query Elsewhere olc1-query-elsewhere','outline-contains: Query Elsewhere olc1-query-elsewhere',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(97,18,96,3,10,353,403,'outline-contains: Regexp Leaf olc1-regexp-624','outline-contains: Regexp Leaf olc1-regexp-624',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(98,19,NULL,0,1,-1,427,'outline-sequence-fixture-ols2','outline-sequence-fixture-ols2',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(99,19,98,1,4,59,285,'outline-sequence: Alpha Parent ols2-alpha','outline-sequence: Alpha Parent ols2-alpha',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(100,19,99,2,5,103,235,'outline-sequence: Beta Middle ols2-beta','outline-sequence: Beta Middle ols2-beta',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(101,19,100,3,6,146,235,'outline-sequence: Gamma Leaf ols2-gamma','outline-sequence: Gamma Leaf ols2-gamma',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(102,19,101,4,7,190,235,'outline-sequence: Delta Deep ols2-delta','outline-sequence: Delta Deep ols2-delta',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(103,19,99,2,8,235,285,'outline-sequence: Gamma Noncontiguous ols2-gap','outline-sequence: Gamma Noncontiguous ols2-gap',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(104,19,98,1,9,285,427,'outline-sequence: Exact Alpha ols2-exact-alpha','outline-sequence: Exact Alpha ols2-exact-alpha',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(105,19,104,2,10,334,427,'outline-sequence: Exact Beta ols2-exact-beta','outline-sequence: Exact Beta ols2-exact-beta',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(106,19,105,3,11,382,427,'outline-sequence: Regexp 517 ols2-regexp','outline-sequence: Regexp 517 ols2-regexp',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(107,20,NULL,0,1,-1,374,'parent-fixture-par1','parent-fixture-par1',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(108,20,107,1,4,49,215,'parent: Matching Parent par1-target','parent: Matching Parent par1-target',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(109,20,108,2,5,132,215,'parent: Direct Child Match par1-a','parent: Direct Child Match par1-a',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(110,20,109,3,6,169,215,'parent: Grandchild Parent Is Child par1-b','parent: Grandchild Parent Is Child par1-b',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(111,20,107,1,7,215,337,'parent: Nonmatching Parent par1-other','parent: Nonmatching Parent par1-other',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(112,20,111,2,8,297,337,'parent: Direct Child Negative par1-c','parent: Direct Child Negative par1-c',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(113,20,107,1,9,337,374,'parent: Top Level No Parent par1-d','parent: Top Level No Parent par1-d',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(114,21,NULL,0,1,-1,128,'file-dir-fixture-fdr5','file-dir-fixture-fdr5',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(115,21,114,1,4,51,86,'file-dir: Directory Match fdr5-a','file-dir: Directory Match fdr5-a',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(116,21,114,1,5,86,128,'file-dir: Directory Second Match fdr5-b','file-dir: Directory Second Match fdr5-b',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(117,22,NULL,0,1,-1,121,'file-path-fixture-fpt4','file-path-fixture-fpt4',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(118,22,117,1,4,52,83,'file-path: Path Match fpt4-a','file-path: Path Match fpt4-a',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(119,22,117,1,5,83,121,'file-path: Path Second Match fpt4-b','file-path: Path Second Match fpt4-b',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(120,23,NULL,0,1,-1,510,'planning-fixture-pln9','planning-fixture-pln9',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(121,23,120,1,5,71,134,'planning: Scheduled pln9-a','TODO planning: Scheduled pln9-a','TODO','open',NULL,'<2037-08-17 Mon>',2134080000,0,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(122,23,120,1,8,134,201,'planning: Deadline pln9-b','TODO planning: Deadline pln9-b','TODO','open',NULL,NULL,NULL,NULL,'<2037-08-17 Mon 13:20>',2134128000,1,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(123,23,120,1,11,201,264,'planning: Closed pln9-c','DONE planning: Closed pln9-c','DONE','closed',NULL,NULL,NULL,NULL,NULL,NULL,NULL,'[2037-08-17 Mon 18:10]',2134145400,1,0,0,'[]');
INSERT INTO headings VALUES(124,23,120,1,14,264,368,'planning: Multiple Planning pln9-d','TODO planning: Multiple Planning pln9-d','TODO','open',NULL,'<2037-08-01 Sat>',2132697600,0,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(125,23,120,1,18,368,438,'planning: Plain Timestamp Negative Control pln9-e','planning: Plain Timestamp Negative Control pln9-e',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(126,23,120,1,21,438,510,'planning: No Timestamp Control pln9-f','planning: No Timestamp Control pln9-f',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(127,24,NULL,0,1,-1,340,'priority-fixture-pri4','priority-fixture-pri4',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(128,24,127,1,5,76,113,'priority: Exact A pri4-a','TODO [#A] priority: Exact A pri4-a','TODO','open','A',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(129,24,127,1,6,113,150,'priority: Exact B pri4-b','TODO [#B] priority: Exact B pri4-b','TODO','open','B',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(130,24,127,1,7,150,187,'priority: Exact C pri4-c','NEXT [#C] priority: Exact C pri4-c','NEXT','open','C',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(131,24,127,1,8,187,240,'priority: No Priority Negative Control pri4-d','TODO priority: No Priority Negative Control pri4-d','TODO','open',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(132,24,127,1,9,240,290,'priority: Done With Priority A pri4-e','DONE [#A] priority: Done With Priority A pri4-e','DONE','closed','A',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(133,24,127,1,10,290,340,'priority: Done With Priority A pri4-e','DONE [#1] priority: Done With Priority A pri4-e','DONE','closed','1',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(134,25,NULL,0,1,-1,15145,'Org Property and Keyword Test','Org Property and Keyword Test',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(135,25,134,1,38,1065,1115,'Empty Property','Empty Property',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(136,25,134,1,43,1115,2674,'Expected file/root values','Expected file/root values',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(137,25,134,1,73,2674,3084,'Local append on the same heading','Local append on the same heading',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(138,25,134,1,87,3084,3422,'Multiple local append rows','Multiple local append rows',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(139,25,134,1,101,3422,3839,'Local append without a base value','Local append without a base value',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(140,25,134,1,114,3839,4251,'Later local definition replaces the earlier definition','Later local definition replaces the earlier definition',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(141,25,134,1,129,4251,4759,'Local append followed by replacement','Local append followed by replacement',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(142,25,134,1,144,4759,5062,'Empty base followed by append','Empty base followed by append',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(143,25,134,1,157,5062,5366,'Base followed by empty append','Base followed by empty append',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(144,25,134,1,170,5366,6824,'Parent append inheritance','Parent append inheritance',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(145,25,144,2,175,5429,5813,'Child with append only','Child with append only',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(146,25,144,2,188,5813,6047,'Child without local value','Child without local value',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(147,25,144,2,197,6047,6407,'Child with local replacement','Child with local replacement',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(148,25,144,2,210,6407,6824,'Child with local replacement and append','Child with local replacement and append',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(149,25,134,1,224,6824,7854,'Parent with appended effective value','Parent with appended effective value',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(150,25,149,2,230,6916,7162,'Inheriting child','Inheriting child',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(151,25,149,2,239,7162,7517,'Child overriding appended parent value','Child overriding appended parent value',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(152,25,149,2,252,7517,7854,'Child appending to appended parent value','Child appending to appended parent value',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(153,25,134,1,264,7854,8893,'Nearest ancestor wins','Nearest ancestor wins',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(154,25,153,2,269,7918,8893,'Parent override','Parent override',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(155,25,154,3,274,7972,8283,'Child inheriting nearest value','Child inheriting nearest value',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(156,25,154,3,284,8283,8597,'Child appending to nearest value','Child appending to nearest value',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(157,25,154,3,296,8597,8893,'Child replacing nearest value','Child replacing nearest value',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(158,25,134,1,308,8893,9937,'Root drawer inheritance','Root drawer inheritance',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(159,25,158,2,310,8920,9210,'Child inheriting root drawer base','Child inheriting root drawer base',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(160,25,158,2,319,9210,9580,'Child appending to root drawer base','Child appending to root drawer base',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(161,25,158,2,331,9580,9937,'Child replacing root drawer base','Child replacing root drawer base',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(162,25,134,1,343,9937,11477,'File keyword inheritance','File keyword inheritance',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(163,25,162,2,345,9965,10276,'Child inheriting appended keyword','Child inheriting appended keyword',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(164,25,162,2,354,10276,10715,'Child appending to file keyword','Child appending to file keyword',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(165,25,162,2,366,10715,11060,'Child replacing file keyword','Child replacing file keyword',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(166,25,162,2,378,11060,11477,'Child replacing and appending file keyword','Child replacing and appending file keyword',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(167,25,134,1,391,11477,11778,'Append before','Append before',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(168,25,134,1,403,11778,12373,'Duplicate property definitions in one drawer','Duplicate property definitions in one drawer',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(169,25,134,1,419,12373,12678,'Duplicate definition followed by append','Duplicate definition followed by append',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(170,25,134,1,432,12678,13278,'Append followed by duplicate replacement','Append followed by duplicate replacement',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(171,25,134,1,450,13278,13584,'Append followed by duplicate replacement','Append followed by duplicate replacement',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(172,25,134,1,463,13584,14236,'Parent duplicate definition','Parent duplicate definition',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(173,25,172,2,469,13663,13914,'Child inheriting duplicate parent value','Child inheriting duplicate parent value',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(174,25,172,2,478,13914,14236,'Child appending to duplicate parent value','Child appending to duplicate parent value',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(175,25,134,1,490,14236,14726,'Parent definition overridden by child duplicate definitions','Parent definition overridden by child duplicate definitions',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(176,25,175,2,495,14333,14726,'Child with two local definitions','Child with two local definitions',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(177,25,134,1,509,14726,15145,'Mixed-case property keys','Mixed-case property keys',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(178,26,NULL,0,1,-1,424,'scheduled-fixture-sch7','scheduled-fixture-sch7',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(179,26,178,1,5,72,136,'scheduled: Date Only sch7-a','TODO scheduled: Date Only sch7-a','TODO','open',NULL,'<2035-06-15 Fri>',2065478400,0,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(180,26,178,1,8,136,206,'scheduled: With Time sch7-b','TODO scheduled: With Time sch7-b','TODO','open',NULL,'<2035-06-15 Fri 11:35>',2065520100,1,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(181,26,178,1,11,206,274,'scheduled: Range Earlier sch7-c','TODO scheduled: Range Earlier sch7-c','TODO','open',NULL,'<2035-06-01 Fri>',2064268800,0,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(182,26,178,1,14,274,346,'scheduled: Range Later sch7-d','TODO scheduled: Range Later sch7-d','TODO','open',NULL,'<2035-06-30 Sat 19:25>',2066844300,1,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(183,26,178,1,17,346,424,'scheduled: Deadline Negative Control sch7-e','TODO scheduled: Deadline Negative Control sch7-e','TODO','open',NULL,NULL,NULL,NULL,'<2035-06-15 Fri>',2065478400,0,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(184,27,NULL,0,1,-1,1896,'tags_fixture_tag3','tags_fixture_tag3',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'["tag3_file_inherited"]');
INSERT INTO headings VALUES(185,27,184,1,5,81,266,'tags: Local Single tag3_a','tags: Local Single tag3_a',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'["tag3_file_inherited","tag3_local_blue"]');
INSERT INTO headings VALUES(186,27,184,1,8,266,444,'tags: Local Any First tag3_b','tags: Local Any First tag3_b',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'["tag3_file_inherited","tag3_any_red"]');
INSERT INTO headings VALUES(187,27,184,1,11,444,626,'tags: Local Any Second tag3_c','tags: Local Any Second tag3_c',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'["tag3_file_inherited","tag3_any_green"]');
INSERT INTO headings VALUES(188,27,184,1,14,626,836,'tags: All Match tag3_d','tags: All Match tag3_d',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'["tag3_file_inherited","tag3_all_gold","tag3_all_silver"]');
INSERT INTO headings VALUES(189,27,184,1,17,836,1351,'tags: Parent Inheritance tag3_e','tags: Parent Inheritance tag3_e',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'["tag3_file_inherited","tag3_parent_violet"]');
INSERT INTO headings VALUES(190,27,189,2,20,1026,1160,'tags: Inherited Child tag3_f','tags: Inherited Child tag3_f',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'["tag3_file_inherited","tag3_parent_violet"]');
INSERT INTO headings VALUES(191,27,189,2,23,1160,1351,'tags: Local Child Override Test tag3_g','tags: Local Child Override Test tag3_g',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'["tag3_file_inherited","tag3_parent_violet"]');
INSERT INTO headings VALUES(192,27,184,1,26,1351,1535,'tags: Regexp Match tag3_h','tags: Regexp Match tag3_h',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'["tag3_file_inherited","tag3_regexp_482"]');
INSERT INTO headings VALUES(193,27,184,1,29,1535,1651,'tags: Filetag Inherited tag3_i','tags: Filetag Inherited tag3_i',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'["tag3_file_inherited"]');
INSERT INTO headings VALUES(194,27,184,1,32,1651,1841,'tags: No Match Control tag3_j','tags: No Match Control tag3_j',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'["tag3_file_inherited","tag3_control_black"]');
INSERT INTO headings VALUES(195,27,184,1,35,1841,1896,'tags: Override Parent File Tag','tags: Override Parent File Tag',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'["tag3_file_inherited"]');
INSERT INTO headings VALUES(196,28,NULL,0,1,-1,115,'Title','Title',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(197,28,196,1,3,16,42,'Title: This Is a Title','Title: This Is a Title',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(198,28,196,1,5,42,74,'title: This Is another Title','title: This Is another Title',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(199,28,196,1,7,74,95,'Title: Same Title','Title: Same Title',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(200,28,196,1,9,95,115,'Title: Same Title','Title: Same Title',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(201,29,NULL,0,1,-1,1118,'File-local TODO keywords','File-local TODO keywords',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(202,29,201,1,8,164,284,'TODO default keyword should stay in title','TODO default keyword should stay in title',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(203,29,201,1,11,284,409,'DONE default done keyword should stay in title','DONE default done keyword should stay in title',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(204,29,201,1,14,409,442,'open keyword with fast key','one open keyword with fast key','one','open',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(205,29,201,1,15,442,483,'another open keyword with fast key','two another open keyword with fast key','two','open',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(206,29,201,1,16,483,520,'closed keyword with fast key','three closed keyword with fast key','three','closed',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(207,29,201,1,17,520,566,'closed keyword with extended fast key','four closed keyword with extended fast key','four','closed',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(208,29,201,1,19,566,612,'open keyword from empty-done-side line','FIVE open keyword from empty-done-side line','FIVE','open',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(209,29,201,1,20,612,666,'another open keyword from empty-done-side line','SIX another open keyword from empty-done-side line','SIX','open',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(210,29,201,1,22,666,701,'open keyword from TYP_TODO','seven open keyword from TYP_TODO','seven','open',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(211,29,201,1,23,701,739,'closed keyword from TYP_TODO','eight closed keyword from TYP_TODO','eight','closed',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(212,29,201,1,25,739,773,'open keyword from SEQ_TODO','nine open keyword from SEQ_TODO','nine','open',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(213,29,201,1,26,773,830,'closed keyword from SEQ_TODO','ten closed keyword from SEQ_TODO','ten','closed',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(214,29,201,1,30,830,907,'closed keyword from later TODO line','eleven closed keyword from later TODO line','eleven','closed',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(215,29,201,1,34,907,964,'open keyword from line defined later in file','late_open open keyword from line defined later in file','late_open','open',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(216,29,201,1,35,964,1024,'closed keyword from line defined later in file','late_done closed keyword from line defined later in file','late_done','closed',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(217,29,201,1,37,1024,1071,'TODO still not valid after later local lines','TODO still not valid after later local lines',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(218,29,201,1,38,1071,1118,'DONE still not valid after later local lines','DONE still not valid after later local lines',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(219,30,NULL,0,1,-1,245,'TODO keywords','TODO keywords',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(220,30,219,1,4,43,74,'Default keyword > TODO','TODO Default keyword > TODO','TODO','open',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(221,30,219,1,6,74,105,'Default keyword > DONE','DONE Default keyword > DONE','DONE','closed',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(222,30,219,1,8,105,162,'Default Keyword with Prio and Cookies','TODO [#A] Default Keyword with Prio and Cookies [0/0]','TODO','open','A',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(223,30,219,1,10,162,202,'Keyword from Config.toml > NEXT','NEXT Keyword from Config.toml > NEXT','NEXT','open',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(224,30,219,1,12,202,245,'Keyword from Config.toml > CANCEL','CANCEL Keyword from Config.toml > CANCEL','CANCEL','closed',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(225,31,NULL,0,1,-1,321,'ts-active-fixture-tsa4','ts-active-fixture-tsa4',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(226,31,225,1,4,52,100,'ts-active: Date Only tsa4-a','ts-active: Date Only tsa4-a',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(227,31,225,1,7,100,154,'ts-active: With Time tsa4-b','ts-active: With Time tsa4-b',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(228,31,225,1,10,154,204,'ts-active: Range Start tsa4-c','ts-active: Range Start tsa4-c',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(229,31,225,1,13,204,258,'ts-active: Range End tsa4-d','ts-active: Range End tsa4-d',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(230,31,225,1,16,258,321,'ts-active: Inactive Negative Control tsa4-e','ts-active: Inactive Negative Control tsa4-e',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(231,32,NULL,0,1,-1,331,'ts-inactive-fixture-tsi5','ts-inactive-fixture-tsi5',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(232,32,231,1,4,54,104,'ts-inactive: Date Only tsi5-a','ts-inactive: Date Only tsi5-a',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(233,32,231,1,7,104,160,'ts-inactive: With Time tsi5-b','ts-inactive: With Time tsi5-b',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(234,32,231,1,10,160,212,'ts-inactive: Range Start tsi5-c','ts-inactive: Range Start tsi5-c',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(235,32,231,1,13,212,268,'ts-inactive: Range End tsi5-d','ts-inactive: Range End tsi5-d',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(236,32,231,1,16,268,331,'ts-inactive: Active Negative Control tsi5-e','ts-inactive: Active Negative Control tsi5-e',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(237,33,NULL,0,1,-1,402,'ts-fixture-tsp3','ts-fixture-tsp3',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(238,33,237,1,4,45,93,'ts: Active Date Only tsp3-a','ts: Active Date Only tsp3-a',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(239,33,237,1,7,93,147,'ts: Active With Time tsp3-b','ts: Active With Time tsp3-b',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(240,33,237,1,10,147,197,'ts: Inactive Date Only tsp3-c','ts: Inactive Date Only tsp3-c',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(241,33,237,1,13,197,253,'ts: Inactive With Time tsp3-d','ts: Inactive With Time tsp3-d',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(242,33,237,1,16,253,336,'ts: Multiple Timestamp Range tsp3-e','ts: Multiple Timestamp Range tsp3-e',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(243,33,237,1,19,336,402,'ts: No Timestamp Control tsp3-f','ts: No Timestamp Control tsp3-f',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
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
INSERT INTO timestamps VALUES(1,18,'closed',0,2099779200,NULL,'inactive','none','[2036-07-16 Wed]',109,125,6);
INSERT INTO timestamps VALUES(2,19,'closed',1,2099824800,NULL,'inactive','none','[2036-07-16 Wed 12:40]',167,189,9);
INSERT INTO timestamps VALUES(3,20,'closed',0,2098483200,NULL,'inactive','none','[2036-07-01 Tue]',235,251,12);
INSERT INTO timestamps VALUES(4,21,'closed',1,2101148100,NULL,'inactive','none','[2036-07-31 Thu 20:15]',295,317,15);
INSERT INTO timestamps VALUES(5,22,'scheduled',0,2099779200,NULL,'active','none','<2036-07-16 Wed>',379,395,18);
INSERT INTO timestamps VALUES(6,24,'deadline',0,2031177600,NULL,'active','none','<2034-05-14 Sun>',115,131,6);
INSERT INTO timestamps VALUES(7,25,'deadline',1,2031215400,NULL,'active','none','<2034-05-14 Sun 10:30>',177,199,9);
INSERT INTO timestamps VALUES(8,26,'deadline',0,2030054400,NULL,'active','none','<2034-05-01 Mon>',249,265,12);
INSERT INTO timestamps VALUES(9,27,'deadline',1,2032710300,NULL,'active','none','<2034-05-31 Wed 17:45>',313,335,15);
INSERT INTO timestamps VALUES(10,28,'scheduled',0,2031177600,NULL,'active','none','<2034-05-14 Sun>',399,415,18);
INSERT INTO timestamps VALUES(11,121,'scheduled',0,2134080000,NULL,'active','none','<2037-08-17 Mon>',116,132,6);
INSERT INTO timestamps VALUES(12,122,'deadline',1,2134128000,NULL,'active','none','<2037-08-17 Mon 13:20>',177,199,9);
INSERT INTO timestamps VALUES(13,123,'closed',1,2134145400,NULL,'inactive','none','[2037-08-17 Mon 18:10]',240,262,12);
INSERT INTO timestamps VALUES(14,124,'scheduled',0,2132697600,NULL,'active','none','<2037-08-01 Sat>',317,333,15);
INSERT INTO timestamps VALUES(15,124,'body',1,2135347200,NULL,'active','none','<2037-08-31 Mon 16:00>',344,366,16);
INSERT INTO timestamps VALUES(16,125,'body',0,2134080000,NULL,'active','none','<2037-08-17 Mon>',420,436,19);
INSERT INTO timestamps VALUES(17,179,'scheduled',0,2065478400,NULL,'active','none','<2035-06-15 Fri>',118,134,6);
INSERT INTO timestamps VALUES(18,180,'scheduled',1,2065520100,NULL,'active','none','<2035-06-15 Fri 11:35>',182,204,9);
INSERT INTO timestamps VALUES(19,181,'scheduled',0,2064268800,NULL,'active','none','<2035-06-01 Fri>',256,272,12);
INSERT INTO timestamps VALUES(20,182,'scheduled',1,2066844300,NULL,'active','none','<2035-06-30 Sat 19:25>',322,344,15);
INSERT INTO timestamps VALUES(21,183,'deadline',0,2065478400,NULL,'active','none','<2035-06-15 Fri>',407,423,18);
INSERT INTO timestamps VALUES(22,226,'body',0,1962662400,NULL,'active','none','<2032-03-12 Fri>',82,98,5);
INSERT INTO timestamps VALUES(23,227,'body',1,1962692400,NULL,'active','none','<2032-03-12 Fri 08:20>',130,152,8);
INSERT INTO timestamps VALUES(24,228,'body',0,1961712000,NULL,'active','none','<2032-03-01 Mon>',186,202,11);
INSERT INTO timestamps VALUES(25,229,'body',1,1964387400,NULL,'active','none','<2032-03-31 Wed 23:10>',234,256,14);
INSERT INTO timestamps VALUES(26,230,'body',0,1962662400,NULL,'inactive','none','[2032-03-12 Fri]',304,320,17);
INSERT INTO timestamps VALUES(27,232,'body',0,1996963200,NULL,'inactive','none','[2033-04-13 Wed]',86,102,5);
INSERT INTO timestamps VALUES(28,233,'body',1,1997023200,NULL,'inactive','none','[2033-04-13 Wed 16:40]',136,158,8);
INSERT INTO timestamps VALUES(29,234,'body',0,1995926400,NULL,'inactive','none','[2033-04-01 Fri]',194,210,11);
INSERT INTO timestamps VALUES(30,235,'body',1,1998510900,NULL,'inactive','none','[2033-04-30 Sat 21:55]',244,266,14);
INSERT INTO timestamps VALUES(31,236,'body',0,1996963200,NULL,'active','none','<2033-04-13 Wed>',314,330,17);
INSERT INTO timestamps VALUES(32,238,'body',0,1925769600,NULL,'active','none','<2031-01-10 Fri>',75,91,5);
INSERT INTO timestamps VALUES(33,239,'body',1,1925802900,NULL,'active','none','<2031-01-10 Fri 09:15>',123,145,8);
INSERT INTO timestamps VALUES(34,240,'body',0,1925856000,NULL,'inactive','none','[2031-01-11 Sat]',179,195,11);
INSERT INTO timestamps VALUES(35,241,'body',1,1925909100,NULL,'inactive','none','[2031-01-11 Sat 14:45]',229,251,14);
INSERT INTO timestamps VALUES(36,242,'body',0,1927670400,NULL,'active','none','<2031-02-01 Sat>',291,307,17);
INSERT INTO timestamps VALUES(37,242,'body',1,1929378600,NULL,'inactive','none','[2031-02-20 Thu 18:30]',312,334,17);
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
INSERT INTO keywords VALUES(1,1,'TITLE','ancestors-fixture-anc3',1);
INSERT INTO keywords VALUES(2,1,'STARTUP','showall',2);
INSERT INTO keywords VALUES(3,8,'TITLE','children-fixture-chd2',1);
INSERT INTO keywords VALUES(4,8,'STARTUP','showall',2);
INSERT INTO keywords VALUES(5,8,'TODO','TODO NEXT | DONE',3);
INSERT INTO keywords VALUES(6,17,'TITLE','closed-fixture-cls8',1);
INSERT INTO keywords VALUES(7,17,'STARTUP','showall',2);
INSERT INTO keywords VALUES(8,17,'TODO','TODO | DONE',3);
INSERT INTO keywords VALUES(9,23,'TITLE','deadline-fixture-dln6',1);
INSERT INTO keywords VALUES(10,23,'STARTUP','showall',2);
INSERT INTO keywords VALUES(11,23,'TODO','TODO | DONE',3);
INSERT INTO keywords VALUES(12,29,'TITLE','descendants-fixture-des4',1);
INSERT INTO keywords VALUES(13,29,'STARTUP','showall',2);
INSERT INTO keywords VALUES(14,29,'TODO','TODO NEXT | DONE',3);
INSERT INTO keywords VALUES(15,38,'TITLE','done-fixture-dne4',1);
INSERT INTO keywords VALUES(16,38,'STARTUP','showall',2);
INSERT INTO keywords VALUES(17,38,'TODO','TODO NEXT | DONE CANCELLED',3);
INSERT INTO keywords VALUES(18,43,'TITLE','file-modified-fixture-fmd8',1);
INSERT INTO keywords VALUES(19,43,'STARTUP','showall',2);
INSERT INTO keywords VALUES(20,46,'TITLE','file-name-fixture-fnm3',1);
INSERT INTO keywords VALUES(21,46,'STARTUP','showall',2);
INSERT INTO keywords VALUES(22,49,'TITLE','Effective File Title FTL6 Unique',1);
INSERT INTO keywords VALUES(23,49,'STARTUP','showall',2);
INSERT INTO keywords VALUES(24,52,'TITLE','has-link-fixture-hln5',1);
INSERT INTO keywords VALUES(25,52,'STARTUP','showall',2);
INSERT INTO keywords VALUES(26,58,'TITLE','has-text',1);
INSERT INTO keywords VALUES(27,58,'STARTUP','showall',2);
INSERT INTO keywords VALUES(28,63,'TITLE','keyword-fixture-kwd7',1);
INSERT INTO keywords VALUES(29,63,'AUTHOR','Author-kwd7-unique',2);
INSERT INTO keywords VALUES(30,63,'KWD7_CUSTOM','custom-value-kwd7-593',3);
INSERT INTO keywords VALUES(31,63,'STARTUP','showall',4);
INSERT INTO keywords VALUES(32,67,'TITLE','level-fixture-lvl8',1);
INSERT INTO keywords VALUES(33,67,'STARTUP','showall',2);
INSERT INTO keywords VALUES(34,74,'TITLE','linked-from-fixture-lfr7',1);
INSERT INTO keywords VALUES(35,74,'STARTUP','showall',2);
INSERT INTO keywords VALUES(36,80,'TITLE','links-to-fixture-lto6',1);
INSERT INTO keywords VALUES(37,80,'STARTUP','showall',2);
INSERT INTO keywords VALUES(38,86,'TITLE','Title can span',1);
INSERT INTO keywords VALUES(39,86,'TITLE','multiple lines,',2);
INSERT INTO keywords VALUES(40,86,'AUTHOR','Hubisan',3);
INSERT INTO keywords VALUES(41,86,'TITLE','even here',9);
INSERT INTO keywords VALUES(42,90,'TITLE','outline-contains-fixture-olc1',1);
INSERT INTO keywords VALUES(43,90,'STARTUP','showall',2);
INSERT INTO keywords VALUES(44,98,'TITLE','outline-sequence-fixture-ols2',1);
INSERT INTO keywords VALUES(45,98,'STARTUP','showall',2);
INSERT INTO keywords VALUES(46,107,'TITLE','parent-fixture-par1',1);
INSERT INTO keywords VALUES(47,107,'STARTUP','showall',2);
INSERT INTO keywords VALUES(48,114,'TITLE','file-dir-fixture-fdr5',1);
INSERT INTO keywords VALUES(49,114,'STARTUP','showall',2);
INSERT INTO keywords VALUES(50,117,'TITLE','file-path-fixture-fpt4',1);
INSERT INTO keywords VALUES(51,117,'STARTUP','showall',2);
INSERT INTO keywords VALUES(52,120,'TITLE','planning-fixture-pln9',1);
INSERT INTO keywords VALUES(53,120,'STARTUP','showall',2);
INSERT INTO keywords VALUES(54,120,'TODO','TODO | DONE',3);
INSERT INTO keywords VALUES(55,127,'TITLE','priority-fixture-pri4',1);
INSERT INTO keywords VALUES(56,127,'STARTUP','showall',2);
INSERT INTO keywords VALUES(57,127,'TODO','TODO NEXT | DONE',3);
INSERT INTO keywords VALUES(58,134,'TITLE','Org Property and Keyword Test',20);
INSERT INTO keywords VALUES(59,134,'STARTUP','showall',21);
INSERT INTO keywords VALUES(60,134,'CATEGORY','category_keyword_value',22);
INSERT INTO keywords VALUES(61,134,'PROPERTY','KEYWORD_APPEND foo=1',23);
INSERT INTO keywords VALUES(62,134,'PROPERTY','KEYWORD_APPEND+ bar=2',24);
INSERT INTO keywords VALUES(63,134,'PROPERTY','KEYWORD_APPEND+ baz=3',25);
INSERT INTO keywords VALUES(64,134,'PROPERTY','KEYWORD_DUPLICATE first',26);
INSERT INTO keywords VALUES(65,134,'PROPERTY','KEYWORD_DUPLICATE second',27);
INSERT INTO keywords VALUES(66,134,'PROPERTY','KEYWORD_RESET old',28);
INSERT INTO keywords VALUES(67,134,'PROPERTY','KEYWORD_RESET+ appended-before-reset',29);
INSERT INTO keywords VALUES(68,134,'PROPERTY','KEYWORD_RESET replacement',30);
INSERT INTO keywords VALUES(69,134,'PROPERTY','KEYWORD_RESET+ appended-after-reset',31);
INSERT INTO keywords VALUES(70,134,'PROPERTY','KEYWORD_APPEND_ONLY only',32);
INSERT INTO keywords VALUES(71,134,'PROPERTY','KEYWORD_APPEND_ONLY+ appended',33);
INSERT INTO keywords VALUES(72,134,'PROPERTY','KEYWORD_EMPTY',34);
INSERT INTO keywords VALUES(73,134,'PROPERTY','KEYWORD_EMPTY+ valid',35);
INSERT INTO keywords VALUES(74,134,'PROPERTY','KEYWORD_VERY_EMPTY',36);
INSERT INTO keywords VALUES(75,178,'TITLE','scheduled-fixture-sch7',1);
INSERT INTO keywords VALUES(76,178,'STARTUP','showall',2);
INSERT INTO keywords VALUES(77,178,'TODO','TODO | DONE',3);
INSERT INTO keywords VALUES(78,184,'TITLE','tags_fixture_tag3',1);
INSERT INTO keywords VALUES(79,184,'STARTUP','showall',2);
INSERT INTO keywords VALUES(80,184,'FILETAGS',':tag3_file_inherited:',3);
INSERT INTO keywords VALUES(81,196,'TITLE','Title',1);
INSERT INTO keywords VALUES(82,201,'TITLE','File-local TODO keywords',1);
INSERT INTO keywords VALUES(83,201,'STARTUP','showall',2);
INSERT INTO keywords VALUES(84,201,'TODO','one(t) two(n) | three(d) four(w@)',3);
INSERT INTO keywords VALUES(85,201,'TODO','FIVE SIX |',4);
INSERT INTO keywords VALUES(86,201,'TYP_TODO','seven | eight',5);
INSERT INTO keywords VALUES(87,201,'SEQ_TODO','nine | ten',6);
INSERT INTO keywords VALUES(88,201,'TODO','| eleven(c)',28);
INSERT INTO keywords VALUES(89,201,'TODO','late_open | late_done',32);
INSERT INTO keywords VALUES(90,219,'TITLE','TODO keywords',1);
INSERT INTO keywords VALUES(91,219,'STARTUP','showall',2);
INSERT INTO keywords VALUES(92,225,'TITLE','ts-active-fixture-tsa4',1);
INSERT INTO keywords VALUES(93,225,'STARTUP','showall',2);
INSERT INTO keywords VALUES(94,231,'TITLE','ts-inactive-fixture-tsi5',1);
INSERT INTO keywords VALUES(95,231,'STARTUP','showall',2);
INSERT INTO keywords VALUES(96,237,'TITLE','ts-fixture-tsp3',1);
INSERT INTO keywords VALUES(97,237,'STARTUP','showall',2);
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
INSERT INTO properties VALUES(1,5,'ANC3_AREA','work-anc3','property_drawer',0,9);
INSERT INTO properties VALUES(2,57,'ID','hln5-target-id','property_drawer',0,18);
INSERT INTO properties VALUES(3,77,'CUSTOM_ID','lfr7-target-a','property_drawer',0,12);
INSERT INTO properties VALUES(4,78,'ID','lfr7-target-id-b','property_drawer',0,17);
INSERT INTO properties VALUES(5,79,'CUSTOM_ID','lfr7-target-c','property_drawer',0,22);
INSERT INTO properties VALUES(6,84,'ID','lto6-target-id','property_drawer',0,15);
INSERT INTO properties VALUES(7,84,'CUSTOM_ID','lto6-target-custom','property_drawer',0,16);
INSERT INTO properties VALUES(8,85,'CUSTOM_ID','lto6-other-custom','property_drawer',0,21);
INSERT INTO properties VALUES(9,134,'CATEGORY','Level 0 Category Property','property_drawer',0,2);
INSERT INTO properties VALUES(10,134,'WHATEVER','level 0 drawer property','property_drawer',0,3);
INSERT INTO properties VALUES(11,134,'OVERWRITE','this one works','property_drawer',0,4);
INSERT INTO properties VALUES(12,134,'ID','7dad9b62-a3cc-43ec-a60f-e650bdaeae6d','property_drawer',0,5);
INSERT INTO properties VALUES(13,134,'ROOT_DRAWER_BASE','root','property_drawer',0,6);
INSERT INTO properties VALUES(14,134,'ROOT_DRAWER_APPEND','root','property_drawer',0,7);
INSERT INTO properties VALUES(15,134,'ROOT_DRAWER_APPEND','appended','property_drawer',1,8);
INSERT INTO properties VALUES(16,134,'ROOT_DRAWER_DUPLICATE','first','property_drawer',0,9);
INSERT INTO properties VALUES(17,134,'ROOT_DRAWER_DUPLICATE','second','property_drawer',0,10);
INSERT INTO properties VALUES(18,134,'ROOT_OVERRIDE_CHAIN','this','property_drawer',0,11);
INSERT INTO properties VALUES(19,134,'ROOT_OVERRIDE_CHAIN','is','property_drawer',0,12);
INSERT INTO properties VALUES(20,134,'ROOT_OVERRIDE_CHAIN','the','property_drawer',0,13);
INSERT INTO properties VALUES(21,134,'ROOT_OVERRIDE_CHAIN','root','property_drawer',0,14);
INSERT INTO properties VALUES(22,134,'TEST','old','property_drawer',0,15);
INSERT INTO properties VALUES(23,134,'TEST','append 1','property_drawer',1,16);
INSERT INTO properties VALUES(24,134,'TEST','new','property_drawer',0,17);
INSERT INTO properties VALUES(25,134,'TEST','append 2','property_drawer',1,18);
INSERT INTO properties VALUES(26,134,'CATEGORY','category_keyword_value','category_keyword',0,22);
INSERT INTO properties VALUES(27,134,'KEYWORD_APPEND','foo=1','property_keyword',0,23);
INSERT INTO properties VALUES(28,134,'KEYWORD_APPEND','bar=2','property_keyword',1,24);
INSERT INTO properties VALUES(29,134,'KEYWORD_APPEND','baz=3','property_keyword',1,25);
INSERT INTO properties VALUES(30,134,'KEYWORD_DUPLICATE','first','property_keyword',0,26);
INSERT INTO properties VALUES(31,134,'KEYWORD_DUPLICATE','second','property_keyword',0,27);
INSERT INTO properties VALUES(32,134,'KEYWORD_RESET','old','property_keyword',0,28);
INSERT INTO properties VALUES(33,134,'KEYWORD_RESET','appended-before-reset','property_keyword',1,29);
INSERT INTO properties VALUES(34,134,'KEYWORD_RESET','replacement','property_keyword',0,30);
INSERT INTO properties VALUES(35,134,'KEYWORD_RESET','appended-after-reset','property_keyword',1,31);
INSERT INTO properties VALUES(36,134,'KEYWORD_APPEND_ONLY','only','property_keyword',0,32);
INSERT INTO properties VALUES(37,134,'KEYWORD_APPEND_ONLY','appended','property_keyword',1,33);
INSERT INTO properties VALUES(38,134,'KEYWORD_EMPTY',NULL,'property_keyword',0,34);
INSERT INTO properties VALUES(39,134,'KEYWORD_EMPTY','valid','property_keyword',1,35);
INSERT INTO properties VALUES(40,134,'KEYWORD_VERY_EMPTY',NULL,'property_keyword',0,36);
INSERT INTO properties VALUES(41,135,'EMPTY_PROP','','property_drawer',0,40);
INSERT INTO properties VALUES(42,137,'VALUE','first','property_drawer',0,75);
INSERT INTO properties VALUES(43,137,'VALUE','second','property_drawer',1,76);
INSERT INTO properties VALUES(44,138,'VALUE','base','property_drawer',0,89);
INSERT INTO properties VALUES(45,138,'VALUE','one','property_drawer',1,90);
INSERT INTO properties VALUES(46,138,'VALUE','two','property_drawer',1,91);
INSERT INTO properties VALUES(47,139,'VALUE','only','property_drawer',1,103);
INSERT INTO properties VALUES(48,140,'VALUE','first','property_drawer',0,116);
INSERT INTO properties VALUES(49,140,'VALUE','second','property_drawer',0,117);
INSERT INTO properties VALUES(50,141,'VALUE','old','property_drawer',0,131);
INSERT INTO properties VALUES(51,141,'VALUE','appended-before-reset','property_drawer',1,132);
INSERT INTO properties VALUES(52,141,'VALUE','replacement','property_drawer',0,133);
INSERT INTO properties VALUES(53,141,'VALUE','appended-after-reset','property_drawer',1,134);
INSERT INTO properties VALUES(54,142,'VALUE','','property_drawer',0,146);
INSERT INTO properties VALUES(55,142,'VALUE','empty base followed by append','property_drawer',1,147);
INSERT INTO properties VALUES(56,143,'VALUE','base followed by empty append','property_drawer',0,159);
INSERT INTO properties VALUES(57,143,'VALUE','','property_drawer',1,160);
INSERT INTO properties VALUES(58,144,'VALUE','parent','property_drawer',0,172);
INSERT INTO properties VALUES(59,145,'VALUE','child','property_drawer',1,177);
INSERT INTO properties VALUES(60,147,'VALUE','child','property_drawer',0,199);
INSERT INTO properties VALUES(61,148,'VALUE','child','property_drawer',0,212);
INSERT INTO properties VALUES(62,148,'VALUE','appended','property_drawer',1,213);
INSERT INTO properties VALUES(63,149,'VALUE','parent','property_drawer',0,226);
INSERT INTO properties VALUES(64,149,'VALUE','appended','property_drawer',1,227);
INSERT INTO properties VALUES(65,151,'VALUE','child','property_drawer',0,241);
INSERT INTO properties VALUES(66,152,'VALUE','child','property_drawer',1,254);
INSERT INTO properties VALUES(67,153,'VALUE','grandparent','property_drawer',0,266);
INSERT INTO properties VALUES(68,154,'VALUE','parent','property_drawer',0,271);
INSERT INTO properties VALUES(69,156,'VALUE','child','property_drawer',1,286);
INSERT INTO properties VALUES(70,157,'VALUE','child','property_drawer',0,298);
INSERT INTO properties VALUES(71,160,'ROOT_DRAWER_BASE','child','property_drawer',1,321);
INSERT INTO properties VALUES(72,161,'ROOT_DRAWER_BASE','child','property_drawer',0,333);
INSERT INTO properties VALUES(73,164,'KEYWORD_APPEND','child=4','property_drawer',1,356);
INSERT INTO properties VALUES(74,165,'KEYWORD_APPEND','local=1','property_drawer',0,368);
INSERT INTO properties VALUES(75,166,'KEYWORD_APPEND','local=1','property_drawer',0,380);
INSERT INTO properties VALUES(76,166,'KEYWORD_APPEND','local=2','property_drawer',1,381);
INSERT INTO properties VALUES(77,167,'VALUE','appending before','property_drawer',1,393);
INSERT INTO properties VALUES(78,167,'VALUE','definition','property_drawer',0,394);
INSERT INTO properties VALUES(79,168,'DEFINED_TWICE','works','property_drawer',0,405);
INSERT INTO properties VALUES(80,168,'DEFINED_TWICE','second is effective','property_drawer',0,406);
INSERT INTO properties VALUES(81,169,'VALUE','first','property_drawer',0,421);
INSERT INTO properties VALUES(82,169,'VALUE','second','property_drawer',0,422);
INSERT INTO properties VALUES(83,169,'VALUE','appended','property_drawer',1,423);
INSERT INTO properties VALUES(84,170,'VALUE','first','property_drawer',0,434);
INSERT INTO properties VALUES(85,170,'VALUE','appended','property_drawer',1,435);
INSERT INTO properties VALUES(86,170,'VALUE','another','property_drawer',1,436);
INSERT INTO properties VALUES(87,170,'VALUE','second','property_drawer',0,437);
INSERT INTO properties VALUES(88,170,'VALUE','value','property_drawer',1,438);
INSERT INTO properties VALUES(89,171,'VALUE','first','property_drawer',0,452);
INSERT INTO properties VALUES(90,171,'VALUE','appended','property_drawer',1,453);
INSERT INTO properties VALUES(91,171,'VALUE','second','property_drawer',0,454);
INSERT INTO properties VALUES(92,172,'VALUE','first','property_drawer',0,465);
INSERT INTO properties VALUES(93,172,'VALUE','second','property_drawer',0,466);
INSERT INTO properties VALUES(94,174,'VALUE','child','property_drawer',1,480);
INSERT INTO properties VALUES(95,175,'VALUE','parent','property_drawer',0,492);
INSERT INTO properties VALUES(96,176,'VALUE','first child','property_drawer',0,497);
INSERT INTO properties VALUES(97,176,'VALUE','second child','property_drawer',0,498);
INSERT INTO properties VALUES(98,177,'MIXED_KEY','first','property_drawer',0,511);
INSERT INTO properties VALUES(99,177,'MIXED_KEY','second','property_drawer',1,512);
INSERT INTO properties VALUES(100,177,'MIXED_KEY','replacement','property_drawer',0,513);
INSERT INTO properties VALUES(101,177,'MIXED_KEY','final','property_drawer',1,514);
CREATE TABLE tags (
    heading_id      INTEGER NOT NULL,
    tag             TEXT NOT NULL,
    FOREIGN KEY (heading_id)
        REFERENCES headings(id)
        ON DELETE CASCADE,
    PRIMARY KEY (heading_id, tag)
);
INSERT INTO tags VALUES(184,'tag3_file_inherited');
INSERT INTO tags VALUES(185,'tag3_local_blue');
INSERT INTO tags VALUES(186,'tag3_any_red');
INSERT INTO tags VALUES(187,'tag3_any_green');
INSERT INTO tags VALUES(188,'tag3_all_gold');
INSERT INTO tags VALUES(188,'tag3_all_silver');
INSERT INTO tags VALUES(189,'tag3_parent_violet');
INSERT INTO tags VALUES(191,'tag3_parent_violet');
INSERT INTO tags VALUES(192,'tag3_regexp_482');
INSERT INTO tags VALUES(194,'tag3_control_black');
INSERT INTO tags VALUES(195,'tag3_file_inherited');
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
INSERT INTO links VALUES(1,10,53,80,139,5,'normal','bracket','[[file:hln5-target-file.org][hln5 unique file description]]','file:hln5-target-file.org','hln5 unique file description','file','hln5-target-file.org',NULL,'/home/hubisan/projects/coding/org-files-db/.project/manual-tests/query-model/org/headings/hln5-target-file.org',NULL,NULL,NULL,NULL,'broken','missing in indexed universe');
INSERT INTO links VALUES(2,10,54,171,239,8,'normal','bracket','[[https://example.invalid/hln5-unique][hln5 unique web description]]','https://example.invalid/hln5-unique','hln5 unique web description','https','//example.invalid/hln5-unique',NULL,NULL,NULL,NULL,NULL,NULL,'unsupported','unsupported link type');
INSERT INTO links VALUES(3,10,55,268,317,11,'normal','bracket','[[id:hln5-target-id][hln5 unique id description]]','id:hln5-target-id','hln5 unique id description','id','hln5-target-id',NULL,NULL,10,57,NULL,'hln5-target-id','resolved',NULL);
INSERT INTO links VALUES(4,14,75,141,188,5,'normal','bracket','[[#lfr7-target-a][lfr7 backlink A description]]','#lfr7-target-a','lfr7 backlink A description','custom-id','lfr7-target-a',NULL,NULL,14,77,'lfr7-target-a',NULL,'resolved',NULL);
INSERT INTO links VALUES(5,14,76,232,284,8,'normal','bracket','[[id:lfr7-target-id-b][lfr7 backlink B description]]','id:lfr7-target-id-b','lfr7 backlink B description','id','lfr7-target-id-b',NULL,NULL,14,78,NULL,'lfr7-target-id-b','resolved',NULL);
INSERT INTO links VALUES(6,15,81,90,145,5,'normal','bracket','[[#lto6-target-custom][lto6 custom target description]]','#lto6-target-custom','lto6 custom target description','custom-id','lto6-target-custom',NULL,NULL,15,84,'lto6-target-custom',NULL,'resolved',NULL);
INSERT INTO links VALUES(7,15,82,179,228,8,'normal','bracket','[[id:lto6-target-id][lto6 id target description]]','id:lto6-target-id','lto6 id target description','id','lto6-target-id',NULL,NULL,15,84,NULL,'lto6-target-id','resolved',NULL);
INSERT INTO links VALUES(8,15,83,272,325,11,'normal','bracket','[[#lto6-other-custom][lto6 other target description]]','#lto6-other-custom','lto6 other target description','custom-id','lto6-other-custom',NULL,NULL,15,85,'lto6-other-custom',NULL,'resolved',NULL);
INSERT INTO links VALUES(9,16,86,73,134,5,'normal','bracket','[[file:../../notes/org-semantics/multipe-title-keywords.org]]','file:../../notes/org-semantics/multipe-title-keywords.org',NULL,'file','../../notes/org-semantics/multipe-title-keywords.org',NULL,'/home/hubisan/projects/coding/org-files-db/.project/manual-tests/query-model/notes/org-semantics/multipe-title-keywords.org',NULL,NULL,NULL,NULL,'unresolved','outside indexed universe');
CREATE TABLE heading_bodies (
    heading_id          INTEGER PRIMARY KEY,
    body_text           TEXT NOT NULL,
    body_byte_start     INTEGER,
    body_byte_end       INTEGER,
    FOREIGN KEY (heading_id)
        REFERENCES headings(id)
        ON DELETE CASCADE
);
INSERT INTO heading_bodies VALUES(53,'[[file:hln5-target-file.org][hln5 unique file description]]',80,139);
INSERT INTO heading_bodies VALUES(54,'[[https://example.invalid/hln5-unique][hln5 unique web description]]',171,239);
INSERT INTO heading_bodies VALUES(55,'[[id:hln5-target-id][hln5 unique id description]]',268,317);
INSERT INTO heading_bodies VALUES(56,'Plain unique text hln5-control.',354,385);
INSERT INTO heading_bodies VALUES(59,replace('single match\nsome text and some more','\n',char(10)),67,103);
INSERT INTO heading_bodies VALUES(61,'multiple matches',158,174);
INSERT INTO heading_bodies VALUES(62,'multiple matches',209,225);
INSERT INTO heading_bodies VALUES(75,'[[#lfr7-target-a][lfr7 backlink A description]]',141,188);
INSERT INTO heading_bodies VALUES(76,'[[id:lfr7-target-id-b][lfr7 backlink B description]]',232,284);
INSERT INTO heading_bodies VALUES(81,'[[#lto6-target-custom][lto6 custom target description]]',90,145);
INSERT INTO heading_bodies VALUES(82,'[[id:lto6-target-id][lto6 id target description]]',179,228);
INSERT INTO heading_bodies VALUES(83,'[[#lto6-other-custom][lto6 other target description]]',272,325);
INSERT INTO heading_bodies VALUES(86,'See [[file:../../notes/org-semantics/multipe-title-keywords.org]]',69,134);
INSERT INTO heading_bodies VALUES(87,replace('This can be proven by using ~org-latex-export-as-latex~:\n\n#+BEGIN_SRC latex\n  \title{Title can span multiple lines, even here}\n#+END_SRC','\n',char(10)),NULL,NULL);
INSERT INTO heading_bodies VALUES(124,'DEADLINE: <2037-08-31 Mon 16:00>',334,366);
INSERT INTO heading_bodies VALUES(125,'<2037-08-17 Mon>',420,436);
INSERT INTO heading_bodies VALUES(126,'Plain unique text pln9-control.',478,509);
INSERT INTO heading_bodies VALUES(135,replace(':PROPERTIES:\n:EMPTY_PROP:\n:END:','\n',char(10)),1082,1113);
INSERT INTO heading_bodies VALUES(136,replace('The synthetic level-0 file/root object should have these direct effective properties:\n\n  src_elisp{(org-entry-get nil "TEST" t)} {{{results(=new append 1 append 2=)}}}\n  src_elisp{(org-entry-get nil "KEYWORD_RESET" t)} {{{results(=replacement appended-after-reset=)}}}\n\n- ~ROOT_DRAWER_BASE = root~\n  src_elisp{(org-entry-get nil "ROOT_DRAWER_BASE" t)} {{{results(=root=)}}}\n- ~ROOT_DRAWER_APPEND = root appended~\n  src_elisp{(org-entry-get nil "ROOT_DRAWER_APPEND" t)} {{{results(=root appended=)}}}\n- ~ROOT_DRAWER_DUPLICATE = second~\n  src_elisp{(org-entry-get nil "ROOT_DRAWER_DUPLICATE" t)} {{{results(=second=)}}}\n- ~ROOT_OVERRIDE_CHAIN = root~\n  src_elisp{(org-entry-get nil "ROOT_OVERRIDE_CHAIN" t)} {{{results(=root=)}}}  \n- ~KEYWORD_APPEND = foo=1 bar=2 baz=3~\n  src_elisp{(org-entry-get nil "KEYWORD_APPEND" t)} {{{results(=foo=1 bar=2 baz=3=)}}}   \n- ~KEYWORD_DUPLICATE = second~\n  src_elisp{(org-entry-get nil "KEYWORD_DUPLICATE" t)} {{{results(=second=)}}}\n- ~KEYWORD_RESET = replacement appended-after-reset~\n  src_elisp{(org-entry-get nil "KEYWORD_RESET" t)} {{{results(=replacement appended-after-reset=)}}}\n- ~KEYWORD_APPEND_ONLY = only appended~\n  src_elisp{(org-entry-get nil "KEYWORD_APPEND_ONLY" t)} {{{results(=only appended=)}}}\n- ~KEYWORD_EMPTY = valid~\n  src_elisp{(org-entry-get nil "KEYWORD_EMPTY" t)} {{{results(=valid=)}}}\n\nWith ~:inherit nil~, these values match only the file/root object.\n\nWith ~:inherit t~, real headings may inherit these values unless a nearer heading defines the same property.','\n',char(10)),1144,2672);
INSERT INTO heading_bodies VALUES(137,replace('Expected:\n\n- ~:inherit nil~ gives ~VALUE = first second~\n  src_elisp{(org-entry-get nil "VALUE" t)} {{{results(=first second=)}}}\n- ~:inherit t~ gives ~VALUE = first second~\n  src_elisp{(org-entry-get nil "VALUE" nil)} {{{results(=first second=)}}}\n- the local base value and local append value are combined in source order','\n',char(10)),2759,3082);
INSERT INTO heading_bodies VALUES(138,replace('Expected:\n\n- ~:inherit nil~ gives ~VALUE = base one two~\n  src_elisp{(org-entry-get nil "VALUE" t)} {{{results(=base one two=)}}}\n- ~:inherit t~ gives ~VALUE = base one two~\n  src_elisp{(org-entry-get nil "VALUE" nil)} {{{results(=base one two=)}}}','\n',char(10)),3172,3420);
INSERT INTO heading_bodies VALUES(139,replace('Expected:\n\n- ~:inherit nil~ gives ~VALUE = only~\n  src_elisp{(org-entry-get nil "VALUE" nil)} {{{results(=only=)}}}\n- ~:inherit t~ gives ~VALUE = only~, unless an inherited base value is available\n  src_elisp{(org-entry-get nil "VALUE" t)} {{{results(=only=)}}}\n- this heading has no ancestor definition for ~VALUE~, so the result remains ~only~','\n',char(10)),3492,3837);
INSERT INTO heading_bodies VALUES(140,replace('Expected:\n\n- ~:inherit nil~ gives ~VALUE = second~\n  src_elisp{(org-entry-get nil "VALUE" nil)} {{{results(=second=)}}}\n- ~:inherit t~ gives ~VALUE = second~\n  src_elisp{(org-entry-get nil "VALUE" nil)} {{{results(=second=)}}}\n- the lower non-append definition wins\n- both raw property rows remain stored','\n',char(10)),3945,4249);
INSERT INTO heading_bodies VALUES(141,replace('Expected:\n\n- ~VALUE = replacement appended-after-reset~\n  src_elisp{(org-entry-get nil "VALUE" t)} {{{results(=replacement appended-before-reset appended-after-reset=)}}}\n  src_elisp{(org-entry-get nil "VALUE" nil)} {{{results(=replacement appended-before-reset appended-after-reset=)}}}\n- the later non-append row resets the previously accumulated value','\n',char(10)),4403,4757);
INSERT INTO heading_bodies VALUES(142,replace(':PROPERTIES:\n:VALUE:\n:VALUE+: empty base followed by append\n:END:\n\nExpected:\n\n- ~VALUE = valid~\n  src_elisp{(org-entry-get nil "VALUE" t)} {{{results(= valid=)}}}\n  src_elisp{(org-entry-get nil "VALUE" nil)} {{{results(= valid=)}}}\n- no leading separator space is added','\n',char(10)),4791,5060);
INSERT INTO heading_bodies VALUES(143,replace(':PROPERTIES:\n:VALUE: base followed by empty append\n:VALUE+:\n:END:\n\nExpected:\n\n- ~VALUE = valid~\n  src_elisp{(org-entry-get nil "VALUE" t)} {{{results(=valid =)}}}\n  src_elisp{(org-entry-get nil "VALUE" nil)} {{{results(=valid =)}}}\n- no trailing separator space is added','\n',char(10)),5094,5364);
INSERT INTO heading_bodies VALUES(145,replace('Expected for the child:\n\n- ~:inherit nil~ gives ~VALUE = child~\n  src_elisp{(org-entry-get nil "VALUE" nil)} {{{results(=child=)}}}\n- ~:inherit t~ gives ~VALUE = parent child~\n  src_elisp{(org-entry-get nil "VALUE" t)} {{{results(=parent child=)}}}\n- the inherited parent value becomes the base for the local append value','\n',char(10)),5490,5811);
INSERT INTO heading_bodies VALUES(146,replace('Expected:\n\n- ~:inherit nil~ finds no direct ~VALUE~\n  src_elisp{(org-entry-get nil "VALUE" nil)} \n- ~:inherit t~ gives ~VALUE = parent~\n  src_elisp{(org-entry-get nil "VALUE" t)} {{{results(=parent=)}}}','\n',char(10)),5843,6045);
INSERT INTO heading_bodies VALUES(147,replace('Expected:\n\n- ~:inherit nil~ gives ~VALUE = child~\n  src_elisp{(org-entry-get nil "VALUE" nil)} {{{results(=child=)}}} \n- ~:inherit t~ gives ~VALUE = child~\n  src_elisp{(org-entry-get nil "VALUE" t)} {{{results(=child=)}}} \n- the local non-append definition replaces the inherited parent value','\n',char(10)),6113,6405);
INSERT INTO heading_bodies VALUES(148,replace('Expected:\n\n- ~:inherit nil~ gives ~VALUE = child appended~\n  src_elisp{(org-entry-get nil "VALUE" nil)} {{{results(=child appended=)}}} \n- ~:inherit t~ gives ~VALUE = child appended~\n  src_elisp{(org-entry-get nil "VALUE" t)} {{{results(=child appended=)}}} \n- the local base prevents the parent value from participating','\n',char(10)),6502,6822);
INSERT INTO heading_bodies VALUES(150,replace('Expected:\n\n- ~:inherit nil~ finds no direct ~VALUE~\n  src_elisp{(org-entry-get nil "VALUE" nil)}  \n- ~:inherit t~ gives ~VALUE = parent appended~\n  src_elisp{(org-entry-get nil "VALUE" t)} {{{results(=parent appended=)}}}','\n',char(10)),6937,7158);
INSERT INTO heading_bodies VALUES(151,replace('Expected:\n\n- ~:inherit nil~ gives ~VALUE = child~\n  src_elisp{(org-entry-get nil "VALUE" nil)} {{{results(=child=)}}}  \n- ~:inherit t~ gives ~VALUE = child~\n  src_elisp{(org-entry-get nil "VALUE" t)} {{{results(=child=)}}}  \n- the child does not receive ~parent appended child~','\n',char(10)),7238,7515);
INSERT INTO heading_bodies VALUES(152,replace('Expected:\n\n- ~:inherit nil~ gives ~VALUE = child~\n  src_elisp{(org-entry-get nil "VALUE" nil)} {{{results(=child=)}}}  \n- ~:inherit t~ gives ~VALUE = parent appended child~\n  src_elisp{(org-entry-get nil "VALUE" t)} {{{results(=parent appended child=)}}}','\n',char(10)),7596,7850);
INSERT INTO heading_bodies VALUES(155,replace('Expected:\n\n- ~:inherit nil~ finds no direct ~VALUE~\n  src_elisp{(org-entry-get nil "VALUE" nil)}   \n- ~:inherit t~ gives ~VALUE = parent~\n  src_elisp{(org-entry-get nil "VALUE" t)} {{{results(=parent=)}}}   \n- the grandparent value is hidden by the nearer parent definition','\n',char(10)),8008,8281);
INSERT INTO heading_bodies VALUES(156,replace('Expected:\n\n- ~:inherit nil~ gives ~VALUE = child~\n  src_elisp{(org-entry-get nil "VALUE" nil)} {{{results(=child=)}}}   \n- ~:inherit t~ gives ~VALUE = parent child~\n  src_elisp{(org-entry-get nil "VALUE" t)} {{{results(=parent child=)}}}','\n',char(10)),8355,8592);
INSERT INTO heading_bodies VALUES(157,replace('Expected:\n\n- ~:inherit nil~ gives ~VALUE = child~\n  src_elisp{(org-entry-get nil "VALUE" nil)} {{{results(=child=)}}}   \n- ~:inherit t~ gives ~VALUE = child~\n  src_elisp{(org-entry-get nil "VALUE" t)} {{{results(=child=)}}}','\n',char(10)),8665,8888);
INSERT INTO heading_bodies VALUES(159,replace('Expected:\n\n- ~:inherit nil~ finds no direct ~ROOT_DRAWER_BASE~\n  src_elisp{(org-entry-get nil "ROOT_DRAWER_BASE" nil)}     \n- ~:inherit t~ gives ~ROOT_DRAWER_BASE = root~\n  src_elisp{(org-entry-get nil "ROOT_DRAWER_BASE" t)} {{{results(=root=)}}}','\n',char(10)),8958,9204);
INSERT INTO heading_bodies VALUES(160,replace('Expected:\n\n- ~:inherit nil~ gives ~ROOT_DRAWER_BASE = child~\n  src_elisp{(org-entry-get nil "ROOT_DRAWER_BASE" nil)} {{{results(=child=)}}}     \n- ~:inherit t~ gives ~ROOT_DRAWER_BASE = root child~\n  src_elisp{(org-entry-get nil "ROOT_DRAWER_BASE" t)} {{{results(=root child=)}}}','\n',char(10)),9295,9574);
INSERT INTO heading_bodies VALUES(161,replace('Expected:\n\n- ~:inherit nil~ gives ~ROOT_DRAWER_BASE = child~\n  src_elisp{(org-entry-get nil "ROOT_DRAWER_BASE" nil)} {{{results(=child=)}}}     \n- ~:inherit t~ gives ~ROOT_DRAWER_BASE = child~\n  src_elisp{(org-entry-get nil "ROOT_DRAWER_BASE" t)} {{{results(=child=)}}}','\n',char(10)),9661,9930);
INSERT INTO heading_bodies VALUES(163,replace('Expected:\n\n- ~:inherit nil~ finds no direct ~KEYWORD_APPEND~\n  src_elisp{(org-entry-get nil "KEYWORD_APPEND" nil)}      \n- ~:inherit t~ gives ~KEYWORD_APPEND = foo=1 bar=2 baz=3~\n  src_elisp{(org-entry-get nil "KEYWORD_APPEND" t)} {{{results(=foo=1 bar=2 baz=3=)}}}','\n',char(10)),10003,10268);
INSERT INTO heading_bodies VALUES(164,replace('Expected:\n\n- ~:inherit nil~ gives ~KEYWORD_APPEND = child=4~\n  src_elisp{(org-entry-get nil "KEYWORD_APPEND" nil)} {{{results(=child=4=)}}} {{{results(=foo=1 bar=2 \n- ~:inherit t~ gives ~KEYWORD_APPEND = foo=1 bar=2 baz=3 child=4~\n  src_elisp{(org-entry-get nil "KEYWORD_APPEND" t)} {{{results(=foo=1 bar=2 baz=3 child=4=)}}} {{{results(=foo=1 bar=2 baz=3','\n',char(10)),10357,10712);
INSERT INTO heading_bodies VALUES(165,replace('Expected:\n\n- ~:inherit nil~ gives ~KEYWORD_APPEND = local=1~\n  src_elisp{(org-entry-get nil "KEYWORD_APPEND" nil)} {{{results(=local=1=)}}} \n- ~:inherit t~ gives ~KEYWORD_APPEND = local=1~\n  src_elisp{(org-entry-get nil "KEYWORD_APPEND" t)} {{{results(=local=1=)}}}','\n',char(10)),10792,11057);
INSERT INTO heading_bodies VALUES(166,replace('Expected:\n\n- ~:inherit nil~ gives ~KEYWORD_APPEND = local=1 local=2~\n  src_elisp{(org-entry-get nil "KEYWORD_APPEND" nil)} {{{results(=local=1 local=2=)}}} \n- ~:inherit t~ gives ~KEYWORD_APPEND = local=1 local=2~\n  src_elisp{(org-entry-get nil "KEYWORD_APPEND" t)} {{{results(=local=1 local=2=)}}}','\n',char(10)),11177,11474);
INSERT INTO heading_bodies VALUES(167,replace('Expected:\n\n- ~VALUE = second appended~\n  src_elisp{(org-entry-get nil "VALUE" nil)} {{{results(=definition appending before=)}}} \n  src_elisp{(org-entry-get nil "VALUE" t)} {{{results(=definition appending before=)}}}','\n',char(10)),11558,11775);
INSERT INTO heading_bodies VALUES(168,replace('Expected:\n\n- both rows are preserved in raw storage\n- ~:inherit nil~ gives ~DEFINED_TWICE = second is effective~\n  src_elisp{(org-entry-get nil "DEFINED_TWICE" nil)} {{{results(=second is effective=)}}} \n- ~:inherit t~ gives ~DEFINED_TWICE = second is effective~\n  src_elisp{(org-entry-get nil "DEFINED_TWICE" t)} {{{results(=second is effective=)}}} {{{results(=second is \n- an exact query for ~works~ does not match\n- an exact query for ~second is effective~ matches','\n',char(10)),11903,12371);
INSERT INTO heading_bodies VALUES(169,replace('Expected:\n\n- ~VALUE = second appended~\n  src_elisp{(org-entry-get nil "VALUE" nil)} {{{results(=second appended=)}}} \n  src_elisp{(org-entry-get nil "VALUE" t)} {{{results(=second appended=)}}}','\n',char(10)),12482,12675);
INSERT INTO heading_bodies VALUES(170,replace('Expected:\n\n- the last non-append definition wins as the base value\n- all append definitions at the same location are added to that winning base value\n- ~:inherit nil~ gives ~VALUE = second appended another value~\n  src_elisp{(org-entry-get nil "VALUE" nil)} {{{results(=second appended another value=)}}} \n- ~:inherit t~ gives ~VALUE = second appended another value\n  src_elisp{(org-entry-get nil "VALUE" t)} {{{results(=second appended another value=)}}}','\n',char(10)),12820,13275);
INSERT INTO heading_bodies VALUES(171,replace('Expected:\n\n- ~VALUE = second appended~\n  src_elisp{(org-entry-get nil "VALUE" nil)} {{{results(=second appended=)}}} \n  src_elisp{(org-entry-get nil "VALUE" t)} {{{results(=second appended=)}}}','\n',char(10)),13388,13581);
INSERT INTO heading_bodies VALUES(173,replace('Expected:\n\n- ~:inherit nil~ finds no direct ~VALUE~\n  src_elisp{(org-entry-get nil "VALUE" nil)}   \n- ~:inherit t~ gives ~VALUE = second~\n  src_elisp{(org-entry-get nil "VALUE" t)} {{{results(=second=)}}}','\n',char(10)),13707,13911);
INSERT INTO heading_bodies VALUES(174,replace('Expected:\n\n- ~:inherit nil~ gives ~VALUE = child~\n  src_elisp{(org-entry-get nil "VALUE" nil)} {{{results(=child=)}}}   \n- ~:inherit t~ gives ~VALUE = second child~\n  src_elisp{(org-entry-get nil "VALUE" t)} {{{results(=second child=)}}}','\n',char(10)),13994,14231);
INSERT INTO heading_bodies VALUES(176,replace('Expected:\n\n- ~:inherit nil~ gives ~VALUE = second child~\n  src_elisp{(org-entry-get nil "VALUE" nil)} {{{results(=second child=)}}}   \n- ~:inherit t~ gives ~VALUE = second child~\n  src_elisp{(org-entry-get nil "VALUE" t)} {{{results(=second child=)}}}   \n- the parent value does not participate','\n',char(10)),14430,14724);
INSERT INTO heading_bodies VALUES(177,replace('Expected after key normalization:\n\n- one effective key ~MIXED_KEY~\n  src_elisp{(org-entry-get nil "MIXED_KEY" nil)} {{{results(=replacement second final=)}}}   \n  src_elisp{(org-entry-get nil "mixed_key" nil)} {{{results(=replacement second final=)}}}\n- effective value ~replacement final~','\n',char(10)),14854,15143);
INSERT INTO heading_bodies VALUES(185,'src_elisp{(message "%s" (org-get-tags))} {{{results(=(tag3-file-inherited tag3_local_blue)=)}}}',167,262);
INSERT INTO heading_bodies VALUES(186,'src_elisp{(message "%s" (org-get-tags))} {{{results(=(tag3-file-inherited tag3_any_red)=)}}}',348,440);
INSERT INTO heading_bodies VALUES(187,'src_elisp{(message "%s" (org-get-tags))} {{{results(=(tag3-file-inherited tag3_any_green)=)}}}',528,622);
INSERT INTO heading_bodies VALUES(188,'src_elisp{(message "%s" (org-get-tags))} {{{results(=(tag3-file-inherited tag3_all_gold tag3_all_silver)=)}}}',723,832);
INSERT INTO heading_bodies VALUES(189,'src_elisp{(message "%s" (org-get-tags))} {{{results(=(tag3-file-inherited tag3_parent_violet)=)}}}',924,1022);
INSERT INTO heading_bodies VALUES(190,'src_elisp{(message "%s" (org-get-tags))} {{{results(=(tag3-file-inherited tag3_parent_violet)=)}}}',1058,1156);
INSERT INTO heading_bodies VALUES(191,'src_elisp{(message "%s" (org-get-tags))} {{{results(=(tag3-file-inherited tag3_parent_violet)=)}}}',1249,1347);
INSERT INTO heading_bodies VALUES(192,'src_elisp{(message "%s" (org-get-tags))} {{{results(=(tag3-file-inherited tag3_regexp_482)=)}}}',1436,1531);
INSERT INTO heading_bodies VALUES(193,'src_elisp{(message "%s" (org-get-tags))} {{{results(=(tag3-file-inherited)=)}}}',1568,1647);
INSERT INTO heading_bodies VALUES(194,'src_elisp{(message "%s" (org-get-tags))} {{{results(=(tag3-file-inherited tag3_control_black)=)}}}',1739,1837);
INSERT INTO heading_bodies VALUES(202,'Default TODO is not valid because file-local TODO lines override defaults.',208,282);
INSERT INTO heading_bodies VALUES(203,'Default DONE is not valid because file-local TODO lines override defaults.',333,407);
INSERT INTO heading_bodies VALUES(226,'<2032-03-12 Fri>',82,98);
INSERT INTO heading_bodies VALUES(227,'<2032-03-12 Fri 08:20>',130,152);
INSERT INTO heading_bodies VALUES(228,'<2032-03-01 Mon>',186,202);
INSERT INTO heading_bodies VALUES(229,'<2032-03-31 Wed 23:10>',234,256);
INSERT INTO heading_bodies VALUES(230,'[2032-03-12 Fri]',304,320);
INSERT INTO heading_bodies VALUES(232,'[2033-04-13 Wed]',86,102);
INSERT INTO heading_bodies VALUES(233,'[2033-04-13 Wed 16:40]',136,158);
INSERT INTO heading_bodies VALUES(234,'[2033-04-01 Fri]',194,210);
INSERT INTO heading_bodies VALUES(235,'[2033-04-30 Sat 21:55]',244,266);
INSERT INTO heading_bodies VALUES(236,'<2033-04-13 Wed>',314,330);
INSERT INTO heading_bodies VALUES(238,'<2031-01-10 Fri>',75,91);
INSERT INTO heading_bodies VALUES(239,'<2031-01-10 Fri 09:15>',123,145);
INSERT INTO heading_bodies VALUES(240,'[2031-01-11 Sat]',179,195);
INSERT INTO heading_bodies VALUES(241,'[2031-01-11 Sat 14:45]',229,251);
INSERT INTO heading_bodies VALUES(242,'<2031-02-01 Sat> and [2031-02-20 Thu 18:30]',291,334);
INSERT INTO heading_bodies VALUES(243,'Plain unique text tsp3-control.',370,401);
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
INSERT INTO outline_path VALUES(1,1,NULL,0,'0000','["ancestors-fixture-anc3"]');
INSERT INTO outline_path VALUES(2,1,1,1,'0000.0001','["ancestors-fixture-anc3","ancestors: Tagged Ancestor anc3-root"]');
INSERT INTO outline_path VALUES(3,1,2,2,'0000.0001.0001','["ancestors-fixture-anc3","ancestors: Tagged Ancestor anc3-root","ancestors: Middle anc3-mid"]');
INSERT INTO outline_path VALUES(4,1,3,3,'0000.0001.0001.0001','["ancestors-fixture-anc3","ancestors: Tagged Ancestor anc3-root","ancestors: Middle anc3-mid","ancestors: Deep Descendant anc3-a"]');
INSERT INTO outline_path VALUES(5,1,1,1,'0000.0002','["ancestors-fixture-anc3","ancestors: Property Ancestor anc3-prop"]');
INSERT INTO outline_path VALUES(6,1,5,2,'0000.0002.0001','["ancestors-fixture-anc3","ancestors: Property Ancestor anc3-prop","ancestors: Property Descendant anc3-b"]');
INSERT INTO outline_path VALUES(7,1,1,1,'0000.0003','["ancestors-fixture-anc3","ancestors: Top Level No Ancestor anc3-c"]');
INSERT INTO outline_path VALUES(8,2,NULL,0,'0000','["children-fixture-chd2"]');
INSERT INTO outline_path VALUES(9,2,8,1,'0000.0001','["children-fixture-chd2","children: Has Direct Child chd2-a"]');
INSERT INTO outline_path VALUES(10,2,9,2,'0000.0001.0001','["children-fixture-chd2","children: Has Direct Child chd2-a","children: Direct TODO Child chd2-child-a"]');
INSERT INTO outline_path VALUES(11,2,8,1,'0000.0002','["children-fixture-chd2","children: Has Nested NEXT chd2-b"]');
INSERT INTO outline_path VALUES(12,2,11,2,'0000.0002.0001','["children-fixture-chd2","children: Has Nested NEXT chd2-b","children: Intermediate Child chd2-mid"]');
INSERT INTO outline_path VALUES(13,2,12,3,'0000.0002.0001.0001','["children-fixture-chd2","children: Has Nested NEXT chd2-b","children: Intermediate Child chd2-mid","children: Grandchild NEXT chd2-grand"]');
INSERT INTO outline_path VALUES(14,2,8,1,'0000.0003','["children-fixture-chd2","children: No Children chd2-c"]');
INSERT INTO outline_path VALUES(15,2,8,1,'0000.0004','["children-fixture-chd2","children: Direct DONE Only chd2-d"]');
INSERT INTO outline_path VALUES(16,2,15,2,'0000.0004.0001','["children-fixture-chd2","children: Direct DONE Only chd2-d","children: Direct DONE Child chd2-child-d"]');
INSERT INTO outline_path VALUES(17,3,NULL,0,'0000','["closed-fixture-cls8"]');
INSERT INTO outline_path VALUES(18,3,17,1,'0000.0001','["closed-fixture-cls8","closed: Date Only cls8-a"]');
INSERT INTO outline_path VALUES(19,3,17,1,'0000.0002','["closed-fixture-cls8","closed: With Time cls8-b"]');
INSERT INTO outline_path VALUES(20,3,17,1,'0000.0003','["closed-fixture-cls8","closed: Range Earlier cls8-c"]');
INSERT INTO outline_path VALUES(21,3,17,1,'0000.0004','["closed-fixture-cls8","closed: Range Later cls8-d"]');
INSERT INTO outline_path VALUES(22,3,17,1,'0000.0005','["closed-fixture-cls8","closed: No Closed Negative Control cls8-e"]');
INSERT INTO outline_path VALUES(23,4,NULL,0,'0000','["deadline-fixture-dln6"]');
INSERT INTO outline_path VALUES(24,4,23,1,'0000.0001','["deadline-fixture-dln6","deadline: Date Only dln6-a"]');
INSERT INTO outline_path VALUES(25,4,23,1,'0000.0002','["deadline-fixture-dln6","deadline: With Time dln6-b"]');
INSERT INTO outline_path VALUES(26,4,23,1,'0000.0003','["deadline-fixture-dln6","deadline: Range Earlier dln6-c"]');
INSERT INTO outline_path VALUES(27,4,23,1,'0000.0004','["deadline-fixture-dln6","deadline: Range Later dln6-d"]');
INSERT INTO outline_path VALUES(28,4,23,1,'0000.0005','["deadline-fixture-dln6","deadline: Scheduled Negative Control dln6-e"]');
INSERT INTO outline_path VALUES(29,5,NULL,0,'0000','["descendants-fixture-des4"]');
INSERT INTO outline_path VALUES(30,5,29,1,'0000.0001','["descendants-fixture-des4","descendants: Direct NEXT Descendant des4-a"]');
INSERT INTO outline_path VALUES(31,5,30,2,'0000.0001.0001','["descendants-fixture-des4","descendants: Direct NEXT Descendant des4-a","descendants: Direct NEXT Child des4-child-a"]');
INSERT INTO outline_path VALUES(32,5,29,1,'0000.0002','["descendants-fixture-des4","descendants: Deep Blocked Descendant des4-b"]');
INSERT INTO outline_path VALUES(33,5,32,2,'0000.0002.0001','["descendants-fixture-des4","descendants: Deep Blocked Descendant des4-b","descendants: Intermediate des4-mid"]');
INSERT INTO outline_path VALUES(34,5,33,3,'0000.0002.0001.0001','["descendants-fixture-des4","descendants: Deep Blocked Descendant des4-b","descendants: Intermediate des4-mid","descendants: Deep Blocked TODO des4-deep                         :des4-blocked:"]');
INSERT INTO outline_path VALUES(35,5,29,1,'0000.0003','["descendants-fixture-des4","descendants: DONE Descendant Only des4-c"]');
INSERT INTO outline_path VALUES(36,5,35,2,'0000.0003.0001','["descendants-fixture-des4","descendants: DONE Descendant Only des4-c","descendants: Done Child des4-child-c"]');
INSERT INTO outline_path VALUES(37,5,29,1,'0000.0004','["descendants-fixture-des4","descendants: No Descendants des4-d"]');
INSERT INTO outline_path VALUES(38,6,NULL,0,'0000','["done-fixture-dne4"]');
INSERT INTO outline_path VALUES(39,6,38,1,'0000.0001','["done-fixture-dne4","done: Done Keyword dne4-a"]');
INSERT INTO outline_path VALUES(40,6,38,1,'0000.0002','["done-fixture-dne4","done: Cancelled Keyword dne4-b"]');
INSERT INTO outline_path VALUES(41,6,38,1,'0000.0003','["done-fixture-dne4","done: Open Negative Control dne4-c"]');
INSERT INTO outline_path VALUES(42,6,38,1,'0000.0004','["done-fixture-dne4","done: No Keyword Negative Control dne4-d"]');
INSERT INTO outline_path VALUES(43,7,NULL,0,'0000','["file-modified-fixture-fmd8"]');
INSERT INTO outline_path VALUES(44,7,43,1,'0000.0001','["file-modified-fixture-fmd8","file-modified: Modified Time Match fmd8-a"]');
INSERT INTO outline_path VALUES(45,7,43,1,'0000.0002','["file-modified-fixture-fmd8","file-modified: Modified Time Second Match fmd8-b"]');
INSERT INTO outline_path VALUES(46,8,NULL,0,'0000','["file-name-fixture-fnm3"]');
INSERT INTO outline_path VALUES(47,8,46,1,'0000.0001','["file-name-fixture-fnm3","file-name: Filename Match fnm3-a"]');
INSERT INTO outline_path VALUES(48,8,46,1,'0000.0002','["file-name-fixture-fnm3","file-name: Filename Second Match fnm3-b"]');
INSERT INTO outline_path VALUES(49,9,NULL,0,'0000','["Effective File Title FTL6 Unique"]');
INSERT INTO outline_path VALUES(50,9,49,1,'0000.0001','["Effective File Title FTL6 Unique","file-title: Effective Title Match ftl6-a"]');
INSERT INTO outline_path VALUES(51,9,49,1,'0000.0002','["Effective File Title FTL6 Unique","file-title: Effective Title Second Match ftl6-b"]');
INSERT INTO outline_path VALUES(52,10,NULL,0,'0000','["has-link-fixture-hln5"]');
INSERT INTO outline_path VALUES(53,10,52,1,'0000.0001','["has-link-fixture-hln5","has-link: File Link hln5-a"]');
INSERT INTO outline_path VALUES(54,10,52,1,'0000.0002','["has-link-fixture-hln5","has-link: HTTPS Link hln5-b"]');
INSERT INTO outline_path VALUES(55,10,52,1,'0000.0003','["has-link-fixture-hln5","has-link: ID Link hln5-c"]');
INSERT INTO outline_path VALUES(56,10,52,1,'0000.0004','["has-link-fixture-hln5","has-link: No Link Control hln5-d"]');
INSERT INTO outline_path VALUES(57,10,52,1,'0000.0005','["has-link-fixture-hln5","has-link: Target Heading hln5-target"]');
INSERT INTO outline_path VALUES(58,11,NULL,0,'0000','["has-text"]');
INSERT INTO outline_path VALUES(59,11,58,1,'0000.0001','["has-text","has-text: Single Has Text"]');
INSERT INTO outline_path VALUES(60,11,58,1,'0000.0002','["has-text","Multiple Machtes"]');
INSERT INTO outline_path VALUES(61,11,60,2,'0000.0002.0001','["has-text","Multiple Machtes","has-text: Multiple Matches 1"]');
INSERT INTO outline_path VALUES(62,11,60,2,'0000.0002.0002','["has-text","Multiple Machtes","has-text: Multiple Matches 2"]');
INSERT INTO outline_path VALUES(63,12,NULL,0,'0000','["keyword-fixture-kwd7"]');
INSERT INTO outline_path VALUES(64,12,63,1,'0000.0001','["keyword-fixture-kwd7","keyword: First Heading kwd7-a"]');
INSERT INTO outline_path VALUES(65,12,63,1,'0000.0002','["keyword-fixture-kwd7","keyword: Second Heading kwd7-b"]');
INSERT INTO outline_path VALUES(66,12,65,2,'0000.0002.0001','["keyword-fixture-kwd7","keyword: Second Heading kwd7-b","keyword: Nested Heading kwd7-c"]');
INSERT INTO outline_path VALUES(67,13,NULL,0,'0000','["level-fixture-lvl8"]');
INSERT INTO outline_path VALUES(68,13,67,1,'0000.0001','["level-fixture-lvl8","level: Level 1 lvl8-a"]');
INSERT INTO outline_path VALUES(69,13,68,2,'0000.0001.0001','["level-fixture-lvl8","level: Level 1 lvl8-a","level: Level 2 lvl8-b"]');
INSERT INTO outline_path VALUES(70,13,69,3,'0000.0001.0001.0001','["level-fixture-lvl8","level: Level 1 lvl8-a","level: Level 2 lvl8-b","level: Level 3 lvl8-c"]');
INSERT INTO outline_path VALUES(71,13,70,4,'0000.0001.0001.0001.0001','["level-fixture-lvl8","level: Level 1 lvl8-a","level: Level 2 lvl8-b","level: Level 3 lvl8-c","level: Level 4 lvl8-d"]');
INSERT INTO outline_path VALUES(72,13,67,1,'0000.0002','["level-fixture-lvl8","level: Second Level 1 lvl8-e"]');
INSERT INTO outline_path VALUES(73,13,72,2,'0000.0002.0001','["level-fixture-lvl8","level: Second Level 1 lvl8-e","level: Second Level 2 lvl8-f"]');
INSERT INTO outline_path VALUES(74,14,NULL,0,'0000','["linked-from-fixture-lfr7"]');
INSERT INTO outline_path VALUES(75,14,74,1,'0000.0001','["linked-from-fixture-lfr7","linked-from: Tagged Source lfr7-source-a"]');
INSERT INTO outline_path VALUES(76,14,74,1,'0000.0002','["linked-from-fixture-lfr7","linked-from: Plain Source lfr7-source-b"]');
INSERT INTO outline_path VALUES(77,14,74,1,'0000.0003','["linked-from-fixture-lfr7","linked-from: Target With Backlink A lfr7-target-a"]');
INSERT INTO outline_path VALUES(78,14,74,1,'0000.0004','["linked-from-fixture-lfr7","linked-from: Target With Backlink B lfr7-target-b"]');
INSERT INTO outline_path VALUES(79,14,74,1,'0000.0005','["linked-from-fixture-lfr7","linked-from: Target Without Backlink lfr7-target-c"]');
INSERT INTO outline_path VALUES(80,15,NULL,0,'0000','["links-to-fixture-lto6"]');
INSERT INTO outline_path VALUES(81,15,80,1,'0000.0001','["links-to-fixture-lto6","links-to: Source To Custom ID lto6-a"]');
INSERT INTO outline_path VALUES(82,15,80,1,'0000.0002','["links-to-fixture-lto6","links-to: Source To ID lto6-b"]');
INSERT INTO outline_path VALUES(83,15,80,1,'0000.0003','["links-to-fixture-lto6","links-to: Source To Other Target lto6-c"]');
INSERT INTO outline_path VALUES(84,15,80,1,'0000.0004','["links-to-fixture-lto6","links-to: Target Heading lto6-target"]');
INSERT INTO outline_path VALUES(85,15,80,1,'0000.0005','["links-to-fixture-lto6","links-to: Other Target Heading lto6-other"]');
INSERT INTO outline_path VALUES(86,16,NULL,0,'0000','["Title can span multiple lines, even here"]');
INSERT INTO outline_path VALUES(87,16,86,1,'0000.0001','["Title can span multiple lines, even here","Unfortunately Everywhere"]');
INSERT INTO outline_path VALUES(88,17,NULL,0,'0000','["no-title-set"]');
INSERT INTO outline_path VALUES(89,17,88,1,'0000.0001','["no-title-set","The parent title should be the file name"]');
INSERT INTO outline_path VALUES(90,18,NULL,0,'0000','["outline-contains-fixture-olc1"]');
INSERT INTO outline_path VALUES(91,18,90,1,'0000.0001','["outline-contains-fixture-olc1","outline-contains: Project Branch olc1-project"]');
INSERT INTO outline_path VALUES(92,18,91,2,'0000.0001.0001','["outline-contains-fixture-olc1","outline-contains: Project Branch olc1-project","outline-contains: Database Branch olc1-database"]');
INSERT INTO outline_path VALUES(93,18,92,3,'0000.0001.0001.0001','["outline-contains-fixture-olc1","outline-contains: Project Branch olc1-project","outline-contains: Database Branch olc1-database","outline-contains: Query Leaf olc1-query"]');
INSERT INTO outline_path VALUES(94,18,91,2,'0000.0001.0002','["outline-contains-fixture-olc1","outline-contains: Project Branch olc1-project","outline-contains: Unrelated Leaf olc1-unrelated"]');
INSERT INTO outline_path VALUES(95,18,90,1,'0000.0002','["outline-contains-fixture-olc1","outline-contains: Other Root olc1-other"]');
INSERT INTO outline_path VALUES(96,18,95,2,'0000.0002.0001','["outline-contains-fixture-olc1","outline-contains: Other Root olc1-other","outline-contains: Query Elsewhere olc1-query-elsewhere"]');
INSERT INTO outline_path VALUES(97,18,96,3,'0000.0002.0001.0001','["outline-contains-fixture-olc1","outline-contains: Other Root olc1-other","outline-contains: Query Elsewhere olc1-query-elsewhere","outline-contains: Regexp Leaf olc1-regexp-624"]');
INSERT INTO outline_path VALUES(98,19,NULL,0,'0000','["outline-sequence-fixture-ols2"]');
INSERT INTO outline_path VALUES(99,19,98,1,'0000.0001','["outline-sequence-fixture-ols2","outline-sequence: Alpha Parent ols2-alpha"]');
INSERT INTO outline_path VALUES(100,19,99,2,'0000.0001.0001','["outline-sequence-fixture-ols2","outline-sequence: Alpha Parent ols2-alpha","outline-sequence: Beta Middle ols2-beta"]');
INSERT INTO outline_path VALUES(101,19,100,3,'0000.0001.0001.0001','["outline-sequence-fixture-ols2","outline-sequence: Alpha Parent ols2-alpha","outline-sequence: Beta Middle ols2-beta","outline-sequence: Gamma Leaf ols2-gamma"]');
INSERT INTO outline_path VALUES(102,19,101,4,'0000.0001.0001.0001.0001','["outline-sequence-fixture-ols2","outline-sequence: Alpha Parent ols2-alpha","outline-sequence: Beta Middle ols2-beta","outline-sequence: Gamma Leaf ols2-gamma","outline-sequence: Delta Deep ols2-delta"]');
INSERT INTO outline_path VALUES(103,19,99,2,'0000.0001.0002','["outline-sequence-fixture-ols2","outline-sequence: Alpha Parent ols2-alpha","outline-sequence: Gamma Noncontiguous ols2-gap"]');
INSERT INTO outline_path VALUES(104,19,98,1,'0000.0002','["outline-sequence-fixture-ols2","outline-sequence: Exact Alpha ols2-exact-alpha"]');
INSERT INTO outline_path VALUES(105,19,104,2,'0000.0002.0001','["outline-sequence-fixture-ols2","outline-sequence: Exact Alpha ols2-exact-alpha","outline-sequence: Exact Beta ols2-exact-beta"]');
INSERT INTO outline_path VALUES(106,19,105,3,'0000.0002.0001.0001','["outline-sequence-fixture-ols2","outline-sequence: Exact Alpha ols2-exact-alpha","outline-sequence: Exact Beta ols2-exact-beta","outline-sequence: Regexp 517 ols2-regexp"]');
INSERT INTO outline_path VALUES(107,20,NULL,0,'0000','["parent-fixture-par1"]');
INSERT INTO outline_path VALUES(108,20,107,1,'0000.0001','["parent-fixture-par1","parent: Matching Parent par1-target"]');
INSERT INTO outline_path VALUES(109,20,108,2,'0000.0001.0001','["parent-fixture-par1","parent: Matching Parent par1-target","parent: Direct Child Match par1-a"]');
INSERT INTO outline_path VALUES(110,20,109,3,'0000.0001.0001.0001','["parent-fixture-par1","parent: Matching Parent par1-target","parent: Direct Child Match par1-a","parent: Grandchild Parent Is Child par1-b"]');
INSERT INTO outline_path VALUES(111,20,107,1,'0000.0002','["parent-fixture-par1","parent: Nonmatching Parent par1-other"]');
INSERT INTO outline_path VALUES(112,20,111,2,'0000.0002.0001','["parent-fixture-par1","parent: Nonmatching Parent par1-other","parent: Direct Child Negative par1-c"]');
INSERT INTO outline_path VALUES(113,20,107,1,'0000.0003','["parent-fixture-par1","parent: Top Level No Parent par1-d"]');
INSERT INTO outline_path VALUES(114,21,NULL,0,'0000','["file-dir-fixture-fdr5"]');
INSERT INTO outline_path VALUES(115,21,114,1,'0000.0001','["file-dir-fixture-fdr5","file-dir: Directory Match fdr5-a"]');
INSERT INTO outline_path VALUES(116,21,114,1,'0000.0002','["file-dir-fixture-fdr5","file-dir: Directory Second Match fdr5-b"]');
INSERT INTO outline_path VALUES(117,22,NULL,0,'0000','["file-path-fixture-fpt4"]');
INSERT INTO outline_path VALUES(118,22,117,1,'0000.0001','["file-path-fixture-fpt4","file-path: Path Match fpt4-a"]');
INSERT INTO outline_path VALUES(119,22,117,1,'0000.0002','["file-path-fixture-fpt4","file-path: Path Second Match fpt4-b"]');
INSERT INTO outline_path VALUES(120,23,NULL,0,'0000','["planning-fixture-pln9"]');
INSERT INTO outline_path VALUES(121,23,120,1,'0000.0001','["planning-fixture-pln9","planning: Scheduled pln9-a"]');
INSERT INTO outline_path VALUES(122,23,120,1,'0000.0002','["planning-fixture-pln9","planning: Deadline pln9-b"]');
INSERT INTO outline_path VALUES(123,23,120,1,'0000.0003','["planning-fixture-pln9","planning: Closed pln9-c"]');
INSERT INTO outline_path VALUES(124,23,120,1,'0000.0004','["planning-fixture-pln9","planning: Multiple Planning pln9-d"]');
INSERT INTO outline_path VALUES(125,23,120,1,'0000.0005','["planning-fixture-pln9","planning: Plain Timestamp Negative Control pln9-e"]');
INSERT INTO outline_path VALUES(126,23,120,1,'0000.0006','["planning-fixture-pln9","planning: No Timestamp Control pln9-f"]');
INSERT INTO outline_path VALUES(127,24,NULL,0,'0000','["priority-fixture-pri4"]');
INSERT INTO outline_path VALUES(128,24,127,1,'0000.0001','["priority-fixture-pri4","priority: Exact A pri4-a"]');
INSERT INTO outline_path VALUES(129,24,127,1,'0000.0002','["priority-fixture-pri4","priority: Exact B pri4-b"]');
INSERT INTO outline_path VALUES(130,24,127,1,'0000.0003','["priority-fixture-pri4","priority: Exact C pri4-c"]');
INSERT INTO outline_path VALUES(131,24,127,1,'0000.0004','["priority-fixture-pri4","priority: No Priority Negative Control pri4-d"]');
INSERT INTO outline_path VALUES(132,24,127,1,'0000.0005','["priority-fixture-pri4","priority: Done With Priority A pri4-e"]');
INSERT INTO outline_path VALUES(133,24,127,1,'0000.0006','["priority-fixture-pri4","priority: Done With Priority A pri4-e"]');
INSERT INTO outline_path VALUES(134,25,NULL,0,'0000','["Org Property and Keyword Test"]');
INSERT INTO outline_path VALUES(135,25,134,1,'0000.0001','["Org Property and Keyword Test","Empty Property"]');
INSERT INTO outline_path VALUES(136,25,134,1,'0000.0002','["Org Property and Keyword Test","Expected file/root values"]');
INSERT INTO outline_path VALUES(137,25,134,1,'0000.0003','["Org Property and Keyword Test","Local append on the same heading"]');
INSERT INTO outline_path VALUES(138,25,134,1,'0000.0004','["Org Property and Keyword Test","Multiple local append rows"]');
INSERT INTO outline_path VALUES(139,25,134,1,'0000.0005','["Org Property and Keyword Test","Local append without a base value"]');
INSERT INTO outline_path VALUES(140,25,134,1,'0000.0006','["Org Property and Keyword Test","Later local definition replaces the earlier definition"]');
INSERT INTO outline_path VALUES(141,25,134,1,'0000.0007','["Org Property and Keyword Test","Local append followed by replacement"]');
INSERT INTO outline_path VALUES(142,25,134,1,'0000.0008','["Org Property and Keyword Test","Empty base followed by append"]');
INSERT INTO outline_path VALUES(143,25,134,1,'0000.0009','["Org Property and Keyword Test","Base followed by empty append"]');
INSERT INTO outline_path VALUES(144,25,134,1,'0000.0010','["Org Property and Keyword Test","Parent append inheritance"]');
INSERT INTO outline_path VALUES(145,25,144,2,'0000.0010.0001','["Org Property and Keyword Test","Parent append inheritance","Child with append only"]');
INSERT INTO outline_path VALUES(146,25,144,2,'0000.0010.0002','["Org Property and Keyword Test","Parent append inheritance","Child without local value"]');
INSERT INTO outline_path VALUES(147,25,144,2,'0000.0010.0003','["Org Property and Keyword Test","Parent append inheritance","Child with local replacement"]');
INSERT INTO outline_path VALUES(148,25,144,2,'0000.0010.0004','["Org Property and Keyword Test","Parent append inheritance","Child with local replacement and append"]');
INSERT INTO outline_path VALUES(149,25,134,1,'0000.0011','["Org Property and Keyword Test","Parent with appended effective value"]');
INSERT INTO outline_path VALUES(150,25,149,2,'0000.0011.0001','["Org Property and Keyword Test","Parent with appended effective value","Inheriting child"]');
INSERT INTO outline_path VALUES(151,25,149,2,'0000.0011.0002','["Org Property and Keyword Test","Parent with appended effective value","Child overriding appended parent value"]');
INSERT INTO outline_path VALUES(152,25,149,2,'0000.0011.0003','["Org Property and Keyword Test","Parent with appended effective value","Child appending to appended parent value"]');
INSERT INTO outline_path VALUES(153,25,134,1,'0000.0012','["Org Property and Keyword Test","Nearest ancestor wins"]');
INSERT INTO outline_path VALUES(154,25,153,2,'0000.0012.0001','["Org Property and Keyword Test","Nearest ancestor wins","Parent override"]');
INSERT INTO outline_path VALUES(155,25,154,3,'0000.0012.0001.0001','["Org Property and Keyword Test","Nearest ancestor wins","Parent override","Child inheriting nearest value"]');
INSERT INTO outline_path VALUES(156,25,154,3,'0000.0012.0001.0002','["Org Property and Keyword Test","Nearest ancestor wins","Parent override","Child appending to nearest value"]');
INSERT INTO outline_path VALUES(157,25,154,3,'0000.0012.0001.0003','["Org Property and Keyword Test","Nearest ancestor wins","Parent override","Child replacing nearest value"]');
INSERT INTO outline_path VALUES(158,25,134,1,'0000.0013','["Org Property and Keyword Test","Root drawer inheritance"]');
INSERT INTO outline_path VALUES(159,25,158,2,'0000.0013.0001','["Org Property and Keyword Test","Root drawer inheritance","Child inheriting root drawer base"]');
INSERT INTO outline_path VALUES(160,25,158,2,'0000.0013.0002','["Org Property and Keyword Test","Root drawer inheritance","Child appending to root drawer base"]');
INSERT INTO outline_path VALUES(161,25,158,2,'0000.0013.0003','["Org Property and Keyword Test","Root drawer inheritance","Child replacing root drawer base"]');
INSERT INTO outline_path VALUES(162,25,134,1,'0000.0014','["Org Property and Keyword Test","File keyword inheritance"]');
INSERT INTO outline_path VALUES(163,25,162,2,'0000.0014.0001','["Org Property and Keyword Test","File keyword inheritance","Child inheriting appended keyword"]');
INSERT INTO outline_path VALUES(164,25,162,2,'0000.0014.0002','["Org Property and Keyword Test","File keyword inheritance","Child appending to file keyword"]');
INSERT INTO outline_path VALUES(165,25,162,2,'0000.0014.0003','["Org Property and Keyword Test","File keyword inheritance","Child replacing file keyword"]');
INSERT INTO outline_path VALUES(166,25,162,2,'0000.0014.0004','["Org Property and Keyword Test","File keyword inheritance","Child replacing and appending file keyword"]');
INSERT INTO outline_path VALUES(167,25,134,1,'0000.0015','["Org Property and Keyword Test","Append before"]');
INSERT INTO outline_path VALUES(168,25,134,1,'0000.0016','["Org Property and Keyword Test","Duplicate property definitions in one drawer"]');
INSERT INTO outline_path VALUES(169,25,134,1,'0000.0017','["Org Property and Keyword Test","Duplicate definition followed by append"]');
INSERT INTO outline_path VALUES(170,25,134,1,'0000.0018','["Org Property and Keyword Test","Append followed by duplicate replacement"]');
INSERT INTO outline_path VALUES(171,25,134,1,'0000.0019','["Org Property and Keyword Test","Append followed by duplicate replacement"]');
INSERT INTO outline_path VALUES(172,25,134,1,'0000.0020','["Org Property and Keyword Test","Parent duplicate definition"]');
INSERT INTO outline_path VALUES(173,25,172,2,'0000.0020.0001','["Org Property and Keyword Test","Parent duplicate definition","Child inheriting duplicate parent value"]');
INSERT INTO outline_path VALUES(174,25,172,2,'0000.0020.0002','["Org Property and Keyword Test","Parent duplicate definition","Child appending to duplicate parent value"]');
INSERT INTO outline_path VALUES(175,25,134,1,'0000.0021','["Org Property and Keyword Test","Parent definition overridden by child duplicate definitions"]');
INSERT INTO outline_path VALUES(176,25,175,2,'0000.0021.0001','["Org Property and Keyword Test","Parent definition overridden by child duplicate definitions","Child with two local definitions"]');
INSERT INTO outline_path VALUES(177,25,134,1,'0000.0022','["Org Property and Keyword Test","Mixed-case property keys"]');
INSERT INTO outline_path VALUES(178,26,NULL,0,'0000','["scheduled-fixture-sch7"]');
INSERT INTO outline_path VALUES(179,26,178,1,'0000.0001','["scheduled-fixture-sch7","scheduled: Date Only sch7-a"]');
INSERT INTO outline_path VALUES(180,26,178,1,'0000.0002','["scheduled-fixture-sch7","scheduled: With Time sch7-b"]');
INSERT INTO outline_path VALUES(181,26,178,1,'0000.0003','["scheduled-fixture-sch7","scheduled: Range Earlier sch7-c"]');
INSERT INTO outline_path VALUES(182,26,178,1,'0000.0004','["scheduled-fixture-sch7","scheduled: Range Later sch7-d"]');
INSERT INTO outline_path VALUES(183,26,178,1,'0000.0005','["scheduled-fixture-sch7","scheduled: Deadline Negative Control sch7-e"]');
INSERT INTO outline_path VALUES(184,27,NULL,0,'0000','["tags_fixture_tag3"]');
INSERT INTO outline_path VALUES(185,27,184,1,'0000.0001','["tags_fixture_tag3","tags: Local Single tag3_a"]');
INSERT INTO outline_path VALUES(186,27,184,1,'0000.0002','["tags_fixture_tag3","tags: Local Any First tag3_b"]');
INSERT INTO outline_path VALUES(187,27,184,1,'0000.0003','["tags_fixture_tag3","tags: Local Any Second tag3_c"]');
INSERT INTO outline_path VALUES(188,27,184,1,'0000.0004','["tags_fixture_tag3","tags: All Match tag3_d"]');
INSERT INTO outline_path VALUES(189,27,184,1,'0000.0005','["tags_fixture_tag3","tags: Parent Inheritance tag3_e"]');
INSERT INTO outline_path VALUES(190,27,189,2,'0000.0005.0001','["tags_fixture_tag3","tags: Parent Inheritance tag3_e","tags: Inherited Child tag3_f"]');
INSERT INTO outline_path VALUES(191,27,189,2,'0000.0005.0002','["tags_fixture_tag3","tags: Parent Inheritance tag3_e","tags: Local Child Override Test tag3_g"]');
INSERT INTO outline_path VALUES(192,27,184,1,'0000.0006','["tags_fixture_tag3","tags: Regexp Match tag3_h"]');
INSERT INTO outline_path VALUES(193,27,184,1,'0000.0007','["tags_fixture_tag3","tags: Filetag Inherited tag3_i"]');
INSERT INTO outline_path VALUES(194,27,184,1,'0000.0008','["tags_fixture_tag3","tags: No Match Control tag3_j"]');
INSERT INTO outline_path VALUES(195,27,184,1,'0000.0009','["tags_fixture_tag3","tags: Override Parent File Tag"]');
INSERT INTO outline_path VALUES(196,28,NULL,0,'0000','["Title"]');
INSERT INTO outline_path VALUES(197,28,196,1,'0000.0001','["Title","Title: This Is a Title"]');
INSERT INTO outline_path VALUES(198,28,196,1,'0000.0002','["Title","title: This Is another Title"]');
INSERT INTO outline_path VALUES(199,28,196,1,'0000.0003','["Title","Title: Same Title"]');
INSERT INTO outline_path VALUES(200,28,196,1,'0000.0004','["Title","Title: Same Title"]');
INSERT INTO outline_path VALUES(201,29,NULL,0,'0000','["File-local TODO keywords"]');
INSERT INTO outline_path VALUES(202,29,201,1,'0000.0001','["File-local TODO keywords","TODO default keyword should stay in title"]');
INSERT INTO outline_path VALUES(203,29,201,1,'0000.0002','["File-local TODO keywords","DONE default done keyword should stay in title"]');
INSERT INTO outline_path VALUES(204,29,201,1,'0000.0003','["File-local TODO keywords","open keyword with fast key"]');
INSERT INTO outline_path VALUES(205,29,201,1,'0000.0004','["File-local TODO keywords","another open keyword with fast key"]');
INSERT INTO outline_path VALUES(206,29,201,1,'0000.0005','["File-local TODO keywords","closed keyword with fast key"]');
INSERT INTO outline_path VALUES(207,29,201,1,'0000.0006','["File-local TODO keywords","closed keyword with extended fast key"]');
INSERT INTO outline_path VALUES(208,29,201,1,'0000.0007','["File-local TODO keywords","open keyword from empty-done-side line"]');
INSERT INTO outline_path VALUES(209,29,201,1,'0000.0008','["File-local TODO keywords","another open keyword from empty-done-side line"]');
INSERT INTO outline_path VALUES(210,29,201,1,'0000.0009','["File-local TODO keywords","open keyword from TYP_TODO"]');
INSERT INTO outline_path VALUES(211,29,201,1,'0000.0010','["File-local TODO keywords","closed keyword from TYP_TODO"]');
INSERT INTO outline_path VALUES(212,29,201,1,'0000.0011','["File-local TODO keywords","open keyword from SEQ_TODO"]');
INSERT INTO outline_path VALUES(213,29,201,1,'0000.0012','["File-local TODO keywords","closed keyword from SEQ_TODO"]');
INSERT INTO outline_path VALUES(214,29,201,1,'0000.0013','["File-local TODO keywords","closed keyword from later TODO line"]');
INSERT INTO outline_path VALUES(215,29,201,1,'0000.0014','["File-local TODO keywords","open keyword from line defined later in file"]');
INSERT INTO outline_path VALUES(216,29,201,1,'0000.0015','["File-local TODO keywords","closed keyword from line defined later in file"]');
INSERT INTO outline_path VALUES(217,29,201,1,'0000.0016','["File-local TODO keywords","TODO still not valid after later local lines"]');
INSERT INTO outline_path VALUES(218,29,201,1,'0000.0017','["File-local TODO keywords","DONE still not valid after later local lines"]');
INSERT INTO outline_path VALUES(219,30,NULL,0,'0000','["TODO keywords"]');
INSERT INTO outline_path VALUES(220,30,219,1,'0000.0001','["TODO keywords","Default keyword > TODO"]');
INSERT INTO outline_path VALUES(221,30,219,1,'0000.0002','["TODO keywords","Default keyword > DONE"]');
INSERT INTO outline_path VALUES(222,30,219,1,'0000.0003','["TODO keywords","Default Keyword with Prio and Cookies"]');
INSERT INTO outline_path VALUES(223,30,219,1,'0000.0004','["TODO keywords","Keyword from Config.toml > NEXT"]');
INSERT INTO outline_path VALUES(224,30,219,1,'0000.0005','["TODO keywords","Keyword from Config.toml > CANCEL"]');
INSERT INTO outline_path VALUES(225,31,NULL,0,'0000','["ts-active-fixture-tsa4"]');
INSERT INTO outline_path VALUES(226,31,225,1,'0000.0001','["ts-active-fixture-tsa4","ts-active: Date Only tsa4-a"]');
INSERT INTO outline_path VALUES(227,31,225,1,'0000.0002','["ts-active-fixture-tsa4","ts-active: With Time tsa4-b"]');
INSERT INTO outline_path VALUES(228,31,225,1,'0000.0003','["ts-active-fixture-tsa4","ts-active: Range Start tsa4-c"]');
INSERT INTO outline_path VALUES(229,31,225,1,'0000.0004','["ts-active-fixture-tsa4","ts-active: Range End tsa4-d"]');
INSERT INTO outline_path VALUES(230,31,225,1,'0000.0005','["ts-active-fixture-tsa4","ts-active: Inactive Negative Control tsa4-e"]');
INSERT INTO outline_path VALUES(231,32,NULL,0,'0000','["ts-inactive-fixture-tsi5"]');
INSERT INTO outline_path VALUES(232,32,231,1,'0000.0001','["ts-inactive-fixture-tsi5","ts-inactive: Date Only tsi5-a"]');
INSERT INTO outline_path VALUES(233,32,231,1,'0000.0002','["ts-inactive-fixture-tsi5","ts-inactive: With Time tsi5-b"]');
INSERT INTO outline_path VALUES(234,32,231,1,'0000.0003','["ts-inactive-fixture-tsi5","ts-inactive: Range Start tsi5-c"]');
INSERT INTO outline_path VALUES(235,32,231,1,'0000.0004','["ts-inactive-fixture-tsi5","ts-inactive: Range End tsi5-d"]');
INSERT INTO outline_path VALUES(236,32,231,1,'0000.0005','["ts-inactive-fixture-tsi5","ts-inactive: Active Negative Control tsi5-e"]');
INSERT INTO outline_path VALUES(237,33,NULL,0,'0000','["ts-fixture-tsp3"]');
INSERT INTO outline_path VALUES(238,33,237,1,'0000.0001','["ts-fixture-tsp3","ts: Active Date Only tsp3-a"]');
INSERT INTO outline_path VALUES(239,33,237,1,'0000.0002','["ts-fixture-tsp3","ts: Active With Time tsp3-b"]');
INSERT INTO outline_path VALUES(240,33,237,1,'0000.0003','["ts-fixture-tsp3","ts: Inactive Date Only tsp3-c"]');
INSERT INTO outline_path VALUES(241,33,237,1,'0000.0004','["ts-fixture-tsp3","ts: Inactive With Time tsp3-d"]');
INSERT INTO outline_path VALUES(242,33,237,1,'0000.0005','["ts-fixture-tsp3","ts: Multiple Timestamp Range tsp3-e"]');
INSERT INTO outline_path VALUES(243,33,237,1,'0000.0006','["ts-fixture-tsp3","ts: No Timestamp Control tsp3-f"]');
PRAGMA writable_schema=ON;
INSERT INTO sqlite_schema(type,name,tbl_name,rootpage,sql)VALUES('table','heading_fts','heading_fts',0,'CREATE VIRTUAL TABLE heading_fts
USING fts5(
    title,
    body,
    tokenize = ''unicode61'',
    content = ''''
)');
CREATE TABLE IF NOT EXISTS 'heading_fts_data'(id INTEGER PRIMARY KEY, block BLOB);
INSERT INTO heading_fts_data VALUES(1,X'815289178f19');
INSERT INTO heading_fts_data VALUES(10,X'000000000101030001010103');
INSERT INTO heading_fts_data VALUES(137438953473,X'00000f4c0230308108060101050201307c0601010802013181640601010406060101040406010103010601010301060101030106010103010601010402013281720801010307020133816206010103010601010301060101030106010103010601010302013481680601010301060101030106010103010601010301060101030201387c0601010401060101036606010106020139816f060101060101313d0206070204040205400a01011a58121b0801011912010c0101180b140b010c0101090e090e010c010109100b100201308165060101070906010104010601010402013181700601010401060101040201328162060101040106010104030601010402013381680601010401060101040306010104020134817106010106020135816f060101070201367c060101076d060101060201377d0601010402013881720601010b0101323e02060702040402053f0a01011c58121b0801011b12010c01011a0b140b020c01010b100b100201308163060101070f0601010903023331816e06010102010601010201060101020106010102010801010207040132816206010102010601010201060101020106010102010601010204013381680601010201060101020106010102010601010201060101020401377c060101030106010102020131816b060101060201338165060101060101334602044208010174121b0801011d12010a010125140b020130816b06010104070601010c0201317c0601010569060101040101344702045d0c0101090e14140201308169060101070201358171060101070202383281400601010f01033531376a0205020135816b0601010701033632346102080101610402060502070102080802060602060602070102080802060502080302070302080302070b0206040206070a0801010405020407050402091c02070602070302070302050704040404020601020603080101815c0302052802060602060c02051d0207060207060207020a6363756d756c61746564810d0601013003047469766581620203010203010203010203010203060204020203010203020464646564810e0601012701060101271b0601011502046674657281080a0101297010050a0101061212240601010328020601020602026c6c812a0601010d120a0301010e050303706861630404050504050502016e810b060101181d080101370a0302633302020501020401020501020501020501020704056573746f7202020403020402020681040601012c0e02030901730202020102020102020102020102020102020301643b060101064e060101290b0206120204380206140601010603056f74686572812a0c0101210f0a0f1c0205070202040202030179813a080401010e01080401010e02057070656e6481081401011904240d28102e0d01080301012b010204010203010601012401080301012b01080601010901080601010801020301080401013002060101240102070f0c0101090a0810010c0101070c1012010c0101070c0b0c010c0101070e0d0e010202020206010a020101060a01020207026564810812010128190f4c10090f050e0101050f050f05070c0101080d080d01020401080101160d010804010127010a050101170e0b02040406010105020a0101050d0d010c0101200f0a0f010a0101050d0d0703696e678118020304020304020304020302020501080101100e0702030202726581090601012d1f060101050206010114020173570601010b5306010109020174812a0601011002087661696c61626c65810b0601011d0101620602060502070802060602060702070802060502090302080302090302070b0206040206070a0801010605020407050402081c020806020803020803020507040404330206060207290207060207060207020761636b6c696e6b4b060101060106010108010206010206010206030172810808010171121b0801011a12010c0101190b140b0302736581080801012d0c0106010127010c0101070e090e01080601011a030803010106010802010104020601012c03060101270b0e0601010a0b090c010e070101080c0b0d010e060101080c0b0c090801010b1003017a810808010173121b0801011c12010a010124140b020165570601010402020603056361757365814a06010107010601010704046f6d657381110601012a0304666f7265810d08010113121a0a030101110e030367696e570601010d03027461640404050504050502046c61636b81420601010f03056f636b6564200204020404070302756581390601010f02036f7468810c060101271c06010103020572616e63685b020501020502017957060101063602050108050101080108040101060c060101250e02050102040102040402050101630702080702060602060602060902070102070502070e02070b02060402060902090402091d02070b0205070404043302060602072902070602070602070202616e570801010311040363656c8160020607036c6564280203030273658131020302046861696e81080801015f0c030264320902060102060102060102050102050102050102060102060303696c6409020501040504020204040405040f04050405040404490204010206020204211002010105070c080d010202010e020101070c070c010e020101070d080d0202030112020101070c070c0408010e020101070c090e030202010e020101070c080d010e020101070c070c020202010e020101090e0a0f010e020101090e090e020202010e020101080e1414010202010202070202010e020101070c080d010206010e020101080d080d0e0204010204060372656e0902020102020102020102020102020104020401020201020202056c6f736564120202010202010202010202010402046502035302020102020402020202020102020202020302733812020501020501020501020501020702076f6d62696e656481090601012e03046e666967815f020401020404057461696e735b02030102030102030102030102030102030102030503726f6c1602060602050d02050102060e08060101064502060108050101060502063402050b080501010e24020606020607080501010603056f6b696573815e020702057573746f6d510a060101040402060101040101640f02070102080502060602060a02060502080e02080f02062a02080b02060702083302060602062902070602070602070207617461626173655c040405040165120203060203811b02032f020406020406020402020402076561646c696e651802020102020102020102020102025e020302060101023b0203030265700402031c02030204030644020503056661756c74814a0803010102010803010102110202010202010202080173814a0601010d010601010d0404696e656481280c01010d0d0c0d2f02060102060701738108080101815f06056974696f6e810b0601012d010a0406010125070601012508060101290c0801010f0e02020301060101070202040302030b017381280204020601010f05020801020603036c746166040405030273341e020601020601020601020401040604010206010205010205040763656e64616e740402040202041802050202050302040b01731e020201020201020201020201020201020201020201040204050772697074696f6e350601010a010601010a01060101091406010108010601010a0506010108010601010901060101080202697273020301020304036563740902040102030502030102030e02030102034e0203030203180601010c0a0601010704060101070506010107040601010704060101070a0601010707036f727973020401020402036c6e3618020501020501020501020501020602036e653427020501020501020601020702036f6573811706010123110601013c080601012803026e650f0204010204130203010203030402030102020102020102025a0203010203460a02040101030502060102070902020302040205726177657281081001012c0c080d090c160203010e050101090b090c010e060101070c0b0d010e050101070c0b0c070207020875706c696361746581080c01014f0c2f0b2002020102020102050102050102030102040102050102070101651602080602072c0207350208070208010208320207060206290208060208060207020661726c69657214020406020472020729020402086666656374697665320204010204550601010d0d0205130e0101110f0a0f1409080101072002046c69737081081a01011010141313121615131512010801010a13010801010b15010801010918010801010911010801010912010801010f0c010801010f0c020801010c12010801010a0f010801010911010801010a13020801010a10010801010911010801010913030801010a0f010801010912010801010911020801010c13010801010b16010801010b15020801010b16010801010b20010801010b15010801010d1901080101070e01080101131701080101070d01080101241701080101070d020801010a0f010801010912020801010a13010801010c0f08060101030106010103010601010301060101030106010103010601010301060101030106010103010601010301060101030307736577686572656004050502046d70747981070802010103010a0101813a0b06080201010501080501010741020501020602026e6457060101183006010105070601010a010601010a560205060205030374727981081a01011210141313121615131512010801010c13010801010d15010801010b18010801010b11010801010b1201080101110c01080101110c020801010e12010801010c0f010801010b11010801010c13020801010c10010801010b11010801010b13030801010c0f010801010b12010801010b11020801010e13010801010d16010801010d15020801010d16010801010d20010801010d15010801010f1901080101090e01080101151701080101090d01080101261701080101090d020801010c0f010801010b12020801010c13010801010e0f020376656e5706010116040772797768657265570203020478616374680404050104040517020301020301020326080101380a04046d706c65360601010303067065637465648108020201060101020106010102010601010201060101020106010102010601010b010601010b0206010102010601010201060101020106010102020601010201060101020106010102030601010201060101020106010102020601010201060101020106010102020601010201060101020106010102010601010201060101020106010102010601010201060101020106010102020601010201060101020206010102010601010204036f7274570601010a040908270a1d1d120931130e131309090d0809290e1f1d1d0d0909190e0d0e09090a0809086c121f1620110d0f1619151b191078551e140d0910541c1855170b120c140a0c0c0e0a100d28400a0908080d1c81341e2c130f0d1c380b13330d1923111d0e150a3212091d181c310a400b14111536332b241427820e0d261e815b0a0c1d0b8131');
INSERT INTO heading_fts_data VALUES(137438953474,X'00000f520930657874656e646564814f02050101664902073502074002063502070203617374814c020501020601020501020602036472357302060102070203696c652c0202010202020202010202020202010202020c0401010205062402081a0202010202020202010202110c03010106814b1a0202020205010204010206130601010b010601010b010601010b010601010b010601010b010601010b010601010b010601010b010601010b010601010b010205070601010801060101080c020901020905046e616d652f020401020405037461678141020303036e616c81310a0101170f060402647381120601010504060101050506010105040601010504060101050a060101050303727374400203490c0101070d080d31020502036d64382c020701020802036e6d332f020601020702076f6c6c6f776564810d02040108040101070108030101051a020401020301020303016f81080801016f121b0801011812010c0101170b140b030172810b0601012e06080101032c170801013a0a0203707434760206010207020272698162060101050106010105030601010504060101050406010105010601010503026f6d4b0203010203010203010203010203450601012c3c02040102050102040102040102040102040102040102040102040702030102030203746c36320207010208010167813f02080204616d6d61650404050202040301706702070202657481081a01011310141313121615131512010801010d13010801010e15010801010c18010801010c11010801010c1201080101120c01080101120c020801010f12010801010d0f010801010c11010801010d13020801010d10010801010c11010801010c13030801010d0f010801010c12010801010c11020801010f13010801010e16010801010e15020801010e16010801010e20010801010e15010801011019010801010a0e010801011617010801010a0d010801012717010801010a0d020801010d0f010801010c12020801010d13010801010f0f0806010107010601010701060101070106010107010601010701060101070106010107010601010701060101070106010107020469766573810908010105130108010105150108010105110108010105110508010108110106010113010801010511010801010513020601011301080101051101080101051103060101130108010105110108010105110206010117010801010515010801010515020601011501080101051a010801010515010801010519020801010c17020801011d17030601011301080101051102080101051302036f6c64813c0601010f020472616e640d020606056368696c640d02036102030606706172656e74811b06010121030365656e813b0601010f01016881400206020261730902030202032a0202010202010202010202010202020402050202020102024d0601012a0302766581080601010a0206656164696e6739020507020401020401020412020501020633080101815e0102070206010129080173810808010181560302726557060101170205696464656e811b0601012402036c6e35350a0601010306010a0601010504010a060101030501080701010501020602047474707336080401010201016981410206020164370c040101020505150801010205050207010c06010102050402016e81090601012f1f08050101072202070102080c02080102080306616374697665816602040202030102030102030102030102030402030102030305686572697481080c010181480d0701080101031301080101031501080101031101080101031105080101061101080101031001080101031101080101031302080101031001080101031101080101031103080101031001080101031101080101031102080101031401080101031501080101031502080101031201080101031a010801010315010801010319020801010a17020801011b170308010103100108010103110208010103130804616e6365811002040e02040402041b020408026564810b0601011906060101270206010128260601010c010601010c010601010c010601010c010601010c01080301010c010601010c010601010c01080401010c010601010c0803696e67811602020502030402030402030a0203030a7465726d6564696174650c0203150203030576616c696436060101040201736e02051d0601011c030601012601060101260c060101230d100101100f0a0f06101d02040102040406010104010601010401016a8142020701036b657981310e01010406040a0f1b0206010207010206010207040173813102050404776f7264270204010204020204160202010202010202461a0101234c100c0b070e0a0d090b1a0203010e050101080a0810010e060101060c1012010e050101060c0b0c010e070101060e0d0e240204010205010203010204010203010203010203010204010203010203010203010203010203010203010203040203010203010203010202010202020377643740020501020501020501046c617374812a0601010403037465721502040602047102020106010129290204200205010207010207010207010207050178570a01010905050206656164696e67810e060101230401665d0205010205030205040205030376656c0702043d0402030104020301040203010402030104020401040204280204170601010402036672374b0a0601010205010a06010103060102080102080102070203696e65815002080102090502070102050102050501735706010115730601010b010601010b0e020901020904016b35040304010403040104030401040304010203050265644b020201020201020201020201020205017351020201020201020201020201020202046f63616c81090a0201012606010203010202010203010202040601012f010204010804010122010804010126110c0101080e090e0114010108040e0409040e040a020509080301010e0102030102030402030b0601010901060101090e0208010208050474696f6e812a060101130303776572810c060101220203746f36510a0801010205010a0701010305010a08010102050102060102070203766c3844020501020501020501020501020601020601076d6163687465733c020303037463682c0206010207020205010206020206010207080601010332020506020501020602020501020611080101814c200601013e140204040204020204060265733d08050101030108050101036a060101460603696e676c0203030179810808010181570206657373616765813906010104010601010401060101040106010104010601010401060101040106010104010601010401060101040106010104020269640302050902061502050403646c65030203610205030378656481310c020101090a0f02076f6469666965642c0403030104030303016e7c0601010601060101056706010105030272653b060101080207756c7469706c653c020201080401010201080401010219060101142502030e020268020301046e616d652f0203010203290209020565617265728108080101815d1306010127060273748119020202020401020501020403066761746976651602050602040d02040102054602050d02050602053402042f02050602050304737465640b0204370203030177810806010118030278740b0205020204110204010204814002060202696c81081c010114101413131216151315120a010c0101040c1304010c0101040d1504010c0101040b0416010e0101040b040f04010a01010d1204010a0101130c04010a0101130c04020c0101070b0410010c0101040c040d010c0101040b040f010c0101040c0411020c0101040c040e010c0101040b040f010c0101040b0411030c0101040c040d010c0101040b0410010c0101040b040f020c0101040e060f010c0101040d0612010c0101040d0611020c0101040d0513010c0101040d051d010c0101040d0512010c0101040f0516010a01010b040c010c01010b0e0514010a01010b040b010c01011c0e0415010a01010b040b020c0101040c040d010c0101040b0410020c0101040c0411010c010110050c0502016f0702050702030802030f02030502030e02043902050d0203050203080601012b03060101220106010122030601010604060101060506010106040601010604060101060a0601010615020331020303016e810c06010123010601012a06060101231706010105040a636f6e746967756f757367020504086d61746368696e676f0203030b726d616c697a6174696f6e813106010105030174811706010124110601013d08060101291a0601010501060101050e020401020401066f626a65637481080a010108814b02036c63315b02060102060102060102060102060102060102060302733263020601020601020601020601020601020601020601020602016e81090204030165810a0c0101080e090e1e0206090601010603026c790f02050302040602040b0205651001018129030c0518030e0101070c07130f0602052202042f0205060205060205020205020370656e2902038123020201020303020201020301020202020203020202047264657281090601013103016735060101062206010108311a01011110141313121615131512010801010b13010801010c15010801010a18010801010a11010801010a1201080101100c01080101100c020801010d12010801010b0f010801010a11010801010b13020801010b10010801010a11010801010a13030801010b0f010801010a12010801010a11020801010d13010801010c16010801010c15020801010c16010801010c20010801010c15010801010e1901080101080e01080101141701080101080d01080101251701080101080d020801010b0f010801010a12020801010b13010801010d0f0806010106010601010601060101060106010106010601010601060101060106010106010601010601060101060106010106020474686572530a0601010305020404060a040405100206020675746c696e655b0202010202010202010202010202010202010202020202010202010202010202010202010202010202010202020976657272696464656e812f020408016581080801015e0c120203250205040203070601010c010601010c0803696e67811702030104706172316c02050102060102070102050102060102070403656e745902030a0205090402040102020104020401040204010202010402061f0202010a0101190d0601080101150c0106010129010601012a01020201080101150d010805010126010a060101160e020202010a0101150c0b01080101160d10020201020501020601020201060101260d080301010e010601010e010601010e040204040e0f120b81070c090d23130b0b2218150b23390b070d06820c811d0b090d0e0b07280a280a090d230c071c1b21812c134815120c34071b07810a0e0c250a0e0f281c1518161312640c0b20170c3f15080a3b0d0b0e111209260f1311260c0914821851180f0d1323101a1c0714351e0c821418350f1d0918');
INSERT INTO heading_fts_data VALUES(137438953475,X'00000cf90c30706172746963697061746581300601012a0b03696e6781140601012d03027468760403030104030302046c61696e38060101021402043102030106010102750601010204056e6e696e677902020102020102020104020401020201020203026e3979020401020401020401020501020701080601010502087265736572766564812806010106040576656e74738114060101280506696f75736c79810d0601012f030269348100020501020501020501020701020701020704016f815e02050504726974798100020201020201020201040204010402050104020503056f6a6563745b0404050401700502068102060101040506657274696573810706010102010601010e060601010201060101020801790502030102038101020301080101816204060101291c0203090204040376656e5706010105010571756572795d0404050304040548080101390a010572616e6765140203010203050203010203811a02030102032e0204010204050204010204070205030177810c060101281c06010108020365616c8108080101815503056365697665811706010125030164813a0601010f030467657870610404050904040556080301010e03046d61696e810c0601012b070173810b060101330309706c6163656d656e7481080a0101277010050c060101040f12060205010205160206010206060a0101150f07080173810c020507060101260703696e67811d02030402030402030102030303736574810810010124086c060a08050e0101070f050f05060173810d0601012d0403756c74810b0601013207017381081a0101171115131312151513161101080101111301080101121501080101101801080101101101080101101201080101160c01080101160c020801011312010601011e010801011011010801011113020601011f010801011011010801011013030601011e01080101101201080101101102060101260108010114160108010114150206010127010c010113051d0b010801011315010801011519010801010e0e010a01011b1706010801010e0d010801012b17010801010e0d020601011e01080101101202080101111301080101140f080601010901060101090106010109010601010901060101090106010109010601010901060101090106010109010601010902036f6f740202065d02052926040101072605090703050a07040c08050907661602020112040101080b090509070112050101060c0b050a07010e040101060c0b0c030177810d0601012c040173810a0205020601012a1c060101040101738139060101050106010105010601010501060101050106010105010601010501060101050106010105010601010501060101050203616d658108080101816101020621060101121d0203010203030174816b0601010505060101050106010105010601010502036368378133020501020501020501020501020604066564756c65641c02035d02033a0202010202010202010202010202020565636f6e642d02060302050302060e02030702030102032b0205030205110c0101500e2d0d010c0101080d080d030c0101070c070c1b06010104011001010f0f0a0f0610010a0101040d0d010c01011f0f0a0f010a0101040d0d02080101150c01080101160d020c0101070d080d01080101160f0a0205030770617261746f72810e06010124010601012403017181540205010205040575656e63656302030102030102030102030102030102030102030102030205686f756c645902052f0601010942020501020602036964658150020701020803046c766572813c0601011203046e676c653b08040101027e020402016f810b0601013003026d653b08010104050304757263654b040504010405040502040102040102043606010130020470616365810e06010125010601012504016e570601011302027263570801010e0d311a01010f10141313121615131512010801010913010801010a15010801010818010801010811010801010812010801010e0c010801010e0c020801010b1201080101090f01080101081101080101091302080101091001080101081101080101081303080101090f010801010812010801010811020801010b13010801010a16010801010a15020801010a16010801010a20010801010a15010801010c1901080101060e01080101121701080101060d01080101231701080101060d02080101090f010801010812020801010913010801010b0f080601010201060101020106010102010601010201060101020106010102010601010201060101020106010102010601010202047461727481640205060205040179814a02060102070303696c6c8159020301020303056f7261676581280601010905026564810c0601012c0208796e74686574696381080601010301017481081c01011611151313121515131611120108010110070108010111080108010113140106010113010601010f0106010115010601011502080101160e01080101120d01080101130d01080101150e02080101120e01080101130d01080101130f03080101120d01080101130e01080101130d020801011611010801011712010801011711020801011414010801011c16010801011710010801011b1201060101190108010120110106010118010801013110010601011802080101120d01080101130e02080101150e020261678143020604013381390a0501010a05010a0601010a05010a0601010a05010c0501010a0505010a0501010a05010a0501010a05010a0701010a05010a0501010a0501080501010a010a0601010a05040367656402020349020404017381390802010108010802010108010802010108010802010108010802010108010802010108010802010108010802010108010802010108010802010108010202030472676574350601010402060101040204040512060101030106010104010404070104040701040406020801010306010801010406010807010107010404050102051702060203657374810806010115370206030278743806010104030a0305010105020203010203400601010475060101040203686174812a06010117030165590402072f0c010102814e140108050101250206010131010806010121010801012808040c01010424070502080101210801080101250603060101210408010120080f0a010103090906060101250402736581080c01010b81411103026973570601010234060101283a020301020303017581720601010a0203696d65130204060204130205010205810702042f020506020506020502020505057374616d707d02040102047402040102040303746c6532040304010403042408010110030202046c04020601040206010402040104020402020801020902016f51040304010403040104030401020301020343020404020404020404020406060101160402040302646f0a020418020581280a0201010309010601010a07020601020601020601020601020603020203020403026d6c815f02050102050301700702036a020302077261696c696e67810f06010123020173816202020102020102020102020102020202020102020102020102020102020202020102020102020102020102020102020302613481620206010206010206010206010207030269358168020601020601020601020601020703027033816e020601020601020601020601020601080601010502047769636581280c01010e0d0c0d03016f810a0c0101090e090e2602040202797081520205010205010d756e666f7274756e6174656c795702020304697175653506010108010801010604010601010701060101034606010103750601010303046c6573738108080101815b0306010117030772656c617465645e040405020473696e675706010107010576616c696481080a0101813b0d060a01010d0c0c010a01010d0c0c3b0601010601060101060e0205010205040275658109100101060b0a0b0a06010c0101060c0b0c0112070101060a09080b0d010c0101060a090a010c0101030d1215010e010103030a0a0c010e01010308050a0c02100101090a090b0a0a010e0501010809070a010e0101060a090a0f010e0101060b0a0b0d010206010c01010809070b010e060101060a090a010e070101060a090c03100501010809070a08010e060101060a090b010e050101060a090a0a0a0101030b0e020a0101030b0d011801010c100606090806060908010a0101030b0d020e0601010809070a010e070101060a090b020e0101060b0a0b09010601012606017381080c050101814b110205696f6c6574813d0601010f010601010f010601010f010377656236060101090301648165060101050306010105010601010503060101050206696e6e696e67812a06010118040173810c060101260d0204110601010803027468130203060203340205010205360204010204030a010181470d0902030202030102030102031b02030402031802040102050102040102040f020405020406020406020402020405036f75744f02053c020407020302046f726b7381280601013b04130b0c1b1a19100d0e17071c0b0c1d1e0a15290e0c0d09140c092e0c1219090b8208420911361a18151d79140a1f150c0c0f090a1c110882110d0a0c0d0a10814d084a0b43460e200b510d140921132c292c0b090f3414141a0f0f0b1225120d0b2d81650c170a180e114a0e');
CREATE TABLE IF NOT EXISTS 'heading_fts_idx'(segid, term, pgno, PRIMARY KEY(segid, term)) WITHOUT ROWID;
INSERT INTO heading_fts_idx VALUES(1,X'',2);
INSERT INTO heading_fts_idx VALUES(1,X'30657874',4);
INSERT INTO heading_fts_idx VALUES(1,X'3070617274',6);
CREATE TABLE IF NOT EXISTS 'heading_fts_docsize'(id INTEGER PRIMARY KEY, sz BLOB);
INSERT INTO heading_fts_docsize VALUES(2,X'0500');
INSERT INTO heading_fts_docsize VALUES(3,X'0400');
INSERT INTO heading_fts_docsize VALUES(4,X'0500');
INSERT INTO heading_fts_docsize VALUES(5,X'0500');
INSERT INTO heading_fts_docsize VALUES(6,X'0500');
INSERT INTO heading_fts_docsize VALUES(7,X'0700');
INSERT INTO heading_fts_docsize VALUES(9,X'0600');
INSERT INTO heading_fts_docsize VALUES(10,X'0700');
INSERT INTO heading_fts_docsize VALUES(11,X'0600');
INSERT INTO heading_fts_docsize VALUES(12,X'0500');
INSERT INTO heading_fts_docsize VALUES(13,X'0500');
INSERT INTO heading_fts_docsize VALUES(14,X'0500');
INSERT INTO heading_fts_docsize VALUES(15,X'0600');
INSERT INTO heading_fts_docsize VALUES(16,X'0700');
INSERT INTO heading_fts_docsize VALUES(18,X'0500');
INSERT INTO heading_fts_docsize VALUES(19,X'0500');
INSERT INTO heading_fts_docsize VALUES(20,X'0500');
INSERT INTO heading_fts_docsize VALUES(21,X'0500');
INSERT INTO heading_fts_docsize VALUES(22,X'0700');
INSERT INTO heading_fts_docsize VALUES(24,X'0500');
INSERT INTO heading_fts_docsize VALUES(25,X'0500');
INSERT INTO heading_fts_docsize VALUES(26,X'0500');
INSERT INTO heading_fts_docsize VALUES(27,X'0500');
INSERT INTO heading_fts_docsize VALUES(28,X'0600');
INSERT INTO heading_fts_docsize VALUES(30,X'0600');
INSERT INTO heading_fts_docsize VALUES(31,X'0700');
INSERT INTO heading_fts_docsize VALUES(32,X'0600');
INSERT INTO heading_fts_docsize VALUES(33,X'0400');
INSERT INTO heading_fts_docsize VALUES(34,X'0800');
INSERT INTO heading_fts_docsize VALUES(35,X'0600');
INSERT INTO heading_fts_docsize VALUES(36,X'0600');
INSERT INTO heading_fts_docsize VALUES(37,X'0500');
INSERT INTO heading_fts_docsize VALUES(39,X'0500');
INSERT INTO heading_fts_docsize VALUES(40,X'0500');
INSERT INTO heading_fts_docsize VALUES(41,X'0600');
INSERT INTO heading_fts_docsize VALUES(42,X'0700');
INSERT INTO heading_fts_docsize VALUES(44,X'0700');
INSERT INTO heading_fts_docsize VALUES(45,X'0800');
INSERT INTO heading_fts_docsize VALUES(47,X'0600');
INSERT INTO heading_fts_docsize VALUES(48,X'0700');
INSERT INTO heading_fts_docsize VALUES(50,X'0700');
INSERT INTO heading_fts_docsize VALUES(51,X'0800');
INSERT INTO heading_fts_docsize VALUES(53,X'0609');
INSERT INTO heading_fts_docsize VALUES(54,X'0609');
INSERT INTO heading_fts_docsize VALUES(55,X'0608');
INSERT INTO heading_fts_docsize VALUES(56,X'0705');
INSERT INTO heading_fts_docsize VALUES(57,X'0600');
INSERT INTO heading_fts_docsize VALUES(59,X'0507');
INSERT INTO heading_fts_docsize VALUES(60,X'0200');
INSERT INTO heading_fts_docsize VALUES(61,X'0502');
INSERT INTO heading_fts_docsize VALUES(62,X'0502');
INSERT INTO heading_fts_docsize VALUES(64,X'0500');
INSERT INTO heading_fts_docsize VALUES(65,X'0500');
INSERT INTO heading_fts_docsize VALUES(66,X'0500');
INSERT INTO heading_fts_docsize VALUES(68,X'0500');
INSERT INTO heading_fts_docsize VALUES(69,X'0500');
INSERT INTO heading_fts_docsize VALUES(70,X'0500');
INSERT INTO heading_fts_docsize VALUES(71,X'0500');
INSERT INTO heading_fts_docsize VALUES(72,X'0600');
INSERT INTO heading_fts_docsize VALUES(73,X'0600');
INSERT INTO heading_fts_docsize VALUES(75,X'0707');
INSERT INTO heading_fts_docsize VALUES(76,X'0709');
INSERT INTO heading_fts_docsize VALUES(77,X'0900');
INSERT INTO heading_fts_docsize VALUES(78,X'0900');
INSERT INTO heading_fts_docsize VALUES(79,X'0800');
INSERT INTO heading_fts_docsize VALUES(81,X'0807');
INSERT INTO heading_fts_docsize VALUES(82,X'0708');
INSERT INTO heading_fts_docsize VALUES(83,X'0807');
INSERT INTO heading_fts_docsize VALUES(84,X'0600');
INSERT INTO heading_fts_docsize VALUES(85,X'0700');
INSERT INTO heading_fts_docsize VALUES(87,X'0218');
INSERT INTO heading_fts_docsize VALUES(89,X'0800');
INSERT INTO heading_fts_docsize VALUES(91,X'0600');
INSERT INTO heading_fts_docsize VALUES(92,X'0600');
INSERT INTO heading_fts_docsize VALUES(93,X'0600');
INSERT INTO heading_fts_docsize VALUES(94,X'0600');
INSERT INTO heading_fts_docsize VALUES(95,X'0600');
INSERT INTO heading_fts_docsize VALUES(96,X'0700');
INSERT INTO heading_fts_docsize VALUES(97,X'0700');
INSERT INTO heading_fts_docsize VALUES(99,X'0600');
INSERT INTO heading_fts_docsize VALUES(100,X'0600');
INSERT INTO heading_fts_docsize VALUES(101,X'0600');
INSERT INTO heading_fts_docsize VALUES(102,X'0600');
INSERT INTO heading_fts_docsize VALUES(103,X'0600');
INSERT INTO heading_fts_docsize VALUES(104,X'0700');
INSERT INTO heading_fts_docsize VALUES(105,X'0700');
INSERT INTO heading_fts_docsize VALUES(106,X'0600');
INSERT INTO heading_fts_docsize VALUES(108,X'0500');
INSERT INTO heading_fts_docsize VALUES(109,X'0600');
INSERT INTO heading_fts_docsize VALUES(110,X'0700');
INSERT INTO heading_fts_docsize VALUES(111,X'0500');
INSERT INTO heading_fts_docsize VALUES(112,X'0600');
INSERT INTO heading_fts_docsize VALUES(113,X'0700');
INSERT INTO heading_fts_docsize VALUES(115,X'0600');
INSERT INTO heading_fts_docsize VALUES(116,X'0700');
INSERT INTO heading_fts_docsize VALUES(118,X'0600');
INSERT INTO heading_fts_docsize VALUES(119,X'0700');
INSERT INTO heading_fts_docsize VALUES(121,X'0400');
INSERT INTO heading_fts_docsize VALUES(122,X'0400');
INSERT INTO heading_fts_docsize VALUES(123,X'0400');
INSERT INTO heading_fts_docsize VALUES(124,X'0507');
INSERT INTO heading_fts_docsize VALUES(125,X'0704');
INSERT INTO heading_fts_docsize VALUES(126,X'0605');
INSERT INTO heading_fts_docsize VALUES(128,X'0500');
INSERT INTO heading_fts_docsize VALUES(129,X'0500');
INSERT INTO heading_fts_docsize VALUES(130,X'0500');
INSERT INTO heading_fts_docsize VALUES(131,X'0700');
INSERT INTO heading_fts_docsize VALUES(132,X'0700');
INSERT INTO heading_fts_docsize VALUES(133,X'0700');
INSERT INTO heading_fts_docsize VALUES(135,X'0204');
INSERT INTO heading_fts_docsize VALUES(136,X'048161');
INSERT INTO heading_fts_docsize VALUES(137,X'0630');
INSERT INTO heading_fts_docsize VALUES(138,X'0427');
INSERT INTO heading_fts_docsize VALUES(139,X'0633');
INSERT INTO heading_fts_docsize VALUES(140,X'072b');
INSERT INTO heading_fts_docsize VALUES(141,X'0530');
INSERT INTO heading_fts_docsize VALUES(142,X'0526');
INSERT INTO heading_fts_docsize VALUES(143,X'0526');
INSERT INTO heading_fts_docsize VALUES(144,X'0300');
INSERT INTO heading_fts_docsize VALUES(145,X'0430');
INSERT INTO heading_fts_docsize VALUES(146,X'041e');
INSERT INTO heading_fts_docsize VALUES(147,X'0429');
INSERT INTO heading_fts_docsize VALUES(148,X'062c');
INSERT INTO heading_fts_docsize VALUES(149,X'0500');
INSERT INTO heading_fts_docsize VALUES(150,X'0220');
INSERT INTO heading_fts_docsize VALUES(151,X'0527');
INSERT INTO heading_fts_docsize VALUES(152,X'0623');
INSERT INTO heading_fts_docsize VALUES(153,X'0300');
INSERT INTO heading_fts_docsize VALUES(154,X'0200');
INSERT INTO heading_fts_docsize VALUES(155,X'0428');
INSERT INTO heading_fts_docsize VALUES(156,X'0521');
INSERT INTO heading_fts_docsize VALUES(157,X'041f');
INSERT INTO heading_fts_docsize VALUES(158,X'0300');
INSERT INTO heading_fts_docsize VALUES(159,X'0526');
INSERT INTO heading_fts_docsize VALUES(160,X'0629');
INSERT INTO heading_fts_docsize VALUES(161,X'0527');
INSERT INTO heading_fts_docsize VALUES(162,X'0300');
INSERT INTO heading_fts_docsize VALUES(163,X'042c');
INSERT INTO heading_fts_docsize VALUES(164,X'053f');
INSERT INTO heading_fts_docsize VALUES(165,X'0427');
INSERT INTO heading_fts_docsize VALUES(166,X'062f');
INSERT INTO heading_fts_docsize VALUES(167,X'021c');
INSERT INTO heading_fts_docsize VALUES(168,X'0645');
INSERT INTO heading_fts_docsize VALUES(169,X'051a');
INSERT INTO heading_fts_docsize VALUES(170,X'0543');
INSERT INTO heading_fts_docsize VALUES(171,X'051a');
INSERT INTO heading_fts_docsize VALUES(172,X'0300');
INSERT INTO heading_fts_docsize VALUES(173,X'051e');
INSERT INTO heading_fts_docsize VALUES(174,X'0621');
INSERT INTO heading_fts_docsize VALUES(175,X'0700');
INSERT INTO heading_fts_docsize VALUES(176,X'0529');
INSERT INTO heading_fts_docsize VALUES(177,X'0427');
INSERT INTO heading_fts_docsize VALUES(179,X'0500');
INSERT INTO heading_fts_docsize VALUES(180,X'0500');
INSERT INTO heading_fts_docsize VALUES(181,X'0500');
INSERT INTO heading_fts_docsize VALUES(182,X'0500');
INSERT INTO heading_fts_docsize VALUES(183,X'0600');
INSERT INTO heading_fts_docsize VALUES(185,X'050e');
INSERT INTO heading_fts_docsize VALUES(186,X'060e');
INSERT INTO heading_fts_docsize VALUES(187,X'060e');
INSERT INTO heading_fts_docsize VALUES(188,X'0511');
INSERT INTO heading_fts_docsize VALUES(189,X'050e');
INSERT INTO heading_fts_docsize VALUES(190,X'050e');
INSERT INTO heading_fts_docsize VALUES(191,X'070e');
INSERT INTO heading_fts_docsize VALUES(192,X'050e');
INSERT INTO heading_fts_docsize VALUES(193,X'050b');
INSERT INTO heading_fts_docsize VALUES(194,X'060e');
INSERT INTO heading_fts_docsize VALUES(195,X'0500');
INSERT INTO heading_fts_docsize VALUES(197,X'0500');
INSERT INTO heading_fts_docsize VALUES(198,X'0500');
INSERT INTO heading_fts_docsize VALUES(199,X'0300');
INSERT INTO heading_fts_docsize VALUES(200,X'0300');
INSERT INTO heading_fts_docsize VALUES(202,X'070c');
INSERT INTO heading_fts_docsize VALUES(203,X'080c');
INSERT INTO heading_fts_docsize VALUES(204,X'0500');
INSERT INTO heading_fts_docsize VALUES(205,X'0600');
INSERT INTO heading_fts_docsize VALUES(206,X'0500');
INSERT INTO heading_fts_docsize VALUES(207,X'0600');
INSERT INTO heading_fts_docsize VALUES(208,X'0700');
INSERT INTO heading_fts_docsize VALUES(209,X'0800');
INSERT INTO heading_fts_docsize VALUES(210,X'0500');
INSERT INTO heading_fts_docsize VALUES(211,X'0500');
INSERT INTO heading_fts_docsize VALUES(212,X'0500');
INSERT INTO heading_fts_docsize VALUES(213,X'0500');
INSERT INTO heading_fts_docsize VALUES(214,X'0600');
INSERT INTO heading_fts_docsize VALUES(215,X'0800');
INSERT INTO heading_fts_docsize VALUES(216,X'0800');
INSERT INTO heading_fts_docsize VALUES(217,X'0800');
INSERT INTO heading_fts_docsize VALUES(218,X'0800');
INSERT INTO heading_fts_docsize VALUES(220,X'0300');
INSERT INTO heading_fts_docsize VALUES(221,X'0300');
INSERT INTO heading_fts_docsize VALUES(222,X'0600');
INSERT INTO heading_fts_docsize VALUES(223,X'0500');
INSERT INTO heading_fts_docsize VALUES(224,X'0500');
INSERT INTO heading_fts_docsize VALUES(226,X'0604');
INSERT INTO heading_fts_docsize VALUES(227,X'0606');
INSERT INTO heading_fts_docsize VALUES(228,X'0604');
INSERT INTO heading_fts_docsize VALUES(229,X'0606');
INSERT INTO heading_fts_docsize VALUES(230,X'0704');
INSERT INTO heading_fts_docsize VALUES(232,X'0604');
INSERT INTO heading_fts_docsize VALUES(233,X'0606');
INSERT INTO heading_fts_docsize VALUES(234,X'0604');
INSERT INTO heading_fts_docsize VALUES(235,X'0606');
INSERT INTO heading_fts_docsize VALUES(236,X'0704');
INSERT INTO heading_fts_docsize VALUES(238,X'0604');
INSERT INTO heading_fts_docsize VALUES(239,X'0606');
INSERT INTO heading_fts_docsize VALUES(240,X'0604');
INSERT INTO heading_fts_docsize VALUES(241,X'0606');
INSERT INTO heading_fts_docsize VALUES(242,X'060b');
INSERT INTO heading_fts_docsize VALUES(243,X'0605');
CREATE TABLE IF NOT EXISTS 'heading_fts_config'(k PRIMARY KEY, v) WITHOUT ROWID;
INSERT INTO heading_fts_config VALUES('version',4);
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
PRAGMA writable_schema=OFF;
COMMIT;
