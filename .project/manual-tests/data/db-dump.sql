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
INSERT INTO files VALUES(1,'././files/file-local-todo-keywords.org',1781808483955605322,1188,NULL,1781861168);
INSERT INTO files VALUES(2,'././files/inherited-heading-tags.org',1781808923034914634,904,NULL,1781861168);
INSERT INTO files VALUES(3,'././files/multipe-title-keywords.org',1781809982908544855,321,NULL,1781861168);
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
INSERT INTO headings VALUES(97,1,NULL,0,1,-1,1188,'File-local TODO keywords','File-local TODO keywords',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(98,1,97,1,10,234,354,'TODO default keyword should stay in title','TODO default keyword should stay in title',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(99,1,97,1,13,354,479,'DONE default done keyword should stay in title','DONE default done keyword should stay in title',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(100,1,97,1,16,479,512,'open keyword with fast key','open keyword with fast key','one','open',NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(101,1,97,1,17,512,553,'another open keyword with fast key','another open keyword with fast key','two','open',NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(102,1,97,1,18,553,590,'closed keyword with fast key','closed keyword with fast key','three','closed',NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(103,1,97,1,19,590,636,'closed keyword with extended fast key','closed keyword with extended fast key','four','closed',NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(104,1,97,1,21,636,682,'open keyword from empty-done-side line','open keyword from empty-done-side line','FIVE','open',NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(105,1,97,1,22,682,736,'another open keyword from empty-done-side line','another open keyword from empty-done-side line','SIX','open',NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(106,1,97,1,24,736,771,'open keyword from TYP_TODO','open keyword from TYP_TODO','seven','open',NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(107,1,97,1,25,771,809,'closed keyword from TYP_TODO','closed keyword from TYP_TODO','eight','closed',NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(108,1,97,1,27,809,843,'open keyword from SEQ_TODO','open keyword from SEQ_TODO','nine','open',NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(109,1,97,1,28,843,900,'closed keyword from SEQ_TODO','closed keyword from SEQ_TODO','ten','closed',NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(110,1,97,1,32,900,977,'closed keyword from later TODO line','closed keyword from later TODO line','eleven','closed',NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(111,1,97,1,36,977,1034,'open keyword from line defined later in file','open keyword from line defined later in file','late_open','open',NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(112,1,97,1,37,1034,1094,'closed keyword from line defined later in file','closed keyword from line defined later in file','late_done','closed',NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(113,1,97,1,39,1094,1141,'TODO still not valid after later local lines','TODO still not valid after later local lines',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(114,1,97,1,40,1141,1188,'DONE still not valid after later local lines','DONE still not valid after later local lines',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(115,2,NULL,0,1,-1,904,'Inherited heading tags','Inherited heading tags',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(116,2,115,1,4,52,491,'parent with one tag','parent with one tag',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'["parent"]');
INSERT INTO headings VALUES(117,2,116,2,7,97,173,'child inherits parent tag','child inherits parent tag',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'["parent"]');
INSERT INTO headings VALUES(118,2,116,2,10,173,491,'child with local tag','child with local tag',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'["parent","child"]');
INSERT INTO headings VALUES(119,2,118,3,13,261,351,'grandchild inherits both','grandchild inherits both',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'["parent","child"]');
INSERT INTO headings VALUES(120,2,118,3,16,351,491,'grandchild with duplicate local tag','grandchild with duplicate local tag',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'["parent","child"]');
INSERT INTO headings VALUES(121,2,115,1,20,491,760,'second parent','second parent',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'["second"]');
INSERT INTO headings VALUES(122,2,121,2,23,573,760,'second child with local tag','second child with local tag',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'["second","child"]');
INSERT INTO headings VALUES(123,2,122,3,26,659,760,'second grandchild with extra tag','second grandchild with extra tag',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'["second","child","extra"]');
INSERT INTO headings VALUES(124,2,115,1,29,760,904,'untagged parent','untagged parent',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(125,2,124,2,31,779,853,'child with only local tag','child with only local tag',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'["local"]');
INSERT INTO headings VALUES(126,2,124,2,34,853,904,'child without tags','child without tags',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(127,3,NULL,0,1,-1,321,'Title can span multiple lines, even here','Title can span multiple lines, even here',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(128,3,127,1,7,136,321,'Unfortunately Everywhere','Unfortunately Everywhere',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
CREATE TABLE todo_keywords (
    file_id         INTEGER NOT NULL,
    keyword         TEXT NOT NULL,
    state_type      TEXT NOT NULL CHECK (state_type IN ('open', 'closed')),
    shortcut        TEXT CHECK (shortcut IS NULL OR length(shortcut) = 1),
    sequence_no     INTEGER NOT NULL,
    FOREIGN KEY (file_id)
        REFERENCES files(id)
        ON DELETE CASCADE,
    PRIMARY KEY (file_id, keyword)
);
INSERT INTO todo_keywords VALUES(1,'one','open','t',0);
INSERT INTO todo_keywords VALUES(1,'two','open','n',1);
INSERT INTO todo_keywords VALUES(1,'FIVE','open',NULL,2);
INSERT INTO todo_keywords VALUES(1,'SIX','open',NULL,3);
INSERT INTO todo_keywords VALUES(1,'seven','open',NULL,4);
INSERT INTO todo_keywords VALUES(1,'nine','open',NULL,5);
INSERT INTO todo_keywords VALUES(1,'late_open','open',NULL,6);
INSERT INTO todo_keywords VALUES(1,'three','closed','d',7);
INSERT INTO todo_keywords VALUES(1,'four','closed','w',8);
INSERT INTO todo_keywords VALUES(1,'eight','closed',NULL,9);
INSERT INTO todo_keywords VALUES(1,'ten','closed',NULL,10);
INSERT INTO todo_keywords VALUES(1,'eleven','closed','c',11);
INSERT INTO todo_keywords VALUES(1,'late_done','closed',NULL,12);
INSERT INTO todo_keywords VALUES(2,'TODO','open',NULL,0);
INSERT INTO todo_keywords VALUES(2,'DONE','closed',NULL,1);
INSERT INTO todo_keywords VALUES(3,'TODO','open',NULL,0);
INSERT INTO todo_keywords VALUES(3,'DONE','closed',NULL,1);
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
INSERT INTO keywords VALUES(36,97,'TITLE','File-local TODO keywords',NULL);
INSERT INTO keywords VALUES(37,97,'STARTUP','showall',NULL);
INSERT INTO keywords VALUES(38,97,'TODO','one(t) two(n) | three(d) four(w@)',NULL);
INSERT INTO keywords VALUES(39,97,'TODO','FIVE SIX |',NULL);
INSERT INTO keywords VALUES(40,97,'TYP_TODO','seven | eight',NULL);
INSERT INTO keywords VALUES(41,97,'SEQ_TODO','nine | ten',NULL);
INSERT INTO keywords VALUES(42,97,'TODO','| eleven(c)',NULL);
INSERT INTO keywords VALUES(43,97,'TODO','late_open | late_done',NULL);
INSERT INTO keywords VALUES(44,115,'TITLE','Inherited heading tags',NULL);
INSERT INTO keywords VALUES(45,115,'STARTUP','showall',NULL);
INSERT INTO keywords VALUES(46,127,'TITLE','Title can span',NULL);
INSERT INTO keywords VALUES(47,127,'TITLE','multiple lines,',NULL);
INSERT INTO keywords VALUES(48,127,'AUTHOR','Hubisan',NULL);
CREATE TABLE properties (
    id              INTEGER PRIMARY KEY,
    heading_id      INTEGER NOT NULL,
    key             TEXT NOT NULL,
    value           TEXT,
    source          TEXT NOT NULL CHECK (
                        source IN ('property_keyword', 'property_drawer', 'category_keyword')
                    ),
    inherited       INTEGER NOT NULL DEFAULT 0 CHECK (inherited IN (0, 1)),
    line_number     INTEGER,
    FOREIGN KEY (heading_id)
        REFERENCES headings(id)
        ON DELETE CASCADE,
    UNIQUE (heading_id, key, source, inherited)
);
CREATE TABLE tags (
    heading_id      INTEGER NOT NULL,
    tag             TEXT NOT NULL,
    inherited       INTEGER NOT NULL DEFAULT 0 CHECK (inherited IN (0, 1)),
    FOREIGN KEY (heading_id)
        REFERENCES headings(id)
        ON DELETE CASCADE,
    PRIMARY KEY (heading_id, tag, inherited)
);
INSERT INTO tags VALUES(116,'parent',0);
INSERT INTO tags VALUES(118,'child',0);
INSERT INTO tags VALUES(120,'parent',0);
INSERT INTO tags VALUES(121,'second',0);
INSERT INTO tags VALUES(122,'child',0);
INSERT INTO tags VALUES(123,'extra',0);
INSERT INTO tags VALUES(125,'local',0);
CREATE TABLE links (
    id                  INTEGER PRIMARY KEY,
    file_id             INTEGER NOT NULL,
    heading_id          INTEGER NOT NULL,
    byte_start          INTEGER NOT NULL,
    byte_end            INTEGER NOT NULL CHECK (byte_end >= byte_start),
    line_number         INTEGER,
    link_type           TEXT,
    target              TEXT NOT NULL,
    target_absolute     TEXT,
    raw_link            TEXT NOT NULL,
    description         TEXT,
    format              TEXT CHECK (format IN ('plain', 'bracket', 'angle') OR format IS NULL),
    search_option       TEXT,
    relation            TEXT,
    resolved_file_id    INTEGER,
    resolved_heading_id INTEGER,
    resolved            INTEGER NOT NULL DEFAULT 0 CHECK (resolved IN (0, 1)),
    broken              INTEGER NOT NULL DEFAULT 0 CHECK (broken IN (0, 1)),
    diagnostic          TEXT,
    FOREIGN KEY (file_id)
        REFERENCES files(id)
        ON DELETE CASCADE,
    FOREIGN KEY (heading_id)
        REFERENCES headings(id)
        ON DELETE CASCADE,
    FOREIGN KEY (resolved_file_id)
        REFERENCES files(id)
        ON DELETE SET NULL,
    FOREIGN KEY (resolved_heading_id)
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
INSERT INTO outline_path VALUES(97,1,NULL,0,'0000','["File-local TODO keywords"]');
INSERT INTO outline_path VALUES(98,1,97,1,'0000.0001','["File-local TODO keywords","TODO default keyword should stay in title"]');
INSERT INTO outline_path VALUES(99,1,97,1,'0000.0002','["File-local TODO keywords","DONE default done keyword should stay in title"]');
INSERT INTO outline_path VALUES(100,1,97,1,'0000.0003','["File-local TODO keywords","open keyword with fast key"]');
INSERT INTO outline_path VALUES(101,1,97,1,'0000.0004','["File-local TODO keywords","another open keyword with fast key"]');
INSERT INTO outline_path VALUES(102,1,97,1,'0000.0005','["File-local TODO keywords","closed keyword with fast key"]');
INSERT INTO outline_path VALUES(103,1,97,1,'0000.0006','["File-local TODO keywords","closed keyword with extended fast key"]');
INSERT INTO outline_path VALUES(104,1,97,1,'0000.0007','["File-local TODO keywords","open keyword from empty-done-side line"]');
INSERT INTO outline_path VALUES(105,1,97,1,'0000.0008','["File-local TODO keywords","another open keyword from empty-done-side line"]');
INSERT INTO outline_path VALUES(106,1,97,1,'0000.0009','["File-local TODO keywords","open keyword from TYP_TODO"]');
INSERT INTO outline_path VALUES(107,1,97,1,'0000.0010','["File-local TODO keywords","closed keyword from TYP_TODO"]');
INSERT INTO outline_path VALUES(108,1,97,1,'0000.0011','["File-local TODO keywords","open keyword from SEQ_TODO"]');
INSERT INTO outline_path VALUES(109,1,97,1,'0000.0012','["File-local TODO keywords","closed keyword from SEQ_TODO"]');
INSERT INTO outline_path VALUES(110,1,97,1,'0000.0013','["File-local TODO keywords","closed keyword from later TODO line"]');
INSERT INTO outline_path VALUES(111,1,97,1,'0000.0014','["File-local TODO keywords","open keyword from line defined later in file"]');
INSERT INTO outline_path VALUES(112,1,97,1,'0000.0015','["File-local TODO keywords","closed keyword from line defined later in file"]');
INSERT INTO outline_path VALUES(113,1,97,1,'0000.0016','["File-local TODO keywords","TODO still not valid after later local lines"]');
INSERT INTO outline_path VALUES(114,1,97,1,'0000.0017','["File-local TODO keywords","DONE still not valid after later local lines"]');
INSERT INTO outline_path VALUES(115,2,NULL,0,'0000','["Inherited heading tags"]');
INSERT INTO outline_path VALUES(116,2,115,1,'0000.0001','["Inherited heading tags","parent with one tag"]');
INSERT INTO outline_path VALUES(117,2,116,2,'0000.0001.0002','["Inherited heading tags","parent with one tag","child inherits parent tag"]');
INSERT INTO outline_path VALUES(118,2,116,2,'0000.0001.0003','["Inherited heading tags","parent with one tag","child with local tag"]');
INSERT INTO outline_path VALUES(119,2,118,3,'0000.0001.0003.0004','["Inherited heading tags","parent with one tag","child with local tag","grandchild inherits both"]');
INSERT INTO outline_path VALUES(120,2,118,3,'0000.0001.0003.0005','["Inherited heading tags","parent with one tag","child with local tag","grandchild with duplicate local tag"]');
INSERT INTO outline_path VALUES(121,2,115,1,'0000.0006','["Inherited heading tags","second parent"]');
INSERT INTO outline_path VALUES(122,2,121,2,'0000.0006.0007','["Inherited heading tags","second parent","second child with local tag"]');
INSERT INTO outline_path VALUES(123,2,122,3,'0000.0006.0007.0008','["Inherited heading tags","second parent","second child with local tag","second grandchild with extra tag"]');
INSERT INTO outline_path VALUES(124,2,115,1,'0000.0009','["Inherited heading tags","untagged parent"]');
INSERT INTO outline_path VALUES(125,2,124,2,'0000.0009.0010','["Inherited heading tags","untagged parent","child with only local tag"]');
INSERT INTO outline_path VALUES(126,2,124,2,'0000.0009.0011','["Inherited heading tags","untagged parent","child without tags"]');
INSERT INTO outline_path VALUES(127,3,NULL,0,'0000','["Title can span multiple lines, even here"]');
INSERT INTO outline_path VALUES(128,3,127,1,'0000.0001','["Title can span multiple lines, even here","Unfortunately Everywhere"]');
CREATE UNIQUE INDEX uq_headings_file_level0
    ON headings(file_id)
    WHERE level = 0;
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
CREATE INDEX idx_todo_keywords_file_state
    ON todo_keywords(file_id, state_type);
CREATE INDEX idx_keywords_heading
    ON keywords(heading_id);
CREATE INDEX idx_keywords_keyword
    ON keywords(keyword);
CREATE INDEX idx_properties_heading
    ON properties(heading_id);
CREATE INDEX idx_properties_key_value
    ON properties(key, value);
CREATE INDEX idx_properties_id
    ON properties(value)
    WHERE key = 'ID';
CREATE INDEX idx_properties_custom_id
    ON properties(value)
    WHERE key = 'CUSTOM_ID';
CREATE INDEX idx_tags_tag
    ON tags(tag);
CREATE INDEX idx_tags_heading
    ON tags(heading_id);
CREATE INDEX idx_links_heading
    ON links(heading_id);
CREATE INDEX idx_links_target
    ON links(target);
CREATE INDEX idx_links_resolved_file
    ON links(resolved_file_id);
CREATE INDEX idx_links_resolved_heading
    ON links(resolved_heading_id);
CREATE INDEX idx_outline_file_materialized_path
    ON outline_path(file_id, materialized_path);
CREATE INDEX idx_outline_parent
    ON outline_path(parent_id);
COMMIT;
