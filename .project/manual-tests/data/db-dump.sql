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
INSERT INTO files VALUES(1,'/home/hubisan/projects/coding/org-files-db/.project/manual-tests/files/file-local-todo-keywords.org',1781808483955605322,1188,NULL,1782245208);
INSERT INTO files VALUES(2,'/home/hubisan/projects/coding/org-files-db/.project/manual-tests/files/keywords.org',1782237960833303823,2276,NULL,1782245208);
INSERT INTO files VALUES(3,'/home/hubisan/projects/coding/org-files-db/.project/manual-tests/files/multipe-title-keywords.org',1781809982908544855,321,NULL,1782245208);
INSERT INTO files VALUES(4,'/home/hubisan/projects/coding/org-files-db/.project/manual-tests/files/planning-lines.org',1782129087807207205,1163,NULL,1782245208);
INSERT INTO files VALUES(5,'/home/hubisan/projects/coding/org-files-db/.project/manual-tests/files/properties.org',1782218329407221851,2783,NULL,1782245208);
INSERT INTO files VALUES(6,'/home/hubisan/projects/coding/org-files-db/.project/manual-tests/files/tags.org',1782243198853649058,1307,NULL,1782245208);
INSERT INTO files VALUES(7,'/home/hubisan/projects/coding/org-files-db/.project/manual-tests/files/timestamp-repeaters.org',1782159459016426918,1626,NULL,1782245208);
INSERT INTO files VALUES(8,'/home/hubisan/projects/coding/org-files-db/.project/manual-tests/files/timestamps.org',1782153027780066037,299,NULL,1782245208);
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
INSERT INTO headings VALUES(1,1,NULL,0,1,-1,1188,'File-local TODO keywords','File-local TODO keywords',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(2,1,1,1,10,234,354,'TODO default keyword should stay in title','TODO default keyword should stay in title',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(3,1,1,1,13,354,479,'DONE default done keyword should stay in title','DONE default done keyword should stay in title',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(4,1,1,1,16,479,512,'open keyword with fast key','open keyword with fast key','one','open',NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(5,1,1,1,17,512,553,'another open keyword with fast key','another open keyword with fast key','two','open',NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(6,1,1,1,18,553,590,'closed keyword with fast key','closed keyword with fast key','three','closed',NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(7,1,1,1,19,590,636,'closed keyword with extended fast key','closed keyword with extended fast key','four','closed',NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(8,1,1,1,21,636,682,'open keyword from empty-done-side line','open keyword from empty-done-side line','FIVE','open',NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(9,1,1,1,22,682,736,'another open keyword from empty-done-side line','another open keyword from empty-done-side line','SIX','open',NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(10,1,1,1,24,736,771,'open keyword from TYP_TODO','open keyword from TYP_TODO','seven','open',NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(11,1,1,1,25,771,809,'closed keyword from TYP_TODO','closed keyword from TYP_TODO','eight','closed',NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(12,1,1,1,27,809,843,'open keyword from SEQ_TODO','open keyword from SEQ_TODO','nine','open',NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(13,1,1,1,28,843,900,'closed keyword from SEQ_TODO','closed keyword from SEQ_TODO','ten','closed',NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(14,1,1,1,32,900,977,'closed keyword from later TODO line','closed keyword from later TODO line','eleven','closed',NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(15,1,1,1,36,977,1034,'open keyword from line defined later in file','open keyword from line defined later in file','late_open','open',NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(16,1,1,1,37,1034,1094,'closed keyword from line defined later in file','closed keyword from line defined later in file','late_done','closed',NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(17,1,1,1,39,1094,1141,'TODO still not valid after later local lines','TODO still not valid after later local lines',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(18,1,1,1,40,1141,1188,'DONE still not valid after later local lines','DONE still not valid after later local lines',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(19,2,NULL,0,1,-1,2276,'Keyword Parsing Fixture Later Title','Keyword Parsing Fixture Later Title',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(20,2,19,1,7,141,604,'First heading','First heading','WURST','open',NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(21,2,20,2,19,454,604,'Child heading','Child heading',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(22,2,19,1,26,604,798,'Second heading','Second heading','IDEA','open',NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(23,2,19,1,33,798,1264,'Boundary: keyword-looking body text','Boundary: keyword-looking body text',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(24,2,19,1,49,1264,2276,'Expected behavior','Expected behavior',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(25,3,NULL,0,1,-1,321,'Title can span multiple lines, even here','Title can span multiple lines, even here',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(26,3,25,1,7,136,321,'Unfortunately Everywhere','Unfortunately Everywhere',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(27,4,NULL,0,1,-1,1163,'Planning timestamp','Planning timestamp',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(28,4,27,1,4,49,1163,'Planning','Planning',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(29,4,28,2,6,62,111,'Simple scheduled','Simple scheduled',NULL,NULL,NULL,'<2024-11-20 Wed>',1732060800,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(30,4,28,2,9,111,158,'Simple deadline','Simple deadline',NULL,NULL,NULL,NULL,NULL,'<2024-12-01 Sun>',1733011200,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(31,4,28,2,12,158,201,'Simple closed','Simple closed',NULL,NULL,NULL,NULL,NULL,NULL,NULL,'[2024-11-21 Thu]',1732147200,0,0,'[]');
INSERT INTO headings VALUES(32,4,28,2,15,201,310,'All on one planning line','All on one planning line',NULL,NULL,NULL,'<2024-11-20 Wed>',1732060800,'<2024-12-01 Sun>',1733011200,'[2024-11-21 Thu]',1732147200,0,0,'[]');
INSERT INTO headings VALUES(33,4,28,2,18,310,358,'With time','With time',NULL,NULL,NULL,'<2024-11-20 Wed 09:30>',1732095000,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(34,4,28,2,21,358,422,'Time range same day','Time range same day',NULL,NULL,NULL,'<2024-11-20 Wed 09:30-11:00>',1732095000,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(35,4,28,2,24,422,482,'Date range','Date range',NULL,NULL,NULL,NULL,NULL,'<2024-12-01 Sun>--<2024-12-03 Tue>',1733011200,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(36,4,28,2,27,482,527,'Repeater','Repeater',NULL,NULL,NULL,'<2024-11-20 Wed +1w>',1732060800,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(37,4,28,2,30,527,582,'Diary expression','Diary expression',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(38,4,28,2,33,582,704,'Multiple same keyword','Multiple same keyword',NULL,NULL,NULL,'<2024-11-21 Thu>',1732147200,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(39,4,28,2,37,704,1163,'Not valid','Not valid',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(40,4,39,3,39,718,827,'Multiple planning lines','Multiple planning lines',NULL,NULL,NULL,'<2024-11-20 Wed>',1732060800,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(41,4,39,3,44,827,922,'Planning not immediately after headline','Planning not immediately after headline',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(42,4,39,3,48,922,1009,'Looks like planning in body','Looks like planning in body',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(43,4,39,3,51,1009,1078,'Lowercase should probably not count','Lowercase should probably not count',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(44,4,39,3,54,1078,1163,'Multiple same keyword','Multiple same keyword',NULL,NULL,NULL,'<2024-11-21 Thu>',1732147200,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(45,5,NULL,0,1,-1,2783,'Org Property and Keyword Test','Org Property and Keyword Test',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'["project","work"]');
INSERT INTO headings VALUES(46,5,45,1,16,465,638,'Task with multiple drawer properties','Task with multiple drawer properties',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'["project","work"]');
INSERT INTO headings VALUES(47,5,45,1,26,638,858,'Task with duplicate drawer properties','Task with duplicate drawer properties',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'["project","work"]');
INSERT INTO headings VALUES(48,5,45,1,35,858,1094,'Task with append operator in drawer','Task with append operator in drawer',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'["project","work"]');
INSERT INTO headings VALUES(49,5,45,1,45,1094,1304,'Task with mixed-case keys','Task with mixed-case keys',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'["project","work"]');
INSERT INTO headings VALUES(50,5,45,1,58,1304,1437,'Task with empty property accepted by Orgize','Task with empty property accepted by Orgize',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'["project","work"]');
INSERT INTO headings VALUES(51,5,45,1,67,1437,1718,'Task with Orgize empty-property limitation','Task with Orgize empty-property limitation',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'["project","work"]');
INSERT INTO headings VALUES(52,5,45,1,76,1718,1984,'Task after file-level property keywords','Task after file-level property keywords',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'["project","work"]');
INSERT INTO headings VALUES(53,5,45,1,83,1984,2264,'Task after later file-level keywords','Task after later file-level keywords',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'["project","work"]');
INSERT INTO headings VALUES(54,5,45,1,89,2264,2783,'Boundary: property-like but not properties','Boundary: property-like but not properties',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'["project","work"]');
INSERT INTO headings VALUES(55,6,NULL,0,1,-1,1307,'Tags and FILETAGS Fixture','Tags and FILETAGS Fixture',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'["file","project","later","extra"]');
INSERT INTO headings VALUES(56,6,55,1,5,82,242,'Parent','Parent',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'["file","project","later","extra","parent"]');
INSERT INTO headings VALUES(57,6,56,2,8,114,242,'Child','Child',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'["file","project","later","extra","parent","child"]');
INSERT INTO headings VALUES(58,6,57,3,11,144,242,'Grandchild','Grandchild',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'["file","project","later","extra","parent","child","grandchild"]');
INSERT INTO headings VALUES(59,6,55,1,14,242,267,'Sibling','Sibling',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'["file","project","later","extra"]');
INSERT INTO headings VALUES(60,6,55,1,17,267,370,'Duplicate Local','Duplicate Local',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'["file","project","later","extra","sibling"]');
INSERT INTO headings VALUES(61,6,55,1,22,370,455,'After Later FILETAGS','After Later FILETAGS',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'["file","project","later","extra","after"]');
INSERT INTO headings VALUES(62,6,55,1,25,455,894,'parent with one tag','parent with one tag',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'["file","project","later","extra","parent"]');
INSERT INTO headings VALUES(63,6,62,2,28,500,576,'child inherits parent tag','child inherits parent tag',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'["file","project","later","extra","parent"]');
INSERT INTO headings VALUES(64,6,62,2,31,576,894,'child with local tag','child with local tag',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'["file","project","later","extra","parent","child"]');
INSERT INTO headings VALUES(65,6,64,3,34,664,754,'grandchild inherits both','grandchild inherits both',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'["file","project","later","extra","parent","child"]');
INSERT INTO headings VALUES(66,6,64,3,37,754,894,'grandchild with duplicate local tag','grandchild with duplicate local tag',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'["file","project","later","extra","parent","child"]');
INSERT INTO headings VALUES(67,6,55,1,41,894,1163,'second parent','second parent',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'["file","project","later","extra","second"]');
INSERT INTO headings VALUES(68,6,67,2,44,976,1163,'second child with local tag','second child with local tag',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'["file","project","later","extra","second","child"]');
INSERT INTO headings VALUES(69,6,68,3,47,1062,1163,'second grandchild with extra tag','second grandchild with extra tag',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'["file","project","later","extra","second","child"]');
INSERT INTO headings VALUES(70,6,55,1,50,1163,1307,'untagged parent','untagged parent',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'["file","project","later","extra"]');
INSERT INTO headings VALUES(71,6,70,2,52,1182,1256,'child with only local tag','child with only local tag',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'["file","project","later","extra","local"]');
INSERT INTO headings VALUES(72,6,70,2,55,1256,1307,'child without tags','child without tags',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'["file","project","later","extra"]');
INSERT INTO headings VALUES(73,7,NULL,0,1,-1,1626,'timestamp-repeaters','timestamp-repeaters',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(74,7,73,1,1,0,180,'Repeater markers','Repeater markers',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(75,7,74,2,3,20,70,'Cumulate plus','Cumulate plus',NULL,NULL,NULL,'<2024-11-20 Wed +1w>',1732060800,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(76,7,74,2,6,70,126,'Catch up plus plus','Catch up plus plus',NULL,NULL,NULL,'<2024-11-20 Wed ++1m>',1732060800,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(77,7,74,2,9,126,180,'Restart dot plus','Restart dot plus',NULL,NULL,NULL,'<2024-11-20 Wed .+2d>',1732060800,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(78,7,73,1,12,180,448,'Repeater units','Repeater units',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(79,7,78,2,14,198,248,'Repeater hour','Repeater hour',NULL,NULL,NULL,'<2024-11-20 Wed +3h>',1732060800,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(80,7,78,2,17,248,297,'Repeater day','Repeater day',NULL,NULL,NULL,'<2024-11-20 Wed +3d>',1732060800,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(81,7,78,2,20,297,347,'Repeater week','Repeater week',NULL,NULL,NULL,'<2024-11-20 Wed +3w>',1732060800,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(82,7,78,2,23,347,398,'Repeater month','Repeater month',NULL,NULL,NULL,'<2024-11-20 Wed +3m>',1732060800,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(83,7,78,2,26,398,448,'Repeater year','Repeater year',NULL,NULL,NULL,'<2024-11-20 Wed +3y>',1732060800,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(84,7,73,1,29,448,677,'Repeater deadline part','Repeater deadline part',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(85,7,84,2,31,474,540,'Repeater with deadline day','Repeater with deadline day',NULL,NULL,NULL,'<2024-11-20 Wed +1w/2d>',1732060800,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(86,7,84,2,34,540,608,'Repeater with deadline week','Repeater with deadline week',NULL,NULL,NULL,'<2024-11-20 Wed ++1m/1w>',1732060800,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(87,7,84,2,37,608,677,'Repeater with deadline month','Repeater with deadline month',NULL,NULL,NULL,'<2024-11-20 Wed .+1y/2m>',1732060800,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(88,7,73,1,40,677,944,'Warning delays','Warning delays',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(89,7,88,2,42,695,742,'Warning all','Warning all',NULL,NULL,NULL,NULL,NULL,'<2024-12-01 Sun -5d>',1733011200,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(90,7,88,2,45,742,792,'Warning first','Warning first',NULL,NULL,NULL,NULL,NULL,'<2024-12-01 Sun --2w>',1733011200,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(91,7,88,2,48,792,846,'Warning hour','Warning hour',NULL,NULL,NULL,NULL,NULL,'<2024-12-01 Sun 09:30 -3h>',1733045400,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(92,7,88,2,51,846,895,'Warning month','Warning month',NULL,NULL,NULL,NULL,NULL,'<2024-12-01 Sun -1m>',1733011200,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(93,7,88,2,54,895,944,'Warning year','Warning year',NULL,NULL,NULL,NULL,NULL,'<2024-12-01 Sun --1y>',1733011200,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(94,7,73,1,57,944,1210,'Repeater and warning combinations','Repeater and warning combinations',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(95,7,94,2,59,981,1041,'Repeater and warning','Repeater and warning',NULL,NULL,NULL,NULL,NULL,'<2024-12-01 Sun +1w -5d>',1733011200,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(96,7,94,2,62,1041,1133,'Catch up repeater with deadline part and warning','Catch up repeater with deadline part and warning',NULL,NULL,NULL,NULL,NULL,'<2024-12-01 Sun ++1m/2d -5d>',1733011200,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(97,7,94,2,65,1133,1210,'Restart repeater with first warning','Restart repeater with first warning',NULL,NULL,NULL,NULL,NULL,'<2024-12-01 Sun .+2w --1w>',1733011200,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(98,7,73,1,68,1210,1453,'Time and range combinations','Time and range combinations',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(99,7,98,2,70,1241,1302,'Time with repeater','Time with repeater',NULL,NULL,NULL,'<2024-11-20 Wed 09:30 +1w>',1732095000,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(100,7,98,2,73,1302,1375,'Time range with repeater','Time range with repeater',NULL,NULL,NULL,'<2024-11-20 Wed 09:30-11:00 +1w>',1732095000,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(101,7,98,2,76,1375,1453,'Date range with repeater','Date range with repeater',NULL,NULL,NULL,NULL,NULL,'<2024-12-01 Sun>--<2024-12-03 Tue +1w>',1733011200,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(102,7,73,1,79,1453,1532,'Inactive timestamp with repeater','Inactive timestamp with repeater',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(103,7,102,2,81,1489,1532,'Inactive repeater','Inactive repeater',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(104,7,73,1,84,1532,1626,'Diary negative case','Diary negative case',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(105,7,104,2,86,1555,1626,'Diary with apparent repeater text','Diary with apparent repeater text',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(106,8,NULL,0,1,-1,299,'Timestamps','Timestamps',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(107,8,106,1,4,40,91,'Meet Peter at the movies','Meet Peter at the movies',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(108,8,106,1,7,91,152,'Discussion on climate change','Discussion on climate change',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(109,8,106,1,10,152,201,'My days off','My days off',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(110,8,106,1,14,201,299,'Can be anywhere','Can be anywhere',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
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
INSERT INTO todo_keywords VALUES(2,'NEXT','open',NULL,1);
INSERT INTO todo_keywords VALUES(2,'IDEA','open','i',2);
INSERT INTO todo_keywords VALUES(2,'WURST','open','w',3);
INSERT INTO todo_keywords VALUES(2,'PLAN','open','p',4);
INSERT INTO todo_keywords VALUES(2,'BUILD','open','b',5);
INSERT INTO todo_keywords VALUES(2,'WAITING','open','w',6);
INSERT INTO todo_keywords VALUES(2,'DONE','closed',NULL,7);
INSERT INTO todo_keywords VALUES(2,'CANCELED','closed',NULL,8);
INSERT INTO todo_keywords VALUES(3,'TODO','open',NULL,0);
INSERT INTO todo_keywords VALUES(3,'DONE','closed',NULL,1);
INSERT INTO todo_keywords VALUES(4,'TODO','open',NULL,0);
INSERT INTO todo_keywords VALUES(4,'DONE','closed',NULL,1);
INSERT INTO todo_keywords VALUES(5,'TODO','open',NULL,0);
INSERT INTO todo_keywords VALUES(5,'DONE','closed',NULL,1);
INSERT INTO todo_keywords VALUES(6,'TODO','open',NULL,0);
INSERT INTO todo_keywords VALUES(6,'DONE','closed',NULL,1);
INSERT INTO todo_keywords VALUES(7,'TODO','open',NULL,0);
INSERT INTO todo_keywords VALUES(7,'DONE','closed',NULL,1);
INSERT INTO todo_keywords VALUES(8,'TODO','open',NULL,0);
INSERT INTO todo_keywords VALUES(8,'DONE','closed',NULL,1);
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
INSERT INTO keywords VALUES(1,1,'TITLE','File-local TODO keywords',1);
INSERT INTO keywords VALUES(2,1,'STARTUP','showall',2);
INSERT INTO keywords VALUES(3,1,'TODO','one(t) two(n) | three(d) four(w@)',3);
INSERT INTO keywords VALUES(4,1,'TODO','FIVE SIX |',4);
INSERT INTO keywords VALUES(5,1,'TYP_TODO','seven | eight',5);
INSERT INTO keywords VALUES(6,1,'SEQ_TODO','nine | ten',6);
INSERT INTO keywords VALUES(7,1,'TODO','| eleven(c)',30);
INSERT INTO keywords VALUES(8,1,'TODO','late_open | late_done',34);
INSERT INTO keywords VALUES(9,19,'TITLE','Keyword Parsing Fixture',1);
INSERT INTO keywords VALUES(10,19,'STARTUP','showall',2);
INSERT INTO keywords VALUES(11,19,'AUTHOR','First Author',3);
INSERT INTO keywords VALUES(12,19,'PROPERTY','before_prop before-value',4);
INSERT INTO keywords VALUES(13,19,'CATEGORY','before-category',5);
INSERT INTO keywords VALUES(14,19,'AUTHOR','Later Author',10);
INSERT INTO keywords VALUES(15,19,'OPTIONS','toc:nil num:t',11);
INSERT INTO keywords VALUES(16,19,'PROPERTY','after_prop after-value',12);
INSERT INTO keywords VALUES(17,19,'PROPERTY','repeated_prop first',13);
INSERT INTO keywords VALUES(18,19,'PROPERTY','repeated_prop second',14);
INSERT INTO keywords VALUES(19,19,'PROPERTY','appended_prop base',15);
INSERT INTO keywords VALUES(20,19,'PROPERTY','appended_prop+ extra',16);
INSERT INTO keywords VALUES(21,19,'CATEGORY','after-category',17);
INSERT INTO keywords VALUES(22,19,'TITLE','Later Title',22);
INSERT INTO keywords VALUES(23,19,'EXPORT_FILE_NAME','later-export-name',23);
INSERT INTO keywords VALUES(24,19,'STARTUP','content',24);
INSERT INTO keywords VALUES(25,19,'TODO','TODO NEXT | DONE CANCELED',29);
INSERT INTO keywords VALUES(26,19,'SEQ_TODO','IDEA(i) WURST(w) PLAN(p) BUILD(b) | DONE(d)',30);
INSERT INTO keywords VALUES(27,19,'TYP_TODO','WAITING(w) | CANCELED(c)',31);
INSERT INTO keywords VALUES(28,25,'TITLE','Title can span',1);
INSERT INTO keywords VALUES(29,25,'TITLE','multiple lines,',2);
INSERT INTO keywords VALUES(30,25,'AUTHOR','Hubisan',3);
INSERT INTO keywords VALUES(31,25,'TITLE','even here',9);
INSERT INTO keywords VALUES(32,27,'TITLE','Planning timestamp',1);
INSERT INTO keywords VALUES(33,27,'STARTUP','showall',2);
INSERT INTO keywords VALUES(34,45,'TITLE','Org Property and Keyword Test',6);
INSERT INTO keywords VALUES(35,45,'STARTUP','showall',7);
INSERT INTO keywords VALUES(36,45,'CATEGORY','category_keyword_value',8);
INSERT INTO keywords VALUES(37,45,'PROPERTY','Effort_ALL 0:10 0:30 1:00',9);
INSERT INTO keywords VALUES(38,45,'PROPERTY','keyword_property valid',10);
INSERT INTO keywords VALUES(39,45,'PROPERTY','keyword_overwritten_by_second invalid',11);
INSERT INTO keywords VALUES(40,45,'PROPERTY','keyword_overwritten_by_second valid',12);
INSERT INTO keywords VALUES(41,45,'PROPERTY','keyword_append foo=1',13);
INSERT INTO keywords VALUES(42,45,'PROPERTY','keyword_append+ bar=2',14);
INSERT INTO keywords VALUES(43,45,'PROPERTY','later_keyword_property works_everywhere',80);
INSERT INTO keywords VALUES(44,45,'CATEGORY','later_category_keyword',81);
INSERT INTO keywords VALUES(45,45,'FILETAGS',':project:work:',90);
INSERT INTO keywords VALUES(46,45,'TAGS','work(w) home(h)',91);
INSERT INTO keywords VALUES(47,45,'COLUMNS','%TODO %50ITEM %Effort{:} %CLOCKSUM',92);
INSERT INTO keywords VALUES(48,45,'CONSTANTS','c=299792458',93);
INSERT INTO keywords VALUES(49,45,'AUTHOR','Jane Doe',94);
INSERT INTO keywords VALUES(50,45,'OPTIONS','toc:nil num:t',95);
INSERT INTO keywords VALUES(51,55,'TITLE','Tags and FILETAGS Fixture',1);
INSERT INTO keywords VALUES(52,55,'STARTUP','showall',2);
INSERT INTO keywords VALUES(53,55,'FILETAGS',':file:project:',3);
INSERT INTO keywords VALUES(54,55,'FILETAGS',':later:extra:',20);
INSERT INTO keywords VALUES(55,106,'TITLE','Timestamps',1);
INSERT INTO keywords VALUES(56,106,'STARTUP','showall',2);
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
INSERT INTO outline_path VALUES(1,1,NULL,0,'0000','["File-local TODO keywords"]');
INSERT INTO outline_path VALUES(2,1,1,1,'0000.0001','["File-local TODO keywords","TODO default keyword should stay in title"]');
INSERT INTO outline_path VALUES(3,1,1,1,'0000.0002','["File-local TODO keywords","DONE default done keyword should stay in title"]');
INSERT INTO outline_path VALUES(4,1,1,1,'0000.0003','["File-local TODO keywords","open keyword with fast key"]');
INSERT INTO outline_path VALUES(5,1,1,1,'0000.0004','["File-local TODO keywords","another open keyword with fast key"]');
INSERT INTO outline_path VALUES(6,1,1,1,'0000.0005','["File-local TODO keywords","closed keyword with fast key"]');
INSERT INTO outline_path VALUES(7,1,1,1,'0000.0006','["File-local TODO keywords","closed keyword with extended fast key"]');
INSERT INTO outline_path VALUES(8,1,1,1,'0000.0007','["File-local TODO keywords","open keyword from empty-done-side line"]');
INSERT INTO outline_path VALUES(9,1,1,1,'0000.0008','["File-local TODO keywords","another open keyword from empty-done-side line"]');
INSERT INTO outline_path VALUES(10,1,1,1,'0000.0009','["File-local TODO keywords","open keyword from TYP_TODO"]');
INSERT INTO outline_path VALUES(11,1,1,1,'0000.0010','["File-local TODO keywords","closed keyword from TYP_TODO"]');
INSERT INTO outline_path VALUES(12,1,1,1,'0000.0011','["File-local TODO keywords","open keyword from SEQ_TODO"]');
INSERT INTO outline_path VALUES(13,1,1,1,'0000.0012','["File-local TODO keywords","closed keyword from SEQ_TODO"]');
INSERT INTO outline_path VALUES(14,1,1,1,'0000.0013','["File-local TODO keywords","closed keyword from later TODO line"]');
INSERT INTO outline_path VALUES(15,1,1,1,'0000.0014','["File-local TODO keywords","open keyword from line defined later in file"]');
INSERT INTO outline_path VALUES(16,1,1,1,'0000.0015','["File-local TODO keywords","closed keyword from line defined later in file"]');
INSERT INTO outline_path VALUES(17,1,1,1,'0000.0016','["File-local TODO keywords","TODO still not valid after later local lines"]');
INSERT INTO outline_path VALUES(18,1,1,1,'0000.0017','["File-local TODO keywords","DONE still not valid after later local lines"]');
INSERT INTO outline_path VALUES(19,2,NULL,0,'0000','["Keyword Parsing Fixture Later Title"]');
INSERT INTO outline_path VALUES(20,2,19,1,'0000.0001','["Keyword Parsing Fixture Later Title","First heading"]');
INSERT INTO outline_path VALUES(21,2,20,2,'0000.0001.0001','["Keyword Parsing Fixture Later Title","First heading","Child heading"]');
INSERT INTO outline_path VALUES(22,2,19,1,'0000.0002','["Keyword Parsing Fixture Later Title","Second heading"]');
INSERT INTO outline_path VALUES(23,2,19,1,'0000.0003','["Keyword Parsing Fixture Later Title","Boundary: keyword-looking body text"]');
INSERT INTO outline_path VALUES(24,2,19,1,'0000.0004','["Keyword Parsing Fixture Later Title","Expected behavior"]');
INSERT INTO outline_path VALUES(25,3,NULL,0,'0000','["Title can span multiple lines, even here"]');
INSERT INTO outline_path VALUES(26,3,25,1,'0000.0001','["Title can span multiple lines, even here","Unfortunately Everywhere"]');
INSERT INTO outline_path VALUES(27,4,NULL,0,'0000','["Planning timestamp"]');
INSERT INTO outline_path VALUES(28,4,27,1,'0000.0001','["Planning timestamp","Planning"]');
INSERT INTO outline_path VALUES(29,4,28,2,'0000.0001.0001','["Planning timestamp","Planning","Simple scheduled"]');
INSERT INTO outline_path VALUES(30,4,28,2,'0000.0001.0002','["Planning timestamp","Planning","Simple deadline"]');
INSERT INTO outline_path VALUES(31,4,28,2,'0000.0001.0003','["Planning timestamp","Planning","Simple closed"]');
INSERT INTO outline_path VALUES(32,4,28,2,'0000.0001.0004','["Planning timestamp","Planning","All on one planning line"]');
INSERT INTO outline_path VALUES(33,4,28,2,'0000.0001.0005','["Planning timestamp","Planning","With time"]');
INSERT INTO outline_path VALUES(34,4,28,2,'0000.0001.0006','["Planning timestamp","Planning","Time range same day"]');
INSERT INTO outline_path VALUES(35,4,28,2,'0000.0001.0007','["Planning timestamp","Planning","Date range"]');
INSERT INTO outline_path VALUES(36,4,28,2,'0000.0001.0008','["Planning timestamp","Planning","Repeater"]');
INSERT INTO outline_path VALUES(37,4,28,2,'0000.0001.0009','["Planning timestamp","Planning","Diary expression"]');
INSERT INTO outline_path VALUES(38,4,28,2,'0000.0001.0010','["Planning timestamp","Planning","Multiple same keyword"]');
INSERT INTO outline_path VALUES(39,4,28,2,'0000.0001.0011','["Planning timestamp","Planning","Not valid"]');
INSERT INTO outline_path VALUES(40,4,39,3,'0000.0001.0011.0001','["Planning timestamp","Planning","Not valid","Multiple planning lines"]');
INSERT INTO outline_path VALUES(41,4,39,3,'0000.0001.0011.0002','["Planning timestamp","Planning","Not valid","Planning not immediately after headline"]');
INSERT INTO outline_path VALUES(42,4,39,3,'0000.0001.0011.0003','["Planning timestamp","Planning","Not valid","Looks like planning in body"]');
INSERT INTO outline_path VALUES(43,4,39,3,'0000.0001.0011.0004','["Planning timestamp","Planning","Not valid","Lowercase should probably not count"]');
INSERT INTO outline_path VALUES(44,4,39,3,'0000.0001.0011.0005','["Planning timestamp","Planning","Not valid","Multiple same keyword"]');
INSERT INTO outline_path VALUES(45,5,NULL,0,'0000','["Org Property and Keyword Test"]');
INSERT INTO outline_path VALUES(46,5,45,1,'0000.0001','["Org Property and Keyword Test","Task with multiple drawer properties"]');
INSERT INTO outline_path VALUES(47,5,45,1,'0000.0002','["Org Property and Keyword Test","Task with duplicate drawer properties"]');
INSERT INTO outline_path VALUES(48,5,45,1,'0000.0003','["Org Property and Keyword Test","Task with append operator in drawer"]');
INSERT INTO outline_path VALUES(49,5,45,1,'0000.0004','["Org Property and Keyword Test","Task with mixed-case keys"]');
INSERT INTO outline_path VALUES(50,5,45,1,'0000.0005','["Org Property and Keyword Test","Task with empty property accepted by Orgize"]');
INSERT INTO outline_path VALUES(51,5,45,1,'0000.0006','["Org Property and Keyword Test","Task with Orgize empty-property limitation"]');
INSERT INTO outline_path VALUES(52,5,45,1,'0000.0007','["Org Property and Keyword Test","Task after file-level property keywords"]');
INSERT INTO outline_path VALUES(53,5,45,1,'0000.0008','["Org Property and Keyword Test","Task after later file-level keywords"]');
INSERT INTO outline_path VALUES(54,5,45,1,'0000.0009','["Org Property and Keyword Test","Boundary: property-like but not properties"]');
INSERT INTO outline_path VALUES(55,6,NULL,0,'0000','["Tags and FILETAGS Fixture"]');
INSERT INTO outline_path VALUES(56,6,55,1,'0000.0001','["Tags and FILETAGS Fixture","Parent"]');
INSERT INTO outline_path VALUES(57,6,56,2,'0000.0001.0001','["Tags and FILETAGS Fixture","Parent","Child"]');
INSERT INTO outline_path VALUES(58,6,57,3,'0000.0001.0001.0001','["Tags and FILETAGS Fixture","Parent","Child","Grandchild"]');
INSERT INTO outline_path VALUES(59,6,55,1,'0000.0002','["Tags and FILETAGS Fixture","Sibling"]');
INSERT INTO outline_path VALUES(60,6,55,1,'0000.0003','["Tags and FILETAGS Fixture","Duplicate Local"]');
INSERT INTO outline_path VALUES(61,6,55,1,'0000.0004','["Tags and FILETAGS Fixture","After Later FILETAGS"]');
INSERT INTO outline_path VALUES(62,6,55,1,'0000.0005','["Tags and FILETAGS Fixture","parent with one tag"]');
INSERT INTO outline_path VALUES(63,6,62,2,'0000.0005.0001','["Tags and FILETAGS Fixture","parent with one tag","child inherits parent tag"]');
INSERT INTO outline_path VALUES(64,6,62,2,'0000.0005.0002','["Tags and FILETAGS Fixture","parent with one tag","child with local tag"]');
INSERT INTO outline_path VALUES(65,6,64,3,'0000.0005.0002.0001','["Tags and FILETAGS Fixture","parent with one tag","child with local tag","grandchild inherits both"]');
INSERT INTO outline_path VALUES(66,6,64,3,'0000.0005.0002.0002','["Tags and FILETAGS Fixture","parent with one tag","child with local tag","grandchild with duplicate local tag"]');
INSERT INTO outline_path VALUES(67,6,55,1,'0000.0006','["Tags and FILETAGS Fixture","second parent"]');
INSERT INTO outline_path VALUES(68,6,67,2,'0000.0006.0001','["Tags and FILETAGS Fixture","second parent","second child with local tag"]');
INSERT INTO outline_path VALUES(69,6,68,3,'0000.0006.0001.0001','["Tags and FILETAGS Fixture","second parent","second child with local tag","second grandchild with extra tag"]');
INSERT INTO outline_path VALUES(70,6,55,1,'0000.0007','["Tags and FILETAGS Fixture","untagged parent"]');
INSERT INTO outline_path VALUES(71,6,70,2,'0000.0007.0001','["Tags and FILETAGS Fixture","untagged parent","child with only local tag"]');
INSERT INTO outline_path VALUES(72,6,70,2,'0000.0007.0002','["Tags and FILETAGS Fixture","untagged parent","child without tags"]');
INSERT INTO outline_path VALUES(73,7,NULL,0,'0000','["timestamp-repeaters"]');
INSERT INTO outline_path VALUES(74,7,73,1,'0000.0001','["timestamp-repeaters","Repeater markers"]');
INSERT INTO outline_path VALUES(75,7,74,2,'0000.0001.0001','["timestamp-repeaters","Repeater markers","Cumulate plus"]');
INSERT INTO outline_path VALUES(76,7,74,2,'0000.0001.0002','["timestamp-repeaters","Repeater markers","Catch up plus plus"]');
INSERT INTO outline_path VALUES(77,7,74,2,'0000.0001.0003','["timestamp-repeaters","Repeater markers","Restart dot plus"]');
INSERT INTO outline_path VALUES(78,7,73,1,'0000.0002','["timestamp-repeaters","Repeater units"]');
INSERT INTO outline_path VALUES(79,7,78,2,'0000.0002.0001','["timestamp-repeaters","Repeater units","Repeater hour"]');
INSERT INTO outline_path VALUES(80,7,78,2,'0000.0002.0002','["timestamp-repeaters","Repeater units","Repeater day"]');
INSERT INTO outline_path VALUES(81,7,78,2,'0000.0002.0003','["timestamp-repeaters","Repeater units","Repeater week"]');
INSERT INTO outline_path VALUES(82,7,78,2,'0000.0002.0004','["timestamp-repeaters","Repeater units","Repeater month"]');
INSERT INTO outline_path VALUES(83,7,78,2,'0000.0002.0005','["timestamp-repeaters","Repeater units","Repeater year"]');
INSERT INTO outline_path VALUES(84,7,73,1,'0000.0003','["timestamp-repeaters","Repeater deadline part"]');
INSERT INTO outline_path VALUES(85,7,84,2,'0000.0003.0001','["timestamp-repeaters","Repeater deadline part","Repeater with deadline day"]');
INSERT INTO outline_path VALUES(86,7,84,2,'0000.0003.0002','["timestamp-repeaters","Repeater deadline part","Repeater with deadline week"]');
INSERT INTO outline_path VALUES(87,7,84,2,'0000.0003.0003','["timestamp-repeaters","Repeater deadline part","Repeater with deadline month"]');
INSERT INTO outline_path VALUES(88,7,73,1,'0000.0004','["timestamp-repeaters","Warning delays"]');
INSERT INTO outline_path VALUES(89,7,88,2,'0000.0004.0001','["timestamp-repeaters","Warning delays","Warning all"]');
INSERT INTO outline_path VALUES(90,7,88,2,'0000.0004.0002','["timestamp-repeaters","Warning delays","Warning first"]');
INSERT INTO outline_path VALUES(91,7,88,2,'0000.0004.0003','["timestamp-repeaters","Warning delays","Warning hour"]');
INSERT INTO outline_path VALUES(92,7,88,2,'0000.0004.0004','["timestamp-repeaters","Warning delays","Warning month"]');
INSERT INTO outline_path VALUES(93,7,88,2,'0000.0004.0005','["timestamp-repeaters","Warning delays","Warning year"]');
INSERT INTO outline_path VALUES(94,7,73,1,'0000.0005','["timestamp-repeaters","Repeater and warning combinations"]');
INSERT INTO outline_path VALUES(95,7,94,2,'0000.0005.0001','["timestamp-repeaters","Repeater and warning combinations","Repeater and warning"]');
INSERT INTO outline_path VALUES(96,7,94,2,'0000.0005.0002','["timestamp-repeaters","Repeater and warning combinations","Catch up repeater with deadline part and warning"]');
INSERT INTO outline_path VALUES(97,7,94,2,'0000.0005.0003','["timestamp-repeaters","Repeater and warning combinations","Restart repeater with first warning"]');
INSERT INTO outline_path VALUES(98,7,73,1,'0000.0006','["timestamp-repeaters","Time and range combinations"]');
INSERT INTO outline_path VALUES(99,7,98,2,'0000.0006.0001','["timestamp-repeaters","Time and range combinations","Time with repeater"]');
INSERT INTO outline_path VALUES(100,7,98,2,'0000.0006.0002','["timestamp-repeaters","Time and range combinations","Time range with repeater"]');
INSERT INTO outline_path VALUES(101,7,98,2,'0000.0006.0003','["timestamp-repeaters","Time and range combinations","Date range with repeater"]');
INSERT INTO outline_path VALUES(102,7,73,1,'0000.0007','["timestamp-repeaters","Inactive timestamp with repeater"]');
INSERT INTO outline_path VALUES(103,7,102,2,'0000.0007.0001','["timestamp-repeaters","Inactive timestamp with repeater","Inactive repeater"]');
INSERT INTO outline_path VALUES(104,7,73,1,'0000.0008','["timestamp-repeaters","Diary negative case"]');
INSERT INTO outline_path VALUES(105,7,104,2,'0000.0008.0001','["timestamp-repeaters","Diary negative case","Diary with apparent repeater text"]');
INSERT INTO outline_path VALUES(106,8,NULL,0,'0000','["Timestamps"]');
INSERT INTO outline_path VALUES(107,8,106,1,'0000.0001','["Timestamps","Meet Peter at the movies"]');
INSERT INTO outline_path VALUES(108,8,106,1,'0000.0002','["Timestamps","Discussion on climate change"]');
INSERT INTO outline_path VALUES(109,8,106,1,'0000.0003','["Timestamps","My days off"]');
INSERT INTO outline_path VALUES(110,8,106,1,'0000.0004','["Timestamps","Can be anywhere"]');
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
INSERT INTO timestamps VALUES(1,29,'scheduled',1732060800,NULL,'active','none','<2024-11-20 Wed>',93,109,7);
INSERT INTO timestamps VALUES(2,30,'deadline',1733011200,NULL,'active','none','<2024-12-01 Sun>',140,156,10);
INSERT INTO timestamps VALUES(3,31,'closed',1732147200,NULL,'inactive','none','[2024-11-21 Thu]',183,199,13);
INSERT INTO timestamps VALUES(4,32,'deadline',1733011200,NULL,'active','none','<2024-12-01 Sun>',239,255,16);
INSERT INTO timestamps VALUES(5,32,'scheduled',1732060800,NULL,'active','none','<2024-11-20 Wed>',267,283,16);
INSERT INTO timestamps VALUES(6,32,'closed',1732147200,NULL,'inactive','none','[2024-11-21 Thu]',292,308,16);
INSERT INTO timestamps VALUES(7,33,'scheduled',1732095000,NULL,'active','none','<2024-11-20 Wed 09:30>',334,356,19);
INSERT INTO timestamps VALUES(8,34,'scheduled',1732095000,1732100400,'active','time_range','<2024-11-20 Wed 09:30-11:00>',392,420,22);
INSERT INTO timestamps VALUES(9,35,'deadline',1733011200,1733184000,'active','date_range','<2024-12-01 Sun>--<2024-12-03 Tue>',446,480,25);
INSERT INTO timestamps VALUES(10,36,'scheduled',1732060800,NULL,'active','none','<2024-11-20 Wed +1w>',505,525,28);
INSERT INTO timestamps VALUES(11,37,'body',NULL,NULL,'diary','none','<%%(diary-float t 42)>',558,580,31);
INSERT INTO timestamps VALUES(12,38,'scheduled',1732060800,NULL,'active','none','<2024-11-20 Wed>',618,634,34);
INSERT INTO timestamps VALUES(13,38,'scheduled',1732147200,NULL,'active','none','<2024-11-21 Thu>',646,662,34);
INSERT INTO timestamps VALUES(14,40,'scheduled',1732060800,NULL,'active','none','<2024-11-20 Wed>',757,773,40);
INSERT INTO timestamps VALUES(15,40,'body',1733011200,NULL,'active','none','<2024-12-01 Sun>',784,800,41);
INSERT INTO timestamps VALUES(16,40,'body',1732147200,NULL,'inactive','none','[2024-11-21 Thu]',809,825,42);
INSERT INTO timestamps VALUES(17,41,'body',1732060800,NULL,'active','none','<2024-11-20 Wed>',904,920,46);
INSERT INTO timestamps VALUES(18,42,'body',1733011200,NULL,'active','none','<2024-12-01 Sun>',978,994,49);
INSERT INTO timestamps VALUES(19,43,'body',1732060800,NULL,'active','none','<2024-11-20 Wed>',1060,1076,52);
INSERT INTO timestamps VALUES(20,44,'scheduled',1732060800,NULL,'active','none','<2024-11-20 Wed>',1115,1131,55);
INSERT INTO timestamps VALUES(21,44,'scheduled',1732147200,NULL,'active','none','<2024-11-21 Thu>',1143,1159,55);
INSERT INTO timestamps VALUES(22,75,'scheduled',1732060800,NULL,'active','none','<2024-11-20 Wed +1w>',48,68,4);
INSERT INTO timestamps VALUES(23,76,'scheduled',1732060800,NULL,'active','none','<2024-11-20 Wed ++1m>',103,124,7);
INSERT INTO timestamps VALUES(24,77,'scheduled',1732060800,NULL,'active','none','<2024-11-20 Wed .+2d>',157,178,10);
INSERT INTO timestamps VALUES(25,79,'scheduled',1732060800,NULL,'active','none','<2024-11-20 Wed +3h>',226,246,15);
INSERT INTO timestamps VALUES(26,80,'scheduled',1732060800,NULL,'active','none','<2024-11-20 Wed +3d>',275,295,18);
INSERT INTO timestamps VALUES(27,81,'scheduled',1732060800,NULL,'active','none','<2024-11-20 Wed +3w>',325,345,21);
INSERT INTO timestamps VALUES(28,82,'scheduled',1732060800,NULL,'active','none','<2024-11-20 Wed +3m>',376,396,24);
INSERT INTO timestamps VALUES(29,83,'scheduled',1732060800,NULL,'active','none','<2024-11-20 Wed +3y>',426,446,27);
INSERT INTO timestamps VALUES(30,85,'scheduled',1732060800,NULL,'active','none','<2024-11-20 Wed +1w/2d>',515,538,32);
INSERT INTO timestamps VALUES(31,86,'scheduled',1732060800,NULL,'active','none','<2024-11-20 Wed ++1m/1w>',582,606,35);
INSERT INTO timestamps VALUES(32,87,'scheduled',1732060800,NULL,'active','none','<2024-11-20 Wed .+1y/2m>',651,675,38);
INSERT INTO timestamps VALUES(33,89,'deadline',1733011200,NULL,'active','none','<2024-12-01 Sun -5d>',720,740,43);
INSERT INTO timestamps VALUES(34,90,'deadline',1733011200,NULL,'active','none','<2024-12-01 Sun --2w>',769,790,46);
INSERT INTO timestamps VALUES(35,91,'deadline',1733045400,NULL,'active','none','<2024-12-01 Sun 09:30 -3h>',818,844,49);
INSERT INTO timestamps VALUES(36,92,'deadline',1733011200,NULL,'active','none','<2024-12-01 Sun -1m>',873,893,52);
INSERT INTO timestamps VALUES(37,93,'deadline',1733011200,NULL,'active','none','<2024-12-01 Sun --1y>',921,942,55);
INSERT INTO timestamps VALUES(38,95,'deadline',1733011200,NULL,'active','none','<2024-12-01 Sun +1w -5d>',1015,1039,60);
INSERT INTO timestamps VALUES(39,96,'deadline',1733011200,NULL,'active','none','<2024-12-01 Sun ++1m/2d -5d>',1103,1131,63);
INSERT INTO timestamps VALUES(40,97,'deadline',1733011200,NULL,'active','none','<2024-12-01 Sun .+2w --1w>',1182,1208,66);
INSERT INTO timestamps VALUES(41,99,'scheduled',1732095000,NULL,'active','none','<2024-11-20 Wed 09:30 +1w>',1274,1300,71);
INSERT INTO timestamps VALUES(42,100,'scheduled',1732095000,1732100400,'active','time_range','<2024-11-20 Wed 09:30-11:00 +1w>',1341,1373,74);
INSERT INTO timestamps VALUES(43,101,'deadline',1733011200,1733184000,'active','date_range','<2024-12-01 Sun>--<2024-12-03 Tue +1w>',1413,1451,77);
INSERT INTO timestamps VALUES(44,103,'body',1732060800,NULL,'inactive','none','[2024-11-20 Wed +1w]',1510,1530,82);
INSERT INTO timestamps VALUES(45,105,'body',NULL,NULL,'diary','none','<%%(diary-float t 42)>',1603,1625,87);
INSERT INTO timestamps VALUES(46,107,'body',1162408500,NULL,'active','none','<2006-11-01 Wed 19:15>',67,89,5);
INSERT INTO timestamps VALUES(47,108,'body',1162461600,1162468800,'active','time_range','<2006-11-02 Thu 10:00-12:00>',122,150,8);
INSERT INTO timestamps VALUES(48,109,'body',1162512000,NULL,'active','none','<2006-11-03 Fri>',166,182,11);
INSERT INTO timestamps VALUES(49,109,'body',1162771200,NULL,'active','none','<2006-11-06 Mon>',183,199,12);
INSERT INTO timestamps VALUES(50,110,'body',1162512000,NULL,'active','none','<2006-11-03 Fri>',237,253,16);
INSERT INTO timestamps VALUES(51,110,'body',1782086400,NULL,'inactive','none','[2026-06-22 Mon]',281,297,18);
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
INSERT INTO timestamp_repeaters VALUES(1,10,'cumulate',1,'week',NULL,NULL,NULL,NULL,NULL);
INSERT INTO timestamp_repeaters VALUES(2,22,'cumulate',1,'week',NULL,NULL,NULL,NULL,NULL);
INSERT INTO timestamp_repeaters VALUES(3,23,'catch_up',1,'month',NULL,NULL,NULL,NULL,NULL);
INSERT INTO timestamp_repeaters VALUES(4,24,'restart',2,'day',NULL,NULL,NULL,NULL,NULL);
INSERT INTO timestamp_repeaters VALUES(5,25,'cumulate',3,'hour',NULL,NULL,NULL,NULL,NULL);
INSERT INTO timestamp_repeaters VALUES(6,26,'cumulate',3,'day',NULL,NULL,NULL,NULL,NULL);
INSERT INTO timestamp_repeaters VALUES(7,27,'cumulate',3,'week',NULL,NULL,NULL,NULL,NULL);
INSERT INTO timestamp_repeaters VALUES(8,28,'cumulate',3,'month',NULL,NULL,NULL,NULL,NULL);
INSERT INTO timestamp_repeaters VALUES(9,29,'cumulate',3,'year',NULL,NULL,NULL,NULL,NULL);
INSERT INTO timestamp_repeaters VALUES(10,30,'cumulate',1,'week',2,'day',NULL,NULL,NULL);
INSERT INTO timestamp_repeaters VALUES(11,31,'catch_up',1,'month',1,'week',NULL,NULL,NULL);
INSERT INTO timestamp_repeaters VALUES(12,32,'restart',1,'year',2,'month',NULL,NULL,NULL);
INSERT INTO timestamp_repeaters VALUES(13,33,NULL,NULL,NULL,NULL,NULL,'all',5,'day');
INSERT INTO timestamp_repeaters VALUES(14,34,NULL,NULL,NULL,NULL,NULL,'first',2,'week');
INSERT INTO timestamp_repeaters VALUES(15,35,NULL,NULL,NULL,NULL,NULL,'all',3,'hour');
INSERT INTO timestamp_repeaters VALUES(16,36,NULL,NULL,NULL,NULL,NULL,'all',1,'month');
INSERT INTO timestamp_repeaters VALUES(17,37,NULL,NULL,NULL,NULL,NULL,'first',1,'year');
INSERT INTO timestamp_repeaters VALUES(18,38,'cumulate',1,'week',NULL,NULL,'all',5,'day');
INSERT INTO timestamp_repeaters VALUES(19,39,'catch_up',1,'month',2,'day','all',5,'day');
INSERT INTO timestamp_repeaters VALUES(20,40,'restart',2,'week',NULL,NULL,'first',1,'week');
INSERT INTO timestamp_repeaters VALUES(21,41,'cumulate',1,'week',NULL,NULL,NULL,NULL,NULL);
INSERT INTO timestamp_repeaters VALUES(22,42,'cumulate',1,'week',NULL,NULL,NULL,NULL,NULL);
INSERT INTO timestamp_repeaters VALUES(23,43,'cumulate',1,'week',NULL,NULL,NULL,NULL,NULL);
INSERT INTO timestamp_repeaters VALUES(24,44,'cumulate',1,'week',NULL,NULL,NULL,NULL,NULL);
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
INSERT INTO properties VALUES(1,19,'BEFORE_PROP','before-value','property_keyword',0,4);
INSERT INTO properties VALUES(2,19,'CATEGORY','before-category','category_keyword',0,5);
INSERT INTO properties VALUES(3,19,'AFTER_PROP','after-value','property_keyword',0,12);
INSERT INTO properties VALUES(4,19,'REPEATED_PROP','first','property_keyword',0,13);
INSERT INTO properties VALUES(5,19,'REPEATED_PROP','second','property_keyword',0,14);
INSERT INTO properties VALUES(6,19,'APPENDED_PROP','base','property_keyword',0,15);
INSERT INTO properties VALUES(7,19,'APPENDED_PROP','extra','property_keyword',1,16);
INSERT INTO properties VALUES(8,19,'CATEGORY','after-category','category_keyword',0,17);
INSERT INTO properties VALUES(9,45,'CATEGORY','Level 0 Category Property','property_drawer',0,2);
INSERT INTO properties VALUES(10,45,'WHATEVER','level 0 drawer property','property_drawer',0,3);
INSERT INTO properties VALUES(11,45,'ID','7dad9b62-a3cc-43ec-a60f-e650bdaeae6d','property_drawer',0,4);
INSERT INTO properties VALUES(12,45,'CATEGORY','category_keyword_value','category_keyword',0,8);
INSERT INTO properties VALUES(13,45,'EFFORT_ALL','0:10 0:30 1:00','property_keyword',0,9);
INSERT INTO properties VALUES(14,45,'KEYWORD_PROPERTY','valid','property_keyword',0,10);
INSERT INTO properties VALUES(15,45,'KEYWORD_OVERWRITTEN_BY_SECOND','invalid','property_keyword',0,11);
INSERT INTO properties VALUES(16,45,'KEYWORD_OVERWRITTEN_BY_SECOND','valid','property_keyword',0,12);
INSERT INTO properties VALUES(17,45,'KEYWORD_APPEND','foo=1','property_keyword',0,13);
INSERT INTO properties VALUES(18,45,'KEYWORD_APPEND','bar=2','property_keyword',1,14);
INSERT INTO properties VALUES(19,45,'LATER_KEYWORD_PROPERTY','works_everywhere','property_keyword',0,80);
INSERT INTO properties VALUES(20,45,'CATEGORY','later_category_keyword','category_keyword',0,81);
INSERT INTO properties VALUES(21,46,'ID','abc','property_drawer',0,18);
INSERT INTO properties VALUES(22,46,'CUSTOM_ID','task-custom-id','property_drawer',0,19);
INSERT INTO properties VALUES(23,46,'EFFORT','0:30','property_drawer',0,20);
INSERT INTO properties VALUES(24,46,'OWNER','Alice','property_drawer',0,21);
INSERT INTO properties VALUES(25,46,'DRAWER_PROP','valid','property_drawer',0,22);
INSERT INTO properties VALUES(26,47,'DEFINED_TWICE','invalid','property_drawer',0,28);
INSERT INTO properties VALUES(27,47,'DEFINED_TWICE','valid','property_drawer',0,29);
INSERT INTO properties VALUES(28,48,'ADD-VALUE','is','property_drawer',0,37);
INSERT INTO properties VALUES(29,48,'ADD-VALUE','valid','property_drawer',1,38);
INSERT INTO properties VALUES(30,49,'ID','lowercase-id','property_drawer',0,47);
INSERT INTO properties VALUES(31,49,'CUSTOM_ID','mixed-case-custom-id','property_drawer',0,48);
INSERT INTO properties VALUES(32,49,'DRAWER_PROP','valid','property_drawer',0,49);
INSERT INTO properties VALUES(33,49,'ADD-VALUE','appended','property_drawer',1,50);
INSERT INTO properties VALUES(34,50,'EMPTY','','property_drawer',0,60);
CREATE TABLE tags (
    heading_id      INTEGER NOT NULL,
    tag             TEXT NOT NULL,
    FOREIGN KEY (heading_id)
        REFERENCES headings(id)
        ON DELETE CASCADE,
    PRIMARY KEY (heading_id, tag)
);
INSERT INTO tags VALUES(45,'project');
INSERT INTO tags VALUES(45,'work');
INSERT INTO tags VALUES(55,'file');
INSERT INTO tags VALUES(55,'project');
INSERT INTO tags VALUES(55,'later');
INSERT INTO tags VALUES(55,'extra');
INSERT INTO tags VALUES(56,'parent');
INSERT INTO tags VALUES(57,'child');
INSERT INTO tags VALUES(58,'project');
INSERT INTO tags VALUES(58,'grandchild');
INSERT INTO tags VALUES(60,'file');
INSERT INTO tags VALUES(60,'sibling');
INSERT INTO tags VALUES(61,'after');
INSERT INTO tags VALUES(62,'parent');
INSERT INTO tags VALUES(64,'child');
INSERT INTO tags VALUES(66,'parent');
INSERT INTO tags VALUES(67,'second');
INSERT INTO tags VALUES(68,'child');
INSERT INTO tags VALUES(69,'extra');
INSERT INTO tags VALUES(71,'local');
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
CREATE INDEX idx_timestamps_heading_id
    ON timestamps(heading_id);
CREATE INDEX idx_timestamps_role_start
    ON timestamps(role, start_ts);
CREATE INDEX idx_timestamps_start
    ON timestamps(start_ts);
CREATE INDEX idx_timestamp_repeaters_timestamp_id
    ON timestamp_repeaters(timestamp_id);
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
COMMIT;
