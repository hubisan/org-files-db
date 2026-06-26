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
INSERT INTO files VALUES(1,'/home/hubisan/projects/coding/org-files-db/.project/manual-tests/files/dir-locals-test-structure/notes/child/deepest-wins-test.org',1782504536000000000,286,NULL,1782515157);
INSERT INTO files VALUES(2,'/home/hubisan/projects/coding/org-files-db/.project/manual-tests/files/dir-locals-test-structure/notes/irrelevant-unsupported/eval-next-to-valid-test.org',1782515075226690860,250,NULL,1782515157);
INSERT INTO files VALUES(3,'/home/hubisan/projects/coding/org-files-db/.project/manual-tests/files/dir-locals-test-structure/notes/override/org-override-test.org',1782504536000000000,374,NULL,1782515157);
INSERT INTO files VALUES(4,'/home/hubisan/projects/coding/org-files-db/.project/manual-tests/files/dir-locals-test-structure/notes/root-test.org',1782504536000000000,277,NULL,1782515157);
INSERT INTO files VALUES(5,'/home/hubisan/projects/coding/org-files-db/.project/manual-tests/files/dir-locals-test-structure/notes/unsafe/unsafe-warn-test.org',1782504536000000000,316,NULL,1782515157);
INSERT INTO files VALUES(6,'/home/hubisan/projects/coding/org-files-db/.project/manual-tests/files/file-local-todo-keywords.org',1781808483955605322,1188,NULL,1782515157);
INSERT INTO files VALUES(7,'/home/hubisan/projects/coding/org-files-db/.project/manual-tests/files/keywords.org',1782237960833303823,2276,NULL,1782515157);
INSERT INTO files VALUES(8,'/home/hubisan/projects/coding/org-files-db/.project/manual-tests/files/multipe-title-keywords.org',1781809982908544855,321,NULL,1782515157);
INSERT INTO files VALUES(9,'/home/hubisan/projects/coding/org-files-db/.project/manual-tests/files/planning-lines.org',1782129087807207205,1163,NULL,1782515157);
INSERT INTO files VALUES(10,'/home/hubisan/projects/coding/org-files-db/.project/manual-tests/files/properties.org',1782218329407221851,2783,NULL,1782515157);
INSERT INTO files VALUES(11,'/home/hubisan/projects/coding/org-files-db/.project/manual-tests/files/tags.org',1782243198853649058,1307,NULL,1782515157);
INSERT INTO files VALUES(12,'/home/hubisan/projects/coding/org-files-db/.project/manual-tests/files/timestamp-repeaters.org',1782159459016426918,1626,NULL,1782515157);
INSERT INTO files VALUES(13,'/home/hubisan/projects/coding/org-files-db/.project/manual-tests/files/timestamps.org',1782153027780066037,299,NULL,1782515157);
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
INSERT INTO headings VALUES(1,1,NULL,0,1,-1,286,'Deepest .dir-locals wins test','Deepest .dir-locals wins test',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(2,1,1,1,3,40,94,'Should be open from notes/child/.dir-locals.el','Should be open from notes/child/.dir-locals.el','NEXT','open',NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(3,1,1,1,4,94,154,'Should be closed from notes/child/.dir-locals.el','Should be closed from notes/child/.dir-locals.el','FINISHED','closed',NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(4,1,1,1,5,154,220,'PLAN Should NOT be recognized because child .dir-locals.el wins','PLAN Should NOT be recognized because child .dir-locals.el wins',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(5,1,1,1,6,220,286,'DONE Should NOT be recognized because child .dir-locals.el wins','DONE Should NOT be recognized because child .dir-locals.el wins',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(6,2,NULL,0,1,-1,250,'Eval next to valid org-todo-keywords test','Eval next to valid org-todo-keywords test',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(7,2,6,1,3,52,116,'Should be open from valid org-todo-keywords despite eval','Should be open from valid org-todo-keywords despite eval','PLAN','open',NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(8,2,6,1,4,116,182,'Should be closed from valid org-todo-keywords despite eval','Should be closed from valid org-todo-keywords despite eval','DONE','closed',NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(9,2,6,1,5,182,250,'TODO Should NOT be recognized because dir-locals TODOs are active','TODO Should NOT be recognized because dir-locals TODOs are active',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(10,3,NULL,0,1,-1,374,'Org in-buffer TODO override test','Org in-buffer TODO override test',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(11,3,10,1,4,84,130,'Should be open from in-buffer #+TODO','Should be open from in-buffer #+TODO','REVIEW','open',NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(12,3,10,1,5,130,182,'Should also be open from in-buffer #+TODO','Should also be open from in-buffer #+TODO','BLOCKED','open',NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(13,3,10,1,6,182,230,'Should be closed from in-buffer #+TODO','Should be closed from in-buffer #+TODO','CLOSED','closed',NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(14,3,10,1,7,230,302,'PLAN Should NOT be recognized because #+TODO overrides .dir-locals.el','PLAN Should NOT be recognized because #+TODO overrides .dir-locals.el',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(15,3,10,1,8,302,374,'DONE Should NOT be recognized because #+TODO overrides .dir-locals.el','DONE Should NOT be recognized because #+TODO overrides .dir-locals.el',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(16,4,NULL,0,1,-1,277,'Root dir-locals test','Root dir-locals test',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(17,4,16,1,3,31,73,'Should be open from .dir-locals.el','Should be open from .dir-locals.el','PLAN','open',NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(18,4,16,1,4,73,120,'Should also be open from .dir-locals.el','Should also be open from .dir-locals.el','WAIT','open',NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(19,4,16,1,5,120,164,'Should be closed from .dir-locals.el','Should be closed from .dir-locals.el','DONE','closed',NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(20,4,16,1,6,164,215,'Should also be closed from .dir-locals.el','Should also be closed from .dir-locals.el','CANCEL','closed',NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(21,4,16,1,7,215,277,'TODO Should NOT be recognized when .dir-locals.el is active','TODO Should NOT be recognized when .dir-locals.el is active',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(22,5,NULL,0,1,-1,316,'Unsafe syntax warning test','Unsafe syntax warning test',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(23,5,22,1,3,37,131,'Should be open from config fallback when unsafe .dir-locals.el is ignored with warning','Should be open from config fallback when unsafe .dir-locals.el is ignored with warning','TODO','open',NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(24,5,22,1,4,131,227,'Should be closed from config fallback when unsafe .dir-locals.el is ignored with warning','Should be closed from config fallback when unsafe .dir-locals.el is ignored with warning','DONE','closed',NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(25,5,22,1,5,227,316,'PLAN Should NOT be recognized in this folder because unsafe .dir-locals.el is unusable','PLAN Should NOT be recognized in this folder because unsafe .dir-locals.el is unusable',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(26,6,NULL,0,1,-1,1188,'File-local TODO keywords','File-local TODO keywords',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(27,6,26,1,10,234,354,'TODO default keyword should stay in title','TODO default keyword should stay in title',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(28,6,26,1,13,354,479,'DONE default done keyword should stay in title','DONE default done keyword should stay in title',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(29,6,26,1,16,479,512,'open keyword with fast key','open keyword with fast key','one','open',NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(30,6,26,1,17,512,553,'another open keyword with fast key','another open keyword with fast key','two','open',NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(31,6,26,1,18,553,590,'closed keyword with fast key','closed keyword with fast key','three','closed',NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(32,6,26,1,19,590,636,'closed keyword with extended fast key','closed keyword with extended fast key','four','closed',NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(33,6,26,1,21,636,682,'open keyword from empty-done-side line','open keyword from empty-done-side line','FIVE','open',NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(34,6,26,1,22,682,736,'another open keyword from empty-done-side line','another open keyword from empty-done-side line','SIX','open',NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(35,6,26,1,24,736,771,'open keyword from TYP_TODO','open keyword from TYP_TODO','seven','open',NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(36,6,26,1,25,771,809,'closed keyword from TYP_TODO','closed keyword from TYP_TODO','eight','closed',NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(37,6,26,1,27,809,843,'open keyword from SEQ_TODO','open keyword from SEQ_TODO','nine','open',NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(38,6,26,1,28,843,900,'closed keyword from SEQ_TODO','closed keyword from SEQ_TODO','ten','closed',NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(39,6,26,1,32,900,977,'closed keyword from later TODO line','closed keyword from later TODO line','eleven','closed',NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(40,6,26,1,36,977,1034,'open keyword from line defined later in file','open keyword from line defined later in file','late_open','open',NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(41,6,26,1,37,1034,1094,'closed keyword from line defined later in file','closed keyword from line defined later in file','late_done','closed',NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(42,6,26,1,39,1094,1141,'TODO still not valid after later local lines','TODO still not valid after later local lines',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(43,6,26,1,40,1141,1188,'DONE still not valid after later local lines','DONE still not valid after later local lines',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(44,7,NULL,0,1,-1,2276,'Keyword Parsing Fixture Later Title','Keyword Parsing Fixture Later Title',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(45,7,44,1,7,141,604,'First heading','First heading','WURST','open',NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(46,7,45,2,19,454,604,'Child heading','Child heading',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(47,7,44,1,26,604,798,'Second heading','Second heading','IDEA','open',NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(48,7,44,1,33,798,1264,'Boundary: keyword-looking body text','Boundary: keyword-looking body text',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(49,7,44,1,49,1264,2276,'Expected behavior','Expected behavior',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(50,8,NULL,0,1,-1,321,'Title can span multiple lines, even here','Title can span multiple lines, even here',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(51,8,50,1,7,136,321,'Unfortunately Everywhere','Unfortunately Everywhere',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(52,9,NULL,0,1,-1,1163,'Planning timestamp','Planning timestamp',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(53,9,52,1,4,49,1163,'Planning','Planning',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(54,9,53,2,6,62,111,'Simple scheduled','Simple scheduled',NULL,NULL,NULL,'<2024-11-20 Wed>',1732060800,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(55,9,53,2,9,111,158,'Simple deadline','Simple deadline',NULL,NULL,NULL,NULL,NULL,'<2024-12-01 Sun>',1733011200,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(56,9,53,2,12,158,201,'Simple closed','Simple closed',NULL,NULL,NULL,NULL,NULL,NULL,NULL,'[2024-11-21 Thu]',1732147200,0,0,'[]');
INSERT INTO headings VALUES(57,9,53,2,15,201,310,'All on one planning line','All on one planning line',NULL,NULL,NULL,'<2024-11-20 Wed>',1732060800,'<2024-12-01 Sun>',1733011200,'[2024-11-21 Thu]',1732147200,0,0,'[]');
INSERT INTO headings VALUES(58,9,53,2,18,310,358,'With time','With time',NULL,NULL,NULL,'<2024-11-20 Wed 09:30>',1732095000,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(59,9,53,2,21,358,422,'Time range same day','Time range same day',NULL,NULL,NULL,'<2024-11-20 Wed 09:30-11:00>',1732095000,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(60,9,53,2,24,422,482,'Date range','Date range',NULL,NULL,NULL,NULL,NULL,'<2024-12-01 Sun>--<2024-12-03 Tue>',1733011200,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(61,9,53,2,27,482,527,'Repeater','Repeater',NULL,NULL,NULL,'<2024-11-20 Wed +1w>',1732060800,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(62,9,53,2,30,527,582,'Diary expression','Diary expression',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(63,9,53,2,33,582,704,'Multiple same keyword','Multiple same keyword',NULL,NULL,NULL,'<2024-11-21 Thu>',1732147200,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(64,9,53,2,37,704,1163,'Not valid','Not valid',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(65,9,64,3,39,718,827,'Multiple planning lines','Multiple planning lines',NULL,NULL,NULL,'<2024-11-20 Wed>',1732060800,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(66,9,64,3,44,827,922,'Planning not immediately after headline','Planning not immediately after headline',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(67,9,64,3,48,922,1009,'Looks like planning in body','Looks like planning in body',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(68,9,64,3,51,1009,1078,'Lowercase should probably not count','Lowercase should probably not count',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(69,9,64,3,54,1078,1163,'Multiple same keyword','Multiple same keyword',NULL,NULL,NULL,'<2024-11-21 Thu>',1732147200,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(70,10,NULL,0,1,-1,2783,'Org Property and Keyword Test','Org Property and Keyword Test',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'["project","work"]');
INSERT INTO headings VALUES(71,10,70,1,16,465,638,'Task with multiple drawer properties','Task with multiple drawer properties',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'["project","work"]');
INSERT INTO headings VALUES(72,10,70,1,26,638,858,'Task with duplicate drawer properties','Task with duplicate drawer properties',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'["project","work"]');
INSERT INTO headings VALUES(73,10,70,1,35,858,1094,'Task with append operator in drawer','Task with append operator in drawer',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'["project","work"]');
INSERT INTO headings VALUES(74,10,70,1,45,1094,1304,'Task with mixed-case keys','Task with mixed-case keys',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'["project","work"]');
INSERT INTO headings VALUES(75,10,70,1,58,1304,1437,'Task with empty property accepted by Orgize','Task with empty property accepted by Orgize',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'["project","work"]');
INSERT INTO headings VALUES(76,10,70,1,67,1437,1718,'Task with Orgize empty-property limitation','Task with Orgize empty-property limitation',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'["project","work"]');
INSERT INTO headings VALUES(77,10,70,1,76,1718,1984,'Task after file-level property keywords','Task after file-level property keywords',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'["project","work"]');
INSERT INTO headings VALUES(78,10,70,1,83,1984,2264,'Task after later file-level keywords','Task after later file-level keywords',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'["project","work"]');
INSERT INTO headings VALUES(79,10,70,1,89,2264,2783,'Boundary: property-like but not properties','Boundary: property-like but not properties',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'["project","work"]');
INSERT INTO headings VALUES(80,11,NULL,0,1,-1,1307,'Tags and FILETAGS Fixture','Tags and FILETAGS Fixture',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'["file","project","later","extra"]');
INSERT INTO headings VALUES(81,11,80,1,5,82,242,'Parent','Parent',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'["file","project","later","extra","parent"]');
INSERT INTO headings VALUES(82,11,81,2,8,114,242,'Child','Child',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'["file","project","later","extra","parent","child"]');
INSERT INTO headings VALUES(83,11,82,3,11,144,242,'Grandchild','Grandchild',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'["file","project","later","extra","parent","child","grandchild"]');
INSERT INTO headings VALUES(84,11,80,1,14,242,267,'Sibling','Sibling',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'["file","project","later","extra"]');
INSERT INTO headings VALUES(85,11,80,1,17,267,370,'Duplicate Local','Duplicate Local',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'["file","project","later","extra","sibling"]');
INSERT INTO headings VALUES(86,11,80,1,22,370,455,'After Later FILETAGS','After Later FILETAGS',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'["file","project","later","extra","after"]');
INSERT INTO headings VALUES(87,11,80,1,25,455,894,'parent with one tag','parent with one tag',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'["file","project","later","extra","parent"]');
INSERT INTO headings VALUES(88,11,87,2,28,500,576,'child inherits parent tag','child inherits parent tag',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'["file","project","later","extra","parent"]');
INSERT INTO headings VALUES(89,11,87,2,31,576,894,'child with local tag','child with local tag',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'["file","project","later","extra","parent","child"]');
INSERT INTO headings VALUES(90,11,89,3,34,664,754,'grandchild inherits both','grandchild inherits both',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'["file","project","later","extra","parent","child"]');
INSERT INTO headings VALUES(91,11,89,3,37,754,894,'grandchild with duplicate local tag','grandchild with duplicate local tag',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'["file","project","later","extra","parent","child"]');
INSERT INTO headings VALUES(92,11,80,1,41,894,1163,'second parent','second parent',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'["file","project","later","extra","second"]');
INSERT INTO headings VALUES(93,11,92,2,44,976,1163,'second child with local tag','second child with local tag',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'["file","project","later","extra","second","child"]');
INSERT INTO headings VALUES(94,11,93,3,47,1062,1163,'second grandchild with extra tag','second grandchild with extra tag',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'["file","project","later","extra","second","child"]');
INSERT INTO headings VALUES(95,11,80,1,50,1163,1307,'untagged parent','untagged parent',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'["file","project","later","extra"]');
INSERT INTO headings VALUES(96,11,95,2,52,1182,1256,'child with only local tag','child with only local tag',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'["file","project","later","extra","local"]');
INSERT INTO headings VALUES(97,11,95,2,55,1256,1307,'child without tags','child without tags',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'["file","project","later","extra"]');
INSERT INTO headings VALUES(98,12,NULL,0,1,-1,1626,'timestamp-repeaters','timestamp-repeaters',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(99,12,98,1,1,0,180,'Repeater markers','Repeater markers',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(100,12,99,2,3,20,70,'Cumulate plus','Cumulate plus',NULL,NULL,NULL,'<2024-11-20 Wed +1w>',1732060800,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(101,12,99,2,6,70,126,'Catch up plus plus','Catch up plus plus',NULL,NULL,NULL,'<2024-11-20 Wed ++1m>',1732060800,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(102,12,99,2,9,126,180,'Restart dot plus','Restart dot plus',NULL,NULL,NULL,'<2024-11-20 Wed .+2d>',1732060800,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(103,12,98,1,12,180,448,'Repeater units','Repeater units',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(104,12,103,2,14,198,248,'Repeater hour','Repeater hour',NULL,NULL,NULL,'<2024-11-20 Wed +3h>',1732060800,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(105,12,103,2,17,248,297,'Repeater day','Repeater day',NULL,NULL,NULL,'<2024-11-20 Wed +3d>',1732060800,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(106,12,103,2,20,297,347,'Repeater week','Repeater week',NULL,NULL,NULL,'<2024-11-20 Wed +3w>',1732060800,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(107,12,103,2,23,347,398,'Repeater month','Repeater month',NULL,NULL,NULL,'<2024-11-20 Wed +3m>',1732060800,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(108,12,103,2,26,398,448,'Repeater year','Repeater year',NULL,NULL,NULL,'<2024-11-20 Wed +3y>',1732060800,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(109,12,98,1,29,448,677,'Repeater deadline part','Repeater deadline part',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(110,12,109,2,31,474,540,'Repeater with deadline day','Repeater with deadline day',NULL,NULL,NULL,'<2024-11-20 Wed +1w/2d>',1732060800,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(111,12,109,2,34,540,608,'Repeater with deadline week','Repeater with deadline week',NULL,NULL,NULL,'<2024-11-20 Wed ++1m/1w>',1732060800,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(112,12,109,2,37,608,677,'Repeater with deadline month','Repeater with deadline month',NULL,NULL,NULL,'<2024-11-20 Wed .+1y/2m>',1732060800,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(113,12,98,1,40,677,944,'Warning delays','Warning delays',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(114,12,113,2,42,695,742,'Warning all','Warning all',NULL,NULL,NULL,NULL,NULL,'<2024-12-01 Sun -5d>',1733011200,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(115,12,113,2,45,742,792,'Warning first','Warning first',NULL,NULL,NULL,NULL,NULL,'<2024-12-01 Sun --2w>',1733011200,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(116,12,113,2,48,792,846,'Warning hour','Warning hour',NULL,NULL,NULL,NULL,NULL,'<2024-12-01 Sun 09:30 -3h>',1733045400,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(117,12,113,2,51,846,895,'Warning month','Warning month',NULL,NULL,NULL,NULL,NULL,'<2024-12-01 Sun -1m>',1733011200,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(118,12,113,2,54,895,944,'Warning year','Warning year',NULL,NULL,NULL,NULL,NULL,'<2024-12-01 Sun --1y>',1733011200,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(119,12,98,1,57,944,1210,'Repeater and warning combinations','Repeater and warning combinations',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(120,12,119,2,59,981,1041,'Repeater and warning','Repeater and warning',NULL,NULL,NULL,NULL,NULL,'<2024-12-01 Sun +1w -5d>',1733011200,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(121,12,119,2,62,1041,1133,'Catch up repeater with deadline part and warning','Catch up repeater with deadline part and warning',NULL,NULL,NULL,NULL,NULL,'<2024-12-01 Sun ++1m/2d -5d>',1733011200,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(122,12,119,2,65,1133,1210,'Restart repeater with first warning','Restart repeater with first warning',NULL,NULL,NULL,NULL,NULL,'<2024-12-01 Sun .+2w --1w>',1733011200,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(123,12,98,1,68,1210,1453,'Time and range combinations','Time and range combinations',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(124,12,123,2,70,1241,1302,'Time with repeater','Time with repeater',NULL,NULL,NULL,'<2024-11-20 Wed 09:30 +1w>',1732095000,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(125,12,123,2,73,1302,1375,'Time range with repeater','Time range with repeater',NULL,NULL,NULL,'<2024-11-20 Wed 09:30-11:00 +1w>',1732095000,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(126,12,123,2,76,1375,1453,'Date range with repeater','Date range with repeater',NULL,NULL,NULL,NULL,NULL,'<2024-12-01 Sun>--<2024-12-03 Tue +1w>',1733011200,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(127,12,98,1,79,1453,1532,'Inactive timestamp with repeater','Inactive timestamp with repeater',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(128,12,127,2,81,1489,1532,'Inactive repeater','Inactive repeater',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(129,12,98,1,84,1532,1626,'Diary negative case','Diary negative case',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(130,12,129,2,86,1555,1626,'Diary with apparent repeater text','Diary with apparent repeater text',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(131,13,NULL,0,1,-1,299,'Timestamps','Timestamps',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(132,13,131,1,4,40,91,'Meet Peter at the movies','Meet Peter at the movies',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(133,13,131,1,7,91,152,'Discussion on climate change','Discussion on climate change',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(134,13,131,1,10,152,201,'My days off','My days off',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(135,13,131,1,14,201,299,'Can be anywhere','Can be anywhere',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
CREATE TABLE todo_keywords (
    file_id         INTEGER NOT NULL,
    keyword         TEXT NOT NULL,
    state_type      TEXT NOT NULL CHECK (state_type IN ('open', 'closed')),
    shortcut        TEXT CHECK (shortcut IS NULL OR length(shortcut) = 1),
    sequence_no     INTEGER NOT NULL,
    source_kind     TEXT NOT NULL CHECK (
                        source_kind IN ('config_default', 'dir_locals', 'org_keyword')
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
        (source_kind = 'dir_locals' AND source_keyword IS NULL AND source_line_number IS NULL)
        OR
        (source_kind = 'org_keyword' AND source_keyword IS NOT NULL AND source_line_number IS NOT NULL)
    ),
    FOREIGN KEY (file_id)
        REFERENCES files(id)
        ON DELETE CASCADE,
    PRIMARY KEY (file_id, keyword)
);
INSERT INTO todo_keywords VALUES(1,'NEXT','open','n',0,'dir_locals',NULL,NULL);
INSERT INTO todo_keywords VALUES(1,'FINISHED','closed','f',1,'dir_locals',NULL,NULL);
INSERT INTO todo_keywords VALUES(2,'PLAN','open','p',0,'dir_locals',NULL,NULL);
INSERT INTO todo_keywords VALUES(2,'DONE','closed','d',1,'dir_locals',NULL,NULL);
INSERT INTO todo_keywords VALUES(3,'REVIEW','open','r',0,'org_keyword','TODO',2);
INSERT INTO todo_keywords VALUES(3,'BLOCKED','open','b',1,'org_keyword','TODO',2);
INSERT INTO todo_keywords VALUES(3,'CLOSED','closed','c',2,'org_keyword','TODO',2);
INSERT INTO todo_keywords VALUES(4,'PLAN','open','p',0,'dir_locals',NULL,NULL);
INSERT INTO todo_keywords VALUES(4,'WAIT','open','w',1,'dir_locals',NULL,NULL);
INSERT INTO todo_keywords VALUES(4,'DONE','closed','d',2,'dir_locals',NULL,NULL);
INSERT INTO todo_keywords VALUES(4,'CANCEL','closed','c',3,'dir_locals',NULL,NULL);
INSERT INTO todo_keywords VALUES(5,'TODO','open',NULL,0,'config_default',NULL,NULL);
INSERT INTO todo_keywords VALUES(5,'DONE','closed',NULL,1,'config_default',NULL,NULL);
INSERT INTO todo_keywords VALUES(6,'one','open','t',0,'org_keyword','TODO',3);
INSERT INTO todo_keywords VALUES(6,'two','open','n',1,'org_keyword','TODO',3);
INSERT INTO todo_keywords VALUES(6,'FIVE','open',NULL,2,'org_keyword','TODO',4);
INSERT INTO todo_keywords VALUES(6,'SIX','open',NULL,3,'org_keyword','TODO',4);
INSERT INTO todo_keywords VALUES(6,'seven','open',NULL,4,'org_keyword','TYP_TODO',5);
INSERT INTO todo_keywords VALUES(6,'nine','open',NULL,5,'org_keyword','SEQ_TODO',6);
INSERT INTO todo_keywords VALUES(6,'late_open','open',NULL,6,'org_keyword','TODO',34);
INSERT INTO todo_keywords VALUES(6,'three','closed','d',7,'org_keyword','TODO',3);
INSERT INTO todo_keywords VALUES(6,'four','closed','w',8,'org_keyword','TODO',3);
INSERT INTO todo_keywords VALUES(6,'eight','closed',NULL,9,'org_keyword','TYP_TODO',5);
INSERT INTO todo_keywords VALUES(6,'ten','closed',NULL,10,'org_keyword','SEQ_TODO',6);
INSERT INTO todo_keywords VALUES(6,'eleven','closed','c',11,'org_keyword','TODO',30);
INSERT INTO todo_keywords VALUES(6,'late_done','closed',NULL,12,'org_keyword','TODO',34);
INSERT INTO todo_keywords VALUES(7,'TODO','open',NULL,0,'org_keyword','TODO',29);
INSERT INTO todo_keywords VALUES(7,'NEXT','open',NULL,1,'org_keyword','TODO',29);
INSERT INTO todo_keywords VALUES(7,'IDEA','open','i',2,'org_keyword','SEQ_TODO',30);
INSERT INTO todo_keywords VALUES(7,'WURST','open','w',3,'org_keyword','SEQ_TODO',30);
INSERT INTO todo_keywords VALUES(7,'PLAN','open','p',4,'org_keyword','SEQ_TODO',30);
INSERT INTO todo_keywords VALUES(7,'BUILD','open','b',5,'org_keyword','SEQ_TODO',30);
INSERT INTO todo_keywords VALUES(7,'WAITING','open','w',6,'org_keyword','TYP_TODO',31);
INSERT INTO todo_keywords VALUES(7,'DONE','closed',NULL,7,'org_keyword','TODO',29);
INSERT INTO todo_keywords VALUES(7,'CANCELED','closed',NULL,8,'org_keyword','TODO',29);
INSERT INTO todo_keywords VALUES(8,'TODO','open',NULL,0,'config_default',NULL,NULL);
INSERT INTO todo_keywords VALUES(8,'DONE','closed',NULL,1,'config_default',NULL,NULL);
INSERT INTO todo_keywords VALUES(9,'TODO','open',NULL,0,'config_default',NULL,NULL);
INSERT INTO todo_keywords VALUES(9,'DONE','closed',NULL,1,'config_default',NULL,NULL);
INSERT INTO todo_keywords VALUES(10,'TODO','open',NULL,0,'config_default',NULL,NULL);
INSERT INTO todo_keywords VALUES(10,'DONE','closed',NULL,1,'config_default',NULL,NULL);
INSERT INTO todo_keywords VALUES(11,'TODO','open',NULL,0,'config_default',NULL,NULL);
INSERT INTO todo_keywords VALUES(11,'DONE','closed',NULL,1,'config_default',NULL,NULL);
INSERT INTO todo_keywords VALUES(12,'TODO','open',NULL,0,'config_default',NULL,NULL);
INSERT INTO todo_keywords VALUES(12,'DONE','closed',NULL,1,'config_default',NULL,NULL);
INSERT INTO todo_keywords VALUES(13,'TODO','open',NULL,0,'config_default',NULL,NULL);
INSERT INTO todo_keywords VALUES(13,'DONE','closed',NULL,1,'config_default',NULL,NULL);
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
INSERT INTO timestamps VALUES(1,54,'scheduled',1732060800,NULL,'active','none','<2024-11-20 Wed>',93,109,7);
INSERT INTO timestamps VALUES(2,55,'deadline',1733011200,NULL,'active','none','<2024-12-01 Sun>',140,156,10);
INSERT INTO timestamps VALUES(3,56,'closed',1732147200,NULL,'inactive','none','[2024-11-21 Thu]',183,199,13);
INSERT INTO timestamps VALUES(4,57,'deadline',1733011200,NULL,'active','none','<2024-12-01 Sun>',239,255,16);
INSERT INTO timestamps VALUES(5,57,'scheduled',1732060800,NULL,'active','none','<2024-11-20 Wed>',267,283,16);
INSERT INTO timestamps VALUES(6,57,'closed',1732147200,NULL,'inactive','none','[2024-11-21 Thu]',292,308,16);
INSERT INTO timestamps VALUES(7,58,'scheduled',1732095000,NULL,'active','none','<2024-11-20 Wed 09:30>',334,356,19);
INSERT INTO timestamps VALUES(8,59,'scheduled',1732095000,1732100400,'active','time_range','<2024-11-20 Wed 09:30-11:00>',392,420,22);
INSERT INTO timestamps VALUES(9,60,'deadline',1733011200,1733184000,'active','date_range','<2024-12-01 Sun>--<2024-12-03 Tue>',446,480,25);
INSERT INTO timestamps VALUES(10,61,'scheduled',1732060800,NULL,'active','none','<2024-11-20 Wed +1w>',505,525,28);
INSERT INTO timestamps VALUES(11,62,'body',NULL,NULL,'diary','none','<%%(diary-float t 42)>',558,580,31);
INSERT INTO timestamps VALUES(12,63,'scheduled',1732060800,NULL,'active','none','<2024-11-20 Wed>',618,634,34);
INSERT INTO timestamps VALUES(13,63,'scheduled',1732147200,NULL,'active','none','<2024-11-21 Thu>',646,662,34);
INSERT INTO timestamps VALUES(14,65,'scheduled',1732060800,NULL,'active','none','<2024-11-20 Wed>',757,773,40);
INSERT INTO timestamps VALUES(15,65,'body',1733011200,NULL,'active','none','<2024-12-01 Sun>',784,800,41);
INSERT INTO timestamps VALUES(16,65,'body',1732147200,NULL,'inactive','none','[2024-11-21 Thu]',809,825,42);
INSERT INTO timestamps VALUES(17,66,'body',1732060800,NULL,'active','none','<2024-11-20 Wed>',904,920,46);
INSERT INTO timestamps VALUES(18,67,'body',1733011200,NULL,'active','none','<2024-12-01 Sun>',978,994,49);
INSERT INTO timestamps VALUES(19,68,'body',1732060800,NULL,'active','none','<2024-11-20 Wed>',1060,1076,52);
INSERT INTO timestamps VALUES(20,69,'scheduled',1732060800,NULL,'active','none','<2024-11-20 Wed>',1115,1131,55);
INSERT INTO timestamps VALUES(21,69,'scheduled',1732147200,NULL,'active','none','<2024-11-21 Thu>',1143,1159,55);
INSERT INTO timestamps VALUES(22,100,'scheduled',1732060800,NULL,'active','none','<2024-11-20 Wed +1w>',48,68,4);
INSERT INTO timestamps VALUES(23,101,'scheduled',1732060800,NULL,'active','none','<2024-11-20 Wed ++1m>',103,124,7);
INSERT INTO timestamps VALUES(24,102,'scheduled',1732060800,NULL,'active','none','<2024-11-20 Wed .+2d>',157,178,10);
INSERT INTO timestamps VALUES(25,104,'scheduled',1732060800,NULL,'active','none','<2024-11-20 Wed +3h>',226,246,15);
INSERT INTO timestamps VALUES(26,105,'scheduled',1732060800,NULL,'active','none','<2024-11-20 Wed +3d>',275,295,18);
INSERT INTO timestamps VALUES(27,106,'scheduled',1732060800,NULL,'active','none','<2024-11-20 Wed +3w>',325,345,21);
INSERT INTO timestamps VALUES(28,107,'scheduled',1732060800,NULL,'active','none','<2024-11-20 Wed +3m>',376,396,24);
INSERT INTO timestamps VALUES(29,108,'scheduled',1732060800,NULL,'active','none','<2024-11-20 Wed +3y>',426,446,27);
INSERT INTO timestamps VALUES(30,110,'scheduled',1732060800,NULL,'active','none','<2024-11-20 Wed +1w/2d>',515,538,32);
INSERT INTO timestamps VALUES(31,111,'scheduled',1732060800,NULL,'active','none','<2024-11-20 Wed ++1m/1w>',582,606,35);
INSERT INTO timestamps VALUES(32,112,'scheduled',1732060800,NULL,'active','none','<2024-11-20 Wed .+1y/2m>',651,675,38);
INSERT INTO timestamps VALUES(33,114,'deadline',1733011200,NULL,'active','none','<2024-12-01 Sun -5d>',720,740,43);
INSERT INTO timestamps VALUES(34,115,'deadline',1733011200,NULL,'active','none','<2024-12-01 Sun --2w>',769,790,46);
INSERT INTO timestamps VALUES(35,116,'deadline',1733045400,NULL,'active','none','<2024-12-01 Sun 09:30 -3h>',818,844,49);
INSERT INTO timestamps VALUES(36,117,'deadline',1733011200,NULL,'active','none','<2024-12-01 Sun -1m>',873,893,52);
INSERT INTO timestamps VALUES(37,118,'deadline',1733011200,NULL,'active','none','<2024-12-01 Sun --1y>',921,942,55);
INSERT INTO timestamps VALUES(38,120,'deadline',1733011200,NULL,'active','none','<2024-12-01 Sun +1w -5d>',1015,1039,60);
INSERT INTO timestamps VALUES(39,121,'deadline',1733011200,NULL,'active','none','<2024-12-01 Sun ++1m/2d -5d>',1103,1131,63);
INSERT INTO timestamps VALUES(40,122,'deadline',1733011200,NULL,'active','none','<2024-12-01 Sun .+2w --1w>',1182,1208,66);
INSERT INTO timestamps VALUES(41,124,'scheduled',1732095000,NULL,'active','none','<2024-11-20 Wed 09:30 +1w>',1274,1300,71);
INSERT INTO timestamps VALUES(42,125,'scheduled',1732095000,1732100400,'active','time_range','<2024-11-20 Wed 09:30-11:00 +1w>',1341,1373,74);
INSERT INTO timestamps VALUES(43,126,'deadline',1733011200,1733184000,'active','date_range','<2024-12-01 Sun>--<2024-12-03 Tue +1w>',1413,1451,77);
INSERT INTO timestamps VALUES(44,128,'body',1732060800,NULL,'inactive','none','[2024-11-20 Wed +1w]',1510,1530,82);
INSERT INTO timestamps VALUES(45,130,'body',NULL,NULL,'diary','none','<%%(diary-float t 42)>',1603,1625,87);
INSERT INTO timestamps VALUES(46,132,'body',1162408500,NULL,'active','none','<2006-11-01 Wed 19:15>',67,89,5);
INSERT INTO timestamps VALUES(47,133,'body',1162461600,1162468800,'active','time_range','<2006-11-02 Thu 10:00-12:00>',122,150,8);
INSERT INTO timestamps VALUES(48,134,'body',1162512000,NULL,'active','none','<2006-11-03 Fri>',166,182,11);
INSERT INTO timestamps VALUES(49,134,'body',1162771200,NULL,'active','none','<2006-11-06 Mon>',183,199,12);
INSERT INTO timestamps VALUES(50,135,'body',1162512000,NULL,'active','none','<2006-11-03 Fri>',237,253,16);
INSERT INTO timestamps VALUES(51,135,'body',1782086400,NULL,'inactive','none','[2026-06-22 Mon]',281,297,18);
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
INSERT INTO keywords VALUES(1,1,'TITLE','Deepest .dir-locals wins test',1);
INSERT INTO keywords VALUES(2,6,'TITLE','Eval next to valid org-todo-keywords test',1);
INSERT INTO keywords VALUES(3,10,'TITLE','Org in-buffer TODO override test',1);
INSERT INTO keywords VALUES(4,10,'TODO','REVIEW(r) BLOCKED(b) | CLOSED(c)',2);
INSERT INTO keywords VALUES(5,16,'TITLE','Root dir-locals test',1);
INSERT INTO keywords VALUES(6,22,'TITLE','Unsafe syntax warning test',1);
INSERT INTO keywords VALUES(7,26,'TITLE','File-local TODO keywords',1);
INSERT INTO keywords VALUES(8,26,'STARTUP','showall',2);
INSERT INTO keywords VALUES(9,26,'TODO','one(t) two(n) | three(d) four(w@)',3);
INSERT INTO keywords VALUES(10,26,'TODO','FIVE SIX |',4);
INSERT INTO keywords VALUES(11,26,'TYP_TODO','seven | eight',5);
INSERT INTO keywords VALUES(12,26,'SEQ_TODO','nine | ten',6);
INSERT INTO keywords VALUES(13,26,'TODO','| eleven(c)',30);
INSERT INTO keywords VALUES(14,26,'TODO','late_open | late_done',34);
INSERT INTO keywords VALUES(15,44,'TITLE','Keyword Parsing Fixture',1);
INSERT INTO keywords VALUES(16,44,'STARTUP','showall',2);
INSERT INTO keywords VALUES(17,44,'AUTHOR','First Author',3);
INSERT INTO keywords VALUES(18,44,'PROPERTY','before_prop before-value',4);
INSERT INTO keywords VALUES(19,44,'CATEGORY','before-category',5);
INSERT INTO keywords VALUES(20,44,'AUTHOR','Later Author',10);
INSERT INTO keywords VALUES(21,44,'OPTIONS','toc:nil num:t',11);
INSERT INTO keywords VALUES(22,44,'PROPERTY','after_prop after-value',12);
INSERT INTO keywords VALUES(23,44,'PROPERTY','repeated_prop first',13);
INSERT INTO keywords VALUES(24,44,'PROPERTY','repeated_prop second',14);
INSERT INTO keywords VALUES(25,44,'PROPERTY','appended_prop base',15);
INSERT INTO keywords VALUES(26,44,'PROPERTY','appended_prop+ extra',16);
INSERT INTO keywords VALUES(27,44,'CATEGORY','after-category',17);
INSERT INTO keywords VALUES(28,44,'TITLE','Later Title',22);
INSERT INTO keywords VALUES(29,44,'EXPORT_FILE_NAME','later-export-name',23);
INSERT INTO keywords VALUES(30,44,'STARTUP','content',24);
INSERT INTO keywords VALUES(31,44,'TODO','TODO NEXT | DONE CANCELED',29);
INSERT INTO keywords VALUES(32,44,'SEQ_TODO','IDEA(i) WURST(w) PLAN(p) BUILD(b) | DONE(d)',30);
INSERT INTO keywords VALUES(33,44,'TYP_TODO','WAITING(w) | CANCELED(c)',31);
INSERT INTO keywords VALUES(34,50,'TITLE','Title can span',1);
INSERT INTO keywords VALUES(35,50,'TITLE','multiple lines,',2);
INSERT INTO keywords VALUES(36,50,'AUTHOR','Hubisan',3);
INSERT INTO keywords VALUES(37,50,'TITLE','even here',9);
INSERT INTO keywords VALUES(38,52,'TITLE','Planning timestamp',1);
INSERT INTO keywords VALUES(39,52,'STARTUP','showall',2);
INSERT INTO keywords VALUES(40,70,'TITLE','Org Property and Keyword Test',6);
INSERT INTO keywords VALUES(41,70,'STARTUP','showall',7);
INSERT INTO keywords VALUES(42,70,'CATEGORY','category_keyword_value',8);
INSERT INTO keywords VALUES(43,70,'PROPERTY','Effort_ALL 0:10 0:30 1:00',9);
INSERT INTO keywords VALUES(44,70,'PROPERTY','keyword_property valid',10);
INSERT INTO keywords VALUES(45,70,'PROPERTY','keyword_overwritten_by_second invalid',11);
INSERT INTO keywords VALUES(46,70,'PROPERTY','keyword_overwritten_by_second valid',12);
INSERT INTO keywords VALUES(47,70,'PROPERTY','keyword_append foo=1',13);
INSERT INTO keywords VALUES(48,70,'PROPERTY','keyword_append+ bar=2',14);
INSERT INTO keywords VALUES(49,70,'PROPERTY','later_keyword_property works_everywhere',80);
INSERT INTO keywords VALUES(50,70,'CATEGORY','later_category_keyword',81);
INSERT INTO keywords VALUES(51,70,'FILETAGS',':project:work:',90);
INSERT INTO keywords VALUES(52,70,'TAGS','work(w) home(h)',91);
INSERT INTO keywords VALUES(53,70,'COLUMNS','%TODO %50ITEM %Effort{:} %CLOCKSUM',92);
INSERT INTO keywords VALUES(54,70,'CONSTANTS','c=299792458',93);
INSERT INTO keywords VALUES(55,70,'AUTHOR','Jane Doe',94);
INSERT INTO keywords VALUES(56,70,'OPTIONS','toc:nil num:t',95);
INSERT INTO keywords VALUES(57,80,'TITLE','Tags and FILETAGS Fixture',1);
INSERT INTO keywords VALUES(58,80,'STARTUP','showall',2);
INSERT INTO keywords VALUES(59,80,'FILETAGS',':file:project:',3);
INSERT INTO keywords VALUES(60,80,'FILETAGS',':later:extra:',20);
INSERT INTO keywords VALUES(61,131,'TITLE','Timestamps',1);
INSERT INTO keywords VALUES(62,131,'STARTUP','showall',2);
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
INSERT INTO properties VALUES(1,44,'BEFORE_PROP','before-value','property_keyword',0,4);
INSERT INTO properties VALUES(2,44,'CATEGORY','before-category','category_keyword',0,5);
INSERT INTO properties VALUES(3,44,'AFTER_PROP','after-value','property_keyword',0,12);
INSERT INTO properties VALUES(4,44,'REPEATED_PROP','first','property_keyword',0,13);
INSERT INTO properties VALUES(5,44,'REPEATED_PROP','second','property_keyword',0,14);
INSERT INTO properties VALUES(6,44,'APPENDED_PROP','base','property_keyword',0,15);
INSERT INTO properties VALUES(7,44,'APPENDED_PROP','extra','property_keyword',1,16);
INSERT INTO properties VALUES(8,44,'CATEGORY','after-category','category_keyword',0,17);
INSERT INTO properties VALUES(9,70,'CATEGORY','Level 0 Category Property','property_drawer',0,2);
INSERT INTO properties VALUES(10,70,'WHATEVER','level 0 drawer property','property_drawer',0,3);
INSERT INTO properties VALUES(11,70,'ID','7dad9b62-a3cc-43ec-a60f-e650bdaeae6d','property_drawer',0,4);
INSERT INTO properties VALUES(12,70,'CATEGORY','category_keyword_value','category_keyword',0,8);
INSERT INTO properties VALUES(13,70,'EFFORT_ALL','0:10 0:30 1:00','property_keyword',0,9);
INSERT INTO properties VALUES(14,70,'KEYWORD_PROPERTY','valid','property_keyword',0,10);
INSERT INTO properties VALUES(15,70,'KEYWORD_OVERWRITTEN_BY_SECOND','invalid','property_keyword',0,11);
INSERT INTO properties VALUES(16,70,'KEYWORD_OVERWRITTEN_BY_SECOND','valid','property_keyword',0,12);
INSERT INTO properties VALUES(17,70,'KEYWORD_APPEND','foo=1','property_keyword',0,13);
INSERT INTO properties VALUES(18,70,'KEYWORD_APPEND','bar=2','property_keyword',1,14);
INSERT INTO properties VALUES(19,70,'LATER_KEYWORD_PROPERTY','works_everywhere','property_keyword',0,80);
INSERT INTO properties VALUES(20,70,'CATEGORY','later_category_keyword','category_keyword',0,81);
INSERT INTO properties VALUES(21,71,'ID','abc','property_drawer',0,18);
INSERT INTO properties VALUES(22,71,'CUSTOM_ID','task-custom-id','property_drawer',0,19);
INSERT INTO properties VALUES(23,71,'EFFORT','0:30','property_drawer',0,20);
INSERT INTO properties VALUES(24,71,'OWNER','Alice','property_drawer',0,21);
INSERT INTO properties VALUES(25,71,'DRAWER_PROP','valid','property_drawer',0,22);
INSERT INTO properties VALUES(26,72,'DEFINED_TWICE','invalid','property_drawer',0,28);
INSERT INTO properties VALUES(27,72,'DEFINED_TWICE','valid','property_drawer',0,29);
INSERT INTO properties VALUES(28,73,'ADD-VALUE','is','property_drawer',0,37);
INSERT INTO properties VALUES(29,73,'ADD-VALUE','valid','property_drawer',1,38);
INSERT INTO properties VALUES(30,74,'ID','lowercase-id','property_drawer',0,47);
INSERT INTO properties VALUES(31,74,'CUSTOM_ID','mixed-case-custom-id','property_drawer',0,48);
INSERT INTO properties VALUES(32,74,'DRAWER_PROP','valid','property_drawer',0,49);
INSERT INTO properties VALUES(33,74,'ADD-VALUE','appended','property_drawer',1,50);
INSERT INTO properties VALUES(34,75,'EMPTY','','property_drawer',0,60);
CREATE TABLE tags (
    heading_id      INTEGER NOT NULL,
    tag             TEXT NOT NULL,
    FOREIGN KEY (heading_id)
        REFERENCES headings(id)
        ON DELETE CASCADE,
    PRIMARY KEY (heading_id, tag)
);
INSERT INTO tags VALUES(70,'project');
INSERT INTO tags VALUES(70,'work');
INSERT INTO tags VALUES(80,'file');
INSERT INTO tags VALUES(80,'project');
INSERT INTO tags VALUES(80,'later');
INSERT INTO tags VALUES(80,'extra');
INSERT INTO tags VALUES(81,'parent');
INSERT INTO tags VALUES(82,'child');
INSERT INTO tags VALUES(83,'project');
INSERT INTO tags VALUES(83,'grandchild');
INSERT INTO tags VALUES(85,'file');
INSERT INTO tags VALUES(85,'sibling');
INSERT INTO tags VALUES(86,'after');
INSERT INTO tags VALUES(87,'parent');
INSERT INTO tags VALUES(89,'child');
INSERT INTO tags VALUES(91,'parent');
INSERT INTO tags VALUES(92,'second');
INSERT INTO tags VALUES(93,'child');
INSERT INTO tags VALUES(94,'extra');
INSERT INTO tags VALUES(96,'local');
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
INSERT INTO heading_bodies VALUES(26,'See [[file:../../notes/org-semantics/file-local-todo-keywords.org]]',164,231);
INSERT INTO heading_bodies VALUES(27,'Default TODO is not valid because file-local TODO lines override defaults.',278,352);
INSERT INTO heading_bodies VALUES(28,'Default DONE is not valid because file-local TODO lines override defaults.',403,477);
INSERT INTO heading_bodies VALUES(45,'This heading has body text before later keywords.',163,212);
INSERT INTO heading_bodies VALUES(46,'This child should not directly receive keyword rows.',471,523);
INSERT INTO heading_bodies VALUES(47,'This heading appears after later keywords.',626,668);
INSERT INTO heading_bodies VALUES(48,unistr('This line mentions #+TITLE: Inline Mention but should only become a keyword row if Orgize exposes it as a keyword node.\u000aThis line mentions #+PROPERTY: inline_prop invalid in prose.\u000a\u000a#+BEGIN_EXAMPLE\u000a#+TITLE: Example Block Title\u000a#+PROPERTY: example_prop invalid\u000a#+CATEGORY: example-category\u000a#+END_EXAMPLE\u000a\u000a#+begin_src org\u000a  ,#+TITLE: Source Block Title\u000a  ,#+PROPERTY: source_prop invalid\u000a  ,#+CATEGORY: source-category\u000a#+end_src'),836,1262);
INSERT INTO heading_bodies VALUES(49,unistr('- All real keyword nodes exposed by Orgize are stored as raw ~keywords~ rows attached to the level 0 heading.\u000a- Keyword rows are not attached to regular headings.\u000a- Duplicate keyword rows are preserved.\u000a- Source order is preserved with ~line_number~ and/or insertion order.\u000a- Generic keywords such as ~TITLE~, ~AUTHOR~, ~STARTUP~, ~OPTIONS~, and ~EXPORT_FILE_NAME~ remain raw keyword rows only.\u000a- ~TODO~, ~SEQ_TODO~, and ~TYP_TODO~ may additionally create normalized ~todo_keywords~ rows if that normalization is in scope.\u000a- ~PROPERTY~ rows may additionally create normalized ~properties~ rows with ~source = property_keyword~ if that normalization is in scope.\u000a- ~CATEGORY~ rows may additionally create normalized ~properties~ rows with ~source = category_keyword~ if that normalization is in scope.\u000a- Keywords inside example/source blocks must not create keyword rows unless Orgize incorrectly exposes them as keyword nodes; if that happens, document the Orgize behavior as a parser risk.'),1285,2275);
INSERT INTO heading_bodies VALUES(50,'See [[file:../../notes/org-semantics/multipe-title-keywords.org]]',69,134);
INSERT INTO heading_bodies VALUES(51,unistr('This can be proven by using ~org-latex-export-as-latex~:\u000a\u000a#+BEGIN_SRC latex\u000a  \\title{Title can span multiple lines, even here}\u000a#+END_SRC'),NULL,NULL);
INSERT INTO heading_bodies VALUES(62,'SCHEDULED: <%%(diary-float t 42)>',547,580);
INSERT INTO heading_bodies VALUES(63,'In that case Org uses the second entry.',663,702);
INSERT INTO heading_bodies VALUES(65,unistr('DEADLINE: <2024-12-01 Sun>\u000aCLOSED: [2024-11-21 Thu]'),774,825);
INSERT INTO heading_bodies VALUES(66,unistr('Some body text first.\u000aSCHEDULED: <2024-11-20 Wed>'),871,920);
INSERT INTO heading_bodies VALUES(67,'This mentions DEADLINE: <2024-12-01 Sun> inside text.',954,1007);
INSERT INTO heading_bodies VALUES(68,'scheduled: <2024-11-20 Wed>',1049,1076);
INSERT INTO heading_bodies VALUES(71,'Body text for the first task.',607,636);
INSERT INTO heading_bodies VALUES(72,unistr('Expected raw/direct storage:\u000a- both DEFINED_TWICE rows should be preserved\u000a- no overwrite should be computed here'),743,856);
INSERT INTO heading_bodies VALUES(73,unistr('Expected raw/direct storage:\u000a- ADD-VALUE = is, append = 0\u000a- ADD-VALUE = valid, append = 1\u000a- final value "is valid" is not computed in this task'),949,1092);
INSERT INTO heading_bodies VALUES(74,unistr('Expected normalized keys:\u000a- ID\u000a- CUSTOM_ID\u000a- DRAWER_PROP\u000a- ADD-VALUE'),1234,1302);
INSERT INTO heading_bodies VALUES(75,unistr('Expected:\u000a- key EMPTY\u000a- value ""\u000a- source property_drawer'),1378,1435);
INSERT INTO heading_bodies VALUES(76,unistr(':PROPERTIES:\u000a:EMPTY:\u000a:END:\u000aExpected for now:\u000a- Orgize may expose this as a generic drawer, not PROPERTY_DRAWER\u000a- parser should not add fallback parsing in this task\u000a- no property row is expected if Orgize does not expose NODE_PROPERTY'),1482,1716);
INSERT INTO heading_bodies VALUES(77,unistr('This heading should not directly receive file-level #+PROPERTY or #+CATEGORY rows.\u000aThose belong to the synthetic level 0 heading only.'),1760,1894);
INSERT INTO heading_bodies VALUES(78,unistr('This heading still should not directly receive those keyword properties.\u000aThey should be stored on level 0 as:\u000a- LATER_KEYWORD_PROPERTY = works_everywhere, source property_keyword\u000a- CATEGORY = later_category_keyword, source category_keyword'),2023,2262);
INSERT INTO heading_bodies VALUES(79,unistr('These keyword lines should not create property rows in this task.\u000aFILETAGS belongs to the later tags task.\u000aTAGS may later become tag-definition metadata.\u000aCOLUMNS references properties but does not define property values.\u000aCONSTANTS belongs to table/formula semantics.\u000aAUTHOR and OPTIONS remain raw keywords.'),2476,2782);
INSERT INTO heading_bodies VALUES(81,'Parent body.',100,112);
INSERT INTO heading_bodies VALUES(82,'Child body.',131,142);
INSERT INTO heading_bodies VALUES(83,'Grandchild repeats one FILETAG locally and adds a local tag.',180,240);
INSERT INTO heading_bodies VALUES(84,'Sibling body.',252,265);
INSERT INTO heading_bodies VALUES(85,'This heading repeats one FILETAG locally.',300,341);
INSERT INTO heading_bodies VALUES(86,'This heading appears after a later FILETAGS keyword.',401,453);
INSERT INTO heading_bodies VALUES(87,'Parent body.',486,498);
INSERT INTO heading_bodies VALUES(88,'Child should have all_tags_json = ["parent"].',529,574);
INSERT INTO heading_bodies VALUES(89,'Child should have all_tags_json = ["parent", "child"].',608,662);
INSERT INTO heading_bodies VALUES(90,'Grandchild should have all_tags_json = ["parent", "child"].',693,752);
INSERT INTO heading_bodies VALUES(91,unistr('Duplicate local tag should not be repeated.\u000aExpected all_tags_json = ["parent", "child"].'),803,892);
INSERT INTO heading_bodies VALUES(92,'Second parent starts a separate tag inheritance branch.',919,974);
INSERT INTO heading_bodies VALUES(93,'Expected all_tags_json = ["second", "child"].',1015,1060);
INSERT INTO heading_bodies VALUES(94,'Expected all_tags_json = ["second", "child", "extra"].',1107,1161);
INSERT INTO heading_bodies VALUES(96,'Expected all_tags_json = ["local"].',1219,1254);
INSERT INTO heading_bodies VALUES(97,'Expected all_tags_json = [].',1278,1306);
INSERT INTO heading_bodies VALUES(110,'SCHEDULED: <2024-11-20 Wed +1w/2d>',504,538);
INSERT INTO heading_bodies VALUES(111,'SCHEDULED: <2024-11-20 Wed ++1m/1w>',571,606);
INSERT INTO heading_bodies VALUES(112,'SCHEDULED: <2024-11-20 Wed .+1y/2m>',640,675);
INSERT INTO heading_bodies VALUES(121,'DEADLINE: <2024-12-01 Sun ++1m/2d -5d>',1093,1131);
INSERT INTO heading_bodies VALUES(128,'[2024-11-20 Wed +1w]',1510,1530);
INSERT INTO heading_bodies VALUES(130,'SCHEDULED: <%%(diary-float t 42)>',1592,1625);
INSERT INTO heading_bodies VALUES(132,'<2006-11-01 Wed 19:15>',67,89);
INSERT INTO heading_bodies VALUES(133,'<2006-11-02 Thu 10:00-12:00>',122,150);
INSERT INTO heading_bodies VALUES(134,unistr('<2006-11-03 Fri>\u000a<2006-11-06 Mon>'),166,199);
INSERT INTO heading_bodies VALUES(135,unistr('Also in the body <2006-11-03 Fri>.\u000a\u000aThis is an inactive one: [2026-06-22 Mon].'),220,298);
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
INSERT INTO outline_path VALUES(1,1,NULL,0,'0000','["Deepest .dir-locals wins test"]');
INSERT INTO outline_path VALUES(2,1,1,1,'0000.0001','["Deepest .dir-locals wins test","Should be open from notes/child/.dir-locals.el"]');
INSERT INTO outline_path VALUES(3,1,1,1,'0000.0002','["Deepest .dir-locals wins test","Should be closed from notes/child/.dir-locals.el"]');
INSERT INTO outline_path VALUES(4,1,1,1,'0000.0003','["Deepest .dir-locals wins test","PLAN Should NOT be recognized because child .dir-locals.el wins"]');
INSERT INTO outline_path VALUES(5,1,1,1,'0000.0004','["Deepest .dir-locals wins test","DONE Should NOT be recognized because child .dir-locals.el wins"]');
INSERT INTO outline_path VALUES(6,2,NULL,0,'0000','["Eval next to valid org-todo-keywords test"]');
INSERT INTO outline_path VALUES(7,2,6,1,'0000.0001','["Eval next to valid org-todo-keywords test","Should be open from valid org-todo-keywords despite eval"]');
INSERT INTO outline_path VALUES(8,2,6,1,'0000.0002','["Eval next to valid org-todo-keywords test","Should be closed from valid org-todo-keywords despite eval"]');
INSERT INTO outline_path VALUES(9,2,6,1,'0000.0003','["Eval next to valid org-todo-keywords test","TODO Should NOT be recognized because dir-locals TODOs are active"]');
INSERT INTO outline_path VALUES(10,3,NULL,0,'0000','["Org in-buffer TODO override test"]');
INSERT INTO outline_path VALUES(11,3,10,1,'0000.0001','["Org in-buffer TODO override test","Should be open from in-buffer #+TODO"]');
INSERT INTO outline_path VALUES(12,3,10,1,'0000.0002','["Org in-buffer TODO override test","Should also be open from in-buffer #+TODO"]');
INSERT INTO outline_path VALUES(13,3,10,1,'0000.0003','["Org in-buffer TODO override test","Should be closed from in-buffer #+TODO"]');
INSERT INTO outline_path VALUES(14,3,10,1,'0000.0004','["Org in-buffer TODO override test","PLAN Should NOT be recognized because #+TODO overrides .dir-locals.el"]');
INSERT INTO outline_path VALUES(15,3,10,1,'0000.0005','["Org in-buffer TODO override test","DONE Should NOT be recognized because #+TODO overrides .dir-locals.el"]');
INSERT INTO outline_path VALUES(16,4,NULL,0,'0000','["Root dir-locals test"]');
INSERT INTO outline_path VALUES(17,4,16,1,'0000.0001','["Root dir-locals test","Should be open from .dir-locals.el"]');
INSERT INTO outline_path VALUES(18,4,16,1,'0000.0002','["Root dir-locals test","Should also be open from .dir-locals.el"]');
INSERT INTO outline_path VALUES(19,4,16,1,'0000.0003','["Root dir-locals test","Should be closed from .dir-locals.el"]');
INSERT INTO outline_path VALUES(20,4,16,1,'0000.0004','["Root dir-locals test","Should also be closed from .dir-locals.el"]');
INSERT INTO outline_path VALUES(21,4,16,1,'0000.0005','["Root dir-locals test","TODO Should NOT be recognized when .dir-locals.el is active"]');
INSERT INTO outline_path VALUES(22,5,NULL,0,'0000','["Unsafe syntax warning test"]');
INSERT INTO outline_path VALUES(23,5,22,1,'0000.0001','["Unsafe syntax warning test","Should be open from config fallback when unsafe .dir-locals.el is ignored with warning"]');
INSERT INTO outline_path VALUES(24,5,22,1,'0000.0002','["Unsafe syntax warning test","Should be closed from config fallback when unsafe .dir-locals.el is ignored with warning"]');
INSERT INTO outline_path VALUES(25,5,22,1,'0000.0003','["Unsafe syntax warning test","PLAN Should NOT be recognized in this folder because unsafe .dir-locals.el is unusable"]');
INSERT INTO outline_path VALUES(26,6,NULL,0,'0000','["File-local TODO keywords"]');
INSERT INTO outline_path VALUES(27,6,26,1,'0000.0001','["File-local TODO keywords","TODO default keyword should stay in title"]');
INSERT INTO outline_path VALUES(28,6,26,1,'0000.0002','["File-local TODO keywords","DONE default done keyword should stay in title"]');
INSERT INTO outline_path VALUES(29,6,26,1,'0000.0003','["File-local TODO keywords","open keyword with fast key"]');
INSERT INTO outline_path VALUES(30,6,26,1,'0000.0004','["File-local TODO keywords","another open keyword with fast key"]');
INSERT INTO outline_path VALUES(31,6,26,1,'0000.0005','["File-local TODO keywords","closed keyword with fast key"]');
INSERT INTO outline_path VALUES(32,6,26,1,'0000.0006','["File-local TODO keywords","closed keyword with extended fast key"]');
INSERT INTO outline_path VALUES(33,6,26,1,'0000.0007','["File-local TODO keywords","open keyword from empty-done-side line"]');
INSERT INTO outline_path VALUES(34,6,26,1,'0000.0008','["File-local TODO keywords","another open keyword from empty-done-side line"]');
INSERT INTO outline_path VALUES(35,6,26,1,'0000.0009','["File-local TODO keywords","open keyword from TYP_TODO"]');
INSERT INTO outline_path VALUES(36,6,26,1,'0000.0010','["File-local TODO keywords","closed keyword from TYP_TODO"]');
INSERT INTO outline_path VALUES(37,6,26,1,'0000.0011','["File-local TODO keywords","open keyword from SEQ_TODO"]');
INSERT INTO outline_path VALUES(38,6,26,1,'0000.0012','["File-local TODO keywords","closed keyword from SEQ_TODO"]');
INSERT INTO outline_path VALUES(39,6,26,1,'0000.0013','["File-local TODO keywords","closed keyword from later TODO line"]');
INSERT INTO outline_path VALUES(40,6,26,1,'0000.0014','["File-local TODO keywords","open keyword from line defined later in file"]');
INSERT INTO outline_path VALUES(41,6,26,1,'0000.0015','["File-local TODO keywords","closed keyword from line defined later in file"]');
INSERT INTO outline_path VALUES(42,6,26,1,'0000.0016','["File-local TODO keywords","TODO still not valid after later local lines"]');
INSERT INTO outline_path VALUES(43,6,26,1,'0000.0017','["File-local TODO keywords","DONE still not valid after later local lines"]');
INSERT INTO outline_path VALUES(44,7,NULL,0,'0000','["Keyword Parsing Fixture Later Title"]');
INSERT INTO outline_path VALUES(45,7,44,1,'0000.0001','["Keyword Parsing Fixture Later Title","First heading"]');
INSERT INTO outline_path VALUES(46,7,45,2,'0000.0001.0001','["Keyword Parsing Fixture Later Title","First heading","Child heading"]');
INSERT INTO outline_path VALUES(47,7,44,1,'0000.0002','["Keyword Parsing Fixture Later Title","Second heading"]');
INSERT INTO outline_path VALUES(48,7,44,1,'0000.0003','["Keyword Parsing Fixture Later Title","Boundary: keyword-looking body text"]');
INSERT INTO outline_path VALUES(49,7,44,1,'0000.0004','["Keyword Parsing Fixture Later Title","Expected behavior"]');
INSERT INTO outline_path VALUES(50,8,NULL,0,'0000','["Title can span multiple lines, even here"]');
INSERT INTO outline_path VALUES(51,8,50,1,'0000.0001','["Title can span multiple lines, even here","Unfortunately Everywhere"]');
INSERT INTO outline_path VALUES(52,9,NULL,0,'0000','["Planning timestamp"]');
INSERT INTO outline_path VALUES(53,9,52,1,'0000.0001','["Planning timestamp","Planning"]');
INSERT INTO outline_path VALUES(54,9,53,2,'0000.0001.0001','["Planning timestamp","Planning","Simple scheduled"]');
INSERT INTO outline_path VALUES(55,9,53,2,'0000.0001.0002','["Planning timestamp","Planning","Simple deadline"]');
INSERT INTO outline_path VALUES(56,9,53,2,'0000.0001.0003','["Planning timestamp","Planning","Simple closed"]');
INSERT INTO outline_path VALUES(57,9,53,2,'0000.0001.0004','["Planning timestamp","Planning","All on one planning line"]');
INSERT INTO outline_path VALUES(58,9,53,2,'0000.0001.0005','["Planning timestamp","Planning","With time"]');
INSERT INTO outline_path VALUES(59,9,53,2,'0000.0001.0006','["Planning timestamp","Planning","Time range same day"]');
INSERT INTO outline_path VALUES(60,9,53,2,'0000.0001.0007','["Planning timestamp","Planning","Date range"]');
INSERT INTO outline_path VALUES(61,9,53,2,'0000.0001.0008','["Planning timestamp","Planning","Repeater"]');
INSERT INTO outline_path VALUES(62,9,53,2,'0000.0001.0009','["Planning timestamp","Planning","Diary expression"]');
INSERT INTO outline_path VALUES(63,9,53,2,'0000.0001.0010','["Planning timestamp","Planning","Multiple same keyword"]');
INSERT INTO outline_path VALUES(64,9,53,2,'0000.0001.0011','["Planning timestamp","Planning","Not valid"]');
INSERT INTO outline_path VALUES(65,9,64,3,'0000.0001.0011.0001','["Planning timestamp","Planning","Not valid","Multiple planning lines"]');
INSERT INTO outline_path VALUES(66,9,64,3,'0000.0001.0011.0002','["Planning timestamp","Planning","Not valid","Planning not immediately after headline"]');
INSERT INTO outline_path VALUES(67,9,64,3,'0000.0001.0011.0003','["Planning timestamp","Planning","Not valid","Looks like planning in body"]');
INSERT INTO outline_path VALUES(68,9,64,3,'0000.0001.0011.0004','["Planning timestamp","Planning","Not valid","Lowercase should probably not count"]');
INSERT INTO outline_path VALUES(69,9,64,3,'0000.0001.0011.0005','["Planning timestamp","Planning","Not valid","Multiple same keyword"]');
INSERT INTO outline_path VALUES(70,10,NULL,0,'0000','["Org Property and Keyword Test"]');
INSERT INTO outline_path VALUES(71,10,70,1,'0000.0001','["Org Property and Keyword Test","Task with multiple drawer properties"]');
INSERT INTO outline_path VALUES(72,10,70,1,'0000.0002','["Org Property and Keyword Test","Task with duplicate drawer properties"]');
INSERT INTO outline_path VALUES(73,10,70,1,'0000.0003','["Org Property and Keyword Test","Task with append operator in drawer"]');
INSERT INTO outline_path VALUES(74,10,70,1,'0000.0004','["Org Property and Keyword Test","Task with mixed-case keys"]');
INSERT INTO outline_path VALUES(75,10,70,1,'0000.0005','["Org Property and Keyword Test","Task with empty property accepted by Orgize"]');
INSERT INTO outline_path VALUES(76,10,70,1,'0000.0006','["Org Property and Keyword Test","Task with Orgize empty-property limitation"]');
INSERT INTO outline_path VALUES(77,10,70,1,'0000.0007','["Org Property and Keyword Test","Task after file-level property keywords"]');
INSERT INTO outline_path VALUES(78,10,70,1,'0000.0008','["Org Property and Keyword Test","Task after later file-level keywords"]');
INSERT INTO outline_path VALUES(79,10,70,1,'0000.0009','["Org Property and Keyword Test","Boundary: property-like but not properties"]');
INSERT INTO outline_path VALUES(80,11,NULL,0,'0000','["Tags and FILETAGS Fixture"]');
INSERT INTO outline_path VALUES(81,11,80,1,'0000.0001','["Tags and FILETAGS Fixture","Parent"]');
INSERT INTO outline_path VALUES(82,11,81,2,'0000.0001.0001','["Tags and FILETAGS Fixture","Parent","Child"]');
INSERT INTO outline_path VALUES(83,11,82,3,'0000.0001.0001.0001','["Tags and FILETAGS Fixture","Parent","Child","Grandchild"]');
INSERT INTO outline_path VALUES(84,11,80,1,'0000.0002','["Tags and FILETAGS Fixture","Sibling"]');
INSERT INTO outline_path VALUES(85,11,80,1,'0000.0003','["Tags and FILETAGS Fixture","Duplicate Local"]');
INSERT INTO outline_path VALUES(86,11,80,1,'0000.0004','["Tags and FILETAGS Fixture","After Later FILETAGS"]');
INSERT INTO outline_path VALUES(87,11,80,1,'0000.0005','["Tags and FILETAGS Fixture","parent with one tag"]');
INSERT INTO outline_path VALUES(88,11,87,2,'0000.0005.0001','["Tags and FILETAGS Fixture","parent with one tag","child inherits parent tag"]');
INSERT INTO outline_path VALUES(89,11,87,2,'0000.0005.0002','["Tags and FILETAGS Fixture","parent with one tag","child with local tag"]');
INSERT INTO outline_path VALUES(90,11,89,3,'0000.0005.0002.0001','["Tags and FILETAGS Fixture","parent with one tag","child with local tag","grandchild inherits both"]');
INSERT INTO outline_path VALUES(91,11,89,3,'0000.0005.0002.0002','["Tags and FILETAGS Fixture","parent with one tag","child with local tag","grandchild with duplicate local tag"]');
INSERT INTO outline_path VALUES(92,11,80,1,'0000.0006','["Tags and FILETAGS Fixture","second parent"]');
INSERT INTO outline_path VALUES(93,11,92,2,'0000.0006.0001','["Tags and FILETAGS Fixture","second parent","second child with local tag"]');
INSERT INTO outline_path VALUES(94,11,93,3,'0000.0006.0001.0001','["Tags and FILETAGS Fixture","second parent","second child with local tag","second grandchild with extra tag"]');
INSERT INTO outline_path VALUES(95,11,80,1,'0000.0007','["Tags and FILETAGS Fixture","untagged parent"]');
INSERT INTO outline_path VALUES(96,11,95,2,'0000.0007.0001','["Tags and FILETAGS Fixture","untagged parent","child with only local tag"]');
INSERT INTO outline_path VALUES(97,11,95,2,'0000.0007.0002','["Tags and FILETAGS Fixture","untagged parent","child without tags"]');
INSERT INTO outline_path VALUES(98,12,NULL,0,'0000','["timestamp-repeaters"]');
INSERT INTO outline_path VALUES(99,12,98,1,'0000.0001','["timestamp-repeaters","Repeater markers"]');
INSERT INTO outline_path VALUES(100,12,99,2,'0000.0001.0001','["timestamp-repeaters","Repeater markers","Cumulate plus"]');
INSERT INTO outline_path VALUES(101,12,99,2,'0000.0001.0002','["timestamp-repeaters","Repeater markers","Catch up plus plus"]');
INSERT INTO outline_path VALUES(102,12,99,2,'0000.0001.0003','["timestamp-repeaters","Repeater markers","Restart dot plus"]');
INSERT INTO outline_path VALUES(103,12,98,1,'0000.0002','["timestamp-repeaters","Repeater units"]');
INSERT INTO outline_path VALUES(104,12,103,2,'0000.0002.0001','["timestamp-repeaters","Repeater units","Repeater hour"]');
INSERT INTO outline_path VALUES(105,12,103,2,'0000.0002.0002','["timestamp-repeaters","Repeater units","Repeater day"]');
INSERT INTO outline_path VALUES(106,12,103,2,'0000.0002.0003','["timestamp-repeaters","Repeater units","Repeater week"]');
INSERT INTO outline_path VALUES(107,12,103,2,'0000.0002.0004','["timestamp-repeaters","Repeater units","Repeater month"]');
INSERT INTO outline_path VALUES(108,12,103,2,'0000.0002.0005','["timestamp-repeaters","Repeater units","Repeater year"]');
INSERT INTO outline_path VALUES(109,12,98,1,'0000.0003','["timestamp-repeaters","Repeater deadline part"]');
INSERT INTO outline_path VALUES(110,12,109,2,'0000.0003.0001','["timestamp-repeaters","Repeater deadline part","Repeater with deadline day"]');
INSERT INTO outline_path VALUES(111,12,109,2,'0000.0003.0002','["timestamp-repeaters","Repeater deadline part","Repeater with deadline week"]');
INSERT INTO outline_path VALUES(112,12,109,2,'0000.0003.0003','["timestamp-repeaters","Repeater deadline part","Repeater with deadline month"]');
INSERT INTO outline_path VALUES(113,12,98,1,'0000.0004','["timestamp-repeaters","Warning delays"]');
INSERT INTO outline_path VALUES(114,12,113,2,'0000.0004.0001','["timestamp-repeaters","Warning delays","Warning all"]');
INSERT INTO outline_path VALUES(115,12,113,2,'0000.0004.0002','["timestamp-repeaters","Warning delays","Warning first"]');
INSERT INTO outline_path VALUES(116,12,113,2,'0000.0004.0003','["timestamp-repeaters","Warning delays","Warning hour"]');
INSERT INTO outline_path VALUES(117,12,113,2,'0000.0004.0004','["timestamp-repeaters","Warning delays","Warning month"]');
INSERT INTO outline_path VALUES(118,12,113,2,'0000.0004.0005','["timestamp-repeaters","Warning delays","Warning year"]');
INSERT INTO outline_path VALUES(119,12,98,1,'0000.0005','["timestamp-repeaters","Repeater and warning combinations"]');
INSERT INTO outline_path VALUES(120,12,119,2,'0000.0005.0001','["timestamp-repeaters","Repeater and warning combinations","Repeater and warning"]');
INSERT INTO outline_path VALUES(121,12,119,2,'0000.0005.0002','["timestamp-repeaters","Repeater and warning combinations","Catch up repeater with deadline part and warning"]');
INSERT INTO outline_path VALUES(122,12,119,2,'0000.0005.0003','["timestamp-repeaters","Repeater and warning combinations","Restart repeater with first warning"]');
INSERT INTO outline_path VALUES(123,12,98,1,'0000.0006','["timestamp-repeaters","Time and range combinations"]');
INSERT INTO outline_path VALUES(124,12,123,2,'0000.0006.0001','["timestamp-repeaters","Time and range combinations","Time with repeater"]');
INSERT INTO outline_path VALUES(125,12,123,2,'0000.0006.0002','["timestamp-repeaters","Time and range combinations","Time range with repeater"]');
INSERT INTO outline_path VALUES(126,12,123,2,'0000.0006.0003','["timestamp-repeaters","Time and range combinations","Date range with repeater"]');
INSERT INTO outline_path VALUES(127,12,98,1,'0000.0007','["timestamp-repeaters","Inactive timestamp with repeater"]');
INSERT INTO outline_path VALUES(128,12,127,2,'0000.0007.0001','["timestamp-repeaters","Inactive timestamp with repeater","Inactive repeater"]');
INSERT INTO outline_path VALUES(129,12,98,1,'0000.0008','["timestamp-repeaters","Diary negative case"]');
INSERT INTO outline_path VALUES(130,12,129,2,'0000.0008.0001','["timestamp-repeaters","Diary negative case","Diary with apparent repeater text"]');
INSERT INTO outline_path VALUES(131,13,NULL,0,'0000','["Timestamps"]');
INSERT INTO outline_path VALUES(132,13,131,1,'0000.0001','["Timestamps","Meet Peter at the movies"]');
INSERT INTO outline_path VALUES(133,13,131,1,'0000.0002','["Timestamps","Discussion on climate change"]');
INSERT INTO outline_path VALUES(134,13,131,1,'0000.0003','["Timestamps","My days off"]');
INSERT INTO outline_path VALUES(135,13,131,1,'0000.0004','["Timestamps","Can be anywhere"]');
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
CREATE INDEX idx_todo_keywords_file_state
    ON todo_keywords(file_id, state_type);
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
