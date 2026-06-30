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
INSERT INTO files VALUES(1,'/home/hubisan/projects/coding/org-files-db/.project/manual-tests/files/bracket-links.org',1782811598000160725,3503,NULL,1782811623);
INSERT INTO files VALUES(2,'/home/hubisan/projects/coding/org-files-db/.project/manual-tests/files/file-local-todo-keywords.org',1782810515620385647,1188,NULL,1782811623);
INSERT INTO files VALUES(3,'/home/hubisan/projects/coding/org-files-db/.project/manual-tests/files/keywords.org',1782237960833303823,2276,NULL,1782811623);
INSERT INTO files VALUES(4,'/home/hubisan/projects/coding/org-files-db/.project/manual-tests/files/multipe-title-keywords.org',1781809982908544855,321,NULL,1782811623);
INSERT INTO files VALUES(5,'/home/hubisan/projects/coding/org-files-db/.project/manual-tests/files/planning-lines.org',1782129087807207205,1163,NULL,1782811623);
INSERT INTO files VALUES(6,'/home/hubisan/projects/coding/org-files-db/.project/manual-tests/files/properties.org',1782218329407221851,2783,NULL,1782811623);
INSERT INTO files VALUES(7,'/home/hubisan/projects/coding/org-files-db/.project/manual-tests/files/tags.org',1782243198853649058,1307,NULL,1782811623);
INSERT INTO files VALUES(8,'/home/hubisan/projects/coding/org-files-db/.project/manual-tests/files/timestamp-repeaters.org',1782159459016426918,1626,NULL,1782811623);
INSERT INTO files VALUES(9,'/home/hubisan/projects/coding/org-files-db/.project/manual-tests/files/timestamps.org',1782153027780066037,299,NULL,1782811623);
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
INSERT INTO headings VALUES(1,1,NULL,0,1,-1,3503,'Bracket Link Fixture','Bracket Link Fixture',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(2,1,1,1,10,290,978,'Internal bracket links','Internal bracket links',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(3,1,2,2,15,374,503,'Custom ID links','Custom ID links',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(4,1,2,2,19,503,614,'Fuzzy heading links','Fuzzy heading links',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(5,1,2,2,23,614,739,'Dedicated target links','Dedicated target links',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(6,1,2,2,28,739,860,'Named target style fuzzy links','Named target style fuzzy links',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(7,1,2,2,33,860,978,'Fuzzy fallback','Fuzzy fallback',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(8,1,1,1,38,978,2222,'File-like bracket links','File-like bracket links',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(9,1,8,2,40,1005,1317,'Explicit file links','Explicit file links',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(10,1,8,2,54,1317,1486,'Explicit file variants','Explicit file variants',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(11,1,8,2,60,1486,1664,'Implicit file links','Implicit file links',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(12,1,8,2,71,1664,2222,'File search options','File search options',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(13,1,1,1,85,2222,2843,'Typed bracket links','Typed bracket links',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(14,1,13,2,87,2245,2619,'Built-in typed links','Built-in typed links',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(15,1,13,2,99,2619,2843,'Unknown and custom typed links','Unknown and custom typed links',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(16,1,1,1,107,2843,3147,'Negative non-bracket examples','Negative non-bracket examples',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(17,1,1,1,119,3147,3503,'Unicode bracket links','Unicode bracket links',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(18,1,17,2,123,3211,3503,'Übung 🚀 Ein Titel mit Umlaut und Emoji','Übung 🚀 Ein Titel mit Umlaut und Emoji',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'["foo","bar"]');
INSERT INTO headings VALUES(19,2,NULL,0,1,-1,1188,'File-local TODO keywords','File-local TODO keywords',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(20,2,19,1,10,234,354,'TODO default keyword should stay in title','TODO default keyword should stay in title',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(21,2,19,1,13,354,479,'DONE default done keyword should stay in title','DONE default done keyword should stay in title',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(22,2,19,1,16,479,512,'open keyword with fast key','open keyword with fast key','one','open',NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(23,2,19,1,17,512,553,'another open keyword with fast key','another open keyword with fast key','two','open',NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(24,2,19,1,18,553,590,'closed keyword with fast key','closed keyword with fast key','three','closed',NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(25,2,19,1,19,590,636,'closed keyword with extended fast key','closed keyword with extended fast key','four','closed',NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(26,2,19,1,21,636,682,'open keyword from empty-done-side line','open keyword from empty-done-side line','FIVE','open',NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(27,2,19,1,22,682,736,'another open keyword from empty-done-side line','another open keyword from empty-done-side line','SIX','open',NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(28,2,19,1,24,736,771,'open keyword from TYP_TODO','open keyword from TYP_TODO','seven','open',NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(29,2,19,1,25,771,809,'closed keyword from TYP_TODO','closed keyword from TYP_TODO','eight','closed',NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(30,2,19,1,27,809,843,'open keyword from SEQ_TODO','open keyword from SEQ_TODO','nine','open',NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(31,2,19,1,28,843,900,'closed keyword from SEQ_TODO','closed keyword from SEQ_TODO','ten','closed',NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(32,2,19,1,32,900,977,'closed keyword from later TODO line','closed keyword from later TODO line','eleven','closed',NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(33,2,19,1,36,977,1034,'open keyword from line defined later in file','open keyword from line defined later in file','late_open','open',NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(34,2,19,1,37,1034,1094,'closed keyword from line defined later in file','closed keyword from line defined later in file','late_done','closed',NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(35,2,19,1,39,1094,1141,'TODO still not valid after later local lines','TODO still not valid after later local lines',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(36,2,19,1,40,1141,1188,'DONE still not valid after later local lines','DONE still not valid after later local lines',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(37,3,NULL,0,1,-1,2276,'Keyword Parsing Fixture Later Title','Keyword Parsing Fixture Later Title',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(38,3,37,1,7,141,604,'First heading','First heading','WURST','open',NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(39,3,38,2,19,454,604,'Child heading','Child heading',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(40,3,37,1,26,604,798,'Second heading','Second heading','IDEA','open',NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(41,3,37,1,33,798,1264,'Boundary: keyword-looking body text','Boundary: keyword-looking body text',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(42,3,37,1,49,1264,2276,'Expected behavior','Expected behavior',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(43,4,NULL,0,1,-1,321,'Title can span multiple lines, even here','Title can span multiple lines, even here',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(44,4,43,1,7,136,321,'Unfortunately Everywhere','Unfortunately Everywhere',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(45,5,NULL,0,1,-1,1163,'Planning timestamp','Planning timestamp',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(46,5,45,1,4,49,1163,'Planning','Planning',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(47,5,46,2,6,62,111,'Simple scheduled','Simple scheduled',NULL,NULL,NULL,'<2024-11-20 Wed>',1732060800,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(48,5,46,2,9,111,158,'Simple deadline','Simple deadline',NULL,NULL,NULL,NULL,NULL,'<2024-12-01 Sun>',1733011200,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(49,5,46,2,12,158,201,'Simple closed','Simple closed',NULL,NULL,NULL,NULL,NULL,NULL,NULL,'[2024-11-21 Thu]',1732147200,0,0,'[]');
INSERT INTO headings VALUES(50,5,46,2,15,201,310,'All on one planning line','All on one planning line',NULL,NULL,NULL,'<2024-11-20 Wed>',1732060800,'<2024-12-01 Sun>',1733011200,'[2024-11-21 Thu]',1732147200,0,0,'[]');
INSERT INTO headings VALUES(51,5,46,2,18,310,358,'With time','With time',NULL,NULL,NULL,'<2024-11-20 Wed 09:30>',1732095000,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(52,5,46,2,21,358,422,'Time range same day','Time range same day',NULL,NULL,NULL,'<2024-11-20 Wed 09:30-11:00>',1732095000,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(53,5,46,2,24,422,482,'Date range','Date range',NULL,NULL,NULL,NULL,NULL,'<2024-12-01 Sun>--<2024-12-03 Tue>',1733011200,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(54,5,46,2,27,482,527,'Repeater','Repeater',NULL,NULL,NULL,'<2024-11-20 Wed +1w>',1732060800,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(55,5,46,2,30,527,582,'Diary expression','Diary expression',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(56,5,46,2,33,582,704,'Multiple same keyword','Multiple same keyword',NULL,NULL,NULL,'<2024-11-21 Thu>',1732147200,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(57,5,46,2,37,704,1163,'Not valid','Not valid',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(58,5,57,3,39,718,827,'Multiple planning lines','Multiple planning lines',NULL,NULL,NULL,'<2024-11-20 Wed>',1732060800,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(59,5,57,3,44,827,922,'Planning not immediately after headline','Planning not immediately after headline',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(60,5,57,3,48,922,1009,'Looks like planning in body','Looks like planning in body',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(61,5,57,3,51,1009,1078,'Lowercase should probably not count','Lowercase should probably not count',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(62,5,57,3,54,1078,1163,'Multiple same keyword','Multiple same keyword',NULL,NULL,NULL,'<2024-11-21 Thu>',1732147200,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(63,6,NULL,0,1,-1,2783,'Org Property and Keyword Test','Org Property and Keyword Test',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'["project","work"]');
INSERT INTO headings VALUES(64,6,63,1,16,465,638,'Task with multiple drawer properties','Task with multiple drawer properties',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'["project","work"]');
INSERT INTO headings VALUES(65,6,63,1,26,638,858,'Task with duplicate drawer properties','Task with duplicate drawer properties',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'["project","work"]');
INSERT INTO headings VALUES(66,6,63,1,35,858,1094,'Task with append operator in drawer','Task with append operator in drawer',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'["project","work"]');
INSERT INTO headings VALUES(67,6,63,1,45,1094,1304,'Task with mixed-case keys','Task with mixed-case keys',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'["project","work"]');
INSERT INTO headings VALUES(68,6,63,1,58,1304,1437,'Task with empty property accepted by Orgize','Task with empty property accepted by Orgize',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'["project","work"]');
INSERT INTO headings VALUES(69,6,63,1,67,1437,1718,'Task with Orgize empty-property limitation','Task with Orgize empty-property limitation',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'["project","work"]');
INSERT INTO headings VALUES(70,6,63,1,76,1718,1984,'Task after file-level property keywords','Task after file-level property keywords',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'["project","work"]');
INSERT INTO headings VALUES(71,6,63,1,83,1984,2264,'Task after later file-level keywords','Task after later file-level keywords',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'["project","work"]');
INSERT INTO headings VALUES(72,6,63,1,89,2264,2783,'Boundary: property-like but not properties','Boundary: property-like but not properties',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'["project","work"]');
INSERT INTO headings VALUES(73,7,NULL,0,1,-1,1307,'Tags and FILETAGS Fixture','Tags and FILETAGS Fixture',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'["file","project","later","extra"]');
INSERT INTO headings VALUES(74,7,73,1,5,82,242,'Parent','Parent',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'["file","project","later","extra","parent"]');
INSERT INTO headings VALUES(75,7,74,2,8,114,242,'Child','Child',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'["file","project","later","extra","parent","child"]');
INSERT INTO headings VALUES(76,7,75,3,11,144,242,'Grandchild','Grandchild',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'["file","project","later","extra","parent","child","grandchild"]');
INSERT INTO headings VALUES(77,7,73,1,14,242,267,'Sibling','Sibling',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'["file","project","later","extra"]');
INSERT INTO headings VALUES(78,7,73,1,17,267,370,'Duplicate Local','Duplicate Local',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'["file","project","later","extra","sibling"]');
INSERT INTO headings VALUES(79,7,73,1,22,370,455,'After Later FILETAGS','After Later FILETAGS',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'["file","project","later","extra","after"]');
INSERT INTO headings VALUES(80,7,73,1,25,455,894,'parent with one tag','parent with one tag',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'["file","project","later","extra","parent"]');
INSERT INTO headings VALUES(81,7,80,2,28,500,576,'child inherits parent tag','child inherits parent tag',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'["file","project","later","extra","parent"]');
INSERT INTO headings VALUES(82,7,80,2,31,576,894,'child with local tag','child with local tag',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'["file","project","later","extra","parent","child"]');
INSERT INTO headings VALUES(83,7,82,3,34,664,754,'grandchild inherits both','grandchild inherits both',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'["file","project","later","extra","parent","child"]');
INSERT INTO headings VALUES(84,7,82,3,37,754,894,'grandchild with duplicate local tag','grandchild with duplicate local tag',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'["file","project","later","extra","parent","child"]');
INSERT INTO headings VALUES(85,7,73,1,41,894,1163,'second parent','second parent',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'["file","project","later","extra","second"]');
INSERT INTO headings VALUES(86,7,85,2,44,976,1163,'second child with local tag','second child with local tag',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'["file","project","later","extra","second","child"]');
INSERT INTO headings VALUES(87,7,86,3,47,1062,1163,'second grandchild with extra tag','second grandchild with extra tag',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'["file","project","later","extra","second","child"]');
INSERT INTO headings VALUES(88,7,73,1,50,1163,1307,'untagged parent','untagged parent',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'["file","project","later","extra"]');
INSERT INTO headings VALUES(89,7,88,2,52,1182,1256,'child with only local tag','child with only local tag',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'["file","project","later","extra","local"]');
INSERT INTO headings VALUES(90,7,88,2,55,1256,1307,'child without tags','child without tags',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'["file","project","later","extra"]');
INSERT INTO headings VALUES(91,8,NULL,0,1,-1,1626,'timestamp-repeaters','timestamp-repeaters',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(92,8,91,1,1,0,180,'Repeater markers','Repeater markers',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(93,8,92,2,3,20,70,'Cumulate plus','Cumulate plus',NULL,NULL,NULL,'<2024-11-20 Wed +1w>',1732060800,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(94,8,92,2,6,70,126,'Catch up plus plus','Catch up plus plus',NULL,NULL,NULL,'<2024-11-20 Wed ++1m>',1732060800,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(95,8,92,2,9,126,180,'Restart dot plus','Restart dot plus',NULL,NULL,NULL,'<2024-11-20 Wed .+2d>',1732060800,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(96,8,91,1,12,180,448,'Repeater units','Repeater units',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(97,8,96,2,14,198,248,'Repeater hour','Repeater hour',NULL,NULL,NULL,'<2024-11-20 Wed +3h>',1732060800,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(98,8,96,2,17,248,297,'Repeater day','Repeater day',NULL,NULL,NULL,'<2024-11-20 Wed +3d>',1732060800,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(99,8,96,2,20,297,347,'Repeater week','Repeater week',NULL,NULL,NULL,'<2024-11-20 Wed +3w>',1732060800,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(100,8,96,2,23,347,398,'Repeater month','Repeater month',NULL,NULL,NULL,'<2024-11-20 Wed +3m>',1732060800,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(101,8,96,2,26,398,448,'Repeater year','Repeater year',NULL,NULL,NULL,'<2024-11-20 Wed +3y>',1732060800,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(102,8,91,1,29,448,677,'Repeater deadline part','Repeater deadline part',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(103,8,102,2,31,474,540,'Repeater with deadline day','Repeater with deadline day',NULL,NULL,NULL,'<2024-11-20 Wed +1w/2d>',1732060800,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(104,8,102,2,34,540,608,'Repeater with deadline week','Repeater with deadline week',NULL,NULL,NULL,'<2024-11-20 Wed ++1m/1w>',1732060800,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(105,8,102,2,37,608,677,'Repeater with deadline month','Repeater with deadline month',NULL,NULL,NULL,'<2024-11-20 Wed .+1y/2m>',1732060800,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(106,8,91,1,40,677,944,'Warning delays','Warning delays',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(107,8,106,2,42,695,742,'Warning all','Warning all',NULL,NULL,NULL,NULL,NULL,'<2024-12-01 Sun -5d>',1733011200,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(108,8,106,2,45,742,792,'Warning first','Warning first',NULL,NULL,NULL,NULL,NULL,'<2024-12-01 Sun --2w>',1733011200,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(109,8,106,2,48,792,846,'Warning hour','Warning hour',NULL,NULL,NULL,NULL,NULL,'<2024-12-01 Sun 09:30 -3h>',1733045400,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(110,8,106,2,51,846,895,'Warning month','Warning month',NULL,NULL,NULL,NULL,NULL,'<2024-12-01 Sun -1m>',1733011200,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(111,8,106,2,54,895,944,'Warning year','Warning year',NULL,NULL,NULL,NULL,NULL,'<2024-12-01 Sun --1y>',1733011200,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(112,8,91,1,57,944,1210,'Repeater and warning combinations','Repeater and warning combinations',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(113,8,112,2,59,981,1041,'Repeater and warning','Repeater and warning',NULL,NULL,NULL,NULL,NULL,'<2024-12-01 Sun +1w -5d>',1733011200,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(114,8,112,2,62,1041,1133,'Catch up repeater with deadline part and warning','Catch up repeater with deadline part and warning',NULL,NULL,NULL,NULL,NULL,'<2024-12-01 Sun ++1m/2d -5d>',1733011200,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(115,8,112,2,65,1133,1210,'Restart repeater with first warning','Restart repeater with first warning',NULL,NULL,NULL,NULL,NULL,'<2024-12-01 Sun .+2w --1w>',1733011200,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(116,8,91,1,68,1210,1453,'Time and range combinations','Time and range combinations',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(117,8,116,2,70,1241,1302,'Time with repeater','Time with repeater',NULL,NULL,NULL,'<2024-11-20 Wed 09:30 +1w>',1732095000,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(118,8,116,2,73,1302,1375,'Time range with repeater','Time range with repeater',NULL,NULL,NULL,'<2024-11-20 Wed 09:30-11:00 +1w>',1732095000,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(119,8,116,2,76,1375,1453,'Date range with repeater','Date range with repeater',NULL,NULL,NULL,NULL,NULL,'<2024-12-01 Sun>--<2024-12-03 Tue +1w>',1733011200,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(120,8,91,1,79,1453,1532,'Inactive timestamp with repeater','Inactive timestamp with repeater',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(121,8,120,2,81,1489,1532,'Inactive repeater','Inactive repeater',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(122,8,91,1,84,1532,1626,'Diary negative case','Diary negative case',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(123,8,122,2,86,1555,1626,'Diary with apparent repeater text','Diary with apparent repeater text',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(124,9,NULL,0,1,-1,299,'Timestamps','Timestamps',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(125,9,124,1,4,40,91,'Meet Peter at the movies','Meet Peter at the movies',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(126,9,124,1,7,91,152,'Discussion on climate change','Discussion on climate change',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(127,9,124,1,10,152,201,'My days off','My days off',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(128,9,124,1,14,201,299,'Can be anywhere','Can be anywhere',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
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
INSERT INTO timestamps VALUES(1,47,'scheduled',1732060800,NULL,'active','none','<2024-11-20 Wed>',93,109,7);
INSERT INTO timestamps VALUES(2,48,'deadline',1733011200,NULL,'active','none','<2024-12-01 Sun>',140,156,10);
INSERT INTO timestamps VALUES(3,49,'closed',1732147200,NULL,'inactive','none','[2024-11-21 Thu]',183,199,13);
INSERT INTO timestamps VALUES(4,50,'deadline',1733011200,NULL,'active','none','<2024-12-01 Sun>',239,255,16);
INSERT INTO timestamps VALUES(5,50,'scheduled',1732060800,NULL,'active','none','<2024-11-20 Wed>',267,283,16);
INSERT INTO timestamps VALUES(6,50,'closed',1732147200,NULL,'inactive','none','[2024-11-21 Thu]',292,308,16);
INSERT INTO timestamps VALUES(7,51,'scheduled',1732095000,NULL,'active','none','<2024-11-20 Wed 09:30>',334,356,19);
INSERT INTO timestamps VALUES(8,52,'scheduled',1732095000,1732100400,'active','time_range','<2024-11-20 Wed 09:30-11:00>',392,420,22);
INSERT INTO timestamps VALUES(9,53,'deadline',1733011200,1733184000,'active','date_range','<2024-12-01 Sun>--<2024-12-03 Tue>',446,480,25);
INSERT INTO timestamps VALUES(10,54,'scheduled',1732060800,NULL,'active','none','<2024-11-20 Wed +1w>',505,525,28);
INSERT INTO timestamps VALUES(11,55,'body',NULL,NULL,'diary','none','<%%(diary-float t 42)>',558,580,31);
INSERT INTO timestamps VALUES(12,56,'scheduled',1732060800,NULL,'active','none','<2024-11-20 Wed>',618,634,34);
INSERT INTO timestamps VALUES(13,56,'scheduled',1732147200,NULL,'active','none','<2024-11-21 Thu>',646,662,34);
INSERT INTO timestamps VALUES(14,58,'scheduled',1732060800,NULL,'active','none','<2024-11-20 Wed>',757,773,40);
INSERT INTO timestamps VALUES(15,58,'body',1733011200,NULL,'active','none','<2024-12-01 Sun>',784,800,41);
INSERT INTO timestamps VALUES(16,58,'body',1732147200,NULL,'inactive','none','[2024-11-21 Thu]',809,825,42);
INSERT INTO timestamps VALUES(17,59,'body',1732060800,NULL,'active','none','<2024-11-20 Wed>',904,920,46);
INSERT INTO timestamps VALUES(18,60,'body',1733011200,NULL,'active','none','<2024-12-01 Sun>',978,994,49);
INSERT INTO timestamps VALUES(19,61,'body',1732060800,NULL,'active','none','<2024-11-20 Wed>',1060,1076,52);
INSERT INTO timestamps VALUES(20,62,'scheduled',1732060800,NULL,'active','none','<2024-11-20 Wed>',1115,1131,55);
INSERT INTO timestamps VALUES(21,62,'scheduled',1732147200,NULL,'active','none','<2024-11-21 Thu>',1143,1159,55);
INSERT INTO timestamps VALUES(22,93,'scheduled',1732060800,NULL,'active','none','<2024-11-20 Wed +1w>',48,68,4);
INSERT INTO timestamps VALUES(23,94,'scheduled',1732060800,NULL,'active','none','<2024-11-20 Wed ++1m>',103,124,7);
INSERT INTO timestamps VALUES(24,95,'scheduled',1732060800,NULL,'active','none','<2024-11-20 Wed .+2d>',157,178,10);
INSERT INTO timestamps VALUES(25,97,'scheduled',1732060800,NULL,'active','none','<2024-11-20 Wed +3h>',226,246,15);
INSERT INTO timestamps VALUES(26,98,'scheduled',1732060800,NULL,'active','none','<2024-11-20 Wed +3d>',275,295,18);
INSERT INTO timestamps VALUES(27,99,'scheduled',1732060800,NULL,'active','none','<2024-11-20 Wed +3w>',325,345,21);
INSERT INTO timestamps VALUES(28,100,'scheduled',1732060800,NULL,'active','none','<2024-11-20 Wed +3m>',376,396,24);
INSERT INTO timestamps VALUES(29,101,'scheduled',1732060800,NULL,'active','none','<2024-11-20 Wed +3y>',426,446,27);
INSERT INTO timestamps VALUES(30,103,'scheduled',1732060800,NULL,'active','none','<2024-11-20 Wed +1w/2d>',515,538,32);
INSERT INTO timestamps VALUES(31,104,'scheduled',1732060800,NULL,'active','none','<2024-11-20 Wed ++1m/1w>',582,606,35);
INSERT INTO timestamps VALUES(32,105,'scheduled',1732060800,NULL,'active','none','<2024-11-20 Wed .+1y/2m>',651,675,38);
INSERT INTO timestamps VALUES(33,107,'deadline',1733011200,NULL,'active','none','<2024-12-01 Sun -5d>',720,740,43);
INSERT INTO timestamps VALUES(34,108,'deadline',1733011200,NULL,'active','none','<2024-12-01 Sun --2w>',769,790,46);
INSERT INTO timestamps VALUES(35,109,'deadline',1733045400,NULL,'active','none','<2024-12-01 Sun 09:30 -3h>',818,844,49);
INSERT INTO timestamps VALUES(36,110,'deadline',1733011200,NULL,'active','none','<2024-12-01 Sun -1m>',873,893,52);
INSERT INTO timestamps VALUES(37,111,'deadline',1733011200,NULL,'active','none','<2024-12-01 Sun --1y>',921,942,55);
INSERT INTO timestamps VALUES(38,113,'deadline',1733011200,NULL,'active','none','<2024-12-01 Sun +1w -5d>',1015,1039,60);
INSERT INTO timestamps VALUES(39,114,'deadline',1733011200,NULL,'active','none','<2024-12-01 Sun ++1m/2d -5d>',1103,1131,63);
INSERT INTO timestamps VALUES(40,115,'deadline',1733011200,NULL,'active','none','<2024-12-01 Sun .+2w --1w>',1182,1208,66);
INSERT INTO timestamps VALUES(41,117,'scheduled',1732095000,NULL,'active','none','<2024-11-20 Wed 09:30 +1w>',1274,1300,71);
INSERT INTO timestamps VALUES(42,118,'scheduled',1732095000,1732100400,'active','time_range','<2024-11-20 Wed 09:30-11:00 +1w>',1341,1373,74);
INSERT INTO timestamps VALUES(43,119,'deadline',1733011200,1733184000,'active','date_range','<2024-12-01 Sun>--<2024-12-03 Tue +1w>',1413,1451,77);
INSERT INTO timestamps VALUES(44,121,'body',1732060800,NULL,'inactive','none','[2024-11-20 Wed +1w]',1510,1530,82);
INSERT INTO timestamps VALUES(45,123,'body',NULL,NULL,'diary','none','<%%(diary-float t 42)>',1603,1625,87);
INSERT INTO timestamps VALUES(46,125,'body',1162408500,NULL,'active','none','<2006-11-01 Wed 19:15>',67,89,5);
INSERT INTO timestamps VALUES(47,126,'body',1162461600,1162468800,'active','time_range','<2006-11-02 Thu 10:00-12:00>',122,150,8);
INSERT INTO timestamps VALUES(48,127,'body',1162512000,NULL,'active','none','<2006-11-03 Fri>',166,182,11);
INSERT INTO timestamps VALUES(49,127,'body',1162771200,NULL,'active','none','<2006-11-06 Mon>',183,199,12);
INSERT INTO timestamps VALUES(50,128,'body',1162512000,NULL,'active','none','<2006-11-03 Fri>',237,253,16);
INSERT INTO timestamps VALUES(51,128,'body',1782086400,NULL,'inactive','none','[2026-06-22 Mon]',281,297,18);
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
INSERT INTO keywords VALUES(1,1,'TITLE','Bracket Link Fixture',1);
INSERT INTO keywords VALUES(2,1,'STARTUP','content',2);
INSERT INTO keywords VALUES(3,19,'TITLE','File-local TODO keywords',1);
INSERT INTO keywords VALUES(4,19,'STARTUP','showall',2);
INSERT INTO keywords VALUES(5,19,'TODO','one(t) two(n) | three(d) four(w@)',3);
INSERT INTO keywords VALUES(6,19,'TODO','FIVE SIX |',4);
INSERT INTO keywords VALUES(7,19,'TYP_TODO','seven | eight',5);
INSERT INTO keywords VALUES(8,19,'SEQ_TODO','nine | ten',6);
INSERT INTO keywords VALUES(9,19,'TODO','| eleven(c)',30);
INSERT INTO keywords VALUES(10,19,'TODO','late_open | late_done',34);
INSERT INTO keywords VALUES(11,37,'TITLE','Keyword Parsing Fixture',1);
INSERT INTO keywords VALUES(12,37,'STARTUP','showall',2);
INSERT INTO keywords VALUES(13,37,'AUTHOR','First Author',3);
INSERT INTO keywords VALUES(14,37,'PROPERTY','before_prop before-value',4);
INSERT INTO keywords VALUES(15,37,'CATEGORY','before-category',5);
INSERT INTO keywords VALUES(16,37,'AUTHOR','Later Author',10);
INSERT INTO keywords VALUES(17,37,'OPTIONS','toc:nil num:t',11);
INSERT INTO keywords VALUES(18,37,'PROPERTY','after_prop after-value',12);
INSERT INTO keywords VALUES(19,37,'PROPERTY','repeated_prop first',13);
INSERT INTO keywords VALUES(20,37,'PROPERTY','repeated_prop second',14);
INSERT INTO keywords VALUES(21,37,'PROPERTY','appended_prop base',15);
INSERT INTO keywords VALUES(22,37,'PROPERTY','appended_prop+ extra',16);
INSERT INTO keywords VALUES(23,37,'CATEGORY','after-category',17);
INSERT INTO keywords VALUES(24,37,'TITLE','Later Title',22);
INSERT INTO keywords VALUES(25,37,'EXPORT_FILE_NAME','later-export-name',23);
INSERT INTO keywords VALUES(26,37,'STARTUP','content',24);
INSERT INTO keywords VALUES(27,37,'TODO','TODO NEXT | DONE CANCELED',29);
INSERT INTO keywords VALUES(28,37,'SEQ_TODO','IDEA(i) WURST(w) PLAN(p) BUILD(b) | DONE(d)',30);
INSERT INTO keywords VALUES(29,37,'TYP_TODO','WAITING(w) | CANCELED(c)',31);
INSERT INTO keywords VALUES(30,43,'TITLE','Title can span',1);
INSERT INTO keywords VALUES(31,43,'TITLE','multiple lines,',2);
INSERT INTO keywords VALUES(32,43,'AUTHOR','Hubisan',3);
INSERT INTO keywords VALUES(33,43,'TITLE','even here',9);
INSERT INTO keywords VALUES(34,45,'TITLE','Planning timestamp',1);
INSERT INTO keywords VALUES(35,45,'STARTUP','showall',2);
INSERT INTO keywords VALUES(36,63,'TITLE','Org Property and Keyword Test',6);
INSERT INTO keywords VALUES(37,63,'STARTUP','showall',7);
INSERT INTO keywords VALUES(38,63,'CATEGORY','category_keyword_value',8);
INSERT INTO keywords VALUES(39,63,'PROPERTY','Effort_ALL 0:10 0:30 1:00',9);
INSERT INTO keywords VALUES(40,63,'PROPERTY','keyword_property valid',10);
INSERT INTO keywords VALUES(41,63,'PROPERTY','keyword_overwritten_by_second invalid',11);
INSERT INTO keywords VALUES(42,63,'PROPERTY','keyword_overwritten_by_second valid',12);
INSERT INTO keywords VALUES(43,63,'PROPERTY','keyword_append foo=1',13);
INSERT INTO keywords VALUES(44,63,'PROPERTY','keyword_append+ bar=2',14);
INSERT INTO keywords VALUES(45,63,'PROPERTY','later_keyword_property works_everywhere',80);
INSERT INTO keywords VALUES(46,63,'CATEGORY','later_category_keyword',81);
INSERT INTO keywords VALUES(47,63,'FILETAGS',':project:work:',90);
INSERT INTO keywords VALUES(48,63,'TAGS','work(w) home(h)',91);
INSERT INTO keywords VALUES(49,63,'COLUMNS','%TODO %50ITEM %Effort{:} %CLOCKSUM',92);
INSERT INTO keywords VALUES(50,63,'CONSTANTS','c=299792458',93);
INSERT INTO keywords VALUES(51,63,'AUTHOR','Jane Doe',94);
INSERT INTO keywords VALUES(52,63,'OPTIONS','toc:nil num:t',95);
INSERT INTO keywords VALUES(53,73,'TITLE','Tags and FILETAGS Fixture',1);
INSERT INTO keywords VALUES(54,73,'STARTUP','showall',2);
INSERT INTO keywords VALUES(55,73,'FILETAGS',':file:project:',3);
INSERT INTO keywords VALUES(56,73,'FILETAGS',':later:extra:',20);
INSERT INTO keywords VALUES(57,124,'TITLE','Timestamps',1);
INSERT INTO keywords VALUES(58,124,'STARTUP','showall',2);
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
INSERT INTO properties VALUES(1,2,'CUSTOM_ID','internal-link-to-custom-id','property_drawer',0,12);
INSERT INTO properties VALUES(2,37,'BEFORE_PROP','before-value','property_keyword',0,4);
INSERT INTO properties VALUES(3,37,'CATEGORY','before-category','category_keyword',0,5);
INSERT INTO properties VALUES(4,37,'AFTER_PROP','after-value','property_keyword',0,12);
INSERT INTO properties VALUES(5,37,'REPEATED_PROP','first','property_keyword',0,13);
INSERT INTO properties VALUES(6,37,'REPEATED_PROP','second','property_keyword',0,14);
INSERT INTO properties VALUES(7,37,'APPENDED_PROP','base','property_keyword',0,15);
INSERT INTO properties VALUES(8,37,'APPENDED_PROP','extra','property_keyword',1,16);
INSERT INTO properties VALUES(9,37,'CATEGORY','after-category','category_keyword',0,17);
INSERT INTO properties VALUES(10,63,'CATEGORY','Level 0 Category Property','property_drawer',0,2);
INSERT INTO properties VALUES(11,63,'WHATEVER','level 0 drawer property','property_drawer',0,3);
INSERT INTO properties VALUES(12,63,'ID','7dad9b62-a3cc-43ec-a60f-e650bdaeae6d','property_drawer',0,4);
INSERT INTO properties VALUES(13,63,'CATEGORY','category_keyword_value','category_keyword',0,8);
INSERT INTO properties VALUES(14,63,'EFFORT_ALL','0:10 0:30 1:00','property_keyword',0,9);
INSERT INTO properties VALUES(15,63,'KEYWORD_PROPERTY','valid','property_keyword',0,10);
INSERT INTO properties VALUES(16,63,'KEYWORD_OVERWRITTEN_BY_SECOND','invalid','property_keyword',0,11);
INSERT INTO properties VALUES(17,63,'KEYWORD_OVERWRITTEN_BY_SECOND','valid','property_keyword',0,12);
INSERT INTO properties VALUES(18,63,'KEYWORD_APPEND','foo=1','property_keyword',0,13);
INSERT INTO properties VALUES(19,63,'KEYWORD_APPEND','bar=2','property_keyword',1,14);
INSERT INTO properties VALUES(20,63,'LATER_KEYWORD_PROPERTY','works_everywhere','property_keyword',0,80);
INSERT INTO properties VALUES(21,63,'CATEGORY','later_category_keyword','category_keyword',0,81);
INSERT INTO properties VALUES(22,64,'ID','abc','property_drawer',0,18);
INSERT INTO properties VALUES(23,64,'CUSTOM_ID','task-custom-id','property_drawer',0,19);
INSERT INTO properties VALUES(24,64,'EFFORT','0:30','property_drawer',0,20);
INSERT INTO properties VALUES(25,64,'OWNER','Alice','property_drawer',0,21);
INSERT INTO properties VALUES(26,64,'DRAWER_PROP','valid','property_drawer',0,22);
INSERT INTO properties VALUES(27,65,'DEFINED_TWICE','invalid','property_drawer',0,28);
INSERT INTO properties VALUES(28,65,'DEFINED_TWICE','valid','property_drawer',0,29);
INSERT INTO properties VALUES(29,66,'ADD-VALUE','is','property_drawer',0,37);
INSERT INTO properties VALUES(30,66,'ADD-VALUE','valid','property_drawer',1,38);
INSERT INTO properties VALUES(31,67,'ID','lowercase-id','property_drawer',0,47);
INSERT INTO properties VALUES(32,67,'CUSTOM_ID','mixed-case-custom-id','property_drawer',0,48);
INSERT INTO properties VALUES(33,67,'DRAWER_PROP','valid','property_drawer',0,49);
INSERT INTO properties VALUES(34,67,'ADD-VALUE','appended','property_drawer',1,50);
INSERT INTO properties VALUES(35,68,'EMPTY','','property_drawer',0,60);
CREATE TABLE tags (
    heading_id      INTEGER NOT NULL,
    tag             TEXT NOT NULL,
    FOREIGN KEY (heading_id)
        REFERENCES headings(id)
        ON DELETE CASCADE,
    PRIMARY KEY (heading_id, tag)
);
INSERT INTO tags VALUES(18,'foo');
INSERT INTO tags VALUES(18,'bar');
INSERT INTO tags VALUES(63,'project');
INSERT INTO tags VALUES(63,'work');
INSERT INTO tags VALUES(73,'file');
INSERT INTO tags VALUES(73,'project');
INSERT INTO tags VALUES(73,'later');
INSERT INTO tags VALUES(73,'extra');
INSERT INTO tags VALUES(74,'parent');
INSERT INTO tags VALUES(75,'child');
INSERT INTO tags VALUES(76,'project');
INSERT INTO tags VALUES(76,'grandchild');
INSERT INTO tags VALUES(78,'file');
INSERT INTO tags VALUES(78,'sibling');
INSERT INTO tags VALUES(79,'after');
INSERT INTO tags VALUES(80,'parent');
INSERT INTO tags VALUES(82,'child');
INSERT INTO tags VALUES(84,'parent');
INSERT INTO tags VALUES(85,'second');
INSERT INTO tags VALUES(86,'child');
INSERT INTO tags VALUES(87,'extra');
INSERT INTO tags VALUES(89,'local');
CREATE TABLE heading_bodies (
    heading_id          INTEGER PRIMARY KEY,
    body_text           TEXT NOT NULL,
    body_byte_start     INTEGER,
    body_byte_end       INTEGER,
    FOREIGN KEY (heading_id)
        REFERENCES headings(id)
        ON DELETE CASCADE
);
INSERT INTO heading_bodies VALUES(1,unistr('# Links before the first real heading should attach to synthetic root.\u000a[[FILE:root-notes.org::42]]\u000a[[root target][root description]]\u000ahttps://example.org/root-plain-should-not-be-stored\u000a<https://example.org/root-angle-should-not-be-stored>'),50,288);
INSERT INTO heading_bodies VALUES(3,unistr('- [[#internal-link-to-custom-id]]\u000a- [[#internal-link-to-custom-id][description: internal-link-to-custom-id]]'),393,501);
INSERT INTO heading_bodies VALUES(4,unistr('- [[*Internal bracket links]]\u000a- [[*Internal bracket links][description: heading link]]'),526,612);
INSERT INTO heading_bodies VALUES(5,unistr('<<dedicated target>>\u000a- [[dedicated target]]\u000a- [[dedicated target][description: dedicated target]]'),640,737);
INSERT INTO heading_bodies VALUES(6,unistr('#+NAME: named target\u000a- [[named target]]\u000a- [[named target][description: named target]]'),773,858);
INSERT INTO heading_bodies VALUES(7,unistr('- [[no matching target]]\u000a- [[no matching target][description: no matching target]]\u000a- [[notes.org]]'),878,976);
INSERT INTO heading_bodies VALUES(9,unistr('- [[file:/etc]]\u000a- [[file:/etc][description: /etc]]\u000a- [[file:/etc/]]\u000a- [[file:/etc/host.conf]]\u000a- [[file:../]]\u000a- [[file:../../sql]]\u000a- [[file:../../sql/]]\u000a- [[file:../parser_test.rs]]\u000a- [[file:./org-test-links.org]]\u000a- [[file:~/.emacs.d]]\u000a- [[file:~/.emacs.d/]]\u000a- [[file:~/.emacs.d/init.el]]'),1028,1315);
INSERT INTO heading_bodies VALUES(10,unistr('- [[file+sys:/etc]]\u000a- [[file+emacs:/etc]]\u000a- [[file+sys:/etc][description: file+sys:/etc]]\u000a- [[file+emacs:/etc][description: file+emacs:/etc]]'),1343,1484);
INSERT INTO heading_bodies VALUES(11,unistr('- [[/etc]]\u000a- [[/etc/]]\u000a- [[/etc/host.conf]]\u000a- [[../]]\u000a- [[../../sql]]\u000a- [[../../sql/]]\u000a- [[../parser_test.rs]]\u000a- [[./org-test-links.org]]\u000a- [[~/memento]]'),1509,1662);
INSERT INTO heading_bodies VALUES(12,unistr('- [[file:./org-test-links.org::10]]\u000a- [[file:./org-test-links.org::#internal-link-to-custom-id]]\u000a- [[file:./org-test-links.org::dedicated target]]\u000a- [[file:./org-test-links.org::*Internal bracket links]]\u000a- [[file:./org-test-links.org::/*.File-like.*/]]\u000a- [[./org-test-links.org::10]]\u000a- [[./org-test-links.org::#internal-link-to-custom-id]]\u000a- [[./org-test-links.org::dedicated target]]\u000a- [[./org-test-links.org::*Internal bracket links]]\u000a- [[./org-test-links.org::/*.File-like.*/]]\u000a- [[file:::10]]\u000a- [[file:::*Internal bracket links]]'),1687,2220);
INSERT INTO heading_bodies VALUES(14,unistr('- [[http://orgmode.org]]\u000a- [[http://orgmode.org][description: http://orgmode.org]]\u000a- [[https://orgmode.org]]\u000a- [[https://orgmode.org][description: https://orgmode.org]]\u000a- [[news:comp.emacs]]\u000a- [[mailto:emacs-orgmode@gnu.org]]\u000a- [[help:org-store-link]]\u000a- [[info:org#External Link]]\u000a- [[shell:ls *.org  ]]\u000a- [[elisp:(find-file "~/.emacs.d/init.el")]]'),2269,2617);
INSERT INTO heading_bodies VALUES(15,unistr('- [[unknown:foo]]\u000a- [[unknown:foo][description: unknown type]]\u000a- [[customlink:test]]\u000a- [[customlink:test][description: customlink:test]]\u000a- [[doi:10.1000/182]]\u000a- [[irc:/irc.com/#emacs/bob]]'),2653,2841);
INSERT INTO heading_bodies VALUES(16,unistr('These should not be stored by the current bracket-link storage task.\u000a\u000a- http://orgmode.org\u000a- https://orgmode.org\u000a- mailto:emacs-orgmode@gnu.org\u000a- file:./org-test-links.org::10\u000a- <file:::*Negative non-bracket examples>\u000a- <shell:ls *.org>\u000a- <https://orgmode.org/ spaces >'),2876,3145);
INSERT INTO heading_bodies VALUES(17,'Text with: äöü ÄÖÜ 😀😇🤖',3172,3209);
INSERT INTO heading_bodies VALUES(18,unistr('Some body text with bracket links:\u000a\u000a- [[file:sub/äöü.txt::target][A file link with Umlaut]]\u000a- [[https://orgmode.org/🔥]]\u000a- [[https://orgmode.org/🔥][Emoji description 😀]]\u000a- [[dedicated target äöü]]'),3292,3502);
INSERT INTO heading_bodies VALUES(19,'See [[file:../../notes/org-semantics/file-local-todo-keywords.org]]',164,231);
INSERT INTO heading_bodies VALUES(20,'Default TODO is not valid because file-local TODO lines override defaults.',278,352);
INSERT INTO heading_bodies VALUES(21,'Default DONE is not valid because file-local TODO lines override defaults.',403,477);
INSERT INTO heading_bodies VALUES(38,'This heading has body text before later keywords.',163,212);
INSERT INTO heading_bodies VALUES(39,'This child should not directly receive keyword rows.',471,523);
INSERT INTO heading_bodies VALUES(40,'This heading appears after later keywords.',626,668);
INSERT INTO heading_bodies VALUES(41,unistr('This line mentions #+TITLE: Inline Mention but should only become a keyword row if Orgize exposes it as a keyword node.\u000aThis line mentions #+PROPERTY: inline_prop invalid in prose.\u000a\u000a#+BEGIN_EXAMPLE\u000a#+TITLE: Example Block Title\u000a#+PROPERTY: example_prop invalid\u000a#+CATEGORY: example-category\u000a#+END_EXAMPLE\u000a\u000a#+begin_src org\u000a  ,#+TITLE: Source Block Title\u000a  ,#+PROPERTY: source_prop invalid\u000a  ,#+CATEGORY: source-category\u000a#+end_src'),836,1262);
INSERT INTO heading_bodies VALUES(42,unistr('- All real keyword nodes exposed by Orgize are stored as raw ~keywords~ rows attached to the level 0 heading.\u000a- Keyword rows are not attached to regular headings.\u000a- Duplicate keyword rows are preserved.\u000a- Source order is preserved with ~line_number~ and/or insertion order.\u000a- Generic keywords such as ~TITLE~, ~AUTHOR~, ~STARTUP~, ~OPTIONS~, and ~EXPORT_FILE_NAME~ remain raw keyword rows only.\u000a- ~TODO~, ~SEQ_TODO~, and ~TYP_TODO~ may additionally create normalized ~todo_keywords~ rows if that normalization is in scope.\u000a- ~PROPERTY~ rows may additionally create normalized ~properties~ rows with ~source = property_keyword~ if that normalization is in scope.\u000a- ~CATEGORY~ rows may additionally create normalized ~properties~ rows with ~source = category_keyword~ if that normalization is in scope.\u000a- Keywords inside example/source blocks must not create keyword rows unless Orgize incorrectly exposes them as keyword nodes; if that happens, document the Orgize behavior as a parser risk.'),1285,2275);
INSERT INTO heading_bodies VALUES(43,'See [[file:../../notes/org-semantics/multipe-title-keywords.org]]',69,134);
INSERT INTO heading_bodies VALUES(44,unistr('This can be proven by using ~org-latex-export-as-latex~:\u000a\u000a#+BEGIN_SRC latex\u000a  \\title{Title can span multiple lines, even here}\u000a#+END_SRC'),NULL,NULL);
INSERT INTO heading_bodies VALUES(55,'SCHEDULED: <%%(diary-float t 42)>',547,580);
INSERT INTO heading_bodies VALUES(56,'In that case Org uses the second entry.',663,702);
INSERT INTO heading_bodies VALUES(58,unistr('DEADLINE: <2024-12-01 Sun>\u000aCLOSED: [2024-11-21 Thu]'),774,825);
INSERT INTO heading_bodies VALUES(59,unistr('Some body text first.\u000aSCHEDULED: <2024-11-20 Wed>'),871,920);
INSERT INTO heading_bodies VALUES(60,'This mentions DEADLINE: <2024-12-01 Sun> inside text.',954,1007);
INSERT INTO heading_bodies VALUES(61,'scheduled: <2024-11-20 Wed>',1049,1076);
INSERT INTO heading_bodies VALUES(64,'Body text for the first task.',607,636);
INSERT INTO heading_bodies VALUES(65,unistr('Expected raw/direct storage:\u000a- both DEFINED_TWICE rows should be preserved\u000a- no overwrite should be computed here'),743,856);
INSERT INTO heading_bodies VALUES(66,unistr('Expected raw/direct storage:\u000a- ADD-VALUE = is, append = 0\u000a- ADD-VALUE = valid, append = 1\u000a- final value "is valid" is not computed in this task'),949,1092);
INSERT INTO heading_bodies VALUES(67,unistr('Expected normalized keys:\u000a- ID\u000a- CUSTOM_ID\u000a- DRAWER_PROP\u000a- ADD-VALUE'),1234,1302);
INSERT INTO heading_bodies VALUES(68,unistr('Expected:\u000a- key EMPTY\u000a- value ""\u000a- source property_drawer'),1378,1435);
INSERT INTO heading_bodies VALUES(69,unistr(':PROPERTIES:\u000a:EMPTY:\u000a:END:\u000aExpected for now:\u000a- Orgize may expose this as a generic drawer, not PROPERTY_DRAWER\u000a- parser should not add fallback parsing in this task\u000a- no property row is expected if Orgize does not expose NODE_PROPERTY'),1482,1716);
INSERT INTO heading_bodies VALUES(70,unistr('This heading should not directly receive file-level #+PROPERTY or #+CATEGORY rows.\u000aThose belong to the synthetic level 0 heading only.'),1760,1894);
INSERT INTO heading_bodies VALUES(71,unistr('This heading still should not directly receive those keyword properties.\u000aThey should be stored on level 0 as:\u000a- LATER_KEYWORD_PROPERTY = works_everywhere, source property_keyword\u000a- CATEGORY = later_category_keyword, source category_keyword'),2023,2262);
INSERT INTO heading_bodies VALUES(72,unistr('These keyword lines should not create property rows in this task.\u000aFILETAGS belongs to the later tags task.\u000aTAGS may later become tag-definition metadata.\u000aCOLUMNS references properties but does not define property values.\u000aCONSTANTS belongs to table/formula semantics.\u000aAUTHOR and OPTIONS remain raw keywords.'),2476,2782);
INSERT INTO heading_bodies VALUES(74,'Parent body.',100,112);
INSERT INTO heading_bodies VALUES(75,'Child body.',131,142);
INSERT INTO heading_bodies VALUES(76,'Grandchild repeats one FILETAG locally and adds a local tag.',180,240);
INSERT INTO heading_bodies VALUES(77,'Sibling body.',252,265);
INSERT INTO heading_bodies VALUES(78,'This heading repeats one FILETAG locally.',300,341);
INSERT INTO heading_bodies VALUES(79,'This heading appears after a later FILETAGS keyword.',401,453);
INSERT INTO heading_bodies VALUES(80,'Parent body.',486,498);
INSERT INTO heading_bodies VALUES(81,'Child should have all_tags_json = ["parent"].',529,574);
INSERT INTO heading_bodies VALUES(82,'Child should have all_tags_json = ["parent", "child"].',608,662);
INSERT INTO heading_bodies VALUES(83,'Grandchild should have all_tags_json = ["parent", "child"].',693,752);
INSERT INTO heading_bodies VALUES(84,unistr('Duplicate local tag should not be repeated.\u000aExpected all_tags_json = ["parent", "child"].'),803,892);
INSERT INTO heading_bodies VALUES(85,'Second parent starts a separate tag inheritance branch.',919,974);
INSERT INTO heading_bodies VALUES(86,'Expected all_tags_json = ["second", "child"].',1015,1060);
INSERT INTO heading_bodies VALUES(87,'Expected all_tags_json = ["second", "child", "extra"].',1107,1161);
INSERT INTO heading_bodies VALUES(89,'Expected all_tags_json = ["local"].',1219,1254);
INSERT INTO heading_bodies VALUES(90,'Expected all_tags_json = [].',1278,1306);
INSERT INTO heading_bodies VALUES(103,'SCHEDULED: <2024-11-20 Wed +1w/2d>',504,538);
INSERT INTO heading_bodies VALUES(104,'SCHEDULED: <2024-11-20 Wed ++1m/1w>',571,606);
INSERT INTO heading_bodies VALUES(105,'SCHEDULED: <2024-11-20 Wed .+1y/2m>',640,675);
INSERT INTO heading_bodies VALUES(114,'DEADLINE: <2024-12-01 Sun ++1m/2d -5d>',1093,1131);
INSERT INTO heading_bodies VALUES(121,'[2024-11-20 Wed +1w]',1510,1530);
INSERT INTO heading_bodies VALUES(123,'SCHEDULED: <%%(diary-float t 42)>',1592,1625);
INSERT INTO heading_bodies VALUES(125,'<2006-11-01 Wed 19:15>',67,89);
INSERT INTO heading_bodies VALUES(126,'<2006-11-02 Thu 10:00-12:00>',122,150);
INSERT INTO heading_bodies VALUES(127,unistr('<2006-11-03 Fri>\u000a<2006-11-06 Mon>'),166,199);
INSERT INTO heading_bodies VALUES(128,unistr('Also in the body <2006-11-03 Fri>.\u000a\u000aThis is an inactive one: [2026-06-22 Mon].'),220,298);
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
INSERT INTO outline_path VALUES(1,1,NULL,0,'0000','["Bracket Link Fixture"]');
INSERT INTO outline_path VALUES(2,1,1,1,'0000.0001','["Bracket Link Fixture","Internal bracket links"]');
INSERT INTO outline_path VALUES(3,1,2,2,'0000.0001.0001','["Bracket Link Fixture","Internal bracket links","Custom ID links"]');
INSERT INTO outline_path VALUES(4,1,2,2,'0000.0001.0002','["Bracket Link Fixture","Internal bracket links","Fuzzy heading links"]');
INSERT INTO outline_path VALUES(5,1,2,2,'0000.0001.0003','["Bracket Link Fixture","Internal bracket links","Dedicated target links"]');
INSERT INTO outline_path VALUES(6,1,2,2,'0000.0001.0004','["Bracket Link Fixture","Internal bracket links","Named target style fuzzy links"]');
INSERT INTO outline_path VALUES(7,1,2,2,'0000.0001.0005','["Bracket Link Fixture","Internal bracket links","Fuzzy fallback"]');
INSERT INTO outline_path VALUES(8,1,1,1,'0000.0002','["Bracket Link Fixture","File-like bracket links"]');
INSERT INTO outline_path VALUES(9,1,8,2,'0000.0002.0001','["Bracket Link Fixture","File-like bracket links","Explicit file links"]');
INSERT INTO outline_path VALUES(10,1,8,2,'0000.0002.0002','["Bracket Link Fixture","File-like bracket links","Explicit file variants"]');
INSERT INTO outline_path VALUES(11,1,8,2,'0000.0002.0003','["Bracket Link Fixture","File-like bracket links","Implicit file links"]');
INSERT INTO outline_path VALUES(12,1,8,2,'0000.0002.0004','["Bracket Link Fixture","File-like bracket links","File search options"]');
INSERT INTO outline_path VALUES(13,1,1,1,'0000.0003','["Bracket Link Fixture","Typed bracket links"]');
INSERT INTO outline_path VALUES(14,1,13,2,'0000.0003.0001','["Bracket Link Fixture","Typed bracket links","Built-in typed links"]');
INSERT INTO outline_path VALUES(15,1,13,2,'0000.0003.0002','["Bracket Link Fixture","Typed bracket links","Unknown and custom typed links"]');
INSERT INTO outline_path VALUES(16,1,1,1,'0000.0004','["Bracket Link Fixture","Negative non-bracket examples"]');
INSERT INTO outline_path VALUES(17,1,1,1,'0000.0005','["Bracket Link Fixture","Unicode bracket links"]');
INSERT INTO outline_path VALUES(18,1,17,2,'0000.0005.0001','["Bracket Link Fixture","Unicode bracket links","Übung 🚀 Ein Titel mit Umlaut und Emoji"]');
INSERT INTO outline_path VALUES(19,2,NULL,0,'0000','["File-local TODO keywords"]');
INSERT INTO outline_path VALUES(20,2,19,1,'0000.0001','["File-local TODO keywords","TODO default keyword should stay in title"]');
INSERT INTO outline_path VALUES(21,2,19,1,'0000.0002','["File-local TODO keywords","DONE default done keyword should stay in title"]');
INSERT INTO outline_path VALUES(22,2,19,1,'0000.0003','["File-local TODO keywords","open keyword with fast key"]');
INSERT INTO outline_path VALUES(23,2,19,1,'0000.0004','["File-local TODO keywords","another open keyword with fast key"]');
INSERT INTO outline_path VALUES(24,2,19,1,'0000.0005','["File-local TODO keywords","closed keyword with fast key"]');
INSERT INTO outline_path VALUES(25,2,19,1,'0000.0006','["File-local TODO keywords","closed keyword with extended fast key"]');
INSERT INTO outline_path VALUES(26,2,19,1,'0000.0007','["File-local TODO keywords","open keyword from empty-done-side line"]');
INSERT INTO outline_path VALUES(27,2,19,1,'0000.0008','["File-local TODO keywords","another open keyword from empty-done-side line"]');
INSERT INTO outline_path VALUES(28,2,19,1,'0000.0009','["File-local TODO keywords","open keyword from TYP_TODO"]');
INSERT INTO outline_path VALUES(29,2,19,1,'0000.0010','["File-local TODO keywords","closed keyword from TYP_TODO"]');
INSERT INTO outline_path VALUES(30,2,19,1,'0000.0011','["File-local TODO keywords","open keyword from SEQ_TODO"]');
INSERT INTO outline_path VALUES(31,2,19,1,'0000.0012','["File-local TODO keywords","closed keyword from SEQ_TODO"]');
INSERT INTO outline_path VALUES(32,2,19,1,'0000.0013','["File-local TODO keywords","closed keyword from later TODO line"]');
INSERT INTO outline_path VALUES(33,2,19,1,'0000.0014','["File-local TODO keywords","open keyword from line defined later in file"]');
INSERT INTO outline_path VALUES(34,2,19,1,'0000.0015','["File-local TODO keywords","closed keyword from line defined later in file"]');
INSERT INTO outline_path VALUES(35,2,19,1,'0000.0016','["File-local TODO keywords","TODO still not valid after later local lines"]');
INSERT INTO outline_path VALUES(36,2,19,1,'0000.0017','["File-local TODO keywords","DONE still not valid after later local lines"]');
INSERT INTO outline_path VALUES(37,3,NULL,0,'0000','["Keyword Parsing Fixture Later Title"]');
INSERT INTO outline_path VALUES(38,3,37,1,'0000.0001','["Keyword Parsing Fixture Later Title","First heading"]');
INSERT INTO outline_path VALUES(39,3,38,2,'0000.0001.0001','["Keyword Parsing Fixture Later Title","First heading","Child heading"]');
INSERT INTO outline_path VALUES(40,3,37,1,'0000.0002','["Keyword Parsing Fixture Later Title","Second heading"]');
INSERT INTO outline_path VALUES(41,3,37,1,'0000.0003','["Keyword Parsing Fixture Later Title","Boundary: keyword-looking body text"]');
INSERT INTO outline_path VALUES(42,3,37,1,'0000.0004','["Keyword Parsing Fixture Later Title","Expected behavior"]');
INSERT INTO outline_path VALUES(43,4,NULL,0,'0000','["Title can span multiple lines, even here"]');
INSERT INTO outline_path VALUES(44,4,43,1,'0000.0001','["Title can span multiple lines, even here","Unfortunately Everywhere"]');
INSERT INTO outline_path VALUES(45,5,NULL,0,'0000','["Planning timestamp"]');
INSERT INTO outline_path VALUES(46,5,45,1,'0000.0001','["Planning timestamp","Planning"]');
INSERT INTO outline_path VALUES(47,5,46,2,'0000.0001.0001','["Planning timestamp","Planning","Simple scheduled"]');
INSERT INTO outline_path VALUES(48,5,46,2,'0000.0001.0002','["Planning timestamp","Planning","Simple deadline"]');
INSERT INTO outline_path VALUES(49,5,46,2,'0000.0001.0003','["Planning timestamp","Planning","Simple closed"]');
INSERT INTO outline_path VALUES(50,5,46,2,'0000.0001.0004','["Planning timestamp","Planning","All on one planning line"]');
INSERT INTO outline_path VALUES(51,5,46,2,'0000.0001.0005','["Planning timestamp","Planning","With time"]');
INSERT INTO outline_path VALUES(52,5,46,2,'0000.0001.0006','["Planning timestamp","Planning","Time range same day"]');
INSERT INTO outline_path VALUES(53,5,46,2,'0000.0001.0007','["Planning timestamp","Planning","Date range"]');
INSERT INTO outline_path VALUES(54,5,46,2,'0000.0001.0008','["Planning timestamp","Planning","Repeater"]');
INSERT INTO outline_path VALUES(55,5,46,2,'0000.0001.0009','["Planning timestamp","Planning","Diary expression"]');
INSERT INTO outline_path VALUES(56,5,46,2,'0000.0001.0010','["Planning timestamp","Planning","Multiple same keyword"]');
INSERT INTO outline_path VALUES(57,5,46,2,'0000.0001.0011','["Planning timestamp","Planning","Not valid"]');
INSERT INTO outline_path VALUES(58,5,57,3,'0000.0001.0011.0001','["Planning timestamp","Planning","Not valid","Multiple planning lines"]');
INSERT INTO outline_path VALUES(59,5,57,3,'0000.0001.0011.0002','["Planning timestamp","Planning","Not valid","Planning not immediately after headline"]');
INSERT INTO outline_path VALUES(60,5,57,3,'0000.0001.0011.0003','["Planning timestamp","Planning","Not valid","Looks like planning in body"]');
INSERT INTO outline_path VALUES(61,5,57,3,'0000.0001.0011.0004','["Planning timestamp","Planning","Not valid","Lowercase should probably not count"]');
INSERT INTO outline_path VALUES(62,5,57,3,'0000.0001.0011.0005','["Planning timestamp","Planning","Not valid","Multiple same keyword"]');
INSERT INTO outline_path VALUES(63,6,NULL,0,'0000','["Org Property and Keyword Test"]');
INSERT INTO outline_path VALUES(64,6,63,1,'0000.0001','["Org Property and Keyword Test","Task with multiple drawer properties"]');
INSERT INTO outline_path VALUES(65,6,63,1,'0000.0002','["Org Property and Keyword Test","Task with duplicate drawer properties"]');
INSERT INTO outline_path VALUES(66,6,63,1,'0000.0003','["Org Property and Keyword Test","Task with append operator in drawer"]');
INSERT INTO outline_path VALUES(67,6,63,1,'0000.0004','["Org Property and Keyword Test","Task with mixed-case keys"]');
INSERT INTO outline_path VALUES(68,6,63,1,'0000.0005','["Org Property and Keyword Test","Task with empty property accepted by Orgize"]');
INSERT INTO outline_path VALUES(69,6,63,1,'0000.0006','["Org Property and Keyword Test","Task with Orgize empty-property limitation"]');
INSERT INTO outline_path VALUES(70,6,63,1,'0000.0007','["Org Property and Keyword Test","Task after file-level property keywords"]');
INSERT INTO outline_path VALUES(71,6,63,1,'0000.0008','["Org Property and Keyword Test","Task after later file-level keywords"]');
INSERT INTO outline_path VALUES(72,6,63,1,'0000.0009','["Org Property and Keyword Test","Boundary: property-like but not properties"]');
INSERT INTO outline_path VALUES(73,7,NULL,0,'0000','["Tags and FILETAGS Fixture"]');
INSERT INTO outline_path VALUES(74,7,73,1,'0000.0001','["Tags and FILETAGS Fixture","Parent"]');
INSERT INTO outline_path VALUES(75,7,74,2,'0000.0001.0001','["Tags and FILETAGS Fixture","Parent","Child"]');
INSERT INTO outline_path VALUES(76,7,75,3,'0000.0001.0001.0001','["Tags and FILETAGS Fixture","Parent","Child","Grandchild"]');
INSERT INTO outline_path VALUES(77,7,73,1,'0000.0002','["Tags and FILETAGS Fixture","Sibling"]');
INSERT INTO outline_path VALUES(78,7,73,1,'0000.0003','["Tags and FILETAGS Fixture","Duplicate Local"]');
INSERT INTO outline_path VALUES(79,7,73,1,'0000.0004','["Tags and FILETAGS Fixture","After Later FILETAGS"]');
INSERT INTO outline_path VALUES(80,7,73,1,'0000.0005','["Tags and FILETAGS Fixture","parent with one tag"]');
INSERT INTO outline_path VALUES(81,7,80,2,'0000.0005.0001','["Tags and FILETAGS Fixture","parent with one tag","child inherits parent tag"]');
INSERT INTO outline_path VALUES(82,7,80,2,'0000.0005.0002','["Tags and FILETAGS Fixture","parent with one tag","child with local tag"]');
INSERT INTO outline_path VALUES(83,7,82,3,'0000.0005.0002.0001','["Tags and FILETAGS Fixture","parent with one tag","child with local tag","grandchild inherits both"]');
INSERT INTO outline_path VALUES(84,7,82,3,'0000.0005.0002.0002','["Tags and FILETAGS Fixture","parent with one tag","child with local tag","grandchild with duplicate local tag"]');
INSERT INTO outline_path VALUES(85,7,73,1,'0000.0006','["Tags and FILETAGS Fixture","second parent"]');
INSERT INTO outline_path VALUES(86,7,85,2,'0000.0006.0001','["Tags and FILETAGS Fixture","second parent","second child with local tag"]');
INSERT INTO outline_path VALUES(87,7,86,3,'0000.0006.0001.0001','["Tags and FILETAGS Fixture","second parent","second child with local tag","second grandchild with extra tag"]');
INSERT INTO outline_path VALUES(88,7,73,1,'0000.0007','["Tags and FILETAGS Fixture","untagged parent"]');
INSERT INTO outline_path VALUES(89,7,88,2,'0000.0007.0001','["Tags and FILETAGS Fixture","untagged parent","child with only local tag"]');
INSERT INTO outline_path VALUES(90,7,88,2,'0000.0007.0002','["Tags and FILETAGS Fixture","untagged parent","child without tags"]');
INSERT INTO outline_path VALUES(91,8,NULL,0,'0000','["timestamp-repeaters"]');
INSERT INTO outline_path VALUES(92,8,91,1,'0000.0001','["timestamp-repeaters","Repeater markers"]');
INSERT INTO outline_path VALUES(93,8,92,2,'0000.0001.0001','["timestamp-repeaters","Repeater markers","Cumulate plus"]');
INSERT INTO outline_path VALUES(94,8,92,2,'0000.0001.0002','["timestamp-repeaters","Repeater markers","Catch up plus plus"]');
INSERT INTO outline_path VALUES(95,8,92,2,'0000.0001.0003','["timestamp-repeaters","Repeater markers","Restart dot plus"]');
INSERT INTO outline_path VALUES(96,8,91,1,'0000.0002','["timestamp-repeaters","Repeater units"]');
INSERT INTO outline_path VALUES(97,8,96,2,'0000.0002.0001','["timestamp-repeaters","Repeater units","Repeater hour"]');
INSERT INTO outline_path VALUES(98,8,96,2,'0000.0002.0002','["timestamp-repeaters","Repeater units","Repeater day"]');
INSERT INTO outline_path VALUES(99,8,96,2,'0000.0002.0003','["timestamp-repeaters","Repeater units","Repeater week"]');
INSERT INTO outline_path VALUES(100,8,96,2,'0000.0002.0004','["timestamp-repeaters","Repeater units","Repeater month"]');
INSERT INTO outline_path VALUES(101,8,96,2,'0000.0002.0005','["timestamp-repeaters","Repeater units","Repeater year"]');
INSERT INTO outline_path VALUES(102,8,91,1,'0000.0003','["timestamp-repeaters","Repeater deadline part"]');
INSERT INTO outline_path VALUES(103,8,102,2,'0000.0003.0001','["timestamp-repeaters","Repeater deadline part","Repeater with deadline day"]');
INSERT INTO outline_path VALUES(104,8,102,2,'0000.0003.0002','["timestamp-repeaters","Repeater deadline part","Repeater with deadline week"]');
INSERT INTO outline_path VALUES(105,8,102,2,'0000.0003.0003','["timestamp-repeaters","Repeater deadline part","Repeater with deadline month"]');
INSERT INTO outline_path VALUES(106,8,91,1,'0000.0004','["timestamp-repeaters","Warning delays"]');
INSERT INTO outline_path VALUES(107,8,106,2,'0000.0004.0001','["timestamp-repeaters","Warning delays","Warning all"]');
INSERT INTO outline_path VALUES(108,8,106,2,'0000.0004.0002','["timestamp-repeaters","Warning delays","Warning first"]');
INSERT INTO outline_path VALUES(109,8,106,2,'0000.0004.0003','["timestamp-repeaters","Warning delays","Warning hour"]');
INSERT INTO outline_path VALUES(110,8,106,2,'0000.0004.0004','["timestamp-repeaters","Warning delays","Warning month"]');
INSERT INTO outline_path VALUES(111,8,106,2,'0000.0004.0005','["timestamp-repeaters","Warning delays","Warning year"]');
INSERT INTO outline_path VALUES(112,8,91,1,'0000.0005','["timestamp-repeaters","Repeater and warning combinations"]');
INSERT INTO outline_path VALUES(113,8,112,2,'0000.0005.0001','["timestamp-repeaters","Repeater and warning combinations","Repeater and warning"]');
INSERT INTO outline_path VALUES(114,8,112,2,'0000.0005.0002','["timestamp-repeaters","Repeater and warning combinations","Catch up repeater with deadline part and warning"]');
INSERT INTO outline_path VALUES(115,8,112,2,'0000.0005.0003','["timestamp-repeaters","Repeater and warning combinations","Restart repeater with first warning"]');
INSERT INTO outline_path VALUES(116,8,91,1,'0000.0006','["timestamp-repeaters","Time and range combinations"]');
INSERT INTO outline_path VALUES(117,8,116,2,'0000.0006.0001','["timestamp-repeaters","Time and range combinations","Time with repeater"]');
INSERT INTO outline_path VALUES(118,8,116,2,'0000.0006.0002','["timestamp-repeaters","Time and range combinations","Time range with repeater"]');
INSERT INTO outline_path VALUES(119,8,116,2,'0000.0006.0003','["timestamp-repeaters","Time and range combinations","Date range with repeater"]');
INSERT INTO outline_path VALUES(120,8,91,1,'0000.0007','["timestamp-repeaters","Inactive timestamp with repeater"]');
INSERT INTO outline_path VALUES(121,8,120,2,'0000.0007.0001','["timestamp-repeaters","Inactive timestamp with repeater","Inactive repeater"]');
INSERT INTO outline_path VALUES(122,8,91,1,'0000.0008','["timestamp-repeaters","Diary negative case"]');
INSERT INTO outline_path VALUES(123,8,122,2,'0000.0008.0001','["timestamp-repeaters","Diary negative case","Diary with apparent repeater text"]');
INSERT INTO outline_path VALUES(124,9,NULL,0,'0000','["Timestamps"]');
INSERT INTO outline_path VALUES(125,9,124,1,'0000.0001','["Timestamps","Meet Peter at the movies"]');
INSERT INTO outline_path VALUES(126,9,124,1,'0000.0002','["Timestamps","Discussion on climate change"]');
INSERT INTO outline_path VALUES(127,9,124,1,'0000.0003','["Timestamps","My days off"]');
INSERT INTO outline_path VALUES(128,9,124,1,'0000.0004','["Timestamps","Can be anywhere"]');
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
INSERT INTO todo_keywords VALUES(2,'one','open','t',0,'org_keyword','TODO',3);
INSERT INTO todo_keywords VALUES(2,'two','open','n',1,'org_keyword','TODO',3);
INSERT INTO todo_keywords VALUES(2,'FIVE','open',NULL,2,'org_keyword','TODO',4);
INSERT INTO todo_keywords VALUES(2,'SIX','open',NULL,3,'org_keyword','TODO',4);
INSERT INTO todo_keywords VALUES(2,'seven','open',NULL,4,'org_keyword','TYP_TODO',5);
INSERT INTO todo_keywords VALUES(2,'nine','open',NULL,5,'org_keyword','SEQ_TODO',6);
INSERT INTO todo_keywords VALUES(2,'late_open','open',NULL,6,'org_keyword','TODO',34);
INSERT INTO todo_keywords VALUES(2,'three','closed','d',7,'org_keyword','TODO',3);
INSERT INTO todo_keywords VALUES(2,'four','closed','w',8,'org_keyword','TODO',3);
INSERT INTO todo_keywords VALUES(2,'eight','closed',NULL,9,'org_keyword','TYP_TODO',5);
INSERT INTO todo_keywords VALUES(2,'ten','closed',NULL,10,'org_keyword','SEQ_TODO',6);
INSERT INTO todo_keywords VALUES(2,'eleven','closed','c',11,'org_keyword','TODO',30);
INSERT INTO todo_keywords VALUES(2,'late_done','closed',NULL,12,'org_keyword','TODO',34);
INSERT INTO todo_keywords VALUES(3,'TODO','open',NULL,0,'org_keyword','TODO',29);
INSERT INTO todo_keywords VALUES(3,'NEXT','open',NULL,1,'org_keyword','TODO',29);
INSERT INTO todo_keywords VALUES(3,'IDEA','open','i',2,'org_keyword','SEQ_TODO',30);
INSERT INTO todo_keywords VALUES(3,'WURST','open','w',3,'org_keyword','SEQ_TODO',30);
INSERT INTO todo_keywords VALUES(3,'PLAN','open','p',4,'org_keyword','SEQ_TODO',30);
INSERT INTO todo_keywords VALUES(3,'BUILD','open','b',5,'org_keyword','SEQ_TODO',30);
INSERT INTO todo_keywords VALUES(3,'WAITING','open','w',6,'org_keyword','TYP_TODO',31);
INSERT INTO todo_keywords VALUES(3,'DONE','closed',NULL,7,'org_keyword','TODO',29);
INSERT INTO todo_keywords VALUES(3,'CANCELED','closed',NULL,8,'org_keyword','TODO',29);
INSERT INTO todo_keywords VALUES(4,'TODO','open',NULL,0,'config_default',NULL,NULL);
INSERT INTO todo_keywords VALUES(4,'DONE','closed',NULL,1,'config_default',NULL,NULL);
INSERT INTO todo_keywords VALUES(5,'TODO','open',NULL,0,'config_default',NULL,NULL);
INSERT INTO todo_keywords VALUES(5,'DONE','closed',NULL,1,'config_default',NULL,NULL);
INSERT INTO todo_keywords VALUES(6,'TODO','open',NULL,0,'config_default',NULL,NULL);
INSERT INTO todo_keywords VALUES(6,'DONE','closed',NULL,1,'config_default',NULL,NULL);
INSERT INTO todo_keywords VALUES(7,'TODO','open',NULL,0,'config_default',NULL,NULL);
INSERT INTO todo_keywords VALUES(7,'DONE','closed',NULL,1,'config_default',NULL,NULL);
INSERT INTO todo_keywords VALUES(8,'TODO','open',NULL,0,'config_default',NULL,NULL);
INSERT INTO todo_keywords VALUES(8,'DONE','closed',NULL,1,'config_default',NULL,NULL);
INSERT INTO todo_keywords VALUES(9,'TODO','open',NULL,0,'config_default',NULL,NULL);
INSERT INTO todo_keywords VALUES(9,'DONE','closed',NULL,1,'config_default',NULL,NULL);
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
INSERT INTO links VALUES(1,1,1,121,148,5,'normal','bracket','[[FILE:root-notes.org::42]]','FILE:root-notes.org::42',NULL,'file','root-notes.org','42',NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(2,1,1,149,182,6,'normal','bracket','[[root target][root description]]','root target','root description','fuzzy','root target',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(3,1,3,395,426,16,'normal','bracket','[[#internal-link-to-custom-id]]','#internal-link-to-custom-id',NULL,'custom-id','#internal-link-to-custom-id',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(4,1,3,429,501,17,'normal','bracket','[[#internal-link-to-custom-id][description: internal-link-to-custom-id]]','#internal-link-to-custom-id','description: internal-link-to-custom-id','custom-id','#internal-link-to-custom-id',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(5,1,4,528,555,20,'normal','bracket','[[*Internal bracket links]]','*Internal bracket links',NULL,'fuzzy','*Internal bracket links',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(6,1,4,558,612,21,'normal','bracket','[[*Internal bracket links][description: heading link]]','*Internal bracket links','description: heading link','fuzzy','*Internal bracket links',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(7,1,5,663,683,25,'normal','bracket','[[dedicated target]]','dedicated target',NULL,'fuzzy','dedicated target',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(8,1,5,686,737,26,'normal','bracket','[[dedicated target][description: dedicated target]]','dedicated target','description: dedicated target','fuzzy','dedicated target',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(9,1,6,796,812,30,'normal','bracket','[[named target]]','named target',NULL,'fuzzy','named target',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(10,1,6,815,858,31,'normal','bracket','[[named target][description: named target]]','named target','description: named target','fuzzy','named target',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(11,1,7,880,902,34,'normal','bracket','[[no matching target]]','no matching target',NULL,'fuzzy','no matching target',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(12,1,7,905,960,35,'normal','bracket','[[no matching target][description: no matching target]]','no matching target','description: no matching target','fuzzy','no matching target',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(13,1,7,963,976,36,'normal','bracket','[[notes.org]]','notes.org',NULL,'fuzzy','notes.org',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(14,1,9,1030,1043,41,'normal','bracket','[[file:/etc]]','file:/etc',NULL,'file','/etc',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(15,1,9,1046,1078,42,'normal','bracket','[[file:/etc][description: /etc]]','file:/etc','description: /etc','file','/etc',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(16,1,9,1081,1095,43,'normal','bracket','[[file:/etc/]]','file:/etc/',NULL,'file','/etc/',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(17,1,9,1098,1121,44,'normal','bracket','[[file:/etc/host.conf]]','file:/etc/host.conf',NULL,'file','/etc/host.conf',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(18,1,9,1124,1136,45,'normal','bracket','[[file:../]]','file:../',NULL,'file','../',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(19,1,9,1139,1157,46,'normal','bracket','[[file:../../sql]]','file:../../sql',NULL,'file','../../sql',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(20,1,9,1160,1179,47,'normal','bracket','[[file:../../sql/]]','file:../../sql/',NULL,'file','../../sql/',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(21,1,9,1182,1208,48,'normal','bracket','[[file:../parser_test.rs]]','file:../parser_test.rs',NULL,'file','../parser_test.rs',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(22,1,9,1211,1240,49,'normal','bracket','[[file:./org-test-links.org]]','file:./org-test-links.org',NULL,'file','./org-test-links.org',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(23,1,9,1243,1262,50,'normal','bracket','[[file:~/.emacs.d]]','file:~/.emacs.d',NULL,'file','~/.emacs.d',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(24,1,9,1265,1285,51,'normal','bracket','[[file:~/.emacs.d/]]','file:~/.emacs.d/',NULL,'file','~/.emacs.d/',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(25,1,9,1288,1315,52,'normal','bracket','[[file:~/.emacs.d/init.el]]','file:~/.emacs.d/init.el',NULL,'file','~/.emacs.d/init.el',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(26,1,10,1345,1362,55,'normal','bracket','[[file+sys:/etc]]','file+sys:/etc',NULL,'file+sys','/etc',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(27,1,10,1365,1384,56,'normal','bracket','[[file+emacs:/etc]]','file+emacs:/etc',NULL,'file+emacs','/etc',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(28,1,10,1387,1432,57,'normal','bracket','[[file+sys:/etc][description: file+sys:/etc]]','file+sys:/etc','description: file+sys:/etc','file+sys','/etc',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(29,1,10,1435,1484,58,'normal','bracket','[[file+emacs:/etc][description: file+emacs:/etc]]','file+emacs:/etc','description: file+emacs:/etc','file+emacs','/etc',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(30,1,11,1511,1519,61,'normal','bracket','[[/etc]]','/etc',NULL,'file','/etc',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(31,1,11,1522,1531,62,'normal','bracket','[[/etc/]]','/etc/',NULL,'file','/etc/',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(32,1,11,1534,1552,63,'normal','bracket','[[/etc/host.conf]]','/etc/host.conf',NULL,'file','/etc/host.conf',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(33,1,11,1555,1562,64,'normal','bracket','[[../]]','../',NULL,'file','../',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(34,1,11,1565,1578,65,'normal','bracket','[[../../sql]]','../../sql',NULL,'file','../../sql',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(35,1,11,1581,1595,66,'normal','bracket','[[../../sql/]]','../../sql/',NULL,'file','../../sql/',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(36,1,11,1598,1619,67,'normal','bracket','[[../parser_test.rs]]','../parser_test.rs',NULL,'file','../parser_test.rs',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(37,1,11,1622,1646,68,'normal','bracket','[[./org-test-links.org]]','./org-test-links.org',NULL,'file','./org-test-links.org',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(38,1,11,1649,1662,69,'normal','bracket','[[~/memento]]','~/memento',NULL,'file','~/memento',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(39,1,12,1689,1722,72,'normal','bracket','[[file:./org-test-links.org::10]]','file:./org-test-links.org::10',NULL,'file','./org-test-links.org','10',NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(40,1,12,1725,1783,73,'normal','bracket','[[file:./org-test-links.org::#internal-link-to-custom-id]]','file:./org-test-links.org::#internal-link-to-custom-id',NULL,'file','./org-test-links.org','#internal-link-to-custom-id',NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(41,1,12,1786,1833,74,'normal','bracket','[[file:./org-test-links.org::dedicated target]]','file:./org-test-links.org::dedicated target',NULL,'file','./org-test-links.org','dedicated target',NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(42,1,12,1836,1890,75,'normal','bracket','[[file:./org-test-links.org::*Internal bracket links]]','file:./org-test-links.org::*Internal bracket links',NULL,'file','./org-test-links.org','*Internal bracket links',NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(43,1,12,1893,1939,76,'normal','bracket','[[file:./org-test-links.org::/*.File-like.*/]]','file:./org-test-links.org::/*.File-like.*/',NULL,'file','./org-test-links.org','/*.File-like.*/',NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(44,1,12,1942,1970,77,'normal','bracket','[[./org-test-links.org::10]]','./org-test-links.org::10',NULL,'file','./org-test-links.org','10',NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(45,1,12,1973,2026,78,'normal','bracket','[[./org-test-links.org::#internal-link-to-custom-id]]','./org-test-links.org::#internal-link-to-custom-id',NULL,'file','./org-test-links.org','#internal-link-to-custom-id',NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(46,1,12,2029,2071,79,'normal','bracket','[[./org-test-links.org::dedicated target]]','./org-test-links.org::dedicated target',NULL,'file','./org-test-links.org','dedicated target',NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(47,1,12,2074,2123,80,'normal','bracket','[[./org-test-links.org::*Internal bracket links]]','./org-test-links.org::*Internal bracket links',NULL,'file','./org-test-links.org','*Internal bracket links',NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(48,1,12,2126,2167,81,'normal','bracket','[[./org-test-links.org::/*.File-like.*/]]','./org-test-links.org::/*.File-like.*/',NULL,'file','./org-test-links.org','/*.File-like.*/',NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(49,1,12,2170,2183,82,'normal','bracket','[[file:::10]]','file:::10',NULL,'file','','10',NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(50,1,12,2186,2220,83,'normal','bracket','[[file:::*Internal bracket links]]','file:::*Internal bracket links',NULL,'file','','*Internal bracket links',NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(51,1,14,2271,2293,88,'normal','bracket','[[http://orgmode.org]]','http://orgmode.org',NULL,'http','//orgmode.org',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(52,1,14,2296,2351,89,'normal','bracket','[[http://orgmode.org][description: http://orgmode.org]]','http://orgmode.org','description: http://orgmode.org','http','//orgmode.org',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(53,1,14,2354,2377,90,'normal','bracket','[[https://orgmode.org]]','https://orgmode.org',NULL,'https','//orgmode.org',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(54,1,14,2380,2437,91,'normal','bracket','[[https://orgmode.org][description: https://orgmode.org]]','https://orgmode.org','description: https://orgmode.org','https','//orgmode.org',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(55,1,14,2440,2459,92,'normal','bracket','[[news:comp.emacs]]','news:comp.emacs',NULL,'news','comp.emacs',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(56,1,14,2462,2494,93,'normal','bracket','[[mailto:emacs-orgmode@gnu.org]]','mailto:emacs-orgmode@gnu.org',NULL,'mailto','emacs-orgmode@gnu.org',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(57,1,14,2497,2520,94,'normal','bracket','[[help:org-store-link]]','help:org-store-link',NULL,'help','org-store-link',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(58,1,14,2523,2549,95,'normal','bracket','[[info:org#External Link]]','info:org#External Link',NULL,'info','org#External Link',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(59,1,14,2552,2572,96,'normal','bracket','[[shell:ls *.org  ]]','shell:ls *.org  ',NULL,'shell','ls *.org  ',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(60,1,14,2575,2617,97,'normal','bracket','[[elisp:(find-file "~/.emacs.d/init.el")]]','elisp:(find-file "~/.emacs.d/init.el")',NULL,'elisp','(find-file "~/.emacs.d/init.el")',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(61,1,15,2655,2670,100,'normal','bracket','[[unknown:foo]]','unknown:foo',NULL,'unknown','foo',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(62,1,15,2673,2715,101,'normal','bracket','[[unknown:foo][description: unknown type]]','unknown:foo','description: unknown type','unknown','foo',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(63,1,15,2718,2737,102,'normal','bracket','[[customlink:test]]','customlink:test',NULL,'customlink','test',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(64,1,15,2740,2789,103,'normal','bracket','[[customlink:test][description: customlink:test]]','customlink:test','description: customlink:test','customlink','test',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(65,1,15,2792,2811,104,'normal','bracket','[[doi:10.1000/182]]','doi:10.1000/182',NULL,'doi','10.1000/182',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(66,1,15,2814,2841,105,'normal','bracket','[[irc:/irc.com/#emacs/bob]]','irc:/irc.com/#emacs/bob',NULL,'irc','/irc.com/#emacs/bob',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(67,1,18,3330,3386,127,'normal','bracket','[[file:sub/äöü.txt::target][A file link with Umlaut]]','file:sub/äöü.txt::target','A file link with Umlaut','file','sub/äöü.txt','target',NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(68,1,18,3389,3417,128,'normal','bracket','[[https://orgmode.org/🔥]]','https://orgmode.org/🔥',NULL,'https','//orgmode.org/🔥',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(69,1,18,3420,3472,129,'normal','bracket','[[https://orgmode.org/🔥][Emoji description 😀]]','https://orgmode.org/🔥','Emoji description 😀','https','//orgmode.org/🔥',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(70,1,18,3475,3502,130,'normal','bracket','[[dedicated target äöü]]','dedicated target äöü',NULL,'fuzzy','dedicated target äöü',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(71,2,19,168,231,8,'normal','bracket','[[file:../../notes/org-semantics/file-local-todo-keywords.org]]','file:../../notes/org-semantics/file-local-todo-keywords.org',NULL,'file','../../notes/org-semantics/file-local-todo-keywords.org',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(72,4,43,73,134,5,'normal','bracket','[[file:../../notes/org-semantics/multipe-title-keywords.org]]','file:../../notes/org-semantics/multipe-title-keywords.org',NULL,'file','../../notes/org-semantics/multipe-title-keywords.org',NULL,NULL,NULL,NULL,NULL,NULL);
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
