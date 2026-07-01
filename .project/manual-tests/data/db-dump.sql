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
INSERT INTO files VALUES(1,'/home/hubisan/projects/coding/org-files-db/.project/manual-tests/files/file-local-todo-keywords.org',1782810515620385647,1188,NULL,1782899589);
INSERT INTO files VALUES(2,'/home/hubisan/projects/coding/org-files-db/.project/manual-tests/files/keywords.org',1782237960833303823,2276,NULL,1782899589);
INSERT INTO files VALUES(3,'/home/hubisan/projects/coding/org-files-db/.project/manual-tests/files/links-angle.org',1782831514259839578,2491,NULL,1782899589);
INSERT INTO files VALUES(4,'/home/hubisan/projects/coding/org-files-db/.project/manual-tests/files/links-bracket--weird-ones.org',1782827941687352485,1591,NULL,1782899589);
INSERT INTO files VALUES(5,'/home/hubisan/projects/coding/org-files-db/.project/manual-tests/files/links-bracket.org',1782811598000160725,3503,NULL,1782899589);
INSERT INTO files VALUES(6,'/home/hubisan/projects/coding/org-files-db/.project/manual-tests/files/links-ignored-regions.org',1782852173083933896,6844,NULL,1782899589);
INSERT INTO files VALUES(7,'/home/hubisan/projects/coding/org-files-db/.project/manual-tests/files/links-plain.org',1782852135861236633,7646,NULL,1782899589);
INSERT INTO files VALUES(8,'/home/hubisan/projects/coding/org-files-db/.project/manual-tests/files/multipe-title-keywords.org',1781809982908544855,321,NULL,1782899589);
INSERT INTO files VALUES(9,'/home/hubisan/projects/coding/org-files-db/.project/manual-tests/files/planning-lines.org',1782129087807207205,1163,NULL,1782899589);
INSERT INTO files VALUES(10,'/home/hubisan/projects/coding/org-files-db/.project/manual-tests/files/properties.org',1782218329407221851,2783,NULL,1782899589);
INSERT INTO files VALUES(11,'/home/hubisan/projects/coding/org-files-db/.project/manual-tests/files/tags.org',1782243198853649058,1307,NULL,1782899589);
INSERT INTO files VALUES(12,'/home/hubisan/projects/coding/org-files-db/.project/manual-tests/files/timestamp-repeaters.org',1782159459016426918,1626,NULL,1782899589);
INSERT INTO files VALUES(13,'/home/hubisan/projects/coding/org-files-db/.project/manual-tests/files/timestamps.org',1782153027780066037,299,NULL,1782899589);
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
INSERT INTO headings VALUES(25,3,NULL,0,1,-1,2491,'Angle Link Fixture','Angle Link Fixture',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(26,3,25,1,10,258,2246,'Angle links','Angle links',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(27,3,26,2,13,274,447,'Basic angle links with spaces','Basic angle links with spaces',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(28,3,26,2,19,447,664,'File-like angle links with search options','File-like angle links with search options',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(29,3,26,2,27,664,852,'File variant angle links with search options','File variant angle links with search options',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(30,3,26,2,33,852,996,'Unknown and custom-looking angle links','Unknown and custom-looking angle links',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(31,3,26,2,40,996,1162,'Action-like angle links','Action-like angle links',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(32,3,26,2,47,1162,1445,'Non-file-like search-option-looking paths','Non-file-like search-option-looking paths',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(33,3,26,2,57,1445,1679,'Special','Special',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(34,3,26,2,68,1679,2044,'Invalid angle candidates','Invalid angle candidates',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(35,3,26,2,86,2044,2246,'Plain links not stored by this task','Plain links not stored by this task',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(36,3,25,1,95,2246,2491,'Unicode angle links','Unicode angle links',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(37,3,36,2,99,2308,2491,'Übung 🚀 Ein Titel mit Umlaut und Emoji','Übung 🚀 Ein Titel mit Umlaut und Emoji',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'["foo","bar"]');
INSERT INTO headings VALUES(38,4,NULL,0,1,-1,1591,'Bracket Link Fixture','Bracket Link Fixture',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(39,5,NULL,0,1,-1,3503,'Bracket Link Fixture','Bracket Link Fixture',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(40,5,39,1,10,290,978,'Internal bracket links','Internal bracket links',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(41,5,40,2,15,374,503,'Custom ID links','Custom ID links',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(42,5,40,2,19,503,614,'Fuzzy heading links','Fuzzy heading links',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(43,5,40,2,23,614,739,'Dedicated target links','Dedicated target links',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(44,5,40,2,28,739,860,'Named target style fuzzy links','Named target style fuzzy links',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(45,5,40,2,33,860,978,'Fuzzy fallback','Fuzzy fallback',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(46,5,39,1,38,978,2222,'File-like bracket links','File-like bracket links',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(47,5,46,2,40,1005,1317,'Explicit file links','Explicit file links',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(48,5,46,2,54,1317,1486,'Explicit file variants','Explicit file variants',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(49,5,46,2,60,1486,1664,'Implicit file links','Implicit file links',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(50,5,46,2,71,1664,2222,'File search options','File search options',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(51,5,39,1,85,2222,2843,'Typed bracket links','Typed bracket links',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(52,5,51,2,87,2245,2619,'Built-in typed links','Built-in typed links',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(53,5,51,2,99,2619,2843,'Unknown and custom typed links','Unknown and custom typed links',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(54,5,39,1,107,2843,3147,'Negative non-bracket examples','Negative non-bracket examples',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(55,5,39,1,119,3147,3503,'Unicode bracket links','Unicode bracket links',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(56,5,55,2,123,3211,3503,'Übung 🚀 Ein Titel mit Umlaut und Emoji','Übung 🚀 Ein Titel mit Umlaut und Emoji',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'["foo","bar"]');
INSERT INTO headings VALUES(57,6,NULL,0,1,-1,6844,'Ignored Region And Source Context Link Fixture https://example.org/in-title-keyword','Ignored Region And Source Context Link Fixture https://example.org/in-title-keyword',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(58,6,57,1,13,531,6844,'Heading with https://example.org/in-heading-title','Heading with https://example.org/in-heading-title',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(59,6,58,2,17,641,1746,'Parsed block contexts','Parsed block contexts',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(60,6,58,2,56,1746,2355,'Drawer contexts','Drawer contexts',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(61,6,58,2,76,2355,2770,'Ignored inline markup','Ignored inline markup',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(62,6,58,2,86,2770,3314,'Ignored inline source and export','Ignored inline source and export',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(63,6,58,2,97,3314,4268,'Ignored block regions','Ignored block regions',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(64,6,58,2,135,4268,4844,'Ignored fixed-width and comment lines','Ignored fixed-width and comment lines',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(65,6,58,2,153,4844,5356,'Ignored file-level keywords','Ignored file-level keywords',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(66,6,58,2,165,5356,6112,'Overlap and precedence guard cases','Overlap and precedence guard cases',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(67,6,58,2,185,6112,6844,'Search options and contexts should remain independent','Search options and contexts should remain independent',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(68,7,NULL,0,1,-1,7646,'Plain Link Fixture','Plain Link Fixture',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(69,7,68,1,23,447,7399,'Plain links','Plain links',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(70,7,69,2,25,462,1008,'Default plain protocols','Default plain protocols',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(71,7,69,2,50,1008,1190,'Custom plain protocols','Custom plain protocols',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(72,7,69,2,59,1190,1487,'Action-like plain protocols','Action-like plain protocols',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(73,7,69,2,71,1487,1784,'Bracket and angle links unaffected by plain config','Bracket and angle links unaffected by plain config',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(74,7,69,2,82,1784,2158,'Trailing punctuation trimming','Trailing punctuation trimming',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(75,7,69,2,99,2158,5146,'Boundary and end behavior','Boundary and end behavior',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(76,7,75,3,106,2504,3198,'Accepted plain-link left boundaries','Accepted plain-link left boundaries',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(77,7,75,3,125,3198,3456,'Rejected plain-link left boundaries','Rejected plain-link left boundaries',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(78,7,75,3,134,3456,4360,'Whitespace and balanced delimiter end behavior','Whitespace and balanced delimiter end behavior',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(79,7,75,3,159,4360,4741,'Emphasis and markup interaction','Emphasis and markup interaction',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(80,7,75,3,168,4741,5146,'Final-character behavior','Final-character behavior',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(81,7,69,2,184,5146,5758,'Search-option-looking paths','Search-option-looking paths',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(82,7,69,2,205,5758,5987,'False positives to watch','False positives to watch',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(83,7,69,2,215,5987,6824,'Ignored forms','Ignored forms',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(84,7,69,2,254,6824,7399,'Parsed regions','Parsed regions',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(85,7,68,1,285,7399,7646,'Unicode plain links','Unicode plain links',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(86,7,85,2,289,7461,7646,'Übung 🚀 Ein Titel mit Umlaut und Emoji','Übung 🚀 Ein Titel mit Umlaut und Emoji',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'["foo","bar"]');
INSERT INTO headings VALUES(87,8,NULL,0,1,-1,321,'Title can span multiple lines, even here','Title can span multiple lines, even here',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(88,8,87,1,7,136,321,'Unfortunately Everywhere','Unfortunately Everywhere',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(89,9,NULL,0,1,-1,1163,'Planning timestamp','Planning timestamp',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(90,9,89,1,4,49,1163,'Planning','Planning',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(91,9,90,2,6,62,111,'Simple scheduled','Simple scheduled',NULL,NULL,NULL,'<2024-11-20 Wed>',1732060800,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(92,9,90,2,9,111,158,'Simple deadline','Simple deadline',NULL,NULL,NULL,NULL,NULL,'<2024-12-01 Sun>',1733011200,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(93,9,90,2,12,158,201,'Simple closed','Simple closed',NULL,NULL,NULL,NULL,NULL,NULL,NULL,'[2024-11-21 Thu]',1732147200,0,0,'[]');
INSERT INTO headings VALUES(94,9,90,2,15,201,310,'All on one planning line','All on one planning line',NULL,NULL,NULL,'<2024-11-20 Wed>',1732060800,'<2024-12-01 Sun>',1733011200,'[2024-11-21 Thu]',1732147200,0,0,'[]');
INSERT INTO headings VALUES(95,9,90,2,18,310,358,'With time','With time',NULL,NULL,NULL,'<2024-11-20 Wed 09:30>',1732095000,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(96,9,90,2,21,358,422,'Time range same day','Time range same day',NULL,NULL,NULL,'<2024-11-20 Wed 09:30-11:00>',1732095000,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(97,9,90,2,24,422,482,'Date range','Date range',NULL,NULL,NULL,NULL,NULL,'<2024-12-01 Sun>--<2024-12-03 Tue>',1733011200,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(98,9,90,2,27,482,527,'Repeater','Repeater',NULL,NULL,NULL,'<2024-11-20 Wed +1w>',1732060800,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(99,9,90,2,30,527,582,'Diary expression','Diary expression',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(100,9,90,2,33,582,704,'Multiple same keyword','Multiple same keyword',NULL,NULL,NULL,'<2024-11-21 Thu>',1732147200,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(101,9,90,2,37,704,1163,'Not valid','Not valid',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(102,9,101,3,39,718,827,'Multiple planning lines','Multiple planning lines',NULL,NULL,NULL,'<2024-11-20 Wed>',1732060800,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(103,9,101,3,44,827,922,'Planning not immediately after headline','Planning not immediately after headline',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(104,9,101,3,48,922,1009,'Looks like planning in body','Looks like planning in body',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(105,9,101,3,51,1009,1078,'Lowercase should probably not count','Lowercase should probably not count',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(106,9,101,3,54,1078,1163,'Multiple same keyword','Multiple same keyword',NULL,NULL,NULL,'<2024-11-21 Thu>',1732147200,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(107,10,NULL,0,1,-1,2783,'Org Property and Keyword Test','Org Property and Keyword Test',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'["project","work"]');
INSERT INTO headings VALUES(108,10,107,1,16,465,638,'Task with multiple drawer properties','Task with multiple drawer properties',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'["project","work"]');
INSERT INTO headings VALUES(109,10,107,1,26,638,858,'Task with duplicate drawer properties','Task with duplicate drawer properties',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'["project","work"]');
INSERT INTO headings VALUES(110,10,107,1,35,858,1094,'Task with append operator in drawer','Task with append operator in drawer',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'["project","work"]');
INSERT INTO headings VALUES(111,10,107,1,45,1094,1304,'Task with mixed-case keys','Task with mixed-case keys',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'["project","work"]');
INSERT INTO headings VALUES(112,10,107,1,58,1304,1437,'Task with empty property accepted by Orgize','Task with empty property accepted by Orgize',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'["project","work"]');
INSERT INTO headings VALUES(113,10,107,1,67,1437,1718,'Task with Orgize empty-property limitation','Task with Orgize empty-property limitation',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'["project","work"]');
INSERT INTO headings VALUES(114,10,107,1,76,1718,1984,'Task after file-level property keywords','Task after file-level property keywords',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'["project","work"]');
INSERT INTO headings VALUES(115,10,107,1,83,1984,2264,'Task after later file-level keywords','Task after later file-level keywords',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'["project","work"]');
INSERT INTO headings VALUES(116,10,107,1,89,2264,2783,'Boundary: property-like but not properties','Boundary: property-like but not properties',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'["project","work"]');
INSERT INTO headings VALUES(117,11,NULL,0,1,-1,1307,'Tags and FILETAGS Fixture','Tags and FILETAGS Fixture',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'["file","project","later","extra"]');
INSERT INTO headings VALUES(118,11,117,1,5,82,242,'Parent','Parent',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'["file","project","later","extra","parent"]');
INSERT INTO headings VALUES(119,11,118,2,8,114,242,'Child','Child',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'["file","project","later","extra","parent","child"]');
INSERT INTO headings VALUES(120,11,119,3,11,144,242,'Grandchild','Grandchild',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'["file","project","later","extra","parent","child","grandchild"]');
INSERT INTO headings VALUES(121,11,117,1,14,242,267,'Sibling','Sibling',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'["file","project","later","extra"]');
INSERT INTO headings VALUES(122,11,117,1,17,267,370,'Duplicate Local','Duplicate Local',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'["file","project","later","extra","sibling"]');
INSERT INTO headings VALUES(123,11,117,1,22,370,455,'After Later FILETAGS','After Later FILETAGS',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'["file","project","later","extra","after"]');
INSERT INTO headings VALUES(124,11,117,1,25,455,894,'parent with one tag','parent with one tag',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'["file","project","later","extra","parent"]');
INSERT INTO headings VALUES(125,11,124,2,28,500,576,'child inherits parent tag','child inherits parent tag',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'["file","project","later","extra","parent"]');
INSERT INTO headings VALUES(126,11,124,2,31,576,894,'child with local tag','child with local tag',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'["file","project","later","extra","parent","child"]');
INSERT INTO headings VALUES(127,11,126,3,34,664,754,'grandchild inherits both','grandchild inherits both',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'["file","project","later","extra","parent","child"]');
INSERT INTO headings VALUES(128,11,126,3,37,754,894,'grandchild with duplicate local tag','grandchild with duplicate local tag',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'["file","project","later","extra","parent","child"]');
INSERT INTO headings VALUES(129,11,117,1,41,894,1163,'second parent','second parent',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'["file","project","later","extra","second"]');
INSERT INTO headings VALUES(130,11,129,2,44,976,1163,'second child with local tag','second child with local tag',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'["file","project","later","extra","second","child"]');
INSERT INTO headings VALUES(131,11,130,3,47,1062,1163,'second grandchild with extra tag','second grandchild with extra tag',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'["file","project","later","extra","second","child"]');
INSERT INTO headings VALUES(132,11,117,1,50,1163,1307,'untagged parent','untagged parent',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'["file","project","later","extra"]');
INSERT INTO headings VALUES(133,11,132,2,52,1182,1256,'child with only local tag','child with only local tag',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'["file","project","later","extra","local"]');
INSERT INTO headings VALUES(134,11,132,2,55,1256,1307,'child without tags','child without tags',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'["file","project","later","extra"]');
INSERT INTO headings VALUES(135,12,NULL,0,1,-1,1626,'timestamp-repeaters','timestamp-repeaters',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(136,12,135,1,1,0,180,'Repeater markers','Repeater markers',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(137,12,136,2,3,20,70,'Cumulate plus','Cumulate plus',NULL,NULL,NULL,'<2024-11-20 Wed +1w>',1732060800,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(138,12,136,2,6,70,126,'Catch up plus plus','Catch up plus plus',NULL,NULL,NULL,'<2024-11-20 Wed ++1m>',1732060800,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(139,12,136,2,9,126,180,'Restart dot plus','Restart dot plus',NULL,NULL,NULL,'<2024-11-20 Wed .+2d>',1732060800,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(140,12,135,1,12,180,448,'Repeater units','Repeater units',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(141,12,140,2,14,198,248,'Repeater hour','Repeater hour',NULL,NULL,NULL,'<2024-11-20 Wed +3h>',1732060800,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(142,12,140,2,17,248,297,'Repeater day','Repeater day',NULL,NULL,NULL,'<2024-11-20 Wed +3d>',1732060800,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(143,12,140,2,20,297,347,'Repeater week','Repeater week',NULL,NULL,NULL,'<2024-11-20 Wed +3w>',1732060800,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(144,12,140,2,23,347,398,'Repeater month','Repeater month',NULL,NULL,NULL,'<2024-11-20 Wed +3m>',1732060800,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(145,12,140,2,26,398,448,'Repeater year','Repeater year',NULL,NULL,NULL,'<2024-11-20 Wed +3y>',1732060800,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(146,12,135,1,29,448,677,'Repeater deadline part','Repeater deadline part',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(147,12,146,2,31,474,540,'Repeater with deadline day','Repeater with deadline day',NULL,NULL,NULL,'<2024-11-20 Wed +1w/2d>',1732060800,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(148,12,146,2,34,540,608,'Repeater with deadline week','Repeater with deadline week',NULL,NULL,NULL,'<2024-11-20 Wed ++1m/1w>',1732060800,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(149,12,146,2,37,608,677,'Repeater with deadline month','Repeater with deadline month',NULL,NULL,NULL,'<2024-11-20 Wed .+1y/2m>',1732060800,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(150,12,135,1,40,677,944,'Warning delays','Warning delays',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(151,12,150,2,42,695,742,'Warning all','Warning all',NULL,NULL,NULL,NULL,NULL,'<2024-12-01 Sun -5d>',1733011200,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(152,12,150,2,45,742,792,'Warning first','Warning first',NULL,NULL,NULL,NULL,NULL,'<2024-12-01 Sun --2w>',1733011200,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(153,12,150,2,48,792,846,'Warning hour','Warning hour',NULL,NULL,NULL,NULL,NULL,'<2024-12-01 Sun 09:30 -3h>',1733045400,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(154,12,150,2,51,846,895,'Warning month','Warning month',NULL,NULL,NULL,NULL,NULL,'<2024-12-01 Sun -1m>',1733011200,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(155,12,150,2,54,895,944,'Warning year','Warning year',NULL,NULL,NULL,NULL,NULL,'<2024-12-01 Sun --1y>',1733011200,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(156,12,135,1,57,944,1210,'Repeater and warning combinations','Repeater and warning combinations',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(157,12,156,2,59,981,1041,'Repeater and warning','Repeater and warning',NULL,NULL,NULL,NULL,NULL,'<2024-12-01 Sun +1w -5d>',1733011200,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(158,12,156,2,62,1041,1133,'Catch up repeater with deadline part and warning','Catch up repeater with deadline part and warning',NULL,NULL,NULL,NULL,NULL,'<2024-12-01 Sun ++1m/2d -5d>',1733011200,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(159,12,156,2,65,1133,1210,'Restart repeater with first warning','Restart repeater with first warning',NULL,NULL,NULL,NULL,NULL,'<2024-12-01 Sun .+2w --1w>',1733011200,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(160,12,135,1,68,1210,1453,'Time and range combinations','Time and range combinations',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(161,12,160,2,70,1241,1302,'Time with repeater','Time with repeater',NULL,NULL,NULL,'<2024-11-20 Wed 09:30 +1w>',1732095000,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(162,12,160,2,73,1302,1375,'Time range with repeater','Time range with repeater',NULL,NULL,NULL,'<2024-11-20 Wed 09:30-11:00 +1w>',1732095000,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(163,12,160,2,76,1375,1453,'Date range with repeater','Date range with repeater',NULL,NULL,NULL,NULL,NULL,'<2024-12-01 Sun>--<2024-12-03 Tue +1w>',1733011200,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(164,12,135,1,79,1453,1532,'Inactive timestamp with repeater','Inactive timestamp with repeater',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(165,12,164,2,81,1489,1532,'Inactive repeater','Inactive repeater',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(166,12,135,1,84,1532,1626,'Diary negative case','Diary negative case',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(167,12,166,2,86,1555,1626,'Diary with apparent repeater text','Diary with apparent repeater text',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(168,13,NULL,0,1,-1,299,'Timestamps','Timestamps',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(169,13,168,1,4,40,91,'Meet Peter at the movies','Meet Peter at the movies',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(170,13,168,1,7,91,152,'Discussion on climate change','Discussion on climate change',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(171,13,168,1,10,152,201,'My days off','My days off',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
INSERT INTO headings VALUES(172,13,168,1,14,201,299,'Can be anywhere','Can be anywhere',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,'[]');
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
INSERT INTO timestamps VALUES(1,91,'scheduled',1732060800,NULL,'active','none','<2024-11-20 Wed>',93,109,7);
INSERT INTO timestamps VALUES(2,92,'deadline',1733011200,NULL,'active','none','<2024-12-01 Sun>',140,156,10);
INSERT INTO timestamps VALUES(3,93,'closed',1732147200,NULL,'inactive','none','[2024-11-21 Thu]',183,199,13);
INSERT INTO timestamps VALUES(4,94,'deadline',1733011200,NULL,'active','none','<2024-12-01 Sun>',239,255,16);
INSERT INTO timestamps VALUES(5,94,'scheduled',1732060800,NULL,'active','none','<2024-11-20 Wed>',267,283,16);
INSERT INTO timestamps VALUES(6,94,'closed',1732147200,NULL,'inactive','none','[2024-11-21 Thu]',292,308,16);
INSERT INTO timestamps VALUES(7,95,'scheduled',1732095000,NULL,'active','none','<2024-11-20 Wed 09:30>',334,356,19);
INSERT INTO timestamps VALUES(8,96,'scheduled',1732095000,1732100400,'active','time_range','<2024-11-20 Wed 09:30-11:00>',392,420,22);
INSERT INTO timestamps VALUES(9,97,'deadline',1733011200,1733184000,'active','date_range','<2024-12-01 Sun>--<2024-12-03 Tue>',446,480,25);
INSERT INTO timestamps VALUES(10,98,'scheduled',1732060800,NULL,'active','none','<2024-11-20 Wed +1w>',505,525,28);
INSERT INTO timestamps VALUES(11,99,'body',NULL,NULL,'diary','none','<%%(diary-float t 42)>',558,580,31);
INSERT INTO timestamps VALUES(12,100,'scheduled',1732060800,NULL,'active','none','<2024-11-20 Wed>',618,634,34);
INSERT INTO timestamps VALUES(13,100,'scheduled',1732147200,NULL,'active','none','<2024-11-21 Thu>',646,662,34);
INSERT INTO timestamps VALUES(14,102,'scheduled',1732060800,NULL,'active','none','<2024-11-20 Wed>',757,773,40);
INSERT INTO timestamps VALUES(15,102,'body',1733011200,NULL,'active','none','<2024-12-01 Sun>',784,800,41);
INSERT INTO timestamps VALUES(16,102,'body',1732147200,NULL,'inactive','none','[2024-11-21 Thu]',809,825,42);
INSERT INTO timestamps VALUES(17,103,'body',1732060800,NULL,'active','none','<2024-11-20 Wed>',904,920,46);
INSERT INTO timestamps VALUES(18,104,'body',1733011200,NULL,'active','none','<2024-12-01 Sun>',978,994,49);
INSERT INTO timestamps VALUES(19,105,'body',1732060800,NULL,'active','none','<2024-11-20 Wed>',1060,1076,52);
INSERT INTO timestamps VALUES(20,106,'scheduled',1732060800,NULL,'active','none','<2024-11-20 Wed>',1115,1131,55);
INSERT INTO timestamps VALUES(21,106,'scheduled',1732147200,NULL,'active','none','<2024-11-21 Thu>',1143,1159,55);
INSERT INTO timestamps VALUES(22,137,'scheduled',1732060800,NULL,'active','none','<2024-11-20 Wed +1w>',48,68,4);
INSERT INTO timestamps VALUES(23,138,'scheduled',1732060800,NULL,'active','none','<2024-11-20 Wed ++1m>',103,124,7);
INSERT INTO timestamps VALUES(24,139,'scheduled',1732060800,NULL,'active','none','<2024-11-20 Wed .+2d>',157,178,10);
INSERT INTO timestamps VALUES(25,141,'scheduled',1732060800,NULL,'active','none','<2024-11-20 Wed +3h>',226,246,15);
INSERT INTO timestamps VALUES(26,142,'scheduled',1732060800,NULL,'active','none','<2024-11-20 Wed +3d>',275,295,18);
INSERT INTO timestamps VALUES(27,143,'scheduled',1732060800,NULL,'active','none','<2024-11-20 Wed +3w>',325,345,21);
INSERT INTO timestamps VALUES(28,144,'scheduled',1732060800,NULL,'active','none','<2024-11-20 Wed +3m>',376,396,24);
INSERT INTO timestamps VALUES(29,145,'scheduled',1732060800,NULL,'active','none','<2024-11-20 Wed +3y>',426,446,27);
INSERT INTO timestamps VALUES(30,147,'scheduled',1732060800,NULL,'active','none','<2024-11-20 Wed +1w/2d>',515,538,32);
INSERT INTO timestamps VALUES(31,148,'scheduled',1732060800,NULL,'active','none','<2024-11-20 Wed ++1m/1w>',582,606,35);
INSERT INTO timestamps VALUES(32,149,'scheduled',1732060800,NULL,'active','none','<2024-11-20 Wed .+1y/2m>',651,675,38);
INSERT INTO timestamps VALUES(33,151,'deadline',1733011200,NULL,'active','none','<2024-12-01 Sun -5d>',720,740,43);
INSERT INTO timestamps VALUES(34,152,'deadline',1733011200,NULL,'active','none','<2024-12-01 Sun --2w>',769,790,46);
INSERT INTO timestamps VALUES(35,153,'deadline',1733045400,NULL,'active','none','<2024-12-01 Sun 09:30 -3h>',818,844,49);
INSERT INTO timestamps VALUES(36,154,'deadline',1733011200,NULL,'active','none','<2024-12-01 Sun -1m>',873,893,52);
INSERT INTO timestamps VALUES(37,155,'deadline',1733011200,NULL,'active','none','<2024-12-01 Sun --1y>',921,942,55);
INSERT INTO timestamps VALUES(38,157,'deadline',1733011200,NULL,'active','none','<2024-12-01 Sun +1w -5d>',1015,1039,60);
INSERT INTO timestamps VALUES(39,158,'deadline',1733011200,NULL,'active','none','<2024-12-01 Sun ++1m/2d -5d>',1103,1131,63);
INSERT INTO timestamps VALUES(40,159,'deadline',1733011200,NULL,'active','none','<2024-12-01 Sun .+2w --1w>',1182,1208,66);
INSERT INTO timestamps VALUES(41,161,'scheduled',1732095000,NULL,'active','none','<2024-11-20 Wed 09:30 +1w>',1274,1300,71);
INSERT INTO timestamps VALUES(42,162,'scheduled',1732095000,1732100400,'active','time_range','<2024-11-20 Wed 09:30-11:00 +1w>',1341,1373,74);
INSERT INTO timestamps VALUES(43,163,'deadline',1733011200,1733184000,'active','date_range','<2024-12-01 Sun>--<2024-12-03 Tue +1w>',1413,1451,77);
INSERT INTO timestamps VALUES(44,165,'body',1732060800,NULL,'inactive','none','[2024-11-20 Wed +1w]',1510,1530,82);
INSERT INTO timestamps VALUES(45,167,'body',NULL,NULL,'diary','none','<%%(diary-float t 42)>',1603,1625,87);
INSERT INTO timestamps VALUES(46,169,'body',1162408500,NULL,'active','none','<2006-11-01 Wed 19:15>',67,89,5);
INSERT INTO timestamps VALUES(47,170,'body',1162461600,1162468800,'active','time_range','<2006-11-02 Thu 10:00-12:00>',122,150,8);
INSERT INTO timestamps VALUES(48,171,'body',1162512000,NULL,'active','none','<2006-11-03 Fri>',166,182,11);
INSERT INTO timestamps VALUES(49,171,'body',1162771200,NULL,'active','none','<2006-11-06 Mon>',183,199,12);
INSERT INTO timestamps VALUES(50,172,'body',1162512000,NULL,'active','none','<2006-11-03 Fri>',237,253,16);
INSERT INTO timestamps VALUES(51,172,'body',1782086400,NULL,'inactive','none','[2026-06-22 Mon]',281,297,18);
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
INSERT INTO keywords VALUES(28,25,'TITLE','Angle Link Fixture',1);
INSERT INTO keywords VALUES(29,25,'STARTUP','content',2);
INSERT INTO keywords VALUES(30,38,'TITLE','Bracket Link Fixture',1);
INSERT INTO keywords VALUES(31,38,'STARTUP','content',2);
INSERT INTO keywords VALUES(32,39,'TITLE','Bracket Link Fixture',1);
INSERT INTO keywords VALUES(33,39,'STARTUP','content',2);
INSERT INTO keywords VALUES(34,57,'TITLE','Ignored Region And Source Context Link Fixture',1);
INSERT INTO keywords VALUES(35,57,'STARTUP','showall',2);
INSERT INTO keywords VALUES(36,57,'TITLE','https://example.org/in-title-keyword',157);
INSERT INTO keywords VALUES(37,57,'AUTHOR','https://example.org/in-author-keyword',158);
INSERT INTO keywords VALUES(38,57,'PROPERTY','link_to_ignore https://example.org/in-property-keyword',159);
INSERT INTO keywords VALUES(39,57,'PROPERTY','bracket_to_ignore [[https://example.org/bracket-in-property-keyword][ignored]]',160);
INSERT INTO keywords VALUES(40,57,'PROPERTY','angle_to_ignore <https://example.org/angle-in-property-keyword>',161);
INSERT INTO keywords VALUES(41,68,'TITLE','Plain Link Fixture',1);
INSERT INTO keywords VALUES(42,68,'STARTUP','showall',2);
INSERT INTO keywords VALUES(43,68,'PROPERTY','link_to_ignore https://example.org/property-keyword',252);
INSERT INTO keywords VALUES(44,87,'TITLE','Title can span',1);
INSERT INTO keywords VALUES(45,87,'TITLE','multiple lines,',2);
INSERT INTO keywords VALUES(46,87,'AUTHOR','Hubisan',3);
INSERT INTO keywords VALUES(47,87,'TITLE','even here',9);
INSERT INTO keywords VALUES(48,89,'TITLE','Planning timestamp',1);
INSERT INTO keywords VALUES(49,89,'STARTUP','showall',2);
INSERT INTO keywords VALUES(50,107,'TITLE','Org Property and Keyword Test',6);
INSERT INTO keywords VALUES(51,107,'STARTUP','showall',7);
INSERT INTO keywords VALUES(52,107,'CATEGORY','category_keyword_value',8);
INSERT INTO keywords VALUES(53,107,'PROPERTY','Effort_ALL 0:10 0:30 1:00',9);
INSERT INTO keywords VALUES(54,107,'PROPERTY','keyword_property valid',10);
INSERT INTO keywords VALUES(55,107,'PROPERTY','keyword_overwritten_by_second invalid',11);
INSERT INTO keywords VALUES(56,107,'PROPERTY','keyword_overwritten_by_second valid',12);
INSERT INTO keywords VALUES(57,107,'PROPERTY','keyword_append foo=1',13);
INSERT INTO keywords VALUES(58,107,'PROPERTY','keyword_append+ bar=2',14);
INSERT INTO keywords VALUES(59,107,'PROPERTY','later_keyword_property works_everywhere',80);
INSERT INTO keywords VALUES(60,107,'CATEGORY','later_category_keyword',81);
INSERT INTO keywords VALUES(61,107,'FILETAGS',':project:work:',90);
INSERT INTO keywords VALUES(62,107,'TAGS','work(w) home(h)',91);
INSERT INTO keywords VALUES(63,107,'COLUMNS','%TODO %50ITEM %Effort{:} %CLOCKSUM',92);
INSERT INTO keywords VALUES(64,107,'CONSTANTS','c=299792458',93);
INSERT INTO keywords VALUES(65,107,'AUTHOR','Jane Doe',94);
INSERT INTO keywords VALUES(66,107,'OPTIONS','toc:nil num:t',95);
INSERT INTO keywords VALUES(67,117,'TITLE','Tags and FILETAGS Fixture',1);
INSERT INTO keywords VALUES(68,117,'STARTUP','showall',2);
INSERT INTO keywords VALUES(69,117,'FILETAGS',':file:project:',3);
INSERT INTO keywords VALUES(70,117,'FILETAGS',':later:extra:',20);
INSERT INTO keywords VALUES(71,168,'TITLE','Timestamps',1);
INSERT INTO keywords VALUES(72,168,'STARTUP','showall',2);
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
INSERT INTO properties VALUES(9,40,'CUSTOM_ID','internal-link-to-custom-id','property_drawer',0,12);
INSERT INTO properties VALUES(10,57,'LINK_TO_IGNORE','https://example.org/in-property-keyword','property_keyword',0,159);
INSERT INTO properties VALUES(11,57,'BRACKET_TO_IGNORE','[[https://example.org/bracket-in-property-keyword][ignored]]','property_keyword',0,160);
INSERT INTO properties VALUES(12,57,'ANGLE_TO_IGNORE','<https://example.org/angle-in-property-keyword>','property_keyword',0,161);
INSERT INTO properties VALUES(13,68,'LINK_TO_IGNORE','https://example.org/property-keyword','property_keyword',0,252);
INSERT INTO properties VALUES(14,107,'CATEGORY','Level 0 Category Property','property_drawer',0,2);
INSERT INTO properties VALUES(15,107,'WHATEVER','level 0 drawer property','property_drawer',0,3);
INSERT INTO properties VALUES(16,107,'ID','7dad9b62-a3cc-43ec-a60f-e650bdaeae6d','property_drawer',0,4);
INSERT INTO properties VALUES(17,107,'CATEGORY','category_keyword_value','category_keyword',0,8);
INSERT INTO properties VALUES(18,107,'EFFORT_ALL','0:10 0:30 1:00','property_keyword',0,9);
INSERT INTO properties VALUES(19,107,'KEYWORD_PROPERTY','valid','property_keyword',0,10);
INSERT INTO properties VALUES(20,107,'KEYWORD_OVERWRITTEN_BY_SECOND','invalid','property_keyword',0,11);
INSERT INTO properties VALUES(21,107,'KEYWORD_OVERWRITTEN_BY_SECOND','valid','property_keyword',0,12);
INSERT INTO properties VALUES(22,107,'KEYWORD_APPEND','foo=1','property_keyword',0,13);
INSERT INTO properties VALUES(23,107,'KEYWORD_APPEND','bar=2','property_keyword',1,14);
INSERT INTO properties VALUES(24,107,'LATER_KEYWORD_PROPERTY','works_everywhere','property_keyword',0,80);
INSERT INTO properties VALUES(25,107,'CATEGORY','later_category_keyword','category_keyword',0,81);
INSERT INTO properties VALUES(26,108,'ID','abc','property_drawer',0,18);
INSERT INTO properties VALUES(27,108,'CUSTOM_ID','task-custom-id','property_drawer',0,19);
INSERT INTO properties VALUES(28,108,'EFFORT','0:30','property_drawer',0,20);
INSERT INTO properties VALUES(29,108,'OWNER','Alice','property_drawer',0,21);
INSERT INTO properties VALUES(30,108,'DRAWER_PROP','valid','property_drawer',0,22);
INSERT INTO properties VALUES(31,109,'DEFINED_TWICE','invalid','property_drawer',0,28);
INSERT INTO properties VALUES(32,109,'DEFINED_TWICE','valid','property_drawer',0,29);
INSERT INTO properties VALUES(33,110,'ADD-VALUE','is','property_drawer',0,37);
INSERT INTO properties VALUES(34,110,'ADD-VALUE','valid','property_drawer',1,38);
INSERT INTO properties VALUES(35,111,'ID','lowercase-id','property_drawer',0,47);
INSERT INTO properties VALUES(36,111,'CUSTOM_ID','mixed-case-custom-id','property_drawer',0,48);
INSERT INTO properties VALUES(37,111,'DRAWER_PROP','valid','property_drawer',0,49);
INSERT INTO properties VALUES(38,111,'ADD-VALUE','appended','property_drawer',1,50);
INSERT INTO properties VALUES(39,112,'EMPTY','','property_drawer',0,60);
CREATE TABLE tags (
    heading_id      INTEGER NOT NULL,
    tag             TEXT NOT NULL,
    FOREIGN KEY (heading_id)
        REFERENCES headings(id)
        ON DELETE CASCADE,
    PRIMARY KEY (heading_id, tag)
);
INSERT INTO tags VALUES(37,'foo');
INSERT INTO tags VALUES(37,'bar');
INSERT INTO tags VALUES(56,'foo');
INSERT INTO tags VALUES(56,'bar');
INSERT INTO tags VALUES(86,'foo');
INSERT INTO tags VALUES(86,'bar');
INSERT INTO tags VALUES(107,'project');
INSERT INTO tags VALUES(107,'work');
INSERT INTO tags VALUES(117,'file');
INSERT INTO tags VALUES(117,'project');
INSERT INTO tags VALUES(117,'later');
INSERT INTO tags VALUES(117,'extra');
INSERT INTO tags VALUES(118,'parent');
INSERT INTO tags VALUES(119,'child');
INSERT INTO tags VALUES(120,'project');
INSERT INTO tags VALUES(120,'grandchild');
INSERT INTO tags VALUES(122,'file');
INSERT INTO tags VALUES(122,'sibling');
INSERT INTO tags VALUES(123,'after');
INSERT INTO tags VALUES(124,'parent');
INSERT INTO tags VALUES(126,'child');
INSERT INTO tags VALUES(128,'parent');
INSERT INTO tags VALUES(129,'second');
INSERT INTO tags VALUES(130,'child');
INSERT INTO tags VALUES(131,'extra');
INSERT INTO tags VALUES(133,'local');
CREATE TABLE heading_bodies (
    heading_id          INTEGER PRIMARY KEY,
    body_text           TEXT NOT NULL,
    body_byte_start     INTEGER,
    body_byte_end       INTEGER,
    FOREIGN KEY (heading_id)
        REFERENCES headings(id)
        ON DELETE CASCADE
);
INSERT INTO heading_bodies VALUES(1,'See [[file:../../notes/org-semantics/file-local-todo-keywords.org]]',164,231);
INSERT INTO heading_bodies VALUES(2,'Default TODO is not valid because file-local TODO lines override defaults.',278,352);
INSERT INTO heading_bodies VALUES(3,'Default DONE is not valid because file-local TODO lines override defaults.',403,477);
INSERT INTO heading_bodies VALUES(20,'This heading has body text before later keywords.',163,212);
INSERT INTO heading_bodies VALUES(21,'This child should not directly receive keyword rows.',471,523);
INSERT INTO heading_bodies VALUES(22,'This heading appears after later keywords.',626,668);
INSERT INTO heading_bodies VALUES(23,unistr('This line mentions #+TITLE: Inline Mention but should only become a keyword row if Orgize exposes it as a keyword node.\u000aThis line mentions #+PROPERTY: inline_prop invalid in prose.\u000a\u000a#+BEGIN_EXAMPLE\u000a#+TITLE: Example Block Title\u000a#+PROPERTY: example_prop invalid\u000a#+CATEGORY: example-category\u000a#+END_EXAMPLE\u000a\u000a#+begin_src org\u000a  ,#+TITLE: Source Block Title\u000a  ,#+PROPERTY: source_prop invalid\u000a  ,#+CATEGORY: source-category\u000a#+end_src'),836,1262);
INSERT INTO heading_bodies VALUES(24,unistr('- All real keyword nodes exposed by Orgize are stored as raw ~keywords~ rows attached to the level 0 heading.\u000a- Keyword rows are not attached to regular headings.\u000a- Duplicate keyword rows are preserved.\u000a- Source order is preserved with ~line_number~ and/or insertion order.\u000a- Generic keywords such as ~TITLE~, ~AUTHOR~, ~STARTUP~, ~OPTIONS~, and ~EXPORT_FILE_NAME~ remain raw keyword rows only.\u000a- ~TODO~, ~SEQ_TODO~, and ~TYP_TODO~ may additionally create normalized ~todo_keywords~ rows if that normalization is in scope.\u000a- ~PROPERTY~ rows may additionally create normalized ~properties~ rows with ~source = property_keyword~ if that normalization is in scope.\u000a- ~CATEGORY~ rows may additionally create normalized ~properties~ rows with ~source = category_keyword~ if that normalization is in scope.\u000a- Keywords inside example/source blocks must not create keyword rows unless Orgize incorrectly exposes them as keyword nodes; if that happens, document the Orgize behavior as a parser risk.'),1285,2275);
INSERT INTO heading_bodies VALUES(25,unistr('# Angle links before the first real heading should attach to synthetic root.\u000a<FILE:root-angle.org::42>\u000a<root:target with spaces>\u000a[[root:target with spaces]]\u000ahttps://example.org/root-plain-should-not-be-stored'),48,256);
INSERT INTO heading_bodies VALUES(27,unistr('- <https://example.com/some path with spaces>\u000a- <https://example.org/ spaces >\u000a- <info:org#External Link>\u000a- <mailto:emacs-orgmode@gnu.org>'),307,445);
INSERT INTO heading_bodies VALUES(28,unistr('- <file:~/code/main.c::255>\u000a- <file:~/xx.org::*My Target>\u000a- <file:~/xx.org::#my-custom-id>\u000a- <file:~/xx.org::/regexp/>\u000a- <file:::find me>\u000a- <file:::*Current File Heading>'),492,662);
INSERT INTO heading_bodies VALUES(29,unistr('- <file+sys:~/code/main.c::255>\u000a- <file+sys:~/xx.org::*My Target>\u000a- <file+emacs:~/code/main.c::255>\u000a- <file+emacs:~/xx.org::#my-custom-id>'),712,850);
INSERT INTO heading_bodies VALUES(30,unistr('- <unknown:foo>\u000a- <jira:ABC-123>\u000a- <customlink:test>\u000a- <doi:10.1000/182>\u000a- <irc:/irc.com/#emacs/bob>'),894,994);
INSERT INTO heading_bodies VALUES(31,unistr('These must be stored as raw source facts only. They must not be executed.\u000a\u000a- <shell:ls *.org>\u000a- <elisp:(find-file "~/.emacs.d/init.el")>'),1024,1160);
INSERT INTO heading_bodies VALUES(32,unistr('These should keep the full path and should not split ~::~ in Phase 3.\u000a\u000a- <id:16ccfc6a-11ba-499f-8bc6-41be30daa3c5::10>\u000a- <attachment:projects.org::10>\u000a- <docview:papers/last.pdf::12>\u000a- <unknown:file.org::10>\u000a- <customlink:file.org::10>'),1208,1443);
INSERT INTO heading_bodies VALUES(33,unistr('Results in an angled link as brackets are escaped:\u000a[[<https://www.gnu.org>\\][Test]]\u000a\u000aOr with a new line\u000a[[<https://www.gnu.org>]\u000a[Test]]\u000a\u000aBut this should be a bracket link:\u000a[[<https://www.gnu.org>][<https://www.gnu.org>]]'),1456,1677);
INSERT INTO heading_bodies VALUES(34,unistr('These should not be stored as angle links.\u000a\u000a- <https://example.com\u000a- <unknown:unterminated\u000a- <file:~/broken.org::10\u000a\u000aThis multiline candidate should also be ignored:\u000a\u000a<https://example.org\u000a  path with newline>\u000a\u000aGarbage before a later valid angle link:\u000a<broken\u000a<https://example.org/later-valid>\u000a<broken <https://example.org/later-valid>'),1708,2042);
INSERT INTO heading_bodies VALUES(35,unistr('These should remain out of scope until plain-link storage is implemented.\u000a\u000a- https://example.org/plain\u000a- file:~/code/main.c::255\u000a- shell:ls *.org\u000a- jira:ABC-123'),2084,2244);
INSERT INTO heading_bodies VALUES(36,'Text with: äöü ÄÖÜ 😀😇🤖',2269,2306);
INSERT INTO heading_bodies VALUES(37,unistr('- <https://orgmode.org/🔥 path with spaces>\u000a- <file:sub/äöü.txt::target>\u000a- <unknown:äöü-😀>'),2389,2490);
INSERT INTO heading_bodies VALUES(38,unistr('This results in a plain link as the \\ needs to be escaped. So there is no actual ][ to make the link valid:\u000a- [[https://nok-no-bracket-link--plain-link.org\\][Test]]\u000a\u000aEven number of backslashes don''t escape and are ignored:\u000a- [[https://ok-even-number-of-backslashes.org\\\\][OK]]\u000a\u000aBrackets in the link are valid if escaped with uneven number of backslashs:\u000a- [[https://ok-escaped-brackets-in-path.org\\]\\[brackets need to be escaped \\[\\] in the path\\]][OK]]\u000a- [[https://ok-escaped-brackets-in-path.org\\\\\\]\\\\\\[brackets need to be escaped \\[\\\\\\] in the path\\]][OK]]\u000a\u000aIn Description the escaping is not needed. As nested links are not allowed. Two brackets after another close the description.\u000a- [[https://ok-description-double-bracket-closes-link][[Test]]]]\u000a- [[https://ok-description-double-bracket-closes-link][[[[[[Test]]]]  \u000a- [[https://ok-description-double-bracket-closes-link][[][][]]]]\u000a- [[https://ok-description-double-bracket-closes-link][test [[https://test.com]]no more link\u000a\u000aLine breaks are =NOT= (yet) supported, or maybe never. Not sure there is a use case for having line breaks in links:\u000a\u000aBracket links and the description can include new lines, the starting. Only inside [], not when splitting \\[\\[ or \\]\\[ or \\]\\].\u000a[[https://www.gnu.org\u000a][Link with new line]]\u000a\u000aIn the path a newline is represented as a space.\u000a[[https://www.gnu.org\u000a][sdfdsf]]\u000a\u000aIndentation on the second line is ignored.\u000a[[https://www.gnu.org\u000a                 x    x][sdfdsf]]\u000a\u000aNoch ein Umlaut im Path:\u000a[[file:sub/äöü.txt::target][A file link with Umlaut]]'),50,1588);
INSERT INTO heading_bodies VALUES(39,unistr('# Links before the first real heading should attach to synthetic root.\u000a[[FILE:root-notes.org::42]]\u000a[[root target][root description]]\u000ahttps://example.org/root-plain-should-not-be-stored\u000a<https://example.org/root-angle-should-not-be-stored>'),50,288);
INSERT INTO heading_bodies VALUES(41,unistr('- [[#internal-link-to-custom-id]]\u000a- [[#internal-link-to-custom-id][description: internal-link-to-custom-id]]'),393,501);
INSERT INTO heading_bodies VALUES(42,unistr('- [[*Internal bracket links]]\u000a- [[*Internal bracket links][description: heading link]]'),526,612);
INSERT INTO heading_bodies VALUES(43,unistr('<<dedicated target>>\u000a- [[dedicated target]]\u000a- [[dedicated target][description: dedicated target]]'),640,737);
INSERT INTO heading_bodies VALUES(44,unistr('#+NAME: named target\u000a- [[named target]]\u000a- [[named target][description: named target]]'),773,858);
INSERT INTO heading_bodies VALUES(45,unistr('- [[no matching target]]\u000a- [[no matching target][description: no matching target]]\u000a- [[notes.org]]'),878,976);
INSERT INTO heading_bodies VALUES(47,unistr('- [[file:/etc]]\u000a- [[file:/etc][description: /etc]]\u000a- [[file:/etc/]]\u000a- [[file:/etc/host.conf]]\u000a- [[file:../]]\u000a- [[file:../../sql]]\u000a- [[file:../../sql/]]\u000a- [[file:../parser_test.rs]]\u000a- [[file:./org-test-links.org]]\u000a- [[file:~/.emacs.d]]\u000a- [[file:~/.emacs.d/]]\u000a- [[file:~/.emacs.d/init.el]]'),1028,1315);
INSERT INTO heading_bodies VALUES(48,unistr('- [[file+sys:/etc]]\u000a- [[file+emacs:/etc]]\u000a- [[file+sys:/etc][description: file+sys:/etc]]\u000a- [[file+emacs:/etc][description: file+emacs:/etc]]'),1343,1484);
INSERT INTO heading_bodies VALUES(49,unistr('- [[/etc]]\u000a- [[/etc/]]\u000a- [[/etc/host.conf]]\u000a- [[../]]\u000a- [[../../sql]]\u000a- [[../../sql/]]\u000a- [[../parser_test.rs]]\u000a- [[./org-test-links.org]]\u000a- [[~/memento]]'),1509,1662);
INSERT INTO heading_bodies VALUES(50,unistr('- [[file:./org-test-links.org::10]]\u000a- [[file:./org-test-links.org::#internal-link-to-custom-id]]\u000a- [[file:./org-test-links.org::dedicated target]]\u000a- [[file:./org-test-links.org::*Internal bracket links]]\u000a- [[file:./org-test-links.org::/*.File-like.*/]]\u000a- [[./org-test-links.org::10]]\u000a- [[./org-test-links.org::#internal-link-to-custom-id]]\u000a- [[./org-test-links.org::dedicated target]]\u000a- [[./org-test-links.org::*Internal bracket links]]\u000a- [[./org-test-links.org::/*.File-like.*/]]\u000a- [[file:::10]]\u000a- [[file:::*Internal bracket links]]'),1687,2220);
INSERT INTO heading_bodies VALUES(52,unistr('- [[http://orgmode.org]]\u000a- [[http://orgmode.org][description: http://orgmode.org]]\u000a- [[https://orgmode.org]]\u000a- [[https://orgmode.org][description: https://orgmode.org]]\u000a- [[news:comp.emacs]]\u000a- [[mailto:emacs-orgmode@gnu.org]]\u000a- [[help:org-store-link]]\u000a- [[info:org#External Link]]\u000a- [[shell:ls *.org  ]]\u000a- [[elisp:(find-file "~/.emacs.d/init.el")]]'),2269,2617);
INSERT INTO heading_bodies VALUES(53,unistr('- [[unknown:foo]]\u000a- [[unknown:foo][description: unknown type]]\u000a- [[customlink:test]]\u000a- [[customlink:test][description: customlink:test]]\u000a- [[doi:10.1000/182]]\u000a- [[irc:/irc.com/#emacs/bob]]'),2653,2841);
INSERT INTO heading_bodies VALUES(54,unistr('These should not be stored by the current bracket-link storage task.\u000a\u000a- http://orgmode.org\u000a- https://orgmode.org\u000a- mailto:emacs-orgmode@gnu.org\u000a- file:./org-test-links.org::10\u000a- <file:::*Negative non-bracket examples>\u000a- <shell:ls *.org>\u000a- <https://orgmode.org/ spaces >'),2876,3145);
INSERT INTO heading_bodies VALUES(55,'Text with: äöü ÄÖÜ 😀😇🤖',3172,3209);
INSERT INTO heading_bodies VALUES(56,unistr('Some body text with bracket links:\u000a\u000a- [[file:sub/äöü.txt::target][A file link with Umlaut]]\u000a- [[https://orgmode.org/🔥]]\u000a- [[https://orgmode.org/🔥][Emoji description 😀]]\u000a- [[dedicated target äöü]]'),3292,3502);
INSERT INTO heading_bodies VALUES(57,unistr('# Fixture for Phase 3 ignored-region filtering and source_context assignment.\u000a# Expected behavior:\u000a# - Links in ignored regions are not stored.\u000a# - Links in parsed regions are stored with the expected source_context.\u000a# - Link raw text, byte ranges, heading/root attachment, and Phase 4 resolution fields remain unchanged.\u000a# - Inline source/export are ignored only when Orgize exposes reliable INLINE_SRC / SNIPPET nodes.\u000a\u000ahttps://example.org/root-normal'),76,529);
INSERT INTO heading_bodies VALUES(58,'Paragraph with https://example.org/in-normal-paragraph.',584,639);
INSERT INTO heading_bodies VALUES(59,unistr('#+BEGIN_VERSE\u000ahttps://example.org/in-verse-block\u000a[[https://example.org/bracket-in-verse][bracket in verse]]\u000a<https://example.org/angle-in-verse>\u000a#+END_VERSE\u000a\u000a#+BEGIN_QUOTE\u000ahttps://example.org/in-quote-block\u000a[[https://example.org/bracket-in-quote][bracket in quote]]\u000a<https://example.org/angle-in-quote>\u000a#+END_QUOTE\u000a\u000a#+BEGIN_CENTER\u000ahttps://example.org/in-center-block\u000a[[https://example.org/bracket-in-center][bracket in center]]\u000a<https://example.org/angle-in-center>\u000a#+END_CENTER\u000a\u000a#+BEGIN_JUSTIFY\u000ahttps://example.org/in-justify-block\u000a[[https://example.org/bracket-in-justify][bracket in justify]]\u000a<https://example.org/angle-in-justify>\u000a#+END_JUSTIFY\u000a\u000a#+BEGIN_SPECIAL\u000ahttps://example.org/in-other-special-block\u000a[[https://example.org/bracket-in-other-special][bracket in other special]]\u000a#+END_SPECIAL\u000a\u000aExpected source_context:\u000a\u000a- Links in ~VERSE~ use ~verse_block~.\u000a- Links in ~QUOTE~ use ~quote_block~.\u000a- Links in ~CENTER~ use ~center_block~.\u000a- Links in ~JUSTIFY~ use ~justify_block~.\u000a- Links in other special blocks stay ~normal~ unless a later contract adds a specific context.'),667,1744);
INSERT INTO heading_bodies VALUES(60,unistr(':PROPERTIES:\u000a:link_to_parse_1: https://example.org/in-property-drawer\u000a:link_to_parse_2: [[https://example.org/bracket-in-property-drawer][property drawer bracket]]\u000a:link_to_parse_3: <https://example.org/angle-in-property-drawer>\u000a:END:\u000a\u000a:A_DRAWER:\u000ahttps://example.org/in-normal-drawer\u000a[[https://example.org/bracket-in-normal-drawer][normal drawer bracket]]\u000a<https://example.org/angle-in-normal-drawer>\u000a:END:\u000a\u000aExpected source_context:\u000a\u000a- Links inside ~:PROPERTIES:~ use ~property_drawer~.\u000a- Links inside normal drawers use ~drawer~.\u000a- Phase 3 does not store drawer names or property names.'),1766,2353);
INSERT INTO heading_bodies VALUES(61,unistr('These links should not be stored.\u000a\u000aInside verbatim markup: ~https://example.org/in-verbatim-markup~\u000aInside code markup: =https://example.org/in-code-markup=\u000a\u000aMixed line with allowed link https://example.org/outside-markup and ignored ~https://example.org/verbatim-same-line~.\u000aMixed line with allowed link https://example.org/outside-code and ignored =https://example.org/code-same-line=.'),2381,2768);
INSERT INTO heading_bodies VALUES(62,unistr('These links should not be stored when Orgize exposes reliable inline nodes.\u000a\u000aInline source form: src_elisp{https://example.org/in-inline-source}\u000aInline source with bracket: src_elisp{[[https://example.org/bracket-in-inline-source][ignored]]}\u000aInline export form: @@html:https://example.org/in-inline-export@@\u000aInline export with bracket text: @@html:[[https://example.org/bracket-in-inline-export][ignored]]@@\u000a\u000aA normal link on the same section should still be stored: https://example.org/after-inline-forms'),2807,3312);
INSERT INTO heading_bodies VALUES(63,unistr('Links in source blocks should not be stored.\u000a\u000a#+BEGIN_SRC sh\u000ahttps://example.org/in-source-block\u000a[[https://example.org/bracket-in-source][ignored]]\u000a<https://example.org/angle-in-source>\u000afile:in-source-block.org\u000a#+END_SRC\u000a\u000aLinks in example blocks should not be stored.\u000a\u000a#+BEGIN_EXAMPLE\u000ahttps://example.org/in-example-block\u000a[[https://example.org/bracket-in-example][ignored]]\u000a<https://example.org/angle-in-example>\u000afile:in-example-block.org\u000a#+END_EXAMPLE\u000a\u000aLinks in export blocks should not be stored.\u000a\u000a#+BEGIN_EXPORT HTML\u000ahttps://example.org/in-export-block\u000a[[https://example.org/bracket-in-export][ignored]]\u000a<https://example.org/angle-in-export>\u000afile:in-export-block.org\u000a#+END_EXPORT\u000a\u000aLinks in comment blocks should not be stored.\u000a\u000a#+BEGIN_COMMENT\u000ahttps://example.org/in-comment-block\u000a[[https://example.org/bracket-in-comment-block][ignored]]\u000a<https://example.org/angle-in-comment-block>\u000afile:in-comment-block.org\u000a#+END_COMMENT'),3340,4266);
INSERT INTO heading_bodies VALUES(64,unistr('Colon example lines should not be stored.\u000a\u000a: https://example.org/in-colon-example-line\u000a: [[https://example.org/bracket-in-colon-example][ignored]]\u000a: <https://example.org/angle-in-colon-example>\u000a: file:colon-example-line.org\u000a\u000aComment lines should not be stored.\u000a\u000a# https://example.org/in-comment-line\u000a# [[https://example.org/bracket-in-comment-line][ignored]]\u000a# <https://example.org/angle-in-comment-line>\u000a# file:comment-line.org\u000a\u000aA normal paragraph link after comments should still be stored: https://example.org/after-comment-lines'),4310,4842);
INSERT INTO heading_bodies VALUES(65,unistr('Links in file-level keywords should not be stored.\u000a\u000aA normal paragraph link after keywords should still be stored: https://example.org/after-keywords'),NULL,NULL);
INSERT INTO heading_bodies VALUES(66,unistr('Ignored inline markup should win over normal paragraph context.\u000a\u000aNormal before markup https://example.org/before-verbatim then ~https://example.org/ignored-verbatim-overlap~ and after markup https://example.org/after-verbatim.\u000a\u000aProperty drawer context should apply only to accepted links, not ignored markup inside the drawer.\u000a\u000a:PROPERTIES:\u000a:normal_property_link: https://example.org/property-drawer-context-link\u000a:ignored_property_code: =https://example.org/ignored-code-inside-property-drawer=\u000a:END:\u000a\u000aNormal drawer context should apply only to accepted links, not ignored markup inside the drawer.\u000a\u000a:ANOTHER_DRAWER:\u000ahttps://example.org/drawer-context-link\u000a~https://example.org/ignored-verbatim-inside-drawer~\u000a:END:'),5395,6110);
INSERT INTO heading_bodies VALUES(67,unistr('These links should be stored with their usual path/search_option behavior and the surrounding source_context.\u000a\u000aParagraph file link: file:./ignored-regions.org::10\u000a\u000a#+BEGIN_QUOTE\u000afile:./ignored-regions.org::*Quoted Target\u000a#+END_QUOTE\u000a\u000a:PROPERTIES:\u000a:file_property_link: file:./ignored-regions.org::#property-target\u000a:END:\u000a\u000a#+BEGIN_SRC sh\u000afile:./ignored-regions.org::ignored-source-search-option\u000a#+END_SRC\u000a\u000aExpected:\u000a\u000a- Paragraph file link uses ~source_context = normal~.\u000a- Quote file link uses ~source_context = quote_block~.\u000a- Property drawer file link uses ~source_context = property_drawer~.\u000a- Source block file link is ignored.\u000a- Search-option splitting remains unchanged.'),6170,6843);
INSERT INTO heading_bodies VALUES(68,unistr('# Plain links before the first real heading should attach to synthetic root.\u000a\u000aAdd this to config.toml:\u000a\u000a#+BEGIN_SRC conf-toml\u000a  [links]\u000a  custom_protocols = ["jira", "customlink"]\u000a#+END_SRC\u000a\u000ahttps://root.example.org\u000afile:root-notes.org\u000aFILE:root-notes.org\u000ajira:ROOT-123\u000ainvalid:should-not-work\u000a\u000a# Bracket and angle links should be unaffected by plain-link config.\u000a[[jira:ROOT-123]]\u000a<jira:ROOT-123>'),48,445);
INSERT INTO heading_bodies VALUES(70,unistr('These should be stored with the default Phase 3 plain-link protocol list.\u000a\u000a- http://orgmode.org\u000a- https://orgmode.org\u000a- file:notes.org\u000a- file+sys:/etc/hosts\u000a- file+emacs:~/.emacs.d/init.el\u000a- ftp://example.org/pub/file.txt\u000a- attachment:projects.org\u000a- bbdb:R.*Stallman\u000a- docview:papers/last.pdf\u000a- doi:10.1000/182\u000a- gnus:group\u000a- rmail:folder#id\u000a- mhe:folder#id\u000a- help:org-store-link\u000a- id:abc123\u000a- info:org#External-Links\u000a- irc:/irc.com/#emacs/bob\u000a- mailto:person@example.org\u000a- news:comp.emacs\u000a- shortdoc:text-properties'),490,1006);
INSERT INTO heading_bodies VALUES(71,unistr('These should be stored only when configured through ~custom_protocols~.\u000a\u000a- jira:ABC-123\u000a- jira:PROJECT-999\u000a- customlink:test\u000a- customlink:with/slash/path'),1035,1188);
INSERT INTO heading_bodies VALUES(72,unistr('These should not be stored by default.\u000a\u000a- shell:ls\u000a- shell:ls *.org\u000a- elisp:org-todo\u000a- elisp:(find-file "~/.emacs.d/init.el")\u000a\u000aThey may be stored only when explicitly enabled through ~plain_protocols~ or ~custom_protocols~.\u000aEven then, they must never be executed.'),1222,1485);
INSERT INTO heading_bodies VALUES(73,unistr('These should still be stored even if all plain-link extraction is disabled.\u000a\u000a- [[jira:ABC-123]]\u000a- [[shell:ls *.org]]\u000a- [[elisp:(find-file "~/.emacs.d/init.el")]]\u000a- <jira:ABC-123>\u000a- <shell:ls *.org>\u000a- <elisp:(find-file "~/.emacs.d/init.el")>'),1542,1782);
INSERT INTO heading_bodies VALUES(74,unistr('The stored raw span should exclude deterministic trailing punctuation.\u000a\u000a- https://example.org.\u000a- https://example.org,\u000a- https://example.org;\u000a- https://example.org:\u000a- https://example.org!\u000a- https://example.org?\u000a- https://example.org)\u000a- https://example.org]\u000a- https://example.org}\u000a- mailto:person@example.org;\u000a- id:abc123.\u000a- file:notes.org,'),1818,2156);
INSERT INTO heading_bodies VALUES(75,unistr('Representative cases checked against observed ~org-element-parse-buffer~ behavior.\u000aThe project keeps the configured protocol list project-owned, but plain-link boundary and end semantics should stay Org-like where feasible.\u000a\u000aOrg keeps balanced ~(...)~, ~[...]~, and ~<...>~ suffixes as part of the plain-link path.'),2188,2502);
INSERT INTO heading_bodies VALUES(76,unistr('These should store the URL without the prefix punctuation.\u000a\u000a- !https://www.example.com/bang-prefix\u000a- @https://www.example.com/at-prefix\u000a- #https://www.example.com/hash-prefix\u000a- ^https://www.example.com/caret-prefix\u000a- &https://www.example.com/ampersand-prefix\u000a- (https://www.example.com/open-paren-prefix\u000a- )https://www.example.com/close-paren-prefix\u000a- _https://www.example.com/underscore-prefix\u000a- -https://www.example.com/dash-prefix\u000a- =https://www.example.com/equal-prefix\u000a- ~https://www.example.com/tilde-prefix\u000a- +https://www.example.com/plus-prefix\u000a- "https://www.example.com/double-quote-prefix\u000a- Prefix:https://www.example.com/after-colon-prefix'),2545,3196);
INSERT INTO heading_bodies VALUES(77,unistr('These should not be stored as plain links.\u000a\u000a- $https://www.example.com/dollar-prefix\u000a- %https://www.example.com/percent-prefix\u000a- ''https://www.example.com/single-quote-prefix\u000a- xhttps://www.example.com/middle-of-word'),3239,3454);
INSERT INTO heading_bodies VALUES(78,unistr('Plain links should stop at whitespace.\u000a\u000a- https://example.org/path with text after whitespace\u000a\u000aBalanced delimiter groups should remain part of the plain-link path when complete.\u000a\u000a- https://example.org/path<balanced-suffix>\u000a- https://example.org/path(foo)\u000a- https://example.org/path[foo]\u000a- https://example.org/path<balanced-angle-suffix>\u000a\u000aUnbalanced or leading closer delimiter cases should stop before the delimiter.\u000a\u000a- https://example.org/path>not-part-of-plain-link\u000a- https://example.org/path<not-part-of-plain-link\u000a- https://example.org/path)not-part-of-plain-link\u000a- https://example.org/path]not-part-of-plain-link\u000a\u000aCurly braces are not Org plain-link balanced groups. The trailing ~}~ remains deterministic punctuation trimming, not balanced-group handling.\u000a\u000a- https://example.org/path{not-a-balanced-plain-link-group}\u000a- https://example.org/path}'),3508,4358);
INSERT INTO heading_bodies VALUES(79,unistr('Exact emphasis/markup interaction remains deferred to later ignored-region/source-context work.\u000aObserved Org behavior extracts the plain link from this emphasis object:\u000a\u000a- *https://example.org/emphasis-delimited*\u000a\u000aThe final scanner/storage expectation for this case should be pinned when ignored-region/source-context handling is implemented.'),4397,4739);
INSERT INTO heading_bodies VALUES(80,unistr('Deterministic trailing punctuation should be trimmed from the stored raw span.\u000a\u000a- https://example.org/path.\u000a- https://example.org/path,\u000a- https://example.org/path;\u000a- https://example.org/path:\u000a- https://example.org/path!\u000a- https://example.org/path?\u000a\u000aA trailing slash or dash should remain part of the stored raw span.\u000a\u000a- https://example.org/path/\u000a- https://example.org/path-'),4771,5144);
INSERT INTO heading_bodies VALUES(81,unistr('Search-option splitting for plain file-like links may be handled by the dedicated search-option task.\u000aUntil then, these are useful manual cases.\u000a\u000a- file:./plain-links.org::10\u000a- file:./plain-links.org::*Plain links\u000a- file:./plain-links.org::#some-custom-id\u000a- file:./plain-links.org::/regexp/\u000a- file:::current file search\u000a- file+sys:./plain-links.org::10\u000a- file+emacs:./plain-links.org::10\u000a\u000aNon-file-like plain links with ~::~ should not be split in Phase 3.\u000a\u000a- id:abc123::10\u000a- attachment:projects.org::10\u000a- docview:papers/last.pdf::12\u000a- jira:ABC-123::10\u000a- customlink:file.org::10'),5178,5756);
INSERT INTO heading_bodies VALUES(82,unistr('These document forms that should either be ignored or treated conservatively.\u000a\u000a- notaprotocol:value\u000a- root:target-without-custom-config\u000a- shellish:ls\u000a- email@example.org\u000a- C:\\Users\\someone\\notes.org'),5787,5985);
INSERT INTO heading_bodies VALUES(83,unistr('These should not be stored. Ignored-region filtering should suppress them before\u000alink rows are emitted.\u000a\u000aInside verbatim: ~https://example.org/verbatim~\u000aInside code: =https://example.org/code=\u000a\u000a#+BEGIN_SRC sh\u000ahttps://example.org/in-source-block\u000afile:in-source-block.org\u000ajira:IN-SRC-1\u000a#+END_SRC\u000a\u000a#+BEGIN_EXAMPLE\u000ahttps://example.org/in-example-block\u000afile:in-example-block.org\u000a#+END_EXAMPLE\u000a\u000a: https://example.org/colon-example-line\u000a: file:colon-example-line.org\u000a\u000a#+BEGIN_COMMENT\u000ahttps://example.org/in-comment-block\u000afile:in-comment-block.org\u000a#+END_COMMENT\u000a\u000a# https://example.org/comment-line\u000a# file:comment-line.org\u000a\u000a#+BEGIN_EXPORT HTML\u000ahttps://example.org/in-export-block\u000afile:in-export-block.org\u000a#+END_EXPORT\u000a\u000a@@html:https://example.org/inline-export@@'),6005,6757);
INSERT INTO heading_bodies VALUES(84,unistr('These should be parsed according to the Phase 3 parsed-region policy and stored\u000awith the matching ~source_context~.\u000a\u000aParagraph with https://example.org/in-paragraph.\u000a\u000a#+BEGIN_VERSE\u000ahttps://example.org/in-verse-block\u000a#+END_VERSE\u000a\u000a#+BEGIN_QUOTE\u000ahttps://example.org/in-quote-block\u000a#+END_QUOTE\u000a\u000a#+BEGIN_CENTER\u000ahttps://example.org/in-center-block\u000a#+END_CENTER\u000a\u000a#+BEGIN_JUSTIFY\u000ahttps://example.org/in-justify-block\u000a#+END_JUSTIFY\u000a\u000a:PROPERTIES:\u000a:link_to_parse_1: https://example.org/in-property-drawer\u000a:END:\u000a\u000a:A_DRAWER:\u000ahttps://example.org/in-normal-drawer\u000a:END:'),6843,7397);
INSERT INTO heading_bodies VALUES(85,'Text with: äöü ÄÖÜ 😀😇🤖',7422,7459);
INSERT INTO heading_bodies VALUES(86,unistr('- https://orgmode.org/🔥\u000a- file:sub/äöü.txt\u000a- file:sub/äöü.txt::target\u000a- customlink:äöü-😀'),7542,7645);
INSERT INTO heading_bodies VALUES(87,'See [[file:../../notes/org-semantics/multipe-title-keywords.org]]',69,134);
INSERT INTO heading_bodies VALUES(88,unistr('This can be proven by using ~org-latex-export-as-latex~:\u000a\u000a#+BEGIN_SRC latex\u000a  \\title{Title can span multiple lines, even here}\u000a#+END_SRC'),NULL,NULL);
INSERT INTO heading_bodies VALUES(99,'SCHEDULED: <%%(diary-float t 42)>',547,580);
INSERT INTO heading_bodies VALUES(100,'In that case Org uses the second entry.',663,702);
INSERT INTO heading_bodies VALUES(102,unistr('DEADLINE: <2024-12-01 Sun>\u000aCLOSED: [2024-11-21 Thu]'),774,825);
INSERT INTO heading_bodies VALUES(103,unistr('Some body text first.\u000aSCHEDULED: <2024-11-20 Wed>'),871,920);
INSERT INTO heading_bodies VALUES(104,'This mentions DEADLINE: <2024-12-01 Sun> inside text.',954,1007);
INSERT INTO heading_bodies VALUES(105,'scheduled: <2024-11-20 Wed>',1049,1076);
INSERT INTO heading_bodies VALUES(108,'Body text for the first task.',607,636);
INSERT INTO heading_bodies VALUES(109,unistr('Expected raw/direct storage:\u000a- both DEFINED_TWICE rows should be preserved\u000a- no overwrite should be computed here'),743,856);
INSERT INTO heading_bodies VALUES(110,unistr('Expected raw/direct storage:\u000a- ADD-VALUE = is, append = 0\u000a- ADD-VALUE = valid, append = 1\u000a- final value "is valid" is not computed in this task'),949,1092);
INSERT INTO heading_bodies VALUES(111,unistr('Expected normalized keys:\u000a- ID\u000a- CUSTOM_ID\u000a- DRAWER_PROP\u000a- ADD-VALUE'),1234,1302);
INSERT INTO heading_bodies VALUES(112,unistr('Expected:\u000a- key EMPTY\u000a- value ""\u000a- source property_drawer'),1378,1435);
INSERT INTO heading_bodies VALUES(113,unistr(':PROPERTIES:\u000a:EMPTY:\u000a:END:\u000aExpected for now:\u000a- Orgize may expose this as a generic drawer, not PROPERTY_DRAWER\u000a- parser should not add fallback parsing in this task\u000a- no property row is expected if Orgize does not expose NODE_PROPERTY'),1482,1716);
INSERT INTO heading_bodies VALUES(114,unistr('This heading should not directly receive file-level #+PROPERTY or #+CATEGORY rows.\u000aThose belong to the synthetic level 0 heading only.'),1760,1894);
INSERT INTO heading_bodies VALUES(115,unistr('This heading still should not directly receive those keyword properties.\u000aThey should be stored on level 0 as:\u000a- LATER_KEYWORD_PROPERTY = works_everywhere, source property_keyword\u000a- CATEGORY = later_category_keyword, source category_keyword'),2023,2262);
INSERT INTO heading_bodies VALUES(116,unistr('These keyword lines should not create property rows in this task.\u000aFILETAGS belongs to the later tags task.\u000aTAGS may later become tag-definition metadata.\u000aCOLUMNS references properties but does not define property values.\u000aCONSTANTS belongs to table/formula semantics.\u000aAUTHOR and OPTIONS remain raw keywords.'),2476,2782);
INSERT INTO heading_bodies VALUES(118,'Parent body.',100,112);
INSERT INTO heading_bodies VALUES(119,'Child body.',131,142);
INSERT INTO heading_bodies VALUES(120,'Grandchild repeats one FILETAG locally and adds a local tag.',180,240);
INSERT INTO heading_bodies VALUES(121,'Sibling body.',252,265);
INSERT INTO heading_bodies VALUES(122,'This heading repeats one FILETAG locally.',300,341);
INSERT INTO heading_bodies VALUES(123,'This heading appears after a later FILETAGS keyword.',401,453);
INSERT INTO heading_bodies VALUES(124,'Parent body.',486,498);
INSERT INTO heading_bodies VALUES(125,'Child should have all_tags_json = ["parent"].',529,574);
INSERT INTO heading_bodies VALUES(126,'Child should have all_tags_json = ["parent", "child"].',608,662);
INSERT INTO heading_bodies VALUES(127,'Grandchild should have all_tags_json = ["parent", "child"].',693,752);
INSERT INTO heading_bodies VALUES(128,unistr('Duplicate local tag should not be repeated.\u000aExpected all_tags_json = ["parent", "child"].'),803,892);
INSERT INTO heading_bodies VALUES(129,'Second parent starts a separate tag inheritance branch.',919,974);
INSERT INTO heading_bodies VALUES(130,'Expected all_tags_json = ["second", "child"].',1015,1060);
INSERT INTO heading_bodies VALUES(131,'Expected all_tags_json = ["second", "child", "extra"].',1107,1161);
INSERT INTO heading_bodies VALUES(133,'Expected all_tags_json = ["local"].',1219,1254);
INSERT INTO heading_bodies VALUES(134,'Expected all_tags_json = [].',1278,1306);
INSERT INTO heading_bodies VALUES(147,'SCHEDULED: <2024-11-20 Wed +1w/2d>',504,538);
INSERT INTO heading_bodies VALUES(148,'SCHEDULED: <2024-11-20 Wed ++1m/1w>',571,606);
INSERT INTO heading_bodies VALUES(149,'SCHEDULED: <2024-11-20 Wed .+1y/2m>',640,675);
INSERT INTO heading_bodies VALUES(158,'DEADLINE: <2024-12-01 Sun ++1m/2d -5d>',1093,1131);
INSERT INTO heading_bodies VALUES(165,'[2024-11-20 Wed +1w]',1510,1530);
INSERT INTO heading_bodies VALUES(167,'SCHEDULED: <%%(diary-float t 42)>',1592,1625);
INSERT INTO heading_bodies VALUES(169,'<2006-11-01 Wed 19:15>',67,89);
INSERT INTO heading_bodies VALUES(170,'<2006-11-02 Thu 10:00-12:00>',122,150);
INSERT INTO heading_bodies VALUES(171,unistr('<2006-11-03 Fri>\u000a<2006-11-06 Mon>'),166,199);
INSERT INTO heading_bodies VALUES(172,unistr('Also in the body <2006-11-03 Fri>.\u000a\u000aThis is an inactive one: [2026-06-22 Mon].'),220,298);
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
INSERT INTO outline_path VALUES(25,3,NULL,0,'0000','["Angle Link Fixture"]');
INSERT INTO outline_path VALUES(26,3,25,1,'0000.0001','["Angle Link Fixture","Angle links"]');
INSERT INTO outline_path VALUES(27,3,26,2,'0000.0001.0001','["Angle Link Fixture","Angle links","Basic angle links with spaces"]');
INSERT INTO outline_path VALUES(28,3,26,2,'0000.0001.0002','["Angle Link Fixture","Angle links","File-like angle links with search options"]');
INSERT INTO outline_path VALUES(29,3,26,2,'0000.0001.0003','["Angle Link Fixture","Angle links","File variant angle links with search options"]');
INSERT INTO outline_path VALUES(30,3,26,2,'0000.0001.0004','["Angle Link Fixture","Angle links","Unknown and custom-looking angle links"]');
INSERT INTO outline_path VALUES(31,3,26,2,'0000.0001.0005','["Angle Link Fixture","Angle links","Action-like angle links"]');
INSERT INTO outline_path VALUES(32,3,26,2,'0000.0001.0006','["Angle Link Fixture","Angle links","Non-file-like search-option-looking paths"]');
INSERT INTO outline_path VALUES(33,3,26,2,'0000.0001.0007','["Angle Link Fixture","Angle links","Special"]');
INSERT INTO outline_path VALUES(34,3,26,2,'0000.0001.0008','["Angle Link Fixture","Angle links","Invalid angle candidates"]');
INSERT INTO outline_path VALUES(35,3,26,2,'0000.0001.0009','["Angle Link Fixture","Angle links","Plain links not stored by this task"]');
INSERT INTO outline_path VALUES(36,3,25,1,'0000.0002','["Angle Link Fixture","Unicode angle links"]');
INSERT INTO outline_path VALUES(37,3,36,2,'0000.0002.0001','["Angle Link Fixture","Unicode angle links","Übung 🚀 Ein Titel mit Umlaut und Emoji"]');
INSERT INTO outline_path VALUES(38,4,NULL,0,'0000','["Bracket Link Fixture"]');
INSERT INTO outline_path VALUES(39,5,NULL,0,'0000','["Bracket Link Fixture"]');
INSERT INTO outline_path VALUES(40,5,39,1,'0000.0001','["Bracket Link Fixture","Internal bracket links"]');
INSERT INTO outline_path VALUES(41,5,40,2,'0000.0001.0001','["Bracket Link Fixture","Internal bracket links","Custom ID links"]');
INSERT INTO outline_path VALUES(42,5,40,2,'0000.0001.0002','["Bracket Link Fixture","Internal bracket links","Fuzzy heading links"]');
INSERT INTO outline_path VALUES(43,5,40,2,'0000.0001.0003','["Bracket Link Fixture","Internal bracket links","Dedicated target links"]');
INSERT INTO outline_path VALUES(44,5,40,2,'0000.0001.0004','["Bracket Link Fixture","Internal bracket links","Named target style fuzzy links"]');
INSERT INTO outline_path VALUES(45,5,40,2,'0000.0001.0005','["Bracket Link Fixture","Internal bracket links","Fuzzy fallback"]');
INSERT INTO outline_path VALUES(46,5,39,1,'0000.0002','["Bracket Link Fixture","File-like bracket links"]');
INSERT INTO outline_path VALUES(47,5,46,2,'0000.0002.0001','["Bracket Link Fixture","File-like bracket links","Explicit file links"]');
INSERT INTO outline_path VALUES(48,5,46,2,'0000.0002.0002','["Bracket Link Fixture","File-like bracket links","Explicit file variants"]');
INSERT INTO outline_path VALUES(49,5,46,2,'0000.0002.0003','["Bracket Link Fixture","File-like bracket links","Implicit file links"]');
INSERT INTO outline_path VALUES(50,5,46,2,'0000.0002.0004','["Bracket Link Fixture","File-like bracket links","File search options"]');
INSERT INTO outline_path VALUES(51,5,39,1,'0000.0003','["Bracket Link Fixture","Typed bracket links"]');
INSERT INTO outline_path VALUES(52,5,51,2,'0000.0003.0001','["Bracket Link Fixture","Typed bracket links","Built-in typed links"]');
INSERT INTO outline_path VALUES(53,5,51,2,'0000.0003.0002','["Bracket Link Fixture","Typed bracket links","Unknown and custom typed links"]');
INSERT INTO outline_path VALUES(54,5,39,1,'0000.0004','["Bracket Link Fixture","Negative non-bracket examples"]');
INSERT INTO outline_path VALUES(55,5,39,1,'0000.0005','["Bracket Link Fixture","Unicode bracket links"]');
INSERT INTO outline_path VALUES(56,5,55,2,'0000.0005.0001','["Bracket Link Fixture","Unicode bracket links","Übung 🚀 Ein Titel mit Umlaut und Emoji"]');
INSERT INTO outline_path VALUES(57,6,NULL,0,'0000','["Ignored Region And Source Context Link Fixture https://example.org/in-title-keyword"]');
INSERT INTO outline_path VALUES(58,6,57,1,'0000.0001','["Ignored Region And Source Context Link Fixture https://example.org/in-title-keyword","Heading with https://example.org/in-heading-title"]');
INSERT INTO outline_path VALUES(59,6,58,2,'0000.0001.0001','["Ignored Region And Source Context Link Fixture https://example.org/in-title-keyword","Heading with https://example.org/in-heading-title","Parsed block contexts"]');
INSERT INTO outline_path VALUES(60,6,58,2,'0000.0001.0002','["Ignored Region And Source Context Link Fixture https://example.org/in-title-keyword","Heading with https://example.org/in-heading-title","Drawer contexts"]');
INSERT INTO outline_path VALUES(61,6,58,2,'0000.0001.0003','["Ignored Region And Source Context Link Fixture https://example.org/in-title-keyword","Heading with https://example.org/in-heading-title","Ignored inline markup"]');
INSERT INTO outline_path VALUES(62,6,58,2,'0000.0001.0004','["Ignored Region And Source Context Link Fixture https://example.org/in-title-keyword","Heading with https://example.org/in-heading-title","Ignored inline source and export"]');
INSERT INTO outline_path VALUES(63,6,58,2,'0000.0001.0005','["Ignored Region And Source Context Link Fixture https://example.org/in-title-keyword","Heading with https://example.org/in-heading-title","Ignored block regions"]');
INSERT INTO outline_path VALUES(64,6,58,2,'0000.0001.0006','["Ignored Region And Source Context Link Fixture https://example.org/in-title-keyword","Heading with https://example.org/in-heading-title","Ignored fixed-width and comment lines"]');
INSERT INTO outline_path VALUES(65,6,58,2,'0000.0001.0007','["Ignored Region And Source Context Link Fixture https://example.org/in-title-keyword","Heading with https://example.org/in-heading-title","Ignored file-level keywords"]');
INSERT INTO outline_path VALUES(66,6,58,2,'0000.0001.0008','["Ignored Region And Source Context Link Fixture https://example.org/in-title-keyword","Heading with https://example.org/in-heading-title","Overlap and precedence guard cases"]');
INSERT INTO outline_path VALUES(67,6,58,2,'0000.0001.0009','["Ignored Region And Source Context Link Fixture https://example.org/in-title-keyword","Heading with https://example.org/in-heading-title","Search options and contexts should remain independent"]');
INSERT INTO outline_path VALUES(68,7,NULL,0,'0000','["Plain Link Fixture"]');
INSERT INTO outline_path VALUES(69,7,68,1,'0000.0001','["Plain Link Fixture","Plain links"]');
INSERT INTO outline_path VALUES(70,7,69,2,'0000.0001.0001','["Plain Link Fixture","Plain links","Default plain protocols"]');
INSERT INTO outline_path VALUES(71,7,69,2,'0000.0001.0002','["Plain Link Fixture","Plain links","Custom plain protocols"]');
INSERT INTO outline_path VALUES(72,7,69,2,'0000.0001.0003','["Plain Link Fixture","Plain links","Action-like plain protocols"]');
INSERT INTO outline_path VALUES(73,7,69,2,'0000.0001.0004','["Plain Link Fixture","Plain links","Bracket and angle links unaffected by plain config"]');
INSERT INTO outline_path VALUES(74,7,69,2,'0000.0001.0005','["Plain Link Fixture","Plain links","Trailing punctuation trimming"]');
INSERT INTO outline_path VALUES(75,7,69,2,'0000.0001.0006','["Plain Link Fixture","Plain links","Boundary and end behavior"]');
INSERT INTO outline_path VALUES(76,7,75,3,'0000.0001.0006.0001','["Plain Link Fixture","Plain links","Boundary and end behavior","Accepted plain-link left boundaries"]');
INSERT INTO outline_path VALUES(77,7,75,3,'0000.0001.0006.0002','["Plain Link Fixture","Plain links","Boundary and end behavior","Rejected plain-link left boundaries"]');
INSERT INTO outline_path VALUES(78,7,75,3,'0000.0001.0006.0003','["Plain Link Fixture","Plain links","Boundary and end behavior","Whitespace and balanced delimiter end behavior"]');
INSERT INTO outline_path VALUES(79,7,75,3,'0000.0001.0006.0004','["Plain Link Fixture","Plain links","Boundary and end behavior","Emphasis and markup interaction"]');
INSERT INTO outline_path VALUES(80,7,75,3,'0000.0001.0006.0005','["Plain Link Fixture","Plain links","Boundary and end behavior","Final-character behavior"]');
INSERT INTO outline_path VALUES(81,7,69,2,'0000.0001.0007','["Plain Link Fixture","Plain links","Search-option-looking paths"]');
INSERT INTO outline_path VALUES(82,7,69,2,'0000.0001.0008','["Plain Link Fixture","Plain links","False positives to watch"]');
INSERT INTO outline_path VALUES(83,7,69,2,'0000.0001.0009','["Plain Link Fixture","Plain links","Ignored forms"]');
INSERT INTO outline_path VALUES(84,7,69,2,'0000.0001.0010','["Plain Link Fixture","Plain links","Parsed regions"]');
INSERT INTO outline_path VALUES(85,7,68,1,'0000.0002','["Plain Link Fixture","Unicode plain links"]');
INSERT INTO outline_path VALUES(86,7,85,2,'0000.0002.0001','["Plain Link Fixture","Unicode plain links","Übung 🚀 Ein Titel mit Umlaut und Emoji"]');
INSERT INTO outline_path VALUES(87,8,NULL,0,'0000','["Title can span multiple lines, even here"]');
INSERT INTO outline_path VALUES(88,8,87,1,'0000.0001','["Title can span multiple lines, even here","Unfortunately Everywhere"]');
INSERT INTO outline_path VALUES(89,9,NULL,0,'0000','["Planning timestamp"]');
INSERT INTO outline_path VALUES(90,9,89,1,'0000.0001','["Planning timestamp","Planning"]');
INSERT INTO outline_path VALUES(91,9,90,2,'0000.0001.0001','["Planning timestamp","Planning","Simple scheduled"]');
INSERT INTO outline_path VALUES(92,9,90,2,'0000.0001.0002','["Planning timestamp","Planning","Simple deadline"]');
INSERT INTO outline_path VALUES(93,9,90,2,'0000.0001.0003','["Planning timestamp","Planning","Simple closed"]');
INSERT INTO outline_path VALUES(94,9,90,2,'0000.0001.0004','["Planning timestamp","Planning","All on one planning line"]');
INSERT INTO outline_path VALUES(95,9,90,2,'0000.0001.0005','["Planning timestamp","Planning","With time"]');
INSERT INTO outline_path VALUES(96,9,90,2,'0000.0001.0006','["Planning timestamp","Planning","Time range same day"]');
INSERT INTO outline_path VALUES(97,9,90,2,'0000.0001.0007','["Planning timestamp","Planning","Date range"]');
INSERT INTO outline_path VALUES(98,9,90,2,'0000.0001.0008','["Planning timestamp","Planning","Repeater"]');
INSERT INTO outline_path VALUES(99,9,90,2,'0000.0001.0009','["Planning timestamp","Planning","Diary expression"]');
INSERT INTO outline_path VALUES(100,9,90,2,'0000.0001.0010','["Planning timestamp","Planning","Multiple same keyword"]');
INSERT INTO outline_path VALUES(101,9,90,2,'0000.0001.0011','["Planning timestamp","Planning","Not valid"]');
INSERT INTO outline_path VALUES(102,9,101,3,'0000.0001.0011.0001','["Planning timestamp","Planning","Not valid","Multiple planning lines"]');
INSERT INTO outline_path VALUES(103,9,101,3,'0000.0001.0011.0002','["Planning timestamp","Planning","Not valid","Planning not immediately after headline"]');
INSERT INTO outline_path VALUES(104,9,101,3,'0000.0001.0011.0003','["Planning timestamp","Planning","Not valid","Looks like planning in body"]');
INSERT INTO outline_path VALUES(105,9,101,3,'0000.0001.0011.0004','["Planning timestamp","Planning","Not valid","Lowercase should probably not count"]');
INSERT INTO outline_path VALUES(106,9,101,3,'0000.0001.0011.0005','["Planning timestamp","Planning","Not valid","Multiple same keyword"]');
INSERT INTO outline_path VALUES(107,10,NULL,0,'0000','["Org Property and Keyword Test"]');
INSERT INTO outline_path VALUES(108,10,107,1,'0000.0001','["Org Property and Keyword Test","Task with multiple drawer properties"]');
INSERT INTO outline_path VALUES(109,10,107,1,'0000.0002','["Org Property and Keyword Test","Task with duplicate drawer properties"]');
INSERT INTO outline_path VALUES(110,10,107,1,'0000.0003','["Org Property and Keyword Test","Task with append operator in drawer"]');
INSERT INTO outline_path VALUES(111,10,107,1,'0000.0004','["Org Property and Keyword Test","Task with mixed-case keys"]');
INSERT INTO outline_path VALUES(112,10,107,1,'0000.0005','["Org Property and Keyword Test","Task with empty property accepted by Orgize"]');
INSERT INTO outline_path VALUES(113,10,107,1,'0000.0006','["Org Property and Keyword Test","Task with Orgize empty-property limitation"]');
INSERT INTO outline_path VALUES(114,10,107,1,'0000.0007','["Org Property and Keyword Test","Task after file-level property keywords"]');
INSERT INTO outline_path VALUES(115,10,107,1,'0000.0008','["Org Property and Keyword Test","Task after later file-level keywords"]');
INSERT INTO outline_path VALUES(116,10,107,1,'0000.0009','["Org Property and Keyword Test","Boundary: property-like but not properties"]');
INSERT INTO outline_path VALUES(117,11,NULL,0,'0000','["Tags and FILETAGS Fixture"]');
INSERT INTO outline_path VALUES(118,11,117,1,'0000.0001','["Tags and FILETAGS Fixture","Parent"]');
INSERT INTO outline_path VALUES(119,11,118,2,'0000.0001.0001','["Tags and FILETAGS Fixture","Parent","Child"]');
INSERT INTO outline_path VALUES(120,11,119,3,'0000.0001.0001.0001','["Tags and FILETAGS Fixture","Parent","Child","Grandchild"]');
INSERT INTO outline_path VALUES(121,11,117,1,'0000.0002','["Tags and FILETAGS Fixture","Sibling"]');
INSERT INTO outline_path VALUES(122,11,117,1,'0000.0003','["Tags and FILETAGS Fixture","Duplicate Local"]');
INSERT INTO outline_path VALUES(123,11,117,1,'0000.0004','["Tags and FILETAGS Fixture","After Later FILETAGS"]');
INSERT INTO outline_path VALUES(124,11,117,1,'0000.0005','["Tags and FILETAGS Fixture","parent with one tag"]');
INSERT INTO outline_path VALUES(125,11,124,2,'0000.0005.0001','["Tags and FILETAGS Fixture","parent with one tag","child inherits parent tag"]');
INSERT INTO outline_path VALUES(126,11,124,2,'0000.0005.0002','["Tags and FILETAGS Fixture","parent with one tag","child with local tag"]');
INSERT INTO outline_path VALUES(127,11,126,3,'0000.0005.0002.0001','["Tags and FILETAGS Fixture","parent with one tag","child with local tag","grandchild inherits both"]');
INSERT INTO outline_path VALUES(128,11,126,3,'0000.0005.0002.0002','["Tags and FILETAGS Fixture","parent with one tag","child with local tag","grandchild with duplicate local tag"]');
INSERT INTO outline_path VALUES(129,11,117,1,'0000.0006','["Tags and FILETAGS Fixture","second parent"]');
INSERT INTO outline_path VALUES(130,11,129,2,'0000.0006.0001','["Tags and FILETAGS Fixture","second parent","second child with local tag"]');
INSERT INTO outline_path VALUES(131,11,130,3,'0000.0006.0001.0001','["Tags and FILETAGS Fixture","second parent","second child with local tag","second grandchild with extra tag"]');
INSERT INTO outline_path VALUES(132,11,117,1,'0000.0007','["Tags and FILETAGS Fixture","untagged parent"]');
INSERT INTO outline_path VALUES(133,11,132,2,'0000.0007.0001','["Tags and FILETAGS Fixture","untagged parent","child with only local tag"]');
INSERT INTO outline_path VALUES(134,11,132,2,'0000.0007.0002','["Tags and FILETAGS Fixture","untagged parent","child without tags"]');
INSERT INTO outline_path VALUES(135,12,NULL,0,'0000','["timestamp-repeaters"]');
INSERT INTO outline_path VALUES(136,12,135,1,'0000.0001','["timestamp-repeaters","Repeater markers"]');
INSERT INTO outline_path VALUES(137,12,136,2,'0000.0001.0001','["timestamp-repeaters","Repeater markers","Cumulate plus"]');
INSERT INTO outline_path VALUES(138,12,136,2,'0000.0001.0002','["timestamp-repeaters","Repeater markers","Catch up plus plus"]');
INSERT INTO outline_path VALUES(139,12,136,2,'0000.0001.0003','["timestamp-repeaters","Repeater markers","Restart dot plus"]');
INSERT INTO outline_path VALUES(140,12,135,1,'0000.0002','["timestamp-repeaters","Repeater units"]');
INSERT INTO outline_path VALUES(141,12,140,2,'0000.0002.0001','["timestamp-repeaters","Repeater units","Repeater hour"]');
INSERT INTO outline_path VALUES(142,12,140,2,'0000.0002.0002','["timestamp-repeaters","Repeater units","Repeater day"]');
INSERT INTO outline_path VALUES(143,12,140,2,'0000.0002.0003','["timestamp-repeaters","Repeater units","Repeater week"]');
INSERT INTO outline_path VALUES(144,12,140,2,'0000.0002.0004','["timestamp-repeaters","Repeater units","Repeater month"]');
INSERT INTO outline_path VALUES(145,12,140,2,'0000.0002.0005','["timestamp-repeaters","Repeater units","Repeater year"]');
INSERT INTO outline_path VALUES(146,12,135,1,'0000.0003','["timestamp-repeaters","Repeater deadline part"]');
INSERT INTO outline_path VALUES(147,12,146,2,'0000.0003.0001','["timestamp-repeaters","Repeater deadline part","Repeater with deadline day"]');
INSERT INTO outline_path VALUES(148,12,146,2,'0000.0003.0002','["timestamp-repeaters","Repeater deadline part","Repeater with deadline week"]');
INSERT INTO outline_path VALUES(149,12,146,2,'0000.0003.0003','["timestamp-repeaters","Repeater deadline part","Repeater with deadline month"]');
INSERT INTO outline_path VALUES(150,12,135,1,'0000.0004','["timestamp-repeaters","Warning delays"]');
INSERT INTO outline_path VALUES(151,12,150,2,'0000.0004.0001','["timestamp-repeaters","Warning delays","Warning all"]');
INSERT INTO outline_path VALUES(152,12,150,2,'0000.0004.0002','["timestamp-repeaters","Warning delays","Warning first"]');
INSERT INTO outline_path VALUES(153,12,150,2,'0000.0004.0003','["timestamp-repeaters","Warning delays","Warning hour"]');
INSERT INTO outline_path VALUES(154,12,150,2,'0000.0004.0004','["timestamp-repeaters","Warning delays","Warning month"]');
INSERT INTO outline_path VALUES(155,12,150,2,'0000.0004.0005','["timestamp-repeaters","Warning delays","Warning year"]');
INSERT INTO outline_path VALUES(156,12,135,1,'0000.0005','["timestamp-repeaters","Repeater and warning combinations"]');
INSERT INTO outline_path VALUES(157,12,156,2,'0000.0005.0001','["timestamp-repeaters","Repeater and warning combinations","Repeater and warning"]');
INSERT INTO outline_path VALUES(158,12,156,2,'0000.0005.0002','["timestamp-repeaters","Repeater and warning combinations","Catch up repeater with deadline part and warning"]');
INSERT INTO outline_path VALUES(159,12,156,2,'0000.0005.0003','["timestamp-repeaters","Repeater and warning combinations","Restart repeater with first warning"]');
INSERT INTO outline_path VALUES(160,12,135,1,'0000.0006','["timestamp-repeaters","Time and range combinations"]');
INSERT INTO outline_path VALUES(161,12,160,2,'0000.0006.0001','["timestamp-repeaters","Time and range combinations","Time with repeater"]');
INSERT INTO outline_path VALUES(162,12,160,2,'0000.0006.0002','["timestamp-repeaters","Time and range combinations","Time range with repeater"]');
INSERT INTO outline_path VALUES(163,12,160,2,'0000.0006.0003','["timestamp-repeaters","Time and range combinations","Date range with repeater"]');
INSERT INTO outline_path VALUES(164,12,135,1,'0000.0007','["timestamp-repeaters","Inactive timestamp with repeater"]');
INSERT INTO outline_path VALUES(165,12,164,2,'0000.0007.0001','["timestamp-repeaters","Inactive timestamp with repeater","Inactive repeater"]');
INSERT INTO outline_path VALUES(166,12,135,1,'0000.0008','["timestamp-repeaters","Diary negative case"]');
INSERT INTO outline_path VALUES(167,12,166,2,'0000.0008.0001','["timestamp-repeaters","Diary negative case","Diary with apparent repeater text"]');
INSERT INTO outline_path VALUES(168,13,NULL,0,'0000','["Timestamps"]');
INSERT INTO outline_path VALUES(169,13,168,1,'0000.0001','["Timestamps","Meet Peter at the movies"]');
INSERT INTO outline_path VALUES(170,13,168,1,'0000.0002','["Timestamps","Discussion on climate change"]');
INSERT INTO outline_path VALUES(171,13,168,1,'0000.0003','["Timestamps","My days off"]');
INSERT INTO outline_path VALUES(172,13,168,1,'0000.0004','["Timestamps","Can be anywhere"]');
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
INSERT INTO todo_keywords VALUES(1,'one','open','t',0,'org_keyword','TODO',3);
INSERT INTO todo_keywords VALUES(1,'two','open','n',1,'org_keyword','TODO',3);
INSERT INTO todo_keywords VALUES(1,'FIVE','open',NULL,2,'org_keyword','TODO',4);
INSERT INTO todo_keywords VALUES(1,'SIX','open',NULL,3,'org_keyword','TODO',4);
INSERT INTO todo_keywords VALUES(1,'seven','open',NULL,4,'org_keyword','TYP_TODO',5);
INSERT INTO todo_keywords VALUES(1,'nine','open',NULL,5,'org_keyword','SEQ_TODO',6);
INSERT INTO todo_keywords VALUES(1,'late_open','open',NULL,6,'org_keyword','TODO',34);
INSERT INTO todo_keywords VALUES(1,'three','closed','d',7,'org_keyword','TODO',3);
INSERT INTO todo_keywords VALUES(1,'four','closed','w',8,'org_keyword','TODO',3);
INSERT INTO todo_keywords VALUES(1,'eight','closed',NULL,9,'org_keyword','TYP_TODO',5);
INSERT INTO todo_keywords VALUES(1,'ten','closed',NULL,10,'org_keyword','SEQ_TODO',6);
INSERT INTO todo_keywords VALUES(1,'eleven','closed','c',11,'org_keyword','TODO',30);
INSERT INTO todo_keywords VALUES(1,'late_done','closed',NULL,12,'org_keyword','TODO',34);
INSERT INTO todo_keywords VALUES(2,'TODO','open',NULL,0,'org_keyword','TODO',29);
INSERT INTO todo_keywords VALUES(2,'NEXT','open',NULL,1,'org_keyword','TODO',29);
INSERT INTO todo_keywords VALUES(2,'IDEA','open','i',2,'org_keyword','SEQ_TODO',30);
INSERT INTO todo_keywords VALUES(2,'WURST','open','w',3,'org_keyword','SEQ_TODO',30);
INSERT INTO todo_keywords VALUES(2,'PLAN','open','p',4,'org_keyword','SEQ_TODO',30);
INSERT INTO todo_keywords VALUES(2,'BUILD','open','b',5,'org_keyword','SEQ_TODO',30);
INSERT INTO todo_keywords VALUES(2,'WAITING','open','w',6,'org_keyword','TYP_TODO',31);
INSERT INTO todo_keywords VALUES(2,'DONE','closed',NULL,7,'org_keyword','TODO',29);
INSERT INTO todo_keywords VALUES(2,'CANCELED','closed',NULL,8,'org_keyword','TODO',29);
INSERT INTO todo_keywords VALUES(3,'TODO','open',NULL,0,'config_default',NULL,NULL);
INSERT INTO todo_keywords VALUES(3,'DONE','closed',NULL,1,'config_default',NULL,NULL);
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
INSERT INTO todo_keywords VALUES(10,'TODO','open',NULL,0,'config_default',NULL,NULL);
INSERT INTO todo_keywords VALUES(10,'DONE','closed',NULL,1,'config_default',NULL,NULL);
INSERT INTO todo_keywords VALUES(11,'TODO','open',NULL,0,'config_default',NULL,NULL);
INSERT INTO todo_keywords VALUES(11,'DONE','closed',NULL,1,'config_default',NULL,NULL);
INSERT INTO todo_keywords VALUES(12,'TODO','open',NULL,0,'config_default',NULL,NULL);
INSERT INTO todo_keywords VALUES(12,'DONE','closed',NULL,1,'config_default',NULL,NULL);
INSERT INTO todo_keywords VALUES(13,'TODO','open',NULL,0,'config_default',NULL,NULL);
INSERT INTO todo_keywords VALUES(13,'DONE','closed',NULL,1,'config_default',NULL,NULL);
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
INSERT INTO links VALUES(1,1,1,168,231,8,'normal','bracket','[[file:../../notes/org-semantics/file-local-todo-keywords.org]]','file:../../notes/org-semantics/file-local-todo-keywords.org',NULL,'file','../../notes/org-semantics/file-local-todo-keywords.org',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(2,3,25,125,150,5,'normal','angle','<FILE:root-angle.org::42>','FILE:root-angle.org::42',NULL,'file','root-angle.org','42',NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(3,3,25,151,176,6,'normal','angle','<root:target with spaces>','root:target with spaces',NULL,'root','target with spaces',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(4,3,25,177,204,7,'normal','bracket','[[root:target with spaces]]','root:target with spaces',NULL,'root','target with spaces',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(5,3,25,205,256,8,'normal','plain','https://example.org/root-plain-should-not-be-stored','https://example.org/root-plain-should-not-be-stored',NULL,'https','//example.org/root-plain-should-not-be-stored',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(6,3,27,309,352,14,'normal','angle','<https://example.com/some path with spaces>','https://example.com/some path with spaces',NULL,'https','//example.com/some path with spaces',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(7,3,27,355,385,15,'normal','angle','<https://example.org/ spaces >','https://example.org/ spaces ',NULL,'https','//example.org/ spaces ',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(8,3,27,388,412,16,'normal','angle','<info:org#External Link>','info:org#External Link',NULL,'info','org#External Link',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(9,3,27,415,445,17,'normal','angle','<mailto:emacs-orgmode@gnu.org>','mailto:emacs-orgmode@gnu.org',NULL,'mailto','emacs-orgmode@gnu.org',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(10,3,28,494,519,20,'normal','angle','<file:~/code/main.c::255>','file:~/code/main.c::255',NULL,'file','~/code/main.c','255',NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(11,3,28,522,549,21,'normal','angle','<file:~/xx.org::*My Target>','file:~/xx.org::*My Target',NULL,'file','~/xx.org','*My Target',NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(12,3,28,552,582,22,'normal','angle','<file:~/xx.org::#my-custom-id>','file:~/xx.org::#my-custom-id',NULL,'file','~/xx.org','#my-custom-id',NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(13,3,28,585,610,23,'normal','angle','<file:~/xx.org::/regexp/>','file:~/xx.org::/regexp/',NULL,'file','~/xx.org','/regexp/',NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(14,3,28,613,629,24,'normal','angle','<file:::find me>','file:::find me',NULL,'file','','find me',NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(15,3,28,632,662,25,'normal','angle','<file:::*Current File Heading>','file:::*Current File Heading',NULL,'file','','*Current File Heading',NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(16,3,29,714,743,28,'normal','angle','<file+sys:~/code/main.c::255>','file+sys:~/code/main.c::255',NULL,'file+sys','~/code/main.c','255',NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(17,3,29,746,777,29,'normal','angle','<file+sys:~/xx.org::*My Target>','file+sys:~/xx.org::*My Target',NULL,'file+sys','~/xx.org','*My Target',NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(18,3,29,780,811,30,'normal','angle','<file+emacs:~/code/main.c::255>','file+emacs:~/code/main.c::255',NULL,'file+emacs','~/code/main.c','255',NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(19,3,29,814,850,31,'normal','angle','<file+emacs:~/xx.org::#my-custom-id>','file+emacs:~/xx.org::#my-custom-id',NULL,'file+emacs','~/xx.org','#my-custom-id',NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(20,3,30,896,909,34,'normal','angle','<unknown:foo>','unknown:foo',NULL,'unknown','foo',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(21,3,30,912,926,35,'normal','angle','<jira:ABC-123>','jira:ABC-123',NULL,'jira','ABC-123',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(22,3,30,929,946,36,'normal','angle','<customlink:test>','customlink:test',NULL,'customlink','test',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(23,3,30,949,966,37,'normal','angle','<doi:10.1000/182>','doi:10.1000/182',NULL,'doi','10.1000/182',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(24,3,30,969,994,38,'normal','angle','<irc:/irc.com/#emacs/bob>','irc:/irc.com/#emacs/bob',NULL,'irc','/irc.com/#emacs/bob',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(25,3,31,1101,1117,44,'normal','angle','<shell:ls *.org>','shell:ls *.org',NULL,'shell','ls *.org',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(26,3,31,1120,1160,45,'normal','angle','<elisp:(find-file "~/.emacs.d/init.el")>','elisp:(find-file "~/.emacs.d/init.el")',NULL,'elisp','(find-file "~/.emacs.d/init.el")',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(27,3,32,1281,1326,51,'normal','angle','<id:16ccfc6a-11ba-499f-8bc6-41be30daa3c5::10>','id:16ccfc6a-11ba-499f-8bc6-41be30daa3c5::10',NULL,'id','16ccfc6a-11ba-499f-8bc6-41be30daa3c5::10',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(28,3,32,1329,1358,52,'normal','angle','<attachment:projects.org::10>','attachment:projects.org::10',NULL,'attachment','projects.org::10',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(29,3,32,1361,1390,53,'normal','angle','<docview:papers/last.pdf::12>','docview:papers/last.pdf::12',NULL,'docview','papers/last.pdf::12',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(30,3,32,1393,1415,54,'normal','angle','<unknown:file.org::10>','unknown:file.org::10',NULL,'unknown','file.org::10',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(31,3,32,1418,1443,55,'normal','angle','<customlink:file.org::10>','customlink:file.org::10',NULL,'customlink','file.org::10',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(32,3,33,1509,1530,59,'normal','angle','<https://www.gnu.org>','https://www.gnu.org',NULL,'https','//www.gnu.org',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(33,3,33,1562,1583,62,'normal','angle','<https://www.gnu.org>','https://www.gnu.org',NULL,'https','//www.gnu.org',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(34,3,33,1629,1677,66,'normal','bracket','[[<https://www.gnu.org>][<https://www.gnu.org>]]','<https://www.gnu.org>','<https://www.gnu.org>','fuzzy','<https://www.gnu.org>',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(35,3,34,1755,1774,72,'normal','plain','https://example.com','https://example.com',NULL,'https','//example.com',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(36,3,34,1802,1823,74,'normal','plain','file:~/broken.org::10','file:~/broken.org::10',NULL,'file','~/broken.org','10',NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(37,3,34,1876,1895,78,'normal','plain','https://example.org','https://example.org',NULL,'https','//example.org',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(38,3,34,1967,2000,83,'normal','angle','<https://example.org/later-valid>','https://example.org/later-valid',NULL,'https','//example.org/later-valid',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(39,3,34,2009,2042,84,'normal','angle','<https://example.org/later-valid>','https://example.org/later-valid',NULL,'https','//example.org/later-valid',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(40,3,35,2161,2186,90,'normal','plain','https://example.org/plain','https://example.org/plain',NULL,'https','//example.org/plain',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(41,3,35,2189,2212,91,'normal','plain','file:~/code/main.c::255','file:~/code/main.c::255',NULL,'file','~/code/main.c','255',NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(42,3,35,2232,2244,93,'normal','plain','jira:ABC-123','jira:ABC-123',NULL,'jira','ABC-123',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(43,3,37,2391,2434,101,'normal','angle','<https://orgmode.org/🔥 path with spaces>','https://orgmode.org/🔥 path with spaces',NULL,'https','//orgmode.org/🔥 path with spaces',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(44,3,37,2437,2466,102,'normal','angle','<file:sub/äöü.txt::target>','file:sub/äöü.txt::target',NULL,'file','sub/äöü.txt','target',NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(45,3,37,2469,2490,103,'normal','angle','<unknown:äöü-😀>','unknown:äöü-😀',NULL,'unknown','äöü-😀',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(46,4,38,162,206,5,'normal','plain','https://nok-no-bracket-link--plain-link.org\','https://nok-no-bracket-link--plain-link.org\',NULL,'https','//nok-no-bracket-link--plain-link.org\',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(47,4,38,275,326,8,'normal','bracket','[[https://ok-even-number-of-backslashes.org\\][OK]]','https://ok-even-number-of-backslashes.org\\','OK','https','//ok-even-number-of-backslashes.org\\',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(48,4,38,406,503,11,'normal','bracket','[[https://ok-escaped-brackets-in-path.org\]\[brackets need to be escaped \[\] in the path\]][OK]]','https://ok-escaped-brackets-in-path.org\]\[brackets need to be escaped \[\] in the path\]','OK','https','//ok-escaped-brackets-in-path.org\]\[brackets need to be escaped \[\] in the path\]',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(49,4,38,506,609,12,'normal','bracket','[[https://ok-escaped-brackets-in-path.org\\\]\\\[brackets need to be escaped \[\\\] in the path\]][OK]]','https://ok-escaped-brackets-in-path.org\\\]\\\[brackets need to be escaped \[\\\] in the path\]','OK','https','//ok-escaped-brackets-in-path.org\\\]\\\[brackets need to be escaped \[\\\] in the path\]',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(50,4,38,739,799,15,'normal','bracket','[[https://ok-description-double-bracket-closes-link][[Test]]','https://ok-description-double-bracket-closes-link','[Test','https','//ok-description-double-bracket-closes-link',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(51,4,38,804,868,16,'normal','bracket','[[https://ok-description-double-bracket-closes-link][[[[[[Test]]','https://ok-description-double-bracket-closes-link','[[[[[Test','https','//ok-description-double-bracket-closes-link',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(52,4,38,875,935,17,'normal','bracket','[[https://ok-description-double-bracket-closes-link][[][][]]','https://ok-description-double-bracket-closes-link','[][][','https','//ok-description-double-bracket-closes-link',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(53,4,38,940,1018,18,'normal','bracket','[[https://ok-description-double-bracket-closes-link][test [[https://test.com]]','https://ok-description-double-bracket-closes-link','test [[https://test.com','https','//ok-description-double-bracket-closes-link',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(54,4,38,1280,1299,23,'normal','plain','https://www.gnu.org','https://www.gnu.org',NULL,'https','//www.gnu.org',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(55,4,38,1375,1394,27,'normal','plain','https://www.gnu.org','https://www.gnu.org',NULL,'https','//www.gnu.org',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(56,4,38,1452,1471,31,'normal','plain','https://www.gnu.org','https://www.gnu.org',NULL,'https','//www.gnu.org',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(57,4,38,1532,1588,35,'normal','bracket','[[file:sub/äöü.txt::target][A file link with Umlaut]]','file:sub/äöü.txt::target','A file link with Umlaut','file','sub/äöü.txt','target',NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(58,5,39,121,148,5,'normal','bracket','[[FILE:root-notes.org::42]]','FILE:root-notes.org::42',NULL,'file','root-notes.org','42',NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(59,5,39,149,182,6,'normal','bracket','[[root target][root description]]','root target','root description','fuzzy','root target',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(60,5,39,183,234,7,'normal','plain','https://example.org/root-plain-should-not-be-stored','https://example.org/root-plain-should-not-be-stored',NULL,'https','//example.org/root-plain-should-not-be-stored',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(61,5,39,235,288,8,'normal','angle','<https://example.org/root-angle-should-not-be-stored>','https://example.org/root-angle-should-not-be-stored',NULL,'https','//example.org/root-angle-should-not-be-stored',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(62,5,41,395,426,16,'normal','bracket','[[#internal-link-to-custom-id]]','#internal-link-to-custom-id',NULL,'custom-id','#internal-link-to-custom-id',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(63,5,41,429,501,17,'normal','bracket','[[#internal-link-to-custom-id][description: internal-link-to-custom-id]]','#internal-link-to-custom-id','description: internal-link-to-custom-id','custom-id','#internal-link-to-custom-id',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(64,5,42,528,555,20,'normal','bracket','[[*Internal bracket links]]','*Internal bracket links',NULL,'fuzzy','*Internal bracket links',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(65,5,42,558,612,21,'normal','bracket','[[*Internal bracket links][description: heading link]]','*Internal bracket links','description: heading link','fuzzy','*Internal bracket links',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(66,5,43,663,683,25,'normal','bracket','[[dedicated target]]','dedicated target',NULL,'fuzzy','dedicated target',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(67,5,43,686,737,26,'normal','bracket','[[dedicated target][description: dedicated target]]','dedicated target','description: dedicated target','fuzzy','dedicated target',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(68,5,44,796,812,30,'normal','bracket','[[named target]]','named target',NULL,'fuzzy','named target',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(69,5,44,815,858,31,'normal','bracket','[[named target][description: named target]]','named target','description: named target','fuzzy','named target',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(70,5,45,880,902,34,'normal','bracket','[[no matching target]]','no matching target',NULL,'fuzzy','no matching target',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(71,5,45,905,960,35,'normal','bracket','[[no matching target][description: no matching target]]','no matching target','description: no matching target','fuzzy','no matching target',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(72,5,45,963,976,36,'normal','bracket','[[notes.org]]','notes.org',NULL,'fuzzy','notes.org',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(73,5,47,1030,1043,41,'normal','bracket','[[file:/etc]]','file:/etc',NULL,'file','/etc',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(74,5,47,1046,1078,42,'normal','bracket','[[file:/etc][description: /etc]]','file:/etc','description: /etc','file','/etc',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(75,5,47,1081,1095,43,'normal','bracket','[[file:/etc/]]','file:/etc/',NULL,'file','/etc/',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(76,5,47,1098,1121,44,'normal','bracket','[[file:/etc/host.conf]]','file:/etc/host.conf',NULL,'file','/etc/host.conf',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(77,5,47,1124,1136,45,'normal','bracket','[[file:../]]','file:../',NULL,'file','../',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(78,5,47,1139,1157,46,'normal','bracket','[[file:../../sql]]','file:../../sql',NULL,'file','../../sql',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(79,5,47,1160,1179,47,'normal','bracket','[[file:../../sql/]]','file:../../sql/',NULL,'file','../../sql/',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(80,5,47,1182,1208,48,'normal','bracket','[[file:../parser_test.rs]]','file:../parser_test.rs',NULL,'file','../parser_test.rs',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(81,5,47,1211,1240,49,'normal','bracket','[[file:./org-test-links.org]]','file:./org-test-links.org',NULL,'file','./org-test-links.org',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(82,5,47,1243,1262,50,'normal','bracket','[[file:~/.emacs.d]]','file:~/.emacs.d',NULL,'file','~/.emacs.d',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(83,5,47,1265,1285,51,'normal','bracket','[[file:~/.emacs.d/]]','file:~/.emacs.d/',NULL,'file','~/.emacs.d/',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(84,5,47,1288,1315,52,'normal','bracket','[[file:~/.emacs.d/init.el]]','file:~/.emacs.d/init.el',NULL,'file','~/.emacs.d/init.el',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(85,5,48,1345,1362,55,'normal','bracket','[[file+sys:/etc]]','file+sys:/etc',NULL,'file+sys','/etc',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(86,5,48,1365,1384,56,'normal','bracket','[[file+emacs:/etc]]','file+emacs:/etc',NULL,'file+emacs','/etc',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(87,5,48,1387,1432,57,'normal','bracket','[[file+sys:/etc][description: file+sys:/etc]]','file+sys:/etc','description: file+sys:/etc','file+sys','/etc',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(88,5,48,1435,1484,58,'normal','bracket','[[file+emacs:/etc][description: file+emacs:/etc]]','file+emacs:/etc','description: file+emacs:/etc','file+emacs','/etc',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(89,5,49,1511,1519,61,'normal','bracket','[[/etc]]','/etc',NULL,'file','/etc',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(90,5,49,1522,1531,62,'normal','bracket','[[/etc/]]','/etc/',NULL,'file','/etc/',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(91,5,49,1534,1552,63,'normal','bracket','[[/etc/host.conf]]','/etc/host.conf',NULL,'file','/etc/host.conf',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(92,5,49,1555,1562,64,'normal','bracket','[[../]]','../',NULL,'file','../',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(93,5,49,1565,1578,65,'normal','bracket','[[../../sql]]','../../sql',NULL,'file','../../sql',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(94,5,49,1581,1595,66,'normal','bracket','[[../../sql/]]','../../sql/',NULL,'file','../../sql/',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(95,5,49,1598,1619,67,'normal','bracket','[[../parser_test.rs]]','../parser_test.rs',NULL,'file','../parser_test.rs',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(96,5,49,1622,1646,68,'normal','bracket','[[./org-test-links.org]]','./org-test-links.org',NULL,'file','./org-test-links.org',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(97,5,49,1649,1662,69,'normal','bracket','[[~/memento]]','~/memento',NULL,'file','~/memento',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(98,5,50,1689,1722,72,'normal','bracket','[[file:./org-test-links.org::10]]','file:./org-test-links.org::10',NULL,'file','./org-test-links.org','10',NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(99,5,50,1725,1783,73,'normal','bracket','[[file:./org-test-links.org::#internal-link-to-custom-id]]','file:./org-test-links.org::#internal-link-to-custom-id',NULL,'file','./org-test-links.org','#internal-link-to-custom-id',NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(100,5,50,1786,1833,74,'normal','bracket','[[file:./org-test-links.org::dedicated target]]','file:./org-test-links.org::dedicated target',NULL,'file','./org-test-links.org','dedicated target',NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(101,5,50,1836,1890,75,'normal','bracket','[[file:./org-test-links.org::*Internal bracket links]]','file:./org-test-links.org::*Internal bracket links',NULL,'file','./org-test-links.org','*Internal bracket links',NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(102,5,50,1893,1939,76,'normal','bracket','[[file:./org-test-links.org::/*.File-like.*/]]','file:./org-test-links.org::/*.File-like.*/',NULL,'file','./org-test-links.org','/*.File-like.*/',NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(103,5,50,1942,1970,77,'normal','bracket','[[./org-test-links.org::10]]','./org-test-links.org::10',NULL,'file','./org-test-links.org','10',NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(104,5,50,1973,2026,78,'normal','bracket','[[./org-test-links.org::#internal-link-to-custom-id]]','./org-test-links.org::#internal-link-to-custom-id',NULL,'file','./org-test-links.org','#internal-link-to-custom-id',NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(105,5,50,2029,2071,79,'normal','bracket','[[./org-test-links.org::dedicated target]]','./org-test-links.org::dedicated target',NULL,'file','./org-test-links.org','dedicated target',NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(106,5,50,2074,2123,80,'normal','bracket','[[./org-test-links.org::*Internal bracket links]]','./org-test-links.org::*Internal bracket links',NULL,'file','./org-test-links.org','*Internal bracket links',NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(107,5,50,2126,2167,81,'normal','bracket','[[./org-test-links.org::/*.File-like.*/]]','./org-test-links.org::/*.File-like.*/',NULL,'file','./org-test-links.org','/*.File-like.*/',NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(108,5,50,2170,2183,82,'normal','bracket','[[file:::10]]','file:::10',NULL,'file','','10',NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(109,5,50,2186,2220,83,'normal','bracket','[[file:::*Internal bracket links]]','file:::*Internal bracket links',NULL,'file','','*Internal bracket links',NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(110,5,52,2271,2293,88,'normal','bracket','[[http://orgmode.org]]','http://orgmode.org',NULL,'http','//orgmode.org',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(111,5,52,2296,2351,89,'normal','bracket','[[http://orgmode.org][description: http://orgmode.org]]','http://orgmode.org','description: http://orgmode.org','http','//orgmode.org',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(112,5,52,2354,2377,90,'normal','bracket','[[https://orgmode.org]]','https://orgmode.org',NULL,'https','//orgmode.org',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(113,5,52,2380,2437,91,'normal','bracket','[[https://orgmode.org][description: https://orgmode.org]]','https://orgmode.org','description: https://orgmode.org','https','//orgmode.org',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(114,5,52,2440,2459,92,'normal','bracket','[[news:comp.emacs]]','news:comp.emacs',NULL,'news','comp.emacs',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(115,5,52,2462,2494,93,'normal','bracket','[[mailto:emacs-orgmode@gnu.org]]','mailto:emacs-orgmode@gnu.org',NULL,'mailto','emacs-orgmode@gnu.org',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(116,5,52,2497,2520,94,'normal','bracket','[[help:org-store-link]]','help:org-store-link',NULL,'help','org-store-link',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(117,5,52,2523,2549,95,'normal','bracket','[[info:org#External Link]]','info:org#External Link',NULL,'info','org#External Link',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(118,5,52,2552,2572,96,'normal','bracket','[[shell:ls *.org  ]]','shell:ls *.org  ',NULL,'shell','ls *.org  ',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(119,5,52,2575,2617,97,'normal','bracket','[[elisp:(find-file "~/.emacs.d/init.el")]]','elisp:(find-file "~/.emacs.d/init.el")',NULL,'elisp','(find-file "~/.emacs.d/init.el")',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(120,5,53,2655,2670,100,'normal','bracket','[[unknown:foo]]','unknown:foo',NULL,'unknown','foo',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(121,5,53,2673,2715,101,'normal','bracket','[[unknown:foo][description: unknown type]]','unknown:foo','description: unknown type','unknown','foo',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(122,5,53,2718,2737,102,'normal','bracket','[[customlink:test]]','customlink:test',NULL,'customlink','test',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(123,5,53,2740,2789,103,'normal','bracket','[[customlink:test][description: customlink:test]]','customlink:test','description: customlink:test','customlink','test',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(124,5,53,2792,2811,104,'normal','bracket','[[doi:10.1000/182]]','doi:10.1000/182',NULL,'doi','10.1000/182',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(125,5,53,2814,2841,105,'normal','bracket','[[irc:/irc.com/#emacs/bob]]','irc:/irc.com/#emacs/bob',NULL,'irc','/irc.com/#emacs/bob',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(126,5,54,2948,2966,111,'normal','plain','http://orgmode.org','http://orgmode.org',NULL,'http','//orgmode.org',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(127,5,54,2969,2988,112,'normal','plain','https://orgmode.org','https://orgmode.org',NULL,'https','//orgmode.org',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(128,5,54,2991,3019,113,'normal','plain','mailto:emacs-orgmode@gnu.org','mailto:emacs-orgmode@gnu.org',NULL,'mailto','emacs-orgmode@gnu.org',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(129,5,54,3022,3051,114,'normal','plain','file:./org-test-links.org::10','file:./org-test-links.org::10',NULL,'file','./org-test-links.org','10',NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(130,5,54,3054,3093,115,'normal','angle','<file:::*Negative non-bracket examples>','file:::*Negative non-bracket examples',NULL,'file','','*Negative non-bracket examples',NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(131,5,54,3096,3112,116,'normal','angle','<shell:ls *.org>','shell:ls *.org',NULL,'shell','ls *.org',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(132,5,54,3115,3145,117,'normal','angle','<https://orgmode.org/ spaces >','https://orgmode.org/ spaces ',NULL,'https','//orgmode.org/ spaces ',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(133,5,56,3330,3386,127,'normal','bracket','[[file:sub/äöü.txt::target][A file link with Umlaut]]','file:sub/äöü.txt::target','A file link with Umlaut','file','sub/äöü.txt','target',NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(134,5,56,3389,3417,128,'normal','bracket','[[https://orgmode.org/🔥]]','https://orgmode.org/🔥',NULL,'https','//orgmode.org/🔥',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(135,5,56,3420,3472,129,'normal','bracket','[[https://orgmode.org/🔥][Emoji description 😀]]','https://orgmode.org/🔥','Emoji description 😀','https','//orgmode.org/🔥',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(136,5,56,3475,3502,130,'normal','bracket','[[dedicated target äöü]]','dedicated target äöü',NULL,'fuzzy','dedicated target äöü',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(137,6,57,498,529,11,'normal','plain','https://example.org/root-normal','https://example.org/root-normal',NULL,'https','//example.org/root-normal',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(138,6,58,546,582,13,'heading','plain','https://example.org/in-heading-title','https://example.org/in-heading-title',NULL,'https','//example.org/in-heading-title',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(139,6,58,599,638,15,'normal','plain','https://example.org/in-normal-paragraph','https://example.org/in-normal-paragraph',NULL,'https','//example.org/in-normal-paragraph',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(140,6,59,681,715,20,'verse_block','plain','https://example.org/in-verse-block','https://example.org/in-verse-block',NULL,'https','//example.org/in-verse-block',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(141,6,59,716,774,21,'verse_block','bracket','[[https://example.org/bracket-in-verse][bracket in verse]]','https://example.org/bracket-in-verse','bracket in verse','https','//example.org/bracket-in-verse',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(142,6,59,775,811,22,'verse_block','angle','<https://example.org/angle-in-verse>','https://example.org/angle-in-verse',NULL,'https','//example.org/angle-in-verse',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(143,6,59,839,873,26,'quote_block','plain','https://example.org/in-quote-block','https://example.org/in-quote-block',NULL,'https','//example.org/in-quote-block',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(144,6,59,874,932,27,'quote_block','bracket','[[https://example.org/bracket-in-quote][bracket in quote]]','https://example.org/bracket-in-quote','bracket in quote','https','//example.org/bracket-in-quote',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(145,6,59,933,969,28,'quote_block','angle','<https://example.org/angle-in-quote>','https://example.org/angle-in-quote',NULL,'https','//example.org/angle-in-quote',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(146,6,59,998,1033,32,'center_block','plain','https://example.org/in-center-block','https://example.org/in-center-block',NULL,'https','//example.org/in-center-block',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(147,6,59,1034,1094,33,'center_block','bracket','[[https://example.org/bracket-in-center][bracket in center]]','https://example.org/bracket-in-center','bracket in center','https','//example.org/bracket-in-center',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(148,6,59,1095,1132,34,'center_block','angle','<https://example.org/angle-in-center>','https://example.org/angle-in-center',NULL,'https','//example.org/angle-in-center',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(149,6,59,1163,1199,38,'justify_block','plain','https://example.org/in-justify-block','https://example.org/in-justify-block',NULL,'https','//example.org/in-justify-block',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(150,6,59,1200,1262,39,'justify_block','bracket','[[https://example.org/bracket-in-justify][bracket in justify]]','https://example.org/bracket-in-justify','bracket in justify','https','//example.org/bracket-in-justify',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(151,6,59,1263,1301,40,'justify_block','angle','<https://example.org/angle-in-justify>','https://example.org/angle-in-justify',NULL,'https','//example.org/angle-in-justify',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(152,6,59,1333,1375,44,'normal','plain','https://example.org/in-other-special-block','https://example.org/in-other-special-block',NULL,'https','//example.org/in-other-special-block',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(153,6,59,1376,1450,45,'normal','bracket','[[https://example.org/bracket-in-other-special][bracket in other special]]','https://example.org/bracket-in-other-special','bracket in other special','https','//example.org/bracket-in-other-special',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(154,6,60,1797,1835,59,'property_drawer','plain','https://example.org/in-property-drawer','https://example.org/in-property-drawer',NULL,'https','//example.org/in-property-drawer',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(155,6,60,1854,1929,60,'property_drawer','bracket','[[https://example.org/bracket-in-property-drawer][property drawer bracket]]','https://example.org/bracket-in-property-drawer','property drawer bracket','https','//example.org/bracket-in-property-drawer',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(156,6,60,1948,1994,61,'property_drawer','angle','<https://example.org/angle-in-property-drawer>','https://example.org/angle-in-property-drawer',NULL,'https','//example.org/angle-in-property-drawer',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(157,6,60,2013,2049,65,'drawer','plain','https://example.org/in-normal-drawer','https://example.org/in-normal-drawer',NULL,'https','//example.org/in-normal-drawer',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(158,6,60,2050,2121,66,'drawer','bracket','[[https://example.org/bracket-in-normal-drawer][normal drawer bracket]]','https://example.org/bracket-in-normal-drawer','normal drawer bracket','https','//example.org/bracket-in-normal-drawer',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(159,6,60,2122,2166,67,'drawer','angle','<https://example.org/angle-in-normal-drawer>','https://example.org/angle-in-normal-drawer',NULL,'https','//example.org/angle-in-normal-drawer',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(160,6,61,2568,2602,83,'normal','plain','https://example.org/outside-markup','https://example.org/outside-markup',NULL,'https','//example.org/outside-markup',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(161,6,61,2686,2718,84,'normal','plain','https://example.org/outside-code','https://example.org/outside-code',NULL,'https','//example.org/outside-code',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(162,6,62,3274,3312,95,'normal','plain','https://example.org/after-inline-forms','https://example.org/after-inline-forms',NULL,'https','//example.org/after-inline-forms',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(163,6,64,4803,4842,151,'normal','plain','https://example.org/after-comment-lines','https://example.org/after-comment-lines',NULL,'https','//example.org/after-comment-lines',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(164,6,65,5320,5354,163,'normal','plain','https://example.org/after-keywords','https://example.org/after-keywords',NULL,'https','//example.org/after-keywords',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(165,6,66,5481,5516,169,'normal','plain','https://example.org/before-verbatim','https://example.org/before-verbatim',NULL,'https','//example.org/before-verbatim',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(166,6,66,5586,5620,169,'normal','plain','https://example.org/after-verbatim','https://example.org/after-verbatim',NULL,'https','//example.org/after-verbatim',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(167,6,66,5759,5807,174,'property_drawer','plain','https://example.org/property-drawer-context-link','https://example.org/property-drawer-context-link',NULL,'https','//example.org/property-drawer-context-link',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(168,6,66,6012,6051,181,'drawer','plain','https://example.org/drawer-context-link','https://example.org/drawer-context-link',NULL,'https','//example.org/drawer-context-link',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(169,6,67,6302,6332,189,'normal','plain','file:./ignored-regions.org::10','file:./ignored-regions.org::10',NULL,'file','./ignored-regions.org','10',NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(170,6,67,6348,6383,192,'quote_block','plain','file:./ignored-regions.org::*Quoted','file:./ignored-regions.org::*Quoted',NULL,'file','./ignored-regions.org','*Quoted',NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(171,6,67,6438,6482,196,'property_drawer','plain','file:./ignored-regions.org::#property-target','file:./ignored-regions.org::#property-target',NULL,'file','./ignored-regions.org','#property-target',NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(172,7,68,239,263,13,'normal','plain','https://root.example.org','https://root.example.org',NULL,'https','//root.example.org',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(173,7,68,264,283,14,'normal','plain','file:root-notes.org','file:root-notes.org',NULL,'file','root-notes.org',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(174,7,68,284,303,15,'normal','plain','FILE:root-notes.org','FILE:root-notes.org',NULL,'file','root-notes.org',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(175,7,68,304,317,16,'normal','plain','jira:ROOT-123','jira:ROOT-123',NULL,'jira','ROOT-123',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(176,7,68,412,429,20,'normal','bracket','[[jira:ROOT-123]]','jira:ROOT-123',NULL,'jira','ROOT-123',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(177,7,68,430,445,21,'normal','angle','<jira:ROOT-123>','jira:ROOT-123',NULL,'jira','ROOT-123',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(178,7,70,567,585,29,'normal','plain','http://orgmode.org','http://orgmode.org',NULL,'http','//orgmode.org',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(179,7,70,588,607,30,'normal','plain','https://orgmode.org','https://orgmode.org',NULL,'https','//orgmode.org',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(180,7,70,610,624,31,'normal','plain','file:notes.org','file:notes.org',NULL,'file','notes.org',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(181,7,70,627,646,32,'normal','plain','file+sys:/etc/hosts','file+sys:/etc/hosts',NULL,'file+sys','/etc/hosts',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(182,7,70,649,678,33,'normal','plain','file+emacs:~/.emacs.d/init.el','file+emacs:~/.emacs.d/init.el',NULL,'file+emacs','~/.emacs.d/init.el',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(183,7,70,681,711,34,'normal','plain','ftp://example.org/pub/file.txt','ftp://example.org/pub/file.txt',NULL,'ftp','//example.org/pub/file.txt',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(184,7,70,714,737,35,'normal','plain','attachment:projects.org','attachment:projects.org',NULL,'attachment','projects.org',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(185,7,70,740,756,36,'normal','plain','bbdb:R.*Stallman','bbdb:R.*Stallman',NULL,'bbdb','R.*Stallman',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(186,7,70,759,782,37,'normal','plain','docview:papers/last.pdf','docview:papers/last.pdf',NULL,'docview','papers/last.pdf',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(187,7,70,785,800,38,'normal','plain','doi:10.1000/182','doi:10.1000/182',NULL,'doi','10.1000/182',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(188,7,70,803,813,39,'normal','plain','gnus:group','gnus:group',NULL,'gnus','group',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(189,7,70,816,831,40,'normal','plain','rmail:folder#id','rmail:folder#id',NULL,'rmail','folder#id',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(190,7,70,834,847,41,'normal','plain','mhe:folder#id','mhe:folder#id',NULL,'mhe','folder#id',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(191,7,70,850,869,42,'normal','plain','help:org-store-link','help:org-store-link',NULL,'help','org-store-link',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(192,7,70,872,881,43,'normal','plain','id:abc123','id:abc123',NULL,'id','abc123',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(193,7,70,884,907,44,'normal','plain','info:org#External-Links','info:org#External-Links',NULL,'info','org#External-Links',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(194,7,70,910,933,45,'normal','plain','irc:/irc.com/#emacs/bob','irc:/irc.com/#emacs/bob',NULL,'irc','/irc.com/#emacs/bob',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(195,7,70,936,961,46,'normal','plain','mailto:person@example.org','mailto:person@example.org',NULL,'mailto','person@example.org',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(196,7,70,964,979,47,'normal','plain','news:comp.emacs','news:comp.emacs',NULL,'news','comp.emacs',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(197,7,70,982,1006,48,'normal','plain','shortdoc:text-properties','shortdoc:text-properties',NULL,'shortdoc','text-properties',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(198,7,71,1110,1122,54,'normal','plain','jira:ABC-123','jira:ABC-123',NULL,'jira','ABC-123',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(199,7,71,1125,1141,55,'normal','plain','jira:PROJECT-999','jira:PROJECT-999',NULL,'jira','PROJECT-999',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(200,7,71,1144,1159,56,'normal','plain','customlink:test','customlink:test',NULL,'customlink','test',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(201,7,71,1162,1188,57,'normal','plain','customlink:with/slash/path','customlink:with/slash/path',NULL,'customlink','with/slash/path',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(202,7,73,1621,1637,75,'normal','bracket','[[jira:ABC-123]]','jira:ABC-123',NULL,'jira','ABC-123',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(203,7,73,1640,1658,76,'normal','bracket','[[shell:ls *.org]]','shell:ls *.org',NULL,'shell','ls *.org',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(204,7,73,1661,1703,77,'normal','bracket','[[elisp:(find-file "~/.emacs.d/init.el")]]','elisp:(find-file "~/.emacs.d/init.el")',NULL,'elisp','(find-file "~/.emacs.d/init.el")',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(205,7,73,1706,1720,78,'normal','angle','<jira:ABC-123>','jira:ABC-123',NULL,'jira','ABC-123',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(206,7,73,1723,1739,79,'normal','angle','<shell:ls *.org>','shell:ls *.org',NULL,'shell','ls *.org',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(207,7,73,1742,1782,80,'normal','angle','<elisp:(find-file "~/.emacs.d/init.el")>','elisp:(find-file "~/.emacs.d/init.el")',NULL,'elisp','(find-file "~/.emacs.d/init.el")',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(208,7,74,1892,1911,86,'normal','plain','https://example.org','https://example.org',NULL,'https','//example.org',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(209,7,74,1915,1934,87,'normal','plain','https://example.org','https://example.org',NULL,'https','//example.org',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(210,7,74,1938,1957,88,'normal','plain','https://example.org','https://example.org',NULL,'https','//example.org',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(211,7,74,1961,1980,89,'normal','plain','https://example.org','https://example.org',NULL,'https','//example.org',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(212,7,74,1984,2003,90,'normal','plain','https://example.org','https://example.org',NULL,'https','//example.org',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(213,7,74,2007,2026,91,'normal','plain','https://example.org','https://example.org',NULL,'https','//example.org',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(214,7,74,2030,2049,92,'normal','plain','https://example.org','https://example.org',NULL,'https','//example.org',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(215,7,74,2053,2072,93,'normal','plain','https://example.org','https://example.org',NULL,'https','//example.org',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(216,7,74,2076,2095,94,'normal','plain','https://example.org','https://example.org',NULL,'https','//example.org',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(217,7,74,2099,2124,95,'normal','plain','mailto:person@example.org','mailto:person@example.org',NULL,'mailto','person@example.org',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(218,7,74,2128,2137,96,'normal','plain','id:abc123','id:abc123',NULL,'id','abc123',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(219,7,74,2141,2155,97,'normal','plain','file:notes.org','file:notes.org',NULL,'file','notes.org',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(220,7,76,2608,2643,110,'normal','plain','https://www.example.com/bang-prefix','https://www.example.com/bang-prefix',NULL,'https','//www.example.com/bang-prefix',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(221,7,76,2647,2680,111,'normal','plain','https://www.example.com/at-prefix','https://www.example.com/at-prefix',NULL,'https','//www.example.com/at-prefix',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(222,7,76,2684,2719,112,'normal','plain','https://www.example.com/hash-prefix','https://www.example.com/hash-prefix',NULL,'https','//www.example.com/hash-prefix',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(223,7,76,2723,2759,113,'normal','plain','https://www.example.com/caret-prefix','https://www.example.com/caret-prefix',NULL,'https','//www.example.com/caret-prefix',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(224,7,76,2763,2803,114,'normal','plain','https://www.example.com/ampersand-prefix','https://www.example.com/ampersand-prefix',NULL,'https','//www.example.com/ampersand-prefix',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(225,7,76,2807,2848,115,'normal','plain','https://www.example.com/open-paren-prefix','https://www.example.com/open-paren-prefix',NULL,'https','//www.example.com/open-paren-prefix',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(226,7,76,2852,2894,116,'normal','plain','https://www.example.com/close-paren-prefix','https://www.example.com/close-paren-prefix',NULL,'https','//www.example.com/close-paren-prefix',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(227,7,76,2898,2939,117,'normal','plain','https://www.example.com/underscore-prefix','https://www.example.com/underscore-prefix',NULL,'https','//www.example.com/underscore-prefix',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(228,7,76,2943,2978,118,'normal','plain','https://www.example.com/dash-prefix','https://www.example.com/dash-prefix',NULL,'https','//www.example.com/dash-prefix',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(229,7,76,2982,3018,119,'normal','plain','https://www.example.com/equal-prefix','https://www.example.com/equal-prefix',NULL,'https','//www.example.com/equal-prefix',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(230,7,76,3022,3058,120,'normal','plain','https://www.example.com/tilde-prefix','https://www.example.com/tilde-prefix',NULL,'https','//www.example.com/tilde-prefix',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(231,7,76,3062,3097,121,'normal','plain','https://www.example.com/plus-prefix','https://www.example.com/plus-prefix',NULL,'https','//www.example.com/plus-prefix',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(232,7,76,3101,3144,122,'normal','plain','https://www.example.com/double-quote-prefix','https://www.example.com/double-quote-prefix',NULL,'https','//www.example.com/double-quote-prefix',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(233,7,76,3154,3196,123,'normal','plain','https://www.example.com/after-colon-prefix','https://www.example.com/after-colon-prefix',NULL,'https','//www.example.com/after-colon-prefix',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(234,7,78,3550,3574,138,'normal','plain','https://example.org/path','https://example.org/path',NULL,'https','//example.org/path',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(235,7,78,3689,3730,142,'normal','plain','https://example.org/path<balanced-suffix>','https://example.org/path<balanced-suffix>',NULL,'https','//example.org/path<balanced-suffix>',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(236,7,78,3733,3762,143,'normal','plain','https://example.org/path(foo)','https://example.org/path(foo)',NULL,'https','//example.org/path(foo)',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(237,7,78,3765,3794,144,'normal','plain','https://example.org/path[foo]','https://example.org/path[foo]',NULL,'https','//example.org/path[foo]',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(238,7,78,3797,3844,145,'normal','plain','https://example.org/path<balanced-angle-suffix>','https://example.org/path<balanced-angle-suffix>',NULL,'https','//example.org/path<balanced-angle-suffix>',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(239,7,78,3928,3952,149,'normal','plain','https://example.org/path','https://example.org/path',NULL,'https','//example.org/path',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(240,7,78,3978,4002,150,'normal','plain','https://example.org/path','https://example.org/path',NULL,'https','//example.org/path',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(241,7,78,4028,4052,151,'normal','plain','https://example.org/path','https://example.org/path',NULL,'https','//example.org/path',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(242,7,78,4078,4102,152,'normal','plain','https://example.org/path','https://example.org/path',NULL,'https','//example.org/path',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(243,7,78,4273,4329,156,'normal','plain','https://example.org/path{not-a-balanced-plain-link-group','https://example.org/path{not-a-balanced-plain-link-group',NULL,'https','//example.org/path{not-a-balanced-plain-link-group',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(244,7,78,4333,4357,157,'normal','plain','https://example.org/path','https://example.org/path',NULL,'https','//example.org/path',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(245,7,79,4570,4609,164,'normal','plain','https://example.org/emphasis-delimited*','https://example.org/emphasis-delimited*',NULL,'https','//example.org/emphasis-delimited*',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(246,7,80,4853,4877,172,'normal','plain','https://example.org/path','https://example.org/path',NULL,'https','//example.org/path',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(247,7,80,4881,4905,173,'normal','plain','https://example.org/path','https://example.org/path',NULL,'https','//example.org/path',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(248,7,80,4909,4933,174,'normal','plain','https://example.org/path','https://example.org/path',NULL,'https','//example.org/path',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(249,7,80,4937,4961,175,'normal','plain','https://example.org/path','https://example.org/path',NULL,'https','//example.org/path',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(250,7,80,4965,4989,176,'normal','plain','https://example.org/path','https://example.org/path',NULL,'https','//example.org/path',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(251,7,80,4993,5017,177,'normal','plain','https://example.org/path','https://example.org/path',NULL,'https','//example.org/path',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(252,7,80,5091,5116,181,'normal','plain','https://example.org/path/','https://example.org/path/',NULL,'https','//example.org/path/',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(253,7,80,5119,5144,182,'normal','plain','https://example.org/path-','https://example.org/path-',NULL,'https','//example.org/path-',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(254,7,81,5326,5352,189,'normal','plain','file:./plain-links.org::10','file:./plain-links.org::10',NULL,'file','./plain-links.org','10',NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(255,7,81,5355,5385,190,'normal','plain','file:./plain-links.org::*Plain','file:./plain-links.org::*Plain',NULL,'file','./plain-links.org','*Plain',NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(256,7,81,5394,5433,191,'normal','plain','file:./plain-links.org::#some-custom-id','file:./plain-links.org::#some-custom-id',NULL,'file','./plain-links.org','#some-custom-id',NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(257,7,81,5436,5468,192,'normal','plain','file:./plain-links.org::/regexp/','file:./plain-links.org::/regexp/',NULL,'file','./plain-links.org','/regexp/',NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(258,7,81,5471,5485,193,'normal','plain','file:::current','file:::current',NULL,'file','','current',NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(259,7,81,5500,5530,194,'normal','plain','file+sys:./plain-links.org::10','file+sys:./plain-links.org::10',NULL,'file+sys','./plain-links.org','10',NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(260,7,81,5533,5565,195,'normal','plain','file+emacs:./plain-links.org::10','file+emacs:./plain-links.org::10',NULL,'file+emacs','./plain-links.org','10',NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(261,7,81,5638,5651,199,'normal','plain','id:abc123::10','id:abc123::10',NULL,'id','abc123::10',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(262,7,81,5654,5681,200,'normal','plain','attachment:projects.org::10','attachment:projects.org::10',NULL,'attachment','projects.org::10',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(263,7,81,5684,5711,201,'normal','plain','docview:papers/last.pdf::12','docview:papers/last.pdf::12',NULL,'docview','papers/last.pdf::12',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(264,7,81,5714,5730,202,'normal','plain','jira:ABC-123::10','jira:ABC-123::10',NULL,'jira','ABC-123::10',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(265,7,81,5733,5756,203,'normal','plain','customlink:file.org::10','customlink:file.org::10',NULL,'customlink','file.org::10',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(266,7,84,6975,7007,259,'normal','plain','https://example.org/in-paragraph','https://example.org/in-paragraph',NULL,'https','//example.org/in-paragraph',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(267,7,84,7024,7058,262,'verse_block','plain','https://example.org/in-verse-block','https://example.org/in-verse-block',NULL,'https','//example.org/in-verse-block',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(268,7,84,7086,7120,266,'quote_block','plain','https://example.org/in-quote-block','https://example.org/in-quote-block',NULL,'https','//example.org/in-quote-block',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(269,7,84,7149,7184,270,'center_block','plain','https://example.org/in-center-block','https://example.org/in-center-block',NULL,'https','//example.org/in-center-block',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(270,7,84,7215,7251,274,'justify_block','plain','https://example.org/in-justify-block','https://example.org/in-justify-block',NULL,'https','//example.org/in-justify-block',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(271,7,84,7298,7336,278,'property_drawer','plain','https://example.org/in-property-drawer','https://example.org/in-property-drawer',NULL,'https','//example.org/in-property-drawer',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(272,7,84,7355,7391,282,'drawer','plain','https://example.org/in-normal-drawer','https://example.org/in-normal-drawer',NULL,'https','//example.org/in-normal-drawer',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(273,7,86,7544,7568,291,'normal','plain','https://orgmode.org/🔥','https://orgmode.org/🔥',NULL,'https','//orgmode.org/🔥',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(274,7,86,7571,7590,292,'normal','plain','file:sub/äöü.txt','file:sub/äöü.txt',NULL,'file','sub/äöü.txt',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(275,7,86,7593,7620,293,'normal','plain','file:sub/äöü.txt::target','file:sub/äöü.txt::target',NULL,'file','sub/äöü.txt','target',NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(276,7,86,7623,7645,294,'normal','plain','customlink:äöü-😀','customlink:äöü-😀',NULL,'customlink','äöü-😀',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(277,8,87,73,134,5,'normal','bracket','[[file:../../notes/org-semantics/multipe-title-keywords.org]]','file:../../notes/org-semantics/multipe-title-keywords.org',NULL,'file','../../notes/org-semantics/multipe-title-keywords.org',NULL,NULL,NULL,NULL,NULL,NULL);
INSERT INTO links VALUES(278,11,122,286,298,17,'normal','plain','file:sibling','file:sibling',NULL,'file','sibling',NULL,NULL,NULL,NULL,NULL,NULL);
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
