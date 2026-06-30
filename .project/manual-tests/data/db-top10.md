# SQLite DB preview

## files

| id |                                                 path                                                 |      mtime_ns       | size | content_hash | indexed_at |
|----|------------------------------------------------------------------------------------------------------|---------------------|------|--------------|------------|
| 1  | /home/hubisan/projects/coding/org-files-db/.project/manual-tests/files/bracket-links--weird-ones.org | 1782827536254923818 | 1526 |              | 1782827554 |
| 2  | /home/hubisan/projects/coding/org-files-db/.project/manual-tests/files/bracket-links.org             | 1782811598000160725 | 3503 |              | 1782827554 |
| 3  | /home/hubisan/projects/coding/org-files-db/.project/manual-tests/files/file-local-todo-keywords.org  | 1782810515620385647 | 1188 |              | 1782827554 |
| 4  | /home/hubisan/projects/coding/org-files-db/.project/manual-tests/files/keywords.org                  | 1782237960833303823 | 2276 |              | 1782827554 |
| 5  | /home/hubisan/projects/coding/org-files-db/.project/manual-tests/files/multipe-title-keywords.org    | 1781809982908544855 | 321  |              | 1782827554 |
| 6  | /home/hubisan/projects/coding/org-files-db/.project/manual-tests/files/planning-lines.org            | 1782129087807207205 | 1163 |              | 1782827554 |
| 7  | /home/hubisan/projects/coding/org-files-db/.project/manual-tests/files/properties.org                | 1782218329407221851 | 2783 |              | 1782827554 |
| 8  | /home/hubisan/projects/coding/org-files-db/.project/manual-tests/files/tags.org                      | 1782243198853649058 | 1307 |              | 1782827554 |
| 9  | /home/hubisan/projects/coding/org-files-db/.project/manual-tests/files/timestamp-repeaters.org       | 1782159459016426918 | 1626 |              | 1782827554 |
| 10 | /home/hubisan/projects/coding/org-files-db/.project/manual-tests/files/timestamps.org                | 1782153027780066037 | 299  |              | 1782827554 |

## heading_bodies

| heading_id |                                                            body_text                                                            | body_byte_start | body_byte_end |
|------------|---------------------------------------------------------------------------------------------------------------------------------|-----------------|---------------|
| 1          | This results in a plain link as the \ needs to be escaped. So there is no actual ][ to make the link valid:                     | 50              | 1523          |
|            | - [[https://nok-no-bracket-link--plain-link.org\][Test]]                                                                        |                 |               |
|            |                                                                                                                                 |                 |               |
|            | Even number of backslashes don't escape and are ignored:                                                                        |                 |               |
|            | - [[https://ok-even-number-of-backslashes.org\\][OK]]                                                                           |                 |               |
|            |                                                                                                                                 |                 |               |
|            | Brackets in the link are valid if escaped with uneven number of backslashs:                                                     |                 |               |
|            | - [[https://ok-escaped-brackets-in-path.org\]\[brackets need to be escaped \[\] in the path\]][OK]]                             |                 |               |
|            | - [[https://ok-escaped-brackets-in-path.org\\\]\\\[brackets need to be escaped \[\\\] in the path\]][OK]]                       |                 |               |
|            |                                                                                                                                 |                 |               |
|            | In Description the escaping is not needed. As nested links are not allowed. Two brackets after another close the description.   |                 |               |
|            | - [[https://ok-description-double-bracket-closes-link][[Test]]]]                                                                |                 |               |
|            | - [[https://ok-description-double-bracket-closes-link][[[[[[Test]]]]                                                            |                 |               |
|            | - [[https://ok-description-double-bracket-closes-link][[][][]]]]                                                                |                 |               |
|            | - [[https://ok-description-double-bracket-closes-link][test [[https://test.com]]no more link                                    |                 |               |
|            |                                                                                                                                 |                 |               |
|            | Line breaks are not (yet) supported, or maybe never                                                                             |                 |               |
|            |                                                                                                                                 |                 |               |
|            | Bracket links and the description can include new lines, the starting. Only inside [], not when splitting \[\[ or \]\[ or \]\]. |                 |               |
|            | [[https://www.gnu.org                                                                                                           |                 |               |
|            | ][Link with new line]]                                                                                                          |                 |               |
|            |                                                                                                                                 |                 |               |
|            | In the path a newline is represented as a space.                                                                                |                 |               |
|            | [[https://www.gnu.org                                                                                                           |                 |               |
|            | ][sdfdsf]]                                                                                                                      |                 |               |
|            |                                                                                                                                 |                 |               |
|            | Indentation on the second line is ignored.                                                                                      |                 |               |
|            | [[https://www.gnu.org                                                                                                           |                 |               |
|            |                  x    x][sdfdsf]]                                                                                               |                 |               |
|            |                                                                                                                                 |                 |               |
|            | Noch ein Umlaut im Path:                                                                                                        |                 |               |
|            | [[file:sub/äöü.txt::target][A file link with Umlaut]]                                                                           |                 |               |
| 2          | # Links before the first real heading should attach to synthetic root.                                                          | 50              | 288           |
|            | [[FILE:root-notes.org::42]]                                                                                                     |                 |               |
|            | [[root target][root description]]                                                                                               |                 |               |
|            | https://example.org/root-plain-should-not-be-stored                                                                             |                 |               |
|            | <https://example.org/root-angle-should-not-be-stored>                                                                           |                 |               |
| 4          | - [[#internal-link-to-custom-id]]                                                                                               | 393             | 501           |
|            | - [[#internal-link-to-custom-id][description: internal-link-to-custom-id]]                                                      |                 |               |
| 5          | - [[*Internal bracket links]]                                                                                                   | 526             | 612           |
|            | - [[*Internal bracket links][description: heading link]]                                                                        |                 |               |
| 6          | <<dedicated target>>                                                                                                            | 640             | 737           |
|            | - [[dedicated target]]                                                                                                          |                 |               |
|            | - [[dedicated target][description: dedicated target]]                                                                           |                 |               |
| 7          | #+NAME: named target                                                                                                            | 773             | 858           |
|            | - [[named target]]                                                                                                              |                 |               |
|            | - [[named target][description: named target]]                                                                                   |                 |               |
| 8          | - [[no matching target]]                                                                                                        | 878             | 976           |
|            | - [[no matching target][description: no matching target]]                                                                       |                 |               |
|            | - [[notes.org]]                                                                                                                 |                 |               |
| 10         | - [[file:/etc]]                                                                                                                 | 1028            | 1315          |
|            | - [[file:/etc][description: /etc]]                                                                                              |                 |               |
|            | - [[file:/etc/]]                                                                                                                |                 |               |
|            | - [[file:/etc/host.conf]]                                                                                                       |                 |               |
|            | - [[file:../]]                                                                                                                  |                 |               |
|            | - [[file:../../sql]]                                                                                                            |                 |               |
|            | - [[file:../../sql/]]                                                                                                           |                 |               |
|            | - [[file:../parser_test.rs]]                                                                                                    |                 |               |
|            | - [[file:./org-test-links.org]]                                                                                                 |                 |               |
|            | - [[file:~/.emacs.d]]                                                                                                           |                 |               |
|            | - [[file:~/.emacs.d/]]                                                                                                          |                 |               |
|            | - [[file:~/.emacs.d/init.el]]                                                                                                   |                 |               |
| 11         | - [[file+sys:/etc]]                                                                                                             | 1343            | 1484          |
|            | - [[file+emacs:/etc]]                                                                                                           |                 |               |
|            | - [[file+sys:/etc][description: file+sys:/etc]]                                                                                 |                 |               |
|            | - [[file+emacs:/etc][description: file+emacs:/etc]]                                                                             |                 |               |
| 12         | - [[/etc]]                                                                                                                      | 1509            | 1662          |
|            | - [[/etc/]]                                                                                                                     |                 |               |
|            | - [[/etc/host.conf]]                                                                                                            |                 |               |
|            | - [[../]]                                                                                                                       |                 |               |
|            | - [[../../sql]]                                                                                                                 |                 |               |
|            | - [[../../sql/]]                                                                                                                |                 |               |
|            | - [[../parser_test.rs]]                                                                                                         |                 |               |
|            | - [[./org-test-links.org]]                                                                                                      |                 |               |
|            | - [[~/memento]]                                                                                                                 |                 |               |

## headings

| id | file_id | parent_id | level | line_number | byte_start | byte_end |             title              |           title_raw            | todo_keyword | todo_type | priority | scheduled_raw | scheduled_ts | deadline_raw | deadline_ts | closed_raw | closed_ts | archivedp | footnote_section_p | all_tags_json |
|----|---------|-----------|-------|-------------|------------|----------|--------------------------------|--------------------------------|--------------|-----------|----------|---------------|--------------|--------------|-------------|------------|-----------|-----------|--------------------|---------------|
| 1  | 1       |           | 0     | 1           | -1         | 1526     | Bracket Link Fixture           | Bracket Link Fixture           |              |           |          |               |              |              |             |            |           | 0         | 0                  | []            |
| 2  | 2       |           | 0     | 1           | -1         | 3503     | Bracket Link Fixture           | Bracket Link Fixture           |              |           |          |               |              |              |             |            |           | 0         | 0                  | []            |
| 3  | 2       | 2         | 1     | 10          | 290        | 978      | Internal bracket links         | Internal bracket links         |              |           |          |               |              |              |             |            |           | 0         | 0                  | []            |
| 4  | 2       | 3         | 2     | 15          | 374        | 503      | Custom ID links                | Custom ID links                |              |           |          |               |              |              |             |            |           | 0         | 0                  | []            |
| 5  | 2       | 3         | 2     | 19          | 503        | 614      | Fuzzy heading links            | Fuzzy heading links            |              |           |          |               |              |              |             |            |           | 0         | 0                  | []            |
| 6  | 2       | 3         | 2     | 23          | 614        | 739      | Dedicated target links         | Dedicated target links         |              |           |          |               |              |              |             |            |           | 0         | 0                  | []            |
| 7  | 2       | 3         | 2     | 28          | 739        | 860      | Named target style fuzzy links | Named target style fuzzy links |              |           |          |               |              |              |             |            |           | 0         | 0                  | []            |
| 8  | 2       | 3         | 2     | 33          | 860        | 978      | Fuzzy fallback                 | Fuzzy fallback                 |              |           |          |               |              |              |             |            |           | 0         | 0                  | []            |
| 9  | 2       | 2         | 1     | 38          | 978        | 2222     | File-like bracket links        | File-like bracket links        |              |           |          |               |              |              |             |            |           | 0         | 0                  | []            |
| 10 | 2       | 9         | 2     | 40          | 1005       | 1317     | Explicit file links            | Explicit file links            |              |           |          |               |              |              |             |            |           | 0         | 0                  | []            |

## keywords

| id | heading_id | keyword  |               value               | line_number |
|----|------------|----------|-----------------------------------|-------------|
| 1  | 1          | TITLE    | Bracket Link Fixture              | 1           |
| 2  | 1          | STARTUP  | content                           | 2           |
| 3  | 2          | TITLE    | Bracket Link Fixture              | 1           |
| 4  | 2          | STARTUP  | content                           | 2           |
| 5  | 20         | TITLE    | File-local TODO keywords          | 1           |
| 6  | 20         | STARTUP  | showall                           | 2           |
| 7  | 20         | TODO     | one(t) two(n) | three(d) four(w@) | 3           |
| 8  | 20         | TODO     | FIVE SIX |                        | 4           |
| 9  | 20         | TYP_TODO | seven | eight                     | 5           |
| 10 | 20         | SEQ_TODO | nine | ten                        | 6           |

## links

| id | file_id | heading_id | byte_start | byte_end | line | source_context | format  |                                                   raw                                                   |                                           raw_target                                            |     raw_description     | link_type |                                           path                                            | search_option | path_absolute | target_file_id | target_heading_id | target_custom_id | target_id |
|----|---------|------------|------------|----------|------|----------------|---------|---------------------------------------------------------------------------------------------------------|-------------------------------------------------------------------------------------------------|-------------------------|-----------|-------------------------------------------------------------------------------------------|---------------|---------------|----------------|-------------------|------------------|-----------|
| 1  | 1       | 1          | 275        | 326      | 8    | normal         | bracket | [[https://ok-even-number-of-backslashes.org\\][OK]]                                                     | https://ok-even-number-of-backslashes.org\\                                                     | OK                      | https     | //ok-even-number-of-backslashes.org\\                                                     |               |               |                |                   |                  |           |
| 2  | 1       | 1          | 406        | 503      | 11   | normal         | bracket | [[https://ok-escaped-brackets-in-path.org\]\[brackets need to be escaped \[\] in the path\]][OK]]       | https://ok-escaped-brackets-in-path.org\]\[brackets need to be escaped \[\] in the path\]       | OK                      | https     | //ok-escaped-brackets-in-path.org\]\[brackets need to be escaped \[\] in the path\]       |               |               |                |                   |                  |           |
| 3  | 1       | 1          | 506        | 609      | 12   | normal         | bracket | [[https://ok-escaped-brackets-in-path.org\\\]\\\[brackets need to be escaped \[\\\] in the path\]][OK]] | https://ok-escaped-brackets-in-path.org\\\]\\\[brackets need to be escaped \[\\\] in the path\] | OK                      | https     | //ok-escaped-brackets-in-path.org\\\]\\\[brackets need to be escaped \[\\\] in the path\] |               |               |                |                   |                  |           |
| 4  | 1       | 1          | 739        | 799      | 15   | normal         | bracket | [[https://ok-description-double-bracket-closes-link][[Test]]                                            | https://ok-description-double-bracket-closes-link                                               | [Test                   | https     | //ok-description-double-bracket-closes-link                                               |               |               |                |                   |                  |           |
| 5  | 1       | 1          | 804        | 868      | 16   | normal         | bracket | [[https://ok-description-double-bracket-closes-link][[[[[[Test]]                                        | https://ok-description-double-bracket-closes-link                                               | [[[[[Test               | https     | //ok-description-double-bracket-closes-link                                               |               |               |                |                   |                  |           |
| 6  | 1       | 1          | 875        | 935      | 17   | normal         | bracket | [[https://ok-description-double-bracket-closes-link][[][][]]                                            | https://ok-description-double-bracket-closes-link                                               | [][][                   | https     | //ok-description-double-bracket-closes-link                                               |               |               |                |                   |                  |           |
| 7  | 1       | 1          | 940        | 1018     | 18   | normal         | bracket | [[https://ok-description-double-bracket-closes-link][test [[https://test.com]]                          | https://ok-description-double-bracket-closes-link                                               | test [[https://test.com | https     | //ok-description-double-bracket-closes-link                                               |               |               |                |                   |                  |           |
| 8  | 1       | 1          | 1467       | 1523     | 35   | normal         | bracket | [[file:sub/äöü.txt::target][A file link with Umlaut]]                                                   | file:sub/äöü.txt::target                                                                        | A file link with Umlaut | file      | sub/äöü.txt                                                                               | target        |               |                |                   |                  |           |
| 9  | 2       | 2          | 121        | 148      | 5    | normal         | bracket | [[FILE:root-notes.org::42]]                                                                             | FILE:root-notes.org::42                                                                         |                         | file      | root-notes.org                                                                            | 42            |               |                |                   |                  |           |
| 10 | 2       | 2          | 149        | 182      | 6    | normal         | bracket | [[root target][root description]]                                                                       | root target                                                                                     | root description        | fuzzy     | root target                                                                               |               |               |                |                   |                  |           |

## outline_path

| heading_id | file_id | parent_id | depth | materialized_path |                                  breadcrumbs_json                                  |
|------------|---------|-----------|-------|-------------------|------------------------------------------------------------------------------------|
| 1          | 1       |           | 0     | 0000              | ["Bracket Link Fixture"]                                                           |
| 2          | 2       |           | 0     | 0000              | ["Bracket Link Fixture"]                                                           |
| 3          | 2       | 2         | 1     | 0000.0001         | ["Bracket Link Fixture","Internal bracket links"]                                  |
| 4          | 2       | 3         | 2     | 0000.0001.0001    | ["Bracket Link Fixture","Internal bracket links","Custom ID links"]                |
| 5          | 2       | 3         | 2     | 0000.0001.0002    | ["Bracket Link Fixture","Internal bracket links","Fuzzy heading links"]            |
| 6          | 2       | 3         | 2     | 0000.0001.0003    | ["Bracket Link Fixture","Internal bracket links","Dedicated target links"]         |
| 7          | 2       | 3         | 2     | 0000.0001.0004    | ["Bracket Link Fixture","Internal bracket links","Named target style fuzzy links"] |
| 8          | 2       | 3         | 2     | 0000.0001.0005    | ["Bracket Link Fixture","Internal bracket links","Fuzzy fallback"]                 |
| 9          | 2       | 2         | 1     | 0000.0002         | ["Bracket Link Fixture","File-like bracket links"]                                 |
| 10         | 2       | 9         | 2     | 0000.0002.0001    | ["Bracket Link Fixture","File-like bracket links","Explicit file links"]           |

## properties

| id | heading_id |      key      |           value            |      source      | append | line_number |
|----|------------|---------------|----------------------------|------------------|--------|-------------|
| 1  | 3          | CUSTOM_ID     | internal-link-to-custom-id | property_drawer  | 0      | 12          |
| 2  | 38         | BEFORE_PROP   | before-value               | property_keyword | 0      | 4           |
| 3  | 38         | CATEGORY      | before-category            | category_keyword | 0      | 5           |
| 4  | 38         | AFTER_PROP    | after-value                | property_keyword | 0      | 12          |
| 5  | 38         | REPEATED_PROP | first                      | property_keyword | 0      | 13          |
| 6  | 38         | REPEATED_PROP | second                     | property_keyword | 0      | 14          |
| 7  | 38         | APPENDED_PROP | base                       | property_keyword | 0      | 15          |
| 8  | 38         | APPENDED_PROP | extra                      | property_keyword | 1      | 16          |
| 9  | 38         | CATEGORY      | after-category             | category_keyword | 0      | 17          |
| 10 | 64         | CATEGORY      | Level 0 Category Property  | property_drawer  | 0      | 2           |

## tags

| heading_id |   tag   |
|------------|---------|
| 19         | foo     |
| 19         | bar     |
| 64         | project |
| 64         | work    |
| 74         | file    |
| 74         | project |
| 74         | later   |
| 74         | extra   |
| 75         | parent  |
| 76         | child   |

## timestamp_repeaters

| id | timestamp_id | repeater_type | repeater_value | repeater_unit | repeater_deadline_value | repeater_deadline_unit | warning_type | warning_value | warning_unit |
|----|--------------|---------------|----------------|---------------|-------------------------|------------------------|--------------|---------------|--------------|
| 1  | 10           | cumulate      | 1              | week          |                         |                        |              |               |              |
| 2  | 22           | cumulate      | 1              | week          |                         |                        |              |               |              |
| 3  | 23           | catch_up      | 1              | month         |                         |                        |              |               |              |
| 4  | 24           | restart       | 2              | day           |                         |                        |              |               |              |
| 5  | 25           | cumulate      | 3              | hour          |                         |                        |              |               |              |
| 6  | 26           | cumulate      | 3              | day           |                         |                        |              |               |              |
| 7  | 27           | cumulate      | 3              | week          |                         |                        |              |               |              |
| 8  | 28           | cumulate      | 3              | month         |                         |                        |              |               |              |
| 9  | 29           | cumulate      | 3              | year          |                         |                        |              |               |              |
| 10 | 30           | cumulate      | 1              | week          | 2                       | day                    |              |               |              |

## timestamps

| id | heading_id |   role    |  start_ts  |   end_ts   |   type   | range_type |             raw_value              | byte_start | byte_end | line_number |
|----|------------|-----------|------------|------------|----------|------------|------------------------------------|------------|----------|-------------|
| 1  | 48         | scheduled | 1732060800 |            | active   | none       | <2024-11-20 Wed>                   | 93         | 109      | 7           |
| 2  | 49         | deadline  | 1733011200 |            | active   | none       | <2024-12-01 Sun>                   | 140        | 156      | 10          |
| 3  | 50         | closed    | 1732147200 |            | inactive | none       | [2024-11-21 Thu]                   | 183        | 199      | 13          |
| 4  | 51         | deadline  | 1733011200 |            | active   | none       | <2024-12-01 Sun>                   | 239        | 255      | 16          |
| 5  | 51         | scheduled | 1732060800 |            | active   | none       | <2024-11-20 Wed>                   | 267        | 283      | 16          |
| 6  | 51         | closed    | 1732147200 |            | inactive | none       | [2024-11-21 Thu]                   | 292        | 308      | 16          |
| 7  | 52         | scheduled | 1732095000 |            | active   | none       | <2024-11-20 Wed 09:30>             | 334        | 356      | 19          |
| 8  | 53         | scheduled | 1732095000 | 1732100400 | active   | time_range | <2024-11-20 Wed 09:30-11:00>       | 392        | 420      | 22          |
| 9  | 54         | deadline  | 1733011200 | 1733184000 | active   | date_range | <2024-12-01 Sun>--<2024-12-03 Tue> | 446        | 480      | 25          |
| 10 | 55         | scheduled | 1732060800 |            | active   | none       | <2024-11-20 Wed +1w>               | 505        | 525      | 28          |

## todo_keywords

| file_id | keyword | state_type | shortcut | sequence_no |  source_kind   | source_keyword | source_line_number |
|---------|---------|------------|----------|-------------|----------------|----------------|--------------------|
| 1       | TODO    | open       |          | 0           | config_default |                |                    |
| 1       | DONE    | closed     |          | 1           | config_default |                |                    |
| 2       | TODO    | open       |          | 0           | config_default |                |                    |
| 2       | DONE    | closed     |          | 1           | config_default |                |                    |
| 3       | one     | open       | t        | 0           | org_keyword    | TODO           | 3                  |
| 3       | two     | open       | n        | 1           | org_keyword    | TODO           | 3                  |
| 3       | FIVE    | open       |          | 2           | org_keyword    | TODO           | 4                  |
| 3       | SIX     | open       |          | 3           | org_keyword    | TODO           | 4                  |
| 3       | seven   | open       |          | 4           | org_keyword    | TYP_TODO       | 5                  |
| 3       | nine    | open       |          | 5           | org_keyword    | SEQ_TODO       | 6                  |

<!--
Local Variables:
eval: (visual-fill-column-mode -1)
truncate-lines: t
End:
-->
