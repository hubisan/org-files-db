# SQLite DB preview

## files

| id |                                                path                                                 |      mtime_ns       | size | content_hash | indexed_at |
|----|-----------------------------------------------------------------------------------------------------|---------------------|------|--------------|------------|
| 1  | /home/hubisan/projects/coding/org-files-db/.project/manual-tests/files/bracket-links.org            | 1782811598000160725 | 3503 |              | 1782811623 |
| 2  | /home/hubisan/projects/coding/org-files-db/.project/manual-tests/files/file-local-todo-keywords.org | 1782810515620385647 | 1188 |              | 1782811623 |
| 3  | /home/hubisan/projects/coding/org-files-db/.project/manual-tests/files/keywords.org                 | 1782237960833303823 | 2276 |              | 1782811623 |
| 4  | /home/hubisan/projects/coding/org-files-db/.project/manual-tests/files/multipe-title-keywords.org   | 1781809982908544855 | 321  |              | 1782811623 |
| 5  | /home/hubisan/projects/coding/org-files-db/.project/manual-tests/files/planning-lines.org           | 1782129087807207205 | 1163 |              | 1782811623 |
| 6  | /home/hubisan/projects/coding/org-files-db/.project/manual-tests/files/properties.org               | 1782218329407221851 | 2783 |              | 1782811623 |
| 7  | /home/hubisan/projects/coding/org-files-db/.project/manual-tests/files/tags.org                     | 1782243198853649058 | 1307 |              | 1782811623 |
| 8  | /home/hubisan/projects/coding/org-files-db/.project/manual-tests/files/timestamp-repeaters.org      | 1782159459016426918 | 1626 |              | 1782811623 |
| 9  | /home/hubisan/projects/coding/org-files-db/.project/manual-tests/files/timestamps.org               | 1782153027780066037 | 299  |              | 1782811623 |

## heading_bodies

| heading_id |                                 body_text                                  | body_byte_start | body_byte_end |
|------------|----------------------------------------------------------------------------|-----------------|---------------|
| 1          | # Links before the first real heading should attach to synthetic root.     | 50              | 288           |
|            | [[FILE:root-notes.org::42]]                                                |                 |               |
|            | [[root target][root description]]                                          |                 |               |
|            | https://example.org/root-plain-should-not-be-stored                        |                 |               |
|            | <https://example.org/root-angle-should-not-be-stored>                      |                 |               |
| 3          | - [[#internal-link-to-custom-id]]                                          | 393             | 501           |
|            | - [[#internal-link-to-custom-id][description: internal-link-to-custom-id]] |                 |               |
| 4          | - [[*Internal bracket links]]                                              | 526             | 612           |
|            | - [[*Internal bracket links][description: heading link]]                   |                 |               |
| 5          | <<dedicated target>>                                                       | 640             | 737           |
|            | - [[dedicated target]]                                                     |                 |               |
|            | - [[dedicated target][description: dedicated target]]                      |                 |               |
| 6          | #+NAME: named target                                                       | 773             | 858           |
|            | - [[named target]]                                                         |                 |               |
|            | - [[named target][description: named target]]                              |                 |               |
| 7          | - [[no matching target]]                                                   | 878             | 976           |
|            | - [[no matching target][description: no matching target]]                  |                 |               |
|            | - [[notes.org]]                                                            |                 |               |
| 9          | - [[file:/etc]]                                                            | 1028            | 1315          |
|            | - [[file:/etc][description: /etc]]                                         |                 |               |
|            | - [[file:/etc/]]                                                           |                 |               |
|            | - [[file:/etc/host.conf]]                                                  |                 |               |
|            | - [[file:../]]                                                             |                 |               |
|            | - [[file:../../sql]]                                                       |                 |               |
|            | - [[file:../../sql/]]                                                      |                 |               |
|            | - [[file:../parser_test.rs]]                                               |                 |               |
|            | - [[file:./org-test-links.org]]                                            |                 |               |
|            | - [[file:~/.emacs.d]]                                                      |                 |               |
|            | - [[file:~/.emacs.d/]]                                                     |                 |               |
|            | - [[file:~/.emacs.d/init.el]]                                              |                 |               |
| 10         | - [[file+sys:/etc]]                                                        | 1343            | 1484          |
|            | - [[file+emacs:/etc]]                                                      |                 |               |
|            | - [[file+sys:/etc][description: file+sys:/etc]]                            |                 |               |
|            | - [[file+emacs:/etc][description: file+emacs:/etc]]                        |                 |               |
| 11         | - [[/etc]]                                                                 | 1509            | 1662          |
|            | - [[/etc/]]                                                                |                 |               |
|            | - [[/etc/host.conf]]                                                       |                 |               |
|            | - [[../]]                                                                  |                 |               |
|            | - [[../../sql]]                                                            |                 |               |
|            | - [[../../sql/]]                                                           |                 |               |
|            | - [[../parser_test.rs]]                                                    |                 |               |
|            | - [[./org-test-links.org]]                                                 |                 |               |
|            | - [[~/memento]]                                                            |                 |               |
| 12         | - [[file:./org-test-links.org::10]]                                        | 1687            | 2220          |
|            | - [[file:./org-test-links.org::#internal-link-to-custom-id]]               |                 |               |
|            | - [[file:./org-test-links.org::dedicated target]]                          |                 |               |
|            | - [[file:./org-test-links.org::*Internal bracket links]]                   |                 |               |
|            | - [[file:./org-test-links.org::/*.File-like.*/]]                           |                 |               |
|            | - [[./org-test-links.org::10]]                                             |                 |               |
|            | - [[./org-test-links.org::#internal-link-to-custom-id]]                    |                 |               |
|            | - [[./org-test-links.org::dedicated target]]                               |                 |               |
|            | - [[./org-test-links.org::*Internal bracket links]]                        |                 |               |
|            | - [[./org-test-links.org::/*.File-like.*/]]                                |                 |               |
|            | - [[file:::10]]                                                            |                 |               |
|            | - [[file:::*Internal bracket links]]                                       |                 |               |

## headings

| id | file_id | parent_id | level | line_number | byte_start | byte_end |             title              |           title_raw            | todo_keyword | todo_type | priority | scheduled_raw | scheduled_ts | deadline_raw | deadline_ts | closed_raw | closed_ts | archivedp | footnote_section_p | all_tags_json |
|----|---------|-----------|-------|-------------|------------|----------|--------------------------------|--------------------------------|--------------|-----------|----------|---------------|--------------|--------------|-------------|------------|-----------|-----------|--------------------|---------------|
| 1  | 1       |           | 0     | 1           | -1         | 3503     | Bracket Link Fixture           | Bracket Link Fixture           |              |           |          |               |              |              |             |            |           | 0         | 0                  | []            |
| 2  | 1       | 1         | 1     | 10          | 290        | 978      | Internal bracket links         | Internal bracket links         |              |           |          |               |              |              |             |            |           | 0         | 0                  | []            |
| 3  | 1       | 2         | 2     | 15          | 374        | 503      | Custom ID links                | Custom ID links                |              |           |          |               |              |              |             |            |           | 0         | 0                  | []            |
| 4  | 1       | 2         | 2     | 19          | 503        | 614      | Fuzzy heading links            | Fuzzy heading links            |              |           |          |               |              |              |             |            |           | 0         | 0                  | []            |
| 5  | 1       | 2         | 2     | 23          | 614        | 739      | Dedicated target links         | Dedicated target links         |              |           |          |               |              |              |             |            |           | 0         | 0                  | []            |
| 6  | 1       | 2         | 2     | 28          | 739        | 860      | Named target style fuzzy links | Named target style fuzzy links |              |           |          |               |              |              |             |            |           | 0         | 0                  | []            |
| 7  | 1       | 2         | 2     | 33          | 860        | 978      | Fuzzy fallback                 | Fuzzy fallback                 |              |           |          |               |              |              |             |            |           | 0         | 0                  | []            |
| 8  | 1       | 1         | 1     | 38          | 978        | 2222     | File-like bracket links        | File-like bracket links        |              |           |          |               |              |              |             |            |           | 0         | 0                  | []            |
| 9  | 1       | 8         | 2     | 40          | 1005       | 1317     | Explicit file links            | Explicit file links            |              |           |          |               |              |              |             |            |           | 0         | 0                  | []            |
| 10 | 1       | 8         | 2     | 54          | 1317       | 1486     | Explicit file variants         | Explicit file variants         |              |           |          |               |              |              |             |            |           | 0         | 0                  | []            |

## keywords

| id | heading_id | keyword  |               value               | line_number |
|----|------------|----------|-----------------------------------|-------------|
| 1  | 1          | TITLE    | Bracket Link Fixture              | 1           |
| 2  | 1          | STARTUP  | content                           | 2           |
| 3  | 19         | TITLE    | File-local TODO keywords          | 1           |
| 4  | 19         | STARTUP  | showall                           | 2           |
| 5  | 19         | TODO     | one(t) two(n) | three(d) four(w@) | 3           |
| 6  | 19         | TODO     | FIVE SIX |                        | 4           |
| 7  | 19         | TYP_TODO | seven | eight                     | 5           |
| 8  | 19         | SEQ_TODO | nine | ten                        | 6           |
| 9  | 19         | TODO     | | eleven(c)                       | 30          |
| 10 | 19         | TODO     | late_open | late_done             | 34          |

## links

| id | file_id | heading_id | byte_start | byte_end | line | source_context | format  |                                   raw                                    |         raw_target          |             raw_description             | link_type |            path             | search_option | path_absolute | target_file_id | target_heading_id | target_custom_id | target_id |
|----|---------|------------|------------|----------|------|----------------|---------|--------------------------------------------------------------------------|-----------------------------|-----------------------------------------|-----------|-----------------------------|---------------|---------------|----------------|-------------------|------------------|-----------|
| 1  | 1       | 1          | 121        | 148      | 5    | normal         | bracket | [[FILE:root-notes.org::42]]                                              | FILE:root-notes.org::42     |                                         | file      | root-notes.org              | 42            |               |                |                   |                  |           |
| 2  | 1       | 1          | 149        | 182      | 6    | normal         | bracket | [[root target][root description]]                                        | root target                 | root description                        | fuzzy     | root target                 |               |               |                |                   |                  |           |
| 3  | 1       | 3          | 395        | 426      | 16   | normal         | bracket | [[#internal-link-to-custom-id]]                                          | #internal-link-to-custom-id |                                         | custom-id | #internal-link-to-custom-id |               |               |                |                   |                  |           |
| 4  | 1       | 3          | 429        | 501      | 17   | normal         | bracket | [[#internal-link-to-custom-id][description: internal-link-to-custom-id]] | #internal-link-to-custom-id | description: internal-link-to-custom-id | custom-id | #internal-link-to-custom-id |               |               |                |                   |                  |           |
| 5  | 1       | 4          | 528        | 555      | 20   | normal         | bracket | [[*Internal bracket links]]                                              | *Internal bracket links     |                                         | fuzzy     | *Internal bracket links     |               |               |                |                   |                  |           |
| 6  | 1       | 4          | 558        | 612      | 21   | normal         | bracket | [[*Internal bracket links][description: heading link]]                   | *Internal bracket links     | description: heading link               | fuzzy     | *Internal bracket links     |               |               |                |                   |                  |           |
| 7  | 1       | 5          | 663        | 683      | 25   | normal         | bracket | [[dedicated target]]                                                     | dedicated target            |                                         | fuzzy     | dedicated target            |               |               |                |                   |                  |           |
| 8  | 1       | 5          | 686        | 737      | 26   | normal         | bracket | [[dedicated target][description: dedicated target]]                      | dedicated target            | description: dedicated target           | fuzzy     | dedicated target            |               |               |                |                   |                  |           |
| 9  | 1       | 6          | 796        | 812      | 30   | normal         | bracket | [[named target]]                                                         | named target                |                                         | fuzzy     | named target                |               |               |                |                   |                  |           |
| 10 | 1       | 6          | 815        | 858      | 31   | normal         | bracket | [[named target][description: named target]]                              | named target                | description: named target               | fuzzy     | named target                |               |               |                |                   |                  |           |

## outline_path

| heading_id | file_id | parent_id | depth | materialized_path |                                  breadcrumbs_json                                  |
|------------|---------|-----------|-------|-------------------|------------------------------------------------------------------------------------|
| 1          | 1       |           | 0     | 0000              | ["Bracket Link Fixture"]                                                           |
| 2          | 1       | 1         | 1     | 0000.0001         | ["Bracket Link Fixture","Internal bracket links"]                                  |
| 3          | 1       | 2         | 2     | 0000.0001.0001    | ["Bracket Link Fixture","Internal bracket links","Custom ID links"]                |
| 4          | 1       | 2         | 2     | 0000.0001.0002    | ["Bracket Link Fixture","Internal bracket links","Fuzzy heading links"]            |
| 5          | 1       | 2         | 2     | 0000.0001.0003    | ["Bracket Link Fixture","Internal bracket links","Dedicated target links"]         |
| 6          | 1       | 2         | 2     | 0000.0001.0004    | ["Bracket Link Fixture","Internal bracket links","Named target style fuzzy links"] |
| 7          | 1       | 2         | 2     | 0000.0001.0005    | ["Bracket Link Fixture","Internal bracket links","Fuzzy fallback"]                 |
| 8          | 1       | 1         | 1     | 0000.0002         | ["Bracket Link Fixture","File-like bracket links"]                                 |
| 9          | 1       | 8         | 2     | 0000.0002.0001    | ["Bracket Link Fixture","File-like bracket links","Explicit file links"]           |
| 10         | 1       | 8         | 2     | 0000.0002.0002    | ["Bracket Link Fixture","File-like bracket links","Explicit file variants"]        |

## properties

| id | heading_id |      key      |           value            |      source      | append | line_number |
|----|------------|---------------|----------------------------|------------------|--------|-------------|
| 1  | 2          | CUSTOM_ID     | internal-link-to-custom-id | property_drawer  | 0      | 12          |
| 2  | 37         | BEFORE_PROP   | before-value               | property_keyword | 0      | 4           |
| 3  | 37         | CATEGORY      | before-category            | category_keyword | 0      | 5           |
| 4  | 37         | AFTER_PROP    | after-value                | property_keyword | 0      | 12          |
| 5  | 37         | REPEATED_PROP | first                      | property_keyword | 0      | 13          |
| 6  | 37         | REPEATED_PROP | second                     | property_keyword | 0      | 14          |
| 7  | 37         | APPENDED_PROP | base                       | property_keyword | 0      | 15          |
| 8  | 37         | APPENDED_PROP | extra                      | property_keyword | 1      | 16          |
| 9  | 37         | CATEGORY      | after-category             | category_keyword | 0      | 17          |
| 10 | 63         | CATEGORY      | Level 0 Category Property  | property_drawer  | 0      | 2           |

## tags

| heading_id |   tag   |
|------------|---------|
| 18         | foo     |
| 18         | bar     |
| 63         | project |
| 63         | work    |
| 73         | file    |
| 73         | project |
| 73         | later   |
| 73         | extra   |
| 74         | parent  |
| 75         | child   |

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
| 1  | 47         | scheduled | 1732060800 |            | active   | none       | <2024-11-20 Wed>                   | 93         | 109      | 7           |
| 2  | 48         | deadline  | 1733011200 |            | active   | none       | <2024-12-01 Sun>                   | 140        | 156      | 10          |
| 3  | 49         | closed    | 1732147200 |            | inactive | none       | [2024-11-21 Thu]                   | 183        | 199      | 13          |
| 4  | 50         | deadline  | 1733011200 |            | active   | none       | <2024-12-01 Sun>                   | 239        | 255      | 16          |
| 5  | 50         | scheduled | 1732060800 |            | active   | none       | <2024-11-20 Wed>                   | 267        | 283      | 16          |
| 6  | 50         | closed    | 1732147200 |            | inactive | none       | [2024-11-21 Thu]                   | 292        | 308      | 16          |
| 7  | 51         | scheduled | 1732095000 |            | active   | none       | <2024-11-20 Wed 09:30>             | 334        | 356      | 19          |
| 8  | 52         | scheduled | 1732095000 | 1732100400 | active   | time_range | <2024-11-20 Wed 09:30-11:00>       | 392        | 420      | 22          |
| 9  | 53         | deadline  | 1733011200 | 1733184000 | active   | date_range | <2024-12-01 Sun>--<2024-12-03 Tue> | 446        | 480      | 25          |
| 10 | 54         | scheduled | 1732060800 |            | active   | none       | <2024-11-20 Wed +1w>               | 505        | 525      | 28          |

## todo_keywords

| file_id |  keyword  | state_type | shortcut | sequence_no |  source_kind   | source_keyword | source_line_number |
|---------|-----------|------------|----------|-------------|----------------|----------------|--------------------|
| 1       | TODO      | open       |          | 0           | config_default |                |                    |
| 1       | DONE      | closed     |          | 1           | config_default |                |                    |
| 2       | one       | open       | t        | 0           | org_keyword    | TODO           | 3                  |
| 2       | two       | open       | n        | 1           | org_keyword    | TODO           | 3                  |
| 2       | FIVE      | open       |          | 2           | org_keyword    | TODO           | 4                  |
| 2       | SIX       | open       |          | 3           | org_keyword    | TODO           | 4                  |
| 2       | seven     | open       |          | 4           | org_keyword    | TYP_TODO       | 5                  |
| 2       | nine      | open       |          | 5           | org_keyword    | SEQ_TODO       | 6                  |
| 2       | late_open | open       |          | 6           | org_keyword    | TODO           | 34                 |
| 2       | three     | closed     | d        | 7           | org_keyword    | TODO           | 3                  |

<!--
Local Variables:
eval: (visual-fill-column-mode -1)
truncate-lines: t
End:
-->
