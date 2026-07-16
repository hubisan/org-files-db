# SQLite DB preview

## db_metadata

|         key         | value |
|---------------------|-------|
| fts_available       | 0     |
| fts_body_indexed    | 0     |
| fts_schema_version  | 0     |
| body_text_available | 0     |

## files

| id |                                                path                                                |      mtime_ns       | size | content_hash | indexed_at |
|----|----------------------------------------------------------------------------------------------------|---------------------|------|--------------|------------|
| 1  | /home/hubisan/projects/coding/org-files-db/.project/manual-tests/query-model/org/00-index.org      | 1783801546374100331 | 1253 |              | 1784233408 |
| 2  | /home/hubisan/projects/coding/org-files-db/.project/manual-tests/query-model/org/archive/2025.org  | 1783796759000000000 | 229  |              | 1784233408 |
| 3  | /home/hubisan/projects/coding/org-files-db/.project/manual-tests/query-model/org/duplicate-ids.org | 1783796759000000000 | 176  |              | 1784233408 |
| 4  | /home/hubisan/projects/coding/org-files-db/.project/manual-tests/query-model/org/notes.org         | 1783796759000000000 | 903  |              | 1784233408 |
| 5  | /home/hubisan/projects/coding/org-files-db/.project/manual-tests/query-model/org/people.org        | 1783796759000000000 | 345  |              | 1784233408 |
| 6  | /home/hubisan/projects/coding/org-files-db/.project/manual-tests/query-model/org/prio.org          | 1783802108961568962 | 80   |              | 1784233408 |
| 7  | /home/hubisan/projects/coding/org-files-db/.project/manual-tests/query-model/org/projects.org      | 1783796759000000000 | 944  |              | 1784233408 |

## heading_bodies


## headings

| id | file_id | parent_id | level | line_number | byte_start | byte_end |           title            |            title_raw            | todo_keyword | todo_type | priority | scheduled_raw | scheduled_ts | scheduled_has_time |      deadline_raw      | deadline_ts | deadline_has_time |       closed_raw       | closed_ts  | closed_has_time | archivedp | footnote_section_p |             all_tags_json              |
|----|---------|-----------|-------|-------------|------------|----------|----------------------------|---------------------------------|--------------|-----------|----------|---------------|--------------|--------------------|------------------------|-------------|-------------------|------------------------|------------|-----------------|-----------|--------------------|----------------------------------------|
| 1  | 1       |           | 0     | 1           | -1         | 1253     | Org Files Test Index       | Org Files Test Index            |              |           |          |               |              |                    |                        |             |                   |                        |            |                 | 0         | 0                  | ["dashboard","index"]                  |
| 2  | 1       | 1         | 1     | 13          | 340        | 386      | Statistic Cookies          | [#B] Statistic Cookies [0/1]    |              |           | B        |               |              |                    |                        |             |                   |                        |            |                 | 0         | 0                  | ["dashboard","index"]                  |
| 3  | 1       | 2         | 2     | 15          | 372        | 386      | test                       | TODO test                       | TODO         | open      |          |               |              |                    |                        |             |                   |                        |            |                 | 0         | 0                  | ["dashboard","index"]                  |
| 4  | 1       | 1         | 1     | 17          | 386        | 413      | Statistic Cookies          | Statistic Cookies [0/0]         |              |           |          |               |              |                    |                        |             |                   |                        |            |                 | 0         | 0                  | ["dashboard","index"]                  |
| 5  | 1       | 1         | 1     | 19          | 413        | 443      | Review query CLI           | NEXT [#A] Review query CLI      | NEXT         | open      | A        |               |              |                    |                        |             |                   |                        |            |                 | 0         | 0                  | ["dashboard","index"]                  |
| 6  | 1       | 1         | 1     | 21          | 443        | 893      | Review query CLI           | NEXT [#A] Review query CLI      | NEXT         | open      | A        |               |              |                    |                        |             |                   |                        |            |                 | 0         | 0                  | ["dashboard","index","project"]        |
| 7  | 1       | 6         | 2     | 31          | 749        | 827      | Add documentation examples | TODO Add documentation examples | TODO         | open      |          |               |              |                    |                        |             |                   |                        |            |                 | 0         | 0                  | ["dashboard","index","project","docs"] |
| 8  | 1       | 6         | 2     | 36          | 827        | 893      | Verify JSON output         | DONE Verify JSON output         | DONE         | closed    |          |               |              |                    |                        |             |                   | [2026-07-10 Fri 17:30] | 1783704600 | 1               | 0         | 0                  | ["dashboard","index","project","test"] |
| 9  | 1       | 1         | 1     | 39          | 893        | 1005     | Inbox item                 | TODO Inbox item                 | TODO         | open      |          |               |              |                    | <2026-12-31 Thu 23:59> | 1798761540  | 1                 |                        |            |                 | 0         | 0                  | ["dashboard","index","inbox"]          |
| 10 | 1       | 1         | 1     | 42          | 1005       | 1253     | Reference links            | Reference links                 |              |           |          |               |              |                    |                        |             |                   |                        |            |                 | 0         | 0                  | ["dashboard","index","links"]          |

## keywords

| id | heading_id | keyword  |        value         | line_number |
|----|------------|----------|----------------------|-------------|
| 1  | 1          | TITLE    | Org Files Test Index | 1           |
| 2  | 1          | FILETAGS | :dashboard:index:    | 2           |
| 3  | 1          | CATEGORY | dashboard            | 3           |
| 4  | 1          | PROPERTY | OWNER Hubi           | 4           |
| 5  | 11         | TITLE    | Archive 2025         | 1           |
| 6  | 11         | FILETAGS | :archive:            | 2           |
| 7  | 11         | CATEGORY | archive              | 3           |
| 8  | 14         | TITLE    | Duplicate IDs        | 1           |
| 9  | 14         | FILETAGS | :test:duplicates:    | 2           |
| 10 | 17         | TITLE    | Technical Notes      | 1           |

## links

| id | file_id | heading_id | byte_start | byte_end | line | source_context | format  |                            raw                             |            raw_target            |        raw_description         | link_type |         path         |  search_option  |                                          path_absolute                                           | target_file_id | target_heading_id | target_custom_id |      target_id       | resolution_status |    resolution_diagnostic    |
|----|---------|------------|------------|----------|------|----------------|---------|------------------------------------------------------------|----------------------------------|--------------------------------|-----------|----------------------|-----------------|--------------------------------------------------------------------------------------------------|----------------|-------------------|------------------|----------------------|-------------------|-----------------------------|
| 1  | 1       | 1          | 124        | 155      | 7    | normal         | bracket | [[file:projects.org][Projects]]                            | file:projects.org                | Projects                       | file      | projects.org         |                 | /home/hubisan/projects/coding/org-files-db/.project/manual-tests/query-model/org/projects.org    | 7              | 31                |                  |                      | resolved          |                             |
| 2  | 1       | 1          | 158        | 211      | 8    | normal         | bracket | [[file:notes.org::*Query Model][Query model heading]]      | file:notes.org::*Query Model     | Query model heading            | file      | notes.org            | *Query Model    | /home/hubisan/projects/coding/org-files-db/.project/manual-tests/query-model/org/notes.org       | 4              | 18                |                  |                      | resolved          |                             |
| 3  | 1       | 1          | 214        | 272      | 9    | normal         | bracket | [[file:notes.org::*Does Not Exist][Broken heading target]] | file:notes.org::*Does Not Exist  | Broken heading target          | file      | notes.org            | *Does Not Exist | /home/hubisan/projects/coding/org-files-db/.project/manual-tests/query-model/org/notes.org       | 4              |                   |                  |                      | broken            | heading not found           |
| 4  | 1       | 1          | 275        | 314      | 10   | normal         | bracket | [[file:not-indexed.org][Outside index]]                    | file:not-indexed.org             | Outside index                  | file      | not-indexed.org      |                 | /home/hubisan/projects/coding/org-files-db/.project/manual-tests/query-model/org/not-indexed.org |                |                   |                  |                      | broken            | missing in indexed universe |
| 5  | 1       | 1          | 317        | 336      | 11   | normal         | plain   | https://example.com                                        | https://example.com              |                                | https     | //example.com        |                 |                                                                                                  |                |                   |                  |                      | unsupported       | unsupported link type       |
| 6  | 1       | 6          | 652        | 706      | 29   | normal         | bracket | [[file:projects.org::*Query Engine][the query engine]]     | file:projects.org::*Query Engine | the query engine               | file      | projects.org         | *Query Engine   | /home/hubisan/projects/coding/org-files-db/.project/manual-tests/query-model/org/projects.org    | 7              | 32                |                  |                      | resolved          |                             |
| 7  | 1       | 6          | 711        | 746      | 29   | normal         | bracket | [[id:project-query-engine][its ID]]                        | id:project-query-engine          | its ID                         | id        | project-query-engine |                 |                                                                                                  | 7              | 32                |                  | project-query-engine | resolved          |                             |
| 8  | 1       | 10         | 1085       | 1123     | 43   | normal         | bracket | [[#review-query-cli][Local custom ID]]                     | #review-query-cli                | Local custom ID                | custom-id | review-query-cli     |                 |                                                                                                  | 1              | 6                 | review-query-cli |                      | resolved          |                             |
| 9  | 1       | 10         | 1126       | 1164     | 44   | normal         | bracket | [[id:duplicate-test-id][Ambiguous ID]]                     | id:duplicate-test-id             | Ambiguous ID                   | id        | duplicate-test-id    |                 |                                                                                                  |                |                   |                  | duplicate-test-id    | ambiguous         | duplicate id                |
| 10 | 1       | 10         | 1167       | 1221     | 45   | normal         | bracket | [[file:notes.org::42][Unsupported file search option]]     | file:notes.org::42               | Unsupported file search option | file      | notes.org            | 42              | /home/hubisan/projects/coding/org-files-db/.project/manual-tests/query-model/org/notes.org       | 4              |                   |                  |                      | resolved          |                             |

## outline_path

| heading_id | file_id | parent_id | depth | materialized_path |                             breadcrumbs_json                             |
|------------|---------|-----------|-------|-------------------|--------------------------------------------------------------------------|
| 1          | 1       |           | 0     | 0000              | ["Org Files Test Index"]                                                 |
| 2          | 1       | 1         | 1     | 0000.0001         | ["Org Files Test Index","Statistic Cookies"]                             |
| 3          | 1       | 2         | 2     | 0000.0001.0001    | ["Org Files Test Index","Statistic Cookies","test"]                      |
| 4          | 1       | 1         | 1     | 0000.0002         | ["Org Files Test Index","Statistic Cookies"]                             |
| 5          | 1       | 1         | 1     | 0000.0003         | ["Org Files Test Index","Review query CLI"]                              |
| 6          | 1       | 1         | 1     | 0000.0004         | ["Org Files Test Index","Review query CLI"]                              |
| 7          | 1       | 6         | 2     | 0000.0004.0001    | ["Org Files Test Index","Review query CLI","Add documentation examples"] |
| 8          | 1       | 6         | 2     | 0000.0004.0002    | ["Org Files Test Index","Review query CLI","Verify JSON output"]         |
| 9          | 1       | 1         | 1     | 0000.0005         | ["Org Files Test Index","Inbox item"]                                    |
| 10         | 1       | 1         | 1     | 0000.0006         | ["Org Files Test Index","Reference links"]                               |

## properties

| id | heading_id |    key    |       value       |      source      | append | line_number |
|----|------------|-----------|-------------------|------------------|--------|-------------|
| 1  | 1          | CATEGORY  | dashboard         | category_keyword | 0      | 3           |
| 2  | 1          | OWNER     | Hubi              | property_keyword | 0      | 4           |
| 3  | 6          | CUSTOM_ID | review-query-cli  | property_drawer  | 0      | 23          |
| 4  | 6          | OWNER     |    Hubi           | property_drawer  | 0      | 24          |
| 5  | 7          | OWNER     |    Alex           | property_drawer  | 0      | 33          |
| 6  | 11         | CATEGORY  | archive           | category_keyword | 0      | 3           |
| 7  | 12         | OWNER     | Hubi              | property_drawer  | 0      | 8           |
| 8  | 15         | ID        | duplicate-test-id | property_drawer  | 0      | 6           |
| 9  | 16         | ID        | duplicate-test-id | property_drawer  | 0      | 11          |
| 10 | 17         | CATEGORY  | notes             | category_keyword | 0      | 3           |

## tags

| heading_id |    tag    |
|------------|-----------|
| 1          | dashboard |
| 1          | index     |
| 6          | project   |
| 6          | dashboard |
| 7          | docs      |
| 8          | test      |
| 9          | inbox     |
| 10         | links     |
| 11         | archive   |
| 12         | legacy    |

## timestamp_repeaters


## timestamps

| id | heading_id |   role   | has_time |  start_ts  | end_ts |   type   | range_type |       raw_value        | byte_start | byte_end | line_number |
|----|------------|----------|----------|------------|--------|----------|------------|------------------------|------------|----------|-------------|
| 1  | 6          | body     | 1        | 1783760400 |        | active   | none       | <2026-07-11 Sat 09:00> | 596        | 618      | 26          |
| 2  | 6          | body     | 0        | 1784332800 |        | active   | none       | <2026-07-18 Sat>       | 629        | 645      | 27          |
| 3  | 8          | closed   | 1        | 1783704600 |        | inactive | none       | [2026-07-10 Fri 17:30] | 869        | 891      | 37          |
| 4  | 9          | deadline | 1        | 1798761540 |        | active   | none       | <2026-12-31 Thu 23:59> | 981        | 1003     | 40          |
| 5  | 12         | closed   | 1        | 1766232000 |        | inactive | none       | [2025-12-20 Sat 12:00] | 106        | 128      | 6           |
| 6  | 13         | closed   | 0        | 1761955200 |        | inactive | none       | [2025-11-01 Sat]       | 212        | 228      | 12          |
| 7  | 19         | body     | 1        | 1783764900 |        | active   | none       | <2026-07-11 Sat 10:15> | 328        | 350      | 16          |
| 8  | 19         | body     | 1        | 1783716300 |        | inactive | none       | [2026-07-10 Fri 20:45] | 374        | 396      | 17          |
| 9  | 19         | body     | 0        | 1783900800 |        | active   | none       | <2026-07-13 Mon>       | 420        | 436      | 18          |
| 10 | 32         | body     | 1        | 1783845000 |        | active   | none       | <2026-07-12 Sun 08:30> | 306        | 328      | 14          |

## todo_keywords

| file_id | keyword | state_type | shortcut | sequence_no |  source_kind   | source_keyword | source_line_number |
|---------|---------|------------|----------|-------------|----------------|----------------|--------------------|
| 1       | TODO    | open       |          | 0           | config_default |                |                    |
| 1       | NEXT    | open       |          | 1           | config_default |                |                    |
| 1       | DONE    | closed     |          | 2           | config_default |                |                    |
| 1       | CANCEL  | closed     |          | 3           | config_default |                |                    |
| 2       | TODO    | open       |          | 0           | config_default |                |                    |
| 2       | NEXT    | open       |          | 1           | config_default |                |                    |
| 2       | DONE    | closed     |          | 2           | config_default |                |                    |
| 2       | CANCEL  | closed     |          | 3           | config_default |                |                    |
| 3       | TODO    | open       |          | 0           | config_default |                |                    |
| 3       | NEXT    | open       |          | 1           | config_default |                |                    |

<!--
Local Variables:
eval: (visual-fill-column-mode -1)
truncate-lines: t
End:
-->
