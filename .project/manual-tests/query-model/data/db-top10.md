# SQLite DB preview

## db_metadata

|         key         | value |
|---------------------|-------|
| body_text_available | 1     |
| fts_available       | 1     |
| fts_body_indexed    | 1     |
| fts_schema_version  | 1     |

## files

| id |                                                    path                                                     |      mtime_ns       | size | content_hash | indexed_at |
|----|-------------------------------------------------------------------------------------------------------------|---------------------|------|--------------|------------|
| 1  | /home/hubisan/projects/coding/org-files-db/.project/manual-tests/query-model/org/headings/ancestors.org     | 1782986400000000000 | 363  |              | 1784477089 |
| 2  | /home/hubisan/projects/coding/org-files-db/.project/manual-tests/query-model/org/headings/children.org      | 1782986400000000000 | 399  |              | 1784477089 |
| 3  | /home/hubisan/projects/coding/org-files-db/.project/manual-tests/query-model/org/headings/closed.org        | 1782986400000000000 | 396  |              | 1784477089 |
| 4  | /home/hubisan/projects/coding/org-files-db/.project/manual-tests/query-model/org/headings/deadline.org      | 1782986400000000000 | 416  |              | 1784477089 |
| 5  | /home/hubisan/projects/coding/org-files-db/.project/manual-tests/query-model/org/headings/descendants.org   | 1782986400000000000 | 474  |              | 1784477089 |
| 6  | /home/hubisan/projects/coding/org-files-db/.project/manual-tests/query-model/org/headings/done.org          | 1782986400000000000 | 243  |              | 1784477089 |
| 7  | /home/hubisan/projects/coding/org-files-db/.project/manual-tests/query-model/org/headings/file-modified.org | 1782900000000000000 | 151  |              | 1784477089 |
| 8  | /home/hubisan/projects/coding/org-files-db/.project/manual-tests/query-model/org/headings/file-name.org     | 1782986400000000000 | 129  |              | 1784477089 |
| 9  | /home/hubisan/projects/coding/org-files-db/.project/manual-tests/query-model/org/headings/file-title.org    | 1782986400000000000 | 155  |              | 1784477089 |
| 10 | /home/hubisan/projects/coding/org-files-db/.project/manual-tests/query-model/org/headings/has-link.org      | 1782986400000000000 | 465  |              | 1784477089 |

## heading_bodies

| heading_id |                              body_text                               | body_byte_start | body_byte_end |
|------------|----------------------------------------------------------------------|-----------------|---------------|
| 53         | [[file:hln5-target-file.org][hln5 unique file description]]          | 80              | 139           |
| 54         | [[https://example.invalid/hln5-unique][hln5 unique web description]] | 171             | 239           |
| 55         | [[id:hln5-target-id][hln5 unique id description]]                    | 268             | 317           |
| 56         | Plain unique text hln5-control.                                      | 354             | 385           |
| 59         | single match                                                         | 67              | 103           |
|            | some text and some more                                              |                 |               |
| 61         | multiple matches                                                     | 158             | 174           |
| 62         | multiple matches                                                     | 209             | 225           |
| 75         | [[#lfr7-target-a][lfr7 backlink A description]]                      | 141             | 188           |
| 76         | [[id:lfr7-target-id-b][lfr7 backlink B description]]                 | 232             | 284           |
| 81         | [[#lto6-target-custom][lto6 custom target description]]              | 90              | 145           |

## heading_fts

| title | body |
|-------|------|
|       |      |
|       |      |
|       |      |
|       |      |
|       |      |
|       |      |
|       |      |
|       |      |
|       |      |
|       |      |

## heading_fts_config

|    k    | v |
|---------|---|
| version | 4 |

## heading_fts_data

|      id      | block |
|--------------|-------|
| 1            | ÅRâ     |
|              | è      |
| 10           |       |
| 137438953473 |       |
| 137438953474 |       |
| 137438953475 |       |

## heading_fts_docsize

| id | sz |
|----|----|
| 2  |    |
| 3  |    |
| 4  |    |
| 5  |    |
| 6  |    |
| 7  |    |
| 9  |    |
| 10 |    |
| 11 |    |
| 12 |    |

## heading_fts_idx

| segid | term  | pgno |
|-------|-------|------|
| 1     |       | 2    |
| 1     | 0ext  | 4    |
| 1     | 0part | 6    |

## headings

| id | file_id | parent_id | level | line_number | byte_start | byte_end |                  title                   |                   title_raw                   | todo_keyword | todo_type | priority | scheduled_raw | scheduled_ts | scheduled_has_time | deadline_raw | deadline_ts | deadline_has_time | closed_raw | closed_ts | closed_has_time | archivedp | footnote_section_p | all_tags_json |
|----|---------|-----------|-------|-------------|------------|----------|------------------------------------------|-----------------------------------------------|--------------|-----------|----------|---------------|--------------|--------------------|--------------|-------------|-------------------|------------|-----------|-----------------|-----------|--------------------|---------------|
| 1  | 1       |           | 0     | 1           | -1         | 363      | ancestors-fixture-anc3                   | ancestors-fixture-anc3                        |              |           |          |               |              |                    |              |             |                   |            |           |                 | 0         | 0                  | []            |
| 2  | 1       | 1         | 1     | 4           | 52         | 198      | ancestors: Tagged Ancestor anc3-root     | ancestors: Tagged Ancestor anc3-root          |              |           |          |               |              |                    |              |             |                   |            |           |                 | 0         | 0                  | []            |
| 3  | 1       | 2         | 2     | 5           | 130        | 198      | ancestors: Middle anc3-mid               | ancestors: Middle anc3-mid                    |              |           |          |               |              |                    |              |             |                   |            |           |                 | 0         | 0                  | []            |
| 4  | 1       | 3         | 3     | 6           | 160        | 198      | ancestors: Deep Descendant anc3-a        | ancestors: Deep Descendant anc3-a             |              |           |          |               |              |                    |              |             |                   |            |           |                 | 0         | 0                  | []            |
| 5  | 1       | 1         | 1     | 7           | 198        | 321      | ancestors: Property Ancestor anc3-prop   | ancestors: Property Ancestor anc3-prop        |              |           |          |               |              |                    |              |             |                   |            |           |                 | 0         | 0                  | []            |
| 6  | 1       | 5         | 2     | 11          | 280        | 321      | ancestors: Property Descendant anc3-b    | ancestors: Property Descendant anc3-b         |              |           |          |               |              |                    |              |             |                   |            |           |                 | 0         | 0                  | []            |
| 7  | 1       | 1         | 1     | 12          | 321        | 363      | ancestors: Top Level No Ancestor anc3-c  | ancestors: Top Level No Ancestor anc3-c       |              |           |          |               |              |                    |              |             |                   |            |           |                 | 0         | 0                  | []            |
| 8  | 2       |           | 0     | 1           | -1         | 399      | children-fixture-chd2                    | children-fixture-chd2                         |              |           |          |               |              |                    |              |             |                   |            |           |                 | 0         | 0                  | []            |
| 9  | 2       | 8         | 1     | 5           | 76         | 161      | children: Has Direct Child chd2-a        | children: Has Direct Child chd2-a             |              |           |          |               |              |                    |              |             |                   |            |           |                 | 0         | 0                  | []            |
| 10 | 2       | 9         | 2     | 6           | 112        | 161      | children: Direct TODO Child chd2-child-a | TODO children: Direct TODO Child chd2-child-a | TODO         | open      |          |               |              |                    |              |             |                   |            |           |                 | 0         | 0                  | []            |

## keywords

| id | heading_id | keyword |         value          | line_number |
|----|------------|---------|------------------------|-------------|
| 1  | 1          | TITLE   | ancestors-fixture-anc3 | 1           |
| 2  | 1          | STARTUP | showall                | 2           |
| 3  | 8          | TITLE   | children-fixture-chd2  | 1           |
| 4  | 8          | STARTUP | showall                | 2           |
| 5  | 8          | TODO    | TODO NEXT | DONE       | 3           |
| 6  | 17         | TITLE   | closed-fixture-cls8    | 1           |
| 7  | 17         | STARTUP | showall                | 2           |
| 8  | 17         | TODO    | TODO | DONE            | 3           |
| 9  | 23         | TITLE   | deadline-fixture-dln6  | 1           |
| 10 | 23         | STARTUP | showall                | 2           |

## links

| id | file_id | heading_id | byte_start | byte_end | line | source_context | format  |                                 raw                                  |                        raw_target                         |        raw_description         | link_type |                         path                         | search_option |                                                        path_absolute                                                        | target_file_id | target_heading_id |  target_custom_id  |    target_id     | resolution_status |    resolution_diagnostic    |
|----|---------|------------|------------|----------|------|----------------|---------|----------------------------------------------------------------------|-----------------------------------------------------------|--------------------------------|-----------|------------------------------------------------------|---------------|-----------------------------------------------------------------------------------------------------------------------------|----------------|-------------------|--------------------|------------------|-------------------|-----------------------------|
| 1  | 10      | 53         | 80         | 139      | 5    | normal         | bracket | [[file:hln5-target-file.org][hln5 unique file description]]          | file:hln5-target-file.org                                 | hln5 unique file description   | file      | hln5-target-file.org                                 |               | /home/hubisan/projects/coding/org-files-db/.project/manual-tests/query-model/org/headings/hln5-target-file.org              |                |                   |                    |                  | broken            | missing in indexed universe |
| 2  | 10      | 54         | 171        | 239      | 8    | normal         | bracket | [[https://example.invalid/hln5-unique][hln5 unique web description]] | https://example.invalid/hln5-unique                       | hln5 unique web description    | https     | //example.invalid/hln5-unique                        |               |                                                                                                                             |                |                   |                    |                  | unsupported       | unsupported link type       |
| 3  | 10      | 55         | 268        | 317      | 11   | normal         | bracket | [[id:hln5-target-id][hln5 unique id description]]                    | id:hln5-target-id                                         | hln5 unique id description     | id        | hln5-target-id                                       |               |                                                                                                                             | 10             | 57                |                    | hln5-target-id   | resolved          |                             |
| 4  | 14      | 75         | 141        | 188      | 5    | normal         | bracket | [[#lfr7-target-a][lfr7 backlink A description]]                      | #lfr7-target-a                                            | lfr7 backlink A description    | custom-id | lfr7-target-a                                        |               |                                                                                                                             | 14             | 77                | lfr7-target-a      |                  | resolved          |                             |
| 5  | 14      | 76         | 232        | 284      | 8    | normal         | bracket | [[id:lfr7-target-id-b][lfr7 backlink B description]]                 | id:lfr7-target-id-b                                       | lfr7 backlink B description    | id        | lfr7-target-id-b                                     |               |                                                                                                                             | 14             | 78                |                    | lfr7-target-id-b | resolved          |                             |
| 6  | 15      | 81         | 90         | 145      | 5    | normal         | bracket | [[#lto6-target-custom][lto6 custom target description]]              | #lto6-target-custom                                       | lto6 custom target description | custom-id | lto6-target-custom                                   |               |                                                                                                                             | 15             | 84                | lto6-target-custom |                  | resolved          |                             |
| 7  | 15      | 82         | 179        | 228      | 8    | normal         | bracket | [[id:lto6-target-id][lto6 id target description]]                    | id:lto6-target-id                                         | lto6 id target description     | id        | lto6-target-id                                       |               |                                                                                                                             | 15             | 84                |                    | lto6-target-id   | resolved          |                             |
| 8  | 15      | 83         | 272        | 325      | 11   | normal         | bracket | [[#lto6-other-custom][lto6 other target description]]                | #lto6-other-custom                                        | lto6 other target description  | custom-id | lto6-other-custom                                    |               |                                                                                                                             | 15             | 85                | lto6-other-custom  |                  | resolved          |                             |
| 9  | 16      | 86         | 73         | 134      | 5    | normal         | bracket | [[file:../../notes/org-semantics/multipe-title-keywords.org]]        | file:../../notes/org-semantics/multipe-title-keywords.org |                                | file      | ../../notes/org-semantics/multipe-title-keywords.org |               | /home/hubisan/projects/coding/org-files-db/.project/manual-tests/query-model/notes/org-semantics/multipe-title-keywords.org |                |                   |                    |                  | unresolved        | outside indexed universe    |

## outline_path

| heading_id | file_id | parent_id | depth |  materialized_path  |                                                          breadcrumbs_json                                                          |
|------------|---------|-----------|-------|---------------------|------------------------------------------------------------------------------------------------------------------------------------|
| 1          | 1       |           | 0     | 0000                | ["ancestors-fixture-anc3"]                                                                                                         |
| 2          | 1       | 1         | 1     | 0000.0001           | ["ancestors-fixture-anc3","ancestors: Tagged Ancestor anc3-root"]                                                                  |
| 3          | 1       | 2         | 2     | 0000.0001.0001      | ["ancestors-fixture-anc3","ancestors: Tagged Ancestor anc3-root","ancestors: Middle anc3-mid"]                                     |
| 4          | 1       | 3         | 3     | 0000.0001.0001.0001 | ["ancestors-fixture-anc3","ancestors: Tagged Ancestor anc3-root","ancestors: Middle anc3-mid","ancestors: Deep Descendant anc3-a"] |
| 5          | 1       | 1         | 1     | 0000.0002           | ["ancestors-fixture-anc3","ancestors: Property Ancestor anc3-prop"]                                                                |
| 6          | 1       | 5         | 2     | 0000.0002.0001      | ["ancestors-fixture-anc3","ancestors: Property Ancestor anc3-prop","ancestors: Property Descendant anc3-b"]                        |
| 7          | 1       | 1         | 1     | 0000.0003           | ["ancestors-fixture-anc3","ancestors: Top Level No Ancestor anc3-c"]                                                               |
| 8          | 2       |           | 0     | 0000                | ["children-fixture-chd2"]                                                                                                          |
| 9          | 2       | 8         | 1     | 0000.0001           | ["children-fixture-chd2","children: Has Direct Child chd2-a"]                                                                      |
| 10         | 2       | 9         | 2     | 0000.0001.0001      | ["children-fixture-chd2","children: Has Direct Child chd2-a","children: Direct TODO Child chd2-child-a"]                           |

## properties

| id | heading_id |    key    |           value           |     source      | append | line_number |
|----|------------|-----------|---------------------------|-----------------|--------|-------------|
| 1  | 5          | ANC3_AREA | work-anc3                 | property_drawer | 0      | 9           |
| 2  | 57         | ID        | hln5-target-id            | property_drawer | 0      | 18          |
| 3  | 77         | CUSTOM_ID | lfr7-target-a             | property_drawer | 0      | 12          |
| 4  | 78         | ID        | lfr7-target-id-b          | property_drawer | 0      | 17          |
| 5  | 79         | CUSTOM_ID | lfr7-target-c             | property_drawer | 0      | 22          |
| 6  | 84         | ID        | lto6-target-id            | property_drawer | 0      | 15          |
| 7  | 84         | CUSTOM_ID | lto6-target-custom        | property_drawer | 0      | 16          |
| 8  | 85         | CUSTOM_ID | lto6-other-custom         | property_drawer | 0      | 21          |
| 9  | 134        | CATEGORY  | Level 0 Category Property | property_drawer | 0      | 2           |
| 10 | 134        | WHATEVER  | level 0 drawer property   | property_drawer | 0      | 3           |

## tags

| heading_id |         tag         |
|------------|---------------------|
| 184        | tag3_file_inherited |
| 185        | tag3_local_blue     |
| 186        | tag3_any_red        |
| 187        | tag3_any_green      |
| 188        | tag3_all_gold       |
| 188        | tag3_all_silver     |
| 189        | tag3_parent_violet  |
| 191        | tag3_parent_violet  |
| 192        | tag3_regexp_482     |
| 194        | tag3_control_black  |

## timestamp_repeaters


## timestamps

| id | heading_id |   role    | has_time |  start_ts  | end_ts |   type   | range_type |       raw_value        | byte_start | byte_end | line_number |
|----|------------|-----------|----------|------------|--------|----------|------------|------------------------|------------|----------|-------------|
| 1  | 18         | closed    | 0        | 2099779200 |        | inactive | none       | [2036-07-16 Wed]       | 109        | 125      | 6           |
| 2  | 19         | closed    | 1        | 2099824800 |        | inactive | none       | [2036-07-16 Wed 12:40] | 167        | 189      | 9           |
| 3  | 20         | closed    | 0        | 2098483200 |        | inactive | none       | [2036-07-01 Tue]       | 235        | 251      | 12          |
| 4  | 21         | closed    | 1        | 2101148100 |        | inactive | none       | [2036-07-31 Thu 20:15] | 295        | 317      | 15          |
| 5  | 22         | scheduled | 0        | 2099779200 |        | active   | none       | <2036-07-16 Wed>       | 379        | 395      | 18          |
| 6  | 24         | deadline  | 0        | 2031177600 |        | active   | none       | <2034-05-14 Sun>       | 115        | 131      | 6           |
| 7  | 25         | deadline  | 1        | 2031215400 |        | active   | none       | <2034-05-14 Sun 10:30> | 177        | 199      | 9           |
| 8  | 26         | deadline  | 0        | 2030054400 |        | active   | none       | <2034-05-01 Mon>       | 249        | 265      | 12          |
| 9  | 27         | deadline  | 1        | 2032710300 |        | active   | none       | <2034-05-31 Wed 17:45> | 313        | 335      | 15          |
| 10 | 28         | scheduled | 0        | 2031177600 |        | active   | none       | <2034-05-14 Sun>       | 399        | 415      | 18          |

## todo_keywords

| file_id | keyword | state_type | shortcut | sequence_no |  source_kind   | source_keyword | source_line_number |
|---------|---------|------------|----------|-------------|----------------|----------------|--------------------|
| 1       | TODO    | open       |          | 0           | config_default |                |                    |
| 1       | NEXT    | open       |          | 1           | config_default |                |                    |
| 1       | DONE    | closed     |          | 2           | config_default |                |                    |
| 1       | CANCEL  | closed     |          | 3           | config_default |                |                    |
| 2       | TODO    | open       |          | 0           | org_keyword    | TODO           | 3                  |
| 2       | NEXT    | open       |          | 1           | org_keyword    | TODO           | 3                  |
| 2       | DONE    | closed     |          | 2           | org_keyword    | TODO           | 3                  |
| 3       | TODO    | open       |          | 0           | org_keyword    | TODO           | 3                  |
| 3       | DONE    | closed     |          | 1           | org_keyword    | TODO           | 3                  |
| 4       | TODO    | open       |          | 0           | org_keyword    | TODO           | 3                  |

<!--
Local Variables:
eval: (visual-fill-column-mode -1)
truncate-lines: t
End:
-->
