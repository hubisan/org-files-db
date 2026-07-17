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
| 1  | /home/hubisan/projects/coding/org-files-db/.project/manual-tests/query-model/org/properties.org    | 1783029353235070183 | 2783 |              | 1784245955 |
| 2  | /home/hubisan/projects/coding/org-files-db/.project/manual-tests/query-model/org/todo-keywords.org | 1784242830730251043 | 0    |              | 1784245955 |

## heading_bodies


## headings

| id | file_id | parent_id | level | line_number | byte_start | byte_end |                    title                    |                  title_raw                  | todo_keyword | todo_type | priority | scheduled_raw | scheduled_ts | scheduled_has_time | deadline_raw | deadline_ts | deadline_has_time | closed_raw | closed_ts | closed_has_time | archivedp | footnote_section_p |   all_tags_json    |
|----|---------|-----------|-------|-------------|------------|----------|---------------------------------------------|---------------------------------------------|--------------|-----------|----------|---------------|--------------|--------------------|--------------|-------------|-------------------|------------|-----------|-----------------|-----------|--------------------|--------------------|
| 1  | 1       |           | 0     | 1           | -1         | 2783     | Org Property and Keyword Test               | Org Property and Keyword Test               |              |           |          |               |              |                    |              |             |                   |            |           |                 | 0         | 0                  | ["project","work"] |
| 2  | 1       | 1         | 1     | 16          | 465        | 638      | Task with multiple drawer properties        | Task with multiple drawer properties        |              |           |          |               |              |                    |              |             |                   |            |           |                 | 0         | 0                  | ["project","work"] |
| 3  | 1       | 1         | 1     | 26          | 638        | 858      | Task with duplicate drawer properties       | Task with duplicate drawer properties       |              |           |          |               |              |                    |              |             |                   |            |           |                 | 0         | 0                  | ["project","work"] |
| 4  | 1       | 1         | 1     | 35          | 858        | 1094     | Task with append operator in drawer         | Task with append operator in drawer         |              |           |          |               |              |                    |              |             |                   |            |           |                 | 0         | 0                  | ["project","work"] |
| 5  | 1       | 1         | 1     | 45          | 1094       | 1304     | Task with mixed-case keys                   | Task with mixed-case keys                   |              |           |          |               |              |                    |              |             |                   |            |           |                 | 0         | 0                  | ["project","work"] |
| 6  | 1       | 1         | 1     | 58          | 1304       | 1437     | Task with empty property accepted by Orgize | Task with empty property accepted by Orgize |              |           |          |               |              |                    |              |             |                   |            |           |                 | 0         | 0                  | ["project","work"] |
| 7  | 1       | 1         | 1     | 67          | 1437       | 1718     | Task with Orgize empty-property limitation  | Task with Orgize empty-property limitation  |              |           |          |               |              |                    |              |             |                   |            |           |                 | 0         | 0                  | ["project","work"] |
| 8  | 1       | 1         | 1     | 76          | 1718       | 1984     | Task after file-level property keywords     | Task after file-level property keywords     |              |           |          |               |              |                    |              |             |                   |            |           |                 | 0         | 0                  | ["project","work"] |
| 9  | 1       | 1         | 1     | 83          | 1984       | 2264     | Task after later file-level keywords        | Task after later file-level keywords        |              |           |          |               |              |                    |              |             |                   |            |           |                 | 0         | 0                  | ["project","work"] |
| 10 | 1       | 1         | 1     | 89          | 2264       | 2783     | Boundary: property-like but not properties  | Boundary: property-like but not properties  |              |           |          |               |              |                    |              |             |                   |            |           |                 | 0         | 0                  | ["project","work"] |

## keywords

| id | heading_id | keyword  |                  value                  | line_number |
|----|------------|----------|-----------------------------------------|-------------|
| 1  | 1          | TITLE    | Org Property and Keyword Test           | 6           |
| 2  | 1          | STARTUP  | showall                                 | 7           |
| 3  | 1          | CATEGORY | category_keyword_value                  | 8           |
| 4  | 1          | PROPERTY | Effort_ALL 0:10 0:30 1:00               | 9           |
| 5  | 1          | PROPERTY | keyword_property valid                  | 10          |
| 6  | 1          | PROPERTY | keyword_overwritten_by_second invalid   | 11          |
| 7  | 1          | PROPERTY | keyword_overwritten_by_second valid     | 12          |
| 8  | 1          | PROPERTY | keyword_append foo=1                    | 13          |
| 9  | 1          | PROPERTY | keyword_append+ bar=2                   | 14          |
| 10 | 1          | PROPERTY | later_keyword_property works_everywhere | 80          |

## links


## outline_path

| heading_id | file_id | parent_id | depth | materialized_path |                                breadcrumbs_json                                 |
|------------|---------|-----------|-------|-------------------|---------------------------------------------------------------------------------|
| 1          | 1       |           | 0     | 0000              | ["Org Property and Keyword Test"]                                               |
| 2          | 1       | 1         | 1     | 0000.0001         | ["Org Property and Keyword Test","Task with multiple drawer properties"]        |
| 3          | 1       | 1         | 1     | 0000.0002         | ["Org Property and Keyword Test","Task with duplicate drawer properties"]       |
| 4          | 1       | 1         | 1     | 0000.0003         | ["Org Property and Keyword Test","Task with append operator in drawer"]         |
| 5          | 1       | 1         | 1     | 0000.0004         | ["Org Property and Keyword Test","Task with mixed-case keys"]                   |
| 6          | 1       | 1         | 1     | 0000.0005         | ["Org Property and Keyword Test","Task with empty property accepted by Orgize"] |
| 7          | 1       | 1         | 1     | 0000.0006         | ["Org Property and Keyword Test","Task with Orgize empty-property limitation"]  |
| 8          | 1       | 1         | 1     | 0000.0007         | ["Org Property and Keyword Test","Task after file-level property keywords"]     |
| 9          | 1       | 1         | 1     | 0000.0008         | ["Org Property and Keyword Test","Task after later file-level keywords"]        |
| 10         | 1       | 1         | 1     | 0000.0009         | ["Org Property and Keyword Test","Boundary: property-like but not properties"]  |

## properties

| id | heading_id |              key              |                value                 |      source      | append | line_number |
|----|------------|-------------------------------|--------------------------------------|------------------|--------|-------------|
| 1  | 1          | CATEGORY                      | Level 0 Category Property            | property_drawer  | 0      | 2           |
| 2  | 1          | WHATEVER                      | level 0 drawer property              | property_drawer  | 0      | 3           |
| 3  | 1          | ID                            | 7dad9b62-a3cc-43ec-a60f-e650bdaeae6d | property_drawer  | 0      | 4           |
| 4  | 1          | CATEGORY                      | category_keyword_value               | category_keyword | 0      | 8           |
| 5  | 1          | EFFORT_ALL                    | 0:10 0:30 1:00                       | property_keyword | 0      | 9           |
| 6  | 1          | KEYWORD_PROPERTY              | valid                                | property_keyword | 0      | 10          |
| 7  | 1          | KEYWORD_OVERWRITTEN_BY_SECOND | invalid                              | property_keyword | 0      | 11          |
| 8  | 1          | KEYWORD_OVERWRITTEN_BY_SECOND | valid                                | property_keyword | 0      | 12          |
| 9  | 1          | KEYWORD_APPEND                | foo=1                                | property_keyword | 0      | 13          |
| 10 | 1          | KEYWORD_APPEND                | bar=2                                | property_keyword | 1      | 14          |

## tags

| heading_id |   tag   |
|------------|---------|
| 1          | project |
| 1          | work    |

## timestamp_repeaters


## timestamps


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

<!--
Local Variables:
eval: (visual-fill-column-mode -1)
truncate-lines: t
End:
-->
