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
| 1  | /home/hubisan/projects/coding/org-files-db/.project/manual-tests/query-model/org/properties.org    | 1784287134693141785 | 2944 |              | 1784287408 |
| 2  | /home/hubisan/projects/coding/org-files-db/.project/manual-tests/query-model/org/todo-keywords.org | 1784242830730251043 | 0    |              | 1784287408 |

## heading_bodies


## headings

| id | file_id | parent_id | level | line_number | byte_start | byte_end |                    title                    |                  title_raw                  | todo_keyword | todo_type | priority | scheduled_raw | scheduled_ts | scheduled_has_time | deadline_raw | deadline_ts | deadline_has_time | closed_raw | closed_ts | closed_has_time | archivedp | footnote_section_p |   all_tags_json    |
|----|---------|-----------|-------|-------------|------------|----------|---------------------------------------------|---------------------------------------------|--------------|-----------|----------|---------------|--------------|--------------------|--------------|-------------|-------------------|------------|-----------|-----------------|-----------|--------------------|--------------------|
| 1  | 1       |           | 0     | 1           | -1         | 2944     | Org Property and Keyword Test               | Org Property and Keyword Test               |              |           |          |               |              |                    |              |             |                   |            |           |                 | 0         | 0                  | ["project","work"] |
| 2  | 1       | 1         | 1     | 17          | 492        | 625      | Multiple drawer properties                  | Multiple drawer properties                  |              |           |          |               |              |                    |              |             |                   |            |           |                 | 0         | 0                  | ["project","work"] |
| 3  | 1       | 1         | 1     | 26          | 625        | 760      | Overwrite Property                          | Overwrite Property                          |              |           |          |               |              |                    |              |             |                   |            |           |                 | 0         | 0                  | ["project","work"] |
| 4  | 1       | 3         | 2     | 32          | 733        | 760      | Overwrite is inherited                      | Overwrite is inherited                      |              |           |          |               |              |                    |              |             |                   |            |           |                 | 0         | 0                  | ["project","work"] |
| 5  | 1       | 1         | 1     | 34          | 760        | 1033     | Duplicate drawer properties                 | Duplicate drawer properties                 |              |           |          |               |              |                    |              |             |                   |            |           |                 | 0         | 0                  | ["project","work"] |
| 6  | 1       | 1         | 1     | 44          | 1033       | 1255     | Task with append operator in drawer         | Task with append operator in drawer         |              |           |          |               |              |                    |              |             |                   |            |           |                 | 0         | 0                  | ["project","work"] |
| 7  | 1       | 1         | 1     | 54          | 1255       | 1465     | Task with mixed-case keys                   | Task with mixed-case keys                   |              |           |          |               |              |                    |              |             |                   |            |           |                 | 0         | 0                  | ["project","work"] |
| 8  | 1       | 1         | 1     | 67          | 1465       | 1598     | Task with empty property accepted by Orgize | Task with empty property accepted by Orgize |              |           |          |               |              |                    |              |             |                   |            |           |                 | 0         | 0                  | ["project","work"] |
| 9  | 1       | 1         | 1     | 76          | 1598       | 1879     | Task with Orgize empty-property limitation  | Task with Orgize empty-property limitation  |              |           |          |               |              |                    |              |             |                   |            |           |                 | 0         | 0                  | ["project","work"] |
| 10 | 1       | 1         | 1     | 85          | 1879       | 2145     | Task after file-level property keywords     | Task after file-level property keywords     |              |           |          |               |              |                    |              |             |                   |            |           |                 | 0         | 0                  | ["project","work"] |

## keywords

| id | heading_id | keyword  |                  value                  | line_number |
|----|------------|----------|-----------------------------------------|-------------|
| 1  | 1          | TITLE    | Org Property and Keyword Test           | 7           |
| 2  | 1          | STARTUP  | showall                                 | 8           |
| 3  | 1          | CATEGORY | category_keyword_value                  | 9           |
| 4  | 1          | PROPERTY | Effort_ALL 0:10 0:30 1:00               | 10          |
| 5  | 1          | PROPERTY | keyword_property valid                  | 11          |
| 6  | 1          | PROPERTY | keyword_overwritten_by_second invalid   | 12          |
| 7  | 1          | PROPERTY | keyword_overwritten_by_second valid     | 13          |
| 8  | 1          | PROPERTY | keyword_append foo=1                    | 14          |
| 9  | 1          | PROPERTY | keyword_append+ bar=2                   | 15          |
| 10 | 1          | PROPERTY | later_keyword_property works_everywhere | 89          |

## links


## outline_path

| heading_id | file_id | parent_id | depth | materialized_path |                                breadcrumbs_json                                 |
|------------|---------|-----------|-------|-------------------|---------------------------------------------------------------------------------|
| 1          | 1       |           | 0     | 0000              | ["Org Property and Keyword Test"]                                               |
| 2          | 1       | 1         | 1     | 0000.0001         | ["Org Property and Keyword Test","Multiple drawer properties"]                  |
| 3          | 1       | 1         | 1     | 0000.0002         | ["Org Property and Keyword Test","Overwrite Property"]                          |
| 4          | 1       | 3         | 2     | 0000.0002.0001    | ["Org Property and Keyword Test","Overwrite Property","Overwrite is inherited"] |
| 5          | 1       | 1         | 1     | 0000.0003         | ["Org Property and Keyword Test","Duplicate drawer properties"]                 |
| 6          | 1       | 1         | 1     | 0000.0004         | ["Org Property and Keyword Test","Task with append operator in drawer"]         |
| 7          | 1       | 1         | 1     | 0000.0005         | ["Org Property and Keyword Test","Task with mixed-case keys"]                   |
| 8          | 1       | 1         | 1     | 0000.0006         | ["Org Property and Keyword Test","Task with empty property accepted by Orgize"] |
| 9          | 1       | 1         | 1     | 0000.0007         | ["Org Property and Keyword Test","Task with Orgize empty-property limitation"]  |
| 10         | 1       | 1         | 1     | 0000.0008         | ["Org Property and Keyword Test","Task after file-level property keywords"]     |

## properties

| id | heading_id |              key              |                value                 |      source      | append | line_number |
|----|------------|-------------------------------|--------------------------------------|------------------|--------|-------------|
| 1  | 1          | CATEGORY                      | Level 0 Category Property            | property_drawer  | 0      | 2           |
| 2  | 1          | WHATEVER                      | level 0 drawer property              | property_drawer  | 0      | 3           |
| 3  | 1          | OVERWRITE                     | this one works                       | property_drawer  | 0      | 4           |
| 4  | 1          | ID                            | 7dad9b62-a3cc-43ec-a60f-e650bdaeae6d | property_drawer  | 0      | 5           |
| 5  | 1          | CATEGORY                      | category_keyword_value               | category_keyword | 0      | 9           |
| 6  | 1          | EFFORT_ALL                    | 0:10 0:30 1:00                       | property_keyword | 0      | 10          |
| 7  | 1          | KEYWORD_PROPERTY              | valid                                | property_keyword | 0      | 11          |
| 8  | 1          | KEYWORD_OVERWRITTEN_BY_SECOND | invalid                              | property_keyword | 0      | 12          |
| 9  | 1          | KEYWORD_OVERWRITTEN_BY_SECOND | valid                                | property_keyword | 0      | 13          |
| 10 | 1          | KEYWORD_APPEND                | foo=1                                | property_keyword | 0      | 14          |

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
