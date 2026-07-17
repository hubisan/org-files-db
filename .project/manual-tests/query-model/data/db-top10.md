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
| 1  | /home/hubisan/projects/coding/org-files-db/.project/manual-tests/query-model/org/properties.org    | 1784291848403117915 | 6566 |              | 1784292329 |
| 2  | /home/hubisan/projects/coding/org-files-db/.project/manual-tests/query-model/org/todo-keywords.org | 1784242830730251043 | 0    |              | 1784292329 |

## heading_bodies


## headings

| id | file_id | parent_id | level | line_number | byte_start | byte_end |                title                |              title_raw              | todo_keyword | todo_type | priority | scheduled_raw | scheduled_ts | scheduled_has_time | deadline_raw | deadline_ts | deadline_has_time | closed_raw | closed_ts | closed_has_time | archivedp | footnote_section_p | all_tags_json |
|----|---------|-----------|-------|-------------|------------|----------|-------------------------------------|-------------------------------------|--------------|-----------|----------|---------------|--------------|--------------------|--------------|-------------|-------------------|------------|-----------|-----------------|-----------|--------------------|---------------|
| 1  | 1       |           | 0     | 1           | -1         | 6566     | Org Property and Keyword Test       | Org Property and Keyword Test       |              |           |          |               |              |                    |              |             |                   |            |           |                 | 0         | 0                  | []            |
| 2  | 1       | 1         | 1     | 30          | 900        | 1517     | Expected file/root values           | Expected file/root values           |              |           |          |               |              |                    |              |             |                   |            |           |                 | 0         | 0                  | []            |
| 3  | 1       | 1         | 1     | 48          | 1517       | 2151     | Nearest ancestor wins               | Nearest ancestor wins               |              |           |          |               |              |                    |              |             |                   |            |           |                 | 0         | 0                  | []            |
| 4  | 1       | 3         | 2     | 53          | 1581       | 2151     | Parent override                     | Parent override                     |              |           |          |               |              |                    |              |             |                   |            |           |                 | 0         | 0                  | []            |
| 5  | 1       | 4         | 3     | 58          | 1635       | 1828     | Child inheriting nearest value      | Child inheriting nearest value      |              |           |          |               |              |                    |              |             |                   |            |           |                 | 0         | 0                  | []            |
| 6  | 1       | 4         | 3     | 66          | 1828       | 1995     | Child appending to nearest value    | Child appending to nearest value    |              |           |          |               |              |                    |              |             |                   |            |           |                 | 0         | 0                  | []            |
| 7  | 1       | 4         | 3     | 76          | 1995       | 2151     | Child replacing nearest value       | Child replacing nearest value       |              |           |          |               |              |                    |              |             |                   |            |           |                 | 0         | 0                  | []            |
| 8  | 1       | 1         | 1     | 86          | 2151       | 2718     | Root drawer inheritance             | Root drawer inheritance             |              |           |          |               |              |                    |              |             |                   |            |           |                 | 0         | 0                  | []            |
| 9  | 1       | 8         | 2     | 88          | 2178       | 2327     | Child inheriting root drawer base   | Child inheriting root drawer base   |              |           |          |               |              |                    |              |             |                   |            |           |                 | 0         | 0                  | []            |
| 10 | 1       | 8         | 2     | 95          | 2327       | 2527     | Child appending to root drawer base | Child appending to root drawer base |              |           |          |               |              |                    |              |             |                   |            |           |                 | 0         | 0                  | []            |

## keywords

| id | heading_id | keyword  |                value                 | line_number |
|----|------------|----------|--------------------------------------|-------------|
| 1  | 1          | TITLE    | Org Property and Keyword Test        | 13          |
| 2  | 1          | STARTUP  | showall                              | 14          |
| 3  | 1          | CATEGORY | category_keyword_value               | 15          |
| 4  | 1          | PROPERTY | KEYWORD_APPEND foo=1                 | 16          |
| 5  | 1          | PROPERTY | KEYWORD_APPEND+ bar=2                | 17          |
| 6  | 1          | PROPERTY | KEYWORD_APPEND+ baz=3                | 18          |
| 7  | 1          | PROPERTY | KEYWORD_DUPLICATE first              | 19          |
| 8  | 1          | PROPERTY | KEYWORD_DUPLICATE second             | 20          |
| 9  | 1          | PROPERTY | KEYWORD_RESET old                    | 21          |
| 10 | 1          | PROPERTY | KEYWORD_RESET+ appended-before-reset | 22          |

## links


## outline_path

| heading_id | file_id | parent_id | depth |  materialized_path  |                                                breadcrumbs_json                                                |
|------------|---------|-----------|-------|---------------------|----------------------------------------------------------------------------------------------------------------|
| 1          | 1       |           | 0     | 0000                | ["Org Property and Keyword Test"]                                                                              |
| 2          | 1       | 1         | 1     | 0000.0001           | ["Org Property and Keyword Test","Expected file/root values"]                                                  |
| 3          | 1       | 1         | 1     | 0000.0002           | ["Org Property and Keyword Test","Nearest ancestor wins"]                                                      |
| 4          | 1       | 3         | 2     | 0000.0002.0001      | ["Org Property and Keyword Test","Nearest ancestor wins","Parent override"]                                    |
| 5          | 1       | 4         | 3     | 0000.0002.0001.0001 | ["Org Property and Keyword Test","Nearest ancestor wins","Parent override","Child inheriting nearest value"]   |
| 6          | 1       | 4         | 3     | 0000.0002.0001.0002 | ["Org Property and Keyword Test","Nearest ancestor wins","Parent override","Child appending to nearest value"] |
| 7          | 1       | 4         | 3     | 0000.0002.0001.0003 | ["Org Property and Keyword Test","Nearest ancestor wins","Parent override","Child replacing nearest value"]    |
| 8          | 1       | 1         | 1     | 0000.0003           | ["Org Property and Keyword Test","Root drawer inheritance"]                                                    |
| 9          | 1       | 8         | 2     | 0000.0003.0001      | ["Org Property and Keyword Test","Root drawer inheritance","Child inheriting root drawer base"]                |
| 10         | 1       | 8         | 2     | 0000.0003.0002      | ["Org Property and Keyword Test","Root drawer inheritance","Child appending to root drawer base"]              |

## properties

| id | heading_id |          key          |                value                 |     source      | append | line_number |
|----|------------|-----------------------|--------------------------------------|-----------------|--------|-------------|
| 1  | 1          | CATEGORY              | Level 0 Category Property            | property_drawer | 0      | 2           |
| 2  | 1          | WHATEVER              | level 0 drawer property              | property_drawer | 0      | 3           |
| 3  | 1          | OVERWRITE             | this one works                       | property_drawer | 0      | 4           |
| 4  | 1          | ID                    | 7dad9b62-a3cc-43ec-a60f-e650bdaeae6d | property_drawer | 0      | 5           |
| 5  | 1          | ROOT_DRAWER_BASE      | root                                 | property_drawer | 0      | 6           |
| 6  | 1          | ROOT_DRAWER_APPEND    | root                                 | property_drawer | 0      | 7           |
| 7  | 1          | ROOT_DRAWER_APPEND    | appended                             | property_drawer | 1      | 8           |
| 8  | 1          | ROOT_DRAWER_DUPLICATE | first                                | property_drawer | 0      | 9           |
| 9  | 1          | ROOT_DRAWER_DUPLICATE | second                               | property_drawer | 0      | 10          |
| 10 | 1          | ROOT_OVERRIDE_CHAIN   | root                                 | property_drawer | 0      | 11          |

## tags


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
