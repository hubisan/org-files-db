# SQLite DB preview

## db_metadata

|         key         | value |
|---------------------|-------|
| fts_available       | 0     |
| fts_body_indexed    | 0     |
| fts_schema_version  | 0     |
| body_text_available | 0     |

## files

| id |                                                     path                                                      |      mtime_ns       | size  | content_hash | indexed_at |
|----|---------------------------------------------------------------------------------------------------------------|---------------------|-------|--------------|------------|
| 1  | /home/hubisan/projects/coding/org-files-db/.project/manual-tests/query-model/org/multipe-title-keywords.org   | 1781826194942471483 | 321   |              | 1784332328 |
| 2  | /home/hubisan/projects/coding/org-files-db/.project/manual-tests/query-model/org/no-title-set.org             | 1783850095870831478 | 44    |              | 1784332328 |
| 3  | /home/hubisan/projects/coding/org-files-db/.project/manual-tests/query-model/org/properties.org               | 1784317456755088878 | 15145 |              | 1784332328 |
| 4  | /home/hubisan/projects/coding/org-files-db/.project/manual-tests/query-model/org/title.org                    | 1784332287046809796 | 115   |              | 1784332328 |
| 5  | /home/hubisan/projects/coding/org-files-db/.project/manual-tests/query-model/org/todo-keywords-file-local.org | 1784331445500650156 | 1118  |              | 1784332328 |
| 6  | /home/hubisan/projects/coding/org-files-db/.project/manual-tests/query-model/org/todo-keywords.org            | 1784331431167729427 | 245   |              | 1784332328 |

## heading_bodies


## headings

| id | file_id | parent_id | level | line_number | byte_start | byte_end |                  title                   |                title_raw                 | todo_keyword | todo_type | priority | scheduled_raw | scheduled_ts | scheduled_has_time | deadline_raw | deadline_ts | deadline_has_time | closed_raw | closed_ts | closed_has_time | archivedp | footnote_section_p | all_tags_json |
|----|---------|-----------|-------|-------------|------------|----------|------------------------------------------|------------------------------------------|--------------|-----------|----------|---------------|--------------|--------------------|--------------|-------------|-------------------|------------|-----------|-----------------|-----------|--------------------|---------------|
| 1  | 1       |           | 0     | 1           | -1         | 321      | Title can span multiple lines, even here | Title can span multiple lines, even here |              |           |          |               |              |                    |              |             |                   |            |           |                 | 0         | 0                  | []            |
| 2  | 1       | 1         | 1     | 7           | 136        | 321      | Unfortunately Everywhere                 | Unfortunately Everywhere                 |              |           |          |               |              |                    |              |             |                   |            |           |                 | 0         | 0                  | []            |
| 3  | 2       |           | 0     | 1           | -1         | 44       | no-title-set                             |                                          |              |           |          |               |              |                    |              |             |                   |            |           |                 | 0         | 0                  | []            |
| 4  | 2       | 3         | 1     | 2           | 1          | 44       | The parent title should be the file name | The parent title should be the file name |              |           |          |               |              |                    |              |             |                   |            |           |                 | 0         | 0                  | []            |
| 5  | 3       |           | 0     | 1           | -1         | 15145    | Org Property and Keyword Test            | Org Property and Keyword Test            |              |           |          |               |              |                    |              |             |                   |            |           |                 | 0         | 0                  | []            |
| 6  | 3       | 5         | 1     | 38          | 1065       | 1115     | Empty Property                           | Empty Property                           |              |           |          |               |              |                    |              |             |                   |            |           |                 | 0         | 0                  | []            |
| 7  | 3       | 5         | 1     | 43          | 1115       | 2674     | Expected file/root values                | Expected file/root values                |              |           |          |               |              |                    |              |             |                   |            |           |                 | 0         | 0                  | []            |
| 8  | 3       | 5         | 1     | 73          | 2674       | 3084     | Local append on the same heading         | Local append on the same heading         |              |           |          |               |              |                    |              |             |                   |            |           |                 | 0         | 0                  | []            |
| 9  | 3       | 5         | 1     | 87          | 3084       | 3422     | Multiple local append rows               | Multiple local append rows               |              |           |          |               |              |                    |              |             |                   |            |           |                 | 0         | 0                  | []            |
| 10 | 3       | 5         | 1     | 101         | 3422       | 3839     | Local append without a base value        | Local append without a base value        |              |           |          |               |              |                    |              |             |                   |            |           |                 | 0         | 0                  | []            |

## keywords

| id | heading_id | keyword  |             value             | line_number |
|----|------------|----------|-------------------------------|-------------|
| 1  | 1          | TITLE    | Title can span                | 1           |
| 2  | 1          | TITLE    | multiple lines,               | 2           |
| 3  | 1          | AUTHOR   | Hubisan                       | 3           |
| 4  | 1          | TITLE    | even here                     | 9           |
| 5  | 5          | TITLE    | Org Property and Keyword Test | 20          |
| 6  | 5          | STARTUP  | showall                       | 21          |
| 7  | 5          | CATEGORY | category_keyword_value        | 22          |
| 8  | 5          | PROPERTY | KEYWORD_APPEND foo=1          | 23          |
| 9  | 5          | PROPERTY | KEYWORD_APPEND+ bar=2         | 24          |
| 10 | 5          | PROPERTY | KEYWORD_APPEND+ baz=3         | 25          |

## links

| id | file_id | heading_id | byte_start | byte_end | line | source_context | format  |                              raw                              |                        raw_target                         | raw_description | link_type |                         path                         | search_option |                                                  path_absolute                                                  | target_file_id | target_heading_id | target_custom_id | target_id | resolution_status |  resolution_diagnostic   |
|----|---------|------------|------------|----------|------|----------------|---------|---------------------------------------------------------------|-----------------------------------------------------------|-----------------|-----------|------------------------------------------------------|---------------|-----------------------------------------------------------------------------------------------------------------|----------------|-------------------|------------------|-----------|-------------------|--------------------------|
| 1  | 1       | 1          | 73         | 134      | 5    | normal         | bracket | [[file:../../notes/org-semantics/multipe-title-keywords.org]] | file:../../notes/org-semantics/multipe-title-keywords.org |                 | file      | ../../notes/org-semantics/multipe-title-keywords.org |               | /home/hubisan/projects/coding/org-files-db/.project/manual-tests/notes/org-semantics/multipe-title-keywords.org |                |                   |                  |           | unresolved        | outside indexed universe |

## outline_path

| heading_id | file_id | parent_id | depth | materialized_path |                            breadcrumbs_json                             |
|------------|---------|-----------|-------|-------------------|-------------------------------------------------------------------------|
| 1          | 1       |           | 0     | 0000              | ["Title can span multiple lines, even here"]                            |
| 2          | 1       | 1         | 1     | 0000.0001         | ["Title can span multiple lines, even here","Unfortunately Everywhere"] |
| 3          | 2       |           | 0     | 0000              | ["no-title-set"]                                                        |
| 4          | 2       | 3         | 1     | 0000.0001         | ["no-title-set","The parent title should be the file name"]             |
| 5          | 3       |           | 0     | 0000              | ["Org Property and Keyword Test"]                                       |
| 6          | 3       | 5         | 1     | 0000.0001         | ["Org Property and Keyword Test","Empty Property"]                      |
| 7          | 3       | 5         | 1     | 0000.0002         | ["Org Property and Keyword Test","Expected file/root values"]           |
| 8          | 3       | 5         | 1     | 0000.0003         | ["Org Property and Keyword Test","Local append on the same heading"]    |
| 9          | 3       | 5         | 1     | 0000.0004         | ["Org Property and Keyword Test","Multiple local append rows"]          |
| 10         | 3       | 5         | 1     | 0000.0005         | ["Org Property and Keyword Test","Local append without a base value"]   |

## properties

| id | heading_id |          key          |                value                 |     source      | append | line_number |
|----|------------|-----------------------|--------------------------------------|-----------------|--------|-------------|
| 1  | 5          | CATEGORY              | Level 0 Category Property            | property_drawer | 0      | 2           |
| 2  | 5          | WHATEVER              | level 0 drawer property              | property_drawer | 0      | 3           |
| 3  | 5          | OVERWRITE             | this one works                       | property_drawer | 0      | 4           |
| 4  | 5          | ID                    | 7dad9b62-a3cc-43ec-a60f-e650bdaeae6d | property_drawer | 0      | 5           |
| 5  | 5          | ROOT_DRAWER_BASE      | root                                 | property_drawer | 0      | 6           |
| 6  | 5          | ROOT_DRAWER_APPEND    | root                                 | property_drawer | 0      | 7           |
| 7  | 5          | ROOT_DRAWER_APPEND    | appended                             | property_drawer | 1      | 8           |
| 8  | 5          | ROOT_DRAWER_DUPLICATE | first                                | property_drawer | 0      | 9           |
| 9  | 5          | ROOT_DRAWER_DUPLICATE | second                               | property_drawer | 0      | 10          |
| 10 | 5          | ROOT_OVERRIDE_CHAIN   | this                                 | property_drawer | 0      | 11          |

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
| 3       | TODO    | open       |          | 0           | config_default |                |                    |
| 3       | NEXT    | open       |          | 1           | config_default |                |                    |

<!--
Local Variables:
eval: (visual-fill-column-mode -1)
truncate-lines: t
End:
-->
