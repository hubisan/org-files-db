# SQLite DB preview

## db_metadata

|         key         | value |
|---------------------|-------|
| fts_available       | 0     |
| fts_body_indexed    | 0     |
| fts_schema_version  | 0     |
| body_text_available | 0     |

## files

| id |                                                path                                                |      mtime_ns       | size  | content_hash | indexed_at |
|----|----------------------------------------------------------------------------------------------------|---------------------|-------|--------------|------------|
| 1  | /home/hubisan/projects/coding/org-files-db/.project/manual-tests/query-model/org/properties.org    | 1784301238008799537 | 15518 |              | 1784313560 |
| 2  | /home/hubisan/projects/coding/org-files-db/.project/manual-tests/query-model/org/todo-keywords.org | 1784242830730251043 | 0     |              | 1784313560 |

## heading_bodies


## headings

| id | file_id | parent_id | level | line_number | byte_start | byte_end |                         title                          |                       title_raw                        | todo_keyword | todo_type | priority | scheduled_raw | scheduled_ts | scheduled_has_time | deadline_raw | deadline_ts | deadline_has_time | closed_raw | closed_ts | closed_has_time | archivedp | footnote_section_p | all_tags_json |
|----|---------|-----------|-------|-------------|------------|----------|--------------------------------------------------------|--------------------------------------------------------|--------------|-----------|----------|---------------|--------------|--------------------|--------------|-------------|-------------------|------------|-----------|-----------------|-----------|--------------------|---------------|
| 1  | 1       |           | 0     | 1           | -1         | 15518    | Org Property and Keyword Test                          | Org Property and Keyword Test                          |              |           |          |               |              |                    |              |             |                   |            |           |                 | 0         | 0                  | []            |
| 2  | 1       | 1         | 1     | 38          | 1065       | 1115     | Empty Property                                         | Empty Property                                         |              |           |          |               |              |                    |              |             |                   |            |           |                 | 0         | 0                  | []            |
| 3  | 1       | 1         | 1     | 43          | 1115       | 2674     | Expected file/root values                              | Expected file/root values                              |              |           |          |               |              |                    |              |             |                   |            |           |                 | 0         | 0                  | []            |
| 4  | 1       | 1         | 1     | 73          | 2674       | 3084     | Local append on the same heading                       | Local append on the same heading                       |              |           |          |               |              |                    |              |             |                   |            |           |                 | 0         | 0                  | []            |
| 5  | 1       | 1         | 1     | 87          | 3084       | 3422     | Multiple local append rows                             | Multiple local append rows                             |              |           |          |               |              |                    |              |             |                   |            |           |                 | 0         | 0                  | []            |
| 6  | 1       | 1         | 1     | 101         | 3422       | 3839     | Local append without a base value                      | Local append without a base value                      |              |           |          |               |              |                    |              |             |                   |            |           |                 | 0         | 0                  | []            |
| 7  | 1       | 1         | 1     | 114         | 3839       | 4251     | Later local definition replaces the earlier definition | Later local definition replaces the earlier definition |              |           |          |               |              |                    |              |             |                   |            |           |                 | 0         | 0                  | []            |
| 8  | 1       | 1         | 1     | 129         | 4251       | 4759     | Local append followed by replacement                   | Local append followed by replacement                   |              |           |          |               |              |                    |              |             |                   |            |           |                 | 0         | 0                  | []            |
| 9  | 1       | 1         | 1     | 144         | 4759       | 5062     | Empty base followed by append                          | Empty base followed by append                          |              |           |          |               |              |                    |              |             |                   |            |           |                 | 0         | 0                  | []            |
| 10 | 1       | 1         | 1     | 157         | 5062       | 5366     | Base followed by empty append                          | Base followed by empty append                          |              |           |          |               |              |                    |              |             |                   |            |           |                 | 0         | 0                  | []            |

## keywords

| id | heading_id | keyword  |                value                 | line_number |
|----|------------|----------|--------------------------------------|-------------|
| 1  | 1          | TITLE    | Org Property and Keyword Test        | 20          |
| 2  | 1          | STARTUP  | showall                              | 21          |
| 3  | 1          | CATEGORY | category_keyword_value               | 22          |
| 4  | 1          | PROPERTY | KEYWORD_APPEND foo=1                 | 23          |
| 5  | 1          | PROPERTY | KEYWORD_APPEND+ bar=2                | 24          |
| 6  | 1          | PROPERTY | KEYWORD_APPEND+ baz=3                | 25          |
| 7  | 1          | PROPERTY | KEYWORD_DUPLICATE first              | 26          |
| 8  | 1          | PROPERTY | KEYWORD_DUPLICATE second             | 27          |
| 9  | 1          | PROPERTY | KEYWORD_RESET old                    | 28          |
| 10 | 1          | PROPERTY | KEYWORD_RESET+ appended-before-reset | 29          |

## links


## outline_path

| heading_id | file_id | parent_id | depth | materialized_path |                                      breadcrumbs_json                                      |
|------------|---------|-----------|-------|-------------------|--------------------------------------------------------------------------------------------|
| 1          | 1       |           | 0     | 0000              | ["Org Property and Keyword Test"]                                                          |
| 2          | 1       | 1         | 1     | 0000.0001         | ["Org Property and Keyword Test","Empty Property"]                                         |
| 3          | 1       | 1         | 1     | 0000.0002         | ["Org Property and Keyword Test","Expected file/root values"]                              |
| 4          | 1       | 1         | 1     | 0000.0003         | ["Org Property and Keyword Test","Local append on the same heading"]                       |
| 5          | 1       | 1         | 1     | 0000.0004         | ["Org Property and Keyword Test","Multiple local append rows"]                             |
| 6          | 1       | 1         | 1     | 0000.0005         | ["Org Property and Keyword Test","Local append without a base value"]                      |
| 7          | 1       | 1         | 1     | 0000.0006         | ["Org Property and Keyword Test","Later local definition replaces the earlier definition"] |
| 8          | 1       | 1         | 1     | 0000.0007         | ["Org Property and Keyword Test","Local append followed by replacement"]                   |
| 9          | 1       | 1         | 1     | 0000.0008         | ["Org Property and Keyword Test","Empty base followed by append"]                          |
| 10         | 1       | 1         | 1     | 0000.0009         | ["Org Property and Keyword Test","Base followed by empty append"]                          |

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
| 10 | 1          | ROOT_OVERRIDE_CHAIN   | this                                 | property_drawer | 0      | 11          |

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
