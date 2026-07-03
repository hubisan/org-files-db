# SQLite DB preview

## files

| id |                                                     path                                                      |      mtime_ns       | size | content_hash | indexed_at |
|----|---------------------------------------------------------------------------------------------------------------|---------------------|------|--------------|------------|
| 1  | /home/hubisan/projects/coding/org-files-db/.project/manual-tests/files/link-file-resolve/notes/index.org      | 1783033910800437182 | 867  |              | 1783074939 |
| 2  | /home/hubisan/projects/coding/org-files-db/.project/manual-tests/files/link-file-resolve/notes/other/peer.org | 1783029353233070171 | 343  |              | 1783074939 |
| 3  | /home/hubisan/projects/coding/org-files-db/.project/manual-tests/files/link-file-resolve/notes/sub/child.org  | 1783029353234070177 | 428  |              | 1783074939 |
| 4  | /home/hubisan/projects/coding/org-files-db/.project/manual-tests/files/link-file-resolve/notes/target.org     | 1783074927466121203 | 1310 |              | 1783074939 |

## heading_bodies


## headings

| id | file_id | parent_id | level | line_number | byte_start | byte_end |        title         |      title_raw       | todo_keyword | todo_type | priority | scheduled_raw | scheduled_ts | deadline_raw | deadline_ts | closed_raw | closed_ts | archivedp | footnote_section_p | all_tags_json |
|----|---------|-----------|-------|-------------|------------|----------|----------------------|----------------------|--------------|-----------|----------|---------------|--------------|--------------|-------------|------------|-----------|-----------|--------------------|---------------|
| 1  | 1       |           | 0     | 1           | -1         | 867      | Index                | Index                |              |           |          |               |              |              |             |            |           | 0         | 0                  | []            |
| 2  | 1       | 1         | 1     | 4           | 35         | 860      | Main index           | Main index           |              |           |          |               |              |              |             |            |           | 0         | 0                  | []            |
| 3  | 1       | 1         | 1     | 43          | 860        | 867      | Main                 | Main                 |              |           |          |               |              |              |             |            |           | 0         | 0                  | []            |
| 4  | 2       |           | 0     | 1           | -1         | 343      | Peer                 | Peer                 |              |           |          |               |              |              |             |            |           | 0         | 0                  | []            |
| 5  | 2       | 4         | 1     | 4           | 34         | 343      | Peer heading         | Peer heading         |              |           |          |               |              |              |             |            |           | 0         | 0                  | []            |
| 6  | 3       |           | 0     | 1           | -1         | 428      | Child                | Child                |              |           |          |               |              |              |             |            |           | 0         | 0                  | []            |
| 7  | 3       | 6         | 1     | 4           | 35         | 428      | Child heading        | Child heading        |              |           |          |               |              |              |             |            |           | 0         | 0                  | []            |
| 8  | 4       |           | 0     | 1           | -1         | 1310     | Target               | Target               |              |           |          |               |              |              |             |            |           | 0         | 0                  | []            |
| 9  | 4       | 8         | 1     | 4           | 36         | 222      | Target heading       | Target heading       |              |           |          |               |              |              |             |            |           | 0         | 0                  | []            |
| 10 | 4       | 8         | 1     | 16          | 222        | 305      | Invalid Target Links | Invalid Target Links |              |           |          |               |              |              |             |            |           | 0         | 0                  | []            |

## keywords

| id | heading_id | keyword |  value  | line_number |
|----|------------|---------|---------|-------------|
| 1  | 1          | TITLE   | Index   | 1           |
| 2  | 1          | STARTUP | showall | 2           |
| 3  | 4          | TITLE   | Peer    | 1           |
| 4  | 4          | STARTUP | showall | 2           |
| 5  | 6          | TITLE   | Child   | 1           |
| 6  | 6          | STARTUP | showall | 2           |
| 7  | 8          | TITLE   | Target  | 1           |
| 8  | 8          | STARTUP | showall | 2           |

## links

| id | file_id | heading_id | byte_start | byte_end | line | source_context | format  |                   raw                    |              raw_target              | raw_description | link_type |      path       |   search_option   |                                                 path_absolute                                                 | target_file_id | target_heading_id | target_custom_id | target_id | resolution_status | resolution_diagnostic |
|----|---------|------------|------------|----------|------|----------------|---------|------------------------------------------|--------------------------------------|-----------------|-----------|-----------------|-------------------|---------------------------------------------------------------------------------------------------------------|----------------|-------------------|------------------|-----------|-------------------|-----------------------|
| 1  | 1       | 2          | 133        | 152      | 12   | normal         | bracket | [[file:target.org]]                      | file:target.org                      |                 | file      | target.org      |                   | /home/hubisan/projects/coding/org-files-db/.project/manual-tests/files/link-file-resolve/notes/target.org     | 4              |                   |                  |           | resolved          |                       |
| 2  | 1       | 2          | 155        | 177      | 13   | normal         | bracket | [[file:sub/child.org]]                   | file:sub/child.org                   |                 | file      | sub/child.org   |                   | /home/hubisan/projects/coding/org-files-db/.project/manual-tests/files/link-file-resolve/notes/sub/child.org  | 3              |                   |                  |           | resolved          |                       |
| 3  | 1       | 2          | 180        | 203      | 14   | normal         | bracket | [[file:other/peer.org]]                  | file:other/peer.org                  |                 | file      | other/peer.org  |                   | /home/hubisan/projects/coding/org-files-db/.project/manual-tests/files/link-file-resolve/notes/other/peer.org | 2              |                   |                  |           | resolved          |                       |
| 4  | 1       | 2          | 206        | 222      | 15   | normal         | bracket | [[./target.org]]                         | ./target.org                         |                 | file      | ./target.org    |                   | /home/hubisan/projects/coding/org-files-db/.project/manual-tests/files/link-file-resolve/notes/target.org     | 4              |                   |                  |           | resolved          |                       |
| 5  | 1       | 2          | 225        | 244      | 16   | normal         | bracket | [[./sub/child.org]]                      | ./sub/child.org                      |                 | file      | ./sub/child.org |                   | /home/hubisan/projects/coding/org-files-db/.project/manual-tests/files/link-file-resolve/notes/sub/child.org  | 3              |                   |                  |           | resolved          |                       |
| 6  | 1       | 2          | 247        | 265      | 17   | normal         | bracket | [[file:index.org]]                       | file:index.org                       |                 | file      | index.org       |                   | /home/hubisan/projects/coding/org-files-db/.project/manual-tests/files/link-file-resolve/notes/index.org      | 1              |                   |                  |           | resolved          |                       |
| 7  | 1       | 2          | 343        | 379      | 22   | normal         | bracket | [[file:target.org::*Target heading]]     | file:target.org::*Target heading     |                 | file      | target.org      | *Target heading   | /home/hubisan/projects/coding/org-files-db/.project/manual-tests/files/link-file-resolve/notes/target.org     | 4              | 9                 |                  |           | resolved          |                       |
| 8  | 1       | 2          | 382        | 420      | 23   | normal         | bracket | [[file:target.org::#target-custom-id]]   | file:target.org::#target-custom-id   |                 | file      | target.org      | #target-custom-id | /home/hubisan/projects/coding/org-files-db/.project/manual-tests/files/link-file-resolve/notes/target.org     | 4              |                   |                  |           | resolved          |                       |
| 9  | 1       | 2          | 423        | 461      | 24   | normal         | bracket | [[file:sub/child.org::*Child heading]]   | file:sub/child.org::*Child heading   |                 | file      | sub/child.org   | *Child heading    | /home/hubisan/projects/coding/org-files-db/.project/manual-tests/files/link-file-resolve/notes/sub/child.org  | 3              | 7                 |                  |           | resolved          |                       |
| 10 | 1       | 2          | 464        | 504      | 25   | normal         | bracket | [[file:sub/child.org::#child-custom-id]] | file:sub/child.org::#child-custom-id |                 | file      | sub/child.org   | #child-custom-id  | /home/hubisan/projects/coding/org-files-db/.project/manual-tests/files/link-file-resolve/notes/sub/child.org  | 3              |                   |                  |           | resolved          |                       |

## outline_path

| heading_id | file_id | parent_id | depth | materialized_path |         breadcrumbs_json          |
|------------|---------|-----------|-------|-------------------|-----------------------------------|
| 1          | 1       |           | 0     | 0000              | ["Index"]                         |
| 2          | 1       | 1         | 1     | 0000.0001         | ["Index","Main index"]            |
| 3          | 1       | 1         | 1     | 0000.0002         | ["Index","Main"]                  |
| 4          | 2       |           | 0     | 0000              | ["Peer"]                          |
| 5          | 2       | 4         | 1     | 0000.0001         | ["Peer","Peer heading"]           |
| 6          | 3       |           | 0     | 0000              | ["Child"]                         |
| 7          | 3       | 6         | 1     | 0000.0001         | ["Child","Child heading"]         |
| 8          | 4       |           | 0     | 0000              | ["Target"]                        |
| 9          | 4       | 8         | 1     | 0000.0001         | ["Target","Target heading"]       |
| 10         | 4       | 8         | 1     | 0000.0002         | ["Target","Invalid Target Links"] |

## properties

| id | heading_id |    key    |                value                 |     source      | append | line_number |
|----|------------|-----------|--------------------------------------|-----------------|--------|-------------|
| 1  | 2          | CUSTOM_ID | main-index                           | property_drawer | 0      | 6           |
| 2  | 2          | ID        | index-id-001                         | property_drawer | 0      | 7           |
| 3  | 5          | CUSTOM_ID | peer-custom-id                       | property_drawer | 0      | 6           |
| 4  | 5          | ID        | peer-id-001                          | property_drawer | 0      | 7           |
| 5  | 7          | CUSTOM_ID | child-custom-id                      | property_drawer | 0      | 6           |
| 6  | 7          | ID        | child-id-001                         | property_drawer | 0      | 7           |
| 7  | 9          | CUSTOM_ID | target-custom-id                     | property_drawer | 0      | 6           |
| 8  | 9          | ID        | target-id-001                        | property_drawer | 0      | 7           |
| 9  | 14         | CUSTOM_ID | e2522e10-0cbf-4e55-9ece-da1d0b7d1b58 | property_drawer | 0      | 38          |

## tags


## timestamp_repeaters


## timestamps

| id | heading_id | role |  start_ts  | end_ts |   type   | range_type |    raw_value    | byte_start | byte_end | line_number |
|----|------------|------|------------|--------|----------|------------|-----------------|------------|----------|-------------|
| 1  | 15         | body | 1783036800 |        | inactive | none       | [2026-07-03 Fr] | 838        | 853      | 43          |
| 2  | 15         | body | 1783036800 |        | inactive | none       | [2026-07-03 Fr] | 894        | 909      | 45          |

## todo_keywords

| file_id | keyword | state_type | shortcut | sequence_no |  source_kind   | source_keyword | source_line_number |
|---------|---------|------------|----------|-------------|----------------|----------------|--------------------|
| 1       | TODO    | open       |          | 0           | config_default |                |                    |
| 1       | DONE    | closed     |          | 1           | config_default |                |                    |
| 2       | TODO    | open       |          | 0           | config_default |                |                    |
| 2       | DONE    | closed     |          | 1           | config_default |                |                    |
| 3       | TODO    | open       |          | 0           | config_default |                |                    |
| 3       | DONE    | closed     |          | 1           | config_default |                |                    |
| 4       | TODO    | open       |          | 0           | config_default |                |                    |
| 4       | DONE    | closed     |          | 1           | config_default |                |                    |

<!--
Local Variables:
eval: (visual-fill-column-mode -1)
truncate-lines: t
End:
-->
