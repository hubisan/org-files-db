# SQLite DB preview

## files

| id |                                                     path                                                      |      mtime_ns       | size | content_hash | indexed_at |
|----|---------------------------------------------------------------------------------------------------------------|---------------------|------|--------------|------------|
| 1  | /home/hubisan/projects/coding/org-files-db/.project/manual-tests/files/link-file-resolve/notes/index.org      | 1783094478000000000 | 2293 |              | 1783095160 |
| 2  | /home/hubisan/projects/coding/org-files-db/.project/manual-tests/files/link-file-resolve/notes/other/peer.org | 1783094478000000000 | 549  |              | 1783095160 |
| 3  | /home/hubisan/projects/coding/org-files-db/.project/manual-tests/files/link-file-resolve/notes/sub/child.org  | 1783094478000000000 | 725  |              | 1783095160 |
| 4  | /home/hubisan/projects/coding/org-files-db/.project/manual-tests/files/link-file-resolve/notes/target.org     | 1783094478000000000 | 1034 |              | 1783095160 |

## heading_bodies


## headings

| id | file_id | parent_id | level | line_number | byte_start | byte_end |               title               |             title_raw             | todo_keyword | todo_type | priority | scheduled_raw | scheduled_ts | deadline_raw | deadline_ts | closed_raw | closed_ts | archivedp | footnote_section_p | all_tags_json |
|----|---------|-----------|-------|-------------|------------|----------|-----------------------------------|-----------------------------------|--------------|-----------|----------|---------------|--------------|--------------|-------------|------------|-----------|-----------|--------------------|---------------|
| 1  | 1       |           | 0     | 1           | -1         | 2293     | Index                             | Index                             |              |           |          |               |              |              |             |            |           | 0         | 0                  | []            |
| 2  | 1       | 1         | 1     | 4           | 35         | 649      | Main Index                        | Main Index                        |              |           |          |               |              |              |             |            |           | 0         | 0                  | []            |
| 3  | 1       | 2         | 2     | 10          | 109        | 267      | Same-file targets                 | Same-file targets                 |              |           |          |               |              |              |             |            |           | 0         | 0                  | []            |
| 4  | 1       | 2         | 2     | 18          | 267        | 374      | Brackets in Title [2026-07-03 Fr] | Brackets in Title [2026-07-03 Fr] |              |           |          |               |              |              |             |            |           | 0         | 0                  | []            |
| 5  | 1       | 2         | 2     | 24          | 374        | 532      | Duplicate Custom                  | Duplicate Custom                  |              |           |          |               |              |              |             |            |           | 0         | 0                  | []            |
| 6  | 1       | 2         | 2     | 31          | 532        | 649      | Duplicate Custom Later            | Duplicate Custom Later            |              |           |          |               |              |              |             |            |           | 0         | 0                  | []            |
| 7  | 1       | 1         | 1     | 38          | 649        | 977      | File target links                 | File target links                 |              |           |          |               |              |              |             |            |           | 0         | 0                  | []            |
| 8  | 1       | 1         | 1     | 58          | 977        | 1292     | File heading-title links          | File heading-title links          |              |           |          |               |              |              |             |            |           | 0         | 0                  | []            |
| 9  | 1       | 1         | 1     | 68          | 1292       | 1451     | Same-file star heading links      | Same-file star heading links      |              |           |          |               |              |              |             |            |           | 0         | 0                  | []            |
| 10 | 1       | 1         | 1     | 75          | 1451       | 1636     | Same-file CUSTOM_ID links         | Same-file CUSTOM_ID links         |              |           |          |               |              |              |             |            |           | 0         | 0                  | []            |

## keywords

| id | heading_id | keyword |  value  | line_number |
|----|------------|---------|---------|-------------|
| 1  | 1          | TITLE   | Index   | 1           |
| 2  | 1          | STARTUP | showall | 2           |
| 3  | 13         | TITLE   | Peer    | 1           |
| 4  | 13         | STARTUP | showall | 2           |
| 5  | 16         | TITLE   | Child   | 1           |
| 6  | 16         | STARTUP | showall | 2           |
| 7  | 18         | TITLE   | Target  | 1           |
| 8  | 18         | STARTUP | showall | 2           |

## links

| id | file_id | heading_id | byte_start | byte_end | line | source_context | format  |                 raw                  |            raw_target            | raw_description | link_type |          path           |  search_option  |                                                    path_absolute                                                     | target_file_id | target_heading_id | target_custom_id | target_id | resolution_status |    resolution_diagnostic    |
|----|---------|------------|------------|----------|------|----------------|---------|--------------------------------------|----------------------------------|-----------------|-----------|-------------------------|-----------------|----------------------------------------------------------------------------------------------------------------------|----------------|-------------------|------------------|-----------|-------------------|-----------------------------|
| 1  | 1       | 7          | 693        | 712      | 42   | normal         | bracket | [[file:target.org]]                  | file:target.org                  |                 | file      | target.org              |                 | /home/hubisan/projects/coding/org-files-db/.project/manual-tests/files/link-file-resolve/notes/target.org            | 4              |                   |                  |           | resolved          |                             |
| 2  | 1       | 7          | 715        | 737      | 43   | normal         | bracket | [[file:sub/child.org]]               | file:sub/child.org               |                 | file      | sub/child.org           |                 | /home/hubisan/projects/coding/org-files-db/.project/manual-tests/files/link-file-resolve/notes/sub/child.org         | 3              |                   |                  |           | resolved          |                             |
| 3  | 1       | 7          | 740        | 763      | 44   | normal         | bracket | [[file:other/peer.org]]              | file:other/peer.org              |                 | file      | other/peer.org          |                 | /home/hubisan/projects/coding/org-files-db/.project/manual-tests/files/link-file-resolve/notes/other/peer.org        | 2              |                   |                  |           | resolved          |                             |
| 4  | 1       | 7          | 766        | 782      | 45   | normal         | bracket | [[./target.org]]                     | ./target.org                     |                 | file      | ./target.org            |                 | /home/hubisan/projects/coding/org-files-db/.project/manual-tests/files/link-file-resolve/notes/target.org            | 4              |                   |                  |           | resolved          |                             |
| 5  | 1       | 7          | 815        | 832      | 49   | normal         | angle   | <file:target.org>                    | file:target.org                  |                 | file      | target.org              |                 | /home/hubisan/projects/coding/org-files-db/.project/manual-tests/files/link-file-resolve/notes/target.org            | 4              |                   |                  |           | resolved          |                             |
| 6  | 1       | 7          | 835        | 850      | 50   | normal         | plain   | file:target.org                      | file:target.org                  |                 | file      | target.org              |                 | /home/hubisan/projects/coding/org-files-db/.project/manual-tests/files/link-file-resolve/notes/target.org            | 4              |                   |                  |           | resolved          |                             |
| 7  | 1       | 7          | 887        | 907      | 54   | normal         | bracket | [[file:missing.org]]                 | file:missing.org                 |                 | file      | missing.org             |                 | /home/hubisan/projects/coding/org-files-db/.project/manual-tests/files/link-file-resolve/notes/missing.org           |                |                   |                  |           | broken            | missing in indexed universe |
| 8  | 1       | 7          | 910        | 940      | 55   | normal         | bracket | [[file:sub/missing-child.org]]       | file:sub/missing-child.org       |                 | file      | sub/missing-child.org   |                 | /home/hubisan/projects/coding/org-files-db/.project/manual-tests/files/link-file-resolve/notes/sub/missing-child.org |                |                   |                  |           | broken            | missing in indexed universe |
| 9  | 1       | 7          | 943        | 975      | 56   | normal         | bracket | [[file:../external/outside.org]]     | file:../external/outside.org     |                 | file      | ../external/outside.org |                 | /home/hubisan/projects/coding/org-files-db/.project/manual-tests/files/link-file-resolve/external/outside.org        |                |                   |                  |           | unresolved        | outside indexed universe    |
| 10 | 1       | 8          | 1007       | 1043     | 60   | normal         | bracket | [[file:target.org::*Target Heading]] | file:target.org::*Target Heading |                 | file      | target.org              | *Target Heading | /home/hubisan/projects/coding/org-files-db/.project/manual-tests/files/link-file-resolve/notes/target.org            | 4              | 19                |                  |           | resolved          |                             |

## outline_path

| heading_id | file_id | parent_id | depth | materialized_path |                      breadcrumbs_json                      |
|------------|---------|-----------|-------|-------------------|------------------------------------------------------------|
| 1          | 1       |           | 0     | 0000              | ["Index"]                                                  |
| 2          | 1       | 1         | 1     | 0000.0001         | ["Index","Main Index"]                                     |
| 3          | 1       | 2         | 2     | 0000.0001.0001    | ["Index","Main Index","Same-file targets"]                 |
| 4          | 1       | 2         | 2     | 0000.0001.0002    | ["Index","Main Index","Brackets in Title [2026-07-03 Fr]"] |
| 5          | 1       | 2         | 2     | 0000.0001.0003    | ["Index","Main Index","Duplicate Custom"]                  |
| 6          | 1       | 2         | 2     | 0000.0001.0004    | ["Index","Main Index","Duplicate Custom Later"]            |
| 7          | 1       | 1         | 1     | 0000.0002         | ["Index","File target links"]                              |
| 8          | 1       | 1         | 1     | 0000.0003         | ["Index","File heading-title links"]                       |
| 9          | 1       | 1         | 1     | 0000.0004         | ["Index","Same-file star heading links"]                   |
| 10         | 1       | 1         | 1     | 0000.0005         | ["Index","Same-file CUSTOM_ID links"]                      |

## properties

| id | heading_id |    key    |      value       |     source      | append | line_number |
|----|------------|-----------|------------------|-----------------|--------|-------------|
| 1  | 2          | CUSTOM_ID | main-index       | property_drawer | 0      | 6           |
| 2  | 2          | ID        | index-id-001     | property_drawer | 0      | 7           |
| 3  | 3          | CUSTOM_ID | same-file-target | property_drawer | 0      | 12          |
| 4  | 3          | ID        | same-file-id-001 | property_drawer | 0      | 13          |
| 5  | 4          | CUSTOM_ID | brackets-heading | property_drawer | 0      | 20          |
| 6  | 4          | ID        | brackets-id-001  | property_drawer | 0      | 21          |
| 7  | 5          | CUSTOM_ID | duplicate-custom | property_drawer | 0      | 26          |
| 8  | 6          | CUSTOM_ID | DUPLICATE-CUSTOM | property_drawer | 0      | 33          |
| 9  | 14         | CUSTOM_ID | peer-custom-id   | property_drawer | 0      | 6           |
| 10 | 14         | ID        | peer-id-001      | property_drawer | 0      | 7           |

## tags


## timestamp_repeaters


## timestamps

| id | heading_id | role |  start_ts  | end_ts |   type   | range_type |    raw_value    | byte_start | byte_end | line_number |
|----|------------|------|------------|--------|----------|------------|-----------------|------------|----------|-------------|
| 1  | 4          | body | 1783036800 |        | inactive | none       | [2026-07-03 Fr] | 288        | 303      | 18          |

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
