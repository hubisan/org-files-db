# SQLite DB preview

## files

| id |                                                     path                                                      |      mtime_ns       | size | content_hash | indexed_at |
|----|---------------------------------------------------------------------------------------------------------------|---------------------|------|--------------|------------|
| 1  | /home/hubisan/projects/coding/org-files-db/.project/manual-tests/files/link-file-resolve/notes/index.org      | 1783114181649212594 | 2629 |              | 1783115425 |
| 2  | /home/hubisan/projects/coding/org-files-db/.project/manual-tests/files/link-file-resolve/notes/other/peer.org | 1783094478000000000 | 549  |              | 1783115425 |
| 3  | /home/hubisan/projects/coding/org-files-db/.project/manual-tests/files/link-file-resolve/notes/sub/child.org  | 1783094478000000000 | 725  |              | 1783115425 |
| 4  | /home/hubisan/projects/coding/org-files-db/.project/manual-tests/files/link-file-resolve/notes/target.org     | 1783094478000000000 | 1034 |              | 1783115425 |

## heading_bodies


## headings

| id | file_id | parent_id | level | line_number | byte_start | byte_end |               title               |             title_raw             | todo_keyword | todo_type | priority | scheduled_raw | scheduled_ts | deadline_raw | deadline_ts | closed_raw | closed_ts | archivedp | footnote_section_p | all_tags_json |
|----|---------|-----------|-------|-------------|------------|----------|-----------------------------------|-----------------------------------|--------------|-----------|----------|---------------|--------------|--------------|-------------|------------|-----------|-----------|--------------------|---------------|
| 1  | 1       |           | 0     | 1           | -1         | 2629     | Index                             | Index                             |              |           |          |               |              |              |             |            |           | 0         | 0                  | []            |
| 2  | 1       | 1         | 1     | 4           | 35         | 713      | Main Index                        | Main Index                        |              |           |          |               |              |              |             |            |           | 0         | 0                  | []            |
| 3  | 1       | 2         | 2     | 10          | 109        | 331      | Same-file targets                 | Same-file targets                 |              |           |          |               |              |              |             |            |           | 0         | 0                  | []            |
| 4  | 1       | 2         | 2     | 20          | 331        | 438      | Brackets in Title [2026-07-03 Fr] | Brackets in Title [2026-07-03 Fr] |              |           |          |               |              |              |             |            |           | 0         | 0                  | []            |
| 5  | 1       | 2         | 2     | 26          | 438        | 596      | Duplicate Custom                  | Duplicate Custom                  |              |           |          |               |              |              |             |            |           | 0         | 0                  | []            |
| 6  | 1       | 2         | 2     | 33          | 596        | 713      | Duplicate Custom Later            | Duplicate Custom Later            |              |           |          |               |              |              |             |            |           | 0         | 0                  | []            |
| 7  | 1       | 1         | 1     | 40          | 713        | 1041     | File target links                 | File target links                 |              |           |          |               |              |              |             |            |           | 0         | 0                  | []            |
| 8  | 1       | 1         | 1     | 60          | 1041       | 1356     | File heading-title links          | File heading-title links          |              |           |          |               |              |              |             |            |           | 0         | 0                  | []            |
| 9  | 1       | 1         | 1     | 70          | 1356       | 1515     | Same-file star heading links      | Same-file star heading links      |              |           |          |               |              |              |             |            |           | 0         | 0                  | []            |
| 10 | 1       | 1         | 1     | 77          | 1515       | 1860     | Same-file CUSTOM_ID links         | Same-file CUSTOM_ID links         |              |           |          |               |              |              |             |            |           | 0         | 0                  | []            |

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
| 1  | 1       | 7          | 757        | 776      | 44   | normal         | bracket | [[file:target.org]]                  | file:target.org                  |                 | file      | target.org              |                 | /home/hubisan/projects/coding/org-files-db/.project/manual-tests/files/link-file-resolve/notes/target.org            | 4              | 18                |                  |           | resolved          |                             |
| 2  | 1       | 7          | 779        | 801      | 45   | normal         | bracket | [[file:sub/child.org]]               | file:sub/child.org               |                 | file      | sub/child.org           |                 | /home/hubisan/projects/coding/org-files-db/.project/manual-tests/files/link-file-resolve/notes/sub/child.org         | 3              | 16                |                  |           | resolved          |                             |
| 3  | 1       | 7          | 804        | 827      | 46   | normal         | bracket | [[file:other/peer.org]]              | file:other/peer.org              |                 | file      | other/peer.org          |                 | /home/hubisan/projects/coding/org-files-db/.project/manual-tests/files/link-file-resolve/notes/other/peer.org        | 2              | 13                |                  |           | resolved          |                             |
| 4  | 1       | 7          | 830        | 846      | 47   | normal         | bracket | [[./target.org]]                     | ./target.org                     |                 | file      | ./target.org            |                 | /home/hubisan/projects/coding/org-files-db/.project/manual-tests/files/link-file-resolve/notes/target.org            | 4              | 18                |                  |           | resolved          |                             |
| 5  | 1       | 7          | 879        | 896      | 51   | normal         | angle   | <file:target.org>                    | file:target.org                  |                 | file      | target.org              |                 | /home/hubisan/projects/coding/org-files-db/.project/manual-tests/files/link-file-resolve/notes/target.org            | 4              | 18                |                  |           | resolved          |                             |
| 6  | 1       | 7          | 899        | 914      | 52   | normal         | plain   | file:target.org                      | file:target.org                  |                 | file      | target.org              |                 | /home/hubisan/projects/coding/org-files-db/.project/manual-tests/files/link-file-resolve/notes/target.org            | 4              | 18                |                  |           | resolved          |                             |
| 7  | 1       | 7          | 951        | 971      | 56   | normal         | bracket | [[file:missing.org]]                 | file:missing.org                 |                 | file      | missing.org             |                 | /home/hubisan/projects/coding/org-files-db/.project/manual-tests/files/link-file-resolve/notes/missing.org           |                |                   |                  |           | broken            | missing in indexed universe |
| 8  | 1       | 7          | 974        | 1004     | 57   | normal         | bracket | [[file:sub/missing-child.org]]       | file:sub/missing-child.org       |                 | file      | sub/missing-child.org   |                 | /home/hubisan/projects/coding/org-files-db/.project/manual-tests/files/link-file-resolve/notes/sub/missing-child.org |                |                   |                  |           | broken            | missing in indexed universe |
| 9  | 1       | 7          | 1007       | 1039     | 58   | normal         | bracket | [[file:../external/outside.org]]     | file:../external/outside.org     |                 | file      | ../external/outside.org |                 | /home/hubisan/projects/coding/org-files-db/.project/manual-tests/files/link-file-resolve/external/outside.org        |                |                   |                  |           | unresolved        | outside indexed universe    |
| 10 | 1       | 8          | 1071       | 1107     | 62   | normal         | bracket | [[file:target.org::*Target Heading]] | file:target.org::*Target Heading |                 | file      | target.org              | *Target Heading | /home/hubisan/projects/coding/org-files-db/.project/manual-tests/files/link-file-resolve/notes/target.org            | 4              | 19                |                  |           | resolved          |                             |

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

| id | heading_id |    key    |           value           |     source      | append | line_number |
|----|------------|-----------|---------------------------|-----------------|--------|-------------|
| 1  | 2          | CUSTOM_ID | main-index                | property_drawer | 0      | 6           |
| 2  | 2          | ID        | index-id-001              | property_drawer | 0      | 7           |
| 3  | 3          | CUSTOM_ID | same-file-target          | property_drawer | 0      | 12          |
| 4  | 3          | CUSTOM_ID |  whitespace-in-custom-id  | property_drawer | 0      | 13          |
| 5  | 3          | ID        |  id-with-whitespace       | property_drawer | 0      | 14          |
| 6  | 3          | ID        | same-file-id-001          | property_drawer | 0      | 15          |
| 7  | 4          | CUSTOM_ID | brackets-heading          | property_drawer | 0      | 22          |
| 8  | 4          | ID        | brackets-id-001           | property_drawer | 0      | 23          |
| 9  | 5          | CUSTOM_ID | duplicate-custom          | property_drawer | 0      | 28          |
| 10 | 6          | CUSTOM_ID | DUPLICATE-CUSTOM          | property_drawer | 0      | 35          |

## tags


## timestamp_repeaters


## timestamps

| id | heading_id | role |  start_ts  | end_ts |   type   | range_type |    raw_value    | byte_start | byte_end | line_number |
|----|------------|------|------------|--------|----------|------------|-----------------|------------|----------|-------------|
| 1  | 4          | body | 1783036800 |        | inactive | none       | [2026-07-03 Fr] | 352        | 367      | 20          |

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
