# SQLite DB preview

## files

| id |                                                 path                                                 |      mtime_ns       | size | content_hash | indexed_at |
|----|------------------------------------------------------------------------------------------------------|---------------------|------|--------------|------------|
| 1  | /home/hubisan/projects/coding/org-files-db/.project/manual-tests/files/angle-links.org               | 1782831514259839578 | 2491 |              | 1782848361 |
| 2  | /home/hubisan/projects/coding/org-files-db/.project/manual-tests/files/bracket-links--weird-ones.org | 1782827941687352485 | 1591 |              | 1782848361 |
| 3  | /home/hubisan/projects/coding/org-files-db/.project/manual-tests/files/bracket-links.org             | 1782811598000160725 | 3503 |              | 1782848361 |
| 4  | /home/hubisan/projects/coding/org-files-db/.project/manual-tests/files/file-local-todo-keywords.org  | 1782810515620385647 | 1188 |              | 1782848361 |
| 5  | /home/hubisan/projects/coding/org-files-db/.project/manual-tests/files/keywords.org                  | 1782237960833303823 | 2276 |              | 1782848361 |
| 6  | /home/hubisan/projects/coding/org-files-db/.project/manual-tests/files/multipe-title-keywords.org    | 1781809982908544855 | 321  |              | 1782848361 |
| 7  | /home/hubisan/projects/coding/org-files-db/.project/manual-tests/files/plain-links.org               | 1782848059897181453 | 7513 |              | 1782848361 |
| 8  | /home/hubisan/projects/coding/org-files-db/.project/manual-tests/files/planning-lines.org            | 1782129087807207205 | 1163 |              | 1782848361 |
| 9  | /home/hubisan/projects/coding/org-files-db/.project/manual-tests/files/properties.org                | 1782218329407221851 | 2783 |              | 1782848361 |
| 10 | /home/hubisan/projects/coding/org-files-db/.project/manual-tests/files/tags.org                      | 1782243198853649058 | 1307 |              | 1782848361 |

## heading_bodies

| heading_id |                                  body_text                                   | body_byte_start | body_byte_end |
|------------|------------------------------------------------------------------------------|-----------------|---------------|
| 1          | # Angle links before the first real heading should attach to synthetic root. | 48              | 256           |
|            | <FILE:root-angle.org::42>                                                    |                 |               |
|            | <root:target with spaces>                                                    |                 |               |
|            | [[root:target with spaces]]                                                  |                 |               |
|            | https://example.org/root-plain-should-not-be-stored                          |                 |               |
| 3          | - <https://example.com/some path with spaces>                                | 307             | 445           |
|            | - <https://example.org/ spaces >                                             |                 |               |
|            | - <info:org#External Link>                                                   |                 |               |
|            | - <mailto:emacs-orgmode@gnu.org>                                             |                 |               |
| 4          | - <file:~/code/main.c::255>                                                  | 492             | 662           |
|            | - <file:~/xx.org::*My Target>                                                |                 |               |
|            | - <file:~/xx.org::#my-custom-id>                                             |                 |               |
|            | - <file:~/xx.org::/regexp/>                                                  |                 |               |
|            | - <file:::find me>                                                           |                 |               |
|            | - <file:::*Current File Heading>                                             |                 |               |
| 5          | - <file+sys:~/code/main.c::255>                                              | 712             | 850           |
|            | - <file+sys:~/xx.org::*My Target>                                            |                 |               |
|            | - <file+emacs:~/code/main.c::255>                                            |                 |               |
|            | - <file+emacs:~/xx.org::#my-custom-id>                                       |                 |               |
| 6          | - <unknown:foo>                                                              | 894             | 994           |
|            | - <jira:ABC-123>                                                             |                 |               |
|            | - <customlink:test>                                                          |                 |               |
|            | - <doi:10.1000/182>                                                          |                 |               |
|            | - <irc:/irc.com/#emacs/bob>                                                  |                 |               |
| 7          | These must be stored as raw source facts only. They must not be executed.    | 1024            | 1160          |
|            |                                                                              |                 |               |
|            | - <shell:ls *.org>                                                           |                 |               |
|            | - <elisp:(find-file "~/.emacs.d/init.el")>                                   |                 |               |
| 8          | These should keep the full path and should not split ~::~ in Phase 3.        | 1208            | 1443          |
|            |                                                                              |                 |               |
|            | - <id:16ccfc6a-11ba-499f-8bc6-41be30daa3c5::10>                              |                 |               |
|            | - <attachment:projects.org::10>                                              |                 |               |
|            | - <docview:papers/last.pdf::12>                                              |                 |               |
|            | - <unknown:file.org::10>                                                     |                 |               |
|            | - <customlink:file.org::10>                                                  |                 |               |
| 9          | Results in an angled link as brackets are escaped:                           | 1456            | 1677          |
|            | [[<https://www.gnu.org>\][Test]]                                             |                 |               |
|            |                                                                              |                 |               |
|            | Or with a new line                                                           |                 |               |
|            | [[<https://www.gnu.org>]                                                     |                 |               |
|            | [Test]]                                                                      |                 |               |
|            |                                                                              |                 |               |
|            | But this should be a bracket link:                                           |                 |               |
|            | [[<https://www.gnu.org>][<https://www.gnu.org>]]                             |                 |               |
| 10         | These should not be stored as angle links.                                   | 1708            | 2042          |
|            |                                                                              |                 |               |
|            | - <https://example.com                                                       |                 |               |
|            | - <unknown:unterminated                                                      |                 |               |
|            | - <file:~/broken.org::10                                                     |                 |               |
|            |                                                                              |                 |               |
|            | This multiline candidate should also be ignored:                             |                 |               |
|            |                                                                              |                 |               |
|            | <https://example.org                                                         |                 |               |
|            |   path with newline>                                                         |                 |               |
|            |                                                                              |                 |               |
|            | Garbage before a later valid angle link:                                     |                 |               |
|            | <broken                                                                      |                 |               |
|            | <https://example.org/later-valid>                                            |                 |               |
|            | <broken <https://example.org/later-valid>                                    |                 |               |
| 11         | These should remain out of scope until plain-link storage is implemented.    | 2084            | 2244          |
|            |                                                                              |                 |               |
|            | - https://example.org/plain                                                  |                 |               |
|            | - file:~/code/main.c::255                                                    |                 |               |
|            | - shell:ls *.org                                                             |                 |               |
|            | - jira:ABC-123                                                               |                 |               |

## headings

| id | file_id | parent_id | level | line_number | byte_start | byte_end |                    title                     |                  title_raw                   | todo_keyword | todo_type | priority | scheduled_raw | scheduled_ts | deadline_raw | deadline_ts | closed_raw | closed_ts | archivedp | footnote_section_p | all_tags_json |
|----|---------|-----------|-------|-------------|------------|----------|----------------------------------------------|----------------------------------------------|--------------|-----------|----------|---------------|--------------|--------------|-------------|------------|-----------|-----------|--------------------|---------------|
| 1  | 1       |           | 0     | 1           | -1         | 2491     | Angle Link Fixture                           | Angle Link Fixture                           |              |           |          |               |              |              |             |            |           | 0         | 0                  | []            |
| 2  | 1       | 1         | 1     | 10          | 258        | 2246     | Angle links                                  | Angle links                                  |              |           |          |               |              |              |             |            |           | 0         | 0                  | []            |
| 3  | 1       | 2         | 2     | 13          | 274        | 447      | Basic angle links with spaces                | Basic angle links with spaces                |              |           |          |               |              |              |             |            |           | 0         | 0                  | []            |
| 4  | 1       | 2         | 2     | 19          | 447        | 664      | File-like angle links with search options    | File-like angle links with search options    |              |           |          |               |              |              |             |            |           | 0         | 0                  | []            |
| 5  | 1       | 2         | 2     | 27          | 664        | 852      | File variant angle links with search options | File variant angle links with search options |              |           |          |               |              |              |             |            |           | 0         | 0                  | []            |
| 6  | 1       | 2         | 2     | 33          | 852        | 996      | Unknown and custom-looking angle links       | Unknown and custom-looking angle links       |              |           |          |               |              |              |             |            |           | 0         | 0                  | []            |
| 7  | 1       | 2         | 2     | 40          | 996        | 1162     | Action-like angle links                      | Action-like angle links                      |              |           |          |               |              |              |             |            |           | 0         | 0                  | []            |
| 8  | 1       | 2         | 2     | 47          | 1162       | 1445     | Non-file-like search-option-looking paths    | Non-file-like search-option-looking paths    |              |           |          |               |              |              |             |            |           | 0         | 0                  | []            |
| 9  | 1       | 2         | 2     | 57          | 1445       | 1679     | Special                                      | Special                                      |              |           |          |               |              |              |             |            |           | 0         | 0                  | []            |
| 10 | 1       | 2         | 2     | 68          | 1679       | 2044     | Invalid angle candidates                     | Invalid angle candidates                     |              |           |          |               |              |              |             |            |           | 0         | 0                  | []            |

## keywords

| id | heading_id | keyword |               value               | line_number |
|----|------------|---------|-----------------------------------|-------------|
| 1  | 1          | TITLE   | Angle Link Fixture                | 1           |
| 2  | 1          | STARTUP | content                           | 2           |
| 3  | 14         | TITLE   | Bracket Link Fixture              | 1           |
| 4  | 14         | STARTUP | content                           | 2           |
| 5  | 15         | TITLE   | Bracket Link Fixture              | 1           |
| 6  | 15         | STARTUP | content                           | 2           |
| 7  | 33         | TITLE   | File-local TODO keywords          | 1           |
| 8  | 33         | STARTUP | showall                           | 2           |
| 9  | 33         | TODO    | one(t) two(n) | three(d) four(w@) | 3           |
| 10 | 33         | TODO    | FIVE SIX |                        | 4           |

## links

| id | file_id | heading_id | byte_start | byte_end | line | source_context | format  |                         raw                         |                     raw_target                      | raw_description | link_type |                     path                      | search_option | path_absolute | target_file_id | target_heading_id | target_custom_id | target_id |
|----|---------|------------|------------|----------|------|----------------|---------|-----------------------------------------------------|-----------------------------------------------------|-----------------|-----------|-----------------------------------------------|---------------|---------------|----------------|-------------------|------------------|-----------|
| 1  | 1       | 1          | 125        | 150      | 5    | normal         | angle   | <FILE:root-angle.org::42>                           | FILE:root-angle.org::42                             |                 | file      | root-angle.org                                | 42            |               |                |                   |                  |           |
| 2  | 1       | 1          | 151        | 176      | 6    | normal         | angle   | <root:target with spaces>                           | root:target with spaces                             |                 | root      | target with spaces                            |               |               |                |                   |                  |           |
| 3  | 1       | 1          | 177        | 204      | 7    | normal         | bracket | [[root:target with spaces]]                         | root:target with spaces                             |                 | root      | target with spaces                            |               |               |                |                   |                  |           |
| 4  | 1       | 1          | 205        | 256      | 8    | normal         | plain   | https://example.org/root-plain-should-not-be-stored | https://example.org/root-plain-should-not-be-stored |                 | https     | //example.org/root-plain-should-not-be-stored |               |               |                |                   |                  |           |
| 5  | 1       | 3          | 309        | 352      | 14   | normal         | angle   | <https://example.com/some path with spaces>         | https://example.com/some path with spaces           |                 | https     | //example.com/some path with spaces           |               |               |                |                   |                  |           |
| 6  | 1       | 3          | 355        | 385      | 15   | normal         | angle   | <https://example.org/ spaces >                      | https://example.org/ spaces                         |                 | https     | //example.org/ spaces                         |               |               |                |                   |                  |           |
| 7  | 1       | 3          | 388        | 412      | 16   | normal         | angle   | <info:org#External Link>                            | info:org#External Link                              |                 | info      | org#External Link                             |               |               |                |                   |                  |           |
| 8  | 1       | 3          | 415        | 445      | 17   | normal         | angle   | <mailto:emacs-orgmode@gnu.org>                      | mailto:emacs-orgmode@gnu.org                        |                 | mailto    | emacs-orgmode@gnu.org                         |               |               |                |                   |                  |           |
| 9  | 1       | 4          | 494        | 519      | 20   | normal         | angle   | <file:~/code/main.c::255>                           | file:~/code/main.c::255                             |                 | file      | ~/code/main.c                                 | 255           |               |                |                   |                  |           |
| 10 | 1       | 4          | 522        | 549      | 21   | normal         | angle   | <file:~/xx.org::*My Target>                         | file:~/xx.org::*My Target                           |                 | file      | ~/xx.org                                      | *My Target    |               |                |                   |                  |           |

## outline_path

| heading_id | file_id | parent_id | depth | materialized_path |                                  breadcrumbs_json                                   |
|------------|---------|-----------|-------|-------------------|-------------------------------------------------------------------------------------|
| 1          | 1       |           | 0     | 0000              | ["Angle Link Fixture"]                                                              |
| 2          | 1       | 1         | 1     | 0000.0001         | ["Angle Link Fixture","Angle links"]                                                |
| 3          | 1       | 2         | 2     | 0000.0001.0001    | ["Angle Link Fixture","Angle links","Basic angle links with spaces"]                |
| 4          | 1       | 2         | 2     | 0000.0001.0002    | ["Angle Link Fixture","Angle links","File-like angle links with search options"]    |
| 5          | 1       | 2         | 2     | 0000.0001.0003    | ["Angle Link Fixture","Angle links","File variant angle links with search options"] |
| 6          | 1       | 2         | 2     | 0000.0001.0004    | ["Angle Link Fixture","Angle links","Unknown and custom-looking angle links"]       |
| 7          | 1       | 2         | 2     | 0000.0001.0005    | ["Angle Link Fixture","Angle links","Action-like angle links"]                      |
| 8          | 1       | 2         | 2     | 0000.0001.0006    | ["Angle Link Fixture","Angle links","Non-file-like search-option-looking paths"]    |
| 9          | 1       | 2         | 2     | 0000.0001.0007    | ["Angle Link Fixture","Angle links","Special"]                                      |
| 10         | 1       | 2         | 2     | 0000.0001.0008    | ["Angle Link Fixture","Angle links","Invalid angle candidates"]                     |

## properties

| id | heading_id |      key       |                value                 |      source      | append | line_number |
|----|------------|----------------|--------------------------------------|------------------|--------|-------------|
| 1  | 16         | CUSTOM_ID      | internal-link-to-custom-id           | property_drawer  | 0      | 12          |
| 2  | 51         | BEFORE_PROP    | before-value                         | property_keyword | 0      | 4           |
| 3  | 51         | CATEGORY       | before-category                      | category_keyword | 0      | 5           |
| 4  | 51         | AFTER_PROP     | after-value                          | property_keyword | 0      | 12          |
| 5  | 51         | REPEATED_PROP  | first                                | property_keyword | 0      | 13          |
| 6  | 51         | REPEATED_PROP  | second                               | property_keyword | 0      | 14          |
| 7  | 51         | APPENDED_PROP  | base                                 | property_keyword | 0      | 15          |
| 8  | 51         | APPENDED_PROP  | extra                                | property_keyword | 1      | 16          |
| 9  | 51         | CATEGORY       | after-category                       | category_keyword | 0      | 17          |
| 10 | 59         | LINK_TO_IGNORE | https://example.org/property-keyword | property_keyword | 0      | 251         |

## tags

| heading_id |   tag   |
|------------|---------|
| 13         | foo     |
| 13         | bar     |
| 32         | foo     |
| 32         | bar     |
| 77         | foo     |
| 77         | bar     |
| 96         | project |
| 96         | work    |
| 106        | file    |
| 106        | project |

## timestamp_repeaters

| id | timestamp_id | repeater_type | repeater_value | repeater_unit | repeater_deadline_value | repeater_deadline_unit | warning_type | warning_value | warning_unit |
|----|--------------|---------------|----------------|---------------|-------------------------|------------------------|--------------|---------------|--------------|
| 1  | 10           | cumulate      | 1              | week          |                         |                        |              |               |              |
| 2  | 22           | cumulate      | 1              | week          |                         |                        |              |               |              |
| 3  | 23           | catch_up      | 1              | month         |                         |                        |              |               |              |
| 4  | 24           | restart       | 2              | day           |                         |                        |              |               |              |
| 5  | 25           | cumulate      | 3              | hour          |                         |                        |              |               |              |
| 6  | 26           | cumulate      | 3              | day           |                         |                        |              |               |              |
| 7  | 27           | cumulate      | 3              | week          |                         |                        |              |               |              |
| 8  | 28           | cumulate      | 3              | month         |                         |                        |              |               |              |
| 9  | 29           | cumulate      | 3              | year          |                         |                        |              |               |              |
| 10 | 30           | cumulate      | 1              | week          | 2                       | day                    |              |               |              |

## timestamps

| id | heading_id |   role    |  start_ts  |   end_ts   |   type   | range_type |             raw_value              | byte_start | byte_end | line_number |
|----|------------|-----------|------------|------------|----------|------------|------------------------------------|------------|----------|-------------|
| 1  | 80         | scheduled | 1732060800 |            | active   | none       | <2024-11-20 Wed>                   | 93         | 109      | 7           |
| 2  | 81         | deadline  | 1733011200 |            | active   | none       | <2024-12-01 Sun>                   | 140        | 156      | 10          |
| 3  | 82         | closed    | 1732147200 |            | inactive | none       | [2024-11-21 Thu]                   | 183        | 199      | 13          |
| 4  | 83         | deadline  | 1733011200 |            | active   | none       | <2024-12-01 Sun>                   | 239        | 255      | 16          |
| 5  | 83         | scheduled | 1732060800 |            | active   | none       | <2024-11-20 Wed>                   | 267        | 283      | 16          |
| 6  | 83         | closed    | 1732147200 |            | inactive | none       | [2024-11-21 Thu]                   | 292        | 308      | 16          |
| 7  | 84         | scheduled | 1732095000 |            | active   | none       | <2024-11-20 Wed 09:30>             | 334        | 356      | 19          |
| 8  | 85         | scheduled | 1732095000 | 1732100400 | active   | time_range | <2024-11-20 Wed 09:30-11:00>       | 392        | 420      | 22          |
| 9  | 86         | deadline  | 1733011200 | 1733184000 | active   | date_range | <2024-12-01 Sun>--<2024-12-03 Tue> | 446        | 480      | 25          |
| 10 | 87         | scheduled | 1732060800 |            | active   | none       | <2024-11-20 Wed +1w>               | 505        | 525      | 28          |

## todo_keywords

| file_id | keyword | state_type | shortcut | sequence_no |  source_kind   | source_keyword | source_line_number |
|---------|---------|------------|----------|-------------|----------------|----------------|--------------------|
| 1       | TODO    | open       |          | 0           | config_default |                |                    |
| 1       | DONE    | closed     |          | 1           | config_default |                |                    |
| 2       | TODO    | open       |          | 0           | config_default |                |                    |
| 2       | DONE    | closed     |          | 1           | config_default |                |                    |
| 3       | TODO    | open       |          | 0           | config_default |                |                    |
| 3       | DONE    | closed     |          | 1           | config_default |                |                    |
| 4       | one     | open       | t        | 0           | org_keyword    | TODO           | 3                  |
| 4       | two     | open       | n        | 1           | org_keyword    | TODO           | 3                  |
| 4       | FIVE    | open       |          | 2           | org_keyword    | TODO           | 4                  |
| 4       | SIX     | open       |          | 3           | org_keyword    | TODO           | 4                  |

<!--
Local Variables:
eval: (visual-fill-column-mode -1)
truncate-lines: t
End:
-->
