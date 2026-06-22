# SQLite DB preview

## files

| id |                                                path                                                 |      mtime_ns       | size | content_hash | indexed_at |
|----|-----------------------------------------------------------------------------------------------------|---------------------|------|--------------|------------|
| 1  | /home/hubisan/projects/coding/org-files-db/.project/manual-tests/files/file-local-todo-keywords.org | 1781808483955605322 | 1188 |              | 1782165208 |
| 2  | /home/hubisan/projects/coding/org-files-db/.project/manual-tests/files/inherited-heading-tags.org   | 1781808923034914634 | 904  |              | 1782165208 |
| 3  | /home/hubisan/projects/coding/org-files-db/.project/manual-tests/files/multipe-title-keywords.org   | 1781809982908544855 | 321  |              | 1782165208 |
| 4  | /home/hubisan/projects/coding/org-files-db/.project/manual-tests/files/planning-lines.org           | 1782129087807207205 | 1163 |              | 1782165208 |
| 5  | /home/hubisan/projects/coding/org-files-db/.project/manual-tests/files/timestamp-repeaters.org      | 1782159459016426918 | 1626 |              | 1782165208 |
| 6  | /home/hubisan/projects/coding/org-files-db/.project/manual-tests/files/timestamps.org               | 1782153027780066037 | 299  |              | 1782165208 |

## heading_bodies


## headings

| id | file_id | parent_id | level | line_number | byte_start | byte_end |                     title                      |                   title_raw                    | todo_keyword | todo_type | priority | scheduled_raw | scheduled_ts | deadline_raw | deadline_ts | closed_raw | closed_ts | archivedp | footnote_section_p | all_tags_json |
|----|---------|-----------|-------|-------------|------------|----------|------------------------------------------------|------------------------------------------------|--------------|-----------|----------|---------------|--------------|--------------|-------------|------------|-----------|-----------|--------------------|---------------|
| 1  | 1       |           | 0     | 1           | -1         | 1188     | File-local TODO keywords                       | File-local TODO keywords                       |              |           |          |               |              |              |             |            |           | 0         | 0                  | []            |
| 2  | 1       | 1         | 1     | 10          | 234        | 354      | TODO default keyword should stay in title      | TODO default keyword should stay in title      |              |           |          |               |              |              |             |            |           | 0         | 0                  | []            |
| 3  | 1       | 1         | 1     | 13          | 354        | 479      | DONE default done keyword should stay in title | DONE default done keyword should stay in title |              |           |          |               |              |              |             |            |           | 0         | 0                  | []            |
| 4  | 1       | 1         | 1     | 16          | 479        | 512      | open keyword with fast key                     | open keyword with fast key                     | one          | open      |          |               |              |              |             |            |           | 0         | 0                  | []            |
| 5  | 1       | 1         | 1     | 17          | 512        | 553      | another open keyword with fast key             | another open keyword with fast key             | two          | open      |          |               |              |              |             |            |           | 0         | 0                  | []            |
| 6  | 1       | 1         | 1     | 18          | 553        | 590      | closed keyword with fast key                   | closed keyword with fast key                   | three        | closed    |          |               |              |              |             |            |           | 0         | 0                  | []            |
| 7  | 1       | 1         | 1     | 19          | 590        | 636      | closed keyword with extended fast key          | closed keyword with extended fast key          | four         | closed    |          |               |              |              |             |            |           | 0         | 0                  | []            |
| 8  | 1       | 1         | 1     | 21          | 636        | 682      | open keyword from empty-done-side line         | open keyword from empty-done-side line         | FIVE         | open      |          |               |              |              |             |            |           | 0         | 0                  | []            |
| 9  | 1       | 1         | 1     | 22          | 682        | 736      | another open keyword from empty-done-side line | another open keyword from empty-done-side line | SIX          | open      |          |               |              |              |             |            |           | 0         | 0                  | []            |
| 10 | 1       | 1         | 1     | 24          | 736        | 771      | open keyword from TYP_TODO                     | open keyword from TYP_TODO                     | seven        | open      |          |               |              |              |             |            |           | 0         | 0                  | []            |

## keywords

| id | heading_id | keyword  |               value               | line_number |
|----|------------|----------|-----------------------------------|-------------|
| 1  | 1          | TITLE    | File-local TODO keywords          |             |
| 2  | 1          | STARTUP  | showall                           |             |
| 3  | 1          | TODO     | one(t) two(n) | three(d) four(w@) |             |
| 4  | 1          | TODO     | FIVE SIX |                        |             |
| 5  | 1          | TYP_TODO | seven | eight                     |             |
| 6  | 1          | SEQ_TODO | nine | ten                        |             |
| 7  | 1          | TODO     | | eleven(c)                       |             |
| 8  | 1          | TODO     | late_open | late_done             |             |
| 9  | 19         | TITLE    | Inherited heading tags            |             |
| 10 | 19         | STARTUP  | showall                           |             |

## links


## outline_path

| heading_id | file_id | parent_id | depth | materialized_path |                               breadcrumbs_json                                |
|------------|---------|-----------|-------|-------------------|-------------------------------------------------------------------------------|
| 1          | 1       |           | 0     | 0000              | ["File-local TODO keywords"]                                                  |
| 2          | 1       | 1         | 1     | 0000.0001         | ["File-local TODO keywords","TODO default keyword should stay in title"]      |
| 3          | 1       | 1         | 1     | 0000.0002         | ["File-local TODO keywords","DONE default done keyword should stay in title"] |
| 4          | 1       | 1         | 1     | 0000.0003         | ["File-local TODO keywords","open keyword with fast key"]                     |
| 5          | 1       | 1         | 1     | 0000.0004         | ["File-local TODO keywords","another open keyword with fast key"]             |
| 6          | 1       | 1         | 1     | 0000.0005         | ["File-local TODO keywords","closed keyword with fast key"]                   |
| 7          | 1       | 1         | 1     | 0000.0006         | ["File-local TODO keywords","closed keyword with extended fast key"]          |
| 8          | 1       | 1         | 1     | 0000.0007         | ["File-local TODO keywords","open keyword from empty-done-side line"]         |
| 9          | 1       | 1         | 1     | 0000.0008         | ["File-local TODO keywords","another open keyword from empty-done-side line"] |
| 10         | 1       | 1         | 1     | 0000.0009         | ["File-local TODO keywords","open keyword from TYP_TODO"]                     |

## properties


## tags

| heading_id |  tag   | inherited |
|------------|--------|-----------|
| 20         | parent | 0         |
| 22         | child  | 0         |
| 24         | parent | 0         |
| 25         | second | 0         |
| 26         | child  | 0         |
| 27         | extra  | 0         |
| 29         | local  | 0         |

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
| 1  | 35         | scheduled | 1732060800 |            | active   | none       | <2024-11-20 Wed>                   | 93         | 109      | 7           |
| 2  | 36         | deadline  | 1733011200 |            | active   | none       | <2024-12-01 Sun>                   | 140        | 156      | 10          |
| 3  | 37         | closed    | 1732147200 |            | inactive | none       | [2024-11-21 Thu]                   | 183        | 199      | 13          |
| 4  | 38         | deadline  | 1733011200 |            | active   | none       | <2024-12-01 Sun>                   | 239        | 255      | 16          |
| 5  | 38         | scheduled | 1732060800 |            | active   | none       | <2024-11-20 Wed>                   | 267        | 283      | 16          |
| 6  | 38         | closed    | 1732147200 |            | inactive | none       | [2024-11-21 Thu]                   | 292        | 308      | 16          |
| 7  | 39         | scheduled | 1732095000 |            | active   | none       | <2024-11-20 Wed 09:30>             | 334        | 356      | 19          |
| 8  | 40         | scheduled | 1732095000 | 1732100400 | active   | time_range | <2024-11-20 Wed 09:30-11:00>       | 392        | 420      | 22          |
| 9  | 41         | deadline  | 1733011200 | 1733184000 | active   | date_range | <2024-12-01 Sun>--<2024-12-03 Tue> | 446        | 480      | 25          |
| 10 | 42         | scheduled | 1732060800 |            | active   | none       | <2024-11-20 Wed +1w>               | 505        | 525      | 28          |

## todo_keywords

| file_id |  keyword  | state_type | shortcut | sequence_no |
|---------|-----------|------------|----------|-------------|
| 1       | one       | open       | t        | 0           |
| 1       | two       | open       | n        | 1           |
| 1       | FIVE      | open       |          | 2           |
| 1       | SIX       | open       |          | 3           |
| 1       | seven     | open       |          | 4           |
| 1       | nine      | open       |          | 5           |
| 1       | late_open | open       |          | 6           |
| 1       | three     | closed     | d        | 7           |
| 1       | four      | closed     | w        | 8           |
| 1       | eight     | closed     |          | 9           |

<!--
Local Variables:
eval: (visual-fill-column-mode -1)
truncate-lines: t
End:
-->
