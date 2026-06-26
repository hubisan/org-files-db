# SQLite DB preview

## files

| id |                                                                 path                                                                  |      mtime_ns       | size | content_hash | indexed_at |
|----|---------------------------------------------------------------------------------------------------------------------------------------|---------------------|------|--------------|------------|
| 1  | /home/hubisan/projects/coding/org-files-db/.project/manual-tests/files/dir-locals-test-structure/README.org                           | 1782504536000000000 | 860  |              | 1782512270 |
| 2  | /home/hubisan/projects/coding/org-files-db/.project/manual-tests/files/dir-locals-test-structure/notes/child/deepest-wins-test.org    | 1782504536000000000 | 286  |              | 1782512270 |
| 3  | /home/hubisan/projects/coding/org-files-db/.project/manual-tests/files/dir-locals-test-structure/notes/override/org-override-test.org | 1782504536000000000 | 374  |              | 1782512270 |
| 4  | /home/hubisan/projects/coding/org-files-db/.project/manual-tests/files/dir-locals-test-structure/notes/root-test.org                  | 1782504536000000000 | 277  |              | 1782512270 |
| 5  | /home/hubisan/projects/coding/org-files-db/.project/manual-tests/files/dir-locals-test-structure/notes/unsafe/unsafe-warn-test.org    | 1782504536000000000 | 316  |              | 1782512270 |
| 6  | /home/hubisan/projects/coding/org-files-db/.project/manual-tests/files/file-local-todo-keywords.org                                   | 1781808483955605322 | 1188 |              | 1782512270 |
| 7  | /home/hubisan/projects/coding/org-files-db/.project/manual-tests/files/keywords.org                                                   | 1782237960833303823 | 2276 |              | 1782512270 |
| 8  | /home/hubisan/projects/coding/org-files-db/.project/manual-tests/files/multipe-title-keywords.org                                     | 1781809982908544855 | 321  |              | 1782512270 |
| 9  | /home/hubisan/projects/coding/org-files-db/.project/manual-tests/files/planning-lines.org                                             | 1782129087807207205 | 1163 |              | 1782512270 |
| 10 | /home/hubisan/projects/coding/org-files-db/.project/manual-tests/files/properties.org                                                 | 1782218329407221851 | 2783 |              | 1782512270 |

## heading_bodies

| heading_id |                                                                                           body_text                                                                                           | body_byte_start | body_byte_end |
|------------|-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|-----------------|---------------|
| 2          | Copy this directory somewhere inside or next to your repo and run your rebuild command with:                                                                                                  | 50              | 859           |
|            |                                                                                                                                                                                               |                 |               |
|            | #+begin_src sh                                                                                                                                                                                |                 |               |
|            | org-files-db rebuild --config Config.toml                                                                                                                                                     |                 |               |
|            | #+end_src                                                                                                                                                                                     |                 |               |
|            |                                                                                                                                                                                               |                 |               |
|            | Expected behavior:                                                                                                                                                                            |                 |               |
|            |                                                                                                                                                                                               |                 |               |
|            | - ~notes/root-test.org~ uses ~notes/.dir-locals.el~.                                                                                                                                          |                 |               |
|            | - ~notes/child/deepest-wins-test.org~ uses ~notes/child/.dir-locals.el~, because deepest match wins.                                                                                          |                 |               |
|            | - ~notes/override/org-override-test.org~ uses its in-buffer ~#+TODO~ and overrides ~.dir-locals.el~.                                                                                          |                 |               |
|            | - ~notes/unsafe/unsafe-warn-test.org~ should produce a warning and fall back to config TODO defaults.                                                                                         |                 |               |
|            |                                                                                                                                                                                               |                 |               |
|            | Toggle tests:                                                                                                                                                                                 |                 |               |
|            |                                                                                                                                                                                               |                 |               |
|            | - Set ~inherit = false~ to check that parent ~.dir-locals.el~ no longer applies to child directories.                                                                                         |                 |               |
|            | - Set ~enabled = false~ to check that only config defaults are used.                                                                                                                          |                 |               |
|            | - Set ~unsupported = "error"~ to check that unsafe reader syntax fails the rebuild.                                                                                                           |                 |               |
| 24         | See [[file:../../notes/org-semantics/file-local-todo-keywords.org]]                                                                                                                           | 164             | 231           |
| 25         | Default TODO is not valid because file-local TODO lines override defaults.                                                                                                                    | 278             | 352           |
| 26         | Default DONE is not valid because file-local TODO lines override defaults.                                                                                                                    | 403             | 477           |
| 43         | This heading has body text before later keywords.                                                                                                                                             | 163             | 212           |
| 44         | This child should not directly receive keyword rows.                                                                                                                                          | 471             | 523           |
| 45         | This heading appears after later keywords.                                                                                                                                                    | 626             | 668           |
| 46         | This line mentions #+TITLE: Inline Mention but should only become a keyword row if Orgize exposes it as a keyword node.                                                                       | 836             | 1262          |
|            | This line mentions #+PROPERTY: inline_prop invalid in prose.                                                                                                                                  |                 |               |
|            |                                                                                                                                                                                               |                 |               |
|            | #+BEGIN_EXAMPLE                                                                                                                                                                               |                 |               |
|            | #+TITLE: Example Block Title                                                                                                                                                                  |                 |               |
|            | #+PROPERTY: example_prop invalid                                                                                                                                                              |                 |               |
|            | #+CATEGORY: example-category                                                                                                                                                                  |                 |               |
|            | #+END_EXAMPLE                                                                                                                                                                                 |                 |               |
|            |                                                                                                                                                                                               |                 |               |
|            | #+begin_src org                                                                                                                                                                               |                 |               |
|            |   ,#+TITLE: Source Block Title                                                                                                                                                                |                 |               |
|            |   ,#+PROPERTY: source_prop invalid                                                                                                                                                            |                 |               |
|            |   ,#+CATEGORY: source-category                                                                                                                                                                |                 |               |
|            | #+end_src                                                                                                                                                                                     |                 |               |
| 47         | - All real keyword nodes exposed by Orgize are stored as raw ~keywords~ rows attached to the level 0 heading.                                                                                 | 1285            | 2275          |
|            | - Keyword rows are not attached to regular headings.                                                                                                                                          |                 |               |
|            | - Duplicate keyword rows are preserved.                                                                                                                                                       |                 |               |
|            | - Source order is preserved with ~line_number~ and/or insertion order.                                                                                                                        |                 |               |
|            | - Generic keywords such as ~TITLE~, ~AUTHOR~, ~STARTUP~, ~OPTIONS~, and ~EXPORT_FILE_NAME~ remain raw keyword rows only.                                                                      |                 |               |
|            | - ~TODO~, ~SEQ_TODO~, and ~TYP_TODO~ may additionally create normalized ~todo_keywords~ rows if that normalization is in scope.                                                               |                 |               |
|            | - ~PROPERTY~ rows may additionally create normalized ~properties~ rows with ~source = property_keyword~ if that normalization is in scope.                                                    |                 |               |
|            | - ~CATEGORY~ rows may additionally create normalized ~properties~ rows with ~source = category_keyword~ if that normalization is in scope.                                                    |                 |               |
|            | - Keywords inside example/source blocks must not create keyword rows unless Orgize incorrectly exposes them as keyword nodes; if that happens, document the Orgize behavior as a parser risk. |                 |               |
| 48         | See [[file:../../notes/org-semantics/multipe-title-keywords.org]]                                                                                                                             | 69              | 134           |

## headings

| id | file_id | parent_id | level | line_number | byte_start | byte_end |                              title                              |                            title_raw                            | todo_keyword | todo_type | priority | scheduled_raw | scheduled_ts | deadline_raw | deadline_ts | closed_raw | closed_ts | archivedp | footnote_section_p | all_tags_json |
|----|---------|-----------|-------|-------------|------------|----------|-----------------------------------------------------------------|-----------------------------------------------------------------|--------------|-----------|----------|---------------|--------------|--------------|-------------|------------|-----------|-----------|--------------------|---------------|
| 1  | 1       |           | 0     | 1           | -1         | 860      | dir-locals test structure                                       | dir-locals test structure                                       |              |           |          |               |              |              |             |            |           | 0         | 0                  | []            |
| 2  | 1       | 1         | 1     | 3           | 36         | 860      | How to use                                                      | How to use                                                      |              |           |          |               |              |              |             |            |           | 0         | 0                  | []            |
| 3  | 2       |           | 0     | 1           | -1         | 286      | Deepest .dir-locals wins test                                   | Deepest .dir-locals wins test                                   |              |           |          |               |              |              |             |            |           | 0         | 0                  | []            |
| 4  | 2       | 3         | 1     | 3           | 40         | 94       | Should be open from notes/child/.dir-locals.el                  | Should be open from notes/child/.dir-locals.el                  | NEXT         | open      |          |               |              |              |             |            |           | 0         | 0                  | []            |
| 5  | 2       | 3         | 1     | 4           | 94         | 154      | Should be closed from notes/child/.dir-locals.el                | Should be closed from notes/child/.dir-locals.el                | FINISHED     | closed    |          |               |              |              |             |            |           | 0         | 0                  | []            |
| 6  | 2       | 3         | 1     | 5           | 154        | 220      | PLAN Should NOT be recognized because child .dir-locals.el wins | PLAN Should NOT be recognized because child .dir-locals.el wins |              |           |          |               |              |              |             |            |           | 0         | 0                  | []            |
| 7  | 2       | 3         | 1     | 6           | 220        | 286      | DONE Should NOT be recognized because child .dir-locals.el wins | DONE Should NOT be recognized because child .dir-locals.el wins |              |           |          |               |              |              |             |            |           | 0         | 0                  | []            |
| 8  | 3       |           | 0     | 1           | -1         | 374      | Org in-buffer TODO override test                                | Org in-buffer TODO override test                                |              |           |          |               |              |              |             |            |           | 0         | 0                  | []            |
| 9  | 3       | 8         | 1     | 4           | 84         | 130      | Should be open from in-buffer #+TODO                            | Should be open from in-buffer #+TODO                            | REVIEW       | open      |          |               |              |              |             |            |           | 0         | 0                  | []            |
| 10 | 3       | 8         | 1     | 5           | 130        | 182      | Should also be open from in-buffer #+TODO                       | Should also be open from in-buffer #+TODO                       | BLOCKED      | open      |          |               |              |              |             |            |           | 0         | 0                  | []            |

## keywords

| id | heading_id | keyword |               value               | line_number |
|----|------------|---------|-----------------------------------|-------------|
| 1  | 1          | TITLE   | dir-locals test structure         | 1           |
| 2  | 3          | TITLE   | Deepest .dir-locals wins test     | 1           |
| 3  | 8          | TITLE   | Org in-buffer TODO override test  | 1           |
| 4  | 8          | TODO    | REVIEW(r) BLOCKED(b) | CLOSED(c)  | 2           |
| 5  | 14         | TITLE   | Root dir-locals test              | 1           |
| 6  | 20         | TITLE   | Unsafe syntax warning test        | 1           |
| 7  | 24         | TITLE   | File-local TODO keywords          | 1           |
| 8  | 24         | STARTUP | showall                           | 2           |
| 9  | 24         | TODO    | one(t) two(n) | three(d) four(w@) | 3           |
| 10 | 24         | TODO    | FIVE SIX |                        | 4           |

## links


## outline_path

| heading_id | file_id | parent_id | depth | materialized_path |                                          breadcrumbs_json                                           |
|------------|---------|-----------|-------|-------------------|-----------------------------------------------------------------------------------------------------|
| 1          | 1       |           | 0     | 0000              | ["dir-locals test structure"]                                                                       |
| 2          | 1       | 1         | 1     | 0000.0001         | ["dir-locals test structure","How to use"]                                                          |
| 3          | 2       |           | 0     | 0000              | ["Deepest .dir-locals wins test"]                                                                   |
| 4          | 2       | 3         | 1     | 0000.0001         | ["Deepest .dir-locals wins test","Should be open from notes/child/.dir-locals.el"]                  |
| 5          | 2       | 3         | 1     | 0000.0002         | ["Deepest .dir-locals wins test","Should be closed from notes/child/.dir-locals.el"]                |
| 6          | 2       | 3         | 1     | 0000.0003         | ["Deepest .dir-locals wins test","PLAN Should NOT be recognized because child .dir-locals.el wins"] |
| 7          | 2       | 3         | 1     | 0000.0004         | ["Deepest .dir-locals wins test","DONE Should NOT be recognized because child .dir-locals.el wins"] |
| 8          | 3       |           | 0     | 0000              | ["Org in-buffer TODO override test"]                                                                |
| 9          | 3       | 8         | 1     | 0000.0001         | ["Org in-buffer TODO override test","Should be open from in-buffer #+TODO"]                         |
| 10         | 3       | 8         | 1     | 0000.0002         | ["Org in-buffer TODO override test","Should also be open from in-buffer #+TODO"]                    |

## properties

| id | heading_id |      key      |           value           |      source      | append | line_number |
|----|------------|---------------|---------------------------|------------------|--------|-------------|
| 1  | 42         | BEFORE_PROP   | before-value              | property_keyword | 0      | 4           |
| 2  | 42         | CATEGORY      | before-category           | category_keyword | 0      | 5           |
| 3  | 42         | AFTER_PROP    | after-value               | property_keyword | 0      | 12          |
| 4  | 42         | REPEATED_PROP | first                     | property_keyword | 0      | 13          |
| 5  | 42         | REPEATED_PROP | second                    | property_keyword | 0      | 14          |
| 6  | 42         | APPENDED_PROP | base                      | property_keyword | 0      | 15          |
| 7  | 42         | APPENDED_PROP | extra                     | property_keyword | 1      | 16          |
| 8  | 42         | CATEGORY      | after-category            | category_keyword | 0      | 17          |
| 9  | 68         | CATEGORY      | Level 0 Category Property | property_drawer  | 0      | 2           |
| 10 | 68         | WHATEVER      | level 0 drawer property   | property_drawer  | 0      | 3           |

## tags

| heading_id |    tag     |
|------------|------------|
| 68         | project    |
| 68         | work       |
| 78         | file       |
| 78         | project    |
| 78         | later      |
| 78         | extra      |
| 79         | parent     |
| 80         | child      |
| 81         | project    |
| 81         | grandchild |

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
| 1  | 52         | scheduled | 1732060800 |            | active   | none       | <2024-11-20 Wed>                   | 93         | 109      | 7           |
| 2  | 53         | deadline  | 1733011200 |            | active   | none       | <2024-12-01 Sun>                   | 140        | 156      | 10          |
| 3  | 54         | closed    | 1732147200 |            | inactive | none       | [2024-11-21 Thu]                   | 183        | 199      | 13          |
| 4  | 55         | deadline  | 1733011200 |            | active   | none       | <2024-12-01 Sun>                   | 239        | 255      | 16          |
| 5  | 55         | scheduled | 1732060800 |            | active   | none       | <2024-11-20 Wed>                   | 267        | 283      | 16          |
| 6  | 55         | closed    | 1732147200 |            | inactive | none       | [2024-11-21 Thu]                   | 292        | 308      | 16          |
| 7  | 56         | scheduled | 1732095000 |            | active   | none       | <2024-11-20 Wed 09:30>             | 334        | 356      | 19          |
| 8  | 57         | scheduled | 1732095000 | 1732100400 | active   | time_range | <2024-11-20 Wed 09:30-11:00>       | 392        | 420      | 22          |
| 9  | 58         | deadline  | 1733011200 | 1733184000 | active   | date_range | <2024-12-01 Sun>--<2024-12-03 Tue> | 446        | 480      | 25          |
| 10 | 59         | scheduled | 1732060800 |            | active   | none       | <2024-11-20 Wed +1w>               | 505        | 525      | 28          |

## todo_keywords

| file_id | keyword  | state_type | shortcut | sequence_no |  source_kind   | source_keyword | source_line_number |
|---------|----------|------------|----------|-------------|----------------|----------------|--------------------|
| 1       | TODO     | open       |          | 0           | config_default |                |                    |
| 1       | DONE     | closed     |          | 1           | config_default |                |                    |
| 2       | NEXT     | open       | n        | 0           | dir_locals     |                |                    |
| 2       | FINISHED | closed     | f        | 1           | dir_locals     |                |                    |
| 3       | REVIEW   | open       | r        | 0           | org_keyword    | TODO           | 2                  |
| 3       | BLOCKED  | open       | b        | 1           | org_keyword    | TODO           | 2                  |
| 3       | CLOSED   | closed     | c        | 2           | org_keyword    | TODO           | 2                  |
| 4       | PLAN     | open       | p        | 0           | dir_locals     |                |                    |
| 4       | WAIT     | open       | w        | 1           | dir_locals     |                |                    |
| 4       | DONE     | closed     | d        | 2           | dir_locals     |                |                    |

<!--
Local Variables:
eval: (visual-fill-column-mode -1)
truncate-lines: t
End:
-->
