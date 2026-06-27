# SQLite DB preview

## files

| id |                                                                           path                                                                            |      mtime_ns       | size | content_hash | indexed_at |
|----|-----------------------------------------------------------------------------------------------------------------------------------------------------------|---------------------|------|--------------|------------|
| 1  | /home/hubisan/projects/coding/org-files-db/.project/manual-tests/files/dir-locals-test-structure/notes/child/deepest-wins-test.org                        | 1782504536000000000 | 286  |              | 1782552522 |
| 2  | /home/hubisan/projects/coding/org-files-db/.project/manual-tests/files/dir-locals-test-structure/notes/irrelevant-unsupported/eval-next-to-valid-test.org | 1782515075226690860 | 250  |              | 1782552522 |
| 3  | /home/hubisan/projects/coding/org-files-db/.project/manual-tests/files/dir-locals-test-structure/notes/override/org-override-test.org                     | 1782504536000000000 | 374  |              | 1782552522 |
| 4  | /home/hubisan/projects/coding/org-files-db/.project/manual-tests/files/dir-locals-test-structure/notes/root-test.org                                      | 1782504536000000000 | 277  |              | 1782552522 |
| 5  | /home/hubisan/projects/coding/org-files-db/.project/manual-tests/files/dir-locals-test-structure/notes/unsafe/unsafe-warn-test.org                        | 1782504536000000000 | 316  |              | 1782552522 |
| 6  | /home/hubisan/projects/coding/org-files-db/.project/manual-tests/files/file-local-todo-keywords.org                                                       | 1781808483955605322 | 1188 |              | 1782552522 |
| 7  | /home/hubisan/projects/coding/org-files-db/.project/manual-tests/files/keywords.org                                                                       | 1782237960833303823 | 2276 |              | 1782552522 |
| 8  | /home/hubisan/projects/coding/org-files-db/.project/manual-tests/files/multipe-title-keywords.org                                                         | 1781809982908544855 | 321  |              | 1782552522 |
| 9  | /home/hubisan/projects/coding/org-files-db/.project/manual-tests/files/planning-lines.org                                                                 | 1782129087807207205 | 1163 |              | 1782552522 |
| 10 | /home/hubisan/projects/coding/org-files-db/.project/manual-tests/files/properties.org                                                                     | 1782218329407221851 | 2783 |              | 1782552522 |

## heading_bodies

| heading_id |                                                                                           body_text                                                                                           | body_byte_start | body_byte_end |
|------------|-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|-----------------|---------------|
| 26         | See [[file:../../notes/org-semantics/file-local-todo-keywords.org]]                                                                                                                           | 164             | 231           |
| 27         | Default TODO is not valid because file-local TODO lines override defaults.                                                                                                                    | 278             | 352           |
| 28         | Default DONE is not valid because file-local TODO lines override defaults.                                                                                                                    | 403             | 477           |
| 45         | This heading has body text before later keywords.                                                                                                                                             | 163             | 212           |
| 46         | This child should not directly receive keyword rows.                                                                                                                                          | 471             | 523           |
| 47         | This heading appears after later keywords.                                                                                                                                                    | 626             | 668           |
| 48         | This line mentions #+TITLE: Inline Mention but should only become a keyword row if Orgize exposes it as a keyword node.                                                                       | 836             | 1262          |
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
| 49         | - All real keyword nodes exposed by Orgize are stored as raw ~keywords~ rows attached to the level 0 heading.                                                                                 | 1285            | 2275          |
|            | - Keyword rows are not attached to regular headings.                                                                                                                                          |                 |               |
|            | - Duplicate keyword rows are preserved.                                                                                                                                                       |                 |               |
|            | - Source order is preserved with ~line_number~ and/or insertion order.                                                                                                                        |                 |               |
|            | - Generic keywords such as ~TITLE~, ~AUTHOR~, ~STARTUP~, ~OPTIONS~, and ~EXPORT_FILE_NAME~ remain raw keyword rows only.                                                                      |                 |               |
|            | - ~TODO~, ~SEQ_TODO~, and ~TYP_TODO~ may additionally create normalized ~todo_keywords~ rows if that normalization is in scope.                                                               |                 |               |
|            | - ~PROPERTY~ rows may additionally create normalized ~properties~ rows with ~source = property_keyword~ if that normalization is in scope.                                                    |                 |               |
|            | - ~CATEGORY~ rows may additionally create normalized ~properties~ rows with ~source = category_keyword~ if that normalization is in scope.                                                    |                 |               |
|            | - Keywords inside example/source blocks must not create keyword rows unless Orgize incorrectly exposes them as keyword nodes; if that happens, document the Orgize behavior as a parser risk. |                 |               |
| 50         | See [[file:../../notes/org-semantics/multipe-title-keywords.org]]                                                                                                                             | 69              | 134           |
| 51         | This can be proven by using ~org-latex-export-as-latex~:                                                                                                                                      |                 |               |
|            |                                                                                                                                                                                               |                 |               |
|            | #+BEGIN_SRC latex                                                                                                                                                                             |                 |               |
|            |   \title{Title can span multiple lines, even here}                                                                                                                                            |                 |               |
|            | #+END_SRC                                                                                                                                                                                     |                 |               |

## headings

| id | file_id | parent_id | level | line_number | byte_start | byte_end |                              title                              |                            title_raw                            | todo_keyword | todo_type | priority | scheduled_raw | scheduled_ts | deadline_raw | deadline_ts | closed_raw | closed_ts | archivedp | footnote_section_p | all_tags_json |
|----|---------|-----------|-------|-------------|------------|----------|-----------------------------------------------------------------|-----------------------------------------------------------------|--------------|-----------|----------|---------------|--------------|--------------|-------------|------------|-----------|-----------|--------------------|---------------|
| 1  | 1       |           | 0     | 1           | -1         | 286      | Deepest .dir-locals wins test                                   | Deepest .dir-locals wins test                                   |              |           |          |               |              |              |             |            |           | 0         | 0                  | []            |
| 2  | 1       | 1         | 1     | 3           | 40         | 94       | NEXT Should be open from notes/child/.dir-locals.el             | NEXT Should be open from notes/child/.dir-locals.el             |              |           |          |               |              |              |             |            |           | 0         | 0                  | []            |
| 3  | 1       | 1         | 1     | 4           | 94         | 154      | FINISHED Should be closed from notes/child/.dir-locals.el       | FINISHED Should be closed from notes/child/.dir-locals.el       |              |           |          |               |              |              |             |            |           | 0         | 0                  | []            |
| 4  | 1       | 1         | 1     | 5           | 154        | 220      | PLAN Should NOT be recognized because child .dir-locals.el wins | PLAN Should NOT be recognized because child .dir-locals.el wins |              |           |          |               |              |              |             |            |           | 0         | 0                  | []            |
| 5  | 1       | 1         | 1     | 6           | 220        | 286      | Should NOT be recognized because child .dir-locals.el wins      | Should NOT be recognized because child .dir-locals.el wins      | DONE         | closed    |          |               |              |              |             |            |           | 0         | 0                  | []            |
| 6  | 2       |           | 0     | 1           | -1         | 250      | Eval next to valid org-todo-keywords test                       | Eval next to valid org-todo-keywords test                       |              |           |          |               |              |              |             |            |           | 0         | 0                  | []            |
| 7  | 2       | 6         | 1     | 3           | 52         | 116      | PLAN Should be open from valid org-todo-keywords despite eval   | PLAN Should be open from valid org-todo-keywords despite eval   |              |           |          |               |              |              |             |            |           | 0         | 0                  | []            |
| 8  | 2       | 6         | 1     | 4           | 116        | 182      | Should be closed from valid org-todo-keywords despite eval      | Should be closed from valid org-todo-keywords despite eval      | DONE         | closed    |          |               |              |              |             |            |           | 0         | 0                  | []            |
| 9  | 2       | 6         | 1     | 5           | 182        | 250      | Should NOT be recognized because dir-locals TODOs are active    | Should NOT be recognized because dir-locals TODOs are active    | TODO         | open      |          |               |              |              |             |            |           | 0         | 0                  | []            |
| 10 | 3       |           | 0     | 1           | -1         | 374      | Org in-buffer TODO override test                                | Org in-buffer TODO override test                                |              |           |          |               |              |              |             |            |           | 0         | 0                  | []            |

## keywords

| id | heading_id | keyword |                   value                   | line_number |
|----|------------|---------|-------------------------------------------|-------------|
| 1  | 1          | TITLE   | Deepest .dir-locals wins test             | 1           |
| 2  | 6          | TITLE   | Eval next to valid org-todo-keywords test | 1           |
| 3  | 10         | TITLE   | Org in-buffer TODO override test          | 1           |
| 4  | 10         | TODO    | REVIEW(r) BLOCKED(b) | CLOSED(c)          | 2           |
| 5  | 16         | TITLE   | Root dir-locals test                      | 1           |
| 6  | 22         | TITLE   | Unsafe syntax warning test                | 1           |
| 7  | 26         | TITLE   | File-local TODO keywords                  | 1           |
| 8  | 26         | STARTUP | showall                                   | 2           |
| 9  | 26         | TODO    | one(t) two(n) | three(d) four(w@)         | 3           |
| 10 | 26         | TODO    | FIVE SIX |                                | 4           |

## links


## outline_path

| heading_id | file_id | parent_id | depth | materialized_path |                                               breadcrumbs_json                                                |
|------------|---------|-----------|-------|-------------------|---------------------------------------------------------------------------------------------------------------|
| 1          | 1       |           | 0     | 0000              | ["Deepest .dir-locals wins test"]                                                                             |
| 2          | 1       | 1         | 1     | 0000.0001         | ["Deepest .dir-locals wins test","NEXT Should be open from notes/child/.dir-locals.el"]                       |
| 3          | 1       | 1         | 1     | 0000.0002         | ["Deepest .dir-locals wins test","FINISHED Should be closed from notes/child/.dir-locals.el"]                 |
| 4          | 1       | 1         | 1     | 0000.0003         | ["Deepest .dir-locals wins test","PLAN Should NOT be recognized because child .dir-locals.el wins"]           |
| 5          | 1       | 1         | 1     | 0000.0004         | ["Deepest .dir-locals wins test","Should NOT be recognized because child .dir-locals.el wins"]                |
| 6          | 2       |           | 0     | 0000              | ["Eval next to valid org-todo-keywords test"]                                                                 |
| 7          | 2       | 6         | 1     | 0000.0001         | ["Eval next to valid org-todo-keywords test","PLAN Should be open from valid org-todo-keywords despite eval"] |
| 8          | 2       | 6         | 1     | 0000.0002         | ["Eval next to valid org-todo-keywords test","Should be closed from valid org-todo-keywords despite eval"]    |
| 9          | 2       | 6         | 1     | 0000.0003         | ["Eval next to valid org-todo-keywords test","Should NOT be recognized because dir-locals TODOs are active"]  |
| 10         | 3       |           | 0     | 0000              | ["Org in-buffer TODO override test"]                                                                          |

## properties

| id | heading_id |      key      |           value           |      source      | append | line_number |
|----|------------|---------------|---------------------------|------------------|--------|-------------|
| 1  | 44         | BEFORE_PROP   | before-value              | property_keyword | 0      | 4           |
| 2  | 44         | CATEGORY      | before-category           | category_keyword | 0      | 5           |
| 3  | 44         | AFTER_PROP    | after-value               | property_keyword | 0      | 12          |
| 4  | 44         | REPEATED_PROP | first                     | property_keyword | 0      | 13          |
| 5  | 44         | REPEATED_PROP | second                    | property_keyword | 0      | 14          |
| 6  | 44         | APPENDED_PROP | base                      | property_keyword | 0      | 15          |
| 7  | 44         | APPENDED_PROP | extra                     | property_keyword | 1      | 16          |
| 8  | 44         | CATEGORY      | after-category            | category_keyword | 0      | 17          |
| 9  | 70         | CATEGORY      | Level 0 Category Property | property_drawer  | 0      | 2           |
| 10 | 70         | WHATEVER      | level 0 drawer property   | property_drawer  | 0      | 3           |

## tags

| heading_id |    tag     |
|------------|------------|
| 70         | project    |
| 70         | work       |
| 80         | file       |
| 80         | project    |
| 80         | later      |
| 80         | extra      |
| 81         | parent     |
| 82         | child      |
| 83         | project    |
| 83         | grandchild |

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
| 1  | 54         | scheduled | 1732060800 |            | active   | none       | <2024-11-20 Wed>                   | 93         | 109      | 7           |
| 2  | 55         | deadline  | 1733011200 |            | active   | none       | <2024-12-01 Sun>                   | 140        | 156      | 10          |
| 3  | 56         | closed    | 1732147200 |            | inactive | none       | [2024-11-21 Thu]                   | 183        | 199      | 13          |
| 4  | 57         | deadline  | 1733011200 |            | active   | none       | <2024-12-01 Sun>                   | 239        | 255      | 16          |
| 5  | 57         | scheduled | 1732060800 |            | active   | none       | <2024-11-20 Wed>                   | 267        | 283      | 16          |
| 6  | 57         | closed    | 1732147200 |            | inactive | none       | [2024-11-21 Thu]                   | 292        | 308      | 16          |
| 7  | 58         | scheduled | 1732095000 |            | active   | none       | <2024-11-20 Wed 09:30>             | 334        | 356      | 19          |
| 8  | 59         | scheduled | 1732095000 | 1732100400 | active   | time_range | <2024-11-20 Wed 09:30-11:00>       | 392        | 420      | 22          |
| 9  | 60         | deadline  | 1733011200 | 1733184000 | active   | date_range | <2024-12-01 Sun>--<2024-12-03 Tue> | 446        | 480      | 25          |
| 10 | 61         | scheduled | 1732060800 |            | active   | none       | <2024-11-20 Wed +1w>               | 505        | 525      | 28          |

## todo_keywords

| file_id | keyword | state_type | shortcut | sequence_no |  source_kind   | source_keyword | source_line_number |
|---------|---------|------------|----------|-------------|----------------|----------------|--------------------|
| 1       | TODO    | open       |          | 0           | config_default |                |                    |
| 1       | DONE    | closed     |          | 1           | config_default |                |                    |
| 2       | TODO    | open       |          | 0           | config_default |                |                    |
| 2       | DONE    | closed     |          | 1           | config_default |                |                    |
| 3       | REVIEW  | open       | r        | 0           | org_keyword    | TODO           | 2                  |
| 3       | BLOCKED | open       | b        | 1           | org_keyword    | TODO           | 2                  |
| 3       | CLOSED  | closed     | c        | 2           | org_keyword    | TODO           | 2                  |
| 4       | TODO    | open       |          | 0           | config_default |                |                    |
| 4       | DONE    | closed     |          | 1           | config_default |                |                    |
| 5       | TODO    | open       |          | 0           | config_default |                |                    |

<!--
Local Variables:
eval: (visual-fill-column-mode -1)
truncate-lines: t
End:
-->
