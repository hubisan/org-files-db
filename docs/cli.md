# CLI Reference

This page documents the user-facing `orgfdb` commands that are stable enough to treat as repository-local reference material.

## `orgfdb headings --json`

`orgfdb headings --json` prints stored heading rows from the SQLite database as JSON. It is a database inspection command: it reads persisted facts only and does not analyze the current contents of Org files.

The command is read-only. It does not scan Org files, rebuild files, resolve targets, or mutate the database.

The output is a JSON array. Synthetic root rows are included by default, and `--no-root` excludes them.

The JSON objects expose the heading fields used by the current CLI contract:

- `id`
- `file_id`
- `file_path`
- `parent_id`
- `level`
- `line_number`
- `byte_start`
- `byte_end`
- `title`
- `title_raw`
- `todo_keyword`
- `todo_type`
- `priority`
- `scheduled_raw`
- `scheduled_ts`
- `deadline_raw`
- `deadline_ts`
- `closed_raw`
- `closed_ts`
- `archivedp`
- `footnote_section_p`
- `all_tags`

Root rows follow the same synthetic file/document conventions as the rest of the repository's CLI and DB contract: they represent file scope, not parser-level Org headings.

If you need the underlying configuration settings for the database path, source file selection, link protocols, TODO keywords, or search options, see [docs/config.md](./config.md).

## `orgfdb links --json`

`orgfdb links --json` prints stored link source facts from the SQLite database as JSON. It is a database inspection command: it reads persisted facts only and does not analyze the current contents of Org files.

The command is read-only. It does not scan Org files, rebuild files, resolve targets, or mutate the database.

The output is a JSON array ordered deterministically by:

1. `file_path`
1. `byte_start`
1. `id`

Root links are included. For root links, `heading_level` is `0` and `heading_path` is an empty array.

The JSON objects expose these public fields:

- `file_id`
- `file_path`
- `heading_id`
- `heading_path`
- `heading_level`
- `source_context`
- `format`
- `link_type`
- `raw`
- `raw_target`
- `raw_description`
- `path`
- `search_option`
- `byte_start`
- `byte_end`
- `line`

The output uses `link_type`, not a legacy `type` field.

Notes:

- `format` identifies the source syntax: `bracket`, `angle`, or `plain`.
- `source_context` records where the link came from in the indexed Org file, such as normal text or a drawer context.
- `path`, `search_option`, `byte_start`, `byte_end`, and `line` provide editor-jump data from the stored database facts.

## Phase 3 link indexing

Phase 3 stores raw link source facts only. The scanner contract deliberately does not resolve targets, build relationship graphs, or infer normalized link behavior beyond the documented source-fact fields. Root-attached links are part of the stored data model and remain visible in CLI output.
