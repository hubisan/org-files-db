# CLI Reference

This page documents the user-facing `orgfdb` commands that are stable enough to treat as repository-local reference material.

## `orgfdb headings --json`

`orgfdb headings --json` prints stored heading rows from the SQLite database as JSON. It is a database inspection command: it reads persisted facts only and does not analyze the current contents of Org files.

The command is read-only. It does not scan Org files, rebuild files, resolve targets, or mutate the database.

The output is a JSON array ordered deterministically by:

1. `file_path`
1. `byte_start`
1. `id`

Synthetic root rows are included by default, and `--no-root` excludes them. Root rows use the synthetic repository conventions: `level = 0`, `parent_id = null`, and they represent file/document scope rather than parser-level Org headings.

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

`--include-root` is retained only as a compatibility no-op. The default output already includes root rows.

If you need the underlying configuration settings for the database path, source file selection, link protocols, TODO keywords, or search options, see [docs/config.md](./config.md).

## `orgfdb links --json`

`orgfdb links --json` prints stored link source facts plus stored resolution snapshot fields from the SQLite database as JSON. It is a database inspection command: it reads persisted facts only and does not analyze the current contents of Org files.

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
- `path_absolute`
- `target_file_id`
- `target_heading_id`
- `target_custom_id`
- `target_id`
- `resolution_status`
- `resolution_diagnostic`
- `byte_start`
- `byte_end`
- `line`

The output uses `link_type`, not a legacy `type` field.

Notes:

- `format` identifies the source syntax: `bracket`, `angle`, or `plain`.
- `source_context` records where the link came from in the indexed Org file, such as normal text or a drawer context.
- `path`, `search_option`, `byte_start`, `byte_end`, and `line` provide editor-jump data from the stored database facts.
- `path_absolute`, `target_file_id`, `target_heading_id`, `target_custom_id`, `target_id`, `resolution_status`, and `resolution_diagnostic` mirror the stored resolver-owned DB columns as-is and may be `null`.
- `resolution_status = null` means no resolver pass has populated the row yet.
- `resolution_status = resolved`, `broken`, `unresolved`, `ambiguous`, and `unsupported` reflect the stored Phase 4 outcome.
- File-only links resolve to the target file's synthetic root heading, and file links to indexed files populate `path_absolute` and `target_file_id`.
- Supported file search options are only `::*Heading` and `::#custom-id`; unsupported examples include `::42`, `::/regexp/`, and dedicated targets such as `::target` or `::<<target>>`.
- Org `id:` links resolve only against indexed non-root headings. External `org-id-locations` are not supported, so missing indexed IDs remain `unresolved` with `id not found`.
- The command opens the existing database in read-only mode. If `--config` is provided, only the stored `db_path` is read from that config file.

## Link storage contract

Phase 3 stores raw link source facts, and later rebuild-time resolution may populate separate Phase 4 target fields. `orgfdb links --json` exposes both sets together without rebuilding, resolving on demand, or mutating the database. Root-attached links are part of the stored data model and remain visible in CLI output, including broken, ambiguous, unresolved, and unsupported rows.
