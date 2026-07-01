# Config Reference

This page lists the `config.toml` variables currently supported by `orgfdb`.

## Top-Level Keys

- `db_path`
- `files`
- `dirs`
- `recursive`
- `parse`
- `links`
- `todo`
- `search`

## `db_path`

Path to the SQLite database file.

- Type: string
- Default: `org-files-db.sqlite`

## `files`

Explicit Org files to index.

- Type: array of strings
- Default: `[]`

## `dirs`

Directories to search for Org files.

- Type: array of strings
- Default: `[]`

## `recursive`

Controls whether directory discovery is recursive.

- Type: boolean
- Default: `false`

## `parse.dir_locals`

Parser-specific `.dir-locals.el` handling.

Supported nested keys:

- `parse.dir_locals.enabled`
- `parse.dir_locals.inherit`
- `parse.dir_locals.unsupported`

Defaults:

- `parse.dir_locals.enabled = false`
- `parse.dir_locals.inherit = true`
- `parse.dir_locals.unsupported = "warn"`

## `links`

Controls plain-link protocol handling.

Supported nested keys:

- `links.plain_protocols`
- `links.custom_protocols`

Defaults:

- `links.plain_protocols` uses the built-in Phase 3 default protocol list.
- `links.custom_protocols` is empty by default.

## `todo`

Controls default TODO keyword configuration.

Supported nested keys:

- `todo.default_open_keywords`
- `todo.default_closed_keywords`

Defaults:

- `todo.default_open_keywords = ["TODO"]`
- `todo.default_closed_keywords = ["DONE"]`

## `search`

Controls search-related indexing behavior.

Supported nested keys:

- `search.fts5_enabled`
- `search.index_body_text`

Defaults:

- `search.fts5_enabled = true`
- `search.index_body_text = false`

## Notes

- Paths are resolved relative to the config file location.
- Omitted nested tables use their documented defaults.
- The documentation here matches the current code-level config parser, not a future mdBook schema.
