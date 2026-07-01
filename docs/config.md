# Config Reference

This page lists the `config.toml` variables currently supported by `orgfdb`.

## Example

```toml
db_path = "org-files-db.sqlite"
files = ["inbox.org", "projects.org"]
dirs = ["notes", "archive"]
recursive = true

[links]
plain_protocols = ["http", "https", "file"]
custom_protocols = ["jira", "customlink"]

[todo]
default_open_keywords = ["TODO(t)", "NEXT(n)"]
default_closed_keywords = ["DONE(d)", "CANCEL(c)"]

[search]
fts5_enabled = true
index_body_text = false
```

## Top-level keys

- `db_path`
- `files`
- `dirs`
- `recursive`
- `links`
- `todo`
- `search`

## `db_path`

Path to the SQLite database file.

- Type: string
- Default: `org-files-db.sqlite`

Relative paths are resolved against the config file location.

## `files`

Explicit Org files to index.

- Type: array of strings
- Default: `[]`

Each entry is a file path. Relative paths are resolved against the config file location.

## `dirs`

Directories to search for Org files.

- Type: array of strings
- Default: `[]`

Each entry is a directory path. Relative paths are resolved against the config file location.

## `recursive`

Controls whether directory discovery descends into subdirectories for entries listed in `dirs`.

- Type: boolean
- Default: `false`

This is a global switch for configured directories. It is not configured per directory entry.

## `[links]`

Controls plain-link protocol handling for Phase 3 link scanning.

Supported keys:

- `plain_protocols`
- `custom_protocols`

### `links.plain_protocols`

Protocols recognized as plain links.

- Type: array of strings
- Default: built-in plain-link protocol list

When this field is omitted, the built-in defaults are used. When it is present, it defines the base list before `custom_protocols` are appended and deduplicated case-insensitively.

### `links.custom_protocols`

Additional project-specific protocols recognized as plain links.

- Type: array of strings
- Default: `[]`

Values are lowercased and deduplicated together with `plain_protocols`.

## `[todo]`

Controls default TODO keyword handling when a file does not define its own in-buffer Org TODO keywords.

Supported keys:

- `default_open_keywords`
- `default_closed_keywords`

### `todo.default_open_keywords`

Default open TODO keywords.

- Type: array of strings
- Default: `["TODO"]`

Entries may include Org-style fast selection keys such as `TODO(t)`.

### `todo.default_closed_keywords`

Default closed TODO keywords.

- Type: array of strings
- Default: `["DONE"]`

Entries may include Org-style fast selection keys such as `DONE(d)`.

## `[search]`

Controls search-related indexing behavior.

Supported keys:

- `fts5_enabled`
- `index_body_text`

### `search.fts5_enabled`

Controls whether SQLite FTS5 indexing is enabled during rebuild.

- Type: boolean
- Default: `true`

### `search.index_body_text`

Controls whether heading body text is indexed for search.

- Type: boolean
- Default: `false`
