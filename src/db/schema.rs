use std::sync::atomic::{AtomicU64, Ordering};

use rusqlite::Connection;

pub const CURRENT_SCHEMA_VERSION: u32 = 7;

const CORE_SCHEMA_SQL: &str = include_str!("../../sql/schema.sql");
const HEADING_FTS_SQL: &str = r#"
CREATE VIRTUAL TABLE IF NOT EXISTS heading_fts
USING fts5(
    title,
    body,
    tokenize = 'unicode61',
    content = ''
)
"#;
const HEADING_FTS_MARKER: &str = "/*__HEADING_FTS__*/";
const TIMESTAMP_REPEATERS_TABLE_SQL: &str = r#"
CREATE TABLE timestamp_repeaters (
    id                          INTEGER PRIMARY KEY,
    timestamp_id                INTEGER NOT NULL UNIQUE,
    repeater_type               TEXT CHECK (
                                    repeater_type IN ('cumulate', 'catch_up', 'restart')
                                    OR repeater_type IS NULL
                                ),
    repeater_value              INTEGER CHECK (
                                    repeater_value IS NULL
                                    OR repeater_value > 0
                                ),
    repeater_unit               TEXT CHECK (
                                    repeater_unit IN ('hour', 'day', 'week', 'month', 'year')
                                    OR repeater_unit IS NULL
                                ),
    repeater_deadline_value     INTEGER CHECK (
                                    repeater_deadline_value IS NULL
                                    OR repeater_deadline_value > 0
                                ),
    repeater_deadline_unit      TEXT CHECK (
                                    repeater_deadline_unit IN ('hour', 'day', 'week', 'month', 'year')
                                    OR repeater_deadline_unit IS NULL
                                ),
    warning_type                TEXT CHECK (
                                    warning_type IN ('all', 'first')
                                    OR warning_type IS NULL
                                ),
    warning_value               INTEGER CHECK (
                                    warning_value IS NULL
                                    OR warning_value > 0
                                ),
    warning_unit                TEXT CHECK (
                                    warning_unit IN ('hour', 'day', 'week', 'month', 'year')
                                    OR warning_unit IS NULL
                                ),
    FOREIGN KEY (timestamp_id)
        REFERENCES timestamps(id)
        ON DELETE CASCADE,
    CHECK (
        (repeater_type IS NULL AND repeater_value IS NULL AND repeater_unit IS NULL)
        OR
        (repeater_type IS NOT NULL AND repeater_value IS NOT NULL AND repeater_unit IS NOT NULL)
    ),
    CHECK (
        (repeater_deadline_value IS NULL AND repeater_deadline_unit IS NULL)
        OR
        (repeater_deadline_value IS NOT NULL AND repeater_deadline_unit IS NOT NULL)
    ),
    CHECK (
        repeater_deadline_value IS NULL
        OR
        repeater_type IS NOT NULL
    ),
    CHECK (
        (warning_type IS NULL AND warning_value IS NULL AND warning_unit IS NULL)
        OR
        (warning_type IS NOT NULL AND warning_value IS NOT NULL AND warning_unit IS NOT NULL)
    ),
    CHECK (
        repeater_type IS NOT NULL
        OR warning_type IS NOT NULL
    )
)
"#;
const PROPERTIES_TABLE_SQL: &str = r#"
CREATE TABLE properties (
    id              INTEGER PRIMARY KEY,
    heading_id      INTEGER NOT NULL,
    key             TEXT NOT NULL,
    value           TEXT,
    source          TEXT NOT NULL CHECK (
                        source IN ('property_keyword', 'property_drawer', 'category_keyword')
                    ),
    append          INTEGER NOT NULL DEFAULT 0 CHECK (append IN (0, 1)),
    line_number     INTEGER,
    FOREIGN KEY (heading_id)
        REFERENCES headings(id)
        ON DELETE CASCADE
)
"#;
const TODO_KEYWORDS_TABLE_SQL: &str = r#"
CREATE TABLE todo_keywords (
    file_id             INTEGER NOT NULL,
    keyword             TEXT NOT NULL,
    state_type          TEXT NOT NULL CHECK (state_type IN ('open', 'closed')),
    shortcut            TEXT CHECK (shortcut IS NULL OR length(shortcut) = 1),
    sequence_no         INTEGER NOT NULL,
    source_kind         TEXT NOT NULL CHECK (
                            source_kind IN ('config_default', 'org_keyword')
                        ),
    source_keyword      TEXT CHECK (
                            source_keyword IN ('TODO', 'SEQ_TODO', 'TYP_TODO')
                            OR source_keyword IS NULL
                        ),
    source_line_number  INTEGER CHECK (
                            source_line_number IS NULL
                            OR source_line_number > 0
                        ),
    CHECK (
        (source_kind = 'config_default' AND source_keyword IS NULL AND source_line_number IS NULL)
        OR
        (source_kind = 'org_keyword' AND source_keyword IS NOT NULL AND source_line_number IS NOT NULL)
    ),
    FOREIGN KEY (file_id)
        REFERENCES files(id)
        ON DELETE CASCADE,
    PRIMARY KEY (file_id, keyword)
)
"#;
const TAGS_TABLE_SQL: &str = r#"
CREATE TABLE tags (
    heading_id      INTEGER NOT NULL,
    tag             TEXT NOT NULL,
    FOREIGN KEY (heading_id)
        REFERENCES headings(id)
        ON DELETE CASCADE,
    PRIMARY KEY (heading_id, tag)
)
"#;
static SQLITE_FTS5_PROBE_COUNTER: AtomicU64 = AtomicU64::new(0);

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SchemaDefinition {
    pub version: u32,
    pub enable_fts: bool,
}

impl SchemaDefinition {
    pub fn new(version: u32, enable_fts: bool) -> Self {
        Self {
            version,
            enable_fts,
        }
    }

    pub fn render_sql(&self, connection: &Connection) -> String {
        let heading_fts_sql =
            if self.enable_fts && sqlite_supports_fts5(connection).unwrap_or(false) {
                HEADING_FTS_SQL
            } else {
                ""
            };

        CORE_SCHEMA_SQL.replace(HEADING_FTS_MARKER, heading_fts_sql)
    }

    pub fn apply(&self, connection: &Connection) -> rusqlite::Result<()> {
        migrate_legacy_timestamp_repeaters(connection)?;
        migrate_legacy_todo_keywords_table(connection)?;
        migrate_legacy_properties_table(connection)?;
        migrate_legacy_tags_table(connection)?;
        migrate_legacy_headings_table(connection)?;
        migrate_legacy_timestamps_table(connection)?;
        migrate_legacy_links_table(connection)?;
        connection.execute_batch(&self.render_sql(connection))
    }
}

impl Default for SchemaDefinition {
    fn default() -> Self {
        Self {
            version: CURRENT_SCHEMA_VERSION,
            enable_fts: true,
        }
    }
}

pub fn sqlite_supports_fts5(connection: &Connection) -> rusqlite::Result<bool> {
    let probe_id = SQLITE_FTS5_PROBE_COUNTER.fetch_add(1, Ordering::Relaxed);
    let savepoint_name = format!("org_files_db_fts5_probe_sp_{probe_id}");
    let table_name = format!("org_files_db_fts5_probe_vt_{probe_id}");

    connection.execute_batch(&format!("SAVEPOINT {savepoint_name};"))?;
    let probe_result = connection.execute_batch(&format!(
        "CREATE VIRTUAL TABLE temp.{table_name}
         USING fts5(
             title,
             body,
             tokenize = 'unicode61',
             content = ''
         );"
    ));
    let cleanup_result = connection.execute_batch(&format!(
        "ROLLBACK TO {savepoint_name}; RELEASE {savepoint_name};"
    ));

    match (probe_result, cleanup_result) {
        (Ok(()), Ok(())) => Ok(true),
        (Err(error), Ok(())) if is_expected_fts5_unavailable_error(&error) => Ok(false),
        (Err(error), Ok(())) => Err(error),
        (_, Err(cleanup_error)) => Err(cleanup_error),
    }
}

pub(crate) fn heading_fts_sql() -> &'static str {
    HEADING_FTS_SQL
}

fn is_expected_fts5_unavailable_error(error: &rusqlite::Error) -> bool {
    let Some(message) = sqlite_error_message(error) else {
        return false;
    };

    message.contains("no such module: fts5")
        || message.contains("no such module: fts")
        || message.contains("unknown tokenizer")
        || message.contains("unrecognized option")
}

fn sqlite_error_message(error: &rusqlite::Error) -> Option<&str> {
    match error {
        rusqlite::Error::SqliteFailure(_, Some(message)) => Some(message.as_str()),
        _ => None,
    }
}

fn migrate_legacy_timestamp_repeaters(connection: &Connection) -> rusqlite::Result<()> {
    if !table_exists(connection, "timestamp_repeaters")? {
        return Ok(());
    }

    let columns = table_columns(connection, "timestamp_repeaters")?;
    let has_kind = columns.iter().any(|column| column == "kind");
    let has_repeater_deadline_value = columns
        .iter()
        .any(|column| column == "repeater_deadline_value");
    let has_repeater_deadline_unit = columns
        .iter()
        .any(|column| column == "repeater_deadline_unit");
    let has_deadline_value = columns.iter().any(|column| column == "deadline_value");
    let has_deadline_unit = columns.iter().any(|column| column == "deadline_unit");

    if timestamp_repeaters_uses_explicit_columns(&columns) {
        return Ok(());
    }

    connection.execute_batch(
        r#"
ALTER TABLE timestamp_repeaters RENAME TO timestamp_repeaters_legacy;
"#,
    )?;
    connection.execute_batch(TIMESTAMP_REPEATERS_TABLE_SQL)?;
    let repeater_type_expr = if has_kind {
        "MAX(CASE WHEN kind = 'repeater' THEN type ELSE NULL END)"
    } else {
        "MAX(CASE WHEN type IN ('cumulate', 'catch_up', 'restart') THEN type ELSE NULL END)"
    };
    let repeater_value_expr = if has_kind {
        "MAX(CASE WHEN kind = 'repeater' THEN value ELSE NULL END)"
    } else {
        "MAX(CASE WHEN type IN ('cumulate', 'catch_up', 'restart') THEN value ELSE NULL END)"
    };
    let repeater_unit_expr = if has_kind {
        "MAX(CASE WHEN kind = 'repeater' THEN unit ELSE NULL END)"
    } else {
        "MAX(CASE WHEN type IN ('cumulate', 'catch_up', 'restart') THEN unit ELSE NULL END)"
    };
    let warning_type_expr = if has_kind {
        "MAX(CASE WHEN kind = 'warning' THEN type ELSE NULL END)"
    } else {
        "NULL"
    };
    let warning_value_expr = if has_kind {
        "MAX(CASE WHEN kind = 'warning' THEN value ELSE NULL END)"
    } else {
        "NULL"
    };
    let warning_unit_expr = if has_kind {
        "MAX(CASE WHEN kind = 'warning' THEN unit ELSE NULL END)"
    } else {
        "NULL"
    };
    let repeater_deadline_value_expr = match (has_repeater_deadline_value, has_deadline_value) {
        (true, true) => "MAX(COALESCE(repeater_deadline_value, deadline_value))",
        (true, false) => "MAX(repeater_deadline_value)",
        (false, true) => "MAX(deadline_value)",
        (false, false) => "NULL",
    };
    let repeater_deadline_unit_expr = match (has_repeater_deadline_unit, has_deadline_unit) {
        (true, true) => "MAX(COALESCE(repeater_deadline_unit, deadline_unit))",
        (true, false) => "MAX(repeater_deadline_unit)",
        (false, true) => "MAX(deadline_unit)",
        (false, false) => "NULL",
    };
    let migration_sql = format!(
        r#"
INSERT INTO timestamp_repeaters (
    id,
    timestamp_id,
    repeater_type,
    repeater_value,
    repeater_unit,
    repeater_deadline_value,
    repeater_deadline_unit,
    warning_type,
    warning_value,
    warning_unit
)
SELECT
    MIN(id),
    timestamp_id,
    {repeater_type_expr} AS repeater_type,
    {repeater_value_expr} AS repeater_value,
    {repeater_unit_expr} AS repeater_unit,
    {repeater_deadline_value_expr} AS repeater_deadline_value,
    {repeater_deadline_unit_expr} AS repeater_deadline_unit,
    {warning_type_expr} AS warning_type,
    {warning_value_expr} AS warning_value,
    {warning_unit_expr} AS warning_unit
FROM timestamp_repeaters_legacy
GROUP BY timestamp_id;

DROP TABLE timestamp_repeaters_legacy;
"#,
    );
    connection.execute_batch(&migration_sql)?;

    Ok(())
}

fn timestamp_repeaters_uses_explicit_columns(columns: &[String]) -> bool {
    [
        "timestamp_id",
        "repeater_type",
        "repeater_value",
        "repeater_unit",
        "repeater_deadline_value",
        "repeater_deadline_unit",
        "warning_type",
        "warning_value",
        "warning_unit",
    ]
    .iter()
    .all(|required| columns.iter().any(|column| column == required))
}

fn migrate_legacy_properties_table(connection: &Connection) -> rusqlite::Result<()> {
    if !table_exists(connection, "properties")? {
        return Ok(());
    }

    let columns = table_columns(connection, "properties")?;
    if properties_table_uses_append_column(&columns) {
        return Ok(());
    }

    connection.execute_batch(
        r#"
ALTER TABLE properties RENAME TO properties_legacy;
"#,
    )?;
    connection.execute_batch(PROPERTIES_TABLE_SQL)?;
    connection.execute_batch(
        r#"
INSERT INTO properties (id, heading_id, key, value, source, append, line_number)
SELECT id, heading_id, key, value, source, 0, line_number
FROM properties_legacy;

DROP TABLE properties_legacy;
"#,
    )?;

    Ok(())
}

fn migrate_legacy_todo_keywords_table(connection: &Connection) -> rusqlite::Result<()> {
    if !table_exists(connection, "todo_keywords")? {
        return Ok(());
    }

    let columns = table_columns(connection, "todo_keywords")?;
    if todo_keywords_table_uses_provenance_columns(&columns) {
        return Ok(());
    }

    connection.execute_batch(
        r#"
ALTER TABLE todo_keywords RENAME TO todo_keywords_legacy;
"#,
    )?;
    connection.execute_batch(TODO_KEYWORDS_TABLE_SQL)?;
    connection.execute_batch(
        r#"
INSERT INTO todo_keywords (
    file_id,
    keyword,
    state_type,
    shortcut,
    sequence_no,
    source_kind,
    source_keyword,
    source_line_number
)
SELECT
    file_id,
    keyword,
    state_type,
    shortcut,
    sequence_no,
    'config_default',
    NULL,
    NULL
FROM todo_keywords_legacy;

DROP TABLE todo_keywords_legacy;
"#,
    )?;

    Ok(())
}

fn todo_keywords_table_uses_provenance_columns(columns: &[String]) -> bool {
    [
        "file_id",
        "keyword",
        "state_type",
        "shortcut",
        "sequence_no",
        "source_kind",
        "source_keyword",
        "source_line_number",
    ]
    .iter()
    .all(|required| columns.iter().any(|column| column == required))
}

fn properties_table_uses_append_column(columns: &[String]) -> bool {
    columns.iter().any(|column| column == "append")
        && !columns.iter().any(|column| column == "inherited")
}

fn migrate_legacy_tags_table(connection: &Connection) -> rusqlite::Result<()> {
    if !table_exists(connection, "tags")? {
        return Ok(());
    }

    let columns = table_columns(connection, "tags")?;
    if tags_table_uses_direct_facts(&columns) {
        return Ok(());
    }

    connection.execute_batch(
        r#"
ALTER TABLE tags RENAME TO tags_legacy;
"#,
    )?;
    connection.execute_batch(TAGS_TABLE_SQL)?;
    connection.execute_batch(
        r#"
INSERT INTO tags (heading_id, tag)
SELECT DISTINCT heading_id, tag
FROM tags_legacy
WHERE inherited = 0;

DROP TABLE tags_legacy;
"#,
    )?;

    Ok(())
}

fn tags_table_uses_direct_facts(columns: &[String]) -> bool {
    columns.len() == 2
        && columns.iter().any(|column| column == "heading_id")
        && columns.iter().any(|column| column == "tag")
}

fn migrate_legacy_headings_table(connection: &Connection) -> rusqlite::Result<()> {
    if !table_exists(connection, "headings")? {
        return Ok(());
    }

    let columns = table_columns(connection, "headings")?;
    if headings_table_matches_current_contract(connection, &columns)? {
        return Ok(());
    }

    connection.execute_batch(
        r#"
ALTER TABLE headings RENAME TO headings_legacy;
"#,
    )?;
    drop_indexes_for_table(connection, "headings_legacy")?;
    connection.execute_batch(CORE_SCHEMA_SQL)?;
    let scheduled_has_time_expr = if has_column(&columns, "scheduled_has_time") {
        "scheduled_has_time"
    } else {
        "NULL"
    };
    let deadline_has_time_expr = if has_column(&columns, "deadline_has_time") {
        "deadline_has_time"
    } else {
        "NULL"
    };
    let closed_has_time_expr = if has_column(&columns, "closed_has_time") {
        "closed_has_time"
    } else {
        "NULL"
    };

    connection.execute_batch(&format!(
        r#"
INSERT INTO headings (
    id,
    file_id,
    parent_id,
    level,
    line_number,
    byte_start,
    byte_end,
    title,
    title_raw,
    todo_keyword,
    todo_type,
    priority,
    scheduled_raw,
    scheduled_ts,
    scheduled_has_time,
    deadline_raw,
    deadline_ts,
    deadline_has_time,
    closed_raw,
    closed_ts,
    closed_has_time,
    archivedp,
    footnote_section_p,
    all_tags_json
)
SELECT
    id,
    file_id,
    parent_id,
    level,
    line_number,
    byte_start,
    byte_end,
    title,
    CASE
        WHEN level = 0 AND title = title_raw THEN NULL
        ELSE title_raw
    END,
    todo_keyword,
    todo_type,
    priority,
    scheduled_raw,
    scheduled_ts,
    {scheduled_has_time_expr},
    deadline_raw,
    deadline_ts,
    {deadline_has_time_expr},
    closed_raw,
    closed_ts,
    {closed_has_time_expr},
    archivedp,
    footnote_section_p,
    all_tags_json
FROM headings_legacy;

DROP TABLE headings_legacy;
"#,
    ))?;

    Ok(())
}

fn headings_table_matches_current_contract(
    connection: &Connection,
    columns: &[String],
) -> rusqlite::Result<bool> {
    if !columns.iter().any(|column| column == "title_raw") {
        return Ok(false);
    }
    if !has_column(columns, "scheduled_has_time")
        || !has_column(columns, "deadline_has_time")
        || !has_column(columns, "closed_has_time")
    {
        return Ok(false);
    }

    let mut statement = connection.prepare("PRAGMA table_info(headings)")?;
    let rows = statement.query_map([], |row| {
        Ok((row.get::<_, String>(1)?, row.get::<_, i64>(3)?))
    })?;

    for row in rows {
        let (name, not_null) = row?;
        if name == "title_raw" {
            return Ok(not_null == 0);
        }
    }

    Ok(false)
}

fn migrate_legacy_timestamps_table(connection: &Connection) -> rusqlite::Result<()> {
    if !table_exists(connection, "timestamps")? {
        return Ok(());
    }

    let columns = table_columns(connection, "timestamps")?;
    if timestamps_table_matches_current_contract(&columns) {
        return Ok(());
    }

    connection.execute_batch(
        r#"
ALTER TABLE timestamps RENAME TO timestamps_legacy;
"#,
    )?;
    drop_indexes_for_table(connection, "timestamps_legacy")?;
    connection.execute_batch(CORE_SCHEMA_SQL)?;
    let has_time_expr = if has_column(&columns, "has_time") {
        "has_time"
    } else {
        "NULL"
    };

    connection.execute_batch(&format!(
        r#"
INSERT INTO timestamps (
    id,
    heading_id,
    role,
    has_time,
    start_ts,
    end_ts,
    type,
    range_type,
    raw_value,
    byte_start,
    byte_end,
    line_number
)
SELECT
    id,
    heading_id,
    role,
    {has_time_expr},
    start_ts,
    end_ts,
    type,
    range_type,
    raw_value,
    byte_start,
    byte_end,
    line_number
FROM timestamps_legacy;

DROP TABLE timestamps_legacy;
"#,
    ))?;

    Ok(())
}

fn timestamps_table_matches_current_contract(columns: &[String]) -> bool {
    has_column(columns, "has_time") && !columns.iter().any(|column| column == "has_repeater")
}

fn migrate_legacy_links_table(connection: &Connection) -> rusqlite::Result<()> {
    if !table_exists(connection, "links")? {
        return Ok(());
    }

    let columns = table_columns(connection, "links")?;
    if links_table_matches_current_contract(&columns) {
        return Ok(());
    }

    let migration_sql = render_links_migration_sql(&columns);
    connection.execute_batch(
        r#"
ALTER TABLE links RENAME TO links_legacy;
"#,
    )?;
    drop_indexes_for_table(connection, "links_legacy")?;
    connection.execute_batch(CORE_SCHEMA_SQL)?;
    connection.execute_batch(&migration_sql)?;
    ensure_links_indexes(connection)?;

    Ok(())
}

fn table_exists(connection: &Connection, table_name: &str) -> rusqlite::Result<bool> {
    connection.query_row(
        "SELECT EXISTS(
             SELECT 1
             FROM sqlite_master
             WHERE type = 'table' AND name = ?1
         )",
        [table_name],
        |row| row.get(0),
    )
}

fn table_columns(connection: &Connection, table_name: &str) -> rusqlite::Result<Vec<String>> {
    let mut statement = connection.prepare(&format!("PRAGMA table_info({table_name})"))?;
    let columns = statement
        .query_map([], |row| row.get(1))?
        .collect::<Result<Vec<String>, _>>()?;
    Ok(columns)
}

fn links_table_matches_current_contract(columns: &[String]) -> bool {
    columns.iter().any(|column| column == "line")
        && columns.iter().any(|column| column == "source_context")
        && columns.iter().any(|column| column == "raw")
        && columns.iter().any(|column| column == "raw_target")
        && columns.iter().any(|column| column == "raw_description")
        && columns.iter().any(|column| column == "link_type")
        && columns.iter().any(|column| column == "path")
        && columns.iter().any(|column| column == "path_absolute")
        && columns.iter().any(|column| column == "target_file_id")
        && columns.iter().any(|column| column == "target_heading_id")
        && columns.iter().any(|column| column == "target_custom_id")
        && columns.iter().any(|column| column == "target_id")
        && columns.iter().any(|column| column == "resolution_status")
        && columns
            .iter()
            .any(|column| column == "resolution_diagnostic")
        && !columns.iter().any(|column| column == "line_number")
        && !columns.iter().any(|column| column == "target")
        && !columns.iter().any(|column| column == "raw_link")
        && !columns.iter().any(|column| column == "description")
        && !columns.iter().any(|column| column == "relation")
        && !columns.iter().any(|column| column == "resolved_file_id")
        && !columns.iter().any(|column| column == "resolved_heading_id")
        && !columns.iter().any(|column| column == "resolved")
        && !columns.iter().any(|column| column == "broken")
        && !columns.iter().any(|column| column == "diagnostic")
}

fn render_links_migration_sql(columns: &[String]) -> String {
    let line_expr = if has_column(columns, "line") {
        "line"
    } else {
        "COALESCE(line_number, 1)"
    };
    let source_context_expr = if has_column(columns, "source_context") {
        "source_context"
    } else {
        "'normal'"
    };
    let raw_expr = if has_column(columns, "raw") {
        "raw"
    } else {
        "raw_link"
    };
    let raw_target_expr = if has_column(columns, "raw_target") {
        "raw_target"
    } else {
        r#"CASE
        WHEN link_type IS NOT NULL AND search_option IS NOT NULL
            THEN lower(link_type) || ':' || target || '::' || search_option
        WHEN link_type IS NOT NULL
            THEN lower(link_type) || ':' || target
        ELSE target
    END"#
    };
    let raw_description_expr = if has_column(columns, "raw_description") {
        "raw_description"
    } else {
        "description"
    };
    let link_type_expr = if has_column(columns, "path") {
        "link_type"
    } else {
        "COALESCE(lower(link_type), 'unknown')"
    };
    let path_expr = if has_column(columns, "path") {
        "path"
    } else {
        "target"
    };
    let path_absolute_expr = if has_column(columns, "path_absolute") {
        "path_absolute"
    } else {
        "target_absolute"
    };
    let target_file_id_expr = if has_column(columns, "target_file_id") {
        "target_file_id"
    } else {
        "resolved_file_id"
    };
    let target_heading_id_expr = if has_column(columns, "target_heading_id") {
        "target_heading_id"
    } else {
        "resolved_heading_id"
    };
    let target_custom_id_expr = if has_column(columns, "target_custom_id") {
        "target_custom_id"
    } else {
        "NULL"
    };
    let target_id_expr = if has_column(columns, "target_id") {
        "target_id"
    } else {
        "NULL"
    };
    let legacy_resolved_expr = if has_column(columns, "resolved") {
        "resolved"
    } else {
        "0"
    };
    let legacy_broken_expr = if has_column(columns, "broken") {
        "broken"
    } else {
        "0"
    };
    let resolution_status_expr = if has_column(columns, "resolution_status") {
        "resolution_status".to_string()
    } else if has_column(columns, "resolved") || has_column(columns, "broken") {
        format!(
            r#"CASE
        WHEN {legacy_resolved_expr} = 1 THEN 'resolved'
        WHEN {legacy_broken_expr} = 1 THEN 'broken'
        ELSE NULL
    END"#
        )
    } else {
        "NULL".to_string()
    };
    let resolution_diagnostic_expr = if has_column(columns, "resolution_diagnostic") {
        "resolution_diagnostic"
    } else if has_column(columns, "diagnostic") {
        "diagnostic"
    } else {
        "NULL"
    };

    format!(
        r#"
INSERT INTO links (
    id,
    file_id,
    heading_id,
    byte_start,
    byte_end,
    line,
    source_context,
    format,
    raw,
    raw_target,
    raw_description,
    link_type,
    path,
    search_option,
    path_absolute,
    target_file_id,
    target_heading_id,
    target_custom_id,
    target_id,
    resolution_status,
    resolution_diagnostic
)
SELECT
    id,
    file_id,
    heading_id,
    byte_start,
    byte_end,
    {line_expr} AS line,
    {source_context_expr} AS source_context,
    COALESCE(format, 'plain') AS format,
    {raw_expr} AS raw,
    {raw_target_expr} AS raw_target,
    {raw_description_expr} AS raw_description,
    {link_type_expr} AS link_type,
    {path_expr} AS path,
    search_option,
    {path_absolute_expr} AS path_absolute,
    {target_file_id_expr} AS target_file_id,
    {target_heading_id_expr} AS target_heading_id,
    {target_custom_id_expr} AS target_custom_id,
    {target_id_expr} AS target_id,
    {resolution_status_expr} AS resolution_status,
    {resolution_diagnostic_expr} AS resolution_diagnostic
FROM links_legacy;

DROP TABLE links_legacy;
"#,
    )
}

fn has_column(columns: &[String], name: &str) -> bool {
    columns.iter().any(|column| column == name)
}

fn drop_indexes_for_table(connection: &Connection, table_name: &str) -> rusqlite::Result<()> {
    let index_names = index_names_for_table(connection, table_name)?;
    for index_name in index_names {
        let quoted_index_name = quote_sqlite_identifier(&index_name);
        connection.execute_batch(&format!("DROP INDEX IF EXISTS {quoted_index_name};"))?;
    }
    Ok(())
}

fn index_names_for_table(
    connection: &Connection,
    table_name: &str,
) -> rusqlite::Result<Vec<String>> {
    let mut statement = connection.prepare(
        "SELECT name
         FROM sqlite_master
         WHERE type = 'index' AND tbl_name = ?1 AND sql IS NOT NULL",
    )?;
    let index_names = statement
        .query_map([table_name], |row| row.get(0))?
        .collect::<Result<Vec<String>, _>>()?;
    Ok(index_names)
}

fn ensure_links_indexes(connection: &Connection) -> rusqlite::Result<()> {
    connection.execute_batch(
        r#"
CREATE INDEX IF NOT EXISTS idx_links_heading
    ON links(heading_id);

CREATE INDEX IF NOT EXISTS idx_links_path
    ON links(path);

CREATE INDEX IF NOT EXISTS idx_links_target_file
    ON links(target_file_id);

CREATE INDEX IF NOT EXISTS idx_links_target_heading
    ON links(target_heading_id);
"#,
    )
}

fn quote_sqlite_identifier(identifier: &str) -> String {
    format!("\"{}\"", identifier.replace('"', "\"\""))
}
