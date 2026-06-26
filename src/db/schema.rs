use rusqlite::Connection;

const CORE_SCHEMA_SQL: &str = include_str!("../../sql/schema.sql");
const HEADING_FTS_SQL: &str = r#"
CREATE VIRTUAL TABLE IF NOT EXISTS heading_fts
USING fts5(
    title,
    body,
    tokenize = 'unicode61'
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
        let heading_fts_sql = if self.enable_fts && sqlite_supports_fts5(connection) {
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
        connection.execute_batch(&self.render_sql(connection))
    }
}

impl Default for SchemaDefinition {
    fn default() -> Self {
        Self {
            version: 1,
            enable_fts: true,
        }
    }
}

pub fn sqlite_supports_fts5(connection: &Connection) -> bool {
    let mut statement = match connection.prepare("PRAGMA compile_options;") {
        Ok(statement) => statement,
        Err(_) => return false,
    };
    let rows = match statement.query_map([], |row| row.get::<_, String>(0)) {
        Ok(rows) => rows,
        Err(_) => return false,
    };

    for row in rows {
        let option = match row {
            Ok(option) => option,
            Err(_) => return false,
        };
        if option == "ENABLE_FTS5" {
            return true;
        }
    }

    false
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
