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
