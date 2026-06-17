use std::fmt;

use rusqlite::Connection;
use serde::Serialize;

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct HeadingListRow {
    pub id: i64,
    pub file_id: i64,
    pub file_path: String,
    pub parent_id: Option<i64>,
    pub level: i64,
    pub line_number: Option<i64>,
    pub byte_start: i64,
    pub byte_end: i64,
    pub title: String,
    pub title_raw: String,
    pub todo_keyword: Option<String>,
    pub todo_type: Option<String>,
    pub priority: Option<char>,
    pub archivedp: bool,
    pub footnote_section_p: bool,
    pub all_tags_json: String,
}

#[derive(Debug, Default)]
pub struct DbReader;

impl DbReader {
    pub fn new() -> Self {
        Self
    }

    pub fn list_headings(connection: &Connection) -> Result<Vec<HeadingListRow>, DbReadError> {
        let mut statement = connection
            .prepare(
                "SELECT
                    headings.id,
                    headings.file_id,
                    files.path,
                    headings.parent_id,
                    headings.level,
                    headings.line_number,
                    headings.byte_start,
                    headings.byte_end,
                    headings.title,
                    headings.title_raw,
                    headings.todo_keyword,
                    headings.todo_type,
                    headings.priority,
                    headings.archivedp,
                    headings.footnote_section_p,
                    headings.all_tags_json
                 FROM headings
                 INNER JOIN files ON files.id = headings.file_id
                 ORDER BY files.path, headings.byte_start, headings.id",
            )
            .map_err(|source| DbReadError::Query {
                operation: "list_headings.prepare",
                source,
            })?;

        let rows = statement
            .query_map([], |row| {
                let priority: Option<String> = row.get(12)?;
                Ok(HeadingListRow {
                    id: row.get(0)?,
                    file_id: row.get(1)?,
                    file_path: row.get(2)?,
                    parent_id: row.get(3)?,
                    level: row.get(4)?,
                    line_number: row.get(5)?,
                    byte_start: row.get(6)?,
                    byte_end: row.get(7)?,
                    title: row.get(8)?,
                    title_raw: row.get(9)?,
                    todo_keyword: row.get(10)?,
                    todo_type: row.get(11)?,
                    priority: priority.and_then(|value| value.chars().next()),
                    archivedp: row.get::<_, i64>(13)? != 0,
                    footnote_section_p: row.get::<_, i64>(14)? != 0,
                    all_tags_json: row.get(15)?,
                })
            })
            .map_err(|source| DbReadError::Query {
                operation: "list_headings.query",
                source,
            })?;

        rows.collect::<Result<Vec<_>, _>>()
            .map_err(|source| DbReadError::Query {
                operation: "list_headings.collect",
                source,
            })
    }
}

#[derive(Debug)]
pub enum DbReadError {
    Query {
        operation: &'static str,
        source: rusqlite::Error,
    },
}

impl fmt::Display for DbReadError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Query { operation, source } => {
                write!(f, "failed to execute DB read {operation}: {source}")
            }
        }
    }
}

impl std::error::Error for DbReadError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self {
            Self::Query { source, .. } => Some(source),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::DbReader;
    use crate::db::{
        open_in_memory_database_with_schema, DbWriter, FileRecordInput, HeadingRecord,
        SchemaDefinition,
    };
    use std::path::PathBuf;

    #[test]
    fn list_headings_returns_stable_rows_for_cli_json() {
        let schema = SchemaDefinition::new(1, false);
        let mut connection =
            open_in_memory_database_with_schema(&schema).expect("database should open");
        let file = FileRecordInput {
            path: PathBuf::from("/tmp/project.org"),
            mtime_ns: 10,
            size: 100,
            content_hash: None,
            indexed_at: None,
        };

        DbWriter::rebuild_file(&mut connection, &file, |tx, file_id| {
            let level0_id = DbWriter::insert_level0_heading(
                tx,
                &HeadingRecord {
                    id: None,
                    file_id,
                    parent_id: None,
                    level: 0,
                    line_number: None,
                    byte_start: 0,
                    byte_end: 100,
                    title: "/tmp/project.org".to_string(),
                    title_raw: "/tmp/project.org".to_string(),
                    todo_keyword: None,
                    todo_type: None,
                    priority: None,
                    scheduled_raw: None,
                    scheduled_ts: None,
                    deadline_raw: None,
                    deadline_ts: None,
                    closed_raw: None,
                    closed_ts: None,
                    archivedp: false,
                    footnote_section_p: false,
                    all_tags_json: "[]".to_string(),
                },
            )?;
            DbWriter::insert_headings(
                tx,
                &[HeadingRecord {
                    id: None,
                    file_id,
                    parent_id: Some(level0_id),
                    level: 1,
                    line_number: Some(2),
                    byte_start: 10,
                    byte_end: 25,
                    title: "Inbox".to_string(),
                    title_raw: "Inbox".to_string(),
                    todo_keyword: Some("TODO".to_string()),
                    todo_type: Some("open".to_string()),
                    priority: Some('A'),
                    scheduled_raw: None,
                    scheduled_ts: None,
                    deadline_raw: None,
                    deadline_ts: None,
                    closed_raw: None,
                    closed_ts: None,
                    archivedp: false,
                    footnote_section_p: false,
                    all_tags_json: "[\"rust\"]".to_string(),
                }],
            )?;
            Ok(())
        })
        .expect("rebuild should succeed");

        let rows = DbReader::list_headings(&connection).expect("rows should load");

        assert_eq!(rows.len(), 2);
        assert_eq!(rows[0].file_path, "/tmp/project.org");
        assert_eq!(rows[0].level, 0);
        assert_eq!(rows[1].title, "Inbox");
        assert_eq!(rows[1].todo_keyword.as_deref(), Some("TODO"));
        assert_eq!(rows[1].priority, Some('A'));
        assert_eq!(rows[1].all_tags_json, "[\"rust\"]");
    }
}
