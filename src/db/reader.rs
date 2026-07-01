use std::fmt;

use rusqlite::Connection;
use serde::Serialize;

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub(crate) struct HeadingListRow {
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
    pub scheduled_raw: Option<String>,
    pub scheduled_ts: Option<i64>,
    pub deadline_raw: Option<String>,
    pub deadline_ts: Option<i64>,
    pub closed_raw: Option<String>,
    pub closed_ts: Option<i64>,
    pub archivedp: bool,
    pub footnote_section_p: bool,
    pub all_tags_json: String,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub(crate) struct LinkListRow {
    pub id: i64,
    pub file_id: i64,
    pub file_path: String,
    pub heading_id: i64,
    pub heading_level: i64,
    pub heading_breadcrumbs_json: String,
    pub source_context: String,
    pub format: String,
    pub link_type: String,
    pub raw: String,
    pub raw_target: String,
    pub raw_description: Option<String>,
    pub path: String,
    pub search_option: Option<String>,
    pub byte_start: i64,
    pub byte_end: i64,
    pub line: i64,
}

#[derive(Debug, Default)]
pub(crate) struct DbReader;

impl DbReader {
    pub(crate) fn list_headings(
        connection: &Connection,
    ) -> Result<Vec<HeadingListRow>, DbReadError> {
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
                    headings.scheduled_raw,
                    headings.scheduled_ts,
                    headings.deadline_raw,
                    headings.deadline_ts,
                    headings.closed_raw,
                    headings.closed_ts,
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
                    scheduled_raw: row.get(13)?,
                    scheduled_ts: row.get(14)?,
                    deadline_raw: row.get(15)?,
                    deadline_ts: row.get(16)?,
                    closed_raw: row.get(17)?,
                    closed_ts: row.get(18)?,
                    archivedp: row.get::<_, i64>(19)? != 0,
                    footnote_section_p: row.get::<_, i64>(20)? != 0,
                    all_tags_json: row.get(21)?,
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

    pub(crate) fn list_links(connection: &Connection) -> Result<Vec<LinkListRow>, DbReadError> {
        let mut statement = connection
            .prepare(
                "SELECT
                    links.id,
                    links.file_id,
                    files.path,
                    links.heading_id,
                    headings.level,
                    outline_path.breadcrumbs_json,
                    links.source_context,
                    links.format,
                    links.link_type,
                    links.raw,
                    links.raw_target,
                    links.raw_description,
                    links.path,
                    links.search_option,
                    links.byte_start,
                    links.byte_end,
                    links.line
                 FROM links
                 INNER JOIN files ON files.id = links.file_id
                 INNER JOIN headings ON headings.id = links.heading_id
                 INNER JOIN outline_path ON outline_path.heading_id = links.heading_id
                 ORDER BY files.path, links.byte_start, links.id",
            )
            .map_err(|source| DbReadError::Query {
                operation: "list_links.prepare",
                source,
            })?;

        let rows = statement
            .query_map([], |row| {
                Ok(LinkListRow {
                    id: row.get(0)?,
                    file_id: row.get(1)?,
                    file_path: row.get(2)?,
                    heading_id: row.get(3)?,
                    heading_level: row.get(4)?,
                    heading_breadcrumbs_json: row.get(5)?,
                    source_context: row.get(6)?,
                    format: row.get(7)?,
                    link_type: row.get(8)?,
                    raw: row.get(9)?,
                    raw_target: row.get(10)?,
                    raw_description: row.get(11)?,
                    path: row.get(12)?,
                    search_option: row.get(13)?,
                    byte_start: row.get(14)?,
                    byte_end: row.get(15)?,
                    line: row.get(16)?,
                })
            })
            .map_err(|source| DbReadError::Query {
                operation: "list_links.query",
                source,
            })?;

        rows.collect::<Result<Vec<_>, _>>()
            .map_err(|source| DbReadError::Query {
                operation: "list_links.collect",
                source,
            })
    }
}

#[derive(Debug)]
pub(crate) enum DbReadError {
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
        open_in_memory_database_with_schema, DbWriter, FileRecordInput, HeadingRecord, LinkRecord,
        OutlinePathRecord, SchemaDefinition,
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
                    byte_start: -1,
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
        assert_eq!(rows[1].scheduled_raw, None);
        assert_eq!(rows[1].scheduled_ts, None);
        assert_eq!(rows[1].all_tags_json, "[\"rust\"]");
    }

    #[test]
    fn list_links_returns_stable_rows_for_cli_json() {
        let schema = SchemaDefinition::new(2, false);
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
            let root_id = DbWriter::insert_level0_heading(
                tx,
                &HeadingRecord {
                    id: None,
                    file_id,
                    parent_id: None,
                    level: 0,
                    line_number: None,
                    byte_start: -1,
                    byte_end: 100,
                    title: "Project".to_string(),
                    title_raw: "Project".to_string(),
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
            DbWriter::insert_outline_path(
                tx,
                &[OutlinePathRecord {
                    heading_id: root_id,
                    file_id,
                    parent_id: None,
                    depth: 0,
                    materialized_path: "0000".to_string(),
                    breadcrumbs_json: "[\"Project\"]".to_string(),
                }],
            )?;
            let child = HeadingRecord {
                id: None,
                file_id,
                parent_id: Some(root_id),
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
                all_tags_json: "[]".to_string(),
            };
            DbWriter::insert_headings(tx, &[child])?;
            let child_id = tx.last_insert_rowid();
            DbWriter::insert_outline_path(
                tx,
                &[OutlinePathRecord {
                    heading_id: child_id,
                    file_id,
                    parent_id: Some(root_id),
                    depth: 1,
                    materialized_path: "0000.0001".to_string(),
                    breadcrumbs_json: "[\"Project\",\"Inbox\"]".to_string(),
                }],
            )?;
            DbWriter::insert_links(
                tx,
                &[
                    LinkRecord {
                        id: Some(5),
                        file_id,
                        heading_id: child_id,
                        byte_start: 40,
                        byte_end: 59,
                        line: 3,
                        source_context: "normal".to_string(),
                        format: "plain".to_string(),
                        raw: "https://example.org".to_string(),
                        raw_target: "https://example.org".to_string(),
                        raw_description: None,
                        link_type: "https".to_string(),
                        path: "//example.org".to_string(),
                        search_option: None,
                    },
                    LinkRecord {
                        id: Some(4),
                        file_id,
                        heading_id: root_id,
                        byte_start: 0,
                        byte_end: 15,
                        line: 1,
                        source_context: "normal".to_string(),
                        format: "bracket".to_string(),
                        raw: "[[id:root-link]]".to_string(),
                        raw_target: "id:root-link".to_string(),
                        raw_description: None,
                        link_type: "id".to_string(),
                        path: "root-link".to_string(),
                        search_option: None,
                    },
                ],
            )?;
            Ok(())
        })
        .expect("rebuild should succeed");

        let rows = DbReader::list_links(&connection).expect("rows should load");

        assert_eq!(rows.len(), 2);
        assert_eq!(rows[0].file_path, "/tmp/project.org");
        assert_eq!(rows[0].heading_level, 0);
        assert_eq!(rows[0].heading_breadcrumbs_json, "[\"Project\"]");
        assert_eq!(rows[0].link_type, "id");
        assert_eq!(rows[1].heading_level, 1);
        assert_eq!(rows[1].heading_breadcrumbs_json, "[\"Project\",\"Inbox\"]");
        assert_eq!(rows[1].byte_start, 40);
    }
}
