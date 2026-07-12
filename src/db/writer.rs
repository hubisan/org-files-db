use std::{fmt, path::PathBuf};

#[cfg(test)]
use rusqlite::Transaction;
use rusqlite::{params, Connection};

use super::schema::sqlite_supports_fts5;

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct FileRecordInput {
    pub path: PathBuf,
    pub mtime_ns: i64,
    pub size: i64,
    pub content_hash: Option<String>,
    pub indexed_at: Option<i64>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct HeadingRecord {
    pub id: Option<i64>,
    pub file_id: i64,
    pub parent_id: Option<i64>,
    pub level: i64,
    pub line_number: Option<i64>,
    pub byte_start: i64,
    pub byte_end: i64,
    pub title: String,
    pub title_raw: Option<String>,
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

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct TodoKeywordRecord {
    pub file_id: i64,
    pub keyword: String,
    pub state_type: String,
    pub shortcut: Option<char>,
    pub sequence_no: i64,
    pub source_kind: String,
    pub source_keyword: Option<String>,
    pub source_line_number: Option<i64>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct TagRecord {
    pub heading_id: i64,
    pub tag: String,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct TimestampRecord {
    pub heading_id: i64,
    pub role: Option<String>,
    pub start_ts: Option<i64>,
    pub end_ts: Option<i64>,
    pub timestamp_type: Option<String>,
    pub range_type: Option<String>,
    pub raw_value: String,
    pub byte_start: i64,
    pub byte_end: i64,
    pub line_number: Option<i64>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct TimestampRepeaterRecord {
    pub timestamp_id: i64,
    pub repeater_type: Option<String>,
    pub repeater_value: Option<i64>,
    pub repeater_unit: Option<String>,
    pub repeater_deadline_value: Option<i64>,
    pub repeater_deadline_unit: Option<String>,
    pub warning_type: Option<String>,
    pub warning_value: Option<i64>,
    pub warning_unit: Option<String>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct KeywordRecord {
    pub heading_id: i64,
    pub keyword: String,
    pub value: Option<String>,
    pub line_number: Option<i64>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct PropertyRecord {
    pub heading_id: i64,
    pub key: String,
    pub value: Option<String>,
    pub source: String,
    pub append: bool,
    pub line_number: Option<i64>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct OutlinePathRecord {
    pub heading_id: i64,
    pub file_id: i64,
    pub parent_id: Option<i64>,
    pub depth: i64,
    pub materialized_path: String,
    pub breadcrumbs_json: String,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct HeadingBodyRecord {
    pub heading_id: i64,
    pub body_text: String,
    pub body_byte_start: Option<i64>,
    pub body_byte_end: Option<i64>,
}

#[allow(dead_code)]
#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct LinkRecord {
    pub id: Option<i64>,
    pub file_id: i64,
    pub heading_id: i64,
    pub byte_start: i64,
    pub byte_end: i64,
    pub line: i64,
    pub source_context: String,
    pub format: String,
    pub raw: String,
    pub raw_target: String,
    pub raw_description: Option<String>,
    pub link_type: String,
    pub path: String,
    pub search_option: Option<String>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct HeadingFtsRecord {
    pub heading_id: i64,
    pub title: String,
    pub body: String,
}

#[derive(Debug, Default)]
pub(crate) struct DbWriter;

impl DbWriter {
    #[cfg(test)]
    pub(crate) fn rebuild_file<T, F>(
        connection: &mut Connection,
        file: &FileRecordInput,
        operation: F,
    ) -> Result<(i64, T), DbWriteError>
    where
        F: FnOnce(&Transaction<'_>, i64) -> Result<T, DbWriteError>,
    {
        let tx = connection
            .transaction()
            .map_err(|source| DbWriteError::Transaction { source })?;
        let file_id = Self::upsert_file(&tx, file)?;
        Self::delete_file_data(&tx, file_id)?;
        let value = operation(&tx, file_id)?;
        tx.commit()
            .map_err(|source| DbWriteError::Transaction { source })?;
        Ok((file_id, value))
    }

    pub(crate) fn upsert_file(
        connection: &Connection,
        file: &FileRecordInput,
    ) -> Result<i64, DbWriteError> {
        connection
            .execute(
                "INSERT INTO files (path, mtime_ns, size, content_hash, indexed_at)
                 VALUES (?1, ?2, ?3, ?4, ?5)
                 ON CONFLICT(path) DO UPDATE SET
                   mtime_ns = excluded.mtime_ns,
                   size = excluded.size,
                   content_hash = excluded.content_hash,
                   indexed_at = excluded.indexed_at",
                params![
                    file.path.to_string_lossy(),
                    file.mtime_ns,
                    file.size,
                    file.content_hash,
                    file.indexed_at
                ],
            )
            .map_err(|source| DbWriteError::Write {
                operation: "upsert_file",
                source,
            })?;

        connection
            .query_row(
                "SELECT id FROM files WHERE path = ?1",
                [file.path.to_string_lossy().as_ref()],
                |row| row.get(0),
            )
            .map_err(|source| DbWriteError::ReadBack {
                operation: "upsert_file",
                source,
            })
    }

    #[cfg(test)]
    pub(crate) fn delete_file_data(
        connection: &Connection,
        file_id: i64,
    ) -> Result<(), DbWriteError> {
        if heading_fts_table_exists(connection)? {
            connection
                .execute(
                    "DELETE FROM heading_fts
                     WHERE rowid IN (SELECT id FROM headings WHERE file_id = ?1)",
                    [file_id],
                )
                .map_err(|source| DbWriteError::Write {
                    operation: "delete_file_data.heading_fts",
                    source,
                })?;
        }

        connection
            .execute("DELETE FROM todo_keywords WHERE file_id = ?1", [file_id])
            .map_err(|source| DbWriteError::Write {
                operation: "delete_file_data.todo_keywords",
                source,
            })?;
        connection
            .execute("DELETE FROM headings WHERE file_id = ?1", [file_id])
            .map_err(|source| DbWriteError::Write {
                operation: "delete_file_data.headings",
                source,
            })?;
        Ok(())
    }

    pub(crate) fn delete_all_indexed_data(connection: &Connection) -> Result<(), DbWriteError> {
        if heading_fts_table_exists(connection)? {
            connection
                .execute("DELETE FROM heading_fts", [])
                .map_err(|source| DbWriteError::Write {
                    operation: "delete_all_indexed_data.heading_fts",
                    source,
                })?;
        }

        connection
            .execute("DELETE FROM files", [])
            .map_err(|source| DbWriteError::Write {
                operation: "delete_all_indexed_data.files",
                source,
            })?;

        Ok(())
    }

    pub(crate) fn insert_level0_heading(
        connection: &Connection,
        heading: &HeadingRecord,
    ) -> Result<i64, DbWriteError> {
        if heading.level != 0 || heading.parent_id.is_some() {
            return Err(DbWriteError::InvalidInput(
                "level 0 heading must have level = 0 and parent_id = NULL",
            ));
        }

        insert_heading(connection, heading, "insert_level0_heading")
    }

    pub(crate) fn insert_headings(
        connection: &Connection,
        headings: &[HeadingRecord],
    ) -> Result<Vec<i64>, DbWriteError> {
        let mut ids = Vec::with_capacity(headings.len());
        for heading in headings {
            if heading.level == 0 {
                return Err(DbWriteError::InvalidInput(
                    "insert_headings only accepts regular headings with level > 0",
                ));
            }
            if heading.parent_id.is_none() {
                return Err(DbWriteError::InvalidInput(
                    "regular headings must provide a parent_id",
                ));
            }
            ids.push(insert_heading(connection, heading, "insert_headings")?);
        }
        Ok(ids)
    }

    pub(crate) fn insert_todo_keywords(
        connection: &Connection,
        rows: &[TodoKeywordRecord],
    ) -> Result<(), DbWriteError> {
        for row in rows {
            connection
                .execute(
                    "INSERT INTO todo_keywords
                     (file_id, keyword, state_type, shortcut, sequence_no, source_kind,
                      source_keyword, source_line_number)
                     VALUES (?1, ?2, ?3, ?4, ?5, ?6, ?7, ?8)",
                    params![
                        row.file_id,
                        row.keyword,
                        row.state_type,
                        row.shortcut.map(|value| value.to_string()),
                        row.sequence_no,
                        row.source_kind,
                        row.source_keyword,
                        row.source_line_number,
                    ],
                )
                .map_err(|source| DbWriteError::Write {
                    operation: "insert_todo_keywords",
                    source,
                })?;
        }
        Ok(())
    }

    pub(crate) fn insert_tags(
        connection: &Connection,
        rows: &[TagRecord],
    ) -> Result<(), DbWriteError> {
        for row in rows {
            connection
                .execute(
                    "INSERT INTO tags (heading_id, tag) VALUES (?1, ?2)",
                    params![row.heading_id, row.tag],
                )
                .map_err(|source| DbWriteError::Write {
                    operation: "insert_tags",
                    source,
                })?;
        }
        Ok(())
    }

    pub(crate) fn insert_timestamps(
        connection: &Connection,
        rows: &[TimestampRecord],
    ) -> Result<Vec<i64>, DbWriteError> {
        let mut ids = Vec::with_capacity(rows.len());
        for row in rows {
            connection
                .execute(
                    "INSERT INTO timestamps
                     (heading_id, role, start_ts, end_ts, type, range_type, raw_value, byte_start,
                      byte_end, line_number)
                     VALUES (?1, ?2, ?3, ?4, ?5, ?6, ?7, ?8, ?9, ?10)",
                    params![
                        row.heading_id,
                        row.role,
                        row.start_ts,
                        row.end_ts,
                        row.timestamp_type,
                        row.range_type,
                        row.raw_value,
                        row.byte_start,
                        row.byte_end,
                        row.line_number,
                    ],
                )
                .map_err(|source| DbWriteError::Write {
                    operation: "insert_timestamps",
                    source,
                })?;
            ids.push(connection.last_insert_rowid());
        }
        Ok(ids)
    }

    pub(crate) fn insert_timestamp_repeaters(
        connection: &Connection,
        rows: &[TimestampRepeaterRecord],
    ) -> Result<(), DbWriteError> {
        for row in rows {
            connection
                .execute(
                    "INSERT INTO timestamp_repeaters
                     (timestamp_id, repeater_type, repeater_value, repeater_unit,
                      repeater_deadline_value, repeater_deadline_unit,
                      warning_type, warning_value, warning_unit)
                     VALUES (?1, ?2, ?3, ?4, ?5, ?6, ?7, ?8, ?9)",
                    params![
                        row.timestamp_id,
                        row.repeater_type,
                        row.repeater_value,
                        row.repeater_unit,
                        row.repeater_deadline_value,
                        row.repeater_deadline_unit,
                        row.warning_type,
                        row.warning_value,
                        row.warning_unit,
                    ],
                )
                .map_err(|source| DbWriteError::Write {
                    operation: "insert_timestamp_repeaters",
                    source,
                })?;
        }
        Ok(())
    }

    pub(crate) fn insert_keywords(
        connection: &Connection,
        rows: &[KeywordRecord],
    ) -> Result<(), DbWriteError> {
        for row in rows {
            connection
                .execute(
                    "INSERT INTO keywords (heading_id, keyword, value, line_number)
                     VALUES (?1, ?2, ?3, ?4)",
                    params![row.heading_id, row.keyword, row.value, row.line_number],
                )
                .map_err(|source| DbWriteError::Write {
                    operation: "insert_keywords",
                    source,
                })?;
        }
        Ok(())
    }

    pub(crate) fn insert_properties(
        connection: &Connection,
        rows: &[PropertyRecord],
    ) -> Result<(), DbWriteError> {
        for row in rows {
            connection
                .execute(
                    "INSERT INTO properties (heading_id, key, value, source, append, line_number)
                     VALUES (?1, ?2, ?3, ?4, ?5, ?6)",
                    params![
                        row.heading_id,
                        row.key,
                        row.value,
                        row.source,
                        bool_to_i64(row.append),
                        row.line_number
                    ],
                )
                .map_err(|source| DbWriteError::Write {
                    operation: "insert_properties",
                    source,
                })?;
        }
        Ok(())
    }

    pub(crate) fn insert_outline_path(
        connection: &Connection,
        rows: &[OutlinePathRecord],
    ) -> Result<(), DbWriteError> {
        for row in rows {
            connection
                .execute(
                    "INSERT INTO outline_path
                     (heading_id, file_id, parent_id, depth, materialized_path, breadcrumbs_json)
                     VALUES (?1, ?2, ?3, ?4, ?5, ?6)",
                    params![
                        row.heading_id,
                        row.file_id,
                        row.parent_id,
                        row.depth,
                        row.materialized_path,
                        row.breadcrumbs_json
                    ],
                )
                .map_err(|source| DbWriteError::Write {
                    operation: "insert_outline_path",
                    source,
                })?;
        }
        Ok(())
    }

    #[allow(dead_code)]
    pub(crate) fn insert_links(
        connection: &Connection,
        rows: &[LinkRecord],
    ) -> Result<(), DbWriteError> {
        for row in rows {
            connection
                .execute(
                    "INSERT INTO links
                     (id, file_id, heading_id, byte_start, byte_end, line, source_context, format,
                      raw, raw_target, raw_description, link_type, path, search_option,
                      path_absolute, target_file_id, target_heading_id, target_custom_id, target_id,
                      resolution_status, resolution_diagnostic)
                     VALUES (?1, ?2, ?3, ?4, ?5, ?6, ?7, ?8, ?9, ?10, ?11, ?12, ?13, ?14,
                             ?15, ?16, ?17, ?18, ?19, ?20, ?21)",
                    params![
                        row.id,
                        row.file_id,
                        row.heading_id,
                        row.byte_start,
                        row.byte_end,
                        row.line,
                        row.source_context,
                        row.format,
                        row.raw,
                        row.raw_target,
                        row.raw_description,
                        row.link_type,
                        row.path,
                        row.search_option,
                        Option::<String>::None,
                        Option::<i64>::None,
                        Option::<i64>::None,
                        Option::<String>::None,
                        Option::<String>::None,
                        Option::<String>::None,
                        Option::<String>::None
                    ],
                )
                .map_err(|source| DbWriteError::Write {
                    operation: "insert_links",
                    source,
                })?;
        }
        Ok(())
    }

    pub(crate) fn insert_heading_fts(
        connection: &Connection,
        rows: &[HeadingFtsRecord],
    ) -> Result<(), DbWriteError> {
        if !heading_fts_table_exists(connection)? || !sqlite_supports_fts5(connection) {
            return Ok(());
        }

        for row in rows {
            connection
                .execute(
                    "INSERT INTO heading_fts (rowid, title, body) VALUES (?1, ?2, ?3)",
                    params![row.heading_id, row.title, row.body],
                )
                .map_err(|source| DbWriteError::Write {
                    operation: "insert_heading_fts",
                    source,
                })?;
        }
        Ok(())
    }

    pub(crate) fn insert_heading_bodies(
        connection: &Connection,
        rows: &[HeadingBodyRecord],
    ) -> Result<(), DbWriteError> {
        for row in rows {
            connection
                .execute(
                    "INSERT INTO heading_bodies
                     (heading_id, body_text, body_byte_start, body_byte_end)
                     VALUES (?1, ?2, ?3, ?4)",
                    params![
                        row.heading_id,
                        row.body_text,
                        row.body_byte_start,
                        row.body_byte_end
                    ],
                )
                .map_err(|source| DbWriteError::Write {
                    operation: "insert_heading_bodies",
                    source,
                })?;
        }
        Ok(())
    }
}

#[derive(Debug)]
pub enum DbWriteError {
    InvalidInput(&'static str),
    ReadBack {
        operation: &'static str,
        source: rusqlite::Error,
    },
    Transaction {
        source: rusqlite::Error,
    },
    Write {
        operation: &'static str,
        source: rusqlite::Error,
    },
}

impl fmt::Display for DbWriteError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::InvalidInput(message) => write!(f, "invalid DB write input: {message}"),
            Self::ReadBack { operation, source } => {
                write!(
                    f,
                    "failed to read back DB result after {operation}: {source}"
                )
            }
            Self::Transaction { source } => {
                write!(f, "failed to commit or start DB transaction: {source}")
            }
            Self::Write { operation, source } => {
                write!(f, "failed to execute DB write {operation}: {source}")
            }
        }
    }
}

impl std::error::Error for DbWriteError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self {
            Self::InvalidInput(_) => None,
            Self::ReadBack { source, .. }
            | Self::Transaction { source }
            | Self::Write { source, .. } => Some(source),
        }
    }
}

fn insert_heading(
    connection: &Connection,
    heading: &HeadingRecord,
    operation: &'static str,
) -> Result<i64, DbWriteError> {
    connection
        .execute(
            "INSERT INTO headings
             (id, file_id, parent_id, level, line_number, byte_start, byte_end, title, title_raw,
              todo_keyword, todo_type, priority, scheduled_raw, scheduled_ts, deadline_raw,
              deadline_ts, closed_raw, closed_ts, archivedp, footnote_section_p, all_tags_json)
             VALUES (?1, ?2, ?3, ?4, ?5, ?6, ?7, ?8, ?9, ?10, ?11, ?12, ?13, ?14, ?15,
                     ?16, ?17, ?18, ?19, ?20, ?21)",
            params![
                heading.id,
                heading.file_id,
                heading.parent_id,
                heading.level,
                heading.line_number,
                heading.byte_start,
                heading.byte_end,
                heading.title,
                heading.title_raw,
                heading.todo_keyword,
                heading.todo_type,
                heading.priority.map(|value| value.to_string()),
                heading.scheduled_raw,
                heading.scheduled_ts,
                heading.deadline_raw,
                heading.deadline_ts,
                heading.closed_raw,
                heading.closed_ts,
                bool_to_i64(heading.archivedp),
                bool_to_i64(heading.footnote_section_p),
                heading.all_tags_json
            ],
        )
        .map_err(|source| DbWriteError::Write { operation, source })?;

    Ok(heading.id.unwrap_or_else(|| connection.last_insert_rowid()))
}

fn bool_to_i64(value: bool) -> i64 {
    if value {
        1
    } else {
        0
    }
}

fn heading_fts_table_exists(connection: &Connection) -> Result<bool, DbWriteError> {
    connection
        .query_row(
            "SELECT COUNT(*) FROM sqlite_master WHERE type = 'table' AND name = 'heading_fts'",
            [],
            |row| row.get::<_, i64>(0),
        )
        .map(|count| count > 0)
        .map_err(|source| DbWriteError::ReadBack {
            operation: "heading_fts_table_exists",
            source,
        })
}

#[cfg(test)]
mod tests {
    use super::{
        DbWriteError, DbWriter, FileRecordInput, HeadingFtsRecord, HeadingRecord, KeywordRecord,
        LinkRecord, OutlinePathRecord, PropertyRecord, TagRecord, TodoKeywordRecord,
    };
    use crate::db::{
        open_in_memory_database_with_schema, sqlite_supports_fts5, SchemaDefinition,
        CURRENT_SCHEMA_VERSION,
    };
    use rusqlite::Connection;
    use std::path::PathBuf;

    type StoredLinkRow = (
        i64,
        String,
        String,
        String,
        Option<String>,
        String,
        Option<String>,
    );

    #[test]
    fn rebuild_of_one_file_is_idempotent() {
        let schema = SchemaDefinition::new(1, false);
        let mut connection =
            open_in_memory_database_with_schema(&schema).expect("database should open");
        let file = file_record("/tmp/project.org", 10, 100);

        let (first_file_id, ()) = DbWriter::rebuild_file(&mut connection, &file, |tx, file_id| {
            let level0_id =
                DbWriter::insert_level0_heading(tx, &level0_heading(file_id, "/tmp/project.org"))?;
            DbWriter::insert_headings(tx, &[child_heading(file_id, level0_id, 10, "Inbox")])?;
            DbWriter::insert_todo_keywords(
                tx,
                &[TodoKeywordRecord {
                    file_id,
                    keyword: "TODO".to_string(),
                    state_type: "open".to_string(),
                    shortcut: None,
                    sequence_no: 0,
                    source_kind: "config_default".to_string(),
                    source_keyword: None,
                    source_line_number: None,
                }],
            )?;
            Ok(())
        })
        .expect("first rebuild should succeed");

        let (second_file_id, ()) = DbWriter::rebuild_file(&mut connection, &file, |tx, file_id| {
            let level0_id =
                DbWriter::insert_level0_heading(tx, &level0_heading(file_id, "/tmp/project.org"))?;
            DbWriter::insert_headings(tx, &[child_heading(file_id, level0_id, 10, "Updated")])?;
            Ok(())
        })
        .expect("second rebuild should succeed");

        assert_eq!(first_file_id, second_file_id);

        let heading_count: i64 = connection
            .query_row(
                "SELECT COUNT(*) FROM headings WHERE file_id = ?1",
                [first_file_id],
                |row| row.get(0),
            )
            .expect("heading count should be queryable");
        let current_title: String = connection
            .query_row(
                "SELECT title FROM headings WHERE file_id = ?1 AND level = 1",
                [first_file_id],
                |row| row.get(0),
            )
            .expect("child title should be queryable");

        assert_eq!(heading_count, 2);
        assert_eq!(current_title, "Updated");
    }

    #[test]
    fn delete_file_data_removes_stale_rows_but_keeps_file_row() {
        let schema = SchemaDefinition::new(1, true);
        let connection =
            open_in_memory_database_with_schema(&schema).expect("database should open");
        let file_id = DbWriter::upsert_file(&connection, &file_record("/tmp/project.org", 10, 100))
            .expect("file should upsert");
        let level0_id = DbWriter::insert_level0_heading(
            &connection,
            &level0_heading(file_id, "/tmp/project.org"),
        )
        .expect("level0 should insert");
        let child_id = DbWriter::insert_headings(
            &connection,
            &[HeadingRecord {
                id: Some(42),
                ..child_heading(file_id, level0_id, 10, "Inbox")
            }],
        )
        .expect("child should insert")[0];

        DbWriter::insert_todo_keywords(
            &connection,
            &[TodoKeywordRecord {
                file_id,
                keyword: "TODO".to_string(),
                state_type: "open".to_string(),
                shortcut: Some('t'),
                sequence_no: 0,
                source_kind: "config_default".to_string(),
                source_keyword: None,
                source_line_number: None,
            }],
        )
        .expect("todo keyword should insert");
        DbWriter::insert_tags(
            &connection,
            &[TagRecord {
                heading_id: child_id,
                tag: "rust".to_string(),
            }],
        )
        .expect("tag should insert");
        DbWriter::insert_keywords(
            &connection,
            &[KeywordRecord {
                heading_id: level0_id,
                keyword: "TITLE".to_string(),
                value: Some("Project".to_string()),
                line_number: Some(1),
            }],
        )
        .expect("keyword should insert");
        DbWriter::insert_properties(
            &connection,
            &[PropertyRecord {
                heading_id: child_id,
                key: "CUSTOM_ID".to_string(),
                value: Some("inbox".to_string()),
                source: "property_drawer".to_string(),
                append: false,
                line_number: Some(4),
            }],
        )
        .expect("property should insert");
        DbWriter::insert_outline_path(
            &connection,
            &[OutlinePathRecord {
                heading_id: child_id,
                file_id,
                parent_id: Some(level0_id),
                depth: 1,
                materialized_path: "0000.0001".to_string(),
                breadcrumbs_json: "[\"/tmp/project.org\",\"Inbox\"]".to_string(),
            }],
        )
        .expect("outline path should insert");
        connection
            .execute(
                "INSERT INTO heading_bodies (heading_id, body_text, body_byte_start, body_byte_end)
                 VALUES (?1, ?2, ?3, ?4)",
                (child_id, "Body", 12_i64, 16_i64),
            )
            .expect("body should insert");
        DbWriter::insert_links(
            &connection,
            &[LinkRecord {
                id: None,
                file_id,
                heading_id: child_id,
                byte_start: 17,
                // "[[file:target]]" is 15 bytes, so [17, 32) is the correct half-open range.
                byte_end: 32,
                line: 3,
                source_context: "normal".to_string(),
                format: "bracket".to_string(),
                raw: "[[file:target]]".to_string(),
                raw_target: "file:target".to_string(),
                raw_description: None,
                link_type: "file".to_string(),
                path: "target".to_string(),
                search_option: None,
            }],
        )
        .expect("link should insert");

        if sqlite_supports_fts5(&connection) {
            DbWriter::insert_heading_fts(
                &connection,
                &[HeadingFtsRecord {
                    heading_id: child_id,
                    title: "Inbox".to_string(),
                    body: String::new(),
                }],
            )
            .expect("fts row should insert");
        }

        DbWriter::delete_file_data(&connection, file_id).expect("cleanup should succeed");

        assert_eq!(count(&connection, "SELECT COUNT(*) FROM files"), 1);
        assert_eq!(count(&connection, "SELECT COUNT(*) FROM headings"), 0);
        assert_eq!(count(&connection, "SELECT COUNT(*) FROM todo_keywords"), 0);
        assert_eq!(count(&connection, "SELECT COUNT(*) FROM tags"), 0);
        assert_eq!(count(&connection, "SELECT COUNT(*) FROM keywords"), 0);
        assert_eq!(count(&connection, "SELECT COUNT(*) FROM properties"), 0);
        assert_eq!(count(&connection, "SELECT COUNT(*) FROM outline_path"), 0);
        assert_eq!(count(&connection, "SELECT COUNT(*) FROM heading_bodies"), 0);
        assert_eq!(count(&connection, "SELECT COUNT(*) FROM links"), 0);
        if sqlite_supports_fts5(&connection) {
            assert_eq!(count(&connection, "SELECT COUNT(*) FROM heading_fts"), 0);
        }
    }

    #[test]
    fn failed_rebuild_rolls_back_inserted_rows() {
        let schema = SchemaDefinition::new(1, false);
        let mut connection =
            open_in_memory_database_with_schema(&schema).expect("database should open");
        let file = file_record("/tmp/project.org", 10, 100);

        DbWriter::rebuild_file(&mut connection, &file, |tx, file_id| {
            let level0_id =
                DbWriter::insert_level0_heading(tx, &level0_heading(file_id, "/tmp/project.org"))?;
            DbWriter::insert_headings(tx, &[child_heading(file_id, level0_id, 10, "Inbox")])?;
            Ok(())
        })
        .expect("seed rebuild should succeed");

        let error = DbWriter::rebuild_file(&mut connection, &file, |tx, file_id| {
            let level0_id =
                DbWriter::insert_level0_heading(tx, &level0_heading(file_id, "/tmp/project.org"))?;
            DbWriter::insert_headings(
                tx,
                &[
                    child_heading(file_id, level0_id, 10, "Duplicate"),
                    child_heading(file_id, level0_id, 10, "Duplicate again"),
                ],
            )?;
            Ok(())
        })
        .expect_err("invalid rebuild should fail");

        match error {
            DbWriteError::Write { .. } => {}
            other => panic!("unexpected error variant: {other}"),
        }

        let heading_count: i64 = connection
            .query_row("SELECT COUNT(*) FROM headings", [], |row| row.get(0))
            .expect("heading count should succeed");
        let child_title: String = connection
            .query_row("SELECT title FROM headings WHERE level = 1", [], |row| {
                row.get(0)
            })
            .expect("child title should succeed");

        assert_eq!(heading_count, 2);
        assert_eq!(child_title, "Inbox");
    }

    #[test]
    fn insert_links_persists_phase3_source_fact_fields() {
        let schema = SchemaDefinition::new(CURRENT_SCHEMA_VERSION, false);
        let connection =
            open_in_memory_database_with_schema(&schema).expect("database should open");
        let file_id = DbWriter::upsert_file(&connection, &file_record("/tmp/project.org", 10, 100))
            .expect("file should upsert");
        let level0_id = DbWriter::insert_level0_heading(
            &connection,
            &level0_heading(file_id, "/tmp/project.org"),
        )
        .expect("level0 should insert");

        DbWriter::insert_links(
            &connection,
            &[
                LinkRecord {
                    id: Some(11),
                    file_id,
                    heading_id: level0_id,
                    byte_start: 0,
                    // "[[file:notes.org::42]]" is 22 bytes, so [0, 22) is correct.
                    byte_end: 22,
                    line: 1,
                    source_context: "property_drawer".to_string(),
                    format: "bracket".to_string(),
                    raw: "[[file:notes.org::42]]".to_string(),
                    raw_target: "file:notes.org::42".to_string(),
                    raw_description: None,
                    link_type: "file".to_string(),
                    path: "notes.org".to_string(),
                    search_option: Some("42".to_string()),
                },
                LinkRecord {
                    id: Some(12),
                    file_id,
                    heading_id: level0_id,
                    byte_start: 30,
                    // "id:abc123" is 9 bytes, so [30, 39) is correct.
                    byte_end: 39,
                    line: 2,
                    source_context: "drawer".to_string(),
                    format: "plain".to_string(),
                    raw: "id:abc123".to_string(),
                    raw_target: "id:abc123".to_string(),
                    raw_description: None,
                    link_type: "id".to_string(),
                    path: "abc123".to_string(),
                    search_option: None,
                },
            ],
        )
        .expect("links should insert");

        let rows: Vec<StoredLinkRow> = {
            let mut statement = connection
                .prepare(
                    "SELECT line, source_context, raw_target, path, search_option, format, raw_description
                     FROM links
                     ORDER BY id",
                )
                .expect("query should prepare");
            statement
                .query_map([], |row| {
                    Ok((
                        row.get(0)?,
                        row.get(1)?,
                        row.get(2)?,
                        row.get(3)?,
                        row.get(4)?,
                        row.get(5)?,
                        row.get(6)?,
                    ))
                })
                .expect("query should run")
                .collect::<Result<Vec<_>, _>>()
                .expect("rows should collect")
        };

        assert_eq!(
            rows,
            vec![
                (
                    1,
                    "property_drawer".to_string(),
                    "file:notes.org::42".to_string(),
                    "notes.org".to_string(),
                    Some("42".to_string()),
                    "bracket".to_string(),
                    None,
                ),
                (
                    2,
                    "drawer".to_string(),
                    "id:abc123".to_string(),
                    "abc123".to_string(),
                    None,
                    "plain".to_string(),
                    None,
                ),
            ]
        );

        let deferred_target_count: i64 = connection
            .query_row(
                "SELECT COUNT(*)
                 FROM links
                 WHERE path_absolute IS NOT NULL
                    OR target_file_id IS NOT NULL
                    OR target_heading_id IS NOT NULL
                    OR target_custom_id IS NOT NULL
                    OR target_id IS NOT NULL
                    OR resolution_status IS NOT NULL
                    OR resolution_diagnostic IS NOT NULL",
                [],
                |row| row.get(0),
            )
            .expect("deferred target fields should be queryable");
        assert_eq!(deferred_target_count, 0);
    }

    #[test]
    fn insert_links_rejects_negative_byte_start_and_schema_rejects_missing_link_type() {
        let schema = SchemaDefinition::new(CURRENT_SCHEMA_VERSION, false);
        let connection =
            open_in_memory_database_with_schema(&schema).expect("database should open");
        let file_id = DbWriter::upsert_file(&connection, &file_record("/tmp/project.org", 10, 100))
            .expect("file should upsert");
        let level0_id = DbWriter::insert_level0_heading(
            &connection,
            &level0_heading(file_id, "/tmp/project.org"),
        )
        .expect("level0 should insert");

        let negative_byte_start = DbWriter::insert_links(
            &connection,
            &[LinkRecord {
                id: Some(21),
                file_id,
                heading_id: level0_id,
                byte_start: -1,
                byte_end: 8,
                line: 1,
                source_context: "normal".to_string(),
                format: "plain".to_string(),
                raw: "id:abc".to_string(),
                raw_target: "id:abc".to_string(),
                raw_description: None,
                link_type: "id".to_string(),
                path: "abc".to_string(),
                search_option: None,
            }],
        )
        .expect_err("negative link byte_start should fail");
        match negative_byte_start {
            DbWriteError::Write { .. } => {}
            other => panic!("unexpected error variant: {other}"),
        }

        let missing_link_type = connection
            .execute(
                "INSERT INTO links
                 (file_id, heading_id, byte_start, byte_end, line, source_context, format,
                  raw, raw_target, raw_description, link_type, path, search_option)
                 VALUES (?1, ?2, ?3, ?4, ?5, ?6, ?7, ?8, ?9, ?10, ?11, ?12, ?13)",
                (
                    file_id,
                    level0_id,
                    0_i64,
                    6_i64,
                    1_i64,
                    "normal",
                    "plain",
                    "id:abc",
                    "id:abc",
                    Option::<String>::None,
                    Option::<String>::None,
                    "abc",
                    Option::<String>::None,
                ),
            )
            .expect_err("NULL link_type should fail");
        assert!(missing_link_type.to_string().contains("NOT NULL"));
    }

    #[test]
    fn insert_heading_fts_is_noop_when_schema_disables_fts() {
        let schema = SchemaDefinition::new(1, false);
        let connection =
            open_in_memory_database_with_schema(&schema).expect("database should open");

        DbWriter::insert_heading_fts(
            &connection,
            &[HeadingFtsRecord {
                heading_id: 1,
                title: "Inbox".to_string(),
                body: String::new(),
            }],
        )
        .expect("fts insert should be a no-op when disabled");

        let count: i64 = connection
            .query_row(
                "SELECT COUNT(*) FROM sqlite_master WHERE type = 'table' AND name = 'heading_fts'",
                [],
                |row| row.get(0),
            )
            .expect("sqlite_master query should succeed");

        assert_eq!(count, 0);
    }

    fn file_record(path: &str, mtime_ns: i64, size: i64) -> FileRecordInput {
        FileRecordInput {
            path: PathBuf::from(path),
            mtime_ns,
            size,
            content_hash: None,
            indexed_at: None,
        }
    }

    fn level0_heading(file_id: i64, path: &str) -> HeadingRecord {
        HeadingRecord {
            id: None,
            file_id,
            parent_id: None,
            level: 0,
            line_number: None,
            byte_start: -1,
            byte_end: 100,
            title: path.to_string(),
            title_raw: None,
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
        }
    }

    fn child_heading(file_id: i64, parent_id: i64, byte_start: i64, title: &str) -> HeadingRecord {
        HeadingRecord {
            id: None,
            file_id,
            parent_id: Some(parent_id),
            level: 1,
            line_number: Some(2),
            byte_start,
            byte_end: byte_start + 8,
            title: title.to_string(),
            title_raw: Some(title.to_string()),
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
        }
    }

    fn count(connection: &Connection, sql: &str) -> i64 {
        connection
            .query_row(sql, [], |row| row.get(0))
            .expect("count query should succeed")
    }
}
