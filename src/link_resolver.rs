use rusqlite::{params, Connection};

use crate::db::DbWriteError;

pub(crate) const UNSUPPORTED_DIAGNOSTIC: &str = "no resolver implemented for link_type";

#[derive(Debug, Default)]
pub(crate) struct LinkResolver;

#[derive(Debug)]
struct StoredLink {
    id: i64,
    link_type: String,
}

impl LinkResolver {
    pub(crate) fn resolve_all(connection: &Connection) -> Result<(), DbWriteError> {
        Self::reset_resolution_fields(connection)?;
        let links = Self::load_links(connection)?;
        for link in links {
            Self::resolve_link(connection, &link)?;
        }
        Ok(())
    }

    fn reset_resolution_fields(connection: &Connection) -> Result<(), DbWriteError> {
        connection
            .execute(
                "UPDATE links
                 SET path_absolute = NULL,
                     target_file_id = NULL,
                     target_heading_id = NULL,
                     target_custom_id = NULL,
                     target_id = NULL,
                     resolution_status = NULL,
                     resolution_diagnostic = NULL",
                [],
            )
            .map_err(|source| DbWriteError::Write {
                operation: "link_resolver.reset_resolution_fields",
                source,
            })?;
        Ok(())
    }

    fn load_links(connection: &Connection) -> Result<Vec<StoredLink>, DbWriteError> {
        let mut statement = connection
            .prepare(
                "SELECT id, link_type
                 FROM links
                 ORDER BY file_id, byte_start, id",
            )
            .map_err(|source| DbWriteError::Write {
                operation: "link_resolver.load_links.prepare",
                source,
            })?;
        let rows = statement
            .query_map([], |row| {
                Ok(StoredLink {
                    id: row.get(0)?,
                    link_type: row.get(1)?,
                })
            })
            .map_err(|source| DbWriteError::Write {
                operation: "link_resolver.load_links.query",
                source,
            })?;
        rows.collect::<Result<Vec<_>, _>>()
            .map_err(|source| DbWriteError::Write {
                operation: "link_resolver.load_links.collect",
                source,
            })
    }

    fn resolve_link(connection: &Connection, link: &StoredLink) -> Result<(), DbWriteError> {
        let _link_type = link.link_type.as_str();
        Self::mark_unsupported(connection, link.id)
    }

    fn mark_unsupported(connection: &Connection, link_id: i64) -> Result<(), DbWriteError> {
        connection
            .execute(
                "UPDATE links
                 SET resolution_status = ?2,
                     resolution_diagnostic = ?3
                 WHERE id = ?1",
                params![link_id, "unsupported", UNSUPPORTED_DIAGNOSTIC],
            )
            .map_err(|source| DbWriteError::Write {
                operation: "link_resolver.mark_unsupported",
                source,
            })?;
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::{LinkResolver, UNSUPPORTED_DIAGNOSTIC};
    use crate::db::{
        open_in_memory_database_with_schema, SchemaDefinition, CURRENT_SCHEMA_VERSION,
    };
    use rusqlite::{params, Connection};

    type ResolvedLinkRow = (
        Option<String>,
        Option<i64>,
        Option<i64>,
        Option<String>,
        Option<String>,
        Option<String>,
        Option<String>,
        String,
        String,
    );

    #[test]
    fn resolve_all_resets_stale_resolver_owned_fields_before_marking_unsupported() {
        let schema = SchemaDefinition::new(CURRENT_SCHEMA_VERSION, false);
        let connection =
            open_in_memory_database_with_schema(&schema).expect("database should open");

        seed_link_fixture(&connection);
        seed_target_fixture(&connection);

        connection
            .execute(
                "UPDATE links
                 SET path_absolute = ?1,
                     target_file_id = ?2,
                     target_heading_id = ?3,
                     target_custom_id = ?4,
                     target_id = ?5,
                     resolution_status = ?6,
                     resolution_diagnostic = ?7
                 WHERE id = 1",
                params![
                    "/tmp/target.org",
                    2_i64,
                    2_i64,
                    "custom-1",
                    "id-1",
                    "resolved",
                    "stale diagnostic",
                ],
            )
            .expect("stale resolver-owned fields should update");

        LinkResolver::resolve_all(&connection).expect("resolution should succeed");

        let row: ResolvedLinkRow = connection
            .query_row(
                "SELECT path_absolute, target_file_id, target_heading_id, target_custom_id,
                        target_id, resolution_status, resolution_diagnostic, raw, raw_target
                 FROM links
                 WHERE id = 1",
                [],
                |row| {
                    Ok((
                        row.get(0)?,
                        row.get(1)?,
                        row.get(2)?,
                        row.get(3)?,
                        row.get(4)?,
                        row.get(5)?,
                        row.get(6)?,
                        row.get(7)?,
                        row.get(8)?,
                    ))
                },
            )
            .expect("resolved row should be queryable");

        assert_eq!(
            row,
            (
                None,
                None,
                None,
                None,
                None,
                Some("unsupported".to_string()),
                Some(UNSUPPORTED_DIAGNOSTIC.to_string()),
                "[[unknown:foo]]".to_string(),
                "unknown:foo".to_string(),
            )
        );
    }

    fn seed_link_fixture(connection: &Connection) {
        connection
            .execute(
                "INSERT INTO files (id, path, mtime_ns, size) VALUES (?1, ?2, ?3, ?4)",
                (1_i64, "/tmp/source.org", 10_i64, 20_i64),
            )
            .expect("source file insert should succeed");
        connection
            .execute(
                "INSERT INTO headings
                 (id, file_id, parent_id, level, byte_start, byte_end, title, title_raw)
                 VALUES (?1, ?2, ?3, ?4, ?5, ?6, ?7, ?8)",
                (
                    1_i64,
                    1_i64,
                    Option::<i64>::None,
                    0_i64,
                    -1_i64,
                    20_i64,
                    "/tmp/source.org",
                    "/tmp/source.org",
                ),
            )
            .expect("source heading insert should succeed");
        connection
            .execute(
                "INSERT INTO links
                 (id, file_id, heading_id, byte_start, byte_end, line, source_context, format,
                  raw, raw_target, raw_description, link_type, path, search_option)
                 VALUES (?1, ?2, ?3, ?4, ?5, ?6, ?7, ?8, ?9, ?10, ?11, ?12, ?13, ?14)",
                params![
                    1_i64,
                    1_i64,
                    1_i64,
                    0_i64,
                    15_i64,
                    1_i64,
                    "normal",
                    "bracket",
                    "[[unknown:foo]]",
                    "unknown:foo",
                    Option::<String>::None,
                    "unknown",
                    "foo",
                    Option::<String>::None,
                ],
            )
            .expect("source link insert should succeed");
    }

    fn seed_target_fixture(connection: &Connection) {
        connection
            .execute(
                "INSERT INTO files (id, path, mtime_ns, size) VALUES (?1, ?2, ?3, ?4)",
                (2_i64, "/tmp/target.org", 30_i64, 40_i64),
            )
            .expect("target file insert should succeed");
        connection
            .execute(
                "INSERT INTO headings
                 (id, file_id, parent_id, level, byte_start, byte_end, title, title_raw)
                 VALUES (?1, ?2, ?3, ?4, ?5, ?6, ?7, ?8)",
                (
                    2_i64,
                    2_i64,
                    Option::<i64>::None,
                    0_i64,
                    -1_i64,
                    40_i64,
                    "/tmp/target.org",
                    "/tmp/target.org",
                ),
            )
            .expect("target heading insert should succeed");
    }
}
