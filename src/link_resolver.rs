use std::{
    collections::{BTreeSet, HashMap},
    path::{Component, Path, PathBuf},
};

use rusqlite::{params, Connection};

use crate::db::DbWriteError;

pub(crate) const UNSUPPORTED_DIAGNOSTIC: &str = "no resolver implemented for link_type";
pub(crate) const FILE_MISSING_DIAGNOSTIC: &str = "file target is missing from the indexed universe";
pub(crate) const FILE_OUTSIDE_UNIVERSE_DIAGNOSTIC: &str =
    "file target is outside the indexed universe";
const FILE_PATH_UNSUPPORTED_DIAGNOSTIC: &str = "file target path could not be normalized safely";

#[derive(Debug, Default)]
pub(crate) struct LinkResolver;

#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub(crate) struct IndexedUniverse {
    recursive_roots: BTreeSet<PathBuf>,
    exact_paths: BTreeSet<PathBuf>,
}

#[derive(Debug)]
struct StoredLink {
    id: i64,
    link_type: String,
    path: String,
    source_file_path: PathBuf,
}

#[derive(Debug, Default)]
struct KnownFiles {
    by_path: HashMap<PathBuf, i64>,
}

impl LinkResolver {
    pub(crate) fn resolve_all(
        connection: &Connection,
        indexed_universe: &IndexedUniverse,
    ) -> Result<(), DbWriteError> {
        Self::reset_resolution_fields(connection)?;
        let known_files = Self::load_known_files(connection)?;
        let links = Self::load_links(connection)?;
        for link in links {
            Self::resolve_link(connection, &link, indexed_universe, &known_files)?;
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
                "SELECT links.id, links.link_type, links.path, files.path
                 FROM links
                 INNER JOIN files ON files.id = links.file_id
                 ORDER BY links.file_id, links.byte_start, links.id",
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
                    path: row.get(2)?,
                    source_file_path: PathBuf::from(row.get::<_, String>(3)?),
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

    fn load_known_files(connection: &Connection) -> Result<KnownFiles, DbWriteError> {
        let mut statement = connection
            .prepare("SELECT id, path FROM files")
            .map_err(|source| DbWriteError::Write {
                operation: "link_resolver.load_known_files.prepare",
                source,
            })?;
        let rows = statement
            .query_map([], |row| {
                Ok((
                    row.get::<_, i64>(0)?,
                    PathBuf::from(row.get::<_, String>(1)?),
                ))
            })
            .map_err(|source| DbWriteError::Write {
                operation: "link_resolver.load_known_files.query",
                source,
            })?;
        let mut known_files = KnownFiles::default();
        for row in rows {
            let (file_id, path) = row.map_err(|source| DbWriteError::Write {
                operation: "link_resolver.load_known_files.collect",
                source,
            })?;
            known_files.by_path.insert(path, file_id);
        }
        Ok(known_files)
    }

    fn resolve_link(
        connection: &Connection,
        link: &StoredLink,
        indexed_universe: &IndexedUniverse,
        known_files: &KnownFiles,
    ) -> Result<(), DbWriteError> {
        if link.link_type == "file" {
            return Self::resolve_file_link(connection, link, indexed_universe, known_files);
        }

        Self::mark_unsupported(connection, link.id)
    }

    fn resolve_file_link(
        connection: &Connection,
        link: &StoredLink,
        indexed_universe: &IndexedUniverse,
        known_files: &KnownFiles,
    ) -> Result<(), DbWriteError> {
        let home_dir = current_home_dir();
        let Some(path_absolute) = normalize_file_target_path(
            &link.source_file_path,
            Path::new(&link.path),
            home_dir.as_deref(),
        ) else {
            return Self::mark_file_path_unsupported(connection, link.id);
        };

        if let Some(target_file_id) = known_files.by_path.get(&path_absolute).copied() {
            return Self::mark_resolved_file(connection, link.id, &path_absolute, target_file_id);
        }

        if indexed_universe.contains(&path_absolute) {
            return Self::mark_broken_file(connection, link.id, &path_absolute);
        }

        Self::mark_unresolved_file(connection, link.id, &path_absolute)
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

    fn mark_resolved_file(
        connection: &Connection,
        link_id: i64,
        path_absolute: &Path,
        target_file_id: i64,
    ) -> Result<(), DbWriteError> {
        connection
            .execute(
                "UPDATE links
                 SET path_absolute = ?2,
                     target_file_id = ?3,
                     resolution_status = ?4,
                     resolution_diagnostic = NULL
                 WHERE id = ?1",
                params![
                    link_id,
                    path_absolute.to_string_lossy().to_string(),
                    target_file_id,
                    "resolved"
                ],
            )
            .map_err(|source| DbWriteError::Write {
                operation: "link_resolver.mark_resolved_file",
                source,
            })?;
        Ok(())
    }

    fn mark_broken_file(
        connection: &Connection,
        link_id: i64,
        path_absolute: &Path,
    ) -> Result<(), DbWriteError> {
        connection
            .execute(
                "UPDATE links
                 SET path_absolute = ?2,
                     target_file_id = NULL,
                     resolution_status = ?3,
                     resolution_diagnostic = ?4
                 WHERE id = ?1",
                params![
                    link_id,
                    path_absolute.to_string_lossy().to_string(),
                    "broken",
                    FILE_MISSING_DIAGNOSTIC
                ],
            )
            .map_err(|source| DbWriteError::Write {
                operation: "link_resolver.mark_broken_file",
                source,
            })?;
        Ok(())
    }

    fn mark_unresolved_file(
        connection: &Connection,
        link_id: i64,
        path_absolute: &Path,
    ) -> Result<(), DbWriteError> {
        connection
            .execute(
                "UPDATE links
                 SET path_absolute = ?2,
                     target_file_id = NULL,
                     resolution_status = ?3,
                     resolution_diagnostic = ?4
                 WHERE id = ?1",
                params![
                    link_id,
                    path_absolute.to_string_lossy().to_string(),
                    "unresolved",
                    FILE_OUTSIDE_UNIVERSE_DIAGNOSTIC
                ],
            )
            .map_err(|source| DbWriteError::Write {
                operation: "link_resolver.mark_unresolved_file",
                source,
            })?;
        Ok(())
    }

    fn mark_file_path_unsupported(
        connection: &Connection,
        link_id: i64,
    ) -> Result<(), DbWriteError> {
        connection
            .execute(
                "UPDATE links
                 SET path_absolute = NULL,
                     target_file_id = NULL,
                     resolution_status = ?2,
                     resolution_diagnostic = ?3
                 WHERE id = ?1",
                params![link_id, "unsupported", FILE_PATH_UNSUPPORTED_DIAGNOSTIC],
            )
            .map_err(|source| DbWriteError::Write {
                operation: "link_resolver.mark_file_path_unsupported",
                source,
            })?;
        Ok(())
    }
}

impl IndexedUniverse {
    pub(crate) fn add_recursive_root(&mut self, path: PathBuf) {
        self.recursive_roots.insert(path);
    }

    pub(crate) fn add_exact_path(&mut self, path: PathBuf) {
        self.exact_paths.insert(path);
    }

    fn contains(&self, path: &Path) -> bool {
        self.exact_paths.contains(path)
            || self
                .recursive_roots
                .iter()
                .any(|root| path.starts_with(root))
    }
}

fn current_home_dir() -> Option<PathBuf> {
    std::env::var_os("HOME")
        .filter(|value| !value.is_empty())
        .map(PathBuf::from)
}

fn normalize_file_target_path(
    source_file_path: &Path,
    raw_path: &Path,
    home_dir: Option<&Path>,
) -> Option<PathBuf> {
    let resolved = if let Some(expanded) = expand_home_directory(raw_path, home_dir) {
        expanded
    } else if raw_path.is_absolute() {
        raw_path.to_path_buf()
    } else {
        let source_dir = source_file_path.parent()?;
        source_dir.join(raw_path)
    };

    let normalized = normalize_absolute_path(resolved);
    normalized.is_absolute().then_some(normalized)
}

fn expand_home_directory(path: &Path, home_dir: Option<&Path>) -> Option<PathBuf> {
    let mut components = path.components();
    let Some(Component::Normal(first_component)) = components.next() else {
        return None;
    };
    if first_component != "~" {
        return None;
    }

    let home_dir = home_dir?;
    let mut resolved = home_dir.to_path_buf();
    for component in components {
        resolved.push(component.as_os_str());
    }
    Some(resolved)
}

fn normalize_absolute_path(path: PathBuf) -> PathBuf {
    let mut normalized = PathBuf::new();

    for component in path.components() {
        match component {
            Component::CurDir => {}
            Component::ParentDir => {
                if !normalized.pop() && !normalized.is_absolute() {
                    normalized.push(component.as_os_str());
                }
            }
            _ => normalized.push(component.as_os_str()),
        }
    }

    normalized
}

#[cfg(test)]
mod tests {
    use super::{
        normalize_file_target_path, IndexedUniverse, LinkResolver, FILE_MISSING_DIAGNOSTIC,
        FILE_OUTSIDE_UNIVERSE_DIAGNOSTIC, UNSUPPORTED_DIAGNOSTIC,
    };
    use crate::db::{
        open_in_memory_database_with_schema, SchemaDefinition, CURRENT_SCHEMA_VERSION,
    };
    use rusqlite::{params, Connection};
    use std::path::{Path, PathBuf};

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

        let universe = IndexedUniverse::default();
        LinkResolver::resolve_all(&connection, &universe).expect("resolution should succeed");

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

    #[test]
    fn resolve_all_resolves_known_file_targets() {
        let schema = SchemaDefinition::new(CURRENT_SCHEMA_VERSION, false);
        let connection =
            open_in_memory_database_with_schema(&schema).expect("database should open");
        seed_file_link_fixture(
            &connection,
            "/tmp/source.org",
            "[[file:target.org]]",
            "file",
            "target.org",
        );
        seed_known_target_file(&connection, "/tmp/target.org", 2);

        let mut universe = IndexedUniverse::default();
        universe.add_exact_path(PathBuf::from("/tmp/source.org"));
        universe.add_exact_path(PathBuf::from("/tmp/target.org"));

        LinkResolver::resolve_all(&connection, &universe).expect("resolution should succeed");

        let row: (Option<String>, Option<i64>, Option<String>, Option<String>) = connection
            .query_row(
                "SELECT path_absolute, target_file_id, resolution_status, resolution_diagnostic
                 FROM links
                 WHERE id = 1",
                [],
                |row| Ok((row.get(0)?, row.get(1)?, row.get(2)?, row.get(3)?)),
            )
            .expect("resolved row should load");
        assert_eq!(
            row,
            (
                Some("/tmp/target.org".to_string()),
                Some(2_i64),
                Some("resolved".to_string()),
                None,
            )
        );
    }

    #[test]
    fn resolve_all_marks_missing_file_targets_inside_universe_broken() {
        let schema = SchemaDefinition::new(CURRENT_SCHEMA_VERSION, false);
        let connection =
            open_in_memory_database_with_schema(&schema).expect("database should open");
        seed_file_link_fixture(
            &connection,
            "/tmp/source.org",
            "[[file:missing.org]]",
            "file",
            "missing.org",
        );

        let mut universe = IndexedUniverse::default();
        universe.add_recursive_root(PathBuf::from("/tmp"));

        LinkResolver::resolve_all(&connection, &universe).expect("resolution should succeed");

        let row: (Option<String>, Option<i64>, Option<String>, Option<String>) = connection
            .query_row(
                "SELECT path_absolute, target_file_id, resolution_status, resolution_diagnostic
                 FROM links
                 WHERE id = 1",
                [],
                |row| Ok((row.get(0)?, row.get(1)?, row.get(2)?, row.get(3)?)),
            )
            .expect("broken row should load");
        assert_eq!(
            row,
            (
                Some("/tmp/missing.org".to_string()),
                None,
                Some("broken".to_string()),
                Some(FILE_MISSING_DIAGNOSTIC.to_string()),
            )
        );
    }

    #[test]
    fn resolve_all_marks_external_file_targets_unresolved() {
        let schema = SchemaDefinition::new(CURRENT_SCHEMA_VERSION, false);
        let connection =
            open_in_memory_database_with_schema(&schema).expect("database should open");
        seed_file_link_fixture(
            &connection,
            "/tmp/source.org",
            "[[file:/outside/world.org]]",
            "file",
            "/outside/world.org",
        );

        let mut universe = IndexedUniverse::default();
        universe.add_exact_path(PathBuf::from("/tmp/source.org"));

        LinkResolver::resolve_all(&connection, &universe).expect("resolution should succeed");

        let row: (Option<String>, Option<i64>, Option<String>, Option<String>) = connection
            .query_row(
                "SELECT path_absolute, target_file_id, resolution_status, resolution_diagnostic
                 FROM links
                 WHERE id = 1",
                [],
                |row| Ok((row.get(0)?, row.get(1)?, row.get(2)?, row.get(3)?)),
            )
            .expect("unresolved row should load");
        assert_eq!(
            row,
            (
                Some("/outside/world.org".to_string()),
                None,
                Some("unresolved".to_string()),
                Some(FILE_OUTSIDE_UNIVERSE_DIAGNOSTIC.to_string()),
            )
        );
    }

    #[test]
    fn normalize_file_target_path_expands_home_and_collapses_dot_segments() {
        let source_file_path = Path::new("/tmp/project/source.org");
        let home_dir = Path::new("/home/tester");

        let relative = normalize_file_target_path(
            source_file_path,
            Path::new("../notes/./a.org"),
            Some(home_dir),
        )
        .expect("relative target should normalize");
        let home_relative = normalize_file_target_path(
            source_file_path,
            Path::new("~/docs/./b.org"),
            Some(home_dir),
        )
        .expect("home-relative target should normalize");

        assert_eq!(relative, PathBuf::from("/tmp/notes/a.org"));
        assert_eq!(home_relative, PathBuf::from("/home/tester/docs/b.org"));
    }

    fn seed_file_link_fixture(
        connection: &Connection,
        source_path: &str,
        raw: &str,
        link_type: &str,
        path: &str,
    ) {
        connection
            .execute(
                "INSERT INTO files (id, path, mtime_ns, size) VALUES (?1, ?2, ?3, ?4)",
                (1_i64, source_path, 10_i64, 20_i64),
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
                    source_path,
                    source_path,
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
                    i64::try_from(raw.len()).expect("raw length should fit in i64"),
                    1_i64,
                    "normal",
                    "bracket",
                    raw,
                    raw,
                    Option::<String>::None,
                    link_type,
                    path,
                    Option::<String>::None,
                ],
            )
            .expect("file link insert should succeed");
    }

    fn seed_known_target_file(connection: &Connection, path: &str, file_id: i64) {
        connection
            .execute(
                "INSERT INTO files (id, path, mtime_ns, size) VALUES (?1, ?2, ?3, ?4)",
                (file_id, path, 30_i64, 40_i64),
            )
            .expect("target file insert should succeed");
        connection
            .execute(
                "INSERT INTO headings
                 (id, file_id, parent_id, level, byte_start, byte_end, title, title_raw)
                 VALUES (?1, ?2, ?3, ?4, ?5, ?6, ?7, ?8)",
                (
                    file_id,
                    file_id,
                    Option::<i64>::None,
                    0_i64,
                    -1_i64,
                    40_i64,
                    path,
                    path,
                ),
            )
            .expect("target heading insert should succeed");
    }
}
