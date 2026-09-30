use std::{
    collections::{BTreeMap, BTreeSet, HashMap},
    path::{Component, Path, PathBuf},
};

use rusqlite::{params, Connection};

mod indexed_universe;
pub(crate) use indexed_universe::IndexedUniverse;

use crate::db::DbWriteError;
use crate::file_identity::{display_path, FileIdentity};

pub(crate) const UNSUPPORTED_DIAGNOSTIC: &str = "unsupported link type";
pub(crate) const FILE_MISSING_DIAGNOSTIC: &str = "missing in indexed universe";
pub(crate) const FILE_OUTSIDE_UNIVERSE_DIAGNOSTIC: &str = "outside indexed universe";
const FILE_PATH_UNSUPPORTED_DIAGNOSTIC: &str = "unsupported path form";
pub(crate) const HEADING_TITLE_MISSING_DIAGNOSTIC: &str = "heading not found";
pub(crate) const SAME_FILE_STAR_HEADING_MISSING_DIAGNOSTIC: &str = "same-file heading not found";
pub(crate) const CUSTOM_ID_MISSING_DIAGNOSTIC: &str = "custom id not found";
pub(crate) const ID_MISSING_DIAGNOSTIC: &str = "id not found";
pub(crate) const DUPLICATE_ID_DIAGNOSTIC: &str = "duplicate id";
const MISSING_SYNTHETIC_ROOT_DIAGNOSTIC: &str = "missing synthetic root heading";
const DUPLICATE_SYNTHETIC_ROOT_DIAGNOSTIC: &str = "duplicate synthetic root headings";

#[derive(Debug, Default)]
pub(crate) struct LinkResolver;

#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub(crate) struct LinkResolutionReport {
    pub(crate) changed_source_paths: BTreeSet<String>,
}

#[derive(Debug)]
struct StoredLink {
    id: i64,
    source_file_id: i64,
    link_type: String,
    path: String,
    search_option: Option<String>,
    source_file_path: PathBuf,
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct StoredResolutionState {
    source_path: String,
    path_absolute: Option<String>,
    target_file_id: Option<i64>,
    target_heading_id: Option<i64>,
    target_custom_id: Option<String>,
    target_id: Option<String>,
    resolution_status: Option<String>,
    resolution_diagnostic: Option<String>,
}

#[derive(Debug, Default)]
struct KnownFiles {
    by_path: HashMap<PathBuf, i64>,
}

#[derive(Debug, Default)]
struct ResolutionIndex {
    global_ids: HashMap<String, Vec<(i64, i64)>>,
    titles: HashMap<(i64, String), Vec<i64>>,
    custom_ids: HashMap<(i64, String), Vec<i64>>,
    roots: HashMap<i64, Vec<i64>>,
}

impl ResolutionIndex {
    fn load(connection: &Connection) -> Result<Self, DbWriteError> {
        let mut index = Self::default();
        let err = |operation| move |source| DbWriteError::Write { operation, source };
        let mut statement = connection
            .prepare(
                "SELECT headings.file_id, headings.id, properties.value
                 FROM properties
                 INNER JOIN headings ON headings.id = properties.heading_id
                 WHERE headings.level > 0 AND properties.key = 'ID'
                 ORDER BY headings.file_id, headings.byte_start, headings.id, properties.id",
            )
            .map_err(err("link_resolver.index.ids.prepare"))?;
        let rows = statement
            .query_map([], |row| {
                Ok((
                    row.get::<_, i64>(0)?,
                    row.get::<_, i64>(1)?,
                    row.get::<_, Option<String>>(2)?,
                ))
            })
            .map_err(err("link_resolver.index.ids.query"))?;
        for row in rows {
            let (file_id, heading_id, value) = row.map_err(err("link_resolver.index.ids.row"))?;
            if let Some(value) = value {
                index
                    .global_ids
                    .entry(value.to_lowercase())
                    .or_default()
                    .push((file_id, heading_id));
            }
        }
        drop(statement);

        let mut statement = connection
            .prepare(
                "SELECT file_id, id, title FROM headings
                 WHERE level > 0 ORDER BY file_id, byte_start, id",
            )
            .map_err(err("link_resolver.index.titles.prepare"))?;
        let rows = statement
            .query_map([], |row| {
                Ok((
                    row.get::<_, i64>(0)?,
                    row.get::<_, i64>(1)?,
                    row.get::<_, String>(2)?,
                ))
            })
            .map_err(err("link_resolver.index.titles.query"))?;
        for row in rows {
            let (file_id, id, title) = row.map_err(err("link_resolver.index.titles.row"))?;
            index
                .titles
                .entry((file_id, title.to_lowercase()))
                .or_default()
                .push(id);
        }
        drop(statement);

        let mut statement = connection
            .prepare(
                "SELECT headings.file_id, headings.id, properties.value
                 FROM properties
                 INNER JOIN headings ON headings.id = properties.heading_id
                 WHERE headings.level > 0 AND properties.key = 'CUSTOM_ID'
                 ORDER BY headings.file_id, headings.byte_start, headings.id, properties.id",
            )
            .map_err(err("link_resolver.index.custom_ids.prepare"))?;
        let rows = statement
            .query_map([], |row| {
                Ok((
                    row.get::<_, i64>(0)?,
                    row.get::<_, i64>(1)?,
                    row.get::<_, Option<String>>(2)?,
                ))
            })
            .map_err(err("link_resolver.index.custom_ids.query"))?;
        for row in rows {
            let (file_id, id, value) = row.map_err(err("link_resolver.index.custom_ids.row"))?;
            if let Some(value) = value {
                index
                    .custom_ids
                    .entry((file_id, value.to_lowercase()))
                    .or_default()
                    .push(id);
            }
        }
        drop(statement);

        let mut statement = connection
            .prepare(
                "SELECT file_id, id FROM headings
                 WHERE level = 0 AND parent_id IS NULL ORDER BY file_id, byte_start, id",
            )
            .map_err(err("link_resolver.index.roots.prepare"))?;
        let rows = statement
            .query_map([], |row| Ok((row.get::<_, i64>(0)?, row.get::<_, i64>(1)?)))
            .map_err(err("link_resolver.index.roots.query"))?;
        for row in rows {
            let (file_id, id) = row.map_err(err("link_resolver.index.roots.row"))?;
            index.roots.entry(file_id).or_default().push(id);
        }
        Ok(index)
    }

    fn global_id_targets(&self, target: &str) -> &[(i64, i64)] {
        self.global_ids
            .get(&target.to_lowercase())
            .map_or(&[], Vec::as_slice)
    }

    fn headings_by_title(&self, file_id: i64, title: &str) -> &[i64] {
        self.titles
            .get(&(file_id, title.to_lowercase()))
            .map_or(&[], Vec::as_slice)
    }

    fn custom_id_headings(&self, file_id: i64, target: &str) -> &[i64] {
        self.custom_ids
            .get(&(file_id, target.to_lowercase()))
            .map_or(&[], Vec::as_slice)
    }

    fn root_headings(&self, file_id: i64) -> &[i64] {
        self.roots.get(&file_id).map_or(&[], Vec::as_slice)
    }
}

impl LinkResolver {
    pub(crate) fn resolve_all(
        connection: &Connection,
        indexed_universe: &IndexedUniverse,
    ) -> Result<LinkResolutionReport, DbWriteError> {
        let before = Self::load_resolution_states(connection)?;
        Self::reset_resolution_fields(connection)?;
        let known_files = Self::load_known_files(connection)?;
        let links = Self::load_links(connection)?;
        let index = ResolutionIndex::load(connection)?;
        for link in links {
            Self::resolve_link(connection, &link, indexed_universe, &known_files, &index)?;
        }
        let after = Self::load_resolution_states(connection)?;
        let changed_source_paths = after
            .iter()
            .filter(|(link_id, state)| before.get(*link_id) != Some(*state))
            .map(|(_link_id, state)| state.source_path.clone())
            .collect();
        Ok(LinkResolutionReport {
            changed_source_paths,
        })
    }

    fn load_resolution_states(
        connection: &Connection,
    ) -> Result<BTreeMap<i64, StoredResolutionState>, DbWriteError> {
        let mut statement = connection
            .prepare(
                "SELECT
                    links.id,
                    files.path,
                    links.path_absolute,
                    links.target_file_id,
                    links.target_heading_id,
                    links.target_custom_id,
                    links.target_id,
                    links.resolution_status,
                    links.resolution_diagnostic
                 FROM links
                 INNER JOIN files ON files.id = links.file_id
                 ORDER BY links.id",
            )
            .map_err(|source| DbWriteError::Write {
                operation: "link_resolver.load_resolution_states.prepare",
                source,
            })?;
        let rows = statement
            .query_map([], |row| {
                Ok((
                    row.get::<_, i64>(0)?,
                    StoredResolutionState {
                        source_path: row.get(1)?,
                        path_absolute: row.get(2)?,
                        target_file_id: row.get(3)?,
                        target_heading_id: row.get(4)?,
                        target_custom_id: row.get(5)?,
                        target_id: row.get(6)?,
                        resolution_status: row.get(7)?,
                        resolution_diagnostic: row.get(8)?,
                    },
                ))
            })
            .map_err(|source| DbWriteError::Write {
                operation: "link_resolver.load_resolution_states.query",
                source,
            })?;
        rows.collect::<Result<BTreeMap<_, _>, _>>()
            .map_err(|source| DbWriteError::Write {
                operation: "link_resolver.load_resolution_states.collect",
                source,
            })
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
                "SELECT links.id, links.file_id, links.link_type, links.path, links.search_option, files.path, files.identity
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
                    source_file_id: row.get(1)?,
                    link_type: row.get(2)?,
                    path: row.get(3)?,
                    search_option: row.get(4)?,
                    source_file_path: {
                        let display_path = row.get::<_, String>(5)?;
                        let identity = row.get::<_, Option<Vec<u8>>>(6)?;
                        identity
                            .and_then(FileIdentity::from_stored_bytes)
                            .and_then(|identity| identity.to_path())
                            .unwrap_or_else(|| PathBuf::from(display_path))
                    },
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
            .prepare("SELECT id, path, identity FROM files")
            .map_err(|source| DbWriteError::Write {
                operation: "link_resolver.load_known_files.prepare",
                source,
            })?;
        let rows = statement
            .query_map([], |row| {
                Ok((
                    row.get::<_, i64>(0)?,
                    row.get::<_, String>(1)?,
                    row.get::<_, Option<Vec<u8>>>(2)?,
                ))
            })
            .map_err(|source| DbWriteError::Write {
                operation: "link_resolver.load_known_files.query",
                source,
            })?;
        let mut known_files = KnownFiles::default();
        for row in rows {
            let (file_id, display_path, identity) = row.map_err(|source| DbWriteError::Write {
                operation: "link_resolver.load_known_files.collect",
                source,
            })?;
            let path = identity
                .and_then(FileIdentity::from_stored_bytes)
                .and_then(|identity| identity.to_path())
                .unwrap_or_else(|| PathBuf::from(display_path));
            known_files.by_path.insert(path, file_id);
        }
        Ok(known_files)
    }

    fn resolve_link(
        connection: &Connection,
        link: &StoredLink,
        indexed_universe: &IndexedUniverse,
        known_files: &KnownFiles,
        index: &ResolutionIndex,
    ) -> Result<(), DbWriteError> {
        // `file+sys` and `file+emacs` only change how Org opens the target.
        if matches!(link.link_type.as_str(), "file" | "file+sys" | "file+emacs") {
            return Self::resolve_file_link(connection, link, indexed_universe, known_files, index);
        }
        if link.link_type == "custom-id" {
            return Self::resolve_same_file_custom_id_link(connection, link, index);
        }
        if link.link_type == "id" {
            return Self::resolve_org_id_link(connection, link, index);
        }
        if link.link_type == "fuzzy" {
            return Self::resolve_same_file_fuzzy_link(connection, link, index);
        }

        Self::write_resolution(connection, link.id, Resolution::unsupported())
    }

    fn resolve_file_link(
        connection: &Connection,
        link: &StoredLink,
        indexed_universe: &IndexedUniverse,
        known_files: &KnownFiles,
        index: &ResolutionIndex,
    ) -> Result<(), DbWriteError> {
        let home_dir = current_home_dir();
        let Some(path_absolute) = normalize_file_target_path(
            &link.source_file_path,
            Path::new(&link.path),
            home_dir.as_deref(),
        ) else {
            return Self::write_resolution(
                connection,
                link.id,
                Resolution::file_path_unsupported(),
            );
        };

        if let Some(target_file_id) = known_files.by_path.get(&path_absolute).copied() {
            return Self::resolve_file_target(
                connection,
                link,
                &path_absolute,
                target_file_id,
                index,
            );
        }

        if indexed_universe.contains(&path_absolute) {
            return Self::write_resolution(
                connection,
                link.id,
                Resolution::broken_file(&path_absolute),
            );
        }

        Self::write_resolution(
            connection,
            link.id,
            Resolution::unresolved_file(&path_absolute),
        )
    }

    fn resolve_same_file_fuzzy_link(
        connection: &Connection,
        link: &StoredLink,
        index: &ResolutionIndex,
    ) -> Result<(), DbWriteError> {
        if let Some(custom_id_target) = same_file_fuzzy_custom_id_target(link.path.as_str()) {
            return Self::resolve_same_file_custom_id_target(
                connection,
                link,
                &custom_id_target,
                index,
            );
        }

        let Some(heading_title) = same_file_fuzzy_star_heading_target(link.path.as_str()) else {
            return Self::write_resolution(connection, link.id, Resolution::unsupported());
        };

        let heading_ids = index.headings_by_title(link.source_file_id, heading_title.as_str());
        match heading_ids {
            [target_heading_id] => Self::write_resolution(
                connection,
                link.id,
                Resolution::resolved_same_file_heading(link.source_file_id, *target_heading_id),
            ),
            [] => Self::write_resolution(
                connection,
                link.id,
                Resolution::broken_same_file_heading(link.source_file_id),
            ),
            [target_heading_id, ..] => Self::write_resolution(
                connection,
                link.id,
                Resolution::resolved_same_file_heading(link.source_file_id, *target_heading_id),
            ),
        }
    }

    fn resolve_same_file_custom_id_link(
        connection: &Connection,
        link: &StoredLink,
        index: &ResolutionIndex,
    ) -> Result<(), DbWriteError> {
        let custom_id_target = normalize_custom_id_lookup_target(link.path.as_str());

        Self::resolve_same_file_custom_id_target(connection, link, &custom_id_target, index)
    }

    fn resolve_same_file_custom_id_target(
        connection: &Connection,
        link: &StoredLink,
        custom_id_target: &str,
        index: &ResolutionIndex,
    ) -> Result<(), DbWriteError> {
        let heading_ids = index.custom_id_headings(link.source_file_id, custom_id_target);
        match heading_ids {
            [target_heading_id] => Self::write_resolution(
                connection,
                link.id,
                Resolution::resolved_same_file_custom_id(
                    link.source_file_id,
                    *target_heading_id,
                    custom_id_target,
                ),
            ),
            [] => Self::write_resolution(
                connection,
                link.id,
                Resolution::broken_same_file_custom_id(link.source_file_id, custom_id_target),
            ),
            [target_heading_id, ..] => Self::write_resolution(
                connection,
                link.id,
                Resolution::resolved_same_file_custom_id(
                    link.source_file_id,
                    *target_heading_id,
                    custom_id_target,
                ),
            ),
        }
    }

    fn resolve_org_id_link(
        connection: &Connection,
        link: &StoredLink,
        index: &ResolutionIndex,
    ) -> Result<(), DbWriteError> {
        let target_id = normalize_id_target(link.path.as_str());
        let matches = index.global_id_targets(&target_id);

        match matches {
            [(target_file_id, target_heading_id)] => Self::write_resolution(
                connection,
                link.id,
                Resolution::resolved_org_id(*target_file_id, *target_heading_id, &target_id),
            ),
            [] => Self::write_resolution(
                connection,
                link.id,
                Resolution::unresolved_org_id(&target_id),
            ),
            [..] => Self::write_resolution(
                connection,
                link.id,
                Resolution::ambiguous_org_id(&target_id),
            ),
        }
    }

    fn resolve_file_target(
        connection: &Connection,
        link: &StoredLink,
        path_absolute: &Path,
        target_file_id: i64,
        index: &ResolutionIndex,
    ) -> Result<(), DbWriteError> {
        if let Some(custom_id_target) = file_custom_id_search_target(link.search_option.as_deref())
        {
            return Self::resolve_file_custom_id_target(
                connection,
                link,
                path_absolute,
                target_file_id,
                &custom_id_target,
                index,
            );
        }

        let Some(heading_title) = heading_title_search_target(link.search_option.as_deref()) else {
            if link.search_option.is_none() {
                return Self::resolve_file_root_target(
                    connection,
                    link.id,
                    path_absolute,
                    target_file_id,
                    index,
                );
            }
            return Self::write_resolution(
                connection,
                link.id,
                Resolution::resolved_file(path_absolute, target_file_id),
            );
        };

        let heading_ids = index.headings_by_title(target_file_id, heading_title.as_str());
        match heading_ids {
            [target_heading_id] => Self::write_resolution(
                connection,
                link.id,
                Resolution::resolved_heading(path_absolute, target_file_id, *target_heading_id),
            ),
            [] => Self::write_resolution(
                connection,
                link.id,
                Resolution::broken_heading_title(path_absolute, target_file_id),
            ),
            [target_heading_id, ..] => Self::write_resolution(
                connection,
                link.id,
                Resolution::resolved_heading(path_absolute, target_file_id, *target_heading_id),
            ),
        }
    }

    fn resolve_file_custom_id_target(
        connection: &Connection,
        link: &StoredLink,
        path_absolute: &Path,
        target_file_id: i64,
        custom_id_target: &str,
        index: &ResolutionIndex,
    ) -> Result<(), DbWriteError> {
        let heading_ids = index.custom_id_headings(target_file_id, custom_id_target);
        match heading_ids {
            [target_heading_id] => Self::write_resolution(
                connection,
                link.id,
                Resolution::resolved_file_custom_id(
                    path_absolute,
                    target_file_id,
                    *target_heading_id,
                    custom_id_target,
                ),
            ),
            [] => Self::write_resolution(
                connection,
                link.id,
                Resolution::broken_file_custom_id(path_absolute, target_file_id, custom_id_target),
            ),
            [target_heading_id, ..] => Self::write_resolution(
                connection,
                link.id,
                Resolution::resolved_file_custom_id(
                    path_absolute,
                    target_file_id,
                    *target_heading_id,
                    custom_id_target,
                ),
            ),
        }
    }

    fn resolve_file_root_target(
        connection: &Connection,
        link_id: i64,
        path_absolute: &Path,
        target_file_id: i64,
        index: &ResolutionIndex,
    ) -> Result<(), DbWriteError> {
        let root_heading_ids = index.root_headings(target_file_id);
        match root_heading_ids {
            [target_heading_id] => Self::write_resolution(
                connection,
                link_id,
                Resolution::resolved_heading(path_absolute, target_file_id, *target_heading_id),
            ),
            [] => Self::write_resolution(
                connection,
                link_id,
                Resolution::resolved_file_with_diagnostic(
                    path_absolute,
                    target_file_id,
                    MISSING_SYNTHETIC_ROOT_DIAGNOSTIC,
                ),
            ),
            [..] => Self::write_resolution(
                connection,
                link_id,
                Resolution::ambiguous_file_root(path_absolute, target_file_id),
            ),
        }
    }

    fn write_resolution(
        connection: &Connection,
        link_id: i64,
        resolution: Resolution<'_>,
    ) -> Result<(), DbWriteError> {
        let (set_path, path_absolute) = resolution.path_absolute.bind();
        let (set_file, target_file_id) = resolution.target_file_id.bind();
        let (set_heading, target_heading_id) = resolution.target_heading_id.bind();
        let (set_custom_id, target_custom_id) = resolution.target_custom_id.bind();
        let (set_id, target_id) = resolution.target_id.bind();
        connection
            .execute(
                "UPDATE links
                 SET path_absolute = CASE WHEN ?2 THEN ?3 ELSE path_absolute END,
                     target_file_id = CASE WHEN ?4 THEN ?5 ELSE target_file_id END,
                     target_heading_id = CASE WHEN ?6 THEN ?7 ELSE target_heading_id END,
                     target_custom_id = CASE WHEN ?8 THEN ?9 ELSE target_custom_id END,
                     target_id = CASE WHEN ?10 THEN ?11 ELSE target_id END,
                     resolution_status = ?12,
                     resolution_diagnostic = ?13
                 WHERE id = ?1",
                params![
                    link_id,
                    set_path,
                    path_absolute,
                    set_file,
                    target_file_id,
                    set_heading,
                    target_heading_id,
                    set_custom_id,
                    target_custom_id,
                    set_id,
                    target_id,
                    resolution.status,
                    resolution.diagnostic,
                ],
            )
            .map_err(|source| DbWriteError::Write {
                operation: "link_resolver.write_resolution",
                source,
            })?;
        Ok(())
    }
}

/// How one `links` UPDATE treats a target column: leave it untouched, set it to NULL, or
/// set it to a value.
#[derive(Debug, Clone, Copy)]
enum Column<T> {
    Keep,
    Null,
    Set(T),
}

impl<T> Column<T> {
    fn bind(self) -> (bool, Option<T>) {
        match self {
            Self::Keep => (false, None),
            Self::Null => (true, None),
            Self::Set(value) => (true, Some(value)),
        }
    }
}

/// The resolution status, diagnostic and target columns written for one link.
#[derive(Debug)]
struct Resolution<'a> {
    status: &'static str,
    diagnostic: Option<&'a str>,
    path_absolute: Column<String>,
    target_file_id: Column<i64>,
    target_heading_id: Column<i64>,
    target_custom_id: Column<&'a str>,
    target_id: Column<&'a str>,
}

impl<'a> Resolution<'a> {
    fn base(status: &'static str, diagnostic: Option<&'a str>) -> Self {
        Self {
            status,
            diagnostic,
            path_absolute: Column::Keep,
            target_file_id: Column::Keep,
            target_heading_id: Column::Keep,
            target_custom_id: Column::Keep,
            target_id: Column::Keep,
        }
    }

    fn unsupported() -> Self {
        Self::base("unsupported", Some(UNSUPPORTED_DIAGNOSTIC))
    }

    fn file_path_unsupported() -> Self {
        Self {
            path_absolute: Column::Null,
            target_file_id: Column::Null,
            ..Self::base("unsupported", Some(FILE_PATH_UNSUPPORTED_DIAGNOSTIC))
        }
    }

    fn resolved_file(path_absolute: &Path, target_file_id: i64) -> Self {
        Self {
            path_absolute: Column::Set(display_path(path_absolute)),
            target_file_id: Column::Set(target_file_id),
            ..Self::base("resolved", None)
        }
    }

    fn resolved_file_with_diagnostic(
        path_absolute: &Path,
        target_file_id: i64,
        diagnostic: &'a str,
    ) -> Self {
        Self {
            target_heading_id: Column::Null,
            ..Self::status_file(path_absolute, target_file_id, "resolved", Some(diagnostic))
        }
    }

    fn resolved_heading(path_absolute: &Path, target_file_id: i64, target_heading_id: i64) -> Self {
        Self {
            target_heading_id: Column::Set(target_heading_id),
            ..Self::resolved_file(path_absolute, target_file_id)
        }
    }

    fn resolved_file_custom_id(
        path_absolute: &Path,
        target_file_id: i64,
        target_heading_id: i64,
        target_custom_id: &'a str,
    ) -> Self {
        Self {
            target_custom_id: Column::Set(target_custom_id),
            target_id: Column::Null,
            ..Self::resolved_heading(path_absolute, target_file_id, target_heading_id)
        }
    }

    fn resolved_same_file_heading(target_file_id: i64, target_heading_id: i64) -> Self {
        Self {
            target_file_id: Column::Set(target_file_id),
            target_heading_id: Column::Set(target_heading_id),
            ..Self::base("resolved", None)
        }
    }

    fn resolved_same_file_custom_id(
        target_file_id: i64,
        target_heading_id: i64,
        target_custom_id: &'a str,
    ) -> Self {
        Self {
            target_custom_id: Column::Set(target_custom_id),
            target_id: Column::Null,
            ..Self::resolved_same_file_heading(target_file_id, target_heading_id)
        }
    }

    fn resolved_org_id(target_file_id: i64, target_heading_id: i64, target_id: &'a str) -> Self {
        Self {
            target_custom_id: Column::Null,
            target_id: Column::Set(target_id),
            ..Self::resolved_same_file_heading(target_file_id, target_heading_id)
        }
    }

    fn broken_file(path_absolute: &Path) -> Self {
        Self::unindexed_file(path_absolute, "broken", FILE_MISSING_DIAGNOSTIC)
    }

    fn unresolved_file(path_absolute: &Path) -> Self {
        Self::unindexed_file(
            path_absolute,
            "unresolved",
            FILE_OUTSIDE_UNIVERSE_DIAGNOSTIC,
        )
    }

    fn ambiguous_file_root(path_absolute: &Path, target_file_id: i64) -> Self {
        Self {
            target_heading_id: Column::Null,
            ..Self::status_file(
                path_absolute,
                target_file_id,
                "ambiguous",
                Some(DUPLICATE_SYNTHETIC_ROOT_DIAGNOSTIC),
            )
        }
    }

    fn broken_heading_title(path_absolute: &Path, target_file_id: i64) -> Self {
        Self {
            target_heading_id: Column::Null,
            ..Self::status_file(
                path_absolute,
                target_file_id,
                "broken",
                Some(HEADING_TITLE_MISSING_DIAGNOSTIC),
            )
        }
    }

    fn broken_file_custom_id(
        path_absolute: &Path,
        target_file_id: i64,
        target_custom_id: &'a str,
    ) -> Self {
        Self {
            target_heading_id: Column::Null,
            target_custom_id: Column::Set(target_custom_id),
            target_id: Column::Null,
            ..Self::status_file(
                path_absolute,
                target_file_id,
                "broken",
                Some(CUSTOM_ID_MISSING_DIAGNOSTIC),
            )
        }
    }

    fn broken_same_file_heading(target_file_id: i64) -> Self {
        Self::broken_same_file_heading_with(
            SAME_FILE_STAR_HEADING_MISSING_DIAGNOSTIC,
            target_file_id,
        )
    }

    fn broken_same_file_custom_id(target_file_id: i64, target_custom_id: &'a str) -> Self {
        Self {
            target_custom_id: Column::Set(target_custom_id),
            target_id: Column::Null,
            ..Self::broken_same_file_heading_with(CUSTOM_ID_MISSING_DIAGNOSTIC, target_file_id)
        }
    }

    fn unresolved_org_id(target_id: &'a str) -> Self {
        Self::org_id_without_target("unresolved", ID_MISSING_DIAGNOSTIC, target_id)
    }

    fn ambiguous_org_id(target_id: &'a str) -> Self {
        Self::org_id_without_target("ambiguous", DUPLICATE_ID_DIAGNOSTIC, target_id)
    }

    /// A file link whose path is recorded but whose target file is not indexed.
    fn unindexed_file(path_absolute: &Path, status: &'static str, diagnostic: &'a str) -> Self {
        Self {
            path_absolute: Column::Set(display_path(path_absolute)),
            target_file_id: Column::Null,
            ..Self::base(status, Some(diagnostic))
        }
    }

    /// A file link that points at an indexed file, with the given status and diagnostic.
    fn status_file(
        path_absolute: &Path,
        target_file_id: i64,
        status: &'static str,
        diagnostic: Option<&'a str>,
    ) -> Self {
        Self {
            path_absolute: Column::Set(display_path(path_absolute)),
            target_file_id: Column::Set(target_file_id),
            ..Self::base(status, diagnostic)
        }
    }

    fn broken_same_file_heading_with(diagnostic: &'a str, target_file_id: i64) -> Self {
        Self {
            target_file_id: Column::Set(target_file_id),
            target_heading_id: Column::Null,
            ..Self::base("broken", Some(diagnostic))
        }
    }

    fn org_id_without_target(
        status: &'static str,
        diagnostic: &'a str,
        target_id: &'a str,
    ) -> Self {
        Self {
            target_file_id: Column::Null,
            target_heading_id: Column::Null,
            target_custom_id: Column::Null,
            target_id: Column::Set(target_id),
            ..Self::base(status, Some(diagnostic))
        }
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

fn heading_title_search_target(search_option: Option<&str>) -> Option<String> {
    normalize_star_heading_title_target(search_option?)
}

fn same_file_fuzzy_star_heading_target(path: &str) -> Option<String> {
    let heading_title = normalize_star_heading_title_target(path)?;
    if path.strip_prefix('*')?.starts_with('*') {
        return None;
    }

    Some(heading_title)
}

fn same_file_fuzzy_custom_id_target(path: &str) -> Option<String> {
    let custom_id_target = normalize_custom_id_target(path)?;
    if path.strip_prefix('#')?.starts_with('#') {
        return None;
    }

    Some(custom_id_target)
}

fn normalize_star_heading_title_target(raw_target: &str) -> Option<String> {
    let heading_title = raw_target.strip_prefix('*')?;
    Some(heading_title.trim().replace("\\[", "[").replace("\\]", "]"))
}

fn normalize_custom_id_target(raw_target: &str) -> Option<String> {
    let custom_id = raw_target.strip_prefix('#')?;
    Some(trim_link_target_padding(custom_id).to_string())
}

fn file_custom_id_search_target(search_option: Option<&str>) -> Option<String> {
    let search_option = search_option?;
    let custom_id_target = normalize_custom_id_target(search_option)?;
    if search_option.strip_prefix('#')?.starts_with('#') {
        return None;
    }

    Some(custom_id_target)
}

fn normalize_custom_id_lookup_target(raw_target: &str) -> String {
    normalize_custom_id_target(raw_target)
        .unwrap_or_else(|| trim_link_target_padding(raw_target).to_string())
}

fn normalize_id_target(raw_target: &str) -> String {
    trim_link_target_padding(raw_target).to_string()
}

/// Stored ID and CUSTOM_ID values are trimmed like Org's property values, and Org finds
/// `[[id:123 56 ]]` and `[[id:123 56]]` alike, so link targets are trimmed of spaces and
/// tabs before matching.
fn trim_link_target_padding(value: &str) -> &str {
    value.trim_matches([' ', '\t'])
}

#[cfg(test)]
mod tests {
    use super::{
        file_custom_id_search_target, heading_title_search_target,
        normalize_custom_id_lookup_target, normalize_custom_id_target, normalize_file_target_path,
        normalize_id_target, same_file_fuzzy_custom_id_target, same_file_fuzzy_star_heading_target,
        IndexedUniverse, LinkResolver, CUSTOM_ID_MISSING_DIAGNOSTIC, DUPLICATE_ID_DIAGNOSTIC,
        FILE_MISSING_DIAGNOSTIC, FILE_OUTSIDE_UNIVERSE_DIAGNOSTIC,
        HEADING_TITLE_MISSING_DIAGNOSTIC, MISSING_SYNTHETIC_ROOT_DIAGNOSTIC,
        SAME_FILE_STAR_HEADING_MISSING_DIAGNOSTIC, UNSUPPORTED_DIAGNOSTIC,
    };
    use crate::db::{
        open_in_memory_database_with_schema, DbWriteError, SchemaDefinition, CURRENT_SCHEMA_VERSION,
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
    type FileHeadingResolutionRow = (
        Option<String>,
        Option<i64>,
        Option<i64>,
        Option<String>,
        Option<String>,
    );
    type FileCustomIdResolutionRow = (
        Option<String>,
        Option<i64>,
        Option<i64>,
        Option<String>,
        Option<String>,
        Option<String>,
    );
    type SameFileHeadingResolutionRow = (Option<i64>, Option<i64>, Option<String>, Option<String>);
    type SameFileCustomIdResolutionRow = (
        Option<i64>,
        Option<i64>,
        Option<String>,
        Option<String>,
        Option<String>,
    );
    type OrgIdResolutionRow = (
        Option<i64>,
        Option<i64>,
        Option<String>,
        Option<String>,
        Option<String>,
        String,
        String,
        Option<String>,
    );
    type OrgIdStatusRow = (
        Option<i64>,
        Option<i64>,
        Option<String>,
        Option<String>,
        Option<String>,
    );

    #[test]
    fn known_files_fall_back_from_malformed_identity_but_propagate_path_decode_errors() {
        let connection = open_in_memory_database_with_schema(&SchemaDefinition::new(
            CURRENT_SCHEMA_VERSION,
            false,
        ))
        .expect("database should open");
        connection
            .execute(
                "INSERT INTO files (path, identity, mtime_ns, size) VALUES (?1, ?2, 1, 1)",
                params!["/tmp/legacy.org", b"orgfdb-path-v1\0unknown\0/path"],
            )
            .expect("malformed identity fixture should insert");

        let known_files = LinkResolver::load_known_files(&connection)
            .expect("malformed identity should use display-path fallback");
        assert!(known_files
            .by_path
            .contains_key(Path::new("/tmp/legacy.org")));

        connection
            .execute(
                "INSERT INTO files (path, mtime_ns, size) VALUES (?1, 1, 1)",
                params![vec![0xff_u8]],
            )
            .expect("non-text path fixture should insert");
        assert!(matches!(
            LinkResolver::load_known_files(&connection),
            Err(DbWriteError::Write {
                operation: "link_resolver.load_known_files.collect",
                ..
            })
        ));
    }

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

    #[test]
    fn resolve_all_resolves_org_id_links_with_exactly_one_match() {
        let schema = SchemaDefinition::new(CURRENT_SCHEMA_VERSION, false);
        let connection =
            open_in_memory_database_with_schema(&schema).expect("database should open");
        seed_file_link_fixture(&connection, "/tmp/source.org", "id:foo", "id", "foo", None);
        seed_known_target_file(&connection, "/tmp/target.org", 2);
        seed_target_heading(&connection, 20, 2, 1, "Heading");
        seed_heading_property(&connection, 20, "ID", Some("foo"));

        let universe = IndexedUniverse::default();
        LinkResolver::resolve_all(&connection, &universe).expect("resolution should succeed");

        let row: OrgIdResolutionRow = connection
            .query_row(
                "SELECT target_file_id, target_heading_id, target_id, resolution_status,
                        resolution_diagnostic, raw, raw_target, raw_description
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
                    ))
                },
            )
            .expect("resolved row should load");
        assert_eq!(
            row,
            (
                Some(2_i64),
                Some(20_i64),
                Some("foo".to_string()),
                Some("resolved".to_string()),
                None,
                "id:foo".to_string(),
                "id:foo".to_string(),
                None,
            )
        );
    }

    #[test]
    fn resolve_all_trims_whitespace_in_org_id_links_before_matching() {
        let schema = SchemaDefinition::new(CURRENT_SCHEMA_VERSION, false);
        let connection =
            open_in_memory_database_with_schema(&schema).expect("database should open");
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
                    24_i64,
                    1_i64,
                    "normal",
                    "bracket",
                    "[[id: FOO ][Description]]",
                    "id: FOO ",
                    Some("Description".to_string()),
                    "id",
                    " FOO ",
                    Option::<String>::None,
                ],
            )
            .expect("source link insert should succeed");
        seed_known_target_file(&connection, "/tmp/target.org", 2);
        seed_target_heading(&connection, 20, 2, 1, "Heading");
        seed_heading_property(&connection, 20, "ID", Some("foo"));

        let universe = IndexedUniverse::default();
        LinkResolver::resolve_all(&connection, &universe).expect("resolution should succeed");

        let row: OrgIdResolutionRow = connection
            .query_row(
                "SELECT target_file_id, target_heading_id, target_id, resolution_status,
                        resolution_diagnostic, raw, raw_target, raw_description
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
                    ))
                },
            )
            .expect("resolved row should load");
        assert_eq!(
            row,
            (
                Some(2_i64),
                Some(20_i64),
                Some("FOO".to_string()),
                Some("resolved".to_string()),
                None,
                "[[id: FOO ][Description]]".to_string(),
                "id: FOO ".to_string(),
                Some("Description".to_string()),
            )
        );
    }

    #[test]
    fn resolve_all_marks_duplicate_org_id_links_ambiguous() {
        let schema = SchemaDefinition::new(CURRENT_SCHEMA_VERSION, false);
        let connection =
            open_in_memory_database_with_schema(&schema).expect("database should open");
        seed_file_link_fixture(
            &connection,
            "/tmp/source.org",
            "<id:dup>",
            "id",
            "dup",
            None,
        );
        seed_known_target_file(&connection, "/tmp/target-a.org", 2);
        seed_target_heading(&connection, 20, 2, 1, "First");
        seed_heading_property(&connection, 20, "ID", Some("dup"));
        seed_known_target_file(&connection, "/tmp/target-b.org", 3);
        seed_target_heading(&connection, 30, 3, 1, "Second");
        seed_heading_property(&connection, 30, "ID", Some("DUP"));

        let universe = IndexedUniverse::default();
        LinkResolver::resolve_all(&connection, &universe).expect("resolution should succeed");

        let row: OrgIdStatusRow = connection
            .query_row(
                "SELECT target_file_id, target_heading_id, target_id, resolution_status,
                            resolution_diagnostic
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
                    ))
                },
            )
            .expect("ambiguous row should load");
        assert_eq!(
            row,
            (
                None,
                None,
                Some("dup".to_string()),
                Some("ambiguous".to_string()),
                Some(DUPLICATE_ID_DIAGNOSTIC.to_string()),
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
            None,
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
    fn resolve_all_treats_file_sys_and_file_emacs_links_as_file_links() {
        for link_type in ["file+sys", "file+emacs"] {
            for (target, expected) in [
                ("target.org", (Some(2_i64), Some("resolved".to_string()))),
                ("missing.org", (None, Some("broken".to_string()))),
            ] {
                let schema = SchemaDefinition::new(CURRENT_SCHEMA_VERSION, false);
                let connection =
                    open_in_memory_database_with_schema(&schema).expect("database should open");
                seed_file_link_fixture(
                    &connection,
                    "/tmp/source.org",
                    &format!("[[{link_type}:{target}]]"),
                    link_type,
                    target,
                    None,
                );
                seed_known_target_file(&connection, "/tmp/target.org", 2);

                let mut universe = IndexedUniverse::default();
                universe.add_recursive_root(PathBuf::from("/tmp"));

                LinkResolver::resolve_all(&connection, &universe)
                    .expect("resolution should succeed");

                let row: (Option<i64>, Option<String>) = connection
                    .query_row(
                        "SELECT target_file_id, resolution_status FROM links WHERE id = 1",
                        [],
                        |row| Ok((row.get(0)?, row.get(1)?)),
                    )
                    .expect("link row should load");
                assert_eq!(row, expected, "{link_type}:{target}");
            }
        }
    }

    #[test]
    fn resolve_all_maps_file_only_links_to_target_root_headings() {
        let schema = SchemaDefinition::new(CURRENT_SCHEMA_VERSION, false);
        let connection =
            open_in_memory_database_with_schema(&schema).expect("database should open");
        seed_file_link_fixture(
            &connection,
            "/tmp/source.org",
            "[[file:target.org]]",
            "file",
            "target.org",
            None,
        );
        seed_known_target_file(&connection, "/tmp/target.org", 2);
        seed_target_heading(&connection, 20, 2, 1, "Child heading");

        let mut universe = IndexedUniverse::default();
        universe.add_exact_path(PathBuf::from("/tmp/source.org"));
        universe.add_exact_path(PathBuf::from("/tmp/target.org"));

        LinkResolver::resolve_all(&connection, &universe).expect("resolution should succeed");

        let row: FileHeadingResolutionRow = connection
            .query_row(
                "SELECT path_absolute, target_file_id, target_heading_id,
                            resolution_status, resolution_diagnostic
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
                    ))
                },
            )
            .expect("resolved row should load");
        assert_eq!(
            row,
            (
                Some("/tmp/target.org".to_string()),
                Some(2_i64),
                Some(2_i64),
                Some("resolved".to_string()),
                None,
            )
        );
    }

    #[test]
    fn resolve_all_keeps_file_links_with_search_options_off_root_fallback() {
        let schema = SchemaDefinition::new(CURRENT_SCHEMA_VERSION, false);
        let connection =
            open_in_memory_database_with_schema(&schema).expect("database should open");
        seed_file_link_fixture(
            &connection,
            "/tmp/source.org",
            "[[file:target.org::/regexp/]]",
            "file",
            "target.org",
            Some("/regexp/"),
        );
        seed_known_target_file(&connection, "/tmp/target.org", 2);

        let mut universe = IndexedUniverse::default();
        universe.add_exact_path(PathBuf::from("/tmp/source.org"));
        universe.add_exact_path(PathBuf::from("/tmp/target.org"));

        LinkResolver::resolve_all(&connection, &universe).expect("resolution should succeed");

        let row: FileHeadingResolutionRow = connection
            .query_row(
                "SELECT path_absolute, target_file_id, target_heading_id,
                            resolution_status, resolution_diagnostic
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
                    ))
                },
            )
            .expect("resolved row should load");
        assert_eq!(
            row,
            (
                Some("/tmp/target.org".to_string()),
                Some(2_i64),
                None,
                Some("resolved".to_string()),
                None,
            )
        );
    }

    #[test]
    fn resolve_all_marks_missing_synthetic_root_heading_as_resolved_corruption() {
        let schema = SchemaDefinition::new(CURRENT_SCHEMA_VERSION, false);
        let connection =
            open_in_memory_database_with_schema(&schema).expect("database should open");
        seed_file_link_fixture(
            &connection,
            "/tmp/source.org",
            "[[file:target.org]]",
            "file",
            "target.org",
            None,
        );
        connection
            .execute(
                "INSERT INTO files (id, path, mtime_ns, size) VALUES (?1, ?2, ?3, ?4)",
                (2_i64, "/tmp/target.org", 30_i64, 40_i64),
            )
            .expect("target file insert should succeed");

        let mut universe = IndexedUniverse::default();
        universe.add_exact_path(PathBuf::from("/tmp/source.org"));
        universe.add_exact_path(PathBuf::from("/tmp/target.org"));

        LinkResolver::resolve_all(&connection, &universe).expect("resolution should succeed");

        let row: FileHeadingResolutionRow = connection
            .query_row(
                "SELECT path_absolute, target_file_id, target_heading_id,
                            resolution_status, resolution_diagnostic
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
                    ))
                },
            )
            .expect("resolved row should load");
        assert_eq!(
            row,
            (
                Some("/tmp/target.org".to_string()),
                Some(2_i64),
                None,
                Some("resolved".to_string()),
                Some(MISSING_SYNTHETIC_ROOT_DIAGNOSTIC.to_string()),
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
            None,
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
            None,
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

    #[test]
    fn resolve_all_resolves_heading_title_search_options_to_target_headings() {
        let schema = SchemaDefinition::new(CURRENT_SCHEMA_VERSION, false);
        let connection =
            open_in_memory_database_with_schema(&schema).expect("database should open");
        seed_file_link_fixture(
            &connection,
            "/tmp/source.org",
            "[[file:target.org::*[2026-07-01 Wed] Review]]",
            "file",
            "target.org",
            Some(r"*\[2026-07-01 Wed\] Review"),
        );
        seed_known_target_file(&connection, "/tmp/target.org", 2);
        seed_target_heading(&connection, 20, 2, 1, "[2026-07-01 Wed] Review");

        let mut universe = IndexedUniverse::default();
        universe.add_exact_path(PathBuf::from("/tmp/source.org"));
        universe.add_exact_path(PathBuf::from("/tmp/target.org"));

        LinkResolver::resolve_all(&connection, &universe).expect("resolution should succeed");

        let row: FileHeadingResolutionRow = connection
            .query_row(
                "SELECT path_absolute, target_file_id, target_heading_id,
                            resolution_status, resolution_diagnostic
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
                    ))
                },
            )
            .expect("resolved row should load");
        assert_eq!(
            row,
            (
                Some("/tmp/target.org".to_string()),
                Some(2_i64),
                Some(20_i64),
                Some("resolved".to_string()),
                None,
            )
        );
    }

    #[test]
    fn resolve_all_normalizes_file_heading_title_search_options_before_matching() {
        let schema = SchemaDefinition::new(CURRENT_SCHEMA_VERSION, false);
        let connection =
            open_in_memory_database_with_schema(&schema).expect("database should open");
        seed_file_link_fixture(
            &connection,
            "/tmp/source.org",
            "[[file:target.org::*   main index   ]]",
            "file",
            "target.org",
            Some("*   main index   "),
        );
        seed_known_target_file(&connection, "/tmp/target.org", 2);
        seed_target_heading(&connection, 20, 2, 1, "Main Index");

        let mut universe = IndexedUniverse::default();
        universe.add_exact_path(PathBuf::from("/tmp/source.org"));
        universe.add_exact_path(PathBuf::from("/tmp/target.org"));

        LinkResolver::resolve_all(&connection, &universe).expect("resolution should succeed");

        let row: FileHeadingResolutionRow = connection
            .query_row(
                "SELECT path_absolute, target_file_id, target_heading_id,
                            resolution_status, resolution_diagnostic
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
                    ))
                },
            )
            .expect("resolved row should load");
        assert_eq!(
            row,
            (
                Some("/tmp/target.org".to_string()),
                Some(2_i64),
                Some(20_i64),
                Some("resolved".to_string()),
                None,
            )
        );
    }

    #[test]
    fn resolve_all_matches_unicode_case_insensitive_file_heading_title_search_options() {
        let schema = SchemaDefinition::new(CURRENT_SCHEMA_VERSION, false);
        let connection =
            open_in_memory_database_with_schema(&schema).expect("database should open");
        seed_file_link_fixture(
            &connection,
            "/tmp/source.org",
            "[[file:target.org::*ärger]]",
            "file",
            "target.org",
            Some("*ärger"),
        );
        seed_known_target_file(&connection, "/tmp/target.org", 2);
        seed_target_heading(&connection, 20, 2, 1, "Ärger");

        let mut universe = IndexedUniverse::default();
        universe.add_exact_path(PathBuf::from("/tmp/source.org"));
        universe.add_exact_path(PathBuf::from("/tmp/target.org"));

        LinkResolver::resolve_all(&connection, &universe).expect("resolution should succeed");

        let row: FileHeadingResolutionRow = connection
            .query_row(
                "SELECT path_absolute, target_file_id, target_heading_id,
                            resolution_status, resolution_diagnostic
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
                    ))
                },
            )
            .expect("resolved row should load");
        assert_eq!(
            row,
            (
                Some("/tmp/target.org".to_string()),
                Some(2_i64),
                Some(20_i64),
                Some("resolved".to_string()),
                None,
            )
        );
    }

    #[test]
    fn resolve_all_marks_missing_heading_title_search_targets_broken() {
        let schema = SchemaDefinition::new(CURRENT_SCHEMA_VERSION, false);
        let connection =
            open_in_memory_database_with_schema(&schema).expect("database should open");
        seed_file_link_fixture(
            &connection,
            "/tmp/source.org",
            "[[file:target.org::*Missing]]",
            "file",
            "target.org",
            Some("*Missing"),
        );
        seed_known_target_file(&connection, "/tmp/target.org", 2);
        seed_target_heading(&connection, 20, 2, 1, "Existing");

        let mut universe = IndexedUniverse::default();
        universe.add_exact_path(PathBuf::from("/tmp/source.org"));
        universe.add_exact_path(PathBuf::from("/tmp/target.org"));

        LinkResolver::resolve_all(&connection, &universe).expect("resolution should succeed");

        let row: FileHeadingResolutionRow = connection
            .query_row(
                "SELECT path_absolute, target_file_id, target_heading_id,
                            resolution_status, resolution_diagnostic
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
                    ))
                },
            )
            .expect("broken row should load");
        assert_eq!(
            row,
            (
                Some("/tmp/target.org".to_string()),
                Some(2_i64),
                None,
                Some("broken".to_string()),
                Some(HEADING_TITLE_MISSING_DIAGNOSTIC.to_string()),
            )
        );
    }

    #[test]
    fn resolve_all_selects_first_duplicate_heading_title_search_target_in_document_order() {
        let schema = SchemaDefinition::new(CURRENT_SCHEMA_VERSION, false);
        let connection =
            open_in_memory_database_with_schema(&schema).expect("database should open");
        seed_file_link_fixture(
            &connection,
            "/tmp/source.org",
            "[[file:target.org::*Duplicate]]",
            "file",
            "target.org",
            Some("*Duplicate"),
        );
        seed_known_target_file(&connection, "/tmp/target.org", 2);
        seed_target_heading(&connection, 20, 2, 1, "Duplicate");
        seed_target_heading(&connection, 21, 2, 1, "Duplicate");

        let mut universe = IndexedUniverse::default();
        universe.add_exact_path(PathBuf::from("/tmp/source.org"));
        universe.add_exact_path(PathBuf::from("/tmp/target.org"));

        LinkResolver::resolve_all(&connection, &universe).expect("resolution should succeed");

        let row: FileHeadingResolutionRow = connection
            .query_row(
                "SELECT path_absolute, target_file_id, target_heading_id,
                            resolution_status, resolution_diagnostic
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
                    ))
                },
            )
            .expect("resolved row should load");
        assert_eq!(
            row,
            (
                Some("/tmp/target.org".to_string()),
                Some(2_i64),
                Some(20_i64),
                Some("resolved".to_string()),
                None,
            )
        );
    }

    #[test]
    fn resolve_all_excludes_synthetic_root_headings_from_heading_title_matches() {
        let schema = SchemaDefinition::new(CURRENT_SCHEMA_VERSION, false);
        let connection =
            open_in_memory_database_with_schema(&schema).expect("database should open");
        seed_file_link_fixture(
            &connection,
            "/tmp/source.org",
            "[[file:target.org::*Only Root]]",
            "file",
            "target.org",
            Some("*Only Root"),
        );
        seed_known_target_file_with_root_title(&connection, "/tmp/target.org", 2, "Only Root");

        let mut universe = IndexedUniverse::default();
        universe.add_exact_path(PathBuf::from("/tmp/source.org"));
        universe.add_exact_path(PathBuf::from("/tmp/target.org"));

        LinkResolver::resolve_all(&connection, &universe).expect("resolution should succeed");

        let row: (Option<i64>, Option<String>, Option<String>) = connection
            .query_row(
                "SELECT target_heading_id, resolution_status, resolution_diagnostic
                 FROM links
                 WHERE id = 1",
                [],
                |row| Ok((row.get(0)?, row.get(1)?, row.get(2)?)),
            )
            .expect("row should load");
        assert_eq!(
            row,
            (
                None,
                Some("broken".to_string()),
                Some(HEADING_TITLE_MISSING_DIAGNOSTIC.to_string()),
            )
        );
    }

    #[test]
    fn resolve_all_keeps_resolved_file_targets_for_unsupported_search_options() {
        let schema = SchemaDefinition::new(CURRENT_SCHEMA_VERSION, false);
        let connection =
            open_in_memory_database_with_schema(&schema).expect("database should open");
        seed_file_link_fixture(
            &connection,
            "/tmp/source.org",
            "[[file:target.org::/regexp/]]",
            "file",
            "target.org",
            Some("/regexp/"),
        );
        seed_known_target_file(&connection, "/tmp/target.org", 2);
        seed_target_heading(&connection, 20, 2, 1, "Target");

        let mut universe = IndexedUniverse::default();
        universe.add_exact_path(PathBuf::from("/tmp/source.org"));
        universe.add_exact_path(PathBuf::from("/tmp/target.org"));

        LinkResolver::resolve_all(&connection, &universe).expect("resolution should succeed");

        let row: FileHeadingResolutionRow = connection
            .query_row(
                "SELECT path_absolute, target_file_id, target_heading_id,
                            resolution_status, resolution_diagnostic
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
                    ))
                },
            )
            .expect("resolved row should load");
        assert_eq!(
            row,
            (
                Some("/tmp/target.org".to_string()),
                Some(2_i64),
                None,
                Some("resolved".to_string()),
                None,
            )
        );
    }

    #[test]
    fn resolve_all_resolves_file_custom_id_search_options_to_target_headings() {
        let schema = SchemaDefinition::new(CURRENT_SCHEMA_VERSION, false);
        let connection =
            open_in_memory_database_with_schema(&schema).expect("database should open");
        seed_file_link_fixture(
            &connection,
            "/tmp/source.org",
            "[[file:target.org::#custom-id]]",
            "file",
            "target.org",
            Some("#custom-id"),
        );
        seed_known_target_file(&connection, "/tmp/target.org", 2);
        seed_target_heading(&connection, 20, 2, 1, "Target");
        seed_heading_property(&connection, 20, "CUSTOM_ID", Some("custom-id"));

        let mut universe = IndexedUniverse::default();
        universe.add_exact_path(PathBuf::from("/tmp/source.org"));
        universe.add_exact_path(PathBuf::from("/tmp/target.org"));

        LinkResolver::resolve_all(&connection, &universe).expect("resolution should succeed");

        let row: FileCustomIdResolutionRow = connection
            .query_row(
                "SELECT path_absolute, target_file_id, target_heading_id, target_custom_id,
                        resolution_status, resolution_diagnostic
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
                    ))
                },
            )
            .expect("resolved row should load");
        assert_eq!(
            row,
            (
                Some("/tmp/target.org".to_string()),
                Some(2_i64),
                Some(20_i64),
                Some("custom-id".to_string()),
                Some("resolved".to_string()),
                None,
            )
        );
    }

    #[test]
    fn resolve_all_trims_whitespace_in_file_custom_id_search_options_before_matching() {
        let schema = SchemaDefinition::new(CURRENT_SCHEMA_VERSION, false);
        let connection =
            open_in_memory_database_with_schema(&schema).expect("database should open");
        seed_file_link_fixture(
            &connection,
            "/tmp/source.org",
            "[[file:target.org::# abc ]]",
            "file",
            "target.org",
            Some("# abc "),
        );
        seed_known_target_file(&connection, "/tmp/target.org", 2);
        seed_target_heading(&connection, 20, 2, 1, "Target");
        seed_heading_property(&connection, 20, "CUSTOM_ID", Some("abc"));

        let mut universe = IndexedUniverse::default();
        universe.add_exact_path(PathBuf::from("/tmp/source.org"));
        universe.add_exact_path(PathBuf::from("/tmp/target.org"));

        LinkResolver::resolve_all(&connection, &universe).expect("resolution should succeed");

        let row: FileCustomIdResolutionRow = connection
            .query_row(
                "SELECT path_absolute, target_file_id, target_heading_id, target_custom_id,
                        resolution_status, resolution_diagnostic
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
                    ))
                },
            )
            .expect("resolved row should load");
        assert_eq!(
            row,
            (
                Some("/tmp/target.org".to_string()),
                Some(2_i64),
                Some(20_i64),
                Some("abc".to_string()),
                Some("resolved".to_string()),
                None,
            )
        );
    }

    #[test]
    fn resolve_all_marks_missing_file_custom_id_search_options_broken() {
        let schema = SchemaDefinition::new(CURRENT_SCHEMA_VERSION, false);
        let connection =
            open_in_memory_database_with_schema(&schema).expect("database should open");
        seed_file_link_fixture(
            &connection,
            "/tmp/source.org",
            "[[file:target.org::#missing]]",
            "file",
            "target.org",
            Some("#missing"),
        );
        seed_known_target_file(&connection, "/tmp/target.org", 2);
        seed_target_heading(&connection, 20, 2, 1, "Target");

        let mut universe = IndexedUniverse::default();
        universe.add_exact_path(PathBuf::from("/tmp/source.org"));
        universe.add_exact_path(PathBuf::from("/tmp/target.org"));

        LinkResolver::resolve_all(&connection, &universe).expect("resolution should succeed");

        let row: FileCustomIdResolutionRow = connection
            .query_row(
                "SELECT path_absolute, target_file_id, target_heading_id, target_custom_id,
                        resolution_status, resolution_diagnostic
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
                    ))
                },
            )
            .expect("broken row should load");
        assert_eq!(
            row,
            (
                Some("/tmp/target.org".to_string()),
                Some(2_i64),
                None,
                Some("missing".to_string()),
                Some("broken".to_string()),
                Some(CUSTOM_ID_MISSING_DIAGNOSTIC.to_string()),
            )
        );
    }

    #[test]
    fn resolve_all_selects_first_duplicate_file_custom_id_search_option_in_document_order() {
        let schema = SchemaDefinition::new(CURRENT_SCHEMA_VERSION, false);
        let connection =
            open_in_memory_database_with_schema(&schema).expect("database should open");
        seed_file_link_fixture(
            &connection,
            "/tmp/source.org",
            "[[file:target.org::#dup]]",
            "file",
            "target.org",
            Some("#dup"),
        );
        seed_known_target_file(&connection, "/tmp/target.org", 2);
        seed_target_heading(&connection, 20, 2, 1, "First");
        seed_target_heading(&connection, 21, 2, 1, "Second");
        seed_heading_property(&connection, 20, "CUSTOM_ID", Some("dup"));
        seed_heading_property(&connection, 21, "CUSTOM_ID", Some("DUP"));

        let mut universe = IndexedUniverse::default();
        universe.add_exact_path(PathBuf::from("/tmp/source.org"));
        universe.add_exact_path(PathBuf::from("/tmp/target.org"));

        LinkResolver::resolve_all(&connection, &universe).expect("resolution should succeed");

        let row: (Option<i64>, Option<String>, Option<String>) = connection
            .query_row(
                "SELECT target_heading_id, target_custom_id, resolution_status
                 FROM links
                 WHERE id = 1",
                [],
                |row| Ok((row.get(0)?, row.get(1)?, row.get(2)?)),
            )
            .expect("resolved row should load");
        assert_eq!(
            row,
            (
                Some(20_i64),
                Some("dup".to_string()),
                Some("resolved".to_string()),
            )
        );
    }

    #[test]
    fn resolve_all_resolves_same_file_fuzzy_star_links_to_source_headings() {
        let schema = SchemaDefinition::new(CURRENT_SCHEMA_VERSION, false);
        let connection =
            open_in_memory_database_with_schema(&schema).expect("database should open");
        seed_file_link_fixture(
            &connection,
            "/tmp/source.org",
            "[[*Heading]]",
            "fuzzy",
            "*Heading",
            None,
        );
        seed_target_heading(&connection, 20, 1, 1, "Heading");

        let universe = IndexedUniverse::default();
        LinkResolver::resolve_all(&connection, &universe).expect("resolution should succeed");

        let row: SameFileHeadingResolutionRow = connection
            .query_row(
                "SELECT target_file_id, target_heading_id, resolution_status, resolution_diagnostic
                 FROM links
                 WHERE id = 1",
                [],
                |row| Ok((row.get(0)?, row.get(1)?, row.get(2)?, row.get(3)?)),
            )
            .expect("resolved row should load");
        assert_eq!(
            row,
            (
                Some(1_i64),
                Some(20_i64),
                Some("resolved".to_string()),
                None
            )
        );
    }

    #[test]
    fn resolve_all_normalizes_same_file_fuzzy_star_links_before_matching() {
        let schema = SchemaDefinition::new(CURRENT_SCHEMA_VERSION, false);
        let connection =
            open_in_memory_database_with_schema(&schema).expect("database should open");
        seed_file_link_fixture(
            &connection,
            "/tmp/source.org",
            "[[*   peer heading   ]]",
            "fuzzy",
            "*   peer heading   ",
            None,
        );
        seed_target_heading(&connection, 20, 1, 1, "Peer Heading");

        let universe = IndexedUniverse::default();
        LinkResolver::resolve_all(&connection, &universe).expect("resolution should succeed");

        let row: SameFileHeadingResolutionRow = connection
            .query_row(
                "SELECT target_file_id, target_heading_id, resolution_status, resolution_diagnostic
                 FROM links
                 WHERE id = 1",
                [],
                |row| Ok((row.get(0)?, row.get(1)?, row.get(2)?, row.get(3)?)),
            )
            .expect("resolved row should load");
        assert_eq!(
            row,
            (
                Some(1_i64),
                Some(20_i64),
                Some("resolved".to_string()),
                None,
            )
        );
    }

    #[test]
    fn resolve_all_matches_unicode_case_insensitive_same_file_fuzzy_star_links() {
        let schema = SchemaDefinition::new(CURRENT_SCHEMA_VERSION, false);
        let connection =
            open_in_memory_database_with_schema(&schema).expect("database should open");
        seed_file_link_fixture(
            &connection,
            "/tmp/source.org",
            "[[*ärger]]",
            "fuzzy",
            "*ärger",
            None,
        );
        seed_target_heading(&connection, 20, 1, 1, "Ärger");

        let universe = IndexedUniverse::default();
        LinkResolver::resolve_all(&connection, &universe).expect("resolution should succeed");

        let row: SameFileHeadingResolutionRow = connection
            .query_row(
                "SELECT target_file_id, target_heading_id, resolution_status, resolution_diagnostic
                 FROM links
                 WHERE id = 1",
                [],
                |row| Ok((row.get(0)?, row.get(1)?, row.get(2)?, row.get(3)?)),
            )
            .expect("resolved row should load");
        assert_eq!(
            row,
            (
                Some(1_i64),
                Some(20_i64),
                Some("resolved".to_string()),
                None,
            )
        );
    }

    #[test]
    fn resolve_all_resolves_same_file_fuzzy_custom_id_links_to_source_headings() {
        let schema = SchemaDefinition::new(CURRENT_SCHEMA_VERSION, false);
        let connection =
            open_in_memory_database_with_schema(&schema).expect("database should open");
        seed_file_link_fixture(
            &connection,
            "/tmp/source.org",
            "[[#custom-id]]",
            "fuzzy",
            "#custom-id",
            None,
        );
        seed_target_heading(&connection, 20, 1, 1, "Heading");
        seed_heading_property(&connection, 20, "CUSTOM_ID", Some("custom-id"));

        let universe = IndexedUniverse::default();
        LinkResolver::resolve_all(&connection, &universe).expect("resolution should succeed");

        let row: SameFileCustomIdResolutionRow = connection
            .query_row(
                "SELECT target_file_id, target_heading_id, target_custom_id,
                        resolution_status, resolution_diagnostic
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
                    ))
                },
            )
            .expect("resolved row should load");
        assert_eq!(
            row,
            (
                Some(1_i64),
                Some(20_i64),
                Some("custom-id".to_string()),
                Some("resolved".to_string()),
                None,
            )
        );
    }

    #[test]
    fn resolve_all_trims_whitespace_in_same_file_fuzzy_custom_id_links_before_matching() {
        let schema = SchemaDefinition::new(CURRENT_SCHEMA_VERSION, false);
        let connection =
            open_in_memory_database_with_schema(&schema).expect("database should open");
        seed_file_link_fixture(
            &connection,
            "/tmp/source.org",
            "[[# Custom-ID ]]",
            "fuzzy",
            "# Custom-ID ",
            None,
        );
        seed_target_heading(&connection, 20, 1, 1, "Heading");
        seed_heading_property(&connection, 20, "CUSTOM_ID", Some("Custom-ID"));

        let universe = IndexedUniverse::default();
        LinkResolver::resolve_all(&connection, &universe).expect("resolution should succeed");

        let row: SameFileCustomIdResolutionRow = connection
            .query_row(
                "SELECT target_file_id, target_heading_id, target_custom_id,
                        resolution_status, resolution_diagnostic
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
                    ))
                },
            )
            .expect("resolved row should load");
        assert_eq!(
            row,
            (
                Some(1_i64),
                Some(20_i64),
                Some("Custom-ID".to_string()),
                Some("resolved".to_string()),
                None,
            )
        );
    }

    #[test]
    fn resolve_all_selects_first_duplicate_same_file_fuzzy_custom_id_link_in_document_order() {
        let schema = SchemaDefinition::new(CURRENT_SCHEMA_VERSION, false);
        let connection =
            open_in_memory_database_with_schema(&schema).expect("database should open");
        seed_file_link_fixture(
            &connection,
            "/tmp/source.org",
            "[[#dup]]",
            "fuzzy",
            "#dup",
            None,
        );
        seed_target_heading(&connection, 20, 1, 1, "First");
        seed_target_heading(&connection, 21, 1, 1, "Second");
        seed_heading_property(&connection, 20, "CUSTOM_ID", Some("dup"));
        seed_heading_property(&connection, 21, "CUSTOM_ID", Some("DUP"));

        let universe = IndexedUniverse::default();
        LinkResolver::resolve_all(&connection, &universe).expect("resolution should succeed");

        let row: SameFileCustomIdResolutionRow = connection
            .query_row(
                "SELECT target_file_id, target_heading_id, target_custom_id,
                        resolution_status, resolution_diagnostic
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
                    ))
                },
            )
            .expect("resolved row should load");
        assert_eq!(
            row,
            (
                Some(1_i64),
                Some(20_i64),
                Some("dup".to_string()),
                Some("resolved".to_string()),
                None,
            )
        );
    }

    #[test]
    fn resolve_all_marks_missing_same_file_fuzzy_custom_id_links_broken() {
        let schema = SchemaDefinition::new(CURRENT_SCHEMA_VERSION, false);
        let connection =
            open_in_memory_database_with_schema(&schema).expect("database should open");
        seed_file_link_fixture(
            &connection,
            "/tmp/source.org",
            "[[#missing]]",
            "fuzzy",
            "#missing",
            None,
        );

        let universe = IndexedUniverse::default();
        LinkResolver::resolve_all(&connection, &universe).expect("resolution should succeed");

        let row: SameFileCustomIdResolutionRow = connection
            .query_row(
                "SELECT target_file_id, target_heading_id, target_custom_id,
                        resolution_status, resolution_diagnostic
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
                    ))
                },
            )
            .expect("broken row should load");
        assert_eq!(
            row,
            (
                Some(1_i64),
                None,
                Some("missing".to_string()),
                Some("broken".to_string()),
                Some(CUSTOM_ID_MISSING_DIAGNOSTIC.to_string()),
            )
        );
    }

    #[test]
    fn resolve_all_marks_missing_same_file_fuzzy_star_links_broken() {
        let schema = SchemaDefinition::new(CURRENT_SCHEMA_VERSION, false);
        let connection =
            open_in_memory_database_with_schema(&schema).expect("database should open");
        seed_file_link_fixture(
            &connection,
            "/tmp/source.org",
            "[[*Missing]]",
            "fuzzy",
            "*Missing",
            None,
        );

        let universe = IndexedUniverse::default();
        LinkResolver::resolve_all(&connection, &universe).expect("resolution should succeed");

        let row: SameFileHeadingResolutionRow = connection
            .query_row(
                "SELECT target_file_id, target_heading_id, resolution_status, resolution_diagnostic
                 FROM links
                 WHERE id = 1",
                [],
                |row| Ok((row.get(0)?, row.get(1)?, row.get(2)?, row.get(3)?)),
            )
            .expect("broken row should load");
        assert_eq!(
            row,
            (
                Some(1_i64),
                None,
                Some("broken".to_string()),
                Some(SAME_FILE_STAR_HEADING_MISSING_DIAGNOSTIC.to_string()),
            )
        );
    }

    #[test]
    fn resolve_all_selects_first_duplicate_same_file_fuzzy_star_link_in_document_order() {
        let schema = SchemaDefinition::new(CURRENT_SCHEMA_VERSION, false);
        let connection =
            open_in_memory_database_with_schema(&schema).expect("database should open");
        seed_file_link_fixture(
            &connection,
            "/tmp/source.org",
            "[[*Duplicate]]",
            "fuzzy",
            "*Duplicate",
            None,
        );
        seed_target_heading(&connection, 20, 1, 1, "Duplicate");
        seed_target_heading(&connection, 21, 1, 1, "Duplicate");

        let universe = IndexedUniverse::default();
        LinkResolver::resolve_all(&connection, &universe).expect("resolution should succeed");

        let row: SameFileHeadingResolutionRow = connection
            .query_row(
                "SELECT target_file_id, target_heading_id, resolution_status, resolution_diagnostic
                 FROM links
                 WHERE id = 1",
                [],
                |row| Ok((row.get(0)?, row.get(1)?, row.get(2)?, row.get(3)?)),
            )
            .expect("resolved row should load");
        assert_eq!(
            row,
            (
                Some(1_i64),
                Some(20_i64),
                Some("resolved".to_string()),
                None,
            )
        );
    }

    #[test]
    fn resolve_all_keeps_non_star_fuzzy_links_unsupported() {
        let schema = SchemaDefinition::new(CURRENT_SCHEMA_VERSION, false);
        let connection =
            open_in_memory_database_with_schema(&schema).expect("database should open");
        seed_file_link_fixture(
            &connection,
            "/tmp/source.org",
            "[[Heading]]",
            "fuzzy",
            "Heading",
            None,
        );
        seed_target_heading(&connection, 20, 1, 1, "Heading");

        let universe = IndexedUniverse::default();
        LinkResolver::resolve_all(&connection, &universe).expect("resolution should succeed");

        let row: SameFileHeadingResolutionRow = connection
            .query_row(
                "SELECT target_file_id, target_heading_id, resolution_status, resolution_diagnostic
                 FROM links
                 WHERE id = 1",
                [],
                |row| Ok((row.get(0)?, row.get(1)?, row.get(2)?, row.get(3)?)),
            )
            .expect("unsupported row should load");
        assert_eq!(
            row,
            (
                None,
                None,
                Some("unsupported".to_string()),
                Some(UNSUPPORTED_DIAGNOSTIC.to_string()),
            )
        );
    }

    #[test]
    fn normalize_custom_id_target_strips_one_leading_hash_and_trims_padding() {
        assert_eq!(
            normalize_custom_id_target("# Custom-ID "),
            Some("Custom-ID".to_string())
        );
        assert_eq!(
            normalize_custom_id_target("##custom-id"),
            Some("#custom-id".to_string())
        );
        assert_eq!(normalize_custom_id_target("custom-id"), None);
    }

    #[test]
    fn normalize_custom_id_lookup_target_accepts_both_stored_path_forms() {
        assert_eq!(
            normalize_custom_id_lookup_target("# Custom-ID "),
            "Custom-ID".to_string()
        );
        assert_eq!(
            normalize_custom_id_lookup_target(" custom-id "),
            "custom-id".to_string()
        );
    }

    #[test]
    fn normalize_id_target_trims_spaces_and_tabs() {
        assert_eq!(normalize_id_target(" FOO \t"), "FOO".to_string());
        assert_eq!(normalize_id_target("foo"), "foo".to_string());
    }

    #[test]
    fn file_custom_id_search_target_requires_exactly_one_leading_hash() {
        assert_eq!(
            file_custom_id_search_target(Some("# Custom-ID ")),
            Some("Custom-ID".to_string())
        );
        assert_eq!(file_custom_id_search_target(Some("##custom-id")), None);
        assert_eq!(file_custom_id_search_target(Some("/regexp/")), None);
        assert_eq!(file_custom_id_search_target(None), None);
    }

    #[test]
    fn heading_title_search_target_strips_one_leading_star_and_unescapes_brackets() {
        assert_eq!(
            heading_title_search_target(Some(r"*\[2026-07-01 Wed\] Review")),
            Some("[2026-07-01 Wed] Review".to_string())
        );
        assert_eq!(
            heading_title_search_target(Some("**Literal Star")),
            Some("*Literal Star".to_string())
        );
        assert_eq!(heading_title_search_target(Some("#custom-id")), None);
        assert_eq!(heading_title_search_target(None), None);
    }

    #[test]
    fn same_file_fuzzy_star_heading_target_requires_exactly_one_leading_star() {
        assert_eq!(
            same_file_fuzzy_star_heading_target(r"*\[2026-07-01 Wed\] Review"),
            Some("[2026-07-01 Wed] Review".to_string())
        );
        assert_eq!(same_file_fuzzy_star_heading_target("**Heading"), None);
        assert_eq!(same_file_fuzzy_star_heading_target("Heading"), None);
    }

    #[test]
    fn same_file_fuzzy_custom_id_target_requires_exactly_one_leading_hash() {
        assert_eq!(
            same_file_fuzzy_custom_id_target("# Custom-ID "),
            Some("Custom-ID".to_string())
        );
        assert_eq!(same_file_fuzzy_custom_id_target("##custom-id"), None);
        assert_eq!(same_file_fuzzy_custom_id_target("custom-id"), None);
    }

    fn seed_file_link_fixture(
        connection: &Connection,
        source_path: &str,
        raw: &str,
        link_type: &str,
        path: &str,
        search_option: Option<&str>,
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
                    search_option,
                ],
            )
            .expect("file link insert should succeed");
    }

    fn seed_known_target_file(connection: &Connection, path: &str, file_id: i64) {
        seed_known_target_file_with_root_title(connection, path, file_id, path);
    }

    fn seed_known_target_file_with_root_title(
        connection: &Connection,
        path: &str,
        file_id: i64,
        root_title: &str,
    ) {
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
                    root_title,
                    root_title,
                ),
            )
            .expect("target heading insert should succeed");
    }

    fn seed_target_heading(
        connection: &Connection,
        heading_id: i64,
        file_id: i64,
        level: i64,
        title: &str,
    ) {
        connection
            .execute(
                "INSERT INTO headings
                 (id, file_id, parent_id, level, byte_start, byte_end, title, title_raw)
                 VALUES (?1, ?2, ?3, ?4, ?5, ?6, ?7, ?8)",
                (
                    heading_id,
                    file_id,
                    Some(file_id),
                    level,
                    heading_id * 10,
                    heading_id * 10 + 5,
                    title,
                    title,
                ),
            )
            .expect("target heading insert should succeed");
    }

    fn seed_heading_property(
        connection: &Connection,
        heading_id: i64,
        key: &str,
        value: Option<&str>,
    ) {
        connection
            .execute(
                "INSERT INTO properties
                 (heading_id, key, value, source, append, line_number)
                 VALUES (?1, ?2, ?3, ?4, ?5, ?6)",
                params![heading_id, key, value, "property_drawer", 0_i64, 1_i64],
            )
            .expect("heading property insert should succeed");
    }
}
