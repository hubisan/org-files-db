use std::{
    cell::RefCell,
    collections::{BTreeMap, BTreeSet, HashMap},
    path::{Component, Path, PathBuf},
};

use rusqlite::{params, Connection, Statement};

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

/// Selects the links a scoped pass re-resolves; see `LinkResolver::resolve_scoped`. Same-file
/// links (`custom-id`, `fuzzy`) and unsupported types only depend on their own file.
const SCOPE_FILTER: &str =
    "WHERE links.file_id IN (SELECT file_id FROM temp.link_resolver_scope_files)
    OR links.resolution_status IS NULL
    OR (links.link_type IN ('file', 'file+sys', 'file+emacs', 'id')
        AND (links.target_file_id IS NULL
            OR links.target_file_id IN (SELECT file_id FROM temp.link_resolver_scope_files)
            OR (links.link_type = 'id'
                AND lower(links.target_id) IN (SELECT id FROM temp.link_resolver_scope_ids))))";

/// Files whose indexed content changed in one apply (created, modified or metadata-updated).
/// Deleted files need no entry: their inbound links lose the target file through the foreign
/// key and are re-resolved as links without a target file.
#[derive(Debug, Default)]
pub(crate) struct ResolutionScope {
    pub(crate) affected_file_ids: BTreeSet<i64>,
}

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
    target_file_id: Option<i64>,
}

/// The links to resolve in resolution order, plus their stored resolution state by link id.
type LoadedLinks = (Vec<StoredLink>, BTreeMap<i64, StoredResolutionState>);

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
    /// Loads the lookup maps. With `restrict_files`, heading titles, custom ids and roots are
    /// limited to the files in `temp.link_resolver_index_files` and global ids to the ids in
    /// `temp.link_resolver_lookup_ids`; each id still lists every file that defines it, so
    /// duplicate detection is unaffected.
    fn load(connection: &Connection, restrict_files: bool) -> Result<Self, DbWriteError> {
        let file_filter = |column: &str| {
            if restrict_files {
                format!("AND {column} IN (SELECT file_id FROM temp.link_resolver_index_files)")
            } else {
                String::new()
            }
        };
        let mut index = Self::default();
        let err = |operation| move |source| DbWriteError::Write { operation, source };
        let mut statement = connection
            .prepare(&format!(
                "SELECT headings.file_id, headings.id, properties.value
                 FROM properties
                 INNER JOIN headings ON headings.id = properties.heading_id
                 WHERE headings.level > 0 AND properties.key = 'ID' {}
                 ORDER BY headings.file_id, headings.byte_start, headings.id, properties.id",
                if restrict_files {
                    "AND lower(properties.value) IN (SELECT id FROM temp.link_resolver_lookup_ids)"
                } else {
                    ""
                }
            ))
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
            .prepare(&format!(
                "SELECT file_id, id, title FROM headings
                 WHERE level > 0 {} ORDER BY file_id, byte_start, id",
                file_filter("file_id")
            ))
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
            .prepare(&format!(
                "SELECT headings.file_id, headings.id, properties.value
                 FROM properties
                 INNER JOIN headings ON headings.id = properties.heading_id
                 WHERE headings.level > 0 AND properties.key = 'CUSTOM_ID' {}
                 ORDER BY headings.file_id, headings.byte_start, headings.id, properties.id",
                file_filter("headings.file_id")
            ))
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
            .prepare(&format!(
                "SELECT file_id, id FROM headings
                 WHERE level = 0 AND parent_id IS NULL {} ORDER BY file_id, byte_start, id",
                file_filter("file_id")
            ))
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
        Self::resolve(connection, indexed_universe, None)
    }

    /// Re-resolves only the links whose resolution can depend on `scope`: links owned by
    /// affected files, links never resolved, and file or id links that lack a target file,
    /// point at an affected file, or name an id defined in an affected file. The result
    /// equals `resolve_all` on a database whose other links were already up to date.
    pub(crate) fn resolve_scoped(
        connection: &Connection,
        indexed_universe: &IndexedUniverse,
        scope: &ResolutionScope,
    ) -> Result<LinkResolutionReport, DbWriteError> {
        Self::resolve(connection, indexed_universe, Some(scope))
    }

    fn resolve(
        connection: &Connection,
        indexed_universe: &IndexedUniverse,
        scope: Option<&ResolutionScope>,
    ) -> Result<LinkResolutionReport, DbWriteError> {
        let filter = match scope {
            Some(scope) => {
                Self::prepare_scope_tables(connection, scope)?;
                SCOPE_FILTER
            }
            None => "",
        };
        let known_files = Self::load_known_files(connection)?;
        let (links, before) = Self::load_links(connection, filter)?;
        if let Some(scope) = scope {
            Self::prepare_index_files(connection, scope, &links, &known_files)?;
        }
        let index = ResolutionIndex::load(connection, scope.is_some())?;
        let writer = ResolutionWriter::new(connection, before)?;
        for link in links {
            Self::resolve_link(&writer, &link, indexed_universe, &known_files, &index)?;
        }
        if scope.is_some() {
            Self::drop_scope_tables(connection)?;
        }
        Ok(LinkResolutionReport {
            changed_source_paths: writer.into_changed_source_paths(),
        })
    }

    fn prepare_scope_tables(
        connection: &Connection,
        scope: &ResolutionScope,
    ) -> Result<(), DbWriteError> {
        let err = |operation| move |source| DbWriteError::Write { operation, source };
        Self::drop_scope_tables(connection)?;
        connection
            .execute_batch(
                "CREATE TEMP TABLE link_resolver_scope_files (file_id INTEGER PRIMARY KEY);
                 CREATE TEMP TABLE link_resolver_scope_ids (id TEXT PRIMARY KEY);
                 CREATE TEMP TABLE link_resolver_index_files (file_id INTEGER PRIMARY KEY);
                 CREATE TEMP TABLE link_resolver_lookup_ids (id TEXT PRIMARY KEY);",
            )
            .map_err(err("link_resolver.scope.create"))?;
        let mut insert = connection
            .prepare("INSERT OR IGNORE INTO temp.link_resolver_scope_files (file_id) VALUES (?1)")
            .map_err(err("link_resolver.scope.files.prepare"))?;
        for file_id in &scope.affected_file_ids {
            insert
                .execute([file_id])
                .map_err(err("link_resolver.scope.files.insert"))?;
        }
        drop(insert);
        connection
            .execute(
                "INSERT OR IGNORE INTO temp.link_resolver_scope_ids (id)
                 SELECT lower(properties.value)
                 FROM properties
                 INNER JOIN headings ON headings.id = properties.heading_id
                 WHERE headings.level > 0
                   AND properties.key = 'ID'
                   AND properties.value IS NOT NULL
                   AND headings.file_id IN (SELECT file_id FROM temp.link_resolver_scope_files)",
                [],
            )
            .map_err(err("link_resolver.scope.ids.insert"))?;
        Ok(())
    }

    /// Files whose headings the scoped links can resolve into: affected files, the current
    /// target files of scoped links, and the files that scoped file links point at now.
    fn prepare_index_files(
        connection: &Connection,
        scope: &ResolutionScope,
        links: &[StoredLink],
        known_files: &KnownFiles,
    ) -> Result<(), DbWriteError> {
        let mut files = scope.affected_file_ids.clone();
        let mut lookup_ids = BTreeSet::new();
        let home_dir = current_home_dir();
        for link in links {
            if link.link_type == "id" {
                lookup_ids.insert(normalize_id_target(&link.path).to_lowercase());
            }
            files.extend(link.target_file_id);
            files.insert(link.source_file_id);
            if matches!(link.link_type.as_str(), "file" | "file+sys" | "file+emacs") {
                if let Some(target) = normalize_file_target_path(
                    &link.source_file_path,
                    Path::new(&link.path),
                    home_dir.as_deref(),
                )
                .and_then(|path| known_files.by_path.get(&path).copied())
                {
                    files.insert(target);
                }
            }
        }
        let err = |operation| move |source| DbWriteError::Write { operation, source };
        let mut insert = connection
            .prepare("INSERT OR IGNORE INTO temp.link_resolver_index_files (file_id) VALUES (?1)")
            .map_err(err("link_resolver.scope.index_files.prepare"))?;
        for file_id in files {
            insert
                .execute([file_id])
                .map_err(err("link_resolver.scope.index_files.insert"))?;
        }
        drop(insert);
        let mut insert = connection
            .prepare("INSERT OR IGNORE INTO temp.link_resolver_lookup_ids (id) VALUES (?1)")
            .map_err(err("link_resolver.scope.lookup_ids.prepare"))?;
        for id in lookup_ids {
            insert
                .execute([id])
                .map_err(err("link_resolver.scope.lookup_ids.insert"))?;
        }
        Ok(())
    }

    fn drop_scope_tables(connection: &Connection) -> Result<(), DbWriteError> {
        connection
            .execute_batch(
                "DROP TABLE IF EXISTS temp.link_resolver_scope_files;
                 DROP TABLE IF EXISTS temp.link_resolver_scope_ids;
                 DROP TABLE IF EXISTS temp.link_resolver_index_files;
                 DROP TABLE IF EXISTS temp.link_resolver_lookup_ids;",
            )
            .map_err(|source| DbWriteError::Write {
                operation: "link_resolver.scope.drop",
                source,
            })
    }

    fn load_links(connection: &Connection, filter: &str) -> Result<LoadedLinks, DbWriteError> {
        let mut statement = connection
            .prepare(&format!(
                "SELECT links.id, links.file_id, links.link_type, links.path, links.search_option, files.path, files.identity, links.target_file_id, links.path_absolute, links.target_heading_id,
                        links.target_custom_id, links.target_id, links.resolution_status,
                        links.resolution_diagnostic
                 FROM links
                 INNER JOIN files ON files.id = links.file_id
                 {filter}
                 ORDER BY links.file_id, links.byte_start, links.id"
            ))
            .map_err(|source| DbWriteError::Write {
                operation: "link_resolver.load_links.prepare",
                source,
            })?;
        let rows = statement
            .query_map([], |row| {
                let source_display_path = row.get::<_, String>(5)?;
                let state = StoredResolutionState {
                    source_path: source_display_path.clone(),
                    path_absolute: row.get(8)?,
                    target_file_id: row.get(7)?,
                    target_heading_id: row.get(9)?,
                    target_custom_id: row.get(10)?,
                    target_id: row.get(11)?,
                    resolution_status: row.get(12)?,
                    resolution_diagnostic: row.get(13)?,
                };
                let link = StoredLink {
                    id: row.get(0)?,
                    source_file_id: row.get(1)?,
                    link_type: row.get(2)?,
                    path: row.get(3)?,
                    search_option: row.get(4)?,
                    source_file_path: {
                        let display_path = source_display_path;
                        let identity = row.get::<_, Option<Vec<u8>>>(6)?;
                        identity
                            .and_then(FileIdentity::from_stored_bytes)
                            .and_then(|identity| identity.to_path())
                            .unwrap_or_else(|| PathBuf::from(display_path))
                    },
                    target_file_id: row.get(7)?,
                };
                Ok((link, state))
            })
            .map_err(|source| DbWriteError::Write {
                operation: "link_resolver.load_links.query",
                source,
            })?;
        let mut links = Vec::new();
        let mut states = BTreeMap::new();
        for row in rows {
            let (link, state) = row.map_err(|source| DbWriteError::Write {
                operation: "link_resolver.load_links.collect",
                source,
            })?;
            states.insert(link.id, state);
            links.push(link);
        }
        Ok((links, states))
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
        connection: &ResolutionWriter<'_>,
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
        connection: &ResolutionWriter<'_>,
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
        connection: &ResolutionWriter<'_>,
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
        connection: &ResolutionWriter<'_>,
        link: &StoredLink,
        index: &ResolutionIndex,
    ) -> Result<(), DbWriteError> {
        let custom_id_target = normalize_custom_id_lookup_target(link.path.as_str());

        Self::resolve_same_file_custom_id_target(connection, link, &custom_id_target, index)
    }

    fn resolve_same_file_custom_id_target(
        connection: &ResolutionWriter<'_>,
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
        connection: &ResolutionWriter<'_>,
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
        connection: &ResolutionWriter<'_>,
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
        connection: &ResolutionWriter<'_>,
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
        connection: &ResolutionWriter<'_>,
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
        writer: &ResolutionWriter<'_>,
        link_id: i64,
        resolution: Resolution<'_>,
    ) -> Result<(), DbWriteError> {
        writer.write(link_id, resolution)
    }
}

/// Applies resolutions for one pass: every link starts from the reset state (all resolver
/// owned columns NULL), so a resolution is compared against the stored row and only written
/// when it differs. The UPDATE statement is prepared once per pass.
struct ResolutionWriter<'conn> {
    statement: RefCell<Statement<'conn>>,
    before: BTreeMap<i64, StoredResolutionState>,
    changed_source_paths: RefCell<BTreeSet<String>>,
}

impl<'conn> ResolutionWriter<'conn> {
    fn new(
        connection: &'conn Connection,
        before: BTreeMap<i64, StoredResolutionState>,
    ) -> Result<Self, DbWriteError> {
        let statement = connection
            .prepare(
                "UPDATE links
                 SET path_absolute = ?2,
                     target_file_id = ?3,
                     target_heading_id = ?4,
                     target_custom_id = ?5,
                     target_id = ?6,
                     resolution_status = ?7,
                     resolution_diagnostic = ?8
                 WHERE id = ?1",
            )
            .map_err(|source| DbWriteError::Write {
                operation: "link_resolver.write_resolution.prepare",
                source,
            })?;
        Ok(Self {
            statement: RefCell::new(statement),
            before,
            changed_source_paths: RefCell::new(BTreeSet::new()),
        })
    }

    fn write(&self, link_id: i64, resolution: Resolution<'_>) -> Result<(), DbWriteError> {
        // `Keep` and `Null` both leave NULL after the implicit reset.
        let path_absolute = resolution.path_absolute.bind().1;
        let target_file_id = resolution.target_file_id.bind().1;
        let target_heading_id = resolution.target_heading_id.bind().1;
        let target_custom_id = resolution.target_custom_id.bind().1;
        let target_id = resolution.target_id.bind().1;
        let previous = self.before.get(&link_id);
        let unchanged = previous.is_some_and(|state| {
            state.path_absolute == path_absolute
                && state.target_file_id == target_file_id
                && state.target_heading_id == target_heading_id
                && state.target_custom_id.as_deref() == target_custom_id
                && state.target_id.as_deref() == target_id
                && state.resolution_status.as_deref() == Some(resolution.status)
                && state.resolution_diagnostic.as_deref() == resolution.diagnostic
        });
        if unchanged {
            return Ok(());
        }
        self.statement
            .borrow_mut()
            .execute(params![
                link_id,
                path_absolute,
                target_file_id,
                target_heading_id,
                target_custom_id,
                target_id,
                resolution.status,
                resolution.diagnostic,
            ])
            .map_err(|source| DbWriteError::Write {
                operation: "link_resolver.write_resolution",
                source,
            })?;
        if let Some(state) = previous {
            self.changed_source_paths
                .borrow_mut()
                .insert(state.source_path.clone());
        }
        Ok(())
    }

    fn into_changed_source_paths(self) -> BTreeSet<String> {
        self.changed_source_paths.into_inner()
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
        HEADING_TITLE_MISSING_DIAGNOSTIC, ID_MISSING_DIAGNOSTIC, MISSING_SYNTHETIC_ROOT_DIAGNOSTIC,
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

    enum TestUniverse {
        Empty,
        ExactSourceAndTarget,
        ExactSourceOnly,
        RecursiveTmp,
    }

    impl TestUniverse {
        fn build(&self) -> IndexedUniverse {
            let mut universe = IndexedUniverse::default();
            match self {
                Self::Empty => {}
                Self::ExactSourceAndTarget => {
                    universe.add_exact_path(PathBuf::from("/tmp/source.org"));
                    universe.add_exact_path(PathBuf::from("/tmp/target.org"));
                }
                Self::ExactSourceOnly => {
                    universe.add_exact_path(PathBuf::from("/tmp/source.org"));
                }
                Self::RecursiveTmp => universe.add_recursive_root(PathBuf::from("/tmp")),
            }
            universe
        }
    }

    /// Resolver-owned columns after `resolve_all`, in this order: `path_absolute`,
    /// `target_file_id`, `target_heading_id`, `target_custom_id`, `target_id`,
    /// `resolution_status`, `resolution_diagnostic`.
    type Expected = (
        Option<&'static str>,
        Option<i64>,
        Option<i64>,
        Option<&'static str>,
        Option<&'static str>,
        &'static str,
        Option<&'static str>,
    );

    type ResolvedColumns = (
        Option<String>,
        Option<i64>,
        Option<i64>,
        Option<String>,
        Option<String>,
        String,
        Option<String>,
    );

    struct ResolveCase {
        name: &'static str,
        raw: &'static str,
        link_type: &'static str,
        path: &'static str,
        search_option: Option<&'static str>,
        /// Overrides for `raw_target` and `raw_description` when they differ from `raw`.
        raw_parts: Option<(&'static str, &'static str)>,
        universe: TestUniverse,
        seed_targets: fn(&Connection),
        expected: Expected,
    }

    const TARGET_PATH: Option<&str> = Some("/tmp/target.org");

    fn seed_none(_connection: &Connection) {}

    #[test]
    fn resolve_all_resolves_links_by_type_and_target_shape() {
        let cases = [
            // Org id links.
            ResolveCase {
                name: "id: exactly one match resolves",
                raw: "id:foo",
                link_type: "id",
                path: "foo",
                search_option: None,
                raw_parts: None,
                universe: TestUniverse::Empty,
                seed_targets: |c| {
                    seed_known_target_file(c, "/tmp/target.org", 2);
                    seed_target_heading(c, 20, 2, 1, "Heading");
                    seed_heading_property(c, 20, "ID", Some("foo"));
                },
                expected: (None, Some(2), Some(20), None, Some("foo"), "resolved", None),
            },
            ResolveCase {
                name: "id: whitespace is trimmed and matching is case-insensitive",
                raw: "[[id: FOO ][Description]]",
                link_type: "id",
                path: " FOO ",
                search_option: None,
                raw_parts: Some(("id: FOO ", "Description")),
                universe: TestUniverse::Empty,
                seed_targets: |c| {
                    seed_known_target_file(c, "/tmp/target.org", 2);
                    seed_target_heading(c, 20, 2, 1, "Heading");
                    seed_heading_property(c, 20, "ID", Some("foo"));
                },
                expected: (None, Some(2), Some(20), None, Some("FOO"), "resolved", None),
            },
            ResolveCase {
                name: "id: duplicate ids across files are ambiguous",
                raw: "<id:dup>",
                link_type: "id",
                path: "dup",
                search_option: None,
                raw_parts: None,
                universe: TestUniverse::Empty,
                seed_targets: |c| {
                    seed_known_target_file(c, "/tmp/target-a.org", 2);
                    seed_target_heading(c, 20, 2, 1, "First");
                    seed_heading_property(c, 20, "ID", Some("dup"));
                    seed_known_target_file(c, "/tmp/target-b.org", 3);
                    seed_target_heading(c, 30, 3, 1, "Second");
                    seed_heading_property(c, 30, "ID", Some("DUP"));
                },
                expected: (
                    None,
                    None,
                    None,
                    None,
                    Some("dup"),
                    "ambiguous",
                    Some(DUPLICATE_ID_DIAGNOSTIC),
                ),
            },
            ResolveCase {
                name: "id: no matching id is unresolved",
                raw: "[[id:missing]]",
                link_type: "id",
                path: "missing",
                search_option: None,
                raw_parts: None,
                universe: TestUniverse::Empty,
                seed_targets: |c| {
                    seed_known_target_file(c, "/tmp/target.org", 2);
                    seed_target_heading(c, 20, 2, 1, "Heading");
                    seed_heading_property(c, 20, "ID", Some("abc"));
                },
                expected: (
                    None,
                    None,
                    None,
                    None,
                    Some("missing"),
                    "unresolved",
                    Some(ID_MISSING_DIAGNOSTIC),
                ),
            },
            ResolveCase {
                name: "id: padded lookup does not prefix-match a longer id",
                raw: "[[id:ab ]]",
                link_type: "id",
                path: "ab ",
                search_option: None,
                raw_parts: None,
                universe: TestUniverse::Empty,
                seed_targets: |c| {
                    seed_known_target_file(c, "/tmp/target.org", 2);
                    seed_target_heading(c, 20, 2, 1, "Heading");
                    seed_heading_property(c, 20, "ID", Some("abc"));
                },
                expected: (
                    None,
                    None,
                    None,
                    None,
                    Some("ab"),
                    "unresolved",
                    Some(ID_MISSING_DIAGNOSTIC),
                ),
            },
            ResolveCase {
                name: "id: leading padding is trimmed before matching",
                raw: "[[id: ab]]",
                link_type: "id",
                path: " ab",
                search_option: None,
                raw_parts: None,
                universe: TestUniverse::Empty,
                seed_targets: |c| {
                    seed_known_target_file(c, "/tmp/target.org", 2);
                    seed_target_heading(c, 20, 2, 1, "Heading");
                    seed_heading_property(c, 20, "ID", Some("abc"));
                },
                expected: (
                    None,
                    None,
                    None,
                    None,
                    Some("ab"),
                    "unresolved",
                    Some(ID_MISSING_DIAGNOSTIC),
                ),
            },
            ResolveCase {
                name: "id: trailing lookup padding with an inner space resolves",
                raw: "[[id:123 56 ]]",
                link_type: "id",
                path: "123 56 ",
                search_option: None,
                raw_parts: None,
                universe: TestUniverse::Empty,
                seed_targets: |c| {
                    seed_known_target_file(c, "/tmp/target.org", 2);
                    seed_target_heading(c, 20, 2, 1, "Heading");
                    seed_heading_property(c, 20, "ID", Some("123 56"));
                },
                expected: (
                    None,
                    Some(2),
                    Some(20),
                    None,
                    Some("123 56"),
                    "resolved",
                    None,
                ),
            },
            ResolveCase {
                name: "id: leading lookup padding resolves",
                raw: "[[id: 23]]",
                link_type: "id",
                path: " 23",
                search_option: None,
                raw_parts: None,
                universe: TestUniverse::Empty,
                seed_targets: |c| {
                    seed_known_target_file(c, "/tmp/target.org", 2);
                    seed_target_heading(c, 20, 2, 1, "Heading");
                    seed_heading_property(c, 20, "ID", Some("23"));
                },
                expected: (None, Some(2), Some(20), None, Some("23"), "resolved", None),
            },
            // File links without search options.
            ResolveCase {
                name: "file: known target resolves",
                raw: "[[file:target.org]]",
                link_type: "file",
                path: "target.org",
                search_option: None,
                raw_parts: None,
                universe: TestUniverse::ExactSourceAndTarget,
                seed_targets: |c| seed_known_target_file(c, "/tmp/target.org", 2),
                expected: (TARGET_PATH, Some(2), Some(2), None, None, "resolved", None),
            },
            ResolveCase {
                name: "file+sys: known target resolves",
                raw: "[[file+sys:target.org]]",
                link_type: "file+sys",
                path: "target.org",
                search_option: None,
                raw_parts: None,
                universe: TestUniverse::RecursiveTmp,
                seed_targets: |c| seed_known_target_file(c, "/tmp/target.org", 2),
                expected: (TARGET_PATH, Some(2), Some(2), None, None, "resolved", None),
            },
            ResolveCase {
                name: "file+sys: missing target is broken",
                raw: "[[file+sys:missing.org]]",
                link_type: "file+sys",
                path: "missing.org",
                search_option: None,
                raw_parts: None,
                universe: TestUniverse::RecursiveTmp,
                seed_targets: |c| seed_known_target_file(c, "/tmp/target.org", 2),
                expected: (
                    Some("/tmp/missing.org"),
                    None,
                    None,
                    None,
                    None,
                    "broken",
                    Some(FILE_MISSING_DIAGNOSTIC),
                ),
            },
            ResolveCase {
                name: "file+emacs: known target resolves",
                raw: "[[file+emacs:target.org]]",
                link_type: "file+emacs",
                path: "target.org",
                search_option: None,
                raw_parts: None,
                universe: TestUniverse::RecursiveTmp,
                seed_targets: |c| seed_known_target_file(c, "/tmp/target.org", 2),
                expected: (TARGET_PATH, Some(2), Some(2), None, None, "resolved", None),
            },
            ResolveCase {
                name: "file+emacs: missing target is broken",
                raw: "[[file+emacs:missing.org]]",
                link_type: "file+emacs",
                path: "missing.org",
                search_option: None,
                raw_parts: None,
                universe: TestUniverse::RecursiveTmp,
                seed_targets: |c| seed_known_target_file(c, "/tmp/target.org", 2),
                expected: (
                    Some("/tmp/missing.org"),
                    None,
                    None,
                    None,
                    None,
                    "broken",
                    Some(FILE_MISSING_DIAGNOSTIC),
                ),
            },
            ResolveCase {
                name: "file: file-only link maps to the target root heading",
                raw: "[[file:target.org]]",
                link_type: "file",
                path: "target.org",
                search_option: None,
                raw_parts: None,
                universe: TestUniverse::ExactSourceAndTarget,
                seed_targets: |c| {
                    seed_known_target_file(c, "/tmp/target.org", 2);
                    seed_target_heading(c, 20, 2, 1, "Child heading");
                },
                expected: (TARGET_PATH, Some(2), Some(2), None, None, "resolved", None),
            },
            ResolveCase {
                name: "file: missing synthetic root heading is resolved corruption",
                raw: "[[file:target.org]]",
                link_type: "file",
                path: "target.org",
                search_option: None,
                raw_parts: None,
                universe: TestUniverse::ExactSourceAndTarget,
                seed_targets: |c| {
                    c.execute(
                        "INSERT INTO files (id, path, mtime_ns, size) VALUES (?1, ?2, ?3, ?4)",
                        (2_i64, "/tmp/target.org", 30_i64, 40_i64),
                    )
                    .expect("target file insert should succeed");
                },
                expected: (
                    TARGET_PATH,
                    Some(2),
                    None,
                    None,
                    None,
                    "resolved",
                    Some(MISSING_SYNTHETIC_ROOT_DIAGNOSTIC),
                ),
            },
            ResolveCase {
                name: "file: missing target inside universe is broken",
                raw: "[[file:missing.org]]",
                link_type: "file",
                path: "missing.org",
                search_option: None,
                raw_parts: None,
                universe: TestUniverse::RecursiveTmp,
                seed_targets: seed_none,
                expected: (
                    Some("/tmp/missing.org"),
                    None,
                    None,
                    None,
                    None,
                    "broken",
                    Some(FILE_MISSING_DIAGNOSTIC),
                ),
            },
            ResolveCase {
                name: "file: target outside universe is unresolved",
                raw: "[[file:/outside/world.org]]",
                link_type: "file",
                path: "/outside/world.org",
                search_option: None,
                raw_parts: None,
                universe: TestUniverse::ExactSourceOnly,
                seed_targets: seed_none,
                expected: (
                    Some("/outside/world.org"),
                    None,
                    None,
                    None,
                    None,
                    "unresolved",
                    Some(FILE_OUTSIDE_UNIVERSE_DIAGNOSTIC),
                ),
            },
            ResolveCase {
                name: "file: dot-relative path resolves",
                raw: "[[./target.org]]",
                link_type: "file",
                path: "./target.org",
                search_option: None,
                raw_parts: None,
                universe: TestUniverse::ExactSourceAndTarget,
                seed_targets: |c| seed_known_target_file(c, "/tmp/target.org", 2),
                expected: (TARGET_PATH, Some(2), Some(2), None, None, "resolved", None),
            },
            ResolveCase {
                name: "file: absolute path resolves",
                raw: "[[/tmp/target.org]]",
                link_type: "file",
                path: "/tmp/target.org",
                search_option: None,
                raw_parts: None,
                universe: TestUniverse::ExactSourceAndTarget,
                seed_targets: |c| seed_known_target_file(c, "/tmp/target.org", 2),
                expected: (TARGET_PATH, Some(2), Some(2), None, None, "resolved", None),
            },
            // File links with unsupported search options.
            ResolveCase {
                name: "file::/regexp/: falls back to file without root heading",
                raw: "[[file:target.org::/regexp/]]",
                link_type: "file",
                path: "target.org",
                search_option: Some("/regexp/"),
                raw_parts: None,
                universe: TestUniverse::ExactSourceAndTarget,
                seed_targets: |c| seed_known_target_file(c, "/tmp/target.org", 2),
                expected: (TARGET_PATH, Some(2), None, None, None, "resolved", None),
            },
            ResolveCase {
                name: "file::/regexp/: keeps resolved file target with headings present",
                raw: "[[file:target.org::/regexp/]]",
                link_type: "file",
                path: "target.org",
                search_option: Some("/regexp/"),
                raw_parts: None,
                universe: TestUniverse::ExactSourceAndTarget,
                seed_targets: |c| {
                    seed_known_target_file(c, "/tmp/target.org", 2);
                    seed_target_heading(c, 20, 2, 1, "Target");
                },
                expected: (TARGET_PATH, Some(2), None, None, None, "resolved", None),
            },
            // File links with heading-title search options.
            ResolveCase {
                name: "file::*title: unescapes brackets and resolves",
                raw: "[[file:target.org::*[2026-07-01 Wed] Review]]",
                link_type: "file",
                path: "target.org",
                search_option: Some(r"*\[2026-07-01 Wed\] Review"),
                raw_parts: None,
                universe: TestUniverse::ExactSourceAndTarget,
                seed_targets: |c| {
                    seed_known_target_file(c, "/tmp/target.org", 2);
                    seed_target_heading(c, 20, 2, 1, "[2026-07-01 Wed] Review");
                },
                expected: (TARGET_PATH, Some(2), Some(20), None, None, "resolved", None),
            },
            ResolveCase {
                name: "file::*title: whitespace and case are normalized",
                raw: "[[file:target.org::*   main index   ]]",
                link_type: "file",
                path: "target.org",
                search_option: Some("*   main index   "),
                raw_parts: None,
                universe: TestUniverse::ExactSourceAndTarget,
                seed_targets: |c| {
                    seed_known_target_file(c, "/tmp/target.org", 2);
                    seed_target_heading(c, 20, 2, 1, "Main Index");
                },
                expected: (TARGET_PATH, Some(2), Some(20), None, None, "resolved", None),
            },
            ResolveCase {
                name: "file::*title: unicode case-insensitive match",
                raw: "[[file:target.org::*ärger]]",
                link_type: "file",
                path: "target.org",
                search_option: Some("*ärger"),
                raw_parts: None,
                universe: TestUniverse::ExactSourceAndTarget,
                seed_targets: |c| {
                    seed_known_target_file(c, "/tmp/target.org", 2);
                    seed_target_heading(c, 20, 2, 1, "Ärger");
                },
                expected: (TARGET_PATH, Some(2), Some(20), None, None, "resolved", None),
            },
            ResolveCase {
                name: "file::*title: missing heading is broken",
                raw: "[[file:target.org::*Missing]]",
                link_type: "file",
                path: "target.org",
                search_option: Some("*Missing"),
                raw_parts: None,
                universe: TestUniverse::ExactSourceAndTarget,
                seed_targets: |c| {
                    seed_known_target_file(c, "/tmp/target.org", 2);
                    seed_target_heading(c, 20, 2, 1, "Existing");
                },
                expected: (
                    TARGET_PATH,
                    Some(2),
                    None,
                    None,
                    None,
                    "broken",
                    Some(HEADING_TITLE_MISSING_DIAGNOSTIC),
                ),
            },
            ResolveCase {
                name: "file::*title: duplicate selects first in document order",
                raw: "[[file:target.org::*Duplicate]]",
                link_type: "file",
                path: "target.org",
                search_option: Some("*Duplicate"),
                raw_parts: None,
                universe: TestUniverse::ExactSourceAndTarget,
                seed_targets: |c| {
                    seed_known_target_file(c, "/tmp/target.org", 2);
                    seed_target_heading(c, 20, 2, 1, "Duplicate");
                    seed_target_heading(c, 21, 2, 1, "Duplicate");
                },
                expected: (TARGET_PATH, Some(2), Some(20), None, None, "resolved", None),
            },
            ResolveCase {
                name: "file::*title: synthetic root heading is excluded from matches",
                raw: "[[file:target.org::*Only Root]]",
                link_type: "file",
                path: "target.org",
                search_option: Some("*Only Root"),
                raw_parts: None,
                universe: TestUniverse::ExactSourceAndTarget,
                seed_targets: |c| {
                    seed_known_target_file_with_root_title(c, "/tmp/target.org", 2, "Only Root");
                },
                expected: (
                    TARGET_PATH,
                    Some(2),
                    None,
                    None,
                    None,
                    "broken",
                    Some(HEADING_TITLE_MISSING_DIAGNOSTIC),
                ),
            },
            // File links with custom-id search options.
            ResolveCase {
                name: "file::#id: resolves to the custom-id heading",
                raw: "[[file:target.org::#custom-id]]",
                link_type: "file",
                path: "target.org",
                search_option: Some("#custom-id"),
                raw_parts: None,
                universe: TestUniverse::ExactSourceAndTarget,
                seed_targets: |c| {
                    seed_known_target_file(c, "/tmp/target.org", 2);
                    seed_target_heading(c, 20, 2, 1, "Target");
                    seed_heading_property(c, 20, "CUSTOM_ID", Some("custom-id"));
                },
                expected: (
                    TARGET_PATH,
                    Some(2),
                    Some(20),
                    Some("custom-id"),
                    None,
                    "resolved",
                    None,
                ),
            },
            ResolveCase {
                name: "file::#id: whitespace is trimmed",
                raw: "[[file:target.org::# abc ]]",
                link_type: "file",
                path: "target.org",
                search_option: Some("# abc "),
                raw_parts: None,
                universe: TestUniverse::ExactSourceAndTarget,
                seed_targets: |c| {
                    seed_known_target_file(c, "/tmp/target.org", 2);
                    seed_target_heading(c, 20, 2, 1, "Target");
                    seed_heading_property(c, 20, "CUSTOM_ID", Some("abc"));
                },
                expected: (
                    TARGET_PATH,
                    Some(2),
                    Some(20),
                    Some("abc"),
                    None,
                    "resolved",
                    None,
                ),
            },
            ResolveCase {
                name: "file::#id: missing custom id is broken",
                raw: "[[file:target.org::#missing]]",
                link_type: "file",
                path: "target.org",
                search_option: Some("#missing"),
                raw_parts: None,
                universe: TestUniverse::ExactSourceAndTarget,
                seed_targets: |c| {
                    seed_known_target_file(c, "/tmp/target.org", 2);
                    seed_target_heading(c, 20, 2, 1, "Target");
                },
                expected: (
                    TARGET_PATH,
                    Some(2),
                    None,
                    Some("missing"),
                    None,
                    "broken",
                    Some(CUSTOM_ID_MISSING_DIAGNOSTIC),
                ),
            },
            ResolveCase {
                name: "file::#id: duplicate selects first in document order",
                raw: "[[file:target.org::#dup]]",
                link_type: "file",
                path: "target.org",
                search_option: Some("#dup"),
                raw_parts: None,
                universe: TestUniverse::ExactSourceAndTarget,
                seed_targets: |c| {
                    seed_known_target_file(c, "/tmp/target.org", 2);
                    seed_target_heading(c, 20, 2, 1, "First");
                    seed_target_heading(c, 21, 2, 1, "Second");
                    seed_heading_property(c, 20, "CUSTOM_ID", Some("dup"));
                    seed_heading_property(c, 21, "CUSTOM_ID", Some("DUP"));
                },
                expected: (
                    TARGET_PATH,
                    Some(2),
                    Some(20),
                    Some("dup"),
                    None,
                    "resolved",
                    None,
                ),
            },
            ResolveCase {
                name: "file::#id: missing file is broken as file missing",
                raw: "[[file:missing.org::#custom-id]]",
                link_type: "file",
                path: "missing.org",
                search_option: Some("#custom-id"),
                raw_parts: None,
                universe: TestUniverse::RecursiveTmp,
                seed_targets: seed_none,
                expected: (
                    Some("/tmp/missing.org"),
                    None,
                    None,
                    None,
                    None,
                    "broken",
                    Some(FILE_MISSING_DIAGNOSTIC),
                ),
            },
            ResolveCase {
                name: "file::#id: lookup does not prefix-match a longer custom id",
                raw: "[[file:target.org::# ab]]",
                link_type: "file",
                path: "target.org",
                search_option: Some("# ab"),
                raw_parts: None,
                universe: TestUniverse::ExactSourceAndTarget,
                seed_targets: |c| {
                    seed_known_target_file(c, "/tmp/target.org", 2);
                    seed_target_heading(c, 20, 2, 1, "Target");
                    seed_heading_property(c, 20, "CUSTOM_ID", Some("abc"));
                },
                expected: (
                    TARGET_PATH,
                    Some(2),
                    None,
                    Some("ab"),
                    None,
                    "broken",
                    Some(CUSTOM_ID_MISSING_DIAGNOSTIC),
                ),
            },
            ResolveCase {
                name: "file::#id: padded lookup matches custom id",
                raw: "[[file:target.org::# abc ]]",
                link_type: "file",
                path: "target.org",
                search_option: Some("# abc "),
                raw_parts: None,
                universe: TestUniverse::ExactSourceAndTarget,
                seed_targets: |c| {
                    seed_known_target_file(c, "/tmp/target.org", 2);
                    seed_target_heading(c, 20, 2, 1, "Target");
                    seed_heading_property(c, 20, "CUSTOM_ID", Some("abc"));
                },
                expected: (
                    TARGET_PATH,
                    Some(2),
                    Some(20),
                    Some("abc"),
                    None,
                    "resolved",
                    None,
                ),
            },
            // Same-file fuzzy star links.
            ResolveCase {
                name: "fuzzy *title: resolves to source heading",
                raw: "[[*Heading]]",
                link_type: "fuzzy",
                path: "*Heading",
                search_option: None,
                raw_parts: None,
                universe: TestUniverse::Empty,
                seed_targets: |c| seed_target_heading(c, 20, 1, 1, "Heading"),
                expected: (None, Some(1), Some(20), None, None, "resolved", None),
            },
            ResolveCase {
                name: "fuzzy *title: whitespace and case are normalized",
                raw: "[[*   peer heading   ]]",
                link_type: "fuzzy",
                path: "*   peer heading   ",
                search_option: None,
                raw_parts: None,
                universe: TestUniverse::Empty,
                seed_targets: |c| seed_target_heading(c, 20, 1, 1, "Peer Heading"),
                expected: (None, Some(1), Some(20), None, None, "resolved", None),
            },
            ResolveCase {
                name: "fuzzy *title: unicode case-insensitive match",
                raw: "[[*ärger]]",
                link_type: "fuzzy",
                path: "*ärger",
                search_option: None,
                raw_parts: None,
                universe: TestUniverse::Empty,
                seed_targets: |c| seed_target_heading(c, 20, 1, 1, "Ärger"),
                expected: (None, Some(1), Some(20), None, None, "resolved", None),
            },
            ResolveCase {
                name: "fuzzy *title: missing heading is broken",
                raw: "[[*Missing]]",
                link_type: "fuzzy",
                path: "*Missing",
                search_option: None,
                raw_parts: None,
                universe: TestUniverse::Empty,
                seed_targets: seed_none,
                expected: (
                    None,
                    Some(1),
                    None,
                    None,
                    None,
                    "broken",
                    Some(SAME_FILE_STAR_HEADING_MISSING_DIAGNOSTIC),
                ),
            },
            ResolveCase {
                name: "fuzzy *title: duplicate selects first in document order",
                raw: "[[*Duplicate]]",
                link_type: "fuzzy",
                path: "*Duplicate",
                search_option: None,
                raw_parts: None,
                universe: TestUniverse::Empty,
                seed_targets: |c| {
                    seed_target_heading(c, 20, 1, 1, "Duplicate");
                    seed_target_heading(c, 21, 1, 1, "Duplicate");
                },
                expected: (None, Some(1), Some(20), None, None, "resolved", None),
            },
            // Same-file fuzzy custom-id links.
            ResolveCase {
                name: "fuzzy #id: resolves to source heading",
                raw: "[[#custom-id]]",
                link_type: "fuzzy",
                path: "#custom-id",
                search_option: None,
                raw_parts: None,
                universe: TestUniverse::Empty,
                seed_targets: |c| {
                    seed_target_heading(c, 20, 1, 1, "Heading");
                    seed_heading_property(c, 20, "CUSTOM_ID", Some("custom-id"));
                },
                expected: (
                    None,
                    Some(1),
                    Some(20),
                    Some("custom-id"),
                    None,
                    "resolved",
                    None,
                ),
            },
            ResolveCase {
                name: "fuzzy #id: whitespace is trimmed",
                raw: "[[# Custom-ID ]]",
                link_type: "fuzzy",
                path: "# Custom-ID ",
                search_option: None,
                raw_parts: None,
                universe: TestUniverse::Empty,
                seed_targets: |c| {
                    seed_target_heading(c, 20, 1, 1, "Heading");
                    seed_heading_property(c, 20, "CUSTOM_ID", Some("Custom-ID"));
                },
                expected: (
                    None,
                    Some(1),
                    Some(20),
                    Some("Custom-ID"),
                    None,
                    "resolved",
                    None,
                ),
            },
            ResolveCase {
                name: "fuzzy #id: duplicate selects first in document order",
                raw: "[[#dup]]",
                link_type: "fuzzy",
                path: "#dup",
                search_option: None,
                raw_parts: None,
                universe: TestUniverse::Empty,
                seed_targets: |c| {
                    seed_target_heading(c, 20, 1, 1, "First");
                    seed_target_heading(c, 21, 1, 1, "Second");
                    seed_heading_property(c, 20, "CUSTOM_ID", Some("dup"));
                    seed_heading_property(c, 21, "CUSTOM_ID", Some("DUP"));
                },
                expected: (None, Some(1), Some(20), Some("dup"), None, "resolved", None),
            },
            ResolveCase {
                name: "fuzzy #id: missing custom id is broken",
                raw: "[[#missing]]",
                link_type: "fuzzy",
                path: "#missing",
                search_option: None,
                raw_parts: None,
                universe: TestUniverse::Empty,
                seed_targets: seed_none,
                expected: (
                    None,
                    Some(1),
                    None,
                    Some("missing"),
                    None,
                    "broken",
                    Some(CUSTOM_ID_MISSING_DIAGNOSTIC),
                ),
            },
            ResolveCase {
                name: "fuzzy *title: escaped brackets are unescaped",
                raw: "[[*\\[2026-07-01 Wed\\] Review]]",
                link_type: "fuzzy",
                path: r"*\[2026-07-01 Wed\] Review",
                search_option: None,
                raw_parts: None,
                universe: TestUniverse::Empty,
                seed_targets: |c| seed_target_heading(c, 20, 1, 1, "[2026-07-01 Wed] Review"),
                expected: (None, Some(1), Some(20), None, None, "resolved", None),
            },
            ResolveCase {
                name: "fuzzy #id: leading padding after hash resolves",
                raw: "[[# abc]]",
                link_type: "fuzzy",
                path: "# abc",
                search_option: None,
                raw_parts: None,
                universe: TestUniverse::Empty,
                seed_targets: |c| {
                    seed_target_heading(c, 20, 1, 1, "Heading");
                    seed_heading_property(c, 20, "CUSTOM_ID", Some("abc"));
                },
                expected: (None, Some(1), Some(20), Some("abc"), None, "resolved", None),
            },
            ResolveCase {
                name: "fuzzy #id: trailing padding resolves",
                raw: "[[#abc ]]",
                link_type: "fuzzy",
                path: "#abc ",
                search_option: None,
                raw_parts: None,
                universe: TestUniverse::Empty,
                seed_targets: |c| {
                    seed_target_heading(c, 20, 1, 1, "Heading");
                    seed_heading_property(c, 20, "CUSTOM_ID", Some("abc"));
                },
                expected: (None, Some(1), Some(20), Some("abc"), None, "resolved", None),
            },
            ResolveCase {
                name: "fuzzy #id: lookup does not prefix-match a longer custom id",
                raw: "[[# ab]]",
                link_type: "fuzzy",
                path: "# ab",
                search_option: None,
                raw_parts: None,
                universe: TestUniverse::Empty,
                seed_targets: |c| {
                    seed_target_heading(c, 20, 1, 1, "Heading");
                    seed_heading_property(c, 20, "CUSTOM_ID", Some("abc"));
                },
                expected: (
                    None,
                    Some(1),
                    None,
                    Some("ab"),
                    None,
                    "broken",
                    Some(CUSTOM_ID_MISSING_DIAGNOSTIC),
                ),
            },
            ResolveCase {
                name: "fuzzy without star or hash stays unsupported",
                raw: "[[Heading]]",
                link_type: "fuzzy",
                path: "Heading",
                search_option: None,
                raw_parts: None,
                universe: TestUniverse::Empty,
                seed_targets: |c| seed_target_heading(c, 20, 1, 1, "Heading"),
                expected: (
                    None,
                    None,
                    None,
                    None,
                    None,
                    "unsupported",
                    Some(UNSUPPORTED_DIAGNOSTIC),
                ),
            },
        ];

        for case in cases {
            let schema = SchemaDefinition::new(CURRENT_SCHEMA_VERSION, false);
            let connection =
                open_in_memory_database_with_schema(&schema).expect("database should open");
            seed_file_link_fixture(
                &connection,
                "/tmp/source.org",
                case.raw,
                case.link_type,
                case.path,
                case.search_option,
            );
            if let Some((raw_target, raw_description)) = case.raw_parts {
                connection
                    .execute(
                        "UPDATE links SET raw_target = ?1, raw_description = ?2 WHERE id = 1",
                        params![raw_target, raw_description],
                    )
                    .expect("raw parts should update");
            }
            (case.seed_targets)(&connection);

            let raw_columns = |connection: &Connection| -> (String, String, Option<String>) {
                connection
                    .query_row(
                        "SELECT raw, raw_target, raw_description FROM links WHERE id = 1",
                        [],
                        |row| Ok((row.get(0)?, row.get(1)?, row.get(2)?)),
                    )
                    .expect("raw columns should load")
            };
            let raw_before = raw_columns(&connection);

            LinkResolver::resolve_all(&connection, &case.universe.build())
                .expect("resolution should succeed");

            let row: ResolvedColumns = connection
                .query_row(
                    "SELECT path_absolute, target_file_id, target_heading_id, target_custom_id,
                            target_id, resolution_status, resolution_diagnostic
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
                        ))
                    },
                )
                .expect("resolved row should load");
            let e = case.expected;
            let expected = (
                e.0.map(str::to_string),
                e.1,
                e.2,
                e.3.map(str::to_string),
                e.4.map(str::to_string),
                e.5.to_string(),
                e.6.map(str::to_string),
            );
            assert_eq!(row, expected, "{}", case.name);
            assert_eq!(
                raw_columns(&connection),
                raw_before,
                "{}: raw columns must be preserved",
                case.name
            );
        }
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
