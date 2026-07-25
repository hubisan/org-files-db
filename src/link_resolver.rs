use std::{
    collections::{BTreeSet, HashMap},
    path::{Component, Path, PathBuf},
};

use rusqlite::{params, Connection};

use crate::db::DbWriteError;
use crate::exclusions::ExclusionMatcher;
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

#[derive(Debug)]
pub(crate) struct IndexedUniverse {
    global_exclusions: ExclusionMatcher,
    globally_excluded_paths: BTreeSet<PathBuf>,
    explicit_inclusions: BTreeSet<PathBuf>,
    explicit_logical_paths: BTreeSet<PathBuf>,
    file_mappings: Vec<(PathBuf, PathBuf)>,
    root_scopes: Vec<IndexedRootScope>,
}

#[derive(Debug)]
struct IndexedRootScope {
    logical_root: PathBuf,
    recursive: bool,
    local_exclusions: ExclusionMatcher,
    directory_mappings: Vec<(PathBuf, PathBuf)>,
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

#[derive(Debug, Default)]
struct KnownFiles {
    by_path: HashMap<PathBuf, i64>,
}

#[derive(Debug)]
struct HeadingCandidate {
    id: i64,
    title: String,
}

#[derive(Debug)]
struct PropertyCandidate {
    heading_id: i64,
    value: Option<String>,
}

#[derive(Debug)]
struct GlobalPropertyCandidate {
    file_id: i64,
    heading_id: i64,
    value: Option<String>,
}

#[derive(Debug)]
struct RootHeadingCandidate {
    heading_id: i64,
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
    ) -> Result<(), DbWriteError> {
        if link.link_type == "file" {
            return Self::resolve_file_link(connection, link, indexed_universe, known_files);
        }
        if link.link_type == "custom-id" {
            return Self::resolve_same_file_custom_id_link(connection, link);
        }
        if link.link_type == "id" {
            return Self::resolve_org_id_link(connection, link);
        }
        if link.link_type == "fuzzy" {
            return Self::resolve_same_file_fuzzy_link(connection, link);
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
            return Self::resolve_file_target(connection, link, &path_absolute, target_file_id);
        }

        if indexed_universe.contains(&path_absolute) {
            return Self::mark_broken_file(connection, link.id, &path_absolute);
        }

        Self::mark_unresolved_file(connection, link.id, &path_absolute)
    }

    fn resolve_same_file_fuzzy_link(
        connection: &Connection,
        link: &StoredLink,
    ) -> Result<(), DbWriteError> {
        if let Some(custom_id_target) = same_file_fuzzy_custom_id_target(link.path.as_str()) {
            return Self::resolve_same_file_custom_id_target(connection, link, &custom_id_target);
        }

        let Some(heading_title) = same_file_fuzzy_star_heading_target(link.path.as_str()) else {
            return Self::mark_unsupported(connection, link.id);
        };

        let heading_ids = Self::load_matching_heading_ids(
            connection,
            link.source_file_id,
            heading_title.as_str(),
        )?;
        match heading_ids.as_slice() {
            [target_heading_id] => Self::mark_resolved_same_file_heading(
                connection,
                link.id,
                link.source_file_id,
                *target_heading_id,
            ),
            [] => Self::mark_broken_same_file_heading(connection, link.id, link.source_file_id),
            [target_heading_id, ..] => Self::mark_resolved_same_file_heading(
                connection,
                link.id,
                link.source_file_id,
                *target_heading_id,
            ),
        }
    }

    fn resolve_same_file_custom_id_link(
        connection: &Connection,
        link: &StoredLink,
    ) -> Result<(), DbWriteError> {
        let custom_id_target = normalize_custom_id_lookup_target(link.path.as_str());

        Self::resolve_same_file_custom_id_target(connection, link, &custom_id_target)
    }

    fn resolve_same_file_custom_id_target(
        connection: &Connection,
        link: &StoredLink,
        custom_id_target: &str,
    ) -> Result<(), DbWriteError> {
        let heading_ids = Self::load_matching_property_heading_ids(
            connection,
            link.source_file_id,
            "CUSTOM_ID",
            custom_id_target,
        )?;
        match heading_ids.as_slice() {
            [target_heading_id] => Self::mark_resolved_same_file_custom_id(
                connection,
                link.id,
                link.source_file_id,
                *target_heading_id,
                custom_id_target,
            ),
            [] => Self::mark_broken_same_file_custom_id(
                connection,
                link.id,
                link.source_file_id,
                custom_id_target,
            ),
            [target_heading_id, ..] => Self::mark_resolved_same_file_custom_id(
                connection,
                link.id,
                link.source_file_id,
                *target_heading_id,
                custom_id_target,
            ),
        }
    }

    fn resolve_org_id_link(connection: &Connection, link: &StoredLink) -> Result<(), DbWriteError> {
        let target_id = normalize_id_target(link.path.as_str());
        let matches = Self::load_matching_global_property_targets(connection, "ID", &target_id)?;

        match matches.as_slice() {
            [(target_file_id, target_heading_id)] => Self::mark_resolved_org_id(
                connection,
                link.id,
                *target_file_id,
                *target_heading_id,
                &target_id,
            ),
            [] => Self::mark_unresolved_org_id(connection, link.id, &target_id),
            [..] => Self::mark_ambiguous_org_id(connection, link.id, &target_id),
        }
    }

    fn resolve_file_target(
        connection: &Connection,
        link: &StoredLink,
        path_absolute: &Path,
        target_file_id: i64,
    ) -> Result<(), DbWriteError> {
        if let Some(custom_id_target) = file_custom_id_search_target(link.search_option.as_deref())
        {
            return Self::resolve_file_custom_id_target(
                connection,
                link,
                path_absolute,
                target_file_id,
                &custom_id_target,
            );
        }

        let Some(heading_title) = heading_title_search_target(link.search_option.as_deref()) else {
            if link.search_option.is_none() {
                return Self::resolve_file_root_target(
                    connection,
                    link.id,
                    path_absolute,
                    target_file_id,
                );
            }
            return Self::mark_resolved_file(connection, link.id, path_absolute, target_file_id);
        };

        let heading_ids =
            Self::load_matching_heading_ids(connection, target_file_id, heading_title.as_str())?;
        match heading_ids.as_slice() {
            [target_heading_id] => Self::mark_resolved_heading(
                connection,
                link.id,
                path_absolute,
                target_file_id,
                *target_heading_id,
            ),
            [] => {
                Self::mark_broken_heading_title(connection, link.id, path_absolute, target_file_id)
            }
            [target_heading_id, ..] => Self::mark_resolved_heading(
                connection,
                link.id,
                path_absolute,
                target_file_id,
                *target_heading_id,
            ),
        }
    }

    fn resolve_file_custom_id_target(
        connection: &Connection,
        link: &StoredLink,
        path_absolute: &Path,
        target_file_id: i64,
        custom_id_target: &str,
    ) -> Result<(), DbWriteError> {
        let heading_ids = Self::load_matching_property_heading_ids(
            connection,
            target_file_id,
            "CUSTOM_ID",
            custom_id_target,
        )?;
        match heading_ids.as_slice() {
            [target_heading_id] => Self::mark_resolved_file_custom_id(
                connection,
                link.id,
                path_absolute,
                target_file_id,
                *target_heading_id,
                custom_id_target,
            ),
            [] => Self::mark_broken_file_custom_id(
                connection,
                link.id,
                path_absolute,
                target_file_id,
                custom_id_target,
            ),
            [target_heading_id, ..] => Self::mark_resolved_file_custom_id(
                connection,
                link.id,
                path_absolute,
                target_file_id,
                *target_heading_id,
                custom_id_target,
            ),
        }
    }

    fn load_matching_heading_ids(
        connection: &Connection,
        file_id: i64,
        title: &str,
    ) -> Result<Vec<i64>, DbWriteError> {
        let normalized_title = unicode_lowercase(title);
        let mut statement = connection
            .prepare(
                "SELECT id, title
                 FROM headings
                 WHERE file_id = ?1
                   AND level > 0
                 ORDER BY byte_start, id",
            )
            .map_err(|source| DbWriteError::Write {
                operation: "link_resolver.load_matching_heading_ids.prepare",
                source,
            })?;
        let rows = statement
            .query_map(params![file_id], |row| {
                Ok(HeadingCandidate {
                    id: row.get(0)?,
                    title: row.get(1)?,
                })
            })
            .map_err(|source| DbWriteError::Write {
                operation: "link_resolver.load_matching_heading_ids.query",
                source,
            })?;
        let candidates =
            rows.collect::<Result<Vec<_>, _>>()
                .map_err(|source| DbWriteError::Write {
                    operation: "link_resolver.load_matching_heading_ids.collect",
                    source,
                })?;
        Ok(candidates
            .into_iter()
            .filter(|candidate| unicode_lowercase(candidate.title.as_str()) == normalized_title)
            .map(|candidate| candidate.id)
            .collect())
    }

    fn load_matching_property_heading_ids(
        connection: &Connection,
        file_id: i64,
        property_key: &str,
        property_target: &str,
    ) -> Result<Vec<i64>, DbWriteError> {
        let normalized_target = unicode_lowercase(property_target);
        let mut statement = connection
            .prepare(
                "SELECT headings.id, properties.value
                 FROM properties
                 INNER JOIN headings ON headings.id = properties.heading_id
                 WHERE headings.file_id = ?1
                   AND headings.level > 0
                   AND properties.key = ?2
                 ORDER BY headings.byte_start, headings.id, properties.id",
            )
            .map_err(|source| DbWriteError::Write {
                operation: "link_resolver.load_matching_property_heading_ids.prepare",
                source,
            })?;
        let rows = statement
            .query_map(params![file_id, property_key], |row| {
                Ok(PropertyCandidate {
                    heading_id: row.get(0)?,
                    value: row.get(1)?,
                })
            })
            .map_err(|source| DbWriteError::Write {
                operation: "link_resolver.load_matching_property_heading_ids.query",
                source,
            })?;
        let candidates =
            rows.collect::<Result<Vec<_>, _>>()
                .map_err(|source| DbWriteError::Write {
                    operation: "link_resolver.load_matching_property_heading_ids.collect",
                    source,
                })?;
        Ok(candidates
            .into_iter()
            .filter(|candidate| {
                candidate.value.as_deref().map(unicode_lowercase).as_deref()
                    == Some(normalized_target.as_str())
            })
            .map(|candidate| candidate.heading_id)
            .collect())
    }

    fn load_matching_global_property_targets(
        connection: &Connection,
        property_key: &str,
        property_target: &str,
    ) -> Result<Vec<(i64, i64)>, DbWriteError> {
        let normalized_target = unicode_lowercase(property_target);
        let mut statement = connection
            .prepare(
                "SELECT headings.file_id, headings.id, properties.value
                 FROM properties
                 INNER JOIN headings ON headings.id = properties.heading_id
                 INNER JOIN files ON files.id = headings.file_id
                 WHERE headings.level > 0
                   AND properties.key = ?1
                 ORDER BY files.path, headings.byte_start, headings.id, properties.id",
            )
            .map_err(|source| DbWriteError::Write {
                operation: "link_resolver.load_matching_global_property_targets.prepare",
                source,
            })?;
        let rows = statement
            .query_map(params![property_key], |row| {
                Ok(GlobalPropertyCandidate {
                    file_id: row.get(0)?,
                    heading_id: row.get(1)?,
                    value: row.get(2)?,
                })
            })
            .map_err(|source| DbWriteError::Write {
                operation: "link_resolver.load_matching_global_property_targets.query",
                source,
            })?;
        let candidates =
            rows.collect::<Result<Vec<_>, _>>()
                .map_err(|source| DbWriteError::Write {
                    operation: "link_resolver.load_matching_global_property_targets.collect",
                    source,
                })?;
        Ok(candidates
            .into_iter()
            .filter(|candidate| {
                candidate.value.as_deref().map(unicode_lowercase).as_deref()
                    == Some(normalized_target.as_str())
            })
            .map(|candidate| (candidate.file_id, candidate.heading_id))
            .collect())
    }

    fn load_root_heading_ids(
        connection: &Connection,
        file_id: i64,
    ) -> Result<Vec<i64>, DbWriteError> {
        let mut statement = connection
            .prepare(
                "SELECT id
                 FROM headings
                 WHERE file_id = ?1
                   AND level = 0
                   AND parent_id IS NULL
                 ORDER BY byte_start, id",
            )
            .map_err(|source| DbWriteError::Write {
                operation: "link_resolver.load_root_heading_ids.prepare",
                source,
            })?;
        let rows = statement
            .query_map(params![file_id], |row| {
                Ok(RootHeadingCandidate {
                    heading_id: row.get(0)?,
                })
            })
            .map_err(|source| DbWriteError::Write {
                operation: "link_resolver.load_root_heading_ids.query",
                source,
            })?;
        let candidates =
            rows.collect::<Result<Vec<_>, _>>()
                .map_err(|source| DbWriteError::Write {
                    operation: "link_resolver.load_root_heading_ids.collect",
                    source,
                })?;
        Ok(candidates
            .into_iter()
            .map(|candidate| candidate.heading_id)
            .collect())
    }

    fn resolve_file_root_target(
        connection: &Connection,
        link_id: i64,
        path_absolute: &Path,
        target_file_id: i64,
    ) -> Result<(), DbWriteError> {
        let root_heading_ids = Self::load_root_heading_ids(connection, target_file_id)?;
        match root_heading_ids.as_slice() {
            [target_heading_id] => Self::mark_resolved_heading(
                connection,
                link_id,
                path_absolute,
                target_file_id,
                *target_heading_id,
            ),
            [] => Self::mark_resolved_file_with_diagnostic(
                connection,
                link_id,
                path_absolute,
                target_file_id,
                MISSING_SYNTHETIC_ROOT_DIAGNOSTIC,
            ),
            [..] => {
                Self::mark_ambiguous_file_root(connection, link_id, path_absolute, target_file_id)
            }
        }
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
                    display_path(path_absolute),
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

    fn mark_resolved_file_with_diagnostic(
        connection: &Connection,
        link_id: i64,
        path_absolute: &Path,
        target_file_id: i64,
        resolution_diagnostic: &str,
    ) -> Result<(), DbWriteError> {
        connection
            .execute(
                "UPDATE links
                 SET path_absolute = ?2,
                     target_file_id = ?3,
                     target_heading_id = NULL,
                     resolution_status = ?4,
                     resolution_diagnostic = ?5
                 WHERE id = ?1",
                params![
                    link_id,
                    display_path(path_absolute),
                    target_file_id,
                    "resolved",
                    resolution_diagnostic,
                ],
            )
            .map_err(|source| DbWriteError::Write {
                operation: "link_resolver.mark_resolved_file_with_diagnostic",
                source,
            })?;
        Ok(())
    }

    fn mark_resolved_heading(
        connection: &Connection,
        link_id: i64,
        path_absolute: &Path,
        target_file_id: i64,
        target_heading_id: i64,
    ) -> Result<(), DbWriteError> {
        connection
            .execute(
                "UPDATE links
                 SET path_absolute = ?2,
                     target_file_id = ?3,
                     target_heading_id = ?4,
                     resolution_status = ?5,
                     resolution_diagnostic = NULL
                 WHERE id = ?1",
                params![
                    link_id,
                    display_path(path_absolute),
                    target_file_id,
                    target_heading_id,
                    "resolved"
                ],
            )
            .map_err(|source| DbWriteError::Write {
                operation: "link_resolver.mark_resolved_heading",
                source,
            })?;
        Ok(())
    }

    fn mark_resolved_file_custom_id(
        connection: &Connection,
        link_id: i64,
        path_absolute: &Path,
        target_file_id: i64,
        target_heading_id: i64,
        target_custom_id: &str,
    ) -> Result<(), DbWriteError> {
        connection
            .execute(
                "UPDATE links
                 SET path_absolute = ?2,
                     target_file_id = ?3,
                     target_heading_id = ?4,
                     target_custom_id = ?5,
                     target_id = NULL,
                     resolution_status = ?6,
                     resolution_diagnostic = NULL
                 WHERE id = ?1",
                params![
                    link_id,
                    display_path(path_absolute),
                    target_file_id,
                    target_heading_id,
                    target_custom_id,
                    "resolved"
                ],
            )
            .map_err(|source| DbWriteError::Write {
                operation: "link_resolver.mark_resolved_file_custom_id",
                source,
            })?;
        Ok(())
    }

    fn mark_resolved_same_file_heading(
        connection: &Connection,
        link_id: i64,
        target_file_id: i64,
        target_heading_id: i64,
    ) -> Result<(), DbWriteError> {
        connection
            .execute(
                "UPDATE links
                 SET target_file_id = ?2,
                     target_heading_id = ?3,
                     resolution_status = ?4,
                     resolution_diagnostic = NULL
                 WHERE id = ?1",
                params![link_id, target_file_id, target_heading_id, "resolved"],
            )
            .map_err(|source| DbWriteError::Write {
                operation: "link_resolver.mark_resolved_same_file_heading",
                source,
            })?;
        Ok(())
    }

    fn mark_resolved_same_file_custom_id(
        connection: &Connection,
        link_id: i64,
        target_file_id: i64,
        target_heading_id: i64,
        target_custom_id: &str,
    ) -> Result<(), DbWriteError> {
        connection
            .execute(
                "UPDATE links
                 SET target_file_id = ?2,
                     target_heading_id = ?3,
                     target_custom_id = ?4,
                     target_id = NULL,
                     resolution_status = ?5,
                     resolution_diagnostic = NULL
                 WHERE id = ?1",
                params![
                    link_id,
                    target_file_id,
                    target_heading_id,
                    target_custom_id,
                    "resolved"
                ],
            )
            .map_err(|source| DbWriteError::Write {
                operation: "link_resolver.mark_resolved_same_file_custom_id",
                source,
            })?;
        Ok(())
    }

    fn mark_resolved_org_id(
        connection: &Connection,
        link_id: i64,
        target_file_id: i64,
        target_heading_id: i64,
        target_id: &str,
    ) -> Result<(), DbWriteError> {
        connection
            .execute(
                "UPDATE links
                 SET target_file_id = ?2,
                     target_heading_id = ?3,
                     target_custom_id = NULL,
                     target_id = ?4,
                     resolution_status = ?5,
                     resolution_diagnostic = NULL
                 WHERE id = ?1",
                params![
                    link_id,
                    target_file_id,
                    target_heading_id,
                    target_id,
                    "resolved"
                ],
            )
            .map_err(|source| DbWriteError::Write {
                operation: "link_resolver.mark_resolved_org_id",
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
                    display_path(path_absolute),
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
                    display_path(path_absolute),
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

    fn mark_ambiguous_file_root(
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
                     target_heading_id = NULL,
                     resolution_status = ?4,
                     resolution_diagnostic = ?5
                 WHERE id = ?1",
                params![
                    link_id,
                    display_path(path_absolute),
                    target_file_id,
                    "ambiguous",
                    DUPLICATE_SYNTHETIC_ROOT_DIAGNOSTIC,
                ],
            )
            .map_err(|source| DbWriteError::Write {
                operation: "link_resolver.mark_ambiguous_file_root",
                source,
            })?;
        Ok(())
    }

    fn mark_broken_heading_title(
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
                     target_heading_id = NULL,
                     resolution_status = ?4,
                     resolution_diagnostic = ?5
                 WHERE id = ?1",
                params![
                    link_id,
                    display_path(path_absolute),
                    target_file_id,
                    "broken",
                    HEADING_TITLE_MISSING_DIAGNOSTIC
                ],
            )
            .map_err(|source| DbWriteError::Write {
                operation: "link_resolver.mark_broken_heading_title",
                source,
            })?;
        Ok(())
    }

    fn mark_broken_file_custom_id(
        connection: &Connection,
        link_id: i64,
        path_absolute: &Path,
        target_file_id: i64,
        target_custom_id: &str,
    ) -> Result<(), DbWriteError> {
        connection
            .execute(
                "UPDATE links
                 SET path_absolute = ?2,
                     target_file_id = ?3,
                     target_heading_id = NULL,
                     target_custom_id = ?4,
                     target_id = NULL,
                     resolution_status = ?5,
                     resolution_diagnostic = ?6
                 WHERE id = ?1",
                params![
                    link_id,
                    display_path(path_absolute),
                    target_file_id,
                    target_custom_id,
                    "broken",
                    CUSTOM_ID_MISSING_DIAGNOSTIC
                ],
            )
            .map_err(|source| DbWriteError::Write {
                operation: "link_resolver.mark_broken_file_custom_id",
                source,
            })?;
        Ok(())
    }

    fn mark_broken_same_file_heading(
        connection: &Connection,
        link_id: i64,
        target_file_id: i64,
    ) -> Result<(), DbWriteError> {
        connection
            .execute(
                "UPDATE links
                 SET target_file_id = ?2,
                     target_heading_id = NULL,
                     resolution_status = ?3,
                     resolution_diagnostic = ?4
                 WHERE id = ?1",
                params![
                    link_id,
                    target_file_id,
                    "broken",
                    SAME_FILE_STAR_HEADING_MISSING_DIAGNOSTIC
                ],
            )
            .map_err(|source| DbWriteError::Write {
                operation: "link_resolver.mark_broken_same_file_heading",
                source,
            })?;
        Ok(())
    }

    fn mark_broken_same_file_custom_id(
        connection: &Connection,
        link_id: i64,
        target_file_id: i64,
        target_custom_id: &str,
    ) -> Result<(), DbWriteError> {
        connection
            .execute(
                "UPDATE links
                 SET target_file_id = ?2,
                     target_heading_id = NULL,
                     target_custom_id = ?3,
                     target_id = NULL,
                     resolution_status = ?4,
                     resolution_diagnostic = ?5
                 WHERE id = ?1",
                params![
                    link_id,
                    target_file_id,
                    target_custom_id,
                    "broken",
                    CUSTOM_ID_MISSING_DIAGNOSTIC
                ],
            )
            .map_err(|source| DbWriteError::Write {
                operation: "link_resolver.mark_broken_same_file_custom_id",
                source,
            })?;
        Ok(())
    }

    fn mark_unresolved_org_id(
        connection: &Connection,
        link_id: i64,
        target_id: &str,
    ) -> Result<(), DbWriteError> {
        connection
            .execute(
                "UPDATE links
                 SET target_file_id = NULL,
                     target_heading_id = NULL,
                     target_custom_id = NULL,
                     target_id = ?2,
                     resolution_status = ?3,
                     resolution_diagnostic = ?4
                 WHERE id = ?1",
                params![link_id, target_id, "unresolved", ID_MISSING_DIAGNOSTIC],
            )
            .map_err(|source| DbWriteError::Write {
                operation: "link_resolver.mark_unresolved_org_id",
                source,
            })?;
        Ok(())
    }

    fn mark_ambiguous_org_id(
        connection: &Connection,
        link_id: i64,
        target_id: &str,
    ) -> Result<(), DbWriteError> {
        connection
            .execute(
                "UPDATE links
                 SET target_file_id = NULL,
                     target_heading_id = NULL,
                     target_custom_id = NULL,
                     target_id = ?2,
                     resolution_status = ?3,
                     resolution_diagnostic = ?4
                 WHERE id = ?1",
                params![link_id, target_id, "ambiguous", DUPLICATE_ID_DIAGNOSTIC],
            )
            .map_err(|source| DbWriteError::Write {
                operation: "link_resolver.mark_ambiguous_org_id",
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

impl Default for IndexedUniverse {
    fn default() -> Self {
        Self {
            global_exclusions: ExclusionMatcher::empty(),
            globally_excluded_paths: BTreeSet::new(),
            explicit_inclusions: BTreeSet::new(),
            explicit_logical_paths: BTreeSet::new(),
            file_mappings: Vec::new(),
            root_scopes: Vec::new(),
        }
    }
}

impl IndexedUniverse {
    pub(crate) fn set_global_exclusions(&mut self, exclusions: ExclusionMatcher) {
        self.global_exclusions = exclusions;
    }

    pub(crate) fn add_root_scope(
        &mut self,
        logical_root: PathBuf,
        canonical_root: PathBuf,
        recursive: bool,
        local_exclusions: ExclusionMatcher,
    ) -> usize {
        let scope_id = self.root_scopes.len();
        self.root_scopes.push(IndexedRootScope {
            directory_mappings: vec![(logical_root.clone(), canonical_root.clone())],
            logical_root,
            recursive,
            local_exclusions,
        });
        scope_id
    }

    pub(crate) fn add_directory_mapping(
        &mut self,
        scope_id: usize,
        logical_directory: PathBuf,
        canonical_directory: PathBuf,
    ) {
        let scope = &mut self.root_scopes[scope_id];
        if !scope
            .directory_mappings
            .iter()
            .any(|mapping| mapping == &(logical_directory.clone(), canonical_directory.clone()))
        {
            scope
                .directory_mappings
                .push((logical_directory, canonical_directory));
        }
    }

    pub(crate) fn add_explicit_logical_path(&mut self, path: PathBuf) {
        self.explicit_logical_paths.insert(path);
    }

    pub(crate) fn add_explicit_mapping(&mut self, logical_path: PathBuf, canonical_path: PathBuf) {
        self.add_explicit_logical_path(logical_path.clone());
        self.explicit_inclusions.insert(canonical_path.clone());
        self.add_source_mapping(logical_path, canonical_path);
    }

    pub(crate) fn add_source_mapping(&mut self, logical_path: PathBuf, canonical_path: PathBuf) {
        if !self
            .file_mappings
            .iter()
            .any(|mapping| mapping == &(logical_path.clone(), canonical_path.clone()))
        {
            self.file_mappings.push((logical_path, canonical_path));
        }
    }

    pub(crate) fn add_globally_excluded_path(&mut self, path: PathBuf) {
        self.file_mappings
            .retain(|(_, mapped_path)| mapped_path != &path);
        self.globally_excluded_paths.insert(path);
    }

    pub(crate) fn root_scope_excludes_file(&self, scope_id: usize, path: &Path) -> bool {
        self.root_scopes[scope_id]
            .local_exclusions
            .matches_file(path)
    }

    pub(crate) fn root_scope_excludes_directory(&self, scope_id: usize, path: &Path) -> bool {
        self.root_scopes[scope_id]
            .local_exclusions
            .matches_directory(path)
    }

    // Compatibility helpers retained for focused resolver fixtures.
    #[cfg(test)]
    pub(crate) fn add_recursive_root(&mut self, path: PathBuf) {
        self.add_root_scope(path.clone(), path, true, ExclusionMatcher::empty());
    }

    #[cfg(test)]
    pub(crate) fn add_exact_path(&mut self, path: PathBuf) {
        self.add_explicit_mapping(path.clone(), path);
    }

    pub(crate) fn contains(&self, path: &Path) -> bool {
        if self.globally_excluded_paths.contains(path)
            || self.root_scopes.iter().any(|scope| {
                scope
                    .logical_candidates(path)
                    .any(|logical_path| self.global_exclusions.matches_file(&logical_path))
            })
        {
            return false;
        }

        self.explicit_inclusions.contains(path)
            || self.root_scopes.iter().any(|scope| scope.includes(path))
    }

    #[allow(dead_code)] // Used by the Phase 7 watcher path normalizer.
    pub(crate) fn is_explicit_candidate(&self, logical_path: &Path, canonical_path: &Path) -> bool {
        self.explicit_logical_paths.contains(logical_path)
            || self.explicit_inclusions.contains(canonical_path)
    }

    #[allow(dead_code)] // Used by the Phase 7 watcher path normalizer.
    pub(crate) fn is_known_source(&self, canonical_path: &Path) -> bool {
        self.file_mappings
            .iter()
            .any(|(_, mapped_path)| mapped_path.as_path() == canonical_path)
    }

    #[allow(dead_code)] // Used by the Phase 7 watcher path normalizer.
    pub(crate) fn normalize_candidate_path(
        &self,
        path: &Path,
        existing_canonical_path: Option<&Path>,
    ) -> Option<PathBuf> {
        if !path.is_absolute() || self.globally_excluded_paths.contains(path) {
            return None;
        }

        if let Some((_, canonical_path)) =
            self.file_mappings
                .iter()
                .find(|(logical_path, canonical_path)| {
                    path == logical_path.as_path() || path == canonical_path.as_path()
                })
        {
            if self.globally_excluded_paths.contains(canonical_path) {
                return None;
            }
            return Some(canonical_path.clone());
        }

        if let Some(canonical_path) = existing_canonical_path {
            if self.globally_excluded_paths.contains(canonical_path) {
                return None;
            }
            if self.contains(canonical_path) || self.includes_logical(path) {
                return Some(canonical_path.to_path_buf());
            }
        }

        if self.contains(path) {
            return Some(path.to_path_buf());
        }

        let mut candidates = self
            .root_scopes
            .iter()
            .flat_map(|scope| scope.canonical_candidates(path))
            .filter(|(_, candidate)| self.contains(candidate))
            .collect::<Vec<_>>();
        candidates.sort_by(|(left_depth, left), (right_depth, right)| {
            right_depth
                .cmp(left_depth)
                .then_with(|| left.as_os_str().cmp(right.as_os_str()))
        });
        candidates.into_iter().next().map(|(_, path)| path)
    }

    #[allow(dead_code)] // Used by the Phase 7 watcher path normalizer.
    fn includes_logical(&self, path: &Path) -> bool {
        if self.global_exclusions.matches_file(path) {
            return false;
        }

        self.explicit_logical_paths.contains(path)
            || self
                .root_scopes
                .iter()
                .any(|scope| scope.includes_logical(path))
    }
}

impl IndexedRootScope {
    fn canonical_candidates(&self, path: &Path) -> Vec<(usize, PathBuf)> {
        self.directory_mappings
            .iter()
            .filter_map(|(logical, canonical)| {
                path.strip_prefix(logical)
                    .ok()
                    .map(|suffix| (logical.components().count(), canonical.join(suffix)))
            })
            .collect()
    }

    fn logical_candidates<'a>(&'a self, path: &'a Path) -> impl Iterator<Item = PathBuf> + 'a {
        self.directory_mappings
            .iter()
            .filter_map(move |(logical, canonical)| {
                path.strip_prefix(canonical)
                    .ok()
                    .map(|suffix| logical.join(suffix))
            })
    }

    fn includes(&self, path: &Path) -> bool {
        self.logical_candidates(path)
            .any(|logical_path| self.includes_logical(&logical_path))
    }

    fn includes_logical(&self, path: &Path) -> bool {
        let Ok(relative) = path.strip_prefix(&self.logical_root) else {
            return false;
        };
        let direct_child = relative.components().count() <= 1;
        (self.recursive || direct_child)
            && !self
                .local_exclusions
                .matches_path_or_excluded_ancestor(path, &self.logical_root)
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
    Some(custom_id.to_string())
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
    normalize_custom_id_target(raw_target).unwrap_or_else(|| raw_target.to_string())
}

fn normalize_id_target(raw_target: &str) -> String {
    raw_target.to_string()
}

fn unicode_lowercase(value: &str) -> String {
    value.to_lowercase()
}

#[cfg(test)]
mod tests {
    use super::{
        file_custom_id_search_target, heading_title_search_target,
        normalize_custom_id_lookup_target, normalize_custom_id_target, normalize_file_target_path,
        normalize_id_target, same_file_fuzzy_custom_id_target, same_file_fuzzy_star_heading_target,
        unicode_lowercase, IndexedUniverse, LinkResolver, CUSTOM_ID_MISSING_DIAGNOSTIC,
        DUPLICATE_ID_DIAGNOSTIC, FILE_MISSING_DIAGNOSTIC, FILE_OUTSIDE_UNIVERSE_DIAGNOSTIC,
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
    fn resolve_all_preserves_whitespace_in_org_id_links_before_matching() {
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
        seed_heading_property(&connection, 20, "ID", Some(" foo "));

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
                Some(" FOO ".to_string()),
                Some("resolved".to_string()),
                None,
                "[[id: FOO ][Description]]".to_string(),
                "id: FOO ".to_string(),
                Some("Description".to_string()),
            )
        );
    }

    #[test]
    fn resolve_all_marks_whitespace_mismatched_org_id_links_unresolved() {
        let schema = SchemaDefinition::new(CURRENT_SCHEMA_VERSION, false);
        let connection =
            open_in_memory_database_with_schema(&schema).expect("database should open");
        seed_file_link_fixture(
            &connection,
            "/tmp/source.org",
            "[[id: ab]]",
            "id",
            " ab",
            None,
        );
        seed_known_target_file(&connection, "/tmp/target.org", 2);
        seed_target_heading(&connection, 20, 2, 1, "Heading");
        seed_heading_property(&connection, 20, "ID", Some("ab"));

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
            .expect("unresolved row should load");
        assert_eq!(
            row,
            (
                None,
                None,
                Some(" ab".to_string()),
                Some("unresolved".to_string()),
                Some(ID_MISSING_DIAGNOSTIC.to_string()),
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
    fn resolve_all_preserves_whitespace_in_file_custom_id_search_options_before_matching() {
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
        seed_heading_property(&connection, 20, "CUSTOM_ID", Some(" abc "));

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
                Some(" abc ".to_string()),
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
    fn resolve_all_preserves_whitespace_in_same_file_fuzzy_custom_id_links_before_matching() {
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
        seed_heading_property(&connection, 20, "CUSTOM_ID", Some(" Custom-ID "));

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
                Some(" Custom-ID ".to_string()),
                Some("resolved".to_string()),
                None,
            )
        );
    }

    #[test]
    fn resolve_all_marks_whitespace_mismatched_same_file_custom_id_links_broken() {
        let schema = SchemaDefinition::new(CURRENT_SCHEMA_VERSION, false);
        let connection =
            open_in_memory_database_with_schema(&schema).expect("database should open");
        seed_file_link_fixture(
            &connection,
            "/tmp/source.org",
            "[[# ab]]",
            "fuzzy",
            "# ab",
            None,
        );
        seed_target_heading(&connection, 20, 1, 1, "Heading");
        seed_heading_property(&connection, 20, "CUSTOM_ID", Some("ab"));

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
                Some(" ab".to_string()),
                Some("broken".to_string()),
                Some(CUSTOM_ID_MISSING_DIAGNOSTIC.to_string()),
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
    fn unicode_lowercase_handles_german_umlauts() {
        assert_eq!(unicode_lowercase("Ärger"), "ärger".to_string());
    }

    #[test]
    fn normalize_custom_id_target_strips_one_leading_hash_and_preserves_whitespace() {
        assert_eq!(
            normalize_custom_id_target("# Custom-ID "),
            Some(" Custom-ID ".to_string())
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
            " Custom-ID ".to_string()
        );
        assert_eq!(
            normalize_custom_id_lookup_target(" custom-id "),
            " custom-id ".to_string()
        );
    }

    #[test]
    fn normalize_id_target_preserves_whitespace() {
        assert_eq!(normalize_id_target(" FOO "), " FOO ".to_string());
        assert_eq!(normalize_id_target("foo"), "foo".to_string());
    }

    #[test]
    fn file_custom_id_search_target_requires_exactly_one_leading_hash() {
        assert_eq!(
            file_custom_id_search_target(Some("# Custom-ID ")),
            Some(" Custom-ID ".to_string())
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
            Some(" Custom-ID ".to_string())
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
