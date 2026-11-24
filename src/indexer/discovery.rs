use super::*;

pub(in crate::indexer) struct DiscoveryResult {
    pub(in crate::indexer) files: Vec<DiscoveredOrgFile>,
    pub(in crate::indexer) filesystem_entries: BTreeMap<PathBuf, DiscoveredFilesystemEntry>,
    pub(in crate::indexer) filesystem_symlinks: BTreeMap<PathBuf, PathBuf>,
    pub(in crate::indexer) suppressed_filesystem_symlinks: BTreeSet<PathBuf>,
    pub(in crate::indexer) globally_excluded_filesystem_symlinks: BTreeMap<PathBuf, PathBuf>,
    pub(in crate::indexer) dangling_filesystem_symlinks: BTreeSet<PathBuf>,
    pub(in crate::indexer) indexed_universe: IndexedUniverse,
    pub(in crate::indexer) missing_explicit_files: Vec<PathBuf>,
    pub(in crate::indexer) had_exclusion_match: bool,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(in crate::indexer) struct DiscoveredOrgFile {
    pub(in crate::indexer) path: PathBuf,
    pub(in crate::indexer) identity: FileIdentity,
    pub(in crate::indexer) scan_root: PathBuf,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub(in crate::indexer) enum ScanRootKind {
    ExplicitFile,
    ConfiguredDir,
}

pub(in crate::indexer) fn discover_org_files(
    config: &Config,
) -> Result<DiscoveryResult, IndexerError> {
    let mut paths = BTreeMap::new();
    let mut filesystem_entries = BTreeMap::new();
    let mut filesystem_symlinks = BTreeMap::new();
    let mut globally_excluded_filesystem_symlinks = BTreeMap::new();
    let mut dangling_filesystem_symlinks = BTreeSet::new();
    let filesystem_internal_paths = FilesystemInternalPaths::new(config);
    let mut indexed_universe = IndexedUniverse::default();
    let mut missing_explicit_files = Vec::new();
    let mut globally_excluded = BTreeSet::new();
    let mut globally_excluded_filesystem_files = BTreeSet::new();
    let global_exclusions = ExclusionMatcher::global(&config.discovery);
    indexed_universe.set_global_exclusions(ExclusionMatcher::global(&config.discovery));
    let mut had_exclusion_match = false;

    for file in &config.files {
        if global_exclusions.matches_file(file) {
            had_exclusion_match = true;
            continue;
        }
        indexed_universe.add_explicit_logical_path(file.clone());
        let canonical_file = match canonicalize_existing_file(file) {
            Ok(path) => path,
            Err(IndexerError::Discover { source, .. })
                if source.kind() == io::ErrorKind::NotFound =>
            {
                missing_explicit_files.push(file.clone());
                continue;
            }
            Err(error) => return Err(error),
        };
        let scan_root = canonical_file
            .parent()
            .unwrap_or(canonical_file.as_path())
            .to_path_buf();
        indexed_universe.add_explicit_mapping(file.clone(), canonical_file.clone());
        insert_discovered_path(
            &mut paths,
            canonical_file,
            scan_root,
            ScanRootKind::ExplicitFile,
        );
    }

    for dir in &config.dirs {
        let canonical_dir = canonicalize_existing_dir(&dir.path)?;
        filesystem_entries.insert(
            canonical_dir.clone(),
            DiscoveredFilesystemEntry {
                kind: FilesystemKind::Directory,
                visible: true,
            },
        );
        let collect_filesystem = !filesystem_internal_paths.contains(&dir.path)
            && !filesystem_internal_paths.contains(&canonical_dir);
        if collect_filesystem && dir.path != canonical_dir {
            filesystem_symlinks.insert(dir.path.clone(), canonical_dir.clone());
        }
        let local_exclusions = ExclusionMatcher::local(dir, &config.discovery);
        let scope_id = indexed_universe.add_root_scope(
            dir.path.clone(),
            canonical_dir.clone(),
            dir.recursive,
            local_exclusions,
        );
        let mut collector = DirectoryCollector {
            scan_root: &canonical_dir,
            recursive: dir.recursive,
            output: &mut paths,
            filesystem_entries: &mut filesystem_entries,
            filesystem_symlinks: &mut filesystem_symlinks,
            filesystem_internal_paths: &filesystem_internal_paths,
            dangling_filesystem_symlinks: &mut dangling_filesystem_symlinks,
            indexed_universe: &mut indexed_universe,
            visited_org_directories: BTreeSet::new(),
            active_filesystem_directories: BTreeSet::new(),
            global_exclusions: &global_exclusions,
            globally_excluded: &mut globally_excluded,
            globally_excluded_filesystem_files: &mut globally_excluded_filesystem_files,
            globally_excluded_filesystem_symlinks: &mut globally_excluded_filesystem_symlinks,
            had_exclusion_match: &mut had_exclusion_match,
            scope_id,
        };
        collector.collect(
            &dir.path,
            &canonical_dir,
            true,
            collect_filesystem.then_some(true),
        )?;
    }

    for excluded in &globally_excluded {
        paths.remove(excluded);
    }
    for excluded in &globally_excluded_filesystem_files {
        filesystem_entries.remove(excluded);
    }
    let mut suppressed_filesystem_symlinks = BTreeSet::new();
    filesystem_symlinks.retain(|alias, canonical| {
        if filesystem_entries.contains_key(canonical) {
            return true;
        }
        // A retarget can make an alias to an excluded target eligible again.
        suppressed_filesystem_symlinks.insert(alias.clone());
        false
    });

    Ok(DiscoveryResult {
        files: paths
            .into_iter()
            .map(|(path, (scan_root, _))| DiscoveredOrgFile {
                identity: FileIdentity::from_canonical_path(&path),
                path,
                scan_root,
            })
            .collect(),
        filesystem_entries,
        filesystem_symlinks,
        suppressed_filesystem_symlinks,
        globally_excluded_filesystem_symlinks,
        dangling_filesystem_symlinks,
        indexed_universe,
        missing_explicit_files,
        had_exclusion_match,
    })
}

pub(in crate::indexer) fn existing_indexed_file_count(
    connection: &Connection,
) -> Result<usize, IndexerError> {
    let count = connection
        .query_row("SELECT COUNT(*) FROM files", [], |row| row.get::<_, i64>(0))
        .map_err(|source| {
            IndexerError::Database(DbError::Inspect {
                target: "existing indexed files".to_string(),
                source,
            })
        })?;

    Ok(count as usize)
}

struct DirectoryCollector<'a> {
    scan_root: &'a Path,
    recursive: bool,
    output: &'a mut BTreeMap<PathBuf, (PathBuf, ScanRootKind)>,
    filesystem_entries: &'a mut BTreeMap<PathBuf, DiscoveredFilesystemEntry>,
    filesystem_symlinks: &'a mut BTreeMap<PathBuf, PathBuf>,
    filesystem_internal_paths: &'a FilesystemInternalPaths,
    dangling_filesystem_symlinks: &'a mut BTreeSet<PathBuf>,
    indexed_universe: &'a mut IndexedUniverse,
    visited_org_directories: BTreeSet<PathBuf>,
    active_filesystem_directories: BTreeSet<PathBuf>,
    global_exclusions: &'a ExclusionMatcher,
    globally_excluded: &'a mut BTreeSet<PathBuf>,
    globally_excluded_filesystem_files: &'a mut BTreeSet<PathBuf>,
    globally_excluded_filesystem_symlinks: &'a mut BTreeMap<PathBuf, PathBuf>,
    had_exclusion_match: &'a mut bool,
    scope_id: usize,
}

impl DirectoryCollector<'_> {
    fn collect(
        &mut self,
        logical_dir: &Path,
        canonical_dir: &Path,
        collect_org: bool,
        filesystem_visibility: Option<bool>,
    ) -> Result<(), IndexerError> {
        // Org keeps the first logical route to each canonical directory. Filesystem
        // exclusions can differ by route, so only cut cycles in the active ancestry.
        let collect_org = collect_org && self.visited_org_directories.insert(canonical_dir.into());
        // None disables filesystem traversal. A present value tracks route visibility.
        let filesystem_visibility = filesystem_visibility.filter(|_| {
            self.active_filesystem_directories
                .insert(canonical_dir.into())
        });
        let collect_filesystem = filesystem_visibility.is_some();
        if !collect_org && !collect_filesystem {
            return Ok(());
        }
        if collect_org {
            self.indexed_universe.add_directory_mapping(
                self.scope_id,
                logical_dir.to_path_buf(),
                canonical_dir.to_path_buf(),
            );
        }
        if collect_filesystem && (logical_dir != canonical_dir || !collect_org) {
            self.indexed_universe.add_filesystem_directory_mapping(
                self.scope_id,
                logical_dir,
                canonical_dir,
            );
        }
        let result = self.collect_children(
            logical_dir,
            canonical_dir,
            collect_org,
            filesystem_visibility,
        );
        if collect_filesystem {
            self.active_filesystem_directories.remove(canonical_dir);
        }
        result
    }

    fn collect_children(
        &mut self,
        logical_dir: &Path,
        canonical_dir: &Path,
        collect_org: bool,
        filesystem_visibility: Option<bool>,
    ) -> Result<(), IndexerError> {
        let mut entries = fs::read_dir(logical_dir)
            .map_err(|source| IndexerError::Discover {
                path: logical_dir.to_path_buf(),
                source,
            })?
            .collect::<Result<Vec<_>, _>>()
            .map_err(|source| IndexerError::Discover {
                path: logical_dir.to_path_buf(),
                source,
            })?;
        entries.sort_by_key(|entry| entry.path());

        for entry in entries {
            let path = entry.path();
            let name = path.file_name().expect("directory entry has a name");
            let canonical_child = canonical_dir.join(name);
            let filesystem_visibility = filesystem_visibility
                .filter(|_| {
                    !self.filesystem_internal_paths.contains(&path)
                        && !self.filesystem_internal_paths.contains(&canonical_child)
                })
                .map(|parent_visible| parent_visible && !name.as_encoded_bytes().starts_with(b"."));
            let collect_filesystem = filesystem_visibility.is_some();
            if !collect_org && !collect_filesystem {
                continue;
            }
            let file_type = entry.file_type().map_err(|source| IndexerError::Discover {
                path: path.clone(),
                source,
            })?;

            let followed_metadata = if file_type.is_symlink() {
                match fs::metadata(&path) {
                    Ok(metadata) => Some(metadata),
                    // Dangling symlinks (e.g. Emacs lock files) are treated as absent.
                    Err(source)
                        if matches!(
                            source.kind(),
                            io::ErrorKind::NotFound | io::ErrorKind::NotADirectory
                        ) =>
                    {
                        if collect_filesystem {
                            self.record_dangling_symlink(path);
                        }
                        continue;
                    }
                    Err(source) => {
                        return Err(IndexerError::Discover {
                            path: path.clone(),
                            source,
                        });
                    }
                }
            } else {
                None
            };
            let is_file = file_type.is_file()
                || followed_metadata
                    .as_ref()
                    .is_some_and(fs::Metadata::is_file);
            let is_dir =
                file_type.is_dir() || followed_metadata.as_ref().is_some_and(fs::Metadata::is_dir);

            if is_file {
                self.collect_file(
                    &path,
                    canonical_child,
                    file_type.is_symlink(),
                    collect_org,
                    filesystem_visibility,
                )?;
            } else if is_dir {
                self.collect_directory(
                    &path,
                    canonical_child,
                    file_type.is_symlink(),
                    collect_org,
                    filesystem_visibility,
                )?;
            }
        }

        Ok(())
    }

    fn record_dangling_symlink(&mut self, path: PathBuf) {
        let eligible_file = !self.global_exclusions.matches_file(&path)
            && !self
                .indexed_universe
                .root_scope_excludes_file(self.scope_id, &path);
        if eligible_file
            || !self
                .indexed_universe
                .root_scope_excludes_directory(self.scope_id, &path)
        {
            self.dangling_filesystem_symlinks.insert(path);
        }
    }

    fn collect_file(
        &mut self,
        path: &Path,
        canonical_child: PathBuf,
        symlink: bool,
        collect_org: bool,
        filesystem_visibility: Option<bool>,
    ) -> Result<(), IndexerError> {
        let org_source = collect_org && is_org_source_path(path);
        if !org_source && filesystem_visibility.is_none() {
            return Ok(());
        }
        let canonical_path = if org_source {
            canonicalize_existing_file(path)?
        } else if let Some(canonical) =
            canonicalize_filesystem_entry(path, canonical_child, symlink)?
        {
            canonical
        } else {
            return Ok(());
        };
        let filesystem_visibility = filesystem_visibility
            .filter(|_| !self.filesystem_internal_paths.contains(&canonical_path));
        if self.global_exclusions.matches_file(path) {
            if filesystem_visibility.is_some() {
                self.globally_excluded_filesystem_files
                    .insert(canonical_path.clone());
                if symlink {
                    self.globally_excluded_filesystem_symlinks
                        .insert(path.to_path_buf(), canonical_path.clone());
                }
            }
            if org_source {
                self.globally_excluded.insert(canonical_path.clone());
                self.indexed_universe
                    .add_globally_excluded_path(canonical_path);
                *self.had_exclusion_match = true;
            }
            return Ok(());
        }
        if self
            .indexed_universe
            .root_scope_excludes_file(self.scope_id, path)
        {
            *self.had_exclusion_match |= org_source;
            return Ok(());
        }
        if let Some(visible) = filesystem_visibility {
            self.record_filesystem_entry(
                path,
                &canonical_path,
                FilesystemKind::File,
                visible,
                symlink,
            );
        }
        if org_source {
            self.indexed_universe
                .add_source_mapping(path.to_path_buf(), canonical_path.clone());
            insert_discovered_path(
                self.output,
                canonical_path,
                self.scan_root.to_path_buf(),
                ScanRootKind::ConfiguredDir,
            );
        }
        Ok(())
    }

    fn collect_directory(
        &mut self,
        path: &Path,
        canonical_child: PathBuf,
        symlink: bool,
        collect_org: bool,
        filesystem_visibility: Option<bool>,
    ) -> Result<(), IndexerError> {
        let collect_org = collect_org && self.recursive;
        if !collect_org && filesystem_visibility.is_none() {
            return Ok(());
        }
        if self
            .indexed_universe
            .root_scope_excludes_directory(self.scope_id, path)
        {
            *self.had_exclusion_match |= collect_org;
            return Ok(());
        }
        let canonical_path = if collect_org {
            canonicalize_existing_dir(path)?
        } else if let Some(canonical) =
            canonicalize_filesystem_entry(path, canonical_child, symlink)?
        {
            canonical
        } else {
            return Ok(());
        };
        let filesystem_visibility = filesystem_visibility
            .filter(|_| !self.filesystem_internal_paths.contains(&canonical_path));
        if let Some(visible) = filesystem_visibility {
            self.record_filesystem_entry(
                path,
                &canonical_path,
                FilesystemKind::Directory,
                visible,
                symlink,
            );
        }
        if self.recursive {
            self.collect(path, &canonical_path, collect_org, filesystem_visibility)?;
        }
        Ok(())
    }

    fn record_filesystem_entry(
        &mut self,
        path: &Path,
        canonical_path: &Path,
        kind: FilesystemKind,
        visible: bool,
        symlink: bool,
    ) {
        self.filesystem_entries
            .entry(canonical_path.to_path_buf())
            .and_modify(|entry| entry.visible |= visible)
            .or_insert(DiscoveredFilesystemEntry { kind, visible });
        if symlink {
            self.filesystem_symlinks
                .insert(path.to_path_buf(), canonical_path.to_path_buf());
        }
    }
}

fn canonicalize_filesystem_entry(
    path: &Path,
    canonical_child: PathBuf,
    symlink: bool,
) -> Result<Option<PathBuf>, IndexerError> {
    if !symlink {
        return Ok(Some(canonical_child));
    }
    match fs::canonicalize(path) {
        Ok(canonical) => Ok(Some(canonical)),
        Err(source)
            if matches!(
                source.kind(),
                io::ErrorKind::NotFound | io::ErrorKind::NotADirectory
            ) =>
        {
            Ok(None)
        }
        Err(source) => Err(IndexerError::Discover {
            path: path.to_path_buf(),
            source,
        }),
    }
}

pub(in crate::indexer) fn insert_discovered_path(
    output: &mut BTreeMap<PathBuf, (PathBuf, ScanRootKind)>,
    path: PathBuf,
    scan_root: PathBuf,
    scan_root_kind: ScanRootKind,
) {
    match output.get_mut(&path) {
        Some((existing_root, existing_kind)) => {
            let prefer_new_root = match (scan_root_kind, *existing_kind) {
                (ScanRootKind::ConfiguredDir, ScanRootKind::ExplicitFile) => true,
                (ScanRootKind::ExplicitFile, ScanRootKind::ConfiguredDir) => false,
                _ => path_depth(&scan_root) > path_depth(existing_root),
            };

            if prefer_new_root {
                *existing_root = scan_root;
                *existing_kind = scan_root_kind;
            }
        }
        None => {
            output.insert(path, (scan_root, scan_root_kind));
        }
    }
}

pub(in crate::indexer) fn path_depth(path: &Path) -> usize {
    path.components().count()
}

pub(in crate::indexer) fn is_org_source_path(path: &Path) -> bool {
    path.extension()
        .and_then(|value| value.to_str())
        .is_some_and(|value| value.eq_ignore_ascii_case("org"))
}

pub(in crate::indexer) fn canonicalize_existing_file(path: &Path) -> Result<PathBuf, IndexerError> {
    let canonical = fs::canonicalize(path).map_err(|source| IndexerError::Discover {
        path: path.to_path_buf(),
        source,
    })?;
    let metadata = fs::metadata(&canonical).map_err(|source| IndexerError::Discover {
        path: path.to_path_buf(),
        source,
    })?;
    if !metadata.is_file() {
        return Err(IndexerError::Discover {
            path: path.to_path_buf(),
            source: io::Error::new(io::ErrorKind::InvalidInput, "configured path is not a file"),
        });
    }
    Ok(canonical)
}

pub(in crate::indexer) fn canonicalize_existing_dir(path: &Path) -> Result<PathBuf, IndexerError> {
    let canonical = fs::canonicalize(path).map_err(|source| IndexerError::Discover {
        path: path.to_path_buf(),
        source,
    })?;
    let metadata = fs::metadata(&canonical).map_err(|source| IndexerError::Discover {
        path: path.to_path_buf(),
        source,
    })?;
    if !metadata.is_dir() {
        return Err(IndexerError::Discover {
            path: path.to_path_buf(),
            source: io::Error::new(
                io::ErrorKind::InvalidInput,
                "configured path is not a directory",
            ),
        });
    }
    Ok(canonical)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_support::{write_file, TestDir};
    use std::os::unix::fs::symlink;

    fn visibility(discovery: &DiscoveryResult, path: &Path) -> Option<bool> {
        discovery
            .filesystem_entries
            .get(&path.canonicalize().unwrap())
            .map(|entry| entry.visible)
    }

    #[test]
    fn filesystem_visibility_is_relative_to_each_configured_root() {
        let fixture = TestDir::new("discovery-root-visibility");
        let root = fixture.path().join(".ancestor/.root");
        let nested = root.join(".private/deep");
        write_file(&root.join("ordinary.txt"), "");
        write_file(&root.join(".dot.txt"), "");
        write_file(&nested.join("ordinary.txt"), "");
        let config_path = fixture.path().join("config.toml");
        write_file(
            &config_path,
            r#"
[[dirs]]
path = ".ancestor/.root"
recursive = true
"#,
        );
        let mut config = Config::load_from_file(config_path).unwrap();
        let discovery = discover_org_files(&config).unwrap();
        for (relative, expected) in [
            ("", true),
            ("ordinary.txt", true),
            (".dot.txt", false),
            (".private", false),
            (".private/deep", false),
            (".private/deep/ordinary.txt", false),
        ] {
            assert_eq!(
                visibility(&discovery, &root.join(relative)),
                Some(expected),
                "{relative:?}"
            );
        }

        config.dirs.push(crate::config::ConfiguredDir {
            path: nested.clone(),
            recursive: true,
            exclude: Vec::new(),
        });
        for _ in 0..2 {
            let discovery = discover_org_files(&config).unwrap();
            assert_eq!(visibility(&discovery, &root.join(".private")), Some(false));
            assert_eq!(visibility(&discovery, &nested), Some(true));
            assert_eq!(
                visibility(&discovery, &nested.join("ordinary.txt")),
                Some(true)
            );
            config.dirs.reverse();
        }
    }

    #[test]
    fn filesystem_visibility_uses_any_eligible_alias_independent_of_traversal_order() {
        let fixture = TestDir::new("discovery-alias-visibility");
        let root = fixture.path().join("root");
        write_file(&root.join(".private/visible.txt"), "");
        write_file(&root.join(".private/local.txt"), "");
        write_file(&root.join(".private/global.txt"), "");
        write_file(&root.join("ordinary/visible.txt"), "");
        fs::create_dir(root.join("z")).unwrap();
        symlink(".private", root.join("alternate")).unwrap();
        symlink("../ordinary", root.join("z/.alternate")).unwrap();
        let config_path = fixture.path().join("config.toml");
        write_file(
            &config_path,
            r#"
files_exclude = ["root/alternate/global.txt"]

[[dirs]]
path = "root"
recursive = true
exclude = ["alternate/local.txt"]
"#,
        );
        let config = Config::load_from_file(config_path).unwrap();
        let discovery = discover_org_files(&config).unwrap();
        for (relative, expected) in [
            (".private", Some(true)),
            (".private/visible.txt", Some(true)),
            (".private/local.txt", Some(false)),
            (".private/global.txt", None),
            ("ordinary", Some(true)),
            ("ordinary/visible.txt", Some(true)),
        ] {
            assert_eq!(
                visibility(&discovery, &root.join(relative)),
                expected,
                "{relative:?}"
            );
        }
    }
}
