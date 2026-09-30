use super::*;

pub(in crate::indexer) struct DiscoveryResult {
    pub(in crate::indexer) files: Vec<DiscoveredOrgFile>,
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
    let mut indexed_universe = IndexedUniverse::default();
    let mut missing_explicit_files = Vec::new();
    let mut globally_excluded = BTreeSet::new();
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
        let mut visited_directories = BTreeSet::new();
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
            indexed_universe: &mut indexed_universe,
            visited_directories: &mut visited_directories,
            global_exclusions: &global_exclusions,
            globally_excluded: &mut globally_excluded,
            had_exclusion_match: &mut had_exclusion_match,
            scope_id,
        };
        collector.collect(&dir.path)?;
    }

    for excluded in &globally_excluded {
        paths.remove(excluded);
    }

    Ok(DiscoveryResult {
        files: paths
            .into_iter()
            .map(|(path, (scan_root, _))| DiscoveredOrgFile {
                identity: FileIdentity::from_canonical_path(&path),
                path,
                scan_root,
            })
            .collect(),
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

pub(in crate::indexer) struct DirectoryCollector<'a> {
    pub(in crate::indexer) scan_root: &'a Path,
    pub(in crate::indexer) recursive: bool,
    pub(in crate::indexer) output: &'a mut BTreeMap<PathBuf, (PathBuf, ScanRootKind)>,
    pub(in crate::indexer) indexed_universe: &'a mut IndexedUniverse,
    pub(in crate::indexer) visited_directories: &'a mut BTreeSet<PathBuf>,
    pub(in crate::indexer) global_exclusions: &'a ExclusionMatcher,
    pub(in crate::indexer) globally_excluded: &'a mut BTreeSet<PathBuf>,
    pub(in crate::indexer) had_exclusion_match: &'a mut bool,
    pub(in crate::indexer) scope_id: usize,
}

impl DirectoryCollector<'_> {
    pub(in crate::indexer) fn collect(&mut self, logical_dir: &Path) -> Result<(), IndexerError> {
        let canonical_dir = canonicalize_existing_dir(logical_dir)?;
        if !self.visited_directories.insert(canonical_dir.clone()) {
            return Ok(());
        }
        self.indexed_universe.add_directory_mapping(
            self.scope_id,
            logical_dir.to_path_buf(),
            canonical_dir,
        );
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
            let file_type = entry.file_type().map_err(|source| IndexerError::Discover {
                path: path.clone(),
                source,
            })?;

            let followed_metadata = if file_type.is_symlink() {
                match fs::metadata(&path) {
                    Ok(metadata) => Some(metadata),
                    // Dangling symlinks (e.g. Emacs lock files) are treated as absent.
                    Err(source) if source.kind() == io::ErrorKind::NotFound => continue,
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
                if is_org_source_path(&path) {
                    let canonical_path = canonicalize_existing_file(&path)?;
                    if self.global_exclusions.matches_file(&path) {
                        self.globally_excluded.insert(canonical_path.clone());
                        self.indexed_universe
                            .add_globally_excluded_path(canonical_path);
                        *self.had_exclusion_match = true;
                        continue;
                    }
                    if self
                        .indexed_universe
                        .root_scope_excludes_file(self.scope_id, &path)
                    {
                        *self.had_exclusion_match = true;
                        continue;
                    }
                    self.indexed_universe
                        .add_source_mapping(path.clone(), canonical_path.clone());
                    insert_discovered_path(
                        self.output,
                        canonical_path,
                        self.scan_root.to_path_buf(),
                        ScanRootKind::ConfiguredDir,
                    );
                }
            } else if self.recursive && is_dir {
                if self
                    .indexed_universe
                    .root_scope_excludes_directory(self.scope_id, &path)
                {
                    *self.had_exclusion_match = true;
                    continue;
                }
                self.collect(&path)?;
            }
        }

        Ok(())
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
