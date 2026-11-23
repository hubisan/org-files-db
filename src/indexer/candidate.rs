use super::*;
use std::path::Component;

const MAX_SYMLINK_HOPS: usize = 40;

#[derive(Debug)]
pub(crate) struct CandidatePathNormalizer {
    indexed_universe: IndexedUniverse,
    filesystem_internal_paths: FilesystemInternalPaths,
    filesystem_directories: BTreeSet<PathBuf>,
    filesystem_symlinks: BTreeMap<PathBuf, PathBuf>,
    filesystem_symlink_targets: BTreeMap<PathBuf, FilesystemKind>,
    globally_excluded_filesystem_symlinks: BTreeMap<PathBuf, PathBuf>,
    globally_excluded_filesystem_targets: BTreeSet<PathBuf>,
    filesystem_dependency_events: BTreeSet<PathBuf>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum CandidatePathResolution {
    Candidate(PathBuf),
    Ignore,
    Reconcile,
}

impl CandidatePathNormalizer {
    pub(crate) fn from_config(config: &Config) -> Result<Self, IndexerError> {
        let discovery = discover_org_files(config)?;
        let filesystem_internal_paths = FilesystemInternalPaths::new(config);
        let filesystem_symlink_targets = discovery
            .filesystem_symlinks
            .values()
            .filter_map(|target| {
                discovery
                    .filesystem_entries
                    .get(target)
                    .map(|entry| (target.clone(), entry.kind))
            })
            .collect();
        let filesystem_directories = discovery
            .filesystem_entries
            .into_iter()
            .filter_map(|(path, entry)| (entry.kind == FilesystemKind::Directory).then_some(path))
            .collect();
        let dangling_filesystem_symlinks = discovery
            .dangling_filesystem_symlinks
            .into_iter()
            .filter_map(|alias| {
                let target = fs::read_link(&alias).ok()?;
                let target = fs::canonicalize(alias.parent()?).ok()?.join(target);
                (!filesystem_internal_paths.contains(&target)).then_some((alias, target))
            })
            .collect::<BTreeMap<_, _>>();
        let globally_excluded_filesystem_targets = discovery
            .globally_excluded_filesystem_symlinks
            .values()
            .cloned()
            .collect();
        let mut filesystem_symlinks = discovery.filesystem_symlinks;
        let mut filesystem_dependency_events = BTreeSet::new();
        for alias in filesystem_symlinks
            .keys()
            .chain(&discovery.suppressed_filesystem_symlinks)
            .chain(discovery.globally_excluded_filesystem_symlinks.keys())
            .chain(dangling_filesystem_symlinks.keys())
            .collect::<BTreeSet<_>>()
        {
            let mut events = BTreeSet::new();
            let mut remaining_links = MAX_SYMLINK_HOPS;
            collect_alias_dependencies(
                alias,
                &mut PathBuf::new(),
                &mut remaining_links,
                &mut events,
            );
            // Direct alias events already have their own handling.
            if let Some(physical_alias) = path_with_canonical_parent(alias) {
                events.remove(&physical_alias);
            }
            filesystem_dependency_events.extend(
                events
                    .into_iter()
                    .filter(|path| !filesystem_internal_paths.contains(path)),
            );
        }
        filesystem_symlinks.extend(dangling_filesystem_symlinks);
        Ok(Self {
            indexed_universe: discovery.indexed_universe,
            filesystem_internal_paths,
            filesystem_directories,
            filesystem_symlinks: alias_event_paths(filesystem_symlinks),
            filesystem_symlink_targets,
            globally_excluded_filesystem_symlinks: alias_event_paths(
                discovery.globally_excluded_filesystem_symlinks,
            ),
            globally_excluded_filesystem_targets,
            filesystem_dependency_events,
        })
    }

    pub(crate) fn watcher_directory_hints(&self) -> Vec<(PathBuf, bool)> {
        let mut hints = BTreeMap::from_iter(self.indexed_universe.watcher_directory_hints());
        for target in self.filesystem_symlink_targets.keys() {
            if let Some(parent) = target.parent() {
                hints.entry(parent.to_path_buf()).or_insert(false);
            }
        }
        for path in &self.filesystem_dependency_events {
            if let Some(parent) = path.parent() {
                hints.entry(parent.to_path_buf()).or_insert(false);
            }
        }
        hints.into_iter().collect()
    }

    pub(crate) fn resolve(&self, path: &Path) -> CandidatePathResolution {
        let logical_path = normalize_syntactic_path(path.to_path_buf());
        if !logical_path.is_absolute() || self.filesystem_internal_paths.contains(&logical_path) {
            return CandidatePathResolution::Ignore;
        }

        if self.filesystem_symlinks.contains_key(&logical_path)
            || self.filesystem_dependency_events.contains(&logical_path)
        {
            return CandidatePathResolution::Reconcile;
        }
        if let Some(previous) = self
            .globally_excluded_filesystem_symlinks
            .get(&logical_path)
        {
            return if fs::canonicalize(&logical_path)
                .as_ref()
                .is_ok_and(|path| path == previous)
            {
                CandidatePathResolution::Ignore
            } else {
                CandidatePathResolution::Reconcile
            };
        }
        let symlink_metadata = match fs::symlink_metadata(&logical_path) {
            Ok(metadata) => metadata,
            Err(source) if source.kind() == io::ErrorKind::NotFound => {
                return self.resolve_missing(&logical_path);
            }
            Err(source) if source.kind() == io::ErrorKind::InvalidInput => {
                return CandidatePathResolution::Ignore;
            }
            Err(_) => return CandidatePathResolution::Reconcile,
        };

        let existing_is_symlink = symlink_metadata.file_type().is_symlink();
        let canonical_path = match fs::canonicalize(&logical_path) {
            Ok(path) => path,
            Err(source) if source.kind() == io::ErrorKind::InvalidInput => {
                return CandidatePathResolution::Ignore;
            }
            Err(source) if source.kind() == io::ErrorKind::NotFound => {
                return if self.is_potential_symlink_path(&logical_path) {
                    CandidatePathResolution::Reconcile
                } else {
                    CandidatePathResolution::Ignore
                };
            }
            Err(_) => return CandidatePathResolution::Reconcile,
        };
        if self.filesystem_internal_paths.contains(&canonical_path) {
            return CandidatePathResolution::Ignore;
        }
        let metadata = if existing_is_symlink {
            match fs::metadata(&canonical_path) {
                Ok(metadata) => metadata,
                Err(source) if source.kind() == io::ErrorKind::InvalidInput => {
                    return CandidatePathResolution::Ignore;
                }
                Err(_) => return CandidatePathResolution::Reconcile,
            }
        } else {
            symlink_metadata
        };
        if let Some(previous_canonical) = self
            .indexed_universe
            .source_mapping_for_logical_path(&logical_path)
        {
            if previous_canonical != canonical_path {
                return CandidatePathResolution::Reconcile;
            }
        }

        if metadata.is_dir() {
            // A new link is eligible by its own location. An unrelated alias
            // should not qualify solely because it points into a watched tree.
            let directory_path = if existing_is_symlink {
                &logical_path
            } else {
                &canonical_path
            };
            return if self
                .indexed_universe
                .includes_candidate_directory(&logical_path, directory_path)
                || self.filesystem_symlink_targets.contains_key(directory_path)
            {
                CandidatePathResolution::Reconcile
            } else {
                CandidatePathResolution::Ignore
            };
        }

        if existing_is_symlink {
            if metadata.is_file() && self.is_potential_symlink_path(&logical_path) {
                let candidate = self.file_candidate(&logical_path, Some(&canonical_path));
                if matches!(candidate, CandidatePathResolution::Candidate(_))
                    || (self
                        .indexed_universe
                        .is_globally_excluded_discovery_file(&logical_path)
                        && (self.indexed_universe.contains(&canonical_path)
                            || self.indexed_universe.includes_filesystem_candidate_file(
                                &canonical_path,
                                &canonical_path,
                            )
                            || self
                                .filesystem_symlink_targets
                                .contains_key(&canonical_path)))
                {
                    return CandidatePathResolution::Reconcile;
                }
            }
            return CandidatePathResolution::Ignore;
        }

        if self.indexed_universe.is_known_directory_path(&logical_path)
            || self.filesystem_directories.contains(&canonical_path)
        {
            return CandidatePathResolution::Reconcile;
        }

        self.file_candidate(&logical_path, Some(&canonical_path))
    }

    fn resolve_missing(&self, logical_path: &Path) -> CandidatePathResolution {
        if self.indexed_universe.is_configured_root_path(logical_path)
            || self.filesystem_symlink_targets.get(logical_path) == Some(&FilesystemKind::Directory)
        {
            return CandidatePathResolution::Reconcile;
        }
        // For a removed entry reached through a directory link, the vanished
        // basename still belongs to the canonical identity of its parent.
        let canonical_path = path_with_canonical_parent(logical_path);
        let canonical_directory = self
            .indexed_universe
            .canonical_directory_for_known_path(logical_path)
            .or_else(|| {
                self.filesystem_directories
                    .get(logical_path)
                    .map(PathBuf::as_path)
            })
            .or_else(|| {
                canonical_path
                    .as_deref()
                    .filter(|path| self.filesystem_directories.contains(*path))
            });
        if let Some(canonical_directory) = canonical_directory {
            let contains_symlinks = [
                &self.filesystem_symlinks,
                &self.globally_excluded_filesystem_symlinks,
            ]
            .into_iter()
            .any(|symlinks| {
                [logical_path, canonical_directory]
                    .into_iter()
                    .any(|directory| {
                        symlinks
                            .range(directory.to_path_buf()..)
                            .next()
                            .is_some_and(|(alias, _)| alias.starts_with(directory))
                    })
            });
            return if contains_symlinks {
                CandidatePathResolution::Reconcile
            } else {
                CandidatePathResolution::Candidate(canonical_directory.to_path_buf())
            };
        }

        self.file_candidate(logical_path, canonical_path.as_deref())
    }

    fn is_potential_symlink_path(&self, path: &Path) -> bool {
        self.indexed_universe.includes_logical_file(path)
            || self.indexed_universe.contains(path)
            || self.indexed_universe.is_known_source(path)
            || self
                .indexed_universe
                .includes_filesystem_candidate_file(path, path)
            || self
                .indexed_universe
                .includes_candidate_directory(path, path)
            || self.filesystem_symlink_targets.contains_key(path)
    }

    fn file_candidate(
        &self,
        logical_path: &Path,
        canonical_path: Option<&Path>,
    ) -> CandidatePathResolution {
        let candidate = canonical_path
            .filter(|path| self.filesystem_symlink_targets.contains_key(*path))
            .map(Path::to_path_buf)
            .or_else(|| {
                self.indexed_universe
                    .normalize_candidate_path(logical_path, canonical_path)
            })
            .or_else(|| {
                let canonical = canonical_path.unwrap_or(logical_path);
                self.indexed_universe
                    .includes_filesystem_candidate_file(logical_path, canonical)
                    .then(|| canonical.to_path_buf())
            })
            .or_else(|| {
                self.filesystem_symlink_targets
                    .contains_key(logical_path)
                    .then(|| logical_path.to_path_buf())
            })
            .or_else(|| {
                self.indexed_universe
                    .is_explicit_logical_path(logical_path)
                    .then(|| logical_path.to_path_buf())
            });
        match candidate {
            Some(path)
                if !self.filesystem_internal_paths.contains(&path)
                    && (!self.globally_excluded_filesystem_targets.contains(&path)
                        || self.indexed_universe.is_known_source(&path)) =>
            {
                CandidatePathResolution::Candidate(path)
            }
            _ => CandidatePathResolution::Ignore,
        }
    }
}

fn alias_event_paths(mut logical_paths: BTreeMap<PathBuf, PathBuf>) -> BTreeMap<PathBuf, PathBuf> {
    // Backends may report a link under the physical path of a traversed directory.
    let physical_paths = logical_paths
        .iter()
        .filter_map(|(alias, target)| {
            let physical = path_with_canonical_parent(alias)?;
            (physical != *alias && physical != *target).then(|| (physical, target.clone()))
        })
        .collect::<Vec<_>>();
    logical_paths.extend(physical_paths);
    logical_paths
}

fn path_with_canonical_parent(path: &Path) -> Option<PathBuf> {
    let (parent, name) = path.parent().zip(path.file_name())?;
    fs::canonicalize(parent)
        .ok()
        .map(|parent| parent.join(name))
}

// Follow links a component at a time. Canonicalizing the entire alias would
// lose intermediate links and would give no target at all for a dangling alias.
fn collect_alias_dependencies(
    path: &Path,
    resolved: &mut PathBuf,
    remaining_links: &mut usize,
    events: &mut BTreeSet<PathBuf>,
) -> bool {
    for component in path.components() {
        let name = match component {
            Component::Normal(name) => name,
            Component::ParentDir => {
                resolved.pop();
                continue;
            }
            Component::CurDir => continue,
            Component::Prefix(_) | Component::RootDir => {
                resolved.push(component.as_os_str());
                continue;
            }
        };
        let candidate = resolved.join(name);
        match fs::symlink_metadata(&candidate) {
            Ok(metadata) if metadata.file_type().is_symlink() => {
                let target = fs::read_link(&candidate);
                events.insert(candidate);
                let Ok(target) = target else {
                    return false;
                };
                if *remaining_links == 0 {
                    return false;
                }
                *remaining_links -= 1;
                if !collect_alias_dependencies(&target, resolved, remaining_links, events) {
                    return false;
                }
            }
            Ok(_) => resolved.push(name),
            Err(error)
                if matches!(
                    error.kind(),
                    io::ErrorKind::NotFound | io::ErrorKind::NotADirectory
                ) =>
            {
                if let Some(event) = first_missing_target_component(&candidate) {
                    events.insert(event);
                }
                return false;
            }
            Err(_) => return false,
        }
    }
    true
}

fn first_missing_target_component(target: &Path) -> Option<PathBuf> {
    for ancestor in target.ancestors().skip(1) {
        if let Ok(canonical) = fs::canonicalize(ancestor) {
            if canonical.is_dir() {
                // Only the first child is visible to a nonrecursive watch.
                let next = target.strip_prefix(ancestor).ok()?.components().next()?;
                return Some(canonical.join(next));
            }
        }
    }
    None
}
