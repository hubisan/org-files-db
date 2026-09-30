use std::{
    collections::{BTreeMap, BTreeSet},
    path::{Path, PathBuf},
};

use crate::exclusions::ExclusionMatcher;

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

    pub(crate) fn is_explicit_candidate(&self, logical_path: &Path, canonical_path: &Path) -> bool {
        self.explicit_logical_paths.contains(logical_path)
            || self.explicit_inclusions.contains(canonical_path)
    }

    pub(crate) fn is_known_source(&self, canonical_path: &Path) -> bool {
        self.file_mappings
            .iter()
            .any(|(_, mapped_path)| mapped_path.as_path() == canonical_path)
    }

    pub(crate) fn source_mapping_for_logical_path(&self, logical_path: &Path) -> Option<&Path> {
        self.file_mappings
            .iter()
            .find(|(logical, _)| logical.as_path() == logical_path)
            .map(|(_, canonical)| canonical.as_path())
    }

    pub(crate) fn is_known_directory_path(&self, path: &Path) -> bool {
        self.canonical_directory_for_known_path(path).is_some()
    }

    pub(crate) fn is_configured_root_path(&self, path: &Path) -> bool {
        self.root_scopes.iter().any(|scope| {
            let canonical_root = scope
                .directory_mappings
                .first()
                .map(|(_, canonical)| canonical.as_path());
            scope.logical_root.as_path() == path || canonical_root == Some(path)
        })
    }

    pub(crate) fn canonical_directory_for_known_path(&self, path: &Path) -> Option<&Path> {
        self.root_scopes.iter().find_map(|scope| {
            scope
                .directory_mappings
                .iter()
                .find(|(logical, canonical)| {
                    logical.as_path() == path || canonical.as_path() == path
                })
                .map(|(_, canonical)| canonical.as_path())
        })
    }

    pub(crate) fn is_explicit_logical_path(&self, path: &Path) -> bool {
        self.explicit_logical_paths.contains(path)
    }

    pub(crate) fn includes_logical_file(&self, path: &Path) -> bool {
        if self.global_exclusions.matches_file(path) {
            return false;
        }

        self.explicit_logical_paths.contains(path)
            || self
                .root_scopes
                .iter()
                .any(|scope| scope.includes_logical(path))
    }

    pub(crate) fn watcher_directory_hints(&self) -> Vec<(PathBuf, bool)> {
        let mut hints = BTreeMap::<PathBuf, bool>::new();

        for scope in &self.root_scopes {
            for (_, canonical) in &scope.directory_mappings {
                let recursive = hints.entry(canonical.clone()).or_insert(false);
                *recursive |= scope.recursive;
            }
        }

        for (_, canonical_file) in &self.file_mappings {
            if let Some(parent) = canonical_file.parent() {
                hints.entry(parent.to_path_buf()).or_insert(false);
            }
        }

        hints.into_iter().collect()
    }

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

    fn includes_logical(&self, path: &Path) -> bool {
        self.includes_logical_file(path)
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
