use super::*;

#[derive(Debug)]
pub(crate) struct CandidatePathNormalizer {
    pub(in crate::indexer) indexed_universe: IndexedUniverse,
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
        Ok(Self {
            indexed_universe: discovery.indexed_universe,
        })
    }

    pub(crate) fn watcher_directory_hints(&self) -> Vec<(PathBuf, bool)> {
        self.indexed_universe.watcher_directory_hints()
    }

    pub(crate) fn resolve(&self, path: &Path) -> CandidatePathResolution {
        let logical_path = normalize_syntactic_path(path.to_path_buf());
        if !logical_path.is_absolute() {
            return CandidatePathResolution::Ignore;
        }

        let symlink_metadata = match fs::symlink_metadata(&logical_path) {
            Ok(metadata) => Some(metadata),
            Err(source) if source.kind() == io::ErrorKind::NotFound => None,
            Err(source) if source.kind() == io::ErrorKind::InvalidInput => {
                return CandidatePathResolution::Ignore;
            }
            Err(_) => return CandidatePathResolution::Reconcile,
        };

        let Some(symlink_metadata) = symlink_metadata else {
            if self.indexed_universe.is_configured_root_path(&logical_path) {
                return CandidatePathResolution::Reconcile;
            }
            if let Some(canonical_directory) = self
                .indexed_universe
                .canonical_directory_for_known_path(&logical_path)
            {
                return CandidatePathResolution::Candidate(canonical_directory.to_path_buf());
            }

            let candidate = self
                .indexed_universe
                .normalize_candidate_path(&logical_path, None)
                .or_else(|| {
                    self.indexed_universe
                        .is_explicit_logical_path(&logical_path)
                        .then(|| logical_path.clone())
                });
            return self.classify_file_candidate(&logical_path, candidate);
        };

        let existing_is_symlink = symlink_metadata.file_type().is_symlink();
        let canonical_path = match fs::canonicalize(&logical_path) {
            Ok(path) => path,
            Err(source) if source.kind() == io::ErrorKind::NotFound => {
                return CandidatePathResolution::Reconcile;
            }
            Err(source) if source.kind() == io::ErrorKind::InvalidInput => {
                return CandidatePathResolution::Ignore;
            }
            Err(_) => return CandidatePathResolution::Reconcile,
        };
        let metadata = match fs::metadata(&canonical_path) {
            Ok(metadata) => metadata,
            Err(source) if source.kind() == io::ErrorKind::InvalidInput => {
                return CandidatePathResolution::Ignore;
            }
            Err(_) => return CandidatePathResolution::Reconcile,
        };

        if metadata.is_dir() {
            return if self.indexed_universe.is_known_directory_path(&logical_path)
                || self
                    .indexed_universe
                    .is_known_directory_path(&canonical_path)
                || self.indexed_universe.includes_logical_file(&logical_path)
                || self.indexed_universe.contains(&canonical_path)
            {
                CandidatePathResolution::Reconcile
            } else {
                CandidatePathResolution::Ignore
            };
        }
        if !metadata.is_file() {
            return CandidatePathResolution::Ignore;
        }

        if let Some(previous_canonical) = self
            .indexed_universe
            .source_mapping_for_logical_path(&logical_path)
        {
            if previous_canonical != canonical_path {
                return CandidatePathResolution::Reconcile;
            }
        }

        let candidate = self
            .indexed_universe
            .normalize_candidate_path(&logical_path, Some(&canonical_path));
        if existing_is_symlink && candidate.is_some() {
            return CandidatePathResolution::Reconcile;
        }

        self.classify_file_candidate(&logical_path, candidate)
    }

    pub(in crate::indexer) fn classify_file_candidate(
        &self,
        logical_path: &Path,
        candidate: Option<PathBuf>,
    ) -> CandidatePathResolution {
        let Some(candidate) = candidate else {
            return CandidatePathResolution::Ignore;
        };

        if self.indexed_universe.is_known_source(&candidate)
            || self
                .indexed_universe
                .is_explicit_candidate(logical_path, &candidate)
            || is_org_source_path(logical_path)
            || is_org_source_path(&candidate)
        {
            CandidatePathResolution::Candidate(candidate)
        } else {
            CandidatePathResolution::Ignore
        }
    }
}
