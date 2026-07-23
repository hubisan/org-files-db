use std::path::{Path, PathBuf};

use globset::{GlobBuilder, GlobMatcher};

use crate::config::{ConfiguredDir, DiscoveryConfig};

/// Compiled exclusion rules over logical discovery paths.
///
/// The configuration loader validates every pattern before this type is built.
#[derive(Debug)]
pub(crate) struct ExclusionMatcher {
    rules: Vec<ScopedGlob>,
}

#[derive(Debug)]
struct ScopedGlob {
    base: Option<PathBuf>,
    file_matcher: Option<GlobMatcher>,
    directory_matcher: Option<GlobMatcher>,
}

impl ExclusionMatcher {
    pub(crate) fn empty() -> Self {
        Self { rules: Vec::new() }
    }

    pub(crate) fn global(config: &DiscoveryConfig) -> Self {
        Self::new(
            &config.files_exclude,
            &config.config_dir,
            config.home_dir.as_deref(),
        )
    }

    pub(crate) fn local(dir: &ConfiguredDir, config: &DiscoveryConfig) -> Self {
        Self::new(&dir.exclude, &dir.path, config.home_dir.as_deref())
    }

    fn new(patterns: &[String], relative_base: &Path, home_dir: Option<&Path>) -> Self {
        Self {
            rules: patterns
                .iter()
                .map(|pattern| ScopedGlob::new(pattern, relative_base, home_dir))
                .collect(),
        }
    }

    pub(crate) fn matches_file(&self, path: &Path) -> bool {
        self.rules.iter().any(|rule| rule.matches(path))
    }

    /// Directory rules are evaluated directly. A trailing `/**` also matches
    /// its directory root so that `archive/**` prunes `archive` itself.
    pub(crate) fn matches_directory(&self, path: &Path) -> bool {
        self.rules.iter().any(|rule| rule.matches_directory(path))
    }

    pub(crate) fn matches_path_or_excluded_ancestor(&self, path: &Path, root: &Path) -> bool {
        if self.matches_file(path) {
            return true;
        }

        let Ok(relative) = path.strip_prefix(root) else {
            return false;
        };
        let mut ancestor = root.to_path_buf();
        for component in relative.components() {
            ancestor.push(component.as_os_str());
            if ancestor != path && self.matches_directory(&ancestor) {
                return true;
            }
        }
        false
    }
}

impl ScopedGlob {
    fn new(pattern: &str, relative_base: &Path, home_dir: Option<&Path>) -> Self {
        let directory_only = pattern.ends_with('/') && !pattern.ends_with("/**");
        let pattern = if directory_only {
            pattern.trim_end_matches('/')
        } else {
            pattern
        };
        let (base, pattern) = if let Some(home_relative) = pattern.strip_prefix("~/") {
            (home_dir.map(Path::to_path_buf), home_relative)
        } else if Path::new(pattern).is_absolute() {
            (None, pattern)
        } else {
            (Some(relative_base.to_path_buf()), pattern)
        };
        let file_matcher = (!directory_only).then(|| compile(pattern));
        let directory_matcher = pattern
            .strip_suffix("/**")
            .filter(|prefix| !prefix.is_empty())
            .map(compile);
        Self {
            base,
            file_matcher,
            directory_matcher: directory_matcher.or_else(|| Some(compile(pattern))),
        }
    }

    fn candidate<'a>(&self, path: &'a Path) -> Option<&'a Path> {
        self.base
            .as_deref()
            .map_or(Some(path), |base| path.strip_prefix(base).ok())
    }

    fn matches(&self, path: &Path) -> bool {
        self.candidate(path).is_some_and(|candidate| {
            self.file_matcher
                .as_ref()
                .is_some_and(|matcher| matcher.is_match(candidate))
        })
    }

    fn matches_directory(&self, path: &Path) -> bool {
        self.candidate(path).is_some_and(|candidate| {
            self.directory_matcher
                .as_ref()
                .is_some_and(|matcher| matcher.is_match(candidate))
        })
    }
}

fn compile(pattern: &str) -> GlobMatcher {
    GlobBuilder::new(pattern)
        .literal_separator(true)
        .backslash_escape(true)
        .build()
        .expect("config validates exclusion globs")
        .compile_matcher()
}

#[cfg(test)]
mod tests {
    use super::ExclusionMatcher;
    use std::path::Path;

    #[test]
    fn directory_patterns_match_their_intended_roots_without_a_probe_path() {
        let base = Path::new("/tmp/root");
        let archive_tree = ExclusionMatcher::new(&["archive/**".to_string()], base, None);
        assert!(archive_tree.matches_directory(Path::new("/tmp/root/archive")));
        assert!(archive_tree.matches_file(Path::new("/tmp/root/archive/old.org")));

        let directory_only = ExclusionMatcher::new(&["archive/".to_string()], base, None);
        assert!(directory_only.matches_directory(Path::new("/tmp/root/archive")));
        assert!(!directory_only.matches_file(Path::new("/tmp/root/archive")));

        let direct_children = ExclusionMatcher::new(&["archive/*".to_string()], base, None);
        assert!(!direct_children.matches_directory(Path::new("/tmp/root/archive")));
        assert!(direct_children.matches_directory(Path::new("/tmp/root/archive/nested")));

        let nested_tree = ExclusionMatcher::new(&["**/archive/**".to_string()], base, None);
        assert!(nested_tree.matches_directory(Path::new("/tmp/root/nested/archive")));
    }

    #[test]
    fn home_relative_rules_keep_the_configured_home_basis() {
        let matcher = ExclusionMatcher::new(
            &["~/private.org".to_string()],
            Path::new("/tmp/config"),
            Some(Path::new("/tmp/injected-home")),
        );
        assert!(matcher.matches_file(Path::new("/tmp/injected-home/private.org")));
        assert!(!matcher.matches_file(Path::new("/tmp/process-home/private.org")));
    }
}
