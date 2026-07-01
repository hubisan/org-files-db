use std::{
    collections::HashSet,
    error::Error,
    fmt, fs,
    path::{Component, Path, PathBuf},
};

use serde::Deserialize;

use crate::{
    parser::{
        LinkScannerConfig, ParseOptions, TodoKeyword, TodoKeywordConfig,
        DEFAULT_PLAIN_LINK_PROTOCOLS,
    },
    todo_keywords::parse_todo_keyword_spec,
};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Config {
    pub db_path: PathBuf,
    pub files: Vec<PathBuf>,
    pub dirs: Vec<PathBuf>,
    pub recursive: bool,
    pub links: LinkConfig,
    pub todo: TodoConfig,
    pub search: SearchConfig,
}

impl Config {
    pub fn load_from_file(path: impl AsRef<Path>) -> Result<Self, ConfigError> {
        let path = path.as_ref();
        let content = fs::read_to_string(path).map_err(|source| ConfigError::ReadFile {
            path: path.to_path_buf(),
            source,
        })?;
        let raw: RawConfig = toml::from_str(&content).map_err(|source| ConfigError::ParseToml {
            path: path.to_path_buf(),
            source,
        })?;
        Self::from_raw(path, raw)
    }

    fn from_raw(path: &Path, raw: RawConfig) -> Result<Self, ConfigError> {
        Self::from_raw_with_home_dir(path, raw, current_home_dir().as_deref())
    }

    fn from_raw_with_home_dir(
        path: &Path,
        raw: RawConfig,
        home_dir: Option<&Path>,
    ) -> Result<Self, ConfigError> {
        let RawConfig {
            db_path: raw_db_path,
            files: raw_files,
            dirs: raw_dirs,
            recursive,
            links,
            todo,
            search,
        } = raw;
        let base_dir = absolute_base_dir(path.parent().unwrap_or_else(|| Path::new(".")));
        let db_path = resolve_path(&base_dir, raw_db_path, home_dir)?;
        let files = raw_files
            .into_iter()
            .map(|file| resolve_path(&base_dir, file, home_dir))
            .collect::<Result<Vec<_>, _>>()?;
        let dirs = raw_dirs
            .into_iter()
            .map(|dir| resolve_path(&base_dir, dir, home_dir))
            .collect::<Result<Vec<_>, _>>()?;
        let todo = todo.unwrap_or(RawTodoConfig {
            default_open_keywords: None,
            default_closed_keywords: None,
        });
        let links = links.unwrap_or_default();
        let search = search.unwrap_or(RawSearchConfig {
            fts5_enabled: None,
            index_body_text: None,
        });

        Ok(Self {
            db_path,
            files,
            dirs,
            recursive,
            links: LinkConfig {
                plain_protocols: effective_plain_link_protocols(
                    links.plain_protocols,
                    links.custom_protocols,
                ),
            },
            todo: TodoConfig {
                default_open_keywords: todo
                    .default_open_keywords
                    .unwrap_or_else(default_open_keyword_specs)
                    .into_iter()
                    .map(|spec| parse_todo_keyword_spec(&spec))
                    .collect(),
                default_closed_keywords: todo
                    .default_closed_keywords
                    .unwrap_or_else(default_closed_keyword_specs)
                    .into_iter()
                    .map(|spec| parse_todo_keyword_spec(&spec))
                    .collect(),
            },
            search: SearchConfig {
                fts5_enabled: search.fts5_enabled.unwrap_or(true),
                index_body_text: search.index_body_text.unwrap_or(false),
            },
        })
    }

    pub fn parse_options(&self) -> ParseOptions {
        ParseOptions {
            todo_keywords: self.todo.to_keyword_config(),
            link_scanner: LinkScannerConfig {
                plain_link_protocols: self.links.plain_protocols.clone(),
            },
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LinkConfig {
    pub plain_protocols: Vec<String>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TodoConfig {
    pub default_open_keywords: Vec<TodoKeyword>,
    pub default_closed_keywords: Vec<TodoKeyword>,
}

impl TodoConfig {
    pub fn to_keyword_config(&self) -> TodoKeywordConfig {
        if self.default_open_keywords.is_empty() && self.default_closed_keywords.is_empty() {
            return TodoKeywordConfig::default();
        }

        TodoKeywordConfig {
            open: self.default_open_keywords.clone(),
            closed: self.default_closed_keywords.clone(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SearchConfig {
    pub fts5_enabled: bool,
    pub index_body_text: bool,
}

impl Default for Config {
    fn default() -> Self {
        Self {
            db_path: PathBuf::from("org-files-db.sqlite"),
            files: Vec::new(),
            dirs: Vec::new(),
            recursive: false,
            links: LinkConfig::default(),
            todo: TodoConfig::default(),
            search: SearchConfig::default(),
        }
    }
}

impl Default for LinkConfig {
    fn default() -> Self {
        Self {
            plain_protocols: DEFAULT_PLAIN_LINK_PROTOCOLS
                .iter()
                .map(|protocol| (*protocol).to_string())
                .collect(),
        }
    }
}

impl Default for TodoConfig {
    fn default() -> Self {
        Self {
            default_open_keywords: default_open_keywords(),
            default_closed_keywords: default_closed_keywords(),
        }
    }
}

impl Default for SearchConfig {
    fn default() -> Self {
        Self {
            fts5_enabled: true,
            index_body_text: false,
        }
    }
}

#[derive(Debug)]
pub enum ConfigError {
    ReadFile {
        path: PathBuf,
        source: std::io::Error,
    },
    ParseToml {
        path: PathBuf,
        source: toml::de::Error,
    },
    MissingFile {
        path: PathBuf,
    },
    MissingDirectory {
        path: PathBuf,
    },
    MissingHomeDirectory {
        path: PathBuf,
    },
}

impl fmt::Display for ConfigError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::ReadFile { path, source } => {
                write!(
                    f,
                    "failed to read config file {}: {}",
                    path.display(),
                    source
                )
            }
            Self::ParseToml { path, source } => {
                write!(
                    f,
                    "failed to parse config file {}: {}",
                    path.display(),
                    source
                )
            }
            Self::MissingFile { path } => {
                write!(f, "configured file does not exist: {}", path.display())
            }
            Self::MissingDirectory { path } => {
                write!(f, "configured directory does not exist: {}", path.display())
            }
            Self::MissingHomeDirectory { path } => write!(
                f,
                "failed to expand config path {}: home directory could not be determined",
                path.display()
            ),
        }
    }
}

impl Error for ConfigError {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        match self {
            Self::ReadFile { source, .. } => Some(source),
            Self::ParseToml { source, .. } => Some(source),
            Self::MissingFile { .. }
            | Self::MissingDirectory { .. }
            | Self::MissingHomeDirectory { .. } => None,
        }
    }
}

#[derive(Debug, Deserialize)]
struct RawConfig {
    #[serde(default = "default_db_path")]
    db_path: PathBuf,
    #[serde(default)]
    files: Vec<PathBuf>,
    #[serde(default)]
    dirs: Vec<PathBuf>,
    #[serde(default)]
    recursive: bool,
    links: Option<RawLinksConfig>,
    todo: Option<RawTodoConfig>,
    search: Option<RawSearchConfig>,
}

#[derive(Debug, Default, Deserialize)]
struct RawLinksConfig {
    plain_protocols: Option<Vec<String>>,
    custom_protocols: Option<Vec<String>>,
}

#[derive(Debug, Deserialize)]
struct RawTodoConfig {
    default_open_keywords: Option<Vec<String>>,
    default_closed_keywords: Option<Vec<String>>,
}

#[derive(Debug, Deserialize)]
struct RawSearchConfig {
    fts5_enabled: Option<bool>,
    index_body_text: Option<bool>,
}

fn default_db_path() -> PathBuf {
    PathBuf::from("org-files-db.sqlite")
}

fn default_open_keyword_specs() -> Vec<String> {
    vec!["TODO".to_string()]
}

fn default_closed_keyword_specs() -> Vec<String> {
    vec!["DONE".to_string()]
}

fn default_open_keywords() -> Vec<TodoKeyword> {
    default_open_keyword_specs()
        .into_iter()
        .map(|spec| parse_todo_keyword_spec(&spec))
        .collect()
}

fn default_closed_keywords() -> Vec<TodoKeyword> {
    default_closed_keyword_specs()
        .into_iter()
        .map(|spec| parse_todo_keyword_spec(&spec))
        .collect()
}

fn effective_plain_link_protocols(
    plain_protocols: Option<Vec<String>>,
    custom_protocols: Option<Vec<String>>,
) -> Vec<String> {
    let defaults_or_explicit = plain_protocols.unwrap_or_else(|| {
        DEFAULT_PLAIN_LINK_PROTOCOLS
            .iter()
            .map(|protocol| (*protocol).to_string())
            .collect()
    });

    deduplicated_protocols(
        defaults_or_explicit
            .into_iter()
            .chain(custom_protocols.unwrap_or_default()),
    )
}

fn deduplicated_protocols(protocols: impl IntoIterator<Item = String>) -> Vec<String> {
    let mut seen = HashSet::new();
    let mut deduplicated = Vec::new();

    for protocol in protocols {
        let normalized = protocol.to_ascii_lowercase();
        if normalized.is_empty() || !seen.insert(normalized.clone()) {
            continue;
        }
        deduplicated.push(normalized);
    }

    deduplicated
}

// Relative paths in the config are resolved relative to the config file location.
// A leading ~ resolves to the current user's home directory before that fallback.
fn resolve_path(
    base_dir: &Path,
    path: PathBuf,
    home_dir: Option<&Path>,
) -> Result<PathBuf, ConfigError> {
    if let Some(expanded) = expand_home_directory(&path, home_dir)? {
        return Ok(absolute_syntactic_path(expanded));
    }

    let resolved = if path.is_absolute() {
        path
    } else {
        base_dir.join(path)
    };

    Ok(absolute_syntactic_path(resolved))
}

fn expand_home_directory(
    path: &Path,
    home_dir: Option<&Path>,
) -> Result<Option<PathBuf>, ConfigError> {
    let mut components = path.components();
    let Some(std::path::Component::Normal(first_component)) = components.next() else {
        return Ok(None);
    };

    if first_component != "~" {
        return Ok(None);
    }

    let home_dir = home_dir.ok_or_else(|| ConfigError::MissingHomeDirectory {
        path: path.to_path_buf(),
    })?;
    let mut resolved = home_dir.to_path_buf();

    for component in components {
        resolved.push(component.as_os_str());
    }

    Ok(Some(resolved))
}

fn normalize_syntactic_path(path: PathBuf) -> PathBuf {
    let mut normalized = PathBuf::new();

    for component in path.components() {
        if matches!(component, Component::CurDir) {
            continue;
        }
        normalized.push(component.as_os_str());
    }

    normalized
}

fn absolute_base_dir(path: &Path) -> PathBuf {
    if path.is_absolute() {
        return normalize_syntactic_path(path.to_path_buf());
    }

    let current_dir = std::env::current_dir().unwrap_or_else(|_| PathBuf::from("."));
    normalize_syntactic_path(current_dir.join(path))
}

fn absolute_syntactic_path(path: PathBuf) -> PathBuf {
    let normalized = normalize_syntactic_path(path);
    if normalized.is_absolute() {
        normalized
    } else {
        let current_dir = std::env::current_dir().unwrap_or_else(|_| PathBuf::from("."));
        normalize_syntactic_path(current_dir.join(normalized))
    }
}

fn current_home_dir() -> Option<PathBuf> {
    if let Some(home) = std::env::var_os("HOME").filter(|value| !value.is_empty()) {
        return Some(PathBuf::from(home));
    }

    if let Some(profile) = std::env::var_os("USERPROFILE").filter(|value| !value.is_empty()) {
        return Some(PathBuf::from(profile));
    }

    let home_drive = std::env::var_os("HOMEDRIVE").filter(|value| !value.is_empty());
    let home_path = std::env::var_os("HOMEPATH").filter(|value| !value.is_empty());
    match (home_drive, home_path) {
        (Some(drive), Some(path)) => Some(PathBuf::from(drive).join(path)),
        _ => None,
    }
}

#[cfg(test)]
mod tests {
    use super::{
        resolve_path, Config, ConfigError, LinkConfig, RawConfig, RawLinksConfig, RawSearchConfig,
        RawTodoConfig,
    };
    use crate::parser::{LinkScannerConfig, ParseOptions, TodoKeyword, TodoKeywordConfig};
    use std::{
        fs,
        path::{Path, PathBuf},
        time::{SystemTime, UNIX_EPOCH},
    };

    struct TestDir {
        path: PathBuf,
    }

    impl TestDir {
        fn new(name: &str) -> Self {
            let unique = SystemTime::now()
                .duration_since(UNIX_EPOCH)
                .expect("system time should be after unix epoch")
                .as_nanos();
            let path = std::env::temp_dir().join(format!(
                "org-files-db-config-tests-{}-{}-{}",
                name,
                std::process::id(),
                unique
            ));
            fs::create_dir_all(&path).expect("test dir should be created");
            Self { path }
        }

        fn path(&self) -> &Path {
            &self.path
        }
    }

    impl Drop for TestDir {
        fn drop(&mut self) {
            let _ = fs::remove_dir_all(&self.path);
        }
    }

    fn write_file(path: &Path, content: &str) {
        if let Some(parent) = path.parent() {
            fs::create_dir_all(parent).expect("parent dir should be created");
        }
        fs::write(path, content).expect("file should be written");
    }

    #[test]
    fn loads_minimal_config() {
        let test_dir = TestDir::new("minimal");
        let config_path = test_dir.path().join("config.toml");

        write_file(
            &config_path,
            r#"
db_path = "db.sqlite"
"#,
        );

        let config = Config::load_from_file(&config_path).expect("config should load");

        assert_eq!(config.db_path, test_dir.path().join("db.sqlite"));
        assert!(config.files.is_empty());
        assert!(config.dirs.is_empty());
        assert!(!config.recursive);
    }

    #[test]
    fn default_todo_keywords_are_available() {
        let test_dir = TestDir::new("todo-defaults");
        let config_path = test_dir.path().join("config.toml");

        write_file(&config_path, r#"db_path = "db.sqlite""#);

        let config = Config::load_from_file(&config_path).expect("config should load");

        assert_eq!(
            config.todo.default_open_keywords,
            vec![TodoKeyword::new("TODO")]
        );
        assert_eq!(
            config.todo.default_closed_keywords,
            vec![TodoKeyword::new("DONE")]
        );
    }

    #[test]
    fn custom_todo_keywords_can_be_configured() {
        let test_dir = TestDir::new("todo-custom");
        let config_path = test_dir.path().join("config.toml");

        write_file(
            &config_path,
            r#"
db_path = "db.sqlite"

[todo]
default_open_keywords = ["PLAN", "BUILD", "REVIEW"]
default_closed_keywords = ["DONE", "CANCEL"]
"#,
        );

        let config = Config::load_from_file(&config_path).expect("config should load");

        assert_eq!(
            config.todo.default_open_keywords,
            vec![
                TodoKeyword::new("PLAN"),
                TodoKeyword::new("BUILD"),
                TodoKeyword::new("REVIEW"),
            ]
        );
        assert_eq!(
            config.todo.default_closed_keywords,
            vec![TodoKeyword::new("DONE"), TodoKeyword::new("CANCEL")]
        );
    }

    #[test]
    fn fast_selection_keys_are_preserved() {
        let test_dir = TestDir::new("todo-fast-keys");
        let config_path = test_dir.path().join("config.toml");

        write_file(
            &config_path,
            r#"
db_path = "db.sqlite"

[todo]
default_open_keywords = ["TODO(t)", "NEXT(n)"]
default_closed_keywords = ["DONE(d)"]
"#,
        );

        let config = Config::load_from_file(&config_path).expect("config should load");

        assert_eq!(
            config.todo.default_open_keywords,
            vec![
                TodoKeyword::with_fast_key("TODO", 't'),
                TodoKeyword::with_fast_key("NEXT", 'n'),
            ]
        );
        assert_eq!(
            config.todo.default_closed_keywords,
            vec![TodoKeyword::with_fast_key("DONE", 'd')]
        );
    }

    #[test]
    fn fast_selection_keys_trim_outer_whitespace_in_config() {
        let test_dir = TestDir::new("todo-fast-keys-trimmed");
        let config_path = test_dir.path().join("config.toml");

        write_file(
            &config_path,
            r#"
db_path = "db.sqlite"

[todo]
default_open_keywords = [" TODO(t) ", " NEXT(n) "]
default_closed_keywords = [" DONE(d) "]
"#,
        );

        let config = Config::load_from_file(&config_path).expect("config should load");

        assert_eq!(
            config.todo.default_open_keywords,
            vec![
                TodoKeyword::with_fast_key("TODO", 't'),
                TodoKeyword::with_fast_key("NEXT", 'n'),
            ]
        );
        assert_eq!(
            config.todo.default_closed_keywords,
            vec![TodoKeyword::with_fast_key("DONE", 'd')]
        );
    }

    #[test]
    fn resolves_relative_paths_against_config_file_location() {
        let test_dir = TestDir::new("relative-paths");
        let config_dir = test_dir.path().join("nested/config");
        let file_path = config_dir.join("notes.org");
        let dir_path = config_dir.join("notes");
        let config_path = config_dir.join("config.toml");

        write_file(&file_path, "* Note");
        fs::create_dir_all(&dir_path).expect("dir path should be created");
        write_file(
            &config_path,
            r#"
db_path = "../db.sqlite"
files = ["notes.org"]
dirs = ["notes"]
recursive = true
"#,
        );

        let config = Config::load_from_file(&config_path).expect("config should load");

        assert_eq!(config.db_path, config_dir.join("../db.sqlite"));
        assert_eq!(config.files, vec![file_path]);
        assert_eq!(config.dirs, vec![dir_path]);
        assert!(config.recursive);
    }

    #[test]
    fn resolves_relative_config_paths_to_absolute_locations() {
        let resolved = resolve_path(
            Path::new("nested/config"),
            PathBuf::from("./notes/test.org"),
            None,
        )
        .expect("relative config path should resolve");
        let current_dir = std::env::current_dir().expect("cwd should be available");

        assert_eq!(resolved, current_dir.join("nested/config/notes/test.org"));
        assert!(resolved.is_absolute());
        assert!(!resolved.to_string_lossy().contains("/./"));
    }

    #[test]
    fn normalizes_syntactic_dots_after_resolving_config_relative_paths() {
        let test_dir = TestDir::new("relative-dot-paths");
        let config_dir = test_dir.path().join("nested/config");
        let file_path = config_dir.join("test.org");
        let dir_path = config_dir.join("notes");
        let config_path = config_dir.join("config.toml");

        write_file(&file_path, "* Note");
        fs::create_dir_all(&dir_path).expect("dir path should be created");
        write_file(
            &config_path,
            r#"
db_path = "./org-files-db.sqlite"
files = ["./test.org"]
dirs = ["./notes"]
"#,
        );

        let config = Config::load_from_file(&config_path).expect("config should load");

        assert_eq!(config.db_path, config_dir.join("org-files-db.sqlite"));
        assert_eq!(config.files, vec![file_path]);
        assert_eq!(config.dirs, vec![dir_path]);
    }

    #[test]
    fn normalizes_syntactic_dots_in_absolute_paths() {
        let test_dir = TestDir::new("absolute-dot-paths");
        let config_path = test_dir.path().join("config.toml");
        let db_path = test_dir.path().join("db/./org-files-db.sqlite");
        let file_path = test_dir.path().join("notes/./test.org");
        let dir_path = test_dir.path().join("dirs/./notes");
        let normalized_file_path = test_dir.path().join("notes/test.org");
        let normalized_dir_path = test_dir.path().join("dirs/notes");
        let config_body = format!(
            r#"
db_path = "{}"
files = ["{}"]
dirs = ["{}"]
"#,
            db_path.display(),
            file_path.display(),
            dir_path.display()
        );

        write_file(&normalized_file_path, "* Note");
        fs::create_dir_all(&normalized_dir_path).expect("dir path should be created");
        write_file(&config_path, &config_body);

        let config = Config::load_from_file(&config_path).expect("config should load");

        assert_eq!(
            config.db_path,
            test_dir.path().join("db/org-files-db.sqlite")
        );
        assert_eq!(config.files, vec![normalized_file_path]);
        assert_eq!(config.dirs, vec![normalized_dir_path]);
    }

    #[test]
    fn normalizes_syntactic_dots_together_with_tilde_expansion() {
        let test_dir = TestDir::new("tilde-dot-paths");
        let config_path = test_dir.path().join("config.toml");
        let home_dir = test_dir.path().join("home");
        let notes_dir = home_dir.join("notes");
        let file_path = notes_dir.join("test.org");

        fs::create_dir_all(&notes_dir).expect("notes dir should be created");
        write_file(&file_path, "* Note");

        let config = Config::from_raw_with_home_dir(
            &config_path,
            raw_config(
                "~/./org-files-db.sqlite",
                vec!["~/./notes/test.org".to_string()],
                vec!["~/./notes".to_string()],
            ),
            Some(home_dir.as_path()),
        )
        .expect("config should load");

        assert_eq!(config.db_path, home_dir.join("org-files-db.sqlite"));
        assert_eq!(config.files, vec![file_path]);
        assert_eq!(config.dirs, vec![notes_dir]);
    }

    #[test]
    fn loads_missing_file_paths_without_validation() {
        let test_dir = TestDir::new("missing-file");
        let config_path = test_dir.path().join("config.toml");
        let missing_file = test_dir.path().join("missing.org");

        write_file(
            &config_path,
            r#"
db_path = "db.sqlite"
files = ["missing.org"]
"#,
        );

        let config = Config::load_from_file(&config_path).expect("config should load");

        assert_eq!(config.files, vec![missing_file]);
    }

    #[test]
    fn loads_missing_directory_paths_without_validation() {
        let test_dir = TestDir::new("missing-dir");
        let config_path = test_dir.path().join("config.toml");
        let missing_dir = test_dir.path().join("missing-dir");

        write_file(
            &config_path,
            r#"
db_path = "db.sqlite"
dirs = ["missing-dir"]
"#,
        );

        let config = Config::load_from_file(&config_path).expect("config should load");

        assert_eq!(config.dirs, vec![missing_dir]);
    }

    #[test]
    fn loads_fts_and_body_indexing_values() {
        let test_dir = TestDir::new("search-config");
        let config_path = test_dir.path().join("config.toml");

        write_file(
            &config_path,
            r#"
db_path = "db.sqlite"

[search]
fts5_enabled = false
index_body_text = true
"#,
        );

        let config = Config::load_from_file(&config_path).expect("config should load");

        assert!(!config.search.fts5_enabled);
        assert!(config.search.index_body_text);
    }

    #[test]
    fn parse_options_carry_configured_todo_keywords() {
        let test_dir = TestDir::new("parse-options");
        let config_path = test_dir.path().join("config.toml");

        write_file(
            &config_path,
            r#"
db_path = "db.sqlite"

[todo]
default_open_keywords = ["PLAN(p)", "BUILD(b)"]
default_closed_keywords = ["DONE(d)"]
"#,
        );

        let config = Config::load_from_file(&config_path).expect("config should load");
        let options = config.parse_options();

        assert_eq!(
            options,
            ParseOptions {
                todo_keywords: TodoKeywordConfig {
                    open: vec![
                        TodoKeyword::with_fast_key("PLAN", 'p'),
                        TodoKeyword::with_fast_key("BUILD", 'b'),
                    ],
                    closed: vec![TodoKeyword::with_fast_key("DONE", 'd')],
                },
                link_scanner: LinkScannerConfig::default(),
            }
        );
    }

    #[test]
    fn empty_configured_todo_keywords_fall_back_to_org_defaults() {
        let test_dir = TestDir::new("empty-todo-config");
        let config_path = test_dir.path().join("config.toml");

        write_file(
            &config_path,
            r#"
db_path = "db.sqlite"

[todo]
default_open_keywords = []
default_closed_keywords = []
"#,
        );

        let config = Config::load_from_file(&config_path).expect("config should load");

        assert_eq!(
            config.parse_options(),
            ParseOptions {
                todo_keywords: TodoKeywordConfig {
                    open: vec![TodoKeyword::new("TODO")],
                    closed: vec![TodoKeyword::new("DONE")],
                },
                link_scanner: LinkScannerConfig::default(),
            }
        );
    }

    #[test]
    fn missing_plain_protocols_use_defaults() {
        let config = Config::default();

        assert_eq!(config.links, LinkConfig::default());
        assert_eq!(
            config.parse_options().link_scanner,
            LinkScannerConfig::default()
        );
    }

    #[test]
    fn empty_plain_protocols_disable_defaults_but_keep_custom_protocols() {
        let test_dir = TestDir::new("empty-plain-protocols");
        let config_path = test_dir.path().join("config.toml");

        write_file(
            &config_path,
            r#"
db_path = "db.sqlite"

[links]
plain_protocols = []
custom_protocols = ["JIRA", "jira", "shell"]
"#,
        );

        let config = Config::load_from_file(&config_path).expect("config should load");

        assert_eq!(
            config.links.plain_protocols,
            vec!["jira".to_string(), "shell".to_string()]
        );
        assert_eq!(
            config.parse_options().link_scanner.plain_link_protocols,
            vec!["jira".to_string(), "shell".to_string()]
        );
    }

    #[test]
    fn expands_leading_tilde_in_db_path() {
        let test_dir = TestDir::new("tilde-db");
        let config_path = test_dir.path().join("config.toml");
        let home_dir = test_dir.path().join("home");
        fs::create_dir_all(&home_dir).expect("home dir should be created");

        let config = Config::from_raw_with_home_dir(
            &config_path,
            raw_config("~/org-files-db.sqlite", Vec::new(), Vec::new()),
            Some(home_dir.as_path()),
        )
        .expect("config should load");

        assert_eq!(config.db_path, home_dir.join("org-files-db.sqlite"));
    }

    #[test]
    fn expands_leading_tilde_in_files() {
        let test_dir = TestDir::new("tilde-files");
        let config_path = test_dir.path().join("config.toml");
        let home_dir = test_dir.path().join("home");
        let notes_dir = home_dir.join("notes");
        let file_path = notes_dir.join("test.org");

        fs::create_dir_all(&notes_dir).expect("notes dir should be created");
        write_file(&file_path, "* Note");

        let config = Config::from_raw_with_home_dir(
            &config_path,
            raw_config(
                "db.sqlite",
                vec!["~/notes/test.org".to_string()],
                Vec::new(),
            ),
            Some(home_dir.as_path()),
        )
        .expect("config should load");

        assert_eq!(config.files, vec![file_path]);
    }

    #[test]
    fn expands_leading_tilde_in_dirs() {
        let test_dir = TestDir::new("tilde-dirs");
        let config_path = test_dir.path().join("config.toml");
        let home_dir = test_dir.path().join("home");
        let notes_dir = home_dir.join("notes");

        fs::create_dir_all(&notes_dir).expect("notes dir should be created");

        let config = Config::from_raw_with_home_dir(
            &config_path,
            raw_config("db.sqlite", Vec::new(), vec!["~/notes".to_string()]),
            Some(home_dir.as_path()),
        )
        .expect("config should load");

        assert_eq!(config.dirs, vec![notes_dir]);
    }

    #[test]
    fn does_not_expand_non_leading_tilde() {
        let test_dir = TestDir::new("tilde-not-leading");
        let config_dir = test_dir.path().join("nested");
        let config_path = config_dir.join("config.toml");
        let file_path = config_dir.join("notes/~draft.org");
        let dir_path = config_dir.join("notes/~drafts");

        write_file(&file_path, "* Note");
        fs::create_dir_all(&dir_path).expect("dir path should be created");

        let config = Config::from_raw_with_home_dir(
            &config_path,
            raw_config(
                "../db.sqlite",
                vec!["notes/~draft.org".to_string()],
                vec!["notes/~drafts".to_string()],
            ),
            None,
        )
        .expect("config should load");

        assert_eq!(config.db_path, config_dir.join("../db.sqlite"));
        assert_eq!(config.files, vec![file_path]);
        assert_eq!(config.dirs, vec![dir_path]);
    }

    #[test]
    fn reports_missing_home_directory_for_leading_tilde() {
        let test_dir = TestDir::new("tilde-missing-home");
        let config_path = test_dir.path().join("config.toml");

        let error = Config::from_raw_with_home_dir(
            &config_path,
            raw_config("~/org-files-db.sqlite", Vec::new(), Vec::new()),
            None,
        )
        .expect_err("config should fail");

        match error {
            ConfigError::MissingHomeDirectory { path } => {
                assert_eq!(path, PathBuf::from("~/org-files-db.sqlite"));
            }
            other => panic!("unexpected error: {other}"),
        }
    }

    fn raw_config(db_path: &str, files: Vec<String>, dirs: Vec<String>) -> RawConfig {
        RawConfig {
            db_path: PathBuf::from(db_path),
            files: files.into_iter().map(PathBuf::from).collect(),
            dirs: dirs.into_iter().map(PathBuf::from).collect(),
            recursive: false,
            links: Some(RawLinksConfig::default()),
            todo: Some(RawTodoConfig {
                default_open_keywords: None,
                default_closed_keywords: None,
            }),
            search: Some(RawSearchConfig {
                fts5_enabled: None,
                index_body_text: None,
            }),
        }
    }
}
