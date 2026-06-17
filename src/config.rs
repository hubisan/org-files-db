use std::{
    error::Error,
    fmt, fs,
    path::{Path, PathBuf},
};

use serde::Deserialize;

use crate::parser::{ParseOptions, TodoKeyword, TodoKeywordConfig};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Config {
    pub db_path: PathBuf,
    pub files: Vec<PathBuf>,
    pub dirs: Vec<PathBuf>,
    pub recursive: bool,
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
        let RawConfig {
            db_path: raw_db_path,
            files: raw_files,
            dirs: raw_dirs,
            recursive,
            todo,
            search,
        } = raw;
        let base_dir = path.parent().unwrap_or_else(|| Path::new("."));
        let db_path = resolve_path(base_dir, raw_db_path);
        let files = raw_files
            .into_iter()
            .map(|file| resolve_path(base_dir, file))
            .collect::<Vec<_>>();
        let dirs = raw_dirs
            .into_iter()
            .map(|dir| resolve_path(base_dir, dir))
            .collect::<Vec<_>>();
        let todo = todo.unwrap_or(RawTodoConfig {
            default_open_keywords: None,
            default_closed_keywords: None,
        });
        let search = search.unwrap_or(RawSearchConfig {
            fts5_enabled: None,
            index_body_text: None,
        });

        validate_file_paths(&files)?;
        validate_dir_paths(&dirs)?;

        Ok(Self {
            db_path,
            files,
            dirs,
            recursive,
            todo: TodoConfig {
                default_open_keywords: todo
                    .default_open_keywords
                    .unwrap_or_else(default_open_keyword_specs)
                    .into_iter()
                    .map(parse_todo_keyword_spec)
                    .collect(),
                default_closed_keywords: todo
                    .default_closed_keywords
                    .unwrap_or_else(default_closed_keyword_specs)
                    .into_iter()
                    .map(parse_todo_keyword_spec)
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
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TodoConfig {
    pub default_open_keywords: Vec<TodoKeyword>,
    pub default_closed_keywords: Vec<TodoKeyword>,
}

impl TodoConfig {
    pub fn to_keyword_config(&self) -> TodoKeywordConfig {
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
            todo: TodoConfig::default(),
            search: SearchConfig::default(),
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
        }
    }
}

impl Error for ConfigError {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        match self {
            Self::ReadFile { source, .. } => Some(source),
            Self::ParseToml { source, .. } => Some(source),
            Self::MissingFile { .. } | Self::MissingDirectory { .. } => None,
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
    todo: Option<RawTodoConfig>,
    search: Option<RawSearchConfig>,
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
        .map(parse_todo_keyword_spec)
        .collect()
}

fn default_closed_keywords() -> Vec<TodoKeyword> {
    default_closed_keyword_specs()
        .into_iter()
        .map(parse_todo_keyword_spec)
        .collect()
}

fn parse_todo_keyword_spec(spec: String) -> TodoKeyword {
    if let Some((name, fast_key)) = split_todo_keyword_spec(&spec) {
        TodoKeyword::with_fast_key(name, fast_key)
    } else {
        TodoKeyword::new(spec)
    }
}

fn split_todo_keyword_spec(spec: &str) -> Option<(&str, char)> {
    let open_paren = spec.rfind('(')?;
    let close_paren = spec.rfind(')')?;
    if close_paren != spec.len() - 1 || open_paren + 2 != close_paren {
        return None;
    }

    let fast_key = spec[open_paren + 1..close_paren].chars().next()?;
    Some((&spec[..open_paren], fast_key))
}

// Relative paths in the config are resolved relative to the config file location.
// This keeps config files portable when a project is moved as a directory tree.
fn resolve_path(base_dir: &Path, path: PathBuf) -> PathBuf {
    if path.is_absolute() {
        path
    } else {
        base_dir.join(path)
    }
}

fn validate_file_paths(files: &[PathBuf]) -> Result<(), ConfigError> {
    for path in files {
        if !path.is_file() {
            return Err(ConfigError::MissingFile { path: path.clone() });
        }
    }
    Ok(())
}

fn validate_dir_paths(dirs: &[PathBuf]) -> Result<(), ConfigError> {
    for path in dirs {
        if !path.is_dir() {
            return Err(ConfigError::MissingDirectory { path: path.clone() });
        }
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::{Config, ConfigError};
    use crate::parser::{ParseOptions, TodoKeyword, TodoKeywordConfig};
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
    fn reports_missing_file_errors() {
        let test_dir = TestDir::new("missing-file");
        let config_path = test_dir.path().join("config.toml");

        write_file(
            &config_path,
            r#"
db_path = "db.sqlite"
files = ["missing.org"]
"#,
        );

        let error = Config::load_from_file(&config_path).expect_err("config should fail");

        match error {
            ConfigError::MissingFile { path } => {
                assert_eq!(path, test_dir.path().join("missing.org"));
            }
            other => panic!("unexpected error: {other}"),
        }
    }

    #[test]
    fn reports_missing_directory_errors() {
        let test_dir = TestDir::new("missing-dir");
        let config_path = test_dir.path().join("config.toml");

        write_file(
            &config_path,
            r#"
db_path = "db.sqlite"
dirs = ["missing-dir"]
"#,
        );

        let error = Config::load_from_file(&config_path).expect_err("config should fail");

        match error {
            ConfigError::MissingDirectory { path } => {
                assert_eq!(path, test_dir.path().join("missing-dir"));
            }
            other => panic!("unexpected error: {other}"),
        }
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
            }
        );
    }
}
