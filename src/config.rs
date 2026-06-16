#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct Config {
    pub database_path: Option<std::path::PathBuf>,
    pub input_paths: Vec<std::path::PathBuf>,
}
