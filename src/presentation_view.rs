use std::{
    collections::BTreeMap,
    env,
    error::Error,
    ffi::OsStr,
    fmt, fs,
    io::{self, BufRead, BufReader, Write},
    os::unix::{
        ffi::OsStrExt,
        fs::{FileTypeExt, PermissionsExt},
        net::{UnixListener, UnixStream},
    },
    path::{Path, PathBuf},
    sync::{
        atomic::{AtomicBool, Ordering},
        Arc, Mutex,
    },
    thread::{self, JoinHandle},
    time::{Duration, SystemTime, UNIX_EPOCH},
};

use serde::{Deserialize, Serialize};
use serde_json::Value;
use sha2::{Digest, Sha256};

use crate::{config::Config, hex_encoding::encode_lower};

const CONTROL_PROTOCOL_VERSION: u32 = 1;
const CONTROL_POLL_INTERVAL: Duration = Duration::from_millis(10);
const CONTROL_SOCKET_HASH_BYTES: usize = 16;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub(crate) enum PresentationViewOutputMode {
    Flat,
    Outline,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub(crate) enum PresentationViewInclude {
    Path,
    Properties,
    EffectiveProperties,
    Keywords,
    Links,
    Backlinks,
    Source,
    Target,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub(crate) struct PresentationViewDefinition {
    pub name: String,
    pub query: String,
    pub output: PresentationViewOutputMode,
    pub includes: Vec<PresentationViewInclude>,
    pub query_timezone: Option<String>,
    pub presentation_spec: Value,
}

impl PresentationViewDefinition {
    fn normalize(mut self) -> Result<Self, ViewRegistryError> {
        if self.name.trim().is_empty() {
            return Err(ViewRegistryError::InvalidName);
        }
        self.includes.sort_unstable();
        self.includes.dedup();
        Ok(self)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub(crate) enum PresentationViewRegistrationAction {
    Registered,
    Replaced,
    Unchanged,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub(crate) struct PresentationViewRegistration {
    pub session_id: String,
    pub name: String,
    pub action: PresentationViewRegistrationAction,
    pub revision: u64,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub(crate) struct RegisteredPresentationView {
    pub session_id: String,
    pub revision: u64,
    pub definition: PresentationViewDefinition,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub(crate) struct PresentationViewRemoval {
    pub session_id: String,
    pub name: String,
    pub removed: bool,
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct PresentationViewEntry {
    revision: u64,
    definition: PresentationViewDefinition,
}

#[derive(Debug)]
struct PresentationViewRegistry {
    session_id: String,
    next_revision: u64,
    views: BTreeMap<String, PresentationViewEntry>,
}

#[derive(Clone)]
pub(crate) struct PresentationViewRegistryHandle {
    inner: Arc<Mutex<PresentationViewRegistry>>,
}

impl PresentationViewRegistryHandle {
    pub(crate) fn for_watcher(config: &Config) -> Self {
        Self::new(session_id_for(config))
    }

    fn new(session_id: String) -> Self {
        Self {
            inner: Arc::new(Mutex::new(PresentationViewRegistry::new(session_id))),
        }
    }
}

impl PresentationViewRegistry {
    fn new(session_id: String) -> Self {
        Self {
            session_id,
            next_revision: 1,
            views: BTreeMap::new(),
        }
    }

    fn register(
        &mut self,
        definition: PresentationViewDefinition,
    ) -> Result<PresentationViewRegistration, ViewRegistryError> {
        let definition = definition.normalize()?;
        let name = definition.name.clone();

        let unchanged_revision = self
            .views
            .get(&name)
            .filter(|existing| existing.definition == definition)
            .map(|existing| existing.revision);
        if let Some(revision) = unchanged_revision {
            return Ok(PresentationViewRegistration {
                session_id: self.session_id.clone(),
                name,
                action: PresentationViewRegistrationAction::Unchanged,
                revision,
            });
        }

        let revision = self.next_revision;
        self.next_revision = self.next_revision.saturating_add(1);
        let action = if self.views.contains_key(&name) {
            PresentationViewRegistrationAction::Replaced
        } else {
            PresentationViewRegistrationAction::Registered
        };
        self.views.insert(
            name.clone(),
            PresentationViewEntry {
                revision,
                definition,
            },
        );

        Ok(PresentationViewRegistration {
            session_id: self.session_id.clone(),
            name,
            action,
            revision,
        })
    }

    fn show(&self, name: &str) -> Result<RegisteredPresentationView, ViewRegistryError> {
        let entry = self
            .views
            .get(name)
            .ok_or_else(|| ViewRegistryError::NotFound(name.to_string()))?;
        Ok(RegisteredPresentationView {
            session_id: self.session_id.clone(),
            revision: entry.revision,
            definition: entry.definition.clone(),
        })
    }

    fn remove(&mut self, name: &str) -> PresentationViewRemoval {
        PresentationViewRemoval {
            session_id: self.session_id.clone(),
            name: name.to_string(),
            removed: self.views.remove(name).is_some(),
        }
    }
}

#[derive(Debug)]
enum ViewRegistryError {
    InvalidName,
    NotFound(String),
}

impl fmt::Display for ViewRegistryError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::InvalidName => write!(f, "presentation view name must not be empty"),
            Self::NotFound(name) => write!(f, "presentation view `{name}` is not registered"),
        }
    }
}

#[derive(Debug, Serialize, Deserialize)]
#[serde(tag = "command", rename_all = "snake_case")]
enum ViewControlRequest {
    Register {
        protocol_version: u32,
        definition: Box<PresentationViewDefinition>,
    },
    Show {
        protocol_version: u32,
        name: String,
    },
    Remove {
        protocol_version: u32,
        name: String,
    },
}

impl ViewControlRequest {
    fn register(definition: PresentationViewDefinition) -> Self {
        Self::Register {
            protocol_version: CONTROL_PROTOCOL_VERSION,
            definition: Box::new(definition),
        }
    }

    fn show(name: String) -> Self {
        Self::Show {
            protocol_version: CONTROL_PROTOCOL_VERSION,
            name,
        }
    }

    fn remove(name: String) -> Self {
        Self::Remove {
            protocol_version: CONTROL_PROTOCOL_VERSION,
            name,
        }
    }

    fn protocol_version(&self) -> u32 {
        match self {
            Self::Register {
                protocol_version, ..
            }
            | Self::Show {
                protocol_version, ..
            }
            | Self::Remove {
                protocol_version, ..
            } => *protocol_version,
        }
    }
}

#[derive(Debug, Serialize, Deserialize)]
#[serde(tag = "status", rename_all = "snake_case")]
enum ViewControlResponse {
    Registered {
        registration: PresentationViewRegistration,
    },
    View {
        view: Box<RegisteredPresentationView>,
    },
    Removed {
        removal: PresentationViewRemoval,
    },
    Error {
        code: String,
        message: String,
    },
}

pub(crate) struct PresentationViewControlServer {
    socket_path: PathBuf,
    shutdown: Arc<AtomicBool>,
    thread: Option<JoinHandle<()>>,
}

impl PresentationViewControlServer {
    pub(crate) fn start(
        config: &Config,
        registry: &PresentationViewRegistryHandle,
    ) -> Result<Self, ViewControlServerError> {
        let socket_path = control_socket_path(config)?;
        Self::start_at_path(&socket_path, registry)
    }

    fn start_at_path(
        socket_path: &Path,
        registry: &PresentationViewRegistryHandle,
    ) -> Result<Self, ViewControlServerError> {
        prepare_control_socket(socket_path)?;
        let listener =
            UnixListener::bind(socket_path).map_err(|source| ViewControlServerError::Bind {
                path: socket_path.to_path_buf(),
                source,
            })?;
        if let Err(source) = fs::set_permissions(socket_path, fs::Permissions::from_mode(0o600)) {
            let _ = fs::remove_file(socket_path);
            return Err(ViewControlServerError::SetPermissions {
                path: socket_path.to_path_buf(),
                source,
            });
        }
        if let Err(source) = listener.set_nonblocking(true) {
            let _ = fs::remove_file(socket_path);
            return Err(ViewControlServerError::ConfigureListener(source));
        }

        let shutdown = Arc::new(AtomicBool::new(false));
        let thread_registry = registry.clone();
        let thread_shutdown = Arc::clone(&shutdown);
        let thread = match thread::Builder::new()
            .name("orgfdb-view-control".to_string())
            .spawn(move || run_control_server(listener, thread_registry, thread_shutdown))
        {
            Ok(thread) => thread,
            Err(source) => {
                let _ = fs::remove_file(socket_path);
                return Err(ViewControlServerError::Spawn(source));
            }
        };

        Ok(Self {
            socket_path: socket_path.to_path_buf(),
            shutdown,
            thread: Some(thread),
        })
    }
}

impl Drop for PresentationViewControlServer {
    fn drop(&mut self) {
        self.shutdown.store(true, Ordering::Release);
        if let Some(thread) = self.thread.take() {
            let _ = thread.join();
        }
        let _ = fs::remove_file(&self.socket_path);
    }
}

fn run_control_server(
    listener: UnixListener,
    registry: PresentationViewRegistryHandle,
    shutdown: Arc<AtomicBool>,
) {
    while !shutdown.load(Ordering::Acquire) {
        match listener.accept() {
            Ok((stream, _address)) => handle_control_connection(stream, &registry),
            Err(source) if source.kind() == io::ErrorKind::WouldBlock => {
                thread::sleep(CONTROL_POLL_INTERVAL);
            }
            Err(_) => thread::sleep(CONTROL_POLL_INTERVAL),
        }
    }
}

fn handle_control_connection(stream: UnixStream, registry: &PresentationViewRegistryHandle) {
    let mut reader = BufReader::new(stream);
    let mut request_line = String::new();
    let response = match reader.read_line(&mut request_line) {
        Ok(0) => return,
        Ok(_) => parse_and_apply_request(&request_line, registry),
        Err(source) => ViewControlResponse::Error {
            code: "read_request".to_string(),
            message: format!("failed to read presentation view request: {source}"),
        },
    };

    let stream = reader.get_mut();
    if serde_json::to_writer(&mut *stream, &response).is_ok() {
        let _ = stream.write_all(b"\n");
    }
}

fn parse_and_apply_request(
    request_line: &str,
    registry: &PresentationViewRegistryHandle,
) -> ViewControlResponse {
    let request: ViewControlRequest = match serde_json::from_str(request_line) {
        Ok(request) => request,
        Err(source) => {
            return ViewControlResponse::Error {
                code: "invalid_request".to_string(),
                message: format!("invalid presentation view request: {source}"),
            };
        }
    };

    if request.protocol_version() != CONTROL_PROTOCOL_VERSION {
        return ViewControlResponse::Error {
            code: "unsupported_protocol".to_string(),
            message: format!(
                "presentation view control protocol {} is not supported. Expected {}",
                request.protocol_version(),
                CONTROL_PROTOCOL_VERSION
            ),
        };
    }

    let mut registry = match registry.inner.lock() {
        Ok(registry) => registry,
        Err(_) => {
            return ViewControlResponse::Error {
                code: "registry_unavailable".to_string(),
                message: "presentation view registry is unavailable".to_string(),
            };
        }
    };

    match request {
        ViewControlRequest::Register { definition, .. } => match registry.register(*definition) {
            Ok(registration) => ViewControlResponse::Registered { registration },
            Err(source) => ViewControlResponse::Error {
                code: "invalid_view".to_string(),
                message: source.to_string(),
            },
        },
        ViewControlRequest::Show { name, .. } => match registry.show(&name) {
            Ok(view) => ViewControlResponse::View {
                view: Box::new(view),
            },
            Err(source) => ViewControlResponse::Error {
                code: "view_not_found".to_string(),
                message: source.to_string(),
            },
        },
        ViewControlRequest::Remove { name, .. } => ViewControlResponse::Removed {
            removal: registry.remove(&name),
        },
    }
}

pub(crate) fn register_presentation_view(
    config: &Config,
    definition: PresentationViewDefinition,
) -> Result<PresentationViewRegistration, ViewControlClientError> {
    match send_control_request(config, ViewControlRequest::register(definition))? {
        ViewControlResponse::Registered { registration } => Ok(registration),
        response => Err(unexpected_response("register", response)),
    }
}

pub(crate) fn show_presentation_view(
    config: &Config,
    name: String,
) -> Result<RegisteredPresentationView, ViewControlClientError> {
    match send_control_request(config, ViewControlRequest::show(name))? {
        ViewControlResponse::View { view } => Ok(*view),
        response => Err(unexpected_response("show", response)),
    }
}

pub(crate) fn remove_presentation_view(
    config: &Config,
    name: String,
) -> Result<PresentationViewRemoval, ViewControlClientError> {
    match send_control_request(config, ViewControlRequest::remove(name))? {
        ViewControlResponse::Removed { removal } => Ok(removal),
        response => Err(unexpected_response("remove", response)),
    }
}

fn send_control_request(
    config: &Config,
    request: ViewControlRequest,
) -> Result<ViewControlResponse, ViewControlClientError> {
    send_control_request_to_path(&control_socket_path(config)?, request)
}

fn send_control_request_to_path(
    socket_path: &Path,
    request: ViewControlRequest,
) -> Result<ViewControlResponse, ViewControlClientError> {
    let mut stream =
        UnixStream::connect(socket_path).map_err(|source| ViewControlClientError::Connect {
            path: socket_path.to_path_buf(),
            source,
        })?;
    serde_json::to_writer(&mut stream, &request).map_err(ViewControlClientError::Serialize)?;
    stream
        .write_all(b"\n")
        .map_err(ViewControlClientError::Write)?;

    let mut reader = BufReader::new(stream);
    let mut response_line = String::new();
    if reader
        .read_line(&mut response_line)
        .map_err(ViewControlClientError::Read)?
        == 0
    {
        return Err(ViewControlClientError::EmptyResponse);
    }
    let response: ViewControlResponse =
        serde_json::from_str(&response_line).map_err(ViewControlClientError::Deserialize)?;
    match response {
        ViewControlResponse::Error { code, message } => {
            Err(ViewControlClientError::Remote { code, message })
        }
        response => Ok(response),
    }
}

fn unexpected_response(
    operation: &'static str,
    response: ViewControlResponse,
) -> ViewControlClientError {
    ViewControlClientError::UnexpectedResponse {
        operation,
        response: response_kind(&response),
    }
}

fn response_kind(response: &ViewControlResponse) -> &'static str {
    match response {
        ViewControlResponse::Registered { .. } => "registered",
        ViewControlResponse::View { .. } => "view",
        ViewControlResponse::Removed { .. } => "removed",
        ViewControlResponse::Error { .. } => "error",
    }
}

fn prepare_control_socket(socket_path: &Path) -> Result<(), ViewControlServerError> {
    let parent = socket_path
        .parent()
        .ok_or_else(|| ViewControlServerError::InvalidPath(socket_path.to_path_buf()))?;
    fs::create_dir_all(parent).map_err(|source| ViewControlServerError::CreateDirectory {
        path: parent.to_path_buf(),
        source,
    })?;
    fs::set_permissions(parent, fs::Permissions::from_mode(0o700)).map_err(|source| {
        ViewControlServerError::SetPermissions {
            path: parent.to_path_buf(),
            source,
        }
    })?;

    match fs::symlink_metadata(socket_path) {
        Ok(metadata) if !metadata.file_type().is_socket() => {
            return Err(ViewControlServerError::UnexpectedExistingPath(
                socket_path.to_path_buf(),
            ));
        }
        Ok(_) => match UnixStream::connect(socket_path) {
            Ok(_) => {
                return Err(ViewControlServerError::AlreadyRunning(
                    socket_path.to_path_buf(),
                ));
            }
            Err(source)
                if matches!(
                    source.kind(),
                    io::ErrorKind::ConnectionRefused | io::ErrorKind::NotFound
                ) =>
            {
                fs::remove_file(socket_path).map_err(|source| {
                    ViewControlServerError::RemoveStaleSocket {
                        path: socket_path.to_path_buf(),
                        source,
                    }
                })?;
            }
            Err(source) => {
                return Err(ViewControlServerError::InspectExistingSocket {
                    path: socket_path.to_path_buf(),
                    source,
                });
            }
        },
        Err(source) if source.kind() == io::ErrorKind::NotFound => {}
        Err(source) => {
            return Err(ViewControlServerError::InspectExistingSocket {
                path: socket_path.to_path_buf(),
                source,
            });
        }
    }

    Ok(())
}

fn control_socket_path(config: &Config) -> Result<PathBuf, ViewControlPathError> {
    let root = control_root(config)?;
    Ok(control_socket_path_in_root(&config.db_path, &root))
}

fn control_socket_path_in_root(db_path: &Path, root: &Path) -> PathBuf {
    let mut digest = Sha256::new();
    digest.update(b"orgfdb-presentation-view-control-v1\0");
    digest.update(os_str_bytes(db_path.as_os_str()));
    let digest = digest.finalize();
    let short = encode_lower(&digest[..CONTROL_SOCKET_HASH_BYTES]);
    root.join(format!("view-{short}.sock"))
}

fn control_root(config: &Config) -> Result<PathBuf, ViewControlPathError> {
    if let Some(root) = absolute_env_path("XDG_RUNTIME_DIR") {
        return Ok(root.join("orgfdb"));
    }
    if let Some(root) = absolute_env_path("XDG_CACHE_HOME") {
        return Ok(root.join("orgfdb").join("control"));
    }
    if let Some(home) = config.discovery.home_dir.as_deref() {
        return Ok(home.join(".cache").join("orgfdb").join("control"));
    }
    Err(ViewControlPathError::MissingRuntimeDirectory)
}

fn absolute_env_path(name: &'static str) -> Option<PathBuf> {
    env::var_os(name)
        .map(PathBuf::from)
        .filter(|path| path.is_absolute())
}

fn os_str_bytes(value: &OsStr) -> &[u8] {
    value.as_bytes()
}

fn session_id_for(config: &Config) -> String {
    let now = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap_or_default()
        .as_nanos();
    let mut digest = Sha256::new();
    digest.update(b"orgfdb-presentation-view-session-v1\0");
    digest.update(os_str_bytes(config.db_path.as_os_str()));
    digest.update(std::process::id().to_le_bytes());
    digest.update(now.to_le_bytes());
    format!("sha256:{}", encode_lower(digest.finalize()))
}

#[derive(Debug)]
pub(crate) enum ViewControlPathError {
    MissingRuntimeDirectory,
}

impl fmt::Display for ViewControlPathError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::MissingRuntimeDirectory => write!(
                f,
                "cannot determine presentation view control directory. Set XDG_RUNTIME_DIR, XDG_CACHE_HOME, or HOME"
            ),
        }
    }
}

impl Error for ViewControlPathError {}

#[derive(Debug)]
pub(crate) enum ViewControlServerError {
    Path(ViewControlPathError),
    InvalidPath(PathBuf),
    CreateDirectory { path: PathBuf, source: io::Error },
    SetPermissions { path: PathBuf, source: io::Error },
    UnexpectedExistingPath(PathBuf),
    AlreadyRunning(PathBuf),
    InspectExistingSocket { path: PathBuf, source: io::Error },
    RemoveStaleSocket { path: PathBuf, source: io::Error },
    Bind { path: PathBuf, source: io::Error },
    ConfigureListener(io::Error),
    Spawn(io::Error),
}

impl From<ViewControlPathError> for ViewControlServerError {
    fn from(source: ViewControlPathError) -> Self {
        Self::Path(source)
    }
}

impl fmt::Display for ViewControlServerError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Path(source) => write!(f, "{source}"),
            Self::InvalidPath(path) => write!(
                f,
                "presentation view control socket has no parent directory: {}",
                path.display()
            ),
            Self::CreateDirectory { path, source } => write!(
                f,
                "failed to create presentation view control directory {}: {source}",
                path.display()
            ),
            Self::SetPermissions { path, source } => write!(
                f,
                "failed to set presentation view control permissions for {}: {source}",
                path.display()
            ),
            Self::UnexpectedExistingPath(path) => write!(
                f,
                "presentation view control path exists and is not a socket: {}",
                path.display()
            ),
            Self::AlreadyRunning(path) => write!(
                f,
                "presentation view control socket is already active: {}",
                path.display()
            ),
            Self::InspectExistingSocket { path, source } => write!(
                f,
                "failed to inspect presentation view control socket {}: {source}",
                path.display()
            ),
            Self::RemoveStaleSocket { path, source } => write!(
                f,
                "failed to remove stale presentation view control socket {}: {source}",
                path.display()
            ),
            Self::Bind { path, source } => write!(
                f,
                "failed to bind presentation view control socket {}: {source}",
                path.display()
            ),
            Self::ConfigureListener(source) => write!(
                f,
                "failed to configure presentation view control listener: {source}"
            ),
            Self::Spawn(source) => write!(
                f,
                "failed to start presentation view control thread: {source}"
            ),
        }
    }
}

impl Error for ViewControlServerError {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        match self {
            Self::Path(source) => Some(source),
            Self::CreateDirectory { source, .. }
            | Self::SetPermissions { source, .. }
            | Self::InspectExistingSocket { source, .. }
            | Self::RemoveStaleSocket { source, .. }
            | Self::Bind { source, .. }
            | Self::ConfigureListener(source)
            | Self::Spawn(source) => Some(source),
            Self::InvalidPath(_) | Self::UnexpectedExistingPath(_) | Self::AlreadyRunning(_) => {
                None
            }
        }
    }
}

#[derive(Debug)]
pub(crate) enum ViewControlClientError {
    Path(ViewControlPathError),
    Connect {
        path: PathBuf,
        source: io::Error,
    },
    Serialize(serde_json::Error),
    Write(io::Error),
    Read(io::Error),
    EmptyResponse,
    Deserialize(serde_json::Error),
    Remote {
        code: String,
        message: String,
    },
    UnexpectedResponse {
        operation: &'static str,
        response: &'static str,
    },
}

impl From<ViewControlPathError> for ViewControlClientError {
    fn from(source: ViewControlPathError) -> Self {
        Self::Path(source)
    }
}

impl fmt::Display for ViewControlClientError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Path(source) => write!(f, "{source}"),
            Self::Connect { path, source } => write!(
                f,
                "failed to connect to the active watcher presentation view registry at {}: {source}",
                path.display()
            ),
            Self::Serialize(source) => write!(f, "failed to serialize presentation view request: {source}"),
            Self::Write(source) => write!(f, "failed to write presentation view request: {source}"),
            Self::Read(source) => write!(f, "failed to read presentation view response: {source}"),
            Self::EmptyResponse => write!(f, "presentation view control connection returned no response"),
            Self::Deserialize(source) => write!(f, "failed to parse presentation view response: {source}"),
            Self::Remote { code, message } => write!(f, "presentation view request failed ({code}): {message}"),
            Self::UnexpectedResponse { operation, response } => write!(
                f,
                "presentation view {operation} request returned unexpected `{response}` response"
            ),
        }
    }
}

impl Error for ViewControlClientError {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        match self {
            Self::Path(source) => Some(source),
            Self::Connect { source, .. } | Self::Write(source) | Self::Read(source) => Some(source),
            Self::Serialize(source) | Self::Deserialize(source) => Some(source),
            Self::EmptyResponse | Self::Remote { .. } | Self::UnexpectedResponse { .. } => None,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::{
        control_socket_path_in_root, send_control_request_to_path, PresentationViewControlServer,
        PresentationViewDefinition, PresentationViewInclude, PresentationViewOutputMode,
        PresentationViewRegistrationAction, PresentationViewRegistryHandle, ViewControlClientError,
        ViewControlRequest, ViewControlResponse, ViewControlServerError,
    };
    use serde_json::json;
    use std::{
        fs,
        path::PathBuf,
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
                "org-files-db-presentation-view-{name}-{}-{unique}",
                std::process::id()
            ));
            fs::create_dir_all(&path).expect("test directory should be created");
            Self { path }
        }
    }

    impl Drop for TestDir {
        fn drop(&mut self) {
            let _ = fs::remove_dir_all(&self.path);
        }
    }

    fn definition(name: &str, title_width: u64) -> PresentationViewDefinition {
        PresentationViewDefinition {
            name: name.to_string(),
            query: "(headings (todo \"NEXT\"))".to_string(),
            output: PresentationViewOutputMode::Flat,
            includes: vec![
                PresentationViewInclude::Properties,
                PresentationViewInclude::Path,
                PresentationViewInclude::Path,
            ],
            query_timezone: Some("Europe/Zurich".to_string()),
            presentation_spec: json!({
                "columns": [
                    {
                        "name": "title",
                        "width": {"mode": "max", "value": title_width}
                    }
                ]
            }),
        }
    }

    #[test]
    fn control_socket_path_is_stable_and_short() {
        let root = PathBuf::from("/tmp/orgfdb-test-control");
        let first = control_socket_path_in_root(PathBuf::from("/tmp/data.sqlite").as_path(), &root);
        let second =
            control_socket_path_in_root(PathBuf::from("/tmp/data.sqlite").as_path(), &root);
        let other =
            control_socket_path_in_root(PathBuf::from("/tmp/other.sqlite").as_path(), &root);

        assert_eq!(first, second);
        assert_ne!(first, other);
        assert!(
            first
                .file_name()
                .expect("socket file name should exist")
                .to_string_lossy()
                .len()
                < 64
        );
    }

    #[test]
    fn server_registers_shows_replaces_and_removes_session_views() {
        let test_dir = TestDir::new("round-trip");
        let socket_path = test_dir.path.join("control.sock");
        let _server = PresentationViewControlServer::start_at_path(
            &socket_path,
            &PresentationViewRegistryHandle::new("test-session".to_string()),
        )
        .expect("control server should start");

        let registered = send_control_request_to_path(
            &socket_path,
            ViewControlRequest::register(definition("agenda", 40)),
        )
        .expect("registration should succeed");
        let ViewControlResponse::Registered { registration } = registered else {
            panic!("registration response should have registered status");
        };
        assert_eq!(
            registration.action,
            PresentationViewRegistrationAction::Registered
        );
        assert_eq!(registration.revision, 1);

        let unchanged = send_control_request_to_path(
            &socket_path,
            ViewControlRequest::register(definition("agenda", 40)),
        )
        .expect("identical registration should succeed");
        let ViewControlResponse::Registered { registration } = unchanged else {
            panic!("unchanged response should have registered status");
        };
        assert_eq!(
            registration.action,
            PresentationViewRegistrationAction::Unchanged
        );
        assert_eq!(registration.revision, 1);

        let replaced = send_control_request_to_path(
            &socket_path,
            ViewControlRequest::register(definition("agenda", 60)),
        )
        .expect("replacement should succeed");
        let ViewControlResponse::Registered { registration } = replaced else {
            panic!("replacement response should have registered status");
        };
        assert_eq!(
            registration.action,
            PresentationViewRegistrationAction::Replaced
        );
        assert_eq!(registration.revision, 2);

        let shown = send_control_request_to_path(
            &socket_path,
            ViewControlRequest::show("agenda".to_string()),
        )
        .expect("show should succeed");
        let ViewControlResponse::View { view } = shown else {
            panic!("show response should have view status");
        };
        assert_eq!(view.session_id, "test-session");
        assert_eq!(view.revision, 2);
        assert_eq!(
            view.definition.includes,
            vec![
                PresentationViewInclude::Path,
                PresentationViewInclude::Properties
            ]
        );
        assert_eq!(
            view.definition.presentation_spec["columns"][0]["width"]["value"],
            60
        );

        let removed = send_control_request_to_path(
            &socket_path,
            ViewControlRequest::remove("agenda".to_string()),
        )
        .expect("remove should succeed");
        let ViewControlResponse::Removed { removal } = removed else {
            panic!("remove response should have removed status");
        };
        assert!(removal.removed);

        let error = send_control_request_to_path(
            &socket_path,
            ViewControlRequest::show("agenda".to_string()),
        )
        .expect_err("removed view should not be found");
        assert!(matches!(
            error,
            ViewControlClientError::Remote { ref code, .. } if code == "view_not_found"
        ));
    }

    #[test]
    fn second_control_server_for_the_same_socket_is_rejected() {
        let test_dir = TestDir::new("duplicate-server");
        let socket_path = test_dir.path.join("control.sock");
        let first_registry = PresentationViewRegistryHandle::new("first-session".to_string());
        let _server = PresentationViewControlServer::start_at_path(&socket_path, &first_registry)
            .expect("first control server should start");
        let second_registry = PresentationViewRegistryHandle::new("second-session".to_string());

        let error =
            match PresentationViewControlServer::start_at_path(&socket_path, &second_registry) {
                Ok(_) => panic!("second control server should fail"),
                Err(error) => error,
            };
        assert!(matches!(error, ViewControlServerError::AlreadyRunning(_)));
    }

    #[test]
    fn new_server_session_starts_with_an_empty_registry() {
        let test_dir = TestDir::new("session-reset");
        let socket_path = test_dir.path.join("control.sock");

        {
            let _server = PresentationViewControlServer::start_at_path(
                &socket_path,
                &PresentationViewRegistryHandle::new("first-session".to_string()),
            )
            .expect("first control server should start");
            send_control_request_to_path(
                &socket_path,
                ViewControlRequest::register(definition("agenda", 40)),
            )
            .expect("first session registration should succeed");
        }

        assert!(!socket_path.exists());

        let _server = PresentationViewControlServer::start_at_path(
            &socket_path,
            &PresentationViewRegistryHandle::new("second-session".to_string()),
        )
        .expect("second control server should start");
        let error = send_control_request_to_path(
            &socket_path,
            ViewControlRequest::show("agenda".to_string()),
        )
        .expect_err("new session should not retain old registration");
        assert!(matches!(
            error,
            ViewControlClientError::Remote { ref code, .. } if code == "view_not_found"
        ));
    }

    #[test]
    fn server_rejects_empty_view_names() {
        let test_dir = TestDir::new("empty-name");
        let socket_path = test_dir.path.join("control.sock");
        let _server = PresentationViewControlServer::start_at_path(
            &socket_path,
            &PresentationViewRegistryHandle::new("test-session".to_string()),
        )
        .expect("control server should start");

        let error = send_control_request_to_path(
            &socket_path,
            ViewControlRequest::register(definition("", 40)),
        )
        .expect_err("empty view name should fail");
        assert!(matches!(
            error,
            ViewControlClientError::Remote { ref code, .. } if code == "invalid_view"
        ));
    }
}
