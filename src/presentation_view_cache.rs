use std::{
    env,
    error::Error,
    fmt, fs,
    fs::{File, OpenOptions},
    io::{self, Read, Seek, Write},
    os::unix::fs::{OpenOptionsExt, PermissionsExt},
    path::{Path, PathBuf},
    sync::atomic::{AtomicU64, Ordering},
    time::{SystemTime, UNIX_EPOCH},
};

use serde::{Deserialize, Serialize};
use sha2::{Digest, Sha256};

use crate::{
    config::Config,
    hex_encoding::encode_lower,
    presentation::PRESENTATION_VERSION,
    presentation_view::{PresentationViewDefinition, RegisteredPresentationView},
};

pub(crate) const PRESENTATION_VIEW_CACHE_FORMAT_VERSION: u32 = 1;
const CACHE_MAGIC: &[u8; 8] = b"ORGFDBVC";
const CACHE_HEADER_LENGTH_BYTES: usize = 4;
const MAX_CACHE_HEADER_BYTES: usize = 64 * 1024;
const CACHE_SIZE_WARNING_BYTES: u64 = 250 * 1024 * 1024;
const SESSION_DIRECTORY_HASH_BYTES: usize = 16;
const VIEW_FILE_HASH_BYTES: usize = 16;
static TEMP_FILE_COUNTER: AtomicU64 = AtomicU64::new(1);

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
struct PresentationViewCacheHeader {
    cache_format_version: u32,
    presentation_version: u32,
    session_id: String,
    database_id: String,
    generation: i64,
    view_name: String,
    view_revision: u64,
    view_definition_id: String,
    payload_bytes: u64,
}

#[allow(dead_code)]
#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct PresentationViewCachePublishReport {
    pub path: PathBuf,
    pub payload_bytes: u64,
    pub size_warning_emitted: bool,
}

#[derive(Debug, Clone)]
pub(crate) struct PresentationViewCacheStore {
    session_id: String,
    session_dir: PathBuf,
}

impl PresentationViewCacheStore {
    pub(crate) fn for_session(
        config: &Config,
        session_id: String,
    ) -> Result<Self, PresentationViewCachePathError> {
        let root = presentation_view_cache_root(config)?;
        Ok(Self::in_root(root, session_id))
    }

    pub(crate) fn in_root(root: PathBuf, session_id: String) -> Self {
        let session_dir = root
            .join("presentation-views")
            .join(session_directory_name(&session_id));
        Self {
            session_id,
            session_dir,
        }
    }

    pub(crate) fn publish(
        &self,
        view: &RegisteredPresentationView,
        database_id: &str,
        generation: i64,
        payload: &[u8],
    ) -> Result<PresentationViewCachePublishReport, PresentationViewCacheWriteError> {
        self.validate_view_session(view)?;
        prepare_private_cache_directory(&self.session_dir)?;

        let payload_bytes = u64::try_from(payload.len()).map_err(|_| {
            PresentationViewCacheWriteError::PayloadTooLarge {
                bytes: payload.len(),
            }
        })?;
        let header = PresentationViewCacheHeader {
            cache_format_version: PRESENTATION_VIEW_CACHE_FORMAT_VERSION,
            presentation_version: PRESENTATION_VERSION,
            session_id: self.session_id.clone(),
            database_id: database_id.to_string(),
            generation,
            view_name: view.definition.name.clone(),
            view_revision: view.revision,
            view_definition_id: view_definition_identity(&view.definition)?,
            payload_bytes,
        };
        let header_json = serde_json::to_vec(&header)
            .map_err(PresentationViewCacheWriteError::SerializeHeader)?;
        if header_json.len() > MAX_CACHE_HEADER_BYTES {
            return Err(PresentationViewCacheWriteError::HeaderTooLarge {
                bytes: header_json.len(),
                maximum: MAX_CACHE_HEADER_BYTES,
            });
        }

        let current_path = self.current_path(&view.definition.name);
        let temp_path = self.unique_temp_path(&view.definition.name);
        if let Err(source) =
            write_and_publish_cache_file(&temp_path, &current_path, &header_json, payload)
        {
            let _ = fs::remove_file(&temp_path);
            return Err(source);
        }

        let size_warning_emitted = cache_size_warning(payload_bytes);
        if size_warning_emitted {
            eprintln!(
                "warning: presentation view `{}` cache payload is {} bytes, which is at least 250 MiB",
                view.definition.name, payload_bytes
            );
        }

        Ok(PresentationViewCachePublishReport {
            path: current_path,
            payload_bytes,
            size_warning_emitted,
        })
    }

    pub(crate) fn open_valid(
        &self,
        view: &RegisteredPresentationView,
        database_id: &str,
        generation: i64,
    ) -> Result<PresentationViewCacheReader, PresentationViewCacheReadError> {
        self.validate_view_session_for_read(view)?;
        let path = self.current_path(&view.definition.name);
        let mut file =
            File::open(&path).map_err(|source| PresentationViewCacheReadError::Open {
                path: path.clone(),
                source,
            })?;
        let header = read_cache_header(&mut file, &path)?;
        let expected_definition_id = view_definition_identity(&view.definition)
            .map_err(PresentationViewCacheReadError::DefinitionIdentity)?;

        validate_header(
            &header,
            &self.session_id,
            view,
            database_id,
            generation,
            &expected_definition_id,
        )?;

        let payload_offset = file.stream_position().map_err(|source| {
            PresentationViewCacheReadError::InspectPayload {
                path: path.clone(),
                source,
            }
        })?;
        let file_bytes = file
            .metadata()
            .map_err(|source| PresentationViewCacheReadError::InspectPayload {
                path: path.clone(),
                source,
            })?
            .len();
        let actual_payload_bytes = file_bytes.checked_sub(payload_offset).ok_or_else(|| {
            PresentationViewCacheReadError::InvalidEnvelope {
                path: path.clone(),
                message: "payload offset exceeds cache file length".to_string(),
            }
        })?;
        if actual_payload_bytes != header.payload_bytes {
            return Err(PresentationViewCacheReadError::PayloadLengthMismatch {
                path,
                expected: header.payload_bytes,
                actual: actual_payload_bytes,
            });
        }

        Ok(PresentationViewCacheReader {
            file,
            path,
            payload_bytes: header.payload_bytes,
        })
    }

    fn current_path(&self, view_name: &str) -> PathBuf {
        self.session_dir.join(view_cache_file_name(view_name))
    }

    fn unique_temp_path(&self, view_name: &str) -> PathBuf {
        let current_name = view_cache_file_name(view_name);
        let now = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .unwrap_or_default()
            .as_nanos();
        let counter = TEMP_FILE_COUNTER.fetch_add(1, Ordering::Relaxed);
        self.session_dir.join(format!(
            ".{current_name}.{}.{now}.{counter}.tmp",
            std::process::id()
        ))
    }

    fn validate_view_session(
        &self,
        view: &RegisteredPresentationView,
    ) -> Result<(), PresentationViewCacheWriteError> {
        if view.session_id != self.session_id {
            return Err(PresentationViewCacheWriteError::SessionMismatch {
                store_session_id: self.session_id.clone(),
                view_session_id: view.session_id.clone(),
            });
        }
        Ok(())
    }

    fn validate_view_session_for_read(
        &self,
        view: &RegisteredPresentationView,
    ) -> Result<(), PresentationViewCacheReadError> {
        if view.session_id != self.session_id {
            return Err(PresentationViewCacheReadError::ExpectedSessionMismatch {
                store_session_id: self.session_id.clone(),
                view_session_id: view.session_id.clone(),
            });
        }
        Ok(())
    }
}

pub(crate) struct PresentationViewCacheReader {
    file: File,
    path: PathBuf,
    payload_bytes: u64,
}

impl PresentationViewCacheReader {
    #[cfg(test)]
    pub(crate) fn payload_bytes(&self) -> u64 {
        self.payload_bytes
    }

    pub(crate) fn copy_payload_to(
        &mut self,
        writer: &mut impl Write,
    ) -> Result<u64, PresentationViewCacheReadError> {
        let copied = io::copy(&mut self.file, writer).map_err(|source| {
            PresentationViewCacheReadError::ReadPayload {
                path: self.path.clone(),
                source,
            }
        })?;
        if copied != self.payload_bytes {
            return Err(PresentationViewCacheReadError::PayloadLengthMismatch {
                path: self.path.clone(),
                expected: self.payload_bytes,
                actual: copied,
            });
        }
        Ok(copied)
    }
}

fn write_and_publish_cache_file(
    temp_path: &Path,
    current_path: &Path,
    header_json: &[u8],
    payload: &[u8],
) -> Result<(), PresentationViewCacheWriteError> {
    let header_length = u32::try_from(header_json.len()).map_err(|_| {
        PresentationViewCacheWriteError::HeaderTooLarge {
            bytes: header_json.len(),
            maximum: u32::MAX as usize,
        }
    })?;
    let mut file = OpenOptions::new()
        .write(true)
        .create_new(true)
        .mode(0o600)
        .open(temp_path)
        .map_err(|source| PresentationViewCacheWriteError::CreateTemp {
            path: temp_path.to_path_buf(),
            source,
        })?;
    file.write_all(CACHE_MAGIC)
        .and_then(|()| file.write_all(&header_length.to_be_bytes()))
        .and_then(|()| file.write_all(header_json))
        .and_then(|()| file.write_all(payload))
        .and_then(|()| file.flush())
        .map_err(|source| PresentationViewCacheWriteError::WriteTemp {
            path: temp_path.to_path_buf(),
            source,
        })?;
    drop(file);

    fs::rename(temp_path, current_path).map_err(|source| PresentationViewCacheWriteError::Publish {
        from: temp_path.to_path_buf(),
        to: current_path.to_path_buf(),
        source,
    })
}

fn read_cache_header(
    file: &mut File,
    path: &Path,
) -> Result<PresentationViewCacheHeader, PresentationViewCacheReadError> {
    let mut magic = [0_u8; CACHE_MAGIC.len()];
    file.read_exact(&mut magic)
        .map_err(|source| PresentationViewCacheReadError::ReadEnvelope {
            path: path.to_path_buf(),
            source,
        })?;
    if &magic != CACHE_MAGIC {
        return Err(PresentationViewCacheReadError::InvalidEnvelope {
            path: path.to_path_buf(),
            message: "cache magic does not match".to_string(),
        });
    }

    let mut header_length_bytes = [0_u8; CACHE_HEADER_LENGTH_BYTES];
    file.read_exact(&mut header_length_bytes)
        .map_err(|source| PresentationViewCacheReadError::ReadEnvelope {
            path: path.to_path_buf(),
            source,
        })?;
    let header_length = u32::from_be_bytes(header_length_bytes) as usize;
    if header_length > MAX_CACHE_HEADER_BYTES {
        return Err(PresentationViewCacheReadError::InvalidEnvelope {
            path: path.to_path_buf(),
            message: format!(
                "cache header is {header_length} bytes, maximum is {MAX_CACHE_HEADER_BYTES}"
            ),
        });
    }

    let mut header_json = vec![0_u8; header_length];
    file.read_exact(&mut header_json).map_err(|source| {
        PresentationViewCacheReadError::ReadEnvelope {
            path: path.to_path_buf(),
            source,
        }
    })?;
    serde_json::from_slice(&header_json).map_err(|source| {
        PresentationViewCacheReadError::DeserializeHeader {
            path: path.to_path_buf(),
            source,
        }
    })
}

fn validate_header(
    header: &PresentationViewCacheHeader,
    session_id: &str,
    view: &RegisteredPresentationView,
    database_id: &str,
    generation: i64,
    expected_definition_id: &str,
) -> Result<(), PresentationViewCacheReadError> {
    if header.cache_format_version != PRESENTATION_VIEW_CACHE_FORMAT_VERSION {
        return Err(PresentationViewCacheReadError::InvalidCacheFormatVersion {
            expected: PRESENTATION_VIEW_CACHE_FORMAT_VERSION,
            actual: header.cache_format_version,
        });
    }
    if header.presentation_version != PRESENTATION_VERSION {
        return Err(PresentationViewCacheReadError::InvalidPresentationVersion {
            expected: PRESENTATION_VERSION,
            actual: header.presentation_version,
        });
    }
    if header.session_id != session_id {
        return Err(PresentationViewCacheReadError::InvalidSession {
            expected: session_id.to_string(),
            actual: header.session_id.clone(),
        });
    }
    if header.database_id != database_id {
        return Err(PresentationViewCacheReadError::InvalidDatabase {
            expected: database_id.to_string(),
            actual: header.database_id.clone(),
        });
    }
    if header.generation != generation {
        return Err(PresentationViewCacheReadError::InvalidGeneration {
            expected: generation,
            actual: header.generation,
        });
    }
    if header.view_name != view.definition.name {
        return Err(PresentationViewCacheReadError::InvalidViewName {
            expected: view.definition.name.clone(),
            actual: header.view_name.clone(),
        });
    }
    if header.view_revision != view.revision {
        return Err(PresentationViewCacheReadError::InvalidViewRevision {
            expected: view.revision,
            actual: header.view_revision,
        });
    }
    if header.view_definition_id != expected_definition_id {
        return Err(PresentationViewCacheReadError::InvalidViewDefinition {
            expected: expected_definition_id.to_string(),
            actual: header.view_definition_id.clone(),
        });
    }
    Ok(())
}

fn view_definition_identity(
    definition: &PresentationViewDefinition,
) -> Result<String, PresentationViewCacheWriteError> {
    let serialized = serde_json::to_vec(definition)
        .map_err(PresentationViewCacheWriteError::SerializeDefinition)?;
    let mut digest = Sha256::new();
    digest.update(b"orgfdb-presentation-view-definition-v1\0");
    digest.update(serialized);
    Ok(format!("sha256:{}", encode_lower(digest.finalize())))
}

pub(crate) fn presentation_view_cache_root(
    config: &Config,
) -> Result<PathBuf, PresentationViewCachePathError> {
    let xdg_cache_home = absolute_env_path("XDG_CACHE_HOME");
    presentation_view_cache_root_from(
        xdg_cache_home.as_deref(),
        config.discovery.home_dir.as_deref(),
    )
}

fn presentation_view_cache_root_from(
    xdg_cache_home: Option<&Path>,
    home: Option<&Path>,
) -> Result<PathBuf, PresentationViewCachePathError> {
    if let Some(root) = xdg_cache_home {
        return Ok(root.join("orgfdb"));
    }
    if let Some(home) = home {
        return Ok(home.join(".cache").join("orgfdb"));
    }
    Err(PresentationViewCachePathError::MissingCacheDirectory)
}

fn absolute_env_path(name: &'static str) -> Option<PathBuf> {
    env::var_os(name)
        .map(PathBuf::from)
        .filter(|path| path.is_absolute())
}

fn cache_size_warning(payload_bytes: u64) -> bool {
    payload_bytes >= CACHE_SIZE_WARNING_BYTES
}

fn session_directory_name(session_id: &str) -> String {
    digest_name(
        b"orgfdb-presentation-view-cache-session-v1\0",
        session_id.as_bytes(),
        SESSION_DIRECTORY_HASH_BYTES,
        "session",
    )
}

fn view_cache_file_name(view_name: &str) -> String {
    format!(
        "{}.cache",
        digest_name(
            b"orgfdb-presentation-view-cache-name-v1\0",
            view_name.as_bytes(),
            VIEW_FILE_HASH_BYTES,
            "view",
        )
    )
}

fn digest_name(prefix: &[u8], value: &[u8], bytes: usize, label: &str) -> String {
    let mut digest = Sha256::new();
    digest.update(prefix);
    digest.update(value);
    let digest = digest.finalize();
    format!("{label}-{}", encode_lower(&digest[..bytes]))
}

fn prepare_private_cache_directory(
    session_dir: &Path,
) -> Result<(), PresentationViewCacheWriteError> {
    fs::create_dir_all(session_dir).map_err(|source| {
        PresentationViewCacheWriteError::CreateDirectory {
            path: session_dir.to_path_buf(),
            source,
        }
    })?;

    let views_root = session_dir.parent().ok_or_else(|| {
        PresentationViewCacheWriteError::InvalidDirectory(session_dir.to_path_buf())
    })?;
    let cache_root = views_root.parent().ok_or_else(|| {
        PresentationViewCacheWriteError::InvalidDirectory(session_dir.to_path_buf())
    })?;
    for path in [cache_root, views_root, session_dir] {
        fs::set_permissions(path, fs::Permissions::from_mode(0o700)).map_err(|source| {
            PresentationViewCacheWriteError::SetPermissions {
                path: path.to_path_buf(),
                source,
            }
        })?;
    }
    Ok(())
}

#[derive(Debug)]
pub(crate) enum PresentationViewCachePathError {
    MissingCacheDirectory,
}

impl fmt::Display for PresentationViewCachePathError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::MissingCacheDirectory => write!(
                f,
                "cannot determine presentation view cache directory. Set XDG_CACHE_HOME or HOME"
            ),
        }
    }
}

impl Error for PresentationViewCachePathError {}

#[derive(Debug)]
pub(crate) enum PresentationViewCacheWriteError {
    SessionMismatch {
        store_session_id: String,
        view_session_id: String,
    },
    SerializeDefinition(serde_json::Error),
    SerializeHeader(serde_json::Error),
    HeaderTooLarge {
        bytes: usize,
        maximum: usize,
    },
    PayloadTooLarge {
        bytes: usize,
    },
    InvalidDirectory(PathBuf),
    CreateDirectory {
        path: PathBuf,
        source: io::Error,
    },
    SetPermissions {
        path: PathBuf,
        source: io::Error,
    },
    CreateTemp {
        path: PathBuf,
        source: io::Error,
    },
    WriteTemp {
        path: PathBuf,
        source: io::Error,
    },
    Publish {
        from: PathBuf,
        to: PathBuf,
        source: io::Error,
    },
}

impl fmt::Display for PresentationViewCacheWriteError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::SessionMismatch {
                store_session_id,
                view_session_id,
            } => write!(
                f,
                "presentation view cache session `{store_session_id}` does not match view session `{view_session_id}`"
            ),
            Self::SerializeDefinition(source) => write!(
                f,
                "failed to serialize presentation view definition for cache identity: {source}"
            ),
            Self::SerializeHeader(source) => {
                write!(f, "failed to serialize presentation view cache header: {source}")
            }
            Self::HeaderTooLarge { bytes, maximum } => write!(
                f,
                "presentation view cache header is {bytes} bytes, maximum is {maximum}"
            ),
            Self::PayloadTooLarge { bytes } => write!(
                f,
                "presentation view cache payload size {bytes} does not fit the cache format"
            ),
            Self::InvalidDirectory(path) => write!(
                f,
                "presentation view cache directory has no cache root: {}",
                path.display()
            ),
            Self::CreateDirectory { path, source } => write!(
                f,
                "failed to create presentation view cache directory {}: {source}",
                path.display()
            ),
            Self::SetPermissions { path, source } => write!(
                f,
                "failed to set presentation view cache permissions for {}: {source}",
                path.display()
            ),
            Self::CreateTemp { path, source } => write!(
                f,
                "failed to create temporary presentation view cache {}: {source}",
                path.display()
            ),
            Self::WriteTemp { path, source } => write!(
                f,
                "failed to write temporary presentation view cache {}: {source}",
                path.display()
            ),
            Self::Publish { from, to, source } => write!(
                f,
                "failed to publish presentation view cache {} as {}: {source}",
                from.display(),
                to.display()
            ),
        }
    }
}

impl Error for PresentationViewCacheWriteError {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        match self {
            Self::SerializeDefinition(source) | Self::SerializeHeader(source) => Some(source),
            Self::CreateDirectory { source, .. }
            | Self::SetPermissions { source, .. }
            | Self::CreateTemp { source, .. }
            | Self::WriteTemp { source, .. }
            | Self::Publish { source, .. } => Some(source),
            Self::SessionMismatch { .. }
            | Self::HeaderTooLarge { .. }
            | Self::PayloadTooLarge { .. }
            | Self::InvalidDirectory(_) => None,
        }
    }
}

#[derive(Debug)]
pub(crate) enum PresentationViewCacheReadError {
    ExpectedSessionMismatch {
        store_session_id: String,
        view_session_id: String,
    },
    DefinitionIdentity(PresentationViewCacheWriteError),
    Open {
        path: PathBuf,
        source: io::Error,
    },
    ReadEnvelope {
        path: PathBuf,
        source: io::Error,
    },
    DeserializeHeader {
        path: PathBuf,
        source: serde_json::Error,
    },
    InvalidEnvelope {
        path: PathBuf,
        message: String,
    },
    InvalidCacheFormatVersion {
        expected: u32,
        actual: u32,
    },
    InvalidPresentationVersion {
        expected: u32,
        actual: u32,
    },
    InvalidSession {
        expected: String,
        actual: String,
    },
    InvalidDatabase {
        expected: String,
        actual: String,
    },
    InvalidGeneration {
        expected: i64,
        actual: i64,
    },
    InvalidViewName {
        expected: String,
        actual: String,
    },
    InvalidViewRevision {
        expected: u64,
        actual: u64,
    },
    InvalidViewDefinition {
        expected: String,
        actual: String,
    },
    InspectPayload {
        path: PathBuf,
        source: io::Error,
    },
    PayloadLengthMismatch {
        path: PathBuf,
        expected: u64,
        actual: u64,
    },
    ReadPayload {
        path: PathBuf,
        source: io::Error,
    },
}

impl fmt::Display for PresentationViewCacheReadError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::ExpectedSessionMismatch {
                store_session_id,
                view_session_id,
            } => write!(
                f,
                "presentation view cache session `{store_session_id}` does not match view session `{view_session_id}`"
            ),
            Self::DefinitionIdentity(source) => write!(f, "{source}"),
            Self::Open { path, source } => write!(
                f,
                "failed to open presentation view cache {}: {source}",
                path.display()
            ),
            Self::ReadEnvelope { path, source } => write!(
                f,
                "failed to read presentation view cache envelope {}: {source}",
                path.display()
            ),
            Self::DeserializeHeader { path, source } => write!(
                f,
                "failed to parse presentation view cache header {}: {source}",
                path.display()
            ),
            Self::InvalidEnvelope { path, message } => write!(
                f,
                "invalid presentation view cache envelope {}: {message}",
                path.display()
            ),
            Self::InvalidCacheFormatVersion { expected, actual } => write!(
                f,
                "presentation view cache format version {actual} is not valid. Expected {expected}"
            ),
            Self::InvalidPresentationVersion { expected, actual } => write!(
                f,
                "presentation view cache wire version {actual} is not valid. Expected {expected}"
            ),
            Self::InvalidSession { expected, actual } => write!(
                f,
                "presentation view cache session `{actual}` is not valid. Expected `{expected}`"
            ),
            Self::InvalidDatabase { expected, actual } => write!(
                f,
                "presentation view cache database `{actual}` is not valid. Expected `{expected}`"
            ),
            Self::InvalidGeneration { expected, actual } => write!(
                f,
                "presentation view cache generation {actual} is not valid. Expected {expected}"
            ),
            Self::InvalidViewName { expected, actual } => write!(
                f,
                "presentation view cache name `{actual}` is not valid. Expected `{expected}`"
            ),
            Self::InvalidViewRevision { expected, actual } => write!(
                f,
                "presentation view cache revision {actual} is not valid. Expected {expected}"
            ),
            Self::InvalidViewDefinition { expected, actual } => write!(
                f,
                "presentation view cache definition `{actual}` is not valid. Expected `{expected}`"
            ),
            Self::InspectPayload { path, source } => write!(
                f,
                "failed to inspect presentation view cache payload {}: {source}",
                path.display()
            ),
            Self::PayloadLengthMismatch {
                path,
                expected,
                actual,
            } => write!(
                f,
                "presentation view cache payload {} has {actual} bytes. Expected {expected}",
                path.display()
            ),
            Self::ReadPayload { path, source } => write!(
                f,
                "failed to read presentation view cache payload {}: {source}",
                path.display()
            ),
        }
    }
}

impl Error for PresentationViewCacheReadError {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        match self {
            Self::DefinitionIdentity(source) => Some(source),
            Self::Open { source, .. }
            | Self::ReadEnvelope { source, .. }
            | Self::InspectPayload { source, .. }
            | Self::ReadPayload { source, .. } => Some(source),
            Self::DeserializeHeader { source, .. } => Some(source),
            Self::ExpectedSessionMismatch { .. }
            | Self::InvalidEnvelope { .. }
            | Self::InvalidCacheFormatVersion { .. }
            | Self::InvalidPresentationVersion { .. }
            | Self::InvalidSession { .. }
            | Self::InvalidDatabase { .. }
            | Self::InvalidGeneration { .. }
            | Self::InvalidViewName { .. }
            | Self::InvalidViewRevision { .. }
            | Self::InvalidViewDefinition { .. }
            | Self::PayloadLengthMismatch { .. } => None,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::{
        cache_size_warning, presentation_view_cache_root_from, read_cache_header,
        view_definition_identity, PresentationViewCacheHeader, PresentationViewCacheReadError,
        PresentationViewCacheStore, PresentationViewCacheWriteError, CACHE_SIZE_WARNING_BYTES,
        PRESENTATION_VIEW_CACHE_FORMAT_VERSION,
    };
    use crate::{
        presentation::PRESENTATION_VERSION,
        presentation_view::{
            PresentationViewDefinition, PresentationViewOutputMode, RegisteredPresentationView,
        },
    };
    use serde_json::json;
    use std::{
        env, fs,
        fs::File,
        io::{Read, Write},
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
            let path = env::temp_dir().join(format!(
                "org-files-db-presentation-view-cache-{name}-{}-{unique}",
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

    fn view(session_id: &str, revision: u64, width: u64) -> RegisteredPresentationView {
        RegisteredPresentationView {
            session_id: session_id.to_string(),
            revision,
            definition: PresentationViewDefinition {
                name: "agenda".to_string(),
                query: "(headings (todo \"NEXT\"))".to_string(),
                output: PresentationViewOutputMode::Flat,
                includes: Vec::new(),
                query_timezone: Some("Europe/Zurich".to_string()),
                presentation_spec: json!({
                    "columns": [
                        {
                            "name": "title",
                            "width": {"mode": "max", "value": width}
                        }
                    ]
                }),
            },
        }
    }

    fn read_payload(
        store: &PresentationViewCacheStore,
        view: &RegisteredPresentationView,
        database_id: &str,
        generation: i64,
    ) -> Result<Vec<u8>, PresentationViewCacheReadError> {
        let mut reader = store.open_valid(view, database_id, generation)?;
        let mut payload = Vec::new();
        reader.copy_payload_to(&mut payload)?;
        Ok(payload)
    }

    #[test]
    fn xdg_cache_home_precedes_home_fallback() {
        let xdg = PathBuf::from("/tmp/xdg-cache");
        let home = PathBuf::from("/tmp/home");
        let root = presentation_view_cache_root_from(Some(&xdg), Some(&home))
            .expect("cache root should resolve");
        assert_eq!(root, xdg.join("orgfdb"));
    }

    #[test]
    fn home_cache_is_used_without_xdg_cache_home() {
        let home = PathBuf::from("/tmp/home");
        let root = presentation_view_cache_root_from(None, Some(&home))
            .expect("cache root should resolve");
        assert_eq!(root, home.join(".cache").join("orgfdb"));
    }

    #[test]
    fn cache_survives_new_store_handle_in_same_session() {
        let test_dir = TestDir::new("cross-handle");
        let first =
            PresentationViewCacheStore::in_root(test_dir.path.clone(), "session-one".to_string());
        let registered = view("session-one", 4, 40);
        let payload = br#"{"presentation_version":2,"rows":[]}"#;
        first
            .publish(&registered, "database-one", 7, payload)
            .expect("cache publish should succeed");

        let second =
            PresentationViewCacheStore::in_root(test_dir.path.clone(), "session-one".to_string());
        let actual = read_payload(&second, &registered, "database-one", 7)
            .expect("cache should remain readable from another store handle");
        assert_eq!(actual, payload);
    }

    #[test]
    fn replacement_keeps_one_current_cache_file() {
        let test_dir = TestDir::new("replace");
        let store =
            PresentationViewCacheStore::in_root(test_dir.path.clone(), "session-one".to_string());
        let registered = view("session-one", 1, 40);
        store
            .publish(&registered, "database-one", 1, b"first")
            .expect("first cache publish should succeed");
        store
            .publish(&registered, "database-one", 2, b"second")
            .expect("replacement cache publish should succeed");

        let entries = fs::read_dir(&store.session_dir)
            .expect("cache session directory should exist")
            .collect::<Result<Vec<_>, _>>()
            .expect("cache directory should be readable");
        assert_eq!(entries.len(), 1);
        assert_eq!(
            entries[0]
                .path()
                .extension()
                .and_then(|value| value.to_str()),
            Some("cache")
        );
        assert_eq!(
            read_payload(&store, &registered, "database-one", 2)
                .expect("replacement payload should be valid"),
            b"second"
        );
    }

    #[test]
    fn cache_rejects_wrong_session_database_generation_and_view_definition() {
        let test_dir = TestDir::new("validity");
        let store =
            PresentationViewCacheStore::in_root(test_dir.path.clone(), "session-one".to_string());
        let registered = view("session-one", 1, 40);
        store
            .publish(&registered, "database-one", 8, b"payload")
            .expect("cache publish should succeed");

        let cache_root = store
            .session_dir
            .parent()
            .expect("session parent should exist")
            .parent()
            .expect("cache root should exist")
            .to_path_buf();
        let other_session_store =
            PresentationViewCacheStore::in_root(cache_root, "session-two".to_string());
        let other_session_view = view("session-two", 1, 40);
        assert!(matches!(
            other_session_store.open_valid(&other_session_view, "database-one", 8),
            Err(PresentationViewCacheReadError::Open { .. })
        ));
        assert!(matches!(
            store.open_valid(&registered, "database-two", 8),
            Err(PresentationViewCacheReadError::InvalidDatabase { .. })
        ));
        assert!(matches!(
            store.open_valid(&registered, "database-one", 9),
            Err(PresentationViewCacheReadError::InvalidGeneration { .. })
        ));

        let changed_revision = view("session-one", 2, 40);
        assert!(matches!(
            store.open_valid(&changed_revision, "database-one", 8),
            Err(PresentationViewCacheReadError::InvalidViewRevision { .. })
        ));

        let changed_definition = view("session-one", 1, 60);
        assert!(matches!(
            store.open_valid(&changed_definition, "database-one", 8),
            Err(PresentationViewCacheReadError::InvalidViewDefinition { .. })
        ));
    }

    #[test]
    fn cache_rejects_wrong_cache_and_presentation_versions() {
        let test_dir = TestDir::new("versions");
        let store =
            PresentationViewCacheStore::in_root(test_dir.path.clone(), "session-one".to_string());
        fs::create_dir_all(&store.session_dir).expect("session directory should exist");
        let registered = view("session-one", 1, 40);
        let path = store.current_path("agenda");
        let definition_id = view_definition_identity(&registered.definition)
            .expect("view definition identity should serialize");
        let mut header = PresentationViewCacheHeader {
            cache_format_version: PRESENTATION_VIEW_CACHE_FORMAT_VERSION + 1,
            presentation_version: PRESENTATION_VERSION,
            session_id: "session-one".to_string(),
            database_id: "database-one".to_string(),
            generation: 1,
            view_name: "agenda".to_string(),
            view_revision: 1,
            view_definition_id: definition_id.clone(),
            payload_bytes: 7,
        };
        write_test_envelope(&path, &header, b"payload");
        assert!(matches!(
            store.open_valid(&registered, "database-one", 1),
            Err(PresentationViewCacheReadError::InvalidCacheFormatVersion { .. })
        ));

        header.cache_format_version = PRESENTATION_VIEW_CACHE_FORMAT_VERSION;
        header.session_id = "session-two".to_string();
        write_test_envelope(&path, &header, b"payload");
        assert!(matches!(
            store.open_valid(&registered, "database-one", 1),
            Err(PresentationViewCacheReadError::InvalidSession { .. })
        ));

        header.session_id = "session-one".to_string();
        header.presentation_version = PRESENTATION_VERSION + 1;
        write_test_envelope(&path, &header, b"payload");
        assert!(matches!(
            store.open_valid(&registered, "database-one", 1),
            Err(PresentationViewCacheReadError::InvalidPresentationVersion { .. })
        ));
    }

    #[test]
    fn truncated_payload_is_rejected_before_reader_is_returned() {
        let test_dir = TestDir::new("truncated");
        let store =
            PresentationViewCacheStore::in_root(test_dir.path.clone(), "session-one".to_string());
        fs::create_dir_all(&store.session_dir).expect("session directory should exist");
        let registered = view("session-one", 1, 40);
        let path = store.current_path("agenda");
        let header = PresentationViewCacheHeader {
            cache_format_version: PRESENTATION_VIEW_CACHE_FORMAT_VERSION,
            presentation_version: PRESENTATION_VERSION,
            session_id: "session-one".to_string(),
            database_id: "database-one".to_string(),
            generation: 1,
            view_name: "agenda".to_string(),
            view_revision: 1,
            view_definition_id: view_definition_identity(&registered.definition)
                .expect("view definition identity should serialize"),
            payload_bytes: 99,
        };
        write_test_envelope(&path, &header, b"short");

        assert!(matches!(
            store.open_valid(&registered, "database-one", 1),
            Err(PresentationViewCacheReadError::PayloadLengthMismatch { .. })
        ));
    }

    #[test]
    fn temporary_file_never_replaces_current_cache_before_rename() {
        let test_dir = TestDir::new("atomic-boundary");
        let store =
            PresentationViewCacheStore::in_root(test_dir.path.clone(), "session-one".to_string());
        let registered = view("session-one", 1, 40);
        store
            .publish(&registered, "database-one", 1, b"current")
            .expect("current cache publish should succeed");

        let temp_path = store.unique_temp_path("agenda");
        File::create(&temp_path)
            .expect("temporary file should be created")
            .write_all(b"partial")
            .expect("temporary file should be writable");

        assert_eq!(
            read_payload(&store, &registered, "database-one", 1)
                .expect("current cache should remain valid"),
            b"current"
        );
    }

    #[test]
    fn cache_reader_streams_raw_payload_without_reserialization() {
        let test_dir = TestDir::new("stream");
        let store =
            PresentationViewCacheStore::in_root(test_dir.path.clone(), "session-one".to_string());
        let registered = view("session-one", 1, 40);
        let payload = b"{\n  \"presentation_version\": 2, \"rows\": []\n}\n";
        let report = store
            .publish(&registered, "database-one", 1, payload)
            .expect("cache publish should succeed");
        assert_eq!(report.payload_bytes, payload.len() as u64);

        let mut reader = store
            .open_valid(&registered, "database-one", 1)
            .expect("cache should be valid");
        assert_eq!(reader.payload_bytes(), payload.len() as u64);
        let mut actual = Vec::new();
        let copied = reader
            .copy_payload_to(&mut actual)
            .expect("payload copy should succeed");
        assert_eq!(copied, payload.len() as u64);
        assert_eq!(actual, payload);
    }

    #[test]
    fn size_warning_threshold_is_250_mib() {
        assert_eq!(CACHE_SIZE_WARNING_BYTES, 250 * 1024 * 1024);
        assert!(!cache_size_warning(CACHE_SIZE_WARNING_BYTES - 1));
        assert!(cache_size_warning(CACHE_SIZE_WARNING_BYTES));
    }

    fn write_test_envelope(path: &Path, header: &PresentationViewCacheHeader, payload: &[u8]) {
        let header_json = serde_json::to_vec(header).expect("test header should serialize");
        let mut file = File::create(path).expect("test cache should be created");
        file.write_all(super::CACHE_MAGIC)
            .expect("cache magic should be written");
        file.write_all(&(header_json.len() as u32).to_be_bytes())
            .expect("header length should be written");
        file.write_all(&header_json)
            .expect("cache header should be written");
        file.write_all(payload)
            .expect("cache payload should be written");
    }

    #[test]
    fn envelope_header_is_small_and_readable_without_payload_parse() {
        let test_dir = TestDir::new("header");
        let store =
            PresentationViewCacheStore::in_root(test_dir.path.clone(), "session-one".to_string());
        let registered = view("session-one", 1, 40);
        store
            .publish(&registered, "database-one", 1, b"not-json-on-purpose")
            .expect("cache publish should succeed");
        let path = store.current_path("agenda");
        let mut file = File::open(&path).expect("cache should open");
        let header = read_cache_header(&mut file, &path).expect("header should parse");
        assert_eq!(header.payload_bytes, 19);
        let mut payload = Vec::new();
        file.read_to_end(&mut payload).expect("payload should read");
        assert_eq!(payload, b"not-json-on-purpose");
    }

    #[test]
    fn publish_rejects_view_from_another_session() {
        let test_dir = TestDir::new("publish-session");
        let store =
            PresentationViewCacheStore::in_root(test_dir.path.clone(), "session-one".to_string());
        let registered = view("session-two", 1, 40);
        assert!(matches!(
            store.publish(&registered, "database-one", 1, b"payload"),
            Err(PresentationViewCacheWriteError::SessionMismatch { .. })
        ));
    }
}
