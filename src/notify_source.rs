use std::{
    collections::VecDeque,
    error::Error,
    fmt, fs, io, mem,
    os::unix::fs::MetadataExt,
    path::{Path, PathBuf},
    sync::{
        atomic::{AtomicBool, Ordering},
        mpsc::{self, Receiver, SyncSender, TryRecvError, TrySendError},
        Arc,
    },
};

#[cfg(target_os = "linux")]
use std::{ffi::OsString, os::unix::ffi::OsStringExt};

#[cfg(test)]
use std::time::{Duration, Instant};

use notify::{
    event::ModifyKind, recommended_watcher, Event, EventKind, RecommendedWatcher, RecursiveMode,
    Watcher,
};

use crate::{
    config::{normalize_syntactic_path, Config},
    file_identity::FileIdentity,
    indexer::{CandidatePathNormalizer, IndexerError},
    watcher::{WatcherInput, WatcherPathEventKind, WatcherUncertainty},
};

pub(crate) const NOTIFY_EVENT_BUFFER_CAPACITY: usize = 1024;
const MAX_RETIRED_NOTIFY_BUFFERS: usize = 2;
const MAX_NOTIFY_PATHS_PER_EVENT: usize = 64;
const MAX_NOTIFY_BACKEND_ERROR_PATHS: usize = 16;
const MAX_NOTIFY_BACKEND_MESSAGE_BYTES: usize = 1024;

type NotifyBufferedMessage = NotifySourceMessage;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum NotifyWatchMode {
    NonRecursive,
    Recursive,
}

impl NotifyWatchMode {
    fn as_notify_mode(self) -> RecursiveMode {
        match self {
            Self::NonRecursive => RecursiveMode::NonRecursive,
            Self::Recursive => RecursiveMode::Recursive,
        }
    }
}

#[derive(Debug, Clone)]
struct NotifyFilesystemSnapshot {
    #[cfg(target_os = "linux")]
    mount_table: Vec<u8>,
}

impl NotifyFilesystemSnapshot {
    fn capture() -> Result<Self, NotifyWatcherError> {
        #[cfg(target_os = "linux")]
        {
            let mount_table_path = PathBuf::from("/proc/self/mountinfo");
            let mount_table = fs::read(&mount_table_path).map_err(|source| {
                NotifyWatcherError::InspectMountTable {
                    path: mount_table_path,
                    source,
                }
            })?;
            Ok(Self { mount_table })
        }

        #[cfg(not(target_os = "linux"))]
        {
            Ok(Self {})
        }
    }

    fn guard(&self, path: &Path) -> Result<NotifyFilesystemGuard, NotifyWatcherError> {
        let canonical_path =
            fs::canonicalize(path).map_err(|source| NotifyWatcherError::InspectWatchPath {
                path: path.to_path_buf(),
                source,
            })?;
        let metadata = fs::metadata(&canonical_path).map_err(|source| {
            NotifyWatcherError::InspectWatchPath {
                path: path.to_path_buf(),
                source,
            }
        })?;
        if !metadata.is_dir() {
            return Err(NotifyWatcherError::WatchPathNotDirectory {
                path: path.to_path_buf(),
            });
        }

        Ok(NotifyFilesystemGuard {
            device: metadata.dev(),
            #[cfg(target_os = "linux")]
            mount: parse_linux_mount_identity(&self.mount_table, &canonical_path).ok_or_else(
                || NotifyWatcherError::InspectMountTable {
                    path: canonical_path,
                    source: io::Error::new(
                        io::ErrorKind::NotFound,
                        "no containing mount was found in /proc/self/mountinfo",
                    ),
                },
            )?,
        })
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct NotifyFilesystemGuard {
    device: u64,
    #[cfg(target_os = "linux")]
    mount: LinuxMountIdentity,
}

#[cfg(target_os = "linux")]
#[derive(Debug, Clone, PartialEq, Eq)]
struct LinuxMountIdentity {
    mount_id: u64,
    major_minor: Vec<u8>,
    root: PathBuf,
    mount_point: PathBuf,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct NotifyWatchTarget {
    path: PathBuf,
    mode: NotifyWatchMode,
    filesystem_guard: NotifyFilesystemGuard,
}

impl NotifyWatchTarget {
    pub(crate) fn path(&self) -> &Path {
        &self.path
    }

    pub(crate) fn mode(&self) -> NotifyWatchMode {
        self.mode
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct NotifyBackendFailure {
    message: String,
    paths: Vec<PathBuf>,
    uncertainty: WatcherUncertainty,
}

impl NotifyBackendFailure {
    pub(crate) fn new(message: String, paths: Vec<PathBuf>) -> Self {
        Self {
            message: bounded_message(message),
            paths: paths
                .into_iter()
                .take(MAX_NOTIFY_BACKEND_ERROR_PATHS)
                .collect(),
            uncertainty: WatcherUncertainty::DroppedEvents,
        }
    }

    fn event_buffer_overflow() -> Self {
        Self {
            message: format!(
                "notify event buffer exceeded its capacity of {NOTIFY_EVENT_BUFFER_CAPACITY}; one or more filesystem events were dropped"
            ),
            paths: Vec::new(),
            uncertainty: WatcherUncertainty::Overflow,
        }
    }

    fn refresh_overlap_overflow() -> Self {
        Self {
            message: format!(
                "notify watch refresh exceeded {MAX_RETIRED_NOTIFY_BUFFERS} retained event buffers; one or more queued filesystem events may have been dropped"
            ),
            paths: Vec::new(),
            uncertainty: WatcherUncertainty::DroppedEvents,
        }
    }

    pub(crate) fn message(&self) -> &str {
        &self.message
    }

    pub(crate) fn paths(&self) -> &[PathBuf] {
        &self.paths
    }

    pub(crate) fn recovery_input(&self) -> WatcherInput {
        WatcherInput::Uncertain(self.uncertainty)
    }
}

impl fmt::Display for NotifyBackendFailure {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "notify backend error: {}", self.message())?;
        if !self.paths().is_empty() {
            write!(f, " (paths:")?;
            for path in self.paths() {
                write!(f, " {}", path.display())?;
            }
            write!(f, ")")?;
        }
        write!(f, "; full reconciliation is required")
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum NotifySourceMessage {
    Input(WatcherInput),
    BackendFailure(NotifyBackendFailure),
}

#[derive(Debug)]
pub(crate) enum NotifyWatcherError {
    SourceUniverse {
        source: IndexerError,
    },
    ConfiguredSourceDirectoryMissing {
        path: PathBuf,
    },
    InspectWatchPath {
        path: PathBuf,
        source: io::Error,
    },
    InspectMountTable {
        path: PathBuf,
        source: io::Error,
    },
    WatchPathNotDirectory {
        path: PathBuf,
    },
    WatchFilesystemChanged {
        path: PathBuf,
    },
    MissingSourceParent {
        path: PathBuf,
    },
    CreateBackend {
        source: notify::Error,
    },
    RegisterWatch {
        path: PathBuf,
        mode: NotifyWatchMode,
        source: notify::Error,
    },
    EventChannelDisconnected,
}

impl fmt::Display for NotifyWatcherError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::SourceUniverse { source } => {
                write!(
                    f,
                    "failed to derive watcher registrations from configured sources: {source}"
                )
            }
            Self::ConfiguredSourceDirectoryMissing { path } => {
                write!(
                    f,
                    "configured source directory does not exist: {}; restore the directory or update the configured path, then restart the watcher",
                    path.display()
                )
            }
            Self::InspectWatchPath { path, source } => write!(
                f,
                "failed to inspect notify watch path {}: {}",
                path.display(),
                source
            ),
            Self::InspectMountTable { path, source } => write!(
                f,
                "failed to inspect filesystem mount identity for {}: {}",
                path.display(),
                source
            ),
            Self::WatchPathNotDirectory { path } => write!(
                f,
                "notify watch path is not a directory: {}",
                path.display()
            ),
            Self::WatchFilesystemChanged { path } => write!(
                f,
                "filesystem identity changed below protected notify watch path {}; refusing reconciliation to preserve the last committed database state",
                path.display()
            ),
            Self::MissingSourceParent { path } => write!(
                f,
                "configured source has no parent directory to watch: {}",
                path.display()
            ),
            Self::CreateBackend { source } => {
                write!(f, "failed to create notify filesystem watcher: {source}")
            }
            Self::RegisterWatch { path, mode, source } => write!(
                f,
                "failed to register {:?} notify watch for {}: {}",
                mode,
                path.display(),
                source
            ),
            Self::EventChannelDisconnected => {
                write!(f, "notify event channel disconnected unexpectedly")
            }
        }
    }
}

impl Error for NotifyWatcherError {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        match self {
            Self::SourceUniverse { source } => Some(source),
            Self::InspectWatchPath { source, .. } | Self::InspectMountTable { source, .. } => {
                Some(source)
            }
            Self::CreateBackend { source } | Self::RegisterWatch { source, .. } => Some(source),
            Self::WatchPathNotDirectory { .. }
            | Self::WatchFilesystemChanged { .. }
            | Self::ConfiguredSourceDirectoryMissing { .. }
            | Self::MissingSourceParent { .. }
            | Self::EventChannelDisconnected => None,
        }
    }
}

struct RetiredNotifyBuffer {
    receiver: Receiver<NotifyBufferedMessage>,
    overflowed: Arc<AtomicBool>,
}

pub(crate) struct NotifyWatcherSource {
    config: Config,
    watcher: RecommendedWatcher,
    receiver: Receiver<NotifyBufferedMessage>,
    event_buffer_overflowed: Arc<AtomicBool>,
    retired_buffers: VecDeque<RetiredNotifyBuffer>,
    refresh_overlap_overflowed: bool,
    watch_targets: Vec<NotifyWatchTarget>,
}

impl NotifyWatcherSource {
    pub(crate) fn from_config(config: &Config) -> Result<Self, NotifyWatcherError> {
        let watch_targets = notify_watch_targets(config)?;
        let (sender, receiver, event_buffer_overflowed) = notify_event_buffer();
        let watcher = create_registered_watcher(
            sender,
            Arc::clone(&event_buffer_overflowed),
            &watch_targets,
        )?;

        Ok(Self {
            config: config.clone(),
            watcher,
            receiver,
            event_buffer_overflowed,
            retired_buffers: VecDeque::new(),
            refresh_overlap_overflowed: false,
            watch_targets,
        })
    }

    pub(crate) fn watch_targets(&self) -> &[NotifyWatchTarget] {
        &self.watch_targets
    }

    pub(crate) fn validate_watch_targets(&self) -> Result<(), NotifyWatcherError> {
        validate_watch_targets(&self.watch_targets)
    }

    pub(crate) fn refresh_watches(&mut self) -> Result<(), NotifyWatcherError> {
        let watch_targets = notify_watch_targets(&self.config)?;
        validate_retained_watch_targets(&self.watch_targets, &watch_targets)?;
        let (sender, receiver, event_buffer_overflowed) = notify_event_buffer();
        let watcher = create_registered_watcher(
            sender,
            Arc::clone(&event_buffer_overflowed),
            &watch_targets,
        )?;
        validate_watch_targets(&watch_targets)?;

        let old_watcher = mem::replace(&mut self.watcher, watcher);
        let old_receiver = mem::replace(&mut self.receiver, receiver);
        let old_overflowed =
            mem::replace(&mut self.event_buffer_overflowed, event_buffer_overflowed);
        self.watch_targets = watch_targets;
        drop(old_watcher);
        self.retire_buffer(old_receiver, old_overflowed);
        Ok(())
    }

    fn retire_buffer(
        &mut self,
        receiver: Receiver<NotifyBufferedMessage>,
        overflowed: Arc<AtomicBool>,
    ) {
        if self.retired_buffers.len() == MAX_RETIRED_NOTIFY_BUFFERS {
            self.retired_buffers.pop_front();
            self.refresh_overlap_overflowed = true;
        }
        self.retired_buffers.push_back(RetiredNotifyBuffer {
            receiver,
            overflowed,
        });
    }

    pub(crate) fn try_recv(&mut self) -> Result<Option<NotifySourceMessage>, NotifyWatcherError> {
        if mem::take(&mut self.refresh_overlap_overflowed) {
            return Ok(Some(NotifySourceMessage::BackendFailure(
                NotifyBackendFailure::refresh_overlap_overflow(),
            )));
        }

        let mut index = 0;
        while index < self.retired_buffers.len() {
            if let Some(message) =
                take_event_buffer_overflow(&self.retired_buffers[index].overflowed)
            {
                return Ok(Some(message));
            }
            let result = self.retired_buffers[index].receiver.try_recv();
            match result {
                Ok(message) => return Ok(Some(message)),
                Err(TryRecvError::Empty) => {
                    index += 1;
                }
                Err(TryRecvError::Disconnected) => {
                    self.retired_buffers.remove(index);
                }
            }
        }

        if let Some(message) = take_event_buffer_overflow(&self.event_buffer_overflowed) {
            return Ok(Some(message));
        }
        match self.receiver.try_recv() {
            Ok(message) => Ok(Some(message)),
            Err(TryRecvError::Empty) => Ok(None),
            Err(TryRecvError::Disconnected) => Err(NotifyWatcherError::EventChannelDisconnected),
        }
    }

    #[cfg(test)]
    pub(crate) fn recv_timeout(
        &mut self,
        timeout: Duration,
    ) -> Result<Option<NotifySourceMessage>, NotifyWatcherError> {
        let started = Instant::now();
        loop {
            if let Some(message) = self.try_recv()? {
                return Ok(Some(message));
            }
            let elapsed = started.elapsed();
            let Some(remaining) = timeout.checked_sub(elapsed) else {
                return Ok(None);
            };
            if remaining.is_zero() {
                return Ok(None);
            }
            std::thread::sleep(remaining.min(Duration::from_millis(10)));
        }
    }
}

pub(crate) fn translate_notify_result(
    result: notify::Result<Event>,
) -> Option<NotifySourceMessage> {
    match result {
        Ok(event) => translate_notify_event(event).map(NotifySourceMessage::Input),
        Err(error) => Some(NotifySourceMessage::BackendFailure(notify_backend_failure(
            error,
        ))),
    }
}

fn translate_notify_event(event: Event) -> Option<WatcherInput> {
    if event.need_rescan() {
        return Some(WatcherInput::Uncertain(WatcherUncertainty::Rescan));
    }

    let kind = match event.kind {
        EventKind::Access(_) => return None,
        EventKind::Create(_) => WatcherPathEventKind::Create,
        EventKind::Modify(ModifyKind::Name(_)) => WatcherPathEventKind::Rename,
        EventKind::Modify(ModifyKind::Metadata(_)) => WatcherPathEventKind::Metadata,
        EventKind::Modify(_) => WatcherPathEventKind::Modify,
        EventKind::Remove(_) => WatcherPathEventKind::Remove,
        EventKind::Any | EventKind::Other => WatcherPathEventKind::Other,
    };

    if event.paths.is_empty() || event.paths.len() > MAX_NOTIFY_PATHS_PER_EVENT {
        return Some(WatcherInput::Uncertain(WatcherUncertainty::Other));
    }

    Some(WatcherInput::Paths {
        kind,
        paths: event.paths,
    })
}

fn notify_backend_failure(error: notify::Error) -> NotifyBackendFailure {
    NotifyBackendFailure::new(error.to_string(), error.paths)
}

fn bounded_message(mut message: String) -> String {
    if message.len() <= MAX_NOTIFY_BACKEND_MESSAGE_BYTES {
        return message;
    }
    const ELLIPSIS: &str = "...";
    let max_prefix_bytes = MAX_NOTIFY_BACKEND_MESSAGE_BYTES.saturating_sub(ELLIPSIS.len());
    let truncate_at = message
        .char_indices()
        .map(|(index, _)| index)
        .take_while(|index| *index <= max_prefix_bytes)
        .last()
        .unwrap_or(0);
    message.truncate(truncate_at);
    message.push_str(ELLIPSIS);
    message
}

fn notify_watch_targets(config: &Config) -> Result<Vec<NotifyWatchTarget>, NotifyWatcherError> {
    let normalizer = CandidatePathNormalizer::from_config(config)
        .map_err(|source| map_source_universe_error(config, source))?;
    let filesystem = NotifyFilesystemSnapshot::capture()?;
    let mut targets = Vec::new();

    for configured_dir in &config.dirs {
        let path = normalize_syntactic_path(configured_dir.path.clone());
        insert_watch_target(
            &mut targets,
            make_watch_target(
                &filesystem,
                path.clone(),
                if configured_dir.recursive {
                    NotifyWatchMode::Recursive
                } else {
                    NotifyWatchMode::NonRecursive
                },
            )?,
        );
        insert_parent_watch_target(&filesystem, &mut targets, &path)?;
    }

    for explicit_file in &config.files {
        let logical_file = normalize_syntactic_path(explicit_file.clone());
        insert_parent_watch_target(&filesystem, &mut targets, &logical_file)?;
    }

    for (path, recursive) in normalizer.watcher_directory_hints() {
        insert_watch_target(
            &mut targets,
            make_watch_target(
                &filesystem,
                path,
                if recursive {
                    NotifyWatchMode::Recursive
                } else {
                    NotifyWatchMode::NonRecursive
                },
            )?,
        );
    }

    targets.sort_by_key(|target| FileIdentity::from_canonical_path(target.path()));
    Ok(targets)
}

fn map_source_universe_error(config: &Config, source: IndexerError) -> NotifyWatcherError {
    match source {
        IndexerError::Discover { path, source }
            if source.kind() == io::ErrorKind::NotFound
                && is_configured_directory_path(config, &path) =>
        {
            NotifyWatcherError::ConfiguredSourceDirectoryMissing { path }
        }
        source => NotifyWatcherError::SourceUniverse { source },
    }
}

fn is_configured_directory_path(config: &Config, path: &Path) -> bool {
    let path = normalize_syntactic_path(path.to_path_buf());
    config
        .dirs
        .iter()
        .any(|configured| normalize_syntactic_path(configured.path.clone()) == path)
}

fn insert_parent_watch_target(
    filesystem: &NotifyFilesystemSnapshot,
    targets: &mut Vec<NotifyWatchTarget>,
    source_path: &Path,
) -> Result<(), NotifyWatcherError> {
    let Some(parent) = source_path.parent() else {
        if source_path.is_absolute() {
            return Ok(());
        }
        return Err(NotifyWatcherError::MissingSourceParent {
            path: source_path.to_path_buf(),
        });
    };
    if parent == source_path {
        return Ok(());
    }
    insert_watch_target(
        targets,
        make_watch_target(
            filesystem,
            normalize_syntactic_path(parent.to_path_buf()),
            NotifyWatchMode::NonRecursive,
        )?,
    );
    Ok(())
}

fn make_watch_target(
    filesystem: &NotifyFilesystemSnapshot,
    path: PathBuf,
    mode: NotifyWatchMode,
) -> Result<NotifyWatchTarget, NotifyWatcherError> {
    let filesystem_guard = filesystem.guard(&path)?;
    Ok(NotifyWatchTarget {
        path,
        mode,
        filesystem_guard,
    })
}

fn insert_watch_target(targets: &mut Vec<NotifyWatchTarget>, target: NotifyWatchTarget) {
    if let Some(existing) = targets
        .iter_mut()
        .find(|existing| existing.path == target.path)
    {
        if target.mode == NotifyWatchMode::Recursive {
            existing.mode = NotifyWatchMode::Recursive;
        }
        return;
    }

    if targets.iter().any(|existing| {
        existing.mode == NotifyWatchMode::Recursive
            && target.path.starts_with(&existing.path)
            && target.filesystem_guard == existing.filesystem_guard
    }) {
        return;
    }

    targets.retain(|existing| {
        !(target.mode == NotifyWatchMode::Recursive
            && existing.path.starts_with(&target.path)
            && existing.filesystem_guard == target.filesystem_guard)
    });
    targets.push(target);
}

fn notify_event_buffer() -> (
    SyncSender<NotifyBufferedMessage>,
    Receiver<NotifyBufferedMessage>,
    Arc<AtomicBool>,
) {
    let (sender, receiver) = mpsc::sync_channel(NOTIFY_EVENT_BUFFER_CAPACITY);
    (sender, receiver, Arc::new(AtomicBool::new(false)))
}

fn enqueue_notify_message(
    sender: &SyncSender<NotifyBufferedMessage>,
    event_buffer_overflowed: &AtomicBool,
    message: NotifyBufferedMessage,
) {
    match sender.try_send(message) {
        Ok(()) => {}
        Err(TrySendError::Full(_)) => {
            event_buffer_overflowed.store(true, Ordering::Release);
        }
        Err(TrySendError::Disconnected(_)) => {}
    }
}

fn enqueue_notify_result(
    sender: &SyncSender<NotifyBufferedMessage>,
    event_buffer_overflowed: &AtomicBool,
    result: notify::Result<Event>,
) {
    if let Some(message) = translate_notify_result(result) {
        enqueue_notify_message(sender, event_buffer_overflowed, message);
    }
}

fn take_event_buffer_overflow(event_buffer_overflowed: &AtomicBool) -> Option<NotifySourceMessage> {
    if event_buffer_overflowed.swap(false, Ordering::AcqRel) {
        Some(NotifySourceMessage::BackendFailure(
            NotifyBackendFailure::event_buffer_overflow(),
        ))
    } else {
        None
    }
}

fn create_registered_watcher(
    sender: SyncSender<NotifyBufferedMessage>,
    event_buffer_overflowed: Arc<AtomicBool>,
    targets: &[NotifyWatchTarget],
) -> Result<RecommendedWatcher, NotifyWatcherError> {
    let mut watcher = recommended_watcher(move |result: notify::Result<Event>| {
        enqueue_notify_result(&sender, &event_buffer_overflowed, result);
    })
    .map_err(|source| NotifyWatcherError::CreateBackend { source })?;
    register_watch_targets(targets, |target| {
        watcher.watch(target.path(), target.mode().as_notify_mode())
    })?;
    Ok(watcher)
}

fn validate_watch_targets(targets: &[NotifyWatchTarget]) -> Result<(), NotifyWatcherError> {
    let filesystem = NotifyFilesystemSnapshot::capture()?;
    for target in targets {
        validate_watch_target_with_snapshot(&filesystem, target)?;
    }
    Ok(())
}

fn validate_retained_watch_targets(
    previous: &[NotifyWatchTarget],
    replacement: &[NotifyWatchTarget],
) -> Result<(), NotifyWatcherError> {
    for previous_target in previous {
        let Some(replacement_target) = replacement
            .iter()
            .find(|target| target.path == previous_target.path)
        else {
            continue;
        };
        if replacement_target.filesystem_guard != previous_target.filesystem_guard {
            return Err(NotifyWatcherError::WatchFilesystemChanged {
                path: previous_target.path.clone(),
            });
        }
    }
    Ok(())
}

#[cfg(test)]
fn validate_watch_target(target: &NotifyWatchTarget) -> Result<(), NotifyWatcherError> {
    let filesystem = NotifyFilesystemSnapshot::capture()?;
    validate_watch_target_with_snapshot(&filesystem, target)
}

fn validate_watch_target_with_snapshot(
    filesystem: &NotifyFilesystemSnapshot,
    target: &NotifyWatchTarget,
) -> Result<(), NotifyWatcherError> {
    let current = filesystem.guard(target.path())?;
    if current != target.filesystem_guard {
        return Err(NotifyWatcherError::WatchFilesystemChanged {
            path: target.path.clone(),
        });
    }
    Ok(())
}

fn register_watch_targets(
    targets: &[NotifyWatchTarget],
    mut register: impl FnMut(&NotifyWatchTarget) -> notify::Result<()>,
) -> Result<(), NotifyWatcherError> {
    for target in targets {
        register(target).map_err(|source| NotifyWatcherError::RegisterWatch {
            path: target.path.clone(),
            mode: target.mode,
            source,
        })?;
    }
    Ok(())
}

#[cfg(target_os = "linux")]
fn parse_linux_mount_identity(content: &[u8], path: &Path) -> Option<LinuxMountIdentity> {
    content
        .split(|byte| *byte == b'\n')
        .filter_map(parse_linux_mount_line)
        .filter(|identity| path.starts_with(&identity.mount_point))
        .max_by_key(|identity| identity.mount_point.components().count())
}

#[cfg(target_os = "linux")]
fn parse_linux_mount_line(line: &[u8]) -> Option<LinuxMountIdentity> {
    let separator = line.windows(3).position(|window| window == b" - ")?;
    let fields = line[..separator]
        .split(|byte| (*byte).is_ascii_whitespace())
        .filter(|field| !field.is_empty())
        .collect::<Vec<_>>();
    if fields.len() < 5 {
        return None;
    }
    let mount_id = std::str::from_utf8(fields[0]).ok()?.parse().ok()?;
    Some(LinuxMountIdentity {
        mount_id,
        major_minor: fields[2].to_vec(),
        root: PathBuf::from(OsString::from_vec(decode_mount_field(fields[3]))),
        mount_point: PathBuf::from(OsString::from_vec(decode_mount_field(fields[4]))),
    })
}

#[cfg(target_os = "linux")]
fn decode_mount_field(field: &[u8]) -> Vec<u8> {
    let mut decoded = Vec::with_capacity(field.len());
    let mut index = 0;
    while index < field.len() {
        if field[index] == b'\\' && index + 3 < field.len() {
            let octal = &field[index + 1..index + 4];
            if octal.iter().all(|byte| (b'0'..=b'7').contains(byte)) {
                decoded.push((octal[0] - b'0') * 64 + (octal[1] - b'0') * 8 + (octal[2] - b'0'));
                index += 4;
                continue;
            }
        }
        decoded.push(field[index]);
        index += 1;
    }
    decoded
}

#[cfg(test)]
mod tests {
    use super::{
        enqueue_notify_result, notify_watch_targets, register_watch_targets,
        take_event_buffer_overflow, translate_notify_result, NotifySourceMessage, NotifyWatchMode,
        NotifyWatcherError, NotifyWatcherSource,
    };
    use crate::{
        config::Config,
        watcher::{
            NormalizedWatcherBatch, WatcherBatchNormalizer, WatcherInput, WatcherPathEventKind,
            WatcherUncertainty,
        },
    };
    use notify::{
        event::{
            AccessKind, AccessMode, CreateKind, DataChange, Flag, MetadataKind, ModifyKind,
            RemoveKind, RenameMode,
        },
        Event, EventKind,
    };
    use std::{
        fs,
        path::{Path, PathBuf},
        sync::{atomic::AtomicBool, mpsc, Arc},
        time::{Duration, Instant, SystemTime, UNIX_EPOCH},
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
                "org-files-db-notify-tests-{}-{}-{}",
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
            fs::create_dir_all(parent).expect("parent dir should exist");
        }
        fs::write(path, content).expect("file should be written");
    }

    fn load_config(test_dir: &TestDir, body: &str) -> Config {
        let config_path = test_dir.path().join("config.toml");
        write_file(&config_path, body);
        Config::load_from_file(config_path).expect("config should load")
    }

    fn input(message: Option<NotifySourceMessage>) -> WatcherInput {
        match message.expect("translated message") {
            NotifySourceMessage::Input(input) => input,
            NotifySourceMessage::BackendFailure(error) => {
                panic!("expected watcher input, got {error}")
            }
        }
    }

    #[test]
    fn translates_create_modify_metadata_remove_and_other_events() {
        let path = PathBuf::from("/tmp/note.org");
        let cases = [
            (
                EventKind::Create(CreateKind::File),
                WatcherPathEventKind::Create,
            ),
            (
                EventKind::Modify(ModifyKind::Data(DataChange::Content)),
                WatcherPathEventKind::Modify,
            ),
            (
                EventKind::Modify(ModifyKind::Metadata(MetadataKind::WriteTime)),
                WatcherPathEventKind::Metadata,
            ),
            (
                EventKind::Remove(RemoveKind::File),
                WatcherPathEventKind::Remove,
            ),
            (EventKind::Other, WatcherPathEventKind::Other),
        ];

        for (event_kind, expected_kind) in cases {
            let event = Event::new(event_kind).add_path(path.clone());
            assert_eq!(
                input(translate_notify_result(Ok(event))),
                WatcherInput::Paths {
                    kind: expected_kind,
                    paths: vec![path.clone()],
                }
            );
        }
    }

    #[test]
    fn translates_complete_rename_with_both_paths_in_backend_order() {
        let old = PathBuf::from("/tmp/old.org");
        let new = PathBuf::from("/tmp/new.org");
        let event = Event::new(EventKind::Modify(ModifyKind::Name(RenameMode::Both)))
            .add_path(old.clone())
            .add_path(new.clone());

        assert_eq!(
            input(translate_notify_result(Ok(event))),
            WatcherInput::Paths {
                kind: WatcherPathEventKind::Rename,
                paths: vec![old, new],
            }
        );
    }

    #[test]
    fn translates_incomplete_rename_for_normalizer_reconciliation() {
        let old = PathBuf::from("/tmp/old.org");
        let event =
            Event::new(EventKind::Modify(ModifyKind::Name(RenameMode::From))).add_path(old.clone());

        assert_eq!(
            input(translate_notify_result(Ok(event))),
            WatcherInput::Paths {
                kind: WatcherPathEventKind::Rename,
                paths: vec![old],
            }
        );
    }

    #[test]
    fn rescan_flag_supersedes_event_paths() {
        let event = Event::new(EventKind::Modify(ModifyKind::Any))
            .add_path(PathBuf::from("/tmp/note.org"))
            .set_flag(Flag::Rescan);

        assert_eq!(
            input(translate_notify_result(Ok(event))),
            WatcherInput::Uncertain(WatcherUncertainty::Rescan)
        );
    }

    #[test]
    fn ignores_non_mutating_access_events() {
        let event = Event::new(EventKind::Access(AccessKind::Open(AccessMode::Any)))
            .add_path(PathBuf::from("/tmp/note.org"));

        assert_eq!(translate_notify_result(Ok(event)), None);
    }

    #[test]
    fn pathless_unknown_event_requests_reconciliation() {
        assert_eq!(
            input(translate_notify_result(Ok(Event::new(EventKind::Any)))),
            WatcherInput::Uncertain(WatcherUncertainty::Other)
        );
    }

    #[test]
    fn backend_error_keeps_context_and_requests_reconciliation() {
        let path = PathBuf::from("/tmp/notes");
        let error = notify::Error::generic("backend queue overflow").add_path(path.clone());
        let Some(NotifySourceMessage::BackendFailure(failure)) =
            translate_notify_result(Err(error))
        else {
            panic!("expected backend failure");
        };

        assert!(failure.message().contains("backend queue overflow"));
        assert_eq!(failure.paths(), &[path]);
        assert_eq!(
            failure.recovery_input(),
            WatcherInput::Uncertain(WatcherUncertainty::DroppedEvents)
        );
        assert!(failure.to_string().contains("full reconciliation"));
    }

    #[test]
    fn bounded_event_buffer_reports_overflow_and_requests_reconciliation() {
        let (sender, receiver) = mpsc::sync_channel(1);
        let overflowed = AtomicBool::new(false);

        enqueue_notify_result(
            &sender,
            &overflowed,
            Ok(Event::new(EventKind::Any).add_path(PathBuf::from("/tmp/first.org"))),
        );
        enqueue_notify_result(
            &sender,
            &overflowed,
            Ok(Event::new(EventKind::Any).add_path(PathBuf::from("/tmp/second.org"))),
        );

        let Some(NotifySourceMessage::BackendFailure(failure)) =
            take_event_buffer_overflow(&overflowed)
        else {
            panic!("expected bounded event-buffer overflow");
        };
        assert!(failure.message().contains("event buffer"));
        assert_eq!(
            failure.recovery_input(),
            WatcherInput::Uncertain(WatcherUncertainty::Overflow)
        );
        assert!(failure.to_string().contains("full reconciliation"));
        assert!(receiver.try_recv().is_ok());
        assert!(take_event_buffer_overflow(&overflowed).is_none());
    }

    #[test]
    fn event_path_payload_is_bounded_before_buffering() {
        let mut event = Event::new(EventKind::Modify(ModifyKind::Any));
        for index in 0..=super::MAX_NOTIFY_PATHS_PER_EVENT {
            event = event.add_path(PathBuf::from(format!("/tmp/note-{index}.org")));
        }

        assert_eq!(
            input(translate_notify_result(Ok(event))),
            WatcherInput::Uncertain(WatcherUncertainty::Other)
        );
    }

    #[test]
    fn backend_failure_context_is_bounded() {
        let message = "x".repeat(super::MAX_NOTIFY_BACKEND_MESSAGE_BYTES * 2);
        let paths = (0..(super::MAX_NOTIFY_BACKEND_ERROR_PATHS + 5))
            .map(|index| PathBuf::from(format!("/tmp/path-{index}")))
            .collect();
        let failure = super::NotifyBackendFailure::new(message, paths);

        assert!(failure.message().len() <= super::MAX_NOTIFY_BACKEND_MESSAGE_BYTES);
        assert_eq!(failure.paths().len(), super::MAX_NOTIFY_BACKEND_ERROR_PATHS);
    }

    #[test]
    fn filesystem_root_needs_no_parent_watch() {
        let filesystem = super::NotifyFilesystemSnapshot::capture().expect("filesystem snapshot");
        let mut targets = Vec::new();

        super::insert_parent_watch_target(&filesystem, &mut targets, Path::new("/"))
            .expect("filesystem root should not require a parent watch");

        assert!(targets.is_empty());
    }

    #[test]
    fn plans_recursive_roots_and_explicit_file_parents_without_duplicate_watches() {
        let test_dir = TestDir::new("targets");
        fs::create_dir_all(test_dir.path().join("notes/nested")).expect("notes root");
        write_file(
            &test_dir.path().join("notes/nested/explicit.org"),
            "* Explicit\n",
        );
        fs::create_dir_all(test_dir.path().join("outside")).expect("outside root");
        write_file(
            &test_dir.path().join("outside/standalone.org"),
            "* Standalone\n",
        );
        let config = load_config(
            &test_dir,
            "db_path = \"db.sqlite\"\nfiles = [\"notes/nested/explicit.org\", \"outside/standalone.org\"]\n[[dirs]]\npath = \"notes\"\nrecursive = true\n[search]\nfts5_enabled = false\n",
        );

        let targets = notify_watch_targets(&config).expect("watch targets");
        assert_eq!(targets.len(), 3);
        assert!(targets.iter().any(|target| {
            target.path() == test_dir.path() && target.mode() == NotifyWatchMode::NonRecursive
        }));
        assert!(targets.iter().any(|target| {
            target.path() == test_dir.path().join("notes")
                && target.mode() == NotifyWatchMode::Recursive
        }));
        assert!(targets.iter().any(|target| {
            target.path() == test_dir.path().join("outside")
                && target.mode() == NotifyWatchMode::NonRecursive
        }));
    }

    #[test]
    fn recursive_target_replaces_existing_nested_non_recursive_target() {
        let test_dir = TestDir::new("target-order");
        fs::create_dir_all(test_dir.path().join("notes/nested")).expect("nested root");
        write_file(
            &test_dir.path().join("notes/nested/explicit.org"),
            "* Explicit\n",
        );
        let config = load_config(
            &test_dir,
            "db_path = \"db.sqlite\"\nfiles = [\"notes/nested/explicit.org\"]\n[[dirs]]\npath = \"notes\"\nrecursive = true\n[search]\nfts5_enabled = false\n",
        );

        let targets = notify_watch_targets(&config).expect("watch targets");
        assert_eq!(targets.len(), 2);
        assert!(targets.iter().any(|target| {
            target.path() == test_dir.path().join("notes")
                && target.mode() == NotifyWatchMode::Recursive
        }));
        assert!(targets.iter().any(|target| {
            target.path() == test_dir.path() && target.mode() == NotifyWatchMode::NonRecursive
        }));
    }

    #[test]
    fn recursive_directory_symlink_adds_a_watch_for_the_canonical_target() {
        use std::os::unix::fs::symlink;

        let test_dir = TestDir::new("directory-symlink-target");
        fs::create_dir_all(test_dir.path().join("notes")).expect("notes root");
        fs::create_dir_all(test_dir.path().join("outside")).expect("outside root");
        write_file(&test_dir.path().join("outside/target.org"), "* Target\n");
        symlink(
            test_dir.path().join("outside"),
            test_dir.path().join("notes/linked"),
        )
        .expect("directory symlink should be created");
        let config = load_config(
            &test_dir,
            "db_path = \"db.sqlite\"\n[[dirs]]\npath = \"notes\"\nrecursive = true\n[search]\nfts5_enabled = false\n",
        );

        let targets = notify_watch_targets(&config).expect("watch targets");

        assert!(targets.iter().any(|target| {
            target.path() == test_dir.path().join("outside")
                && target.mode() == NotifyWatchMode::Recursive
        }));
    }

    #[test]
    fn explicit_file_symlink_adds_a_watch_for_the_canonical_parent() {
        use std::os::unix::fs::symlink;

        let test_dir = TestDir::new("explicit-symlink-target");
        fs::create_dir_all(test_dir.path().join("links")).expect("links directory");
        fs::create_dir_all(test_dir.path().join("outside")).expect("outside directory");
        let target = test_dir.path().join("outside/target.org");
        let alias = test_dir.path().join("links/alias.org");
        write_file(&target, "* Target\n");
        symlink(&target, &alias).expect("file symlink should be created");
        let config = load_config(
            &test_dir,
            "db_path = \"db.sqlite\"\nfiles = [\"links/alias.org\"]\n[search]\nfts5_enabled = false\n",
        );

        let targets = notify_watch_targets(&config).expect("watch targets");

        assert!(targets.iter().any(|watch| {
            watch.path() == test_dir.path().join("outside")
                && watch.mode() == NotifyWatchMode::NonRecursive
        }));
    }

    #[test]
    fn refresh_drops_an_obsolete_external_symlink_target() {
        use std::os::unix::fs::symlink;

        let test_dir = TestDir::new("obsolete-symlink-target");
        let notes = test_dir.path().join("notes");
        let first_parent = test_dir.path().join("first-parent");
        let second_parent = test_dir.path().join("second-parent");
        let alias = notes.join("alias.org");
        let first = first_parent.join("first.org");
        let second = second_parent.join("second.org");
        fs::create_dir_all(&notes).expect("notes directory");
        write_file(&first, "* First\n");
        write_file(&second, "* Second\n");
        symlink(&first, &alias).expect("initial symlink");
        let config = load_config(
            &test_dir,
            "db_path = \"db.sqlite\"\n[[dirs]]\npath = \"notes\"\nrecursive = true\n[search]\nfts5_enabled = false\n",
        );
        let mut source = NotifyWatcherSource::from_config(&config).expect("notify source");
        assert!(source
            .watch_targets()
            .iter()
            .any(|target| target.path() == first_parent));

        fs::remove_file(&alias).expect("old symlink removal");
        symlink(&second, &alias).expect("replacement symlink");
        fs::remove_dir_all(&first_parent).expect("obsolete target removal");
        source
            .refresh_watches()
            .expect("obsolete external target must not block refresh");

        assert!(!source
            .watch_targets()
            .iter()
            .any(|target| target.path() == first_parent));
        assert!(source
            .watch_targets()
            .iter()
            .any(|target| target.path() == second_parent));
    }

    #[test]
    fn missing_configured_directory_reports_actionable_error() {
        let test_dir = TestDir::new("missing-root");
        let config = load_config(
            &test_dir,
            "db_path = \"db.sqlite\"\n[[dirs]]\npath = \"missing\"\nrecursive = true\n[search]\nfts5_enabled = false\n",
        );

        let error = notify_watch_targets(&config).expect_err("missing root should fail");
        assert!(matches!(
            &error,
            NotifyWatcherError::ConfiguredSourceDirectoryMissing { path }
                if path == &test_dir.path().join("missing")
        ));
        assert_eq!(
            error.to_string(),
            format!(
                "configured source directory does not exist: {}; restore the directory or update the configured path, then restart the watcher",
                test_dir.path().join("missing").display()
            )
        );
    }

    #[test]
    fn watch_registration_failure_reports_target_and_mode() {
        let test_dir = TestDir::new("registration-error");
        fs::create_dir_all(test_dir.path().join("notes")).expect("notes root");
        let config = load_config(
            &test_dir,
            "db_path = \"db.sqlite\"\n[[dirs]]\npath = \"notes\"\nrecursive = true\n[search]\nfts5_enabled = false\n",
        );
        let targets = notify_watch_targets(&config).expect("watch targets");

        let notes = test_dir.path().join("notes");
        let error = register_watch_targets(&targets, |target| {
            if target.path() == notes {
                Err(notify::Error::generic("planned registration failure"))
            } else {
                Ok(())
            }
        })
        .expect_err("registration should fail");

        assert!(matches!(
            error,
            NotifyWatcherError::RegisterWatch {
                ref path,
                mode: NotifyWatchMode::Recursive,
                ..
            } if path == &test_dir.path().join("notes")
        ));
    }

    #[test]
    fn translated_unrelated_and_sqlite_events_are_filtered_by_candidate_normalizer() {
        let test_dir = TestDir::new("candidate-filter");
        fs::create_dir_all(test_dir.path().join("notes")).expect("notes root");
        let config = load_config(
            &test_dir,
            "db_path = \"notes/db.sqlite\"\n[[dirs]]\npath = \"notes\"\nrecursive = true\n[search]\nfts5_enabled = false\n",
        );
        let normalizer = WatcherBatchNormalizer::from_config(&config).expect("normalizer");
        let inputs = [
            input(translate_notify_result(Ok(Event::new(EventKind::Modify(
                ModifyKind::Any,
            ))
            .add_path(test_dir.path().join("notes/db.sqlite"))))),
            input(translate_notify_result(Ok(Event::new(EventKind::Modify(
                ModifyKind::Any,
            ))
            .add_path(test_dir.path().join("notes/db.sqlite-wal"))))),
            input(translate_notify_result(Ok(Event::new(EventKind::Modify(
                ModifyKind::Any,
            ))
            .add_path(test_dir.path().join("outside.org"))))),
        ];

        assert_eq!(
            normalizer.normalize(inputs),
            NormalizedWatcherBatch::Candidates(Vec::new())
        );
    }

    #[test]
    fn watch_target_validation_reports_a_removed_root() {
        let test_dir = TestDir::new("removed-watch-root");
        let root = test_dir.path().join("notes");
        fs::create_dir_all(&root).expect("notes root");
        let config = load_config(
            &test_dir,
            "db_path = \"db.sqlite\"\n[[dirs]]\npath = \"notes\"\nrecursive = true\n[search]\nfts5_enabled = false\n",
        );
        let source = NotifyWatcherSource::from_config(&config).expect("notify source");
        fs::remove_dir_all(&root).expect("watch root should be removed");

        let error = source
            .validate_watch_targets()
            .expect_err("removed root should fail validation");

        assert!(matches!(
            error,
            NotifyWatcherError::InspectWatchPath { path, .. } if path == root
        ));
    }

    #[test]
    fn refresh_retains_queued_messages_from_the_previous_buffer() {
        let test_dir = TestDir::new("retired-buffer");
        fs::create_dir_all(test_dir.path().join("notes")).expect("notes root");
        let config = load_config(
            &test_dir,
            "db_path = \"db.sqlite\"\n[[dirs]]\npath = \"notes\"\nrecursive = true\n[search]\nfts5_enabled = false\n",
        );
        let mut source = NotifyWatcherSource::from_config(&config).expect("notify source");
        let (sender, receiver) = mpsc::sync_channel(1);
        sender
            .send(NotifySourceMessage::Input(WatcherInput::Uncertain(
                WatcherUncertainty::Rescan,
            )))
            .expect("message should queue");
        source.retire_buffer(receiver, Arc::new(AtomicBool::new(false)));
        drop(sender);

        assert_eq!(
            source.try_recv().expect("retired buffer should read"),
            Some(NotifySourceMessage::Input(WatcherInput::Uncertain(
                WatcherUncertainty::Rescan
            )))
        );
    }

    #[test]
    fn empty_retired_buffer_does_not_hide_later_buffered_messages() {
        let test_dir = TestDir::new("retired-buffer-order");
        fs::create_dir_all(test_dir.path().join("notes")).expect("notes root");
        let config = load_config(
            &test_dir,
            "db_path = \"db.sqlite\"\n[[dirs]]\npath = \"notes\"\nrecursive = true\n[search]\nfts5_enabled = false\n",
        );
        let mut source = NotifyWatcherSource::from_config(&config).expect("notify source");
        let (empty_sender, empty_receiver) = mpsc::sync_channel(1);
        let (message_sender, message_receiver) = mpsc::sync_channel(1);
        message_sender
            .send(NotifySourceMessage::Input(WatcherInput::Uncertain(
                WatcherUncertainty::Rescan,
            )))
            .expect("message should queue");
        source.retire_buffer(empty_receiver, Arc::new(AtomicBool::new(false)));
        source.retire_buffer(message_receiver, Arc::new(AtomicBool::new(false)));

        assert_eq!(
            source.try_recv().expect("later retired buffer should read"),
            Some(NotifySourceMessage::Input(WatcherInput::Uncertain(
                WatcherUncertainty::Rescan
            )))
        );
        drop(empty_sender);
        drop(message_sender);
    }

    #[test]
    fn refresh_overlap_buffers_remain_bounded_and_report_uncertainty() {
        let test_dir = TestDir::new("retired-buffer-bound");
        fs::create_dir_all(test_dir.path().join("notes")).expect("notes root");
        let config = load_config(
            &test_dir,
            "db_path = \"db.sqlite\"\n[[dirs]]\npath = \"notes\"\nrecursive = true\n[search]\nfts5_enabled = false\n",
        );
        let mut source = NotifyWatcherSource::from_config(&config).expect("notify source");
        let mut senders = Vec::new();
        for _ in 0..=super::MAX_RETIRED_NOTIFY_BUFFERS {
            let (sender, receiver) = mpsc::sync_channel(1);
            senders.push(sender);
            source.retire_buffer(receiver, Arc::new(AtomicBool::new(false)));
        }

        assert_eq!(
            source.retired_buffers.len(),
            super::MAX_RETIRED_NOTIFY_BUFFERS
        );
        let Some(NotifySourceMessage::BackendFailure(failure)) =
            source.try_recv().expect("overflow should be reported")
        else {
            panic!("expected refresh-overlap uncertainty");
        };
        assert_eq!(
            failure.recovery_input(),
            WatcherInput::Uncertain(WatcherUncertainty::DroppedEvents)
        );
        drop(senders);
    }

    #[test]
    fn changed_filesystem_guard_is_rejected_before_reconciliation() {
        let test_dir = TestDir::new("filesystem-guard");
        fs::create_dir_all(test_dir.path().join("notes")).expect("notes root");
        let config = load_config(
            &test_dir,
            "db_path = \"db.sqlite\"\n[[dirs]]\npath = \"notes\"\nrecursive = true\n[search]\nfts5_enabled = false\n",
        );
        let mut target = notify_watch_targets(&config)
            .expect("watch targets")
            .into_iter()
            .find(|target| target.path() == test_dir.path().join("notes"))
            .expect("configured root target");
        target.filesystem_guard.device = target.filesystem_guard.device.wrapping_add(1);

        let error = super::validate_watch_target(&target)
            .expect_err("changed filesystem identity should be rejected");

        assert!(matches!(
            error,
            NotifyWatcherError::WatchFilesystemChanged { path }
                if path == test_dir.path().join("notes")
        ));
    }

    #[cfg(target_os = "linux")]
    #[test]
    fn linux_mountinfo_parser_uses_the_deepest_mount_and_decodes_paths() {
        let content = b"10 1 8:1 / / rw - ext4 /dev/root rw\n11 10 8:2 /sub\\040root /tmp/mounted\\040notes rw - ext4 /dev/data rw\n";
        let path = Path::new("/tmp/mounted notes/project");

        let identity =
            super::parse_linux_mount_identity(content, path).expect("deepest mount should parse");

        assert_eq!(identity.mount_id, 11);
        assert_eq!(identity.major_minor, b"8:2");
        assert_eq!(identity.root, PathBuf::from("/sub root"));
        assert_eq!(identity.mount_point, PathBuf::from("/tmp/mounted notes"));
    }

    #[test]
    fn recommended_backend_observes_created_file_below_recursive_root() {
        let test_dir = TestDir::new("real-backend");
        fs::create_dir_all(test_dir.path().join("notes")).expect("notes root");
        let config = load_config(
            &test_dir,
            "db_path = \"db.sqlite\"\n[[dirs]]\npath = \"notes\"\nrecursive = true\n[search]\nfts5_enabled = false\n",
        );
        let mut source = NotifyWatcherSource::from_config(&config).expect("notify source");
        assert_eq!(source.watch_targets().len(), 2);
        source
            .refresh_watches()
            .expect("notify registrations should refresh");
        let normalizer = WatcherBatchNormalizer::from_config(&config).expect("normalizer");
        let note = test_dir.path().join("notes/new.org");
        write_file(&note, "* New\n");
        let canonical_note = fs::canonicalize(&note).expect("canonical note");
        let deadline = Instant::now() + Duration::from_secs(5);

        while Instant::now() < deadline {
            let Some(message) = source
                .recv_timeout(Duration::from_millis(200))
                .expect("receive notify message")
            else {
                continue;
            };

            match message {
                NotifySourceMessage::Input(input) => match normalizer.normalize([input]) {
                    NormalizedWatcherBatch::Candidates(paths) => {
                        if paths.contains(&canonical_note) {
                            return;
                        }
                    }
                    NormalizedWatcherBatch::Reconcile => return,
                },
                NotifySourceMessage::BackendFailure(error) => {
                    panic!("notify backend failed during integration test: {error}");
                }
            }
        }

        panic!(
            "notify backend did not report created Org file {} within timeout",
            note.display()
        );
    }
}
