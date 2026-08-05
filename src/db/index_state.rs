use std::{
    collections::{BTreeMap, BTreeSet},
    fmt,
};

use chrono::{DateTime, SecondsFormat, Utc};
use rusqlite::{params, Connection, OptionalExtension, Transaction};
use serde::Serialize;

use super::writer::DbWriteError;

pub(crate) const INDEX_STATE_SCHEMA_VERSION: u32 = 12;

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct IndexState {
    pub database_id: String,
    pub generation: i64,
    pub last_changed_at: String,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum AffectedFileAction {
    Upsert,
    Delete,
}

impl AffectedFileAction {
    fn as_str(self) -> &'static str {
        match self {
            Self::Upsert => "upsert",
            Self::Delete => "delete",
        }
    }

    fn parse(value: &str) -> Result<Self, IndexStateReadError> {
        match value {
            "upsert" => Ok(Self::Upsert),
            "delete" => Ok(Self::Delete),
            other => Err(IndexStateReadError::Integrity(format!(
                "index generation journal contains unsupported action {other:?}"
            ))),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct AffectedFile {
    pub path: String,
    pub action: AffectedFileAction,
}

impl AffectedFile {
    pub(crate) fn upsert(path: impl Into<String>) -> Self {
        Self {
            path: path.into(),
            action: AffectedFileAction::Upsert,
        }
    }

    pub(crate) fn delete(path: impl Into<String>) -> Self {
        Self {
            path: path.into(),
            action: AffectedFileAction::Delete,
        }
    }
}

#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub(crate) struct IndexGenerationChange {
    pub full_invalidation: bool,
    pub files: Vec<AffectedFile>,
}

impl IndexGenerationChange {
    pub(crate) fn full_invalidation() -> Self {
        Self {
            full_invalidation: true,
            files: Vec::new(),
        }
    }

    pub(crate) fn from_files(files: Vec<AffectedFile>) -> Self {
        Self {
            full_invalidation: false,
            files,
        }
    }

    pub(crate) fn is_empty(&self) -> bool {
        !self.full_invalidation && self.files.is_empty()
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum CacheAction {
    Unchanged,
    Patch,
    Rebuild,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct IndexChanges {
    pub database_id: String,
    pub from_generation: i64,
    pub to_generation: i64,
    pub oldest_available_generation: i64,
    pub cache_action: CacheAction,
    pub complete: bool,
    pub reason: Option<String>,
    pub upsert_files: Vec<String>,
    pub deleted_files: Vec<String>,
}

#[derive(Debug)]
pub enum IndexStateReadError {
    Query {
        operation: &'static str,
        source: rusqlite::Error,
    },
    Integrity(String),
    InvalidRequest(String),
}

impl fmt::Display for IndexStateReadError {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Query { operation, source } => {
                write!(
                    formatter,
                    "failed to read index state during {operation}: {source}"
                )
            }
            Self::Integrity(message) => {
                write!(formatter, "invalid index-state database: {message}")
            }
            Self::InvalidRequest(message) => {
                write!(formatter, "invalid index-state request: {message}")
            }
        }
    }
}

impl std::error::Error for IndexStateReadError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self {
            Self::Query { source, .. } => Some(source),
            Self::Integrity(_) | Self::InvalidRequest(_) => None,
        }
    }
}

pub(crate) fn initialize_index_state_schema(
    connection: &Connection,
    on_disk_version: u32,
    migrated_populated_database: bool,
) -> rusqlite::Result<()> {
    if on_disk_version < INDEX_STATE_SCHEMA_VERSION {
        let existing_rows: i64 =
            connection.query_row("SELECT COUNT(*) FROM index_state", [], |row| row.get(0))?;
        if existing_rows == 0 {
            let database_id = generate_database_id(connection)?;
            let last_changed_at = utc_now();
            let generation = if migrated_populated_database { 1 } else { 0 };
            connection.execute(
                "INSERT INTO index_state (singleton, database_id, generation, last_changed_at)\n                 VALUES (1, ?1, ?2, ?3)",
                params![database_id, generation, last_changed_at],
            )?;
            if generation == 1 {
                connection.execute(
                    "INSERT INTO index_generations\n                     (generation, committed_at, full_invalidation)\n                     VALUES (1, ?1, 1)",
                    [last_changed_at],
                )?;
            }
        }
    }

    validate_index_state_schema(connection).map_err(|message| {
        rusqlite::Error::ToSqlConversionFailure(Box::new(IndexStateIntegrityError(message)))
    })
}

fn validate_index_state_schema(connection: &Connection) -> Result<(), String> {
    let row_count = connection
        .query_row("SELECT COUNT(*) FROM index_state", [], |row| {
            row.get::<_, i64>(0)
        })
        .map_err(|error| error.to_string())?;
    if row_count != 1 {
        return Err(format!(
            "index_state must contain exactly one singleton row, found {row_count}"
        ));
    }

    let (singleton, database_id, generation, last_changed_at) = connection
        .query_row(
            "SELECT singleton, database_id, generation, last_changed_at FROM index_state",
            [],
            |row| {
                Ok((
                    row.get::<_, i64>(0)?,
                    row.get::<_, String>(1)?,
                    row.get::<_, i64>(2)?,
                    row.get::<_, String>(3)?,
                ))
            },
        )
        .map_err(|error| error.to_string())?;
    if singleton != 1 {
        return Err(format!(
            "index_state singleton must equal 1, found {singleton}"
        ));
    }
    let state = IndexState {
        database_id,
        generation,
        last_changed_at,
    };
    validate_state_data(&state)?;
    validate_journal_state(connection, state.generation, &state.last_changed_at)?;
    Ok(())
}

pub(crate) fn read_index_state(connection: &Connection) -> Result<IndexState, IndexStateReadError> {
    type StateSnapshot = (
        String,
        i64,
        String,
        i64,
        Option<i64>,
        Option<String>,
        Option<i64>,
        bool,
    );

    let snapshot: Option<StateSnapshot> = connection
        .query_row(
            "SELECT
                 state.database_id,
                 state.generation,
                 state.last_changed_at,
                 (SELECT COUNT(*) FROM index_state),
                 (SELECT MAX(generation) FROM index_generations),
                 (
                     SELECT committed_at
                     FROM index_generations
                     WHERE generation = state.generation
                 ),
                 (
                     SELECT full_invalidation
                     FROM index_generations
                     WHERE generation = state.generation
                 ),
                 EXISTS (
                     SELECT 1
                     FROM index_generation_files
                     WHERE generation = state.generation
                 )
             FROM index_state AS state
             WHERE state.singleton = 1",
            [],
            |row| {
                Ok((
                    row.get(0)?,
                    row.get(1)?,
                    row.get(2)?,
                    row.get(3)?,
                    row.get(4)?,
                    row.get(5)?,
                    row.get(6)?,
                    row.get::<_, i64>(7)? != 0,
                ))
            },
        )
        .optional()
        .map_err(|source| IndexStateReadError::Query {
            operation: "read_index_state.snapshot",
            source,
        })?;
    let Some((
        database_id,
        generation,
        last_changed_at,
        row_count,
        journal_generation,
        committed_at,
        full_invalidation,
        has_files,
    )) = snapshot
    else {
        return Err(IndexStateReadError::Integrity(
            "index_state is missing the required singleton row".to_string(),
        ));
    };
    if row_count != 1 {
        return Err(IndexStateReadError::Integrity(format!(
            "index_state must contain exactly one singleton row, found {row_count}"
        )));
    }

    let state = IndexState {
        database_id,
        generation,
        last_changed_at,
    };
    validate_state_values(&state)?;
    validate_journal_snapshot(
        &state,
        journal_generation,
        committed_at.as_deref(),
        full_invalidation,
        has_files,
    )?;
    Ok(state)
}

fn validate_journal_snapshot(
    state: &IndexState,
    journal_generation: Option<i64>,
    committed_at: Option<&str>,
    full_invalidation: Option<i64>,
    has_files: bool,
) -> Result<(), IndexStateReadError> {
    let integrity = |message| IndexStateReadError::Integrity(message);
    if state.generation == 0 {
        if let Some(maximum) = journal_generation {
            return Err(integrity(format!(
                "generation journal contains generation {maximum} while index_state is generation 0"
            )));
        }
        return Ok(());
    }

    if let Some(maximum) = journal_generation {
        if maximum > state.generation {
            return Err(integrity(format!(
                "generation journal is newer than index_state generation {}",
                state.generation
            )));
        }
    }
    let committed_at = committed_at.ok_or_else(|| {
        integrity(format!(
            "generation journal is missing current generation {}",
            state.generation
        ))
    })?;
    validate_utc_timestamp("committed_at", committed_at).map_err(integrity)?;
    if committed_at != state.last_changed_at {
        return Err(integrity(format!(
            "index_state last_changed_at does not match committed_at for generation {}",
            state.generation
        )));
    }
    match full_invalidation {
        Some(0) if !has_files => Err(integrity(format!(
            "generation {} has neither full invalidation nor affected files",
            state.generation
        ))),
        Some(0 | 1) => Ok(()),
        Some(other) => Err(integrity(format!(
            "generation {} contains invalid full_invalidation value {other}",
            state.generation
        ))),
        None => Err(integrity(format!(
            "generation journal is missing current generation {}",
            state.generation
        ))),
    }
}

pub(crate) fn read_index_changes(
    connection: &Connection,
    expected_database_id: &str,
    since_generation: i64,
) -> Result<IndexChanges, IndexStateReadError> {
    if since_generation < 0 {
        return Err(IndexStateReadError::InvalidRequest(
            "since-generation must be non-negative".to_string(),
        ));
    }
    let state = read_index_state(connection)?;
    let oldest_available_generation = oldest_available_generation(connection, state.generation)?;

    if state.database_id != expected_database_id {
        return Ok(rebuild_changes(
            &state,
            since_generation,
            oldest_available_generation,
            false,
            "database-id-changed",
        ));
    }
    if since_generation > state.generation {
        return Err(IndexStateReadError::InvalidRequest(format!(
            "since-generation {since_generation} is newer than current generation {}",
            state.generation
        )));
    }
    if since_generation == state.generation {
        return Ok(IndexChanges {
            database_id: state.database_id,
            from_generation: since_generation,
            to_generation: state.generation,
            oldest_available_generation,
            cache_action: CacheAction::Unchanged,
            complete: true,
            reason: None,
            upsert_files: Vec::new(),
            deleted_files: Vec::new(),
        });
    }
    if since_generation < oldest_available_generation {
        return Ok(rebuild_changes(
            &state,
            since_generation,
            oldest_available_generation,
            false,
            "history-unavailable",
        ));
    }

    let generations = load_generations_after(connection, since_generation, state.generation)?;
    let expected_count = usize::try_from(state.generation - since_generation).map_err(|_| {
        IndexStateReadError::Integrity("generation range does not fit in memory".to_string())
    })?;
    if generations.len() != expected_count
        || generations
            .iter()
            .enumerate()
            .any(|(index, (generation, _))| {
                *generation != since_generation + i64::try_from(index).unwrap_or(i64::MAX) + 1
            })
    {
        return Err(IndexStateReadError::Integrity(format!(
            "generation journal is incomplete between {since_generation} and {}",
            state.generation
        )));
    }
    if generations.iter().any(|(_, full)| *full) {
        return Ok(rebuild_changes(
            &state,
            since_generation,
            oldest_available_generation,
            true,
            "full-invalidation",
        ));
    }

    let actions = load_net_file_actions(connection, since_generation, state.generation)?;
    let mut upsert_files = Vec::new();
    let mut deleted_files = Vec::new();
    for (path, action) in actions {
        match action {
            AffectedFileAction::Upsert => upsert_files.push(path),
            AffectedFileAction::Delete => deleted_files.push(path),
        }
    }
    Ok(IndexChanges {
        database_id: state.database_id,
        from_generation: since_generation,
        to_generation: state.generation,
        oldest_available_generation,
        cache_action: CacheAction::Patch,
        complete: true,
        reason: None,
        upsert_files,
        deleted_files,
    })
}

pub(crate) fn advance_index_generation(
    transaction: &Transaction<'_>,
    change: &IndexGenerationChange,
) -> Result<IndexState, DbWriteError> {
    if change.is_empty() {
        return Err(DbWriteError::InvalidInput(
            "index generation change must contain affected files or full invalidation",
        ));
    }
    let deduplicated = deduplicate_actions(transaction, &change.files)?;
    let timestamp = utc_now();
    let updated = transaction
        .execute(
            "UPDATE index_state\n             SET generation = generation + 1, last_changed_at = ?1\n             WHERE singleton = 1",
            [&timestamp],
        )
        .map_err(|source| DbWriteError::Write {
            operation: "advance_index_generation.update_state",
            source,
        })?;
    if updated != 1 {
        return Err(DbWriteError::InvalidInput(
            "index_state singleton row is missing or duplicated",
        ));
    }
    let state = transaction
        .query_row(
            "SELECT database_id, generation, last_changed_at\n             FROM index_state WHERE singleton = 1",
            [],
            |row| {
                Ok(IndexState {
                    database_id: row.get(0)?,
                    generation: row.get(1)?,
                    last_changed_at: row.get(2)?,
                })
            },
        )
        .map_err(|source| DbWriteError::ReadBack {
            operation: "advance_index_generation.read_state",
            source,
        })?;
    transaction
        .execute(
            "INSERT INTO index_generations\n             (generation, committed_at, full_invalidation)\n             VALUES (?1, ?2, ?3)",
            params![
                state.generation,
                timestamp,
                if change.full_invalidation { 1_i64 } else { 0_i64 }
            ],
        )
        .map_err(|source| DbWriteError::Write {
            operation: "advance_index_generation.insert_batch",
            source,
        })?;

    for (path, action) in deduplicated {
        transaction
            .execute(
                "INSERT INTO index_generation_files (generation, path, action)\n                 VALUES (?1, ?2, ?3)",
                params![state.generation, path, action.as_str()],
            )
            .map_err(|source| DbWriteError::Write {
                operation: "advance_index_generation.insert_file",
                source,
            })?;
    }
    Ok(state)
}

fn deduplicate_actions(
    connection: &Connection,
    files: &[AffectedFile],
) -> Result<BTreeMap<String, AffectedFileAction>, DbWriteError> {
    let mut actions = BTreeMap::<String, BTreeSet<&'static str>>::new();
    for file in files {
        if file.path.is_empty() {
            return Err(DbWriteError::InvalidInput(
                "affected file path must not be empty",
            ));
        }
        actions
            .entry(file.path.clone())
            .or_default()
            .insert(file.action.as_str());
    }

    let mut deduplicated = BTreeMap::new();
    for (path, candidates) in actions {
        let action = if candidates.len() == 1 {
            AffectedFileAction::parse(candidates.first().copied().unwrap_or(""))
                .map_err(|_| DbWriteError::InvalidInput("invalid affected-file action"))?
        } else {
            let exists = connection
                .query_row("SELECT 1 FROM files WHERE path = ?1", [&path], |_| Ok(()))
                .optional()
                .map_err(|source| DbWriteError::ReadBack {
                    operation: "advance_index_generation.resolve_duplicate_action",
                    source,
                })?
                .is_some();
            if exists {
                AffectedFileAction::Upsert
            } else {
                AffectedFileAction::Delete
            }
        };
        deduplicated.insert(path, action);
    }
    Ok(deduplicated)
}

fn validate_state_values(state: &IndexState) -> Result<(), IndexStateReadError> {
    validate_state_data(state).map_err(IndexStateReadError::Integrity)
}

fn validate_state_data(state: &IndexState) -> Result<(), String> {
    if !is_uuid_v4(&state.database_id) {
        return Err(format!(
            "database_id must be a lowercase-or-uppercase UUIDv4, found {:?}",
            state.database_id
        ));
    }
    if state.generation < 0 {
        return Err(format!(
            "generation must be non-negative, found {}",
            state.generation
        ));
    }
    validate_utc_timestamp("last_changed_at", &state.last_changed_at)
}

fn validate_utc_timestamp(field: &str, value: &str) -> Result<(), String> {
    let timestamp = DateTime::parse_from_rfc3339(value).map_err(|error| {
        format!("{field} must be an RFC 3339 timestamp, found {value:?}: {error}")
    })?;
    if timestamp.offset().local_minus_utc() != 0 {
        return Err(format!("{field} must use UTC, found {value:?}"));
    }
    Ok(())
}

fn validate_journal_state(
    connection: &Connection,
    generation: i64,
    last_changed_at: &str,
) -> Result<(), String> {
    let journal_generation = connection
        .query_row("SELECT MAX(generation) FROM index_generations", [], |row| {
            row.get::<_, Option<i64>>(0)
        })
        .map_err(|error| error.to_string())?;
    match (generation, journal_generation) {
        (0, None) => Ok(()),
        (0, Some(maximum)) => Err(format!(
            "generation journal contains generation {maximum} while index_state is generation 0"
        )),
        (current, Some(maximum)) if maximum > current => Err(format!(
            "generation journal is newer than index_state generation {current}"
        )),
        (current, _) => {
            let committed_at =
                validate_generation_record(connection, current)?.ok_or_else(|| {
                    format!("generation journal is missing current generation {current}")
                })?;
            if committed_at != last_changed_at {
                return Err(format!(
                    "index_state last_changed_at does not match committed_at for generation {current}"
                ));
            }
            Ok(())
        }
    }
}

fn validate_generation_record(
    connection: &Connection,
    generation: i64,
) -> Result<Option<String>, String> {
    let row = connection
        .query_row(
            "SELECT
                 generations.committed_at,
                 generations.full_invalidation,
                 EXISTS (
                     SELECT 1
                     FROM index_generation_files AS files
                     WHERE files.generation = generations.generation
                 )
             FROM index_generations AS generations
             WHERE generations.generation = ?1",
            [generation],
            |row| {
                Ok((
                    row.get::<_, String>(0)?,
                    row.get::<_, i64>(1)?,
                    row.get::<_, i64>(2)? != 0,
                ))
            },
        )
        .optional()
        .map_err(|error| error.to_string())?;
    let Some((committed_at, full_invalidation, has_files)) = row else {
        return Ok(None);
    };
    validate_utc_timestamp("committed_at", &committed_at)?;
    match full_invalidation {
        0 if !has_files => Err(format!(
            "generation {generation} has neither full invalidation nor affected files"
        )),
        0 | 1 => Ok(Some(committed_at)),
        other => Err(format!(
            "generation {generation} contains invalid full_invalidation value {other}"
        )),
    }
}

fn is_uuid_v4(value: &str) -> bool {
    let bytes = value.as_bytes();
    bytes.len() == 36
        && bytes[8] == b'-'
        && bytes[13] == b'-'
        && bytes[14] == b'4'
        && bytes[18] == b'-'
        && matches!(bytes[19].to_ascii_lowercase(), b'8' | b'9' | b'a' | b'b')
        && bytes[23] == b'-'
        && bytes
            .iter()
            .enumerate()
            .all(|(index, byte)| matches!(index, 8 | 13 | 18 | 23) || byte.is_ascii_hexdigit())
}

fn oldest_available_generation(
    connection: &Connection,
    current_generation: i64,
) -> Result<i64, IndexStateReadError> {
    let minimum = connection
        .query_row("SELECT MIN(generation) FROM index_generations", [], |row| {
            row.get::<_, Option<i64>>(0)
        })
        .map_err(|source| IndexStateReadError::Query {
            operation: "read_index_changes.oldest_generation",
            source,
        })?;
    Ok(minimum.map_or(current_generation, |generation| {
        generation.saturating_sub(1)
    }))
}

fn load_generations_after(
    connection: &Connection,
    since_generation: i64,
    current_generation: i64,
) -> Result<Vec<(i64, bool)>, IndexStateReadError> {
    let mut statement = connection
        .prepare(
            "SELECT
                 generations.generation,
                 generations.committed_at,
                 generations.full_invalidation,
                 EXISTS (
                     SELECT 1
                     FROM index_generation_files AS files
                     WHERE files.generation = generations.generation
                 )
             FROM index_generations AS generations
             WHERE generations.generation > ?1 AND generations.generation <= ?2
             ORDER BY generations.generation",
        )
        .map_err(|source| IndexStateReadError::Query {
            operation: "read_index_changes.prepare_generations",
            source,
        })?;
    let rows = statement
        .query_map(params![since_generation, current_generation], |row| {
            Ok((
                row.get::<_, i64>(0)?,
                row.get::<_, String>(1)?,
                row.get::<_, i64>(2)?,
                row.get::<_, i64>(3)? != 0,
            ))
        })
        .map_err(|source| IndexStateReadError::Query {
            operation: "read_index_changes.query_generations",
            source,
        })?;
    let rows =
        rows.collect::<Result<Vec<_>, _>>()
            .map_err(|source| IndexStateReadError::Query {
                operation: "read_index_changes.collect_generations",
                source,
            })?;
    rows.into_iter()
        .map(|(generation, committed_at, full_invalidation, has_files)| {
            validate_utc_timestamp("committed_at", &committed_at)
                .map_err(IndexStateReadError::Integrity)?;
            let full_invalidation = match full_invalidation {
                0 => false,
                1 => true,
                other => {
                    return Err(IndexStateReadError::Integrity(format!(
                        "generation {generation} contains invalid full_invalidation value {other}"
                    )))
                }
            };
            if !full_invalidation && !has_files {
                return Err(IndexStateReadError::Integrity(format!(
                    "generation {generation} has neither full invalidation nor affected files"
                )));
            }
            Ok((generation, full_invalidation))
        })
        .collect()
}

fn load_net_file_actions(
    connection: &Connection,
    since_generation: i64,
    current_generation: i64,
) -> Result<BTreeMap<String, AffectedFileAction>, IndexStateReadError> {
    let mut statement = connection
        .prepare(
            "SELECT path, action\n             FROM index_generation_files\n             WHERE generation > ?1 AND generation <= ?2\n             ORDER BY generation, path",
        )
        .map_err(|source| IndexStateReadError::Query {
            operation: "read_index_changes.prepare_files",
            source,
        })?;
    let rows = statement
        .query_map(params![since_generation, current_generation], |row| {
            Ok((row.get::<_, String>(0)?, row.get::<_, String>(1)?))
        })
        .map_err(|source| IndexStateReadError::Query {
            operation: "read_index_changes.query_files",
            source,
        })?;
    let mut actions = BTreeMap::new();
    for row in rows {
        let (path, action) = row.map_err(|source| IndexStateReadError::Query {
            operation: "read_index_changes.collect_files",
            source,
        })?;
        actions.insert(path, AffectedFileAction::parse(&action)?);
    }
    Ok(actions)
}

fn rebuild_changes(
    state: &IndexState,
    since_generation: i64,
    oldest_available_generation: i64,
    complete: bool,
    reason: &str,
) -> IndexChanges {
    IndexChanges {
        database_id: state.database_id.clone(),
        from_generation: since_generation,
        to_generation: state.generation,
        oldest_available_generation,
        cache_action: CacheAction::Rebuild,
        complete,
        reason: Some(reason.to_string()),
        upsert_files: Vec::new(),
        deleted_files: Vec::new(),
    }
}

fn generate_database_id(connection: &Connection) -> rusqlite::Result<String> {
    connection.query_row(
        "SELECT lower(\n             substr(value, 1, 8) || '-' ||\n             substr(value, 9, 4) || '-' ||\n             '4' || substr(value, 14, 3) || '-' ||\n             substr('89ab', abs(random() % 4) + 1, 1) || substr(value, 18, 3) || '-' ||\n             substr(value, 21, 12)\n         )\n         FROM (SELECT hex(randomblob(16)) AS value)",
        [],
        |row| row.get(0),
    )
}

pub(crate) fn utc_now() -> String {
    Utc::now().to_rfc3339_opts(SecondsFormat::Secs, true)
}

#[derive(Debug)]
struct IndexStateIntegrityError(String);

impl fmt::Display for IndexStateIntegrityError {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        formatter.write_str(&self.0)
    }
}

impl std::error::Error for IndexStateIntegrityError {}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::db::{
        open_database, open_existing_database_read_only, open_in_memory_database_with_schema,
        SchemaDefinition, CURRENT_SCHEMA_VERSION,
    };
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
                "org-files-db-index-state-tests-{name}-{}-{unique}",
                std::process::id()
            ));
            fs::create_dir_all(&path).expect("test directory should be created");
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

    #[test]
    fn new_database_has_stable_identity_and_generation_zero() {
        let connection = open_in_memory_database_with_schema(&SchemaDefinition::new(
            CURRENT_SCHEMA_VERSION,
            false,
        ))
        .expect("database should open");
        let first = read_index_state(&connection).expect("state should load");
        let second = read_index_state(&connection).expect("state should remain readable");
        assert_eq!(first, second);
        assert_eq!(first.generation, 0);
        assert!(!first.database_id.is_empty());
    }

    #[test]
    fn generation_and_journal_roll_back_together() {
        let mut connection = open_in_memory_database_with_schema(&SchemaDefinition::new(
            CURRENT_SCHEMA_VERSION,
            false,
        ))
        .expect("database should open");
        let before = read_index_state(&connection).expect("state should load");
        {
            let tx = connection.transaction().expect("transaction should start");
            advance_index_generation(
                &tx,
                &IndexGenerationChange::from_files(vec![AffectedFile::upsert("/tmp/a.org")]),
            )
            .expect("generation should advance inside transaction");
        }
        assert_eq!(
            read_index_state(&connection).expect("state should load"),
            before
        );
        let journal_count: i64 = connection
            .query_row("SELECT COUNT(*) FROM index_generations", [], |row| {
                row.get(0)
            })
            .expect("journal count should load");
        assert_eq!(journal_count, 0);
    }

    #[test]
    fn changes_collapse_to_the_latest_action_per_path() {
        let mut connection = open_in_memory_database_with_schema(&SchemaDefinition::new(
            CURRENT_SCHEMA_VERSION,
            false,
        ))
        .expect("database should open");
        let database_id = read_index_state(&connection)
            .expect("state should load")
            .database_id;
        for change in [
            IndexGenerationChange::from_files(vec![AffectedFile::upsert("/tmp/a.org")]),
            IndexGenerationChange::from_files(vec![
                AffectedFile::delete("/tmp/a.org"),
                AffectedFile::upsert("/tmp/b.org"),
            ]),
        ] {
            let tx = connection.transaction().expect("transaction should start");
            advance_index_generation(&tx, &change).expect("generation should advance");
            tx.commit().expect("transaction should commit");
        }
        let changes =
            read_index_changes(&connection, &database_id, 0).expect("changes should load");
        assert_eq!(changes.cache_action, CacheAction::Patch);
        assert_eq!(changes.upsert_files, vec!["/tmp/b.org"]);
        assert_eq!(changes.deleted_files, vec!["/tmp/a.org"]);
    }

    #[test]
    fn changes_report_identity_mismatch_future_generation_and_unavailable_history() {
        let mut connection = open_in_memory_database_with_schema(&SchemaDefinition::new(
            CURRENT_SCHEMA_VERSION,
            false,
        ))
        .expect("database should open");
        let initial = read_index_state(&connection).expect("state should load");

        let identity_change = read_index_changes(&connection, "different-database", 0)
            .expect("identity mismatch should be a cache transition");
        assert_eq!(identity_change.cache_action, CacheAction::Rebuild);
        assert_eq!(
            identity_change.reason.as_deref(),
            Some("database-id-changed")
        );
        assert!(!identity_change.complete);

        let future = read_index_changes(&connection, &initial.database_id, 1)
            .expect_err("future generation should fail");
        assert!(future.to_string().contains("newer than current generation"));

        for path in ["/tmp/a.org", "/tmp/b.org"] {
            let tx = connection.transaction().expect("transaction should start");
            advance_index_generation(
                &tx,
                &IndexGenerationChange::from_files(vec![AffectedFile::upsert(path)]),
            )
            .expect("generation should advance");
            tx.commit().expect("transaction should commit");
        }
        connection
            .execute("DELETE FROM index_generations WHERE generation = 1", [])
            .expect("old generation should be pruned in the fixture");

        let unavailable = read_index_changes(&connection, &initial.database_id, 0)
            .expect("unavailable history should request rebuild");
        assert_eq!(unavailable.cache_action, CacheAction::Rebuild);
        assert_eq!(unavailable.reason.as_deref(), Some("history-unavailable"));
        assert!(!unavailable.complete);
        assert_eq!(unavailable.oldest_available_generation, 1);
    }

    #[test]
    fn conflicting_actions_collapse_to_the_committed_file_presence() {
        let mut connection = open_in_memory_database_with_schema(&SchemaDefinition::new(
            CURRENT_SCHEMA_VERSION,
            false,
        ))
        .expect("database should open");
        connection
            .execute(
                "INSERT INTO files (path, mtime_ns, size) VALUES ('/tmp/a.org', 1, 1)",
                [],
            )
            .expect("file should insert");
        let tx = connection.transaction().expect("transaction should start");
        advance_index_generation(
            &tx,
            &IndexGenerationChange::from_files(vec![
                AffectedFile::delete("/tmp/a.org"),
                AffectedFile::upsert("/tmp/a.org"),
            ]),
        )
        .expect("generation should advance");
        tx.commit().expect("transaction should commit");
        let first_action: String = connection
            .query_row(
                "SELECT action FROM index_generation_files WHERE generation = 1 AND path = '/tmp/a.org'",
                [],
                |row| row.get(0),
            )
            .expect("first action should load");
        assert_eq!(first_action, "upsert");

        let tx = connection.transaction().expect("transaction should start");
        tx.execute("DELETE FROM files WHERE path = '/tmp/a.org'", [])
            .expect("file should delete");
        advance_index_generation(
            &tx,
            &IndexGenerationChange::from_files(vec![
                AffectedFile::upsert("/tmp/a.org"),
                AffectedFile::delete("/tmp/a.org"),
            ]),
        )
        .expect("generation should advance");
        tx.commit().expect("transaction should commit");
        let second_action: String = connection
            .query_row(
                "SELECT action FROM index_generation_files WHERE generation = 2 AND path = '/tmp/a.org'",
                [],
                |row| row.get(0),
            )
            .expect("second action should load");
        assert_eq!(second_action, "delete");
    }

    #[test]
    fn full_invalidation_requests_a_complete_view_rebuild() {
        let mut connection = open_in_memory_database_with_schema(&SchemaDefinition::new(
            CURRENT_SCHEMA_VERSION,
            false,
        ))
        .expect("database should open");
        let database_id = read_index_state(&connection)
            .expect("state should load")
            .database_id;
        let tx = connection.transaction().expect("transaction should start");
        advance_index_generation(&tx, &IndexGenerationChange::full_invalidation())
            .expect("generation should advance");
        tx.commit().expect("transaction should commit");
        let changes =
            read_index_changes(&connection, &database_id, 0).expect("changes should load");
        assert_eq!(changes.cache_action, CacheAction::Rebuild);
        assert_eq!(changes.reason.as_deref(), Some("full-invalidation"));
        assert!(changes.complete);
    }

    #[test]
    fn populated_version_eleven_migrates_to_generation_one_with_full_invalidation() {
        let connection = open_in_memory_database_with_schema(&SchemaDefinition::new(11, false))
            .expect("version-eleven fixture should open");
        connection
            .execute(
                "INSERT INTO files (path, mtime_ns, size) VALUES ('/tmp/existing.org', 1, 1)",
                [],
            )
            .expect("legacy file should insert");
        connection
            .execute_batch(
                "DROP TABLE index_generation_files;
                 DROP TABLE index_generations;
                 DROP TABLE index_state;",
            )
            .expect("new index-state tables should be removable from the fixture");
        connection
            .pragma_update(None, "user_version", 11_u32)
            .expect("legacy schema version should persist");

        SchemaDefinition::new(CURRENT_SCHEMA_VERSION, false)
            .apply(&connection)
            .expect("migration should apply");
        let state = read_index_state(&connection).expect("migrated state should load");
        assert_eq!(state.generation, 1);
        assert!(!state.database_id.is_empty());
        let full_invalidation: i64 = connection
            .query_row(
                "SELECT full_invalidation FROM index_generations WHERE generation = 1",
                [],
                |row| row.get(0),
            )
            .expect("migration generation should exist");
        assert_eq!(full_invalidation, 1);
        let file_count: i64 = connection
            .query_row("SELECT COUNT(*) FROM files", [], |row| row.get(0))
            .expect("legacy files should remain");
        assert_eq!(file_count, 1);
    }

    #[test]
    fn database_identity_survives_reopen_and_changes_after_recreation() {
        let test_dir = TestDir::new("identity");
        let database_path = test_dir.path().join("index.sqlite");
        let first_id = {
            let connection = open_database(&database_path).expect("database should open");
            read_index_state(&connection)
                .expect("state should load")
                .database_id
        };
        let reopened_id = {
            let connection = open_database(&database_path).expect("database should reopen");
            read_index_state(&connection)
                .expect("state should load after reopen")
                .database_id
        };
        assert_eq!(first_id, reopened_id);

        for path in [
            database_path.clone(),
            PathBuf::from(format!("{}-wal", database_path.display())),
            PathBuf::from(format!("{}-shm", database_path.display())),
        ] {
            let _ = fs::remove_file(path);
        }
        let recreated = open_database(&database_path).expect("database should be recreated");
        let recreated_id = read_index_state(&recreated)
            .expect("recreated state should load")
            .database_id;
        assert_ne!(first_id, recreated_id);
    }

    #[test]
    fn one_batch_with_several_files_advances_once_and_deduplicates_paths() {
        let mut connection = open_in_memory_database_with_schema(&SchemaDefinition::new(
            CURRENT_SCHEMA_VERSION,
            false,
        ))
        .expect("database should open");
        let before = read_index_state(&connection).expect("state should load");
        let tx = connection.transaction().expect("transaction should start");
        advance_index_generation(
            &tx,
            &IndexGenerationChange::from_files(vec![
                AffectedFile::upsert("/tmp/a.org"),
                AffectedFile::upsert("/tmp/a.org"),
                AffectedFile::delete("/tmp/b.org"),
            ]),
        )
        .expect("batch should advance");
        tx.commit().expect("transaction should commit");

        let after = read_index_state(&connection).expect("state should load");
        assert_eq!(after.generation, before.generation + 1);
        let generation_rows: i64 = connection
            .query_row("SELECT COUNT(*) FROM index_generations", [], |row| {
                row.get(0)
            })
            .expect("generation rows should count");
        let file_rows: i64 = connection
            .query_row("SELECT COUNT(*) FROM index_generation_files", [], |row| {
                row.get(0)
            })
            .expect("affected file rows should count");
        assert_eq!(generation_rows, 1);
        assert_eq!(file_rows, 2);
    }

    #[test]
    fn empty_change_is_rejected_without_advancing_state() {
        let mut connection = open_in_memory_database_with_schema(&SchemaDefinition::new(
            CURRENT_SCHEMA_VERSION,
            false,
        ))
        .expect("database should open");
        let before = read_index_state(&connection).expect("state should load");
        {
            let tx = connection.transaction().expect("transaction should start");
            let error = advance_index_generation(&tx, &IndexGenerationChange::default())
                .expect_err("empty change should fail");
            assert!(error.to_string().contains("must contain affected files"));
        }
        assert_eq!(
            read_index_state(&connection).expect("state should load"),
            before
        );
    }

    #[test]
    fn deleted_path_remains_in_the_committed_journal() {
        let mut connection = open_in_memory_database_with_schema(&SchemaDefinition::new(
            CURRENT_SCHEMA_VERSION,
            false,
        ))
        .expect("database should open");
        connection
            .execute(
                "INSERT INTO files (path, mtime_ns, size) VALUES ('/tmp/deleted.org', 1, 1)",
                [],
            )
            .expect("file should insert");
        let tx = connection.transaction().expect("transaction should start");
        tx.execute("DELETE FROM files WHERE path = '/tmp/deleted.org'", [])
            .expect("file should delete");
        advance_index_generation(
            &tx,
            &IndexGenerationChange::from_files(vec![AffectedFile::delete("/tmp/deleted.org")]),
        )
        .expect("deletion generation should advance");
        tx.commit().expect("transaction should commit");

        let action: String = connection
            .query_row(
                "SELECT action FROM index_generation_files WHERE path = '/tmp/deleted.org'",
                [],
                |row| row.get(0),
            )
            .expect("deleted path should remain in journal");
        assert_eq!(action, "delete");
    }

    #[test]
    fn readers_observe_generation_and_index_rows_atomically() {
        let test_dir = TestDir::new("atomic-visibility");
        let database_path = test_dir.path().join("index.sqlite");
        let mut writer = open_database(&database_path).expect("writer database should open");
        let reader = open_existing_database_read_only(&database_path)
            .expect("read-only database should open");
        let before = read_index_state(&reader).expect("initial state should load");

        let tx = writer
            .transaction()
            .expect("writer transaction should start");
        tx.execute(
            "INSERT INTO files (path, mtime_ns, size) VALUES ('/tmp/atomic.org', 1, 1)",
            [],
        )
        .expect("file should insert inside transaction");
        advance_index_generation(
            &tx,
            &IndexGenerationChange::from_files(vec![AffectedFile::upsert("/tmp/atomic.org")]),
        )
        .expect("generation should advance inside transaction");

        assert_eq!(
            read_index_state(&reader).expect("old state should remain visible"),
            before
        );
        let before_count: i64 = reader
            .query_row("SELECT COUNT(*) FROM files", [], |row| row.get(0))
            .expect("reader should query files");
        assert_eq!(before_count, 0);

        tx.commit().expect("writer transaction should commit");
        let after = read_index_state(&reader).expect("new state should become visible");
        assert_eq!(after.generation, before.generation + 1);
        let after_count: i64 = reader
            .query_row("SELECT COUNT(*) FROM files", [], |row| row.get(0))
            .expect("reader should query committed files");
        assert_eq!(after_count, 1);
    }

    #[test]
    fn singleton_constraints_and_missing_state_are_reported() {
        let connection = open_in_memory_database_with_schema(&SchemaDefinition::new(
            CURRENT_SCHEMA_VERSION,
            false,
        ))
        .expect("database should open");
        let duplicate = connection
            .execute(
                "INSERT INTO index_state
                 (singleton, database_id, generation, last_changed_at)
                 VALUES (1, 'duplicate', 0, '2026-08-05T00:00:00Z')",
                [],
            )
            .expect_err("second singleton should fail");
        assert!(duplicate.to_string().contains("UNIQUE"));
        let invalid_singleton = connection
            .execute(
                "INSERT INTO index_state
                 (singleton, database_id, generation, last_changed_at)
                 VALUES (2, 'invalid', 0, '2026-08-05T00:00:00Z')",
                [],
            )
            .expect_err("invalid singleton should fail");
        assert!(invalid_singleton.to_string().contains("CHECK"));

        connection
            .execute("DELETE FROM index_state", [])
            .expect("state row should delete for corruption fixture");
        let error = read_index_state(&connection).expect_err("missing state should fail");
        assert!(error
            .to_string()
            .contains("missing the required singleton row"));
    }

    #[test]
    fn corrupt_identity_timestamp_and_journal_state_are_rejected() {
        let connection = open_in_memory_database_with_schema(&SchemaDefinition::new(
            CURRENT_SCHEMA_VERSION,
            false,
        ))
        .expect("database should open");
        let original = read_index_state(&connection).expect("state should load");

        connection
            .execute(
                "UPDATE index_state SET database_id = 'not-a-uuid' WHERE singleton = 1",
                [],
            )
            .expect("database ID should be corrupted for the fixture");
        let error = read_index_state(&connection).expect_err("invalid database ID should fail");
        assert!(error.to_string().contains("UUIDv4"));

        connection
            .execute(
                "UPDATE index_state
                 SET database_id = ?1, last_changed_at = 'not-a-timestamp'
                 WHERE singleton = 1",
                [&original.database_id],
            )
            .expect("timestamp should be corrupted for the fixture");
        let error = read_index_state(&connection).expect_err("invalid timestamp should fail");
        assert!(error.to_string().contains("RFC 3339"));

        connection
            .execute(
                "UPDATE index_state SET last_changed_at = ?1 WHERE singleton = 1",
                [&original.last_changed_at],
            )
            .expect("timestamp should be restored");
        connection
            .execute(
                "UPDATE index_state
                 SET generation = 1, last_changed_at = '2026-08-05T00:00:01Z'
                 WHERE singleton = 1",
                [],
            )
            .expect("state should advance for timestamp mismatch fixture");
        connection
            .execute(
                "INSERT INTO index_generations
                 (generation, committed_at, full_invalidation)
                 VALUES (1, '2026-08-05T00:00:00Z', 1)",
                [],
            )
            .expect("mismatched generation should insert for fixture");
        let error = read_index_state(&connection)
            .expect_err("mismatched state and generation timestamps should fail");
        assert!(error.to_string().contains("does not match committed_at"));

        connection
            .execute("DELETE FROM index_generations", [])
            .expect("mismatched generation should be removed");
        connection
            .execute(
                "UPDATE index_state
                 SET generation = 0, last_changed_at = ?1
                 WHERE singleton = 1",
                [&original.last_changed_at],
            )
            .expect("state should be restored");
        connection
            .execute(
                "INSERT INTO index_generations
                 (generation, committed_at, full_invalidation)
                 VALUES (1, ?1, 1)",
                [&original.last_changed_at],
            )
            .expect("newer journal row should insert for corruption fixture");
        let error = read_index_state(&connection).expect_err("newer journal should fail");
        assert!(error
            .to_string()
            .contains("while index_state is generation 0"));
    }

    #[test]
    fn non_full_generation_without_affected_files_is_corrupt() {
        let connection = open_in_memory_database_with_schema(&SchemaDefinition::new(
            CURRENT_SCHEMA_VERSION,
            false,
        ))
        .expect("database should open");
        let initial = read_index_state(&connection).expect("state should load");
        connection
            .execute(
                "UPDATE index_state SET generation = 1 WHERE singleton = 1",
                [],
            )
            .expect("state generation should update for fixture");
        connection
            .execute(
                "INSERT INTO index_generations
                 (generation, committed_at, full_invalidation)
                 VALUES (1, ?1, 0)",
                [&initial.last_changed_at],
            )
            .expect("empty non-full generation should insert for fixture");

        let error = read_index_changes(&connection, &initial.database_id, 0)
            .expect_err("empty non-full generation should fail");
        assert!(error
            .to_string()
            .contains("neither full invalidation nor affected files"));
    }
}
