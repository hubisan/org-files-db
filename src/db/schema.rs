use std::sync::atomic::{AtomicU64, Ordering};

use std::collections::HashMap;

use rusqlite::{Connection, OptionalExtension};

use crate::db::{DbWriter, EffectivePropertyRecord, EffectiveTagRecord};
use crate::property::{derive_effective_properties, PropertyRow};
use crate::tag::derive_effective_tags;

use super::{
    index_state::{initialize_index_state_schema, INDEX_STATE_SCHEMA_VERSION},
    DB_METADATA_FTS_AVAILABLE_KEY, DB_METADATA_FTS_BODY_INDEXED_KEY,
    DB_METADATA_FTS_SCHEMA_VERSION_KEY,
};

pub const CURRENT_SCHEMA_VERSION: u32 = 12;
const EFFECTIVE_PROPERTIES_SCHEMA_VERSION: u32 = 10;
const EFFECTIVE_TAGS_SCHEMA_VERSION: u32 = 11;
const EFFECTIVE_TAGS_ORDER_BACKUP_TABLE: &str = "orgfdb_effective_tags_order_backup";

const CORE_SCHEMA_SQL: &str = include_str!("../../sql/schema.sql");
const HEADING_FTS_SQL: &str = r#"
CREATE VIRTUAL TABLE IF NOT EXISTS heading_fts
USING fts5(
    title,
    body,
    tokenize = 'unicode61',
    content = ''
)
"#;
const HEADING_FTS_MARKER: &str = "/*__HEADING_FTS__*/";
const TIMESTAMP_REPEATERS_TABLE_SQL: &str = r#"
CREATE TABLE timestamp_repeaters (
    id                          INTEGER PRIMARY KEY,
    timestamp_id                INTEGER NOT NULL UNIQUE,
    repeater_type               TEXT CHECK (
                                    repeater_type IN ('cumulate', 'catch_up', 'restart')
                                    OR repeater_type IS NULL
                                ),
    repeater_value              INTEGER CHECK (
                                    repeater_value IS NULL
                                    OR repeater_value > 0
                                ),
    repeater_unit               TEXT CHECK (
                                    repeater_unit IN ('hour', 'day', 'week', 'month', 'year')
                                    OR repeater_unit IS NULL
                                ),
    repeater_deadline_value     INTEGER CHECK (
                                    repeater_deadline_value IS NULL
                                    OR repeater_deadline_value > 0
                                ),
    repeater_deadline_unit      TEXT CHECK (
                                    repeater_deadline_unit IN ('hour', 'day', 'week', 'month', 'year')
                                    OR repeater_deadline_unit IS NULL
                                ),
    warning_type                TEXT CHECK (
                                    warning_type IN ('all', 'first')
                                    OR warning_type IS NULL
                                ),
    warning_value               INTEGER CHECK (
                                    warning_value IS NULL
                                    OR warning_value > 0
                                ),
    warning_unit                TEXT CHECK (
                                    warning_unit IN ('hour', 'day', 'week', 'month', 'year')
                                    OR warning_unit IS NULL
                                ),
    FOREIGN KEY (timestamp_id)
        REFERENCES timestamps(id)
        ON DELETE CASCADE,
    CHECK (
        (repeater_type IS NULL AND repeater_value IS NULL AND repeater_unit IS NULL)
        OR
        (repeater_type IS NOT NULL AND repeater_value IS NOT NULL AND repeater_unit IS NOT NULL)
    ),
    CHECK (
        (repeater_deadline_value IS NULL AND repeater_deadline_unit IS NULL)
        OR
        (repeater_deadline_value IS NOT NULL AND repeater_deadline_unit IS NOT NULL)
    ),
    CHECK (
        repeater_deadline_value IS NULL
        OR
        repeater_type IS NOT NULL
    ),
    CHECK (
        (warning_type IS NULL AND warning_value IS NULL AND warning_unit IS NULL)
        OR
        (warning_type IS NOT NULL AND warning_value IS NOT NULL AND warning_unit IS NOT NULL)
    ),
    CHECK (
        repeater_type IS NOT NULL
        OR warning_type IS NOT NULL
    )
)
"#;
const PROPERTIES_TABLE_SQL: &str = r#"
CREATE TABLE properties (
    id              INTEGER PRIMARY KEY,
    heading_id      INTEGER NOT NULL,
    key             TEXT NOT NULL,
    value           TEXT,
    source          TEXT NOT NULL CHECK (
                        source IN ('property_keyword', 'property_drawer', 'category_keyword')
                    ),
    append          INTEGER NOT NULL DEFAULT 0 CHECK (append IN (0, 1)),
    line_number     INTEGER,
    FOREIGN KEY (heading_id)
        REFERENCES headings(id)
        ON DELETE CASCADE
)
"#;
const TODO_KEYWORDS_TABLE_SQL: &str = r#"
CREATE TABLE todo_keywords (
    file_id             INTEGER NOT NULL,
    keyword             TEXT NOT NULL,
    state_type          TEXT NOT NULL CHECK (state_type IN ('open', 'closed')),
    shortcut            TEXT CHECK (shortcut IS NULL OR length(shortcut) = 1),
    sequence_no         INTEGER NOT NULL,
    source_kind         TEXT NOT NULL CHECK (
                            source_kind IN ('config_default', 'org_keyword')
                        ),
    source_keyword      TEXT CHECK (
                            source_keyword IN ('TODO', 'SEQ_TODO', 'TYP_TODO')
                            OR source_keyword IS NULL
                        ),
    source_line_number  INTEGER CHECK (
                            source_line_number IS NULL
                            OR source_line_number > 0
                        ),
    CHECK (
        (source_kind = 'config_default' AND source_keyword IS NULL AND source_line_number IS NULL)
        OR
        (source_kind = 'org_keyword' AND source_keyword IS NOT NULL AND source_line_number IS NOT NULL)
    ),
    FOREIGN KEY (file_id)
        REFERENCES files(id)
        ON DELETE CASCADE,
    PRIMARY KEY (file_id, keyword)
)
"#;
const TAGS_TABLE_SQL: &str = r#"
CREATE TABLE tags (
    heading_id      INTEGER NOT NULL,
    tag             TEXT NOT NULL,
    FOREIGN KEY (heading_id)
        REFERENCES headings(id)
        ON DELETE CASCADE,
    PRIMARY KEY (heading_id, tag)
)
"#;
static SQLITE_FTS5_PROBE_COUNTER: AtomicU64 = AtomicU64::new(0);

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SchemaDefinition {
    pub version: u32,
    pub enable_fts: bool,
}

impl SchemaDefinition {
    pub fn new(version: u32, enable_fts: bool) -> Self {
        Self {
            version,
            enable_fts,
        }
    }

    pub fn render_sql(&self, connection: &Connection) -> String {
        let heading_fts_sql =
            if self.enable_fts && sqlite_supports_fts5(connection).unwrap_or(false) {
                HEADING_FTS_SQL
            } else {
                ""
            };

        CORE_SCHEMA_SQL.replace(HEADING_FTS_MARKER, heading_fts_sql)
    }

    pub fn apply(&self, connection: &Connection) -> rusqlite::Result<()> {
        let on_disk_version: u32 =
            connection.pragma_query_value(None, "user_version", |row| row.get(0))?;
        let migrated_populated_database = on_disk_version < INDEX_STATE_SCHEMA_VERSION
            && table_exists(connection, "files")?
            && connection.query_row("SELECT EXISTS(SELECT 1 FROM files)", [], |row| {
                row.get::<_, bool>(0)
            })?;
        let mut needs_effective_properties_backfill = on_disk_version
            < EFFECTIVE_PROPERTIES_SCHEMA_VERSION
            || !table_exists(connection, "effective_properties")?;
        let mut needs_effective_tags_backfill = on_disk_version < EFFECTIVE_TAGS_SCHEMA_VERSION
            || !table_exists(connection, "effective_tags")?;
        let headings_need_migration = if table_exists(connection, "headings")? {
            let columns = table_columns(connection, "headings")?;
            !headings_table_matches_current_contract(connection, &columns)?
        } else {
            false
        };
        let effective_tags_need_fk_repair = table_exists(connection, "effective_tags")?
            && table_references(connection, "effective_tags", "headings_legacy")?;
        let tags_need_migration = if table_exists(connection, "tags")? {
            let columns = table_columns(connection, "tags")?;
            !tags_table_uses_direct_facts(&columns)
        } else {
            false
        };

        // Preserve the public effective-tag order only when schema work may
        // rebuild headings, canonical tags, or effective_tags. Healthy
        // schema-version-12 opens must not scan and reserialize every tag.
        if needs_effective_tags_backfill
            || headings_need_migration
            || effective_tags_need_fk_repair
            || tags_need_migration
        {
            prepare_effective_tags_order_backup(connection)?;
        }

        migrate_legacy_files_identity(connection)?;
        migrate_legacy_timestamp_repeaters(connection)?;
        migrate_legacy_todo_keywords_table(connection)?;
        migrate_legacy_properties_table(connection)?;
        if migrate_legacy_tags_table(connection)? {
            needs_effective_tags_backfill = true;
        }
        if migrate_legacy_headings_table(connection)? {
            needs_effective_properties_backfill = true;
            needs_effective_tags_backfill = true;
        }
        migrate_legacy_timestamps_table(connection)?;
        migrate_legacy_links_table(connection)?;
        let repaired = repair_tables_depending_on_headings(connection)?;
        needs_effective_properties_backfill |= repaired.effective_properties;
        needs_effective_tags_backfill |= repaired.effective_tags;
        migrate_v8_index_set(connection)?;
        connection.execute_batch(&self.render_sql(connection))?;
        initialize_index_state_schema(connection, on_disk_version, migrated_populated_database)?;
        if needs_effective_properties_backfill {
            connection.execute("DELETE FROM effective_properties", [])?;
            backfill_effective_properties(connection)?;
        }
        if needs_effective_tags_backfill {
            connection.execute("DELETE FROM effective_tags", [])?;
            backfill_effective_tags(connection)?;
        }
        connection.execute_batch(&format!(
            "DROP TABLE IF EXISTS temp.{EFFECTIVE_TAGS_ORDER_BACKUP_TABLE};"
        ))?;
        Ok(())
    }
}

fn prepare_effective_tags_order_backup(connection: &Connection) -> rusqlite::Result<()> {
    connection.execute_batch(&format!(
        "DROP TABLE IF EXISTS temp.{EFFECTIVE_TAGS_ORDER_BACKUP_TABLE};
         CREATE TEMP TABLE {EFFECTIVE_TAGS_ORDER_BACKUP_TABLE} (
             heading_id INTEGER PRIMARY KEY,
             all_tags_json TEXT NOT NULL
         );"
    ))?;

    if !table_exists(connection, "headings")? {
        return Ok(());
    }

    let columns = table_columns(connection, "headings")?;
    if has_column(&columns, "all_tags_json") {
        connection.execute_batch(&format!(
            "INSERT INTO temp.{EFFECTIVE_TAGS_ORDER_BACKUP_TABLE} (heading_id, all_tags_json)
             SELECT id, all_tags_json FROM headings;"
        ))?;
        return Ok(());
    }

    if !table_exists(connection, "effective_tags")? {
        return Ok(());
    }

    let mut statement = connection.prepare(
        "SELECT effective.heading_id, effective.tag
         FROM effective_tags AS effective
         INNER JOIN headings ON headings.id = effective.heading_id
         ORDER BY effective.heading_id, effective.position",
    )?;
    let rows = statement
        .query_map([], |row| {
            Ok((row.get::<_, i64>(0)?, row.get::<_, String>(1)?))
        })?
        .collect::<Result<Vec<_>, _>>()?;
    drop(statement);

    let mut current_heading_id = None;
    let mut current_tags = Vec::<String>::new();
    for (heading_id, tag) in rows {
        if let Some(current) = current_heading_id {
            if current != heading_id {
                insert_effective_tag_order_backup(connection, current, &current_tags)?;
                current_tags.clear();
            }
        }
        current_heading_id = Some(heading_id);
        current_tags.push(tag);
    }
    if let Some(heading_id) = current_heading_id {
        insert_effective_tag_order_backup(connection, heading_id, &current_tags)?;
    }

    Ok(())
}

fn insert_effective_tag_order_backup(
    connection: &Connection,
    heading_id: i64,
    tags: &[String],
) -> rusqlite::Result<()> {
    let tags_json = serde_json::to_string(tags).map_err(|error| {
        effective_tags_migration_error(format!(
            "failed to serialize effective tag order for heading {heading_id}: {error}"
        ))
    })?;
    connection.execute(
        &format!(
            "INSERT INTO temp.{EFFECTIVE_TAGS_ORDER_BACKUP_TABLE}
             (heading_id, all_tags_json) VALUES (?1, ?2)"
        ),
        rusqlite::params![heading_id, tags_json],
    )?;
    Ok(())
}

fn backfill_effective_tags(connection: &Connection) -> rusqlite::Result<()> {
    let mut file_statement =
        connection.prepare("SELECT DISTINCT file_id FROM headings ORDER BY file_id")?;
    let file_ids = file_statement
        .query_map([], |row| row.get::<_, i64>(0))?
        .collect::<Result<Vec<_>, _>>()?;
    drop(file_statement);

    for file_id in file_ids {
        backfill_effective_tags_for_file(connection, file_id)?;
    }

    let mismatch_count: i64 = connection.query_row(
        "SELECT COUNT(*) FROM effective_tags AS effective
         INNER JOIN headings ON headings.id = effective.heading_id
         WHERE effective.file_id != headings.file_id",
        [],
        |row| row.get(0),
    )?;
    if mismatch_count != 0 {
        return Err(effective_tags_migration_error(format!(
            "effective_tags backfill produced {mismatch_count} row(s) whose file_id does not match the owning heading"
        )));
    }

    let foreign_key_violations: i64 =
        connection.query_row("SELECT COUNT(*) FROM pragma_foreign_key_check", [], |row| {
            row.get(0)
        })?;
    if foreign_key_violations != 0 {
        return Err(effective_tags_migration_error(format!(
            "effective_tags backfill left {foreign_key_violations} foreign-key violation(s)"
        )));
    }

    Ok(())
}

fn backfill_effective_tags_for_file(connection: &Connection, file_id: i64) -> rusqlite::Result<()> {
    let mut heading_statement = connection.prepare(
        "SELECT id, parent_id, level
         FROM headings
         WHERE file_id = ?1
         ORDER BY id",
    )?;
    let heading_rows = heading_statement
        .query_map([file_id], |row| {
            Ok((
                row.get::<_, i64>(0)?,
                row.get::<_, Option<i64>>(1)?,
                row.get::<_, i64>(2)?,
            ))
        })?
        .collect::<Result<Vec<_>, _>>()?;
    drop(heading_statement);

    let root_ids = heading_rows
        .iter()
        .filter_map(|(heading_id, _, level)| (*level == 0).then_some(*heading_id))
        .collect::<Vec<_>>();
    if root_ids.len() != 1 {
        return Err(effective_tags_migration_error(format!(
            "file {file_id} must contain exactly one synthetic level-0 root before effective_tags can be backfilled; found {}",
            root_ids.len()
        )));
    }
    let root_id = root_ids[0];
    let parent_by_heading = heading_rows
        .iter()
        .map(|(heading_id, parent_id, _)| (*heading_id, *parent_id))
        .collect::<HashMap<_, _>>();

    for (heading_id, parent_id, level) in &heading_rows {
        if *heading_id == root_id {
            if parent_id.is_some() {
                return Err(effective_tags_migration_error(format!(
                    "file {file_id} root heading {root_id} must not have a parent"
                )));
            }
            continue;
        }
        if *level == 0 {
            return Err(effective_tags_migration_error(format!(
                "file {file_id} contains more than one synthetic level-0 root"
            )));
        }
        let Some(parent_id) = parent_id else {
            return Err(effective_tags_migration_error(format!(
                "file {file_id} heading {heading_id} is not the root but has no parent"
            )));
        };
        if !parent_by_heading.contains_key(parent_id) {
            let parent_file_id = connection
                .query_row(
                    "SELECT file_id FROM headings WHERE id = ?1",
                    [parent_id],
                    |row| row.get::<_, i64>(0),
                )
                .optional()?;
            let detail = match parent_file_id {
                Some(parent_file_id) => format!("belongs to file {parent_file_id}"),
                None => "does not exist".to_string(),
            };
            return Err(effective_tags_migration_error(format!(
                "file {file_id} heading {heading_id} references parent {parent_id}, which {detail}"
            )));
        }
    }

    for heading_id in parent_by_heading.keys().copied() {
        validate_heading_reaches_root(file_id, heading_id, root_id, &parent_by_heading)?;
    }
    validate_all_headings_reachable(file_id, root_id, &parent_by_heading)?;

    let mut tag_statement = connection.prepare(
        "SELECT tags.heading_id, tags.tag
         FROM tags
         INNER JOIN headings ON headings.id = tags.heading_id
         WHERE headings.file_id = ?1
         ORDER BY tags.heading_id, tags.tag",
    )?;
    let direct_tag_rows = tag_statement
        .query_map([file_id], |row| {
            Ok((row.get::<_, i64>(0)?, row.get::<_, String>(1)?))
        })?
        .collect::<Result<Vec<_>, _>>()?;
    drop(tag_statement);
    let mut direct_tags_by_heading = HashMap::<i64, Vec<String>>::new();
    for (heading_id, tag) in direct_tag_rows {
        direct_tags_by_heading
            .entry(heading_id)
            .or_default()
            .push(tag);
    }

    let derived = derive_effective_tags(&parent_by_heading, &direct_tags_by_heading);
    let mut derived_by_heading = HashMap::<i64, Vec<String>>::new();
    for row in derived {
        derived_by_heading
            .entry(row.heading_id)
            .or_default()
            .push(row.tag);
    }

    let mut preserved_order_by_heading = HashMap::<i64, Vec<String>>::new();
    let mut order_statement = connection.prepare(&format!(
        "SELECT backup.heading_id, backup.all_tags_json
         FROM temp.{EFFECTIVE_TAGS_ORDER_BACKUP_TABLE} AS backup
         INNER JOIN headings ON headings.id = backup.heading_id
         WHERE headings.file_id = ?1
         ORDER BY backup.heading_id"
    ))?;
    let order_rows = order_statement
        .query_map([file_id], |row| {
            Ok((row.get::<_, i64>(0)?, row.get::<_, String>(1)?))
        })?
        .collect::<Result<Vec<_>, _>>()?;
    drop(order_statement);

    for (heading_id, all_tags_json) in order_rows {
        let ordered_tags = serde_json::from_str::<Vec<String>>(&all_tags_json).map_err(|error| {
            effective_tags_migration_error(format!(
                "file {file_id} heading {heading_id} has invalid all_tags_json during effective_tags migration: {error}"
            ))
        })?;
        preserved_order_by_heading.insert(heading_id, ordered_tags);
    }

    let mut projected = Vec::new();
    for (heading_id, _, _) in &heading_rows {
        let derived_tags = derived_by_heading.remove(heading_id).unwrap_or_default();
        let expected = derived_tags
            .iter()
            .cloned()
            .collect::<std::collections::HashSet<_>>();
        if expected.len() != derived_tags.len() {
            return Err(effective_tags_migration_error(format!(
                "file {file_id} heading {heading_id} produced duplicate derived effective tags"
            )));
        }

        let ordered = if let Some(preserved) = preserved_order_by_heading.remove(heading_id) {
            let mut preserved_set = std::collections::HashSet::new();
            for tag in &preserved {
                if !preserved_set.insert(tag.clone()) {
                    return Err(effective_tags_migration_error(format!(
                        "file {file_id} heading {heading_id} has duplicate tag {tag:?} in legacy all_tags_json"
                    )));
                }
            }
            if preserved_set != expected {
                let missing = expected
                    .difference(&preserved_set)
                    .cloned()
                    .collect::<std::collections::BTreeSet<_>>();
                let unexpected = preserved_set
                    .difference(&expected)
                    .cloned()
                    .collect::<std::collections::BTreeSet<_>>();
                return Err(effective_tags_migration_error(format!(
                    "file {file_id} heading {heading_id} legacy all_tags_json does not match canonical tag inheritance; missing={missing:?}, unexpected={unexpected:?}"
                )));
            }
            preserved
        } else {
            derived_tags
        };

        for (position, tag) in ordered.into_iter().enumerate() {
            let position = i64::try_from(position).map_err(|_| {
                effective_tags_migration_error(format!(
                    "file {file_id} heading {heading_id} has too many effective tags"
                ))
            })?;
            projected.push(EffectiveTagRecord {
                heading_id: *heading_id,
                file_id,
                tag,
                position,
            });
        }
    }

    if !derived_by_heading.is_empty() || !preserved_order_by_heading.is_empty() {
        return Err(effective_tags_migration_error(format!(
            "file {file_id} effective tag backfill contained rows for unknown headings"
        )));
    }

    let projected_count = projected.len();
    DbWriter::insert_effective_tags(connection, &projected).map_err(|error| match error {
        crate::db::DbWriteError::Write { source, .. } => source,
        other => effective_tags_migration_error(format!(
            "failed to insert effective_tags for file {file_id}: {other}"
        )),
    })?;

    let stored_count: i64 = connection.query_row(
        "SELECT COUNT(*) FROM effective_tags WHERE file_id = ?1",
        [file_id],
        |row| row.get(0),
    )?;
    if stored_count != projected_count as i64 {
        return Err(effective_tags_migration_error(format!(
            "file {file_id} effective_tags backfill stored {stored_count} row(s), expected {projected_count}"
        )));
    }

    Ok(())
}

#[derive(Debug)]
struct EffectiveTagsMigrationError(String);

impl std::fmt::Display for EffectiveTagsMigrationError {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        formatter.write_str(&self.0)
    }
}

impl std::error::Error for EffectiveTagsMigrationError {}

fn effective_tags_migration_error(message: String) -> rusqlite::Error {
    rusqlite::Error::UserFunctionError(Box::new(EffectiveTagsMigrationError(message)))
}

fn backfill_effective_properties(connection: &Connection) -> rusqlite::Result<()> {
    let mut file_statement =
        connection.prepare("SELECT DISTINCT file_id FROM headings ORDER BY file_id")?;
    let file_ids = file_statement
        .query_map([], |row| row.get::<_, i64>(0))?
        .collect::<Result<Vec<_>, _>>()?;
    drop(file_statement);

    for file_id in file_ids {
        backfill_effective_properties_for_file(connection, file_id)?;
    }

    let mismatch_count: i64 = connection.query_row(
        "SELECT COUNT(*) FROM effective_properties AS effective
         INNER JOIN headings ON headings.id = effective.heading_id
         WHERE effective.file_id != headings.file_id",
        [],
        |row| row.get(0),
    )?;
    if mismatch_count != 0 {
        return Err(effective_properties_migration_error(format!(
            "effective_properties backfill produced {mismatch_count} row(s) whose file_id does not match the owning heading"
        )));
    }

    let foreign_key_violations: i64 =
        connection.query_row("SELECT COUNT(*) FROM pragma_foreign_key_check", [], |row| {
            row.get(0)
        })?;
    if foreign_key_violations != 0 {
        return Err(effective_properties_migration_error(format!(
            "effective_properties backfill left {foreign_key_violations} foreign-key violation(s)"
        )));
    }

    Ok(())
}

fn backfill_effective_properties_for_file(
    connection: &Connection,
    file_id: i64,
) -> rusqlite::Result<()> {
    let mut heading_statement = connection.prepare(
        "SELECT id, parent_id, level
         FROM headings
         WHERE file_id = ?1
         ORDER BY id",
    )?;
    let heading_rows = heading_statement
        .query_map([file_id], |row| {
            Ok((
                row.get::<_, i64>(0)?,
                row.get::<_, Option<i64>>(1)?,
                row.get::<_, i64>(2)?,
            ))
        })?
        .collect::<Result<Vec<_>, _>>()?;
    drop(heading_statement);

    let root_ids = heading_rows
        .iter()
        .filter_map(|(heading_id, _, level)| (*level == 0).then_some(*heading_id))
        .collect::<Vec<_>>();
    if root_ids.len() != 1 {
        return Err(effective_properties_migration_error(format!(
            "file {file_id} must contain exactly one synthetic level-0 root before effective_properties can be backfilled; found {}",
            root_ids.len()
        )));
    }
    let root_id = root_ids[0];

    let parent_by_heading = heading_rows
        .iter()
        .map(|(heading_id, parent_id, _)| (*heading_id, *parent_id))
        .collect::<HashMap<_, _>>();

    for (heading_id, parent_id, level) in &heading_rows {
        if *heading_id == root_id {
            if parent_id.is_some() {
                return Err(effective_properties_migration_error(format!(
                    "file {file_id} root heading {root_id} must not have a parent"
                )));
            }
            continue;
        }

        if *level == 0 {
            return Err(effective_properties_migration_error(format!(
                "file {file_id} contains more than one synthetic level-0 root"
            )));
        }

        let Some(parent_id) = parent_id else {
            return Err(effective_properties_migration_error(format!(
                "file {file_id} heading {heading_id} is not the root but has no parent"
            )));
        };

        if !parent_by_heading.contains_key(parent_id) {
            let parent_file_id = connection
                .query_row(
                    "SELECT file_id FROM headings WHERE id = ?1",
                    [parent_id],
                    |row| row.get::<_, i64>(0),
                )
                .optional()?;
            let detail = match parent_file_id {
                Some(parent_file_id) => {
                    format!("belongs to file {parent_file_id}")
                }
                None => "does not exist".to_string(),
            };
            return Err(effective_properties_migration_error(format!(
                "file {file_id} heading {heading_id} references parent {parent_id}, which {detail}"
            )));
        }
    }

    for heading_id in parent_by_heading.keys().copied() {
        validate_heading_reaches_root(file_id, heading_id, root_id, &parent_by_heading)?;
    }
    validate_all_headings_reachable(file_id, root_id, &parent_by_heading)?;

    let mut property_statement = connection.prepare(
        "SELECT properties.id, properties.heading_id, properties.key, properties.value,
                properties.append, properties.line_number
         FROM properties
         INNER JOIN headings ON headings.id = properties.heading_id
         WHERE headings.file_id = ?1
         ORDER BY properties.line_number, properties.id",
    )?;
    let property_rows = property_statement
        .query_map([file_id], |row| {
            Ok(PropertyRow {
                id: row.get(0)?,
                heading_id: row.get(1)?,
                key: row.get(2)?,
                value: row.get(3)?,
                append: row.get::<_, i64>(4)? != 0,
                line_number: row.get(5)?,
            })
        })?
        .collect::<Result<Vec<_>, _>>()?;
    drop(property_statement);

    let mut rows_by_heading = HashMap::<i64, Vec<PropertyRow>>::new();
    for row in property_rows {
        rows_by_heading.entry(row.heading_id).or_default().push(row);
    }

    let projected = derive_effective_properties(&parent_by_heading, &rows_by_heading)
        .into_iter()
        .map(|row| EffectivePropertyRecord {
            heading_id: row.heading_id,
            file_id,
            key: row.key,
            local_value: row.local_value,
            effective_value: row.effective_value,
        })
        .collect::<Vec<_>>();
    let projected_count = projected.len();

    DbWriter::insert_effective_properties(connection, &projected).map_err(|error| match error {
        crate::db::DbWriteError::Write { source, .. } => source,
        other => effective_properties_migration_error(format!(
            "failed to insert effective_properties for file {file_id}: {other}"
        )),
    })?;

    let stored_count: i64 = connection.query_row(
        "SELECT COUNT(*) FROM effective_properties WHERE file_id = ?1",
        [file_id],
        |row| row.get(0),
    )?;
    if stored_count != projected_count as i64 {
        return Err(effective_properties_migration_error(format!(
            "file {file_id} effective_properties backfill stored {stored_count} row(s), expected {projected_count}"
        )));
    }

    Ok(())
}

fn validate_heading_reaches_root(
    file_id: i64,
    heading_id: i64,
    root_id: i64,
    parent_by_heading: &HashMap<i64, Option<i64>>,
) -> rusqlite::Result<()> {
    let mut current = heading_id;
    let mut path = std::collections::HashSet::new();

    loop {
        if current == root_id {
            return Ok(());
        }
        if !path.insert(current) {
            return Err(effective_properties_migration_error(format!(
                "file {file_id} contains a parent cycle involving heading {current}"
            )));
        }

        let Some(parent_id) = parent_by_heading.get(&current) else {
            return Err(effective_properties_migration_error(format!(
                "file {file_id} heading {current} is unreachable from root {root_id}"
            )));
        };
        let Some(parent_id) = *parent_id else {
            return Err(effective_properties_migration_error(format!(
                "file {file_id} heading {heading_id} is unreachable from root {root_id}"
            )));
        };
        current = parent_id;
    }
}

fn validate_all_headings_reachable(
    file_id: i64,
    root_id: i64,
    parent_by_heading: &HashMap<i64, Option<i64>>,
) -> rusqlite::Result<()> {
    let mut children = HashMap::<i64, Vec<i64>>::new();
    for (&heading_id, &parent_id) in parent_by_heading {
        if let Some(parent_id) = parent_id {
            children.entry(parent_id).or_default().push(heading_id);
        }
    }

    let mut visited = std::collections::HashSet::new();
    let mut stack = vec![root_id];
    while let Some(heading_id) = stack.pop() {
        if !visited.insert(heading_id) {
            return Err(effective_properties_migration_error(format!(
                "file {file_id} visits heading {heading_id} more than once while validating the root tree"
            )));
        }
        if let Some(child_ids) = children.get(&heading_id) {
            stack.extend(child_ids.iter().copied());
        }
    }

    if visited.len() != parent_by_heading.len() {
        let mut unreachable = parent_by_heading
            .keys()
            .filter(|heading_id| !visited.contains(heading_id))
            .copied()
            .collect::<Vec<_>>();
        unreachable.sort_unstable();
        return Err(effective_properties_migration_error(format!(
            "file {file_id} has headings unreachable from root {root_id}: {unreachable:?}"
        )));
    }

    Ok(())
}

#[derive(Debug)]
struct EffectivePropertiesMigrationError(String);

impl std::fmt::Display for EffectivePropertiesMigrationError {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        formatter.write_str(&self.0)
    }
}

impl std::error::Error for EffectivePropertiesMigrationError {}

fn effective_properties_migration_error(message: String) -> rusqlite::Error {
    rusqlite::Error::UserFunctionError(Box::new(EffectivePropertiesMigrationError(message)))
}

#[cfg(test)]
mod effective_properties_migration_tests {
    use super::backfill_effective_properties;
    use rusqlite::Connection;

    fn migration_fixture() -> Connection {
        let connection = Connection::open_in_memory().expect("database should open");
        connection
            .execute_batch(
                "CREATE TABLE files (
                     id INTEGER PRIMARY KEY
                 );
                 CREATE TABLE headings (
                     id INTEGER PRIMARY KEY,
                     file_id INTEGER NOT NULL,
                     parent_id INTEGER,
                     level INTEGER NOT NULL
                 );
                 CREATE TABLE properties (
                     id INTEGER PRIMARY KEY,
                     heading_id INTEGER NOT NULL,
                     key TEXT NOT NULL,
                     value TEXT,
                     append INTEGER NOT NULL,
                     line_number INTEGER
                 );
                 CREATE TABLE effective_properties (
                     heading_id INTEGER NOT NULL,
                     file_id INTEGER NOT NULL,
                     key TEXT NOT NULL,
                     local_value TEXT,
                     effective_value TEXT NOT NULL,
                     PRIMARY KEY (heading_id, key)
                 );",
            )
            .expect("migration fixture schema should seed");
        connection
    }

    fn error_text(connection: &Connection) -> String {
        backfill_effective_properties(connection)
            .expect_err("backfill should reject malformed hierarchy")
            .to_string()
    }

    #[test]
    fn file_scoped_backfill_resolves_root_parent_and_local_properties() {
        let connection = migration_fixture();
        connection
            .execute_batch(
                "INSERT INTO files (id) VALUES (1);
                 INSERT INTO headings (id, file_id, parent_id, level) VALUES
                     (10, 1, NULL, 0),
                     (11, 1, 10, 1),
                     (12, 1, 11, 2);
                 INSERT INTO properties (id, heading_id, key, value, append, line_number) VALUES
                     (1, 10, 'category', 'work', 0, 1),
                     (2, 11, 'AREA', 'infra', 0, 2),
                     (3, 12, 'AREA+', 'ops', 1, 3),
                     (4, 12, 'EMPTY', '', 0, 4);",
            )
            .expect("migration facts should seed");

        backfill_effective_properties(&connection).expect("backfill should succeed");

        let inherited: (Option<String>, String) = connection
            .query_row(
                "SELECT local_value, effective_value
                 FROM effective_properties
                 WHERE heading_id = 12 AND key = 'CATEGORY'",
                [],
                |row| Ok((row.get(0)?, row.get(1)?)),
            )
            .expect("inherited category should load");
        assert_eq!(inherited, (None, "work".to_string()));

        let appended: (Option<String>, String) = connection
            .query_row(
                "SELECT local_value, effective_value
                 FROM effective_properties
                 WHERE heading_id = 12 AND key = 'AREA'",
                [],
                |row| Ok((row.get(0)?, row.get(1)?)),
            )
            .expect("appended property should load");
        assert_eq!(appended, (Some("ops".to_string()), "infra ops".to_string()));

        let empty: (Option<String>, String) = connection
            .query_row(
                "SELECT local_value, effective_value
                 FROM effective_properties
                 WHERE heading_id = 12 AND key = 'EMPTY'",
                [],
                |row| Ok((row.get(0)?, row.get(1)?)),
            )
            .expect("empty property should load");
        assert_eq!(empty, (Some(String::new()), String::new()));
    }

    #[test]
    fn file_scoped_backfill_rejects_cross_file_parent() {
        let connection = migration_fixture();
        connection
            .execute_batch(
                "INSERT INTO files (id) VALUES (1), (2);
                 INSERT INTO headings (id, file_id, parent_id, level) VALUES
                     (10, 1, NULL, 0),
                     (20, 2, NULL, 0),
                     (11, 1, 20, 1);",
            )
            .expect("cross-file fixture should seed");

        let error = error_text(&connection);
        assert!(
            error.contains("belongs to file 2"),
            "unexpected error: {error}"
        );
    }

    #[test]
    fn file_scoped_backfill_rejects_missing_parent() {
        let connection = migration_fixture();
        connection
            .execute_batch(
                "INSERT INTO files (id) VALUES (1);
                 INSERT INTO headings (id, file_id, parent_id, level) VALUES
                     (10, 1, NULL, 0),
                     (11, 1, 999, 1);",
            )
            .expect("missing-parent fixture should seed");

        let error = error_text(&connection);
        assert!(
            error.contains("does not exist"),
            "unexpected error: {error}"
        );
    }

    #[test]
    fn file_scoped_backfill_rejects_parent_cycle() {
        let connection = migration_fixture();
        connection
            .execute_batch(
                "INSERT INTO files (id) VALUES (1);
                 INSERT INTO headings (id, file_id, parent_id, level) VALUES
                     (10, 1, NULL, 0),
                     (11, 1, 12, 1),
                     (12, 1, 11, 2);",
            )
            .expect("cycle fixture should seed");

        let error = error_text(&connection);
        assert!(error.contains("parent cycle"), "unexpected error: {error}");
    }

    #[test]
    fn file_scoped_backfill_rejects_multiple_roots() {
        let connection = migration_fixture();
        connection
            .execute_batch(
                "INSERT INTO files (id) VALUES (1);
                 INSERT INTO headings (id, file_id, parent_id, level) VALUES
                     (10, 1, NULL, 0),
                     (11, 1, NULL, 0);",
            )
            .expect("multiple-root fixture should seed");

        let error = error_text(&connection);
        assert!(error.contains("exactly one"), "unexpected error: {error}");
    }
}

#[cfg(test)]
mod effective_tags_migration_tests {
    use super::{
        backfill_effective_tags, prepare_effective_tags_order_backup,
        EFFECTIVE_TAGS_ORDER_BACKUP_TABLE,
    };
    use rusqlite::Connection;

    fn migration_fixture() -> Connection {
        let connection = Connection::open_in_memory().expect("database should open");
        connection
            .execute_batch(
                "CREATE TABLE files (
                     id INTEGER PRIMARY KEY
                 );
                 CREATE TABLE headings (
                     id INTEGER PRIMARY KEY,
                     file_id INTEGER NOT NULL,
                     parent_id INTEGER,
                     level INTEGER NOT NULL,
                     all_tags_json TEXT NOT NULL DEFAULT '[]'
                 );
                 CREATE TABLE tags (
                     heading_id INTEGER NOT NULL,
                     tag TEXT NOT NULL,
                     PRIMARY KEY (heading_id, tag)
                 );
                 CREATE TABLE effective_tags (
                     heading_id INTEGER NOT NULL,
                     file_id INTEGER NOT NULL,
                     tag TEXT NOT NULL,
                     position INTEGER NOT NULL,
                     PRIMARY KEY (heading_id, tag),
                     UNIQUE (heading_id, position)
                 );",
            )
            .expect("migration fixture schema should seed");
        connection
    }

    fn run_backfill(connection: &Connection) -> rusqlite::Result<()> {
        prepare_effective_tags_order_backup(connection)?;
        backfill_effective_tags(connection)
    }

    fn error_text(connection: &Connection) -> String {
        run_backfill(connection)
            .expect_err("backfill should reject malformed hierarchy")
            .to_string()
    }

    #[test]
    fn order_backup_uses_current_effective_tags_when_heading_json_is_absent() {
        let connection = Connection::open_in_memory().expect("database should open");
        connection
            .execute_batch(
                "CREATE TABLE headings (
                     id INTEGER PRIMARY KEY,
                     file_id INTEGER NOT NULL,
                     parent_id INTEGER,
                     level INTEGER NOT NULL
                 );
                 CREATE TABLE effective_tags (
                     heading_id INTEGER NOT NULL,
                     file_id INTEGER NOT NULL,
                     tag TEXT NOT NULL,
                     position INTEGER NOT NULL,
                     PRIMARY KEY (heading_id, tag),
                     UNIQUE (heading_id, position)
                 );
                 INSERT INTO headings (id, file_id, parent_id, level) VALUES
                     (10, 1, NULL, 0),
                     (11, 1, 10, 1);
                 INSERT INTO effective_tags (heading_id, file_id, tag, position) VALUES
                     (10, 1, 'zeta', 0),
                     (10, 1, 'alpha', 1),
                     (11, 1, 'zeta', 0),
                     (11, 1, 'alpha', 1),
                     (11, 1, 'child', 2);",
            )
            .expect("current effective-tag fixture should seed");

        prepare_effective_tags_order_backup(&connection)
            .expect("current effective tag order should back up");
        let child_order: String = connection
            .query_row(
                &format!(
                    "SELECT all_tags_json
                     FROM temp.{EFFECTIVE_TAGS_ORDER_BACKUP_TABLE}
                     WHERE heading_id = 11"
                ),
                [],
                |row| row.get(0),
            )
            .expect("backed-up order should load");
        assert_eq!(child_order, r#"["zeta","alpha","child"]"#);
    }

    #[test]
    fn file_scoped_backfill_preserves_visible_order_and_deduplicates_tags() {
        let connection = migration_fixture();
        connection
            .execute_batch(
                "INSERT INTO files (id) VALUES (1);
                 INSERT INTO headings (id, file_id, parent_id, level, all_tags_json) VALUES
                     (10, 1, NULL, 0, '[\"file\",\"shared\"]'),
                     (11, 1, 10, 1, '[\"file\",\"shared\",\"parent\"]'),
                     (12, 1, 11, 2, '[\"file\",\"shared\",\"parent\",\"child\"]');
                 INSERT INTO tags (heading_id, tag) VALUES
                     (10, 'file'),
                     (10, 'shared'),
                     (11, 'parent'),
                     (11, 'shared'),
                     (12, 'child'),
                     (12, 'file');",
            )
            .expect("migration facts should seed");

        run_backfill(&connection).expect("backfill should succeed");

        let mut statement = connection
            .prepare(
                "SELECT tag, position
                 FROM effective_tags
                 WHERE heading_id = 12
                 ORDER BY position",
            )
            .expect("effective tag query should prepare");
        let rows = statement
            .query_map([], |row| {
                Ok((row.get::<_, String>(0)?, row.get::<_, i64>(1)?))
            })
            .expect("effective tag query should run")
            .collect::<Result<Vec<_>, _>>()
            .expect("effective tag rows should decode");
        assert_eq!(
            rows,
            vec![
                ("file".to_string(), 0),
                ("shared".to_string(), 1),
                ("parent".to_string(), 2),
                ("child".to_string(), 3),
            ]
        );
    }

    #[test]
    fn file_scoped_backfill_rejects_cross_file_parent() {
        let connection = migration_fixture();
        connection
            .execute_batch(
                "INSERT INTO files (id) VALUES (1), (2);
                 INSERT INTO headings (id, file_id, parent_id, level) VALUES
                     (10, 1, NULL, 0),
                     (20, 2, NULL, 0),
                     (11, 1, 20, 1);",
            )
            .expect("cross-file fixture should seed");

        let error = error_text(&connection);
        assert!(
            error.contains("belongs to file 2"),
            "unexpected error: {error}"
        );
    }

    #[test]
    fn file_scoped_backfill_rejects_missing_parent() {
        let connection = migration_fixture();
        connection
            .execute_batch(
                "INSERT INTO files (id) VALUES (1);
                 INSERT INTO headings (id, file_id, parent_id, level) VALUES
                     (10, 1, NULL, 0),
                     (11, 1, 999, 1);",
            )
            .expect("missing-parent fixture should seed");

        let error = error_text(&connection);
        assert!(
            error.contains("does not exist"),
            "unexpected error: {error}"
        );
    }

    #[test]
    fn file_scoped_backfill_rejects_parent_cycle() {
        let connection = migration_fixture();
        connection
            .execute_batch(
                "INSERT INTO files (id) VALUES (1);
                 INSERT INTO headings (id, file_id, parent_id, level) VALUES
                     (10, 1, NULL, 0),
                     (11, 1, 12, 1),
                     (12, 1, 11, 2);",
            )
            .expect("cycle fixture should seed");

        let error = error_text(&connection);
        assert!(error.contains("parent cycle"), "unexpected error: {error}");
    }

    #[test]
    fn file_scoped_backfill_rejects_multiple_roots() {
        let connection = migration_fixture();
        connection
            .execute_batch(
                "INSERT INTO files (id) VALUES (1);
                 INSERT INTO headings (id, file_id, parent_id, level) VALUES
                     (10, 1, NULL, 0),
                     (11, 1, NULL, 0);",
            )
            .expect("multiple-root fixture should seed");

        let error = error_text(&connection);
        assert!(error.contains("exactly one"), "unexpected error: {error}");
    }

    #[test]
    fn invalid_legacy_tag_order_aborts_backfill() {
        let connection = migration_fixture();
        connection
            .execute_batch(
                "INSERT INTO files (id) VALUES (1);
                 INSERT INTO headings (id, file_id, parent_id, level, all_tags_json) VALUES
                     (10, 1, NULL, 0, 'not-json');
                 INSERT INTO tags (heading_id, tag) VALUES (10, 'file');",
            )
            .expect("invalid order fixture should seed");

        let error = run_backfill(&connection)
            .expect_err("invalid stored tag order must abort migration")
            .to_string();
        assert!(
            error.contains("invalid all_tags_json"),
            "unexpected error: {error}"
        );
    }

    #[test]
    fn mismatched_legacy_tag_set_aborts_backfill() {
        let connection = migration_fixture();
        connection
            .execute_batch(
                r#"INSERT INTO files (id) VALUES (1);
                 INSERT INTO headings (id, file_id, parent_id, level, all_tags_json) VALUES
                     (10, 1, NULL, 0, '["unexpected"]');
                 INSERT INTO tags (heading_id, tag) VALUES (10, 'file');"#,
            )
            .expect("mismatched order fixture should seed");

        let error = run_backfill(&connection)
            .expect_err("mismatched stored tag set must abort migration")
            .to_string();
        assert!(
            error.contains("does not match canonical tag inheritance"),
            "unexpected error: {error}"
        );
        assert!(
            error.contains("file"),
            "missing tag should be reported: {error}"
        );
        assert!(
            error.contains("unexpected"),
            "unexpected tag should be reported: {error}"
        );
    }

    #[test]
    fn duplicate_legacy_tag_order_aborts_backfill() {
        let connection = migration_fixture();
        connection
            .execute_batch(
                r#"INSERT INTO files (id) VALUES (1);
                 INSERT INTO headings (id, file_id, parent_id, level, all_tags_json) VALUES
                     (10, 1, NULL, 0, '["file","file"]');
                 INSERT INTO tags (heading_id, tag) VALUES (10, 'file');"#,
            )
            .expect("duplicate order fixture should seed");

        let error = run_backfill(&connection)
            .expect_err("duplicate stored tag order must abort migration")
            .to_string();
        assert!(error.contains("duplicate tag"), "unexpected error: {error}");
    }
}

fn migrate_v8_index_set(connection: &Connection) -> rusqlite::Result<()> {
    connection.execute_batch(
        r#"
DROP INDEX IF EXISTS idx_tags_heading;
DROP INDEX IF EXISTS idx_keywords_heading;
DROP INDEX IF EXISTS idx_timestamp_repeaters_timestamp_id;
"#,
    )
}

fn migrate_legacy_files_identity(connection: &Connection) -> rusqlite::Result<()> {
    if !table_exists(connection, "files")? {
        return Ok(());
    }

    let columns = table_columns(connection, "files")?;
    if !columns.iter().any(|column| column == "identity") {
        connection.execute_batch("ALTER TABLE files ADD COLUMN identity BLOB;")?;
    }

    // A legacy TEXT path cannot prove its original native bytes. Leave it NULL
    // until a successful discovery observes and writes a tagged identity.
    connection.execute_batch(
        "CREATE UNIQUE INDEX IF NOT EXISTS files_identity_unique
         ON files(identity) WHERE identity IS NOT NULL;",
    )
}

#[cfg(test)]
mod file_identity_migration_tests {
    use super::{SchemaDefinition, CURRENT_SCHEMA_VERSION};
    use rusqlite::Connection;

    fn assert_files_identity_index_contract(connection: &Connection) {
        let mut statement = connection
            .prepare("PRAGMA index_list('files')")
            .expect("files index list should prepare");
        let index_names = statement
            .query_map([], |row| row.get::<_, String>(1))
            .expect("files index list should query")
            .collect::<Result<Vec<_>, _>>()
            .expect("files index names should decode");
        drop(statement);

        let identity_indexes = index_names
            .iter()
            .filter(|name| {
                let mut index_info = connection
                    .prepare(&format!("PRAGMA index_info('{name}')"))
                    .expect("index info should prepare");
                index_info
                    .query_map([], |row| row.get::<_, Option<String>>(2))
                    .expect("index info should query")
                    .collect::<Result<Vec<_>, _>>()
                    .expect("index columns should decode")
                    .iter()
                    .flatten()
                    .any(|column| column == "identity")
            })
            .cloned()
            .collect::<Vec<_>>();
        assert_eq!(identity_indexes, vec!["files_identity_unique"]);

        let mut identity_statement = connection
            .prepare("PRAGMA index_info('files_identity_unique')")
            .expect("identity index info should prepare");
        let identity_columns = identity_statement
            .query_map([], |row| row.get::<_, String>(2))
            .expect("identity index info should query")
            .collect::<Result<Vec<_>, _>>()
            .expect("identity index columns should decode");
        assert_eq!(identity_columns, vec!["identity"]);
    }

    #[test]
    fn fresh_files_schema_has_one_named_partial_identity_index() {
        let connection = Connection::open_in_memory().expect("database should open");
        SchemaDefinition::new(CURRENT_SCHEMA_VERSION, false)
            .apply(&connection)
            .expect("schema should apply");

        assert_files_identity_index_contract(&connection);
    }

    #[test]
    fn legacy_files_keep_null_identity_during_schema_application() {
        let connection = Connection::open_in_memory().expect("database should open");
        connection
            .execute_batch(
                "CREATE TABLE files (
                     id INTEGER PRIMARY KEY,
                     path TEXT NOT NULL UNIQUE,
                     mtime_ns INTEGER NOT NULL,
                     size INTEGER NOT NULL,
                     content_hash TEXT,
                     indexed_at INTEGER
                 );
                 INSERT INTO files (path, mtime_ns, size) VALUES ('/tmp/notes.org', 1, 2);",
            )
            .expect("legacy files table should seed");

        SchemaDefinition::new(CURRENT_SCHEMA_VERSION, false)
            .apply(&connection)
            .expect("schema should migrate legacy files");

        let identity: Option<Vec<u8>> = connection
            .query_row(
                "SELECT identity FROM files WHERE path = '/tmp/notes.org'",
                [],
                |row| row.get(0),
            )
            .expect("identity should load");
        assert_eq!(identity, None);
        assert_files_identity_index_contract(&connection);
    }
}

impl Default for SchemaDefinition {
    fn default() -> Self {
        Self {
            version: CURRENT_SCHEMA_VERSION,
            enable_fts: true,
        }
    }
}

pub fn sqlite_supports_fts5(connection: &Connection) -> rusqlite::Result<bool> {
    let probe_id = SQLITE_FTS5_PROBE_COUNTER.fetch_add(1, Ordering::Relaxed);
    let savepoint_name = format!("org_files_db_fts5_probe_sp_{probe_id}");
    let table_name = format!("org_files_db_fts5_probe_vt_{probe_id}");

    connection.execute_batch(&format!("SAVEPOINT {savepoint_name};"))?;
    let probe_result = connection.execute_batch(&format!(
        "CREATE VIRTUAL TABLE temp.{table_name}
         USING fts5(
             title,
             body,
             tokenize = 'unicode61',
             content = ''
         );"
    ));
    let cleanup_result = connection.execute_batch(&format!(
        "ROLLBACK TO {savepoint_name}; RELEASE {savepoint_name};"
    ));

    match (probe_result, cleanup_result) {
        (Ok(()), Ok(())) => Ok(true),
        (Err(error), Ok(())) if is_expected_fts5_unavailable_error(&error) => Ok(false),
        (Err(error), Ok(())) => Err(error),
        (_, Err(cleanup_error)) => Err(cleanup_error),
    }
}

pub(crate) fn heading_fts_sql() -> &'static str {
    HEADING_FTS_SQL
}

fn is_expected_fts5_unavailable_error(error: &rusqlite::Error) -> bool {
    let Some(message) = sqlite_error_message(error) else {
        return false;
    };

    message.contains("no such module: fts5")
        || message.contains("no such module: fts")
        || message.contains("unknown tokenizer")
        || message.contains("unrecognized option")
}

fn sqlite_error_message(error: &rusqlite::Error) -> Option<&str> {
    match error {
        rusqlite::Error::SqliteFailure(_, Some(message)) => Some(message.as_str()),
        _ => None,
    }
}

fn migrate_legacy_timestamp_repeaters(connection: &Connection) -> rusqlite::Result<()> {
    if !table_exists(connection, "timestamp_repeaters")? {
        return Ok(());
    }

    let columns = table_columns(connection, "timestamp_repeaters")?;
    let has_kind = columns.iter().any(|column| column == "kind");
    let has_repeater_deadline_value = columns
        .iter()
        .any(|column| column == "repeater_deadline_value");
    let has_repeater_deadline_unit = columns
        .iter()
        .any(|column| column == "repeater_deadline_unit");
    let has_deadline_value = columns.iter().any(|column| column == "deadline_value");
    let has_deadline_unit = columns.iter().any(|column| column == "deadline_unit");

    if timestamp_repeaters_uses_explicit_columns(&columns) {
        return Ok(());
    }

    connection.execute_batch(
        r#"
ALTER TABLE timestamp_repeaters RENAME TO timestamp_repeaters_legacy;
"#,
    )?;
    connection.execute_batch(TIMESTAMP_REPEATERS_TABLE_SQL)?;
    let repeater_type_expr = if has_kind {
        "MAX(CASE WHEN kind = 'repeater' THEN type ELSE NULL END)"
    } else {
        "MAX(CASE WHEN type IN ('cumulate', 'catch_up', 'restart') THEN type ELSE NULL END)"
    };
    let repeater_value_expr = if has_kind {
        "MAX(CASE WHEN kind = 'repeater' THEN value ELSE NULL END)"
    } else {
        "MAX(CASE WHEN type IN ('cumulate', 'catch_up', 'restart') THEN value ELSE NULL END)"
    };
    let repeater_unit_expr = if has_kind {
        "MAX(CASE WHEN kind = 'repeater' THEN unit ELSE NULL END)"
    } else {
        "MAX(CASE WHEN type IN ('cumulate', 'catch_up', 'restart') THEN unit ELSE NULL END)"
    };
    let warning_type_expr = if has_kind {
        "MAX(CASE WHEN kind = 'warning' THEN type ELSE NULL END)"
    } else {
        "NULL"
    };
    let warning_value_expr = if has_kind {
        "MAX(CASE WHEN kind = 'warning' THEN value ELSE NULL END)"
    } else {
        "NULL"
    };
    let warning_unit_expr = if has_kind {
        "MAX(CASE WHEN kind = 'warning' THEN unit ELSE NULL END)"
    } else {
        "NULL"
    };
    let repeater_deadline_value_expr = match (has_repeater_deadline_value, has_deadline_value) {
        (true, true) => "MAX(COALESCE(repeater_deadline_value, deadline_value))",
        (true, false) => "MAX(repeater_deadline_value)",
        (false, true) => "MAX(deadline_value)",
        (false, false) => "NULL",
    };
    let repeater_deadline_unit_expr = match (has_repeater_deadline_unit, has_deadline_unit) {
        (true, true) => "MAX(COALESCE(repeater_deadline_unit, deadline_unit))",
        (true, false) => "MAX(repeater_deadline_unit)",
        (false, true) => "MAX(deadline_unit)",
        (false, false) => "NULL",
    };
    let migration_sql = format!(
        r#"
INSERT INTO timestamp_repeaters (
    id,
    timestamp_id,
    repeater_type,
    repeater_value,
    repeater_unit,
    repeater_deadline_value,
    repeater_deadline_unit,
    warning_type,
    warning_value,
    warning_unit
)
SELECT
    MIN(id),
    timestamp_id,
    {repeater_type_expr} AS repeater_type,
    {repeater_value_expr} AS repeater_value,
    {repeater_unit_expr} AS repeater_unit,
    {repeater_deadline_value_expr} AS repeater_deadline_value,
    {repeater_deadline_unit_expr} AS repeater_deadline_unit,
    {warning_type_expr} AS warning_type,
    {warning_value_expr} AS warning_value,
    {warning_unit_expr} AS warning_unit
FROM timestamp_repeaters_legacy
GROUP BY timestamp_id;

DROP TABLE timestamp_repeaters_legacy;
"#,
    );
    connection.execute_batch(&migration_sql)?;

    Ok(())
}

fn timestamp_repeaters_uses_explicit_columns(columns: &[String]) -> bool {
    [
        "timestamp_id",
        "repeater_type",
        "repeater_value",
        "repeater_unit",
        "repeater_deadline_value",
        "repeater_deadline_unit",
        "warning_type",
        "warning_value",
        "warning_unit",
    ]
    .iter()
    .all(|required| columns.iter().any(|column| column == required))
}

fn migrate_legacy_properties_table(connection: &Connection) -> rusqlite::Result<()> {
    if !table_exists(connection, "properties")? {
        return Ok(());
    }

    let columns = table_columns(connection, "properties")?;
    if properties_table_uses_append_column(&columns) {
        return Ok(());
    }

    connection.execute_batch(
        r#"
ALTER TABLE properties RENAME TO properties_legacy;
"#,
    )?;
    connection.execute_batch(PROPERTIES_TABLE_SQL)?;
    connection.execute_batch(
        r#"
INSERT INTO properties (id, heading_id, key, value, source, append, line_number)
SELECT id, heading_id, key, value, source, 0, line_number
FROM properties_legacy;

DROP TABLE properties_legacy;
"#,
    )?;

    Ok(())
}

fn migrate_legacy_todo_keywords_table(connection: &Connection) -> rusqlite::Result<()> {
    if !table_exists(connection, "todo_keywords")? {
        return Ok(());
    }

    let columns = table_columns(connection, "todo_keywords")?;
    if todo_keywords_table_uses_provenance_columns(&columns) {
        return Ok(());
    }

    connection.execute_batch(
        r#"
ALTER TABLE todo_keywords RENAME TO todo_keywords_legacy;
"#,
    )?;
    connection.execute_batch(TODO_KEYWORDS_TABLE_SQL)?;
    connection.execute_batch(
        r#"
INSERT INTO todo_keywords (
    file_id,
    keyword,
    state_type,
    shortcut,
    sequence_no,
    source_kind,
    source_keyword,
    source_line_number
)
SELECT
    file_id,
    keyword,
    state_type,
    shortcut,
    sequence_no,
    'config_default',
    NULL,
    NULL
FROM todo_keywords_legacy;

DROP TABLE todo_keywords_legacy;
"#,
    )?;

    Ok(())
}

fn todo_keywords_table_uses_provenance_columns(columns: &[String]) -> bool {
    [
        "file_id",
        "keyword",
        "state_type",
        "shortcut",
        "sequence_no",
        "source_kind",
        "source_keyword",
        "source_line_number",
    ]
    .iter()
    .all(|required| columns.iter().any(|column| column == required))
}

fn properties_table_uses_append_column(columns: &[String]) -> bool {
    columns.iter().any(|column| column == "append")
        && !columns.iter().any(|column| column == "inherited")
}

fn migrate_legacy_tags_table(connection: &Connection) -> rusqlite::Result<bool> {
    if !table_exists(connection, "tags")? {
        return Ok(false);
    }

    let columns = table_columns(connection, "tags")?;
    if tags_table_uses_direct_facts(&columns) {
        return Ok(false);
    }

    connection.execute_batch(
        r#"
ALTER TABLE tags RENAME TO tags_legacy;
"#,
    )?;
    connection.execute_batch(TAGS_TABLE_SQL)?;
    connection.execute_batch(
        r#"
INSERT INTO tags (heading_id, tag)
SELECT DISTINCT heading_id, tag
FROM tags_legacy
WHERE inherited = 0;

DROP TABLE tags_legacy;
"#,
    )?;

    Ok(true)
}

fn tags_table_uses_direct_facts(columns: &[String]) -> bool {
    columns.len() == 2
        && columns.iter().any(|column| column == "heading_id")
        && columns.iter().any(|column| column == "tag")
}

fn migrate_legacy_headings_table(connection: &Connection) -> rusqlite::Result<bool> {
    if !table_exists(connection, "headings")? {
        return Ok(false);
    }

    let columns = table_columns(connection, "headings")?;
    if headings_table_matches_current_contract(connection, &columns)? {
        return Ok(false);
    }

    // Derived projections must be recreated after a headings-table rebuild so
    // SQLite cannot retarget their foreign keys to headings_legacy.
    drop_table_if_exists(connection, "effective_properties")?;
    drop_table_if_exists(connection, "effective_tags")?;

    connection.execute_batch(
        r#"
ALTER TABLE headings RENAME TO headings_legacy;
"#,
    )?;
    drop_indexes_for_table(connection, "headings_legacy")?;
    let dependent_backups = collect_headings_repair_backups(connection, true)?;
    connection.execute_batch(CORE_SCHEMA_SQL)?;
    let scheduled_has_time_expr = if has_column(&columns, "scheduled_has_time") {
        "scheduled_has_time"
    } else {
        "NULL"
    };
    let deadline_has_time_expr = if has_column(&columns, "deadline_has_time") {
        "deadline_has_time"
    } else {
        "NULL"
    };
    let closed_has_time_expr = if has_column(&columns, "closed_has_time") {
        "closed_has_time"
    } else {
        "NULL"
    };

    connection.execute_batch(&format!(
        r#"
INSERT INTO headings (
    id,
    file_id,
    parent_id,
    level,
    line_number,
    byte_start,
    byte_end,
    title,
    title_raw,
    todo_keyword,
    todo_type,
    priority,
    scheduled_raw,
    scheduled_ts,
    scheduled_has_time,
    deadline_raw,
    deadline_ts,
    deadline_has_time,
    closed_raw,
    closed_ts,
    closed_has_time,
    archivedp,
    footnote_section_p
)
SELECT
    id,
    file_id,
    parent_id,
    level,
    line_number,
    byte_start,
    byte_end,
    title,
    CASE
        WHEN level = 0 AND title = title_raw THEN NULL
        ELSE title_raw
    END,
    todo_keyword,
    todo_type,
    priority,
    scheduled_raw,
    scheduled_ts,
    {scheduled_has_time_expr},
    deadline_raw,
    deadline_ts,
    {deadline_has_time_expr},
    closed_raw,
    closed_ts,
    {closed_has_time_expr},
    archivedp,
    footnote_section_p
FROM headings_legacy;
"#,
    ))?;
    restore_headings_dependent_tables(connection, &dependent_backups)?;
    rebuild_outline_path(connection)?;
    invalidate_search_trust_metadata(connection)?;
    drop_table_if_exists(connection, "headings_legacy")?;
    drop_backed_up_tables(connection, &dependent_backups)?;

    Ok(true)
}

fn headings_table_matches_current_contract(
    connection: &Connection,
    columns: &[String],
) -> rusqlite::Result<bool> {
    if !columns.iter().any(|column| column == "title_raw") {
        return Ok(false);
    }
    if !has_column(columns, "scheduled_has_time")
        || !has_column(columns, "deadline_has_time")
        || !has_column(columns, "closed_has_time")
    {
        return Ok(false);
    }
    if has_column(columns, "all_tags_json") {
        return Ok(false);
    }

    let table_sql: String = connection.query_row(
        "SELECT sql FROM sqlite_master WHERE type = 'table' AND name = 'headings'",
        [],
        |row| row.get(0),
    )?;
    if table_sql.contains("length(priority) = 1") {
        return Ok(false);
    }

    let mut statement = connection.prepare("PRAGMA table_info(headings)")?;
    let rows = statement.query_map([], |row| {
        Ok((row.get::<_, String>(1)?, row.get::<_, i64>(3)?))
    })?;

    for row in rows {
        let (name, not_null) = row?;
        if name == "title_raw" {
            return Ok(not_null == 0);
        }
    }

    Ok(false)
}

fn migrate_legacy_timestamps_table(connection: &Connection) -> rusqlite::Result<()> {
    if !table_exists(connection, "timestamps")? {
        return Ok(());
    }

    let columns = table_columns(connection, "timestamps")?;
    if timestamps_table_matches_current_contract(&columns) {
        return Ok(());
    }

    connection.execute_batch(
        r#"
ALTER TABLE timestamps RENAME TO timestamps_legacy;
"#,
    )?;
    drop_indexes_for_table(connection, "timestamps_legacy")?;
    connection.execute_batch(CORE_SCHEMA_SQL)?;
    let has_time_expr = if has_column(&columns, "has_time") {
        "has_time"
    } else {
        "NULL"
    };

    connection.execute_batch(&format!(
        r#"
INSERT INTO timestamps (
    id,
    heading_id,
    role,
    has_time,
    start_ts,
    end_ts,
    type,
    range_type,
    raw_value,
    byte_start,
    byte_end,
    line_number
)
SELECT
    id,
    heading_id,
    role,
    {has_time_expr},
    start_ts,
    end_ts,
    type,
    range_type,
    raw_value,
    byte_start,
    byte_end,
    line_number
FROM timestamps_legacy;

DROP TABLE timestamps_legacy;
"#,
    ))?;

    Ok(())
}

fn timestamps_table_matches_current_contract(columns: &[String]) -> bool {
    has_column(columns, "has_time") && !columns.iter().any(|column| column == "has_repeater")
}

fn migrate_legacy_links_table(connection: &Connection) -> rusqlite::Result<()> {
    if !table_exists(connection, "links")? {
        return Ok(());
    }

    let columns = table_columns(connection, "links")?;
    if links_table_matches_current_contract(&columns) {
        return Ok(());
    }

    let migration_sql = render_links_migration_sql(&columns);
    connection.execute_batch(
        r#"
ALTER TABLE links RENAME TO links_legacy;
"#,
    )?;
    drop_indexes_for_table(connection, "links_legacy")?;
    connection.execute_batch(CORE_SCHEMA_SQL)?;
    connection.execute_batch(&migration_sql)?;
    ensure_links_indexes(connection)?;
    drop_table_if_exists(connection, "links_legacy")?;

    Ok(())
}

fn table_exists(connection: &Connection, table_name: &str) -> rusqlite::Result<bool> {
    connection.query_row(
        "SELECT EXISTS(
             SELECT 1
             FROM sqlite_master
             WHERE type = 'table' AND name = ?1
         )",
        [table_name],
        |row| row.get(0),
    )
}

fn table_columns(connection: &Connection, table_name: &str) -> rusqlite::Result<Vec<String>> {
    let mut statement = connection.prepare(&format!("PRAGMA table_info({table_name})"))?;
    let columns = statement
        .query_map([], |row| row.get(1))?
        .collect::<Result<Vec<String>, _>>()?;
    Ok(columns)
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum HeadingsDependentTable {
    Keywords,
    Properties,
    Tags,
    Timestamps,
    TimestampRepeaters,
    HeadingBodies,
    Links,
    OutlinePath,
}

impl HeadingsDependentTable {
    fn table_name(self) -> &'static str {
        match self {
            Self::Keywords => "keywords",
            Self::Properties => "properties",
            Self::Tags => "tags",
            Self::Timestamps => "timestamps",
            Self::TimestampRepeaters => "timestamp_repeaters",
            Self::HeadingBodies => "heading_bodies",
            Self::Links => "links",
            Self::OutlinePath => "outline_path",
        }
    }

    fn backup_table_name(self) -> &'static str {
        match self {
            Self::Keywords => "keywords_headings_repair_backup",
            Self::Properties => "properties_headings_repair_backup",
            Self::Tags => "tags_headings_repair_backup",
            Self::Timestamps => "timestamps_headings_repair_backup",
            Self::TimestampRepeaters => "timestamp_repeaters_headings_repair_backup",
            Self::HeadingBodies => "heading_bodies_headings_repair_backup",
            Self::Links => "links_headings_repair_backup",
            Self::OutlinePath => "outline_path_headings_repair_backup",
        }
    }
}

#[derive(Debug, Default)]
struct RebuiltDerivedProjections {
    effective_properties: bool,
    effective_tags: bool,
}

fn repair_tables_depending_on_headings(
    connection: &Connection,
) -> rusqlite::Result<RebuiltDerivedProjections> {
    let repairs = collect_headings_repair_backups(connection, false)?;
    let rebuild_effective_properties = table_exists(connection, "effective_properties")?
        && table_references(connection, "effective_properties", "headings_legacy")?;
    let rebuild_effective_tags = table_exists(connection, "effective_tags")?
        && table_references(connection, "effective_tags", "headings_legacy")?;

    if repairs.is_empty() && !rebuild_effective_properties && !rebuild_effective_tags {
        return Ok(RebuiltDerivedProjections::default());
    }

    if rebuild_effective_properties {
        drop_table_if_exists(connection, "effective_properties")?;
    }
    if rebuild_effective_tags {
        drop_table_if_exists(connection, "effective_tags")?;
    }

    connection.execute_batch(CORE_SCHEMA_SQL)?;
    restore_headings_dependent_tables(connection, &repairs)?;
    rebuild_outline_path(connection)?;
    invalidate_search_trust_metadata(connection)?;
    drop_backed_up_tables(connection, &repairs)?;
    Ok(RebuiltDerivedProjections {
        effective_properties: rebuild_effective_properties,
        effective_tags: rebuild_effective_tags,
    })
}

fn collect_headings_repair_backups(
    connection: &Connection,
    force: bool,
) -> rusqlite::Result<Vec<HeadingsDependentTable>> {
    let candidates = [
        HeadingsDependentTable::Keywords,
        HeadingsDependentTable::Properties,
        HeadingsDependentTable::Tags,
        HeadingsDependentTable::Timestamps,
        HeadingsDependentTable::HeadingBodies,
        HeadingsDependentTable::Links,
        HeadingsDependentTable::OutlinePath,
    ];
    let mut repairs = Vec::new();
    let mut needs_timestamp_repeaters = false;

    for candidate in candidates {
        let table_name = candidate.table_name();
        if !table_exists(connection, table_name)? {
            continue;
        }
        if force || table_references(connection, table_name, "headings_legacy")? {
            rename_table_for_headings_repair(connection, candidate)?;
            if candidate == HeadingsDependentTable::Timestamps {
                needs_timestamp_repeaters = true;
            }
            repairs.push(candidate);
        }
    }

    if needs_timestamp_repeaters && table_exists(connection, "timestamp_repeaters")? {
        rename_table_for_headings_repair(connection, HeadingsDependentTable::TimestampRepeaters)?;
        repairs.push(HeadingsDependentTable::TimestampRepeaters);
    }

    Ok(repairs)
}

fn rename_table_for_headings_repair(
    connection: &Connection,
    table: HeadingsDependentTable,
) -> rusqlite::Result<()> {
    let table_name = quote_sqlite_identifier(table.table_name());
    let backup_name = quote_sqlite_identifier(table.backup_table_name());
    connection.execute_batch(&format!(
        "ALTER TABLE {table_name} RENAME TO {backup_name};"
    ))?;
    drop_indexes_for_table(connection, table.backup_table_name())
}

fn restore_headings_dependent_tables(
    connection: &Connection,
    repairs: &[HeadingsDependentTable],
) -> rusqlite::Result<()> {
    for table in repairs.iter().copied() {
        match table {
            HeadingsDependentTable::Keywords => restore_keywords_table(connection)?,
            HeadingsDependentTable::Properties => restore_properties_table(connection)?,
            HeadingsDependentTable::Tags => restore_tags_table(connection)?,
            HeadingsDependentTable::Timestamps => restore_timestamps_table(connection)?,
            HeadingsDependentTable::TimestampRepeaters => {
                restore_timestamp_repeaters_table(connection)?
            }
            HeadingsDependentTable::HeadingBodies => restore_heading_bodies_table(connection)?,
            HeadingsDependentTable::Links => restore_links_table(connection)?,
            HeadingsDependentTable::OutlinePath => {}
        }
    }
    Ok(())
}

fn restore_keywords_table(connection: &Connection) -> rusqlite::Result<()> {
    connection.execute_batch(
        r#"
INSERT INTO keywords (id, heading_id, keyword, value, line_number)
SELECT id, heading_id, keyword, value, line_number
FROM keywords_headings_repair_backup;
"#,
    )
}

fn restore_properties_table(connection: &Connection) -> rusqlite::Result<()> {
    connection.execute_batch(
        r#"
INSERT INTO properties (id, heading_id, key, value, source, append, line_number)
SELECT id, heading_id, key, value, source, append, line_number
FROM properties_headings_repair_backup;
"#,
    )
}

fn restore_tags_table(connection: &Connection) -> rusqlite::Result<()> {
    connection.execute_batch(
        r#"
INSERT INTO tags (heading_id, tag)
SELECT heading_id, tag
FROM tags_headings_repair_backup;
"#,
    )
}

fn restore_timestamps_table(connection: &Connection) -> rusqlite::Result<()> {
    let source = HeadingsDependentTable::Timestamps.backup_table_name();
    let columns = table_columns(connection, source)?;
    let has_time_expr = if has_column(&columns, "has_time") {
        "has_time"
    } else {
        "NULL"
    };

    connection.execute_batch(&format!(
        r#"
INSERT INTO timestamps (
    id,
    heading_id,
    role,
    has_time,
    start_ts,
    end_ts,
    type,
    range_type,
    raw_value,
    byte_start,
    byte_end,
    line_number
)
SELECT
    id,
    heading_id,
    role,
    {has_time_expr},
    start_ts,
    end_ts,
    type,
    range_type,
    raw_value,
    byte_start,
    byte_end,
    line_number
FROM {source};
"#,
    ))
}

fn restore_timestamp_repeaters_table(connection: &Connection) -> rusqlite::Result<()> {
    let source = HeadingsDependentTable::TimestampRepeaters.backup_table_name();
    let columns = table_columns(connection, source)?;

    if timestamp_repeaters_uses_explicit_columns(&columns) {
        return connection.execute_batch(&format!(
            r#"
INSERT INTO timestamp_repeaters (
    id,
    timestamp_id,
    repeater_type,
    repeater_value,
    repeater_unit,
    repeater_deadline_value,
    repeater_deadline_unit,
    warning_type,
    warning_value,
    warning_unit
)
SELECT
    id,
    timestamp_id,
    repeater_type,
    repeater_value,
    repeater_unit,
    repeater_deadline_value,
    repeater_deadline_unit,
    warning_type,
    warning_value,
    warning_unit
FROM {source};
"#,
        ));
    }

    let has_kind = columns.iter().any(|column| column == "kind");
    let has_repeater_deadline_value = columns
        .iter()
        .any(|column| column == "repeater_deadline_value");
    let has_repeater_deadline_unit = columns
        .iter()
        .any(|column| column == "repeater_deadline_unit");
    let has_deadline_value = columns.iter().any(|column| column == "deadline_value");
    let has_deadline_unit = columns.iter().any(|column| column == "deadline_unit");

    let repeater_type_expr = if has_kind {
        "MAX(CASE WHEN kind = 'repeater' THEN type ELSE NULL END)"
    } else {
        "MAX(CASE WHEN type IN ('cumulate', 'catch_up', 'restart') THEN type ELSE NULL END)"
    };
    let repeater_value_expr = if has_kind {
        "MAX(CASE WHEN kind = 'repeater' THEN value ELSE NULL END)"
    } else {
        "MAX(CASE WHEN type IN ('cumulate', 'catch_up', 'restart') THEN value ELSE NULL END)"
    };
    let repeater_unit_expr = if has_kind {
        "MAX(CASE WHEN kind = 'repeater' THEN unit ELSE NULL END)"
    } else {
        "MAX(CASE WHEN type IN ('cumulate', 'catch_up', 'restart') THEN unit ELSE NULL END)"
    };
    let warning_type_expr = if has_kind {
        "MAX(CASE WHEN kind = 'warning' THEN type ELSE NULL END)"
    } else {
        "NULL"
    };
    let warning_value_expr = if has_kind {
        "MAX(CASE WHEN kind = 'warning' THEN value ELSE NULL END)"
    } else {
        "NULL"
    };
    let warning_unit_expr = if has_kind {
        "MAX(CASE WHEN kind = 'warning' THEN unit ELSE NULL END)"
    } else {
        "NULL"
    };
    let repeater_deadline_value_expr = match (has_repeater_deadline_value, has_deadline_value) {
        (true, true) => "MAX(COALESCE(repeater_deadline_value, deadline_value))",
        (true, false) => "MAX(repeater_deadline_value)",
        (false, true) => "MAX(deadline_value)",
        (false, false) => "NULL",
    };
    let repeater_deadline_unit_expr = match (has_repeater_deadline_unit, has_deadline_unit) {
        (true, true) => "MAX(COALESCE(repeater_deadline_unit, deadline_unit))",
        (true, false) => "MAX(repeater_deadline_unit)",
        (false, true) => "MAX(deadline_unit)",
        (false, false) => "NULL",
    };

    connection.execute_batch(&format!(
        r#"
INSERT INTO timestamp_repeaters (
    id,
    timestamp_id,
    repeater_type,
    repeater_value,
    repeater_unit,
    repeater_deadline_value,
    repeater_deadline_unit,
    warning_type,
    warning_value,
    warning_unit
)
SELECT
    MIN(id),
    timestamp_id,
    {repeater_type_expr} AS repeater_type,
    {repeater_value_expr} AS repeater_value,
    {repeater_unit_expr} AS repeater_unit,
    {repeater_deadline_value_expr} AS repeater_deadline_value,
    {repeater_deadline_unit_expr} AS repeater_deadline_unit,
    {warning_type_expr} AS warning_type,
    {warning_value_expr} AS warning_value,
    {warning_unit_expr} AS warning_unit
FROM {source}
GROUP BY timestamp_id;
"#,
    ))
}

fn restore_heading_bodies_table(connection: &Connection) -> rusqlite::Result<()> {
    connection.execute_batch(
        r#"
INSERT INTO heading_bodies (heading_id, body_text, body_byte_start, body_byte_end)
SELECT heading_id, body_text, body_byte_start, body_byte_end
FROM heading_bodies_headings_repair_backup;
"#,
    )
}

fn restore_links_table(connection: &Connection) -> rusqlite::Result<()> {
    let source = HeadingsDependentTable::Links.backup_table_name();
    let columns = table_columns(connection, source)?;
    let migration_sql = render_links_migration_sql_from_table(&columns, source);
    connection.execute_batch(&migration_sql)?;
    ensure_links_indexes(connection)
}

fn rebuild_outline_path(connection: &Connection) -> rusqlite::Result<()> {
    connection.execute_batch(
        r#"
DELETE FROM outline_path;

WITH RECURSIVE computed_outline (
    heading_id,
    file_id,
    parent_id,
    depth,
    materialized_path,
    breadcrumbs_json
) AS (
    SELECT
        headings.id,
        headings.file_id,
        headings.parent_id,
        0,
        '0000',
        json_array(headings.title)
    FROM headings
    WHERE headings.level = 0

    UNION ALL

    SELECT
        child.id,
        child.file_id,
        child.parent_id,
        parent.depth + 1,
        parent.materialized_path || '.' || printf(
            '%04d',
            (
                SELECT COUNT(*)
                FROM headings AS sibling
                WHERE sibling.parent_id = child.parent_id
                  AND (
                      sibling.byte_start < child.byte_start
                      OR (sibling.byte_start = child.byte_start AND sibling.id <= child.id)
                  )
            )
        ),
        json_insert(parent.breadcrumbs_json, '$[#]', child.title)
    FROM headings AS child
    INNER JOIN computed_outline AS parent ON parent.heading_id = child.parent_id
    WHERE child.level > 0
)
INSERT INTO outline_path (
    heading_id,
    file_id,
    parent_id,
    depth,
    materialized_path,
    breadcrumbs_json
)
SELECT
    heading_id,
    file_id,
    parent_id,
    depth,
    materialized_path,
    breadcrumbs_json
FROM computed_outline;
"#,
    )
}

fn invalidate_search_trust_metadata(connection: &Connection) -> rusqlite::Result<()> {
    connection.execute(
        "INSERT OR REPLACE INTO db_metadata (key, value) VALUES (?1, ?2)",
        (DB_METADATA_FTS_AVAILABLE_KEY, "0"),
    )?;
    connection.execute(
        "INSERT OR REPLACE INTO db_metadata (key, value) VALUES (?1, ?2)",
        (DB_METADATA_FTS_BODY_INDEXED_KEY, "0"),
    )?;
    connection.execute(
        "INSERT OR REPLACE INTO db_metadata (key, value) VALUES (?1, ?2)",
        (DB_METADATA_FTS_SCHEMA_VERSION_KEY, "0"),
    )?;
    Ok(())
}

fn drop_backed_up_tables(
    connection: &Connection,
    repairs: &[HeadingsDependentTable],
) -> rusqlite::Result<()> {
    for table in repairs.iter().copied() {
        drop_table_if_exists(connection, table.backup_table_name())?;
    }
    Ok(())
}

fn drop_table_if_exists(connection: &Connection, table_name: &str) -> rusqlite::Result<()> {
    let quoted = quote_sqlite_identifier(table_name);
    connection.execute_batch(&format!("DROP TABLE IF EXISTS {quoted};"))
}

fn table_references(
    connection: &Connection,
    table_name: &str,
    target_table: &str,
) -> rusqlite::Result<bool> {
    let quoted = quote_sqlite_identifier(table_name);
    let mut statement = connection.prepare(&format!("PRAGMA foreign_key_list({quoted})"))?;
    let mut rows = statement.query([])?;
    while let Some(row) = rows.next()? {
        let referenced_table: String = row.get(2)?;
        if referenced_table == target_table {
            return Ok(true);
        }
    }
    Ok(false)
}

fn links_table_matches_current_contract(columns: &[String]) -> bool {
    columns.iter().any(|column| column == "line")
        && columns.iter().any(|column| column == "source_context")
        && columns.iter().any(|column| column == "raw")
        && columns.iter().any(|column| column == "raw_target")
        && columns.iter().any(|column| column == "raw_description")
        && columns.iter().any(|column| column == "link_type")
        && columns.iter().any(|column| column == "path")
        && columns.iter().any(|column| column == "path_absolute")
        && columns.iter().any(|column| column == "target_file_id")
        && columns.iter().any(|column| column == "target_heading_id")
        && columns.iter().any(|column| column == "target_custom_id")
        && columns.iter().any(|column| column == "target_id")
        && columns.iter().any(|column| column == "resolution_status")
        && columns
            .iter()
            .any(|column| column == "resolution_diagnostic")
        && !columns.iter().any(|column| column == "line_number")
        && !columns.iter().any(|column| column == "target")
        && !columns.iter().any(|column| column == "raw_link")
        && !columns.iter().any(|column| column == "description")
        && !columns.iter().any(|column| column == "relation")
        && !columns.iter().any(|column| column == "resolved_file_id")
        && !columns.iter().any(|column| column == "resolved_heading_id")
        && !columns.iter().any(|column| column == "resolved")
        && !columns.iter().any(|column| column == "broken")
        && !columns.iter().any(|column| column == "diagnostic")
}

fn render_links_migration_sql(columns: &[String]) -> String {
    render_links_migration_sql_from_table(columns, "links_legacy")
}

fn render_links_migration_sql_from_table(columns: &[String], source_table: &str) -> String {
    let line_expr = if has_column(columns, "line") {
        "line"
    } else {
        "COALESCE(line_number, 1)"
    };
    let source_context_expr = if has_column(columns, "source_context") {
        "source_context"
    } else {
        "'normal'"
    };
    let raw_expr = if has_column(columns, "raw") {
        "raw"
    } else {
        "raw_link"
    };
    let raw_target_expr = if has_column(columns, "raw_target") {
        "raw_target"
    } else {
        r#"CASE
        WHEN link_type IS NOT NULL AND search_option IS NOT NULL
            THEN lower(link_type) || ':' || target || '::' || search_option
        WHEN link_type IS NOT NULL
            THEN lower(link_type) || ':' || target
        ELSE target
    END"#
    };
    let raw_description_expr = if has_column(columns, "raw_description") {
        "raw_description"
    } else {
        "description"
    };
    let link_type_expr = if has_column(columns, "path") {
        "link_type"
    } else {
        "COALESCE(lower(link_type), 'unknown')"
    };
    let path_expr = if has_column(columns, "path") {
        "path"
    } else {
        "target"
    };
    let path_absolute_expr = if has_column(columns, "path_absolute") {
        "path_absolute"
    } else {
        "target_absolute"
    };
    let target_file_id_expr = if has_column(columns, "target_file_id") {
        "target_file_id"
    } else {
        "resolved_file_id"
    };
    let target_heading_id_expr = if has_column(columns, "target_heading_id") {
        "target_heading_id"
    } else {
        "resolved_heading_id"
    };
    let target_custom_id_expr = if has_column(columns, "target_custom_id") {
        "target_custom_id"
    } else {
        "NULL"
    };
    let target_id_expr = if has_column(columns, "target_id") {
        "target_id"
    } else {
        "NULL"
    };
    let legacy_resolved_expr = if has_column(columns, "resolved") {
        "resolved"
    } else {
        "0"
    };
    let legacy_broken_expr = if has_column(columns, "broken") {
        "broken"
    } else {
        "0"
    };
    let resolution_status_expr = if has_column(columns, "resolution_status") {
        "resolution_status".to_string()
    } else if has_column(columns, "resolved") || has_column(columns, "broken") {
        format!(
            r#"CASE
        WHEN {legacy_resolved_expr} = 1 THEN 'resolved'
        WHEN {legacy_broken_expr} = 1 THEN 'broken'
        ELSE NULL
    END"#
        )
    } else {
        "NULL".to_string()
    };
    let resolution_diagnostic_expr = if has_column(columns, "resolution_diagnostic") {
        "resolution_diagnostic"
    } else if has_column(columns, "diagnostic") {
        "diagnostic"
    } else {
        "NULL"
    };

    format!(
        r#"
INSERT INTO links (
    id,
    file_id,
    heading_id,
    byte_start,
    byte_end,
    line,
    source_context,
    format,
    raw,
    raw_target,
    raw_description,
    link_type,
    path,
    search_option,
    path_absolute,
    target_file_id,
    target_heading_id,
    target_custom_id,
    target_id,
    resolution_status,
    resolution_diagnostic
)
SELECT
    id,
    file_id,
    heading_id,
    byte_start,
    byte_end,
    {line_expr} AS line,
    {source_context_expr} AS source_context,
    COALESCE(format, 'plain') AS format,
    {raw_expr} AS raw,
    {raw_target_expr} AS raw_target,
    {raw_description_expr} AS raw_description,
    {link_type_expr} AS link_type,
    {path_expr} AS path,
    search_option,
    {path_absolute_expr} AS path_absolute,
    {target_file_id_expr} AS target_file_id,
    {target_heading_id_expr} AS target_heading_id,
    {target_custom_id_expr} AS target_custom_id,
    {target_id_expr} AS target_id,
    {resolution_status_expr} AS resolution_status,
    {resolution_diagnostic_expr} AS resolution_diagnostic
FROM {source_table};
"#,
    )
}

fn has_column(columns: &[String], name: &str) -> bool {
    columns.iter().any(|column| column == name)
}

fn drop_indexes_for_table(connection: &Connection, table_name: &str) -> rusqlite::Result<()> {
    let index_names = index_names_for_table(connection, table_name)?;
    for index_name in index_names {
        let quoted_index_name = quote_sqlite_identifier(&index_name);
        connection.execute_batch(&format!("DROP INDEX IF EXISTS {quoted_index_name};"))?;
    }
    Ok(())
}

fn index_names_for_table(
    connection: &Connection,
    table_name: &str,
) -> rusqlite::Result<Vec<String>> {
    let mut statement = connection.prepare(
        "SELECT name
         FROM sqlite_master
         WHERE type = 'index' AND tbl_name = ?1 AND sql IS NOT NULL",
    )?;
    let index_names = statement
        .query_map([table_name], |row| row.get(0))?
        .collect::<Result<Vec<String>, _>>()?;
    Ok(index_names)
}

fn ensure_links_indexes(connection: &Connection) -> rusqlite::Result<()> {
    connection.execute_batch(
        r#"
CREATE INDEX IF NOT EXISTS idx_links_heading
    ON links(heading_id);

CREATE INDEX IF NOT EXISTS idx_links_path
    ON links(path);

CREATE INDEX IF NOT EXISTS idx_links_target_file
    ON links(target_file_id);

CREATE INDEX IF NOT EXISTS idx_links_target_heading
    ON links(target_heading_id);
"#,
    )
}

fn quote_sqlite_identifier(identifier: &str) -> String {
    format!("\"{}\"", identifier.replace('"', "\"\""))
}
