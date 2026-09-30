use std::path::Path;

use rusqlite::Connection;
use serde::Serialize;

use crate::db::{DbReader, HeadingListRow, LinkListRow};

use super::{error::CliError, open_cli_database};

pub(super) fn headings_json_rows(
    exclude_root: bool,
    config_path: Option<&Path>,
) -> Result<Vec<HeadingJsonRow>, CliError> {
    let connection = open_cli_database(config_path)?;
    headings_rows_for_json(&connection, exclude_root)
}

pub(super) fn links_json_rows(config_path: Option<&Path>) -> Result<Vec<LinkJsonRow>, CliError> {
    let connection = open_cli_database(config_path)?;
    links_rows_for_json(&connection)
}

pub(super) fn headings_rows_for_json(
    connection: &Connection,
    exclude_root: bool,
) -> Result<Vec<HeadingJsonRow>, CliError> {
    let mut rows = DbReader::list_headings(connection).map_err(CliError::DbRead)?;
    if exclude_root {
        rows.retain(|row| row.level > 0);
    }
    rows.into_iter().map(HeadingJsonRow::try_from).collect()
}

pub(super) fn links_rows_for_json(connection: &Connection) -> Result<Vec<LinkJsonRow>, CliError> {
    DbReader::list_links(connection)
        .map_err(CliError::DbRead)?
        .into_iter()
        .map(LinkJsonRow::try_from)
        .collect()
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub(super) struct HeadingJsonRow {
    pub(super) id: i64,
    pub(super) file_id: i64,
    pub(super) file_path: String,
    pub(super) parent_id: Option<i64>,
    pub(super) level: i64,
    pub(super) line_number: Option<i64>,
    pub(super) byte_start: i64,
    pub(super) byte_end: i64,
    pub(super) title: String,
    pub(super) title_raw: Option<String>,
    pub(super) todo_keyword: Option<String>,
    pub(super) todo_type: Option<String>,
    pub(super) priority: Option<String>,
    pub(super) scheduled_raw: Option<String>,
    pub(super) scheduled_ts: Option<i64>,
    pub(super) deadline_raw: Option<String>,
    pub(super) deadline_ts: Option<i64>,
    pub(super) closed_raw: Option<String>,
    pub(super) closed_ts: Option<i64>,
    pub(super) archivedp: bool,
    pub(super) footnote_section_p: bool,
    pub(super) all_tags: Vec<String>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub(super) struct LinkJsonRow {
    pub(super) file_id: i64,
    pub(super) file_path: String,
    pub(super) heading_id: i64,
    pub(super) heading_path: Vec<String>,
    pub(super) heading_level: i64,
    pub(super) source_context: String,
    pub(super) format: String,
    pub(super) link_type: String,
    pub(super) raw: String,
    pub(super) raw_target: String,
    pub(super) raw_description: Option<String>,
    pub(super) path: String,
    pub(super) search_option: Option<String>,
    pub(super) path_absolute: Option<String>,
    pub(super) target_file_id: Option<i64>,
    pub(super) target_heading_id: Option<i64>,
    pub(super) target_custom_id: Option<String>,
    pub(super) target_id: Option<String>,
    pub(super) resolution_status: Option<String>,
    pub(super) resolution_diagnostic: Option<String>,
    pub(super) byte_start: i64,
    pub(super) byte_end: i64,
    pub(super) line: i64,
}

impl TryFrom<HeadingListRow> for HeadingJsonRow {
    type Error = CliError;

    fn try_from(row: HeadingListRow) -> Result<Self, Self::Error> {
        Ok(Self {
            id: row.id,
            file_id: row.file_id,
            file_path: row.file_path,
            parent_id: row.parent_id,
            level: row.level,
            line_number: row.line_number,
            byte_start: row.byte_start,
            byte_end: row.byte_end,
            title: row.title,
            title_raw: row.title_raw,
            todo_keyword: row.todo_keyword,
            todo_type: row.todo_type,
            priority: row.priority,
            scheduled_raw: row.scheduled_raw,
            scheduled_ts: row.scheduled_ts,
            deadline_raw: row.deadline_raw,
            deadline_ts: row.deadline_ts,
            closed_raw: row.closed_raw,
            closed_ts: row.closed_ts,
            archivedp: row.archivedp,
            footnote_section_p: row.footnote_section_p,
            all_tags: row.all_tags,
        })
    }
}

impl TryFrom<LinkListRow> for LinkJsonRow {
    type Error = CliError;

    fn try_from(row: LinkListRow) -> Result<Self, Self::Error> {
        let breadcrumbs: Vec<String> = serde_json::from_str(&row.heading_breadcrumbs_json)
            .map_err(|source| CliError::InvalidHeadingPath {
                heading_id: row.heading_id,
                source,
            })?;
        let heading_path = strip_root_breadcrumb(breadcrumbs, row.heading_level);

        Ok(Self {
            file_id: row.file_id,
            file_path: row.file_path,
            heading_id: row.heading_id,
            heading_path,
            heading_level: row.heading_level,
            source_context: row.source_context,
            format: row.format,
            link_type: row.link_type,
            raw: row.raw,
            raw_target: row.raw_target,
            raw_description: row.raw_description,
            path: row.path,
            search_option: row.search_option,
            path_absolute: row.path_absolute,
            target_file_id: row.target_file_id,
            target_heading_id: row.target_heading_id,
            target_custom_id: row.target_custom_id,
            target_id: row.target_id,
            resolution_status: row.resolution_status,
            resolution_diagnostic: row.resolution_diagnostic,
            byte_start: row.byte_start,
            byte_end: row.byte_end,
            line: row.line,
        })
    }
}

pub(super) fn strip_root_breadcrumb(
    mut breadcrumbs: Vec<String>,
    heading_level: i64,
) -> Vec<String> {
    if heading_level == 0 {
        Vec::new()
    } else {
        if !breadcrumbs.is_empty() {
            breadcrumbs.remove(0);
        }
        breadcrumbs
    }
}
