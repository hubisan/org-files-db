use super::*;

/// Owned, DB-free preparation output suitable for later change planning and
/// parallel parsing. The file record carries the stable source snapshot.
#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct PreparedFile {
    pub(in crate::indexer) path: PathBuf,
    pub(in crate::indexer) identity: FileIdentity,
    pub(in crate::indexer) document: ParsedOrgDocument,
    pub(in crate::indexer) todo_keywords: ResolvedTodoKeywords,
    pub(in crate::indexer) diagnostics: Vec<IndexDiagnostic>,
    pub(in crate::indexer) file_record: FileRecordInput,
}

pub(in crate::indexer) fn normalize_document(
    document: ParsedOrgDocument,
    path: &Path,
    content: &str,
) -> ParsedOrgDocument {
    let mut normalized = document;
    let level_zero_title = synthetic_level_zero_title(path, normalized.metadata.title.as_deref());
    let needs_level_zero = normalized
        .headings
        .first()
        .map(|heading| heading.level != 0 || heading.parent_index.is_some())
        .unwrap_or(true);

    if needs_level_zero {
        normalized.headings.insert(
            0,
            synthetic_level_zero_heading(
                path,
                content,
                &level_zero_title,
                normalized.metadata.title.as_deref(),
            ),
        );
    } else {
        let level_zero = &mut normalized.headings[0];
        level_zero.file_path = path.to_path_buf();
        level_zero.level = 0;
        level_zero.parent_index = None;
        level_zero.byte_start = 0;
        level_zero.byte_end = content.len();
        level_zero.line_number = Some(1);
        level_zero.title = level_zero_title;
        level_zero.title_raw = source_document_title(normalized.metadata.title.as_deref());
        level_zero.is_root = true;
    }

    normalize_heading_parent_indexes(&mut normalized.headings);

    for heading in &mut normalized.headings {
        heading.file_path = path.to_path_buf();
    }

    normalized.file_path = path.to_path_buf();
    normalized
}

pub(in crate::indexer) fn normalize_heading_parent_indexes(headings: &mut [ParsedHeading]) {
    if headings.is_empty() {
        return;
    }

    headings[0].level = 0;
    headings[0].parent_index = None;
    headings[0].is_root = true;

    let mut stack = vec![0usize];
    for index in 1..headings.len() {
        let current_level = headings[index].level;
        headings[index].is_root = false;

        while let Some(&parent_index) = stack.last() {
            if headings[parent_index].level < current_level {
                break;
            }
            stack.pop();
        }

        let parent_index = stack.last().copied().unwrap_or(0);
        headings[index].parent_index = Some(parent_index);
        stack.push(index);
    }
}

pub(in crate::indexer) fn synthetic_level_zero_heading(
    path: &Path,
    content: &str,
    title: &str,
    source_title: Option<&str>,
) -> ParsedHeading {
    let mut heading = ParsedHeading::new(path, 0, title.to_string(), 0, content.len());
    heading.title_raw = source_document_title(source_title);
    heading.line_number = Some(1);
    heading.is_root = true;
    heading
}

pub(in crate::indexer) fn synthetic_level_zero_title(
    path: &Path,
    document_title: Option<&str>,
) -> String {
    if let Some(title) = source_document_title(document_title) {
        return title;
    }

    path.file_stem()
        .or_else(|| path.file_name())
        .and_then(|name| name.to_str().map(str::to_string))
        .filter(|name| !name.is_empty())
        .unwrap_or_else(|| display_path(path))
}

pub(in crate::indexer) fn source_document_title(document_title: Option<&str>) -> Option<String> {
    document_title
        .map(str::trim)
        .filter(|title| !title.is_empty())
        .map(str::to_string)
}

pub(in crate::indexer) fn index_document(
    connection: &Connection,
    file_id: i64,
    document: &ParsedOrgDocument,
    todo_keywords: &ResolvedTodoKeywords,
    index_body_text: bool,
) -> Result<usize, DbWriteError> {
    if document.headings.is_empty() || document.headings[0].level != 0 {
        return Err(DbWriteError::InvalidInput(
            "normalized documents must start with a level 0 heading",
        ));
    }

    let level0_heading = &document.headings[0];
    let level0_id = DbWriter::insert_level0_heading(
        connection,
        &heading_record(file_id, None, level0_heading).map_err(db_write_invalid_input)?,
    )?;

    let mut heading_ids = vec![level0_id];
    let mut outline_rows = vec![outline_record(
        level0_id,
        file_id,
        None,
        0,
        outline_root_materialized_path(),
        vec![level0_heading.title.clone()],
    )
    .map_err(db_write_invalid_input)?];

    let mut child_ordinals = vec![0usize; document.headings.len()];

    for (heading_index, heading) in document.headings.iter().enumerate().skip(1) {
        let parent_index = heading.parent_index.unwrap_or(0);
        let parent_id = heading_ids.get(parent_index).copied().ok_or_else(|| {
            DbWriteError::InvalidInput("heading parent_index must reference an earlier heading")
        })?;
        let heading_id =
            DbWriter::insert_headings(
                connection,
                &[heading_record(file_id, Some(parent_id), heading)
                    .map_err(db_write_invalid_input)?],
            )?[0];

        if heading_ids.len() != heading_index {
            return Err(DbWriteError::InvalidInput(
                "heading insertion order must match parsed heading order",
            ));
        }

        heading_ids.push(heading_id);
        let parent_outline = &outline_rows[parent_index];
        let sibling_ordinal = child_ordinals[parent_index] + 1;
        child_ordinals[parent_index] = sibling_ordinal;
        outline_rows.push(
            outline_record(
                heading_id,
                file_id,
                Some(parent_id),
                parent_outline.depth + 1,
                outline_child_materialized_path(&parent_outline.materialized_path, sibling_ordinal),
                extend_breadcrumbs(&parent_outline.breadcrumbs_json, &heading.title)
                    .map_err(db_write_invalid_input)?,
            )
            .map_err(db_write_invalid_input)?,
        );
    }

    let keyword_rows = document
        .metadata
        .keywords
        .iter()
        .map(|keyword| KeywordRecord {
            heading_id: level0_id,
            keyword: keyword.key.clone(),
            value: keyword.value.clone(),
            line_number: keyword.line_number.map(i64::from),
        })
        .collect::<Vec<_>>();
    let todo_rows = todo_keyword_rows(file_id, &todo_keywords.entries);
    let tag_rows = document
        .headings
        .iter()
        .enumerate()
        .flat_map(|(index, heading)| {
            let heading_id = heading_ids[index];
            heading
                .tags
                .iter()
                .cloned()
                .map(move |tag| TagRecord { heading_id, tag })
        })
        .collect::<Vec<_>>();
    let property_rows = document
        .headings
        .iter()
        .enumerate()
        .flat_map(|(index, heading)| {
            let heading_id = heading_ids[index];
            heading
                .properties
                .iter()
                .map(move |property| PropertyRecord {
                    heading_id,
                    key: property.key.clone(),
                    value: property.value.clone(),
                    source: property.source.as_db_str().to_string(),
                    append: property.append,
                    line_number: property.line_number.map(i64::from),
                })
        })
        .collect::<Vec<_>>();
    let body_rows = if index_body_text {
        document
            .headings
            .iter()
            .enumerate()
            .map(|(index, heading)| heading_body_record(heading_ids[index], heading))
            .filter_map(Result::transpose)
            .collect::<Result<Vec<_>, _>>()
            .map_err(db_write_invalid_input)?
    } else {
        Vec::new()
    };
    let timestamp_rows = document
        .headings
        .iter()
        .enumerate()
        .flat_map(|(index, heading)| {
            let heading_id = heading_ids[index];
            heading
                .timestamps
                .iter()
                .map(move |timestamp| timestamp_record(heading_id, timestamp))
        })
        .collect::<Result<Vec<_>, _>>()
        .map_err(db_write_invalid_input)?;
    let link_rows = document
        .links
        .iter()
        .map(|link| link_record(file_id, &heading_ids, &document.headings, link))
        .collect::<Result<Vec<_>, _>>()
        .map_err(db_write_invalid_input)?;

    DbWriter::insert_todo_keywords(connection, &todo_rows)?;
    DbWriter::insert_keywords(connection, &keyword_rows)?;
    DbWriter::insert_tags(connection, &tag_rows)?;

    let parents = document
        .headings
        .iter()
        .enumerate()
        .map(|(index, heading)| {
            (
                heading_ids[index],
                heading.parent_index.map(|parent| heading_ids[parent]),
            )
        })
        .collect::<std::collections::HashMap<_, _>>();
    let direct_tags_by_heading = document
        .headings
        .iter()
        .enumerate()
        .map(|(index, heading)| (heading_ids[index], heading.tags.clone()))
        .collect::<std::collections::HashMap<_, _>>();
    let effective_tag_rows = derive_effective_tags(&parents, &direct_tags_by_heading)
        .into_iter()
        .map(|row| EffectiveTagRecord {
            heading_id: row.heading_id,
            file_id,
            tag: row.tag,
            position: row.position,
        })
        .collect::<Vec<_>>();
    DbWriter::insert_effective_tags(connection, &effective_tag_rows)?;

    DbWriter::insert_properties(connection, &property_rows)?;
    let mut properties_by_heading = std::collections::HashMap::<i64, Vec<PropertyRow>>::new();
    for (order, property) in property_rows.iter().enumerate() {
        properties_by_heading
            .entry(property.heading_id)
            .or_default()
            .push(PropertyRow {
                id: order as i64,
                heading_id: property.heading_id,
                key: property.key.clone(),
                value: property.value.clone(),
                append: property.append,
                line_number: property.line_number,
                source: property.source.clone(),
            });
    }
    let projection = derive_effective_properties(&parents, &properties_by_heading)
        .into_iter()
        .map(|row| EffectivePropertyRecord {
            heading_id: row.heading_id,
            file_id,
            key: row.key,
            local_value: row.local_value,
            effective_value: row.effective_value,
        })
        .collect::<Vec<_>>();
    DbWriter::insert_effective_properties(connection, &projection)?;
    DbWriter::insert_outline_path(connection, &outline_rows)?;
    DbWriter::insert_heading_bodies(connection, &body_rows)?;
    DbWriter::insert_links(connection, &link_rows)?;
    let timestamp_ids = DbWriter::insert_timestamps(connection, &timestamp_rows)?;
    let timestamp_repeater_rows = document
        .headings
        .iter()
        .flat_map(|heading| heading.timestamps.iter())
        .zip(timestamp_ids.iter().copied())
        .filter_map(|(timestamp, timestamp_id)| {
            timestamp_repeater_record(timestamp_id, &timestamp.modifiers).transpose()
        })
        .collect::<Result<Vec<_>, _>>()
        .map_err(db_write_invalid_input)?;
    DbWriter::insert_timestamp_repeaters(connection, &timestamp_repeater_rows)?;

    Ok(document.headings.len())
}

pub(in crate::indexer) fn timestamp_record(
    heading_id: i64,
    timestamp: &ParsedTimestamp,
) -> Result<TimestampRecord, &'static str> {
    Ok(TimestampRecord {
        heading_id,
        role: timestamp.role.map(timestamp_role_name),
        has_time: timestamp.has_time,
        start_ts: timestamp.start_ts,
        end_ts: timestamp.end_ts,
        timestamp_type: Some(timestamp_type_name(timestamp).to_string()),
        range_type: Some(timestamp_range_type_name(timestamp).to_string()),
        raw_value: timestamp.raw_value.clone(),
        byte_start: i64::try_from(timestamp.byte_start)
            .map_err(|_| "timestamp byte_start out of range")?,
        byte_end: i64::try_from(timestamp.byte_end)
            .map_err(|_| "timestamp byte_end out of range")?,
        line_number: timestamp.line_number.map(i64::from),
    })
}

pub(in crate::indexer) fn link_record(
    file_id: i64,
    heading_ids: &[i64],
    headings: &[ParsedHeading],
    link: &ParsedLink,
) -> Result<LinkRecord, &'static str> {
    let heading_index = owning_heading_index(headings, link.byte_start)
        .ok_or("link byte range must attach to a heading including root")?;

    Ok(LinkRecord {
        id: None,
        file_id,
        heading_id: heading_ids
            .get(heading_index)
            .copied()
            .ok_or("link heading index must reference an inserted heading")?,
        byte_start: i64::try_from(link.byte_start).map_err(|_| "link byte_start out of range")?,
        byte_end: i64::try_from(link.byte_end).map_err(|_| "link byte_end out of range")?,
        line: i64::from(link.line),
        source_context: link.source_context.as_db_str().to_string(),
        format: link.format.clone(),
        raw: link.raw.clone(),
        raw_target: link.raw_target.clone(),
        raw_description: link.raw_description.clone(),
        link_type: link.link_type.clone(),
        path: link.path.clone(),
        search_option: link.search_option.clone(),
    })
}

pub(in crate::indexer) fn owning_heading_index(
    headings: &[ParsedHeading],
    byte_start: usize,
) -> Option<usize> {
    // Headings are in document order (non-decreasing byte_start), so every candidate
    // lies before the partition point; walk back to the last one whose range contains it.
    let upper = headings.partition_point(|heading| heading.byte_start <= byte_start);
    headings[..upper]
        .iter()
        .enumerate()
        .rev()
        .find(|(_, heading)| byte_start < heading.byte_end)
        .map(|(index, _)| index)
}

pub(in crate::indexer) fn timestamp_repeater_record(
    timestamp_id: i64,
    modifiers: &[crate::parser::ParsedTimestampModifier],
) -> Result<Option<TimestampRepeaterRecord>, &'static str> {
    let mut row = TimestampRepeaterRecord {
        timestamp_id,
        repeater_type: None,
        repeater_value: None,
        repeater_unit: None,
        repeater_deadline_value: None,
        repeater_deadline_unit: None,
        warning_type: None,
        warning_value: None,
        warning_unit: None,
    };

    for modifier in modifiers {
        match modifier.kind {
            ParsedTimestampModifierKind::Repeater => {
                if row.repeater_type.is_some() {
                    return Err("timestamp modifiers must not contain multiple repeater entries");
                }
                row.repeater_type = Some(repeater_modifier_type_name(modifier.modifier_type)?);
                row.repeater_value = Some(modifier.value);
                row.repeater_unit = Some(timestamp_unit_name(modifier.unit).to_string());
                row.repeater_deadline_value = modifier.repeater_deadline_value;
                row.repeater_deadline_unit = modifier
                    .repeater_deadline_unit
                    .map(|unit| timestamp_unit_name(unit).to_string());
            }
            ParsedTimestampModifierKind::Warning => {
                if row.warning_type.is_some() {
                    return Err("timestamp modifiers must not contain multiple warning entries");
                }
                row.warning_type = Some(warning_modifier_type_name(modifier.modifier_type)?);
                row.warning_value = Some(modifier.value);
                row.warning_unit = Some(timestamp_unit_name(modifier.unit).to_string());
            }
        }
    }

    if row.repeater_type.is_none() && row.warning_type.is_none() {
        return Ok(None);
    }

    Ok(Some(row))
}

pub(in crate::indexer) fn heading_record(
    file_id: i64,
    parent_id: Option<i64>,
    heading: &ParsedHeading,
) -> Result<HeadingRecord, &'static str> {
    let todo_type = heading.todo_type.as_ref().map(|value| match value {
        TodoType::Open => "open".to_string(),
        TodoType::Closed => "closed".to_string(),
    });

    Ok(HeadingRecord {
        id: None,
        file_id,
        parent_id,
        level: i64::from(heading.level),
        line_number: heading.line_number.map(i64::from),
        byte_start: if heading.level == 0 {
            -1
        } else {
            i64::try_from(heading.byte_start).map_err(|_| "byte_start out of range")?
        },
        byte_end: i64::try_from(heading.byte_end).map_err(|_| "byte_end out of range")?,
        title: heading.title.clone(),
        title_raw: heading.title_raw.clone(),
        todo_keyword: heading.todo_keyword.clone(),
        todo_type,
        priority: heading.priority.clone(),
        scheduled_raw: heading.planning.scheduled_raw().map(str::to_string),
        scheduled_ts: heading.planning.scheduled_ts(),
        scheduled_has_time: heading.planning.scheduled_has_time(),
        deadline_raw: heading.planning.deadline_raw().map(str::to_string),
        deadline_ts: heading.planning.deadline_ts(),
        deadline_has_time: heading.planning.deadline_has_time(),
        closed_raw: heading.planning.closed_raw().map(str::to_string),
        closed_ts: heading.planning.closed_ts(),
        closed_has_time: heading.planning.closed_has_time(),
        archivedp: heading.is_archived,
        footnote_section_p: false,
    })
}

pub(in crate::indexer) fn timestamp_role_name(role: ParsedTimestampRole) -> String {
    match role {
        ParsedTimestampRole::Scheduled => "scheduled".to_string(),
        ParsedTimestampRole::Deadline => "deadline".to_string(),
        ParsedTimestampRole::Closed => "closed".to_string(),
        ParsedTimestampRole::Body => "body".to_string(),
    }
}

pub(in crate::indexer) fn timestamp_type_name(timestamp: &ParsedTimestamp) -> &'static str {
    match timestamp.timestamp_type {
        crate::parser::ParsedTimestampType::Active => "active",
        crate::parser::ParsedTimestampType::Inactive => "inactive",
        crate::parser::ParsedTimestampType::Diary => "diary",
    }
}

pub(in crate::indexer) fn timestamp_range_type_name(timestamp: &ParsedTimestamp) -> &'static str {
    match timestamp.range_type {
        crate::parser::ParsedTimestampRangeType::None => "none",
        crate::parser::ParsedTimestampRangeType::DateRange => "date_range",
        crate::parser::ParsedTimestampRangeType::TimeRange => "time_range",
        crate::parser::ParsedTimestampRangeType::DateTimeRange => "datetime_range",
        crate::parser::ParsedTimestampRangeType::Unknown => "unknown",
    }
}

pub(in crate::indexer) fn repeater_modifier_type_name(
    modifier_type: ParsedTimestampModifierType,
) -> Result<String, &'static str> {
    match modifier_type {
        ParsedTimestampModifierType::Cumulate => Ok("cumulate".to_string()),
        ParsedTimestampModifierType::CatchUp => Ok("catch_up".to_string()),
        ParsedTimestampModifierType::Restart => Ok("restart".to_string()),
        ParsedTimestampModifierType::All | ParsedTimestampModifierType::First => {
            Err("warning modifier type cannot be stored as a repeater")
        }
    }
}

pub(in crate::indexer) fn warning_modifier_type_name(
    modifier_type: ParsedTimestampModifierType,
) -> Result<String, &'static str> {
    match modifier_type {
        ParsedTimestampModifierType::All => Ok("all".to_string()),
        ParsedTimestampModifierType::First => Ok("first".to_string()),
        ParsedTimestampModifierType::Cumulate
        | ParsedTimestampModifierType::CatchUp
        | ParsedTimestampModifierType::Restart => {
            Err("repeater modifier type cannot be stored as a warning")
        }
    }
}

pub(in crate::indexer) fn timestamp_unit_name(unit: ParsedTimestampUnit) -> &'static str {
    match unit {
        ParsedTimestampUnit::Hour => "hour",
        ParsedTimestampUnit::Day => "day",
        ParsedTimestampUnit::Week => "week",
        ParsedTimestampUnit::Month => "month",
        ParsedTimestampUnit::Year => "year",
    }
}

pub(in crate::indexer) fn todo_keyword_rows(
    file_id: i64,
    todo_keywords: &[ResolvedTodoKeywordEntry],
) -> Vec<TodoKeywordRecord> {
    todo_keywords
        .iter()
        .map(|keyword| TodoKeywordRecord {
            file_id,
            keyword: keyword.keyword.clone(),
            state_type: keyword.state_type.clone(),
            shortcut: keyword.shortcut,
            sequence_no: keyword.sequence_no,
            source_kind: keyword.source_kind.as_db_str().to_string(),
            source_keyword: keyword.source_keyword.clone(),
            source_line_number: keyword.source_line_number.map(i64::from),
        })
        .collect()
}

pub(in crate::indexer) fn outline_record(
    heading_id: i64,
    file_id: i64,
    parent_id: Option<i64>,
    depth: i64,
    materialized_path: String,
    breadcrumbs: Vec<String>,
) -> Result<OutlinePathRecord, &'static str> {
    Ok(OutlinePathRecord {
        heading_id,
        file_id,
        parent_id,
        depth,
        materialized_path,
        breadcrumbs_json: serde_json::to_string(&breadcrumbs)
            .map_err(|_| "outline breadcrumb serialization failed")?,
    })
}

pub(in crate::indexer) fn extend_breadcrumbs(
    breadcrumbs_json: &str,
    title: &str,
) -> Result<Vec<String>, &'static str> {
    let mut breadcrumbs: Vec<String> = serde_json::from_str(breadcrumbs_json)
        .map_err(|_| "outline breadcrumb deserialization failed")?;
    breadcrumbs.push(title.to_string());
    Ok(breadcrumbs)
}

pub(in crate::indexer) fn zero_pad_path_segment(value: usize) -> String {
    format!("{value:04}")
}

pub(in crate::indexer) fn outline_root_materialized_path() -> String {
    zero_pad_path_segment(0)
}

pub(in crate::indexer) fn outline_child_materialized_path(
    parent_path: &str,
    sibling_ordinal: usize,
) -> String {
    format!("{parent_path}.{}", zero_pad_path_segment(sibling_ordinal))
}

pub(in crate::indexer) fn heading_body_record(
    heading_id: i64,
    heading: &ParsedHeading,
) -> Result<Option<HeadingBodyRecord>, &'static str> {
    let Some(body_text) = heading.body_text.clone() else {
        return Ok(None);
    };

    Ok(Some(HeadingBodyRecord {
        heading_id,
        body_text,
        body_byte_start: heading
            .body_byte_start
            .map(i64::try_from)
            .transpose()
            .map_err(|_| "heading body_byte_start out of range")?,
        body_byte_end: heading
            .body_byte_end
            .map(i64::try_from)
            .transpose()
            .map_err(|_| "heading body_byte_end out of range")?,
    }))
}

pub(in crate::indexer) fn db_write_invalid_input(message: &'static str) -> DbWriteError {
    DbWriteError::InvalidInput(message)
}
