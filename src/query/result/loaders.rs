use super::*;

pub(in crate::query::result) struct StoredFile {
    pub(in crate::query::result) id: i64,
    pub(in crate::query::result) path: String,
    pub(in crate::query::result) name: String,
    pub(in crate::query::result) dir: String,
    pub(in crate::query::result) mtime_ns: i64,
    pub(in crate::query::result) size: i64,
    pub(in crate::query::result) content_hash: Option<String>,
    pub(in crate::query::result) indexed_at: Option<i64>,
    pub(in crate::query::result) root_heading_id: i64,
    pub(in crate::query::result) root_title: String,
    pub(in crate::query::result) root_title_raw: Option<String>,
    pub(in crate::query::result) root_line_number: Option<i64>,
}

#[derive(Debug, Clone)]
pub(in crate::query::result) struct StoredHeading {
    pub(in crate::query::result) id: i64,
    pub(in crate::query::result) file_id: i64,
    pub(in crate::query::result) parent_id: Option<i64>,
    pub(in crate::query::result) level: i64,
    pub(in crate::query::result) line_number: Option<i64>,
    pub(in crate::query::result) byte_start: i64,
    pub(in crate::query::result) byte_end: i64,
    pub(in crate::query::result) title: String,
    pub(in crate::query::result) title_raw: Option<String>,
    pub(in crate::query::result) todo_keyword: Option<String>,
    pub(in crate::query::result) todo_type: Option<String>,
    pub(in crate::query::result) priority: Option<String>,
    pub(in crate::query::result) scheduled_raw: Option<String>,
    pub(in crate::query::result) scheduled_ts: Option<i64>,
    pub(in crate::query::result) deadline_raw: Option<String>,
    pub(in crate::query::result) deadline_ts: Option<i64>,
    pub(in crate::query::result) closed_raw: Option<String>,
    pub(in crate::query::result) closed_ts: Option<i64>,
    pub(in crate::query::result) archivedp: bool,
    pub(in crate::query::result) footnote_section_p: bool,
    pub(in crate::query::result) all_tags: Vec<String>,
}

#[derive(Debug, Clone)]
pub(in crate::query::result) struct StoredLink {
    pub(in crate::query::result) id: i64,
    pub(in crate::query::result) file_id: i64,
    pub(in crate::query::result) heading_id: i64,
    pub(in crate::query::result) source_context: String,
    pub(in crate::query::result) format: String,
    pub(in crate::query::result) link_type: String,
    pub(in crate::query::result) raw: String,
    pub(in crate::query::result) raw_target: String,
    pub(in crate::query::result) raw_description: Option<String>,
    pub(in crate::query::result) link_path: String,
    pub(in crate::query::result) search_option: Option<String>,
    pub(in crate::query::result) path_absolute: Option<String>,
    pub(in crate::query::result) target_file_id: Option<i64>,
    pub(in crate::query::result) target_heading_id: Option<i64>,
    pub(in crate::query::result) target_custom_id: Option<String>,
    pub(in crate::query::result) target_id: Option<String>,
    pub(in crate::query::result) resolution_status: Option<String>,
    pub(in crate::query::result) resolution_diagnostic: Option<String>,
    pub(in crate::query::result) byte_start: i64,
    pub(in crate::query::result) byte_end: i64,
    pub(in crate::query::result) line: i64,
}

#[derive(Debug, Clone)]
pub(in crate::query::result) struct StoredProperty {
    pub(in crate::query::result) id: i64,
    pub(in crate::query::result) heading_id: i64,
    pub(in crate::query::result) fact: PropertyFact,
}

#[derive(Debug, Clone)]
pub(in crate::query::result) struct StoredKeyword {
    pub(in crate::query::result) id: i64,
    pub(in crate::query::result) heading_id: i64,
    pub(in crate::query::result) fact: KeywordFact,
}

pub(in crate::query::result) fn collect_matched_ids(
    rows: &QueryRows,
) -> (BTreeSet<i64>, BTreeSet<i64>, Vec<LinkQueryRow>) {
    match rows {
        QueryRows::Headings(rows) => {
            let mut file_ids = BTreeSet::new();
            let mut heading_ids = BTreeSet::new();
            for row in rows {
                match row {
                    HeadingQueryMatch::File(row) => {
                        file_ids.insert(row.id);
                    }
                    HeadingQueryMatch::Heading(row) => {
                        file_ids.insert(row.file_id);
                        heading_ids.insert(row.id);
                    }
                }
            }
            (file_ids, heading_ids, Vec::new())
        }
        QueryRows::Links(rows) => {
            let mut file_ids = BTreeSet::new();
            let mut heading_ids = BTreeSet::new();
            for row in rows {
                file_ids.insert(row.file_id);
                heading_ids.insert(row.heading_id);
            }
            (file_ids, heading_ids, rows.clone())
        }
        QueryRows::Files(rows) => {
            let file_ids = rows.iter().map(|row| row.id).collect::<BTreeSet<_>>();
            (file_ids, BTreeSet::new(), Vec::new())
        }
    }
}

pub(in crate::query::result) fn matched_metadata_heading_ids(
    matched_heading_ids: &BTreeSet<i64>,
    matched_file_ids: &BTreeSet<i64>,
    files: &HashMap<i64, StoredFile>,
) -> BTreeSet<i64> {
    let mut heading_ids = matched_heading_ids.clone();
    for file_id in matched_file_ids {
        if let Some(file) = files.get(file_id) {
            heading_ids.insert(file.root_heading_id);
        }
    }
    heading_ids
}

pub(in crate::query::result) fn load_files(
    connection: &Connection,
    file_ids: &BTreeSet<i64>,
) -> Result<HashMap<i64, StoredFile>, QueryShapeError> {
    if file_ids.is_empty() {
        return Ok(HashMap::new());
    }

    let chunk_size = id_chunk_capacity(connection, 0);
    let file_ids = file_ids.iter().copied().collect::<Vec<_>>();
    let mut files = HashMap::new();
    for chunk in file_ids.chunks(chunk_size) {
        let sql = format!(
            "/* orgfdb:enrich-files params={} */
             SELECT
                files.id,
                files.path,
                files.mtime_ns,
                files.size,
                files.content_hash,
                files.indexed_at,
                root.id,
                root.title,
                root.title_raw,
                root.line_number
             FROM files
             INNER JOIN headings AS root ON root.file_id = files.id AND root.level = 0
             WHERE files.id IN ({})",
            chunk.len(),
            placeholders(chunk.len())
        );
        let mut statement = connection
            .prepare(&sql)
            .map_err(|source| QueryShapeError::database("load_files.prepare", source))?;
        let rows = statement
            .query_map(params_from_iter(chunk.iter()), |row| {
                let path: String = row.get(1)?;
                let path_ref = Path::new(&path);
                let name = path_ref
                    .file_name()
                    .and_then(|value| value.to_str())
                    .unwrap_or(path.as_str())
                    .to_string();
                let dir = path_ref
                    .parent()
                    .and_then(|value| value.to_str())
                    .unwrap_or(".")
                    .to_string();
                Ok(StoredFile {
                    id: row.get(0)?,
                    path,
                    name,
                    dir,
                    mtime_ns: row.get(2)?,
                    size: row.get(3)?,
                    content_hash: row.get(4)?,
                    indexed_at: row.get(5)?,
                    root_heading_id: row.get(6)?,
                    root_title: row.get(7)?,
                    root_title_raw: row.get(8)?,
                    root_line_number: row.get(9)?,
                })
            })
            .map_err(|source| QueryShapeError::database("load_files.query", source))?;

        for row in rows {
            let file =
                row.map_err(|source| QueryShapeError::database("load_files.collect", source))?;
            files.insert(file.id, file);
        }
    }
    Ok(files)
}

pub(in crate::query::result) fn load_file_ids_for_headings(
    connection: &Connection,
    heading_ids: &[i64],
) -> Result<BTreeSet<i64>, QueryShapeError> {
    if heading_ids.is_empty() {
        return Ok(BTreeSet::new());
    }

    let chunk_size = id_chunk_capacity(connection, 0);
    let mut file_ids = BTreeSet::new();
    for chunk in heading_ids.chunks(chunk_size) {
        let sql = format!(
            "/* orgfdb:enrich-file-ids params={} */
             SELECT file_id FROM headings WHERE id IN ({})",
            chunk.len(),
            placeholders(chunk.len())
        );
        let mut statement = connection.prepare(&sql).map_err(|source| {
            QueryShapeError::database("load_file_ids_for_headings.prepare", source)
        })?;
        let rows = statement
            .query_map(params_from_iter(chunk.iter()), |row| row.get(0))
            .map_err(|source| {
                QueryShapeError::database("load_file_ids_for_headings.query", source)
            })?;

        for row in rows {
            file_ids.insert(row.map_err(|source| {
                QueryShapeError::database("load_file_ids_for_headings.collect", source)
            })?);
        }
    }
    Ok(file_ids)
}

pub(in crate::query::result) fn load_headings_for_files(
    connection: &Connection,
    file_ids: &BTreeSet<i64>,
) -> Result<HashMap<i64, StoredHeading>, QueryShapeError> {
    load_headings(connection, "headings.file_id", file_ids)
}

pub(in crate::query::result) fn load_headings(
    connection: &Connection,
    filter_column: &str,
    ids: &BTreeSet<i64>,
) -> Result<HashMap<i64, StoredHeading>, QueryShapeError> {
    if ids.is_empty() {
        return Ok(HashMap::new());
    }

    let chunk_size = id_chunk_capacity(connection, 0);
    let ids = ids.iter().copied().collect::<Vec<_>>();
    let mut headings = HashMap::new();
    for chunk in ids.chunks(chunk_size) {
        let sql = format!(
            "/* orgfdb:enrich-headings params={} */
             SELECT
                headings.id,
                headings.file_id,
                headings.parent_id,
                headings.level,
                headings.line_number,
                headings.byte_start,
                headings.byte_end,
                headings.title,
                headings.title_raw,
                headings.todo_keyword,
                headings.todo_type,
                headings.priority,
                headings.scheduled_raw,
                headings.scheduled_ts,
                headings.deadline_raw,
                headings.deadline_ts,
                headings.closed_raw,
                headings.closed_ts,
                headings.archivedp,
                headings.footnote_section_p,
                outline_path.breadcrumbs_json
             FROM headings
             LEFT JOIN outline_path ON outline_path.heading_id = headings.id
             WHERE {filter_column} IN ({})",
            chunk.len(),
            placeholders(chunk.len())
        );
        let mut statement = connection
            .prepare(&sql)
            .map_err(|source| QueryShapeError::database("load_headings.prepare", source))?;
        let rows = statement
            .query_map(params_from_iter(chunk.iter()), |row| {
                let priority: Option<String> = row.get(11)?;
                Ok((
                    row.get::<_, i64>(0)?,
                    row.get::<_, Option<String>>(20)?,
                    StoredHeading {
                        id: row.get(0)?,
                        file_id: row.get(1)?,
                        parent_id: row.get(2)?,
                        level: row.get(3)?,
                        line_number: row.get(4)?,
                        byte_start: row.get(5)?,
                        byte_end: row.get(6)?,
                        title: row.get(7)?,
                        title_raw: row.get(8)?,
                        todo_keyword: row.get(9)?,
                        todo_type: row.get(10)?,
                        priority,
                        scheduled_raw: row.get(12)?,
                        scheduled_ts: row.get(13)?,
                        deadline_raw: row.get(14)?,
                        deadline_ts: row.get(15)?,
                        closed_raw: row.get(16)?,
                        closed_ts: row.get(17)?,
                        archivedp: row.get::<_, i64>(18)? != 0,
                        footnote_section_p: row.get::<_, i64>(19)? != 0,
                        all_tags: Vec::new(),
                    },
                ))
            })
            .map_err(|source| QueryShapeError::database("load_headings.query", source))?;

        for heading in rows {
            let (id, breadcrumbs_json, heading) = heading
                .map_err(|source| QueryShapeError::database("load_headings.collect", source))?;
            let breadcrumbs_json = breadcrumbs_json.ok_or_else(|| {
                QueryShapeError::missing(format!(
                    "missing outline_path row for stored heading id {id}"
                ))
            })?;
            let _: Vec<String> = serde_json::from_str(&breadcrumbs_json)
                .map_err(|source| QueryShapeError::invalid_json("breadcrumbs_json", id, source))?;
            headings.insert(heading.id, heading);
        }
    }
    load_effective_tags_for_stored_headings(connection, &mut headings)?;
    Ok(headings)
}

pub(in crate::query::result) fn load_properties_from_relation(
    connection: &Connection,
    relation: &MatchedSqlRelation,
    include_headings: bool,
    include_roots: bool,
) -> Result<Vec<StoredProperty>, QueryShapeError> {
    let mut properties = Vec::new();
    if let Some(compiled) = relation.heading_relation().filter(|_| include_headings) {
        load_properties_from_compiled_relation(
            connection,
            compiled,
            heading_relation_columns(),
            "id",
            &mut properties,
        )?;
    }
    if let Some(compiled) = relation.root_relation().filter(|_| include_roots) {
        load_properties_from_compiled_relation(
            connection,
            compiled,
            file_relation_columns(),
            "root_heading_id",
            &mut properties,
        )?;
    }
    properties.sort_by(|left, right| {
        left.heading_id
            .cmp(&right.heading_id)
            .then_with(|| left.fact.line_number.cmp(&right.fact.line_number))
            .then_with(|| left.id.cmp(&right.id))
    });
    Ok(properties)
}

pub(in crate::query::result) fn load_properties_from_compiled_relation(
    connection: &Connection,
    compiled: &crate::query::CompiledSqlQuery,
    columns: &str,
    heading_id_column: &str,
    properties: &mut Vec<StoredProperty>,
) -> Result<(), QueryShapeError> {
    let sql = format!(
        "/* orgfdb:enrich-properties params={} */
         WITH matched({columns}) AS ({})
         SELECT properties.id, properties.heading_id, properties.key, properties.value,
                properties.source, properties.append, properties.line_number, properties.source
         FROM matched
         INNER JOIN properties
           ON properties.heading_id = matched.{heading_id_column}",
        compiled.params.len(),
        compiled.sql,
    );
    let mut statement = connection
        .prepare(&sql)
        .map_err(|source| QueryShapeError::database("load_properties_relation.prepare", source))?;
    let rows = statement
        .query_map(params_from_iter(compiled.params.iter()), |row| {
            Ok(StoredProperty {
                id: row.get(0)?,
                heading_id: row.get(1)?,
                fact: PropertyFact {
                    key: row.get(2)?,
                    value: row.get(3)?,
                    source: row.get(4)?,
                    append: row.get::<_, i64>(5)? != 0,
                    line_number: row.get(6)?,
                },
            })
        })
        .map_err(|source| QueryShapeError::database("load_properties_relation.query", source))?;
    properties.extend(
        rows.collect::<Result<Vec<_>, _>>().map_err(|source| {
            QueryShapeError::database("load_properties_relation.collect", source)
        })?,
    );
    Ok(())
}

pub(in crate::query::result) fn load_effective_properties_from_relation(
    connection: &Connection,
    relation: &MatchedSqlRelation,
    heading_ids: &BTreeSet<i64>,
    include_headings: bool,
    include_roots: bool,
) -> Result<HashMap<i64, Vec<EffectivePropertyFact>>, QueryShapeError> {
    let mut effective = heading_ids
        .iter()
        .copied()
        .map(|heading_id| (heading_id, Vec::new()))
        .collect::<HashMap<_, _>>();
    if let Some(compiled) = relation.heading_relation().filter(|_| include_headings) {
        load_effective_properties_from_compiled_relation(
            connection,
            compiled,
            heading_relation_columns(),
            "id",
            &mut effective,
        )?;
    }
    if let Some(compiled) = relation.root_relation().filter(|_| include_roots) {
        load_effective_properties_from_compiled_relation(
            connection,
            compiled,
            file_relation_columns(),
            "root_heading_id",
            &mut effective,
        )?;
    }
    for facts in effective.values_mut() {
        facts.sort_by(|left, right| left.key.cmp(&right.key));
    }
    Ok(effective)
}

pub(in crate::query::result) fn load_effective_properties_from_compiled_relation(
    connection: &Connection,
    compiled: &crate::query::CompiledSqlQuery,
    columns: &str,
    heading_id_column: &str,
    effective: &mut HashMap<i64, Vec<EffectivePropertyFact>>,
) -> Result<(), QueryShapeError> {
    let sql = format!(
        "/* orgfdb:enrich-effective-properties params={} */
         WITH matched({columns}) AS ({})
         SELECT effective_properties.heading_id,
                effective_properties.key,
                effective_properties.effective_value
         FROM matched
         INNER JOIN effective_properties
           ON effective_properties.heading_id = matched.{heading_id_column}",
        compiled.params.len(),
        compiled.sql,
    );
    let mut statement = connection.prepare(&sql).map_err(|source| {
        QueryShapeError::database("load_effective_properties_relation.prepare", source)
    })?;
    let rows = statement
        .query_map(params_from_iter(compiled.params.iter()), |row| {
            Ok((
                row.get::<_, i64>(0)?,
                EffectivePropertyFact {
                    key: row.get(1)?,
                    value: Some(row.get(2)?),
                },
            ))
        })
        .map_err(|source| {
            QueryShapeError::database("load_effective_properties_relation.query", source)
        })?;
    for row in rows {
        let (heading_id, fact) = row.map_err(|source| {
            QueryShapeError::database("load_effective_properties_relation.collect", source)
        })?;
        effective.entry(heading_id).or_default().push(fact);
    }
    Ok(())
}

pub(in crate::query::result) fn load_keywords_from_relation(
    connection: &Connection,
    relation: &MatchedSqlRelation,
    include_headings: bool,
    include_roots: bool,
) -> Result<Vec<StoredKeyword>, QueryShapeError> {
    let mut keywords = Vec::new();
    if let Some(compiled) = relation.heading_relation().filter(|_| include_headings) {
        load_keywords_from_compiled_relation(
            connection,
            compiled,
            heading_relation_columns(),
            "id",
            &mut keywords,
        )?;
    }
    if let Some(compiled) = relation.root_relation().filter(|_| include_roots) {
        load_keywords_from_compiled_relation(
            connection,
            compiled,
            file_relation_columns(),
            "root_heading_id",
            &mut keywords,
        )?;
    }
    keywords.sort_by(|left, right| {
        left.heading_id
            .cmp(&right.heading_id)
            .then_with(|| left.fact.line_number.cmp(&right.fact.line_number))
            .then_with(|| left.id.cmp(&right.id))
    });
    Ok(keywords)
}

pub(in crate::query::result) fn load_keywords_from_compiled_relation(
    connection: &Connection,
    compiled: &crate::query::CompiledSqlQuery,
    columns: &str,
    heading_id_column: &str,
    keywords: &mut Vec<StoredKeyword>,
) -> Result<(), QueryShapeError> {
    let sql = format!(
        "/* orgfdb:enrich-keywords params={} */
         WITH matched({columns}) AS ({})
         SELECT keywords.id, keywords.heading_id, keywords.keyword,
                keywords.value, keywords.line_number
         FROM matched
         INNER JOIN keywords
           ON keywords.heading_id = matched.{heading_id_column}",
        compiled.params.len(),
        compiled.sql,
    );
    let mut statement = connection
        .prepare(&sql)
        .map_err(|source| QueryShapeError::database("load_keywords_relation.prepare", source))?;
    let rows = statement
        .query_map(params_from_iter(compiled.params.iter()), |row| {
            Ok(StoredKeyword {
                id: row.get(0)?,
                heading_id: row.get(1)?,
                fact: KeywordFact {
                    keyword: row.get(2)?,
                    value: row.get(3)?,
                    line_number: row.get(4)?,
                },
            })
        })
        .map_err(|source| QueryShapeError::database("load_keywords_relation.query", source))?;
    keywords.extend(
        rows.collect::<Result<Vec<_>, _>>().map_err(|source| {
            QueryShapeError::database("load_keywords_relation.collect", source)
        })?,
    );
    Ok(())
}

pub(in crate::query::result) fn load_root_tags_from_relation(
    connection: &Connection,
    relation: &MatchedSqlRelation,
) -> Result<HashMap<i64, Vec<String>>, QueryShapeError> {
    let Some(compiled) = relation.root_relation() else {
        return Ok(HashMap::new());
    };
    let sql = format!(
        "/* orgfdb:enrich-tags params={} */
         WITH matched({}) AS ({})
         SELECT matched.root_heading_id, effective_tags.position, effective_tags.tag
         FROM matched
         INNER JOIN effective_tags
           ON effective_tags.heading_id = matched.root_heading_id",
        compiled.params.len(),
        file_relation_columns(),
        compiled.sql,
    );
    let mut statement = connection
        .prepare(&sql)
        .map_err(|source| QueryShapeError::database("load_root_tags_relation.prepare", source))?;
    let rows = statement
        .query_map(params_from_iter(compiled.params.iter()), |row| {
            Ok((
                row.get::<_, i64>(0)?,
                row.get::<_, i64>(1)?,
                row.get::<_, String>(2)?,
            ))
        })
        .map_err(|source| QueryShapeError::database("load_root_tags_relation.query", source))?;
    let mut positioned = HashMap::<i64, Vec<(i64, String)>>::new();
    for row in rows {
        let (heading_id, position, tag) = row.map_err(|source| {
            QueryShapeError::database("load_root_tags_relation.collect", source)
        })?;
        positioned
            .entry(heading_id)
            .or_default()
            .push((position, tag));
    }
    Ok(positioned
        .into_iter()
        .map(|(heading_id, mut tags)| {
            tags.sort_by_key(|(position, _)| *position);
            (
                heading_id,
                tags.into_iter().map(|(_, tag)| tag).collect::<Vec<_>>(),
            )
        })
        .collect())
}

pub(in crate::query::result) fn load_effective_tags_for_stored_headings(
    connection: &Connection,
    headings: &mut HashMap<i64, StoredHeading>,
) -> Result<(), QueryShapeError> {
    if headings.is_empty() {
        return Ok(());
    }

    let heading_ids = headings.keys().copied().collect::<BTreeSet<_>>();
    for (heading_id, tags) in load_effective_tags_for_heading_ids(connection, &heading_ids)? {
        let heading = headings.get_mut(&heading_id).ok_or_else(|| {
            QueryShapeError::missing(format!(
                "effective_tags references unloaded heading id {heading_id}"
            ))
        })?;
        heading.all_tags = tags;
    }
    Ok(())
}

pub(in crate::query::result) fn load_effective_tags_for_heading_ids(
    connection: &Connection,
    heading_ids: &BTreeSet<i64>,
) -> Result<HashMap<i64, Vec<String>>, QueryShapeError> {
    if heading_ids.is_empty() {
        return Ok(HashMap::new());
    }

    let chunk_size = id_chunk_capacity(connection, 0);
    let heading_ids = heading_ids.iter().copied().collect::<Vec<_>>();
    let mut positioned_tags = HashMap::<i64, Vec<(i64, String)>>::new();
    for chunk in heading_ids.chunks(chunk_size) {
        let sql = format!(
            "/* orgfdb:enrich-tags params={} */
             SELECT heading_id, position, tag
             FROM effective_tags
             WHERE heading_id IN ({})",
            chunk.len(),
            placeholders(chunk.len())
        );
        let mut statement = connection
            .prepare(&sql)
            .map_err(|source| QueryShapeError::database("load_effective_tags.prepare", source))?;
        let rows = statement
            .query_map(params_from_iter(chunk.iter()), |row| {
                Ok((
                    row.get::<_, i64>(0)?,
                    row.get::<_, i64>(1)?,
                    row.get::<_, String>(2)?,
                ))
            })
            .map_err(|source| QueryShapeError::database("load_effective_tags.query", source))?;
        for row in rows {
            let (heading_id, position, tag) = row.map_err(|source| {
                QueryShapeError::database("load_effective_tags.collect", source)
            })?;
            positioned_tags
                .entry(heading_id)
                .or_default()
                .push((position, tag));
        }
    }

    Ok(positioned_tags
        .into_iter()
        .map(|(heading_id, mut tags)| {
            tags.sort_by_key(|(position, _)| *position);
            (
                heading_id,
                tags.into_iter().map(|(_, tag)| tag).collect::<Vec<_>>(),
            )
        })
        .collect())
}

pub(in crate::query::result) fn load_properties(
    connection: &Connection,
    heading_ids: &BTreeSet<i64>,
) -> Result<Vec<StoredProperty>, QueryShapeError> {
    if heading_ids.is_empty() {
        return Ok(Vec::new());
    }

    let chunk_size = id_chunk_capacity(connection, 0);
    let target_ids = heading_ids.iter().copied().collect::<Vec<_>>();
    let mut properties = Vec::new();
    for chunk in target_ids.chunks(chunk_size) {
        let sql = format!(
            "/* orgfdb:enrich-properties params={} */
             SELECT properties.id, properties.heading_id, properties.key, properties.value,
                    properties.source, properties.append, properties.line_number, properties.source
             FROM properties
             WHERE properties.heading_id IN ({})",
            chunk.len(),
            placeholders(chunk.len())
        );
        let mut statement = connection
            .prepare(&sql)
            .map_err(|source| QueryShapeError::database("load_properties.prepare", source))?;
        let rows = statement
            .query_map(params_from_iter(chunk.iter()), |row| {
                Ok(StoredProperty {
                    id: row.get(0)?,
                    heading_id: row.get(1)?,
                    fact: PropertyFact {
                        key: row.get(2)?,
                        value: row.get(3)?,
                        source: row.get(4)?,
                        append: row.get::<_, i64>(5)? != 0,
                        line_number: row.get(6)?,
                    },
                })
            })
            .map_err(|source| QueryShapeError::database("load_properties.query", source))?;
        properties.extend(
            rows.collect::<Result<Vec<_>, _>>()
                .map_err(|source| QueryShapeError::database("load_properties.collect", source))?,
        );
    }
    properties.sort_by(|left, right| {
        left.heading_id
            .cmp(&right.heading_id)
            .then_with(|| left.fact.line_number.cmp(&right.fact.line_number))
            .then_with(|| left.id.cmp(&right.id))
    });
    Ok(properties)
}

pub(in crate::query::result) fn load_effective_properties(
    connection: &Connection,
    heading_ids: &BTreeSet<i64>,
) -> Result<HashMap<i64, Vec<EffectivePropertyFact>>, QueryShapeError> {
    if heading_ids.is_empty() {
        return Ok(HashMap::new());
    }

    let chunk_size = id_chunk_capacity(connection, 0);
    let target_ids = heading_ids.iter().copied().collect::<Vec<_>>();
    let mut effective = target_ids
        .iter()
        .copied()
        .map(|heading_id| (heading_id, Vec::new()))
        .collect::<HashMap<_, _>>();
    for chunk in target_ids.chunks(chunk_size) {
        let sql = format!(
            "/* orgfdb:enrich-effective-properties params={} */
             SELECT heading_id, key, effective_value
             FROM effective_properties
             WHERE heading_id IN ({})",
            chunk.len(),
            placeholders(chunk.len())
        );
        let mut statement = connection.prepare(&sql).map_err(|source| {
            QueryShapeError::database("load_effective_properties.prepare", source)
        })?;
        let rows = statement
            .query_map(params_from_iter(chunk.iter()), |row| {
                Ok((
                    row.get::<_, i64>(0)?,
                    EffectivePropertyFact {
                        key: row.get(1)?,
                        value: Some(row.get(2)?),
                    },
                ))
            })
            .map_err(|source| {
                QueryShapeError::database("load_effective_properties.query", source)
            })?;
        for row in rows {
            let (heading_id, fact) = row.map_err(|source| {
                QueryShapeError::database("load_effective_properties.collect", source)
            })?;
            effective.entry(heading_id).or_default().push(fact);
        }
    }
    for facts in effective.values_mut() {
        facts.sort_by(|left, right| left.key.cmp(&right.key));
    }
    Ok(effective)
}

pub(in crate::query::result) fn load_keywords(
    connection: &Connection,
    heading_ids: &BTreeSet<i64>,
) -> Result<Vec<StoredKeyword>, QueryShapeError> {
    if heading_ids.is_empty() {
        return Ok(Vec::new());
    }

    let chunk_size = id_chunk_capacity(connection, 0);
    let target_ids = heading_ids.iter().copied().collect::<Vec<_>>();
    let mut keywords = Vec::new();
    for chunk in target_ids.chunks(chunk_size) {
        let sql = format!(
            "/* orgfdb:enrich-keywords params={} */
             SELECT id, heading_id, keyword, value, line_number
             FROM keywords
             WHERE heading_id IN ({})",
            chunk.len(),
            placeholders(chunk.len())
        );
        let mut statement = connection
            .prepare(&sql)
            .map_err(|source| QueryShapeError::database("load_keywords.prepare", source))?;
        let rows = statement
            .query_map(params_from_iter(chunk.iter()), |row| {
                Ok(StoredKeyword {
                    id: row.get(0)?,
                    heading_id: row.get(1)?,
                    fact: KeywordFact {
                        keyword: row.get(2)?,
                        value: row.get(3)?,
                        line_number: row.get(4)?,
                    },
                })
            })
            .map_err(|source| QueryShapeError::database("load_keywords.query", source))?;
        keywords.extend(
            rows.collect::<Result<Vec<_>, _>>()
                .map_err(|source| QueryShapeError::database("load_keywords.collect", source))?,
        );
    }
    keywords.sort_by(|left, right| {
        left.heading_id
            .cmp(&right.heading_id)
            .then_with(|| left.fact.line_number.cmp(&right.fact.line_number))
            .then_with(|| left.id.cmp(&right.id))
    });
    Ok(keywords)
}

pub(in crate::query::result) fn sort_link_map_groups(
    grouped: &mut HashMap<i64, Vec<StoredLink>>,
    files: &HashMap<i64, StoredFile>,
) -> Result<(), QueryShapeError> {
    for links in grouped.values_mut() {
        if let Some(link) = links.iter().find(|link| !files.contains_key(&link.file_id)) {
            return Err(QueryShapeError::missing(format!(
                "missing stored file row for included link {} source file {}",
                link.id, link.file_id
            )));
        }
        links.sort_by(|left, right| {
            let left_path = &files
                .get(&left.file_id)
                .expect("link source files were checked above")
                .path;
            let right_path = &files
                .get(&right.file_id)
                .expect("link source files were checked above")
                .path;
            left_path
                .cmp(right_path)
                .then_with(|| left.byte_start.cmp(&right.byte_start))
                .then_with(|| left.id.cmp(&right.id))
        });
    }
    Ok(())
}

pub(in crate::query::result) fn load_links_by_file(
    connection: &Connection,
    file_ids: &BTreeSet<i64>,
) -> Result<HashMap<i64, Vec<StoredLink>>, QueryShapeError> {
    load_link_map(connection, "links.file_id", file_ids)
}

pub(in crate::query::result) fn load_backlinks_by_file(
    connection: &Connection,
    file_ids: &BTreeSet<i64>,
) -> Result<HashMap<i64, Vec<StoredLink>>, QueryShapeError> {
    load_link_map(connection, "links.target_file_id", file_ids)
}

pub(in crate::query::result) fn load_links_by_heading(
    connection: &Connection,
    heading_ids: &BTreeSet<i64>,
) -> Result<HashMap<i64, Vec<StoredLink>>, QueryShapeError> {
    load_link_map(connection, "links.heading_id", heading_ids)
}

pub(in crate::query::result) fn load_backlinks_by_heading(
    connection: &Connection,
    heading_ids: &BTreeSet<i64>,
) -> Result<HashMap<i64, Vec<StoredLink>>, QueryShapeError> {
    load_link_map(connection, "links.target_heading_id", heading_ids)
}

pub(in crate::query::result) fn load_link_map(
    connection: &Connection,
    id_column: &str,
    ids: &BTreeSet<i64>,
) -> Result<HashMap<i64, Vec<StoredLink>>, QueryShapeError> {
    if ids.is_empty() {
        return Ok(HashMap::new());
    }

    let chunk_size = id_chunk_capacity(connection, 0);
    let ids = ids.iter().copied().collect::<Vec<_>>();
    let mut grouped = HashMap::<i64, Vec<StoredLink>>::new();
    for chunk in ids.chunks(chunk_size) {
        let sql = format!(
            "/* orgfdb:enrich-links params={} */
             SELECT
                links.id,
                links.file_id,
                links.heading_id,
                links.source_context,
                links.format,
                links.link_type,
                links.raw,
                links.raw_target,
                links.raw_description,
                links.path,
                links.search_option,
                links.path_absolute,
                links.target_file_id,
                links.target_heading_id,
                links.target_custom_id,
                links.target_id,
                links.resolution_status,
                links.resolution_diagnostic,
                links.byte_start,
                links.byte_end,
                links.line,
                {id_column}
             FROM links
             WHERE {id_column} IN ({})",
            chunk.len(),
            placeholders(chunk.len())
        );
        let mut statement = connection
            .prepare(&sql)
            .map_err(|source| QueryShapeError::database("load_link_map.prepare", source))?;
        let rows = statement
            .query_map(params_from_iter(chunk.iter()), |row| {
                Ok((
                    row.get::<_, i64>(21)?,
                    StoredLink {
                        id: row.get(0)?,
                        file_id: row.get(1)?,
                        heading_id: row.get(2)?,
                        source_context: row.get(3)?,
                        format: row.get(4)?,
                        link_type: row.get(5)?,
                        raw: row.get(6)?,
                        raw_target: row.get(7)?,
                        raw_description: row.get(8)?,
                        link_path: row.get(9)?,
                        search_option: row.get(10)?,
                        path_absolute: row.get(11)?,
                        target_file_id: row.get(12)?,
                        target_heading_id: row.get(13)?,
                        target_custom_id: row.get(14)?,
                        target_id: row.get(15)?,
                        resolution_status: row.get(16)?,
                        resolution_diagnostic: row.get(17)?,
                        byte_start: row.get(18)?,
                        byte_end: row.get(19)?,
                        line: row.get(20)?,
                    },
                ))
            })
            .map_err(|source| QueryShapeError::database("load_link_map.query", source))?;
        for row in rows {
            let (group_id, link) =
                row.map_err(|source| QueryShapeError::database("load_link_map.collect", source))?;
            grouped.entry(group_id).or_default().push(link);
        }
    }
    Ok(grouped)
}

pub(in crate::query::result) fn placeholders(count: usize) -> String {
    std::iter::repeat_n("?", count)
        .collect::<Vec<_>>()
        .join(", ")
}
