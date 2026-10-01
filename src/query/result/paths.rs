use super::*;

pub(in crate::query::result) fn validate_outline_path_relation(
    connection: &Connection,
    relation: &MatchedSqlRelation,
    include_headings: bool,
    include_roots: bool,
) -> Result<(), QueryShapeError> {
    if let Some(compiled) = relation.heading_relation().filter(|_| include_headings) {
        validate_outline_path_compiled_relation(
            connection,
            compiled,
            heading_relation_columns(),
            "id",
        )?;
    }
    if let Some(compiled) = relation.root_relation().filter(|_| include_roots) {
        validate_outline_path_compiled_relation(
            connection,
            compiled,
            file_relation_columns(),
            "root_heading_id",
        )?;
    }
    Ok(())
}

pub(in crate::query::result) fn validate_outline_path_root_relation(
    connection: &Connection,
    relation: &MatchedSqlRelation,
) -> Result<(), QueryShapeError> {
    if let Some(compiled) = relation.root_relation() {
        validate_outline_path_compiled_relation(
            connection,
            compiled,
            file_relation_columns(),
            "root_heading_id",
        )?;
    }
    Ok(())
}

pub(in crate::query::result) fn validate_outline_path_compiled_relation(
    connection: &Connection,
    compiled: &crate::query::CompiledSqlQuery,
    columns: &str,
    heading_id_column: &str,
) -> Result<(), QueryShapeError> {
    let sql = format!(
        "/* orgfdb:validate-outline-path params={} */
         WITH matched({columns}) AS ({})
         SELECT matched.{heading_id_column}, outline_path.breadcrumbs_json
         FROM matched
         LEFT JOIN outline_path
           ON outline_path.heading_id = matched.{heading_id_column}",
        compiled.params.len(),
        compiled.sql,
    );
    let mut statement = connection.prepare(&sql).map_err(|source| {
        QueryShapeError::database("validate_outline_path_relation.prepare", source)
    })?;
    let rows = statement
        .query_map(params_from_iter(compiled.params.iter()), |row| {
            Ok((row.get::<_, i64>(0)?, row.get::<_, Option<String>>(1)?))
        })
        .map_err(|source| {
            QueryShapeError::database("validate_outline_path_relation.query", source)
        })?;

    for row in rows {
        let (heading_id, breadcrumbs_json) = row.map_err(|source| {
            QueryShapeError::database("validate_outline_path_relation.collect", source)
        })?;
        let breadcrumbs_json = breadcrumbs_json.ok_or_else(|| {
            QueryShapeError::missing(format!(
                "missing outline_path row for stored heading id {heading_id}"
            ))
        })?;
        let _: Vec<String> = serde_json::from_str(&breadcrumbs_json).map_err(|source| {
            QueryShapeError::invalid_json("breadcrumbs_json", heading_id, source)
        })?;
    }
    Ok(())
}

pub(in crate::query::result) fn load_heading_paths_recursive_from_relation(
    connection: &Connection,
    relation: &MatchedSqlRelation,
    rows: &[HeadingQueryMatch],
) -> Result<HashMap<i64, Vec<PathEntry>>, QueryShapeError> {
    let Some(compiled) = relation.heading_relation() else {
        return Ok(HashMap::new());
    };
    let expected_ids = rows
        .iter()
        .filter_map(|row| match row {
            HeadingQueryMatch::Heading(row) => Some(row.id),
            HeadingQueryMatch::File(_) => None,
        })
        .collect::<BTreeSet<_>>();
    if expected_ids.is_empty() {
        return Ok(HashMap::new());
    }

    let file_sql = format!(
        "/* orgfdb:enrich-path-files params={} */
         WITH matched({}) AS ({})
         SELECT matched.id, files.id, files.path, root.title, root.title_raw
         FROM matched
         INNER JOIN files ON files.id = matched.file_id
         INNER JOIN headings AS root ON root.file_id = files.id AND root.level = 0",
        compiled.params.len(),
        heading_relation_columns(),
        compiled.sql,
    );
    let mut file_statement = connection
        .prepare(&file_sql)
        .map_err(|source| QueryShapeError::database("load_heading_paths.files.prepare", source))?;
    let file_rows = file_statement
        .query_map(params_from_iter(compiled.params.iter()), |row| {
            Ok((
                row.get::<_, i64>(0)?,
                FilePathEntry {
                    id: row.get(1)?,
                    path: row.get(2)?,
                    title: row.get(3)?,
                    title_raw: row.get(4)?,
                },
            ))
        })
        .map_err(|source| QueryShapeError::database("load_heading_paths.files.query", source))?;
    let mut files_by_heading = HashMap::new();
    for row in file_rows {
        let (heading_id, file) = row.map_err(|source| {
            QueryShapeError::database("load_heading_paths.files.collect", source)
        })?;
        files_by_heading.insert(heading_id, file);
    }

    let chain_sql = format!(
        "/* orgfdb:enrich-path-headings params={} */
         WITH RECURSIVE matched({}) AS ({}),
         chain(origin_id, file_id, id, parent_id, level, title, title_raw, distance) AS (
             SELECT matched.id, matched.file_id, matched.id, matched.parent_id, matched.level,
                    matched.title, matched.title_raw, 0
             FROM matched
             UNION ALL
             SELECT chain.origin_id, chain.file_id, parent.id, parent.parent_id, parent.level,
                    parent.title, parent.title_raw, chain.distance + 1
             FROM chain
             INNER JOIN headings AS parent
               ON parent.id = chain.parent_id
              AND parent.file_id = chain.file_id
             WHERE chain.parent_id IS NOT NULL
         )
         SELECT chain.origin_id, chain.id, chain.parent_id, chain.level, chain.title,
                chain.title_raw, chain.distance, outline_path.breadcrumbs_json
         FROM chain
         LEFT JOIN outline_path ON outline_path.heading_id = chain.id",
        compiled.params.len(),
        heading_relation_columns(),
        compiled.sql,
    );
    let mut chain_statement = connection.prepare(&chain_sql).map_err(|source| {
        QueryShapeError::database("load_heading_paths.headings.prepare", source)
    })?;
    let chain_rows = chain_statement
        .query_map(params_from_iter(compiled.params.iter()), |row| {
            Ok((
                row.get::<_, i64>(0)?,
                row.get::<_, i64>(1)?,
                row.get::<_, Option<i64>>(2)?,
                row.get::<_, i64>(3)?,
                row.get::<_, String>(4)?,
                row.get::<_, Option<String>>(5)?,
                row.get::<_, i64>(6)?,
                row.get::<_, Option<String>>(7)?,
            ))
        })
        .map_err(|source| QueryShapeError::database("load_heading_paths.headings.query", source))?;
    let mut headings_by_origin = HashMap::<i64, Vec<(i64, HeadingPathEntry)>>::new();
    let mut terminal_by_origin = HashMap::<i64, (i64, Option<i64>, i64)>::new();
    for row in chain_rows {
        let (origin_id, heading_id, parent_id, level, title, title_raw, distance, breadcrumbs_json) =
            row.map_err(|source| {
                QueryShapeError::database("load_heading_paths.headings.collect", source)
            })?;
        if terminal_by_origin
            .get(&origin_id)
            .is_none_or(|(_, _, current_distance)| distance > *current_distance)
        {
            terminal_by_origin.insert(origin_id, (level, parent_id, distance));
        }
        let breadcrumbs_json = breadcrumbs_json.ok_or_else(|| {
            QueryShapeError::missing(format!(
                "missing outline_path row for stored heading id {heading_id}"
            ))
        })?;
        let _: Vec<String> = serde_json::from_str(&breadcrumbs_json).map_err(|source| {
            QueryShapeError::invalid_json("breadcrumbs_json", heading_id, source)
        })?;
        if level == 0 {
            continue;
        }
        let title_raw = title_raw.ok_or_else(|| {
            QueryShapeError::missing(format!(
                "missing title_raw for stored heading row {heading_id}"
            ))
        })?;
        headings_by_origin.entry(origin_id).or_default().push((
            distance,
            HeadingPathEntry {
                id: heading_id,
                title,
                title_raw,
                level,
            },
        ));
    }

    let mut paths = HashMap::with_capacity(expected_ids.len());
    for heading_id in expected_ids {
        match terminal_by_origin.remove(&heading_id) {
            Some((level, Some(parent_id), _)) if level > 0 => {
                return Err(QueryShapeError::missing(format!(
                    "missing stored heading row for id {parent_id}"
                )));
            }
            _ => {}
        }
        let file = files_by_heading.remove(&heading_id).ok_or_else(|| {
            QueryShapeError::missing(format!(
                "missing stored file row for heading id {heading_id}"
            ))
        })?;
        let mut headings = headings_by_origin.remove(&heading_id).unwrap_or_default();
        headings.sort_by_key(|entry| std::cmp::Reverse(entry.0));
        let mut path = Vec::with_capacity(headings.len() + 1);
        path.push(PathEntry::File(file));
        path.extend(
            headings
                .into_iter()
                .map(|(_, heading)| PathEntry::Heading(heading)),
        );
        paths.insert(heading_id, path);
    }
    Ok(paths)
}

#[derive(Debug, Clone)]
pub(in crate::query::result) struct RustPathAncestor {
    pub(in crate::query::result) id: i64,
    pub(in crate::query::result) file_id: i64,
    pub(in crate::query::result) parent_id: Option<i64>,
    pub(in crate::query::result) level: i64,
    pub(in crate::query::result) title: String,
    pub(in crate::query::result) title_raw: Option<String>,
}

#[derive(Debug)]
pub(in crate::query::result) struct PendingRustPath {
    pub(in crate::query::result) origin_id: i64,
    pub(in crate::query::result) file_id: i64,
    pub(in crate::query::result) file_path: String,
    pub(in crate::query::result) path: Vec<PathEntry>,
}

pub(crate) fn load_heading_paths_from_relation(
    connection: &Connection,
    relation: &MatchedSqlRelation,
    rows: &[HeadingQueryMatch],
) -> Result<HashMap<i64, Vec<PathEntry>>, QueryShapeError> {
    if relation.heading_relation().is_none() {
        return Ok(HashMap::new());
    }

    let matched_by_id = rows
        .iter()
        .filter_map(|row| match row {
            HeadingQueryMatch::Heading(row) => Some((row.id, row)),
            HeadingQueryMatch::File(_) => None,
        })
        .collect::<HashMap<_, _>>();
    let expected_ids = matched_by_id.keys().copied().collect::<BTreeSet<_>>();
    if expected_ids.is_empty() {
        return Ok(HashMap::new());
    }

    validate_outline_path_rows(connection, &expected_ids)?;

    let mut frontier = matched_by_id
        .values()
        .filter_map(|row| row.parent_id)
        .collect::<BTreeSet<_>>();
    let mut expanded = BTreeSet::new();
    let mut ancestors = HashMap::<i64, RustPathAncestor>::new();

    while !frontier.is_empty() {
        let to_load = frontier
            .iter()
            .copied()
            .filter(|heading_id| {
                !matched_by_id.contains_key(heading_id) && !ancestors.contains_key(heading_id)
            })
            .collect::<BTreeSet<_>>();
        if !to_load.is_empty() {
            ancestors.extend(load_rust_path_ancestors(connection, &to_load)?);
        }

        let mut next_frontier = BTreeSet::new();
        for heading_id in frontier {
            if !expanded.insert(heading_id) {
                continue;
            }
            let (level, parent_id) = if let Some(row) = matched_by_id.get(&heading_id) {
                (row.level, row.parent_id)
            } else {
                let row = ancestors.get(&heading_id).ok_or_else(|| {
                    QueryShapeError::missing(format!(
                        "missing stored heading row for id {heading_id}"
                    ))
                })?;
                (row.level, row.parent_id)
            };
            if let Some(parent_id) = parent_id.filter(|_| level > 0) {
                next_frontier.insert(parent_id);
            }
        }
        frontier = next_frontier;
    }

    let mut paths = HashMap::with_capacity(matched_by_id.len());
    let mut pending = Vec::new();
    let mut fallback_file_ids = BTreeSet::new();
    let mut seen = Vec::new();
    for origin in matched_by_id.values() {
        let mut path = Vec::with_capacity(origin.level.max(0) as usize + 1);
        let mut file = None;
        let mut current_id = Some(origin.id);
        seen.clear();
        while let Some(heading_id) = current_id {
            if seen.contains(&heading_id) {
                return Err(QueryShapeError::missing(format!(
                    "cyclic stored heading parent chain at id {heading_id}"
                )));
            }
            seen.push(heading_id);

            if let Some(row) = matched_by_id.get(&heading_id) {
                if heading_id != origin.id && row.file_id != origin.file_id {
                    return Err(QueryShapeError::missing(format!(
                        "missing stored heading row for id {heading_id}"
                    )));
                }
                if row.level == 0 {
                    file = Some(FilePathEntry {
                        id: origin.file_id,
                        path: origin.file_path.clone(),
                        title: row.title.clone(),
                        title_raw: row.title_raw.clone(),
                    });
                    break;
                }
                let title_raw = row.title_raw.clone().ok_or_else(|| {
                    QueryShapeError::missing(format!(
                        "missing title_raw for stored heading row {heading_id}"
                    ))
                })?;
                path.push(PathEntry::Heading(HeadingPathEntry {
                    id: row.id,
                    title: row.title.clone(),
                    title_raw,
                    level: row.level,
                }));
                current_id = row.parent_id;
                continue;
            }

            let row = ancestors.get(&heading_id).ok_or_else(|| {
                QueryShapeError::missing(format!("missing stored heading row for id {heading_id}"))
            })?;
            if row.file_id != origin.file_id {
                return Err(QueryShapeError::missing(format!(
                    "missing stored heading row for id {heading_id}"
                )));
            }
            if row.level == 0 {
                file = Some(FilePathEntry {
                    id: origin.file_id,
                    path: origin.file_path.clone(),
                    title: row.title.clone(),
                    title_raw: row.title_raw.clone(),
                });
                break;
            }
            let title_raw = row.title_raw.clone().ok_or_else(|| {
                QueryShapeError::missing(format!(
                    "missing title_raw for stored heading row {heading_id}"
                ))
            })?;
            path.push(PathEntry::Heading(HeadingPathEntry {
                id: row.id,
                title: row.title.clone(),
                title_raw,
                level: row.level,
            }));
            current_id = row.parent_id;
        }
        path.reverse();
        match file {
            Some(file) => {
                path.insert(0, PathEntry::File(file));
                paths.insert(origin.id, path);
            }
            None => {
                fallback_file_ids.insert(origin.file_id);
                pending.push(PendingRustPath {
                    origin_id: origin.id,
                    file_id: origin.file_id,
                    file_path: origin.file_path.clone(),
                    path,
                });
            }
        }
    }

    let fallback_roots = if fallback_file_ids.is_empty() {
        HashMap::new()
    } else {
        load_rust_path_file_roots(connection, &fallback_file_ids)?
    };

    for mut pending in pending {
        let (title, title_raw) = fallback_roots.get(&pending.file_id).ok_or_else(|| {
            QueryShapeError::missing(format!(
                "missing stored file row for heading id {}",
                pending.origin_id
            ))
        })?;
        pending.path.insert(
            0,
            PathEntry::File(FilePathEntry {
                id: pending.file_id,
                path: pending.file_path,
                title: title.clone(),
                title_raw: title_raw.clone(),
            }),
        );
        paths.insert(pending.origin_id, pending.path);
    }
    Ok(paths)
}

pub(in crate::query::result) fn load_rust_path_ancestors(
    connection: &Connection,
    heading_ids: &BTreeSet<i64>,
) -> Result<HashMap<i64, RustPathAncestor>, QueryShapeError> {
    let chunk_size = id_chunk_capacity(connection, 0);
    let heading_ids = heading_ids.iter().copied().collect::<Vec<_>>();
    let mut ancestors = HashMap::new();
    for chunk in heading_ids.chunks(chunk_size) {
        let sql = format!(
            "/* orgfdb:enrich-path-rust-ancestors params={} */
             SELECT headings.id, headings.file_id, headings.parent_id, headings.level,
                    headings.title, headings.title_raw, outline_path.breadcrumbs_json
             FROM headings
             LEFT JOIN outline_path ON outline_path.heading_id = headings.id
             WHERE headings.id IN ({})",
            chunk.len(),
            placeholders(chunk.len())
        );
        let mut statement = connection.prepare(&sql).map_err(|source| {
            QueryShapeError::database("load_heading_paths.rust_ancestors.prepare", source)
        })?;
        let rows = statement
            .query_map(params_from_iter(chunk.iter()), |row| {
                Ok((
                    RustPathAncestor {
                        id: row.get(0)?,
                        file_id: row.get(1)?,
                        parent_id: row.get(2)?,
                        level: row.get(3)?,
                        title: row.get(4)?,
                        title_raw: row.get(5)?,
                    },
                    row.get::<_, Option<String>>(6)?,
                ))
            })
            .map_err(|source| {
                QueryShapeError::database("load_heading_paths.rust_ancestors.query", source)
            })?;
        let mut found = BTreeSet::new();
        for row in rows {
            let (ancestor, breadcrumbs_json) = row.map_err(|source| {
                QueryShapeError::database("load_heading_paths.rust_ancestors.collect", source)
            })?;
            validate_outline_json(ancestor.id, breadcrumbs_json)?;
            found.insert(ancestor.id);
            ancestors.insert(ancestor.id, ancestor);
        }
        ensure_all_heading_ids_found(chunk, &found)?;
    }
    Ok(ancestors)
}

pub(in crate::query::result) fn ensure_all_heading_ids_found(
    heading_ids: &[i64],
    found: &BTreeSet<i64>,
) -> Result<(), QueryShapeError> {
    for heading_id in heading_ids {
        if !found.contains(heading_id) {
            return Err(QueryShapeError::missing(format!(
                "missing stored heading row for id {heading_id}"
            )));
        }
    }
    Ok(())
}

pub(in crate::query::result) fn validate_outline_json(
    heading_id: i64,
    breadcrumbs_json: Option<String>,
) -> Result<(), QueryShapeError> {
    let breadcrumbs_json = breadcrumbs_json.ok_or_else(|| {
        QueryShapeError::missing(format!(
            "missing outline_path row for stored heading id {heading_id}"
        ))
    })?;
    let _: Vec<String> = serde_json::from_str(&breadcrumbs_json)
        .map_err(|source| QueryShapeError::invalid_json("breadcrumbs_json", heading_id, source))?;
    Ok(())
}

pub(in crate::query::result) fn load_rust_path_file_roots(
    connection: &Connection,
    file_ids: &BTreeSet<i64>,
) -> Result<HashMap<i64, (String, Option<String>)>, QueryShapeError> {
    if file_ids.is_empty() {
        return Ok(HashMap::new());
    }

    let chunk_size = id_chunk_capacity(connection, 0);
    let file_ids = file_ids.iter().copied().collect::<Vec<_>>();
    let mut roots = HashMap::new();
    for chunk in file_ids.chunks(chunk_size) {
        let sql = format!(
            "/* orgfdb:enrich-path-rust-file-roots params={} */
             SELECT files.id, root.title, root.title_raw
             FROM files
             INNER JOIN headings AS root ON root.file_id = files.id AND root.level = 0
             WHERE files.id IN ({})",
            chunk.len(),
            placeholders(chunk.len())
        );
        let mut statement = connection.prepare(&sql).map_err(|source| {
            QueryShapeError::database("load_heading_paths.rust_file_roots.prepare", source)
        })?;
        let rows = statement
            .query_map(params_from_iter(chunk.iter()), |row| {
                Ok((
                    row.get::<_, i64>(0)?,
                    row.get::<_, String>(1)?,
                    row.get::<_, Option<String>>(2)?,
                ))
            })
            .map_err(|source| {
                QueryShapeError::database("load_heading_paths.rust_file_roots.query", source)
            })?;
        for row in rows {
            let (file_id, title, title_raw) = row.map_err(|source| {
                QueryShapeError::database("load_heading_paths.rust_file_roots.collect", source)
            })?;
            roots.insert(file_id, (title, title_raw));
        }
    }
    Ok(roots)
}

pub(in crate::query::result) fn validate_outline_path_rows(
    connection: &Connection,
    heading_ids: &BTreeSet<i64>,
) -> Result<(), QueryShapeError> {
    if heading_ids.is_empty() {
        return Ok(());
    }

    let chunk_size = id_chunk_capacity(connection, 0);
    let heading_ids = heading_ids.iter().copied().collect::<Vec<_>>();
    for chunk in heading_ids.chunks(chunk_size) {
        let sql = format!(
            "/* orgfdb:validate-outline-path params={} */
             SELECT heading_id, breadcrumbs_json
             FROM outline_path
             WHERE heading_id IN ({})",
            chunk.len(),
            placeholders(chunk.len())
        );
        let mut statement = connection.prepare(&sql).map_err(|source| {
            QueryShapeError::database("validate_outline_path_rows.prepare", source)
        })?;
        let rows = statement
            .query_map(params_from_iter(chunk.iter()), |row| {
                Ok((row.get::<_, i64>(0)?, row.get::<_, String>(1)?))
            })
            .map_err(|source| {
                QueryShapeError::database("validate_outline_path_rows.query", source)
            })?;

        let mut found = BTreeSet::new();
        for row in rows {
            let (heading_id, breadcrumbs_json) = row.map_err(|source| {
                QueryShapeError::database("validate_outline_path_rows.collect", source)
            })?;
            let _: Vec<String> = serde_json::from_str(&breadcrumbs_json).map_err(|source| {
                QueryShapeError::invalid_json("breadcrumbs_json", heading_id, source)
            })?;
            found.insert(heading_id);
        }

        for heading_id in chunk {
            if !found.contains(heading_id) {
                return Err(QueryShapeError::missing(format!(
                    "missing outline_path row for stored heading id {heading_id}"
                )));
            }
        }
    }

    Ok(())
}
