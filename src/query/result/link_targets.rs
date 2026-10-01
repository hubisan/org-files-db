use super::*;

/// Where a resolved link points, for client actions that open the target.
///
/// The location uses the same conventions as `Location` on result nodes: a
/// file target (the synthetic level-0 root) has no `byte_start`.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LinkTargetLocation {
    pub file_path: String,
    pub line: Option<i64>,
    pub byte_start: Option<i64>,
}

/// Loads the target location of every resolved link among the top-level
/// `results`, keyed by link id. Unresolved links have no entry.
///
/// This reads only the target rows, so it does not need the `target` include
/// and does not change the normal query JSON.
pub fn load_link_target_locations(
    connection: &Connection,
    results: &[QueryResultNode],
) -> Result<HashMap<i64, LinkTargetLocation>, QueryShapeError> {
    let link_ids = results
        .iter()
        .filter_map(|result| match result {
            QueryResultNode::Link(link)
                if link.resolution_status.as_deref() == Some("resolved") =>
            {
                Some(link.id)
            }
            _ => None,
        })
        .collect::<Vec<_>>();
    let mut locations = HashMap::with_capacity(link_ids.len());
    if link_ids.is_empty() {
        return Ok(locations);
    }

    let chunk_size = id_chunk_capacity(connection, 0);
    for chunk in link_ids.chunks(chunk_size) {
        let sql = format!(
            "/* orgfdb:link-target-locations params={} */
             SELECT
                links.id,
                files.path,
                COALESCE(target.level, 0),
                COALESCE(target.line_number, root.line_number),
                target.byte_start
             FROM links
             LEFT JOIN headings AS target ON target.id = links.target_heading_id
             INNER JOIN files ON files.id = COALESCE(target.file_id, links.target_file_id)
             LEFT JOIN headings AS root ON root.file_id = files.id AND root.level = 0
             WHERE links.id IN ({})",
            chunk.len(),
            placeholders(chunk.len())
        );
        let mut statement = connection
            .prepare(&sql)
            .map_err(|source| QueryShapeError::database("link_target_locations.prepare", source))?;
        let rows = statement
            .query_map(params_from_iter(chunk.iter()), |row| {
                let level: i64 = row.get(2)?;
                let byte_start: Option<i64> = row.get(4)?;
                Ok((
                    row.get::<_, i64>(0)?,
                    LinkTargetLocation {
                        file_path: row.get(1)?,
                        line: row.get(3)?,
                        byte_start: if level > 0 { byte_start } else { None },
                    },
                ))
            })
            .map_err(|source| QueryShapeError::database("link_target_locations.query", source))?;
        for row in rows {
            let (link_id, location) = row.map_err(|source| {
                QueryShapeError::database("link_target_locations.collect", source)
            })?;
            locations.insert(link_id, location);
        }
    }
    Ok(locations)
}
