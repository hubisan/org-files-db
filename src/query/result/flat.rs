use super::*;

pub(in crate::query::result) struct FlatMetadataContext {
    pub(in crate::query::result) properties: HashMap<i64, Vec<PropertyFact>>,
    pub(in crate::query::result) effective_properties: HashMap<i64, Vec<EffectivePropertyFact>>,
    pub(in crate::query::result) keywords: HashMap<i64, Vec<KeywordFact>>,
    pub(in crate::query::result) root_tags: HashMap<i64, Vec<String>>,
    pub(in crate::query::result) heading_paths: HashMap<i64, Vec<PathEntry>>,
}

impl FlatMetadataContext {
    pub(in crate::query::result) fn load(
        connection: &Connection,
        rows: &QueryRows,
        includes: &[QueryInclude],
        relation: Option<&MatchedSqlRelation>,
    ) -> Result<Self, QueryShapeError> {
        let include_set = includes.iter().copied().collect::<BTreeSet<_>>();
        let mut metadata_heading_ids = BTreeSet::new();
        let mut root_heading_ids = BTreeSet::new();
        let mut has_heading_rows = false;

        match rows {
            QueryRows::Headings(rows) => {
                for row in rows {
                    match row {
                        HeadingQueryMatch::File(row) => {
                            metadata_heading_ids.insert(row.root_heading_id);
                            root_heading_ids.insert(row.root_heading_id);
                        }
                        HeadingQueryMatch::Heading(row) => {
                            metadata_heading_ids.insert(row.id);
                            has_heading_rows = true;
                        }
                    }
                }
            }
            QueryRows::Files(rows) => {
                for row in rows {
                    metadata_heading_ids.insert(row.root_heading_id);
                    root_heading_ids.insert(row.root_heading_id);
                }
            }
            QueryRows::Links(_) => {}
        }
        let has_root_rows = !root_heading_ids.is_empty();

        if let Some(relation) = relation {
            if include_set.contains(&QueryInclude::Path) && matches!(rows, QueryRows::Headings(_)) {
                if has_root_rows {
                    validate_outline_path_root_relation(connection, relation)?;
                }
            } else {
                validate_outline_path_relation(
                    connection,
                    relation,
                    has_heading_rows,
                    has_root_rows,
                )?;
            }
        } else {
            validate_outline_path_rows(connection, &metadata_heading_ids)?;
        }

        let mut properties = HashMap::new();
        if include_set.contains(&QueryInclude::Properties) {
            let loaded = if let Some(relation) = relation {
                load_properties_from_relation(
                    connection,
                    relation,
                    has_heading_rows,
                    has_root_rows,
                )?
            } else {
                load_properties(connection, &metadata_heading_ids)?
            };
            for property in loaded {
                properties
                    .entry(property.heading_id)
                    .or_insert_with(Vec::new)
                    .push(property.fact);
            }
        }

        let effective_properties = if include_set.contains(&QueryInclude::EffectiveProperties) {
            if let Some(relation) = relation {
                load_effective_properties_from_relation(
                    connection,
                    relation,
                    &metadata_heading_ids,
                    has_heading_rows,
                    has_root_rows,
                )?
            } else {
                load_effective_properties(connection, &metadata_heading_ids)?
            }
        } else {
            HashMap::new()
        };

        let mut keywords = HashMap::new();
        if include_set.contains(&QueryInclude::Keywords) {
            let loaded = if let Some(relation) = relation {
                load_keywords_from_relation(connection, relation, has_heading_rows, has_root_rows)?
            } else {
                load_keywords(connection, &metadata_heading_ids)?
            };
            for keyword in loaded {
                keywords
                    .entry(keyword.heading_id)
                    .or_insert_with(Vec::new)
                    .push(keyword.fact);
            }
        }

        let root_tags = if let Some(relation) = relation {
            if has_root_rows {
                load_root_tags_from_relation(connection, relation)?
            } else {
                HashMap::new()
            }
        } else {
            load_effective_tags_for_heading_ids(connection, &root_heading_ids)?
        };

        let heading_paths = if include_set.contains(&QueryInclude::Path) {
            match (rows, relation) {
                (QueryRows::Headings(rows), Some(relation)) => {
                    load_heading_paths_from_relation(connection, relation, rows)?
                }
                (QueryRows::Headings(_), None) => HashMap::new(),
                (QueryRows::Files(_), _) | (QueryRows::Links(_), _) => HashMap::new(),
            }
        } else {
            HashMap::new()
        };

        Ok(Self {
            properties,
            effective_properties,
            keywords,
            root_tags,
            heading_paths,
        })
    }

    pub(in crate::query::result) fn shape_file_row(
        &mut self,
        row: FileQueryRow,
        domain: ResultDomain,
        includes: &[QueryInclude],
    ) -> Result<FileResultNode, QueryShapeError> {
        let path_ref = Path::new(&row.path);
        let name = path_ref
            .file_name()
            .and_then(|value| value.to_str())
            .unwrap_or(row.path.as_str())
            .to_string();
        let dir = path_ref
            .parent()
            .and_then(|value| value.to_str())
            .unwrap_or(".")
            .to_string();

        let tags = self
            .root_tags
            .remove(&row.root_heading_id)
            .unwrap_or_default();
        let node_path = includes.contains(&QueryInclude::Path).then(|| {
            vec![PathEntry::File(FilePathEntry {
                id: row.id,
                path: row.path.clone(),
                title: row.root_title.clone(),
                title_raw: row.root_title_raw.clone(),
            })]
        });
        let properties = includes.contains(&QueryInclude::Properties).then(|| {
            self.properties
                .remove(&row.root_heading_id)
                .unwrap_or_default()
        });
        let effective_properties =
            includes
                .contains(&QueryInclude::EffectiveProperties)
                .then(|| {
                    self.effective_properties
                        .remove(&row.root_heading_id)
                        .unwrap_or_default()
                });
        let keywords = includes.contains(&QueryInclude::Keywords).then(|| {
            self.keywords
                .remove(&row.root_heading_id)
                .unwrap_or_default()
        });

        let node = FileResultNode {
            kind: public_result_kind(domain, 0),
            matched: true,
            id: row.id,
            level: 0,
            path: row.path.clone(),
            name,
            dir,
            title: row.root_title,
            title_raw: row.root_title_raw,
            root_heading_id: row.root_heading_id,
            mtime_ns: row.mtime_ns,
            size: row.size,
            content_hash: row.content_hash,
            indexed_at: row.indexed_at,
            location: Location {
                file_path: row.path,
                line: row.root_line_number,
                byte_start: None,
                byte_end: None,
            },
            tags,
            node_path,
            properties,
            effective_properties,
            keywords,
            links: None,
            backlinks: None,
            children: None,
        };
        Ok(node)
    }

    pub(in crate::query::result) fn shape_heading_row(
        &mut self,
        row: HeadingQueryRow,
        includes: &[QueryInclude],
    ) -> Result<HeadingResultNode, QueryShapeError> {
        let all_tags = serde_json::from_str(&row.all_tags_json)
            .map_err(|source| QueryShapeError::invalid_json("all_tags_json", row.id, source))?;

        let node_path = includes
            .contains(&QueryInclude::Path)
            .then(|| self.heading_paths.remove(&row.id).unwrap_or_default());
        let properties = includes
            .contains(&QueryInclude::Properties)
            .then(|| self.properties.remove(&row.id).unwrap_or_default());
        let effective_properties =
            includes
                .contains(&QueryInclude::EffectiveProperties)
                .then(|| {
                    self.effective_properties
                        .remove(&row.id)
                        .unwrap_or_default()
                });
        let keywords = includes
            .contains(&QueryInclude::Keywords)
            .then(|| self.keywords.remove(&row.id).unwrap_or_default());

        let node = HeadingResultNode {
            kind: public_result_kind(ResultDomain::Headings, row.level),
            matched: true,
            id: row.id,
            file_id: row.file_id,
            parent_id: row.parent_id,
            level: row.level,
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
            all_tags,
            location: Location {
                file_path: row.file_path,
                line: row.line_number,
                byte_start: Some(row.byte_start),
                byte_end: Some(row.byte_end),
            },
            node_path,
            properties,
            effective_properties,
            keywords,
            links: None,
            backlinks: None,
            children: None,
        };
        Ok(node)
    }

    pub(in crate::query::result) fn shape_link_row(
        &mut self,
        row: &LinkQueryRow,
    ) -> LinkResultNode {
        LinkResultNode {
            kind: QueryResultKind::Link,
            matched: true,
            id: row.id,
            file_id: row.file_id,
            heading_id: row.heading_id,
            heading_level: row.heading_level,
            source_context: row.source_context.clone(),
            format: row.format.clone(),
            link_type: row.link_type.clone(),
            raw: row.raw.clone(),
            raw_target: row.raw_target.clone(),
            raw_description: row.raw_description.clone(),
            link_path: row.path.clone(),
            search_option: row.search_option.clone(),
            path_absolute: row.path_absolute.clone(),
            target_file_id: row.target_file_id,
            target_heading_id: row.target_heading_id,
            target_custom_id: row.target_custom_id.clone(),
            target_id: row.target_id.clone(),
            resolution_status: row.resolution_status.clone(),
            resolution_diagnostic: row.resolution_diagnostic.clone(),
            location: Location {
                file_path: row.file_path.clone(),
                line: Some(row.line),
                byte_start: Some(row.byte_start),
                byte_end: Some(row.byte_end),
            },
            node_path: None,
            source: None,
            target: None,
        }
    }
}
