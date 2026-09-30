use super::*;

pub(in crate::query::result) struct EnrichmentContext {
    pub(in crate::query::result) files: HashMap<i64, StoredFile>,
    pub(in crate::query::result) headings: HashMap<i64, StoredHeading>,
    pub(in crate::query::result) properties: HashMap<i64, Vec<PropertyFact>>,
    pub(in crate::query::result) effective_properties: HashMap<i64, Vec<EffectivePropertyFact>>,
    pub(in crate::query::result) keywords: HashMap<i64, Vec<KeywordFact>>,
    pub(in crate::query::result) links_by_file: HashMap<i64, Vec<StoredLink>>,
    pub(in crate::query::result) links_by_heading: HashMap<i64, Vec<StoredLink>>,
    pub(in crate::query::result) backlinks_by_file: HashMap<i64, Vec<StoredLink>>,
    pub(in crate::query::result) backlinks_by_heading: HashMap<i64, Vec<StoredLink>>,
}

impl EnrichmentContext {
    pub(in crate::query::result) fn load(
        connection: &Connection,
        rows: &QueryRows,
        includes: &[QueryInclude],
    ) -> Result<Self, QueryShapeError> {
        let include_set = includes.iter().copied().collect::<BTreeSet<_>>();
        let (matched_file_ids, matched_heading_ids, matched_link_rows) = collect_matched_ids(rows);

        let mut relevant_file_ids = matched_file_ids.clone();

        for link in &matched_link_rows {
            relevant_file_ids.insert(link.file_id);
            if let Some(file_id) = link.target_file_id {
                relevant_file_ids.insert(file_id);
            }
        }

        let mut links_by_file = if include_set.contains(&QueryInclude::Links) {
            load_links_by_file(connection, &matched_file_ids)?
        } else {
            HashMap::new()
        };
        let mut backlinks_by_file = if include_set.contains(&QueryInclude::Backlinks) {
            load_backlinks_by_file(connection, &matched_file_ids)?
        } else {
            HashMap::new()
        };
        let mut links_by_heading = if include_set.contains(&QueryInclude::Links) {
            load_links_by_heading(connection, &matched_heading_ids)?
        } else {
            HashMap::new()
        };
        let mut backlinks_by_heading = if include_set.contains(&QueryInclude::Backlinks) {
            load_backlinks_by_heading(connection, &matched_heading_ids)?
        } else {
            HashMap::new()
        };

        for links in links_by_file.values() {
            for link in links {
                relevant_file_ids.insert(link.file_id);
                if let Some(file_id) = link.target_file_id {
                    relevant_file_ids.insert(file_id);
                }
            }
        }
        for links in backlinks_by_file.values() {
            for link in links {
                relevant_file_ids.insert(link.file_id);
                if let Some(file_id) = link.target_file_id {
                    relevant_file_ids.insert(file_id);
                }
            }
        }
        for links in links_by_heading.values() {
            for link in links {
                relevant_file_ids.insert(link.file_id);
                if let Some(file_id) = link.target_file_id {
                    relevant_file_ids.insert(file_id);
                }
            }
        }
        for links in backlinks_by_heading.values() {
            for link in links {
                relevant_file_ids.insert(link.file_id);
                if let Some(file_id) = link.target_file_id {
                    relevant_file_ids.insert(file_id);
                }
            }
        }

        let files = load_files(connection, &relevant_file_ids)?;
        sort_link_map_groups(&mut links_by_file, &files)?;
        sort_link_map_groups(&mut backlinks_by_file, &files)?;
        sort_link_map_groups(&mut links_by_heading, &files)?;
        sort_link_map_groups(&mut backlinks_by_heading, &files)?;
        let headings = load_headings_for_files(connection, &relevant_file_ids)?;
        let metadata_heading_ids =
            matched_metadata_heading_ids(&matched_heading_ids, &matched_file_ids, &files);
        let direct_property_heading_ids = metadata_heading_ids.clone();

        let mut properties = HashMap::new();
        let loaded_properties = if include_set.contains(&QueryInclude::Properties) {
            Some(load_properties(connection, &direct_property_heading_ids)?)
        } else {
            None
        };
        if let Some(loaded_properties) = loaded_properties.as_ref() {
            for property in loaded_properties {
                properties
                    .entry(property.heading_id)
                    .or_insert_with(Vec::new)
                    .push(property.fact.clone());
            }
        }

        let effective_properties = if include_set.contains(&QueryInclude::EffectiveProperties) {
            load_effective_properties(connection, &metadata_heading_ids)?
        } else {
            HashMap::new()
        };

        let mut keywords = HashMap::new();
        if include_set.contains(&QueryInclude::Keywords) {
            for keyword in load_keywords(connection, &metadata_heading_ids)? {
                keywords
                    .entry(keyword.heading_id)
                    .or_insert_with(Vec::new)
                    .push(keyword.fact);
            }
        }

        Ok(Self {
            files,
            headings,
            properties,
            effective_properties,
            keywords,
            links_by_file,
            links_by_heading,
            backlinks_by_file,
            backlinks_by_heading,
        })
    }

    pub(in crate::query::result) fn shape_file_node(
        &self,
        file_id: i64,
        domain: ResultDomain,
        matched: bool,
        includes: &[QueryInclude],
        with_children: bool,
    ) -> Result<FileResultNode, QueryShapeError> {
        let file = self.file(file_id)?;
        let root_heading = self.heading(file.root_heading_id)?;
        Ok(FileResultNode {
            kind: public_result_kind(domain, root_heading.level),
            matched,
            id: file.id,
            level: 0,
            path: file.path.clone(),
            name: file.name.clone(),
            dir: file.dir.clone(),
            title: file.root_title.clone(),
            title_raw: file.root_title_raw.clone(),
            root_heading_id: file.root_heading_id,
            mtime_ns: file.mtime_ns,
            size: file.size,
            content_hash: file.content_hash.clone(),
            indexed_at: file.indexed_at,
            location: Location {
                file_path: file.path.clone(),
                line: file.root_line_number,
                byte_start: None,
                byte_end: None,
            },
            tags: root_heading.all_tags.clone(),
            node_path: if includes.contains(&QueryInclude::Path) {
                Some(vec![self.file_path_entry(file.id)?])
            } else {
                None
            },
            properties: includes.contains(&QueryInclude::Properties).then(|| {
                self.properties
                    .get(&file.root_heading_id)
                    .cloned()
                    .unwrap_or_default()
            }),
            effective_properties: includes.contains(&QueryInclude::EffectiveProperties).then(
                || {
                    self.effective_properties
                        .get(&file.root_heading_id)
                        .cloned()
                        .unwrap_or_default()
                },
            ),
            keywords: includes.contains(&QueryInclude::Keywords).then(|| {
                self.keywords
                    .get(&file.root_heading_id)
                    .cloned()
                    .unwrap_or_default()
            }),
            links: includes
                .contains(&QueryInclude::Links)
                .then(|| self.build_included_links(self.links_by_file.get(&file.id)))
                .transpose()?,
            backlinks: includes
                .contains(&QueryInclude::Backlinks)
                .then(|| self.build_included_links(self.backlinks_by_file.get(&file.id)))
                .transpose()?,
            children: with_children.then(Vec::new),
        })
    }

    pub(in crate::query::result) fn shape_heading_node(
        &self,
        heading_id: i64,
        matched: bool,
        includes: &[QueryInclude],
        with_children: bool,
    ) -> Result<HeadingResultNode, QueryShapeError> {
        let heading = self.heading(heading_id)?;
        let file = self.file(heading.file_id)?;
        Ok(HeadingResultNode {
            kind: public_result_kind(ResultDomain::Headings, heading.level),
            matched,
            id: heading.id,
            file_id: heading.file_id,
            parent_id: heading.parent_id,
            level: heading.level,
            title: heading.title.clone(),
            title_raw: heading.title_raw.clone(),
            todo_keyword: heading.todo_keyword.clone(),
            todo_type: heading.todo_type.clone(),
            priority: heading.priority.clone(),
            scheduled_raw: heading.scheduled_raw.clone(),
            scheduled_ts: heading.scheduled_ts,
            deadline_raw: heading.deadline_raw.clone(),
            deadline_ts: heading.deadline_ts,
            closed_raw: heading.closed_raw.clone(),
            closed_ts: heading.closed_ts,
            archivedp: heading.archivedp,
            footnote_section_p: heading.footnote_section_p,
            all_tags: heading.all_tags.clone(),
            location: Location {
                file_path: file.path.clone(),
                line: heading.line_number,
                byte_start: Some(heading.byte_start),
                byte_end: Some(heading.byte_end),
            },
            node_path: includes
                .contains(&QueryInclude::Path)
                .then(|| self.path_entries_for_heading(heading.id))
                .transpose()?,
            properties: includes.contains(&QueryInclude::Properties).then(|| {
                self.properties
                    .get(&heading.id)
                    .cloned()
                    .unwrap_or_default()
            }),
            effective_properties: includes.contains(&QueryInclude::EffectiveProperties).then(
                || {
                    self.effective_properties
                        .get(&heading.id)
                        .cloned()
                        .unwrap_or_default()
                },
            ),
            keywords: includes
                .contains(&QueryInclude::Keywords)
                .then(|| self.keywords.get(&heading.id).cloned().unwrap_or_default()),
            links: includes
                .contains(&QueryInclude::Links)
                .then(|| self.build_included_links(self.links_by_heading.get(&heading.id)))
                .transpose()?,
            backlinks: includes
                .contains(&QueryInclude::Backlinks)
                .then(|| self.build_included_links(self.backlinks_by_heading.get(&heading.id)))
                .transpose()?,
            children: with_children.then(Vec::new),
        })
    }

    pub(in crate::query::result) fn shape_link_node(
        &self,
        row: &LinkQueryRow,
        includes: &[QueryInclude],
    ) -> Result<LinkResultNode, QueryShapeError> {
        let file = self.file(row.file_id)?;
        Ok(LinkResultNode {
            kind: public_result_kind(ResultDomain::Links, row.heading_level),
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
                file_path: file.path.clone(),
                line: Some(row.line),
                byte_start: Some(row.byte_start),
                byte_end: Some(row.byte_end),
            },
            node_path: includes
                .contains(&QueryInclude::Path)
                .then(|| self.path_entries_for_link_source(row.heading_id))
                .transpose()?,
            source: includes
                .contains(&QueryInclude::Source)
                .then(|| self.link_source(row.file_id, row.heading_id))
                .transpose()?,
            target: includes
                .contains(&QueryInclude::Target)
                .then(|| self.link_target_from_row(row))
                .transpose()?,
        })
    }

    pub(in crate::query::result) fn shape_heading_outline(
        &self,
        rows: &[HeadingQueryMatch],
        includes: &[QueryInclude],
    ) -> Result<Vec<QueryResultNode>, QueryShapeError> {
        let mut roots = BTreeMap::<String, FileResultNode>::new();
        for row in rows {
            let file = match row {
                HeadingQueryMatch::File(row) => self.file(row.id)?,
                HeadingQueryMatch::Heading(row) => self.file(row.file_id)?,
            };
            if let std::collections::btree_map::Entry::Vacant(entry) =
                roots.entry(file.path.clone())
            {
                entry.insert(self.shape_file_node(
                    file.id,
                    ResultDomain::Headings,
                    false,
                    &[],
                    true,
                )?);
            }
        }

        for row in rows {
            let file_path = match row {
                HeadingQueryMatch::File(row) => &row.path,
                HeadingQueryMatch::Heading(row) => &row.file_path,
            };
            let file_node = roots.get_mut(file_path).ok_or_else(|| {
                QueryShapeError::missing(format!(
                    "missing outline file root for stored path {file_path}"
                ))
            })?;
            match row {
                HeadingQueryMatch::File(file_row) => {
                    file_node.matched = true;
                    if includes.contains(&QueryInclude::Path) {
                        file_node.node_path = Some(vec![self.file_path_entry(file_row.id)?]);
                    }
                }
                HeadingQueryMatch::Heading(row) => {
                    let heading_path = self.heading_chain_without_root(row.id)?;
                    insert_heading_outline(file_node, &heading_path, row.id, self, includes)?;
                }
            }
        }

        Ok(roots.into_values().map(QueryResultNode::File).collect())
    }

    pub(in crate::query::result) fn shape_link_outline(
        &self,
        rows: &[LinkQueryRow],
        includes: &[QueryInclude],
    ) -> Result<Vec<QueryResultNode>, QueryShapeError> {
        let mut roots = BTreeMap::<String, FileResultNode>::new();
        for row in rows {
            let file = self.file(row.file_id)?;
            if let std::collections::btree_map::Entry::Vacant(entry) =
                roots.entry(file.path.clone())
            {
                entry.insert(self.shape_file_node(
                    file.id,
                    ResultDomain::Headings,
                    false,
                    &[],
                    true,
                )?);
            }
        }

        for row in rows {
            let file = self.file(row.file_id)?;
            let file_node = roots.get_mut(&file.path).ok_or_else(|| {
                QueryShapeError::missing(format!(
                    "missing outline file root for stored path {}",
                    file.path
                ))
            })?;
            if row.heading_level == 0 {
                let file_node_id = file_node.id;
                let children = file_node.children.as_mut().ok_or_else(|| {
                    QueryShapeError::missing(format!(
                        "missing outline children for file node {file_node_id}"
                    ))
                })?;
                children.push(QueryResultNode::Link(Box::new(
                    self.shape_link_node(row, includes)?,
                )));
            } else {
                let heading_path = self.heading_chain_without_root(row.heading_id)?;
                let parent = ensure_heading_outline_path(file_node, &heading_path, self, &[])?;
                let parent_id = parent.id;
                let children = parent.children.as_mut().ok_or_else(|| {
                    QueryShapeError::missing(format!(
                        "missing outline children for heading node {parent_id}"
                    ))
                })?;
                children.push(QueryResultNode::Link(Box::new(
                    self.shape_link_node(row, includes)?,
                )));
            }
        }

        let mut results = roots.into_values().collect::<Vec<_>>();
        for file in &mut results {
            sort_outline_children(file.children.as_mut());
        }
        Ok(results.into_iter().map(QueryResultNode::File).collect())
    }

    pub(in crate::query::result) fn build_included_links(
        &self,
        links: Option<&Vec<StoredLink>>,
    ) -> Result<Vec<IncludedLink>, QueryShapeError> {
        links
            .cloned()
            .unwrap_or_default()
            .into_iter()
            .map(|link| self.included_link(&link))
            .collect()
    }

    pub(in crate::query::result) fn included_link(
        &self,
        link: &StoredLink,
    ) -> Result<IncludedLink, QueryShapeError> {
        let source_path = self.path_entries_for_link_source(link.heading_id)?;
        Ok(IncludedLink {
            id: link.id,
            source_context: link.source_context.clone(),
            format: link.format.clone(),
            link_type: link.link_type.clone(),
            raw: link.raw.clone(),
            raw_target: link.raw_target.clone(),
            raw_description: link.raw_description.clone(),
            link_path: link.link_path.clone(),
            search_option: link.search_option.clone(),
            path_absolute: link.path_absolute.clone(),
            target_file_id: link.target_file_id,
            target_heading_id: link.target_heading_id,
            target_custom_id: link.target_custom_id.clone(),
            target_id: link.target_id.clone(),
            resolution_status: link.resolution_status.clone(),
            resolution_diagnostic: link.resolution_diagnostic.clone(),
            location: Location {
                file_path: self.file(link.file_id)?.path.clone(),
                line: Some(link.line),
                byte_start: Some(link.byte_start),
                byte_end: Some(link.byte_end),
            },
            source_path: source_path.clone(),
            source: self.link_source(link.file_id, link.heading_id)?,
            target: self.link_target_from_stored(link)?,
        })
    }

    pub(in crate::query::result) fn link_source(
        &self,
        file_id: i64,
        heading_id: i64,
    ) -> Result<LinkSource, QueryShapeError> {
        let file = self.file(file_id)?;
        let heading = self.heading(heading_id)?;
        let source_path = self.path_entries_for_link_source(heading_id)?;
        Ok(LinkSource {
            file: FileRef {
                id: file.id,
                path: file.path.clone(),
                title: file.root_title.clone(),
                title_raw: file.root_title_raw.clone(),
            },
            heading: if heading.level == 0 {
                None
            } else {
                Some(self.heading_ref(heading_id)?)
            },
            source_path,
        })
    }

    pub(in crate::query::result) fn link_target_from_row(
        &self,
        row: &LinkQueryRow,
    ) -> Result<LinkTarget, QueryShapeError> {
        self.link_target_fields(
            &row.raw_target,
            row.target_file_id,
            row.target_heading_id,
            row.resolution_status.as_deref(),
            row.resolution_diagnostic.clone(),
        )
    }

    pub(in crate::query::result) fn link_target_from_stored(
        &self,
        link: &StoredLink,
    ) -> Result<LinkTarget, QueryShapeError> {
        self.link_target_fields(
            &link.raw_target,
            link.target_file_id,
            link.target_heading_id,
            link.resolution_status.as_deref(),
            link.resolution_diagnostic.clone(),
        )
    }

    pub(in crate::query::result) fn link_target_fields(
        &self,
        raw_target: &str,
        target_file_id: Option<i64>,
        target_heading_id: Option<i64>,
        resolution_status: Option<&str>,
        resolution_diagnostic: Option<String>,
    ) -> Result<LinkTarget, QueryShapeError> {
        let resolved = resolution_status == Some("resolved");
        let file = if resolved {
            target_file_id
                .map(|file_id| self.file_ref(file_id))
                .transpose()?
        } else {
            None
        };
        let target_heading = if resolved {
            target_heading_id
                .map(|heading_id| self.heading(heading_id))
                .transpose()?
        } else {
            None
        };
        // Plain file links resolve to the synthetic level-0 heading internally.
        // Expose only real Org headings as heading targets.
        let heading = match target_heading {
            Some(heading) if heading.level > 0 => Some(self.heading_ref(heading.id)?),
            _ => None,
        };
        let resolved_kind = if resolved {
            match target_heading {
                Some(heading) if heading.level > 0 => Some(QueryTarget::Headings),
                Some(_) => Some(QueryTarget::Files),
                None if target_file_id.is_some() => Some(QueryTarget::Files),
                None => None,
            }
        } else {
            None
        };

        Ok(LinkTarget {
            resolved_kind,
            file,
            heading,
            raw_target: raw_target.to_string(),
            resolution_status: resolution_status.map(str::to_string),
            resolution_diagnostic,
        })
    }

    pub(in crate::query::result) fn file_ref(
        &self,
        file_id: i64,
    ) -> Result<FileRef, QueryShapeError> {
        let file = self.file(file_id)?;
        Ok(FileRef {
            id: file.id,
            path: file.path.clone(),
            title: file.root_title.clone(),
            title_raw: file.root_title_raw.clone(),
        })
    }

    pub(in crate::query::result) fn heading_ref(
        &self,
        heading_id: i64,
    ) -> Result<HeadingRef, QueryShapeError> {
        let heading = self.heading(heading_id)?;
        let title_raw = heading.title_raw.clone().ok_or_else(|| {
            QueryShapeError::missing(format!(
                "missing title_raw for stored heading row {}",
                heading.id
            ))
        })?;
        let outline_path = self
            .heading_chain_without_root(heading_id)?
            .into_iter()
            .map(|id| self.heading(id).map(|entry| entry.title.clone()))
            .collect::<Result<Vec<_>, _>>()?;
        Ok(HeadingRef {
            id: heading.id,
            title: heading.title.clone(),
            title_raw,
            level: heading.level,
            outline_path,
        })
    }

    pub(in crate::query::result) fn path_entries_for_heading(
        &self,
        heading_id: i64,
    ) -> Result<Vec<PathEntry>, QueryShapeError> {
        let heading = self.heading(heading_id)?;
        let file = self.file(heading.file_id)?;
        let mut path = vec![PathEntry::File(FilePathEntry {
            id: file.id,
            path: file.path.clone(),
            title: file.root_title.clone(),
            title_raw: file.root_title_raw.clone(),
        })];

        for id in self.heading_chain_without_root(heading_id)? {
            let entry = self.heading(id)?;
            let title_raw = entry.title_raw.clone().ok_or_else(|| {
                QueryShapeError::missing(format!(
                    "missing title_raw for stored heading row {}",
                    entry.id
                ))
            })?;
            path.push(PathEntry::Heading(HeadingPathEntry {
                id: entry.id,
                title: entry.title.clone(),
                title_raw,
                level: entry.level,
            }));
        }

        Ok(path)
    }

    pub(in crate::query::result) fn path_entries_for_link_source(
        &self,
        heading_id: i64,
    ) -> Result<Vec<PathEntry>, QueryShapeError> {
        let heading = self.heading(heading_id)?;
        if heading.level == 0 {
            let file = self.file(heading.file_id)?;
            return Ok(vec![PathEntry::File(FilePathEntry {
                id: file.id,
                path: file.path.clone(),
                title: file.root_title.clone(),
                title_raw: file.root_title_raw.clone(),
            })]);
        }
        self.path_entries_for_heading(heading_id)
    }

    pub(in crate::query::result) fn heading_chain_without_root(
        &self,
        heading_id: i64,
    ) -> Result<Vec<i64>, QueryShapeError> {
        let mut chain = Vec::new();
        let mut current = Some(heading_id);
        while let Some(id) = current {
            let heading = self.heading(id)?;
            if heading.level == 0 {
                break;
            }
            chain.push(heading.id);
            current = heading.parent_id;
        }
        chain.reverse();
        Ok(chain)
    }

    pub(in crate::query::result) fn file(
        &self,
        file_id: i64,
    ) -> Result<&StoredFile, QueryShapeError> {
        self.files.get(&file_id).ok_or_else(|| {
            QueryShapeError::missing(format!("missing stored file row for id {file_id}"))
        })
    }

    pub(in crate::query::result) fn heading(
        &self,
        heading_id: i64,
    ) -> Result<&StoredHeading, QueryShapeError> {
        self.headings.get(&heading_id).ok_or_else(|| {
            QueryShapeError::missing(format!("missing stored heading row for id {heading_id}"))
        })
    }

    pub(in crate::query::result) fn file_path_entry(
        &self,
        file_id: i64,
    ) -> Result<PathEntry, QueryShapeError> {
        let file = self.file(file_id)?;
        Ok(PathEntry::File(FilePathEntry {
            id: file.id,
            path: file.path.clone(),
            title: file.root_title.clone(),
            title_raw: file.root_title_raw.clone(),
        }))
    }
}
