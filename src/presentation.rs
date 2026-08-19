use std::{cmp::Ordering, error::Error, fmt, path::Path};

use serde::{
    ser::{SerializeMap, SerializeSeq},
    Deserialize, Serialize, Serializer,
};

use crate::query::{PathEntry, QueryInclude, QueryResultNode, QueryTarget};

const DEFAULT_TRUNCATION_MARKER: &str = "…";
const DEFAULT_OUTLINE_SEPARATOR: &str = " » ";

const NO_INCLUDES: &[QueryInclude] = &[];
const PATH_INCLUDE: &[QueryInclude] = &[QueryInclude::Path];
const TARGET_INCLUDE: &[QueryInclude] = &[QueryInclude::Target];
const EFFECTIVE_PROPERTIES_INCLUDE: &[QueryInclude] = &[QueryInclude::EffectiveProperties];
const KEYWORDS_INCLUDE: &[QueryInclude] = &[QueryInclude::Keywords];

pub const PRESENTATION_VERSION: u32 = 2;

const PRESENTATION_ROLE_VALUES: &[&str] = &[
    "heading",
    "title",
    "todo",
    "done",
    "priority",
    "tag",
    "date",
    "file-name",
    "file-path",
    "keyword-name",
    "keyword-value",
    "property-name",
    "property-value",
];

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PresentationResponse {
    pub presentation_version: u32,
    pub database_id: String,
    pub generation: i64,
    pub results: Vec<QueryResultNode>,
    pub rows: Vec<PresentationRow>,
}

impl Serialize for PresentationResponse {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let mut map = serializer.serialize_map(Some(6))?;
        map.serialize_entry("presentation_version", &self.presentation_version)?;
        map.serialize_entry("database_id", &self.database_id)?;
        map.serialize_entry("generation", &self.generation)?;
        map.serialize_entry("results", &self.results)?;
        map.serialize_entry("schemas", &PresentationWireSchemas)?;
        map.serialize_entry("rows", &PresentationWireRows(&self.rows))?;
        map.end()
    }
}

struct PresentationWireSchemas;

impl Serialize for PresentationWireSchemas {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let mut map = serializer.serialize_map(Some(6))?;
        map.serialize_entry("row_fields", &["result_index", "row_context", "cells"])?;
        map.serialize_entry("cell_fields", &["search_text", "display_text", "role"])?;
        map.serialize_entry("row_context_shapes", &PresentationWireRowContextShapes)?;
        map.serialize_entry("display_text_null", "same-as-search_text")?;
        map.serialize_entry("role_encoding", "null-or-index-into-role_values")?;
        map.serialize_entry("role_values", PRESENTATION_ROLE_VALUES)?;
        map.end()
    }
}

struct PresentationWireRowContextShapes;

impl Serialize for PresentationWireRowContextShapes {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let mut map = serializer.serialize_map(Some(3))?;
        map.serialize_entry("tag", &["kind", "value"])?;
        map.serialize_entry("effective-property", &["kind", "name", "value"])?;
        map.serialize_entry("keyword", &["kind", "name", "value"])?;
        map.end()
    }
}

struct PresentationWireRows<'a>(&'a [PresentationRow]);

impl Serialize for PresentationWireRows<'_> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let mut seq = serializer.serialize_seq(Some(self.0.len()))?;
        for row in self.0 {
            seq.serialize_element(&PresentationWireRow(row))?;
        }
        seq.end()
    }
}

struct PresentationWireRow<'a>(&'a PresentationRow);

impl Serialize for PresentationWireRow<'_> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let row = self.0;
        let mut seq = serializer.serialize_seq(Some(3))?;
        seq.serialize_element(&row.result_index)?;
        seq.serialize_element(&PresentationWireRowContext(row.row_context.as_ref()))?;
        seq.serialize_element(&PresentationWireCells(&row.cells))?;
        seq.end()
    }
}

struct PresentationWireRowContext<'a>(Option<&'a PresentationRowContext>);

impl Serialize for PresentationWireRowContext<'_> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        match self.0 {
            None => serializer.serialize_none(),
            Some(PresentationRowContext::Tag { value }) => {
                let mut seq = serializer.serialize_seq(Some(2))?;
                seq.serialize_element("tag")?;
                seq.serialize_element(value)?;
                seq.end()
            }
            Some(PresentationRowContext::EffectiveProperty { name, value }) => {
                let mut seq = serializer.serialize_seq(Some(3))?;
                seq.serialize_element("effective-property")?;
                seq.serialize_element(name)?;
                seq.serialize_element(value)?;
                seq.end()
            }
            Some(PresentationRowContext::Keyword { name, value }) => {
                let mut seq = serializer.serialize_seq(Some(3))?;
                seq.serialize_element("keyword")?;
                seq.serialize_element(name)?;
                seq.serialize_element(value)?;
                seq.end()
            }
        }
    }
}

struct PresentationWireCells<'a>(&'a [PresentationCell]);

impl Serialize for PresentationWireCells<'_> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let mut seq = serializer.serialize_seq(Some(self.0.len()))?;
        for cell in self.0 {
            seq.serialize_element(&PresentationWireCell(cell))?;
        }
        seq.end()
    }
}

struct PresentationWireCell<'a>(&'a PresentationCell);

impl Serialize for PresentationWireCell<'_> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let cell = self.0;
        let mut seq = serializer.serialize_seq(Some(3))?;
        seq.serialize_element(&cell.search_text)?;
        if cell.search_text == cell.display_text {
            seq.serialize_element(&Option::<&str>::None)?;
        } else {
            seq.serialize_element(&cell.display_text)?;
        }
        seq.serialize_element(&cell.role.map(presentation_role_index))?;
        seq.end()
    }
}

const fn presentation_role_index(role: PresentationRole) -> u8 {
    match role {
        PresentationRole::Heading => 0,
        PresentationRole::Title => 1,
        PresentationRole::Todo => 2,
        PresentationRole::Done => 3,
        PresentationRole::Priority => 4,
        PresentationRole::Tag => 5,
        PresentationRole::Date => 6,
        PresentationRole::FileName => 7,
        PresentationRole::FilePath => 8,
        PresentationRole::KeywordName => 9,
        PresentationRole::KeywordValue => 10,
        PresentationRole::PropertyName => 11,
        PresentationRole::PropertyValue => 12,
    }
}

impl PresentationResponse {
    pub fn new(
        database_id: impl Into<String>,
        generation: i64,
        results: Vec<QueryResultNode>,
        rows: Vec<PresentationRow>,
    ) -> Self {
        Self {
            presentation_version: PRESENTATION_VERSION,
            database_id: database_id.into(),
            generation,
            results,
            rows,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct PresentationRow {
    pub result_index: usize,
    pub row_context: Option<PresentationRowContext>,
    pub cells: Vec<PresentationCell>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
#[serde(tag = "kind", rename_all = "kebab-case")]
pub enum PresentationRowContext {
    Tag { value: String },
    EffectiveProperty { name: String, value: String },
    Keyword { name: String, value: String },
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct PresentationCell {
    pub search_text: String,
    pub display_text: String,
    pub role: Option<PresentationRole>,
}

const HEADING_RESULTS: &[PresentationResultKind] = &[PresentationResultKind::Heading];
const LINK_RESULTS: &[PresentationResultKind] = &[PresentationResultKind::Link];
const SEARCH_RESULTS: &[PresentationResultKind] = &[PresentationResultKind::Search];
const HEADING_AND_FILE_RESULTS: &[PresentationResultKind] = &[
    PresentationResultKind::Heading,
    PresentationResultKind::File,
];
const HEADING_AND_SEARCH_RESULTS: &[PresentationResultKind] = &[
    PresentationResultKind::Heading,
    PresentationResultKind::Search,
];
const FILE_LOCATION_RESULTS: &[PresentationResultKind] = &[
    PresentationResultKind::Heading,
    PresentationResultKind::File,
    PresentationResultKind::Link,
    PresentationResultKind::Search,
];

#[derive(Debug, Clone, PartialEq, Eq, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct PresentationSpec {
    pub columns: Vec<PresentationColumnSpec>,
    #[serde(default)]
    pub sort: Vec<PresentationSortSpec>,
    #[serde(default)]
    pub row_source: Option<PresentationRowSourceSpec>,
}

impl PresentationSpec {
    pub fn parse_json(input: &str) -> Result<Self, PresentationSpecError> {
        let spec: Self = serde_json::from_str(input).map_err(PresentationSpecError::Json)?;
        spec.validate()?;
        Ok(spec)
    }

    fn validate(&self) -> Result<(), PresentationSpecError> {
        if self.columns.is_empty() {
            return Err(PresentationSpecError::Invalid(
                "columns must contain at least one column".to_string(),
            ));
        }

        for (index, column) in self.columns.iter().enumerate() {
            column.width.validate(index)?;
            self.validate_column_options(index, column)?;
            self.validate_row_source_column("columns", index, column.name)?;
        }

        for (index, sort) in self.sort.iter().enumerate() {
            self.validate_row_source_column("sort", index, sort.column)?;
        }

        Ok(())
    }

    pub fn validate_for_query_target(
        &self,
        target: QueryTarget,
    ) -> Result<(), PresentationSpecError> {
        self.validate_for_result_kind(PresentationResultKind::from_query_target(target))
    }

    pub fn validate_for_result_kind(
        &self,
        result_kind: PresentationResultKind,
    ) -> Result<(), PresentationSpecError> {
        match self.row_source {
            Some(row_source) if !row_source.kind.supports(result_kind) => {
                return Err(PresentationSpecError::Invalid(format!(
                    "row_source kind `{}` is not supported for {} results",
                    row_source.kind.as_str(),
                    result_kind.as_str()
                )));
            }
            _ => {}
        }

        for (index, column) in self.columns.iter().enumerate() {
            if !column.name.definition().supports(result_kind) {
                return Err(PresentationSpecError::Invalid(format!(
                    "columns[{index}].name `{}` is not supported for {} results",
                    column.name.as_str(),
                    result_kind.as_str()
                )));
            }
        }

        for (index, sort) in self.sort.iter().enumerate() {
            if !sort.column.definition().supports(result_kind) {
                return Err(PresentationSpecError::Invalid(format!(
                    "sort[{index}].column `{}` is not supported for {} results",
                    sort.column.as_str(),
                    result_kind.as_str()
                )));
            }
        }

        Ok(())
    }

    pub fn required_includes_for_query_target(
        &self,
        target: QueryTarget,
    ) -> Result<Vec<QueryInclude>, PresentationSpecError> {
        let result_kind = PresentationResultKind::from_query_target(target);
        self.validate_for_result_kind(result_kind)?;

        let mut includes = Vec::new();
        for column in self
            .columns
            .iter()
            .map(|column| column.name)
            .chain(self.sort.iter().map(|sort| sort.column))
        {
            append_unique_includes(
                &mut includes,
                column.definition().required_includes(result_kind),
            );
        }
        if let Some(row_source) = self.row_source {
            append_unique_includes(&mut includes, row_source.kind.required_includes());
        }
        Ok(includes)
    }

    pub fn combined_includes_for_query_target(
        &self,
        target: QueryTarget,
        explicit: &[QueryInclude],
    ) -> Result<Vec<QueryInclude>, PresentationSpecError> {
        let mut includes = Vec::new();
        append_unique_includes(&mut includes, explicit);
        let inferred = self.required_includes_for_query_target(target)?;
        append_unique_includes(&mut includes, &inferred);
        Ok(includes)
    }

    pub fn extract_value(
        &self,
        column: PresentationColumn,
        result: &QueryResultNode,
        row_context: Option<&PresentationRowContext>,
    ) -> Result<PresentationExtractedValue, PresentationValueError> {
        if let Some(spec) = self.columns.iter().find(|spec| spec.name == column) {
            return spec.extract_value(result, row_context);
        }

        let outline_path = column
            .definition()
            .options
            .outline_path
            .then(PresentationOutlinePathSpec::default);
        extract_registered_value(column, result, row_context, outline_path.as_ref())
    }

    pub fn sort_rows(
        &self,
        results: &[QueryResultNode],
        rows: Vec<PresentationRow>,
    ) -> Result<Vec<PresentationRow>, PresentationSortError> {
        if self.sort.is_empty() {
            return Ok(rows);
        }

        let plan = self.prepare_sort_rows(results, rows)?;
        Ok(self.finish_sort_rows(plan))
    }

    pub(crate) fn prepare_sort_rows(
        &self,
        results: &[QueryResultNode],
        rows: Vec<PresentationRow>,
    ) -> Result<PresentationSortPlan, PresentationSortError> {
        let mut sortable_rows = Vec::with_capacity(rows.len());
        for (original_index, row) in rows.into_iter().enumerate() {
            let result = results.get(row.result_index).ok_or_else(|| {
                PresentationSortError::new(format!(
                    "presentation row {original_index} references missing result_index {}",
                    row.result_index
                ))
            })?;
            let mut keys = Vec::with_capacity(self.sort.len());
            for (sort_index, sort) in self.sort.iter().enumerate() {
                let extracted = self
                    .extract_value(sort.column, result, row.row_context.as_ref())
                    .map_err(|source| {
                        PresentationSortError::new(format!(
                            "failed to extract sort[{sort_index}] column `{}` for row {original_index}: {source}",
                            sort.column.as_str()
                        ))
                    })?;
                keys.push(extracted.value);
            }
            sortable_rows.push(PresentationSortableRow {
                original_index,
                row,
                keys,
            });
        }
        Ok(PresentationSortPlan {
            rows: sortable_rows,
        })
    }

    pub(crate) fn finish_sort_rows(&self, mut plan: PresentationSortPlan) -> Vec<PresentationRow> {
        plan.rows.sort_by(|left, right| {
            for (index, sort) in self.sort.iter().enumerate() {
                let ordering =
                    compare_sort_values(&left.keys[index], &right.keys[index], sort.direction);
                if ordering != Ordering::Equal {
                    return ordering;
                }
            }
            left.original_index.cmp(&right.original_index)
        });
        plan.rows.into_iter().map(|entry| entry.row).collect()
    }

    pub fn layout_rows(
        &self,
        results: &[QueryResultNode],
        rows: Vec<PresentationRow>,
    ) -> Result<Vec<PresentationRow>, PresentationLayoutError> {
        let plan = self.prepare_layout_rows(results, rows)?;
        let widths = self.resolve_layout_widths(plan.natural_widths())?;
        Ok(self.finish_layout_rows(plan, &widths))
    }

    pub(crate) fn prepare_layout_rows(
        &self,
        results: &[QueryResultNode],
        rows: Vec<PresentationRow>,
    ) -> Result<PresentationLayoutPlan, PresentationLayoutError> {
        let mut natural_widths = vec![0; self.columns.len()];
        let mut prepared_rows = Vec::with_capacity(rows.len());

        for (row_index, mut row) in rows.into_iter().enumerate() {
            let result = results.get(row.result_index).ok_or_else(|| {
                PresentationLayoutError::new(format!(
                    "presentation row {row_index} references missing result_index {}",
                    row.result_index
                ))
            })?;
            let mut cells = Vec::with_capacity(self.columns.len());

            for (column_index, column) in self.columns.iter().enumerate() {
                let extracted = column
                    .extract_value(result, row.row_context.as_ref())
                    .map_err(|source| {
                        PresentationLayoutError::new(format!(
                            "failed to extract columns[{column_index}] `{}` for row {row_index}: {source}",
                            column.name.as_str()
                        ))
                    })?;
                let search_text = extracted.search_text();
                natural_widths[column_index] =
                    natural_widths[column_index].max(presentation_text_width(&search_text));
                cells.push(PresentationCell {
                    search_text,
                    display_text: String::new(),
                    role: extracted.role,
                });
            }

            row.cells = cells;
            prepared_rows.push(row);
        }

        Ok(PresentationLayoutPlan {
            rows: prepared_rows,
            natural_widths,
        })
    }

    pub(crate) fn resolve_layout_widths(
        &self,
        natural_widths: &[usize],
    ) -> Result<Vec<usize>, PresentationLayoutError> {
        self.columns
            .iter()
            .enumerate()
            .map(|(column_index, column)| {
                resolve_layout_width(&column.width, natural_widths[column_index], column_index)
            })
            .collect()
    }

    pub(crate) fn finish_layout_rows(
        &self,
        mut plan: PresentationLayoutPlan,
        widths: &[usize],
    ) -> Vec<PresentationRow> {
        debug_assert_eq!(widths.len(), self.columns.len());
        for row in &mut plan.rows {
            for (column_index, cell) in row.cells.iter_mut().enumerate() {
                cell.display_text = fit_cell_text(
                    &cell.search_text,
                    widths[column_index],
                    self.columns[column_index].truncate.as_ref(),
                );
            }
        }
        plan.rows
    }

    pub fn expand_rows(
        &self,
        results: &[QueryResultNode],
    ) -> Result<Vec<PresentationRow>, PresentationRowExpansionError> {
        let Some(row_source) = self.row_source else {
            return Ok((0..results.len())
                .map(|result_index| PresentationRow {
                    result_index,
                    row_context: None,
                    cells: Vec::new(),
                })
                .collect());
        };

        let mut rows = Vec::new();
        for (result_index, result) in results.iter().enumerate() {
            match row_source.kind {
                PresentationRowSourceKind::Tags => {
                    let tags = match result {
                        QueryResultNode::File(node) => &node.tags,
                        QueryResultNode::Heading(node) => &node.all_tags,
                        QueryResultNode::Link(_) => {
                            return Err(PresentationRowExpansionError::unsupported_result(
                                row_source.kind,
                                result,
                                result_index,
                            ));
                        }
                    };
                    for value in tags {
                        rows.push(PresentationRow {
                            result_index,
                            row_context: Some(PresentationRowContext::Tag {
                                value: value.clone(),
                            }),
                            cells: Vec::new(),
                        });
                    }
                }
                PresentationRowSourceKind::EffectiveProperties => {
                    let properties = match result {
                        QueryResultNode::File(node) => node.effective_properties.as_deref(),
                        QueryResultNode::Heading(node) => node.effective_properties.as_deref(),
                        QueryResultNode::Link(_) => {
                            return Err(PresentationRowExpansionError::unsupported_result(
                                row_source.kind,
                                result,
                                result_index,
                            ));
                        }
                    }
                    .ok_or_else(|| {
                        PresentationRowExpansionError::missing_include(
                            row_source.kind,
                            "effective_properties",
                            result_index,
                        )
                    })?;
                    for property in properties {
                        rows.push(PresentationRow {
                            result_index,
                            row_context: Some(PresentationRowContext::EffectiveProperty {
                                name: property.key.clone(),
                                value: property.value.clone().unwrap_or_default(),
                            }),
                            cells: Vec::new(),
                        });
                    }
                }
                PresentationRowSourceKind::Keywords => {
                    let keywords = match result {
                        QueryResultNode::File(node) => node.keywords.as_deref(),
                        QueryResultNode::Heading(node) => node.keywords.as_deref(),
                        QueryResultNode::Link(_) => {
                            return Err(PresentationRowExpansionError::unsupported_result(
                                row_source.kind,
                                result,
                                result_index,
                            ));
                        }
                    }
                    .ok_or_else(|| {
                        PresentationRowExpansionError::missing_include(
                            row_source.kind,
                            "keywords",
                            result_index,
                        )
                    })?;
                    for keyword in keywords {
                        rows.push(PresentationRow {
                            result_index,
                            row_context: Some(PresentationRowContext::Keyword {
                                name: keyword.keyword.clone(),
                                value: keyword.value.clone().unwrap_or_default(),
                            }),
                            cells: Vec::new(),
                        });
                    }
                }
            }
        }

        Ok(rows)
    }

    pub fn build_response(
        &self,
        database_id: impl Into<String>,
        generation: i64,
        results: Vec<QueryResultNode>,
    ) -> Result<PresentationResponse, PresentationBuildError> {
        let rows = self
            .expand_rows(&results)
            .map_err(PresentationBuildError::Expansion)?;
        let rows = self
            .sort_rows(&results, rows)
            .map_err(PresentationBuildError::Sort)?;
        let rows = self
            .layout_rows(&results, rows)
            .map_err(PresentationBuildError::Layout)?;

        Ok(PresentationResponse::new(
            database_id,
            generation,
            results,
            rows,
        ))
    }

    fn validate_column_options(
        &self,
        index: usize,
        column: &PresentationColumnSpec,
    ) -> Result<(), PresentationSpecError> {
        if column.outline_path.is_some() && !column.name.definition().options.outline_path {
            return Err(PresentationSpecError::Invalid(format!(
                "columns[{index}].outline_path is not supported by column `{}`",
                column.name.as_str()
            )));
        }
        Ok(())
    }

    fn validate_row_source_column(
        &self,
        section: &str,
        index: usize,
        column: PresentationColumn,
    ) -> Result<(), PresentationSpecError> {
        let Some(required) = column.definition().required_row_source else {
            return Ok(());
        };
        let actual = self.row_source.map(|source| source.kind);
        if actual == Some(required) {
            return Ok(());
        }

        Err(PresentationSpecError::Invalid(format!(
            "{section}[{index}].{} `{}` requires row_source kind `{}`",
            if section == "sort" { "column" } else { "name" },
            column.as_str(),
            required.as_str()
        )))
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct PresentationColumnSpec {
    pub name: PresentationColumn,
    #[serde(default)]
    pub width: PresentationWidthSpec,
    #[serde(default)]
    pub truncate: Option<PresentationTruncationSpec>,
    #[serde(default)]
    pub outline_path: Option<PresentationOutlinePathSpec>,
}

impl PresentationColumnSpec {
    pub fn extract_value(
        &self,
        result: &QueryResultNode,
        row_context: Option<&PresentationRowContext>,
    ) -> Result<PresentationExtractedValue, PresentationValueError> {
        let outline_path = self
            .name
            .definition()
            .options
            .outline_path
            .then(|| self.outline_path.clone().unwrap_or_default());
        extract_registered_value(self.name, result, row_context, outline_path.as_ref())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct PresentationSortSpec {
    pub column: PresentationColumn,
    #[serde(default)]
    pub direction: PresentationSortDirection,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Deserialize)]
#[serde(rename_all = "kebab-case")]
pub enum PresentationColumn {
    Title,
    TodoKeyword,
    TodoType,
    Priority,
    OutlinePath,
    Tags,
    ScheduledRaw,
    DeadlineRaw,
    ClosedRaw,
    FileTitle,
    FileName,
    FilePath,
    LineNumber,
    LinkType,
    LinkTarget,
    LinkDescription,
    ResolutionStatus,
    SourceOutlinePath,
    TargetOutlinePath,
    Rank,
    Tag,
    PropertyName,
    PropertyValue,
    KeywordName,
    KeywordValue,
}

impl PresentationColumn {
    pub const fn as_str(self) -> &'static str {
        match self {
            Self::Title => "title",
            Self::TodoKeyword => "todo-keyword",
            Self::TodoType => "todo-type",
            Self::Priority => "priority",
            Self::OutlinePath => "outline-path",
            Self::Tags => "tags",
            Self::ScheduledRaw => "scheduled-raw",
            Self::DeadlineRaw => "deadline-raw",
            Self::ClosedRaw => "closed-raw",
            Self::FileTitle => "file-title",
            Self::FileName => "file-name",
            Self::FilePath => "file-path",
            Self::LineNumber => "line-number",
            Self::LinkType => "link-type",
            Self::LinkTarget => "link-target",
            Self::LinkDescription => "link-description",
            Self::ResolutionStatus => "resolution-status",
            Self::SourceOutlinePath => "source-outline-path",
            Self::TargetOutlinePath => "target-outline-path",
            Self::Rank => "rank",
            Self::Tag => "tag",
            Self::PropertyName => "property-name",
            Self::PropertyValue => "property-value",
            Self::KeywordName => "keyword-name",
            Self::KeywordValue => "keyword-value",
        }
    }

    pub fn definition(self) -> PresentationColumnDefinition {
        use PresentationColumn as Column;
        use PresentationRole as SemanticRole;
        use PresentationRoleRule as Role;
        use PresentationValueSource as Value;

        let common = PresentationColumnOptionSupport {
            outline_path: false,
        };
        let outline = PresentationColumnOptionSupport { outline_path: true };

        match self {
            Column::Title => PresentationColumnDefinition::new(
                HEADING_AND_SEARCH_RESULTS,
                PresentationIncludeRule::None,
                Value::Title,
                Role::Static(SemanticRole::Title),
                common,
                None,
            ),
            Column::TodoKeyword => PresentationColumnDefinition::new(
                HEADING_RESULTS,
                PresentationIncludeRule::None,
                Value::TodoKeyword,
                Role::TodoKeyword,
                common,
                None,
            ),
            Column::TodoType => PresentationColumnDefinition::new(
                HEADING_RESULTS,
                PresentationIncludeRule::None,
                Value::TodoType,
                Role::None,
                common,
                None,
            ),
            Column::Priority => PresentationColumnDefinition::new(
                HEADING_RESULTS,
                PresentationIncludeRule::None,
                Value::Priority,
                Role::Static(SemanticRole::Priority),
                common,
                None,
            ),
            Column::OutlinePath => PresentationColumnDefinition::new(
                HEADING_AND_SEARCH_RESULTS,
                PresentationIncludeRule::ForKind(PresentationResultKind::Heading, PATH_INCLUDE),
                Value::OutlinePath,
                Role::Static(SemanticRole::Heading),
                outline,
                None,
            ),
            Column::Tags => PresentationColumnDefinition::new(
                HEADING_AND_FILE_RESULTS,
                PresentationIncludeRule::None,
                Value::Tags,
                Role::Static(SemanticRole::Tag),
                common,
                None,
            ),
            Column::ScheduledRaw => PresentationColumnDefinition::new(
                HEADING_RESULTS,
                PresentationIncludeRule::None,
                Value::ScheduledRaw,
                Role::Static(SemanticRole::Date),
                common,
                None,
            ),
            Column::DeadlineRaw => PresentationColumnDefinition::new(
                HEADING_RESULTS,
                PresentationIncludeRule::None,
                Value::DeadlineRaw,
                Role::Static(SemanticRole::Date),
                common,
                None,
            ),
            Column::ClosedRaw => PresentationColumnDefinition::new(
                HEADING_RESULTS,
                PresentationIncludeRule::None,
                Value::ClosedRaw,
                Role::Static(SemanticRole::Date),
                common,
                None,
            ),
            Column::FileTitle => PresentationColumnDefinition::new(
                HEADING_AND_FILE_RESULTS,
                PresentationIncludeRule::ForKind(PresentationResultKind::Heading, PATH_INCLUDE),
                Value::FileTitle,
                Role::Static(SemanticRole::Title),
                common,
                None,
            ),
            Column::FileName => PresentationColumnDefinition::new(
                FILE_LOCATION_RESULTS,
                PresentationIncludeRule::None,
                Value::FileName,
                Role::Static(SemanticRole::FileName),
                common,
                None,
            ),
            Column::FilePath => PresentationColumnDefinition::new(
                FILE_LOCATION_RESULTS,
                PresentationIncludeRule::None,
                Value::FilePath,
                Role::Static(SemanticRole::FilePath),
                common,
                None,
            ),
            Column::LineNumber => PresentationColumnDefinition::new(
                FILE_LOCATION_RESULTS,
                PresentationIncludeRule::None,
                Value::LineNumber,
                Role::None,
                common,
                None,
            ),
            Column::LinkType => PresentationColumnDefinition::new(
                LINK_RESULTS,
                PresentationIncludeRule::None,
                Value::LinkType,
                Role::None,
                common,
                None,
            ),
            Column::LinkTarget => PresentationColumnDefinition::new(
                LINK_RESULTS,
                PresentationIncludeRule::None,
                Value::LinkTarget,
                Role::None,
                common,
                None,
            ),
            Column::LinkDescription => PresentationColumnDefinition::new(
                LINK_RESULTS,
                PresentationIncludeRule::None,
                Value::LinkDescription,
                Role::None,
                common,
                None,
            ),
            Column::ResolutionStatus => PresentationColumnDefinition::new(
                LINK_RESULTS,
                PresentationIncludeRule::None,
                Value::ResolutionStatus,
                Role::None,
                common,
                None,
            ),
            Column::SourceOutlinePath => PresentationColumnDefinition::new(
                LINK_RESULTS,
                PresentationIncludeRule::Always(PATH_INCLUDE),
                Value::SourceOutlinePath,
                Role::Static(SemanticRole::Heading),
                outline,
                None,
            ),
            Column::TargetOutlinePath => PresentationColumnDefinition::new(
                LINK_RESULTS,
                PresentationIncludeRule::Always(TARGET_INCLUDE),
                Value::TargetOutlinePath,
                Role::Static(SemanticRole::Heading),
                outline,
                None,
            ),
            Column::Rank => PresentationColumnDefinition::new(
                SEARCH_RESULTS,
                PresentationIncludeRule::None,
                Value::Rank,
                Role::None,
                common,
                None,
            ),
            Column::Tag => PresentationColumnDefinition::new(
                HEADING_AND_FILE_RESULTS,
                PresentationIncludeRule::None,
                Value::RowTag,
                Role::Static(SemanticRole::Tag),
                common,
                Some(PresentationRowSourceKind::Tags),
            ),
            Column::PropertyName => PresentationColumnDefinition::new(
                HEADING_AND_FILE_RESULTS,
                PresentationIncludeRule::Always(EFFECTIVE_PROPERTIES_INCLUDE),
                Value::RowPropertyName,
                Role::Static(SemanticRole::PropertyName),
                common,
                Some(PresentationRowSourceKind::EffectiveProperties),
            ),
            Column::PropertyValue => PresentationColumnDefinition::new(
                HEADING_AND_FILE_RESULTS,
                PresentationIncludeRule::Always(EFFECTIVE_PROPERTIES_INCLUDE),
                Value::RowPropertyValue,
                Role::Static(SemanticRole::PropertyValue),
                common,
                Some(PresentationRowSourceKind::EffectiveProperties),
            ),
            Column::KeywordName => PresentationColumnDefinition::new(
                HEADING_AND_FILE_RESULTS,
                PresentationIncludeRule::Always(KEYWORDS_INCLUDE),
                Value::RowKeywordName,
                Role::Static(SemanticRole::KeywordName),
                common,
                Some(PresentationRowSourceKind::Keywords),
            ),
            Column::KeywordValue => PresentationColumnDefinition::new(
                HEADING_AND_FILE_RESULTS,
                PresentationIncludeRule::Always(KEYWORDS_INCLUDE),
                Value::RowKeywordValue,
                Role::Static(SemanticRole::KeywordValue),
                common,
                Some(PresentationRowSourceKind::Keywords),
            ),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PresentationResultKind {
    Heading,
    File,
    Link,
    Search,
}

impl PresentationResultKind {
    pub fn from_query_target(target: QueryTarget) -> Self {
        match target {
            QueryTarget::Headings => Self::Heading,
            QueryTarget::Files => Self::File,
            QueryTarget::Links => Self::Link,
        }
    }

    pub const fn as_str(self) -> &'static str {
        match self {
            Self::Heading => "heading",
            Self::File => "file",
            Self::Link => "link",
            Self::Search => "search",
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PresentationValueSource {
    Title,
    TodoKeyword,
    TodoType,
    Priority,
    OutlinePath,
    Tags,
    ScheduledRaw,
    DeadlineRaw,
    ClosedRaw,
    FileTitle,
    FileName,
    FilePath,
    LineNumber,
    LinkType,
    LinkTarget,
    LinkDescription,
    ResolutionStatus,
    SourceOutlinePath,
    TargetOutlinePath,
    Rank,
    RowTag,
    RowPropertyName,
    RowPropertyValue,
    RowKeywordName,
    RowKeywordValue,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum PresentationValue {
    Missing,
    Text(String),
    Integer(i64),
    TextList(Vec<String>),
    Outline(PresentationOutlineValue),
}

impl PresentationValue {
    pub fn to_text(&self) -> String {
        match self {
            Self::Missing => String::new(),
            Self::Text(value) => value.clone(),
            Self::Integer(value) => value.to_string(),
            Self::TextList(values) => values.join(","),
            Self::Outline(value) => value.components.join(&value.separator),
        }
    }

    fn compare_present(&self, other: &Self) -> Ordering {
        match (self, other) {
            (Self::Text(left), Self::Text(right)) => left.cmp(right),
            (Self::Integer(left), Self::Integer(right)) => left.cmp(right),
            (Self::TextList(left), Self::TextList(right)) => left.cmp(right),
            (Self::Outline(left), Self::Outline(right)) => left.components.cmp(&right.components),
            (left, right) => left.sort_type_rank().cmp(&right.sort_type_rank()),
        }
    }

    const fn sort_type_rank(&self) -> u8 {
        match self {
            Self::Missing => 0,
            Self::Text(_) => 1,
            Self::Integer(_) => 2,
            Self::TextList(_) => 3,
            Self::Outline(_) => 4,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PresentationOutlineValue {
    pub components: Vec<String>,
    pub separator: String,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PresentationExtractedValue {
    pub value: PresentationValue,
    pub role: Option<PresentationRole>,
}

impl PresentationExtractedValue {
    pub fn search_text(&self) -> String {
        self.value.to_text()
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PresentationValueError {
    message: String,
}

impl PresentationValueError {
    fn new(message: impl Into<String>) -> Self {
        Self {
            message: message.into(),
        }
    }
}

impl fmt::Display for PresentationValueError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.message)
    }
}

impl Error for PresentationValueError {}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PresentationSortError {
    message: String,
}

impl PresentationSortError {
    fn new(message: impl Into<String>) -> Self {
        Self {
            message: message.into(),
        }
    }
}

impl fmt::Display for PresentationSortError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.message)
    }
}

impl Error for PresentationSortError {}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PresentationLayoutError {
    message: String,
}

impl PresentationLayoutError {
    fn new(message: impl Into<String>) -> Self {
        Self {
            message: message.into(),
        }
    }
}

impl fmt::Display for PresentationLayoutError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.message)
    }
}

impl Error for PresentationLayoutError {}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PresentationRowExpansionError {
    message: String,
}

impl PresentationRowExpansionError {
    fn new(message: impl Into<String>) -> Self {
        Self {
            message: message.into(),
        }
    }

    fn unsupported_result(
        row_source: PresentationRowSourceKind,
        result: &QueryResultNode,
        result_index: usize,
    ) -> Self {
        let kind = match result {
            QueryResultNode::File(node) => node.kind.as_str(),
            QueryResultNode::Heading(node) => node.kind.as_str(),
            QueryResultNode::Link(node) => node.kind.as_str(),
        };
        Self::new(format!(
            "row_source kind `{}` cannot expand {kind} result at result_index {result_index}",
            row_source.as_str()
        ))
    }

    fn missing_include(
        row_source: PresentationRowSourceKind,
        include: &str,
        result_index: usize,
    ) -> Self {
        Self::new(format!(
            "row_source kind `{}` requires query include `{include}` for result_index {result_index}",
            row_source.as_str()
        ))
    }
}

impl fmt::Display for PresentationRowExpansionError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.message)
    }
}

impl Error for PresentationRowExpansionError {}

#[derive(Debug)]
pub enum PresentationBuildError {
    Expansion(PresentationRowExpansionError),
    Sort(PresentationSortError),
    Layout(PresentationLayoutError),
}

impl fmt::Display for PresentationBuildError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Expansion(source) => write!(f, "failed to expand presentation rows: {source}"),
            Self::Sort(source) => write!(f, "failed to sort presentation rows: {source}"),
            Self::Layout(source) => write!(f, "failed to prepare presentation cells: {source}"),
        }
    }
}

impl Error for PresentationBuildError {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        match self {
            Self::Expansion(source) => Some(source),
            Self::Sort(source) => Some(source),
            Self::Layout(source) => Some(source),
        }
    }
}

#[derive(Debug, Clone)]
pub(crate) struct PresentationSortPlan {
    rows: Vec<PresentationSortableRow>,
}

#[derive(Debug, Clone)]
struct PresentationSortableRow {
    original_index: usize,
    row: PresentationRow,
    keys: Vec<PresentationValue>,
}

#[derive(Debug, Clone)]
pub(crate) struct PresentationLayoutPlan {
    rows: Vec<PresentationRow>,
    natural_widths: Vec<usize>,
}

impl PresentationLayoutPlan {
    pub(crate) fn natural_widths(&self) -> &[usize] {
        &self.natural_widths
    }
}

fn compare_sort_values(
    left: &PresentationValue,
    right: &PresentationValue,
    direction: PresentationSortDirection,
) -> Ordering {
    match (left, right) {
        (PresentationValue::Missing, PresentationValue::Missing) => Ordering::Equal,
        (PresentationValue::Missing, _) => Ordering::Greater,
        (_, PresentationValue::Missing) => Ordering::Less,
        _ => {
            let ordering = left.compare_present(right);
            match direction {
                PresentationSortDirection::Asc => ordering,
                PresentationSortDirection::Desc => ordering.reverse(),
            }
        }
    }
}

fn resolve_layout_width(
    spec: &PresentationWidthSpec,
    natural_width: usize,
    column_index: usize,
) -> Result<usize, PresentationLayoutError> {
    spec.validate(column_index)
        .map_err(|source| PresentationLayoutError::new(source.to_string()))?;

    Ok(match spec.mode {
        PresentationWidthMode::Auto => natural_width,
        PresentationWidthMode::Max => {
            natural_width.min(spec.value.expect("validated max width") as usize)
        }
        PresentationWidthMode::Fixed => spec.value.expect("validated fixed width") as usize,
    })
}

fn presentation_text_width(value: &str) -> usize {
    value.chars().count()
}

fn fit_cell_text(
    value: &str,
    width: usize,
    truncation: Option<&PresentationTruncationSpec>,
) -> String {
    let value_width = presentation_text_width(value);
    let mut display = if value_width > width {
        let default_truncation = PresentationTruncationSpec::default();
        let truncation = truncation.unwrap_or(&default_truncation);
        truncate_text(value, width, truncation)
    } else {
        value.to_string()
    };

    let display_width = presentation_text_width(&display);
    if display_width < width {
        display.push_str(&" ".repeat(width - display_width));
    }
    display
}

fn truncate_text(value: &str, width: usize, truncation: &PresentationTruncationSpec) -> String {
    if width == 0 {
        return String::new();
    }

    let marker_width = presentation_text_width(&truncation.marker);
    if marker_width >= width {
        return text_prefix(&truncation.marker, width);
    }

    let content_width = width - marker_width;
    match truncation.position {
        PresentationTruncationPosition::Left => {
            format!("{}{}", truncation.marker, text_suffix(value, content_width))
        }
        PresentationTruncationPosition::Middle => {
            let left_width = content_width.div_ceil(2);
            let right_width = content_width / 2;
            format!(
                "{}{}{}",
                text_prefix(value, left_width),
                truncation.marker,
                text_suffix(value, right_width)
            )
        }
        PresentationTruncationPosition::Right => {
            format!("{}{}", text_prefix(value, content_width), truncation.marker)
        }
    }
}

fn text_prefix(value: &str, width: usize) -> String {
    value.chars().take(width).collect()
}

fn text_suffix(value: &str, width: usize) -> String {
    let skip = presentation_text_width(value).saturating_sub(width);
    value.chars().skip(skip).collect()
}

fn append_unique_includes(target: &mut Vec<QueryInclude>, values: &[QueryInclude]) {
    for value in values {
        if !target.contains(value) {
            target.push(*value);
        }
    }
}

fn extract_registered_value(
    column: PresentationColumn,
    result: &QueryResultNode,
    row_context: Option<&PresentationRowContext>,
    outline_path: Option<&PresentationOutlinePathSpec>,
) -> Result<PresentationExtractedValue, PresentationValueError> {
    let definition = column.definition();
    let value = definition
        .value_source
        .extract(column, result, row_context, outline_path)?;
    let todo_type = match result {
        QueryResultNode::Heading(node) => node.todo_type.as_deref(),
        QueryResultNode::File(_) | QueryResultNode::Link(_) => None,
    };
    Ok(PresentationExtractedValue {
        value,
        role: definition.role_rule.resolve(todo_type),
    })
}

impl PresentationValueSource {
    fn extract(
        self,
        column: PresentationColumn,
        result: &QueryResultNode,
        row_context: Option<&PresentationRowContext>,
        outline_path: Option<&PresentationOutlinePathSpec>,
    ) -> Result<PresentationValue, PresentationValueError> {
        use PresentationValue as Value;

        let value = match self {
            Self::Title => match result {
                QueryResultNode::File(node) => Value::Text(node.title.clone()),
                QueryResultNode::Heading(node) => Value::Text(node.title.clone()),
                QueryResultNode::Link(_) => return Err(unexpected_result(column, result)),
            },
            Self::TodoKeyword => match result {
                QueryResultNode::Heading(node) => optional_text(node.todo_keyword.as_deref()),
                QueryResultNode::File(_) => Value::Missing,
                QueryResultNode::Link(_) => return Err(unexpected_result(column, result)),
            },
            Self::TodoType => match result {
                QueryResultNode::Heading(node) => optional_text(node.todo_type.as_deref()),
                QueryResultNode::File(_) => Value::Missing,
                QueryResultNode::Link(_) => return Err(unexpected_result(column, result)),
            },
            Self::Priority => match result {
                QueryResultNode::Heading(node) => optional_text(node.priority.as_deref()),
                QueryResultNode::File(_) => Value::Missing,
                QueryResultNode::Link(_) => return Err(unexpected_result(column, result)),
            },
            Self::OutlinePath => extract_generic_outline(column, result, outline_path)?,
            Self::Tags => match result {
                QueryResultNode::File(node) => Value::TextList(node.tags.clone()),
                QueryResultNode::Heading(node) => Value::TextList(node.all_tags.clone()),
                QueryResultNode::Link(_) => return Err(unexpected_result(column, result)),
            },
            Self::ScheduledRaw => match result {
                QueryResultNode::Heading(node) => optional_text(node.scheduled_raw.as_deref()),
                QueryResultNode::File(_) => Value::Missing,
                QueryResultNode::Link(_) => return Err(unexpected_result(column, result)),
            },
            Self::DeadlineRaw => match result {
                QueryResultNode::Heading(node) => optional_text(node.deadline_raw.as_deref()),
                QueryResultNode::File(_) => Value::Missing,
                QueryResultNode::Link(_) => return Err(unexpected_result(column, result)),
            },
            Self::ClosedRaw => match result {
                QueryResultNode::Heading(node) => optional_text(node.closed_raw.as_deref()),
                QueryResultNode::File(_) => Value::Missing,
                QueryResultNode::Link(_) => return Err(unexpected_result(column, result)),
            },
            Self::FileTitle => extract_file_title(column, result)?,
            Self::FileName => extract_file_name(result),
            Self::FilePath => Value::Text(result_file_path(result).to_string()),
            Self::LineNumber => result_line(result)
                .map(Value::Integer)
                .unwrap_or(Value::Missing),
            Self::LinkType => match result {
                QueryResultNode::Link(node) => Value::Text(node.link_type.clone()),
                QueryResultNode::File(_) | QueryResultNode::Heading(_) => {
                    return Err(unexpected_result(column, result));
                }
            },
            Self::LinkTarget => match result {
                QueryResultNode::Link(node) => Value::Text(node.raw_target.clone()),
                QueryResultNode::File(_) | QueryResultNode::Heading(_) => {
                    return Err(unexpected_result(column, result));
                }
            },
            Self::LinkDescription => match result {
                QueryResultNode::Link(node) => optional_text(node.raw_description.as_deref()),
                QueryResultNode::File(_) | QueryResultNode::Heading(_) => {
                    return Err(unexpected_result(column, result));
                }
            },
            Self::ResolutionStatus => match result {
                QueryResultNode::Link(node) => optional_text(node.resolution_status.as_deref()),
                QueryResultNode::File(_) | QueryResultNode::Heading(_) => {
                    return Err(unexpected_result(column, result));
                }
            },
            Self::SourceOutlinePath => extract_source_outline(column, result, outline_path)?,
            Self::TargetOutlinePath => extract_target_outline(column, result, outline_path)?,
            Self::Rank => {
                return Err(PresentationValueError::new(
                    "rank extraction requires a search result representation",
                ));
            }
            Self::RowTag => match row_context {
                Some(PresentationRowContext::Tag { value }) => Value::Text(value.clone()),
                _ => return Err(missing_row_context(column, "tag")),
            },
            Self::RowPropertyName => match row_context {
                Some(PresentationRowContext::EffectiveProperty { name, .. }) => {
                    Value::Text(name.clone())
                }
                _ => return Err(missing_row_context(column, "effective-property")),
            },
            Self::RowPropertyValue => match row_context {
                Some(PresentationRowContext::EffectiveProperty { value, .. }) => {
                    Value::Text(value.clone())
                }
                _ => return Err(missing_row_context(column, "effective-property")),
            },
            Self::RowKeywordName => match row_context {
                Some(PresentationRowContext::Keyword { name, .. }) => Value::Text(name.clone()),
                _ => return Err(missing_row_context(column, "keyword")),
            },
            Self::RowKeywordValue => match row_context {
                Some(PresentationRowContext::Keyword { value, .. }) => Value::Text(value.clone()),
                _ => return Err(missing_row_context(column, "keyword")),
            },
        };
        Ok(value)
    }
}

fn optional_text(value: Option<&str>) -> PresentationValue {
    value
        .map(|value| PresentationValue::Text(value.to_string()))
        .unwrap_or(PresentationValue::Missing)
}

fn result_file_path(result: &QueryResultNode) -> &str {
    match result {
        QueryResultNode::File(node) => &node.location.file_path,
        QueryResultNode::Heading(node) => &node.location.file_path,
        QueryResultNode::Link(node) => &node.location.file_path,
    }
}

fn result_line(result: &QueryResultNode) -> Option<i64> {
    match result {
        QueryResultNode::File(node) => node.location.line,
        QueryResultNode::Heading(node) => node.location.line,
        QueryResultNode::Link(node) => node.location.line,
    }
}

fn extract_file_name(result: &QueryResultNode) -> PresentationValue {
    if let QueryResultNode::File(node) = result {
        return PresentationValue::Text(node.name.clone());
    }

    let path = result_file_path(result);
    let name = Path::new(path)
        .file_name()
        .and_then(|name| name.to_str())
        .unwrap_or(path);
    PresentationValue::Text(name.to_string())
}

fn extract_file_title(
    column: PresentationColumn,
    result: &QueryResultNode,
) -> Result<PresentationValue, PresentationValueError> {
    match result {
        QueryResultNode::File(node) => Ok(PresentationValue::Text(node.title.clone())),
        QueryResultNode::Heading(node) => {
            let path = node
                .node_path
                .as_deref()
                .ok_or_else(|| missing_include_data(column, "path"))?;
            let (root, _) = outline_data(path);
            root.map(PresentationValue::Text)
                .ok_or_else(|| PresentationValueError::new("heading path has no file root"))
        }
        QueryResultNode::Link(_) => Err(unexpected_result(column, result)),
    }
}

fn extract_generic_outline(
    column: PresentationColumn,
    result: &QueryResultNode,
    options: Option<&PresentationOutlinePathSpec>,
) -> Result<PresentationValue, PresentationValueError> {
    let options = require_outline_options(column, options)?;
    match result {
        QueryResultNode::File(node) => {
            let headings = if options.include_match {
                vec![node.title.clone()]
            } else {
                Vec::new()
            };
            Ok(outline_value(None, headings, options))
        }
        QueryResultNode::Heading(node) => {
            let path = node
                .node_path
                .as_deref()
                .ok_or_else(|| missing_include_data(column, "path"))?;
            let (root, headings) = outline_data(path);
            Ok(outline_value(root, headings, options))
        }
        QueryResultNode::Link(_) => Err(unexpected_result(column, result)),
    }
}

fn extract_source_outline(
    column: PresentationColumn,
    result: &QueryResultNode,
    options: Option<&PresentationOutlinePathSpec>,
) -> Result<PresentationValue, PresentationValueError> {
    let options = require_outline_options(column, options)?;
    let QueryResultNode::Link(node) = result else {
        return Err(unexpected_result(column, result));
    };
    let path = node
        .node_path
        .as_deref()
        .ok_or_else(|| missing_include_data(column, "path"))?;
    let (root, headings) = outline_data(path);
    Ok(outline_value(root, headings, options))
}

fn extract_target_outline(
    column: PresentationColumn,
    result: &QueryResultNode,
    options: Option<&PresentationOutlinePathSpec>,
) -> Result<PresentationValue, PresentationValueError> {
    let options = require_outline_options(column, options)?;
    let QueryResultNode::Link(node) = result else {
        return Err(unexpected_result(column, result));
    };
    let target = node
        .target
        .as_ref()
        .ok_or_else(|| missing_include_data(column, "target"))?;
    let root = target.file.as_ref().map(|file| file.title.clone());
    let headings = target
        .heading
        .as_ref()
        .map(|heading| heading.outline_path.clone())
        .unwrap_or_default();
    Ok(outline_value(root, headings, options))
}

fn require_outline_options(
    column: PresentationColumn,
    options: Option<&PresentationOutlinePathSpec>,
) -> Result<&PresentationOutlinePathSpec, PresentationValueError> {
    options.ok_or_else(|| {
        PresentationValueError::new(format!(
            "presentation column `{}` requires outline-path options",
            column.as_str()
        ))
    })
}

fn outline_data(path: &[PathEntry]) -> (Option<String>, Vec<String>) {
    let mut root = None;
    let mut headings = Vec::new();
    for entry in path {
        match entry {
            PathEntry::File(file) => {
                if root.is_none() {
                    root = Some(file.title.clone());
                }
            }
            PathEntry::Heading(heading) => headings.push(heading.title.clone()),
        }
    }
    (root, headings)
}

fn outline_value(
    root: Option<String>,
    mut headings: Vec<String>,
    options: &PresentationOutlinePathSpec,
) -> PresentationValue {
    if !options.include_match && !headings.is_empty() {
        headings.pop();
    }

    let mut components = Vec::new();
    if options.include_root {
        components.extend(root);
    }
    components.extend(headings);
    PresentationValue::Outline(PresentationOutlineValue {
        components,
        separator: options.separator.clone(),
    })
}

fn missing_include_data(column: PresentationColumn, include: &str) -> PresentationValueError {
    PresentationValueError::new(format!(
        "presentation column `{}` requires query include `{include}`",
        column.as_str()
    ))
}

fn missing_row_context(column: PresentationColumn, kind: &str) -> PresentationValueError {
    PresentationValueError::new(format!(
        "presentation column `{}` requires row context `{kind}`",
        column.as_str()
    ))
}

fn unexpected_result(
    column: PresentationColumn,
    result: &QueryResultNode,
) -> PresentationValueError {
    let kind = match result {
        QueryResultNode::File(node) => node.kind.as_str(),
        QueryResultNode::Heading(node) => node.kind.as_str(),
        QueryResultNode::Link(node) => node.kind.as_str(),
    };
    PresentationValueError::new(format!(
        "presentation column `{}` cannot extract a value from {kind} result",
        column.as_str()
    ))
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize)]
#[serde(rename_all = "kebab-case")]
pub enum PresentationRole {
    Heading,
    Title,
    Todo,
    Done,
    Priority,
    Tag,
    Date,
    FileName,
    FilePath,
    KeywordName,
    KeywordValue,
    PropertyName,
    PropertyValue,
}

impl PresentationRole {
    pub const fn as_str(self) -> &'static str {
        match self {
            Self::Heading => "heading",
            Self::Title => "title",
            Self::Todo => "todo",
            Self::Done => "done",
            Self::Priority => "priority",
            Self::Tag => "tag",
            Self::Date => "date",
            Self::FileName => "file-name",
            Self::FilePath => "file-path",
            Self::KeywordName => "keyword-name",
            Self::KeywordValue => "keyword-value",
            Self::PropertyName => "property-name",
            Self::PropertyValue => "property-value",
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PresentationRoleRule {
    None,
    Static(PresentationRole),
    TodoKeyword,
}

impl PresentationRoleRule {
    pub fn resolve(self, todo_type: Option<&str>) -> Option<PresentationRole> {
        match self {
            Self::None => None,
            Self::Static(role) => Some(role),
            Self::TodoKeyword => match todo_type {
                Some("open") => Some(PresentationRole::Todo),
                Some("closed") => Some(PresentationRole::Done),
                _ => None,
            },
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct PresentationColumnOptionSupport {
    pub outline_path: bool,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct PresentationColumnDefinition {
    pub result_kinds: &'static [PresentationResultKind],
    pub value_source: PresentationValueSource,
    pub role_rule: PresentationRoleRule,
    pub options: PresentationColumnOptionSupport,
    pub required_row_source: Option<PresentationRowSourceKind>,
    include_rule: PresentationIncludeRule,
}

impl PresentationColumnDefinition {
    const fn new(
        result_kinds: &'static [PresentationResultKind],
        include_rule: PresentationIncludeRule,
        value_source: PresentationValueSource,
        role_rule: PresentationRoleRule,
        options: PresentationColumnOptionSupport,
        required_row_source: Option<PresentationRowSourceKind>,
    ) -> Self {
        Self {
            result_kinds,
            value_source,
            role_rule,
            options,
            required_row_source,
            include_rule,
        }
    }

    pub fn supports(self, result_kind: PresentationResultKind) -> bool {
        self.result_kinds.contains(&result_kind)
    }

    pub fn required_includes(self, result_kind: PresentationResultKind) -> &'static [QueryInclude] {
        match self.include_rule {
            PresentationIncludeRule::None => NO_INCLUDES,
            PresentationIncludeRule::Always(includes) => includes,
            PresentationIncludeRule::ForKind(expected, includes) => {
                if expected == result_kind {
                    includes
                } else {
                    NO_INCLUDES
                }
            }
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum PresentationIncludeRule {
    None,
    Always(&'static [QueryInclude]),
    ForKind(PresentationResultKind, &'static [QueryInclude]),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Deserialize, Default)]
#[serde(rename_all = "kebab-case")]
pub enum PresentationSortDirection {
    #[default]
    Asc,
    Desc,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct PresentationRowSourceSpec {
    pub kind: PresentationRowSourceKind,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Deserialize)]
#[serde(rename_all = "kebab-case")]
pub enum PresentationRowSourceKind {
    Tags,
    EffectiveProperties,
    Keywords,
}

impl PresentationRowSourceKind {
    pub const fn as_str(self) -> &'static str {
        match self {
            Self::Tags => "tags",
            Self::EffectiveProperties => "effective-properties",
            Self::Keywords => "keywords",
        }
    }

    pub const fn supports(self, result_kind: PresentationResultKind) -> bool {
        matches!(
            result_kind,
            PresentationResultKind::Heading | PresentationResultKind::File
        )
    }

    pub const fn required_includes(self) -> &'static [QueryInclude] {
        match self {
            Self::Tags => NO_INCLUDES,
            Self::EffectiveProperties => EFFECTIVE_PROPERTIES_INCLUDE,
            Self::Keywords => KEYWORDS_INCLUDE,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct PresentationWidthSpec {
    #[serde(default)]
    pub mode: PresentationWidthMode,
    #[serde(default)]
    pub value: Option<u32>,
}

impl PresentationWidthSpec {
    fn validate(&self, column_index: usize) -> Result<(), PresentationSpecError> {
        match (self.mode, self.value) {
            (PresentationWidthMode::Auto, None) => Ok(()),
            (PresentationWidthMode::Auto, Some(_)) => Err(PresentationSpecError::Invalid(format!(
                "columns[{column_index}].width mode auto does not accept value"
            ))),
            (PresentationWidthMode::Max | PresentationWidthMode::Fixed, None) => {
                Err(PresentationSpecError::Invalid(format!(
                    "columns[{column_index}].width mode {} requires value",
                    self.mode.as_str()
                )))
            }
            (PresentationWidthMode::Max | PresentationWidthMode::Fixed, Some(0)) => {
                Err(PresentationSpecError::Invalid(format!(
                    "columns[{column_index}].width value must be greater than zero"
                )))
            }
            (PresentationWidthMode::Max | PresentationWidthMode::Fixed, Some(_)) => Ok(()),
        }
    }
}

impl Default for PresentationWidthSpec {
    fn default() -> Self {
        Self {
            mode: PresentationWidthMode::Auto,
            value: None,
        }
    }
}
#[derive(Debug, Clone, Copy, PartialEq, Eq, Deserialize, Default)]
#[serde(rename_all = "kebab-case")]
pub enum PresentationWidthMode {
    #[default]
    Auto,
    Max,
    Fixed,
}

impl PresentationWidthMode {
    fn as_str(self) -> &'static str {
        match self {
            Self::Auto => "auto",
            Self::Max => "max",
            Self::Fixed => "fixed",
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct PresentationTruncationSpec {
    #[serde(default)]
    pub position: PresentationTruncationPosition,
    #[serde(default = "default_truncation_marker")]
    pub marker: String,
}

impl Default for PresentationTruncationSpec {
    fn default() -> Self {
        Self {
            position: PresentationTruncationPosition::Right,
            marker: default_truncation_marker(),
        }
    }
}
#[derive(Debug, Clone, Copy, PartialEq, Eq, Deserialize, Default)]
#[serde(rename_all = "kebab-case")]
pub enum PresentationTruncationPosition {
    Left,
    Middle,
    #[default]
    Right,
}

#[derive(Debug, Clone, PartialEq, Eq, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct PresentationOutlinePathSpec {
    #[serde(default = "default_outline_separator")]
    pub separator: String,
    #[serde(default)]
    pub include_root: bool,
    #[serde(default = "default_true")]
    pub include_match: bool,
}

impl Default for PresentationOutlinePathSpec {
    fn default() -> Self {
        Self {
            separator: default_outline_separator(),
            include_root: false,
            include_match: true,
        }
    }
}

fn default_truncation_marker() -> String {
    DEFAULT_TRUNCATION_MARKER.to_string()
}

fn default_outline_separator() -> String {
    DEFAULT_OUTLINE_SEPARATOR.to_string()
}

fn default_true() -> bool {
    true
}

#[derive(Debug)]
pub enum PresentationSpecError {
    Json(serde_json::Error),
    Invalid(String),
}

impl fmt::Display for PresentationSpecError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Json(source) => write!(
                f,
                "failed to parse presentation specification JSON: {source}"
            ),
            Self::Invalid(message) => write!(f, "invalid presentation specification: {message}"),
        }
    }
}

impl Error for PresentationSpecError {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        match self {
            Self::Json(source) => Some(source),
            Self::Invalid(_) => None,
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::query::result::{FilePathEntry, FileRef, HeadingPathEntry, HeadingRef};
    use crate::query::{
        EffectivePropertyFact, FileResultNode, HeadingResultNode, KeywordFact, LinkResultNode,
        LinkTarget, Location, PathEntry, QueryInclude, QueryResultKind, QueryResultNode,
        QueryTarget,
    };

    use super::{
        presentation_role_index, PresentationCell, PresentationColumn, PresentationResponse,
        PresentationResultKind, PresentationRole, PresentationRoleRule, PresentationRow,
        PresentationRowContext, PresentationRowSourceKind, PresentationSortDirection,
        PresentationSpec, PresentationTruncationPosition, PresentationValue,
        PresentationValueSource, PresentationWidthMode, PRESENTATION_ROLE_VALUES,
        PRESENTATION_VERSION,
    };

    fn file_result(id: i64, title: &str) -> QueryResultNode {
        QueryResultNode::File(FileResultNode {
            kind: QueryResultKind::File,
            matched: true,
            id,
            level: 0,
            path: format!("/notes/{title}.org"),
            name: format!("{title}.org"),
            dir: "/notes".to_string(),
            title: title.to_string(),
            title_raw: Some(title.to_string()),
            root_heading_id: id,
            mtime_ns: 0,
            size: 0,
            content_hash: None,
            indexed_at: None,
            location: Location {
                file_path: format!("/notes/{title}.org"),
                line: Some(1),
                byte_start: None,
                byte_end: None,
            },
            tags: Vec::new(),
            node_path: None,
            properties: None,
            effective_properties: None,
            keywords: None,
            links: None,
            backlinks: None,
            children: None,
        })
    }

    fn heading_result() -> QueryResultNode {
        QueryResultNode::Heading(HeadingResultNode {
            kind: QueryResultKind::Heading,
            matched: true,
            id: 12,
            file_id: 1,
            parent_id: Some(11),
            level: 2,
            title: "Task".to_string(),
            title_raw: Some("Task".to_string()),
            todo_keyword: Some("TODO".to_string()),
            todo_type: Some("open".to_string()),
            priority: Some("A".to_string()),
            scheduled_raw: Some("<2026-08-17 Mon>".to_string()),
            scheduled_ts: None,
            deadline_raw: None,
            deadline_ts: None,
            closed_raw: None,
            closed_ts: None,
            archivedp: false,
            footnote_section_p: false,
            all_tags: vec!["project".to_string(), "emacs".to_string()],
            location: Location {
                file_path: "/notes/notes.org".to_string(),
                line: Some(12),
                byte_start: Some(100),
                byte_end: Some(120),
            },
            node_path: Some(vec![
                PathEntry::File(FilePathEntry {
                    id: 1,
                    path: "/notes/notes.org".to_string(),
                    title: "Notes".to_string(),
                    title_raw: Some("Notes".to_string()),
                }),
                PathEntry::Heading(HeadingPathEntry {
                    id: 11,
                    title: "Parent".to_string(),
                    title_raw: "Parent".to_string(),
                    level: 1,
                }),
                PathEntry::Heading(HeadingPathEntry {
                    id: 12,
                    title: "Task".to_string(),
                    title_raw: "Task".to_string(),
                    level: 2,
                }),
            ]),
            properties: None,
            effective_properties: None,
            keywords: None,
            links: None,
            backlinks: None,
            children: None,
        })
    }

    fn heading_result_with(
        id: i64,
        title: &str,
        priority: Option<&str>,
        line: i64,
    ) -> QueryResultNode {
        let mut result = heading_result();
        let QueryResultNode::Heading(node) = &mut result else {
            unreachable!("heading_result should return a heading");
        };
        node.id = id;
        node.title = title.to_string();
        node.title_raw = Some(title.to_string());
        node.priority = priority.map(str::to_string);
        node.location.line = Some(line);
        if let Some(PathEntry::Heading(path_entry)) =
            node.node_path.as_mut().and_then(|path| path.last_mut())
        {
            path_entry.id = id;
            path_entry.title = title.to_string();
            path_entry.title_raw = title.to_string();
        }
        result
    }

    fn empty_row(result_index: usize) -> PresentationRow {
        PresentationRow {
            result_index,
            row_context: None,
            cells: Vec::new(),
        }
    }

    fn link_result() -> QueryResultNode {
        QueryResultNode::Link(Box::new(LinkResultNode {
            kind: QueryResultKind::Link,
            matched: true,
            id: 21,
            file_id: 1,
            heading_id: 12,
            heading_level: 2,
            source_context: "heading".to_string(),
            format: "bracket".to_string(),
            link_type: "file".to_string(),
            raw: "[[file:target.org::*Target][Go]]".to_string(),
            raw_target: "file:target.org::*Target".to_string(),
            raw_description: Some("Go".to_string()),
            link_path: "target.org".to_string(),
            search_option: Some("*Target".to_string()),
            path_absolute: Some("/notes/target.org".to_string()),
            target_file_id: Some(2),
            target_heading_id: Some(22),
            target_custom_id: None,
            target_id: None,
            resolution_status: Some("resolved".to_string()),
            resolution_diagnostic: None,
            location: Location {
                file_path: "/notes/notes.org".to_string(),
                line: Some(12),
                byte_start: Some(100),
                byte_end: Some(140),
            },
            node_path: Some(vec![
                PathEntry::File(FilePathEntry {
                    id: 1,
                    path: "/notes/notes.org".to_string(),
                    title: "Notes".to_string(),
                    title_raw: Some("Notes".to_string()),
                }),
                PathEntry::Heading(HeadingPathEntry {
                    id: 12,
                    title: "Task".to_string(),
                    title_raw: "Task".to_string(),
                    level: 2,
                }),
            ]),
            source: None,
            target: Some(LinkTarget {
                resolved_kind: Some(QueryTarget::Headings),
                file: Some(FileRef {
                    id: 2,
                    path: "/notes/target.org".to_string(),
                    title: "Target File".to_string(),
                    title_raw: Some("Target File".to_string()),
                }),
                heading: Some(HeadingRef {
                    id: 22,
                    title: "Target".to_string(),
                    title_raw: "Target".to_string(),
                    level: 2,
                    outline_path: vec!["Section".to_string(), "Target".to_string()],
                }),
                raw_target: "file:target.org::*Target".to_string(),
                resolution_status: Some("resolved".to_string()),
                resolution_diagnostic: None,
            }),
        }))
    }

    #[test]
    fn wire_response_separates_results_rows_and_visible_cells() {
        let response = PresentationResponse::new(
            "00000000-0000-4000-8000-000000000001",
            42,
            vec![file_result(7, "project")],
            vec![PresentationRow {
                result_index: 0,
                row_context: None,
                cells: vec![PresentationCell {
                    search_text: "A complete value that stays searchable".to_string(),
                    display_text: "A complete value…".to_string(),
                    role: Some(PresentationRole::Title),
                }],
            }],
        );

        let value =
            serde_json::to_value(&response).expect("presentation response should serialize");

        assert_eq!(value["presentation_version"], PRESENTATION_VERSION);
        assert_eq!(value["database_id"], "00000000-0000-4000-8000-000000000001");
        assert_eq!(value["generation"], 42);
        assert_eq!(value["results"].as_array().map(Vec::len), Some(1));
        assert_eq!(value["rows"].as_array().map(Vec::len), Some(1));
        assert_eq!(
            value["schemas"]["row_fields"],
            serde_json::json!(["result_index", "row_context", "cells"])
        );
        assert_eq!(
            value["schemas"]["cell_fields"],
            serde_json::json!(["search_text", "display_text", "role"])
        );
        assert_eq!(value["schemas"]["display_text_null"], "same-as-search_text");
        assert_eq!(
            value["schemas"]["role_encoding"],
            "null-or-index-into-role_values"
        );
        assert_eq!(value["rows"][0][0], 0);
        assert!(value["rows"][0][1].is_null());
        assert_eq!(
            value["rows"][0][2][0][0],
            "A complete value that stays searchable"
        );
        assert_eq!(value["rows"][0][2][0][1], "A complete value…");
        assert_eq!(value["rows"][0][2][0][2], 1);
        assert_eq!(value["schemas"]["role_values"][1], "title");
    }

    #[test]
    fn several_rows_can_reference_one_result_with_structured_contexts() {
        let response = PresentationResponse::new(
            "00000000-0000-4000-8000-000000000001",
            9,
            vec![file_result(11, "notes")],
            vec![
                PresentationRow {
                    result_index: 0,
                    row_context: Some(PresentationRowContext::Tag {
                        value: "project".to_string(),
                    }),
                    cells: Vec::new(),
                },
                PresentationRow {
                    result_index: 0,
                    row_context: Some(PresentationRowContext::EffectiveProperty {
                        name: "OWNER".to_string(),
                        value: "Daniel".to_string(),
                    }),
                    cells: Vec::new(),
                },
                PresentationRow {
                    result_index: 0,
                    row_context: Some(PresentationRowContext::Keyword {
                        name: "TITLE".to_string(),
                        value: "Notes".to_string(),
                    }),
                    cells: Vec::new(),
                },
            ],
        );

        let value =
            serde_json::to_value(&response).expect("presentation response should serialize");

        assert_eq!(value["results"].as_array().map(Vec::len), Some(1));
        assert_eq!(value["rows"].as_array().map(Vec::len), Some(3));
        assert!(value["rows"]
            .as_array()
            .expect("rows should be an array")
            .iter()
            .all(|row| row[0] == 0));
        assert_eq!(value["rows"][0][1], serde_json::json!(["tag", "project"]));
        assert_eq!(
            value["rows"][1][1],
            serde_json::json!(["effective-property", "OWNER", "Daniel"])
        );
        assert_eq!(
            value["rows"][2][1],
            serde_json::json!(["keyword", "TITLE", "Notes"])
        );
    }

    #[test]
    fn wire_response_uses_null_display_sentinel_for_equal_text() {
        let response = PresentationResponse::new(
            "00000000-0000-4000-8000-000000000001",
            1,
            vec![file_result(1, "notes")],
            vec![PresentationRow {
                result_index: 0,
                row_context: None,
                cells: vec![PresentationCell {
                    search_text: "same".to_string(),
                    display_text: "same".to_string(),
                    role: None,
                }],
            }],
        );

        let value =
            serde_json::to_value(&response).expect("presentation response should serialize");

        assert_eq!(
            value["rows"][0][2][0],
            serde_json::json!(["same", null, null])
        );
    }

    #[test]
    fn wire_role_indexes_match_the_emitted_role_table() {
        let roles = [
            PresentationRole::Heading,
            PresentationRole::Title,
            PresentationRole::Todo,
            PresentationRole::Done,
            PresentationRole::Priority,
            PresentationRole::Tag,
            PresentationRole::Date,
            PresentationRole::FileName,
            PresentationRole::FilePath,
            PresentationRole::KeywordName,
            PresentationRole::KeywordValue,
            PresentationRole::PropertyName,
            PresentationRole::PropertyValue,
        ];

        for role in roles {
            assert_eq!(
                PRESENTATION_ROLE_VALUES[usize::from(presentation_role_index(role))],
                role.as_str()
            );
        }
    }

    #[test]
    fn cell_role_can_be_null() {
        let cell = PresentationCell {
            search_text: "open".to_string(),
            display_text: "open".to_string(),
            role: None,
        };

        let value = serde_json::to_value(&cell).expect("presentation cell should serialize");
        assert!(value["role"].is_null());
    }

    #[test]
    fn minimal_spec_uses_deterministic_defaults() {
        let spec = PresentationSpec::parse_json(r#"{"columns":[{"name":"title"}]}"#)
            .expect("minimal presentation spec should parse");

        assert_eq!(spec.columns.len(), 1);
        assert_eq!(spec.columns[0].name, PresentationColumn::Title);
        assert_eq!(spec.columns[0].width.mode, PresentationWidthMode::Auto);
        assert_eq!(spec.columns[0].width.value, None);
        assert_eq!(spec.columns[0].truncate, None);
        assert_eq!(spec.columns[0].outline_path, None);
        assert!(spec.sort.is_empty());
        assert_eq!(spec.row_source, None);
    }

    #[test]
    fn complete_spec_parses_all_initial_options() {
        let spec = PresentationSpec::parse_json(
            r#"{
                "columns": [
                    {
                        "name": "outline-path",
                        "width": {"mode": "max", "value": 80},
                        "truncate": {"position": "middle", "marker": "..."},
                        "outline_path": {
                            "separator": " / ",
                            "include_root": true,
                            "include_match": false
                        }
                    }
                ],
                "sort": [{"column": "title", "direction": "desc"}],
                "row_source": {"kind": "effective-properties"}
            }"#,
        )
        .expect("complete presentation spec should parse");

        let column = &spec.columns[0];
        assert_eq!(column.name, PresentationColumn::OutlinePath);
        assert_eq!(column.width.mode, PresentationWidthMode::Max);
        assert_eq!(column.width.value, Some(80));
        let truncation = column.truncate.as_ref().expect("truncation should exist");
        assert_eq!(truncation.position, PresentationTruncationPosition::Middle);
        assert_eq!(truncation.marker, "...");
        let outline = column
            .outline_path
            .as_ref()
            .expect("outline-path options should exist");
        assert_eq!(outline.separator, " / ");
        assert!(outline.include_root);
        assert!(!outline.include_match);
        assert_eq!(spec.sort[0].column, PresentationColumn::Title);
        assert_eq!(spec.sort[0].direction, PresentationSortDirection::Desc);
        assert_eq!(
            spec.row_source.map(|source| source.kind),
            Some(PresentationRowSourceKind::EffectiveProperties)
        );
    }

    #[test]
    fn nested_option_objects_use_deterministic_defaults() {
        let spec = PresentationSpec::parse_json(
            r#"{
                "columns": [
                    {
                        "name": "outline-path",
                        "truncate": {},
                        "outline_path": {}
                    }
                ],
                "sort": [{"column": "title"}]
            }"#,
        )
        .expect("defaulted nested options should parse");

        let column = &spec.columns[0];
        let truncation = column.truncate.as_ref().expect("truncation should exist");
        assert_eq!(truncation.position, PresentationTruncationPosition::Right);
        assert_eq!(truncation.marker, "…");
        let outline = column
            .outline_path
            .as_ref()
            .expect("outline-path options should exist");
        assert_eq!(outline.separator, " » ");
        assert!(!outline.include_root);
        assert!(outline.include_match);
        assert_eq!(spec.sort[0].direction, PresentationSortDirection::Asc);
    }

    #[test]
    fn public_column_names_parse_and_internal_names_are_rejected() {
        for name in [
            "title",
            "todo-keyword",
            "todo-type",
            "priority",
            "outline-path",
            "tags",
            "scheduled-raw",
            "deadline-raw",
            "closed-raw",
            "file-title",
            "file-name",
            "file-path",
            "line-number",
            "link-type",
            "link-target",
            "link-description",
            "resolution-status",
            "source-outline-path",
            "target-outline-path",
            "rank",
        ] {
            let input = format!(r#"{{"columns":[{{"name":"{name}"}}]}}"#);
            PresentationSpec::parse_json(&input).expect("public presentation column should parse");
        }

        for name in [
            "byte-start",
            "byte-end",
            "file-id",
            "parent-id",
            "link-path",
            "raw-target",
            "raw-description",
            "unknown",
            "",
        ] {
            let input = format!(r#"{{"columns":[{{"name":"{name}"}}]}}"#);
            let error = PresentationSpec::parse_json(&input)
                .expect_err("non-public presentation column should fail");
            assert!(error
                .to_string()
                .starts_with("failed to parse presentation specification JSON:"));
        }
    }

    #[test]
    fn registry_matches_public_result_contexts() {
        for column in [
            PresentationColumn::Title,
            PresentationColumn::TodoKeyword,
            PresentationColumn::TodoType,
            PresentationColumn::Priority,
            PresentationColumn::OutlinePath,
            PresentationColumn::Tags,
            PresentationColumn::ScheduledRaw,
            PresentationColumn::DeadlineRaw,
            PresentationColumn::ClosedRaw,
            PresentationColumn::FileTitle,
            PresentationColumn::FileName,
            PresentationColumn::FilePath,
            PresentationColumn::LineNumber,
        ] {
            assert!(column
                .definition()
                .supports(PresentationResultKind::Heading));
        }

        for column in [
            PresentationColumn::FileTitle,
            PresentationColumn::FileName,
            PresentationColumn::FilePath,
            PresentationColumn::Tags,
            PresentationColumn::LineNumber,
        ] {
            assert!(column.definition().supports(PresentationResultKind::File));
        }

        for column in [
            PresentationColumn::LinkType,
            PresentationColumn::LinkTarget,
            PresentationColumn::LinkDescription,
            PresentationColumn::ResolutionStatus,
            PresentationColumn::SourceOutlinePath,
            PresentationColumn::TargetOutlinePath,
            PresentationColumn::FileName,
            PresentationColumn::FilePath,
            PresentationColumn::LineNumber,
        ] {
            assert!(column.definition().supports(PresentationResultKind::Link));
        }

        for column in [
            PresentationColumn::Title,
            PresentationColumn::OutlinePath,
            PresentationColumn::FileName,
            PresentationColumn::FilePath,
            PresentationColumn::LineNumber,
            PresentationColumn::Rank,
        ] {
            assert!(column.definition().supports(PresentationResultKind::Search));
        }
    }

    #[test]
    fn registry_carries_value_role_and_option_metadata() {
        let todo = PresentationColumn::TodoKeyword.definition();
        assert_eq!(todo.value_source, PresentationValueSource::TodoKeyword);
        assert_eq!(todo.role_rule, PresentationRoleRule::TodoKeyword);
        assert!(!todo.options.outline_path);

        let outline = PresentationColumn::OutlinePath.definition();
        assert_eq!(outline.value_source, PresentationValueSource::OutlinePath);
        assert_eq!(
            outline.role_rule,
            PresentationRoleRule::Static(PresentationRole::Heading)
        );
        assert!(outline.options.outline_path);

        let file_path = PresentationColumn::FilePath.definition();
        assert_eq!(file_path.value_source, PresentationValueSource::FilePath);
        assert_eq!(
            file_path.role_rule,
            PresentationRoleRule::Static(PresentationRole::FilePath)
        );
    }

    #[test]
    fn semantic_roles_have_stable_wire_names() {
        for (role, name) in [
            (PresentationRole::Heading, "heading"),
            (PresentationRole::Title, "title"),
            (PresentationRole::Todo, "todo"),
            (PresentationRole::Done, "done"),
            (PresentationRole::Priority, "priority"),
            (PresentationRole::Tag, "tag"),
            (PresentationRole::Date, "date"),
            (PresentationRole::FileName, "file-name"),
            (PresentationRole::FilePath, "file-path"),
            (PresentationRole::KeywordName, "keyword-name"),
            (PresentationRole::KeywordValue, "keyword-value"),
            (PresentationRole::PropertyName, "property-name"),
            (PresentationRole::PropertyValue, "property-value"),
        ] {
            assert_eq!(role.as_str(), name);
            assert_eq!(
                serde_json::to_string(&role).expect("presentation role should serialize"),
                format!(r#""{name}""#)
            );
        }
    }

    #[test]
    fn todo_keyword_role_uses_open_and_closed_semantics() {
        let rule = PresentationColumn::TodoKeyword.definition().role_rule;

        assert_eq!(rule.resolve(Some("open")), Some(PresentationRole::Todo));
        assert_eq!(rule.resolve(Some("closed")), Some(PresentationRole::Done));
        assert_eq!(rule.resolve(None), None);
        assert_eq!(rule.resolve(Some("unknown")), None);
    }

    #[test]
    fn outline_paths_use_one_heading_role_without_level_metadata() {
        for column in [
            PresentationColumn::OutlinePath,
            PresentationColumn::SourceOutlinePath,
            PresentationColumn::TargetOutlinePath,
        ] {
            assert_eq!(
                column.definition().role_rule.resolve(None),
                Some(PresentationRole::Heading)
            );
        }
    }

    #[test]
    fn columns_without_distinct_display_semantics_have_no_role() {
        for column in [
            PresentationColumn::TodoType,
            PresentationColumn::LineNumber,
            PresentationColumn::LinkType,
            PresentationColumn::LinkTarget,
            PresentationColumn::LinkDescription,
            PresentationColumn::ResolutionStatus,
            PresentationColumn::Rank,
        ] {
            assert_eq!(column.definition().role_rule.resolve(None), None);
        }
    }

    #[test]
    fn columns_and_sort_rules_validate_against_the_query_target() {
        let headings = PresentationSpec::parse_json(
            r#"{"columns":[{"name":"title"}],"sort":[{"column":"priority"}]}"#,
        )
        .expect("heading columns should parse");
        headings
            .validate_for_query_target(QueryTarget::Headings)
            .expect("heading columns should validate for headings");

        let invalid_display =
            PresentationSpec::parse_json(r#"{"columns":[{"name":"link-target"}]}"#)
                .expect("known link column should parse");
        assert_eq!(
            invalid_display
                .validate_for_query_target(QueryTarget::Headings)
                .expect_err("link column should fail for headings")
                .to_string(),
            "invalid presentation specification: columns[0].name `link-target` is not supported for heading results"
        );

        let invalid_sort = PresentationSpec::parse_json(
            r#"{"columns":[{"name":"file-name"}],"sort":[{"column":"rank"}]}"#,
        )
        .expect("known search sort column should parse");
        assert_eq!(
            invalid_sort
                .validate_for_query_target(QueryTarget::Files)
                .expect_err("search sort column should fail for files")
                .to_string(),
            "invalid presentation specification: sort[0].column `rank` is not supported for file results"
        );
    }

    #[test]
    fn include_requirements_come_from_the_same_registry() {
        let headings = PresentationSpec::parse_json(
            r#"{
                "columns":[{"name":"outline-path"},{"name":"file-title"}],
                "sort":[{"column":"outline-path"}]
            }"#,
        )
        .expect("heading presentation should parse");
        assert_eq!(
            headings
                .required_includes_for_query_target(QueryTarget::Headings)
                .expect("heading includes should derive"),
            vec![QueryInclude::Path]
        );

        let heading_file_title =
            PresentationSpec::parse_json(r#"{"columns":[{"name":"file-title"}]}"#)
                .expect("heading file-title presentation should parse");
        assert_eq!(
            heading_file_title
                .required_includes_for_query_target(QueryTarget::Headings)
                .expect("heading file-title include should derive"),
            vec![QueryInclude::Path]
        );
        assert!(heading_file_title
            .required_includes_for_query_target(QueryTarget::Files)
            .expect("file file-title includes should derive")
            .is_empty());

        let links = PresentationSpec::parse_json(
            r#"{
                "columns":[{"name":"source-outline-path"}],
                "sort":[{"column":"target-outline-path"}]
            }"#,
        )
        .expect("link presentation should parse");
        assert_eq!(
            links
                .required_includes_for_query_target(QueryTarget::Links)
                .expect("link includes should derive"),
            vec![QueryInclude::Path, QueryInclude::Target]
        );

        let properties = PresentationSpec::parse_json(
            r#"{
                "columns":[{"name":"property-name"},{"name":"file-name"}],
                "row_source":{"kind":"effective-properties"}
            }"#,
        )
        .expect("property row presentation should parse");
        assert_eq!(
            properties
                .required_includes_for_query_target(QueryTarget::Files)
                .expect("property includes should derive"),
            vec![QueryInclude::EffectiveProperties]
        );

        let tags = PresentationSpec::parse_json(
            r#"{
                "columns":[{"name":"tag"}],
                "row_source":{"kind":"tags"}
            }"#,
        )
        .expect("tag row presentation should parse");
        assert!(tags
            .required_includes_for_query_target(QueryTarget::Headings)
            .expect("tag includes should derive")
            .is_empty());

        let keywords = PresentationSpec::parse_json(
            r#"{
                "columns":[{"name":"file-name"}],
                "row_source":{"kind":"keywords"}
            }"#,
        )
        .expect("keyword row presentation should parse");
        assert_eq!(
            keywords
                .required_includes_for_query_target(QueryTarget::Files)
                .expect("keyword includes should derive"),
            vec![QueryInclude::Keywords]
        );
    }

    #[test]
    fn row_source_and_hidden_sort_includes_are_inferred_and_combined() {
        let spec = PresentationSpec::parse_json(
            r#"{
                "columns":[{"name":"file-name"}],
                "sort":[{"column":"outline-path"}],
                "row_source":{"kind":"effective-properties"}
            }"#,
        )
        .expect("presentation specification should parse");

        assert_eq!(
            spec.required_includes_for_query_target(QueryTarget::Headings)
                .expect("presentation includes should derive"),
            vec![QueryInclude::Path, QueryInclude::EffectiveProperties]
        );
        assert_eq!(
            spec.combined_includes_for_query_target(
                QueryTarget::Headings,
                &[QueryInclude::Links, QueryInclude::Path]
            )
            .expect("explicit and inferred includes should combine"),
            vec![
                QueryInclude::Links,
                QueryInclude::Path,
                QueryInclude::EffectiveProperties
            ]
        );
    }

    #[test]
    fn heading_values_use_structured_result_data_and_outline_options() {
        let result = heading_result();
        let spec = PresentationSpec::parse_json(
            r#"{
                "columns":[
                    {"name":"title"},
                    {"name":"todo-keyword"},
                    {"name":"tags"},
                    {"name":"file-title"},
                    {"name":"file-name"},
                    {"name":"line-number"},
                    {
                        "name":"outline-path",
                        "outline_path":{
                            "separator":" / ",
                            "include_root":true,
                            "include_match":false
                        }
                    }
                ]
            }"#,
        )
        .expect("heading presentation should parse");

        let title = spec
            .extract_value(PresentationColumn::Title, &result, None)
            .expect("title should extract");
        assert_eq!(title.search_text(), "Task");
        assert_eq!(title.role, Some(PresentationRole::Title));

        let todo = spec
            .extract_value(PresentationColumn::TodoKeyword, &result, None)
            .expect("todo keyword should extract");
        assert_eq!(todo.search_text(), "TODO");
        assert_eq!(todo.role, Some(PresentationRole::Todo));

        let tags = spec
            .extract_value(PresentationColumn::Tags, &result, None)
            .expect("tags should extract");
        assert_eq!(
            &tags.value,
            &PresentationValue::TextList(vec!["project".to_string(), "emacs".to_string()])
        );
        assert_eq!(tags.search_text(), "project,emacs");

        assert_eq!(
            spec.extract_value(PresentationColumn::FileTitle, &result, None)
                .expect("file title should extract")
                .search_text(),
            "Notes"
        );
        assert_eq!(
            spec.extract_value(PresentationColumn::FileName, &result, None)
                .expect("file name should extract")
                .search_text(),
            "notes.org"
        );
        assert_eq!(
            spec.extract_value(PresentationColumn::LineNumber, &result, None)
                .expect("line number should extract")
                .value,
            PresentationValue::Integer(12)
        );
        assert_eq!(
            spec.extract_value(PresentationColumn::OutlinePath, &result, None)
                .expect("outline path should extract")
                .search_text(),
            "Notes / Parent"
        );
    }

    #[test]
    fn link_outline_values_use_inferred_path_and_target_shapes() {
        let result = link_result();
        let spec = PresentationSpec::parse_json(
            r#"{
                "columns":[
                    {"name":"link-target"},
                    {"name":"link-description"},
                    {"name":"source-outline-path"},
                    {
                        "name":"target-outline-path",
                        "outline_path":{
                            "separator":" / ",
                            "include_root":true,
                            "include_match":false
                        }
                    }
                ]
            }"#,
        )
        .expect("link presentation should parse");

        assert_eq!(
            spec.extract_value(PresentationColumn::LinkTarget, &result, None)
                .expect("link target should extract")
                .search_text(),
            "file:target.org::*Target"
        );
        assert_eq!(
            spec.extract_value(PresentationColumn::LinkDescription, &result, None)
                .expect("link description should extract")
                .search_text(),
            "Go"
        );
        assert_eq!(
            spec.extract_value(PresentationColumn::SourceOutlinePath, &result, None)
                .expect("source outline should extract")
                .search_text(),
            "Task"
        );
        assert_eq!(
            spec.extract_value(PresentationColumn::TargetOutlinePath, &result, None)
                .expect("target outline should extract")
                .search_text(),
            "Target File / Section"
        );
    }

    #[test]
    fn row_context_values_use_the_reserved_column_registry() {
        let result = file_result(7, "notes");
        let spec = PresentationSpec::parse_json(
            r#"{
                "columns":[{"name":"property-name"},{"name":"property-value"}],
                "row_source":{"kind":"effective-properties"}
            }"#,
        )
        .expect("property presentation should parse");
        let context = PresentationRowContext::EffectiveProperty {
            name: "OWNER".to_string(),
            value: "Daniel".to_string(),
        };

        assert_eq!(
            spec.extract_value(PresentationColumn::PropertyName, &result, Some(&context))
                .expect("property name should extract")
                .search_text(),
            "OWNER"
        );
        assert_eq!(
            spec.extract_value(PresentationColumn::PropertyValue, &result, Some(&context))
                .expect("property value should extract")
                .search_text(),
            "Daniel"
        );
    }

    #[test]
    fn extraction_reports_missing_inferred_data_instead_of_hiding_it() {
        let mut result = heading_result();
        let QueryResultNode::Heading(node) = &mut result else {
            panic!("expected heading result");
        };
        node.node_path = None;
        let spec = PresentationSpec::parse_json(r#"{"columns":[{"name":"outline-path"}]}"#)
            .expect("outline presentation should parse");

        let error = spec
            .extract_value(PresentationColumn::OutlinePath, &result, None)
            .expect_err("missing path include data should fail");
        assert_eq!(
            error.to_string(),
            "presentation column `outline-path` requires query include `path`"
        );
    }

    #[test]
    fn sorting_supports_ascending_descending_and_hidden_columns() {
        let results = vec![
            file_result(1, "Zulu"),
            file_result(2, "Alpha"),
            file_result(3, "Middle"),
        ];
        let rows = vec![empty_row(0), empty_row(1), empty_row(2)];

        let ascending = PresentationSpec::parse_json(
            r#"{
                "columns":[{"name":"file-name"}],
                "sort":[{"column":"file-title","direction":"asc"}]
            }"#,
        )
        .expect("ascending file-title sort should parse");
        let ascending_rows = ascending
            .sort_rows(&results, rows.clone())
            .expect("ascending rows should sort");
        assert_eq!(
            ascending_rows
                .iter()
                .map(|row| row.result_index)
                .collect::<Vec<_>>(),
            vec![1, 2, 0]
        );

        let descending = PresentationSpec::parse_json(
            r#"{
                "columns":[{"name":"file-name"}],
                "sort":[{"column":"file-title","direction":"desc"}]
            }"#,
        )
        .expect("descending file-title sort should parse");
        let descending_rows = descending
            .sort_rows(&results, rows)
            .expect("descending rows should sort");
        assert_eq!(
            descending_rows
                .iter()
                .map(|row| row.result_index)
                .collect::<Vec<_>>(),
            vec![0, 2, 1]
        );
    }

    #[test]
    fn sorting_applies_multiple_typed_keys_in_declared_order() {
        let results = vec![
            heading_result_with(20, "Ten", Some("A"), 10),
            heading_result_with(21, "Two", Some("A"), 2),
            heading_result_with(22, "One", Some("B"), 1),
        ];
        let spec = PresentationSpec::parse_json(
            r#"{
                "columns":[{"name":"title"}],
                "sort":[
                    {"column":"priority","direction":"asc"},
                    {"column":"line-number","direction":"asc"}
                ]
            }"#,
        )
        .expect("multi-key heading sort should parse");

        let rows = spec
            .sort_rows(&results, vec![empty_row(0), empty_row(1), empty_row(2)])
            .expect("multi-key rows should sort");
        assert_eq!(
            rows.iter().map(|row| row.result_index).collect::<Vec<_>>(),
            vec![1, 0, 2]
        );
    }

    #[test]
    fn sorting_keeps_missing_values_last_and_equal_values_in_input_order() {
        let results = vec![
            heading_result_with(20, "First missing", None, 1),
            heading_result_with(21, "B", Some("B"), 2),
            heading_result_with(22, "Second missing", None, 3),
            heading_result_with(23, "A", Some("A"), 4),
        ];
        let input_rows = vec![empty_row(2), empty_row(0), empty_row(1), empty_row(3)];

        let ascending = PresentationSpec::parse_json(
            r#"{
                "columns":[{"name":"title"}],
                "sort":[{"column":"priority","direction":"asc"}]
            }"#,
        )
        .expect("ascending priority sort should parse");
        let ascending_rows = ascending
            .sort_rows(&results, input_rows.clone())
            .expect("ascending rows should sort");
        assert_eq!(
            ascending_rows
                .iter()
                .map(|row| row.result_index)
                .collect::<Vec<_>>(),
            vec![3, 1, 2, 0]
        );

        let descending = PresentationSpec::parse_json(
            r#"{
                "columns":[{"name":"title"}],
                "sort":[{"column":"priority","direction":"desc"}]
            }"#,
        )
        .expect("descending priority sort should parse");
        let descending_rows = descending
            .sort_rows(&results, input_rows)
            .expect("descending rows should sort");
        assert_eq!(
            descending_rows
                .iter()
                .map(|row| row.result_index)
                .collect::<Vec<_>>(),
            vec![1, 3, 2, 0]
        );
    }

    #[test]
    fn sorting_can_use_row_context_for_multiple_rows_of_one_result() {
        let results = vec![file_result(7, "notes")];
        let spec = PresentationSpec::parse_json(
            r#"{
                "columns":[{"name":"property-value"}],
                "sort":[{"column":"property-name"}],
                "row_source":{"kind":"effective-properties"}
            }"#,
        )
        .expect("property row sort should parse");
        let rows = vec![
            PresentationRow {
                result_index: 0,
                row_context: Some(PresentationRowContext::EffectiveProperty {
                    name: "ZETA".to_string(),
                    value: "last".to_string(),
                }),
                cells: Vec::new(),
            },
            PresentationRow {
                result_index: 0,
                row_context: Some(PresentationRowContext::EffectiveProperty {
                    name: "ALPHA".to_string(),
                    value: "first".to_string(),
                }),
                cells: Vec::new(),
            },
        ];

        let rows = spec
            .sort_rows(&results, rows)
            .expect("property rows should sort");
        assert_eq!(
            rows[0].row_context,
            Some(PresentationRowContext::EffectiveProperty {
                name: "ALPHA".to_string(),
                value: "first".to_string(),
            })
        );
        assert_eq!(
            rows[1].row_context,
            Some(PresentationRowContext::EffectiveProperty {
                name: "ZETA".to_string(),
                value: "last".to_string(),
            })
        );
    }

    #[test]
    fn sorting_reports_missing_result_references() {
        let spec = PresentationSpec::parse_json(
            r#"{
                "columns":[{"name":"file-name"}],
                "sort":[{"column":"file-title"}]
            }"#,
        )
        .expect("file sort should parse");

        let error = spec
            .sort_rows(&[file_result(1, "notes")], vec![empty_row(4)])
            .expect_err("missing result reference should fail");
        assert_eq!(
            error.to_string(),
            "presentation row 0 references missing result_index 4"
        );
    }

    #[test]
    fn layout_auto_uses_one_shared_width_and_preserves_search_text() {
        let results = vec![file_result(1, "A"), file_result(2, "Longer")];
        let spec = PresentationSpec::parse_json(r#"{"columns":[{"name":"title"}]}"#)
            .expect("auto-width presentation should parse");

        let rows = spec
            .layout_rows(&results, vec![empty_row(0), empty_row(1)])
            .expect("auto-width rows should layout");

        assert_eq!(rows[0].cells[0].search_text, "A");
        assert_eq!(rows[0].cells[0].display_text, "A     ");
        assert_eq!(rows[1].cells[0].search_text, "Longer");
        assert_eq!(rows[1].cells[0].display_text, "Longer");
    }

    #[test]
    fn layout_max_width_limits_shared_width_and_counts_marker() {
        let results = vec![file_result(1, "abcdefgh"), file_result(2, "xy")];
        let spec = PresentationSpec::parse_json(
            r#"{
                "columns":[{
                    "name":"title",
                    "width":{"mode":"max","value":4},
                    "truncate":{"position":"right","marker":".."}
                }]
            }"#,
        )
        .expect("max-width presentation should parse");

        let rows = spec
            .layout_rows(&results, vec![empty_row(0), empty_row(1)])
            .expect("max-width rows should layout");

        assert_eq!(rows[0].cells[0].search_text, "abcdefgh");
        assert_eq!(rows[0].cells[0].display_text, "ab..");
        assert_eq!(rows[1].cells[0].display_text, "xy  ");
    }

    #[test]
    fn layout_max_width_stays_at_natural_width_below_limit() {
        let results = vec![file_result(1, "xy")];
        let spec = PresentationSpec::parse_json(
            r#"{"columns":[{"name":"title","width":{"mode":"max","value":8}}]}"#,
        )
        .expect("max-width presentation should parse");

        let rows = spec
            .layout_rows(&results, vec![empty_row(0)])
            .expect("max-width row should layout");

        assert_eq!(rows[0].cells[0].display_text, "xy");
    }

    #[test]
    fn layout_fixed_width_supports_left_middle_and_right_truncation() {
        let results = vec![file_result(1, "abcdefgh")];
        let spec = PresentationSpec::parse_json(
            r#"{
                "columns":[
                    {
                        "name":"title",
                        "width":{"mode":"fixed","value":5},
                        "truncate":{"position":"left"}
                    },
                    {
                        "name":"title",
                        "width":{"mode":"fixed","value":5},
                        "truncate":{"position":"middle"}
                    },
                    {
                        "name":"title",
                        "width":{"mode":"fixed","value":5},
                        "truncate":{"position":"right"}
                    }
                ]
            }"#,
        )
        .expect("fixed-width presentation should parse");

        let rows = spec
            .layout_rows(&results, vec![empty_row(0)])
            .expect("fixed-width row should layout");
        let cells = &rows[0].cells;

        assert_eq!(cells[0].display_text, "…efgh");
        assert_eq!(cells[1].display_text, "ab…gh");
        assert_eq!(cells[2].display_text, "abcd…");
        assert!(cells.iter().all(|cell| cell.search_text == "abcdefgh"));
    }

    #[test]
    fn layout_fixed_width_pads_short_values() {
        let results = vec![file_result(1, "abc")];
        let spec = PresentationSpec::parse_json(
            r#"{"columns":[{"name":"title","width":{"mode":"fixed","value":5}}]}"#,
        )
        .expect("fixed-width presentation should parse");

        let rows = spec
            .layout_rows(&results, vec![empty_row(0)])
            .expect("fixed-width row should layout");

        assert_eq!(rows[0].cells[0].display_text, "abc  ");
    }

    #[test]
    fn layout_empty_marker_hard_truncates_non_ascii_text() {
        let results = vec![file_result(1, "åäöé")];
        let spec = PresentationSpec::parse_json(
            r#"{
                "columns":[{
                    "name":"title",
                    "width":{"mode":"fixed","value":3},
                    "truncate":{"position":"right","marker":""}
                }]
            }"#,
        )
        .expect("hard-truncation presentation should parse");

        let rows = spec
            .layout_rows(&results, vec![empty_row(0)])
            .expect("non-ASCII row should layout");
        let cell = &rows[0].cells[0];

        assert_eq!(cell.search_text, "åäöé");
        assert_eq!(cell.display_text, "åäö");
        assert_eq!(cell.display_text.chars().count(), 3);
    }

    #[test]
    fn layout_default_truncation_is_unicode_safe() {
        let results = vec![file_result(1, "naïve")];
        let spec = PresentationSpec::parse_json(
            r#"{"columns":[{"name":"title","width":{"mode":"fixed","value":4}}]}"#,
        )
        .expect("default truncation presentation should parse");

        let rows = spec
            .layout_rows(&results, vec![empty_row(0)])
            .expect("Unicode row should layout");
        let cell = &rows[0].cells[0];

        assert_eq!(cell.search_text, "naïve");
        assert_eq!(cell.display_text, "naï…");
        assert_eq!(cell.display_text.chars().count(), 4);
    }

    #[test]
    fn layout_truncates_marker_when_marker_exceeds_column_width() {
        let results = vec![file_result(1, "abcdef")];
        let spec = PresentationSpec::parse_json(
            r#"{
                "columns":[{
                    "name":"title",
                    "width":{"mode":"fixed","value":2},
                    "truncate":{"marker":"..."}
                }]
            }"#,
        )
        .expect("wide-marker presentation should parse");

        let rows = spec
            .layout_rows(&results, vec![empty_row(0)])
            .expect("wide-marker row should layout");

        assert_eq!(rows[0].cells[0].display_text, "..");
    }

    #[test]
    fn layout_reports_missing_result_references() {
        let spec = PresentationSpec::parse_json(r#"{"columns":[{"name":"title"}]}"#)
            .expect("presentation should parse");

        let error = spec
            .layout_rows(&[file_result(1, "notes")], vec![empty_row(3)])
            .expect_err("missing result reference should fail");
        assert_eq!(
            error.to_string(),
            "presentation row 0 references missing result_index 3"
        );
    }

    #[test]
    fn tag_row_source_expands_rows_in_source_order_and_uses_shared_widths() {
        let mut result = file_result(1, "notes");
        let QueryResultNode::File(node) = &mut result else {
            unreachable!("file_result should return a file");
        };
        node.tags = vec!["x".to_string(), "project".to_string()];
        let original = result.clone();
        let spec = PresentationSpec::parse_json(
            r#"{
                "columns":[{"name":"file-name"},{"name":"tag"}],
                "row_source":{"kind":"tags"}
            }"#,
        )
        .expect("tag presentation should parse");

        let response = spec
            .build_response("database-id", 7, vec![result])
            .expect("tag rows should build");

        assert_eq!(response.results, vec![original]);
        assert_eq!(response.rows.len(), 2);
        assert!(response.rows.iter().all(|row| row.result_index == 0));
        assert_eq!(
            response.rows[0].row_context,
            Some(PresentationRowContext::Tag {
                value: "x".to_string()
            })
        );
        assert_eq!(
            response.rows[1].row_context,
            Some(PresentationRowContext::Tag {
                value: "project".to_string()
            })
        );
        assert_eq!(response.rows[0].cells[0].search_text, "notes.org");
        assert_eq!(response.rows[1].cells[0].search_text, "notes.org");
        assert_eq!(response.rows[0].cells[1].search_text, "x");
        assert_eq!(response.rows[0].cells[1].display_text, "x      ");
        assert_eq!(response.rows[1].cells[1].display_text, "project");
        assert_eq!(response.rows[0].cells[1].role, Some(PresentationRole::Tag));
    }

    #[test]
    fn empty_row_source_produces_no_rows() {
        let result = file_result(1, "notes");
        let spec = PresentationSpec::parse_json(
            r#"{
                "columns":[{"name":"file-name"},{"name":"tag"}],
                "row_source":{"kind":"tags"}
            }"#,
        )
        .expect("tag presentation should parse");

        let response = spec
            .build_response("database-id", 7, vec![result.clone()])
            .expect("empty tag source should build");

        assert_eq!(response.results, vec![result]);
        assert!(response.rows.is_empty());
    }

    #[test]
    fn effective_property_rows_preserve_source_order_and_repeat_normal_columns() {
        let mut result = heading_result();
        let QueryResultNode::Heading(node) = &mut result else {
            unreachable!("heading_result should return a heading");
        };
        node.effective_properties = Some(vec![
            EffectivePropertyFact {
                key: "OWNER".to_string(),
                value: Some("Daniel".to_string()),
            },
            EffectivePropertyFact {
                key: "AREA".to_string(),
                value: Some("infra".to_string()),
            },
        ]);
        let spec = PresentationSpec::parse_json(
            r#"{
                "columns":[
                    {"name":"title"},
                    {"name":"property-name"},
                    {"name":"property-value"}
                ],
                "row_source":{"kind":"effective-properties"}
            }"#,
        )
        .expect("property presentation should parse");

        let response = spec
            .build_response("database-id", 7, vec![result])
            .expect("property rows should build");

        assert_eq!(response.rows.len(), 2);
        assert_eq!(response.rows[0].cells[0].search_text, "Task");
        assert_eq!(response.rows[1].cells[0].search_text, "Task");
        assert_eq!(response.rows[0].cells[1].search_text, "OWNER");
        assert_eq!(response.rows[1].cells[1].search_text, "AREA");
        assert_eq!(response.rows[1].cells[1].display_text, "AREA ");
        assert_eq!(response.rows[0].cells[2].search_text, "Daniel");
        assert_eq!(response.rows[1].cells[2].search_text, "infra");
        assert_eq!(response.rows[1].cells[2].display_text, "infra ");
    }

    #[test]
    fn keyword_rows_preserve_duplicates_and_absent_values() {
        let mut result = file_result(1, "notes");
        let QueryResultNode::File(node) = &mut result else {
            unreachable!("file_result should return a file");
        };
        node.keywords = Some(vec![
            KeywordFact {
                keyword: "TITLE".to_string(),
                value: Some("First".to_string()),
                line_number: Some(1),
            },
            KeywordFact {
                keyword: "TITLE".to_string(),
                value: Some("Second".to_string()),
                line_number: Some(2),
            },
            KeywordFact {
                keyword: "EMPTY".to_string(),
                value: None,
                line_number: Some(3),
            },
        ]);
        let spec = PresentationSpec::parse_json(
            r#"{
                "columns":[{"name":"keyword-name"},{"name":"keyword-value"}],
                "row_source":{"kind":"keywords"}
            }"#,
        )
        .expect("keyword presentation should parse");

        let response = spec
            .build_response("database-id", 7, vec![result])
            .expect("keyword rows should build");

        assert_eq!(response.rows.len(), 3);
        assert_eq!(
            response.rows[0].row_context,
            Some(PresentationRowContext::Keyword {
                name: "TITLE".to_string(),
                value: "First".to_string(),
            })
        );
        assert_eq!(
            response.rows[1].row_context,
            Some(PresentationRowContext::Keyword {
                name: "TITLE".to_string(),
                value: "Second".to_string(),
            })
        );
        assert_eq!(response.rows[2].cells[1].search_text, "");
    }

    #[test]
    fn hidden_row_column_sorts_expanded_rows_before_layout() {
        let mut result = file_result(1, "notes");
        let QueryResultNode::File(node) = &mut result else {
            unreachable!("file_result should return a file");
        };
        node.tags = vec!["zeta".to_string(), "alpha".to_string()];
        let spec = PresentationSpec::parse_json(
            r#"{
                "columns":[{"name":"file-name"}],
                "sort":[{"column":"tag","direction":"asc"}],
                "row_source":{"kind":"tags"}
            }"#,
        )
        .expect("hidden tag sort should parse");

        let response = spec
            .build_response("database-id", 7, vec![result])
            .expect("expanded rows should sort");

        assert_eq!(
            response
                .rows
                .iter()
                .map(|row| row.row_context.clone())
                .collect::<Vec<_>>(),
            vec![
                Some(PresentationRowContext::Tag {
                    value: "alpha".to_string()
                }),
                Some(PresentationRowContext::Tag {
                    value: "zeta".to_string()
                }),
            ]
        );
        assert!(response.rows.iter().all(|row| row.cells.len() == 1));
    }

    #[test]
    fn row_expansion_reports_missing_inferred_include_data() {
        let result = file_result(1, "notes");
        let spec = PresentationSpec::parse_json(
            r#"{
                "columns":[{"name":"property-name"}],
                "row_source":{"kind":"effective-properties"}
            }"#,
        )
        .expect("property presentation should parse");

        let error = spec
            .build_response("database-id", 7, vec![result])
            .expect_err("missing effective property data should fail");
        assert_eq!(
            error.to_string(),
            "failed to expand presentation rows: row_source kind `effective-properties` requires query include `effective_properties` for result_index 0"
        );
    }

    #[test]
    fn build_response_runs_sort_and_layout_without_duplicating_results() {
        let results = vec![file_result(1, "Alpha"), file_result(2, "Long title")];
        let spec = PresentationSpec::parse_json(
            r#"{
                "columns":[{
                    "name":"title",
                    "width":{"mode":"fixed","value":6}
                }],
                "sort":[{"column":"title","direction":"desc"}]
            }"#,
        )
        .expect("presentation should parse");

        let response = spec
            .build_response("database-id", 7, results.clone())
            .expect("presentation response should build");

        assert_eq!(response.database_id, "database-id");
        assert_eq!(response.generation, 7);
        assert_eq!(response.results, results);
        assert_eq!(response.rows.len(), 2);
        assert_eq!(response.rows[0].result_index, 1);
        assert_eq!(response.rows[1].result_index, 0);
        assert_eq!(response.rows[0].row_context, None);
        assert_eq!(response.rows[0].cells[0].search_text, "Long title");
        assert_eq!(response.rows[0].cells[0].display_text, "Long …");
        assert_eq!(
            response.rows[0].cells[0].role,
            Some(PresentationRole::Title)
        );
    }

    #[test]
    fn row_sources_are_limited_to_heading_and_file_results() {
        let spec = PresentationSpec::parse_json(
            r#"{
                "columns":[{"name":"link-target"}],
                "row_source":{"kind":"tags"}
            }"#,
        )
        .expect("known row source should parse");

        assert_eq!(
            spec.validate_for_query_target(QueryTarget::Links)
                .expect_err("link results should reject row expansion")
                .to_string(),
            "invalid presentation specification: row_source kind `tags` is not supported for link results"
        );
    }

    #[test]
    fn outline_path_options_are_limited_to_outline_columns() {
        let error = PresentationSpec::parse_json(
            r#"{"columns":[{"name":"title","outline_path":{"include_root":true}}]}"#,
        )
        .expect_err("title should reject outline-path options");
        assert_eq!(
            error.to_string(),
            "invalid presentation specification: columns[0].outline_path is not supported by column `title`"
        );

        for name in ["outline-path", "source-outline-path", "target-outline-path"] {
            let input = format!(
                r#"{{"columns":[{{"name":"{name}","outline_path":{{"include_root":true}}}}]}}"#
            );
            PresentationSpec::parse_json(&input)
                .expect("outline-path column should accept outline options");
        }
    }

    #[test]
    fn reserved_row_columns_require_the_matching_row_source() {
        for (column, row_source) in [
            ("tag", "tags"),
            ("property-name", "effective-properties"),
            ("property-value", "effective-properties"),
            ("keyword-name", "keywords"),
            ("keyword-value", "keywords"),
        ] {
            let input = format!(
                r#"{{"columns":[{{"name":"{column}"}}],"row_source":{{"kind":"{row_source}"}}}}"#
            );
            PresentationSpec::parse_json(&input)
                .expect("reserved row column should accept matching row source");
        }

        let error = PresentationSpec::parse_json(r#"{"columns":[{"name":"tag"}]}"#)
            .expect_err("tag should require tag row source");
        assert_eq!(
            error.to_string(),
            "invalid presentation specification: columns[0].name `tag` requires row_source kind `tags`"
        );

        let error = PresentationSpec::parse_json(
            r#"{
                "columns":[{"name":"file-name"}],
                "sort":[{"column":"keyword-name"}],
                "row_source":{"kind":"tags"}
            }"#,
        )
        .expect_err("keyword sort should require keyword row source");
        assert_eq!(
            error.to_string(),
            "invalid presentation specification: sort[0].column `keyword-name` requires row_source kind `keywords`"
        );
    }

    #[test]
    fn all_supported_row_sources_parse() {
        for (value, expected) in [
            ("tags", PresentationRowSourceKind::Tags),
            (
                "effective-properties",
                PresentationRowSourceKind::EffectiveProperties,
            ),
            ("keywords", PresentationRowSourceKind::Keywords),
        ] {
            let input =
                format!(r#"{{"columns":[{{"name":"title"}}],"row_source":{{"kind":"{value}"}}}}"#);
            let spec = PresentationSpec::parse_json(&input)
                .expect("recognized presentation row source should parse");
            assert_eq!(spec.row_source.map(|source| source.kind), Some(expected));
        }
    }

    #[test]
    fn unknown_fields_and_values_are_rejected() {
        for input in [
            r#"{"columns":[{"name":"title"}],"unknown":true}"#,
            r#"{"columns":[{"name":"title","unknown":true}]}"#,
            r#"{"columns":[{"name":"unknown"}]}"#,
            r#"{"columns":[{"name":"title","width":{"mode":"percent","value":50}}]}"#,
            r#"{"columns":[{"name":"title","truncate":{"position":"end"}}]}"#,
            r#"{"columns":[{"name":"title"}],"sort":[{"column":"title","direction":"up"}]}"#,
            r#"{"columns":[{"name":"title"}],"sort":[{"column":"unknown"}]}"#,
            r#"{"columns":[{"name":"title"}],"row_source":"tags"}"#,
            r#"{"columns":[{"name":"title"}],"row_source":{"kind":"properties"}}"#,
            r#"{"columns":[{"name":"title"}],"row_source":{"kind":"tags","unknown":true}}"#,
            r#"{"columns":[{"name":"title"}],"row_source":{"kind":"tags"},"row_source":{"kind":"keywords"}}"#,
        ] {
            let error = PresentationSpec::parse_json(input)
                .expect_err("unknown presentation input should fail");
            assert!(error
                .to_string()
                .starts_with("failed to parse presentation specification JSON:"));
        }
    }

    #[test]
    fn invalid_width_shapes_have_clear_errors() {
        for (input, expected) in [
            (
                r#"{"columns":[{"name":"title","width":{"mode":"auto","value":10}}]}"#,
                "columns[0].width mode auto does not accept value",
            ),
            (
                r#"{"columns":[{"name":"title","width":{"mode":"max"}}]}"#,
                "columns[0].width mode max requires value",
            ),
            (
                r#"{"columns":[{"name":"title","width":{"mode":"fixed","value":0}}]}"#,
                "columns[0].width value must be greater than zero",
            ),
        ] {
            let error = PresentationSpec::parse_json(input)
                .expect_err("invalid presentation width should fail");
            assert_eq!(
                error.to_string(),
                format!("invalid presentation specification: {expected}")
            );
        }
    }

    #[test]
    fn empty_columns_are_rejected() {
        let error = PresentationSpec::parse_json(r#"{"columns":[]}"#)
            .expect_err("empty presentation columns should fail");
        assert_eq!(
            error.to_string(),
            "invalid presentation specification: columns must contain at least one column"
        );
    }
}
