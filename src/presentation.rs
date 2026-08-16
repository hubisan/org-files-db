use std::{error::Error, fmt};

use serde::Deserialize;

use crate::query::{QueryInclude, QueryTarget};

const DEFAULT_TRUNCATION_MARKER: &str = "…";
const DEFAULT_OUTLINE_SEPARATOR: &str = " » ";

const NO_INCLUDES: &[QueryInclude] = &[];
const PATH_INCLUDE: &[QueryInclude] = &[QueryInclude::Path];
const TARGET_INCLUDE: &[QueryInclude] = &[QueryInclude::Target];
const EFFECTIVE_PROPERTIES_INCLUDE: &[QueryInclude] = &[QueryInclude::EffectiveProperties];
const KEYWORDS_INCLUDE: &[QueryInclude] = &[QueryInclude::Keywords];

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
            for include in column.definition().required_includes(result_kind) {
                if !includes.contains(include) {
                    includes.push(*include);
                }
            }
        }
        Ok(includes)
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
                Role::Static("title"),
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
                Role::Static("priority"),
                common,
                None,
            ),
            Column::OutlinePath => PresentationColumnDefinition::new(
                HEADING_AND_SEARCH_RESULTS,
                PresentationIncludeRule::ForKind(PresentationResultKind::Heading, PATH_INCLUDE),
                Value::OutlinePath,
                Role::Static("heading"),
                outline,
                None,
            ),
            Column::Tags => PresentationColumnDefinition::new(
                HEADING_AND_FILE_RESULTS,
                PresentationIncludeRule::None,
                Value::Tags,
                Role::Static("tag"),
                common,
                None,
            ),
            Column::ScheduledRaw => PresentationColumnDefinition::new(
                HEADING_RESULTS,
                PresentationIncludeRule::None,
                Value::ScheduledRaw,
                Role::Static("date"),
                common,
                None,
            ),
            Column::DeadlineRaw => PresentationColumnDefinition::new(
                HEADING_RESULTS,
                PresentationIncludeRule::None,
                Value::DeadlineRaw,
                Role::Static("date"),
                common,
                None,
            ),
            Column::ClosedRaw => PresentationColumnDefinition::new(
                HEADING_RESULTS,
                PresentationIncludeRule::None,
                Value::ClosedRaw,
                Role::Static("date"),
                common,
                None,
            ),
            Column::FileTitle => PresentationColumnDefinition::new(
                HEADING_AND_FILE_RESULTS,
                PresentationIncludeRule::ForKind(PresentationResultKind::Heading, PATH_INCLUDE),
                Value::FileTitle,
                Role::Static("title"),
                common,
                None,
            ),
            Column::FileName => PresentationColumnDefinition::new(
                FILE_LOCATION_RESULTS,
                PresentationIncludeRule::None,
                Value::FileName,
                Role::Static("file-name"),
                common,
                None,
            ),
            Column::FilePath => PresentationColumnDefinition::new(
                FILE_LOCATION_RESULTS,
                PresentationIncludeRule::None,
                Value::FilePath,
                Role::Static("file-path"),
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
                Role::Static("heading"),
                outline,
                None,
            ),
            Column::TargetOutlinePath => PresentationColumnDefinition::new(
                LINK_RESULTS,
                PresentationIncludeRule::Always(TARGET_INCLUDE),
                Value::TargetOutlinePath,
                Role::Static("heading"),
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
                Role::Static("tag"),
                common,
                Some(PresentationRowSourceKind::Tags),
            ),
            Column::PropertyName => PresentationColumnDefinition::new(
                HEADING_AND_FILE_RESULTS,
                PresentationIncludeRule::Always(EFFECTIVE_PROPERTIES_INCLUDE),
                Value::RowPropertyName,
                Role::Static("property-name"),
                common,
                Some(PresentationRowSourceKind::EffectiveProperties),
            ),
            Column::PropertyValue => PresentationColumnDefinition::new(
                HEADING_AND_FILE_RESULTS,
                PresentationIncludeRule::Always(EFFECTIVE_PROPERTIES_INCLUDE),
                Value::RowPropertyValue,
                Role::Static("property-value"),
                common,
                Some(PresentationRowSourceKind::EffectiveProperties),
            ),
            Column::KeywordName => PresentationColumnDefinition::new(
                HEADING_AND_FILE_RESULTS,
                PresentationIncludeRule::Always(KEYWORDS_INCLUDE),
                Value::RowKeywordName,
                Role::Static("keyword-name"),
                common,
                Some(PresentationRowSourceKind::Keywords),
            ),
            Column::KeywordValue => PresentationColumnDefinition::new(
                HEADING_AND_FILE_RESULTS,
                PresentationIncludeRule::Always(KEYWORDS_INCLUDE),
                Value::RowKeywordValue,
                Role::Static("keyword-value"),
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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PresentationRoleRule {
    None,
    Static(&'static str),
    TodoKeyword,
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
    use crate::query::{QueryInclude, QueryTarget};

    use super::{
        PresentationColumn, PresentationResultKind, PresentationRoleRule,
        PresentationRowSourceKind, PresentationSortDirection, PresentationSpec,
        PresentationTruncationPosition, PresentationValueSource, PresentationWidthMode,
    };

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
        assert_eq!(outline.role_rule, PresentationRoleRule::Static("heading"));
        assert!(outline.options.outline_path);

        let file_path = PresentationColumn::FilePath.definition();
        assert_eq!(file_path.value_source, PresentationValueSource::FilePath);
        assert_eq!(
            file_path.role_rule,
            PresentationRoleRule::Static("file-path")
        );
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
    fn all_planned_row_sources_parse() {
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
