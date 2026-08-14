use std::{error::Error, fmt};

use serde::Deserialize;

const DEFAULT_TRUNCATION_MARKER: &str = "…";
const DEFAULT_OUTLINE_SEPARATOR: &str = " » ";

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
            if column.name.trim().is_empty() {
                return Err(PresentationSpecError::Invalid(format!(
                    "columns[{index}].name must not be empty"
                )));
            }
            column.width.validate(index)?;
        }

        for (index, sort) in self.sort.iter().enumerate() {
            if sort.column.trim().is_empty() {
                return Err(PresentationSpecError::Invalid(format!(
                    "sort[{index}].column must not be empty"
                )));
            }
        }

        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct PresentationColumnSpec {
    pub name: String,
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
    pub column: String,
    #[serde(default)]
    pub direction: PresentationSortDirection,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Deserialize)]
#[serde(rename_all = "kebab-case")]
#[derive(Default)]
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

#[derive(Debug, Clone, Copy, PartialEq, Eq, Deserialize)]
#[serde(rename_all = "kebab-case")]
#[derive(Default)]
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

#[derive(Debug, Clone, Copy, PartialEq, Eq, Deserialize)]
#[serde(rename_all = "kebab-case")]
#[derive(Default)]
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
    use super::{
        PresentationRowSourceKind, PresentationSortDirection, PresentationSpec,
        PresentationTruncationPosition, PresentationWidthMode,
    };

    #[test]
    fn minimal_spec_uses_deterministic_defaults() {
        let spec = PresentationSpec::parse_json(r#"{"columns":[{"name":"title"}]}"#)
            .expect("minimal presentation spec should parse");

        assert_eq!(spec.columns.len(), 1);
        assert_eq!(spec.columns[0].name, "title");
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
            r#"{"columns":[{"name":"title","width":{"mode":"percent","value":50}}]}"#,
            r#"{"columns":[{"name":"title","truncate":{"position":"end"}}]}"#,
            r#"{"columns":[{"name":"title"}],"sort":[{"column":"title","direction":"up"}]}"#,
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
    fn empty_columns_and_names_are_rejected() {
        for (input, expected) in [
            (
                r#"{"columns":[]}"#,
                "columns must contain at least one column",
            ),
            (
                r#"{"columns":[{"name":""}]}"#,
                "columns[0].name must not be empty",
            ),
            (
                r#"{"columns":[{"name":"title"}],"sort":[{"column":""}]}"#,
                "sort[0].column must not be empty",
            ),
        ] {
            let error = PresentationSpec::parse_json(input)
                .expect_err("invalid empty presentation field should fail");
            assert_eq!(
                error.to_string(),
                format!("invalid presentation specification: {expected}")
            );
        }
    }
}
