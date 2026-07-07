use std::fmt;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum QueryParseErrorKind {
    Lexical,
    UnexpectedEof,
    UnexpectedToken,
    InvalidTopLevelForm,
    InvalidTargetForm,
    MissingExplicitTargetWrapper,
    InvalidBooleanForm,
    InvalidOptionSyntax,
    InvalidPredicateForm,
    StructuralArity,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct QueryParseError {
    pub kind: QueryParseErrorKind,
    pub message: String,
    pub byte_range: Option<(usize, usize)>,
}

impl QueryParseError {
    pub fn new(
        kind: QueryParseErrorKind,
        message: impl Into<String>,
        byte_range: Option<(usize, usize)>,
    ) -> Self {
        Self {
            kind,
            message: message.into(),
            byte_range,
        }
    }
}

impl fmt::Display for QueryParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some((start, end)) = self.byte_range {
            write!(f, "{} at bytes {}..{}", self.message, start, end)
        } else {
            write!(f, "{}", self.message)
        }
    }
}

impl std::error::Error for QueryParseError {}
