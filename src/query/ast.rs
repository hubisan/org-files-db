use serde::Serialize;

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct QueryAst {
    pub target: QueryTarget,
    pub predicate: Option<Expr>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum QueryTarget {
    Headings,
    Links,
    Files,
    FsFiles,
    FsDirs,
}

impl QueryTarget {
    /// The target name used in the query language and diagnostics.
    pub const fn as_str(self) -> &'static str {
        match self {
            Self::Headings => "headings",
            Self::Links => "links",
            Self::Files => "files",
            Self::FsFiles => "fs-files",
            Self::FsDirs => "fs-dirs",
        }
    }

    pub const fn is_filesystem(self) -> bool {
        matches!(self, Self::FsFiles | Self::FsDirs)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub enum Expr {
    And(Vec<Expr>),
    Or(Vec<Expr>),
    Not(Box<Expr>),
    Call(PredicateCall),
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct PredicateCall {
    pub name: String,
    pub args: Vec<PredicateArg>,
    pub options: Vec<QueryOption>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub enum PredicateArg {
    Scalar(QueryValue),
    NestedQuery(Box<QueryAst>),
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub enum QueryValue {
    String(String),
    Integer(i64),
    Symbol(String),
    Keyword(String),
    Bool(bool),
    TemporalBounds(TemporalBounds),
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct TemporalBounds {
    pub start: i64,
    pub exclusive_end: i64,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct QueryOption {
    pub name: String,
    pub value: QueryValue,
}
