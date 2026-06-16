use std::path::PathBuf;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DiagnosticSeverity {
    Warning,
    Error,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ParseDiagnostic {
    pub severity: DiagnosticSeverity,
    pub message: String,
    pub file_path: Option<PathBuf>,
    pub line_number: Option<u32>,
    pub byte_range: Option<(usize, usize)>,
}

impl ParseDiagnostic {
    pub fn warning(message: impl Into<String>) -> Self {
        Self {
            severity: DiagnosticSeverity::Warning,
            message: message.into(),
            file_path: None,
            line_number: None,
            byte_range: None,
        }
    }

    pub fn error(message: impl Into<String>) -> Self {
        Self {
            severity: DiagnosticSeverity::Error,
            message: message.into(),
            file_path: None,
            line_number: None,
            byte_range: None,
        }
    }

    pub fn with_file_path(mut self, file_path: impl Into<PathBuf>) -> Self {
        self.file_path = Some(file_path.into());
        self
    }

    pub fn with_line_number(mut self, line_number: u32) -> Self {
        self.line_number = Some(line_number);
        self
    }

    pub fn with_byte_range(mut self, start: usize, end: usize) -> Self {
        self.byte_range = Some((start, end));
        self
    }
}
