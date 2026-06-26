use std::{
    fs,
    path::{Path, PathBuf},
};

use crate::{
    config::{DirLocalsConfig, DirLocalsUnsupportedPolicy},
    parser::TodoKeywordConfig,
    todo_keywords::parse_todo_keyword_spec,
};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DirLocalsResolver {
    config: DirLocalsConfig,
}

impl DirLocalsResolver {
    pub fn new(config: DirLocalsConfig) -> Self {
        Self { config }
    }

    pub fn resolve_for_file(
        &self,
        file_path: &Path,
        scan_root: &Path,
    ) -> Result<DirLocalsResolution, DirLocalsError> {
        if !self.config.enabled {
            return Ok(DirLocalsResolution::default());
        }

        let Some(dir_locals_path) = find_dir_locals_path(file_path, scan_root, self.config.inherit)
        else {
            return Ok(DirLocalsResolution::default());
        };

        let content =
            fs::read_to_string(&dir_locals_path).map_err(|source| DirLocalsError::ReadFile {
                path: dir_locals_path.clone(),
                source,
            })?;
        // Structural parse failures are still fatal for the source file, but
        // targeted TODO extraction failures are reported separately as
        // diagnostics. Under warn/ignore we continue with config defaults and
        // treat the .dir-locals source as unusable for this file.
        let parsed = match parse_dir_locals(&content) {
            Ok(parsed) => parsed,
            Err(message) => match self.config.unsupported {
                DirLocalsUnsupportedPolicy::Error => {
                    return Err(DirLocalsError::Unsupported {
                        path: dir_locals_path.clone(),
                        message,
                    });
                }
                DirLocalsUnsupportedPolicy::Warn => {
                    return Ok(DirLocalsResolution {
                        todo_keywords: None,
                        source_path: Some(dir_locals_path.clone()),
                        diagnostics: vec![DirLocalsDiagnostic {
                            path: dir_locals_path,
                            message,
                        }],
                    });
                }
                DirLocalsUnsupportedPolicy::Ignore => {
                    return Ok(DirLocalsResolution {
                        todo_keywords: None,
                        source_path: Some(dir_locals_path),
                        diagnostics: Vec::new(),
                    });
                }
            },
        };

        if !parsed.diagnostics.is_empty()
            && matches!(self.config.unsupported, DirLocalsUnsupportedPolicy::Error)
        {
            return Err(DirLocalsError::Unsupported {
                path: dir_locals_path.clone(),
                message: parsed.diagnostics.join("; "),
            });
        }

        let diagnostics = if matches!(self.config.unsupported, DirLocalsUnsupportedPolicy::Warn) {
            parsed
                .diagnostics
                .into_iter()
                .map(|message| DirLocalsDiagnostic {
                    path: dir_locals_path.clone(),
                    message,
                })
                .collect()
        } else {
            Vec::new()
        };

        Ok(DirLocalsResolution {
            todo_keywords: parsed.todo_keywords,
            source_path: Some(dir_locals_path),
            diagnostics,
        })
    }
}

#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct DirLocalsResolution {
    pub todo_keywords: Option<TodoKeywordConfig>,
    pub source_path: Option<PathBuf>,
    pub diagnostics: Vec<DirLocalsDiagnostic>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DirLocalsDiagnostic {
    pub path: PathBuf,
    pub message: String,
}

#[derive(Debug)]
pub enum DirLocalsError {
    ReadFile {
        path: PathBuf,
        source: std::io::Error,
    },
    Unsupported {
        path: PathBuf,
        message: String,
    },
}

impl std::fmt::Display for DirLocalsError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::ReadFile { path, source } => {
                write!(
                    f,
                    "failed to read .dir-locals file {}: {}",
                    path.display(),
                    source
                )
            }
            Self::Unsupported { path, message } => {
                write!(
                    f,
                    "unsupported .dir-locals file {}: {}",
                    path.display(),
                    message
                )
            }
        }
    }
}

impl std::error::Error for DirLocalsError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self {
            Self::ReadFile { source, .. } => Some(source),
            Self::Unsupported { .. } => None,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct ParsedDirLocals {
    todo_keywords: Option<TodoKeywordConfig>,
    diagnostics: Vec<String>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum Expr {
    Symbol(String),
    String(String),
    List(Vec<Expr>),
    DottedPair(Box<Expr>, Box<Expr>),
    ReaderSyntax(Box<Expr>),
}

fn find_dir_locals_path(file_path: &Path, scan_root: &Path, inherit: bool) -> Option<PathBuf> {
    let mut current = file_path.parent()?;
    if !current.starts_with(scan_root) {
        return None;
    }

    loop {
        let candidate = current.join(".dir-locals.el");
        if candidate.is_file() {
            return Some(candidate);
        }

        if !inherit || current == scan_root {
            return None;
        }

        current = current.parent()?;
        if !current.starts_with(scan_root) {
            return None;
        }
    }
}

fn parse_dir_locals(content: &str) -> Result<ParsedDirLocals, String> {
    let expr = Parser::new(content).parse()?;
    extract_todo_keywords(expr)
}

fn extract_todo_keywords(expr: Expr) -> Result<ParsedDirLocals, String> {
    let Expr::List(entries) = expr else {
        return Err("top-level .dir-locals form must be a list".to_string());
    };

    let mut nil_keywords = None;
    let mut org_mode_keywords = None;
    let mut diagnostics = Vec::new();

    for entry in entries {
        let Expr::DottedPair(mode, vars) = entry else {
            continue;
        };

        let Some(mode_name) = symbol_name(&mode) else {
            continue;
        };

        if mode_name != "nil" && mode_name != "org-mode" {
            continue;
        }

        let Some(variable_entries) = list_items(&vars) else {
            continue;
        };

        let mut mode_todo_keywords = None;
        for variable_entry in variable_entries {
            let Expr::DottedPair(variable, value) = variable_entry else {
                continue;
            };

            let Some(variable_name) = symbol_name(variable) else {
                continue;
            };

            if variable_name == "org-todo-keywords" {
                match extract_org_todo_keywords(value.as_ref()) {
                    Ok(Some(config)) => mode_todo_keywords = Some(config),
                    Ok(None) => {}
                    Err(message) => diagnostics.push(message),
                }
            }
        }

        if mode_name == "org-mode" {
            org_mode_keywords = mode_todo_keywords;
        } else if nil_keywords.is_none() {
            nil_keywords = mode_todo_keywords;
        }
    }

    Ok(ParsedDirLocals {
        todo_keywords: org_mode_keywords.or(nil_keywords),
        diagnostics,
    })
}

fn extract_org_todo_keywords(value: &Expr) -> Result<Option<TodoKeywordConfig>, String> {
    if matches!(value, Expr::ReaderSyntax(_)) {
        return Err(unsafe_org_todo_keywords_error(
            "reader syntax is not supported",
        ));
    }

    let Some(forms) = list_items(value) else {
        return Err(unsafe_org_todo_keywords_error(
            "org-todo-keywords must map to a list of forms",
        ));
    };

    let mut open = Vec::new();
    let mut closed = Vec::new();

    for form in forms {
        if matches!(form, Expr::ReaderSyntax(_)) {
            return Err(unsafe_org_todo_keywords_error(
                "reader syntax is not supported",
            ));
        }

        let Some(items) = list_items(form) else {
            return Err(unsafe_org_todo_keywords_error(
                "org-todo-keywords forms must be lists",
            ));
        };

        let Some(Expr::Symbol(kind)) = items.first() else {
            return Err(unsafe_org_todo_keywords_error(
                "org-todo-keywords form must start with a symbol",
            ));
        };

        if kind != "sequence" {
            return Err(unsafe_org_todo_keywords_error(format!(
                "unsupported org-todo-keywords form: {kind}"
            )));
        }

        let sequence = parse_sequence_items(&items[1..]).map_err(unsafe_org_todo_keywords_error)?;
        open.extend(sequence.open);
        closed.extend(sequence.closed);
    }

    if open.is_empty() && closed.is_empty() {
        Ok(None)
    } else {
        Ok(Some(TodoKeywordConfig { open, closed }.deduplicated()))
    }
}

fn parse_sequence_items(items: &[Expr]) -> Result<TodoKeywordConfig, String> {
    let mut open = Vec::new();
    let mut closed = Vec::new();
    let mut seen_separator = false;

    for item in items {
        if matches!(item, Expr::ReaderSyntax(_)) {
            return Err("reader syntax is not supported".to_string());
        }

        let Expr::String(value) = item else {
            return Err("sequence items must be literal strings".to_string());
        };

        if value == "|" {
            if seen_separator {
                return Err("sequence may contain at most one | separator".to_string());
            }
            seen_separator = true;
            continue;
        }

        let keyword = parse_todo_keyword_spec(value);
        if seen_separator {
            closed.push(keyword);
        } else {
            open.push(keyword);
        }
    }

    if !seen_separator {
        let Some(closed_keyword) = open.pop() else {
            return Err("sequence must contain at least one TODO keyword".to_string());
        };
        closed.push(closed_keyword);
    }

    Ok(TodoKeywordConfig { open, closed })
}

fn unsafe_org_todo_keywords_error(message: impl Into<String>) -> String {
    format!("ignored unsafe org-todo-keywords value: {}", message.into())
}

fn list_items(expr: &Expr) -> Option<&[Expr]> {
    match expr {
        Expr::List(items) => Some(items),
        _ => None,
    }
}

fn symbol_name(expr: &Expr) -> Option<&str> {
    match expr {
        Expr::Symbol(value) => Some(value.as_str()),
        _ => None,
    }
}

struct Parser<'a> {
    input: &'a str,
    pos: usize,
}

impl<'a> Parser<'a> {
    fn new(input: &'a str) -> Self {
        Self { input, pos: 0 }
    }

    fn parse(mut self) -> Result<Expr, String> {
        self.skip_whitespace();
        let expr = self.parse_expr()?;
        self.skip_whitespace();
        if self.peek().is_some() {
            return Err("unexpected trailing input".to_string());
        }
        Ok(expr)
    }

    fn parse_expr(&mut self) -> Result<Expr, String> {
        self.skip_whitespace();
        match self.peek() {
            Some('(') => self.parse_list(),
            Some('"') => self.parse_string().map(Expr::String),
            Some('#') => self.parse_reader_syntax(),
            Some('\'') => Err("quote syntax is not supported".to_string()),
            Some('`') => Err("backquote syntax is not supported".to_string()),
            Some(',') => Err("unquote syntax is not supported".to_string()),
            Some(')') => Err("unexpected )".to_string()),
            Some('.') => Err("unexpected .".to_string()),
            Some(_) => self.parse_symbol().map(Expr::Symbol),
            None => Err("unexpected end of input".to_string()),
        }
    }

    fn parse_list(&mut self) -> Result<Expr, String> {
        self.expect('(')?;
        self.skip_whitespace();
        if self.peek() == Some(')') {
            self.expect(')')?;
            return Ok(Expr::List(Vec::new()));
        }

        let first = self.parse_expr()?;
        self.skip_whitespace();
        if self.peek() == Some('.') {
            self.expect('.')?;
            self.skip_whitespace();
            let second = self.parse_expr()?;
            self.skip_whitespace();
            self.expect(')')?;
            return Ok(Expr::DottedPair(Box::new(first), Box::new(second)));
        }

        let mut items = vec![first];
        while self.peek() != Some(')') {
            items.push(self.parse_expr()?);
            self.skip_whitespace();
        }
        self.expect(')')?;
        Ok(Expr::List(items))
    }

    fn parse_reader_syntax(&mut self) -> Result<Expr, String> {
        self.expect('#')?;
        match self.peek() {
            Some('.') => {
                self.bump();
                let expr = self.parse_expr()?;
                Ok(Expr::ReaderSyntax(Box::new(expr)))
            }
            Some(_) => Err("reader syntax is not supported".to_string()),
            None => Err("reader syntax is not supported".to_string()),
        }
    }

    fn parse_string(&mut self) -> Result<String, String> {
        self.expect('"')?;
        let mut value = String::new();
        while let Some(ch) = self.peek() {
            match ch {
                '"' => {
                    self.bump();
                    return Ok(value);
                }
                '\\' => {
                    self.bump();
                    let escaped = self
                        .bump()
                        .ok_or_else(|| "unterminated escape sequence".to_string())?;
                    value.push(match escaped {
                        '"' => '"',
                        '\\' => '\\',
                        'n' => '\n',
                        't' => '\t',
                        other => other,
                    });
                }
                _ => {
                    self.bump();
                    value.push(ch);
                }
            }
        }

        Err("unterminated string".to_string())
    }

    fn parse_symbol(&mut self) -> Result<String, String> {
        let start = self.pos;
        while let Some(ch) = self.peek() {
            if ch.is_whitespace() || matches!(ch, '(' | ')' | '"' | '\'' | '`' | ',') {
                break;
            }
            if ch == ';' {
                return Err("comments are not supported".to_string());
            }
            self.bump();
        }

        if self.pos == start {
            return Err("expected symbol".to_string());
        }

        Ok(self.input[start..self.pos].to_string())
    }

    fn skip_whitespace(&mut self) {
        loop {
            while self.peek().is_some_and(char::is_whitespace) {
                self.bump();
            }

            if self.peek() == Some(';') {
                while let Some(ch) = self.bump() {
                    if ch == '\n' {
                        break;
                    }
                }
                continue;
            }

            break;
        }
    }

    fn expect(&mut self, expected: char) -> Result<(), String> {
        match self.bump() {
            Some(value) if value == expected => Ok(()),
            Some(value) => Err(format!("expected {expected}, found {value}")),
            None => Err(format!("expected {expected}, found end of input")),
        }
    }

    fn peek(&self) -> Option<char> {
        self.input[self.pos..].chars().next()
    }

    fn bump(&mut self) -> Option<char> {
        let ch = self.peek()?;
        self.pos += ch.len_utf8();
        Some(ch)
    }
}

#[cfg(test)]
mod tests {
    use super::{parse_dir_locals, DirLocalsResolver};
    use crate::config::{DirLocalsConfig, DirLocalsUnsupportedPolicy};
    use crate::parser::{TodoKeyword, TodoKeywordConfig};
    use std::{
        fs,
        path::{Path, PathBuf},
        time::{SystemTime, UNIX_EPOCH},
    };

    struct TestDir {
        path: PathBuf,
    }

    impl TestDir {
        fn new(name: &str) -> Self {
            let unique = SystemTime::now()
                .duration_since(UNIX_EPOCH)
                .expect("system time should be after unix epoch")
                .as_nanos();
            let path = std::env::temp_dir().join(format!(
                "org-files-db-dir-locals-tests-{}-{}-{}",
                name,
                std::process::id(),
                unique
            ));
            fs::create_dir_all(&path).expect("test dir should be created");
            Self { path }
        }

        fn path(&self) -> &Path {
            &self.path
        }
    }

    impl Drop for TestDir {
        fn drop(&mut self) {
            let _ = fs::remove_dir_all(&self.path);
        }
    }

    fn write_file(path: &Path, content: &str) {
        if let Some(parent) = path.parent() {
            fs::create_dir_all(parent).expect("parent dir should exist");
        }
        fs::write(path, content).expect("file should be written");
    }

    #[test]
    fn safe_org_mode_example_extracts_todo_keywords() {
        let parsed = parse_dir_locals(
            r#"((org-mode . ((org-todo-keywords . ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)" "CANCEL(c)"))))))"#,
        )
        .expect("dir locals should parse");

        assert_eq!(
            parsed.todo_keywords,
            Some(TodoKeywordConfig {
                open: vec![
                    TodoKeyword::with_fast_key("TODO", 't'),
                    TodoKeyword::with_fast_key("NEXT", 'n'),
                ],
                closed: vec![
                    TodoKeyword::with_fast_key("DONE", 'd'),
                    TodoKeyword::with_fast_key("CANCEL", 'c'),
                ],
            })
        );
        assert!(parsed.diagnostics.is_empty());
    }

    #[test]
    fn safe_nil_mode_example_extracts_todo_keywords() {
        let parsed =
            parse_dir_locals(r#"((nil . ((org-todo-keywords . ((sequence "TODO" "|" "DONE"))))))"#)
                .expect("dir locals should parse");

        assert_eq!(
            parsed.todo_keywords,
            Some(TodoKeywordConfig {
                open: vec![TodoKeyword::new("TODO")],
                closed: vec![TodoKeyword::new("DONE")],
            })
        );
        assert!(parsed.diagnostics.is_empty());
    }

    #[test]
    fn fast_selection_keys_are_preserved() {
        let parsed = parse_dir_locals(
            r#"((org-mode . ((org-todo-keywords . ((sequence "PLAN(p)" "|" "DONE(d)"))))))"#,
        )
        .expect("dir locals should parse");

        assert_eq!(
            parsed.todo_keywords,
            Some(TodoKeywordConfig {
                open: vec![TodoKeyword::with_fast_key("PLAN", 'p')],
                closed: vec![TodoKeyword::with_fast_key("DONE", 'd')],
            })
        );
    }

    #[test]
    fn semicolon_comments_are_ignored_in_safe_subset() {
        let parsed = parse_dir_locals(
            r#"
; top-level comment
((org-mode . (
  ; variable comment
  (org-todo-keywords . ((sequence "TODO" "|" "DONE")))
))) ; trailing comment
"#,
        )
        .expect("dir locals should parse");

        assert_eq!(
            parsed.todo_keywords,
            Some(TodoKeywordConfig {
                open: vec![TodoKeyword::new("TODO")],
                closed: vec![TodoKeyword::new("DONE")],
            })
        );
        assert!(parsed.diagnostics.is_empty());
    }

    #[test]
    fn unsupported_type_form_is_reported() {
        let parsed =
            parse_dir_locals(r#"((org-mode . ((org-todo-keywords . ((type "A" "|" "B"))))))"#)
                .expect("dir locals should parse as data");

        assert_eq!(parsed.todo_keywords, None);
        assert_eq!(
            parsed.diagnostics,
            vec![
                "ignored unsafe org-todo-keywords value: unsupported org-todo-keywords form: type"
                    .to_string()
            ]
        );
    }

    #[test]
    fn eval_variable_is_ignored_without_evaluation() {
        let parsed = parse_dir_locals(
            r#"((org-mode . ((eval . (dangerous-call)) (org-todo-keywords . ((sequence "TODO" "|" "DONE"))))))"#,
        )
        .expect("dir locals should parse as data");

        assert_eq!(
            parsed.todo_keywords,
            Some(TodoKeywordConfig {
                open: vec![TodoKeyword::new("TODO")],
                closed: vec![TodoKeyword::new("DONE")],
            })
        );
        assert!(parsed.diagnostics.is_empty());
    }

    #[test]
    fn unsafe_org_todo_keywords_reader_syntax_is_reported() {
        let parsed = parse_dir_locals(r#"((org-mode . ((org-todo-keywords . #.(boom)))))"#)
            .expect("dir locals should parse as data");

        assert_eq!(parsed.todo_keywords, None);
        assert_eq!(
            parsed.diagnostics,
            vec![
                "ignored unsafe org-todo-keywords value: reader syntax is not supported"
                    .to_string()
            ]
        );
    }

    #[test]
    fn arbitrary_variables_do_not_affect_todo_keywords() {
        let parsed = parse_dir_locals(
            r#"((org-mode . ((org-special . ("ignored")) (org-todo-keywords . ((sequence "TODO" "|" "DONE"))))))"#,
        )
        .expect("dir locals should parse as data");

        assert_eq!(
            parsed.todo_keywords,
            Some(TodoKeywordConfig {
                open: vec![TodoKeyword::new("TODO")],
                closed: vec![TodoKeyword::new("DONE")],
            })
        );
        assert!(parsed.diagnostics.is_empty());
    }

    #[test]
    fn resolver_warns_for_unsafe_org_todo_keywords_values() {
        let test_dir = TestDir::new("resolver-warn");
        let notes_dir = test_dir.path().join("notes");
        let org_path = notes_dir.join("file.org");
        let dir_locals_path = notes_dir.join(".dir-locals.el");
        write_file(&org_path, "* TODO Test\n");
        write_file(
            &dir_locals_path,
            r#"((org-mode . ((org-todo-keywords . #.(boom)) (eval . (danger)))))"#,
        );

        let resolver = DirLocalsResolver::new(DirLocalsConfig {
            enabled: true,
            inherit: true,
            unsupported: DirLocalsUnsupportedPolicy::Warn,
        });
        let resolution = resolver
            .resolve_for_file(&org_path, &notes_dir)
            .expect("warning policy should continue");

        assert_eq!(resolution.source_path, Some(dir_locals_path));
        assert_eq!(resolution.diagnostics.len(), 1);
        assert!(resolution.diagnostics[0]
            .message
            .contains("ignored unsafe org-todo-keywords value: reader syntax is not supported"));
        assert_eq!(resolution.todo_keywords, None);
    }

    #[test]
    fn resolver_warns_and_ignores_unsafe_org_todo_keywords_under_warn_policy() {
        let test_dir = TestDir::new("resolver-unparseable-warn");
        let notes_dir = test_dir.path().join("notes");
        let org_path = notes_dir.join("file.org");
        let dir_locals_path = notes_dir.join(".dir-locals.el");
        write_file(&org_path, "* TODO Test\n");
        write_file(
            &dir_locals_path,
            r#"((org-mode . ((org-todo-keywords . ((sequence "TODO" "|" #.(boom)))))))"#,
        );

        let resolver = DirLocalsResolver::new(DirLocalsConfig {
            enabled: true,
            inherit: true,
            unsupported: DirLocalsUnsupportedPolicy::Warn,
        });
        let resolution = resolver
            .resolve_for_file(&org_path, &notes_dir)
            .expect("warn policy should continue");

        assert_eq!(resolution.source_path, Some(dir_locals_path));
        assert_eq!(resolution.todo_keywords, None);
        assert_eq!(resolution.diagnostics.len(), 1);
        assert!(resolution.diagnostics[0]
            .message
            .contains("ignored unsafe org-todo-keywords value: reader syntax is not supported"));
    }

    #[test]
    fn resolver_ignores_unparseable_input_under_ignore_policy() {
        let test_dir = TestDir::new("resolver-unparseable-ignore");
        let notes_dir = test_dir.path().join("notes");
        let org_path = notes_dir.join("file.org");
        let dir_locals_path = notes_dir.join(".dir-locals.el");
        write_file(&org_path, "* TODO Test\n");
        write_file(&dir_locals_path, "; comment only\n#.(boom)\n");

        let resolver = DirLocalsResolver::new(DirLocalsConfig {
            enabled: true,
            inherit: true,
            unsupported: DirLocalsUnsupportedPolicy::Ignore,
        });
        let resolution = resolver
            .resolve_for_file(&org_path, &notes_dir)
            .expect("ignore policy should continue");

        assert_eq!(resolution.source_path, Some(dir_locals_path));
        assert_eq!(resolution.todo_keywords, None);
        assert!(resolution.diagnostics.is_empty());
    }

    #[test]
    fn resolver_errors_on_unparseable_input_under_error_policy() {
        let test_dir = TestDir::new("resolver-unparseable-error");
        let notes_dir = test_dir.path().join("notes");
        let org_path = notes_dir.join("file.org");
        let dir_locals_path = notes_dir.join(".dir-locals.el");
        write_file(&org_path, "* TODO Test\n");
        write_file(
            &dir_locals_path,
            r#"((org-mode . ((org-todo-keywords . ((sequence "TODO" "|" #.(boom)))))))"#,
        );

        let resolver = DirLocalsResolver::new(DirLocalsConfig {
            enabled: true,
            inherit: true,
            unsupported: DirLocalsUnsupportedPolicy::Error,
        });
        let error = resolver
            .resolve_for_file(&org_path, &notes_dir)
            .expect_err("error policy should fail");

        match error {
            super::DirLocalsError::Unsupported { path, message } => {
                assert_eq!(path, dir_locals_path);
                assert!(message.contains(
                    "ignored unsafe org-todo-keywords value: reader syntax is not supported"
                ));
            }
            other => panic!("unexpected error: {other}"),
        }
    }
}
