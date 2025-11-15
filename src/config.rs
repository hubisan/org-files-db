// src/config.rs
//
// Lädt TODO-Keywords aus:
// 1) CLI (--todo="A,B,C") -> via ENV ORG_FILES_DB_TODO_CLI
// 2) Datei (~/.config/org-files-db/todo_keywords.txt oder --todo-file=PFAD)
// 3) Fallback (Unicode-Uppercase-Regel)
//

use std::path::PathBuf;
use std::fs;

/// Der Modus, wie TODO-Keywords bestimmt werden.
#[derive(Debug, Clone)]
pub enum TodoMode {
    /// Benutzerdefinierte TODO-Liste (CLI + Datei kombiniert)
    UserDefined(Vec<String>),

    /// Automatische Unicode-Uppercase-Regel
    AutoUppercase,
}

//
// ─────────────────────────────────────────────
//   TODO LISTE LADEN (CLI + FILE + Fallback)
// ─────────────────────────────────────────────
//

/// Lädt die TODO-Liste.
///
/// cli_keywords und file_override_path werden gewöhnlich als None
/// aufgerufen, weil wir die Werte aus ENV nutzen.
///
/// ENV-Variablen:
///   ORG_FILES_DB_TODO_CLI="A,B,C"
///   ORG_FILES_DB_TODO_FILE="/path/to/file"
pub fn load_todo_keywords(
    cli_keywords: Option<&str>,
    file_override_path: Option<&str>,
) -> TodoMode {
    let mut keywords: Vec<String> = Vec::new();

    //
    // 1. CLI
    //
    if let Some(s) = cli_keywords {
        for kw in s.split(',') {
            let k = kw.trim();
            if !k.is_empty() {
                keywords.push(k.to_string());
            }
        }
    }

    //
    // 2. Datei-Liste
    //
    let path = file_override_path
        .map(PathBuf::from)
        .unwrap_or_else(config_file_path);

    if let Ok(content) = fs::read_to_string(&path) {
        for line in content.lines() {
            let l = line.trim();
            if !l.is_empty() && !keywords.contains(&l.to_string()) {
                keywords.push(l.to_string());
            }
        }
    }

    //
    // 3. Ergebnis: User-defined vorhanden?
    //
    if !keywords.is_empty() {
        return TodoMode::UserDefined(keywords);
    }

    //
    // 4. Fallback: Uppercase-Regel
    //
    TodoMode::AutoUppercase
}

//
// ─────────────────────────────────────────────
//   UNICODE-UPPERCASE FALLBACK
// ─────────────────────────────────────────────
//

/// Ein Uppercase-Wort für automatische TODO-Erkennung.
///
/// Erlaubt:
/// - Unicode-Großbuchstaben: ÄÖÜA-Z
/// - Ziffern
/// - '_' und '-'
///
/// Beispiele, die als TODO gelten:
///   ÜBERSICHT
///   WARTEN
///   PLAN_2025
///
/// Beispiele, die NICHT gelten:
///   Übersicht
///   Warten
///   Todo
pub fn is_uppercase_word(s: &str) -> bool {
    s.chars()
        .all(|c| c.is_uppercase() || c.is_numeric() || c == '_' || c == '-')
}

//
// ─────────────────────────────────────────────
//   DEFAULT-PFAD FÜR TODO-KEYWORD-DATEI
// ─────────────────────────────────────────────
//

/// Liefert den Pfad zu:
///   ~/.config/org-files-db/todo_keywords.txt
pub fn config_file_path() -> PathBuf {
    let base = std::env::var("XDG_CONFIG_HOME")
        .map(PathBuf::from)
        .unwrap_or_else(|_| {
            dirs::home_dir()
                .unwrap_or_else(|| PathBuf::from("."))
                .join(".config")
        });

    base.join("org-files-db").join("todo_keywords.txt")
}
