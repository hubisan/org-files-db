use std::{
    fs,
    io::{self, Write},
    process::ExitCode,
};

use rusqlite::Connection;
use serde::Serialize;

use crate::{
    config::Config,
    db::{
        open_existing_database_read_only, read_index_changes, read_index_state,
        read_schema_version, IndexChanges, CURRENT_SCHEMA_VERSION,
    },
    indexer::{Indexer, RebuildOptions, RebuildReport},
    parser::OrgizeAdapter,
    presentation_view_rebuild::run_rebuild_worker,
    watcher_cli::run_watch_command,
};

mod args;
mod error;
mod listing;
mod query;
mod search;
#[cfg(test)]
mod tests;
mod view;

use args::{Cli, CliOutputFormat, CliQueryOutputFormat, Command};
use clap::Parser;
use error::CliError;
use listing::{headings_json_rows, links_json_rows};
use query::{
    parse_query_presentation_spec, presentation_response_with_restriction,
    query_response_with_restriction, read_restricted_file_paths,
};
use search::{cli_search_scope, search_json_rows};
#[cfg(feature = "bench")]
pub(crate) use search::{
    production_search_result_count_with_connection,
    production_search_stable_results_with_connection, CliSearchScope,
};
use view::run_view_command;

pub fn run() -> ExitCode {
    let stdout = io::stdout();
    let mut handle = stdout.lock();
    match run_with_args_and_writer(std::env::args_os(), &mut handle) {
        Ok(()) => ExitCode::SUCCESS,
        Err(error) => {
            eprintln!("{error}");
            ExitCode::from(error.exit_code())
        }
    }
}

fn run_with_args_and_writer<I, T, W>(args: I, writer: &mut W) -> Result<(), CliError>
where
    I: IntoIterator<Item = T>,
    T: Into<std::ffi::OsString> + Clone,
    W: Write,
{
    let cli = match Cli::try_parse_from(args) {
        Ok(cli) => cli,
        // Help and version requests are successful output, not usage errors.
        Err(error) if !error.use_stderr() => {
            write!(writer, "{}", error.render()).map_err(CliError::Io)?;
            return Ok(());
        }
        Err(error) => return Err(CliError::Parse(error)),
    };
    match cli.command {
        Command::Rebuild {
            config,
            allow_empty,
            accept_source_root_changes,
        } => {
            let report = rebuild_with_options(&config, allow_empty, accept_source_root_changes)?;
            print_diagnostics(&report);
            Ok(())
        }
        Command::Watch { config } => {
            let config = Config::load_from_file(config).map_err(CliError::Config)?;
            let stderr = io::stderr();
            let mut handle = stderr.lock();
            run_watch_command(&config, &mut handle).map_err(CliError::Watcher)
        }
        Command::Headings {
            format,
            no_root,
            include_root,
            config,
        } => {
            let _deprecated_include_root = include_root;
            let output_format = format.selected();
            let rows = headings_json_rows(no_root, config.as_deref())?;
            write_output(output_format, writer, &rows)
        }
        Command::Links { format, config } => {
            let output_format = format.selected();
            let rows = links_json_rows(config.as_deref())?;
            write_output(output_format, writer, &rows)
        }
        Command::Status { format, config } => {
            let output_format = format.selected();
            let config = load_cli_config(config.as_deref())?;
            let connection =
                open_existing_database_read_only(&config.db_path).map_err(CliError::Database)?;
            let database_path = fs::canonicalize(&config.db_path).map_err(|source| {
                CliError::CanonicalizeDatabasePath {
                    path: config.db_path.clone(),
                    source,
                }
            })?;
            let schema_version = current_index_state_schema_version(&connection)?;
            let state = read_index_state(&connection).map_err(CliError::IndexState)?;
            let response = StatusJsonResponse {
                schema_version,
                database_path: database_path.display().to_string(),
                database_id: state.database_id,
                generation: state.generation,
                last_changed_at: state.last_changed_at,
            };
            write_output(output_format, writer, &response)
        }
        Command::Changes {
            format,
            database_id,
            since_generation,
            config,
        } => {
            let output_format = format.selected();
            let connection = open_cli_database(config.as_deref())?;
            let schema_version = current_index_state_schema_version(&connection)?;
            let changes = read_index_changes(&connection, &database_id, since_generation)
                .map_err(CliError::IndexState)?;
            let response = ChangesJsonResponse {
                schema_version,
                changes,
            };
            write_output(output_format, writer, &response)
        }
        Command::View { command } => run_view_command(command, writer),
        Command::PresentationViewRebuildWorker {
            db,
            cache_root,
            database_id,
            generation,
            effective_query_date,
        } => {
            let stdin = io::stdin();
            run_rebuild_worker(
                &db,
                &cache_root,
                &database_id,
                generation,
                effective_query_date.as_deref(),
                stdin.lock(),
            )
            .map_err(CliError::PresentationViewRebuild)
        }
        Command::Query {
            format,
            output,
            include,
            restrict_files_json,
            presentation_spec_json,
            config,
            query,
        } => {
            let output_format = format.selected();
            let presentation_spec =
                parse_query_presentation_spec(output_format, presentation_spec_json.as_deref())?;
            let restricted_file_paths = if let Some(source) = restrict_files_json.as_deref() {
                Some(read_restricted_file_paths(source)?)
            } else {
                None
            };

            match output_format {
                CliQueryOutputFormat::Json => {
                    let response = query_response_with_restriction(
                        &query,
                        output,
                        &include,
                        config.as_deref(),
                        restricted_file_paths,
                    )?;
                    write_json_output(writer, &response)
                }
                CliQueryOutputFormat::PresentationJson => {
                    let spec = presentation_spec.as_ref().ok_or_else(|| {
                        CliError::InvalidPresentationUsage(
                            "--format presentation-json requires --presentation-spec-json"
                                .to_string(),
                        )
                    })?;
                    let response = presentation_response_with_restriction(
                        &query,
                        output,
                        &include,
                        config.as_deref(),
                        restricted_file_paths,
                        spec,
                    )?;
                    write_compact_json_output(writer, &response)
                }
            }
        }
        Command::Search {
            format,
            title,
            body,
            config,
            expression,
        } => {
            let output_format = format.selected();
            let rows = search_json_rows(
                cli_search_scope(title, body),
                &expression,
                config.as_deref(),
            )?;
            write_output(output_format, writer, &rows)
        }
    }
}

#[cfg(test)]
fn rebuild(config_path: impl AsRef<std::path::Path>) -> Result<RebuildReport, CliError> {
    rebuild_with_options(config_path, false, false)
}

fn rebuild_with_options(
    config_path: impl AsRef<std::path::Path>,
    allow_empty: bool,
    accept_source_root_changes: bool,
) -> Result<RebuildReport, CliError> {
    Indexer::new(OrgizeAdapter::new())
        .rebuild_from_config_path_with_rebuild_options(
            config_path,
            RebuildOptions {
                allow_empty,
                accept_source_root_changes,
            },
        )
        .map_err(CliError::Indexer)
}

#[derive(Debug, Serialize)]
struct StatusJsonResponse {
    schema_version: u32,
    database_path: String,
    database_id: String,
    generation: i64,
    last_changed_at: String,
}

#[derive(Debug, Serialize)]
struct ChangesJsonResponse {
    schema_version: u32,
    #[serde(flatten)]
    changes: IndexChanges,
}

fn current_index_state_schema_version(connection: &Connection) -> Result<u32, CliError> {
    let version = read_schema_version(connection).map_err(CliError::SchemaInspect)?;
    if version != CURRENT_SCHEMA_VERSION {
        return Err(CliError::UnsupportedIndexStateSchema {
            on_disk_version: version,
            required_version: CURRENT_SCHEMA_VERSION,
        });
    }
    Ok(version)
}

fn open_cli_database(config_path: Option<&std::path::Path>) -> Result<Connection, CliError> {
    let config = load_cli_config(config_path)?;
    open_existing_database_read_only(&config.db_path).map_err(CliError::Database)
}

fn load_cli_config(config_path: Option<&std::path::Path>) -> Result<Config, CliError> {
    match config_path {
        Some(config_path) => Config::load_from_file(config_path).map_err(CliError::Config),
        None => Ok(Config::default()),
    }
}

fn write_output<T: Serialize>(
    format: CliOutputFormat,
    writer: &mut impl Write,
    value: &T,
) -> Result<(), CliError> {
    match format {
        CliOutputFormat::Json => write_json_output(writer, value),
    }
}

fn write_json_output<T: Serialize>(writer: &mut impl Write, value: &T) -> Result<(), CliError> {
    serde_json::to_writer_pretty(&mut *writer, value).map_err(CliError::Json)?;
    writer.write_all(b"\n").map_err(CliError::Io)
}

fn write_compact_json_output<T: Serialize>(
    writer: &mut impl Write,
    value: &T,
) -> Result<(), CliError> {
    serde_json::to_writer(&mut *writer, value).map_err(CliError::Json)?;
    writer.write_all(b"\n").map_err(CliError::Io)
}

fn print_diagnostics(report: &RebuildReport) {
    for diagnostic in &report.diagnostics {
        let label = match diagnostic.severity {
            crate::parser::DiagnosticSeverity::Warning => "warning",
            crate::parser::DiagnosticSeverity::Error => "error",
        };
        match (&diagnostic.file_path, diagnostic.line_number) {
            (Some(path), Some(line)) => {
                eprintln!("{label}: {}:{line}: {}", path.display(), diagnostic.message);
            }
            (Some(path), None) => {
                eprintln!("{label}: {}: {}", path.display(), diagnostic.message);
            }
            (None, _) => {
                eprintln!("{label}: {}", diagnostic.message);
            }
        }
    }
}
