use std::{
    fs,
    io::{self, Read},
    path::Path,
};

use crate::{
    db::{index_state::IndexState, open_existing_database_read_only, read_index_state},
    presentation::{PresentationResponse, PresentationSpec},
    query::{
        execute_and_shape_query, parse_query, sqlite_query_validation_options, validate_query,
        QueryExecutionOptions, QueryInclude, QueryResponse,
    },
};

use super::{
    args::{CliQueryInclude, CliQueryOutput, CliQueryOutputFormat},
    error::CliError,
    load_cli_config,
};

#[cfg(test)]
pub(super) fn query_json_response(
    query: &str,
    output: CliQueryOutput,
    includes: &[CliQueryInclude],
    config_path: Option<&Path>,
) -> Result<QueryResponse, CliError> {
    query_json_response_with_restriction(query, output, includes, config_path, None)
}

#[cfg(test)]
pub(super) fn query_json_response_with_restriction(
    query: &str,
    output: CliQueryOutput,
    includes: &[CliQueryInclude],
    config_path: Option<&Path>,
    restricted_file_paths: Option<Vec<String>>,
) -> Result<QueryResponse, CliError> {
    query_response_with_restriction(
        query,
        output,
        includes,
        config_path,
        restricted_file_paths,
        &IndexStateGuard::default(),
    )
}

/// Optional `--expect-database-id` / `--expect-generation` checks, applied to
/// the index state read inside the query's own read transaction.
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub(super) struct IndexStateGuard {
    pub(super) expect_database_id: Option<String>,
    pub(super) expect_generation: Option<i64>,
}

impl IndexStateGuard {
    fn is_empty(&self) -> bool {
        self.expect_database_id.is_none() && self.expect_generation.is_none()
    }

    fn check(&self, state: &IndexState) -> Result<(), CliError> {
        let database_id_matches = self
            .expect_database_id
            .as_ref()
            .is_none_or(|expected| *expected == state.database_id);
        let generation_matches = self
            .expect_generation
            .is_none_or(|expected| expected == state.generation);
        if database_id_matches && generation_matches {
            return Ok(());
        }
        Err(CliError::StaleIndex {
            expected_database_id: self.expect_database_id.clone(),
            expected_generation: self.expect_generation,
            actual_database_id: state.database_id.clone(),
            actual_generation: state.generation,
        })
    }
}

pub(super) fn query_response_with_restriction(
    query: &str,
    output: CliQueryOutput,
    includes: &[CliQueryInclude],
    config_path: Option<&Path>,
    restricted_file_paths: Option<Vec<String>>,
    guard: &IndexStateGuard,
) -> Result<QueryResponse, CliError> {
    let config = load_cli_config(config_path)?;
    let connection =
        open_existing_database_read_only(&config.db_path).map_err(CliError::Database)?;
    if !guard.is_empty() {
        // The guard check and the query must see one committed snapshot.
        connection
            .execute_batch("BEGIN DEFERRED TRANSACTION")
            .map_err(|source| CliError::PresentationSnapshot {
                operation: "begin",
                source,
            })?;
        let state = read_index_state(&connection).map_err(CliError::IndexState)?;
        guard.check(&state)?;
    }
    let parsed = parse_query(query).map_err(CliError::QueryParse)?;
    let validation_options =
        sqlite_query_validation_options(&connection).map_err(CliError::QueryExecute)?;
    let validated = validate_query(parsed, &validation_options).map_err(CliError::QueryValidate)?;
    let explicit_includes = includes
        .iter()
        .copied()
        .map(QueryInclude::from)
        .collect::<Vec<_>>();
    let options = QueryExecutionOptions {
        output_mode: output.into(),
        includes: explicit_includes,
        query_timezone: config.query.timezone.clone(),
        now_utc: None,
        restricted_file_paths,
    };
    let response =
        execute_and_shape_query(&connection, &validated, &options).map_err(CliError::QueryShape)?;
    if !guard.is_empty() {
        connection
            .execute_batch("COMMIT")
            .map_err(|source| CliError::PresentationSnapshot {
                operation: "commit",
                source,
            })?;
    }
    Ok(response)
}

pub(super) fn presentation_response_with_restriction(
    query: &str,
    output: CliQueryOutput,
    includes: &[CliQueryInclude],
    config_path: Option<&Path>,
    restricted_file_paths: Option<Vec<String>>,
    spec: &PresentationSpec,
    guard: &IndexStateGuard,
) -> Result<PresentationResponse, CliError> {
    let config = load_cli_config(config_path)?;
    let connection =
        open_existing_database_read_only(&config.db_path).map_err(CliError::Database)?;
    connection
        .execute_batch("BEGIN DEFERRED TRANSACTION")
        .map_err(|source| CliError::PresentationSnapshot {
            operation: "begin",
            source,
        })?;

    let state = read_index_state(&connection).map_err(CliError::IndexState)?;
    guard.check(&state)?;
    let parsed = parse_query(query).map_err(CliError::QueryParse)?;
    let validation_options =
        sqlite_query_validation_options(&connection).map_err(CliError::QueryExecute)?;
    let validated = validate_query(parsed, &validation_options).map_err(CliError::QueryValidate)?;
    let explicit_includes = includes
        .iter()
        .copied()
        .map(QueryInclude::from)
        .collect::<Vec<_>>();
    let query_includes = spec
        .combined_includes_for_query_target(validated.target, &explicit_includes)
        .map_err(CliError::PresentationSpec)?;
    let options = QueryExecutionOptions {
        output_mode: output.into(),
        includes: query_includes,
        query_timezone: config.query.timezone.clone(),
        now_utc: None,
        restricted_file_paths,
    };
    let query_response =
        execute_and_shape_query(&connection, &validated, &options).map_err(CliError::QueryShape)?;
    let response = spec
        .build_response(state.database_id, state.generation, query_response.results)
        .map_err(CliError::PresentationBuild)?;

    connection
        .execute_batch("COMMIT")
        .map_err(|source| CliError::PresentationSnapshot {
            operation: "commit",
            source,
        })?;
    Ok(response)
}

pub(super) fn read_restricted_file_paths(source: &str) -> Result<Vec<String>, CliError> {
    let content = if source == "-" {
        let mut content = String::new();
        io::stdin()
            .read_to_string(&mut content)
            .map_err(|source| CliError::ReadRestriction {
                location: "stdin".to_string(),
                source,
            })?;
        content
    } else {
        fs::read_to_string(source).map_err(|error| CliError::ReadRestriction {
            location: source.to_string(),
            source: error,
        })?
    };
    let paths = serde_json::from_str::<Vec<String>>(&content).map_err(CliError::RestrictionJson)?;
    if paths.iter().any(|path| path.is_empty()) {
        return Err(CliError::InvalidRestriction(
            "restricted file paths must not be empty".to_string(),
        ));
    }
    Ok(paths
        .into_iter()
        .collect::<std::collections::BTreeSet<_>>()
        .into_iter()
        .collect())
}

pub(super) fn parse_query_presentation_spec(
    format: CliQueryOutputFormat,
    source: Option<&str>,
) -> Result<Option<PresentationSpec>, CliError> {
    match (format, source) {
        (CliQueryOutputFormat::Json, None) => Ok(None),
        (CliQueryOutputFormat::Json, Some(_)) => Err(CliError::InvalidPresentationUsage(
            "--presentation-spec-json requires --format presentation-json".to_string(),
        )),
        (CliQueryOutputFormat::PresentationJson, None) => Err(CliError::InvalidPresentationUsage(
            "--format presentation-json requires --presentation-spec-json".to_string(),
        )),
        (CliQueryOutputFormat::PresentationJson, Some(source)) => {
            PresentationSpec::parse_json(source)
                .map(Some)
                .map_err(CliError::PresentationSpec)
        }
    }
}
